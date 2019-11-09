pow = 1
iter = 500
estim_pnpt = function(List_matrix, n, K, whichfunc, funcname, psi0){
  if(missing(n)){
    n = nrow(List_matrix)
  }
  if(missing(K)){
    K = ncol(List_matrix) - 1
  }
  
  l = ncol(List_matrix) - K
  List_matrix = na.omit(List_matrix)
  #removing all rows with only 0's
  List_matrix_cov = List_matrix[which(rowSums(List_matrix[,1:K]) > 0),]
  colnames(List_matrix_cov) = c(paste("L", 1:K, sep = ''), paste("x", 1:(ncol(List_matrix_cov) - K), sep = ''))
  #N = number of observed or captured units
  N = nrow(List_matrix_cov)
  
  #psi_summary and psi_summary2 store n_hat for (plug-in, bias-corrected) and (plug-in, tmle)
  #var_summary and var_summary2 store varience(n_hat) for (plug-in, bias-corrected) and (plug-in, tmle)
  psi_summary = matrix(n, nrow = K*(K - 1)/2, ncol = 2*length(whichfunc))
  rownames(psi_summary) = unlist(sapply(1:(K - 1), function(k) {
    sapply((k + 1):K, function(s) {
      return(paste(k, ", ", s, sep = ''))
    })}))
  colnames(psi_summary) = paste(rep(funcname, each = 2), c('', " BC"), sep = '')
  var_summary = psi_summary
  psi_summary2 = psi_summary
  var_summary2 = psi_summary
  
  #splitting samples to avoid over-fitting
  set1 = sample(N, ceiling(N/2), replace = FALSE)
  List1 = as.data.frame(List_matrix_cov[set1,])
  List2 = as.data.frame(List_matrix_cov[-set1,])
  
  for(i in 1:(K - 1)){
    for(j in (i + 1):K){ #print(c(i,j))
      
      yi = List1[,paste("L", i, sep = '')]
      yj = List1[,paste("L", j, sep = '')]
      
      
      for (fi in 1:length(whichfunc)){ 
        print(fi)
        
        estims = try(whichfunc[[fi]](List2, List1, K, i, j))
        if (class(estims) == "try-error") {
            next
        }
        q1dot = estims$q1dot
        qdot1 = estims$qdot1
        q11 = estims$q11
        
        datmat = as.data.frame(cbind(yi, yj, yi*yj, q1dot - q11, qdot1 - q11, q11, List1[,-(1:K)], yi*(1 - yj), yj*(1 - yi)))
        datmat[,4:6] = cbind(apply(datmat[,4:6], 2, function(u) {return(pmax(pmin(u, 0.995), 0.005))}))
        colnames(datmat) = c("yi", "yj", "yij", "q10", "q01", "q11", colnames(List1)[-(1:K)], "yi0", "y0j")
        
        
        psihat = max(min(1/mean(q1dot*qdot1/q11, na.rm = TRUE), 0.95), 0.05)
        
        Qnphihat = mean(-psihat^2 * ((yj - qdot1)*q1dot/q11
                                     + (yi - q1dot)*qdot1/q11 
                                     - (yi*yj - q11)*q1dot*qdot1/q11^2 + q1dot*qdot1/q11 - 1/psihat), na.rm = TRUE)
        psihatq = max(min(psihat + Qnphihat, 0.95), 0.05)
        
        psi_summary[paste(i, ", ", j, sep = ''),(2*fi - 1):(2*fi)] = pmax(n, n/c(psihat, psihatq))
        
        semivarx = mean(1/psihat^2/datmat$q11 - (datmat$q10 + datmat$q01 + 2*datmat$q11)/datmat$q11/psihat)
        varx = (semivarx + 2/psihat - 1/psihat^2)*psihat^4
        
        semivarxq = mean(1/datmat$q11/psihatq^2 - (datmat$q10 + datmat$q01 + 2*datmat$q11)/datmat$q11/psihatq)
        varxq = (semivarxq + 2/psihatq - 1/psihatq^2)*psihatq^4
        
        var_summary[paste(i, ", ", j, sep = ''),(2*fi - 1):(2*fi)] = sqrt((c(varx, varxq)+c(psihat, psihatq))/c(psihat, psihatq)^2/(1-c(psihat, psihatq))*
                                                                            N/c(psihat, psihatq)*(1-c(psihat, psihatq))/c(psihat, psihatq))
        
        # start of tmle algorithm
        epsilon_error = 1
        cnt = 0
        
        q11_vec = numeric(0)
        q10_vec = q11_vec
        q01_vec = q11_vec
        
        while (abs(epsilon_error) > 0.0001){
          cnt = cnt + 1
          
          q11_vec = c(q11_vec, mean((yi*yj - datmat[,"q11"])*(datmat$q10 + datmat$q11)*((datmat$q01 + datmat$q11)/datmat$q11^2
                                                                                        - (datmat$q01 + datmat$q11)/datmat$q11 - (datmat$q10 + datmat$q11)/datmat$q11
          )))
          q10_vec = c(q10_vec, mean((yi*(1 - yj) - datmat[,"q10"])*(datmat$q01 + datmat$q11)/datmat$q11))
          q01_vec = c(q01_vec, mean(((1 - yi)*yj - datmat[,"q01"])*(datmat$q10 + datmat$q11)/datmat$q11))
          
          datmat_old = datmat
          if (cnt > iter){break}
          
          dat2 = cbind(datmat$yi0, logit(datmat$q10), ((datmat$q10 + datmat$q11)/datmat$q11)^(pow))
          colnames(dat2) = c("yi0", "logitq10", "ratio")
          dat2 = as.data.frame(dat2)
          mod2 = try(glm(yi0 ~ offset(logitq10) + ratio, family = binomial(), data = dat2, na.action = na.omit))
          if (class(mod2) != "try-error"){
            datmat$q10 = predict(mod2, newdata = dat2, type = "response")
            
          }
          head(cbind(datmat, datmat_old))
          datmat[,4:6] = cbind(apply(datmat[,4:6], 2, function(u) {return(pmax(pmin(u, 0.995), 0.05))}))
          
          dat3 = cbind(datmat$y0j, logit(datmat$q01), ((datmat$q01 + datmat$q11)/datmat$q11)^(pow) )
          colnames(dat3) = c("y0j", "logitq01", "ratio")
          dat3 = as.data.frame(dat3)
          mod3 = try(glm(y0j ~ offset(logitq01) + ratio, family = binomial(), data = dat3, na.action = na.omit))
          if (class(mod3) != "try-error"){
            datmat$q01 = predict(mod3, newdata = dat3, type = "response")
          }
          datmat[,4:6] = cbind(apply(datmat[,4:6], 2, function(u) {return(pmax(pmin(u, 0.995), 0.05))}))
          
          dat1 = cbind(datmat$yij, logit(datmat$q11), ((datmat$q10 + datmat$q11)/datmat$q11
                                                       + (datmat$q01 + datmat$q11)/datmat$q11
                                                       - (datmat$q10 + datmat$q11)*(datmat$q01 + datmat$q11)/datmat$q11^2)^(pow) )
          colnames(dat1) = c("yij", "logitq11", "ratio")
          dat1 = as.data.frame(dat1)
          mod1 = try(glm(yij ~ offset(logitq11) + ratio, family = binomial(), data = dat1, na.action = na.omit))
          if (class(mod1) != "try-error"){
            datmat[,"q11"] = predict(mod1, newdata = dat1, type = "response")
            datmat[,"q11"] = pmin(datmat[,"q11"], 1 - datmat$q10 - datmat$q01)
          }
          
          datmat[,4:6] = cbind(apply(datmat[,4:6], 2, function(u) {return(pmax(pmin(u, 0.995), 0.05))}))
          
          q_change = unlist(datmat_old[,4:6] - datmat[,4:6])
          epsilon_error = max(abs(c(mod2$coefficients[2], mod3$coefficients[2], mod2$coefficients[2])), na.rm = TRUE)
          c(mod1$coefficients[2], mod3$coefficients[2], mod2$coefficients[2])
          
        }
        
        psihat = max(min(1/mean((datmat$q10 + datmat$q11)*(datmat$q01 + datmat$q11)/datmat$q11), 0.95), 0.05)
        phihat = -psihat^2 * ((yj - datmat$q01 - datmat$q11)*(datmat$q10 + datmat$q11)/datmat$q11
                              + (yi - datmat$q10 - datmat$q11)*(datmat$q01 + datmat$q11)/datmat$q11 
                              - (yi*yj - datmat$q11)*(datmat$q10 + datmat$q11)*(datmat$q01 + datmat$q11)/datmat$q11^2 + (datmat$q10 + datmat$q11)*(datmat$q01 + datmat$q11)/datmat$q11 - 1/psihat)
        psihatq = max(min(psihat + mean(phihat), 0.95), 0.05)
        psi_summary2[paste(i, ", ", j, sep = ''),(2*fi - 1):(2*fi)] = pmax(n, n/c(psihat, psihatq))
        
        
        semivarx = mean(1/psihat^2/datmat$q11 - (datmat$q10 + datmat$q01 + 2*datmat$q11)/datmat$q11/psihat)
        varx = #mean(((datmat$q10 + datmat$q11)*(datmat$q01 + datmat$q11)/datmat$q11)^2)/N - 1/psihat^2/N#
          (semivarx + 2/psihat - 1/psihat^2)*psihat^4
        
        semivarxq = mean(1/datmat$q11/psihatq&2 - (datmat$q10 + datmat$q01 + 2*datmat$q11)/datmat$q11/psihatq)
        varxq =  (semivarxq + 2/psihatq - 1/psihatq^2)*psihatq^4
        
        var_summary2[paste(i, ", ", j, sep = ''),(2*fi - 1):(2*fi)] = sqrt((c(varx, varxq)+c(psihat, psihatq))/c(psihat, psihatq)^2/(1-c(psihat, psihatq))*
                                                                             N/c(psihat, psihatq)*(1-c(psihat, psihatq))/c(psihat, psihatq))
      }
    } 
  }
  
  return(list(psi_summary = psi_summary, var_summary = var_summary, psi_summary2 = psi_summary2, var_summary2 = var_summary2))
}