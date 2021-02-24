#---------------------------------------------------------------------------
#    Kill estimate by different departamentos
#---------------------------------------------------------------------------
devtools::install_github("mqnjqrid/drpop")
library(drpop)
source("~/peru_1.R")
K = 2
eps = 0.005
provyes = FALSE

list2 = pmax(x[,16], x[,17])
misage = rep(1, length(age))
misage[is.na(age)] = 0
age_nona = age
age_nona[is.na(age_nona)] = 0
iddpto = round(ui/10000)
idprov = round(ui/100)
datap0 = cbind(x[,c(10)], list2, age_nona, perpe1, misage, x[,c(2, 6)], ui, iddpto, idprov, strata)

datap0 = merge(datap0, depcoord, by.x = "iddpto", by.y = "IDDPTO", all.x = TRUE)

datap0 = merge(datap0, cordarea, by.x = "strata", by.y = "strata", all.x = TRUE)

datap0 = merge(datap0, provcoord, by.x = "idprov", by.y = "IDPROV", all.x = TRUE)

if(provyes == FALSE){
  datap0 = datap0[,-grep("_p", names(datap0))]
}

datap0 = datap0[,-which(names(datap0) %in% c("ui", "idprov", "strata"))]
datap0$Sexo[which(is.na(datap0$Sexo))] = 'N'
datap0$misage = factor(datap0$misage)
datap0$Sexo = factor(datap0$Sexo)
datap0$perpe1 = factor(datap0$perpe1)
datap0$Situacion = factor(datap0$Situacion)
datap0 = data.frame(datap0)

delcol = which(names(datap0) %in% c("misage", "Sexo", "Situacion", "ubina", "depna", "provna"))
datap_modelmatrix = cbind(datap0[,-delcol], model.matrix(~misage + Sexo + Situacion, datap0))

funcname = c("logit", "sl")

nmat = matrix(NA, nrow = 25, ncol = 6*length(funcname))
varnmat = nmat
psimat = nmat
varpsimat = nmat

orgsize = matrix(NA, ncol = 6, nrow = 25)
for (departmentos in 1:25){
  for (agent in 1:2){
    print(c("d a", departmentos, agent))
    K = 2
    datapset = datap_modelmatrix[
      datap_modelmatrix$iddpto %in% c(departmentos) & datap_modelmatrix$perpe1 %in% c(agent),
      -which(names(datap_modelmatrix) %in% c("iddpto", "perpe1"))]

    n = length(intersect(which(idepa == departmentos), which(perpe == agent)))
    N = nrow(na.omit(datapset))
    orgsize[departmentos, (agent*2 - 1):(agent*2)] = c(n, N)
  }
}
colnames(orgsize) = c("EST_n", "EST_N", "SLU_n", "SLU_N", "OTR_n", "OTR_N")

set.seed(123)
for (departmentos in 1:25){#c(5,9,10,12)){
  nvec = numeric(0)
  varnvec = nvec
  psivec = numeric(0)
  varpsivec = nvec
  for (agent in 1:2){
    print(c("d a", departmentos, agent))
    K = 2
    datapset = datap_modelmatrix[
      datap_modelmatrix$iddpto %in% c(departmentos) & datap_modelmatrix$perpe1 %in% c(agent),
      -which(names(datap_modelmatrix) %in% c("iddpto", "perpe1"))]
    if(sum(is.na(datapset$x_p))/nrow(datap_modelmatrix) > 0.3) {
      datapset = datapset[,-grep("_p", names(datapset))]
    }

    datapset = datapset[,-which(apply(datapset[,!(names(datapset) %in% c("(Intercept)"))], 2, function(col){length(unique(col)) <= 1}))[-1]]

    n = length(intersect(which(idepa == departmentos), which(perpe == agent)))
    l = ncol(datapset) - K
    if(length(datapset) < 3){
      print("no")
      nvec = cbind(nvec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
      varnvec = cbind(varnvec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
      psivec = cbind(psivec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
      varpsivec = cbind(varpsivec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
    }else if((min(c("x...c.10..", "list2")%in% names(datapset)) == 0) | nrow(datapset) <= 10 | sum(datapset[,1]*datapset[,2]) <= 5) {
      print("no")
      nvec = cbind(nvec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
      varnvec = cbind(varnvec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
      psivec = cbind(psivec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
      varpsivec = cbind(varpsivec, matrix(n, ncol = 3*length(funcname), nrow = K*(K - 1)/2))
    }else{
      print("yes")
      List_matrix = na.omit(datapset)
      N = nrow(List_matrix)
      est_val = psinhat(List_matrix, K = K, funcname = funcname, nfolds = 2, eps = eps)

      nvec = cbind(nvec, (est_val$n))
      varnvec = cbind(varnvec, (est_val$varn))
      psivec = cbind(psivec, (est_val$psi))
      varpsivec = cbind(varpsivec, (est_val$sigma2))

      if (sum(est_val$n_summary == "NaN") > 1){
        nvec[nvec == "NaN"] = n
        varnvec[varnvec == "NaN"] = 0
        psivec[psivec == "NaN"] = n
        varpsivec[varpsivec == "NaN"] = 0
      }

    }
  }

  nmat[departmentos,] = nvec
  varnmat[departmentos,] = varnvec
  psimat[departmentos,] = psivec
  varpsimat[departmentos,] = varpsivec
}

output_n = nmat
colnames(output_n) = paste(rep(c("EST", "SLU"), each = 3*length(funcname)), rep(paste(rep(c('logit', 'sl'), each = 3), c('_PI', "_DR", "_TMLE"), sep = ''), 2), sep = '_')
colnames(nmat) = colnames(output_n)
colnames(psimat) = colnames(output_n)
colnames(varpsimat) = colnames(output_n)
colnames(varnmat) = colnames(output_n)
