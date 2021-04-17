expit = function(x) {
  exp(x)/(1 + exp(x))
}
logit = function(x) {
  log(x/(1 - x))
}

ep_vec = c(-3.015, -2.513, -2.118, -1.758, -1.42, -1.065, -0.66, -0.098)
#for l = 1, capture probabilities are 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
ep = ep_vec[it]
pi1 = function(x) {
  expit( ep + sum(c(0.4)*x))
}
pi2 = function(x) {
  expit( ep + sum(c(0.3)*x))
}
dat_p = function(n, l){
  x = matrix(runif(n*l, 0, 1) + 2,
             nrow = n, ncol = l)
  y1 = unlist(apply(x, 1, function(xi) {sample(c(0, 1), 1, replace = TRUE, prob = c( 1 - pi1(xi), pi1(xi)))}))
  y2 = unlist(apply(x, 1, function(xi) {sample(c(0, 1), 1, replace = TRUE, prob = c( 1 - pi2(xi), pi2(xi)))}))
  xp = do.call("cbind", lapply(1:ncol(x),
                               function(li){
                                 if(li%%4 == 1){
                                  return(exp(x[,li]/2))
                                 }else if(li%%4 == 2){
                                  return(x[,li]/(1 + exp(x[,li -1])) + 10)
                                 }else if(li%%4 == 3){
                                   return((x[,li]*x[,li-2]/25 + 0.6)^3)
                                 }else{
                                   return((x[,li -2] + x[,li] + 20)^2)
                                 }
  }))
  List_matrix = cbind(y1, y2, x)
  List_matrix_xstar = cbind(y1, y2, xp)

  p1 = unlist(apply(x, 1, pi1))
  p2 = unlist(apply(x, 1, pi2))

  q1 = p1/(1 - (1-p1)*(1-p2))
  q2 = p2/(1 - (1-p1)*(1-p2))
  q12 = p2*p1/(1 - (1-p1)*(1-p2))
  return(list(List_matrix = List_matrix, List_matrix_xstar = List_matrix_xstar,
              psi0 = 1 -  mean(apply(x, 1, function(xx){return((1 - pi1(xx))*(1 - pi2(xx)))}))
              ))
}
