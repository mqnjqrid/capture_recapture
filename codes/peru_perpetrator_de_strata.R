library(ggplot2)
library(ggpubr)
library(robustbase)

devtools::install_github("mqnjqrid/drpop")
library(drpop)
source("~/peru_1.R")
K = 2
nfolds = 5
eps = 0.005

funcname = c("logit", "sl")

psimat = data.frame(folds = rep(1:nfolds, 5*3), situacion = rep(rep(c("DES", "MUE", "all"), each = nfolds), 5), perpetrator = rep(1:5, each = nfolds*3), matrix(NA, nrow = nfolds*3*5, ncol = length(funcname)*4))
psiestim = data.frame(perpetrator = rep(1:5, each = 3*length(funcname)*3),
                      situacion = rep(rep(c("DES", "MUE", "all"), each = length(funcname)*3), 5),
                      model = rep(rep(funcname, each = 3), 15),
                      method = rep(c("PI", "BC", "TMLE"), length(funcname)*5*3),
                      matrix(NA, ncol = 6, nrow = 3*3*5*length(funcname)))
colnames(psiestim)[-(1:4)] = c("n", "N", "psi", "varphi", "n_hat", "varn")

list2 = pmax(x[,16], x[,17])
misage = rep(1, length(age))
misage[is.na(age)] = 0
age_nona = age
age_nona[is.na(age_nona)] = 0
iddpto = round(ui/10000)
idprov = round(ui/100)

datap0 = cbind(x[,c(10)], list2, age_nona, perpe1, misage, x[,c(2, 6)], ui, iddpto, idprov, strata)
datap0 = merge(datap0, cordarea, by.x = "strata", by.y = "strata", all.x = TRUE)
datap0 = merge(datap0, depcoord, by.x = "iddpto", by.y = "IDDPTO", all.x = TRUE)

datap0 = datap0[,-which(names(datap0) %in% c("ui", "iddpto", "idprov", "strata"))]
datap0$Sexo[which(is.na(datap0$Sexo))] = 'N'
datap0$misage = factor(datap0$misage)
datap0$Sexo = factor(datap0$Sexo)
datap0$perpe1 = factor(datap0$perpe1)
datap0$Situacion = factor(datap0$Situacion)
datap0 = data.frame(datap0)
datap0 = na.omit(datap0)

for(situation in c("DES", "MUE", "all")){
  delcol = which(names(datap0) %in% c("misage", "Sexo", "Situacion", "ubina", "depna", "provna"))
  if(situation == "all"){
    datap_modelmatrix = cbind(datap0[,-delcol], model.matrix(~misage + Sexo + Situacion + perpe1, datap0))
  }else if(situation == "MUE"){
    datap = datap0[datap0$Situacion == 'MUE',]
    datap_modelmatrix = cbind(datap[,-delcol], model.matrix(~misage + Sexo + perpe1, datap))
  }else if(situation == "DES"){
    datap = datap0[datap0$Situacion == 'DES',]
    datap_modelmatrix = cbind(datap[,-delcol], model.matrix(~misage + Sexo + perpe1, datap))
  }

    for(agent in list(c(1), c(2), c(3), c(4))){

    print(c(agent))
    K = 2
    datapset = datap_modelmatrix[datap_modelmatrix$perpe1 %in% agent, -grep("perpe", names(datap_modelmatrix))]

    if(situation == "all"){
      n = length(which(perpe1 %in% agent))
    }else if(situation == "MUE"){
      n = length(intersect(which(perpe1 %in% agent), which(x$Situacion == "MUE")))
    }else if(situation == "DES"){
      n = length(intersect(which(perpe1 %in% agent), which(x$Situacion == "DES")))
    }
    #n = length(which(perpe1 == agent))
    print(n)
    l = ncol(datapset) - K
    List_matrix = (datapset)
    #print(head(List_matrix, 2))

    N = length(which(rowSums(List_matrix[,1:K]) > 0))
    if (N >= 5) {
      est_val = psinhat(List_matrix, K = K, funcname = funcname, nfolds = nfolds, eps = eps, twolist = FALSE)
      #psimat[psimat$situacion == situation & psimat$perpetrator == agent,-c(1:3)][1:nfolds,] = cbind(est_val$psimat, est_val$psimat2)
      psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent,5:10] = cbind(n, N, t(est_val$psi), t(est_val$sigma2),
                                                                                                   t(est_val$n), t(est_val$varn))
    }else{
      psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent,5:10] = cbind(n, N, NA, NA, n, 0)

    }
  }

  agent = 5

  datapset = datap_modelmatrix[, -which(names(datap_modelmatrix) == "perpe1") ]

  if(situation == "all"){
    n = 24692
  }else if(situation == "MUE"){
    n = sum(x$Situacion == "MUE")
  }else if(situation == "DES"){
    n = sum(x$Situacion == "DES")
  }

  print(n)
  l = ncol(datapset) - K
  List_matrix = (datapset)

  N = length(which(rowSums(List_matrix[,1:K]) > 0))
  if (N >= 5) {
    est_val = psinhat(List_matrix, K = K, funcname = funcname, nfolds = nfolds, eps = eps, twolist = FALSE)
    #psimat[psimat$situacion == situation & psimat$perpetrator == agent,-c(1:3)][1:nfolds,] = cbind(est_val$psimat, est_val$psimat2)
    psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent,5:10] = cbind(n, N, t(est_val$psi), t(est_val$sigma2),
                                                                                           t(est_val$n), t(est_val$varn))
  }else{
    psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent,5:10] = cbind(n, N, NA, NA, n, 0)

  }
}
psibarplot = psiestim
psibarplot$situacion = factor(psibarplot$situacion)
levels(psibarplot$situacion) = list("disappeared"="DES", "dead"="MUE", "combined"="all")
psibarplot$method = factor(psibarplot$method)
levels(psibarplot$method) = list("PI"="PI", "DR"="DR", "TMLE"="TMLE")
psibarplot$perpetrator = factor(psibarplot$perpetrator)
levels(psibarplot$perpetrator) = list("EST"=1, "SLU"=2, "OTR"=3, "NOD"=4, "Total"=5)
tsize = 12
psibarplot$n_hat = as.numeric(as.character(psibarplot$n_hat))
options(scipen = 5)

tsize = 15

psipibctr = ggplot(psibarplot, aes(x = perpetrator, y = n_hat, fill = perpetrator)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Number of killings") +
  #ylim(c(0, NA)) +
  scale_fill_manual(name = "Perpetrator", labels = c("State", "PCP-Shining Path", "Others", "Unidentified", "Total"), values = c("EST" = "#56B4E9", "SLU" = "#E69F00", "OTR" = "#999999", "NOD" = "lightgreen", "Total" = "lightcoral")) +
  facet_grid(situacion~method + model, scales = "free_y", labeller=label_wrap_gen(width = 10, multi_line = FALSE)) +
  geom_errorbar(aes(ymin = pmax(n_hat - 1.95*sqrt(varn), n), ymax = n_hat + 1.95*sqrt(varn)), size = 0.5) +
  theme(text = element_text(size = tsize), axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), legend.position = "bottom")

psipibctr
