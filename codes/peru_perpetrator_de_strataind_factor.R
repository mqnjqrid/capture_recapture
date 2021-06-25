library(ggplot2)
library(ggpubr)
library(robustbase)
library(plyr)
library(stringr)

#devtools::install_github("mqnjqrid/drpop")
library(drpop)
setwd("C:/Users/manja/OneDrive/Documents/Capture_Recapture")
load("data/peru_data/raw_peru_and_geoball.Rdata")

set.seed(10)
K = 2
nfolds = 5
eps = 0.01

skipperp = c(0)
funcname = c("ranger")

psiestim = data.frame(perpetrator = rep(1:5, each = 3*length(funcname)*3),
                      situacion = rep(rep(c("DES", "MUE", "all"), each = length(funcname)*3), 5),
                      matrix(NA, ncol = 10, nrow = 3*3*5*length(funcname)))
colnames(psiestim)[-(1:2)] = c("listpair", "model", "method", "psi", "sigma", "n", "sigman", "cin.l", "cin.u", "N")

list1 = x[,10]
list2 = pmax(x[,16], x[,17])
misage = rep(1, length(age))
misage[is.na(age)] = 0
age_nona = age
age_nona[is.na(age_nona)] = 0
iddpto = round(ui/10000)
idprov = round(ui/100)

depna = rep(1, length(iddpto))
depna[is.na(iddpto)] = 0
stratana = rep(1, length(strata))
stratana[strata == 59] = 0

datap0 = cbind(list1, list2, age_nona, perpe1, misage, x[,c(2, 6)], ui, iddpto, strata, depna, stratana)
datap0 = merge(datap0, strcoord, by.x = "strata", by.y = "strata", all.x = TRUE)
datap0 = merge(datap0, depcoord, by.x = "iddpto", by.y = "IDDPTO", all.x = TRUE)

datap0[datap0$stratana == 0, c("x_str", "y_str", "area_str")] = matrix(rep(c(-75.02, -9.19, median(strcoord$area_str)), sum(stratana == 0)), ncol = 3, byrow = TRUE)
datap0[datap0$depna == 0 | datap0$stratana == 0, c("x_de", "y_de", "area_de")] = matrix(rep(c(-75.02, -9.19, median(depcoord$area_de)), sum(datap0$depna == 0 | datap0$stratana == 0)), ncol = 3, byrow = TRUE)

#rm(list = c("age", "cordarea", "depcoord", "provcoord", "distcoord", "perucoord", "misage", "age_nona"))

datap0$strata = as.factor(datap0$strata)
datap0 = datap0[,-which(names(datap0) %in% c("ui", "iddpto", "idprov"))]
datap0$Sexo[which(is.na(datap0$Sexo))] = 'N'
datap0$Sexo[datap0$Sexo %in% c('I', 'S')] = 'N'
datap0$misage = factor(datap0$misage)
datap0$Sexo = factor(datap0$Sexo)
datap0$perpe1 = factor(datap0$perpe1)
datap0$Situacion = factor(datap0$Situacion)

datap0 = data.frame(datap0)
datap0 = na.omit(datap0)

datap0 = reformat(datap0, capturelists = c("list1", "list2"))

for(situation in c("DES", "MUE", "all")){
  if(situation == "all"){
    datap = datap0
  }else if(situation == "MUE"){
    datap = datap0[datap0$Situacion == 'MUE', !(names(datap0) == "Situacion")]
  }else if(situation == "DES"){
    datap = datap0[datap0$Situacion == 'DES', !(names(datap0) == "Situacion")]
  }
  
  for(agent in list(c(1), c(2), c(3), c(4))){
    if(agent %in% skipperp){
      next
    }
    
    print(c(agent))
    datapset = datap[datap$perpe1 %in% agent, -grep("perpe", names(datap))]
    
    if(situation == "all"){
      n = length(which(perpe1 %in% agent))
    }else if(situation == "MUE"){
      n = length(intersect(which(perpe1 %in% agent), which(x$Situacion == "MUE")))
    }else if(situation == "DES"){
      n = length(intersect(which(perpe1 %in% agent), which(x$Situacion == "DES")))
    }
    print(n)
    l = ncol(datapset) - K
    List_matrix = (datapset)

    N = length(which(rowSums(List_matrix[,1:K]) > 0))
    if (N >= 5) {
      est_val = popsize(List_matrix, K = K, funcname = funcname, nfolds = nfolds, eps = eps, twolist = FALSE, Nmin = 200, sl.lib = c("SL.ranger"))
      if("DR" %in% est_val$result$method){
        psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 3:11] = est_val$result
        psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 'N'] = est_val$N
      }                                                                                        
    }else{
      psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 'n'] = n
      psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 'N'] = n
    }
  }
  
  agent = 5
  if(agent %in% skipperp){
    next
  }
  datapset = datap
  
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
    est_val = popsize(List_matrix, K = K, funcname = funcname, nfolds = nfolds, eps = 0.01, twolist = FALSE)
    psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 3:11] = est_val$result
    psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 'N'] = est_val$N
  }else{
    psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 'n'] = n
    psiestim[psiestim$situacion == situation & psiestim$perpetrator == agent, 'N'] = n
  }
}
psibarplot = psiestim
psibarplot$situacion = factor(psibarplot$situacion)
levels(psibarplot$situacion) = list("disappeared"="DES", "dead"="MUE", "combined"="all")
psibarplot$method = factor(psibarplot$method)
levels(psibarplot$method) = list("PI"="PI", "DR"="DR", "TMLE"="TMLE")
psibarplot$perpetrator = factor(psibarplot$perpetrator)
levels(psibarplot$perpetrator) = list("EST"=1, "SLU"=2, "OTR"=3, "NOD"=4, "Total"=5)
options(scipen = 5)
save(psiestim, psibarplot, file = paste0("data/peru_data/killing_barplot2_eps0", str_replace(paste0(eps), "0.", ''), "_latlong_de_strataind_alldata.Rdata"))

#load("data/peru_data/killing_barplot2_eps001_latlong_de_strataind_alldata.Rdata")
tsize = 15

slctmethod = "DR"
psipibctr1 = ggplot(psibarplot[psibarplot$model %in% c("ranger") & psibarplot$perpetrator %in% c("Total") & psibarplot$method == slctmethod,], aes(x = situacion, y = n, fill = situacion)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Estimated number of victims") +
  ggtitle("") +
  #scale_fill_manual(name = "", values = c("dead" = "#56B4E9", "disappeared" = "#E69F00", "combined" = "lightgreen", "Total" = "lightcoral")) +
  scale_fill_grey("", start = 0.35, end = 0.84, 
                  #values = c("disappeared"="disappearances", "dead"="killings", "combined"="combined")
                  labels = c("disappearances", "killings", "combined")
                  ) +
  facet_wrap(~perpetrator, nrow = 1,
             labeller = as_labeller(c("EST"="State", "SLU"="PCP-Shining Path", "OTR"="Others", "NOD"="Unidentified", "Total"="Total"))) +
  geom_errorbar(aes(ymin = cin.l, ymax = cin.u), size = 0.5) +
  theme(text = element_text(size = tsize), axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())


psipibctr2 = ggplot(psibarplot[psibarplot$model %in% c("ranger") & psibarplot$perpetrator %in% c("EST", "SLU", "NOD") & psibarplot$method == slctmethod,], aes(x = situacion, y = n, fill = situacion)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Estimated number of victims") +
  ggtitle("") +
  #scale_fill_manual(name = "", values = c("dead" = "#56B4E9", "disappeared" = "#E69F00", "combined" = "lightgreen", "Total" = "lightcoral")) +
  scale_fill_grey("", start = 0.35, end = 0.84, 
                  #values = c("disappeared"="disappearances", "dead"="killings", "combined"="combined")
                  labels = c("disappearances", "killings", "combined")
  ) +
  facet_wrap(~perpetrator, nrow = 1,
             labeller = as_labeller(c("EST"="State", "SLU"="PCP-Shining Path", "OTR"="Others", "NOD"="Unidentified", "Total"="Total"))) +
  geom_errorbar(aes(ymin = cin.l, ymax = cin.u), size = 0.5) +
  theme(text = element_text(size = tsize), axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = tsize))

g2 = ggarrange(psipibctr2, psipibctr1 + ylab(NULL), common.legend = TRUE, ncol = 2, legend = "bottom", widths = c(3, 1.2))
g2

datatable = psibarplot[psibarplot$model %in% c("ranger") & psibarplot$method == "DR" & psibarplot$situacion == "combined",c("perpetrator", "N", "n", "cin.l", "cin.u")]
datatable$CI = paste0('[', round(datatable$cin.l), ', ', round(datatable$cin.u), ']')
datatable$n = round(datatable$n)
datatable$perpetrator = c("State", "PCP-Shining Path", "Others", "Unidentified", "Total")
xtable::xtable(datatable[,-c(4, 5)], row.names = FALSE)
