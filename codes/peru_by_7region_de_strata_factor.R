#---------------------------------------------------------------------------
#    Kill estimate by different regions
#---------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(robustbase)
library(plyr)
library(stringr)

#devtools::install_github("mqnjqrid/drpop")
library(drpop)
setwd("C:/Users/manja/OneDrive/Documents/Capture_Recapture")
load("data/peru_data/raw_peru_and_geoball.Rdata")

K = 2
nfolds = 5
eps = 0.01

funcname = c("ranger")

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

reg_cal <- function(iddpto){
  reg = rep(7, length(iddpto))
  reg[iddpto %in% c(5)] = 1
  reg[iddpto %in% c(12, 19)] = 2
  reg[iddpto %in% c(10, 22, 25)] = 3
  reg[iddpto %in% c(8, 3, 21)] = 4
  reg[iddpto %in% c(9)] = 5
  reg[iddpto %in% c(7, 15)] = 6
  return(reg)
}

reg = reg_cal(iddpto)
datap0 = cbind(list1, list2, age_nona, perpe1, misage, x[,c(2, 6)], ui, iddpto, strata, depna, stratana, reg)
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
datap0 = datap0[datap0$depna == 1 & datap0$stratana == 1,]
datap0 = na.omit(datap0)

datap0 = reformat(datap0, capturelists = c("list1", "list2"))
#delcol = which(names(datap0) %in% c("misage", "Sexo", "Situacion", "ubina", "depna", "provna", "strata"))

set.seed(10)
output = matrix(NA, ncol = 3, nrow = 7)
colnames(output) = c("region", "1", "2")
output = as.data.frame(output)
output$region = c(1:7)
for (region in 1:7){
  for (agent in 1:2){
    print(c("r a", region, agent))
    K = 2
    datapset = datap0[
      datap0$reg %in% c(region) & datap0$perpe1 %in% c(agent),
      -which(names(datap0) %in% c("iddpto", "reg", "perpe1"))]
    
    n = length(intersect(which(reg == region), which(perpe1 == agent)))
    l = ncol(datapset) - K
    if(length(datapset) < 3){
      print("no")
      output[output$region == region, as.character(agent)] = n
    }else if((min(c("list1", "list2")%in% names(datapset)) == 0) | nrow(datapset) <= 10 | sum(datapset[,1]*datapset[,2]) <= 5) {
      print("no")
      output[output$region == region, as.character(agent)] = n
    }else{
      print("yes")
      List_matrix = na.omit(datapset)
      N = nrow(List_matrix)
      est_val = popsize(List_matrix, K = K, funcname = funcname, nfolds = 5, eps = eps, TMLE = FALSE, PLUGIN = FALSE, Nmin = 1)
      output[output$region == region, as.character(agent)] = est_val$result$n
      
      if (sum(est_val$result$n == "NaN") >= 1){
        output[output$region == region, as.character(agent)] = n
      }
    }
  }
}

save(reg, output,
     file = paste0("data/peru_data/region_kills_for_map_eps0", str_replace(paste0(eps), "0.", ''), "_strataind_alldata.Rdata"))
