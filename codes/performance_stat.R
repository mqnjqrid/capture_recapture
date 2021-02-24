#devtools::install_github("mqnjqrid/drpop")
library(drpop)
library(ggplot2)
library(reshape2)
library(plyr)
library(ggpubr)
library(gridExtra)
library(beepr) 
n0 = 5000; l = 1
source("~/indep_cov_Tilling_simulation.R")
simuldraw = 500

alpha_vec = c(0.1, 0.25, 0.5)#, 0.2, 0.15, 0.1)
omega_vec = c(0.5, 1)
datorg = numeric(0)
varorg = datorg
norg = datorg
varnorg = datorg

twolist = FALSE
psi0 =  dat_p(n0, l)$psi0
set.seed(1)

for(alp in 1:length(alpha_vec)) {
  alpha = alpha_vec[alp]
  for(omg in 1:length(omega_vec)){print(c(n0, alp, omg))
    omega = omega_vec[omg]
    K = 2;l = 1

    for (s in 1:simuldraw){print(s)
      datap = dat_p(n0, l)
      List_matrix = datap$List_matrix

      N = sum(pmax(List_matrix[,1], List_matrix[,2]))

      est_val = psinhat_simul(List_matrix, n = n0, K = 2, omega = omega, alpha = alpha, twolist = twolist, eps = 0.005, nfolds = 1)

      datorg = rbind(datorg, cbind(est_val$psi, alpha, omega, n0))

      varorg = rbind(varorg, cbind(est_val$sigma2/N, alpha, omega, n0))

      norg = rbind(norg, cbind(est_val$n, alpha, omega, n0))

      varnorg = rbind(varnorg, cbind(est_val$varn, alpha, omega, n0))
    }
    beep(sound = 10)
  }
}
datorg = data.frame(datorg)
datorg = melt(datorg, id.vars = c("alpha", "omega", "n0"), value.name = "psi", variable.name = "model")
varorg = data.frame(varorg)
varorg = melt(varorg, id.vars = c("alpha", "omega", "n0"), value.name = "varpsi", variable.name = "model")
norg = data.frame(norg)
norg = melt(norg, id.vars = c("alpha", "omega", "n0"), value.name = "n", variable.name = "model")
varnorg = data.frame(varnorg)
varnorg = melt(varnorg, id.vars = c("alpha", "omega", "n0"), value.name = "varn", variable.name = "model")

psidata = merge(datorg, varorg, by = c("model", 'alpha', 'omega', "n0"))
psidata$alpha = as.numeric(as.character(psidata$alpha))
psidata$omega = as.numeric(as.character(psidata$omega))
psidata$psi0 = psi0

ndata = merge(norg, varnorg, by = c('model', 'alpha', 'omega', "n0"))
ndata$alpha = as.numeric(as.character(ndata$alpha))
ndata$omega = as.numeric(as.character(ndata$omega))
ndata$n = as.numeric(as.character(ndata$n))
ndata$varn = as.numeric(as.character(ndata$varn))

#save(psidata, ndata, psi0, n0,
#    file = paste("~/data/simulated/data_psi0", round(10*psi0), "_n0_", n0, ".Rdata", sep = ''))
#
#load("~/data/simulated/data_psi03_n0_5000.Rdata")

################### Plots for psi and n
dat_pibctr_summary = ddply(psidata, c("alpha", "omega", "n0", "model"), summarise,
                           mean = mean(abs(psi - psi0)),
                           rmse = sqrt(mean((psi - psi0)^2)),
                           sd = sqrt(var(psi))
)
#dat_pibctr_summary$model = ordered(factor(dat_pibctr_summary$model), levels = c("PI", "DR", "TMLE"))

dat_pibctr_summary$coverage = NA
for(alpha in unique(psidata$alpha)){
  for(omega in unique(psidata$omega)){
    for(model in unique(dat_pibctr_summary$model)){
      cat(n0, alpha, omega, model, '\n')
      subdat = ndata[ndata$n0 == n0 & ndata$alpha == alpha & ndata$omega == omega & ndata$model == model,]
      dat_pibctr_summary[dat_pibctr_summary$n0 == n0 & dat_pibctr_summary$alpha == alpha &
                           dat_pibctr_summary$omega == omega & dat_pibctr_summary$model == model,
      ]$coverage = mean(abs(subdat$n0 - subdat$n) <= sqrt(subdat$varn)*1.95, na.rm = TRUE)
    }
  }
}
dat_pibctr_summary$coverage = 1 - dat_pibctr_summary$coverage

tsize = 12

gbasic = ggplot(data = dat_pibctr_summary, aes(x = factor(model), fill = factor(model))) +
  theme_bw() +
  theme(text = element_text(size = tsize), legend.text=element_text(size=tsize), axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), legend.position = "bottom") +
  facet_grid(omega ~ alpha, labeller = label_bquote(rows = omega==.(omega), cols = alpha==.(alpha)), scales = "free_y") +
  #scale_fill_manual("Estimation method", values=c("red", "#E69F00", "#56B4E9", "gray")) +
  scale_fill_grey(start = 0, end = 1)

v1 = gbasic +
  geom_bar(mapping =  aes(y = mean), stat = "identity", color = "black") +
  labs(fill = "method", title = substitute(paste("Bias of ", psi, ', ', psi, ' = ', var), list(var = round(psi0, 1))), x = NULL, y = NULL)

v2 = gbasic +
  geom_bar(mapping = aes(y = rmse), stat = "identity", color = "black") +
  labs(fill = "method", title = expression(paste("RMSE of ", psi)), x = NULL, y = NULL)

v3 = gbasic +
  geom_bar(mapping =  aes(y = coverage), stat = "identity", color = "black") +
  labs(fill = "method", title = "Mis-coverage of n", x = NULL, y = NULL)

ggarrange(v1, v2, v3, ncol = 3, common.legend = TRUE, legend = "bottom")
