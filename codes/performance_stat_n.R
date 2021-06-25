library(drpop)
library(ggplot2)
library(reshape2)
library(plyr)
library(ggpubr)
library(gridExtra)
library(beepr)
#it = 2, 4, 7 for psi = 0.3, 0.5, 0.8
n0 = 1000; l = 1; it = 2
source("codes/indep_cov_Tilling_simulation.R")
simuldraw = 500
n_vec = c(1:5)*5000
twolist = FALSE
alpha_vec = 0.25
omega_vec = 1
datorg = numeric(0)
varorg = datorg
norg = datorg
varnorg = datorg

psi0 =  dat_p(n0, l)$psi0
set.seed(1)

for(n0 in n_vec) {
  for(alp in 1:length(alpha_vec)) {
    alpha = alpha_vec[alp]
    for(omg in 1:length(omega_vec)){print(c(n0, alp, omg))
      omega = omega_vec[omg]
      K = 2;l = 1

      for (s in 1:simuldraw){print(s)
        datap = dat_p(n, l)
        List_matrix = datap$List_matrix

        N = sum(pmax(List_matrix[,1], List_matrix[,2]))

        est_val = popsize_simul(List_matrix, n = n0, K = 2, omega = omega, alpha = alpha, twolist = twolist, eps = 0.005, nfolds = 2)

        datorg = rbind(datorg, cbind(est_val$psi, alpha, omega, n0))

        varorg = rbind(varorg, cbind(est_val$sigma2/N, alpha, omega, n0))

        norg = rbind(norg, cbind(est_val$n, alpha, omega, n0))

        varnorg = rbind(varnorg, cbind(est_val$varn, alpha, omega, n0))
      }
      beep(sound = 10)
    }
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
psidata$omega = as.numeric(as.character(psidata$sigma))
psidata$psi0 = psi0

ndata = merge(norg, varnorg, by = c('model', 'alpha', 'omega', "n0"))
ndata$alpha = as.numeric(as.character(ndata$alpha))
ndata$omega = as.numeric(as.character(ndata$sigma))

ndata = ndata

save(psidata, ndata, psi0,
         file = paste("data_psi0", round(10*psi0), ".Rdata", sep = ''))
#load("cond_indep_pair/data_psi08.Rdata")     

################### Plots for psi as capture probability
dat_pibctr_summary = ddply(psidata, c("alpha", "omega", "n0", "model"), summarise,
                           mean = mean(abs(psi - psi0)),
                           rmse = sqrt(mean((psi - psi0)^2)),
                           sd = sqrt(var(psi))
                           )

dat_pibctr_summary$coverage = NA
############## manually calculating coverage
for(n0 in unique(psidata$n0)){
  for(alpha in unique(psidata$alpha)){
    for(omega in unique(psidata$omega)){
      for(model in unique(dat_pibctr_summary$model)){
        n0;alpha;omega;model
        subdat = ndata[ndata$n0 == n0 & ndata$alpha == alpha & ndata$omega == omega & ndata$model == model,]
        dat_pibctr_summary[dat_pibctr_summary$n0 == n0 & dat_pibctr_summary$alpha == alpha &
                             dat_pibctr_summary$omega == omega & dat_pibctr_summary$model == model,
        ]$coverage = mean(abs(subdat$n0 - subdat$n) <= sqrt(subdat$varn)*1.95, na.rm = TRUE)
      }
    }
  }
}
dat_pibctr_summary$model = (factor(dat_pibctr_summary$model, levels = c("PI", "DR", "TMLE")))
dat_pibctr_summary = data.frame(dat_pibctr_summary)
tsize = 12
psize = 3

ggbasic = ggplot(data = dat_pibctr_summary[dat_pibctr_summary$omega == 1,],
                 aes(x = n0, linetype = model, shape = model)) +
  theme_bw() +
  theme(legend.key.width = unit(3, "line"), legend.text=element_text(size = tsize), text = element_text(size = tsize), axis.text.x = element_text(angle = 0),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(min(dat_pibctr_summary$n0), max(dat_pibctr_summary$n0), 10000)) +
  labs(color = "method", shape = "method", linetype = "method", x = 'n', y = NULL) +
  scale_color_grey(start = 0, end = 0.75, breaks = c("PI", "DR", "TMLE"), labels = c("Plug-in", "Doubly robust", "TMLE")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed"), breaks = c("PI", "DR", "TMLE"), labels = c("Plug-in", "Doubly robust", "TMLE")) +
  scale_shape_manual(values = c(16, 17, 15), breaks = c("PI", "DR", "TMLE"), labels = c("Plug-in", "Doubly robust", "TMLE"))

v1 = ggbasic +
  geom_line(aes(y = mean)) +
  geom_point(size = psize, aes(y = mean, color = model)) +
  labs(title = substitute(paste("Bias of ", psi, ', true ', psi, ' = ', var), list(var = round(psi0, 1))))

v2 = ggbasic +
  geom_line(aes(y = rmse)) +
  geom_point(size = psize, aes(y = rmse, color = model)) +
  labs(title = expression(paste("RMSE of ", psi)))

v3 = ggbasic +
  geom_hline(yintercept = 0.95, linetype = "dotdash", color = "grey") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 0.95, 1)) +
  geom_line(aes(y = coverage)) +
  geom_point(size = psize, aes(y = coverage, color = model)) +
  labs(title = "Coverage of n")

ggarrange(v1, v2, v3, ncol = 3, common.legend = TRUE, legend = "bottom")