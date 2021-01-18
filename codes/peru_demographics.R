library(ggplot2)
library(ggpubr)
library(plyr)
source("C:/Users/manja/Dropbox/capture_recapture/codes/Peru_codes/peru_1.R")

covariate_data = cbind(perpe, x$Sexo, age, strata, ui, x$Situacion)
demo_cvr = cbind(covariate_data[x[,10] == 1,], "CVR")
demo_dp = cbind(covariate_data[x[,17] == 1,], "DP")
demo_odh = cbind(covariate_data[x[,16] == 1,], "ODH")
demo_data = rbind(demo_cvr, demo_dp, demo_odh)
colnames(demo_data) = c("perpe", "sex", "age", "strata", "ui", "Situacion", "list")
demo_data = data.frame(demo_data)
demo_data[,"sex"] = as.factor(demo_data[,"sex"])
demo_data[,"perpe"] = revalue(demo_data$perpe, c("1" = "State", "2" = "PCP-Shining Path", "3" = "Others"))
demo_data[,"age"] = as.numeric(demo_data[,"age"])
demo_data[,"strata"] = as.numeric(demo_data[,"strata"])
demo_data[,"ui"] = as.numeric(demo_data[,"ui"])
demo_data[,"Situacion"] = as.factor(demo_data[,"Situacion"])
demo_data$Situacion = mapvalues(demo_data$Situacion, from = c("DES", "MUE"), to = c("disappeared", "    dead"))
demo_data$uifac = 2
demo_data$uifac[demo_data$ui == 0] = 0
demo_data$uifac[is.na(demo_data$ui)] = 1
demo_data$uifac = as.factor(demo_data$uifac)
demo_data = na.omit(demo_data)
demo_data = demo_data[(demo_data$sex %in% c('F', 'M')), ]

pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/peru_demographics.pdf", height = 4.5, width = 6)
ggplot(demo_data, aes(age, fill = sex)) +
  geom_density(alpha = 0.5, color = "black") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.1, end = 0.8)
dev.off()
pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/sex_kill_count.pdf", height = 4.5, width = 6)
ggplot(demo_data, aes(sex, fill = sex)) +
  geom_bar(color = "black") +
  theme_bw() +
  theme(text = element_text(size = 12), legend.position = "none") +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.3, end = 0.8)
dev.off()

pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/peru_demographics_situacion.pdf", height = 4.5, width = 6)
ggplot(demo_data, aes(age, fill = Situacion)) +
  geom_density(alpha = 0.5, color = "black") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.1, end = 0.8)
dev.off()


pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/sex_kill_count_situacion.pdf", height = 4.5, width = 6)
ggplot(demo_data, aes(Situacion, fill = Situacion)) +
  geom_bar(color = "black", position = "dodge") +
  theme_bw() +
  theme(text = element_text(size = 12), legend.position = "none") +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.3, end = 0.8)
dev.off()

tsize = 20

g1 = ggplot(demo_data, aes(age, fill = sex)) +
  geom_density(alpha = 0.5, color = "black") +
  theme_bw() +
  theme(text = element_text(size = tsize)) +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.1, end = 0.8)
g2 = ggplot(demo_data, aes(sex, fill = sex)) +
  geom_bar(color = "black") +
  theme_bw() +
  theme(text = element_text(size = tsize), legend.position = "none") +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.3, end = 0.8)
g3 = ggplot(demo_data, aes(age, fill = Situacion)) +
  geom_density(alpha = 0.5, color = "black") +
  theme_bw() +
  theme(text = element_text(size = tsize)) +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.1, end = 0.8)
g4 = ggplot(demo_data, aes(Situacion, fill = sex)) +
  geom_bar(color = "black", position = "dodge") +
  theme_bw() +
  xlab("situation") +
  theme(text = element_text(size = tsize), legend.position = "none") +
  facet_grid(list~perpe, scales = "free") +
  scale_fill_grey(start = 0.3, end = 0.8)
pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/peru_demographics.pdf", height = 4.5*1.2, width = 12*1.2, onefile = FALSE)
ggarrange(g1, g4, common.legend = TRUE, legend = "bottom")
dev.off()

ggplot(demo_data[demo_data$uifac != 2,], aes(uifac, fill = Situacion)) +
  geom_bar(color = "black", stat = "count", position = "dodge") +
  theme(text = element_text(size = 12), legend.position = "none") +
  facet_grid(list~perpe) +
  #scale_fill_grey(start = 0.3, end = 0.8) +
  theme_bw()

ggplot(demo_data, aes(age, fill = situation)) +
  geom_density(alpha = 0.5, color = "black") +
  theme(text = element_text(size = 12)) +
  facet_grid(list~perpe) +
  scale_fill_grey(start = 0.1, end = 0.8) +
  theme_bw()
dev.off()

covariate_data = cbind(perpe, x$Sexo, age, as.numeric(x$AnoHechos))
demo_cvr = cbind(covariate_data[x[,10] == 1,], "CVR")
demo_dp = cbind(covariate_data[x[,17] == 1,], "DP")
demo_odh = cbind(covariate_data[x[,16] == 1,], "ODH")
demo_data = rbind(demo_cvr, demo_dp, demo_odh)
colnames(demo_data) = c("perpe", "sex", "age", "death_year", "list")
demo_data = data.frame(demo_data)
demo_data[,"sex"] = as.factor(demo_data[,"sex"])
demo_data[,"perpe"] = revalue(demo_data$perpe, c("1" = "State", "2" = "PCP-Shining Path", "3" = "Others"))
demo_data[,"age"] = as.numeric(demo_data[,"age"])
demo_data[,"death_year"] = as.numeric(as.character(demo_data[,"death_year"]))
demo_data = na.omit(demo_data)
demo_data = demo_data[(demo_data$sex %in% c('F', 'M')), ]


ggplot(demo_data, aes(death_year, fill = sex)) +
  geom_bar() +
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 90)) +
  xlim(c(1980, 2000)) +
  facet_grid(list~perpe)

############################ interaction among lists

demo_data = cbind(perpe1, x$Sexo, age, x$Situacion, x[,10], pmax(x[,16], x[,17]), x[,10]+2*pmax(x[,16], x[,17]))
colnames(demo_data) = c("perpe", "sex", "age", "situacion", "CVR", "DP-ODH", "list2")
demo_data = data.frame(demo_data)
demo_data[,"sex"] = as.factor(demo_data[,"sex"])
demo_data[,"perpe"] = revalue(demo_data$perpe, c("1" = "State", "2" = "PCP-Shining Path", "3" = "Others", "4" = "nod"))
demo_data$perpe = factor(demo_data$perpe, levels = c("State", "PCP-Shining Path", "Others", "nod"))
demo_data[,"age"] = as.numeric(demo_data[,"age"])
demo_data = na.omit(demo_data)
demo_data = demo_data[(demo_data$sex %in% c('F', 'M')), ]
demo_data[,"list2"] = as.factor(demo_data[,"list2"])

ggplot(demo_data, aes(x = "", y = list2, fill = list2)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  facet_grid(situacion ~ perpe) + #, scales = "free") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

par(mfrow = c(3,5), mar = c(0,0,0,0), oma = c(0,2,2,0))
for (situation in c("DES",
                   "MUE",
                   "all"
  )){

  if(situation == "all"){
    demo_data1 = demo_data
  }else if(situation == "MUE"){
    demo_data1 = demo_data[demo_data$situacion == 'MUE',]
  }else if(situation == "DES"){
    demo_data1 = demo_data[demo_data$situacion == 'DES',]
  }
  for(agent in c("State", "PCP-Shining Path", "Others", "nod")){

    print(c(agent))
    datap = demo_data1[demo_data1$perpe == agent,]

    datap$list2 = revalue(datap$list2, c("1" = "10", "2" = "01", "3" = "11"))

    pie(summary(datap$list2), col = c("#999999", "#E69F00", "#56B4E9"))
    if (situation == "DES") {
      mtext(agent, side = 3)
    }
    if(agent == "State") {
      mtext(situation, side = 2)
    }

  }
  datap = demo_data1
  datap$list2 = revalue(datap$list2, c("1" = "10", "2" = "01", "3" = "11"))

  pie(summary(datap$list2), col = c("#999999", "#E69F00", "#56B4E9"))
  if (situation == "DES") {
    mtext("Total", side = 3)
  }
}





