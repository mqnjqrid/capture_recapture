#rm(list=ls(all=TRUE))

# load maps ---------------------------------------------------------------

library(ggmap)
library(ggplot2)
library(raster)
library(maptools)
library(maps)
library(viridis)
library(reshape2)
library(rayshader)
library(tidyverse)
library(dplyr)
library(sf)
library(ggpubr)
# first map of Peru -----------------------------------------------------
setwd("C:/Users/manja/OneDrive/Documents/Capture_Recapture")
peru = st_read("data/shape_files/DEPARTAMENTOS.shp", quiet = TRUE)
peru_reg = st_read("data/shape_files/REGIONS.shp", quiet = TRUE)

library(scales) #for trans_new
modulus_trans <- function(lambda){
  trans_new("modulus",
            transform = function(y){
              if(lambda != 0){
                yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
              } else {
                yt = sign(y) * (log(abs(y) + 1))
              }
              return(yt)
            },
            inverse = function(yt){
              if(lambda != 0){
                y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
              } else {
                y <- (exp(abs(yt)) - 1) * sign(yt)
                
              }
              return(y)
            }
  )
}

charcols = function(charvec, nrow){
  return(matrix(rep(unlist(charvec), nrow), nrow = nrow, byrow = TRUE))
}


##############################################################################
###                  Joined departments
##############################################################################
load("data/peru_data/region_kills_for_map_eps001_strataind_alldata.Rdata")

slu_est_diff = output
slu_est_diff$diff = output[,'2'] - output[,'1']
slu_est_diff = slu_est_diff[c(1, 2, 5, 6, 3, 7, 4),]

peru_str = cbind(peru_reg, slu_est_diff$diff)

colnames(peru_str)[3] = "difference"
peru_str$difference = as.numeric(as.character(peru_str$difference))
#peru_str$model = factor(peru_str$model, levels = c('logit', 'ranger'))
#peru_str$method = factor(peru_str$method, levels = c("PI", "DR", "TMLE"))
#peru_str$method = revalue(peru_str$method, c("BC" = "DR"))
label_vec = c(round(min(slu_est_diff$diff, na.rm = TRUE)), -500, -100, -20, 0, 20, 100,  500, round(max(slu_est_diff$diff, na.rm = TRUE)))

peru_str$sign = '+'
peru_str$sign[peru_str$difference<0] = '-'

gmodulus = ggplot(peru_str) +
  geom_sf(aes(fill = difference)) +
  theme_bw() +
  scale_fill_gradient(trans = #"identity",
                         modulus_trans(0.25),
                       # midpoint = 0,#zero,
                       low = "black",#"#56B4E9",
                       high = "white",#"#E69F00",# "#FFD700",#,#"goldenrod3"
                       space = "Lab",
                       limits = range(label_vec)+c(-1,1)
                       , breaks = label_vec, labels = label_vec
  ) +
  #geom_sf_text(aes(label = sign)) +
  #facet_grid(model~method) +
  #facet_wrap(~method, labeller = as_labeller(c("PI"="Plug-in", "DR"="Doubly robust", "TMLE"="TMLE"))) +
  ggtitle(NULL) +
  #ggtitle("Difference in the estimated number of victims between the PCP-Shining Path and the State") +
  #theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.key.width = unit(2.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
gmodulus

#pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/kill_slu_est_diff_region_ranger_1_23_eps0005_strataind_DR.pdf", width = 9, height = 4.5)
#gmodulus
#dev.off()

##############################################################################
###                  Individual departments
##############################################################################
load("data/peru_data/department_kills_for_map_eps001_strataind_alldata.Rdata")

slu_est_diffd = output
slu_est_diffd$diff = output[,'2'] - output[,'1']

peru_dep = cbind(peru, slu_est_diffd$diff)

colnames(peru_dep)[5] = "difference"
peru_dep$difference = as.numeric(as.character(peru_dep$difference))

label_vec = c(round(min(slu_est_diffd$diff, slu_est_diff$diff, na.rm = TRUE)), -500, -100, -20, 0, 20, 100,  500, round(max(slu_est_diffd$diff, slu_est_diff$diff, na.rm = TRUE)))

peru_dep$sign = '+'
peru_dep$sign[peru_str$difference<0] = '-'

gmodulusd = ggplot(peru_dep) +
  geom_sf(aes(fill = difference)) +
  scale_fill_gradient(trans = #"identity",
                         modulus_trans(0.25),
                       # midpoint = 0,#zero,
                       low = "black",#"#56B4E9", ,
                       high = "white",#"#E69F00",# "#FFD700",#,#"goldenrod3"
                       space = "Lab",
                       limits = c(min(label_vec)-1, max(label_vec)+1)
                       , breaks = label_vec, labels = label_vec
  ) +
  #geom_sf_text(aes(label = sign)) +
  ggtitle(NULL) +
  #facet_grid(model~method) +
  #facet_wrap(~method, labeller = as_labeller(c("PI"="Plug-in", "DR"="Doubly robust", "TMLE"="TMLE"))) +
  #ggtitle("Difference in the estimated number of victims between the PCP-Shining Path and the State") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        #plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.key.width = unit(2.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
gmodulusd

#pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/kill_slu_est_diff_ranger_1_23_eps001_strataind_dep_region.pdf")
#gmodulusd
#dev.off()

g_depstr = ggarrange(gmodulusd + geom_text(x = -78, y = -18, label = "Departments"),
                     gmodulus + geom_text(x = -78, y = -18, label = "Regions"),
                     common.legend = TRUE, legend = "bottom")
g_dep_str = annotate_figure(g_depstr, top = text_grob("Difference in the estimated number of victims between the PCP-Shining Path and the State"))#, fig.lab = c("by department", "by region")
g_dep_str
pdf("C:/Users/manja/Dropbox/capture_recapture/codes/images/peru_kill_map/kill_slu_est_diff_ranger_1_23_eps001_strataind_dep_region_alldata_bw.pdf", width = 7.89, height = 4.5)
g_dep_str
dev.off()
