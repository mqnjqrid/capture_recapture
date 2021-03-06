# color departments of Peru by number of killings

library(ggmap)
library(ggplot2)
library(raster)
library(maptools)
library(maps)
library(viridis)
library(reshape2)
library(rayshader)
library(tidyverse)
library(sf)
# first map of Peru -----------------------------------------------------

peru = st_read("~/shape_files/DEPARTAMENTOS.shp", quiet = TRUE)
k = 1
load("~/data/department_kills_for_map_eps0005.Rdata")
slu_est_diff = output_n[,str_subset(colnames(output_n),"SLU_sl")] -
               output_n[,str_subset(colnames(output_n),"EST_sl")]
colnames(slu_est_diff) = gsub("SLU_", "", str_subset(colnames(output_n),"SLU_sl"))

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

colvecvalue = cbind(slu_est_diff[,1], charcols(strsplit(colnames(slu_est_diff)[1], '_'), 25))
peru_dep = cbind(peru, colvecvalue)
colvecvalue = cbind(slu_est_diff[,2], charcols(strsplit(colnames(slu_est_diff)[2], '_'), 25))
peru_dep = rbind(peru_dep, cbind(peru, colvecvalue))
colvecvalue = cbind(slu_est_diff[,3], charcols(strsplit(colnames(slu_est_diff)[3], '_'), 25))
peru_dep = rbind(peru_dep, cbind(peru, colvecvalue))

colnames(peru_dep)[5:7] = c("difference", "model", "method")
peru_dep$difference = as.numeric(as.character(peru_dep$difference))
peru_dep$model = factor(peru_dep$model, levels = c('logit', 'sl'))
peru_dep$method = factor(peru_dep$method, levels = c("PI", "DR", "TMLE"))
label_vec = c(round(min(slu_est_diff)), -1500, -1000, -500, -100, 0, 100,  500, 1000, 1500, round(max(slu_est_diff)))
gmodulus = ggplot(peru_dep[peru_dep$model != "SjL",]) +
  geom_sf(aes(fill = difference)) +
  scale_fill_gradient2(trans = #"identity",
                       modulus_trans(0.5),
                      # midpoint = 0,#zero,
                       low = "dodgerblue1",#"#56B4E9", ,
                       mid = "white",
                       high = "yellow2",#"#E69F00",# "#FFD700",#,#"goldenrod3"
                       space = "Lab",
                       limits = c(min(peru_dep$difference)-1, max(peru_dep$difference)+1)
                      , breaks = label_vec, labels = label_vec
                       ) +
  #facet_grid(model~method) +
  facet_wrap(~method) +
  ggtitle("Difference in the estimated number of killings between the\nPCP-Shining Path and the State") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90), legend.position = "bottom", legend.key.width = unit(2.5, "cm"))
gmodulus

##############################################################################
###                  RATIO BETWEEN SLU AND EST
##############################################################################

slu_est_prop = output_n[,str_subset(colnames(output_n),"SLU_sl")]/(output_n[,str_subset(colnames(output_n),"SLU_sl")] +
  output_n[,str_subset(colnames(output_n),"EST_sl")])
colnames(slu_est_prop) = gsub("SLU_sl_", "", str_subset(colnames(output_n),"SLU_sl"))
zero = 0
min_slu_est_prop = min(slu_est_prop)

colvecvalue = cbind(slu_est_prop[,1], colnames(slu_est_prop)[1])
peru_dep = cbind(peru, colvecvalue)
colvecvalue = cbind(slu_est_prop[,2], colnames(slu_est_prop)[2])
peru_dep = rbind(peru_dep, cbind(peru, colvecvalue))
colvecvalue = cbind(slu_est_prop[,3], colnames(slu_est_prop)[3])
peru_dep = rbind(peru_dep, cbind(peru, colvecvalue))

colnames(peru_dep)[5:6] = c("proportion", "method")
peru_dep$proportion = as.numeric(as.character(peru_dep$proportion))

#label_vec = c(round(min_slu_est_diff), -2000, -1000, -500, 0, 250, 500, round(max(slu_est_diff)))
gprop = ggplot(peru_dep) +
  geom_sf(aes(fill = proportion)) +
  scale_fill_gradient2(trans = "identity",
                        # modulus_trans(0),
                       midpoint = 0.5,
                       low = "dodgerblue1",#"#56B4E9", ,
                       mid = "white",
                       high = "#FFD700",#"goldenrod3"#"#E69F00"
                       space = "Lab"
                      , limits = c(0, 1)
  ) +
  facet_wrap(~method) +
  ggtitle("Proportion in estimated killings of SLU out of SLU and EST") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90), legend.position = "bottom", legend.key.width = unit(2.5, "cm"))
gprop

##############################################################################
