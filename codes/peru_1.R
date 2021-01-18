library(sf)
library(lwgeom)
library(plyr)
library(dplyr)
source("C:/Users/manja/Dropbox/capture_recapture/codes/Peru_codes/peru_data.R")
peru_all = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_all.tab", sep = '\t', header = TRUE)
#peru_dep_coor = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_dep_coor.tab", sep = '\t')
#peru_de_acor = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_de_acor.tab", sep = '\t', header = TRUE)
#peru_di_acor = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_p_acor.tab", sep = '\t', header = TRUE)
#peru_p_acor = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_di_acor.tab", sep = '\t', header = TRUE)

#peru_dep = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_dep.tab", sep = '\t', header = TRUE)
#peru_dist = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_dist.tab", sep = '\t', header = TRUE)
#peru_prov = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/peru_prov.tab", sep = '\t', header = TRUE)

distxy = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/distxy.tab", sep = '\t', header = TRUE)

#library(foreign)
#dd = read.dbf("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/departamentos.dbf")

strata = rep(59, nrow(x))

strata[idepa==5 & iprov==5 & idist %in% c(0, 5, 6)] = 25
strata[idepa==5 & iprov==2] = 32
strata[idepa==5 & iprov==10] = 35
strata[idepa==5 & iprov==11] = 36
strata[idepa==12 & iprov %in% c(3, 5, 7, 8)] = 51
strata[idepa==10] = 14
strata[idepa==22 & iprov %in% c(6, 10)] = 14
strata[idepa==3 | idepa==8] = 11
strata[idepa==21] = 47
strata[idepa==9 & iprov %in% c(3, 6)] = 48
strata[idepa==7] = 2
strata[idepa==15 & iprov %in% c(3, 9)] = 6
strata[idepa %in% c(16, 17, 25)] = 1
strata[idepa == 2 & iprov == 14] = 3
strata[idepa == 2 & !(iprov %in% c(5, 14))] = 4
strata[idepa==4 | idepa==18 | idepa==13 | idepa==14 | idepa==18 | idepa==20 | idepa==11 | idepa==23 | idepa==24] = 5
strata[idepa == 15 & iprov %in% c(2, 4, 6, 7, 8)] = 7
strata[idepa == 15 & iprov == 1 & idist %in% c(33, 35, 42)] = 8
strata[idepa == 15 & iprov == 1 & !(idist %in% c(33, 35, 42))] = 9
strata[idepa == 15 & iprov %in% c(5, 10)] = 10
strata[idepa == 1 | idepa == 6 | (idepa == 22 & iprov %in% c(8,1,5,4,3))] = 12
strata[idepa == 22 & iprov %in% c(2, 7)] = 13
strata[idepa == 19] = 15
strata[idepa == 5 & iprov %in% c(7, 8)] = 16
strata[idepa == 5 & iprov == 6 & idist %in% c(2, 5, 6, 8, 4, 3)] = 17
strata[idepa == 5 & iprov == 6 & !(idist %in% c(2, 5, 6, 8, 4, 3))] = 18
strata[idepa == 5 & iprov == 4 & idist %in% c(2, 3, 8)] = 19
strata[idepa==5 & iprov==4 & idist %in% c(0, 1)] = 20
strata[idepa==5 & iprov==4 & idist==4] = 21
strata[idepa==5 & iprov==4 & idist==5] = 22
strata[idepa==5 & iprov==4 & idist==6] = 23
strata[idepa==5 & iprov==4 & idist==7] = 24
strata[idepa==5 & iprov==5 & idist==1] = 26
strata[idepa==5 & iprov==5 & idist==2] = 27
strata[idepa==5 & iprov==5 & idist==3] = 28
strata[idepa==5 & iprov==5 & idist==4] = 29
strata[idepa==5 & iprov==5 & idist==7] = 30
strata[idepa==5 & iprov==5 & idist==8] = 31
strata[idepa==5 & iprov==2] = 32
strata[idepa==5 & iprov==3] = 33
strata[idepa==5 & iprov==9] = 34
strata[idepa==5 & iprov==1 & idist %in% c(6, 15, 7, 1, 8)] = 37
strata[idepa == 5 & iprov == 1 & idist == 3] = 38
strata[idepa == 5 & iprov == 1 & idist == 4] = 39
strata[idepa == 5 & iprov == 1 & idist %in% c(2, 11)] = 40
strata[idepa == 5 & iprov == 1 & idist == 5] = 41
strata[idepa == 5 & iprov == 1 & idist == 9] = 42
strata[idepa == 5 & iprov == 1 & idist == 10] = 43
strata[idepa == 5 & iprov == 1 & idist == 12] = 44
strata[idepa == 9 & iprov %in% c(1, 2, 5, 7)] = 45
strata[idepa == 5 & iprov == 1 & idist == 14] = 46
strata[idepa == 5 & iprov == 1 & idist == 13] = 49
strata[idepa == 9 & iprov == 4] = 50
strata[idepa == 12 & iprov %in% c(3, 7, 8)] = 51
strata[idepa == 12 & iprov %in% c(4, 2, 9)] = 52
strata[idepa == 12 & iprov == 1 & idist == 1] = 53
strata[idepa == 12 & iprov == 1 & idist == 7] = 54
strata[idepa == 12 & iprov == 1 & idist == 14] = 55
strata[idepa == 12 & iprov == 1 & !(idist %in% c(1, 7, 14))] = 56
strata[idepa == 12 & iprov == 6] = 57
strata[idepa == 22 & iprov %in% c(0, 9)] = 58

jj = rep(7, nrow(x))
jj[strata %in% c(25, 32, 35, 36)] = 1
jj[strata == 51] = 2
jj[strata == 14] = 3
jj[strata %in% c(11, 47)] = 4
jj[strata == 48] = 5
jj[strata %in% c(2, 6)] = 6

####################################################
peru = st_read("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/shape_files/DEPARTAMENTOS.shp", stringsAsFactors = FALSE, quiet = TRUE)
peru_ll <- st_transform(peru, "+proj=longlat +ellps=WGS84 +datum=WGS84")
coordfile = st_coordinates(peru_ll)
depcoord = aggregate(cbind(X, Y) ~ L3, "mean" , data = coordfile)
depcoord = cbind(peru$IDDPTO, depcoord[,2:3])
colnames(depcoord) = c("IDDPTO", "x_de", "y_de")
depcoord$area_de = units::set_units(x = st_area(peru), value = hectare)

peru = st_read("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/shape_files/PROVINCIAS.shp", stringsAsFactors = FALSE, quiet = TRUE)
peru_ll <- st_transform(peru, "+proj=longlat +ellps=WGS84 +datum=WGS84")
coordfile = st_coordinates(peru_ll)
provcoord = aggregate(cbind(X, Y) ~ L3, "mean" , data = coordfile)
provcoord = cbind(peru$IDPROV, provcoord[,2:3])
colnames(provcoord) = c("IDPROV", "x_p", "y_p")
provcoord$area_p = units::set_units(x = st_area(peru), value = hectare)

peru = st_read("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/shape_files/DISTRITOS.shp", stringsAsFactors = FALSE, quiet = TRUE)
peru_ll <- st_transform(peru, "+proj=longlat +ellps=WGS84 +datum=WGS84")
coordfile = st_coordinates(peru_ll)
distcoord = aggregate(cbind(X, Y) ~ L3, "mean" , data = coordfile)
distcoord = cbind(peru$IDDIST, distcoord[,2:3])
colnames(distcoord) =  c("IDDIST", "x_di", "y_di")
distcoord$area_di = units::set_units(x = st_area(peru), value = hectare)

perucoord = peru[,c("IDDPTO", "IDPROV", "IDDIST")]
st_geometry(perucoord) = NULL
perucoord = perucoord %>% merge(depcoord, by = "IDDPTO") %>%
            merge(provcoord, by = "IDPROV") %>% merge(distcoord, by = "IDDIST")
perucoord$IDDIST = as.numeric(as.character(perucoord$IDDIST))
perucoord$IDPROV = as.numeric(as.character(perucoord$IDPROV))
perucoord$IDDPTO = as.numeric(as.character(perucoord$IDDPTO))
depcoord$IDDPTO = as.numeric(as.character(depcoord$IDDPTO))
provcoord$IDPROV = as.numeric(as.character(provcoord$IDPROV))

##############
# ui1 = ui
# ui1[is.na(ui1)] = 0
# ssnocord = unique(sort(ui1[(ui %in% perucoord$IDDIST) == F & round(ui1%%100) > 0]))

ss = cbind(idepa, iprov, idist, strata)
ss = data.frame(ss)
ss$IDDIST = idepa*10000 + iprov*100 + idist
ss$IDPROV = idepa*100 + iprov
ss$lat = NA
ss$long = NA
ssdi = merge(ss, perucoord[,c("IDDIST", "x_di", "y_di", "area_di")], by = "IDDIST", all.x = T)
ssdi = merge(ssdi, provcoord, by = "IDPROV", all.x = T)
ssdi = merge(ssdi, depcoord, by.x = "idepa", by.y = "IDDPTO", all.x = T)
ssdi[is.na(ssdi$x_di), c("x_di", "y_di")] = ssdi[is.na(ssdi$x_di), c("x_p", "y_p")]
ssdi[is.na(ssdi$x_di), c("x_di", "y_di")] = ssdi[is.na(ssdi$x_di), c("x_de", "y_de")]

sscord = ssdi[,c("strata", "idepa", "iprov", "idist", "IDDIST", "IDPROV", "x_di", "y_di", "area_di", "area_p", "area_de")]
sscombo = #dplyr::count(sscord)#
unique(sscord)#[,1:5])
sscombo$area_p = as.numeric(sscombo$area_p)
sscombo$area_di = as.numeric(sscombo$area_di)
sscombo$area_de = as.numeric(sscombo$area_de)
sscombo[order(sscombo$strata, sscombo$idepa),]
sscombo[order(sscombo$IDPROV, sscombo$idepa),]
sscombo[order(sscombo$IDPROV, sscombo$idepa),]

cordarea = matrix(NA, ncol = 3, nrow = 58)
colnames(cordarea) = c("long", "lat", "area")
for(strataa in 1:58){
  #print(c(strataa, length(unique(idist[strata == strataa])), length(unique(iprov[strata == strataa])),
  #        length(unique(idepa[strata == strataa])), length(unique(strata[iprov %in% unique(iprov[strata == strataa])])),
  #         length(unique(strata[idepa %in% unique(idepa[strata == strataa])]))))
  strataset = sscombo[sscombo$strata == strataa,]

  if(length(unique(sscombo[sscombo$idepa %in% strataset$idepa,]$strata)) == 1) {print(c(strataa, 1))
    cordarea[strataa, 1:2] = colMeans(depcoord[depcoord$IDDPTO %in% strataset$idepa, c("x_de", "y_de")])
    cordarea[strataa, 3] = sum(depcoord[depcoord$IDDPTO %in% strataset$idepa, c("area_de")])
  }else if (length(unique(sscombo[sscombo$IDPROV %in% strataset$IDPROV,]$strata)) == 1) {print(c(strataa, 2))
    #print(strataset[order(strataset$IDPROV),])
    cordarea[strataa, 1:2] = colMeans(provcoord[provcoord$IDPROV %in% strataset$IDPROV, c("x_p", "y_p")])
    cordarea[strataa, 3] = sum(provcoord[provcoord$IDPROV %in% strataset$IDPROV, c("area_p")])
  }else{print(c(strataa, 3))
    #print(strataset[order(strataset$IDDIST),])
    cordarea[strataa, 1:2] = colMeans(perucoord[perucoord$IDDIST %in% strataset$IDDIST, c("x_di", "y_di")])
    cordarea[strataa, 3] = sum(perucoord[perucoord$IDDIST %in% strataset$IDDIST, c("area_di")])
  }
}
cordarea = as.data.frame(cordarea)
cordarea$strata = 1:58
depcoord$area_de = as.numeric(depcoord$area_de)
rm(list = c("peru", "peru_ll", "coordfile", "ss", "ssdi", "sscombo"))

