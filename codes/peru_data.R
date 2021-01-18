load("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/IntermuestraV1-2.RData")
ubigeo = read.table("C:/Users/manja/Dropbox/capture_recapture/data/Peru_killings/ubigeo2002.txt", header = TRUE, sep = "\t")

ubigeo2 = ubigeo
colnames(ubigeo2) = c("idepa", "iprov", "idist", "lugar")

geni = ubigeo2$idepa*10000 + ubigeo2$iprov*100 + ubigeo2$idist

perpe = rep(3, nrow(x))
perpe[x$Agente %in% c("EST", "PAR", "RON")] = 1
perpe[x$Agente == "SLU"] = 2

perpe1 = perpe
perpe1[is.na(x$Agente)] = 4
perpe1[x$Agente %in% c("NOD", "ENF", "EMR", "ESL")] = 4

miss = which(perpe == 4)

age = x$AnoHechos - x$AnoNac
age[age < 0 | age > 110] = NA 

ui = x$UbiHechos/10000

ui = round(ui)

idepa = round(ui/10000)
iprov = round((ui - idepa*10000)/100)
idist = round(ui - idepa*10000 - iprov*100)

source = 4*x$ODH + 2*x$DP + x$CVR + 1

sl = rep(0, nrow(x))
sl[perpe == 2] = 1

esta = rep(0, nrow(x))
esta[perpe == 1] = 1

source1 = rep(0, nrow(x))
source1[source == 2] = 1

ui2 = x$UbiHechos - ui*10000

otheri = rep(0, nrow(x))
otheri = (perpe1 == 3)

table(perpe, source)
table(otheri[perpe == 3], source[perpe == 3])

