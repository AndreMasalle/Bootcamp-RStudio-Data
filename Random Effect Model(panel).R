library("plm")
library(performance)
library(pcse)
library(nortest)

panel<- Data_Penugasan_Pertemuan_3_Regresi_Panel

#deskriptif
summary(panel)
cor(panel[,-1]) #skip nama tabel

paneldata <-pdata.frame(panel, index=c("No", "Tahun"))
model<-NP~UK+K_IND+K_AUD 

#Random
random<-plm(model, paneldata, model = "random", effect="individual")
summary(random)