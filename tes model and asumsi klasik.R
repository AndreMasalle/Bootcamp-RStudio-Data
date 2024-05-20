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

##pooled
pooled <-plm(model, paneldata, model = "pooling")
summary(pooled)

##fixed effects 
fixed <-plm(model,paneldata,model="within", effect = "individual")
summary(fixed)

##random effects
random<-plm(model, paneldata, model = "random", effect="individual")
summary(random)


##chow test : choose between pooled vs fixed
chow_panel<-pFtest(fixed,pooled)
chow_panel
##hausman test:choose between random vs fixed
hausman_panel<-phtest(fixed,random)
hausman_panel
##breuschpagan test : choose between pooled vs rm,andom
bp_panel<-plmtest(pooled, type=c("bp"))
bp_panel

check_collinearity(pooled)
residpooled<-pooled$residual
shapiro.test(residpooled)
nortest::ad.test(residpooled)
check_autocorrelation(pooled) 
check_heteroscedasticity(pooled)

# model gls
gls <- pggls(NP~UK+K_IND+K_AUD+lag(NP), data=paneldata, model="pooling")
summary(gls)
residgls<-gls$residual
shapiro.test(residgls)  

#pcse
ols <- lm(model, panel)
pcse <- pcse(ols,groupN = panel$No, groupT = panel$Tahun)
summary(pcse)