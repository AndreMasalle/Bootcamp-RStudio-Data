library(MVN)
library(MASS)

titan<-Data_penugasan_pertemuan_4_Titanic

head(titan)
cek<- mvn(titan[,-1], mvnTest = 'royston',  alpha = 0.05)
cek

fit <- lda(survived~RoundAge+pclass+sex, data=titan,na.action="na.omit")
fit

fit <- lda(survived~RoundAge+pclass+sex, data=titan,na.action="na.omit",CV=T)
ct <- table(titan$survived, fit$class)
ct
diag(prop.table(ct, 1))
sum(diag(prop.table(ct)))
