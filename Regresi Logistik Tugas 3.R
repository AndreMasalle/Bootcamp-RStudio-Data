library(performance)
library(car)

titan<-Data_penugasan_pertemuan_4_Titanic

#dekriptif
table(titan$survived) #buat liat frekuensi
prop.table(table(titan$survived))*100 #liat presentasi logistik kondisi

summary(titan) #biasa lah masa gak tau

#pembentukan model
m<-glm(survived~RoundAge+pclass+sex,data = titan, family=binomial(link="logit")) 
# yang diatas tuh buat model dengan variabel dependennya tuh kondisi, 
#family=binomial(link="logit) tuh brarti dia pake regresi binomial trus tipenya logisitik 

summary(m) 

model.null<-glm(survived~1,data = titan, family=binomial(link="logit"))
anova(m,model.null,test="Chisq")

#cek asumsi
check_outliers(m) 
multicollinearity(m)
performance_hosmer(m, n_bins = 10) #Ngeliat kalo modelnya cocok atau gak

#cek performa
r2_nagelkerke(m) # Variabel yang diteliti bisa berpengaruh berapa persen diliat dari output ini
performance_accuracy(m) # akurasi modelnya bisa menebak sebasar berapa persen


