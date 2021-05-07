library(survival)
library(dplyr)
library(readxl)
mydata <- read_excel("H:/Kampus/TUGAS/KULIAH/Tk. 3 Sem 6/AKH/Data_kematian_balita_1.xlsx")

#deskriptif
#deskriptif
summary(mydata$`Ijazah tertinggi yang dimiliki Ibu`)
summary(mydata$`Kegiatan ibu seminggu lalu`)
summary(mydata$`Usia ibu saat persalinan pertama (tahun)`)
summary(mydata$`Tipe kelahiran`)
summary(mydata$`Jenis kelamin balita`)
summary(mydata$`Status kematian balita`)
summary(mydata$`Sumber air minum`)
summary(mydata$sanitasi)
summary(mydata$`Interval kelahiran (bulan)`)
summary(mydata$`Usia balita (bulan)`)


#Define variable
x1<-mydata$`Ijazah tertinggi yang dimiliki Ibu`
x1<-recode_factor(x1, '1'="tidak punya ijazah SD", '2'="SD/MI/sederajat", '3'="SMP/MTs/sederajat", '4'="SMA/MA/sederajat", '5'="SMK", '6'="Diploma I/II", '7'="Diploma III/Sarjana Muda",'8'="Diploma IV/S1", '9'="S2/S3")
x2<-mydata$`Kegiatan ibu seminggu lalu`
x2<-recode_factor(x2,'4'="lainnya", '1'="bekerja",'2'="sekolah",'3'="mengurus rumah tangga")
x3<-mydata$`Usia ibu saat persalinan pertama (tahun)`
x4<-mydata$`Tipe kelahiran`
x4<-recode_factor(x4, '1'="tunggal",'2'="kembar")
x5<-mydata$`Jenis kelamin balita`
x5<-recode_factor(x5, '1'="laki-laki",'2'="perempuan")
x6<-mydata$`Status kematian balita`

x7<-mydata$`Sumber air minum`
x7<-recode_factor(x7,'12'="lainnya", '1'="air kemasan", '2'="air isi ulang", '3'="Sleding sampai rumah", '4'="leding eceran", '5'="pompa", '6'="sumur terlindung", '7'="sumur tak terlindung",'8'="mata air terlindung", '9'="mata air tak terlindung",'10'="air sungai",'11'="air hujan")
x8<-mydata$sanitasi
x8<-recode_factor(x8,'4'="tidak ada", '1'="jamban sendiri",'2'="jamban bersama",'3'="jamban umum")
x9<-mydata$`Interval kelahiran (bulan)`
x10<-mydata$`Usia balita (bulan)`


#Pembentukan model dan elimasi backward
#Semua variabel
weibull.far=survreg(Surv(x10, x6) ~ x1+x2+x3+x4+x5
                    +x7+x8+x9,dist = "weibull")
summary(weibull.far)
AIC1<-AIC(weibull.far)
#Model lengkap Weibull 
library(SurvRegCensCov)
ConvertWeibull(weibull.far,conf.level = 0.95)


#eliminasi x7
weibull.far=survreg(Surv(x10, x6) ~ x1+x2+x3+x4+x5
                    +x8+x9,dist = "weibull")
summary(weibull.far)
AIC2<-AIC(weibull.far)

#eliminasi x5
weibull.far=survreg(Surv(x10, x6) ~ x1+x2+x3+x4
                    +x8+x9,dist = "weibull")
summary(weibull.far)
AIC3<-AIC(weibull.far)

#eliminasi x8
weibull.far=survreg(Surv(x10, x6) ~ x1+x2+x3+x4
                    +x9,dist = "weibull")
summary(weibull.far)
AIC4<-AIC(weibull.far)

#eliminasi x4 (model 1)
weibull.far=survreg(Surv(x10, x6) ~ x1+x2+x3
                    +x9,dist = "weibull")
summary(weibull.far)
AIC5<-AIC(weibull.far)
#AIC terkecil
#Model 1 Weibull 
library(SurvRegCensCov)
ConvertWeibull(weibull.far,conf.level = 0.95)

#model 1
weibull.far.alt<-WeibullReg(
  Surv(x10, x6) ~ x1+x2+x3,data = mydata,conf.level = 0.95)
weibull.far.alt

#eliminasi x9 (model 2)
weibull.far=survreg(Surv(x10, x6) ~ x1+x2+x3
                    ,dist = "weibull")
summary(weibull.far)
AIC6<-AIC(weibull.far)
#Signifikan x1 x2 x3

#Model 2 Weibull 
library(SurvRegCensCov)
ConvertWeibull(weibull.far,conf.level = 0.95)
#model 2
weibull.far.alt<-WeibullReg(
  Surv(x10, x6) ~ x1+x2+x3,data = mydata,conf.level = 0.95)
weibull.far.alt


nilai<-c(AIC1,AIC2,AIC3,AIC4,AIC5,AIC6)
modeli<-c("AIC1","AIC2","AIC3","AIC4","AIC5","AIC6")
hasil<-data.frame(modeli,nilai)
hasil


kmsurvival<-survfit(Surv(x10,x6)~x4)
summary(kmsurvival)
plot(kmsurvival,xlab = "Time",ylab = "Survival Pobability")

#Adequacy Weibull Model
WeibullDiag(Surv(x10,x6)~x2)

#Perbandingan Parametrik vs Non Par
library(eha)
phreg.mydata<-phreg(Surv(x10, x6) ~ x1+x2+x3,
                    data = mydata,dist = "weibull")
coxreg.mydata<-coxreg(Surv(x10, x6) ~ x1+x2+x3,
                      data = mydata)
check.dist(coxreg.mydata,phreg.mydata)

#sumber https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5233524/
