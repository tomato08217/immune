library(Hmisc)
library(survival)
setwd("D:/杨斌/977/0609")
train<-read.csv("train_clinical.csv")
#read the data
source("GND_test.v2.r")
d<-train

#we decided we are interested in calibration at 36 mo and 60 mo,
#so censor after 36 and 60
#already done in excel


#calculate predicted probability at pre-specified time (adm.cens)
survcox_d<-coxph(data=train, Surv(PFS,label.2.status)~riskscore+M+C.reactive.protein)
summary(survcox_d)
survfit_d=survfit(survcox_d, newdata=d, se.fit=FALSE)

survpr24=survfit_d$surv[54,]
estsurv24=survpr24
estinc24=1-survpr24


#split into deciles
d$dec24=as.numeric(cut2(estinc24, g=3))


source("GND_test.v2.r")
#calculate the GND test
GND.result1=GND.calib(pred=estinc24, tvar=d$PFS, out=d$label.2.status, 
                     cens.t=24, groups=d$dec24, adm.cens=24)

GND.result1
