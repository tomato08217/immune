setwd("D:/NJPLA/杨斌/immune")
data<-read.csv("sp_reduced.csv",sep=",",header=T)#
for (i in c(11,13:39)){
  data[,i] <- as.factor(data[,i])
}
str(data)
library(tableone)
data$Age<-as.numeric(data$Age)
train <- subset(data,Train2==1)
test <- subset(data,Train2==0)

#ROC
library(pROC)
roc1<-roc(train$label1,train$riskscore2,ci=TRUE, of="auc")
roc1$ci #CI
plot(roc1, print.auc=TRUE,print.thres=TRUE,col="blue")
roc2<-roc(test$label1,test$riskscore2,ci=TRUE)
roc2$ci #CI
plot(roc2, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE)
#叠加
plot(roc2,print.auc=TRUE,print.thres=TRUE,col="red") #叠加

roc.test(roc1, roc2, reuse.auc=FALSE)
summary(fit)#??????????????????
exp(coef(fit))#??????OR
exp(confint(fit))#??????95%CI OR
confint(fit)#??????95%Coef


#画confusionMatrix

predicted.classes.train <- ifelse(train$riskscore2 > 0.689, "1", "0")
predicted.classes.test <- ifelse(test$riskscore2 > 0.60, "1", "0")

library(caret)
confusionMatrix(table(train$label1, predicted.classes.train)) 
confusionMatrix(table(test$label1, predicted.classes.test)) 

#clinical

library(tableone)
fvars<-c("Gender","Smoking.status","Family.history","TTF.1","Ki.67","Histologic.type",	"Stage","T","N","M","Lymph.node.metastasis","Intrapulmonary.metastasis","Brain.metastasis","Liver.metastasis","Bone.metastasis","Adrenal.metastasis","Pleural.metastasis")
vars<-c("Gender","Age","Smoking.status","Family.history","TTF.1","Ki.67","Histologic.type",	"Stage","T","N","M","Lymph.node.metastasis","Intrapulmonary.metastasis","Brain.metastasis","Liver.metastasis","Bone.metastasis","Adrenal.metastasis","Pleural.metastasis","W","Neutrophil.","Monocyte.","C.reactive.protein","CEA","NSE")
nonnormal<-c("W","Neutrophil.","Monocyte.","C.reactive.protein","CEA","NSE")
tableone<-CreateTableOne(vars=vars,strata=c("Train2"),data=data)
table1<-print(tableone,cramVars=fVars,nonnormal=nonnormal,showAllLevels=T)
write.csv(table1,file="table1.csv")
#uni logistic
result1<-c()
for (i in 11:39) {
fit<-glm(label1~data[,i],data=data,family="binomial")
result1<-rbind(result1,c(colnames(data)[i],coef(summary(fit))[2,c(1,2,4)]))}

fit<-glm(label1~as.factor(Stage),data=data,family="binomial")
summary(fit)
exp(coef(fit))#??????OR
exp(confint(fit))

combine<-glm(label1~riskscore2+Age + Histologic.type +Stage+ N + M + Intrapulmonary.metastasis+Stage+Platelets..10.9.,family="binomial",train)

library(MASS)
combine1<-stepAIC(combine)
summary(combine1)
exp(coef(combine1))#??????OR
exp(confint(combine1))

library(rJava)
library(glmulti)
glmulti(label1~riskscore2+Age + Histologic.type +Stage+ N + M + Intrapulmonary.metastasis+Stage+Platelets..10.9., data = train,
        level = 1, 
        method = "h", 
        crit = "aic", 
        confsetsize = 5, 
        plotty = F, report = F, 
        fitfunction = "glm", 
        family = binomial) 


library(pROC)
pred.combine.train<- predict(combine1, as.data.frame(train), type="response") 
pred.combine.test<- predict(combine1, as.data.frame(test), type="response")
roc5<-roc(train$label1,pred.combine.train,ci=TRUE, of="auc")
roc6<-roc(test$label1,pred.combine.test,ci=TRUE, of="auc")
plot(roc5,print.thres=F,print.auc=F,plot=TRUE,col="black")
plot(roc6,add=T,print.thres=F,print.auc=F,plot=TRUE,col="grey")

#confusion matrix

predicted.classes.train.combine <- ifelse(pred.combine.train > 0.547, "1", "0")
predicted.classes.test.combine <- ifelse(pred.combine.test > 0.397, "1", "0")

#画confusionMatrix
library(caret)
confusionMatrix(table(train$label1, predicted.classes.train.combine)) 
confusionMatrix(table(test$label1, predicted.classes.test.combine)) 

#calibration curve in Python

#nomogram
#Nomogram,lrm must
fit1<-lrm(label1~riskscore2+M+N+Age,train)
library(rms)
dd<-datadist(train[,6:126])
options(datadist="dd")  
nom <- nomogram(fit1, fun=plogis,fun.at=seq(0.1,1,by=0.1),lp=F, funlabel="Risk")
plot(nom)
#dca
library(devtools) 
library(rmda)
rmodelforDCA_train<-decision_curve(label1~riskscore,family="binomial",data=train,thresholds = seq(0,1, by = .01),bootstraps = 10)
combinemodelforDCA_train<-decision_curve(label1~riskscore+M+N+Age,,family="binomial",data=train,thresholds = seq(0,1, by = .01),bootstraps = 10)
rmodelforDCA_test<-decision_curve(label1~riskscore,family="binomial",data=test,thresholds = seq(0,1, by = .01),bootstraps = 10)
combinemodelforDCA_test<-decision_curve(label1~riskscore+M+N+Age,family="binomial",data=test,thresholds = seq(0,1, by = .01),bootstraps = 10)

plot_decision_curve( list(rmodelforDCA_train,combinemodelforDCA_train), curve.names = c("rmodel", "combinemodel"),col = c("#F8766D", "#00BFC4"), cost.benefit.axis = FALSE,confidence.intervals =FALSE,legend.position = "none")
legend("bottomright", inset=.05, c("none","all","radiomics model","clinical plus radiomics model"),lty=c(1),cex=0.3, col=c("black","grey","#F8766D", "#00BFC4"))
plot_decision_curve( list(rmodelforDCA_test,combinemodelforDCA_test), curve.names = c("rmodel", "combinemodel"),col = c("#AD0000", "#85FFA5"), cost.benefit.axis = FALSE,confidence.intervals =FALSE,legend.position = "none")
legend("bottomright", inset=.05, c("none","all","radiomics model","clinical plus radiomics model"),lty=c(1),cex=0.3, col=c("black","grey","#AD0000", "#85FFA5"))


#survival
library(survival)
y_train<- Surv(train$PFS,train$label.2.status) 
y_test<- Surv(test$PFS,test$label.2.status) 
y_data<- Surv(data$PFS,data$label.2.status) 
survival_rad<-coxph(y_train~riskscore2,train)
summary(survival_rad)
method<- survConcordance(Surv(test$PFS,test$label.2.status) ~ predict(survival_rad, test))
cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
survival_combine<-coxph(y_train~riskscore2+Intrapulmonary.metastasis+Adrenal.metastasis+C.reactive.protein+M,train)
library(MASS)
survival_combine1<-stepAIC(survival_combine)
summary(survival_combine1)
method<- survConcordance(Surv(test$PFS,test$label.2.status) ~ predict(survival_combine, test))
cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
#nomo
library(rms)
dd<-datadist(train[,6:126])
options(datadist="dd")
#build with cph
nomo<-cph(y_train~riskscore2+M+C.reactive.protein,x=T,y=T,data=train,surv=T,time.inc=24)#define predicion time point
surv<- Survival(nomo)
surv1<-function(x)surv(20,lp=x)
nom<-nomogram(nomo, fun=surv1,funlabel=c("2 year Survival Probability"),lp=F,fun.at=c('0.90','0.70','0.5','0.3','0.1'),maxscale=10)
plot(nom,xfrac = 0.6)
#decision curve
library(devtools) 
library(rmda)
rmodelforDCA<-decision_curve(label.2.status~riskscore,data =train,thresholds = seq(0,1, by = .01),bootstraps = 10)
combinemodelforDCA<-decision_curve(label.2.status~riskscore+M+N+Age,data=train,thresholds = seq(0,1, by = .01),bootstraps = 10)
plot_decision_curve( list(rmodelforDCA,combinemodelforDCA), curve.names = c("cmodel","crmodel"),col = c("blue","black"), cost.benefit.axis = FALSE,confidence.intervals =FALSE,legend.position = "none")
legend("topright", inset=.05, c("none","all","radiomics model","clinical plus radiomics model"),lty=c(1),cex=0.3, col=c("black","grey","blue", "black"))
rmodelforDCA<-decision_curve(label.2.status~riskscore,data =test,thresholds = seq(0,1, by = .01),bootstraps = 10)
combinemodelforDCA<-decision_curve(label.2.status~riskscore+M+N+Age,data=test,thresholds = seq(0,1, by = .01),bootstraps = 10)
plot_decision_curve( list(rmodelforDCA,combinemodelforDCA), curve.names = c("cmodel","crmodel"),col = c("blue","black"), cost.benefit.axis = FALSE,confidence.intervals =FALSE,legend.position = "none")
legend("topright", inset=.05, c("none","all","radiomics model","clinical plus radiomics model"),lty=c(1),cex=0.3, col=c("black","grey","blue", "black"))

#KM
library(survival)
library(survminer)
res.cut <- surv_cutpoint(train, time = "PFS", event = "label.2.status",variables = c("riskscore2","C.reactive.protein","newscore"))
res.cat <- surv_categorize(res.cut)
risk_score_fitforKM <- survfit(Surv(PFS,label.2.status) ~riskscore2, data = res.cat)
ggsurvplot(risk_score_fitforKM , data = res.cat, risk.table = TRUE, pval=T,conf.int = F,xlab='Time in Months',legend = "bottom",legend.title = "risk",legend.labs = c("rad score high risk","rad score low risk"))
CRP_fitforKM <- survfit(Surv(PFS,label.2.status) ~riskscore, data = res.cat)
ggsurvplot(CRP_fitforKM , data = res.cat, risk.table = TRUE, pval=T,conf.int = F,xlab='Time in Months',legend = "bottom",legend.title = "risk",legend.labs = c("CRP high risk","CRP low risk"))
M_fitforKM <- survfit(Surv(PFS,label.2.status) ~M, data = data)
ggsurvplot(M_fitforKM , data = data, risk.table = TRUE, pval=T,conf.int = F,xlab='Time in Months',legend = "bottom",legend.title = "risk",legend.labs = c("M0","M1"))
new_score_fitforKM <- survfit(Surv(PFS,label.2.status) ~newscore, data = res.cat)
ggsurvplot(new_score_fitforKM , data = res.cat, risk.table = TRUE, pval=T,conf.int = F,xlab='Time in Months',legend = "bottom",legend.title = "risk",legend.labs = c("new score high risk","new score low risk"))

#calibration curve
library(rms)
Rmodel<-cph(y_data~riskscore,data,x=T,y=T,surv=TRUE,time.inc=10)
cal<- calibrate(Rmodel,cmethod='KM', method='boot', u=10, m=14,B=50)
plot(cal,lwd = 2,lty = 1,errbar.col = c("blue"),xlim = c(0,1),ylim= c(0,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("blue"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)
lines(cal[,c('mean.predicted',"KM")],type= 'b',lwd = 2,col = c("blue"),pch = 16)

CRmodel<-cph(y_data~riskscore+M+C.reactive.protein,data,x=T,y=T,surv=TRUE,time.inc=18)
cal<- calibrate(CRmodel,cmethod='KM', method='boot', u=18, m=16,B=50)
plot(cal,lwd = 2,lty = 1,errbar.col = c("blue"),xlim = c(0,1),ylim= c(0,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("blue"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)
lines(cal[,c('mean.predicted',"KM")],type= 'b',lwd = 2,col = c("blue"),pch = 16)

#integrated brier score
dd=datadist(train[,10:38])
options(datadist="dd")
Models <- list("CRmodel"= coxph(Surv(PFS,label.2.status)~riskscore+M+C.reactive.protein, data=train,x=TRUE,y=TRUE),
 "Rmodel" = coxph(Surv(PFS,label.2.status)~ riskscore, data=train,x=TRUE,y=TRUE))
p <- pec(object = Models,cens.model = "cox", data=train, splitMethod="Boot632plus", B=100,reference = FALSE)
print(p)
par(mai=c(1,1,1,1))
plot(p,type="l",smooth=TRUE,legend = FALSE,xlim=c(0,24),axis1.at=seq(0,24,2), xlab="Survival weeks", ylab="Prediction error",col = c("red", "blue","black"),lwd = c(3,3,3),lty = c(1,1,1))

library(survcomp)
cindex_cr <- concordance.index(predict(survival_rad),surv.time = train$PFS,  surv.event = train$label.2.status,method = "noether") 
cindex_r<-concordance.index(predict(survival_combine1),surv.time = train$PFS,  surv.event = train$label.2.status,method = "noether") 

cindex.comp(cindex_cr, cindex_r)

library(nomogramFormula)
results <- formula_rd(nomo)
points_train<- points_cal(formula = results$formula,rd=train)
points_test<- points_cal(formula = results$formula,rd=test)
points2<-prob_cal(reg = CRmodel,times = c(20))
