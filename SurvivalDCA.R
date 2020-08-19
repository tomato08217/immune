source("stdca.R")
library(survival)

Rmodel<- coxph(Surv(PFS,label.2.status,type = "right")~riskscore, data=train)
Combinemodel<- coxph(Surv(PFS,label.2.status,type = "right")~riskscore+M+C.reactive.protein, data=train)

train$Rfail= c(1- (summary(survfit(Rmodel, newdata=train), times=5)$surv))
train$CRfail = c(1- (summary(survfit(Combinemodel, newdata=train), times=5)$surv))

CR=stdca(data=train, outcome="label.2.status", ttoutcome="PFS",timepoint=5, predictors= c("CRfail"),smooth=TRUE)
R=stdca(data=train, outcome="label.2.status", ttoutcome="PFS", predictors= c("Rfail"),timepoint=5,smooth=TRUE)

plot(CR$net.benefit.threshold, CR$net.benefit.none, type = "l", lwd=2, xlim=c(0,1.0), ylim=c(-0.05,0.5), xlab = "Threshold Probability",ylab = "Net Benefit")
lines(CR$net.benefit$threshold, CR$net.benefit$all, type="l", col=8, lwd=0.5)
lines(CR$net.benefit$threshold, CR$net.benefit$none, type="l", col=8, lwd=0.5,lty=2)
lines(CR$net.benefit$threshold, CR$net.benefit$CRfail, type="l", col="red",lwd=2)
lines(R$net.benefit$threshold, R$net.benefit$Rfail, type="l", col = "blue",lwd=2)
legend("topright", inset=.05, c("All", "None","radiomics model","clinical plus radiomics model"),lty=c(1,2,1,1,1),cex=0.3, col=c("grey","grey","blue","red"),lwd=c(1,1,2,2),bty="o")
