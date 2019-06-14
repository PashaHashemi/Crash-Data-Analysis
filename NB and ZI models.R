#-------------------------------------------------------------0.1
# the purpose of this script is to develope negative binomial regression model and zero-inflated model
rm(list=(ls()))

library(effects);library(margins);library(dplyr);library(MASS);library(pscl);library(AER);library(leaps);library(pastecs);library(ggplot2);library(lme4); prt = TRUE

setwd("C:/Users/Pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/NegResults")
startTime = proc.time()

mydata <- read.csv("C:/Users/pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/1-Combo/0.1_Int_All.csv")
mydata["Length"]<- 0.1
mydata["Year"]<- 10
mydata<- na.omit(mydata)
mydata$Guardrails.Length <- mydata$Guardrails.Length / 0.1
mydata$PAINTED.Length <- mydata$PAINTED.Length / 0.1
mydata$ShoulderType <- mydata$ShoulderType / 0.1


mydata$Bridge.Indicator[which(mydata$Bridge.Indicator>1)]<-1

mydata["PercentageTruck"]= (mydata$AADT_SigleTruck+mydata$AADT_ComTruck)*100/(mydata$AADT)
colnames(mydata)[33] <- paste("PofLengthOfGuardrail")
colnames(mydata)[36] <- paste("PofLengthOfMedian")
colnames(mydata)[16] <- paste("PaintedMedianWidth")
colnames(mydata)[16] <- paste("PaintedMedianWidth")
colnames(mydata)[32] <- paste("Mean_FrictionDemand")
colnames(mydata)[39] <- paste("PofLengthOfACShoulderType")
mydata<- mydata[,-(c(24,25,27,28,30,31,32,35,37,38))]

# mydata["Abs_IRIChanges"]= abs(mydata$IRI-mydata$IRI_ENV_Ave)
# mydata["Abs_CurveChanges"]= abs(mydata$Abs_Curvature-mydata$Curve_ENV_Ave)
# mydata["Abs_GradeChanges"]= abs(mydata$Grade-mydata$Grade_ENV_Ave)


StatData <- stat.desc(mydata[,-(c(1,2,3,4,6,8,10,11,17,20,21,22,35,37,38))],basic=TRUE, desc=TRUE, norm=FALSE)
write.csv(StatData,
          file="C:/Users/Pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/NegResults/0.1_VaribaleSummary_4.csv")
write.csv(cor(mydata[,-(c(1,2,3,4,5,6,8,10,11,17,20,21,22,35,37,38,39,40))]),
          file="C:/Users/Pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/NegResults/0.1_VaribaleCorrelation_4.csv")


null=glm.nb(SegmentCrashFreq~1, data=mydata)
summary(null)
full=glm.nb(SegmentCrashFreq ~ offset(log(Length))+offset(log(Year))+offset(log(AADT))+Curve_SD+
              log(AADT)+ Abs_Curvature+PercentageTruck+Grade+LaneWidth+PaintedMedianWidth+
              Rutting+shoulder_Width+Grade_SD+(PofLengthOfGuardrail)+Bridge.Indicator+(PofLengthOfMedian)
            ,data=mydata)
summary(full)
m1<-step(null, scope = list(upper=full), data=mydata, direction="both")
summary(m1)


m2 <- update(m1, . ~ . -LaneWidth )

summary(m2)


m3 <- update(m2, . ~ . +offset(log(Length))+offset(log(Year))+offset(log(AADT)))
summary(m3)


if (prt == TRUE){
  out <- capture.output(summary(m3,correlation = TRUE),
                        file="C:/Users/Pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/NegResults/0.1_Update_All_Sumy.txt")
  write.csv((summary(m3)$coefficients),
            file="C:/Users/Pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/NegResults/0.1_Update_All_Coef.csv")
  write.csv(cbind(mydata[,1:6],fitted=m3$fitted.values,residual=m3$residuals), file="C:/Users/Pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/NegResults/0.1_Update_fittedValue.csv")
  
}



nullZ=zeroinfl(SegmentCrashFreq~1, data=mydata, link = "logit", dist = "negbin")
fullZ=zeroinfl(SegmentCrashFreq ~ offset(log(Length))+offset(log(Year))+offset(log(AADT))+
                 log(AADT)+ Abs_Curvature+I(Abs_Curvature^2)+PercentageTruck+Grade+I(Grade^2)+IRI+LaneWidth+PaintedMedianWidth+
                 Rutting+shoulder_Width+Curve_ENV_SD+Curve_ENV_Ave+Grade_SD+Grade_ENV_SD+
                 Grade_ENV_Ave+IRI_SD+IRI_ENV_SD+IRI_ENV_Ave+Mean_FrictionDemand+(PofLengthOfGuardrail)+Bridge.Indicator+(PofLengthOfMedian),data=mydata, link = "logit", dist = "negbin")
summary(fullZ)
m1Z<-step(nullZ, scope = list(upper=fullZ), data=mydata, direction="both")
summary(m1Z)

out <- capture.output(summary(m1Z,correlation = TRUE),
                      file="C:/Users/Pasha/Dropbox/Ph.D Research/road data/TRB all material/Modeling Resutls/NegResults/0.1_Zero_All_Sumy.txt")


save.image(file='myEnvironment0.1.RData')