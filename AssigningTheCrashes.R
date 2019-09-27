# This script is desinged to automatically assing the crashes to the different roadway segments
rm(list = ls())

dd =read.csv("C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/Check_Data_Arpril17/Output/F_D_All_A_CL_P.csv")
#dd =subset(dd, dd$NewSeverity >= 2)
roadsectiondata =read.csv("C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/roadsectiondata.csv")

setwd("C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/Check_Data_Arpril17/Output/Charts/ALLNEW")
check = FALSE

# In this script , you need to identify a vector including the desired segment lengths
# This program will generate histograms as well.

SegLenght2 <- c(0.1,0.2,0.3,0.5,1,2)  # a vector of different segement lengths

n <- length(SegLenght2)
dataset <- NA
for (j in 1:n){
  SegLenght <- SegLenght2[j]
  roadsectiondata =read.csv("C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/roadsectiondata.csv")
  for (i in 1: nrow(roadsectiondata)){
    crashdata=subset(dd, PRIM.ROUTE.NO.27.==roadsectiondata[i,1])
    roadsectiondata[i,3]<- roadsectiondata[i,3]-((roadsectiondata[i,3]-roadsectiondata[i,2]) %% SegLenght)

    BMP <-seq(from=roadsectiondata[i,2],to=roadsectiondata[i,3]-SegLenght,SegLenght)
    EMP <-seq(from=(roadsectiondata[i,2]+SegLenght) ,to= (roadsectiondata[i,3]),SegLenght)
    Route <- rep(roadsectiondata[i,1],length(BMP))
    SegmentCrashFreq <- rep(0,length(BMP))

    d=subset(dd, PRIM.ROUTE.NO.27.==roadsectiondata[i,1])
    ncrashes<-length((d$REPORT.NO))

    for (m in 1:nrow(crashdata)) {
      for (n in 1:length(BMP)) {
        if ( (BMP[n] <=d$MEAS[m]) & (d$MEAS[m] <= EMP[n]) ) {
          SegmentCrashFreq[n]=SegmentCrashFreq[n]+1
          break
        }
      }
    }

    dataset = rbind(dataset,data.frame(BMP,EMP,SegmentCrashFreq,Route ))

    #assign (paste(SegLenght,"_",roadsectiondata[i,4],"_CrashN",sep=""), data.frame(BMP,EMP,SegmentCrashFreq,Route ))
    #write.csv( assign (paste(SegLenght,"_",roadsectiondata[i,4],"_CrashN",sep=""), data.frame(BMP,EMP,SegmentCrashFreq,Route )),file=paste(SegLenght,"_",roadsectiondata[i,4],"_CrashN.csv",sep=""),row.names = F)
  }
  dataset <- dataset[-1,]
  dataset <- cbind ( Seg=c(1:nrow(dataset)),dataset )

  write.csv( dataset,file=paste(SegLenght,"_","_Crash_P.csv",sep=""),row.names = FALSE)


  k=max(dataset$SegmentCrashFreq)

  CrashFreq <- rep(0,k+1)

  for (j in 1:nrow(dataset)){
    for (i in 1:(k+1)){
      if (dataset$SegmentCrashFreq[j] == (i-1)) {
        CrashFreq[i]= CrashFreq[i]+1
        break
      }
    }
  }
CrashFreqP=CrashFreq[1:15]


#rm(list=setdiff(ls(), "CrashFreqP"))

dd =read.csv("C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/Check_Data_Arpril17/Output/F_D_All_A_CL_N.csv")
# dd =subset(dd, dd$NewSeverity >= 2)
roadsectiondata =read.csv("C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/roadsectiondata.csv")


check = FALSE

dataset <- 0

  #SegLenght <- SegLenght2[j]
  roadsectiondata =read.csv("C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/roadsectiondata.csv")
  for (i in 1: nrow(roadsectiondata)){
    crashdata=subset(dd, PRIM.ROUTE.NO.27.==roadsectiondata[i,1])
    roadsectiondata[i,3]<- roadsectiondata[i,3]-((roadsectiondata[i,3]-roadsectiondata[i,2]) %% SegLenght)

    BMP <-seq(from=roadsectiondata[i,2],to=roadsectiondata[i,3]-SegLenght,SegLenght)
    EMP <-seq(from=(roadsectiondata[i,2]+SegLenght) ,to= (roadsectiondata[i,3]),SegLenght)
    Route <- rep(roadsectiondata[i,1],length(BMP))
    SegmentCrashFreq <- rep(0,length(BMP))

    d=subset(dd, PRIM.ROUTE.NO.27.==roadsectiondata[i,1])
    ncrashes<-length((d$REPORT.NO))

    for (m in 1:nrow(crashdata)) {
      for (n in 1:length(BMP)) {
        if ( (BMP[n] <=d$MEAS[m]) & (d$MEAS[m] <= EMP[n]) ) {
          SegmentCrashFreq[n]=SegmentCrashFreq[n]+1
          break
        }
      }
    }

    dataset = rbind(dataset,data.frame(BMP,EMP,SegmentCrashFreq,Route ))

    #assign (paste(SegLenght,"_",roadsectiondata[i,4],"_CrashN",sep=""), data.frame(BMP,EMP,SegmentCrashFreq,Route ))
    #write.csv( assign (paste(SegLenght,"_",roadsectiondata[i,4],"_CrashN",sep=""), data.frame(BMP,EMP,SegmentCrashFreq,Route )),file=paste(SegLenght,"_",roadsectiondata[i,4],"_CrashN.csv",sep=""),row.names = F)
  }
  dataset <- dataset[-1,]
  dataset <- cbind ( Seg=c(1:nrow(dataset)),dataset )

  write.csv( dataset,file=paste(SegLenght,"_","_Crash_N.csv",sep=""),row.names = FALSE)


  k=max(dataset$SegmentCrashFreq)

  CrashFreq <- rep(0,k+1)

  for (j in 1:nrow(dataset)){
    for (i in 1:(k+1)){
      if (dataset$SegmentCrashFreq[j] == (i-1)) {
        CrashFreq[i]= CrashFreq[i]+1
        break
      }
    }
  }

  CrashFreqN=CrashFreq[1:15]


CrashFreq= CrashFreqP+CrashFreqN


PercentageOfZero=CrashFreq[1]*100/sum(CrashFreq)
PercentageOfOne=CrashFreq[2]*100/sum(CrashFreq)

CrashFreq=CrashFreq[1:15]
Route <- "All Routes P"
names <- c (0:(length(CrashFreq)-1)) #anyone is fine
jpeg(paste(Route,SegLenght,".jpeg"))
barplot(CrashFreq/sum(CrashFreq), width =.02, ylim = c(0,1),names.arg=as.character(names), xlab = "The total number of observed crashes",
        ylab = "Proportions to the total number of segments", space = .2, main=paste("\n\n\n\nSegment Lenght=",SegLenght,"miles","\nPercentage of Zeros=",round(PercentageOfZero,2),"%" ,"\nPercentage of Ones=",round(PercentageOfOne,2),"%"),cex.main=1.5 ,cex.names = 1.5 ,cex.axis=1.5, cex.lab = 1.5  )
dev.off()
dataset <- 0
}
