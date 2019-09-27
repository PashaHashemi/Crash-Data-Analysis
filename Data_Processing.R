
#  The purpose of this script is to prepare the data for the statistical anlaysis

rm(list = ls())
WWrite <- TRUE   # to enable write.csv
startTime = proc.time()
setwd("/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/Check_Data_Arpril17")
#----------------Getting Raw data
#--------------------all crashes-------------
##1-1)Readying the Raw Data an Combining them
for (i in 2005:2014) {
  filename = paste(i," data new.csv", sep = "")
  assign(paste("D", i,"A", sep =""), cbind(Year=i, read.csv(filename)))
}
for (i in 2005:2014) {
  filename = paste(i," data new2.csv", sep = "")
  assign(paste("D", i,"B", sep =""), cbind(Year=i, read.csv(filename)))
}

D_All_A = rbind(D2005A,D2006A,D2007A,D2008A,D2009A,D2010A,D2011A,D2012A,D2013A,D2014A)
Nobs=nrow(D_All_A); Numebr=c(1:Nobs) ; D_All_A= cbind(Number,D_All_A)

D_All_B = rbind(D2005B,D2006B,D2007B,D2008B,D2009B,D2010B,D2011B,D2012B,D2013B,D2014B)
Nobs=nrow(D_All_B); Numebr=c(1:Nobs) ; D_All_B= cbind(Number,D_All_B)
#write.csv( D_All_A, file="try.csv")
#write.csv( D_All_B, file="try.csv")
# rm(D2006A,D2007A,D2008A,D2009A,D2010A,D2011A,D2012A,D2013A)
# rm(D2006B,D2007B,D2008B,D2009B,D2010B,D2011B,D2012B,D2013B)
rm(list=setdiff(ls(), c("D_All_A","D_All_B","startTime","WWrite")))


D_All_A[c(5,6,10,11,12,18, c(25:28),44,45,47,48,53,c(80:119),c(121:169))] <- NULL #delet extra colomns
D_All_B[c(12,16,17,41,42,43,44)] <- NULL #delet extra colomns

D_All_A["MEAS"] <- NA
D_All_B["MEAS"] <- NA

if (WWrite==TRUE){
  write.csv( D_All_A, file="C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/Check_Data_Arpril17/Output/D_All_A.csv",row.names = F)
  write.csv( D_All_B, file="C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/Check_Data_Arpril17/Output/D_All_B.csv",row.names = F)
  
}
##----------------
#1) All crash
require(dplyr)
library(dplyr)
#-------------------------------------------------Filtering for crash level
D_All_A_CL <-subset(D_All_A, COUNTY.2. %in%   c('HAW','HON','KAU' ,'MAU'))  #58926 Crash
#-------------------------------------------------Filtering for Vehicle level
D_All_B_CL <-subset(D_All_B, !(UNIT.32. %in% NA))    #108779 vehicle
#-------------------------------------------------Filtering for Passanger level
D_All_C_CL <- D_All_B # 154533People
#------------------------------------------------------------
#2)Subset the RwD crashes (It starts with F)

# Filter for Intersection Related Crash
NoRow= (nrow(D_All_A_CL))
IntersectionFree=rep(0,NoRow)
for (i in 1:NoRow) {

  if ((is.na(D_All_A_CL$LOC.HARMFUL.EVENT.31A.))[i] ){D_All_A_CL$LOC.HARMFUL.EVENT.31A.[i]=48 }
  if ((is.na(D_All_A_CL$INTERSECT.116.))[i] ){D_All_A_CL$INTERSECT.116.[i]=1 }  #since we dont know , so we dont delet the casenumber.
  
  if ((!((D_All_A_CL$REFER.DIST.FT.29.[i] == 0) & (D_All_A_CL$REFER.DIST.MILE.29.[i] == 0)))
      & (D_All_A_CL$INTERSECT.116.[i]==1) &
      (!((D_All_A_CL$LOC.HARMFUL.EVENT.31A.[i] == 1) | (D_All_A_CL$LOC.HARMFUL.EVENT.31A.[i] == 2))))
  {IntersectionFree[i]=1}
}
D_All_A_CL <- cbind(IntersectionFree, D_All_A_CL)
#-------------------------------------------------
#2)Filtering for crash level

FState=c('HAW','HON') # ,'KAU' ,'MAU'
FYear=c(2005:2014)
FRoute=c(11,19,72,83,93,99,750,930)  #50,56,30 other islands
# Filter for action
Frunoff =c (2,3,6,21,22,23,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,71,74,102)# run off the road
Fheadon =c (11,80,83,85)# head on road
FRwD=c(Frunoff,Fheadon)
FIntersectionFree = 1
FMV.TOT.9. =levels(factor( D_All_A_CL$MV.TOT.9.[! D_All_A_CL$MV.TOT.9. %in% 0 ] ))
#FMV.TOT.9. =levels(factor( D_All_A_CL$MV.TOT.9.[D_All_A_CL$MV.TOT.9. != 0 ] )) #Another way to do the same thing as the PREVIOUS LINE does

#levels(factor(D_All_A_CL$MV.TOT.9.))

F_D_All_A_CL <-filter (D_All_A_CL,
                       COUNTY.2.%in% FState,
                       Year %in% FYear ,
                       PRIM.ROUTE.NO.27. %in% FRoute
                       ,EVNT.01.ACTION..31. %in% FRwD
                       ,IntersectionFree %in% FIntersectionFree
                       ,MV.TOT.9. %in% FMV.TOT.9.
)
F_D_All_A_CL[1] <- NULL # to remove extra colomns (intersection free crashes)
#write.csv( F_D_All_A_CL, file="C:/Users/pasha/Dropbox/Ph.D Research/MyCode1Arpril/Check_Data_Arpril17/Output/F_D_All_A_CL.csv")
#---------------------------------------------------------
#part 2
# How to filter the road (selcting only two lanes two way roads)
#72
R72L=c (2.08,13.23) # + direction  (2.33 , 13.53) for negative

#83
R83L=c (0,39.5) # Only one lane + c (0,39.59)

#93
R93L=c (12.71,19.524) # positive direction and negative direction are the same

#99
R99L=c (0,6.52)  

#R99L2=c (13.36,15.7) 

#750
R750L=c (0.84 ,7.21 )

#930
R930L=c (11.3,17.3) 



#11  bigIsland  415
# R11L=c (8.39,115.8)  
# until 109 is state road
R11L=c (8.39,109)

#19 bigIsland   297
# R19L  <- c(8.79,56.06)  #0 TO 52 IS STATE
R19L  <- c(8.79,52)

# R19L2 <- c( 56.87, 86.51) 
#58  TA 86.51 IS STATE
R19L2 <- c( 58, 86.51)

#30 Maui        118
#56 Kaui        116
#50 Kaui        117
#---------------------------------------------------------------------
N= nrow( F_D_All_A_CL)
CheckCrash=rep(0,N)
for (i in 1:N){
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==72){
    CheckCrash[i]=findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R72L ) == 1  #xi is the location of crash
  }
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==83){
    CheckCrash[i]=findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R83L) == 1  #xi is the location of crash
  }
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==93){
    CheckCrash[i]=findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R93L ) == 1  #xi is the location of crash
  }
  
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==99 ){
    CheckCrash[i]=(findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R99L ) == 1)   #xi is the location of crash
    
  }
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==750){
    CheckCrash[i]=((findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R750L ) == 1)) #xi is the location of crash
  }
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==930){
    CheckCrash[i]=findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R930L ) == 1  #xi is the location of crash
  }
  #big Island
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==11){
    CheckCrash[i]=findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R11L ) == 1  #xi is the location of crash
  }
  
  if(F_D_All_A_CL$PRIM.ROUTE.NO.27.[i]==19){
    CheckCrash[i]=findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R19L ) == 1 |
      (findInterval(F_D_All_A_CL$PRIM.MILE.POST.28.[i],  R19L2 ) == 1) #xi is the location of crash
  }
  
}
F_D_All_A_CL =cbind(CheckCrash,F_D_All_A_CL)
F_D_All_A_CL= subset(F_D_All_A_CL, CheckCrash %in% TRUE)
F_D_All_A_CL[1] <- NULL
#-------
