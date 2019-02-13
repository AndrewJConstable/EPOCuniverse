function (Npop,FirstAge,LastAge,RecMn,RecCV,NatMort,Obs)
{

CVint<-(RecCV[length(RecCV)]-RecCV[1])/(Obs-1)
Mint<-(NatMort[length(NatMort)]-NatMort[1])/(Obs-1)

CaseCV<-RecCV[1]
if(CVint>0) CaseCV<-(c(1:Obs)-1)*CVint+RecCV[1]
CaseM<-NatMort[1]
if(Mint>0) CaseM<-(c(1:Obs)-1)*Mint+NatMort[1]


B0ratio<-vector("numeric",Obs)

UseB0mean<-FALSE # False = determine B0 using medianR : True - determine B0 on meanR

for (i in 1:Obs){
  useCV<-ifelse(CVint>0,CaseCV[i],CaseCV[1])
  useM<-ifelse(Mint>0,CaseM[i],CaseM[1])

B0ratio[i]<-B0test(Npop,FirstAge,LastAge,RecMn,useCV,useM,UseB0mean)
}
if(CVint>0) plot(CaseCV,B0ratio)
if(Mint>0) plot(CaseM,B0ratio)
}

