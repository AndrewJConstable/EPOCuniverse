v<-0.1  # from escaped group to vulnerable group
vprime<-0.5 # from vulnerable to escaped gropu

V<-0.6
Vprime<-0.4
Vres<-V
VresPrime<-Vprime
Tmax<-100
Time<-c(0:Tmax)

for (i in 1:Tmax){
 V<-(V+Vprime*v-V*vprime)
 Vprime<-(Vprime+V*vprime-Vprime*v)
Vres<-c(Vres,V)
VresPrime<-c(VresPrime,Vprime)
  }
  plot(Time,Vres,ylim=c(0,2),col="black")
  lines(Time,VresPrime,col="blue")
cat(" Vulnerable biomass = ",V,sep="","\n")
cat(" Escaped biomass    = ",Vprime,sep="","\n")
cat(" Total biomass      = ",(V+Vprime),sep="","\n")
cat(" Prop escaped       = ",Vprime/(V+Vprime),sep="","\n")
cat(" Calc Prop escaped  = ",vprime/(v+vprime),sep="","\n")
