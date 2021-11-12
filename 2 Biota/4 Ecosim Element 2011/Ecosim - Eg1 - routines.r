################################################################################
# Routines
################################################################################

# 1. Check ecosystem model Time 0 status and parameters for general attributes
#    to check if likely to run

  # i) check if fx/x  for each element is suitable (if at equilibrium then rate should be close to 0)
         DEFUNCvals<-list(fx = NA, qbj_t = ODEparams$qbj_0, Mpi_t = ODEparams$Mpi_0, V = V, Month = -1,Tor = rep(1,ODEparams$BmsN))
         Res<-SolveODE_AdamsBashforth(Esim_DEfunc_01,Bms,BmsMin,ODEparams,DEFUNCvals,Time0=0,TimeEnd=2/12,TimeStepMin=1/12)
        print(Res$fx[,1]/Bms)
        
# 2. Run model

  DEFUNCvals<-list(fx = NA, qbj_t = ODEparams$qbj_0, Mpi_t = ODEparams$Mpi_0, V = V, Month = -1,Tor = rep(1,ODEparams$BmsN))
  Res<-SolveODE_AdamsBashforth(Esim_DEfunc_01,Bms,BmsMin,ODEparams,DEFUNCvals,Time0=0,TimeEnd=100,TimeStepMin=1/12)

# 3. Display results
#     default is to display in log domain so as all will be displayed in detail

Ydata<-log(Res$XperStep)
Ymin<-floor(min(Ydata))
Ymax<-ceiling(max(Ydata))
Xmin<-min(Res$Time)
Xmax<-max(Res$Time)
plot(NA,NA,xlab="Time",ylab="Log Biomass",xlim=c(Xmin,Xmax),ylim=c(Ymin,Ymax))
for (i in 1:BmsN) lines(Res$Time,Ydata[i,],col=BmsRef[1,i])

