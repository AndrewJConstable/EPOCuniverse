################################################################################
# Routines
################################################################################

# 1. Check ecosystem model Time 0 status and parameters for general attributes
#    to check if likely to run

# setup initial variables V and PB
# after a suitable burn in period, the following 'equilibrium' parameters should be reset

    # calculate 'equilibrium' Qij
    #  note:
    #       1. consumption when a=0 does not occur,
    #       2. Tor remains 1 for top predators (Mpi=0) i.e. balancing predation risk with optimal consumption rate is not an issue for top predators

        Qij_0 <- apply(ODEparams$BmsRef,2,function(BmsRef,Bms,a_t,v_t,vprime,M,PB){
                             (a_t[,BmsRef]*v_t[,BmsRef]*Bms*Bms[BmsRef])/(v_t[,BmsRef]+vprime[,BmsRef]+a_t[,BmsRef]*Bms[BmsRef])#+M-PB)
                            },Bms,ODEparams$Va,ODEparams$Vv,ODEparams$Vvprime,ODEparams$M,ODEparams$PB)


    # update Mpi  = sum across j (Qij/Bi)
      ODEparams$Mpi_0 <- apply(ODEparams$BmsRef,2,function(BmsRef,Q,B){sum(Q[BmsRef,]*B[BmsRef])},Qij_0,Bms)
    # update qbj = sum across i qbij where qbij = Qij/Bj
      ODEparams$qbj_0 <- apply(ODEparams$BmsRef,2,function(BmsRef,Q,B){sum(Q[,BmsRef]/B[BmsRef])},Qij_0,Bms)
    # update PB - productivity to biomass ratio - calculated as G.Q/B

print("ODE Params loaded")
print(ODEparams)
print("")

       V=apply(ODEparams$BmsRef,2,function(BmsRef,Bms,v,vprime,a,M,PB){
                        (v[,BmsRef]*Bms)/(v[,BmsRef]+vprime[,BmsRef]+a[,BmsRef]*Bms)#+M-PB)
                        },Bms,ODEparams$Vv,ODEparams$Vvprime,ODEparams$Va,ODEparams$M,ODEparams$PB)

      PB_0  <-  apply(ODEparams$BmsRef,2,function(BmsRef,Q,B,Params){
                                if(Params$G[BmsRef]>0) sum(Q[,BmsRef]/B[BmsRef])
                                else Params$r[BmsRef]*(1-B[BmsRef]/Params$K[BmsRef])
                                },Qij_0,Bms,ODEparams)


  # i) check if fx/x  for each element is suitable (if at equilibrium then rate should be close to 0)
         DEFUNCvals<-list(fx = NA, qbj_t = ODEparams$qbj_0, Mpi_t = ODEparams$Mpi_0, V = V, PB = PB_0, Month = -1,Tau = rep(1,ODEparams$BmsN))
         Res<-SolveODE_AdamsBashforth(Esim_DEfunc_01prime,Bms,BmsMin,ODEparams,DEFUNCvals,Time0=0,TimeEnd=2/12,TimeStepMin=1/12)
        print(Res$fx[,1]/Bms)
        
# 2. Run model

  DEFUNCvals<-list(fx = NA, qbj_t = ODEparams$qbj_0, Mpi_t = ODEparams$Mpi_0, V = V, PB = PB_0, Month = -1,Tau = rep(1,ODEparams$BmsN))
  Res<-SolveODE_AdamsBashforth(Esim_DEfunc_01prime,Bms,BmsMin,ODEparams,DEFUNCvals,Time0=0,TimeEnd=1,TimeStepMin=1/12)

