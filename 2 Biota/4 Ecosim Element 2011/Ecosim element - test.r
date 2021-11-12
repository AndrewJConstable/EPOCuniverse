# Ecosim test routines
# by Andrew Constable
# May 2011
#
# derived from Walters & Martell (2004)

#################################################################################
# Description

# Test routines start with a 4 species system.  No fishing mortality

################################################################################
# Universe

# Species 1 = phytoplankton
#              modelled with a density-dependent surplus production model
# Species 2 = zooplankton
#             secondary producer where mortality is primarily from predation
# Species 3 = Fish
#             higher trophic level with mortality by consumption and by causes other then mortality
# Species 4 = Shark
#             upper trophic level with mortality by causes other then mortality

SpNames <- c("PP","Zpl","Fish","Shark")

################################################################################
# Input Data

Bms <-  c(1E6,1E5,1E4,1E3) # initialise biomass
BmsMin<- c(1E-6,1E-6,1E-6,1E-6) # minimum values in ODE
BmsN<-length(Bms)
BmsRef<-matrix(c(1:BmsN),nrow=1)

Vprop <- matrix(   # proportion of the species as prey vulnerable to consumption by predator
                   # columns = predators, rows = prey
          # "PP", "Zpl", "Fish", "Shark"
         c(    0,   1.0,      0,       0
          ,    0,     0,    1.0,       0
          ,    0,     0,      0,     1.0
          ,    0,     0,      0,       0
           )
         ,nrow=BmsN,byrow=TRUE)

V<-apply(Vprop,2,function(Vp,Bms){Vp*Bms},Bms)
print(V)

################################################################################
# Parameters

ODEparams <- list(
    G  = c(0,0.1,0.3,0.2) # growth efficiency given biomass eaten (could determine from P/B and per capita consumption : P/B/C (if G=0 then no consumption - driven by surplus production model
   ,r  = c(0.2,0.2,0.1,0.01) # density dependent growth parameter r when G=0
   ,K  = c(1E6,1E5,1E4,1E3) # density dependent growth parameter K when G=0
   ,M  = c(0,0.3,0.3,0.1) # mortality rate per year through causes other than predation
   ,Vv = matrix(   # instantaneous flux (per year) from escaped portion of prey biomass to portion vulnerable to predator
                   # columns = predators, rows = prey
          # "PP", "Zpl", "Fish", "Shark"
         c(    0,   1.0,      0,       0
          ,    0,     0,    1.0,       0
          ,    0,     0,      0,     1.0
          ,    0,     0,      0,       0
           )
         ,nrow=BmsN,byrow=TRUE)
   ,Vvprime = matrix(   # instantaneous flux (per year) from portion of prey biomass vulnerable to predator to portion escaped from predator
                   # columns = predators, rows = prey
          # "PP", "Zpl", "Fish", "Shark"
         c(    0,   0.0,      0,       0
          ,    0,     0,    0.0,       0
          ,    0,     0,      0,     0.0
          ,    0,     0,      0,       0
           )
         ,nrow=BmsN,byrow=TRUE)

   ,Va = matrix(   # instantaneous per V predation mortality of prey biomass to predator under maximal conditions
                   # columns = predators, rows = prey
          # "PP", "Zpl", "Fish", "Shark"
         c(    0,   0.8E-5,      0,       0
          ,    0,     0,    0.8E-4,       0
          ,    0,     0,      0,     0.2E-3
          ,    0,     0,      0,       0
           )
         ,nrow=BmsN,byrow=TRUE)

   ,Si = c(0.00,0.00,0.00,0.00) # numerical rate adjustment factor (0<S<1)
   ,Hh = matrix(   # handling time (in same units as other rate parameters) by predator per unit of prey (fraction of year per unit prey per unit predator)
                # 1E-4  is approximately 1 hour;  2E-6 is approx 1 minute
                   # columns = predators, rows = prey
          # "PP", "Zpl", "Fish", "Shark"
         c(    0,  2E-6,      0,       0
          ,    0,     0,   2E-6,       0
          ,    0,     0,      0,    1E-4
          ,    0,     0,      0,       0
           )
         ,nrow=BmsN,byrow=TRUE)
   ,BmsRef = BmsRef
   ,BmsN   = BmsN
   ,qbj_0  = NA    # placeholder for initialisation at equilibrium
   ,Mpi_0  = NA    # placeholder for initialisation at equilibrium
    ) # end list

# after a suitable burn in period, the following 'equilibrium' parameters should be reset

    # calculate 'equilibrium' Qij
    #  note:
    #       1. consumption when a=0 does not occur,
    #       2. Tor remains 1 for top predators (Mpi=0) i.e. balancing predation risk with optimal consumption rate is not an issue for top predators
    
        Qij_0 <- apply(Params$BmsRef,2,function(BmsRef,Bms,a_t,v_t,vprime){
                           Res<-Bms*0
                           Res[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1)] <- (a_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]*
                                                             v_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]*Bms[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1)]*Bms[BmsRef])/
                                                             (v_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]+
                                                              vprime[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]+
                                                              a_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]*
                                                              Bms[BmsRef])
                           Res[(a_t[,BmsRef]>0 & v_t[,BmsRef]==1)] <- a_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]==1),BmsRef]*
                                                                      Bms[(a_t[,BmsRef]>0 & v_t[,BmsRef]==1)]*
                                                                      Bms[BmsRef]
                           Res
                                     },Bms,ODEparams$Va,ODEparams$Vv,ODEparams$Vvprime)
    # update Mpi  = sum across j (Qij/Bi)
      ODEparams$Mpi_0 <- apply(Params$BmsRef,2,function(BmsRef,Q,B){sum(Q[BmsRef,]*B[BmsRef])},Qij_0,Bms)
    # update qbj = sum across i qbij where qbij = Qij/Bj
      ODEparams$qbj_0 <- apply(Params$BmsRef,2,function(BmsRef,Q,B){sum(Q[,BmsRef]/B[BmsRef])},Qij_0,Bms)






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


################################################################################
# Functions
################################################################################
SolveODE_AdamsBashforth<-function(
                             DEFUNC             # function of ODEs
                            ,x                  # initial values of x to solve
                            ,Xmin               # vector of minimum values permitted in x
                            ,Params             # parameters that are input to DEFUNC
                            ,DEFUNCvals         # values calculated in DEFUNC that need to be retained for subsequent iterations - the values parsed into this function are the initial values for t0
                            ,Time0=0            # time at start of interval
                            ,TimeEnd=1          # time at end of interval
                            ,TimeStepMin=2E-6   # minimum time step for equal interval
                            ){ # return x, Time (vector of times during interval), ODEresults (col = time step, row = variable)
  # Walters version of Adams-Bashforth - see box 6.1, p 134)

  # notes
  #        ? need to deal with fractions of TimeStep in weighting the derivative in the last time step

    # 1. set up time matrix
        # equal time steps for now

        TimeInterval<-TimeEnd-Time0
        TimeStepsN<-ceiling(TimeInterval/TimeStepMin) # round up and make the adjustment for incomplete timestep at end in the Time matrix
        TimeStepRef<-c(1:(TimeStepsN+1))
        TimeSteps<-TimeStepRef*TimeStepMin
        Time<-cbind(# matrix of time steps
               TimeStepRef   # col 1 = number of the timestep
              ,c(Time0,(Time0+(TimeStepRef[c(1:(TimeStepsN-1))]*TimeStepMin)),TimeEnd)  # col 2 = time at beginning of step
              ,c(rep(TimeStepMin,(TimeStepsN-1)),(TimeInterval-((TimeStepsN-1)*TimeStepMin)),0) # col 3 = length of time step
                 )

    # 2. set up ODE results & add to Data
        ODEresult<-matrix(NA,nrow=length(x),ncol=(TimeStepsN+1))          # create matrix of ODE values for all time steps + 1 (the last row will remain NAs but be compatible with dx at time 0 and no dx at end time).
        XperStep<-matrix(NA,nrow=length(x),ncol=(TimeStepsN))          # create matrix for X after each time step
        Xstart<-x

    # 3. loop through timesteps calling FUNC
         # Timestep 1 - Euler method
                DEFUNCvals<-DEFUNC(x,Params,DEFUNCvals,Time,1)
                ODEresult[,1]<-DEFUNCvals$fx
                x<-x+DEFUNCvals$fx*Time[1,3]
                x[x<Xmin]<-Xmin[x<Xmin]
                XperStep[,1]<-x

         # Timestep 2 - Adams-Bashforth approximation of change
            if(TimeStepsN>1){
              for (TimeStep in 2:TimeStepsN){
              
#              cat('Timestep : ',TimeStep,sep="","\n")
#              cat('X0 =',x,sep="  ","\n")
                DEFUNCvals<-DEFUNC(x,Params,DEFUNCvals,Time,TimeStep)
                ODEresult[,TimeStep]<-DEFUNCvals$fx
                x<-x+Time[TimeStep,3]/2*(3*DEFUNCvals$fx-ODEresult[,(TimeStep-1)])
                x[x<Xmin]<-Xmin[x<Xmin]
#              cat('X1 =',x,sep="  ","\n")
                XperStep[,TimeStep]<-x
                }
              } # end if
        
      list(x = x, Time = Time[,2], XperStep = cbind(Xstart,XperStep),fx = ODEresult)
      
      } # end ODE function
      
      

Esim_DEfunc_01<-function(
                 x
                ,Params
                ,DEvals
                ,Time
                ,TimeStep
                ){

    # update Tor
        # increment per month rather than every time step in the ODE

    #  note:
    #       1. consumption when a=0 does not occur,
    #       2. Tor remains 1 for top predators (Mpi=0) i.e. balancing predation risk with optimal consumption rate is not an issue for top predators

# cat("     DEFUNC Point 1",sep="","\n")

        CurrentMonth <- floor(Time[TimeStep,2]*12)
        if(CurrentMonth>DEvals$Month){
          DEvals$Tor[Params$Mpi_0>0 & Params$qbj_0>0] <- DEvals$Tor[Params$Mpi_0>0 & Params$qbj_0>0]*
                                                        (1-Params$Si[Params$Mpi_0>0 & Params$qbj_0>0])+
                                                        (Params$Si[Params$Mpi_0>0 & Params$qbj_0>0]*
                                                        Params$qbj_0[Params$Mpi_0>0  & Params$qbj_0>0]*
                                                        DEvals$Mpi_t[Params$Mpi_0>0 & Params$qbj_0>0])/
                                                        (DEvals$qbj_t[Params$Mpi_0>0 & Params$qbj_0>0]*
                                                        Params$Mpi_0[Params$Mpi_0>0 & Params$qbj_0>0]) # update in discrete time steps of one month
           DEvals$Month<-CurrentMonth
           }

# cat("     DEFUNC Point 2 : Tor = ",DEvals$Tor,sep="","\n")

    # update Hj = 1 + sum across prey k (akj*hkj*Vkj)
        Hj <- 1+apply(Params$BmsRef,2,function(BmsRef,a,Hh,V){sum(a[,BmsRef]*Hh[,BmsRef]*V[,BmsRef])},DEvals$aij_t,Params$hij,DEvals$V)

    # calculate  aij(t) and vij(t) for current time step
        aij_t <- t(apply(Params$Va,1,function(Va,Tor,Hj){
                      Va[Va>0]<-Va[Va>0]*Tor[Va>0]/Hj[Va>0]
                      Va},DEvals$Tor,Hj))

       # Vv_t should remain =1 if, at time 0, Vv_0 = 1
       
        Vv_t <- apply(Params$Vv,2,function(Vv,Tor){
                                                   Vv[Vv<1]<-Vv[Vv<1]*Tor[Vv<1]
                                                   Vv
                                                   },DEvals$Tor)

# cat("     DEFUNC Point 3",sep="","\n")

    # calculate new Qij
        Qij_t <- apply(Params$BmsRef,2,function(BmsRef,Bms,a_t,v_t,vprime){
                           Res<-Bms*0
                           Res[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1)] <- (a_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]*
                                                             v_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]*Bms[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1)]*Bms[BmsRef])/
                                                             (v_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]+
                                                              vprime[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]+
                                                              a_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]<1),BmsRef]*
                                                              Bms[BmsRef])
                           Res[(a_t[,BmsRef]>0 & v_t[,BmsRef]==1)] <- a_t[(a_t[,BmsRef]>0 & v_t[,BmsRef]==1),BmsRef]*
                                                                      Bms[(a_t[,BmsRef]>0 & v_t[,BmsRef]==1)]*
                                                                      Bms[BmsRef]
                           Res
                                     },x,aij_t,Vv_t,Params$Vvprime)

# cat("     DEFUNC Point 4",sep="","\n")

        # derivatives
        DEvals$fx <- apply(Params$BmsRef,2,function(BmsRef,Bms,G,Qij_t,Moi,r,K){
                               if(G[BmsRef]>0) (G[BmsRef]*sum(Qij_t[,BmsRef])-sum(Qij_t[BmsRef,])-Moi[BmsRef]*Bms[BmsRef])
                               else r[BmsRef]*Bms[BmsRef]*(1-Bms[BmsRef]/K[BmsRef])-sum(Qij_t[BmsRef,])
                                     },x,Params$G,Qij_t,Params$M,Params$r,Params$K)

    # update V according to equation 10.2 but using the modified  aij_t & v_t

# cat("     DEFUNC Point 5",sep="","\n")

       DEvals$V=apply(Params$BmsRef,2,function(BmsRef,Bms,v,vprime,a){
                        Res<-Bms*0
                        Res[(a[,BmsRef]>0 & v[,BmsRef]<1)]<-(v[(a[,BmsRef]>0 & v[,BmsRef]<1),BmsRef]*Bms[(a[,BmsRef]>0 & v[,BmsRef]<1)])/
                                           (v[(a[,BmsRef]>0 & v[,BmsRef]<1),BmsRef]+vprime[(a[,BmsRef]>0 & v[,BmsRef]<1),BmsRef]+
                                           a[(a[,BmsRef]>0 & v[,BmsRef]<1),BmsRef]*Bms[BmsRef])
                        Res[(a[,BmsRef]>0 & v[,BmsRef]==1)]<-Bms[(a[,BmsRef]>0 & v[,BmsRef]==1)]
                        Res
                        },x,Vv_t,Params$Vvprime,aij_t)

# cat("     DEFUNC Point 6",sep="","\n")

    # update Mpi  = sum across j (Qij/Bi)
          DEvals$Mpi_t <- apply(Params$BmsRef,2,function(BmsRef,Q,B){sum(Q[BmsRef,]*B[BmsRef])},Qij_t,x)
      
    # update qbj = sum across i qbij where qbij = Qij/Bj
          DEvals$qbj_t <- apply(Params$BmsRef,2,function(BmsRef,Q,B){sum(Q[,BmsRef]/B[BmsRef])},Qij_t,x)
      return(DEvals)
      } # end DEfunc
      
