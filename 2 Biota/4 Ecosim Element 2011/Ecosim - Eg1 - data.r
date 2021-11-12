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
   ,PB_0   = NA    # initial productivity to biomass ratio - calculated as G.Q/B
    ) # end list

# after a suitable burn in period, the following 'equilibrium' parameters should be reset

    # calculate 'equilibrium' Qij
    #  note:
    #       1. consumption when a=0 does not occur,
    #       2. Tor remains 1 for top predators (Mpi=0) i.e. balancing predation risk with optimal consumption rate is not an issue for top predators
    
        Qij_0 <- apply(ODEparams$BmsRef,2,function(BmsRef,Bms,a_t,v_t,vprime){
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
      ODEparams$Mpi_0 <- apply(ODEparams$BmsRef,2,function(BmsRef,Q,B){sum(Q[BmsRef,]*B[BmsRef])},Qij_0,Bms)
    # update qbj = sum across i qbij where qbij = Qij/Bj
      ODEparams$qbj_0 <- apply(ODEparams$BmsRef,2,function(BmsRef,Q,B){sum(Q[,BmsRef]/B[BmsRef])},Qij_0,Bms)
    # update PB - productivity to biomass ratio - calculated as G.Q/B
      ODEparams$PB_0  <-  apply(ODEparams$BmsRef,2,function(BmsRef,G,Q,B){sum(Q[,BmsRef]/B[BmsRef])},ODEparams$G,Qij_0,Bms)

print("ODE Params loaded")
print(ODEparams)
print("")



