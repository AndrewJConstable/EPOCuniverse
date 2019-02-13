# Ecosim function: Esim_DEfunc_01prime
# by Andrew Constable
# May 2011

# derived from Walters & Martell (2004)

################################################################################
# Notes
#
# 20110518 explored problem identified in characterising vulnerable population (Eq 10.1 in Walters & Martell (2004)
#          Vulnerable biomass and Q updated to include mortality and PB ratio in denominator


################################################################################

Esim_DEfunc_01prime<-function(
                 x
                ,Params
                ,DEvals
                ,Time
                ,TimeStep
                ){

    # update Tau
        # increment per month rather than every time step in the ODE

    #  note:
    #       1. consumption when a=0 does not occur,
    #       2. Tau remains 1 for top predators (Mpi=0) i.e. balancing predation risk with optimal consumption rate is not an issue for top predators

# cat("     DEFUNC Point 1",sep="","\n")

        CurrentMonth <- floor(Time[TimeStep,2]*12)
        if(CurrentMonth>DEvals$Month){
          DEvals$Tau[Params$Mpi_0>0 & Params$qbj_0>0] <- DEvals$Tau[Params$Mpi_0>0 & Params$qbj_0>0]*
                                                        (1-Params$Si[Params$Mpi_0>0 & Params$qbj_0>0])+
                                                        (Params$Si[Params$Mpi_0>0 & Params$qbj_0>0]*
                                                        Params$qbj_0[Params$Mpi_0>0  & Params$qbj_0>0]*
                                                        DEvals$Mpi_t[Params$Mpi_0>0 & Params$qbj_0>0])/
                                                        (DEvals$qbj_t[Params$Mpi_0>0 & Params$qbj_0>0]*
                                                        Params$Mpi_0[Params$Mpi_0>0 & Params$qbj_0>0]) # update in discrete time steps of one month
           DEvals$Month<-CurrentMonth
           }

# cat("     DEFUNC Point 2",sep="","\n")

    # update Hj = 1 + sum across prey k (akj*hkj*Vkj)
        Hj <- 1+apply(Params$BmsRef,2,function(BmsRef,a,Hh,V){sum(a[,BmsRef]*Hh[,BmsRef]*V[,BmsRef])},DEvals$aij_t,Params$hij,DEvals$V)

    # calculate  aij(t) and vij(t) for current time step
        aij_t <- t(apply(Params$Va,1,function(Va,Tau,Hj){
                      Va[Va>0]<-Va[Va>0]*Tau[Va>0]/Hj[Va>0]
                      Va},DEvals$Tau,Hj))

       # Vv_t should remain =1 if, at time 0, Vv_0 = 1
       
        Vv_t <- apply(Params$Vv,2,function(Vv,Tau){
                                                   Vv[Vv<1]<-Vv[Vv<1]*Tau[Vv<1]
                                                   Vv
                                                   },DEvals$Tau)

# cat("     DEFUNC Point 3",sep="","\n")


        Qij_t <- apply(Params$BmsRef,2,function(BmsRef,Bms,a_t,v_t,vprime,M,PB){
                             (a_t[,BmsRef]*v_t[,BmsRef]*Bms*Bms[BmsRef])/(v_t[,BmsRef]+vprime[,BmsRef]+a_t[,BmsRef]*Bms[BmsRef])#+M-PB)
                            },x,aij_t,Vv_t,Params$Vvprime,Params$M,DEvals$PB)

        Production <- apply(Params$BmsRef,2,function(BmsRef,Bms,Qij_t,G,r,K){
                               if(G[BmsRef]>0) G[BmsRef]*sum(Qij_t)
                               else r[BmsRef]*Bms[BmsRef]*(1-Bms[BmsRef]/K[BmsRef])
                               },x,Qij_t,Params$G,Params$r,Params$K)

        # derivatives
        DEvals$fx <- apply(Params$BmsRef,2,function(BmsRef,Bms,Production,Qij_t,Moi){
                               Production[BmsRef]-sum(Qij_t[BmsRef,])-Moi[BmsRef]*Bms[BmsRef]
                                     },x,Production,Qij_t,Params$M)

    # update V according to equation 10.2 but using the modified  aij_t & v_t

# cat("     DEFUNC Point 4",sep="","\n")

       DEvals$V=apply(Params$BmsRef,2,function(BmsRef,Bms,v,vprime,a,M,PB){
                        (v[,BmsRef]*Bms)/(v[,BmsRef]+vprime[,BmsRef]+a[,BmsRef]*Bms)#+M-PB)
                        },x,Vv_t,Params$Vvprime,aij_t,Params$M,DEvals$PB)

# cat("     DEFUNC Point 5",sep="","\n")

    # update Mpi  = sum across j (Qij/Bi)
          DEvals$Mpi_t <- apply(Params$BmsRef,2,function(BmsRef,Q,B){sum(Q[BmsRef,]*B[BmsRef])},Qij_t,x)
      
    # update qbj = sum across i qbij where qbij = Qij/Bj
          DEvals$qbj_t <- apply(Params$BmsRef,2,function(BmsRef,Q,B){sum(Q[,BmsRef]/B[BmsRef])},Qij_t,x)

    # update PB = G.Q/B
          DEvals$PB <- Production/x
# cat("     DEFUNC Point 6",sep="","\n")
      return(DEvals)
      } # end DEfunc
      
