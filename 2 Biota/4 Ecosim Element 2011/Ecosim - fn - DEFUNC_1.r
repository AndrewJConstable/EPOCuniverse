# Ecosim function Esim_DEfunc_01
# by Andrew Constable
# May 2011
#
# derived from Walters & Martell (2004)

Esim_DEfunc_01<-function(
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

# cat("     DEFUNC Point 2 : Tau = ",DEvals$Tau,sep="","\n")

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
      
