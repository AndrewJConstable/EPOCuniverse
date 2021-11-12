CSI<-function(Yperf,BaseYr1,BaseYrEnd,CSIbase){
  IsMatrix<-is.matrix(Yperf)
  if(IsMatrix){
      Npred<-ncol(Yperf)
      Nyears<-nrow(Yperf)
    } else Nyears<-length(Yperf)

  if(is.null(CSIbase)){
    if(IsMatrix){
      Baseline<-Yperf[c(BaseYr1:BaseYrEnd),]
      BaseMns<-NULL
      for (p in 1:Npred) BaseMns<-c(BaseMns,mean(Baseline[,p]))
      BaseSDs<-NULL
      for (p in 1:Npred) BaseSDs<-c(BaseSDs,sd(Baseline[,p]))
      StandBaseline<-Baseline
      for (p in 1:Npred) StandBaseline[,p]<-(StandBaseline[,p]-BaseMns[p])/BaseSDs[p]
      CSI_S<-sqrt(sum(cov(StandBaseline)))
      CSIbase<-list(BaseMns = BaseMns, BaseSDs = BaseSDs, CSI_S = CSI_S)
      } else {

      Baseline<-Yperf[c(BaseYr1:BaseYrEnd)]
      BaseMns<-mean(Baseline)
      BaseSDs<-sd(Baseline)
      StandBaseline<-Baseline
      StandBaseline<-(StandBaseline-BaseMns)/BaseSDs
      CSI_S<-BaseSDs
      CSIbase<-list(BaseMns = BaseMns, BaseSDs = BaseSDs, CSI_S = CSI_S)
      }
   } # end if (is.null(CSIbase))

 # full time series of CSI
    StandYperf<-Yperf # had transformation here
    if(IsMatrix){
      for (p in 1:Npred) StandYperf[,p]<-(StandYperf[,p]-CSIbase$BaseMns[p])/CSIbase$BaseSDs[p]
      CSI_Yperf<-NULL
      for(y in 1:Nyears) CSI_Yperf<-c(CSI_Yperf,sum(StandYperf[y,])/CSIbase$CSI_S)
      } else {
        StandYperf<-(StandYperf-CSIbase$BaseMns)/CSIbase$BaseSDs
        CSI_Yperf<-NULL
        for(y in 1:Nyears) CSI_Yperf<-c(CSI_Yperf,StandYperf/CSIbase$CSI_S)
      }
    list(CSIbase    = CSIbase
        ,CSIseries  = CSI_Yperf)
 } # end function CSI
K