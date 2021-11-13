# Source: Pre.KPFM.01.R
# by A. Constable
# last edit: 4 July 2008

Nscenarios   <- 3
Npolygons <- 18

EnvFiles   = list(State = "KrillEnvState.dat")

Files.Krill = list(PlotName      = "Krill"
                  ,PlotLineColour= "black"
                  ,PlotLineType  = 1
                  ,PlotLineWidth = 3
                  ,SSMUs         = c(1:18)
                  ,PlotRelSSMUs     = c(1:15)   # relative positions of SSMUs to plot
                  ,State_N       = "Biota_Krill_State_N.dat"
                  ,State_B       = "Biota_Krill_State_B.dat"
                  ,State_RepCond = "Biota_Krill_State_RepCond.dat"
                  )
Files.Seal = list(PlotName      = "Seals"
                 ,PlotLineColour= "red"
                 ,PlotLineType  = 1
                 ,PlotLineWidth = 1
                 ,SSMUs         = c(3,4,7,10,11,12,14,15)
                 ,PlotRelSSMUs     = c(1,2,3,4,5,6,7,8)   # relative positions of SSMUs to plot
                 ,State_N       = "Biota.Seal.State.N.dat"
                 ,State_B       = "Biota.Seal.State.B.dat"
                 ,State_Stage   = "Biota.Seal.State.Stage.dat"
                 )

Files.PengA3 = list(PlotName      = "PenguinsA3"
                   ,PlotLineColour= "green"
                   ,PlotLineType  = 1
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(3,4,5,7,10,12)
                   ,PlotRelSSMUs  = c(1:6)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.PengA3.State.N.dat"
                   ,State_B       = "Biota.PengA3.State.B.dat"
                   ,State_Stage   = "Biota.PengA3.State.Stage.dat"
                   )

Files.PengA4 = list(PlotName      = "PenguinsA4"
                   ,PlotLineColour= "green"
                   ,PlotLineType  = 2
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(2,6,15)
                   ,PlotRelSSMUs  = c(1,2,3)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.PengA4.State.N.dat"
                   ,State_B       = "Biota.PengA4.State.B.dat"
                   ,State_Stage   = "Biota.PengA4.State.Stage.dat"
                   )

Files.PengA5 = list(PlotName      = "PenguinsA5"
                   ,PlotLineColour= "green"
                   ,PlotLineType  = 3
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(8,11,14)
                   ,PlotRelSSMUs  = c(1,2,3)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.PengA5.State.N.dat"
                   ,State_B       = "Biota.PengA5.State.B.dat"
                   ,State_Stage   = "Biota.PengA5.State.Stage.dat"
                   )

Files.FishA2F1 = list(PlotName    = "FishA2F1"
                   ,PlotLineColour= "blue"
                   ,PlotLineType  = 1
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(9,13)
                   ,PlotRelSSMUs  = c(1,2)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.FishA2F1.State.N.dat"
                   ,State_B       = "Biota.FishA2F1.State.B.dat"
                   ,State_Stage   = "Biota.FishA2F1.State.Stage.dat"
                   )

Files.FishA2F2 = list(PlotName    = "FishA2F2"
                   ,PlotLineColour= "blue"
                   ,PlotLineType  = 2
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(14,15)
                   ,PlotRelSSMUs  = c(1,2)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.FishA2F2.State.N.dat"
                   ,State_B       = "Biota.FishA2F2.State.B.dat"
                   ,State_Stage   = "Biota.FishA2F2.State.Stage.dat"
                   )

Files.FishA2F3 = list(PlotName    = "FishA2F3"
                   ,PlotLineColour= "blue"
                   ,PlotLineType  = 3
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(1,7,10,11)
                   ,PlotRelSSMUs  = c(1,2,3,4)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.FishA2F3.State.N.dat"
                   ,State_B       = "Biota.FishA2F3.State.B.dat"
                   ,State_Stage   = "Biota.FishA2F3.State.Stage.dat"
                   )

Files.FishA3F1 = list(PlotName    = "FishA3F1"
                   ,PlotLineColour= "blue"
                   ,PlotLineType  = 4
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(2,3,4,5,6,8,12)
                   ,PlotRelSSMUs  = c(1,2,3,4,5,6,7)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.FishA3F1.State.N.dat"
                   ,State_B       = "Biota.FishA3F1.State.B.dat"
                   ,State_Stage   = "Biota.FishA3F1.State.Stage.dat"
                   )

Files.Whales = list(PlotName      = "Whales"
                   ,PlotLineColour= "purple"
                   ,PlotLineType  = 1
                   ,PlotLineWidth = 1
                   ,SSMUs         = c(1)
                   ,PlotRelSSMUs  = c(1)   # relative positions of SSMUs to plot
                   ,State_N       = "Biota.Whale.State.N.dat"
                   ,State_B       = "Biota.Whale.State.B.dat"
                   ,State_Stage   = "Biota.Whale.State.Stage.dat"
                   )

Files.Fishery = list(PlotName     = "Fishery"
                   ,PlotLineColour= "black"
                   ,PlotLineType  = 4
                   ,PlotLineWidth = 3
                   ,SSMUs         = c(2,3,4,5,6,8,12)
                   ,PlotRelSSMUs  = c(1,2,3,4,5,6,7)   # relative positions of SSMUs to plot
                   ,Catch       = "FisheryCatchRecords.dat"
                   )


################################################################################
# plot nominated quantile of total numbers on same plot with different axes for each scaled so that
# mean of the time series of each element is at the same point on the axes


# plot input data

PlotElmnts<-list(Krill = Files.Krill
                ,Seals = Files.Seal
                ,PengA3 = Files.PengA3
                ,PengA4 = Files.PengA4
                ,PengA5 = Files.PengA5
                ,FishA2F1 = Files.FishA2F1
                ,FishA2F2 = Files.FishA2F2
                ,FishA2F3 = Files.FishA2F3
                ,FishA3F1 = Files.FishA3F1
                ,Whales   = Files.Whales
                ,Fishery = Files.Fishery
                )

#########################################################
# plot totals

FirstYear<-1950
StandardiseYear<-1970
ProbQuantile<-0.5
PlotScenario<-NA  # if NA then plot quantile across all scenarios
PlotTimeInYear <- 1  # if NA then print all times - otherwise only nominated time
Xlabel<-"Year"
Ylabel<-"Relative Population Size"
Ylimits<-c(0,5)
# plot derived variables
Nelmnts<-length(PlotElmnts)

LegendNames<-NULL
LineColour<-NULL
LineType<-NULL
LineWidths<-NULL
for (Ne in 1:Nelmnts){

  SSMUs       <-PlotElmnts[[Ne]]$SSMUs
  PlotRelSSMUs<-PlotElmnts[[Ne]]$PlotRelSSMUs
  FileState_N <-PlotElmnts[[Ne]]$State_N

  LegendNames <-c(LegendNames,PlotElmnts[[Ne]]$PlotName)
  LineColour  <-c(LineColour,PlotElmnts[[Ne]]$PlotLineColour)
  LineType    <-c(LineType,PlotElmnts[[Ne]]$PlotLineType)
  LineWidths  <-c(LineWidths,PlotElmnts[[Ne]]$PlotLineWidth)

  # read file - each file has
      #  col 1 = scenario
      #  col 2 = year
      #  col 3 = start of period as fraction of year
      #  col 4..polygonsN = number of individuals : one column for each polygon

  Data  <-read.csv(paste(RootDir,FileState_N,sep=""), header = FALSE)

  # restrict data to only polygons to be plotted (adding on number of columns with factor data)
  # and exclude years prior to FirstYear

  Data<-Data[Data[,2]>=FirstYear,c(1:3,(PlotRelSSMUs+3))]
  DataCols<-ncol(Data)

  if(Ne==1){ # initiate plot
    Years<-unique(Data[,2])
    FirstYear<-min(Years)
    LastYear<-max(Years)
    
    windows()
    plot(NA,NA,xlim=c(FirstYear,LastYear),ylim=Ylimits,xlab=Xlabel,ylab=Ylabel)
    } # end if Ne==1

  Times <- unique(Data[,3])

  # subset data to times to plot
  if(!is.na(PlotTimeInYear)){
          Data<-Data[Data[,3]==Times[PlotTimeInYear],]
          Times<-Times[PlotTimeInYear]
        }
  TimesN<-length(Times)
  
  Years<-unique(Data[,2])
  YearsN<-length(Years)

  # generate time series of data pooling across SSMUs to plot and finding quantile
  PlotData<-NULL
  for (pd in 1:nrow(Data)){
      PlotData<-rbind(PlotData,c(Data[pd,1],(Data[pd,2]+Data[pd,3]),sum(Data[pd,c(4:DataCols)])))
    } # end pd

  # plot value for a given scenario or quantile from scenarios
  if (!is.na(PlotScenario)){
    PlotData<-PlotData[PlotData[,1]==PlotScenario,c(2,3)]
    } else {
    PD<-NULL
    for (y in 1:YearsN){
      for (tm in 1:TimesN){
        PD<-rbind(PD,c((Years[y]+Times[tm]),quantile(PlotData[PlotData[,2]==(Years[y]+Times[tm]),3],ProbQuantile)))
        } # end tm
      } # end y
      PlotData<-PD
    }

  # determine reference point
  RefPtData<-PlotData[floor(PlotData[,1])==StandardiseYear,2]
  RefPt<-RefPtData[1] # take first observation in year
  PlotData[,2]<-PlotData[,2]/RefPt
  lines(PlotData[,1],PlotData[,2],col=LineColour[Ne],lty=LineType[Ne],lwd=LineWidths[Ne])
  cat(LegendNames[Ne]," : Ref value ",RefPt," : Year ",StandardiseYear,"\n",sep="")
  } # end Ne

  legend("topright", legend=LegendNames, col = LineColour,
       lty=LineType, lwd=LineWidths
       ,title = "Element")


###################################################################################
###################################################################################
# plot stages for different taxa

Eno<-2
Scenario<-1
Time<-1  # if NA plot all times
QtyCol<-6   # stage abundance (6), reproductive condition (7) or health (8)
Xlabel<-"Year"
Ylabel<-"Number"

PlotAdultsOnly<-FALSE # if true then limit to adults

PlotData<-read.csv(paste(RootDir,PlotElmnts[[Eno]]$State_Stage,sep=""), header = FALSE)
             # Col 1 =  CurrentScenario
             # Col 2 =  CurrentYear
             # Col 3 =  proportion of year
             # Col 4 =  polygon
             # Col 5 =  stage
             # Col 6 =  Quantity at stage
             # Col 7 =  Reproductive condition
             # Col 8 =  Health
PlotData<-PlotData[PlotData[,1]==Scenario,]
Stages<-unique(PlotData[,5])
StagesN<-length(Stages)
Years<-unique(PlotData[,2])
YearsN<-length(Years)
Times<-unique(PlotData[,3])
TimesN<-length(Times)
Polygons<-unique(PlotData[,4])
PolygonsN<-length(Polygons)

if (!is.na(Time)) {
  PlotData<-PlotData[PlotData[,3]==Times[Time],]
  Times<-unique(PlotData[,3])
  TimesN<-length(Times)
  }

FirstYear<-Years[1]
LastYear<-Years[YearsN]

# pool across polygons
PD<-NULL
for (y in 1:YearsN){
  for (tm in 1:TimesN){
    for (st in 1:StagesN){
      PD<-rbind(PD,c(
                (Years[y]+Times[tm])
                ,st
                ,sum(PlotData[PlotData[,2]==Years[y] &
                              PlotData[,3]==Times[tm] &
                              PlotData[,5]==Stages[st]
                              ,QtyCol])
                ))
      } # end st
    } # end tm
  } # end y

Ymin<-min(PD[,3])
Ymax<-max(PD[,3])
windows()
plot(NA,NA,xlim=c(FirstYear,LastYear),ylim=c(Ymin,Ymax),xlab=Xlabel,ylab=Ylabel)
for (st in 1:StagesN) lines(PD[PD[,2]==st,1]
                           ,PD[PD[,2]==st,3]
                           ,col=st)

legend("topright", legend=as.character(Stages), col = Stages,
       lty=Stages/Stages
       ,title = "Stage")

#######################################################################
# Krill Initial abundance Multiplier 1E6
#   Krill      : Ref value 3.141015e+19 : Year 1970
#   Seals      : Ref value 4947083      : Year 1970
#   PenguinsA3 : Ref value 9602090      : Year 1970
#   PenguinsA4 : Ref value 3129497      : Year 1970
#   PenguinsA5 : Ref value 18153045     : Year 1970
#   FishA2F1   : Ref value 2.863712e+12 : Year 1970
#   FishA2F2   : Ref value 21867531570  : Year 1970
#   FishA2F3   : Ref value 51845442287  : Year 1970
#   FishA3F1   : Ref value 52263531280  : Year 1970
#   Whales     : Ref value 73700.11     : Year 1970

# Krill Initial abundance Multiplier 1E3
#   Krill      : Ref value 4.377481e+16 : Year 1970
#   Seals      : Ref value 3585581      : Year 1970
#   PenguinsA3 : Ref value 9601772      : Year 1970
#   PenguinsA4 : Ref value 3129394      : Year 1970
#   PenguinsA5 : Ref value 18152337     : Year 1970
#   FishA2F1   : Ref value 2.863589e+12 : Year 1970
#   FishA2F2   : Ref value 21865838835  : Year 1970
#   FishA2F3   : Ref value 51845299214  : Year 1970
#   FishA3F1   : Ref value 52260370212  : Year 1970
#   Whales     : Ref value 73699.23     : Year 1970

# Krill Initial abundance Multiplier 1E1
#   Krill      : Ref value 1.276383e+16 : Year 1970
#   Seals      : Ref value 1050430 : Year 1970
#   PenguinsA3 : Ref value 9413186 : Year 1970
#   PenguinsA4 : Ref value 3077809 : Year 1970
#   PenguinsA5 : Ref value 13418450 : Year 1970
#   FishA2F1   : Ref value 2.814509e+12 : Year 1970
#   FishA2F2   : Ref value 16667521870 : Year 1970
#   FishA2F3   : Ref value 51418718092 : Year 1970
#   FishA3F1   : Ref value 51977035142 : Year 1970
#   Whales     : Ref value 73455.42 : Year 1970