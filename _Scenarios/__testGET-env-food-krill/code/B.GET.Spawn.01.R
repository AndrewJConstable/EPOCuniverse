#B.GET.Spawn.01<-function(
if (!isGeneric("spawn"))
setGeneric("spawn", function(element, universe) standardGeneric("spawn"))
setMethod("spawn", signature(element="GET", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           B.GET.Spawn.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         15 June 2015
		# Description:        Spawning direct from reproductive condition to Stage 1 (adds to Stage 1)
	
		# data set (dSet) requirements]
		#   GonadToEggs    cost of producing an egg from mass of gonad (i.e. Eggs = Cond.R/GonadToEggs)
    #   EggDistn       matrix (rows = source, cols = destination) of the distribution of eggs from source polygons to other polygons

		# Get a handle on some necessary universe and element state data
		action <- getRTState(universe, "currentAction")
		# ActionMat row
		# Col  1  = module
		# Col  2  = element
		# Col  3  = period
		# Col  4  = reference day in year
		# Col  5  = action reference number in period (NA if no actions)
		# Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions)
		periodInfo <- getRTState(universe, "currentPeriodInfo")
		# periodInfo  # information about the active period for use in subroutines
		# Number      = eTSD
		# Day         = PropYear[eTSD,1]
		# KnifeEdge   = if(PropYear[eTSD,2]==0) FALSE else TRUE
		# YearPropn   = PropYear[eTSD,3]
		# PeriodStart = PreviousDay/365 # proportion of year passed since 0 Jan
		#                               # to beginning of time period
		# PeriodEnd   = PreviousDay/365+PropYear[eTSD,3]
		elemState <- getState(element)
		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset

		#-------------------------------------------------------------------------------
    Gonad<-elemState$Cond.R  # [st,pn]


    # gives vector of eggs per polygon


    # alter the biomass and frequency if needed of the stage in the polygon

    Eggs<-NULL
    for (pn in 1:getSlot(element, "polygonsN")){
      if(elemState$StageStrUnits==1){  # if primary units are abundance in number
            Clutch<-0
            for (pnO in 1:getSlot(element, "polygonsN")){
                NatStagePNO<-elemState$Abundance[[1]][pnO]*elemState$Stage[,pnO]
        		    for (st in 1:elemState$StageN) Clutch<-Clutch+(elemState$Cond.S[st,pnO]*Gonad[st,pnO]*NatStagePNO[st]*dSet$EggProdn[pnO,pn])  # this gives eggs per gWW of adults
                }
         } else { # if primary units are abundance in mass
            Clutch<-0
            for (pnO in 1:getSlot(element, "polygonsN")){
                BatStagePNO<-elemState$Abundance[[2]][pnO]*elemState$Stage[,pnO]
        		    for (st in 1:elemState$StageN) Clutch<-Clutch+(Gonad[st,pnO]*BatStagePNO[st]*dSet$EggProdn[pnO,pn])  # this gives eggs per gWW of adults
                }
         } # end if else
         Eggs<-c(Eggs,Clutch)
       } # end pn


    for (pn in 1:getSlot(element, "polygonsN")){
      if(elemState$StageStrUnits==1){  # if primary units are abundance in number
            NatStage<-elemState$Abundance[[1]][pn]*elemState$Stage[,pn]
            TotalStage1<-NatStage[1]+Eggs[pn]
            elemState$Cond.S[1,pn] <- if(TotalStage1>0) {  # weighted mean
                 (NatStage[1]*elemState$Cond.S[1,pn]+Eggs[pn]*dSet$Cond.S[pn])/TotalStage1
                 } else 0
            NatStage[1] <- TotalStage1

            elemState$Abundance[[1]][pn]<-sum(NatStage)
            elemState$Abundance[[2]][pn]<-sum(NatStage*elemState$Cond.S[,pn])
            elemState$Stage[,pn] <- if(elemState$Abundance[[1]][pn]>0) NatStage/elemState$Abundance[[1]][pn] else 1/elemState$StageN

         } else { # if primary units are abundance in mass
              BatStage<-elemState$Abundance[[2]][pn]*elemState$Stage[,pn]  # first part
              CondS<-elemState$Cond.S[,pn]
              NatStage<-BatStage*0
              NatStage[CondS>0]<- BatStage[CondS>0]/CondS[(CondS[,pn]>0),pn]
              Young    <- NatStage[1]+Eggs[pn]
              elemState$Cond.S[1,pn] <- if(Young>0) (elemState$Cond.S[1,pn]*NatStage[1]+Eggs[pn]*dSet$Cond.S[pn])/Young else 0
              NatStage[1]<-Young
              BatStage[1]<-Young*elemState$Cond.S[1,pn]
              elemState$Abundance[[2]][pn]<-sum(BatStage)
              CondS<-elemState$Cond.S[,pn]
              elemState$Abundance[[1]][pn]<-sum(NatStage)
              elemState$Stage[,pn]<-if(elemState$Abundance[[2]][pn]>0) BatStage/elemState$Abundance[[2]][pn] else 1/elemState$StageN
         } # end if else
       elemState$Cond.R[,pn]  <- 0
       elemState$Cond.H[1,pn] <- dSet$Cond.H[pn]
       } # end pn




		# Update state for the element of universe
		setState(element, value=elemState)
	}
)

