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
    Eggs<-Gonad[1,]*0

		for (st in 1:elemState$StageN) Eggs<-Eggs+Gonad[st,]%*%dSet$EggProdn

    # alter the biomass and frequency if needed of the stage in the polygon

    for (pn in 1:getSlot(element, "polygonsN")){
      if(elemState$StageStrUnits==1){  # if primary units are abundance in number

            NatStage<-elemState$Abundance[[1]][pn]*elemState$Stage[,pn]
            Young <- NatStage[1]+Eggs[pn]
            elemState$Cond.S[1,pn] <- (elemState$Cond.S[1,pn]*NatStage[1]+Eggs[pn]*dSet$Cond.S[pn])/Young
            NatStage[1]<-Young
            elemState$Abundance[[1]][pn]<-sum(NatStage)
            elemState$Abundance[[2]][pn]<-sum(NatStage*elemState$Cond.S[,pn])
            elemState$Stage[[pn]][,2]<-NatStage/elemState$Abundance[[1]][pn]

         } else { # if primary units are abundance in mass
            NatStage <- elemState$Abundance[[2]][pn]*elemState$Stage[,pn]/elemState$Cond.S[,pn]
            BatStage <- NatStage*elemState$Cond.S[,pn]
            Young    <- BatStage[1]+Eggs[pn]
            elemState$Cond.S[1,pn] <- (elemState$Cond.S[1,pn]*BatStage[1]+Eggs[pn]*dSet$Cond.S[pn])/Young
            BatStage[1]<-Young
            elemState$Abundance[[2]][pn]<-sum(BatStage)
            elemState$Abundance[[1]][pn]<-sum(BatStage/elemState$Cond.S[,pn])
            elemState$Stage[,pn]<-BatStage/elemState$Abundance[[2]][pn]
         } # end if else
       elemState$Cond.R[,pn]  <- 0
       elemState$Cond.H[1,pn] <- dSet$Cond.H[pn]
       } # end pn

		# Update state for the element of universe
		setState(element, value=elemState)
	}
)

