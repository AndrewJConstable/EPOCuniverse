# B.Pr.KPFM.BreedersToNonbreeders.01<-function(
if (!isGeneric("breedersToNonbreeders")) 
setGeneric("breedersToNonbreeders", function(element, universe) standardGeneric("breedersToNonbreeders"))
setMethod("breedersToNonbreeders", signature(element="Predator", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           B.Pr.KPFM.BreedersToNonbreeders.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         23 June 2008
		# Description:        General predator function -
		#-------------------------------------------------------------------------------
		# data set (dSet) requirements
		#   StageNonbreeder   number of the stage that is designated as nonbreeder
		#   StageBreeder      number of the stage that is designated as breeder

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
		Breeders <- elemState$BreederOrigin
		for (pn in 1:getSlot(element, "polygonsN")) {   # from origin polygons

			# number of breeders to be returned
			Breeders[,pn]<-Breeders[,pn] *
						   elemState$Abundance$num.ind[pn] *
						   elemState$Stage[[pn]][dSet$StageBreeder,2]
		}

		elemState$Cond.R[[pn]]$FoodRealised[dSet$StageBreeder] <- 0
		elemState$Cond.R[[pn]]$FoodMax[dSet$StageBreeder] <- 0

		# update abundance and size structure of predator in polygon
		for (pn in 1:getSlot(element, "polygonsN")){
			NatStage <- elemState$Abundance$num.ind[pn] * elemState$Stage[[pn]][,2]
			NatStage[dSet$StageNonbreeder] <- NatStage[dSet$StageNonbreeder] + sum(Breeders[pn,])
			NatStage[dSet$StageBreeder] <- 0
			elemState$Abundance$num.ind[pn] <- sum(NatStage)
			elemState$Stage[[pn]][,2] <- NatStage / elemState$Abundance$num.ind[pn]
			elemState$Abundance$mass[pn] <- sum(NatStage * elemState$Cond.S[[pn]])
		} 

		# update state with original distribution of breeders
		elemState$BreederOrigin <- dSet$Breeders

		# Update state for the element of universe
		setState(element, value=elemState)
	}
)
