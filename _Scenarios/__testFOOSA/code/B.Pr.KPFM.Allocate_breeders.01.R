# B.Pr.KPFM.Allocate_breeders.01
if (!isGeneric("allocateBreeders"))
setGeneric("allocateBreeders", function(element, universe) standardGeneric("allocateBreeders"))
setMethod("allocateBreeders", signature(element="Predator", universe="Universe"),  
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           B.Pr.KPFM.Allocate_breeders.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         3 July 2008
		# Description:        General predator function -
		# data set (dSet) requirements
		#   StageNonbreeder   number of the stage that is designated as nonbreeder
		#   StageBreeder      number of the stage that is designated as breeder
		#   SSMUdest  matrix(c( # proportion of breeders
		#             from origin breeding SSMU (rows) going to destination SSMU (cols)

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
	
		Breeders <- dSet$SSMUdest # proportions of origin ending up in destination - will be turned into numbers
		NonBreedersToBreed <- rep(0, getSlot(element, "polygonsN"))
		NonBreeders <- NonBreedersToBreed

		for (pn in 1:getSlot(element, "polygonsN")) {   # from origin polygons (rows) to destination polygons (cols)

			# determine proportion of nonbreeders from polygon to attend breeding sites
			Qbar <- elemState$Cond.R[[pn]]$FoodRealised[dSet$StageNonbreeder] /
										elemState$Cond.R[[pn]]$FoodMax[dSet$StageNonbreeder]
          
			if (Qbar > 1) Qbar <- 1

			# reset reproductive condition to what should be remaining after allocation i.e.
			# how much might be left over to support breeding success if needed
			elemState$Cond.R[[pn]]$FoodRealised[dSet$StageNonbreeder] <- elemState$Cond.R[[pn]]$FoodRealised[dSet$StageNonbreeder] * dSet$RepConditionRemaining
			elemState$Cond.R[[pn]]$FoodMax[dSet$StageNonbreeder] <- elemState$Cond.R[[pn]]$FoodMax[dSet$StageNonbreeder] * dSet$RepConditionRemaining
			elemState$Cond.R[[pn]]$FoodRealised[dSet$StageBreeder] <- elemState$Cond.R[[pn]]$FoodRealised[dSet$StageNonbreeder]
			elemState$Cond.R[[pn]]$FoodMax[dSet$StageBreeder] <- elemState$Cond.R[[pn]]$FoodMax[dSet$StageNonbreeder]

			# number of non-breeders to be removed from Origin Polygon
			NonBreeders[pn] <- elemState$Abundance$num.ind[pn] * elemState$Stage[[pn]][dSet$StageNonbreeder,2]
			NonBreedersToBreed[pn] <- NonBreeders[pn] * dSet$maxPropBreeders[pn] * Qbar^dSet$Phi[pn]
			NonBreeders[pn] <- NonBreeders[pn] - NonBreedersToBreed[pn]
			
			# number of breeders allocated to destination polygons
			Breeders[pn,] <- Breeders[pn,] * NonBreedersToBreed[pn]
		}
		
		#-------------------------------------------------------------------------------
		# update abundance and size structure of predator in polygon
		for (pn in 1:getSlot(element, "polygonsN")) {
			NatStage <- elemState$Abundance$num.ind[pn] * elemState$Stage[[pn]][,2]
			NatStage[dSet$StageNonbreeder] <- NonBreeders[pn] # non breeders lost from polygon
			NatStage[dSet$StageBreeder]   <- sum(Breeders[,pn]) # breeders gained to polygon
			elemState$Abundance$num.ind[pn] <- sum(NatStage)
			elemState$Stage[[pn]][,2] <- NatStage / elemState$Abundance$num.ind[pn]
			elemState$Abundance$mass[pn] <- sum(NatStage * elemState$Cond.S[[pn]])

			# determine proportion of breeders in each destination polygon that originated
			# from each of the origin polygons
			Breeders[,pn] <- Breeders[,pn] / sum(Breeders[,pn])
		}

		# update state with original distribution of breeders
		elemState$BreederOrigin <- Breeders

		# Update state for the element of universe
		setState(element, value=elemState)
	}
)

