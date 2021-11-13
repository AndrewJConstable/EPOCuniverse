#B.Pr.KPFM.Reproduce.01<-function(
if (!isGeneric("reproduce")) 
setGeneric("reproduce", function(element, universe) standardGeneric("reproduce"))
setMethod("reproduce", signature(element="Predator", universe="Universe"), 
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           B.Pr.KPFM.Reproduce.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         3 July 2008
		# Description:        General predator function -
	
		# data set (dSet) requirements]
		#   StageBreeder      number of the stage that is designated as breeder
		#   RepConditionRemaining   proportion of reproductive condition to remain after reproduction
		#   offspring mortality parameters - vector for polygons
		#        M = nominal mortality of offspring over breeding period
		#        z = max proportion of nominal mortality that is subject to variation
		#        v = effect of density dependence on dependent variable
		#        alpha = maximum reproductive rate per female
		#        propfemale = proportion of breeding population that is female

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

		for (pn in 1:getSlot(element, "polygonsN")){   # from origin polygons

			 FemaleOutput <- dSet$propfemale[pn] *
							  dSet$alpha[pn] *
							  elemState$Abundance$num.ind[pn] *
							  elemState$Stage[[pn]][dSet$StageBreeder,2]

			# determine proportion of nonbreeders from polygon to attend breeding sites
			Qbar <- 1  # set Qbar =1 if no data on reproductive condition
			
			if (elemState$Cond.R[[pn]]$FoodMax[dSet$StageBreeder] > 0) {
				Qbar <- elemState$Cond.R[[pn]]$FoodRealised[dSet$StageBreeder] / elemState$Cond.R[[pn]]$FoodMax[dSet$StageBreeder]

				if (Qbar>1) Qbar<-1
     
				# reset reproductive condition to what should be remaining after allocation i.e.
				# how much might be left over to support breeding success if needed
				elemState$Cond.R[[pn]]$FoodRealised[dSet$StageBreeder] <- elemState$Cond.R[[pn]]$FoodRealised[dSet$StageBreeder] * dSet$RepConditionRemaining
				elemState$Cond.R[[pn]]$FoodMax[dSet$StageBreeder] <- elemState$Cond.R[[pn]]$FoodMax[dSet$StageBreeder] * dSet$RepConditionRemaining
			} else {
				epocMessage(element, "Reproductive Condition - max food = 0 - reproductive output may not be reliable")
			}
  
			# Offspring
			Offspring <- FemaleOutput *
					  (exp(dSet$M[pn]) + dSet$z[pn] *
					  (Qbar^(-dSet$M[pn] / log(dSet$v[pn])) - exp(-dSet$M[pn])))

			# add to juvenile class 1 (Age 0) and update size structure and mass		
			NatStage <- elemState$Abundance$num.ind[pn] * elemState$Stage[[pn]][,2]
			NatStage[1] <- Offspring
			elemState$Abundance$num.ind[pn] <- sum(NatStage)
			elemState$Stage[[pn]][,2] <- NatStage / elemState$Abundance$num.ind[pn]
			elemState$Abundance$mass[pn] <- sum(NatStage * elemState$Cond.S[[pn]])
		}
		
		# Update state for the element of universe
		setState(element, value=elemState)
	}
)

