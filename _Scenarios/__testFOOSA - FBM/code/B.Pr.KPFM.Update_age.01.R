#B.Pr.KPFM.Update_age.01<-function(
if (!isGeneric("updateAge")) 
setGeneric("updateAge", function(element, universe) standardGeneric("updateAge"))
setMethod("updateAge", signature(element="Predator", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           B.Pr.KPFM.Update_age.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         2 July 2008
		# Description:        General predator function -
		
		#     For each polygon, advance juveniles by one age with the oldest juveniles
		#     put into non-breeders
	
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
		
		# by polygon and by stage
		for (pn in 1:getSlot(element, "polygonsN")) {
			for (st in (elemState$StageN - 2):1) {
				# numbers do not change - just proportions
				# advance age
				elemState$Stage[[pn]][(st+1),2] <- elemState$Stage[[pn]][st,2]
				elemState$Stage[[pn]][st,2] <- 0
			}
      
			# alter biomass
			elemState$Abundance$mass[pn] <- sum(elemState$Stage[[pn]][,2] *
												  elemState$Abundance$num.ind[pn] *
												  elemState$Cond.S[[pn]])
		}
		
		# Update state for the element of universe
		setState(element, value=elemState)
	}
)