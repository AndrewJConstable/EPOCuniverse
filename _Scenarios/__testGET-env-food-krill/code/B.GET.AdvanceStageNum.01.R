#B.GET.advanceStageNum.01<-function(
# add numbers in one stage to another stage and assume new size - leave zero in previous stage

if (!isGeneric("advanceStageNum"))
setGeneric("advanceStageNum", function(element, universe) standardGeneric("advanceStageNum"))
setMethod("advanceStageNum", signature(element="GET", universe="Universe"),
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

		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset

		# by polygon and by stage
		for (pn in 1:getSlot(element, "polygonsN")) {
      elemState$Stage[,pn] <-elemState$Stage[,pn]%*%dSet[[pn]]
			# alter biomass
			elemState$Abundance$mass[pn] <- sum(elemState$Stage[,pn] *
												  elemState$Abundance$num.ind[pn] *
												  elemState$Cond.S[,pn])
		}
		
		# Update state for the element of universe
		setState(element, value=elemState)
	}
)