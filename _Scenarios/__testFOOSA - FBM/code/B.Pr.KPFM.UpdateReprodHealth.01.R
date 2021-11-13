#B.Pr.KPFM.UpdateReprodHealth.01<-function(
if (!isGeneric("updateReprodHealth")) 
setGeneric("updateReprodHealth", function(element, universe) standardGeneric("updateReprodHealth"))
setMethod("updateReprodHealth", signature(element="Predator", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
      
		# Function:           B.Pr.KPFM.UpdateReprodHealth.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         2 July 2008
		# Description:        General predator function -

		# Condition is separated by stage within polygon of origin
		#     Taxon$State$Cond.R
		#            list(Polygon 1 = list (FoodRealised = vector stages
		#                                  ,FoodMax      = vector stages
		#                                   ) # end list for polygon
		#                 ) # end list for condition
		# condition for a stage in a polygon is read as FoodRealised/FoodMax
		# data are accumulated from Transition matrix

		# Transition$Consumption
		#    col
		#      1. mortality source local population (relative in element)
		#      2. mortality source stage
		#      3. prey module
		#      4. prey element
		#      5. prey polygon (relative in element)
		#      6. units of qnty
		#      7. stage
		#      8. stage realised quantity
		#      9. stage maximum quantity
		#     10. stage size
		#     11. calendar year
		#     12. period
		#     13. proportion of year
		################################################################################

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
		elemTrans <- getTransition(element)
		preyElement <- getTimestep(element, action[3])$actions[[action[5]]]$relatedIndexes
		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset
		Consumed <- elemTrans$Consumption

		# by polygon by stage
		for (pn in 1:getSlot(element, "polygonsN")){
			for (st in 1:elemState$StageN){
				# sum the aggregate prey value consumed by each stage for each polygon of origin
				StageRealisedFood <- 0
				StageMaxFood <- 0
      
				for(prey in 1:length(dSet$FoodValue)){
					StageRealisedFood <- StageRealisedFood +
										  dSet$FoodValue[prey] *
										  sum(Consumed[Consumed[,1] == pn & Consumed[,2] == st
													   & Consumed[,3] == preyElement[prey,1]
													   & Consumed[,4] == preyElement[prey,2]
													   ,8]) # realised prey quantity
					StageMaxFood <- StageMaxFood+
									  dSet$FoodValue[prey]*
									  sum(Consumed[Consumed[,1] == pn & Consumed[,2] == st
												   & Consumed[,3] == preyElement[prey,1]
												   & Consumed[,4] == preyElement[prey,2]
												   ,9]) # realised prey quantity
				}
				
				# add results to existing reproductive and health conditions by polygon and life stage
				 elemState$Cond.R[[pn]]$FoodRealised[st] <- elemState$Cond.R[[pn]]$FoodRealised[st] + StageRealisedFood
				 elemState$Cond.R[[pn]]$FoodMax[st] <- elemState$Cond.R[[pn]]$FoodMax[st] + StageMaxFood
				 elemState$Cond.H[[pn]]$FoodRealised[st] <- elemState$Cond.H[[pn]]$FoodRealised[st] + StageRealisedFood
				 elemState$Cond.H[[pn]]$FoodMax[st] <- elemState$Cond.H[[pn]]$FoodMax[st] + StageMaxFood
			}
		}

		# Update state for the element of universe
		setState(element, value=elemState)
		setTransition(element, "Consumption", value=NULL)
	}
)
