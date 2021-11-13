#M.CL.KPFM.manager.CatchRemaining.01<-function(
if (!isGeneric("catchRemaining")) 
setGeneric("catchRemaining", function(element, universe) standardGeneric("catchRemaining"))
setMethod("catchRemaining", signature(element="Manager", universe="Universe"), 
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           M.CL.KPFM.manager.CatchRemaining.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         7 July 2008
		# Description:        Management - SSMU TAC setting process - surveys of nominated SSMUs
		
		# Note - one survey per year
		
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
		ManagerDset <- getTimestep(element, action[3])$actions[[action[5]]]$dset
		fisheries <- getTimestep(element, action[3])$actions[[action[5]]]$relatedIndexes
		
		# for each TAC taxon, in State list
		# if TAC greater than 0 and not equal to NA then check catch in related elements (fishery)
		# and subtract from TAC to give catch remaining.
		if (elemState$SSMU.TAC.taxa.N > 0) { # only check if there is a fishery
			for (taxon in 1:elemState$SSMU.TAC.taxa.N) {
				targetRE <- elemState$SSMU.TAC.taxa[taxon,]
				targetElem <- universe$modules[[targetRE[1]]][[targetRE[2]]]
				
				for (pn in 1:getSlot(element, "polygonsN")) {
					
					for (fi in 1:nrow(fisheries)) {
						fisheryElem <- universe$modules[[fisheries[fi,][1]]][[fisheries[fi,][2]]]
						
						# read catch for taxon in polygon
						targetPolygon <- getPolygons(element)[pn]
						targetPolygon <- which(getPolygons(fisheryElem)==targetPolygon)
						targetTaxon <- which(getState(fisheryElem, "TACtaxa")[,1]==targetRE[1] &
										getState(fisheryElem, "TACtaxa")[,2]==targetRE[2])

						if (sum(targetPolygon) > 0 & sum(targetTaxon) > 0) {
							elemState$SSMU.CatchRemaining[[taxon]][pn] <- elemState$SSMU.TAC[[taxon]][pn] 
								- getState(fisheryElem, "Catch")[targetTaxon, targetPolygon]
						}
					}
				}
			}
		}
		
		# Update element state
		setState(element, value=elemState)
	}
)