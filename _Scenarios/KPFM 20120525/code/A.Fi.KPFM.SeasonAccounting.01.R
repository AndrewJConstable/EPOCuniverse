#A.Fi.KPFM.SeasonAccounting.01<-function(
if (!isGeneric("seasonAccounting")) 
setGeneric("seasonAccounting", function(element, universe) standardGeneric("seasonAccounting"))
setMethod("seasonAccounting", signature(element="Fishery", universe="Universe"),  
    function(
		element,
		universe	# access to universe if needed
		)     
    {
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
		printParams <- getFunctionData(element, "printState")$dset

		if (getRTState(universe, "currentYear") >= getTrial(universe, item="firstFishingYear") &
			getRTState(universe, "currentYear") <= getTrial(universe, item="lastFishingYear")) {

			if (printParams$Catch$output & getState(element, "TargetN") > 0) {
				
				fileConn <- getFileConnection(element, "State", getRuntimePath(universe, printParams$Catch$fname), "a")
				for (target in 1:elemState$TargetN) {
					for (pn in 1:getSlot(element, "polygonsN")) {
    
						Efficiency <- NA

						if (elemState$Efficiency[[target]][pn,2]>0) Efficiency <- elemState$Efficiency[[target]][pn,1] / elemState$Efficiency[[target]][pn,2]
						
						# print  trial,year,fraction of year to start period, target sp module, target sp abs ID, polygon, catch, efficiency
						writeFileConnection(element, getRTState(universe, "currentTrial"),
										getRTState(universe, "currentYear"),
										periodInfo$PeriodStart,
										elemState$TACtaxa[target,1],
										getSignature(universe$modules[[elemState$TACtaxa[target,1]]][[elemState$TACtaxa[target,2]]], "ID.absolute"),
										getPolygons(element)[pn],
										elemState$Catch[target,pn],
										Efficiency, 
										sep=",", conn=fileConn)

						# reset catch and efficiency to 0
						elemState$Catch[target,pn] <- 0
						elemState$Efficiency[[target]][pn,1] <- 0
						elemState$Efficiency[[target]][pn,2] <- 0
					}
				}

				# Update state
				setState(element, value=elemState)
			}
		}
	}
)
