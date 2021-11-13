#M.CL.KPFM.manager.printState.01<-function(
setMethod("printState", signature(element="Manager", universe="Universe"), 
    function(element, universe)	{	
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
		
		# print catch
		if (!is.null(ManagerDset$Catch$output) && ManagerDset$Catch$output && elemState$SSMU.TAC.taxa.N > 0) {
			fileConn <- getFileConnection(element, "State", getRuntimePath(universe, ManagerDset$Catch$fname), "a")
			
			for(target in 1:elemState$SSMU.TAC.taxa.N) {
				for (pn in 1:getSlot(element, "polygonsN")) {

					# print  trial,year,fraction of year to start period, target sp module, target sp abs ID, polygon, TAC, realised catch
					writeFileConnection(element, getRTState(universe, "currentTrial"),
										getRTState(universe, "currentYear"),
										getRTState(universe, "currentPeriod"),
										periodInfo$PeriodStart,
										elemState$SSMU.TAC.taxa[target,1],
										getSignature(universe$modules[[elemState$SSMU.TAC.taxa[target,1]]][[elemState$SSMU.TAC.taxa[target,2]]], "ID.absolute"),
										getPolygons(element)[pn],
										elemState$SSMU.TAC[[target]][pn],
										(elemState$SSMU.TAC[[target]][pn] - elemState$SSMU.CatchRemaining[[target]][pn]),
										sep=",", conn=fileConn)
				}
			}
		}
	}
)

