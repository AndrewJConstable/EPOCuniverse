# Create S4 method 'printState'
#B.PP.GenBms.printState.01<-function(
setMethod("printState", signature(element="PPgenBMS", universe="Universe"),
    function(element, universe) {

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
		dSet <- getFunctionData(element, "printState")$dset
		
		# print biomass
		if(dSet$Biomass$output) {
			fileConn <- getFileConnection(element, "abundBState", getRuntimePath(universe, dSet$Biomass$fname), "a")
			# print line in file - Scenario,Year,Day,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentScenario"),
							getRTState(universe, "currentYear"),
							getRTState(universe, "currentPeriod"),
							periodInfo$PeriodEnd,
							asCSVCharacter(elemState$Abundance$mass), 
							sep=",", conn=fileConn)
		} 

	}
)