# Create S4 method 'printState'
#E.O.Env.EnvGenVar.printPeriodState.01<-function(
setMethod("printState", signature(element="EnvGenVar", universe="Universe"),
    function(element, universe ) {
		# Get a handle on some necessary universe and element state data
		periodInfo <- getRTState(universe, "currentPeriodInfo")
		# periodInfo  # information about the active period for use in subroutines
		# Number      = eTSD
		# Day         = PropYear[eTSD,1]
		# KnifeEdge   = if(PropYear[eTSD,2]==0) FALSE else TRUE
		# YearPropn   = PropYear[eTSD,3]
		# PeriodStart = PreviousDay/365 # proportion of year passed since 0 Jan
		#                               # to beginning of time period
		# PeriodEnd   = PreviousDay/365+PropYear[eTSD,3]
		printParams <- getFunctionData(element, "printState")$dset

		if(printParams$State$output) {
			# print line in file - Scenario,Year,Day,vector of polygon values of environment
			fileConn <- getFileConnection(element, "State", getRuntimePath(universe, printParams$State$fname), "a")
			writeFileConnection(element, getRTState(universe, "currentScenario"),
                    getRTState(universe, "currentYear"),
					getRTState(universe, "currentPeriod"),
                    periodInfo$PeriodStart,
                    asCSVCharacter(getState(element, "PolygonEnv")), 
					sep=",", conn=fileConn)

		}
	}
)
