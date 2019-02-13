# Create S4 method 'printState'
#B.MI.Es.KPFM.recruit.01<-function(
setMethod("printState", signature(element="Krill", universe="Universe"), 
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
		KrillDset <- getFunctionData(element, "printState")$dset
		
		# print number
		if(KrillDset$Number$output) {
			fileConn <- getFileConnection(element, "abundNState", getRuntimePath(universe, KrillDset$Number$fname), "a")
			# print line in file - Scenario,Year,Day,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentScenario"),
								getRTState(universe, "currentYear"),
								getRTState(universe, "currentPeriod"),
								periodInfo$PeriodStart,
								asCSVCharacter(elemState$Abundance$num.ind), 
								sep=",", conn=fileConn)
		}

		# print biomass
		if(KrillDset$Biomass$output) {
			fileConn <- getFileConnection(element, "abundBState", getRuntimePath(universe, KrillDset$Biomass$fname), "a")
			# print line in file - Scenario,Year,Day,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentScenario"),
							getRTState(universe, "currentYear"),
							getRTState(universe, "currentPeriod"),
							periodInfo$PeriodStart,
							asCSVCharacter(elemState$Abundance$mass), 
							sep=",", conn=fileConn)
		} 

		# print reproductive condition
		if(KrillDset$Reprod_Cond$output) {
			fileConn <- getFileConnection(element, "condRState", getRuntimePath(universe, KrillDset$Reprod_Cond$fname), "a")
			# print line in file - Scenario,Year,Day,RepCondYear,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentScenario"),
							getRTState(universe, "currentYear"),
							getRTState(universe, "currentPeriod"),
							periodInfo$PeriodStart,
							asCSVCharacter(elemState$Cond.R[1,]), 
							sep=",", conn=fileConn)	
		}
	}
)