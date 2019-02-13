#B.GET.printState.01<-function(
setMethod("printState", signature(element="GET", universe="Universe"),
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

#-------------------------------------------------------------------------------
# 1. Population number

		if (dSet$Number$output) {
			fileConn <- getFileConnection(element, "abundNState", getRuntimePath(universe, dSet$Number$fname), "a")
			
			# print line in file - Scenario,Year,Day,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentScenario"),
								getRTState(universe, "currentYear"),
								getRTState(universe, "currentPeriod"),
								periodInfo$PeriodStart,
								asCSVCharacter(getState(element, "Abundance")$num.ind),
								sep=",", conn=fileConn)
		}

#-------------------------------------------------------------------------------
# 2. Population biomass

		if(dSet$Biomass$output) {
			fileConn <- getFileConnection(element, "abundBState", getRuntimePath(universe, dSet$Biomass$fname), "a")
			
			# print line in file - Scenario,Year,Day,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentScenario"),
								getRTState(universe, "currentYear"),
								getRTState(universe, "currentPeriod"),
								periodInfo$PeriodStart,
								asCSVCharacter(getState(element, "Abundance")$mass),
								sep=",", conn=fileConn)
		}

#-------------------------------------------------------------------------------
# x. Condition - Health

		if(dSet$Stage$output | dSet$Size$output | dSet$Reprod_Cond$output | dSet$Health$output) {

			# combined file based on details under Stage
			fileConn <- getFileConnection(element, "stageState", getRuntimePath(universe, dSet$Stage$fname), "a")
    
			# print line in file - Scenario,Year,Day,polygon, stage, quantity, reprod_cond, Health
			for (pn in 1:getSlot(element, "polygonsN")){
				for (st in 1:elemState$StageN){
					Size <- ifelse(dSet$Size$output,elemState$Cond.S[st,pn],NA)
					ReprodCondition <- ifelse(dSet$Reprod_Cond$output,ifelse(elemState$Cond.R[[pn]]$Max[st] > 0
										,(elemState$Cond.R[[pn]]$Realised[st] /
										  elemState$Cond.R[[pn]]$Max[st]),NA),NA)
					Health <- ifelse(dSet$Health$output,ifelse(elemState$Cond.H[[pn]]$Max[st] > 0
										,(elemState$Cond.H[[pn]]$Realised[st] /
										  elemState$Cond.H[[pn]]$Max[st]),NA),NA)
					writeFileConnection(element, getRTState(universe, "currentScenario"),
										getRTState(universe, "currentYear"),
										getRTState(universe, "currentPeriod"),
										periodInfo$PeriodStart,
										getPolygons(element)[pn],
										st,
										elemState$Abundance$num.ind[pn] * elemState$Stage[st,pn],
                    Size,
										ReprodCondition,
										Health,
										sep=",", conn=fileConn)
				}
			}
		}
	}
)

