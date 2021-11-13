#B.Pr.KPFM.printState.01<-function(
setMethod("printState", signature(element="Predator", universe="Universe"), 
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
		PredState <- getFunctionData(element, "printState")
		
		# print number
		if (PredState$dset$Number$output) {
			fileConn <- getFileConnection(element, "abundNState", getRuntimePath(universe, PredState$dset$Number$fname), "a")
			
			# print line in file - Trial,Year,Day,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentTrial"),
								getRTState(universe, "currentYear"),
								getRTState(universe, "currentPeriod"),
								periodInfo$PeriodStart,
								asCSVCharacter(getState(element, "Abundance")$num.ind),
								sep=",", conn=fileConn)
		}

		# print biomass
		if(PredState$dset$Biomass$output) {
			fileConn <- getFileConnection(element, "abundBState", getRuntimePath(universe, PredState$dset$Biomass$fname), "a")
			
			# print line in file - Trial,Year,Day,vector of polygon values of krill abundance
			writeFileConnection(element, getRTState(universe, "currentTrial"),
								getRTState(universe, "currentYear"),
								getRTState(universe, "currentPeriod"),
								periodInfo$PeriodStart,
								asCSVCharacter(getState(element, "Abundance")$mass),
								sep=",", conn=fileConn)
		}

		if(PredState$dset$Stage$output | PredState$dset$Reprod_Cond$output | PredState$dset$Health$output) {

			# combined file based on details under Stage
			fileConn <- getFileConnection(element, "stageState", getRuntimePath(universe, PredState$dset$Stage$fname), "a")
    
			# print line in file - Trial,Year,Day,polygon, stage, quantity, reprod_cond, Health
			for (pn in 1:getSlot(element, "polygonsN")){
				for (st in 1:elemState$StageN){
					ReprodCondition <- ifelse(elemState$Cond.R[[pn]]$FoodMax[st] > 0
										,(elemState$Cond.R[[pn]]$FoodRealised[st] /
										  elemState$Cond.R[[pn]]$FoodMax[st]),NA)
					Health <- ifelse(elemState$Cond.H[[pn]]$FoodMax[st] > 0
										,(elemState$Cond.H[[pn]]$FoodRealised[st] /
										  elemState$Cond.H[[pn]]$FoodMax[st]),NA)
					writeFileConnection(element, getRTState(universe, "currentTrial"),
										getRTState(universe, "currentYear"),
										getRTState(universe, "currentPeriod"),
										periodInfo$PeriodStart,
										getPolygons(element)[pn],
										st,
										elemState$Abundance$num.ind[pn] * elemState$Stage[[pn]][st,2],
										ReprodCondition,
										Health,
										sep=",", conn=fileConn)
				}
			}
		}
	}
)

