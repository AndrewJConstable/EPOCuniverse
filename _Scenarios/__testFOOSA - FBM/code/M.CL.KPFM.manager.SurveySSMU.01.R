#M.CL.KPFM.manager.SurveySSMU.01<-function(
if (!isGeneric("surveySSMU")) 
setGeneric("surveySSMU", function(element, universe) standardGeneric("surveySSMU"))
setMethod("surveySSMU", signature(element="Manager", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           M.CL.KPFM.manager.SurveySSMU.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         7 July 2008
		# Description:        Management - SSMU TAC setting process - surveys of nominated SSMUs
	
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
		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset
		rYear <- getRTState(universe, "relativeYear")
	
		Survey <- NA
		for (su in 1:dSet$SurveysN) if(dSet$SurveyDetails[[su]]$Survey.Year == getRTState(universe, "currentYear")) Survey <- su

		if (!is.na(Survey)) { # if there is a survey, do this routine

			# read deviates from file
			fileName <- getRuntimePath(universe, getAttribute(element, "RuntimeFile"))
			fileConn <- getFileConnection(element, "RuntimeFile", fileName, "r")
			PNdeviates <- fromCSVCharacter(readFileConnection(element, conn=fileConn, linenum=7), type="double")
			PNdeviates <- PNdeviates[getSlot(element, "recordElements")]
			Ntargets <- length(dSet$SurveyDetails[[Survey]]$Target.elements)

			SurveyResults <- NULL

			for (pn in 1:getSlot(element, "polygonsN")) {   # from origin polygons

				SurveyAbund <- NULL
				for (target in 1:Ntargets) {

					targetRE <- getTimestep(element, action[3])$actions[[action[5]]]$relatedIndexes[dSet$SurveyDetails[[Survey]]$Target.elements[target],]
					targetElem <- getEPOCElement(universe, targetRE[1], targetRE[2])
					TargetAbund <- 0

					for(targetPN in 1:dSet$SurveyDetails[[Survey]]$Target.details[[target]]$TargetPn[[pn]]$PnN) {

						# determine the relative reference in target element for the target polygon
						targetPolygon <- dSet$SurveyDetails[[Survey]]$Target.details[[target]]$TargetPn[[pn]]$Pns[targetPN,1]
						targetPolygon <- which(getPolygons(targetElem)==targetPolygon)

						TargetPN.BMS <- getState(targetElem, "Abundance")[[2]][targetPolygon]
						TargetPN.prop <- dSet$SurveyDetails[[Survey]]$Target.details[[target]]$TargetPn[[pn]]$Pns[targetPN,2] # proportion of target polygon in predator polygon

						# estimate abundance using Survey Deviates
						# read target polygon sd in log domain (converted from CV during setup)
						SDlog <- getState(targetElem, "Space")[targetPolygon]
						
						# convert abundance to log domain
						TargetPN.survey <- exp(log(TargetPN.BMS * TargetPN.prop) - (SDlog^2) / 2 + PNdeviates[pn] * SDlog)
						TargetAbund <- TargetAbund + TargetPN.survey

					}
					
					SurveyAbund <- c(SurveyAbund,TargetAbund)
				}
				# accumulate abundances of different targets - rows = targets, cols = polygons
				SurveyResults <- rbind(SurveyResults,
									c(getRTState(universe, "currentYear"),
									periodInfo$PeriodStart,
									targetRE[1],
									targetRE[2],
									getPolygons(element)[pn],
									SurveyAbund))
			}

			# save results to file if wanted
			if (dSet$OutputFlag) {
				fileConn <- getFileConnection(element, "State", getRuntimePath(universe, dSet$OutputFile), "a")
				for (sr in 1:nrow(SurveyResults)) {
					writeFileConnection(element, getRTState(universe, "currentScenario"), 
												asCSVCharacter(SurveyResults[sr,]),
												sep=",",conn=fileConn)
				}
			}
			
			# save results in State
			setState(element, "SSMU.Survey.results", rbind(getState(element, "SSMU.Survey.results"), SurveyResults))
		}
	}
)