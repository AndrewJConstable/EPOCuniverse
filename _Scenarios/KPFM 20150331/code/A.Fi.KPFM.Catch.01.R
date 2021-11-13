#A.Fi.KPFM.Catch.01<-function(
if (!isGeneric("catch")) 
setGeneric("catch", function(element, universe) standardGeneric("catch"))
setMethod("catch", signature(element="Fishery", universe="Universe"), 
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
		elemTrans <- getTransition(element)
		elemTS <- getTimestep(element, action[3])$actions[[action[5]]]
		dSet <- elemTS$dset
		# references for determine catch remaining in polygons from manager (convert to fishery polygons)
		manager <- elemTS$relatedIndexes[dSet$ManagerRef,]
		
		RemainingCatch <- NULL
		ManagerPolygons <- NULL

		if (manager[1] > 0 && manager[2] > 0) {
			RemainingCatch <- getState(universe$modules[[manager[1]]][[manager[2]]], "SSMU.CatchRemaining")
			ManagerPolygons <- getPolygons(universe$modules[[manager[1]]][[manager[2]]])
		}

		if (!is.null(RemainingCatch)) { # NULL means no fishing

			for (pn in 1:getSlot(element, "polygonsN")) {   # from origin polygons

				Result <- NULL
				for (target in 1:dSet$TargetTaxaRefN) {

					# determine catch available to be caught - if none then skip
					# determine amount of TAC remaining
					targetRE <- elemTS$relatedIndexes[dSet$TargetTaxaRef[target],]
					targetElem <- universe$modules[[targetRE[1]]][[targetRE[2]]]
					TACtaxonRef <- which(getState(universe$modules[[manager[1]]][[manager[2]]], "SSMU.TAC.taxa")[,1]==targetRE[1]
									& getState(universe$modules[[manager[1]]][[manager[2]]], "SSMU.TAC.taxa")[,2]==targetRE[2])

					if (sum(TACtaxonRef) > 0) { # only limit catch if taxon is present in manager
						CatchRemaining <- RemainingCatch[[TACtaxonRef]]
						RemainCatchPN <- CatchRemaining[ManagerPolygons==getPolygons(element)[pn]]
					} else {
						RemainCatchPN <- NA
					}

					if (!is.null(RemainCatchPN) & !is.null(elemState$SpaceTimePattern) & (is.na(RemainCatchPN) | RemainCatchPN>0)) {

						for(targetPN in 1:dSet$Target.Taxa[[target]]$TargetPn[[pn]]$PnN){
							# determine the relative reference in prey element for the target polygon
							targetPolygon <- dSet$Target.Taxa[[target]]$TargetPn[[pn]]$Pns[targetPN,1]
							targetPolygon <- which(getPolygons(targetElem)==targetPolygon)

							# available quantities in each stage class of target
							#     Holling_availability is used to establish density of available prey
							if (dSet$Target.Taxa[[target]]$Holling_units == getState(targetElem, "StageStrUnits")){
								AvailableTargetStr <- dSet$Target.Taxa[[target]]$Holling_availability *
									getState(targetElem, "Abundance")[[dSet$Target.Taxa[[target]]$Holling_units]][targetPolygon] *
									getState(targetElem, "Stage")[[targetPolygon]][,2]
								AvailableTargetTotal <- sum(AvailableTargetStr)
							} else {
								epocErrorMessage(element, "Holling units not the same as target stage structure units - must change", halt=TRUE)
							}

							# establish Holling value according to available target
							TargetPNarea <- getSpatial(universe, "polygonAreas")[getPolygons(targetElem)[targetPolygon]]
							TargetDensity <- AvailableTargetTotal/TargetPNarea
							Holling <- (TargetDensity^(dSet$Target.Taxa[[target]]$Holling_q+1)) / (dSet$Target.Taxa[[target]]$Holling_D+(TargetDensity^(dSet$Target.Taxa[[target]]$Holling_q+1)))

							# determine target caught as a vector of quanitities for target stages
							PNtargetMax <- dSet$Target.Taxa[[target]]$PerVessel* # max per capita consumption
										   dSet$Target.Taxa[[target]]$TargetPn[[pn]]$Pns[targetPN,2] # proportion of target polygon in predator polygon

							# protect from divide by 0
							if (AvailableTargetTotal > 0) AvailableTargetStr <- AvailableTargetStr / AvailableTargetTotal

							PNtargetMax <- PNtargetMax * AvailableTargetStr
							PNtarget <- PNtargetMax * Holling

							# adjust PNtargetMax and PNtarget by the effort applied to the polygon in that period
							PNtargetMax <- PNtargetMax * elemState$SpaceTimePattern[pn, periodInfo$Number]
							PNtarget <- PNtarget * elemState$SpaceTimePattern[pn, periodInfo$Number]
							CatchEst <- PNtarget * getState(targetElem, "Cond.S")[[targetPolygon]] * getState(targetElem, "Abundance")$mass.unit

							# if amount caught less than or equal to TAC remaining then record catch as numbers by stage
							# else adjust numbers at stage to give correct TAC remaining
							if(!is.na(RemainCatchPN) & CatchEst>RemainCatchPN) {
								Adj <- CatchEst
								if(CatchEst>0) Adj <- RemainCatchPN/CatchEst
								PNtarget <- PNtarget * Adj
								PNtargetMax <- PNtargetMax * Adj
							}							
							
							# concatenate results for use later
							for (pst in 1:getState(targetElem, "StageN")){
								Result <- c(Result,
											target            	# 1. number of target in relatedElements
											 ,targetPolygon     # 2. target polygon
											 ,pst             	# 3. target stage
											 ,getState(targetElem, "StageStrUnits") # 4. stage units
											 ,PNtarget[pst] 	# 5. stage quantity
											 ,PNtargetMax[pst] 	# 6. maximum stage quantity possible
										)
							}
						}
					}
				}				
				
				# add to catch in catch by fishery only if there are results
				if (!is.null(Result)) {

					Result <- matrix(Result,ncol=6,byrow=TRUE)
					ResCol1 <- unique(Result[,1])
					ResCol2 <- unique(Result[,2])
					ResCol3 <- unique(Result[,3])

					for (r1 in 1:length(ResCol1)){
						for (r2 in 1:length(ResCol2)){
							for (r3 in 1:length(ResCol3)){
								SumRef <- (Result[,1]== ResCol1[r1] &
										   Result[,2]== ResCol2[r2] &
										   Result[,3]== ResCol3[r3])
								Sum <- sum(Result[SumRef,5])
								SumMax <- sum(Result[SumRef,6])
								Units <- ifelse(SumMax>0,mean(Result[SumRef,4]),NA)
								if (SumMax>0) {
									targetRE <- elemTS$relatedIndexes[ResCol1[r1],]
									targetREElement <- getEPOCElement(universe, targetRE[1], targetRE[2])
									mortality <- rbind(getTransition(targetREElement, "Mortality"),
										c(
										   ResCol2[r2]       #    1.   1.  target subject polygon (relative in element)
										  ,Units             #    2.   2.  units of qnty
										  ,ResCol3[r3]       #    3.   3.  stage
										  ,Sum               #    4.   4.  stage quantity
										  ,getState(targetREElement, "Cond.S")[[ResCol2[r2]]][ResCol3[r3]] ** getState(targetREElement, "Abundance")$mass.unit      #    5.   5.  stage size
										  ,action[1]         #    6.   6.  mortality source module
										  ,action[2]         #    7.   7.  mortality source element
										  ,pn                #    8.   8.  mortality source local population (relative in element)
										  ,1                 #    9.   mortality source stage - no stage structure in fishery
										))
									setTransition(targetREElement, "Mortality", value=mortality)
									doUpdate(targetREElement, TRUE)

									elemTrans$Consumption <- rbind(  # Consumption is standard for any element "consuming" another element
										elemTrans$Consumption,c(
															 # column from above
										   pn                #    8.   mortality source local population (relative in element)
										  ,1                 #    9.   mortality source stage - no stage structure in fishery
										  ,targetRE[1]       #    3.   target module
										  ,targetRE[2]       #    4.  target element
										  ,ResCol2[r2]       #    1.   target subject polygon (relative in element)
										  ,Units     		 #    2.   units of qnty
										  ,ResCol3[r3]       #    3.   stage
										  ,Sum               #    4.   stage realised quantity (after competition adjustment in updata_state of target)
										  ,SumMax            #    4.   stage maximum quantity (on first calculation of consumption)
										  ,getState(targetREElement, "Cond.S")[[ResCol2[r2]]][ResCol3[r3]] **
												getState(targetREElement, "Abundance")$mass.unit    #    5.   stage size
										  ,getRTState(universe, "currentYear")  					# 	11. calendar year
										  ,getRTState(universe, "currentPeriod")       				# 	12. period
										  ,periodInfo$YearPropn                       				# 	13. proportion of year
										))
								}
							}
						}
					}
				}
			}
		}

		# Update universe element transition consumption data
		setTransition(element, "Consumption", value=elemTrans$Consumption)
	}
)
