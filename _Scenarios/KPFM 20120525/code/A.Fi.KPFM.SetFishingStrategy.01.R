#A.Fi.KPFM.SetFishingStrategy.01<-function(
if (!isGeneric("setFishingStrategy")) 
setGeneric("setFishingStrategy", function(element, universe) standardGeneric("setFishingStrategy"))
setMethod("setFishingStrategy", signature(element="Fishery", universe="Universe"), 
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
		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset
		fisheryTaxa <- getTimestep(element, action[3])$actions[[action[5]]]$relatedIndexes

		if (getRTState(universe, "currentYear") == getTrial(universe, item="firstFishingYear")) { # only done in the first season at present

			trialPeriodInfo <- getAttribute(universe, "PeriodInfo") # list(PropInYear, StartPoint, EndPoint)
			PeriodsN <- length(trialPeriodInfo$StartPoint)

			# effort currently weighted by area of SSMUs - could update strategy every
			# year based on density, abundance or change the fishery so as it encompasses
			# a number of SSMUs thereby accounting for ideal free fishing mortality
			# 1. determine proportion of total fishing area within each SSMU to be fished
			SSMUareas <- getSpatial(universe, "polygonAreas")[getPolygons(element)]
			propSSMUareas <- SSMUareas / sum(SSMUareas)

			# 2. loop through polygons to determine relative fishing patterns
			EffortMatrix <- NULL

			for (pn in 1:getSlot(element, "polygonsN")) {

				# a. relative effort in polygon
				RelEffort <- propSSMUareas[pn]

				# b. take input data on polygon fishing pattern (day, month, nominated relative effort)
				#        and convert to matrix (col 1 = prop of year to change, col 2 = input effort)
				Pattern <- NULL
				Npatterns <- nrow(dSet[[pn]])
				for (p in 1:Npatterns){
					PropYear <- dayFromDate(dSet[[pn]][p,1],dSet[[pn]][p,2]) / 365
					Pattern <- rbind(Pattern,c(PropYear,dSet[[pn]][p,3]))
				}
				Pattern <- matrix(Pattern,ncol=2)
				Npatterns <- nrow(Pattern)
				
				# if the first pattern is part way through year then last pattern crosses into start of year
				if(Pattern[1,1]>0) {
					Pattern <- rbind(c(0,Pattern[Npatterns,2]),Pattern)
					Npatterns <- Npatterns+1
				}

				# c. determine average effort per period 
				PeriodPattern <- NULL
				LastPattern <- Pattern[1,2]
        
				for (pe in 1:PeriodsN){
					ref <- which(Pattern[,1] <= trialPeriodInfo$EndPoint[pe] & Pattern[,1] > trialPeriodInfo$StartPoint[pe])
					if (sum(ref) > 0) {
						# loop through patterns remaining and determine proportions of period taken up by the pattern
						# (p2-p1)/period has a pattern from p1
						PatPe <- LastPattern * (Pattern[ref[1],1] - trialPeriodInfo$StartPoint[pe])
						LastPattern <- Pattern[ref[1],2]
						Nrow <- length(ref)
						if (Nrow>1) {
							for (n in 2:Nrow) {
								PatPe <- PatPe + LastPattern * (Pattern[ref[n],1] - Pattern[ref[(n-1)],1])
								LastPattern <- Pattern[ref[n],2]
							}
						}
						PatPe <- PatPe+LastPattern * (trialPeriodInfo$EndPoint[pe] - Pattern[ref[Nrow],1])
					} else {
						PatPe <- LastPattern * (trialPeriodInfo$EndPoint[pe] - trialPeriodInfo$StartPoint[pe])
					}
					PeriodPattern <- c(PeriodPattern,PatPe)
				}

				# standardise to relative effort for polygon and add to effort matrix
				EffortMatrix <- rbind(EffortMatrix,(RelEffort * PeriodPattern / sum(PeriodPattern)))
			}

			# 3. standardise fishing pattern to reflect effort in each polygon in each season
			elemState$SpaceTimePattern <- elemState$Effort * EffortMatrix / sum(EffortMatrix)

			# 4. set fishing taxa in State
			elemState$TargetN <- nrow(fisheryTaxa)  # number of taxa caught by fishery (updated when manager updates list
			elemState$TACtaxa <- fisheryTaxa           # may be updated when TAC is set (will include by=catch spp as well
			
			# Update state
			setState(element, value=elemState)
		}
	}
)