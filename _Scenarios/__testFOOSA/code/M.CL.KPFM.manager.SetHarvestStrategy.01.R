#M.CL.KPFM.manager.SetHarvestStrategy.01<-function(
if (!isGeneric("setHarvestStrategy")) 
setGeneric("setHarvestStrategy", function(element, universe) standardGeneric("setHarvestStrategy"))
setMethod("setHarvestStrategy", signature(element="Manager", universe="Universe"), 
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           M.CL.KPFM.manager.SetHarvestStrategy.01
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
		elemState <- getState(element)
		elemTS <- getTimestep(element, action[3])$actions[[action[5]]]

		RelatedElements <- elemTS$relatedIndexes
		# if first year of fishing allowed - set default harvest strategy
		polygonsN <- getSlot(element, "polygonsN")
		if(getScenario(universe, item="firstFishingYear") == getRTState(universe, "currentYear")){
			Nelements <- nrow(RelatedElements)
			elemState$SSMU.TAC <- rep(list(rep(NA, polygonsN)), Nelements)
			elemState$SSMU.TAC.taxa <- RelatedElements  # filled in when harvest strategy is set - matrix - col 1 = module, col 2 = element
			elemState$SSMU.TAC.taxa.N <- Nelements
		}

		if(elemTS$dset$SetInYear == getRTState(universe, "currentYear")) {

			SurveyRef <- (elemState$SSMU.Survey.results[,1] == elemTS$dset$SurveyYear)
										  #        col 1    = Year
										  #        col 2    = fraction of year (period start)
										  #        col 3    = module
										  #        col 4    = element
										  #        col 5    = polygon first
										  #        col PN+4 = polygon last

			if(sum(SurveyRef) > 0) {
				Nelements <- nrow(RelatedElements)
				elemState$SSMU.TAC <- rep(list(rep(NA, polygonsN)), Nelements)
				for (taxon in 1:Nelements){
					ref <- (SurveyRef & elemState$SSMU.Survey.results[,3] == RelatedElements[taxon,1] &
												elemState$SSMU.Survey.results[,4] == RelatedElements[taxon,2])

					if (sum(ref) > 0) {
						# assign SSMU TACs in State - list for each TAC.taxa - vector of biomass by polygon
						elemState$SSMU.TAC[[taxon]] <- elemTS$dset$Gamma * ifelse(ref,elemState$SSMU.Survey.results[,6],0)
						# place TAC.taxa in State
					}
				}
				elemState$SSMU.TAC.taxa <- RelatedElements  # filled in when harvest strategy is set - matrix - col 1 = module, col 2 = element
				elemState$SSMU.TAC.taxa.N <- Nelements
			}
		}

		# if year after last year of fishing then set to NULL harvest strategy (i.e. no fishing)
		if ((getScenario(universe, item="lastFishingYear") + 1) == getRTState(universe, "currentYear")) {
			elemState["SSMU.TAC"] <- list(NULL)
				# note - setting the following causes NULL to be converted to 0 and causes problems with Catch routine
				#    Elmnt$State$SSMU.TAC.taxa   <- NULL  # filled in when harvest strategy is set - matrix - col 1 = module, col 2 = element
				#    Elmnt$State$SSMU.TAC.taxa.N <- 0
			elemState["SSMU.TAC.taxa"] <- list(NULL)
			elemState["SSMU.TAC.taxa.N"] <- 0
		}

		# Update element state
		setState(element, value=elemState)
	}
)
