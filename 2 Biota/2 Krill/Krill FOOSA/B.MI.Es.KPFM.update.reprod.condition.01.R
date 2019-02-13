# Create S4 method 'updateReprodCond'
#B.MI.Es.KPFM.update.reprod.condition.01<-function(
if (!isGeneric("updateReprodCond")) 
setGeneric("updateReprodCond", function(element, universe) standardGeneric("updateReprodCond"))
setMethod("updateReprodCond", signature(element="Krill", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# data set (dSet) requirements
		#    vectors for each polygon
		#       P      = Biomass Standard for productivity (~ carrying capacity)
		#       alpha  = maximum per mass recruitment
		#       beta   = prop of P to give 0.5 max recruits
		#    function constants
		#       lag    = integer >0, number of years lag between spawning and recruitment
		#       KPFM   = logical - if TRUE then use KPFM approximation in function
		#
		########################################################
		# Instructions for use
		#     Ideally, reproductive condition should be updated after recruitment occurs
		#     as this function deletes the earliest year (first row) and adds new environment
		#     to last row.  For example, if lag=1 then recruitment in current year
		#     is dependent on reproduction in previous year.  If condition is updated before
		#     reproduction then the single row in Reprod.Condition will contain condition
		#     from the current year rather than the previous year.
		#     In contrast, if lag is to be zero (i.e. recruitment occurs in same year as reproduction) then
		#     set lag to 1 and ensure that recruitment is after reproduction in the year

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
		krillEnvIndx <- getTimestep(element, action[3])$actions[[action[5]]]$relatedIndexes
		krillEnv <- getTimestep(element, action[3])$actions[[action[5]]]$relatedElements
		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset

		if (is.null(universe$modules[[krillEnvIndx[1]]][[krillEnvIndx[2]]])) {
			epocErrorMessage(element, "Missing element '", krillEnv[2], "' from ", krillEnv[1], 
						" module as required by '", getSignature(element, "Name.short"), "' relationship.", halt=TRUE)
		}
		
		# vector of adult biomass in polygons
		kBms <- elemState$Abundance$mass

		# Biomass Standard productivity in biomass (vector - P for each polygon)
		Pprime<-dSet$P

		if(!dSet$KPFM) Pprime <- Pprime * getState(getEPOCElement(krillEnvIndx[1,1], krillEnvIndx[1,2]), "PolygonEnv")

		# proportion of K in polygons
		Kprop <- Pprime  # to avoid divide by zero error
		Kprop[Kprop>0] <- kBms[Kprop>0] / Kprop[Kprop>0]

		# calculate reproductive condition
		RC <- dSet$alpha * Pprime * Kprop / (dSet$beta + Kprop)

		# update reproductive condition - delete first row and add most recent RC to last row
		elemState$Cond.R <- rbind(elemState$Cond.R[-1,], c(getRTState(universe, "currentYear"), RC))
		
		# Update state for the element of universe
		setState(element, value=elemState)
	}
)

