# Create S4 method 'recruit'
#B.MI.Es.KPFM.recruit.01<-function(
if (!isGeneric("recruit")) 
setGeneric("recruit", function(element, universe) standardGeneric("recruit"))
setMethod("recruit", signature(element="Krill", universe="Universe"),
    function(
		element,
		universe               # access to universe if needed
		)
	{
		# Function:           B.MI.Es.KPFM.recruit.01
		# Version             0.01
		# Description:        Population dynamics function - KPFM recruitment version 1
		# Primary attributes: update recruits based on reproductive condition and environment
		# Return data:        adds recruits to transition
		#-------------------------------------------------------------------------------
		# dependent elements
		#    Module 1, ID 32001 (Krill environment)
		#      Data References
		#         Elmnt$State$PolygonEnv = vector of environments for each polygon
		#-------------------------------------------------------------------------------
		# data set (dSet) requirements
		#       dispersal   = matrix - rows: origin polygons
		#                              cols: destination polygons
		#                              cells: prop of origin ending up in destination
		#                                     over the period of time it takes from
		#                                     spawning to recruitment
		#       Ehat        = vector - value of Environment for which
		#                              survivorship of recruits = 1
		#       MeanWt      = mean weight of individual krill
		#       KPFM        = logical - if TRUE then use KPFM approximation in function
		#
		#-------------------------------------------------------------------------------
		
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
		elemTimesteps <- getTimestep(element)
		
		krillEnvIndx <- elemTimesteps[[action[3]]]$actions[[action[5]]]$relatedIndexes
		krillEnv <- elemTimesteps[[action[3]]]$actions[[action[5]]]$relatedElements
		dSet     <- elemTimesteps[[action[3]]]$actions[[action[5]]]$dset
	
		if (is.null(universe$modules[[krillEnvIndx[1]]][[krillEnvIndx[2]]])) {
			epocErrorMessage(element, "Missing element '", krillEnv[2], "' from ", krillEnv[1], 
						" module as required by '", getSignature(element, "Name.short"), "' relationship.", halt=TRUE)
		}
		
		# read reproductive condition in polygons
		RC <- elemState$Cond.R[1,c(2:ncol(elemState$Cond.R))]

		# distribute reproductive condition across polygons according to dispersal
		# matrix and sum for each polygon to give vector of spawn number in each polygon
		# where reproductive condition in each polygon reflects the number spawned in that polygon
		RC <- RC %*% dSet$dispersal   #  using matrix algebra

		# read state of environment for polygons
		E <- getState(universe$modules[[krillEnvIndx[1]]][[krillEnvIndx[2]]], "PolygonEnv")
		
		# convert environment to index of survivorship
		  SI <- E/dSet$Ehat
		  if(!dSet$KPFM) SI[SI>1] <- 1

		# recruitment is product of polygon young by survivorship index for that polygon
		  if (elemState$StageStrUnits==1) Rec <- RC*SI
		  if (elemState$StageStrUnits==2) Rec <- RC*SI*dSet$MeanWt  # vector of recruitments per polygon converted to mass
		  
		# transfer recruits to transition matrix in units specified by StageStrUnits
		elemTrans$Young <- elemTrans$Young + Rec

		# Update universe elements transition data
		setTransition(element, value=elemTrans) 
		doUpdate(element, TRUE)
	}
)

