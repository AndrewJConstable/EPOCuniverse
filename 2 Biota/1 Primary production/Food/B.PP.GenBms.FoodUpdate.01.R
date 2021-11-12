# Create S4 method 'recruit'
#B.PP.GenBms.FoodUpdate.01<-function(
if (!isGeneric("foodUpdate"))
setGeneric("foodUpdate", function(element, universe) standardGeneric("foodUpdate"))
setMethod("foodUpdate", signature(element="PPgenBMS", universe="Universe"),
    function(
		element,
		universe               # access to universe if needed
		)
	{
		# Function:           B.PP.GenBms.FoodUpdate.01
		# Version             0.01
		# Description:        Update food - Primary Production general Biomass variable as food for secondary producers
		# Primary attributes: update food in PPgenBMS based on an environment variable
		# Return data:        adds food to state
		#-------------------------------------------------------------------------------
		# dependent elements
		#    Environment variable with a vector of environments for each polygon
		#      Data References
		#         Elmnt$State$PolygonEnv = vector of environments for each polygon
		#-------------------------------------------------------------------------------
		# data set (dSet) requirements
		#     dSet could be used to transform environment variable
    #     at present no transformation is undertaken
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
		elemTimesteps <- getTimestep(element)
		
		EnvElementIndx <- elemTimesteps[[action[3]]]$actions[[action[5]]]$relatedIndexes
		EnvNames <- elemTimesteps[[action[3]]]$actions[[action[5]]]$relatedElements
		dSet     <- elemTimesteps[[action[3]]]$actions[[action[5]]]$dset
	
		if (is.null(universe$modules[[EnvElementIndx[1]]][[EnvElementIndx[2]]])) {
			epocErrorMessage(element, "Missing element '", EnvNames[2], "' from ", EnvNames[1],
						" module as required by '", getSignature(element, "Name.short"), "' relationship.", halt=TRUE)
		}
		

		# read state of environment for polygons
		E <- getState(universe$modules[[EnvElementIndx[1]]][[EnvElementIndx[2]]], "PolygonEnv")

    Food<-E

    elemState$Abundance$mass <- elemState$Abundance$mass+Food

		# Update state for the element of universe
		setState(element, value=elemState)
	}
)

