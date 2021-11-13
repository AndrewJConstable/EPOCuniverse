#A.Fi.KPFM.01<-function (Config,InputData)
################################################################################
# Fishery element class for all EPOC Elements
# S4
# 28/5/2009 Troy Robertson
################################################################################
# Function:           A.Fi.KPFM.01
#                     SW Atlantic - Fishery
# Description:        Activity - KPFM fishery in SSMUs
#
# Primary attributes: 

#          Config - list of environments with configuration data
#                 - required inputs are
#                    Config$ScenariosN
#                    Config$Scenario$YearStart
#                    Config$Scenario$YearEnd
#                    Config$Polygon$Polygon.Names
#                    Config$Scenario$RootDir
#   ############################################################################
# Extend base class
setClass("Fishery", contains="Activity")

setMethod("initialize", signature(.Object="Fishery"),
    function(.Object, universe, dataPath, ...) {
	
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		# Space and time definitions for Elmnt
		spatial <- getSpatial(universe)
    
		# Routines and data for initialising Elmnt
		# Parameters used to initialise environment but may be altered
		# in the Setup function and/or may not be representative of the Universe e.g. natural mortality rate
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - declare State")
		
		# Elmnt$State - initial characteristics
		setState(.Object, "TargetN", 0)			# number of taxa caught by fishery (updated when manager updates list
		setState(.Object, "TACtaxa", NULL)	   	# may be updated when TAC is set (will include by=catch spp as well
		setState(.Object, "Effort", getAttribute(.Object, "VesselsInFleet"))
		setState(.Object, "SpaceTimePattern", NULL)  # effort in each period and each polygon
		setState(.Object, "Catch", getAttribute(.Object, "Init.Catch.SSMU"))
		setState(.Object, "Efficiency", getAttribute(.Object, "Init.Efficiency.SSMU"))
		setState(.Object, "StartYear", getAttribute(.Object, "Year.Set.Fishery.Strategy"))

		# Set the initial state value as was held by dset of Scenario.setup originally
		.Object <- setSlot(.Object, "initialState", value=getState(.Object))
		
		# set placeholder for transition states - note that Update is checked at the end of each period to see if
		# the State needs updating. If FALSE then overlooked.
		transition <- list(
            CompetitorElements = NULL, 	# matrix - col 1 = module, col 2 = elementID
            CompCoeff   		= NULL   	# matrix - cols - polygons, rows = competitors
        )
		
		# Update transition
		setTransition(.Object, value=transition)
		doUpdate(.Object, FALSE)
		
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - end setup")
		
		# return Element
		return(.Object)
	}
)

# Function:           B.Fi.KPFM.Time0.fn.01.R
# Description:        Initialise Predator at the beginning of a scenario
# Create S4 method 'initialiseReplicate'
# These are tasks required to be completed prior to running the simulation once all elements are setup
setMethod("initialiseReplicate", signature(element="Fishery", universe="Universe"),
    function(element, universe) {
		# initialise Predator state
		element <- setState(element, value=getSlot(element, "initialState"))
		doPrintFinal(element, TRUE)
		
		return(element)
	}
)

#Create S4 method 'initialiseTransition'
#B.Fi.KPFM.TransitionSetup.01<-function (ec,e,Universe)
#setGeneric("initialiseTransition", function(element) standardGeneric("initialiseTransition"))
setMethod("initialiseTransition", signature(element="Fishery", universe="Universe"),
    function(element, universe) {
		# Note: Transfer/Adoption not considered as a transition but instantaneous transfer
		#       from one element to another that should occur as an "AFTER" action
		if (is.null(getSlot(element, "polygonsN"))) element <- setSlot(element, "polygonsN", 1)
		
		doUpdate(element, FALSE)
        setTransition(element, "Consumption", NULL) # the name is standard for one element "consuming" another
                                      # column from mortality transition in target elements
#                        1. pop         #    8.   mortality source local population (relative in element)
#                        2. st          #    9.   mortality source stage
#                        3. targetRE[1]   #    /.   target module
#                        4. targetRE[2]   #    /.  target element
#                        5. ResCol2[r2] #    1.   target subject polygon (relative in element)
#                        6. Units       #    2.   units of qnty
#                        7. ResCol3[r3] #    3.   stage
#                        8. Sum         #    4.   stage realised quantity (after competition adjustment in updata_state of prey)
#                        9. SumMax      #    /.   stage maximum quantity (on first calculation of consumption)
#                       10. Universe[[targetRE[1]]][[targetRE[2]]]$State$Cond.S[[ResCol2[r2]]][ResCol3[r3]]  #    5.   stage size
#                       11. Universe$Config$RealTimeState$CurrentYear  # /. calendar year
#                       12. Universe$Config$RealTimeState$Period       # /. period
#                       13. PeriodInfo$YearPropn                       # /. proportion of year
		
		return(element)
	}
)