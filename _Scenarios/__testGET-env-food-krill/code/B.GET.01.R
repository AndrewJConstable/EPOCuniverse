################################################################################
# Generalised ECOC Taxon element class for all EPOC Elements
# Description:        Biological Element, GET 
# Primary attributes: Element incorporates all life stages of taxon
#                     Derived from combined elements from Constable (2006, 2008)
# Stage structured population
# S4 development 3/2/2009, 1/4/20012 	Troy Robertson
#
# This element: 7 June 2015  AC

################################################################################

# B.GET.01<-function (Config,InputData)
# Extend base class
setClass("GET", contains="Biota")

setMethod("initialize", signature(.Object="GET"),
    function(.Object, universe, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		

		# Taxon$State - initial characteristics

		initAbund <- getAttribute(.Object, "Init.abundance")
		stage <- getAttribute(.Object, "Stage")

    if(stage$StageStrUnits == 1){  # units as numbers
      init.N <- initAbund
      init.B <- init.N
      for (pn in 1:getSlot(.Object, "polygonsN")) init.B[pn] <- sum(init.N[pn]*stage$StageStr[,pn]*stage$StageSize[,pn])
      } else { # units as biomass
      init.B <- initAbund
      init.N <- init.B
      for (pn in 1:getSlot(.Object, "polygonsN")) init.N[pn] <- sum(init.B[pn]*stage$StageStr[,pn]/stage$StageSize[,pn])
      }

		# set up Taxon$State - initial characteristics
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - declare State")
		setState(.Object, "Abundance", list(num.ind  = init.N
														,mass       = init.B
														,mass.unit  = getAttribute(.Object, "ScaleToTonnes")    # relative to tonnes
																  #   i.e. coefficient to scale
																  #   biomass to individuals
																  #   e.g. individuals of approx
																  #   1 gram would be 1E-6
														)
		)
        setState(.Object, "StageN", stage$StageN)
        setState(.Object, "StageStrUnits", stage$StageStrUnits) # reference to which abundance units are used by each age structure
        setState(.Object, "Stage", stage$StageStr)
        setState(.Object, "Space", NULL) 
        setState(.Object, "Cond.S", stage$StageSize)
        setState(.Object, "Cond.R", stage$StageCondR)
        setState(.Object, "Cond.H", stage$StageCondH)
		
		# Set the initial state value as was held by dset of Scenario.setup originally
		.Object <- setSlot(.Object, "initialState", value=getState(.Object))

		
		# set placeholder for transition states - note that Update is checked at the end of each period to see if
		# the State needs updating. If FALSE then overlooked.

		transition <- getAttribute(.Object, "Transition.data")

		# Update transition
		setTransition(.Object, value=transition)
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - end setup")

		#   return GET object
        return(.Object)
	}
)

# Function:           B.GET.Time0.fn.01.R
# Description:        Initialise GET at the beginning of a trial
# Create S4 method 'initialiseReplicate'
# These are tasks required to be completed prior to running a trial once all elements are setup
setMethod("initialiseReplicate", signature(element="GET", universe="Universe"),
    function(element, universe) {
		# initialise GET state
		element <- setState(element, value=getSlot(element, "initialState"))
		doPrintFinal(element, TRUE)
		
		return(element)
	}
)

# Function:           B.GET.TransitionSetup.01
# Description:        Setup transition environment for GET
# Create S4 method 'initialiseTransition'
setMethod("initialiseTransition", signature(element="GET", universe="Universe"),
    function(element, universe) {
		
		if (is.null(getSlot(element, "polygonsN"))) element <- setSlot(element, "polygonsN", 1)
        doUpdate(element, FALSE)
		
		setTransition(element, "Mortality", NULL)
    setTransition(element, "Emigration", NULL)
    setTransition(element, "Consumption", NULL)
    setTransition(element, "MigrateConsumption", NULL)

		return(element)
	}
)