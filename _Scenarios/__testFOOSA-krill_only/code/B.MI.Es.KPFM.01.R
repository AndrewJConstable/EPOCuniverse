################################################################################
# Krill element class for all EPOC Elements
# S4
# 3/2/2009 Troy Robertson
# 1/4/2012	Modified TR
################################################################################

# Extend base class
setClass("Krill", contains="Biota")
	
setMethod("initialize", signature(.Object="Krill"),
    function(.Object, universe, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)

		spatial <- getSpatial(universe)
       
        # Reproductive condition in each polygon
        # derived from reproductive condition input data where condition is
        # maximum reproduction = alpha * P
        .Object@nYrs <- getAttribute(.Object, "Reprod_Cond")$lag+1
		
        # initial abundance
        initAbund <- getAttribute(.Object, "Init.density") * getAttribute(.Object, "Init.density.multiplier") * 
							getSpatial(universe, "polygonAreas")[getPolygons(.Object)]

        # estimate biomass - using default stage structure in params
        # stage structure is matrix - col 1 = age, col 2 = relative abundance
		stage <- getAttribute(.Object, "Stage")
		initBiomass <- NULL
        for (pn in 1:getSlot(.Object, "polygonsN")){
            bmsPn <- sum(initAbund[pn] * stage$StageStr[[pn]][,2] * stage$StageSize[[pn]])
            initBiomass <- c(initBiomass, bmsPn)
        }
    
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - declare State")
       
        # Taxon$State - initial characteristics
        setState(.Object, "Abundance", list(num.ind  = initAbund
                                        ,mass       = initBiomass
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
        setState(.Object, "Space", log(1 + getAttribute(.Object, "PolygonDensityCV")^2)^0.5) # converted CVs to log domain to make easier later
        setState(.Object, "Cond.S", stage$StageSize)
        setState(.Object, "Cond.H", 0)
		
		# Set the initial state value as was held by dset of Scenario.setup originally
		.Object <- setSlot(.Object, "initialState", value=getState(.Object))

        # set placeholder for transition states - note that Update is checked at the end of each period to see if
        # the State needs updating. If FALSE then overlooked.
        transition <- list(
            CompetitorElements = getAttribute(.Object, "Transition.data")$CompetitorElements, # matrix - col 1 = module, col 2 = elementID
            CompCoeff   = getAttribute(.Object, "Transition.data")$CompetitionCoefficients    # matrix - cols - polygons, rows = competitors
        )
		# Update transition
		setTransition(.Object, value=transition)
		
        epocVerboseMessage(.Object, getSignatureLine(.Object), " - end setup ")
        
        # return Taxon environment
        return(.Object)
    }
)


# Create S4 method 'initialiseReplicate'
# These are tasks required to be completed prior to running the simulation once all elements are setup
#
#B.MI.Es.KPFM.Time0.fn.01
setMethod("initialiseReplicate", signature(element="Krill", universe="Universe"),
    function(element, universe) {
		scenario <- getScenario(universe)
		
		doPrintFinal(element, TRUE)
				
		# initialise Krill state
		setState(element, value=getSlot(element, "initialState"))
		
		condR <- NULL
		nyears <- getSlot(element, "nYrs")
		for (y in 1:nyears){
            condR <- rbind(condR, c((getSlot(scenario, "yearStart")-(nyears+1)+y)
                         ,(getAttribute(element, "Reprod_Cond")$alpha * getAttribute(element, "Reprod_Cond")$P)))
        }

		setState(element, "Cond.R", condR)

		return(element)
	}
)

#Create S4 method 'initialiseTransition'
#B.MI.Es.KPFM.TransitionSetup.01<-function (ec,e,Universe)
#setGeneric("initialiseTransition", function(element) standardGeneric("initialiseTransition"))
setMethod("initialiseTransition", signature(element="Krill", universe="Universe"),
    function(element, universe) {
		# Function:           B.MI.Es.KPFM.TransitionSetup.01
		# Description:        Setup transition environment for krill
		# Note: Transfer/Adoption not considered as a transition but instantaneous transfer
		#       from one element to another that should occur as an "AFTER" action
		if (is.null(getSlot(element, "polygonsN"))) element <- setSlot(element, "polygonsN", 1)
		
		doUpdate(element, FALSE)
		
		# Subtractions
		setTransition(element, "Mortality", NULL)
			#                         $change is a matrix with the following columns
			#                         1.  subject polygon (e.g. destination polygon in migrate; prey polygon in foodweb)
			#                         2.  origin polygon  (e.g. origin polygon in migrate; predator polygon in foodweb)
			#                         3.  module          (e.g. fishery, biota = 2)
			#                         4.  element         (e.g. ref ID for migrate element or predator ref ID in foodweb)
			#                         5.  units of qnty   (e.g. 1 = number, 2 = biomass)
			#                         6.  quantity        (total quantity of change ie. + or -; NB this column is not to be summed as it needs to be multiplied by stagestructure)
			#                         7.  stage age       (the age given to the stage)
			#                         8.  stage proportn  (the proportion of the total quantity given to the stage)
			#                         9.  stage size      (size of the stage - need if size is flexible)

		setTransition(element, "Emigration", NULL)
			# Additions
		setTransition(element, "Young", rep(0, getSlot(element, "polygonsN")))    # for accumulating offspring from reproduction in each polygon of the element
		setTransition(element, "Consumption", NULL)
			#                         $consumption saves the consumption of the different elements
			#                         $consumption is a matrix with the following columns
			#                         1.  subject polygon (e.g. predator polygon in foodweb)
			#                         2.  origin polygon  (e.g. prey polygon in foodweb)
			#                         3.  module          (e.g. fishery, biota, etc.)
			#                         4.  element         (e.g. prey ref ID in foodweb)
			#                         5.  units of qnty   (e.g. 1 = number, 2 = biomass)
			#                         6.  quantity        (total quantity of change ie. + or -; NB this column is not to be summed as it needs to be multiplied by stagestructure)
			#                         7.  stage age       (the age given to the stage)
			#                         8.  stage proportn  (the proportion of the total quantity given to the stage)
			#                         9.  stage size      (size of the stage - need if size is flexible)
			#                        10.  calendar year
			#                        11.  proportion of year
		setTransition(element, "Immigration", NULL)
		
		return(element)
	}
)

