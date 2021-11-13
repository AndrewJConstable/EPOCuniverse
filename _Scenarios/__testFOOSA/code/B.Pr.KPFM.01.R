################################################################################
# Predator element class for all EPOC Elements
#                     SW Atlantic - Antarctic Peninsula
# Description:        Biological Element, Group Predators, KPFM general predator
# Primary attributes: Element incorporates all life stages of taxon
#                     Stage structure Juveniles (age structured), non-breeders, breeders
# S4
# 3/2/2009 	Troy Robertson
# 1/4/2012	Modified TR
################################################################################
# B.Pr.KPFM.01<-function (Config,InputData)
# Extend base class
setClass("Predator", contains="Biota")

setMethod("initialize", signature(.Object="Predator"),
    function(.Object, universe, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		# Parameters used to initialise environment but may be altered
		# in the Setup function and/or may not be representative of the Universe e.g. natural mortality rate
		# All animals start off in peak condition
		# list(polygons), in each polygon, list(StagesN.consumption,StagesN.max possible consumption)
		CondByPolygon <- list(
			FoodRealised = rep(1, getAttribute(.Object, "Stage")$StageN)
			,FoodMax      = rep(1, getAttribute(.Object, "Stage")$StageN)
		) 
		condR <- rep(list(CondByPolygon), getSlot(.Object, "polygonsN"))

		# Taxon$State - initial characteristics
		# initial stage structure and abundances in each polygon
		initAbund <- getAttribute(.Object, "Init.abundance")
  
		Init.Stage.Str<-NULL
		mortality <- getAttribute(.Object, "Mortality")
		stage <- getAttribute(.Object, "Stage")
		
		for (pn in 1:getSlot(.Object, "polygonsN")){
			
			Stage.Str <- getAttribute(.Object, "Reproduction")$alpha[pn] # pups
			for(j in 2: stage$JuveAgeN) {
				Stage.Str <- c(Stage.Str,Stage.Str[j-1]*
                                       exp(-(mortality[[1]][[(j-1)]]$M[pn] +
                                             mortality[[2]][[(j-1)]]$M[pn])))
			}

			# note that the start of the simulation is Jan 1 therefore all population is breeding
			# nonbreeders (sum to infinity from (JuveAgeN+1)
			Stage.Str <- c(Stage.Str,0)

			# breeders
			Stage.Str <- c(Stage.Str,Stage.Str[stage$JuveAgeN]*
                       exp(-(mortality[[1]][[stage$JuveAgeN]]$M[pn]+
                             mortality[[2]][[stage$JuveAgeN]]$M[pn]))/
                       (1-exp(-(mortality[[1]][[(stage$JuveAgeN+1)]]$M[pn]+
                             mortality[[2]][[(stage$JuveAgeN+1)]]$M[pn])))
                       )

			# frequency distribution
			Stage.Str <- Stage.Str/sum(Stage.Str)
			Stage.Str <- cbind(c(1:length(Stage.Str)),Stage.Str)
			Init.Stage.Str <- c(Init.Stage.Str,list(Stage.Str))
      
			# now adjust abundance and biomass to reflect whole population even though abundances in
			# data file are just for breeders/non-breeders
			initAbund[pn] <- initAbund[pn]/sum(Stage.Str[c((stage$JuveAgeN+1):(stage$JuveAgeN+2)),2])
		}

		# Estimate biomass - using default stage structure in InputData
		# stage structure is matrix - col 1 = age, col 2 = relative abundance
		initBiomass <- NULL
		for (pn in 1:getSlot(.Object, "polygonsN")){
			bmsPn <- sum(initAbund[pn] * stage$StageStr[[pn]][,2] * stage$StageSize[[pn]])
			initBiomass <- c(initBiomass, bmsPn)
		}

		# set up Taxon - Krill - with functions
		# Taxon$State - initial characteristics
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - declare State")
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
        setState(.Object, "Stage", Init.Stage.Str)
        setState(.Object, "Space", NULL) 
        setState(.Object, "Cond.R", condR)			
        setState(.Object, "Cond.S", stage$StageSize)
        setState(.Object, "Cond.H", condR)
		setState(.Object, "BreederOrigin", getAttribute(.Object, "Allocate.breeders")$SSMUdest)
		
		# Set the initial state value as was held by dset of Scenario.setup originally
		.Object <- setSlot(.Object, "initialState", value=getState(.Object))
		
		# set placeholder for transition states - note that Update is checked at the end of each period to see if
		# the State needs updating. If FALSE then overlooked.
		transition <- list(
            CompetitorElements = NULL, 		# matrix - col 1 = module, col 2 = elementID
            CompCoeff   = NULL    			# matrix - cols - polygons, rows = competitors
        )
		# Update transition
		setTransition(.Object, value=transition)
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - end setup")

		#   return Predator object
        return(.Object)
	}
)

# Function:           B.Pr.KPFM.Time0.fn.01.R
# Description:        Initialise Predator at the beginning of a scenario
# Create S4 method 'initialiseReplicate'
# These are tasks required to be completed prior to running the simulation once all elements are setup
setMethod("initialiseReplicate", signature(element="Predator", universe="Universe"), 
    function(element, universe) {
		# initialise Predator state
		element <- setState(element, value=getSlot(element, "initialState"))
		doPrintFinal(element, TRUE)
		
		return(element)
	}
)

# Function:           B.Pr.KPFM.TransitionSetup.01
# Description:        Setup transition environment for predators
# Create S4 method 'initialiseTransition'
setMethod("initialiseTransition", signature(element="Predator", universe="Universe"),
    function(element, universe) {
		
		if (is.null(getSlot(element, "polygonsN"))) element <- setSlot(element, "polygonsN", 1)
        doUpdate(element, FALSE)
		
		setTransition(element, "Mortality", NULL)
#                        1.  prey subject polygon (relative in element)
#                        2.  units of qnty
#                        3.  stage
#                        4.  stage quantity
#                        5.  stage size
#                        6.  mortality source module (do not need to identify where mortality came from)
#                        7.  mortality source element
#                        8.  mortality source local population (relative in element)
#                        9.  mortality source stage

        setTransition(element, "Consumption", NULL)
                                      # column from above
#                        1. pop         #    8.   mortality source local population (relative in element)
#                        2. st          #    9.   mortality source stage
#                        3. preyRE[1]   #    /.   prey module
#                        4. preyRE[2]   #    /.  prey element
#                        5. ResCol2[r2] #    1.   prey subject polygon (relative in element)
#                        6. Units       #    2.   units of qnty
#                        7. ResCol3[r3] #    3.   stage
#                        8. Sum         #    4.   stage realised quantity (after competition adjustment in updata_state of prey)
#                        9. SumMax      #    /.   stage maximum quantity (on first calculation of consumption)
#                       10. Universe[[preyRE[1]]][[preyRE[2]]]$State$Cond.S[[ResCol2[r2]]][ResCol3[r3]]  #    5.   stage size
#                       11. Universe$Config$RealTimeState$CurrentYear  # /. calendar year
#                       12. Universe$Config$RealTimeState$Period       # /. period
#                       13. PeriodInfo$YearPropn                       # /. proportion of year

		return(element)
	}
)