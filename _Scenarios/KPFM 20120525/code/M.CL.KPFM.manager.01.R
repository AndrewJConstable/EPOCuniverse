################################################################################
# Manager element class for all EPOC Elements
# SW Atlantic - TAC setting process for SSMUs
# Description:        Management, KPFM SSMU catch limits
# S4
# 26/2/2009 Troy Robertson
# 1/4/2012	Modified TR
################################################################################
#M.CL.KPFM.manager.01<-function (Config,InputData)
setClass("Manager", contains="Management")

setMethod("initialize", signature(.Object="Manager"),
    function(.Object, universe, dataPath, ...) {
	
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)

		# Parameters used to initialise environment but may be altered in the Setup function 
		# and/or may not be representative of the Universe e.g. natural mortality rate

		# Elmnt$State - initial characteristics
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - declare State")
        setState(.Object, "SSMU.Survey.results", NULL)   # format col 1    = Year
																	#        col 2    = fraction of year (period start)
																	#        col 3    = module
																	#        col 4    = .Object
																	#        col 5    = polygon first
																	#        col PN+4 = polygon last
        setState(.Object, "SSMU.CatchRemaining", NULL) 	# list for each TACtaxa - vector of biomass by polygon
        setState(.Object, "SSMU.TAC", NULL) 			# as for CatchRemaining
        setState(.Object, "SSMU.TAC.taxa", NULL)  		# filled in when harvest strategy is set - matrix - col 1 = module, col 2 = .Object
        setState(.Object, "SSMU.TAC.taxa.N", 0)
        setState(.Object, "SSMU.Season.Start", dayFromDate(getAttribute(.Object, "SeasonStart")$Day,
														   getAttribute(.Object, "SeasonStart")$Month) / 365) # start of season as proportion of year
		
		# Set the initial state value as was held by dset of Trial.setup originally
		.Object <- setSlot(.Object, "initialState", value=getState(.Object))
		
		.Object <- setSlot(.Object, "recordElements", c(3:(2+length(getPolygons(.Object)))))

		# set placeholder for transition states - note that Update is checked at the end of each period to see if
		# the State needs updating. If FALSE then overlooked.
		transition <- list(
            CompetitorElements = NULL,  # matrix - col 1 = module, col 2 = elementID
            CompCoeff   = NULL    		# matrix - cols - polygons, rows = competitors
        )
		# Update transition
		
		setTransition(.Object, value=transition)
		doUpdate(.Object, FALSE)
		
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - end setup")
		
		#   return Element
		return(.Object)
	}
)

# Function:           M.CL.KPFM.manager.Time0.fn.01
# Description:        Initialise manager at the beginning of a trial
# Create S4 method 'initialiseTrial'
# These are tasks required to be completed prior to running the simulation once all elements are setup
setMethod("initialiseTrial", signature(element="Manager", universe="Universe"),
    function(element, universe) {
		trial <- getTrial(universe)
		
		doPrintFinal(element, TRUE)
		
		# initialise Manager state
		setState(element, value=getSlot(element, "initialState"))
		
		fileName <- getRuntimePath(universe, getAttribute(element, "RuntimeFile"))
		
		epocVerboseMessage(element, "Management - setting file of deviates for surveys")
		
		# Trial characteristics
		trialsN <- length(getSlot(universe, "trials"))
		surveysN <- getAttribute(element, "SSMU.Survey.Details")$SurveysN
		polygonsN <- getSlot(element, "polygonsN")
		
		# establish path by opening file to read - if not present then create file
		epocVerboseMessage(element, "Searching for survey deviates file at: ", fileName)

		# If no existing file or settings instruct replacement, then rewrite runtime file
		if (!file.exists(fileName) | getAttribute(element, "ReplaceFile")) {
			epocVerboseMessage(element, "Survey deviates - creating new file")
			# Open file and store file handle for later use
			fileConn <- getFileConnection(element, "RuntimeFile", fileName, "w")
			
			# write header information
			writeFileConnection(element, "Survey Deviates   : ",as.character(getAttribute(element, "RuntimeFile")), sep="", conn=fileConn)
			writeFileConnection(element, "Polygons          : ",as.character(polygonsN), sep="", conn=fileConn)
			writeFileConnection(element, "Trials            : ",as.character(trialsN), sep="", conn=fileConn)
			writeFileConnection(element, "Surveys per trial : ",as.character(surveysN), sep="", conn=fileConn)
			writeFileConnection(element, "Data", sep="", conn=fileConn)
			writeFileConnection(element, "Trial", "Survey", asCSVCharacter(getSpatial(universe, "polygonNames")[getPolygons(element)]), sep=",", conn=fileConn)
			writeFileConnection(element, conn=fileConn)

			# generate random deviates
			# - get random number sequence for years in trial if needed
			# - return matrix (rows = trials * years, columns = polygons)
			tr <- getRTState(universe, "currentTrial")
			deviates <- getAttribute(element, "Deviates")
			if (deviates$UseRandSeq) {
				if(!is.na(deviates$Seed)){
					set.seed(deviates$Seed * tr)
				} else {
					set.seed(.Random.seed * tr)
				}
			}

			# TR Took out repeated loop as doesn't make sense if trials have different files and/or start/end years
			for (su in 1:surveysN){
				if (deviates$UseRandSeq) {
					RandData <- rnorm(polygonsN, mean=0, sd=1)
				} else {
					RandData <- rep(0, polygonsN)
				}
				RandData <- c(tr, su, RandData)
				
				writeFileConnection(element, asCSVCharacter(RandData), conn = fileConn)
			}
			closeFileConnection(element, "RuntimeFile")
		}
	  
		# check file for valid contents by reading header information
		# then read lines until first record for use in trials
		epocVerboseMessage(element, "Survey deviates - checking file and making ready for trials")
			
		fileConn <- getFileConnection(element, "RuntimeFile", fileName, "r")

		epocVerboseMessage(element, "\t", "Opened Survey deviates File")
		useFile <- TRUE
		nextLine <- readFileConnection(element, conn=fileConn, linenum=1)
		epocVerboseMessage(element, "\t", nextLine)
		nextLine <- readFileConnection(element, conn=fileConn, linenum=0)
		epocVerboseMessage(element, "\t", nextLine)
		if (as.integer(substring(nextLine, 21, last = 1000000))!=polygonsN) useFile<-FALSE
		nextLine <- readFileConnection(element, conn=fileConn, linenum=0)
		epocVerboseMessage(element, "\t", nextLine)
		if (as.integer(substring(nextLine, 21, last = 1000000))!=trialsN) useFile<-FALSE
		nextLine <- readFileConnection(element, conn=fileConn, linenum=0)
		epocVerboseMessage(element, "\t", nextLine)
		if (as.integer(substring(nextLine, 21, last = 1000000))!=surveysN) useFile<-FALSE
		readFileConnection(element, conn=fileConn, linenum=0)
		readFileConnection(element, conn=fileConn, linenum=0)
		if (!useFile) epocErrorMessage(element, "Survey deviates file incorrect - program terminated", halt=TRUE)
		epocVerboseMessage(element, "\t", "Survey deviates - file ready to read")
		epocVerboseMessage(element, "")
		
		return(element)
	}
)
