################################################################################
# KrillEnvironment element class for all EPOC Elements
# S4
# 26/2/2009 Troy Robertson
################################################################################
#E.O.Env.KPFM.01<-function (Config,KrillEnv)
# Extend base class
setClass("KrillEnvironment", contains="Environment")

setMethod("initialize", signature(element="KrillEnvironment"),

    function(element, universe, dataPath, ...) {

        # first call parents (Element) initialize method
        element <- callNextMethod(element, dataPath, ...)

		# Function:           E.O.Env.KPFM.01.R
		# Description:        Environmental Element, Group Ocean,
		#                     Environment for modifying krill recruitment
		# Primary attributes: 

		#   ############################################################################
		#   Factor$State - initial characteristics
		epocVerboseMessage(element, getSignatureLine(element), " - declare State")
		# declare State
		setState(element, "PolygonEnv", getPolygons(element)) 		# fill with polygon numbers as placeholders
		
		# Set the initial state value as was held by dset of Trial.setup originally
		element <- setSlot(element, "initialState", value=getState(element))
		
		element <- setSlot(element, "recordElements", c(3:(2 + length(getPolygons(element)))))	

		#   set placeholder for transition states - note that Update is checked at the end of each period to see if
		#   the State needs updating. If FALSE then overlooked.
		# Update transition
		doUpdate(element, FALSE)
			
		epocVerboseMessage(element, getSignatureLine(element), " - end setup")

		return(element)
	}
)
	
# Create S4 method 'initialiseTrial'
# These are tasks required to be completed prior to running the simulation once all elements are setup
#
#setGeneric("initialiseTrial", function(element, trial="Trial", config="list") standardGeneric("initialiseTrial"))
setMethod("initialiseTrial", signature(element="KrillEnvironment", universe="Universe"), 
    function(element, universe) {
		trial <- getTrial(universe)
	
		doPrintFinal(element, TRUE)
	
		# initialise Predator state
		setState(element, value=getSlot(element, "initialState"))
		fileName <- file.path(getSlot(trial, "trialDir"), getAttribute(element, "RuntimeFile"))
		
		#   #################################################
		#   Trial characteristics
		tr <- getRTState(universe, "currentTrial")
		trialsN <- length(getSlot(universe, "trials"))
		
		#   ############################################################################
		#   establish path by opening file to read - if not present then create file
		epocVerboseMessage(element, "Searching for krill environment file at: ", fileName)

		########################################
		if (!file.exists(fileName) | getAttribute(element, "ReplaceFile")) {
    
			epocVerboseMessage(element, "Krill Environment - creating new file")
			# Open file and store file handle for later use
			#fileConn <- file(description = fileName, open = "w")
			fileConn <- getFileConnection(element, "RuntimeFile", fileName, "w")
			yearsN <- getSlot(trial, "yearsN") + getAttribute(element, "Initialise")$Records
			
			# write header information
			writeFileConnection(element, "Krill Environment : ",as.character(getSignature(trial, "Name.short")),sep="",conn = fileConn)
			writeFileConnection(element, "Polygons          : ",as.character(getSlot(element, "polygonsN")),sep="",conn = fileConn)
			writeFileConnection(element, "Trials            : ",as.character(trialsN),sep="",conn = fileConn)
			writeFileConnection(element, "Years per trial   : ",as.character(yearsN),sep="",conn = fileConn)
			writeFileConnection(element, "Initialise        : ",as.character(getAttribute(element, "Initialise")$Records),sep="",conn = fileConn)
			writeFileConnection(element, "First Year        : ",as.character(getSlot(trial, "yearStart")),sep="",conn = fileConn)
			writeFileConnection(element, "Last Year         : ",as.character(getSlot(trial, "yearEnd")),sep="",conn = fileConn)
			writeFileConnection(element, "Data",sep="",conn = fileConn)
			writeFileConnection(element, "Trial","Year", getSpatial(universe, "polygonNames")[getPolygons(element)],sep=",",conn = fileConn)
			writeFileConnection(element, "",conn = fileConn)

			# generate region time series of environments
			# Check if there is a user supplied regionalSeries function
			error <- try(methodName <- getFunctionData(element, "regionalSeries")$actionMethod, silent=TRUE)
			#methodName <- getFunctionData(element, "RegionalSeries")$actionMethod
			if (exists("error") && class(error) == "try-error") {
				# Use standard method
				envRegion <- regionalSeries(element, universe)
			} else if (hasMethod(methodName, signature(class(element)[[1]], "Universe"))) {
				envRegion <- do.call(methodName, list(element, universe))
			}

			#       generate random variation in region environment
			# - get random number sequence for years in trial if needed
			# - return matrix (rows = trials, columns = years)
            if (getAttribute(element, "Region_Variation")$UseRandSeq) {
				# Added trial num to create variation across trials
				if (!is.na(getAttribute(element, "Region_Variation")$Seed)){
					set.seed(getAttribute(element, "Region_Variation")$Seed * tr)	
				} else {
					set.seed(.Random.seed * tr)
				}
				LogSD <- sqrt(log(1 + getAttribute(element, "Region_Variation")$CV^2))
				#Rvector <- exp(rnorm((yearsN * trialsN),mean=0,sd=LogSD))	# trialsN was changing output
				Rvector <- exp(rnorm((yearsN),mean=0,sd=LogSD))
				epocDebugMessage(element, "Random Regional Variation:")
				epocDebugMessage(element, Rvector)
			} else {
				#Rvector <- exp(rep(0,yearsN * trialsN))					# trialsN was changing output
				Rvector <- exp(rep(0,yearsN))
			}
            #Rvector <- matrix(Rvector,ncol=yearsN)

			#       generate polygon scaling coefficients & write to file
			if (getAttribute(element, "Polygon_Variation")$UseRandSeq) {
				if(!is.na(getAttribute(element, "Polygon_Variation")$Seed)){
					set.seed(getAttribute(element, "Polygon_Variation")$Seed * tr)
				} else {
					set.seed(.Random.seed * tr)
				}
			}
			
            year0 <- getSlot(trial, "yearStart") - getAttribute(element, "Initialise")$Records - 1
            # TR Took out repeated loop as doesn't make sense if trials have different files and/or start/end years
            #for (tr in 1:trialsN){
				for (yr in 1:yearsN){
					if(getAttribute(element, "Region_Variation")$UseRandSeq) {
						VarCovMat <- crossprod(diag(getAttribute(element, "Polygon_Variation")$Var),t(diag(getAttribute(element, "Polygon_Variation")$Var)%*%getAttribute(element, "Polygon_Variation")$CorrMat))
						Pvector <- mvrnorm(n=1, mu=getAttribute(element, "Polygon_Variation")$ScaleCoeff,VarCovMat)
					} else {
						Pvector <- getAttribute(element, "Polygon_Variation")$ScaleCoeff
					}
					Evector <- c(tr, (year0 + yr))
					Evector <- c(Evector, envRegion[yr] * Rvector[yr] * Pvector)
					#Evector <- c(Evector, envRegion[yr] * Rvector[tr,yr] * Pvector)

					writeFileConnection(element, as.character(Evector),sep=",",conn = fileConn)
					writeFileConnection(element, "", conn = fileConn) # next line
                } 
			#}
			closeFileConnection(element, "RuntimeFile")
		} 

		# check file for valid contents by reading header information
		#    then read lines until first record for use in trials

		epocVerboseMessage(element, "Krill Environment - checking file and making ready for trials","\n")

		fileConn <- getFileConnection(element, "RuntimeFile", fileName, "r")
		
		epocVerboseMessage(element, "\t", "Opened Krill Environment File","\n")
		useFile <- TRUE
		nextLine <- readFileConnection(element, conn=fileConn, oneline=TRUE)
		epocVerboseMessage(element, "\t", nextLine)
		nextLine <- readFileConnection(element, conn=fileConn, oneline=TRUE)
		epocVerboseMessage(element, "\t", nextLine)
		if (as.integer(substring(nextLine, 21, last = 1000000))!=getSlot(element, "polygonsN")) useFile<-FALSE
		nextLine <- readFileConnection(element, conn=fileConn, oneline=TRUE)
		epocVerboseMessage(element, "\t", nextLine)
		if (as.integer(substring(nextLine, 21, last = 1000000))!=trialsN) useFile <- FALSE
		readFileConnection(element, conn=fileConn, oneline=TRUE)
		readFileConnection(element, conn=fileConn, oneline=TRUE)
		nextLine <- readFileConnection(element, conn=fileConn, oneline=TRUE)
		epocVerboseMessage(element, "\t", nextLine)
		if (as.integer(substring(nextLine, 21, last = 1000000))!=getSlot(trial, "yearStart")) useFile <- FALSE
		nextLine <- readFileConnection(element, conn=fileConn, oneline=TRUE)
		epocVerboseMessage(element, "\t", nextLine)
		if (as.integer(substring(nextLine, 21, last = 1000000))!=getSlot(trial, "yearEnd")) useFile<-FALSE
		readFileConnection(element, conn=fileConn, oneline=TRUE)
		readFileConnection(element, conn=fileConn, oneline=TRUE)
		if (!useFile) epocErrorMessage(element, "Krill Environment file incorrect - program terminated", halt=TRUE)

		epocVerboseMessage(element, "\n", "\t", "Krill Environment - file ready to read\n")

		# add stateFile to Outfiles list - for closing at end
		#config$Outfiles <- c(config$Outfiles, list(KrillEnv = list(FileName = fileName, Path = stateFile)))

		# declare Data - this is the place for active data and file management
		# element@data <- list(stateFile = stateFile,  # file is open through FilePath
						    # FileName = fileName,
						    # RecordElements = c(3:(2 + element@polygonsN))
						   # ) 
						   
		# Function:           E.O.Env.KPFM.Time0.fn.01.R
		nRec <- getAttribute(element, "Initialise")$Records

		#   ############################################################################
		#   Routines to prepare for initialising Factor
		#   ############################################################################

		#   create file of krill environments to be used in run time
		epocVerboseMessage(element, "Krill Environment - setting file of krill environments")	
		
		if (nRec > 0){
			#element$datafileConn <- file(description = fileName, open = "r")
			for (rec in 1:nRec){
				record <- readFileConnection(element, conn=fileConn, oneline=TRUE)
				record <- as.list(read.csv(text=record, header=FALSE))
				#record <- scan(file = fileConn, quiet=TRUE, sep = ",", nlines = 1)
				setState(element, "PolygonEnv", record[getSlot(element, "recordElements")])
			}
		} else {
			setState(element, "PolygonEnv", rep(NA, getSlot(element, "polygonsN")))
		}
		
		return(element)
	}
)

# Create S4 method 'regionalSeries'
#E.O.Env.KPFM.regional.series.01<-function(
#if (isGeneric("regionalSeries")) removeGeneric("regionalSeries") 
#if (!isGeneric("regionalSeries")) 
setGeneric("regionalSeries", function(element, universe) standardGeneric("regionalSeries"))
setMethod("regionalSeries", signature(element="KrillEnvironment", universe="Universe"), 
    function(element, universe)
    {
		# Function:           E.O.Env.KPFM.regional.series.01.R
		# Description:        Generate a time series of X in the KPFM krill environment
		#                     Used to modify krill recruitment
		# Input parameters
		#   Year1 = first year in series
		#   YearLast = last year in series
		#   KPFM_RecEnvParams = parameters to determine recruitment scaling value
		#          for the region in a given year

		#    Factor$Signature <- list(
		#      ID           = ,
		#      Name.full    = "",
		#      Name.short   = "",
		#      Morph        = "",
		#      Version      = "01",
		#      Authors      = "A.Constable",
		#      last.edit    = "12 March 2008"
		#      ) # end Signature
		trial <- getTrial(universe)
		year1 <- getSlot(trial, "yearStart") - getAttribute(element, "Initialise")$Records
		yearsN <- getSlot(trial, "yearEnd") - year1 + 1
		if (is.null(getAttribute(element, "KPFM_RecEnvParams"))) {
			KPFM_RecEnvParams <- list(Year0 = 1900
									   ,a = list(slope = 0.0, int = 1.0)
									   ,p = list(slope = 0.0, int = 1.0)
									   ,f = list(slope = 0.0, int = 1.0)
									   ,Xmin = list(slope = 0.0, int = 1))
		} else {
			KPFM_RecEnvParams <- getAttribute(element, "KPFM_RecEnvParams")
		}
		
		yearsToYear0 <- (year1 - KPFM_RecEnvParams$Year0)
		Series <- rep(1,yearsN)
		tprime_firstYear <- yearsToYear0 + 2
		tprime <- 0.00
		for (i in (yearsToYear0 + 1):(yearsToYear0 + yearsN)){

			# scale parameters
			p <- KPFM_RecEnvParams$p$slope*i+KPFM_RecEnvParams$p$int
			a <- KPFM_RecEnvParams$a$slope*i+KPFM_RecEnvParams$a$int
			f <- KPFM_RecEnvParams$f$slope*i+KPFM_RecEnvParams$f$int
			Xmin <- KPFM_RecEnvParams$Xmin$slope*i+KPFM_RecEnvParams$Xmin$int

			if((KPFM_RecEnvParams$f$slope>0.00 | KPFM_RecEnvParams$p$slope>0.00)
						& i>=tprime_firstYear){ # solve for tprime
				# first determine if the (t-1) position was on the rise or fall of the cycle
				# then search for tprime on the respective rise or fall of the new cycle
				# bracketed by the maximum and minumum
				OldPhase<-(((i-1)+tprime+f_old)/p_old)
				OldPiFraction<-OldPhase-2*floor(OldPhase/2)
				NewPhase<-(((i-1)+f)/p)
				NewPiFraction<-NewPhase-2*floor(NewPhase/2)

				if (OldPiFraction>=0.5 & OldPiFraction<=1.5){ # on fall of cycle
					Fall<-TRUE
					tprime_min<-(0.5-NewPiFraction)
					tprime_max<-(1.5-NewPiFraction)
				} else { # on rise of cycle
					Fall<-FALSE
					if (NewPiFraction<0.5) {
						tprime_min<-(-0.5-NewPiFraction)
						tprime_max<-(0.5-NewPiFraction)
					} else {
						tprime_min<-(1.5-NewPiFraction)
						tprime_max<-(2.5-NewPiFraction)
					}
				}

				if(Series[(i-yearsToYear0-1)]>(2*a+Xmin) | Series[(i-yearsToYear0-1)]<Xmin) { #Xt-1 is outside range of new function then snap to max or min
					if(Series[(i-yearsToYear0-1)]>(2*a+Xmin)) {
						tprime<-ifelse (Fall,tprime_min,tprime_max)
					} else {
						tprime<-ifelse (Fall,tprime_max,tprime_min)
					}   
				} else { # search for Xt-1 in new function
					tprime<-optimise(function(tprime,a,PiFraction,Xmin,X_t_minus_1)
								abs((a*(sin((PiFraction+tprime)*pi)+1)+Xmin)-X_t_minus_1)
							 ,interval = c(tprime_min,tprime_max)
							 ,a=a
							 ,PiFraction=NewPiFraction
							 ,Xmin=Xmin
							 ,X_t_minus_1=Series[(i-yearsToYear0-1)])
				}
				tprime<-tprime[[1]]*p # convert pi fraction into years

			} 

			Series[i-yearsToYear0] <- ifelse((p>0),(a*(sin((i+tprime+f)*pi/p)+1)+Xmin),Xmin)
		  
			a_old<-a
			p_old<-p
			f_old<-f
			Xmin_old<-Xmin
		  
		}
		
		return(Series)
	}
)