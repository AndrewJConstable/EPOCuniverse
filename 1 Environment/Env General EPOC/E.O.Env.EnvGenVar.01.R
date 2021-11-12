################################################################################
# Module       : Environment
# Element Class: EnvGenVar01
# Description:
#      General habitat variable for modelling:
#          * regional cycles and change
#          * regional stochasticity
#          * variation between polygons within regions
#          * seasonal cycles within polygons
#
# History:
# 9/7/2008  Original R environment - Andrew Constable
#              E.O.Env.KPFM.01
# 26/2/2009 converted to S4 class - Troy Robertson
# 1/4/2012	Modified TR
# 6/4/2015  Modified from Krill Environment class to General Environment Class AC

################################################################################
################################################################################

setClass("EnvGenVar", contains="Environment")

################################################################################
setMethod("initialize", signature(.Object="EnvGenVar"),

    function(.Object, universe, dataPath, ...) {

        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)

		# State - initial characteristics
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - declare State")
		# declare State
		setState(.Object, "PolygonEnv", getPolygons(.Object)) 		# fill with polygon numbers as placeholders

		# Set the initial state value as was held by dset of Scenario.setup originally
		.Object <- setSlot(.Object, "initialState", value=getState(.Object))

		.Object <- setSlot(.Object, "recordElements", c(4:(3 + length(getPolygons(.Object)))))

    #   check presence of file of the habitat variable, if not present then create one

		fileName <- getRuntimePath(universe, getAttribute(.Object, "RuntimeFile"))

		#   -----------------------------------------------------------------------
		#   establish path by opening file to read - if not present then create file
		epocVerboseMessage(.Object, "Searching for General Environmental Variable file at: ", fileName)

		#   -----------------------------------------------------------------------
		if (!file.exists(fileName) | getAttribute(.Object, "ReplaceFile")) {
			# Generate time series of environments
			# Check if there is a user supplied regionalSeries function
			methodName <- .Object$functions$timeSeriesEnvVar$actionMethod
			#methodName <- getFunctionData(element, "regionalSeries")$actionMethod
			if (is.null(methodName) || methodName == "") {	# Use standard method
				epocDebugMessage(.Object, class(.Object), ": '", getSignature(.Object, "Name.short"),
											"' using inbuilt 'timeSeriesEnvVar' method")
				envRegion <- timeSeriesEnvVar(.Object, universe)
			} else if (hasMethod(methodName, signature(class(.Object)[[1]], "Universe"))) {
				epocDebugMessage(.Object, class(.Object), ": '", getSignature(.Object, "Name.short"),
											"' using user defined 'timeSeriesEnvVar' method '", methodName, "'")
				envRegion <- do.call(methodName, list(.Object, universe))
			} else {
				epocErrorMessage(.Object, class(.Object), ": '", getSignature(.Object, "Name.short"),
											"' has no timeSeriesEnvVar method '", methodName, "' available!", halt=TRUE)
			}

      } # end if file exists


    # use scenario 1 as the base scenario for developing the data file.

    scenario<-getScenario(universe,1)

    # check file has correct dimensions

		# check file for valid contents by reading header information
		#    then read lines until first record for use in scenarios
		epocVerboseMessage(.Object, "General Environmental Variable - checking file")
		fileConn <- getFileConnection(.Object, "RuntimeFile", fileName, "r")

		useFile <- TRUE
		nextLine <- readFileConnection(.Object, conn=fileConn, linenum=1)
		epocVerboseMessage(.Object, "\t", nextLine)
		nextLine <- readFileConnection(.Object, conn=fileConn, linenum=0)
		epocVerboseMessage(.Object, "\t", nextLine)
		if (as.integer(substring(nextLine, 25, last = 1000000))!=getSlot(.Object, "polygonsN")) useFile<-FALSE
		nextLine <- readFileConnection(.Object, conn=fileConn, linenum=0)
		epocVerboseMessage(.Object, "\t", nextLine)
		if (as.integer(substring(nextLine, 25, last = 1000000))!=getSlot(scenario, "replicateCnt")) useFile <- FALSE
		readFileConnection(.Object, conn=fileConn, linenum=0)
		readFileConnection(.Object, conn=fileConn, linenum=0)
		nextLine <- readFileConnection(.Object, conn=fileConn, linenum=0)
		epocVerboseMessage(.Object, "\t", nextLine)
		if (as.integer(substring(nextLine, 25, last = 1000000))!=getSlot(scenario, "yearStart")) useFile <- FALSE
		nextLine <- readFileConnection(.Object, conn=fileConn, linenum=0)
		epocVerboseMessage(.Object, "\t", nextLine)
		if (as.integer(substring(nextLine, 25, last = 1000000))!=getSlot(scenario, "yearEnd")) useFile<-FALSE
		nextLine <- readFileConnection(.Object, conn=fileConn, linenum=0)
		epocVerboseMessage(.Object, "\t", nextLine)
		if (as.integer(substring(nextLine, 25, last = 1000000))!=getAttribute(.Object, "NtimesInYear")) useFile<-FALSE
		readFileConnection(.Object, conn=fileConn, linenum=0)
		readFileConnection(.Object, conn=fileConn, linenum=0)

			closeFileConnection(.Object, "RuntimeFile")


		if (!useFile) epocErrorMessage(.Object, "File for General Environmental Variable incorrect - program terminated", halt=TRUE)
		epocVerboseMessage(.Object, "\t", "General Environmental Variable - file ready to read")


		# set placeholder for transition states - note that Update is checked at the end of each period to see if
		# the State needs updating. If FALSE then overlooked.
		# Update transition
		doUpdate(.Object, FALSE)
			
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - end setup")

		return(.Object)
	}
)
	
################################################################################
# Create S4 method 'initialiseScenario'

setMethod("initialiseScenario", signature(element="EnvGenVar", universe="Universe"),
    function(element, universe) {

		scenario <- getScenario(universe)
	
		doPrintFinal(element, TRUE)
	
		epocVerboseMessage(element, "General Environmental Variable - setting file for general variable data")

		fileName <- getRuntimePath(universe, getAttribute(element, "RuntimeFile"))
		fileConn <- getFileConnection(element, "RuntimeFile", fileName, "r")

    readFileConnection(element, conn=element$fileConnections[["RuntimeFile"]], linenum=10)  # read 10 lines and leave open

		epocVerboseMessage(element, "")
		return(element)
	}
)

################################################################################
# Create S4 method 'initialiseReplicate'
#
setMethod("initialiseReplicate", signature(element="EnvGenVar", universe="Universe"),
    function(element, universe) {

		epocVerboseMessage(element, "General Environmental Variable - initialise replicate trial")

		fileConn <- getFileConnection(element, "RuntimeFile")

		nRec <- getAttribute(element, "Initialise")$Records
		if (nRec > 0){
				record <- readFileConnection(element, conn=fileConn, linenum=0)
				record <- fromCSVCharacter(record, type="double")

				setState(element, "PolygonEnv", record[getSlot(element, "recordElements")])
		} else {
			setState(element, "PolygonEnv", rep(NA, getSlot(element, "polygonsN")))
		}

		epocVerboseMessage(element, "")

		return(element)
	}
)

################################################################################
if (!isGeneric("timeSeriesEnvVar"))
setGeneric("timeSeriesEnvVar", function(element, universe) standardGeneric("timeSeriesEnvVar"))
setMethod("timeSeriesEnvVar", signature(element="EnvGenVar", universe="Universe"),
    function(element, universe)
    {

# Function: Environment - Generalised time series
#
# by Andrew Constable
# Australian Antarctic Division
# Antarctic Climate and Ecosystems CRC
#
# derived from Constable 2008.
#
# Description avaialable in
#
# "C:\\_proj\\EPOC\\_Elements\\Habitats\\Env General EPOC\\Functions - Environment.doc"
#

#EnvGenVar_TS<-function(yearStart,yearEnd,TrialsN,EnvGenVar){
Scenario<-getSlot(universe, "scenarios")

      trialsN          <- getSlot(Scenario[[1]], "replicateCnt")
			yearsN           <- getSlot(Scenario[[1]], "yearsN")  # number of years from start to finish in file
      polygonsN        <- getSlot(element, "polygonsN")
      yearStart        <- getSlot(Scenario[[1]], "yearStart")
      yearEnd          <- getSlot(Scenario[[1]], "yearEnd")
      NtimesInYear      <- getAttribute(element, "NtimesInYear")

# -----------------------------------------------------------------------------
# 1 Local functions


# 1.1 Seasonal Cycle

# A seasonal cycle is modelled as a sine curve beginning at the minimum followed by ascent, a maximum value for a period, descent, then a minimum value for a period.  The lengths of each of the four periods are user-determined using the following matrix - Cycle:
#
#                      Start Time   Rise
#            Ascent    0.0   0.25   1.0
#            Max       0.25  0.1    0.0
#            Descent   0.35  0.5   -1.0
#            Min       0.85  0.15   0.0
#            End       1.0   0.0    0.0
#
# This matrix could be changed from one year to the next if random functions are incorporated into the routine before calling the function.
#
# Inputs are:
#
#       Times    vector  times as proportions of the year for which values need to be returned
#       Cycle    matrix  as above
#       TimeMin  real    the proportion of the year passed when the seasoal cycle begins from minimum
#       Max      real    maximum value in cycle (may change between cycles)
#       Min      real    minimum value in cycle (may change between cycles)
#

SeasonVal<-function(Times,Cycle,TimeMin,Max,Min){

     # need to ensure that the times are positive relative to seasonal minimum (TimeMin)
     Tprime <- if(sum(Times>=TimeMin)>0) (Times-TimeMin) else (Times-TimeMin+1)

     Splt<-split(Tprime, cut(Tprime, Cycle[,"Start"],include.lowest = TRUE))

     Y<-NULL
     MaxMin<-c(((Max-Min)*0.5),Max,((Max-Min)*0.5),Min)
     for(i in seq_along(Splt)){
       if(length(Splt[[i]])>0) {
            Y<-c(Y,if(i==2 | i==4) Splt[[i]]/Splt[[i]]*MaxMin[i] else
            (MaxMin[i]*(sin((Splt[[i]]-Cycle[i,"Start"]-Cycle[i,"Rise"]*0.5*Cycle[i,"Time"])/
                              Cycle[i,"Time"]*pi)+1)+Min)
                 ) # end concatenation
            } # end if
       } # end do loop
     return(Y)
     } # end function


#---------------------------------------------------------------------------
## 2. Regional Series

# Vector of time series of region values including values for
#
#       Year0 (the year prior to the time series),
#       Records,
#       yearStart:yearEnd
#
#The first year is to facilitate the seasonal cycle routine.

FirstYear<-yearStart-1
AllYearsN<-yearEnd-FirstYear+1
RefYear0<-FirstYear-1

# assemble the parameters that will go in to it
   # Parameters for general environmental time series in a region
		if (is.null(getAttribute(element, "RegionTSParams"))) {
			    RegionTSParams <- list(Year0 = 1900
									   ,a = list(slope = 0.0, int = 1.0)
									   ,p = list(slope = 0.0, int = 1.0)
									   ,f = list(slope = 0.0, int = 1.0)
									   ,Xmin = list(slope = 0.0, int = 1))
     		   } else RegionTSParams <- getAttribute(element, "RegionTSParams")

  	yearsFromYear0 <- (FirstYear - RegionTSParams$Year0)
		Series <- rep(1,AllYearsN)
		tprime_firstYear <- yearsFromYear0 + 2
		tprime <- 0.00

		for (i in (yearsFromYear0 + 1):(yearsFromYear0 + AllYearsN)){

			# scale parameters
			p    <- RegionTSParams$p$slope*i+RegionTSParams$p$int
			a    <- RegionTSParams$a$slope*i+RegionTSParams$a$int
			f    <- RegionTSParams$f$slope*i+RegionTSParams$f$int
			Xmin <- RegionTSParams$Xmin$slope*i+RegionTSParams$Xmin$int

			if((RegionTSParams$f$slope>0.00 | RegionTSParams$p$slope>0.00)
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

				if(Series[(i-yearsFromYear0-1)]>(2*a+Xmin) | Series[(i-yearsFromYear0-1)]<Xmin) { #Xt-1 is outside range of new function then snap to max or min
					if(Series[(i-yearsFromYear0-1)]>(2*a+Xmin)) {
						tprime<-ifelse (Fall,tprime_min,tprime_max)
					} else {
						tprime<-ifelse (Fall,tprime_max,tprime_min)
					}
				} else { # search for Xt-1 in new function
					tprime<-optimise(function(tprime,a,PiFraction,Xmin,X_t_minus_1)
								abs((a*(sin((PiFraction+tprime)*2*pi)+1)+Xmin)-X_t_minus_1)
							 ,interval = c(tprime_min,tprime_max)
							 ,a=a
							 ,PiFraction=NewPiFraction
							 ,Xmin=Xmin
							 ,X_t_minus_1=Series[(i-yearsFromYear0-1)])
				}
				tprime<-tprime[[1]]*p # convert pi fraction into years

			}

			Series[i-yearsFromYear0] <- ifelse((p>0),(a*(sin((i+tprime+f)*2*pi/p)+1)+Xmin),Xmin)

			a_old<-a
			p_old<-p
			f_old<-f
			Xmin_old<-Xmin
		} # end year loop

RegionTS<-Series

#----------------------------------------------------------------------------
## 3. Regional Series variability

#  generate log-normal variates to be applied to regional time series and replicate sequence (rows)
#  for each replicate trial

 if (getAttribute(element, "Region_Variation")$UseRandSeq) {

  			if (!is.na(getAttribute(element, "Region_Variation")$Seed)){
					set.seed(getAttribute(element, "Region_Variation")$Seed)
				} else {
					set.seed(.Random.seed)
				}

				LogSD <- sqrt(log(1 + getAttribute(element, "Region_Variation")$CV^2))
        Rvector <- matrix(exp(rnorm((length(RegionTS)*trialsN),mean=0,sd=LogSD)),nrow=trialsN)

				epocDebugMessage(element, "Random Regional Variation:")
				epocDebugMessage(element, Rvector)
			} else {
				Rvector <- matrix(rep(1,(length(RegionTS)*trialsN)),nrow=trialsN)
			}

#----------------------------------------------------------------------------
# 4. Output of Replicate * Year * Polygon * Season
#
# This part of the routine has three main functions based on the trial*year matrix of regional values:
# i) derive polygon values for each year given a regional value and the variance-covariance relationships
# ii) if needed, give values at specified times over the year in each polygon given a seasonal cycle
# iii) iterate this process over all replicate trials
#
# Given the potentially large number of values (rep*year*polygon*season), these data are written to file with the first records providing header information and each record thereafter corresponding to:
#       Column 1: Trial
#       Column 2: Year
#       Column 3: Day in year as a proportion of Year
#       Column 4+(N-1): Polygon 1 to N
#
# For ease of programming, the 'birthdate' of the annual value needs to coincide with the beginning of the seasonal cycle.  The function for the seasonal cycle is written with the capacity to have a flexible minimum and maximum.  At present, this routine fixes the minimum at 0 and uses the annual value from the regional/polygon series as the maximum for a year.  the change-over of this value in a year occurs at the birthdate i.e. at the minimum value of the cycle just prior to the ascending limb.
#
# 4.1 Setup

# 4.1.1 Set seed for polygon values

#Note: mvrnorm was not available in MASS library in R version 3.1.2. - using library mvtnorm with the
# may need function rmvnorm in its place
               #          library(mvtnorm)

  		if (getAttribute(element, "Polygon_Variation")$UseRandSeq) {
				if(!is.na(getAttribute(element, "Polygon_Variation")$Seed)){
					set.seed(getAttribute(element, "Polygon_Variation")$Seed)
				} else {
					set.seed(.Random.seed)
				}
			}

# 4.1.2 check multivariate normal package is loaded

			# Load MASS package for mvrnorm function
			if (!require("MASS", quietly=TRUE)) {
				epocErrorMessage(element, "MASS package required for mvrnorm function in General Environmental Variable initialiseReplicate method.  Please install!", halt=TRUE)
			}

# 4.1.3 Initialise variables for loops

# Set up parameters for seasonal cycle in polygons if needed

timeInYearN<-1  # if only an annual value

TimeCycleMin<-getSlot(element, "birthday") # decimal time of year

if(getAttribute(element, "Seasonal_Cycle")$DoCycle){
 SeasonMin<-NULL
 SeasonCycle<-NULL
 for(poly in 1:polygonsN){
     if (getAttribute(element, "Seasonal_Cycle")$Params_Vary_Polygons) {
      SeasonMin <- c(SeasonMin,list(getAttribute(element, "Seasonal_Cycle")$Params[[p]]$Min))
      SeasonCycle<-c(SeasonCycle,list(getAttribute(element, "Seasonal_Cycle")$Params[[p]]$Cycle))
      } else {
      SeasonMin <- c(SeasonMin,list(getAttribute(element, "Seasonal_Cycle")$Params[[1]]$Min))
      SeasonCycle<-c(SeasonCycle,list(getAttribute(element, "Seasonal_Cycle")$Params[[1]]$Cycle))
      } # end if
     } # end poly
  IntervalDecimals<-getAttribute(element, "Seasonal_Cycle")$Intervals/365
  IntervalsBeforeBirthday<- (IntervalDecimals-TimeCycleMin) < 0
 } # end if DoCycle


# 4.1.4 Open file and write header information

			epocVerboseMessage(element, "General Environmental Variable - creating new file")
 		fileName <- getRuntimePath(universe, getAttribute(element, "RuntimeFile"))

			fileConn <- getFileConnection(element, "RuntimeFile", fileName, "w")

			# write header information
			writeFileConnection(element, "General Environment   : ",as.character(getSignature(getScenario(universe, 1),"Name.short")), sep="", conn=fileConn)
			writeFileConnection(element, "Polygons              : ",as.character(getSlot(element, "polygonsN")), sep="", conn=fileConn)
			writeFileConnection(element, "Trials                : ",as.character(getScenario(universe, 1,"replicateCnt")), sep="", conn=fileConn)
			writeFileConnection(element, "Years per Trial       : ",as.character(getScenario(universe, 1,"yearsN")), sep="", conn=fileConn)
			writeFileConnection(element, "Records before series : ",as.character(getAttribute(element, "Initialise")$Records), sep="", conn=fileConn)
			writeFileConnection(element, "First Year            : ",as.character(getScenario(universe, 1,"yearStart")), sep="", conn=fileConn)
			writeFileConnection(element, "Last Year             : ",as.character(getScenario(universe, 1,"yearEnd")), sep="", conn=fileConn)
			writeFileConnection(element, "Times In Year         : ",as.character(NtimesInYear), sep="", conn=fileConn)
			writeFileConnection(element, "Data", sep="", conn = fileConn)
			writeFileConnection(element, "Scenario", "Year", asCSVCharacter(getSpatial(universe, "polygonNames")[getPolygons(element)]), sep=",", conn=fileConn)

# Notes for the data records:
#
#      1. the first record needs to be at time 0 (at beginning of first period)
#                 that record gets read at initialisation of a replicate trial
#      2. subsequent records are read coinciding with the correct period in the year
#

# 4.2 Loops


# 4.2.1 Start trial loop

for (tr in 1:trialsN){

# Initialise polygons for Year0

yr<-1

Pvector<-rep(1,polygonsN)
if(polygonsN>1){  # do not do polygon variation if only one polygon
  if(getAttribute(element, "Polygon_Variation")$UseRandSeq) {
        VarCovMat <- crossprod(diag(getAttribute(element, "Polygon_Variation")$Var),t(diag(getAttribute(element, "Polygon_Variation")$Var)%*%getAttribute(element, "Polygon_Variation")$CorrMat))
        Pvector <- mvrnorm(n=1, mu=getAttribute(element, "Polygon_Variation")$ScaleCoeff,VarCovMat)
				} else {
					Pvector <- getAttribute(element, "Polygon_Variation")$ScaleCoeff
				}
       # generate polygon values within year in PolygonVals
  }
  PolygonVals <- RegionTS[yr] * Rvector[tr,yr] * Pvector


# save time 0 record to file

  if(getAttribute(element, "Seasonal_Cycle")$DoCycle) {
        Y<-NULL
        for (poly in 1:polygonsN){
                 Y<-c(Y,SeasonVal(0,SeasonCycle[[poly]],TimeCycleMin,PolygonVals[poly],SeasonMin[[poly]]))
                 } # end poly
     } else Y<-PolygonVals

				writeFileConnection(element, asCSVCharacter(c(tr,(RefYear0+yr),0.00,Y)), conn = fileConn)

 # 4.2.2 Start year loop


 for (yr in 2:length(RegionTS)){ # note that (yearStart-Records) is the second element
#                                   of the time series as the first element is Year0

#  A. Do Seasonal Cycle times in year prior to birthdate (use last year's values)
#     loop through polygons and get values for cycle

if(getAttribute(element, "Seasonal_Cycle")$DoCycle){
  Y<-NULL
  for (poly in 1:polygonsN){

    Y<-c(Y,SeasonVal(IntervalDecimals[IntervalsBeforeBirthday],SeasonCycle[[poly]]
                     ,TimeCycleMin,PolygonVals[poly],SeasonMin[[poly]]))
  } # end poly
  Y<-matrix(Y,ncol=polygonsN)
} # end if


#  B. Do Seasonal Cycle times in year on or after the birthdate
#     Estimate polygon values for the year

Pvector<-rep(1,polygonsN)

if(polygonsN>1){  # do not do polygon variation if only one polygon
			if(getAttribute(element, "Polygon_Variation")$UseRandSeq) {
    VarCovMat <- crossprod(diag(getAttribute(element, "Polygon_Variation")$Var),t(diag(getAttribute(element, "Polygon_Variation")$Var)%*%getAttribute(element, "Polygon_Variation")$CorrMat))
      Pvector <- mvrnorm(n=1, mu=getAttribute(element, "Polygon_Variation")$ScaleCoeff,VarCovMat)
				} else {
					Pvector <- getAttribute(element, "Polygon_Variation")$ScaleCoeff
				}
       # generate polygon values within year in PolygonVals
}

				PolygonVals <- RegionTS[yr] * Rvector[tr,yr] * Pvector

Yfull<-NULL
for (poly in 1:polygonsN){
if(getAttribute(element, "Seasonal_Cycle")$DoCycle){
    Yfull<-c(Yfull,Y[,poly]
         ,SeasonVal(IntervalDecimals[!IntervalsBeforeBirthday],SeasonCycle[[poly]]
                   ,TimeCycleMin,PolygonVals[poly],SeasonMin[[poly]]))
    } else {
    Yfull<-c(Yfull,PolygonVals[poly])
    } # end if
  } # end poly

  Yfull<-matrix(Yfull,ncol=polygonsN)


#  C. Write to file

  if(getAttribute(element, "Seasonal_Cycle")$DoCycle){
    for (i in 1:getAttribute(element, "Seasonal_Cycle")$IntervalsN){
				writeFileConnection(element, asCSVCharacter(c(tr,(RefYear0+yr),IntervalDecimals[i],Yfull[i,])), conn = fileConn)
     } # end i
    } else {
				writeFileConnection(element, asCSVCharacter(c(tr,(RefYear0+yr),TimeCycleMin,Yfull[1,])), conn = fileConn)

    }

# End loops
   } # end year
 } # end trial

# close file
			closeFileConnection(element, "RuntimeFile")
} # end function

) # end setMethod(timeSeriesEnvVar)