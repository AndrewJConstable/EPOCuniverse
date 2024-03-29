# Function: Environment - Generalised time series
# ========================================================
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


###############################################################################
###############################################################################
EnvGenVar_TS<-function(yearStart,yearEnd,TrialsN,EnvGenVar){

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
#T his matrix could be changed from one year to the next if random functions are incorporated into the routine before calling the function.
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
Times_prime <- if(sum(Times>=TimeMin)>0) (Times-TimeMin) else (Times-TimeMin+1)

Splt<-split(Times_prime, cut(Times_prime, Cycle[,"Start"],include.lowest = TRUE))

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

# assemble the parameters that will go in to it
   # Parameters for general environmental time series in a region
#E		if (is.null(getAttribute(element, "RegionTSParams"))) {

Records        <- EnvGenVar$Initialise$Records
RegionTSParams <- EnvGenVar$RegionTSParams

      if (is.null(RegionTSParams)) {
			RegionTSParams <- list(Year0 = 1900
									   ,a = list(slope = 0.0, int = 1.0)
									   ,p = list(slope = 0.0, int = 1.0)
									   ,f = list(slope = 0.0, int = 1.0)
									   ,Xmin = list(slope = 0.0, int = 1))
		} else {
#E			RegionTSParams <- getAttribute(element, "RegionTSParams")
			RegionTSParams <- RegionTSParams
		}

    year1 <- yearStart - Records - 1  # minus 1 here is to give year0 for routine
		yearsN <- yearEnd - year1 + 1

  	yearsFromYear0 <- (year1 - RegionTSParams$Year0)
		Series <- rep(1,yearsN)
		tprime_firstYear <- yearsFromYear0 + 2
		tprime <- 0.00

		for (i in (yearsFromYear0 + 1):(yearsFromYear0 + yearsN)){

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

#E if (getAttribute(element, "Region_Variation")$UseRandSeq) {
if (EnvGenVar$Region_Variation$UseRandSeq) {

#E  			if (!is.na(getAttribute(element, "Region_Variation")$Seed)){
    		if (!is.na(EnvGenVar$Region_Variation$Seed)){
#E					set.seed(getAttribute(element, "Region_Variation")$Seed)
    				set.seed(EnvGenVar$Region_Variation$Seed)
				} else {
					set.seed(.Random.seed)
				}

#E				LogSD <- sqrt(log(1 + getAttribute(element, "Region_Variation")$CV^2))
  			LogSD <- sqrt(log(1 + EnvGenVar$Region_Variation$CV^2))

        Rvector <- matrix(exp(rnorm((length(RegionTS)*TrialsN),mean=0,sd=LogSD)),nrow=TrialsN)

#E				epocDebugMessage(element, "Random Regional Variation:")
#E				epocDebugMessage(element, Rvector)
			} else {
				Rvector <- matrix(rep(1,(length(RegionTS)*TrialsN)),nrow=TrialsN)
			}

#----------------------------------------------------------------------------
# 4. Output of Replicate * Year * Polygon * Season
#
# This part of the routine has three main functions based on the trial*year matrix of regional values:
#
# i) derive polygon values for each year given a regional value and the variance-covariance relationships
#
# ii) if needed, give values at specified times over the year in each polygon given a seasonal cycle
#
# iii) iterate this process over all replicate trials
#
# Given the potentially large number of values (rep*year*polygon*season), these data are written to file with the first records providing header information and each record thereafter corresponding to:
#
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

#E  		if (getAttribute(element, "Polygon_Variation")$UseRandSeq) {
    	if (EnvGenVar$Polygon_Variation$UseRandSeq) {
#E				if(!is.na(getAttribute(element, "Polygon_Variation")$Seed)){
  			if(!is.na(EnvGenVar$Polygon_Variation$Seed)){
#E					set.seed(getAttribute(element, "Polygon_Variation")$Seed)
  				set.seed(EnvGenVar$Polygon_Variation$Seed)
				} else {
					set.seed(.Random.seed)
				}
			}

# 4.1.2 check multivariate normal package is loaded

			# Load MASS package for mvrnorm function
			if (!require("MASS", quietly=TRUE)) {
				epocErrorMessage(element, "MASS package required for mvrnorm function in General Environmental Variable initialiseReplicate method.  Please install!", halt=TRUE)
			}

# 4.1.3 Determine number of years for data in a trial and set reference year in order to correctly output year in file

#E            year0 <- getSlot(scenario, "yearStart") - getAttribute(element, "Initialise")$Records - 1

  yearsN<-yearEnd-yearStart+1+EnvGenVar$Initialise$Records

  year0 <- yearStart - EnvGenVar$Initialise$Records - 2 # noting that the time series has one year prior to first year to be recorded in file

# 4.1.4 Initialise variables for loops

# Set up parameters for seasonal cycle in polygons if needed

if(EnvGenVar$Seasonal_Cycle$DoCycle){
 SeasonMin<-NULL
 SeasonCycle<-NULL
 for(poly in 1:EnvGenVar$polygonsN){
     if (EnvGenVar$Seasonal_Cycle$Params_Vary_Polygons) {
      SeasonMin <- c(SeasonMin,list(EnvGenVar$Seasonal_Cycle$Params[[p]]$Min))
      SeasonCycle<-c(SeasonCycle,list(EnvGenVar$Seasonal_Cycle$Params[[p]]$Cycle))
      } else {
      SeasonMin <- c(SeasonMin,list(EnvGenVar$Seasonal_Cycle$Params[[1]]$Min))
      SeasonCycle<-c(SeasonCycle,list(EnvGenVar$Seasonal_Cycle$Params[[1]]$Cycle))
      } # end if
     } # end poly
}
TimeCycleMin<-dayFromDate(EnvGenVar$birthdate$day,EnvGenVar$birthdate$month)/365
IntervalDecimals<-EnvGenVar$Seasonal_Cycle$Intervals/365
IntervalsBeforeBirthday<- (IntervalDecimals-TimeCycleMin) < 0


# 4.1.5 Open file and write header information

cat("General Environment - creating new file","\n",sep="")
fileConn<-file(description = EnvGenVar$RuntimeFile, open = "w")

# write header information
cat("General Environment : ",as.character(EnvGenVar$RuntimeFile),"\n",sep="",file = fileConn)
cat("Polygons            : ",as.character(EnvGenVar$polygonsN),"\n",sep="",file = fileConn)
cat("Trials              : ",as.character(TrialsN),"\n",sep="",file = fileConn)
cat("Years per trial     : ",as.character(yearsN),"\n",sep="",file = fileConn)
cat("   Initialise       : ",as.character(EnvGenVar$Initialise$Records),"\n",sep="",file = fileConn)
cat("   First Year       : ",as.character(yearStart),"\n",sep="",file = fileConn)
cat("   Last Year        : ",as.character(yearEnd),"\n",sep="",file = fileConn)
cat("   Times in year    : ",as.character(if(EnvGenVar$Seasonal_Cycle$DoCycle)
                                             EnvGenVar$Seasonal_Cycle$Intervals/365 else
                                             dayFromDate(EnvGenVar$birthdate$day
                                                        ,EnvGenVar$birthdate$month)/365
                                           ),"\n",sep=" ",file = fileConn)
cat("Data","\n",sep="",file = fileConn)
cat("Trial","Year","Time",as.character(EnvGenVar$polygons),sep=",",file = fileConn)
cat("\n",file = fileConn)

# Notes for the data records:
#
#      1. the first record needs to be at time 0 (at beginning of first period)
#                 that record gets read at initialisation of a replicate trial
#      2. subsequent records are read coinciding with the correct period in the year
#
# Thus the records would be:
#      1.   the value at the time of that start day (if a logical is TRUE) or the value
#              at the most recent time step just prior to the Start day (if a logical is FALSE)
#      2.   the value at the next time step in the vector
#      3.   and so on until the end of the year..
#      y.   the last time step in the year
#      y+1. the first time step of the vector and so on..


# 4.2 Loops

# 4.2.1 Start trial loop

for (tr in 1:TrialsN){

# Initialise polygons for Year0

yr<-1

#E  		if(getAttribute(element, "Region_Variation")$UseRandSeq) {
		if(EnvGenVar$Region_Variation$UseRandSeq) {
#E    VarCovMat <- crossprod(diag(getAttribute(element, "Polygon_Variation")$Var),t(diag(getAttribute(element, "Polygon_Variation")$Var)%*%getAttribute(element, "Polygon_Variation")$CorrMat))
    VarCovMat <- crossprod(diag(EnvGenVar$Polygon_Variation$Var),t(diag(EnvGenVar$Polygon_Variation$Var)%*%EnvGenVar$Polygon_Variation$CorrMat))
#E      Pvector <- mvrnorm(n=1, mu=getAttribute(element, "Polygon_Variation")$ScaleCoeff,VarCovMat)
      Pvector <- mvrnorm(n=1, mu=EnvGenVar$Polygon_Variation$ScaleCoeff,VarCovMat)
				} else {
#E					Pvector <- getAttribute(element, "Polygon_Variation")$ScaleCoeff
  				Pvector <- EnvGenVar$Polygon_Variation$ScaleCoeff
				}
       # generate polygon values within year in PolygonVals
				PolygonVals <- RegionTS[yr] * Rvector[tr,yr] * Pvector

 # 4.2.2 Start year loop


 for (yr in 2:length(RegionTS)){ # note that (yearStart-Records) is the second element
#                                   of the time series as the first element is Year0

#  A. Do Seasonal Cycle times in year prior to birthdate (use last year's values)
#     loop through polygons and get values for cycle

if(EnvGenVar$Seasonal_Cycle$DoCycle){
  Y<-NULL
  for (poly in 1:EnvGenVar$polygonsN){

    Y<-c(Y,SeasonVal(IntervalDecimals[IntervalsBeforeBirthday],SeasonCycle[[poly]]
                     ,TimeCycleMin,PolygonVals[poly],SeasonMin[[poly]]))
  } # end poly
  Y<-matrix(Y,ncol=EnvGenVar$polygonsN)
} # end if


#  B. Do Seasonal Cycle times in year on or after the birthdate
#     Estimate polygon values for the year

#E			if(getAttribute(element, "Region_Variation")$UseRandSeq) {
		if(EnvGenVar$Region_Variation$UseRandSeq) {
#E    VarCovMat <- crossprod(diag(getAttribute(element, "Polygon_Variation")$Var),t(diag(getAttribute(element, "Polygon_Variation")$Var)%*%getAttribute(element, "Polygon_Variation")$CorrMat))
    VarCovMat <- crossprod(diag(EnvGenVar$Polygon_Variation$Var),t(diag(EnvGenVar$Polygon_Variation$Var)%*%EnvGenVar$Polygon_Variation$CorrMat))
#E      Pvector <- mvrnorm(n=1, mu=getAttribute(element, "Polygon_Variation")$ScaleCoeff,VarCovMat)
      Pvector <- mvrnorm(n=1, mu=EnvGenVar$Polygon_Variation$ScaleCoeff,VarCovMat)
				} else {
#E					Pvector <- getAttribute(element, "Polygon_Variation")$ScaleCoeff
  				Pvector <- EnvGenVar$Polygon_Variation$ScaleCoeff
				}
       # generate polygon values within year in PolygonVals
				PolygonVals <- RegionTS[yr] * Rvector[tr,yr] * Pvector

Yfull<-NULL
for (poly in 1:EnvGenVar$polygonsN){
if(EnvGenVar$Seasonal_Cycle$DoCycle){
    Yfull<-c(Yfull,Y[,poly]
         ,SeasonVal(IntervalDecimals[!IntervalsBeforeBirthday],SeasonCycle[[poly]]
                   ,TimeCycleMin,PolygonVals[poly],SeasonMin[[poly]]))
    } else {
    Yfull<-c(Yfull,PolygonVals[poly])
    } # end if
  } # end poly

  Yfull<-matrix(Yfull,ncol=EnvGenVar$polygonsN)


#  C. Write to file
#     ?? need to consider where to start to allow for initialisation

  if(EnvGenVar$Seasonal_Cycle$DoCycle){
    for (i in 1:EnvGenVar$Seasonal_Cycle$IntervalsN){
                cat(as.character(c(tr,(year0+yr),IntervalDecimals[i],Yfull[i,])),sep=",",file = fileConn)
                cat("\n", file = fileConn) # next line
     } # end i
    } else {
                cat(as.character(c(tr,(year0+yr),TimeCycleMin,Yfull[1,])),sep=",",file = fileConn)
                cat("\n", file = fileConn) # next line

    }

# End loops
   } # end year
 } # end trial

# close file
close(fileConn)
} # end function
########################################################################
########################################################################

# Test to see it works

setwd("C:\\_proj\\EPOC\\_Elements\\Habitats\\Env General EPOC\\test\\")
source("C:\\_proj\\EPOC\\_Utilities\\DayFromDate.R")
source("E.O.Env.EnvGenVar.data.01.R")
StartDate<-list(day = 1, month=1)
yearStart<-1995
yearEnd<-2005
TrialsN<-1

EnvGenVar_TS(yearStart,yearEnd,TrialsN,EnvGenVar)
