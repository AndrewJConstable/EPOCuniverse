# Create S4 method 'regionalSeries'
#E.O.Env.KPFM.regional.series.01<-function(
#if (isGeneric("regionalSeries")) removeGeneric("regionalSeries") 
if (!isGeneric("regionalSeries")) 
setGeneric("regionalSeries", function(element, universe) standardGeneric("regionalSeries"))
setMethod("regionalSeries", signature(element="KrillEnvironment", universe="Universe"),
    function(element, universe)
    {
		# Function:           E.O.Env.KPFM.regional.series.01.R
		# Description:        Generate a time series of X in the KPFM krill environment
		#                     Used to modify krill recruitment
		scenario <- getScenario(universe)
		year1 <- scenario@yearStart - getAttribute(element, "Initialise")$Records
		yearsN <- scenario@yearEnd - year1 + 1
		
		# KPFM_RecEnvParams = parameters to determine recruitment scaling value for the region in a given year
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
  