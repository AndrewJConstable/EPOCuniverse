# Input data - E.O.Env.EnvGenVar.data.01.R
# General Environmental Variable

# routine to enable processing of input data if needed but without function

# Data structure

# EnvGenVar (list)
#    $signature
#    $polygonsN
#    $polygons
#    $birthdate
#    $RuntimeFile     
#    $ReplaceFile     
#    $EnvGenVar_Params
#    $Region_Variation
#    $Polygon_Variation
#    $NtimesInYear
#    $SeasonalCycle
#    $Initialise
#    $FunctionsList
#    $OutputFiles
#    $OutputFlags
#    $Functions
#    $Timesteps
#
#


EnvGenVar 				  <- list()
EnvGenVar$signature	<- list(ClassName	 = "EnvGenVar",
														ID           = 12001,
														Name.full    = "Environment General Variable 1",
														Name.short   = "EnvGenVar01",
														Morph        = "General",
														Revision      = "01",
														Authors      = "A.Constable",
														Last.edit    = "06 April 2015"
                            )# end signature
	
# Polygon reference - which polygons are to be used out of the list of defined polygons
EnvGenVar$polygons       <- c(1:1)
EnvGenVar$polygonsN		   <- length(EnvGenVar$polygons)

# Birthdate - specify day and month to be used as time 0 in the year for the variable
EnvGenVar$birthdate      <- list(day = 1, month = 6)

# File for storing environmental variable 
EnvGenVar$RuntimeFile    <- paste(EnvGenVar$signature$Name.short,"_input01.dat",sep="")
EnvGenVar$ReplaceFile    <- TRUE

# parameters for generating regional annual time series (TS) for the environmental general variable
EnvGenVar$RegionTSParams  <- list(
							                      Year0 = 1900
							                     ,a = list(slope = 0.0, int = 1.0)
							                     ,p = list(slope = 0.0, int = 1.0)
							                     ,f = list(slope = 0.0, int = 1.0)
							                     ,Xmin = list(slope = 0.0, int = 1)
                                   )

EnvGenVar$Region_Variation  <- list(Seed = 2345, UseRandSeq = TRUE, CV = 1.0)
EnvGenVar$Polygon_Variation <- list(Seed = 5000, UseRandSeq = TRUE
							  						  	   ,ScaleCoeff = rep(1,EnvGenVar$polygonsN)
							  						  	   ,Var = rep(0,EnvGenVar$polygonsN)
							  						  	   ,CorrMat = matrix(rep(EnvGenVar$polygonsN,(1*EnvGenVar$polygonsN)),ncol=EnvGenVar$polygonsN)
                                   )
EnvGenVar$NtimesInYear    <- 1 # this will be changed to the length of Seasonal_Cycle$IntervalsN if DoCycle
EnvGenVar$AnnualUpdateDay <- dayFromDate(day=1,month=10)

EnvGenVar$Seasonal_Cycle <- list(DoCycle = TRUE
                                ,FirstRecordAtProjStartDate = TRUE # if false then read at last time interval closest to Start Date
                                ,Intervals = c(31, 61, 92, 122, 153, 183, 213, 244, 274, 305, 335, 365) # 12 equal time periods
                                ,IntervalsN = 12
                                ,Params_Vary_Polygons = FALSE  # if FALSE then just use first set in Params list
                                                                 # if TRUE, replicate params in list for each polygon
                                ,Params = list(P1 = list(Min = 0
                                                        ,Cycle = matrix(c(0.0 ,0.25, 1.0
                                                                        , 0.25,0.1 , 0.0
                                                                        , 0.35,0.5 ,-1.0
                                                                        , 0.85,0.15, 0.0
                                                                        , 1.0 ,0.0 , 0.0)
                                                                        ,ncol=3,byrow=TRUE
                                                                        ,dimnames=list(c("Ascent","Max","Descent","Min","End")
                                                                                      ,c("Start","Time","Rise")))
                                                           ) # end P1 list
                                                 ) # end Params list
                                ) # end seasonal cycle list
if(EnvGenVar$Seasonal_Cycle$DoCycle) EnvGenVar$NtimesInYear <- EnvGenVar$Seasonal_Cycle$IntervalsN # this will be changed to the length of Seasonal_Cycle$IntervalsN if DoCycle

EnvGenVar$Initialise        <- list(Records = 1)  # 0 or >0 indicates that a record is to be used (>0) to initialise a replicate trial

EnvGenVar$FunctionsList     <- list(
							   StatePrint      = list(actionMethod = "printState"
													             ,actionFile = file.path("code", "E.O.Env.EnvGenVar.printState.01.R"))
							  ,UpdateEnv = list(actionMethod = "envUpdate"
													             ,actionFile = file.path("code", "E.O.Env.EnvGenVar.envUpdate.01.R"))
                ) # end FunctionsList

EnvGenVar$OutputFiles        <- list(State = paste(EnvGenVar$signature$Name.short,"_State.dat",sep=""))
EnvGenVar$OutputFlags        <- list(PrintState = TRUE)

#   #############################################################
#   Ancillary functions
#   #############################################################

EnvGenVar$Functions 			<- list(

			# function to undertake element-specific setup of actions
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup         = NULL
		 ,printState = list(actionMethod = EnvGenVar$FunctionsList$StatePrint$actionMethod
								       ,actionFile   = EnvGenVar$FunctionsList$StatePrint$actionFile
								       ,dset   = list(  # List because may need more than one file to print state
											                State = list(output = EnvGenVar$OutputFlags$PrintState
														                      ,fname  = EnvGenVar$OutputFiles$State
														                      ,path   = NULL) # end State list
									                   ) # end dset list
                       )# end printState list
     ) # end Functions list

EnvGenVar$TimestepShell 			<- list(
           calday    = NULL  # end of timestep 1
          ,actionsN  = NULL
          ,actions   = list(
					            printState = list(
                             actionMethod = EnvGenVar$FunctionsList$StatePrint$actionMethod
										        ,actionFile = EnvGenVar$FunctionsList$StatePrint$actionFile
										        ,tsType = "FirstPeriod" 		# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										        ,tsTiming = "Before"    		# "Before","During","After"
										        ,relatedElements = NULL
										        ,dset = list(  # List because may need more than one file to print state
													               State = list(output = EnvGenVar$OutputFlags$PrintState
																                     ,fname  = EnvGenVar$OutputFiles$State
																                     ,path   = NULL)
										                     ) # end dset list
					                  ) # end printState list
                    ,update.environments = list(actionMethod = EnvGenVar$FunctionsList$UpdateEnv$actionMethod
	                                                     ,actionFile = EnvGenVar$FunctionsList$UpdateEnv$actionFile
                                                       ,tsType = "LastPeriod" 	# "AllPeriods","FirstPeriod","LastPeriod")
                                                       ,tsTiming = "After"    	# "Before","During","After"
                                                       ,relatedElements = NULL 	# matrix - col 1 = module; col 2 = absolute ID of element; if none then NULL
                                                       ,dSet      = NULL)       	# pattern for input to function
                                               ) # end actions list
                                ) # end timestep shell
EnvGenVar$Timesteps<-NULL
if(!EnvGenVar$Seasonal_Cycle$DoCycle){
   EnvGenVar$Timesteps 			<- list(EnvGenVar$TimestepShell)
   EnvGenVar$Timesteps[[1]]$calday<-EnvGenVar$AnnualUpdateDay
} else {
   for (tstep in 1:EnvGenVar$Seasonal_Cycle$IntervalsN){
       EnvGenVar$Timesteps<- c(EnvGenVar$Timesteps,list(EnvGenVar$TimestepShell))
       EnvGenVar$Timesteps[[tstep]]$calday<-EnvGenVar$Seasonal_Cycle$Intervals[tstep]
       }
   } # end else

# declare variable to be sourced
EnvGenVar

