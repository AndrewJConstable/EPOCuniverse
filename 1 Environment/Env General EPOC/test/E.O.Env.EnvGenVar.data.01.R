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
EnvGenVar$birthdate      <- list(day = 1, month = 7)

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
							  						  	   ,ScaleCoeff = rep(1,18)
							  						  	   ,Var = rep(0,18)
							  						  	   ,CorrMat = matrix(rep(1,(18*18)),ncol=18)
                                   )
EnvGenVar$Seasonal_Cycle <- list(DoCycle = FALSE
                                ,FirstRecordAtProjStartDate = TRUE # if false then read at last time interval closest to Start Date
                                ,Intervals = c(dayFromDate(1,1)
                                              ,dayFromDate(1,2)
                                              ,dayFromDate(1,3)
                                              ,dayFromDate(1,4)
                                              ,dayFromDate(1,5)
                                              ,dayFromDate(1,6)
                                              ,dayFromDate(1,7)
                                              ,dayFromDate(1,8)
                                              ,dayFromDate(1,9)
                                              ,dayFromDate(1,10)
                                              ,dayFromDate(1,11)
                                              ,dayFromDate(1,12))
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
EnvGenVar$Initialise        <- list(Records = 1)

EnvGenVar$FunctionsList     <- list(
                 RegionalSeries  = list(actionMethod = "regionalSeries"
													             ,actionFile = file.path("code", "E.O.EnvGenVar.regional.series.01.R"))
							  ,SeasonalCycle   = list(actionMethod = "seasonCycle"
													             ,actionFile = file.path("code", "E.O.Env.EnvGenVar.SeasonCycle.01.R"))
							  ,StatePrint      = list(actionMethod = "printState"
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
#			RegionalSeries = list(actionMethod = KrillEnv$FunctionsList$RegionalSeries$actionMethod,
#							   actionFile = KrillEnv$FunctionsList$RegionalSeries$actionFile),
		 ,printState = list(actionMethod = EnvGenVar$FunctionsList$StatePrint$actionMethod
								       ,actionFile   = EnvGenVar$FunctionsList$StatePrint$actionFile
								       ,dset   = list(  # List because may need more than one file to print state
											                State = list(output = EnvGenVar$OutputFlags$PrintState
														                      ,fname  = EnvGenVar$OutputFiles$State
														                      ,path   = NULL) # end State list
									                   ) # end dset list
                       )# end printState list
     ) # end Functions list

EnvGenVar$Timesteps 			<- list(

			Break_point  = list(  # knife-edge step for general environmental variable
				 calday    = dayFromDate(01,10)
				,actionsN = 1
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
				                  ) # end actions list
			                 ) # end Break_point list

			,UpdateAnnualEnv  = list(
           day    = dayFromDate(01,10)
          ,actionsN = 1
          ,actions   = list(
					# function reads next record, checking correct scenario and year
                      update.annual.environments = list(actionMethod = EnvGenVar$FunctionsList$UpdateAnnualEnv$actionMethod
	                                                     ,actionFile = EnvGenVar$FunctionsList$UpdateAnnualEnv$actionFile
                                                       ,tsType = "LastPeriod" 	# "AllPeriods","FirstPeriod","LastPeriod")
                                                       ,tsTiming = "After"    	# "Before","During","After"
                                                       ,relatedElements = NULL 	# matrix - col 1 = module; col 2 = absolute ID of element; if none then NULL
                                                       ,dSet      = NULL)       	# pattern for input to function
                                               ) # end actions list
                                ) # end update annual environment of general variable for the year
      ) # end timesteps list

# declare variable to be sourced
EnvGenVar

