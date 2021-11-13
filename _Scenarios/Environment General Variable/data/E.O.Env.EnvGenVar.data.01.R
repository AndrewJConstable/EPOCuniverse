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
EnvGenVar$signature	<- list(ClassName	    = "EnvGenVar"
														,ID           = 12001
														,Name.full    = "Environment General Variable 1"
														,Name.short   = "E_Gen_01"
														,Morph        = "General"
														,Revision     = "01"
														,Authors      = "A.Constable"
														,Last.edit    = "06 April 2015"
                            )# end signature
	
# Polygon reference - which polygons are to be used out of the list of defined polygons
EnvGenVar$polygons       <- c(1:18)
EnvGenVar$polygonsN		   <- length(EnvGenVar$polygons)

# Birthdate - specify day and month to be used as time 0 in the year for the variable
EnvGenVar$birthdate      <- list(day = 1, month = 6)

# File for storing environmental variable 
EnvGenVar$RuntimeFile    <- paste(EnvGenVar$signature$Name.short,"_input01.dat",sep="")
EnvGenVar$ReplaceFile    <- TRUE

# parameters for generating data for the environmental general variable
EnvGenVar$EnvGenVar_Params  <- list(
							                      Year0 = 1900
							                     ,a = list(slope = 0.0, int = 1.0)
							                     ,p = list(slope = 0.0, int = 1.0)
							                     ,f = list(slope = 0.0, int = 1.0)
							                     ,Xmin = list(slope = 0.0, int = 1)
                                    ) # end params
EnvGenVar$Region_Variation  <- list(Seed = 2345, UseRandSeq = TRUE, CV = 1.0)
EnvGenVar$Polygon_Variation <- list(Seed = 5000, UseRandSeq = TRUE
							  						  	   ,ScaleCoeff = rep(1,18)
							  						  	   ,Var = rep(0,18)
							  						  	   ,CorrMat = matrix(rep(1,(18*18)),ncol=18)
                                   ) # end polygon variation

EnvGenVar$Initialise        <- list(Records = 1)

EnvGenVar$FunctionsList     <- list(
                 RegionalSeries  = list(actionMethod = "regionalSeries"
													             ,actionFile = file.path("code", "E.O.EnvGenVar.regional.series.01.R"))
							  ,StatePrint      = list(actionMethod = "printState"
													             ,actionFile = file.path("code", "E.O.Env.EnvGenVar.printState.01.R"))
							  ,UpdateAnnualEnv = list(actionMethod = "annualUpdate"
													             ,actionFile = file.path("code", "E.O.Env.EnvGenVar.annual_update.01.R"))
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
		 ,RegionalSeries = list(actionMethod = EnvGenVar$FunctionsList$RegionalSeries$actionMethod,
							   actionFile = EnvGenVar$FunctionsList$RegionalSeries$actionFile),
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
           calday    = dayFromDate(01,10)
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
                                ) # end set state of general variable for the year
      ) # end timesteps list

# declare variable to be sourced
EnvGenVar

