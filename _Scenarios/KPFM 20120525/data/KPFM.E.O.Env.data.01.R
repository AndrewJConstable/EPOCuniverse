# Input data - E.O.Env.KPFM.data.01.R
# KrillEnv

# routine to enable processing of input data if needed but without function

KrillEnv 				<- list()
KrillEnv$signature 		<- list(
	ClassName	 = "KrillEnvironment",
	ID           = 12001,
	Name.full    = "KPFM Krill recruitment environment",
	Name.short   = "KPFM_env",
	Morph        = "KPFM",
	Revision      = "01",
	Authors      = "A.Constable",
	Last.edit    = "12 March 2008"
)
	
# Polygon reference - which polygons are to be used out of the list of defined polygons
KrillEnv$polygonsN		<- 18
KrillEnv$polygons       <- c(1:18)

	# Birthdate - specify day and month to be used as time 0 in the year for the taxon
KrillEnv$birthdate      <- list(day = 1, month = 6)

	# Krill reproductive environments
KrillEnv$RuntimeFile      	<- "KrillEnv0101.dat"
KrillEnv$ReplaceFile        <- TRUE
KrillEnv$KPFM_RecEnvParams  <- list(
							   Year0 = 1900
							  ,a = list(slope = 0.0, int = 1.0)
							  ,p = list(slope = 0.0, int = 1.0)
							  ,f = list(slope = 0.0, int = 1.0)
							  ,Xmin = list(slope = 0.0, int = 1)
)
KrillEnv$Region_Variation   <- list(Seed = 2345, UseRandSeq = TRUE, CV = 1.0)
KrillEnv$Polygon_Variation  <- list(Seed = 5000, UseRandSeq = TRUE
							  ,ScaleCoeff = rep(1,18)
							  ,Var = rep(0,18)
							  ,CorrMat = matrix(rep(1,(18*18)),ncol=18)
)
KrillEnv$Initialise         <- list(Records = 1)
KrillEnv$FunctionsList      <- list(
#RegionalSeries  = list(actionMethod = "regionalSeries", 
#													  actionFile = file.path("code", "E.O.Env.KPFM.regional.series.01.R"))
#							  ,
							  StatePrint      = list(actionMethod = "printState", 
													  actionFile = file.path("code", "E.O.Env.KPFM.printState.01.R"))
							  ,UpdateAnnualEnv = list(actionMethod = "annualUpdate", 
													  actionFile = file.path("code", "E.O.Env.KPFM.annual_update.01.R"))
)
KrillEnv$OutputFiles        <- list(State = "KrillEnvState.dat")
KrillEnv$OutputFlags        <- list(PrintState = TRUE)

#   #############################################################
#   Ancillary functions
#   #############################################################
KrillEnv$Functions 			<- list(

			# function to undertake element-specific setup of actions
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup         = NULL,
#			RegionalSeries = list(actionMethod = KrillEnv$FunctionsList$RegionalSeries$actionMethod,
#							   actionFile = KrillEnv$FunctionsList$RegionalSeries$actionFile),
			printState = list(actionMethod = KrillEnv$FunctionsList$StatePrint$actionMethod,
								  actionFile   = KrillEnv$FunctionsList$StatePrint$actionFile,
								  dset   = list(  # List because may need more than one file to print state
											State = list(output = KrillEnv$OutputFlags$PrintState,
														 fname  = KrillEnv$OutputFiles$State,
														 path   = NULL)
									)
            ) 
)

KrillEnv$Timesteps 			<- list(
			Break_point  = list(  # knife-edge step for krill env.
				calday    = dayFromDate(01,10),
				actionsN = 1,
				actions   = list(
					printState = list(actionMethod = KrillEnv$FunctionsList$StatePrint$actionMethod,
										  actionFile = KrillEnv$FunctionsList$StatePrint$actionFile,
										  tsType = "FirstPeriod", 		# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										  tsTiming = "Before",    		# "Before","During","After"
										  relatedElements = NULL,
										  dset = list(  # List because may need more than one file to print state
													  State = list(output = KrillEnv$OutputFlags$PrintState,
																   fname  = KrillEnv$OutputFiles$State,
																   path   = NULL)
										  ) 
					) 
				)
			),
			Set_Krill_rec_env  = list(
				calday    = dayFromDate(01,10),
                actionsN = 1,
                actions   = list(
					# function reads next record, checking correct trial and year
					update.annual.environments = list(actionMethod = KrillEnv$FunctionsList$UpdateAnnualEnv$actionMethod,
													  actionFile = KrillEnv$FunctionsList$UpdateAnnualEnv$actionFile,
													  tsType = "LastPeriod", 	# "AllPeriods","FirstPeriod","LastPeriod")
													  tsTiming = "After",    	# "Before","During","After"
													  relatedElements = NULL, 	# matrix - col 1 = module; col 2 = absolute ID of element; if none then NULL
													  dSet      = NULL)       	# pattern for input to function
                ) 
            )
) 

# declare variable to be sourced
KrillEnv

