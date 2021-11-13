# Input data - M.CL.KPFM.manager.data.01.R
# KPFM SSMU TAC manager

# notes on development of general function - mark completed as done

#    SurveySSMU              (done)  M.CL.KPFM.manager.SurveySSMU.01.R
#    CatchRemaining          (done)  M.CL.KPFM.manager.CatchRemaining.01.R
#    SetHarvestStrategy      (done)  M.CL.KPFM.manager.SetHarvestStrategy.01.R
#    StartSeason             (done)  M.CL.KPFM.manager.StartSeason.01.R
#    Scenario_setup             (done)  M.CL.KPFM.manager.Time0.fn.01.R
#    StatePrint              (todo)  M.CL.KPFM.manager.printState.01.R


################################################################################
# start data set

Manage <- list()
#-------------------------------------------------------------------------------
Manage$signature		<- list(
	ClassName		= "Manager",
	ID             	= 42001,
	Name.full       = "KPFM TAC setting process - SSMUs",
	Name.short      = "SSMUtac",
	Morph        	= "KPFM",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "7 July 2008"
)
	
Manage$polygonsN 		<- 15  # number of local populations
	
Manage$Polygons        <- c(1:15)
                           # reference numbers to polygons in the list of
                           # defined polygons

Manage$Birthdate       <- list(Day = 1, Month = 12)
                           # day and month to be used as time 0 in the year
                           # for the taxon

#-------------------------------------------------------------------------------
Manage$ScaleToTonnes   <- 1
  
#-------------------------------------------------------------------------------
Manage$Init.TAC.SSMU   <- rep(0,15)     # catch limits set to zero (NA would mean no fishing controls)

#-------------------------------------------------------------------------------
Manage$SSMU.Surveys    <- list( # one vector for each survey
                                 # polygons in which surveys are undertaken
								CCAMLR2000 = c(1:Manage$polygonsN)
)

Manage$SSMU.Survey.Elements  	<- matrix(c("Biota", "Krill"),ncol=2,byrow=TRUE) # krill

Manage$SSMU.Survey.Details 	<- list( #
					SurveyDetails = list(
						CCAMLR2000 = list(
							Survey.Year     = 1952
						   ,Target.elements = c(1)  # vector - row positions of elements from Survey.Elements
						   ,Polygons        = c(1:15)
						   ,Target.details = list(
								Krill = list(
									TargetPn         = list( # for each polygon for survey (relative reference) give proportion of target polygon in survey polygon
											P1 = list(Pns = matrix(c(1,1),ncol=2,byrow=TRUE) # polygon number, proportion in survey polygon
													 ,PnN = 1)
										   ,P2 = list(Pns = matrix(c(2,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P3 = list(Pns = matrix(c(3,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P4 = list(Pns = matrix(c(4,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P5 = list(Pns = matrix(c(5,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P6 = list(Pns = matrix(c(6,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P7 = list(Pns = matrix(c(7,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P8 = list(Pns = matrix(c(8,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P9 = list(Pns = matrix(c(9,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P10 = list(Pns = matrix(c(10,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P11 = list(Pns = matrix(c(11,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P12 = list(Pns = matrix(c(12,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P13 = list(Pns = matrix(c(13,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P14 = list(Pns = matrix(c(14,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
										   ,P15 = list(Pns = matrix(c(15,1),ncol=2,byrow=TRUE)
													 ,PnN = 1)
									)
								)
							)
						)
					)
				   ,SurveysN = 1
				   ,OutputFile      = "SSMU_surveys.dat"
				   ,OutputFlag      = TRUE
				   ,OutputPath      = NULL
)
                     
Manage$SetHarvestStrategy  <- list(
								Elements = matrix(c("Biota", "Krill"),ncol=2,byrow=TRUE)
							   ,dset     = list(
									SetInYear = 1952
								   ,SurveyYear = 1952
								   ,Gamma      = 0.1
								)
)
	
Manage$SeasonStart   		<- list(Day = 1, Month = 12)
                            
	#SeasonStart$Day $Month
Manage$CatchRemaining       <-list(
                            Elements = matrix(c("Activity", "KrillFishery"),ncol=2,byrow=TRUE) # fisheries
                           ,dset     = NULL
)
#-------------------------------------------------------------------------------
Manage$RuntimeFile         <- "SurveyDevs0101.dat"
Manage$ReplaceFile         <- TRUE
Manage$Deviates            <- list(Seed = 5873, UseRandSeq = TRUE, CV = 1.0)

#-------------------------------------------------------------------------------
Manage$Initialise          <- list(NULL)
Manage$Transition.data     <- list(NULL)
Manage$PrintState          <- list(OutDir   = NULL, OutFiles = NULL)
Manage$FunctionsList       <- list (SurveySSMU 	= list(actionMethod = "surveySSMU", 
														   actionFile = file.path("code", "M.CL.KPFM.manager.SurveySSMU.01.R")),
								CatchRemaining 		= list(actionMethod = "catchRemaining", 
													       actionFile = file.path("code", "M.CL.KPFM.manager.CatchRemaining.01.R")),
								SetHarvestStrategy 	= list(actionMethod = "setHarvestStrategy", 
													       actionFile = file.path("code", "M.CL.KPFM.manager.SetHarvestStrategy.01.R")),
								StartSeason 		= list(actionMethod = "startSeason", 
														   actionFile = file.path("code", "M.CL.KPFM.manager.StartSeason.01.R")),
								StatePrint 			= list(actionMethod = "printState", 
														   actionFile = file.path("code", "M.CL.KPFM.manager.printState.01.R"))
)
#-------------------------------------------------------------------------------
Manage$OutputFiles         <- list(State_TAC    = "SSMU_TAC.dat"
                             ,State_Survey = "SSMU_surveys.dat"
)
#-------------------------------------------------------------------------------
Manage$OutputFlags         <- list(Print_State_TAC = TRUE
                             ,Print_State_Survey = TRUE
)

Manage$Functions 			<- list(

			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup        = NULL,

			printState  = list(actionMethod = Manage$FunctionsList$StatePrint$actionMethod,
							   actionFile   = Manage$FunctionsList$StatePrint$actionFile, 
							   dset   = list(  # List because may need more than one file to print state
                                        Catch = list(output = Manage$OutputFlags$Print_State_TAC	
                                                      ,fname  = Manage$OutputFiles$State_TAC
                                                      ,path   = NULL),
										# TR Added, to allow truncation of SSMU_survey.dat file 
										Survey = list(output = Manage$OutputFlags$Print_State_Survey,
													 fname  = Manage$OutputFiles$State_Survey,
													 path   = NULL)
								)
            ),
			stateUpdate = NULL 
)
		
#   #############################################################
		#   Elmnt$TimeSteps
		#   #############################################################

		#   the characteristics of a time step between the previous time and the specified time (in days)
		#   is given in a list(days in calendar year, number of functions to be carried out, list of named functions)
		#   knife-edge functions can be included by repeating the same day

		#  Actions (s = summer, w = winter) in InputData$Functions
Manage$Timesteps 			<- list(
			StartSeasonAndSurvey     = list(
				calday = dayFromDate(01,02),
                actionsN = NULL, # will be updated below
                actions=list(
                    catchRemaining 		= list(actionMethod = Manage$FunctionsList$CatchRemaining$actionMethod,
												actionFile = Manage$FunctionsList$CatchRemaining$actionFile,
												tsType = "AllPeriods", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
												tsTiming = "Before",    # "Before","During","After"
												transAction = NULL,
												relatedElements = Manage$CatchRemaining$Elements, # fisheries
												dset   = Manage$CatchRemaining$dset
                    ),
                    printState		= list(actionMethod = Manage$FunctionsList$StatePrint$actionMethod,
												actionFile = Manage$FunctionsList$StatePrint$actionFile,
                                                tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
                                                tsTiming = "Before",    # "Before","During","After"
                                                relatedElements = NULL,
                                                dset   = list(  # List because may need more than one file to print state
															Catch = list(output = Manage$OutputFlags$Print_State_TAC,
                                                                         fname  = Manage$OutputFiles$State_TAC,
                                                                         path   = NULL),
															# TR Added, to allow truncation of SSMU_survey.dat file 
															Survey = list(output = Manage$OutputFlags$Print_State_Survey,
                                                                         fname  = Manage$OutputFiles$State_Survey,
                                                                         path   = NULL)
                                                            )
                    ),
                    setHarvestStrategy 	= list(actionMethod = Manage$FunctionsList$SetHarvestStrategy$actionMethod,
												actionFile = Manage$FunctionsList$SetHarvestStrategy$actionFile,
												tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
												tsTiming = "Before",    # "Before","During","After"
												transAction = NULL,
												relatedElements = Manage$SetHarvestStrategy$Elements,
												dset   = Manage$SetHarvestStrategy$dset
                    ),
                    startSeason 		= list(actionMethod = Manage$FunctionsList$StartSeason$actionMethod,
												actionFile = Manage$FunctionsList$StartSeason$actionFile,
												tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
												tsTiming = "Before",    # "Before","During","After"
												transAction = NULL,
												relatedElements = NULL,
												dset   = NULL
                    ),
                    krillSurvey 		= list(actionMethod = Manage$FunctionsList$SurveySSMU$actionMethod,
												actionFile = Manage$FunctionsList$SurveySSMU$actionFile,
												tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
												tsTiming = "After",    # "Before","During","After"
												transAction = NULL,
												relatedElements = Manage$SSMU.Survey.Elements,
												dset   = Manage$SSMU.Survey.Details
                    )
                )
            ),
			EndFishingSeason  = list(
				calday = dayFromDate(30,11),
                actionsN = NULL,
                actions=list(
					# catch remaining is for during the season - StartSeason will adjust catch remaining at beginning of season
					catchRemaining 		= list(actionMethod = Manage$FunctionsList$CatchRemaining$actionMethod,
												actionFile = Manage$FunctionsList$CatchRemaining$actionFile,
												tsType = "AllPeriods", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
												tsTiming = "Before",    # "Before","During","After"
												transAction = NULL,
												relatedElements = Manage$CatchRemaining$Elements, # fisheries
												dset   = Manage$CatchRemaining$dset
                    )
                )
            )
)
		
# declare variable to be sourced
Manage