# Input data - B.MI.Es.KPFM.data.01.R
# Krill

# routine to enable processing of input data if needed but without function

Krill <- list()
Krill$signature 			<- list(
	ClassName       = "KrillKPFM",
	ID              = 22001,
	Name.full       = "Euphausia superba (Antarctic krill)",
	Name.short      = "Es-KPFM",
	Morph        	= "KPFM",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "7 July 2008"
)

Krill$polygonsN		 		<- 1
Krill$polygons            	<- c(1:Krill$polygonsN)
								# reference numbers to polygons in the list of
								# defined polygons

Krill$birthdate        	  	<- list(Day = 1, Month = 10)
								# day and month to be used as time 0 in the year
								# for the taxon

Krill$Recruit_data        	<- list(
							   dispersal = {dmat<-matrix(0,nrow=Krill$polygonsN,ncol=Krill$polygonsN);
											for (i in 1:Krill$polygonsN){dmat[i,i]<-1};dmat}
										   # matrix - rows: origin polygons
										   #          cols: destination polygons
										   #          cells: prop of origin ending up in destination
										   #                 over the period of time it takes from
										   #                 spawning to recruitment
							  ,Ehat      = rep(1,Krill$polygonsN)
										   # vector - value of Environment for which
										   #          survivorship of recruits = 1
							  ,MeanWt    = 0.46 # grams
										   # mean weight of individual krill
							  ,KPFM      = TRUE
										   # logical - if TRUE then use KPFM approximation in function
)
                           
#-------------------------------------------------------------------------------
Krill$Mortality          	<- list(summer = list(
                                          adults = list(M = rep(0.0,Krill$polygonsN))
                                          ) # end summer
									,winter = list(
                                          adults = list(M = rep(0.0,Krill$polygonsN))
                                          ) # end winter
)

#-------------------------------------------------------------------------------
Krill$Migration.data        <- list( # from Watters etal 2006 - instantaneous rates of movement (as M)
                           Season1 = matrix(c(1), ncol=Krill$polygonsN,byrow=TRUE) # end season 1
                          ,Season2 = matrix(c(1), ncol=Krill$polygonsN,byrow=TRUE) # end season 2
)
Krill$PolygonDensityCV      	<- rep(1.0,Krill$polygonsN)
#  ,Reprod.relatedElements = matrix(c(1,12001),ncol=2,byrow=TRUE),
#  ,Recruit.relatedElements = matrix(c(1,12001),ncol=2,byrow=TRUE),
Krill$Reprod.relatedElements 	<- matrix(c("Environment", "KrillEnv"),ncol=2,byrow=TRUE)
Krill$Recruit.relatedElements 	<- matrix(c("Environment", "KrillEnv"),ncol=2,byrow=TRUE)
Krill$Reprod_Cond          		<- list(
                           P      = c( # vector - Biomass Standard for productivity in each polygon
                                            4.7*10^12 # APPA
                                      ) # end P
                          ,alpha  = rep(7.9,Krill$polygonsN)
                                    # vector - maximum per mass recruitment for each polygon
                          ,beta   = rep(0.01,Krill$polygonsN)
                                    # vector - prop of K to give 0.5 max recruits for each polygon
                          ,lag    = 2
                                    # integer >0, number of years lag between spawning and recruitment
                          ,KPFM   = TRUE
                                    # logical - if TRUE then use KPFM approximation in function
)
Krill$ScaleToTonnes         <- 1E-6
Krill$Init.density          <- c( 11.2 # APPA
)
Krill$Init.density.multiplier <- 1E1
Krill$Stage                 <- list(StageN = 1
                               ,StageStrUnits  = 1 # (1 = N, 2 = B)
                               ,StageStr  = rep(list(matrix(c(1,1),ncol=2,byrow=TRUE)),Krill$polygonsN)
                               ,StageSize = rep(list(c(0.46)),Krill$polygonsN)
)
Krill$Initialise            <- list(NULL)
Krill$Transition.data       <- list(Availability = rep(1.0,Krill$polygonsN) # availability in each polygon
                               ,CompetitorElements = NULL # enter as matrix - cols module, element
                               ,CompetitionCoefficients = NULL # competition coefficients in each polygon
)
Krill$PrintState        <- list(OutDir   = NULL, OutFiles = NULL)
Krill$FunctionsList      <- list(Reproduce       = list(actionMethod = "updateReprodCond", 
													  actionFile = file.path("code", "B.MI.Es.KPFM.update.reprod.condition.01.R")),
								Recruit         = list(actionMethod = "recruit",
													  actionFile = file.path("code", "B.MI.Es.KPFM.recruit.01.R")),
								Migrate_setup   = list(actionMethod = "migrateSetup",
													  actionFile = file.path("code", "B.MI.Es.KPFM.migrate.setup.01.R")),
								Migrate         = list(actionMethod = "migrate",
												      actionFile = file.path("code", "B.MI.Es.KPFM.migrate.01.R")),
								Mortality       = list(actionMethod = "mortality",
													  actionFile = file.path("code", "B.MI.Es.KPFM.Mortality.01.R")),
								Mortality_setup = list(actionMethod = "mortalitySetup",
													  actionFile = file.path("code", "B.MI.Es.KPFM.Mortality.setup.01.R")),
								StatePrint      = list(actionMethod = "printState",
													  actionFile = file.path("code", "B.MI.Es.KPFM.printState.01.R")),
								StateUpdate     = list(actionMethod = "updateState",
													  actionFile = file.path("code", "B.MI.Es.KPFM.UpdateState.01.R"))#,
								# DoStuff     	= list(actionMethod = "doStuff",
													  # actionFile = file.path("code", "DoStuff.R"))
)
Krill$OutputFiles       <- list(State_N = "Biota.Krill.State.N.dat"
                             ,State_B = "Biota.Krill.State.B.dat"
                             ,State_RepCond = "Biota.Krill.State.RepCond.dat"
)
Krill$OutputFlags       <- list(PrintState_N = TRUE
                             ,PrintState_B = TRUE
                             ,PrintState_RepCond = TRUE
)
							 
Krill$Functions <- list(
            # function to undertake element-specific setup of actions
            # (not including the generalised actions)
            # e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
            setup = NULL,
            printState = list(actionMethod = Krill$FunctionsList$StatePrint$actionMethod,
							  actionFile   = Krill$FunctionsList$StatePrint$actionFile,
                              dset = list(  # List because may need more than one file to print state
                                        Number = list(output = Krill$OutputFlags$PrintState_N
                                                      ,fname  = Krill$OutputFiles$State_N
                                                      ,path   = NULL),
                                        Biomass = list(output = Krill$OutputFlags$PrintState_B
                                                      ,fname  = Krill$OutputFiles$State_B
                                                      ,path   = NULL),
                                        Reprod_Cond = list(output = Krill$OutputFlags$PrintState_RepCond
                                                      ,fname  = Krill$OutputFiles$State_RepCond
                                                      ,path   = NULL)
										)
            ),
            stateUpdate      = list(actionMethod = Krill$FunctionsList$StateUpdate$actionMethod,
									actionFile = Krill$FunctionsList$StateUpdate$actionFile,
									dset   = list(Availability = Krill$Transition.data$Availability))
)
		
	#   #############################################################
	#   Taxon$TimeSteps
	#   #############################################################

	#   the characteristics of a time step between the previous time and the specified time (in days)
	#   is given in a list(days in calendar year, number of functions to be carried out, list of named functions)
	#   knife-edge functions can be included by repeating the same day
Krill$Timesteps 		<- list(

		Summer = list(calday=dayFromDate(31,3),
			actionsN=NULL,  # will be updated below
			actions=list(
				reproduce = list(actionMethod = Krill$FunctionsList$Reproduce$actionMethod,
							     actionFile = Krill$FunctionsList$Reproduce$actionFile,
								 tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								 tsTiming = "Before",    # "Before","During","After"
								 transAction = NULL, # list(fn = , dset = )
								 relatedElements = Krill$Reprod.relatedElements, # matrix - col 1 = module; col 2 = absolute ID of element; if none then NULL
								 dset 			 = Krill$Reprod_Cond
							), 

				mortality = list(actionMethod = Krill$FunctionsList$Mortality$actionMethod,
							     actionFile = Krill$FunctionsList$Mortality$actionFile,
								 tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								 tsTiming = "During",    # "Before","During","After"
								 transAction = list(actionMethod = Krill$FunctionsList$Mortality_setup$actionMethod,
														  actionFile = Krill$FunctionsList$Mortality_setup$actionFile,
														  dset = NULL),
								 relatedElements = NULL,
								 dset   	  = Krill$Mortality[[1]]
							),

				migrate   = list(actionMethod = Krill$FunctionsList$Migrate$actionMethod,
								 actionFile = Krill$FunctionsList$Migrate$actionFile,
								 tsType = "AllPeriods", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								 tsTiming = "During",    # "Before","During","After"
								 transAction = list(actionMethod = Krill$FunctionsList$Migrate_setup$actionMethod,
															  actionFile = Krill$FunctionsList$Migrate_setup$actionFile,
															  dset = NULL),
								 relatedElements = NULL,
								 dset   		 = Krill$Migration.data[[1]]
							),
				printState = list(actionMethod = Krill$FunctionsList$StatePrint$actionMethod,
								  actionFile = Krill$FunctionsList$StatePrint$actionFile,
								  tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								  tsTiming = "After",    # "Before","During","After"
								  relatedElements = NULL,
								  dset   = list(  # List because may need more than one file to print state
												  Number = list(output = Krill$OutputFlags$PrintState_N
																,fname  = Krill$OutputFiles$State_N
																,path   = NULL)
												 ,Biomass = list(output = Krill$OutputFlags$PrintState_B
																,fname  = Krill$OutputFiles$State_B
																,path   = NULL)
												 ,Reprod_Cond = list(output = Krill$OutputFlags$PrintState_RepCond
																,fname  = Krill$OutputFiles$State_RepCond
																,path   = NULL)
												)
							)#,
				# doStuff = list(actionMethod = Krill$FunctionsList$DoStuff$actionMethod,
								  # actionFile = Krill$FunctionsList$DoStuff$actionFile,
								  # tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								  # tsTiming = "After",    # "Before","During","After"
								  # relatedElements = NULL,
								  # dset   = list(  # List because may need more than one file to print state
												  # Number = list(output = Krill$OutputFlags$PrintState_N
																# ,fname  = Krill$OutputFiles$State_N
																# ,path   = NULL)
												 # ,Biomass = list(output = Krill$OutputFlags$PrintState_B
																# ,fname  = Krill$OutputFiles$State_B
																# ,path   = NULL)
												 # ,Reprod_Cond = list(output = Krill$OutputFlags$PrintState_RepCond
																# ,fname  = Krill$OutputFiles$State_RepCond
																# ,path   = NULL)
												# )
							# )
				)
		),
		Winter = list(calday=dayFromDate(30,9),
			actionsN=NULL, # will be updated below
			actions=list(
				recruit   = list(actionMethod = Krill$FunctionsList$Recruit$actionMethod,
								 actionFile = Krill$FunctionsList$Recruit$actionFile,
								 tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								 tsTiming = "Before",    # "Before","During","After"
								 transAction = NULL, # list(fn = , dset = )
								 relatedElements = Krill$Recruit.relatedElements,
								 dset   = Krill$Recruit_data
							),

				mortality = list(actionMethod = Krill$FunctionsList$Mortality$actionMethod,
								 actionFile = Krill$FunctionsList$Mortality$actionFile,
								 tsType = "AllPeriods",		# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								 tsTiming = "During" ,   		# "Before","During","After"
								 transAction = list(actionMethod = Krill$FunctionsList$Mortality_setup$actionMethod,
														  actionFile = Krill$FunctionsList$Mortality_setup$actionFile,
														  dset = NULL),
								 relatedElements = NULL,
								 dset   = Krill$Mortality[[2]]
							),

				migrate   = list(actionMethod = Krill$FunctionsList$Migrate$actionMethod,
								 actionFile = Krill$FunctionsList$Migrate$actionFile,
								 tsType = "AllPeriods", 		# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								 tsTiming = "During",    		# "Before","During","After"
								 transAction = list(actionMethod = Krill$FunctionsList$Migrate_setup$actionMethod,
															   actionFile = Krill$FunctionsList$Migrate_setup$actionFile,
															   dset = NULL),
								 relatedElements = NULL,
								 dset   = Krill$Migration.data[[2]]
							),
				printState = list(actionMethod = Krill$FunctionsList$StatePrint$actionMethod,
								  actionFile = Krill$FunctionsList$StatePrint$actionFile,
								  tsType = "LastPeriod", 		# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								  tsTiming = "After",    		# "Before","During","After"
								  relatedElements = NULL,
								  dset   = list(  # List because may need more than one file to print state
												Number = list(output = Krill$OutputFlags$PrintState_N,
															  fname  = Krill$OutputFiles$State_N,
															  path   = NULL),
												Biomass = list(output = Krill$OutputFlags$PrintState_B,
															   fname  = Krill$OutputFiles$State_B,
															   path   = NULL),
												Reprod_Cond = list(output = Krill$OutputFlags$PrintState_RepCond,
																   fname  = Krill$OutputFiles$State_RepCond,
																   path   = NULL)
												) 
							)
			) 
		)
)
		
# declare variable to be sourced
Krill
