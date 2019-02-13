# Input data - B.GET.Krill.data.01.R
# Description:

Krill 					<- list()
################################################################################
# 1. Signature
#-------------------------------------------------------------------------------

Krill$signature			<- list(
	ClassName		    = "GET",
	ID             	= 22001,
	Name.full       = "GET Krill",
	Name.short      = "Krill",
	Morph        	= "Krill",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "6 June 2015"
)

################################################################################
# 2. Spatial structure
#-------------------------------------------------------------------------------

Krill$polygonsN 			   <- 1
Krill$Polygons           <- c(1)
                           # reference numbers to polygons in the list of
                           # defined polygons in the spatial file


################################################################################
# 3. Calendar
#-------------------------------------------------------------------------------

Krill$Birthdate          <- list(Day = 1, Month = 10)
                           # day and month to be used as time 0 in the year
                           # for the taxon

Krill$TimeStepEndDays    <- c(dayFromDate(31,3),dayFromDate(30,9))
Krill$TimeStepNames      <- c("Summmer","Winter")

################################################################################
# 4. Elements required in Universe
#-------------------------------------------------------------------------------

Krill$ElementsRequired   <- matrix(c("Biota","PPfood"),byrow=TRUE,ncol=2)


################################################################################
# 5. State
#-------------------------------------------------------------------------------

Krill$ScaleToTonnes       <- 0.005
  
#-------------------------------------------------------------------------------
# Initial abundance - vector for polygons
#   units are those identified as the primary units for determining stage frequencies

Krill$Init.abundance      <- c(
                             74798    #  3 APDPW
                            ) # end Init.abundance


#			Stage.Str <- c(Stage.Str,Stage.Str[stage$JuveAgeN]*
#                       exp(-(mortality[[1]][[stage$JuveAgeN]]$M[pn]+
#                             mortality[[2]][[stage$JuveAgeN]]$M[pn]))/
#                       (1-exp(-(mortality[[1]][[(stage$JuveAgeN+1)]]$M[pn]+
#                             mortality[[2]][[(stage$JuveAgeN+1)]]$M[pn])))
#                       )

#-------------------------------------------------------------------------------
Krill$StageN <- 3

Krill$Stage             <- list(StageN         = Krill$StageN         # "larvae", "recruited juveniles", "adults"
                               ,Maturity       = c(0,0,1)  # maturity ogive
                               ,StageStrUnits  = 1         # (1 = N, 2 = B)
                               ,StageStr       = matrix(rep(c(0,0.3,0.7),Krill$polygonsN),ncol=,Krill$polygonsN)      # list by polygon TO BE PROVIDED
                               ,StageSize      = matrix(   # established as a matrix: cols=polygon, rows=stage
                                                        rep(c(0.1 # larvae
                                                           ,0.1 # juveniles (will grow according to per capita growth
                                                           ,0.46# adults
                                                      ),Krill$polygonsN),nrow=3)
		                            ,StageCondR    = matrix(rep(0, Krill$StageN*Krill$polygonsN),ncol=Krill$polygonsN)
		                            ,StageCondH    = matrix(rep(1, Krill$StageN*Krill$polygonsN),ncol=Krill$polygonsN)
                               )# end Stage


################################################################################
# 6. Abundance actions data
#-------------------------------------------------------------------------------

Krill$data.Mortality <- list(AllTimes = list(  # if different for each time period then have one list of mortality data for each stage according to each time step
                                   # M = nominal mortality over period
                                   # z = threshold of health condition below which survivorship is impacted by health
                                   # vprime = calculate this value as (-0.693/log(z[pn]/v[pn])) (v is value of health condition for which survivorship is reduced to 0.5  (v must be less than z)

                                 Larvae      = list (M = c(0.0)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,Krill$polygonsN)
                                                                ,vprime = rep(0,Krill$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,Juvenile      = list (M = c(0.0)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,Krill$polygonsN)
                                                                ,vprime = rep(0,Krill$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,Adult     = list (M = c(0.0)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,Krill$polygonsN)
                                                                ,vprime = rep(0,Krill$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                             ) # end time period
                                 )# end list Mortality


#-------------------------------------------------------------------------------
Krill$data.Migration <- list(AllTimes = list(  # if different for each time period then have one list of migration data for each stage according to each time step
                                Larvae   = matrix(c(1), ncol=Krill$polygonsN,byrow=TRUE)
                               ,Juvenile = matrix(c(1), ncol=Krill$polygonsN,byrow=TRUE)
                               ,Adult    = matrix(c(1), ncol=Krill$polygonsN,byrow=TRUE)
                               ) # end time
                             ) # end migration data

Krill$data.advanceStage <- list(  # by polygon  # uses function advanceStageWtdBMS
                           # matrix for advancing each stage to the next stage with the adult stage being a plus stage
                           # cols = new stage, rows = proportion from each old stage going to new stage
                            P1 = matrix(c(0,1,0, 
                                          0,0,1,
                                          0,0,1),
                                          ncol=3,byrow=TRUE) # end polygon
                              ) # end recruitment

################################################################################
# 7. Energetics actions data (consumption, growth, reproduction
#-------------------------------------------------------------------------------

Krill$Consume  			<- list(
         relatedElements = matrix(c("Biota", "PPfood"),ncol=2,byrow=TRUE) # krill
        ,feeding.Polygon    = c(1)  # reference numbers for polygons in polygon
                                     # list in which feeding can occur by local populations
                                     # this list is used to set up the proportions
                                     # of prey polygons in the feeding polygons below
        ,feeding.Polygon.N   = 1
       # reference to related elements below is by relative row number
        ,dset = list(

             #-----------------------------------
              AllTimes  = list( # by predator stage - if NULL then no consumption by that stage
                    #-------------------------
                      Larvae   = NULL
                    #-------------------------
                     ,Juvenile = list(
                          feeding.Polygon.N   = 1
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
                           1),ncol=1,byrow=TRUE)
                         ,Prey = list(
                            PPfood = list(
                                    PerCapita            = 556054.5  # maximum per capita consumption per year
                                   ,PropInDiet           = 1  # proportion of this prey in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 30
                                   ,Holling_units        = 1
                                   ,Holling_availability = c(1) # prey stage structure (like selectivity) used to calculate prey density for Holling equation (i.e. what consumers can see)
                                   ,Prey_selectivity     = c(1)
                                   ,PreyPn         = list( # for each predator feeding polygon, list prey polygons (relative reference) and proportion of prey polygon in predator polygon
                                            P1 = list(Pns = matrix(c(1,1),ncol=2,byrow=TRUE) # polygon number, proportion in pred polygon
                                                     ,PnN = 1)
                                                   ) # end PreyPn list
                                   ) # end individual prey list
                               ) # end Prey list
                           ) # end stage

                    #-------------------------
                     ,Adult = list(
                          feeding.Polygon.N   = 1
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
                           1),ncol=1,byrow=TRUE)
                         ,Prey = list(
                            PPfood = list(
                                    PerCapita            = 556054.5  # maximum per capita consumption per year
                                   ,PropInDiet           = 1  # proportion of this prey in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 30
                                   ,Holling_units        = 1
                                   ,Holling_availability = c(1) # prey stage structure (like selectivity) used to calculate prey density for Holling equation (i.e. what consumers can see)
                                   ,Prey_selectivity     = c(1)
                                   ,PreyPn         = list( # for each predator feeding polygon, list prey polygons (relative reference) and proportion of prey polygon in predator polygon
                                            P1 = list(Pns = matrix(c(1,1),ncol=2,byrow=TRUE) # polygon number, proportion in pred polygon
                                                     ,PnN = 1)
                                                   ) # end PreyPn list
                                   ) # end individual prey list
                               ) # end Prey list
                           ) # end stage
                    #-------------------------
                    ) # end time period list
                #-------------------------
                 ) # end dset list
           #-------------------------
            ) # end consumption


Krill$data.AllocateConsumption<-list(Summer = list(
                                         IsConsumer  = c(FALSE, TRUE, TRUE) # vector - logical to indicate if stage is a consumer
                                        ,FoodValue =  list(Larvae    = 0, # vector - food value for each prey type, in order of Prey in list of dependent elements
                                                           Juveniles = 0,
                                                           Adults    = 0)
                                        ,AllocateToReproduction = TRUE
                                        ,dMetab   = list(Larvae    = 0,    # per mass metabolic requirements per year
                                                         Juveniles = 0,
                                                         Adults    = 0)
                                        ,dReprod  = list(Larvae    = list(Rmax = 0),
                                                         Juveniles = list(Rmax = 0),
                                                         Adults    = list(Rmax = 1) # average Rmax = RmaxFemale*SexRatio*MaturityOgive
                                                         ) # end dReprod
                                        ,dGrowth  = list(Larvae    = 1,
                                                         Juveniles = 1,
                                                         Adults    = 1)
                                        ,Starve   = list(Larvae    = 1,
                                                         Juveniles = 1,
                                                         Adults    = 1)

                                        ,Cpp      = list( # list by stage with a matrix (rows = origin, cols = destination) for immigration to destination polygons of
                                                          # the per capita food that the immigrants are given in their destination polygon
                                                          # Cpp relates to the proportion of the per capita food from the origin polygon that is used.
                                                          # The remainder (1-Cpp) is inherited as per capita food from the destination polygon

                                                      Larvae    = rep(1.0,Krill$polygonsN)  # 1 =  all per capita food for immigrants comes from origin polygon
                                                     ,Juveniles = rep(1.0,Krill$polygonsN)
                                                     ,Adults    = rep(1.0,Krill$polygonsN)
                                                       ) # end Cpp
                                        ) # end summer (allocation to reproduction only occurs in summer
                                    ) # end data.AllocateConsumption


# for efficiency the EggDistribution matrix is divided by the cost (gonad mass) of producing one egg
EggDistnMat<-matrix(rep(0,Krill$polygonsN*Krill$polygonsN),ncol=Krill$polygonsN)
for(i in 1:Krill$polygonsN) EggDistnMat[i,i] <- 1
EggProdn<-EggDistnMat/GonadToEggs


Krill$data.Spawn<-list(EggProdn    = EggProdn, # matrix that distributes eggs from source to destination polygons, including cost of producing eggs from gonads
                       Cond.S      = rep(1,Krill$polygonsN), # size to be inherited to Stage 1 - vector for polygons
                       Cond.R      = rep(0,Krill$polygonsN), # reproductive condition to be inherited to Stage 1 - vector for polygons
                       Cond.H      = rep(1,Krill$polygonsN) # health to be inherited to Stage 1 - vector for polygons
                       )# end spawn data

#-------------------------------------------------------------------------------
Krill$Initialise          <- list(NULL)
Krill$Transition.data     <- list(NULL) # end transition.data


Krill$PrintState          <- list(OutDir   = NULL, OutFiles = NULL)
Krill$FunctionsList       <- list(
								Mortality             = list(actionMethod = "mortality",
														                 actionFile   = file.path("code", "B.GET.Mortality.01.R")),
								Mortality_setup       = list(actionMethod = "mortalitySetup",
														                 actionFile   = file.path("code", "B.GET.Mortality.setup.01.R")),
								Migrate               = list(actionMethod = "migrate",
														                 actionFile   = file.path("code", "B.GET.migrate.01.R")),
								Migrate_setup         = list(actionMethod = "migrateSetup",
														                 actionFile   = file.path("code", "B.GET.migrate.setup.01.R")),
								AdvanceStage          = list(actionMethod = "advanceStageNumWtdBMS",
														                 actionFile   = file.path("code", "B.GET.AdvanceStageNumWtdBMS.01.R")),
								Consume               = list(actionMethod = "consume",
														                 actionFile   = file.path("code", "B.GET.Consume.01.R")),
								Consume_setup         = list(actionMethod = "consumeSetup", 
														                 actionFile   = file.path("code", "B.GET.Consume.setup.01.R")),
								AllocateConsumption   = list(actionMethod = "allocateConsumption",
														                 actionFile   = file.path("code", "B.GET.AllocateConsumption.01.R")),
								Spawn                 = list(actionMethod = "spawn",
														                 actionFile   = file.path("code", "B.GET.Spawn.01.R")),
								StateUpdate           = list(actionMethod = "updateState",
														                 actionFile   = file.path("code", "B.GET.UpdateState.01.R"))
								StatePrint            = list(actionMethod = "printState",
														                 actionFile   = file.path("code", "B.GET.printState.01.R")),
#								TransitionCompetition = list(actionMethod = "transitionCompetition",
#														                 actionFile   = file.path("code", "B.GET.TransitionCompetition.01.R"))
)


#-------------------------------------------------------------------------------
Krill$OutputFiles     <- list(State_N       = "B.Krill.State.N.dat"
                             ,State_B       = "B.Krill.State.B.dat"
                             ,State_Stage   = "B.Krill.State.Stage.dat"
                             ,State_Size    = "B.Krill.State.Size.dat"
                             ,State_RepCond = "B.Krill.State.RepCond.dat"
                             ,State_Health  = "B.Krill.State.Health.dat"
)
#-------------------------------------------------------------------------------
Krill$OutputFlags     <- list(PrintState_N       = TRUE
                             ,PrintState_B       = TRUE
                             ,PrintState_Stage   = TRUE
                             ,PrintState_Size    = TRUE
                             ,PrintState_RepCond = TRUE
                             ,PrintState_Health  = TRUE
)
	
Krill$Functions 			<- list(
			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup = NULL,
			# data and function to initialise element at the beginning of each scenario
			#   i.e. how should the element be reset at time 0
			printState    = list(actionMethod = Krill$FunctionsList$StatePrint$actionMethod,
								           actionFile   = Krill$FunctionsList$StatePrint$actionFile,
								           dset         = list(  # List because may need more than one file to print state
									                Number      = list(output = Krill$OutputFlags$PrintState_N,
												                             fname  = Krill$OutputFiles$State_N,
												                             path   = NULL),
																	Biomass     = list(output = Krill$OutputFlags$PrintState_B,
																			     	         fname  = Krill$OutputFiles$State_B,
																			     	         path   = NULL),
																	Stage       = list(output = Krill$OutputFlags$PrintState_Stage,
																				  	         fname  = Krill$OutputFiles$State_Stage,
																				  	         path   = NULL),
																	Size        = list(output = Krill$OutputFlags$PrintState_Size,
																			  	   		     fname  = Krill$OutputFiles$State_Size,
																			  	   		     path   = NULL),
																	Reprod_Cond = list(output = Krill$OutputFlags$PrintState_RepCond,
																		  	   			     fname  = Krill$OutputFiles$State_RepCond,
																		  	   			     path   = NULL),
																	Health      = list(output = Krill$OutputFlags$PrintState_Health,
																	  	   			       fname  = Krill$OutputFiles$State_Health,
																	  	   			       path   = NULL)
																  ) # end dset
			                     ), # # end printState
      
			stateUpdate  = list(actionMethod = Krill$FunctionsList$StateUpdate$actionMethod,
								          actionFile   = Krill$FunctionsList$StateUpdate$actionFile,
								          dset         = NULL
			                    ) # end stateUpdate
       ) # end functions list
		
#   #############################################################
#   Taxon$TimeSteps
#   #############################################################

#   the characteristics of a time step between the previous time and the specified time (in days)
#   is given in a list(days in calendar year, number of functions to be carried out, list of named functions)
#   knife-edge functions can be included by repeating the same day

#  Actions (s = summer, w = winter) in InputData$Functions
	#s   - Allocate_breeders
	#s/w - Consume
	#s/w - Update_health
	#s/w - Update_reprod_cond
	#s   - Update_age
	#s   - Reproduce
	#s   - BreedersToNonbreeders

Krill$Timesteps 			<- list(
	Summer = list(calday   = dayFromDate(31,3),
		            actionsN = NULL,  # will be updated below
		            actions  = list(
														advanceStage 		 = list(actionMethod      = Krill$FunctionsList$AdvanceStage$actionMethod,
																											actionFile      = Krill$FunctionsList$AdvanceStage$actionFile,
																											tsType          = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
																											tsTiming        = "Before",    # "Before","During","After"
																											transAction     = NULL,
																											relatedElements = NULL,
																											dset            = Krill$data.advanceStage
															                        ), # end advance_stage

                       			consume     	     = list(actionMethod    = Krill$FunctionsList$Consume$actionMethod,
																											actionFile      = Krill$FunctionsList$Consume$actionFile,
																											tsType          = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
																											tsTiming        = "During",    # "Before","During","After"
																											transAction     = list(actionMethod = Krill$FunctionsList$Consume_setup$actionMethod,
											        								                               actionFile   = Krill$FunctionsList$Consume_setup$actionFile,
																																	           dset         = NULL),
																											relatedElements = Krill$Consume$relatedElements,
																											dset            = Krill$Consume$dset[[1]]
                                                			), # end consume

                            mortality   	     = list(actionMethod    = Krill$FunctionsList$Mortality$actionMethod,
																											actionFile      = Krill$FunctionsList$Mortality$actionFile,
																											tsType          = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
																											tsTiming        = "During",    # "Before","During","After"
																											transAction     = list(actionMethod = Krill$FunctionsList$Mortality_setup$actionMethod,
																																	           actionFile   = Krill$FunctionsList$Mortality_setup$actionFile,
																																	           dset         = NULL),
																											relatedElements = NULL,
																											dset            = Krill$data.Mortality[[1]]
			                                                ),

                            migrate    	       = list(actionMethod    = Krill$FunctionsList$Migrate$actionMethod,
																											actionFile      = Krill$FunctionsList$Migrate$actionFile,
																											tsType          = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
																											tsTiming        = "During",    # "Before","During","After"
																											transAction     = list(actionMethod = Krill$FunctionsList$Migrate_setup$actionMethod,
																																	           actionFile   = Krill$FunctionsList$Migrate_setup$actionFile,
																																	           dset         = NULL),
																											relatedElements = NULL,
																											dset            = Krill$data.Migration[[1]]
			                                                ),

                            allocateConsumption = list(actionMethod    = Krill$FunctionsList$AllocateConsumption$actionMethod,
																											actionFile      = Krill$FunctionsList$AllocateConsumption$actionFile,
																											tsType          = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
																											tsTiming        = "After",    # "Before","During","After"
																											transAction     = NULL,
																											relatedElements = Krill$Consume$relatedElements,
																											dset            = Krill$data.AllocateConsumption[[1]]
			                                                ),

                            spawn               = list(actionMethod    = Krill$FunctionsList$Spawn$actionMethod,
																											actionFile      = Krill$FunctionsList$Spawn$actionFile,
																											tsType          = "LastPeriod",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
																											tsTiming        = "After",    # "Before","During","After"
																											transAction     = NULL,
																											relatedElements = NULL,
																											dset            = Krill$data.Spawn[[1]]
			                                                ),

														printState 		  = list(actionMethod = Krill$FunctionsList$StatePrint$actionMethod,
																											actionFile = Krill$FunctionsList$StatePrint$actionFile,
																											tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
																											tsTiming = "After",    # "Before","During","After"
																											relatedElements = NULL,
																											dset = Krill$FunctionsList$StatePrint$dset
															                        ) # end printState
		) # end
	), # end summer

	Winter = list(calday=dayFromDate(30,9),
		actionsN=NULL, # will be updated below
		actions=list(
			consume      = list(actionMethod = Krill$FunctionsList$Consume$actionMethod,
								actionFile = Krill$FunctionsList$Consume$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = NULL,
								relatedElements = Krill$Consume$relatedElements,
								dset   = Krill$Consume$dset[[2]]
			),

			mortality    = list(actionMethod = Krill$FunctionsList$Mortality$actionMethod,
								actionFile = Krill$FunctionsList$Mortality$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = list(actionMethod = Krill$FunctionsList$Mortality_setup$actionMethod,
														actionFile = Krill$FunctionsList$Mortality_setup$actionFile,
														dset = NULL),
								relatedElements = NULL,
								dset   = Krill$Mortality[[2]]
			),

			update_rep_health_cond = list(actionMethod = Krill$FunctionsList$UpdateReprodHealth$actionMethod,
										actionFile = Krill$FunctionsList$UpdateReprodHealth$actionFile,
										tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    	# "Before","During","After"
										transAction = NULL,
										relatedElements = Krill$Consume$relatedElements,
										dset   = Krill$ReprodHealth[[2]]
			),

			printState   = list(actionMethod = Krill$FunctionsList$StatePrint$actionMethod,
								actionFile = Krill$FunctionsList$StatePrint$actionFile,
								tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "After",    # "Before","During","After"
								relatedElements = NULL,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Krill$OutputFlags$PrintState_N,
												  fname  = Krill$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Krill$OutputFlags$PrintState_B,
												   fname  = Krill$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Krill$OutputFlags$PrintState_Stage,
												  fname  = Krill$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Krill$OutputFlags$PrintState_RepCond,
													   fname  = Krill$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Krill$OutputFlags$PrintState_Health,
												  fname  = Krill$OutputFiles$State_Health,
												  path   = NULL)
								)
			)
		)
	)
)

# declare variable to be sourced
Krill
