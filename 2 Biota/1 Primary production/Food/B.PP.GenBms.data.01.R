# Input data - B.MI.Es.KPFM.data.01.R
# PPfood

# routine to enable processing of input data if needed but without function

PPfood <- list()
PPfood$signature 			<- list(
	ClassName       = "PPgenBMS",
	ID              = 22002,
	Name.full       = "Phytoplankton Food",
	Name.short      = "PPfood",
	Morph        	  = "PPfood",
	Revision      	= "01",
	Authors      	  = "A.Constable",
	Last.edit    	  = "4 June 2015"
)

PPfood$polygonsN		 		<- 1
PPfood$polygons            	<- c(1:PPfood$polygonsN)
								# reference numbers to polygons in the list of
								# defined polygons

PPfood$birthdate        	  	<- list(Day = 1, Month = 7)
								# day and month to be used as time 0 in the year
								# for the taxon

                           
#-------------------------------------------------------------------------------
PPfood$Sequest          	<- list(summer = list(
                                          adults = list(M = rep(0.0,PPfood$polygonsN))
                                          ) # end summer
									,winter = list(
                                          adults = list(M = rep(0.0,PPfood$polygonsN))
                                          ) # end winter
)

#-------------------------------------------------------------------------------
PPfood$Migration.data        <- list( # instantaneous rates of movement (as M)
                                      # for each matrix: rows = origin, cols = destination
                           Season1 = matrix(c(1), ncol=PPfood$polygonsN,byrow=TRUE) # end season 1
                          ,Season2 = matrix(c(1), ncol=PPfood$polygonsN,byrow=TRUE) # end season 2
)
PPfood$PolygonDensityCV      	<- rep(1.0,PPfood$polygonsN)
PPfood$Food.relatedElements 	<- matrix(c("Environment", "EV01"),ncol=2,byrow=TRUE)
PPfood$ScaleToTonnes         <- 1E-6
PPfood$Init.density          <- c( 0 # APPA
)
PPfood$Init.density.multiplier <- 1E0
PPfood$Stage                 <- list(StageN = 1
                               ,StageStrUnits  = 2 # (1 = N, 2 = B)
                               ,StageStr  = rep(list(matrix(c(1,1),ncol=2,byrow=TRUE)),PPfood$polygonsN)
                               ,StageSize = rep(list(c(1)),PPfood$polygonsN)
)
PPfood$Initialise            <- list(NULL)
PPfood$Transition.data       <- list(Availability = rep(1.0,PPfood$polygonsN) # availability in each polygon
                               ,CompetitorElements = NULL # enter as matrix - cols module, element (one row for each consummer ie. row in CompetitionCoefficients
                               ,CompetitionCoefficients = NULL # competition coefficients in each polygon for each consumer competitor listed in CompetitorElements - one row for each competitor
)
PPfood$PrintState        <- list(OutDir   = NULL, OutFiles = NULL)
PPfood$FunctionsList      <- list(
								UpdateFood      = list(actionMethod = "foodUpdate"
												              ,actionFile   = file.path("code", "B.PP.GenBms.FoodUpdate.01.R")),
								Migrate_setup   = list(actionMethod = "migrateSetup"
													            ,actionFile   = file.path("code", "B.PP.GenBms.migrate.setup.01.R")),
								Migrate         = list(actionMethod = "migrate"
												              ,actionFile   = file.path("code", "B.PP.GenBms.migrate.01.R")),
								Sequest         = list(actionMethod = "sequest"
													            ,actionFile   = file.path("code", "B.PP.GenBms.Sequest.01.R")),
								Sequest_setup = list(actionMethod = "sequestSetup",
													  actionFile = file.path("code", "B.PP.GenBms.Sequest.setup.01.R")),
								StatePrint      = list(actionMethod = "printState",
													  actionFile = file.path("code", "B.PP.GenBms.printState.01.R")),
								StateUpdate     = list(actionMethod = "updateState",
													  actionFile = file.path("code", "B.PP.GenBms.UpdateState.01.R"))#,
)
PPfood$OutputFiles       <- list(State_B = "Biota.PPfood.State.B.dat"
)
PPfood$OutputFlags       <- list(PrintState_B = TRUE
)
							 
PPfood$Functions <- list(
            # function to undertake element-specific setup of actions
            # (not including the generalised actions)
            # e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
            setup = NULL
           ,printState = list(actionMethod = PPfood$FunctionsList$StatePrint$actionMethod,
							  actionFile   = PPfood$FunctionsList$StatePrint$actionFile,
                              dset = list(  # List because may need more than one file to print state
                                        Biomass = list(output = PPfood$OutputFlags$PrintState_B
                                                      ,fname  = PPfood$OutputFiles$State_B
                                                      ,path   = NULL)
										                      )
                               ) # end printState
           ,stateUpdate      = list(actionMethod = PPfood$FunctionsList$StateUpdate$actionMethod,
									actionFile = PPfood$FunctionsList$StateUpdate$actionFile,
									dset   = list(Availability = PPfood$Transition.data$Availability)) # end stateUpdate
           ) # end Functions
		
	#   #############################################################
	#   Taxon$TimeSteps
	#   #############################################################

	#   the characteristics of a time step between the previous time and the specified time (in days)
	#   is given in a list(days in calendar year, number of functions to be carried out, list of named functions)
	#   knife-edge functions can be included by repeating the same day

PPfood$TimestepIntervals = c(31, 61, 92, 122, 153, 183, 213, 244, 274, 305, 335, 365) # copied from EnvGenVar

PPfood$TimestepShell = list(
      calday      =  NULL  # will fill in from TimestepIntervals
		 ,actionsN    =  NULL  # will be updated below
		 ,actions     =  list(
				 foodUpdate = list(actionMethod    = PPfood$FunctionsList$UpdateFood$actionMethod
							            ,actionFile      = PPfood$FunctionsList$UpdateFood$actionFile
								          ,tsType          = "FirstPeriod"  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								          ,tsTiming        = "Before"    # "Before","During","After"
								          ,transAction     = NULL
								          ,relatedElements = PPfood$Food.relatedElements
								          ,dset   	       = NULL
							            )  # end foodUpdate
			  ,sequest    = list(actionMethod     = PPfood$FunctionsList$Sequest$actionMethod
							            ,actionFile       = PPfood$FunctionsList$Sequest$actionFile
								          ,tsType           = "AllPeriods"  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								          ,tsTiming         = "During"    # "Before","During","After"
								          ,transAction      = list(actionMethod = PPfood$FunctionsList$Sequest_setup$actionMethod,
														                      actionFile   = PPfood$FunctionsList$Sequest_setup$actionFile,
														                      dset         = NULL)
								          ,relatedElements  = NULL
								          ,dset   	        = PPfood$Sequest[[1]]
							            )
				,migrate    = list(actionMethod     = PPfood$FunctionsList$Migrate$actionMethod
								          ,actionFile       = PPfood$FunctionsList$Migrate$actionFile
								          ,tsType           = "AllPeriods" # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								          ,tsTiming         = "During"    # "Before","During","After"
								          ,transAction      = list(actionMethod = PPfood$FunctionsList$Migrate_setup$actionMethod
															                    ,actionFile   = PPfood$FunctionsList$Migrate_setup$actionFile
															                    ,dset         = NULL)
								          ,relatedElements  = NULL
								          ,dset   		      = PPfood$Migration.data[[1]]
							)
				,printState = list(actionMethod = PPfood$FunctionsList$StatePrint$actionMethod,
								           actionFile = PPfood$FunctionsList$StatePrint$actionFile,
								           tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								           tsTiming = "After",    # "Before","During","After"
								           relatedElements = NULL,
								           dset   = list(  # List because may need more than one file to print state
												            Biomass = list(output = PPfood$OutputFlags$PrintState_B
																                  ,fname  = PPfood$OutputFiles$State_B
																                  ,path   = NULL)
												                 ) # end dset list
							              ) # end printState list
				 ) # end actions list
		) # end Shell list


PPfood$Timesteps<-NULL
   for (tstep in 1:length(PPfood$TimestepIntervals)){
       PPfood$Timesteps<- c(PPfood$Timesteps,list(PPfood$TimestepShell))
       PPfood$Timesteps[[tstep]]$calday<-PPfood$TimestepIntervals[tstep]
       }

		
# declare variable to be sourced
PPfood
