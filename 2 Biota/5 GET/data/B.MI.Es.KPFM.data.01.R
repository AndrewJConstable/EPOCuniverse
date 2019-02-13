# Input data - B.MI.Es.KPFM.data.01.R
# Krill

# routine to enable processing of input data if needed but without function

Krill <- list()
Krill$signature 			<- list(
	ClassName       = "Krill",
	ID              = 22001,
	Name.full       = "Euphausia superba (Antarctic krill)",
	Name.short      = "Es-KPFM",
	Morph        	= "KPFM",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "7 July 2008"
)

Krill$polygonsN		 		<- 18
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
                           Season1 = matrix(c(  # rows = origin, cols = destination
                                  #SSMU.1,  SSMU.2,  SSMU.3,  SSMU.4,  SSMU.5,  SSMU.6,  SSMU.7,  SSMU.8,  SSMU.9, SSMU.10, SSMU.11, SSMU.12, SSMU.13, SSMU.14, SSMU.15,    BT.1,    BT.2,    BT.3
                                  0.00000, 0.03856, 0.00912, 0.00151, 0.00912, 0.01524, 0.01371, 0.00000, 0.31167, 0.00607, 0.00000, 0.00303, 0.38896, 0.01371, 0.01065, 0.00000, 0.01065, 0.08523 #  SSMU.1
                                 ,0.07696, 0.00000, 0.00000, 0.00000, 0.01869, 0.00000, 0.00000, 0.00000, 0.32542, 0.00000, 0.00000, 0.00000, 0.27541, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #  SSMU.2
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.18958, 0.00000, 0.00000, 0.00000, 0.00000, 0.24512 #  SSMU.3
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.04445, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.30228, 0.00000, 0.00000, 0.00000, 0.00000, 1.18958 #  SSMU.4
                                 ,0.00000, 0.03279, 0.03279, 0.00000, 0.00000, 0.03279, 0.00000, 0.00000, 0.03279, 0.00000, 0.00000, 0.00000, 0.94908, 0.00000, 0.00000, 0.00000, 0.00000, 0.21511 #  SSMU.5
                                 ,0.02353, 0.00000, 0.00000, 0.00000, 0.02353, 0.00000, 0.00000, 0.00000, 0.02353, 0.00000, 0.00000, 0.00000, 0.15028, 0.00000, 0.00000, 0.00000, 0.00000, 0.76547 #  SSMU.6
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.26028, 0.00000, 0.00000, 0.00000, 0.06454, 0.00000, 0.00000, 0.00000, 0.00000, 0.92676 #  SSMU.7
                                 ,0.08607, 0.00000, 0.00000, 0.00000, 0.00000, 0.05292, 0.04211, 0.00000, 0.18026, 0.03142, 0.00000, 0.00000, 0.01036, 0.00000, 0.00000, 0.00000, 0.00000, 0.04211 #  SSMU.8
                                 ,0.00459, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00183, 0.01106, 0.00551, 0.00183, 0.00000, 0.00000, 0.00000, 0.00000, 1.01091 #  SSMU.9
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 2.99573, 0.00000, 0.00000, 0.00000, 0.05129, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 # SSMU.10
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 2.77259, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 # SSMU.11
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.65823, 0.00000, 0.21131, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 # SSMU.12
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00971, 0.01558, 0.00000, 0.00097, 1.55766 # SSMU.13
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.04652, 0.00000, 0.22884, 0.00000, 0.00000, 1.29928 # SSMU.14
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.01504, 0.00000, 0.00000, 0.00000, 0.00000, 1.71979 # SSMU.15
                                 ,0.12867, 0.00498, 0.00284, 0.00320, 0.00071, 0.00249, 0.00749, 0.00000, 0.01503, 0.00000, 0.00000, 0.00000, 0.09336, 0.00000, 0.00000, 0.00000, 0.22617, 0.00000 #    BT.1
                                 ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00216, 0.00000, 0.00000, 0.00000, 0.25611, 0.02740, 0.02186, 0.00000, 0.00000, 0.00000 #    BT.2
                                 ,0.13630, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00819, 0.05554, 0.35043, 0.01128, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #    BT.3
                                      ), ncol=Krill$polygonsN,byrow=TRUE) # end season 1
                          ,Season2 = matrix(c(
                                 #SSMU.1,  SSMU.2,  SSMU.3,  SSMU.4,  SSMU.5,  SSMU.6,  SSMU.7,  SSMU.8,  SSMU.9, SSMU.10, SSMU.11, SSMU.12, SSMU.13, SSMU.14, SSMU.15,    BT.1,    BT.2,    BT.3
                                 0.00000, 0.03385, 0.00000, 0.00000, 0.01371, 0.01371, 0.06242, 0.00000, 0.23993, 0.00912, 0.00151, 0.00000, 0.40698, 0.02295, 0.01832, 0.00000, 0.02606, 0.00000 #  SSMU.1
                                ,0.09716, 0.00000, 0.00000, 0.00000, 0.03774, 0.00000, 0.00000, 0.00000, 0.16034, 0.00000, 0.00000, 0.00000, 0.40547, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #  SSMU.2
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.04445, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 2.03688, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #  SSMU.3
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.04445, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.30228, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #  SSMU.4
                                ,0.00000, 0.00000, 0.03279, 0.00000, 0.00000, 0.06669, 0.00000, 0.00000, 0.10178, 0.00000, 0.00000, 0.00000, 1.48808, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #  SSMU.5
                                ,0.00000, 0.00000, 0.00000, 0.02353, 0.02353, 0.00000, 0.00000, 0.00000, 0.07232, 0.00000, 0.00000, 0.00000, 0.32721, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #  SSMU.6
                                ,0.04256, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.23361, 0.00000, 0.00000, 0.00000, 0.04256, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #  SSMU.7
                                ,0.03142, 0.00000, 0.00000, 0.00000, 0.00000, 0.05292, 0.07490, 0.00000, 0.08607, 0.00000, 0.00000, 0.00000, 0.08607, 0.00000, 0.00000, 0.00000, 0.00000, 0.01036 #  SSMU.8
                                ,0.00736, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00551, 0.00736, 0.00921, 0.00183, 0.00000, 0.00551, 0.00000, 0.00000, 0.00000 #  SSMU.9
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.89712, 0.00000, 0.00000, 0.05129, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 # SSMU.10
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,11.51293, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 # SSMU.11
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.94591, 0.00000, 0.10008, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 # SSMU.12
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00387, 0.00776, 0.00000, 0.00000, 0.00000 # SSMU.13
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.07062, 0.00000, 0.25783, 0.00000, 0.00000, 0.00000 # SSMU.14
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.06156, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 # SSMU.15
                                ,0.12827, 0.00463, 0.00284, 0.00142, 0.00142, 0.00071, 0.00641, 0.00000, 0.01107, 0.00000, 0.00000, 0.00000, 0.10197, 0.00000, 0.00000, 0.00000, 0.22617, 0.00000 #    BT.1
                                ,0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00433, 0.00000, 0.00000, 0.00000, 0.26594, 0.02186, 0.01635, 0.00000, 0.00000, 0.00000 #    BT.2
                                ,0.13630, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.03001, 0.06529, 0.40089, 0.00511, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000 #    BT.3
                                      ), ncol=Krill$polygonsN,byrow=TRUE) # end season 2
)
Krill$PolygonDensityCV      	<- rep(1.0,Krill$polygonsN)
#  ,Reprod.relatedElements = matrix(c(1,12001),ncol=2,byrow=TRUE),
#  ,Recruit.relatedElements = matrix(c(1,12001),ncol=2,byrow=TRUE),
Krill$Reprod.relatedElements 	<- matrix(c("Environment", "KrillEnv"),ncol=2,byrow=TRUE)
Krill$Recruit.relatedElements 	<- matrix(c("Environment", "KrillEnv"),ncol=2,byrow=TRUE)
Krill$Reprod_Cond          		<- list(
                           P      = c( # vector - Biomass Standard for productivity in each polygon
                                            4.7*10^12 # APPA
                                          , 1.3*10^12 # APW
                                          , 0.5*10^12 # APDPW
                                          , 0.5*10^12 # APDPE
                                          , 0.7*10^12 # APBSW
                                          , 1.0*10^12 # APBSE
                                          , 1.3*10^12 # APEI
                                          , 2.2*10^12 # APE
                                          ,19.8*10^12 # SOPA
                                          , 2.3*10^12 # SOW
                                          , 1.5*10^12 # SONE
                                          , 2.2*10^12 # SOSE
                                          ,22.5*10^12 # SGPA
                                          , 1.6*10^12 # SGW
                                          , 2.1*10^12 # SGE
                                          , 0.0       # Bathtub 1
                                          , 0.0       # Bathtub 2
                                          , 0.0       # Bathtub 3
                                      ) # end P
                          ,alpha  = c(rep(7.9,(Krill$polygonsN-3)),0,0,0)
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
                            , 37.7 # APW
                            , 37.7 # APDPW
                            , 37.7 # APDPE
                            , 37.7 # APBSW
                            , 37.7 # APBSE
                            , 37.7 # APEI
                            , 37.7 # APE
                            , 24.5 # SOPA
                            ,150.4 # SOW
                            ,150.4 # SONE
                            ,150.4 # SOSE
                            , 24.5 # SGPA
                            , 39.3 # SGW
                            , 39.3 # SGE
                            , 0   # Bathtub 1
                            , 0   # Bathtub 2
                            , 0   # Bathtub 3
)
Krill$Init.density.multiplier <- 1E1
Krill$Stage                 <- list(StageN = 1
                               ,StageStrUnits  = 1 # (1 = N, 2 = B)
                               ,StageStr  = rep(list(matrix(c(1,1),ncol=2,byrow=TRUE)),Krill$polygonsN)
                               ,StageSize = rep(list(c(0.46)),Krill$polygonsN)
)
Krill$Initialise            <- list(NULL)
Krill$Transition.data       <- list(Availability = rep(1.0,Krill$polygonsN) # availability in each polygon
                               ,CompetitorElements = matrix(c("Biota", "Seals" # seals
                                                              ,"Biota", "PenguinsA3" # penguin rec age 3
                                                              ,"Biota", "PenguinsA4" # penguin rec age 4
                                                              ,"Biota", "PenguinsA5" # penguin rec age 5
                                                              ,"Biota", "FishA2F1" # fish rec age 2 feed 1
                                                              ,"Biota", "FishA2F2" # fish rec age 2 feed 2
                                                              ,"Biota", "FishA2F3" # fish rec age 2 feed 3
                                                              ,"Biota", "FishA3F1" # fish rec age 3 feed 1
                                                              ,"Biota", "Whales" # whales
                                                              ,"Activity", "KrillFishery" # fishery
                                                               ),ncol=2,byrow=TRUE) # enter as matrix - cols module, element
                               ,CompetitionCoefficients = matrix(c(rep(1,Krill$polygonsN) # seals
                                                                   ,rep(1,Krill$polygonsN) # penguin rec age 3
                                                                   ,rep(1,Krill$polygonsN) # penguin rec age 4
                                                                   ,rep(1,Krill$polygonsN) # penguin rec age 5
                                                                   ,rep(1,Krill$polygonsN) # fish rec age 2 feed 1
                                                                   ,rep(1,Krill$polygonsN) # fish rec age 2 feed 2
                                                                   ,rep(1,Krill$polygonsN) # fish rec age 2 feed 3
                                                                   ,rep(1,Krill$polygonsN) # fish rec age 3 feed 1
                                                                   ,rep(1,Krill$polygonsN) # whales
                                                                   ,rep(1,Krill$polygonsN) # fishery
                                                                   ),ncol=(Krill$polygonsN),byrow=TRUE) # competition coefficients in each polygon
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
