# Input data - B.Pr.KPFM.01.data.Whale.01.R
# KPFM predator

# notes on development of general function - mark completed as done
#    Allocate_breeders       (completed)  B.Pr.KPFM.Allocate_breeders.01.R
#    Consume                 (completed)  B.Pr.KPFM.Consume.01.R
#    UpdateReprodHealth      (completed)  B.Pr.KPFM.UpdateReprodHealth.01.R
#    Update_age              (completed)  B.Pr.KPFM.Update_age.01.R
#    Reproduce               (completed)  B.Pr.KPFM.Reproduce.01.R
#    Mortality               (completed)  B.Pr.KPFM.Mortality.01.R
#    BreedersToNonbreeders   (completed)  B.Pr.KPFM.BreedersToNonbreeders.01.R
#    Scenario_setup             (completed)  B.Pr.KPFM.Time0.fn.01.R
#    StatePrint              (completed)  B.Pr.KPFM.printState.01.R
#    TransitionSetup         (completed)  B.Pr.KPFM.TransitionSetup.01.R
#    StateUpdate             (completed)  B.Pr.KPFM.update_State.01.R

# Watters etal data
#  SSMUv     Abund   Proportion
#                    in area       M	       Mswitch Mprop  Ralpha  RSpeak  RRpeak  Rphi  Qq  QQmax      Qk5
#  1 APPA    9233    0.313    0.034996  0       0      0.11    10000   918     0.37  0   119440272  15
#  2 APW     767     0.026
#  3 APDPW   330     0.011
#  4 APDPE   341     0.011
#  5 APBSW   460     0.015
#  6 APBSE   600     0.020
#  7 APEI    773     0.026
#  8 APE     1284    0.043
#  9 SOPA    6808    0.230
# 10 SOW     131     0.004
# 11 SONE    86      0.002
# 12 SOSE    126     0.004
# 13 SGPA    7737    0.262
# 14 SGW     354     0.012
# 15 SGE     452     0.015
# Total      29482

# M	       Mswitch Mprop  Ralpha  RSpeak  RRpeak  Rphi  Qq  QQmax      Qk5
# 0.034996 0       0      0.11    10000   918     0.37  0   119440272  15 #

# feeding proportions c(0.319,0.026,0.011,0.011,0.015,0.020,0.026,0.043,0.230,0.004,0.002,0.004,0.262,0.012,0.015)

################################################################################
# initial parameters to help with setting up the data set

#RootPath<-"M:\\EPOC\\EPOC\\"

Npolygons <- 1  # number of local populations

################################################################################
# start data set

Whale 						<- list()

#-------------------------------------------------------------------------------
Whale$signature				<- list(
	ClassName		= "Predator",
	ID             	= 23009,
	Name.full       = "KPFM whale Rec Age 5",
	Name.short      = "Whale",
	Morph        	= "KPFM",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "7 July 2008"
)
	
Whale$polygonsN				<- 1
Whale$Polygons              <- c(1)
                           # reference numbers to polygons in the list of
                           # defined polygons

Whale$Birthdate             <- list(Day = 1, Month = 4)
                           # day and month to be used as time 0 in the year
                           # for the taxon

#-------------------------------------------------------------------------------
Whale$ScaleToTonnes         <- 1
  
#-------------------------------------------------------------------------------
Whale$Init.abundance        <- c(
                             29482   #  1 = whole area
)

#-------------------------------------------------------------------------------
Whale$Stage                 <- list(StageN = 7 # "pups", number of age classes in juveniles + nonbreeders (5) and breeders (6)
                               ,JuveAgeN  = 5 # equivalent to lag in KPFM
                               ,StageStrUnits  = 1 # (1 = N, 2 = B)
                               ,StageStr  = NULL # established as a list by polygon in setup
                               ,StageSize = rep(list(c(0.1 # Age 0
                                                      ,0.5 # Age 1
                                                      ,1.0 # Age 2
                                                      ,1.0 # Age 3
                                                      ,1.0 # Age 4
                                                      ,1.0 # nonbreeders
                                                      ,1.0 # breeders
                                                      )),Whale$polygonsN)
)

#-------------------------------------------------------------------------------
Whale$Mortality          	<- list(summer = list(
                                   # M = nominal mortality over period
                                   # z = max proportion of nominal mortality that is subject to variation
                                   # v= effect of density dependence on dependent variable

                                 Age0      = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age1      = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age2     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age3     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age4     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,nonBreeders  = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,breeders     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                             ) # end summer
                               ,winter = list(
                                 Age0      = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age1      = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age2     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age3     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,Age4     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,nonBreeders  = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                ,breeders     = list (M = c(0.035)
                                                     ,z = rep(0,Whale$polygonsN)
                                                     ,v = rep(0,Whale$polygonsN))
                                             ) # end winter
)

#-------------------------------------------------------------------------------
Whale$Allocate.breeders     <- list(
                              StageNonbreeder = 6 # designated stage of nonbreeder - should always be one less than breeder
                             ,StageBreeder    = 7 #  designated stage of breeder
                             ,Phi             = rep(1,Whale$polygonsN)
                             ,maxPropBreeders = rep(1,Whale$polygonsN) # max proportion of non-breeders that can become breeders
                             ,SSMUdest = matrix(c( # proportion of breeders from origin breeding SSMU (rows) going to destination SSMU (cols)
                                  1
                                  ),ncol=Whale$polygonsN,byrow=TRUE)
                             ,RepConditionRemaining = 0 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Whale$Reproduction     		<- list(
                              StageBreeder    = 7 #  designated stage of breeder
                          #   offspring mortality parameters - vector for polygons
                             ,M = c(0.034996) # nominal mortality of offspring over breeding period
                             ,z = rep(0,Whale$polygonsN) # max proportion of nominal mortality that is subject to variation
                             ,v = rep(1.5,Whale$polygonsN) # effect of density dependence on dependent variable
                                  #calculation of alpha from Watters etal Ralpha
                                  #            print( Ralpha vector *exp(c(M vector))*AgeRec)
                             ,alpha = c(0.1560912) # maximum reproductive rate per female
                             ,propfemale = rep(1,Whale$polygonsN) # proportion of breeding population that is female
                             ,RepConditionRemaining = 1 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Whale$Consume  				<- list(
         relatedElements = matrix(c("Biota", "Krill"),ncol=2,byrow=TRUE) # krill
        ,feeding.SSMUs    = c(1:18)  # reference numbers for polygons in polygon
                                     # list in which feeding can occur by local populations
                                     # this list is used to set up the proportions
                                     # of prey polygons in the feeding polygons below
        ,feeding.SSMU.N   = 18
       # reference to related elements below is by relative row number
        ,dset = list(
        
              #-----------------------------------
              summer  = list( # by predator stage - if NULL then no consumption by that stage
                      Age0 = NULL
                     ,Age1 = NULL
                     ,Age2 = NULL
                     ,Age3 = NULL
                     ,Age4 = NULL

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   = 18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16 17 18
                           0.319,0.026,0.011,0.011,0.015,0.020,0.026,0.043,0.230,0.004,0.002,0.004,0.262,0.012,0.015, 0, 0, 0 # all areas
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 119440272  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 20
                                   ,Holling_units        = 1
                                   ,Holling_availability = c(1) # krill stage structure (like selectivity) used to calculate prey density for Holling equation (i.e. what predators can see)
                                   ,Prey_selectivity     = c(1)
                                   ,PreyPn         = list( # for each predator feeding polygon, list prey polygons (relative reference) and proportion of prey polygon in predator polygon
                                            P1 = list(Pns = matrix(c(1,1),ncol=2,byrow=TRUE) # polygon number, proportion in pred polygon
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
                                           ,P16 = list(Pns = matrix(c(16,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                           ,P17 = list(Pns = matrix(c(17,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                           ,P18 = list(Pns = matrix(c(18,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                                   ) # end PreyPn list
                                   ) # end krill list
                               ) # end Prey list
                           ) # end NonBreeder list

                  #-------------------------
                     ,Breeder = list(
                          feeding.SSMU.N   = 18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16 17 18
                           0.319,0.026,0.011,0.011,0.015,0.020,0.026,0.043,0.230,0.004,0.002,0.004,0.262,0.012,0.015, 0, 0, 0 # all areas
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 119440272  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 20
                                   ,Holling_units        = 1
                                   ,Holling_availability = c(1) # krill stage structure (like selectivity) used to calculate prey density for Holling equation
                                   ,Prey_selectivity     = c(1)
                                   ,PreyPn         = list( # for each predator feeding polygon, list prey polygons (relative reference) and proportion of prey polygon in predator polygon
                                            P1 = list(Pns = matrix(c(1,1),ncol=2,byrow=TRUE) # polygon number, proportion in pred polygon
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
                                           ,P16 = list(Pns = matrix(c(16,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                           ,P17 = list(Pns = matrix(c(17,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                           ,P18 = list(Pns = matrix(c(18,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                                   ) # end PreyPn list
                                   ) # end krill list
                               ) # end Prey list
                            ) # end Breeder list
                            ) # end summer list


              #-----------------------------------
             ,winter = list( # by predator stage - if NULL then no consumption by that stage
                      Age0 = NULL
                     ,Age1 = NULL
                     ,Age2 = NULL
                     ,Age3 = NULL
                     ,Age4 = NULL

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   = 18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16 17 18
                           0.319,0.026,0.011,0.011,0.015,0.020,0.026,0.043,0.230,0.004,0.002,0.004,0.262,0.012,0.015, 0, 0, 0 # all areas
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 1  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 20
                                   ,Holling_units        = 1
                                   ,Holling_availability = c(1) # krill stage structure (like selectivity) used to calculate prey density for Holling equation
                                   ,Prey_selectivity     = c(1)
                                   ,PreyPn         = list( # for each predator feeding polygon, list prey polygons (relative reference) and proportion of prey polygon in predator polygon
                                            P1 = list(Pns = matrix(c(1,1),ncol=2,byrow=TRUE) # polygon number, proportion in pred polygon
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
                                           ,P16 = list(Pns = matrix(c(16,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                           ,P17 = list(Pns = matrix(c(17,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                           ,P18 = list(Pns = matrix(c(18,1),ncol=2,byrow=TRUE)
                                                     ,PnN = 1)
                                                   ) # end PreyPn list
                                   ) # end krill list
                               ) # end Prey list
                           ) # end NonBreeder list
                     ,Breeder = NULL

                  #-------------------------
                     ) # end winter list
                    ) # end dset list
)

#-------------------------------------------------------------------------------
Whale$ReprodHealth          <- list(summer = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
                               ,winter = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
)

#-------------------------------------------------------------------------------
Whale$Update.age 			<- NULL
#-------------------------------------------------------------------------------
Whale$Breeders.to.nonbreeders <- list(
                              StageNonbreeder = 6 # designated stage of nonbreeder
                             ,StageBreeder    = 7 #  designated stage of breeder
                             ,Breeders        = matrix(0,nrow=Whale$polygonsN,ncol=Whale$polygonsN)
)

#-------------------------------------------------------------------------------
Whale$Initialise            <- list(NULL)
Whale$Transition.data       <- list()
Whale$PrintState            <- list(OutDir   = NULL, OutFiles = NULL)
Whale$FunctionsList         <- list(Allocate_breeders = list(actionMethod = "allocateBreeders", 
														    actionFile = file.path("code", "B.Pr.KPFM.Allocate_breeders.01.R")),
								Consume               = list(actionMethod = "consume", 
														    actionFile = file.path("code", "B.Pr.KPFM.Consume.01.R")),
								Consume_setup         = list(actionMethod = "consumeSetup", 
														     actionFile = file.path("code", "B.Pr.KPFM.Consume.setup.01.R")),
								UpdateReprodHealth    = list(actionMethod = "updateReprodHealth", 
														     actionFile = file.path("code", "B.Pr.KPFM.UpdateReprodHealth.01.R")),
								Update_age            = list(actionMethod = "updateAge", 
														     actionFile = file.path("code", "B.Pr.KPFM.Update_age.01.R")),
								Reproduce             = list(actionMethod = "reproduce", 
														     actionFile = file.path("code", "B.Pr.KPFM.Reproduce.01.R")),
								Reproduce_setup       = list(actionMethod = "reproduceSetup", 
														     actionFile = file.path("code", "B.Pr.KPFM.Reproduce.setup.01.R")),
								Mortality             = list(actionMethod = "mortality", 
														     actionFile = file.path("code", "B.Pr.KPFM.Mortality.01.R")),
								Mortality_setup       = list(actionMethod = "mortalitySetup", 
														     actionFile = file.path("code", "B.Pr.KPFM.Mortality.setup.01.R")),
								BreedersToNonbreeders = list(actionMethod = "breedersToNonbreeders", 
														     actionFile = file.path("code", "B.Pr.KPFM.BreedersToNonbreeders.01.R")),
								StatePrint            = list(actionMethod = "printState", 
														     actionFile = file.path("code", "B.Pr.KPFM.printState.01.R")),
								StateUpdate           = list(actionMethod = "updateState",
														     actionFile = file.path("code", "B.Pr.KPFM.update_State.01.R"))
)
#-------------------------------------------------------------------------------
Whale$OutputFiles         	<- list(State_N       = "Biota.Whale.State.N.dat"
								 ,State_B       = "Biota.Whale.State.B.dat"
								 ,State_Stage   = "Biota.Whale.State.Stage.dat"
								 ,State_RepCond = "Biota.Whale.State.RepCond.dat"
								 ,State_Health  = "Biota.Whale.State.Health.dat"
)
#-------------------------------------------------------------------------------
Whale$OutputFlags         	<- list(PrintState_N       = TRUE
								 ,PrintState_B       = TRUE
								 ,PrintState_Stage   = TRUE
								 ,PrintState_RepCond = FALSE
								 ,PrintState_Health  = FALSE
)

Whale$Functions 			<- list(
			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup = NULL,
			# data and function to initialise element at the beginning of each scenario
			#   i.e. how should the element be reset at time 0
			printState    = list(actionMethod = Whale$FunctionsList$StatePrint$actionMethod,
								actionFile   = Whale$FunctionsList$StatePrint$actionFile,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Whale$OutputFlags$PrintState_N,
												  fname  = Whale$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Whale$OutputFlags$PrintState_B,
												   fname  = Whale$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Whale$OutputFlags$PrintState_Stage,
												  fname  = Whale$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Whale$OutputFlags$PrintState_RepCond,
													   fname  = Whale$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Whale$OutputFlags$PrintState_Health,
												  fname  = Whale$OutputFiles$State_Health,
												  path   = NULL)
								)
			),
      
			stateUpdate  = list(actionMethod = Whale$FunctionsList$StateUpdate$actionMethod,
								actionFile = Whale$FunctionsList$StateUpdate$actionFile,
								dset   = NULL
			)
)
		
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

Whale$Timesteps 			<- list(
	Summer = list(calday=dayFromDate(31,3),
		actionsN=NULL,  # will be updated below
		actions=list(
			allocate_breeders = list(actionMethod = Whale$FunctionsList$Allocate_breeders$actionMethod,
									actionFile = Whale$FunctionsList$Allocate_breeders$actionFile,
									tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "Before",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Whale$Allocate.breeders
			),

			consume     	  = list(actionMethod = Whale$FunctionsList$Consume$actionMethod,
									actionFile = Whale$FunctionsList$Consume$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Whale$FunctionsList$Consume_setup$actionMethod,
															actionFile = Whale$FunctionsList$Consume_setup$actionFile,
															dset = NULL),
									relatedElements = Whale$Consume$relatedElements,
									dset   = Whale$Consume$dset[[1]]
			),

			mortality   	  = list(actionMethod = Whale$FunctionsList$Mortality$actionMethod,
									actionFile = Whale$FunctionsList$Mortality$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Whale$FunctionsList$Mortality_setup$actionMethod,
															actionFile = Whale$FunctionsList$Mortality_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Whale$Mortality[[1]]
			),

			update_rep_health = list(actionMethod = Whale$FunctionsList$UpdateReprodHealth$actionMethod,
									actionFile = Whale$FunctionsList$UpdateReprodHealth$actionFile,
									tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    	# "Before","During","After"
									transAction = NULL,
									relatedElements = Whale$Consume$relatedElements,
									dset   = Whale$ReprodHealth[[1]]
			),

			update_age 		  = list(actionMethod = Whale$FunctionsList$Update_age$actionMethod,
									actionFile = Whale$FunctionsList$Update_age$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Whale$Update.age
			),

			reproduce   	  = list(actionMethod = Whale$FunctionsList$Reproduce$actionMethod,
									actionFile = Whale$FunctionsList$Reproduce$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = list(actionMethod = Whale$FunctionsList$Reproduce_setup$actionMethod,
															actionFile = Whale$FunctionsList$Reproduce_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Whale$Reproduction
			),

			breedersToNonbreeders = list(actionMethod = Whale$FunctionsList$BreedersToNonbreeders$actionMethod,
										actionFile = Whale$FunctionsList$BreedersToNonbreeders$actionFile,
										tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    # "Before","During","After"
										transAction = NULL, # list(fn = , dset = )
										relatedElements = NULL,
										dset   = Whale$Breeders.to.nonbreeders
			),
			printState 		  = list(actionMethod = Whale$FunctionsList$StatePrint$actionMethod,
									actionFile = Whale$FunctionsList$StatePrint$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									relatedElements = NULL,
									dset = list(  # List because may need more than one file to print state
											Number = list(output = Whale$OutputFlags$PrintState_N,
														  fname  = Whale$OutputFiles$State_N,
														  path   = NULL),
											Biomass = list(output = Whale$OutputFlags$PrintState_B,
														   fname  = Whale$OutputFiles$State_B,
														   path   = NULL),
											Stage  = list(output = Whale$OutputFlags$PrintState_Stage,
														  fname  = Whale$OutputFiles$State_Stage,
														  path   = NULL),
											Reprod_Cond = list(output = Whale$OutputFlags$PrintState_RepCond,
															   fname  = Whale$OutputFiles$State_RepCond,
															   path   = NULL),
											Health = list(output = Whale$OutputFlags$PrintState_Health,
														  fname  = Whale$OutputFiles$State_Health,
														  path   = NULL)
									)
			)
		) 
	),
	Winter = list(calday=dayFromDate(30,9),
		actionsN=NULL, # will be updated below
		actions=list(
			consume      = list(actionMethod = Whale$FunctionsList$Consume$actionMethod,
								actionFile = Whale$FunctionsList$Consume$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = NULL,
								relatedElements = Whale$Consume$relatedElements,
								dset   = Whale$Consume$dset[[2]]
			),

			mortality    = list(actionMethod = Whale$FunctionsList$Mortality$actionMethod,
								actionFile = Whale$FunctionsList$Mortality$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = list(actionMethod = Whale$FunctionsList$Mortality_setup$actionMethod,
														actionFile = Whale$FunctionsList$Mortality_setup$actionFile,
														dset = NULL),
								relatedElements = NULL,
								dset   = Whale$Mortality[[2]]
			),

			update_rep_health_cond = list(actionMethod = Whale$FunctionsList$UpdateReprodHealth$actionMethod,
										actionFile = Whale$FunctionsList$UpdateReprodHealth$actionFile,
										tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    	# "Before","During","After"
										transAction = NULL,
										relatedElements = Whale$Consume$relatedElements,
										dset   = Whale$ReprodHealth[[2]]
			),

			printState   = list(actionMethod = Whale$FunctionsList$StatePrint$actionMethod,
								actionFile = Whale$FunctionsList$StatePrint$actionFile,
								tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "After",    # "Before","During","After"
								relatedElements = NULL,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Whale$OutputFlags$PrintState_N,
												  fname  = Whale$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Whale$OutputFlags$PrintState_B,
												   fname  = Whale$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Whale$OutputFlags$PrintState_Stage,
												  fname  = Whale$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Whale$OutputFlags$PrintState_RepCond,
													   fname  = Whale$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Whale$OutputFlags$PrintState_Health,
												  fname  = Whale$OutputFiles$State_Health,
												  path   = NULL)
								)
			)
		)
	)
)

# declare variable to be sourced
Whale