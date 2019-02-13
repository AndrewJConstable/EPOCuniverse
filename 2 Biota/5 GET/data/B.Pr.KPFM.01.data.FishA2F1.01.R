# Input data - B.Pr.KPFM.01.data.FishA2F1.01.R
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

#  SSMU     RecAge  PengID     InitAbund   M          Mswitch Mprop  Ralpha Rphi    Hq      PCsummer H50  PCwinter
#  1  APPA   2      N          8402727132  0.369327   0       0      10     0.37    0       1950     5    272
#  2  APW    3      N          834429245   0.24829    0       0      10     0.37    0       3386     5    674
#  3  APDPW  3      N          301932508   0.293375   0       0      10     0.37    0       2881     5    493
#  4  APDPE  3      N          322045302   0.282369   0       0      10     0.37    0       3008     5    533
#  5  APBSW  3      N          438862551   0.278247   0       0      10     0.37    0       3054     5    548
#  6  APBSE  3      N          574691426   0.277919   0       0      10     0.37    0       3058     5    550
#  7  APEI   2      N          679683642   0.353186   0       0      10     0.37    0       2157     5    313
#  8  APE    3      N          1553175192  0.233962   0       0      10     0.37    0       3540     5    743
#  9  SOPA   2      N          69055728795 0.484497   0       0      10     0.37    0       304      5    52
# 10  SOW    2      N          320095594   0.380949   0       0      10     0.37    0       1797     5    243
# 11  SONE   2      N          196909670   0.345772   0       0      10     0.37    0       2250     5    333
# 12  SOSE   3      N          389311850   0.236438   0       0      10     0.37    0       3514     5    730
# 13  SGPA   2      N          2.48777E+11 0.496202   0       0      10     0.37    0       93       5    35
# 14  SGW    2      N          1140779048  0.335801   0       0      10     0.37    0       1433     5    383
# 15  SGE    2      N          1477796628  0.343818   0       0      10     0.37    0       1372     5    359

# sorted by RecAge & feeding per capita rate

#  SSMU     RecAge  PengID     InitAbund   M          Mswitch Mprop  Ralpha Rphi    Hq      PCsummer H50  PCwinter

# A2F1
#  9  SOPA   2      32005      69055728795 0.484497   0       0      10     0.37    0       304      5    52
# 13  SGPA   2      32005      2.48777E+11 0.496202   0       0      10     0.37    0       93       5    35

# A2F2
# 14  SGW    2      32006      1140779048  0.335801   0       0      10     0.37    0       1433     5    383
# 15  SGE    2      32006      1477796628  0.343818   0       0      10     0.37    0       1372     5    359

# A2F3
#  1  APPA   2      32007      8402727132  0.369327   0       0      10     0.37    0       1950     5    272
#  7  APEI   2      32007      679683642   0.353186   0       0      10     0.37    0       2157     5    313
# 10  SOW    2      32007      320095594   0.380949   0       0      10     0.37    0       1797     5    243
# 11  SONE   2      32007      196909670   0.345772   0       0      10     0.37    0       2250     5    333


# routine to enable processing of input data if needed but without function

################################################################################
# start data set

Fish 					<- list()

Fish$signature			<- list(
	ClassName			= "Predator",
	ID           		= 23005,
	Name.full           = "KPFM fish RecAge 2 Feed 1 - approx 200",
	Name.short          = "FishA2F1",
	Morph        		= "KPFM",
	Revision      		= "01",
	Authors      		= "A.Constable",
	Last.edit    		= "7 July 2008"
)

Fish$polygonsN 		<- 2  # number of local populations
Fish$polygons      	<- c(9,13)
							# reference numbers to polygons in the list of
							# defined polygons

Fish$birthdate     	<- list(Day = 1, Month = 4)
							# day and month to be used as time 0 in the year
							# for the taxon

#-------------------------------------------------------------------------------
Fish$ScaleToTonnes  	<- 0.005
  
#-------------------------------------------------------------------------------
Fish$Init.abundance    <- c(
								69055728795  #  9  SOPA
								,2.48777E+11 # 13  SGPA
)

#-------------------------------------------------------------------------------
Fish$Stage             <- list(StageN = 4 # "pups", number of age classes in juveniles + nonbreeders (5) and breeders (6)
								,JuveAgeN  = 2 # equivalent to lag in KPFM
								,StageStrUnits  = 1 # (1 = N, 2 = B)
								,StageStr  = NULL # established as a list by polygon in setup
								,StageSize = rep(list(c(0.0001 # Age 0
                                                      ,0.0002 # Age 1
                                                      ,0.0005 # nonbreeders
                                                      ,0.0005 # breeders
                                                      )),Fish$polygonsN)
)

#-------------------------------------------------------------------------------
Fish$Mortality         <- list(summer = list(
									   # M = nominal mortality over period
									   # z = max proportion of nominal mortality that is subject to variation
									   # v= effect of density dependence on dependent variable

										Age0      = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
										,Age1      = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
										,nonBreeders  = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
										,breeders     = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
									),
									winter = list(
										Age0      = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
										,Age1      = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
										,nonBreeders  = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
										,breeders     = list (M = c(0.484497,0.496202)
															 ,z = rep(0,Fish$polygonsN)
															 ,v = rep(0,Fish$polygonsN))
                                    )
)

#-------------------------------------------------------------------------------
Fish$Allocate.breeders <- list(
									StageNonbreeder = 3 # designated stage of nonbreeder - should always be one less than breeder
									,StageBreeder    = 4 #  designated stage of breeder
									,Phi             = rep(3.5,Fish$polygonsN)
									,maxPropBreeders = rep(1,Fish$polygonsN) # max proportion of non-breeders that can become breeders
									,SSMUdest = matrix(c( # proportion of breeders from origin breeding SSMU (rows) going to destination SSMU (cols)
									  1,0
									 ,0,1
									  ),ncol=Fish$polygonsN,byrow=TRUE)
									,RepConditionRemaining = 0 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Fish$Reproduction     	<- list(
								StageBreeder    = 4 #  designated stage of breeder
								#   offspring mortality parameters - vector for polygons
								,M = c(0.484497,0.496202) # nominal mortality of offspring over breeding period
								,z = rep(0,Fish$polygonsN) # max proportion of nominal mortality that is subject to variation
								,v = rep(1.5,Fish$polygonsN) # effect of density dependence on dependent variable
                                  #calculation of alpha from Watters etal Ralpha
                                  #            print( Ralpha vector *exp(c(M vector))*AgeRec)
								,alpha = (0.15*c(69.44764, 72.77650)) # maximum reproductive rate per female
								,propfemale = rep(1,Fish$polygonsN) # proportion of breeding population that is female
								,RepConditionRemaining = 1 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Fish$Consume 			<- list(
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

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   = 18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
                           0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony  9  SOPA
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 # colony 13  SGPA
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 200  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 15
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
                         # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
                           0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony  9  SOPA
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 # colony 13  SGPA
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 200  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 15
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

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   = 18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                        #1     2     3     4     5     6     7     8     9     10    11 12    13    14    15    16   17 18
                           0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony  9  SOPA
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 # colony 13  SGPA
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 40  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 15
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
            )
        )
)

#-------------------------------------------------------------------------------
Fish$ReprodHealth      <- list(summer = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
                               ,winter = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
)

#-------------------------------------------------------------------------------
Fish$Update.age 		<- NULL
#-------------------------------------------------------------------------------
Fish$Breeders.to.nonbreeders <- list(
                              StageNonbreeder = 3 # designated stage of nonbreeder
                             ,StageBreeder    = 4 #  designated stage of breeder
                             ,Breeders        = matrix(0,nrow=Fish$polygonsN,ncol=Fish$polygonsN)
)

#-------------------------------------------------------------------------------
Fish$Initialise        <- list(NULL)
Fish$Transition.data   <- list()
Fish$PrintState        <- list(OutDir   = NULL, OutFiles = NULL)
Fish$FunctionsList     <- list(Allocate_breeders     = list(actionMethod = "allocateBreeders", 
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
Fish$OutputFiles       <- list(State_N     = "Biota.FishA2F1.State.N.dat"
									 ,State_B       = "Biota.FishA2F1.State.B.dat"
									 ,State_Stage   = "Biota.FishA2F1.State.Stage.dat"
									 ,State_RepCond = "Biota.FishA2F1.State.RepCond.dat"
									 ,State_Health  = "Biota.FishA2F1.State.Health.dat"
)
#-------------------------------------------------------------------------------
Fish$OutputFlags       <- list(PrintState_N       = TRUE
                             ,PrintState_B       = TRUE
                             ,PrintState_Stage   = TRUE
                             ,PrintState_RepCond = FALSE
                             ,PrintState_Health  = FALSE
)

Fish$Functions 		<- list(
			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup = NULL,
			# data and function to initialise element at the beginning of each scenario
			#   i.e. how should the element be reset at time 0
			printState    = list(actionMethod = Fish$FunctionsList$StatePrint$actionMethod,
								actionFile   = Fish$FunctionsList$StatePrint$actionFile,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Fish$OutputFlags$PrintState_N,
												  fname  = Fish$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Fish$OutputFlags$PrintState_B,
												   fname  = Fish$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Fish$OutputFlags$PrintState_Stage,
												  fname  = Fish$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Fish$OutputFlags$PrintState_RepCond,
													   fname  = Fish$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Fish$OutputFlags$PrintState_Health,
												  fname  = Fish$OutputFiles$State_Health,
												  path   = NULL)
								)
			),
      
			stateUpdate  = list(actionMethod = Fish$FunctionsList$StateUpdate$actionMethod,
								actionFile = Fish$FunctionsList$StateUpdate$actionFile,
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

Fish$Timesteps 		<- list(
	Summer = list(calday=dayFromDate(31,3),
		actionsN=NULL,  # will be updated below
		actions=list(
			allocate_breeders = list(actionMethod = Fish$FunctionsList$Allocate_breeders$actionMethod,
									actionFile = Fish$FunctionsList$Allocate_breeders$actionFile,
									tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "Before",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Fish$Allocate.breeders
			),

			consume     	  = list(actionMethod = Fish$FunctionsList$Consume$actionMethod,
									actionFile = Fish$FunctionsList$Consume$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Fish$FunctionsList$Consume_setup$actionMethod,
															actionFile = Fish$FunctionsList$Consume_setup$actionFile,
															dset = NULL),
									relatedElements = Fish$Consume$relatedElements,
									dset   = Fish$Consume$dset[[1]]
			),

			mortality   	  = list(actionMethod = Fish$FunctionsList$Mortality$actionMethod,
									actionFile = Fish$FunctionsList$Mortality$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Fish$FunctionsList$Mortality_setup$actionMethod,
															actionFile = Fish$FunctionsList$Mortality_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Fish$Mortality[[1]]
			),

			update_rep_health = list(actionMethod = Fish$FunctionsList$UpdateReprodHealth$actionMethod,
									actionFile = Fish$FunctionsList$UpdateReprodHealth$actionFile,
									tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    	# "Before","During","After"
									transAction = NULL,
									relatedElements = Fish$Consume$relatedElements,
									dset   = Fish$ReprodHealth[[1]]
			),

			update_age 		  = list(actionMethod = Fish$FunctionsList$Update_age$actionMethod,
									actionFile = Fish$FunctionsList$Update_age$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Fish$Update.age
			),

			reproduce   	  = list(actionMethod = Fish$FunctionsList$Reproduce$actionMethod,
									actionFile = Fish$FunctionsList$Reproduce$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = list(actionMethod = Fish$FunctionsList$Reproduce_setup$actionMethod,
															actionFile = Fish$FunctionsList$Reproduce_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Fish$Reproduction
			),

			breedersToNonbreeders = list(actionMethod = Fish$FunctionsList$BreedersToNonbreeders$actionMethod,
										actionFile = Fish$FunctionsList$BreedersToNonbreeders$actionFile,
										tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    # "Before","During","After"
										transAction = NULL, # list(fn = , dset = )
										relatedElements = NULL,
										dset   = Fish$Breeders.to.nonbreeders
			),
			printState 		  = list(actionMethod = Fish$FunctionsList$StatePrint$actionMethod,
									actionFile = Fish$FunctionsList$StatePrint$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									relatedElements = NULL,
									dset = list(  # List because may need more than one file to print state
											Number = list(output = Fish$OutputFlags$PrintState_N,
														  fname  = Fish$OutputFiles$State_N,
														  path   = NULL),
											Biomass = list(output = Fish$OutputFlags$PrintState_B,
														   fname  = Fish$OutputFiles$State_B,
														   path   = NULL),
											Stage  = list(output = Fish$OutputFlags$PrintState_Stage,
														  fname  = Fish$OutputFiles$State_Stage,
														  path   = NULL),
											Reprod_Cond = list(output = Fish$OutputFlags$PrintState_RepCond,
															   fname  = Fish$OutputFiles$State_RepCond,
															   path   = NULL),
											Health = list(output = Fish$OutputFlags$PrintState_Health,
														  fname  = Fish$OutputFiles$State_Health,
														  path   = NULL)
									)
			)
		) 
	),
	Winter = list(calday=dayFromDate(30,9),
		actionsN=NULL, # will be updated below
		actions=list(
			consume      = list(actionMethod = Fish$FunctionsList$Consume$actionMethod,
								actionFile = Fish$FunctionsList$Consume$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = NULL,
								relatedElements = Fish$Consume$relatedElements,
								dset   = Fish$Consume$dset[[2]]
			),

			mortality    = list(actionMethod = Fish$FunctionsList$Mortality$actionMethod,
								actionFile = Fish$FunctionsList$Mortality$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = list(actionMethod = Fish$FunctionsList$Mortality_setup$actionMethod,
														actionFile = Fish$FunctionsList$Mortality_setup$actionFile,
														dset = NULL),
								relatedElements = NULL,
								dset   = Fish$Mortality[[2]]
			),

			update_rep_health_cond = list(actionMethod = Fish$FunctionsList$UpdateReprodHealth$actionMethod,
										actionFile = Fish$FunctionsList$UpdateReprodHealth$actionFile,
										tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    	# "Before","During","After"
										transAction = NULL,
										relatedElements = Fish$Consume$relatedElements,
										dset   = Fish$ReprodHealth[[2]]
			),

			printState   = list(actionMethod = Fish$FunctionsList$StatePrint$actionMethod,
								actionFile = Fish$FunctionsList$StatePrint$actionFile,
								tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "After",    # "Before","During","After"
								relatedElements = NULL,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Fish$OutputFlags$PrintState_N,
												  fname  = Fish$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Fish$OutputFlags$PrintState_B,
												   fname  = Fish$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Fish$OutputFlags$PrintState_Stage,
												  fname  = Fish$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Fish$OutputFlags$PrintState_RepCond,
													   fname  = Fish$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Fish$OutputFlags$PrintState_Health,
												  fname  = Fish$OutputFiles$State_Health,
												  path   = NULL)
								)
			)
		)
	)
)

# declare variable to be sourced
Fish
