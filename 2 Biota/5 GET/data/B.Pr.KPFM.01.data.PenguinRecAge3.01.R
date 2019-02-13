# Input data - B.Pr.KPFM.01.data.PenguinRecAge3.01.R
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
#  1 APPA   NA      NA         0           NA         0       0      NA     0.37     NA     0        NA   NA
#  2 APW    4       32003      253873      0.083492   0       0      0.2    0.37     0      676754   20   481040
#  3 APDPW  3       32002      74798       0.111567   0       0      0.23   0.37     0      554859   20   446368
#  4 APDPE  3       32002      1084367     0.111572   0       0      0.22   0.37     0      546937   20   449224
#  5 APBSW  3       32002      1160224     0.111572   0       0      0.22   0.37     0      547800   20   448911
#  6 APBSE  4       32003      298817      0.081008   0       0      0.19   0.37     0      684375   20   486200
#  7 APEI   3       32002      1413511     0.111572   0       0      0.22   0.37     0      546148   20   449510
#  8 APE    5       32004      823403      0.058653   0       0      0.14   0.37     0      797203   20   528720
#  9 SOPA   NA      NA         0           NA         0       0      NA     0.37     NA     0        NA   NA
# 10 SOW    3       32002      2286        0.111572   0       0      0.22   0.37     0      545793   20   449638
# 11 SONE   5       32004      584507      0.059521   0       0      0.15   0.37     0      791914   20   526953
# 12 SOSE   3       32002      2003958     0.098935   0       0      0.2    0.37     0      594790   20   464345
# 13 SGPA   NA      NA         0           NA         0       0      NA     0.37     NA     0        NA   0
# 14 SGW    5       32004      6642811     0.041684   0       0      0.11   0.37     0      494138   20   489791
# 15 SGE    4       32003      564496      0.066077   0       0      0.18   0.37     0      536057   20   467405

# sorted by ID
#  SSMU     RecAge  PengID     InitAbund   M          Mswitch Mprop  Ralpha Rphi    Hq      PCsummer H50  PCwinter

#  3 APDPW  3       32002      74798       0.111567   0       0      0.23   0.37     0      554859   20   446368
#  4 APDPE  3       32002      1084367     0.111572   0       0      0.22   0.37     0      546937   20   449224
#  5 APBSW  3       32002      1160224     0.111572   0       0      0.22   0.37     0      547800   20   448911
#  7 APEI   3       32002      1413511     0.111572   0       0      0.22   0.37     0      546148   20   449510
# 10 SOW    3       32002      2286        0.111572   0       0      0.22   0.37     0      545793   20   449638
# 12 SOSE   3       32002      2003958     0.098935   0       0      0.2    0.37     0      594790   20   464345
# means                                                                              0      556054.5 20   451332.7

#  2 APW    4       32003      253873      0.083492   0       0      0.2    0.37     0      676754   20   481040
#  6 APBSE  4       32003      298817      0.081008   0       0      0.19   0.37     0      684375   20   486200
# 15 SGE    4       32003      564496      0.066077   0       0      0.18   0.37     0      536057   20   467405

#  8 APE    5       32004      823403      0.058653   0       0      0.14   0.37     0      797203   20   528720
# 11 SONE   5       32004      584507      0.059521   0       0      0.15   0.37     0      791914   20   526953
# 14 SGW    5       32004      6642811     0.041684   0       0      0.11   0.37     0      494138   20   489791

#  1 APPA   NA      NA         0           NA         0       0      NA     0.37     NA     0        NA   NA
#  9 SOPA   NA      NA         0           NA         0       0      NA     0.37     NA     0        NA   NA
# 13 SGPA   NA      NA         0           NA         0       0      NA     0.37     NA     0        NA   0
################################################################################
# start data set

Penguin 					<- list()
#-------------------------------------------------------------------------------
Penguin$signature			<- list(
	ClassName		= "Predator",
	ID             	= 23001,
	Name.full       = "KPFM penguin RecAge 3",
	Name.short      = "Penguin RecAge 3",
	Morph        	= "KPFM",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "7 July 2008"
)
	
Penguin$polygonsN 			<- 6
Penguin$Polygons            <- c(3,4,5,7,10,12)
                           # reference numbers to polygons in the list of
                           # defined polygons

Penguin$Birthdate           <- list(Day = 1, Month = 4)
                           # day and month to be used as time 0 in the year
                           # for the taxon

#-------------------------------------------------------------------------------
Penguin$ScaleToTonnes       <- 0.005
  
#-------------------------------------------------------------------------------
Penguin$Init.abundance      <- c(
                             74798    #  3 APDPW
                            ,1084367  #  4 APDPE
                            ,1160224  #  5 APBSW
                            ,1413511  #  7 APEI
                            ,2286     # 10 SOW
                            ,2003958  # 12 SOSE
)

#-------------------------------------------------------------------------------
Penguin$Stage               <- list(StageN = 5 # "pups", number of age classes in juveniles + nonbreeders (5) and breeders (6)
                               ,JuveAgeN  = 3 # equivalent to lag in KPFM
                               ,StageStrUnits  = 1 # (1 = N, 2 = B)
                               ,StageStr  = NULL # established as a list by polygon in setup
                               ,StageSize = rep(list(c(0.001 # Age 0
                                                      ,0.002 # Age 1
                                                      ,0.005 # Age 2
                                                      ,0.005 # nonbreeders
                                                      ,0.005 # breeders
                                                      )),Penguin$polygonsN)
)

#-------------------------------------------------------------------------------
Penguin$Mortality           <- list(summer = list(
                                   # M = nominal mortality over period
                                   # z = max proportion of nominal mortality that is subject to variation
                                   # v= effect of density dependence on dependent variable

                                 Age0      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,Age1      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,Age2     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,nonBreeders  = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,breeders     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                             ) # end summer
                               ,winter = list(
                                 Age0      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,Age1      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,Age2     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,nonBreeders  = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                ,breeders     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                     ,z = rep(0,Penguin$polygonsN)
                                                     ,v = rep(0,Penguin$polygonsN))
                                             ) # end winter
)

#-------------------------------------------------------------------------------
Penguin$Allocate.breeders   <- list(
                              StageNonbreeder = 4 # designated stage of nonbreeder - should always be one less than breeder
                             ,StageBreeder    = 5 #  designated stage of breeder
                             ,Phi             = rep(1,Penguin$polygonsN)
                             ,maxPropBreeders = rep(1,Penguin$polygonsN) # max proportion of non-breeders that can become breeders
                             ,SSMUdest = matrix(c( # proportion of breeders from origin breeding SSMU (rows) going to destination SSMU (cols)
                                  1,0,0,0,0,0
                                 ,0,1,0,0,0,0
                                 ,0,0,1,0,0,0
                                 ,0,0,0,1,0,0
                                 ,0,0,0,0,1,0
                                 ,0,0,0,0,0,1
                                  ),ncol=Penguin$polygonsN,byrow=TRUE)
                             ,RepConditionRemaining = 0 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Penguin$Reproduction     	<- list(
                              StageBreeder    = 5 #  designated stage of breeder
                          #   offspring mortality parameters - vector for polygons
                             ,M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935) # nominal mortality of offspring over breeding period
                             ,z = rep(0,Penguin$polygonsN) # max proportion of nominal mortality that is subject to variation
                             ,v = rep(1.5,Penguin$polygonsN) # effect of density dependence on dependent variable
                                  #calculation of alpha from Watters etal Ralpha
                                  #            print( Ralpha vector *exp(c(M vector))*AgeRec)
                             ,alpha = c(0.4492059, 0.4296881, 0.4296881, 0.4296881, 0.4296881, 0.3621025) # maximum reproductive rate per female
                             ,propfemale = rep(1,Penguin$polygonsN) # proportion of breeding population that is female
                             ,RepConditionRemaining = 1 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Penguin$Consume  			<- list(
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

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   = 18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  3
                          ,0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  4
                          ,0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  5
                          ,0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  7
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU 10
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 # colony SSMU 12
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 556054.5  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 30
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
                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  3
                          ,0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  4
                          ,0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  5
                          ,0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  7
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU 10
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 # colony SSMU 12
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 556054.5  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 30
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

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   = 18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                        #1     2     3     4     5     6     7     8     9     10    11 12    13    14    15    16   17 18
                         0.17 ,0.02 ,0.07 ,0.01 ,0.01 ,0.01 ,0.01 ,0.04 ,0.28 ,0.01 ,0 ,0.01 ,0.32 ,0.02 ,0.02 ,0   ,0 ,0   # colony SSMU  3
                        ,0.17 ,0.02 ,0.01 ,0.01 ,0.01 ,0.01 ,0.01 ,0.03 ,0.31 ,0.01 ,0 ,0.01 ,0.35 ,0.02 ,0.02 ,0   ,0 ,0   # colony SSMU  4
                        ,0.17 ,0.02 ,0.01 ,0.01 ,0.02 ,0.01 ,0.01 ,0.03 ,0.31 ,0.01 ,0 ,0.01 ,0.35 ,0.02 ,0.02 ,0   ,0 ,0   # colony SSMU  5
                        ,0.17 ,0.02 ,0.01 ,0.01 ,0.01 ,0.01 ,0.02 ,0.03 ,0.32 ,0.01 ,0 ,0.01 ,0.36 ,0.02 ,0.02 ,0   ,0 ,0   # colony SSMU  7
                        ,0.17 ,0.02 ,0.01 ,0.01 ,0.01 ,0.01 ,0.01 ,0.03 ,0.32 ,0.01 ,0 ,0.01 ,0.36 ,0.02 ,0.02 ,0   ,0 ,0   # colony SSMU 10
                        ,0.13 ,0.01 ,0.01 ,0.01 ,0.01 ,0.01 ,0.01 ,0.02 ,0.24 ,0    ,0 ,0.04 ,0.27 ,0.01 ,0.02 ,0.1 ,0 ,0.1 # colony SSMU 12
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 451332.7  # maximum per capita consumption of krill
                                   ,PropInDiet           = 1  # proportion of krill in diet
                                   ,Holling_q            = 1
                                   ,Holling_D            = 30
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
Penguin$ReprodHealth        <- list(summer = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
                               ,winter = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
)

#-------------------------------------------------------------------------------
Penguin$Update.age 			<- NULL
#-------------------------------------------------------------------------------
Penguin$Breeders.to.nonbreeders <- list(
                              StageNonbreeder = 4 # designated stage of nonbreeder
                             ,StageBreeder    = 5 #  designated stage of breeder
                             ,Breeders        = matrix(0,nrow=Penguin$polygonsN,ncol=Penguin$polygonsN)
)

#-------------------------------------------------------------------------------
Penguin$Initialise          <- list(NULL)
Penguin$Transition.data     <- list()
Penguin$PrintState          <- list(OutDir   = NULL, OutFiles = NULL)
Penguin$FunctionsList       <- list(Allocate_breeders = list(actionMethod = "allocateBreeders", 
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
Penguin$OutputFiles         <- list(State_N      = "Biota.PengA3.State.N.dat"
                             ,State_B       = "Biota.PengA3.State.B.dat"
                             ,State_Stage   = "Biota.PengA3.State.Stage.dat"
                             ,State_RepCond = "Biota.PengA3.State.RepCond.dat"
                             ,State_Health  = "Biota.PengA3.State.Health.dat"
)
#-------------------------------------------------------------------------------
Penguin$OutputFlags         <- list(PrintState_N      = TRUE
                             ,PrintState_B       = TRUE
                             ,PrintState_Stage   = TRUE
                             ,PrintState_RepCond = TRUE
                             ,PrintState_Health  = TRUE
)
	
Penguin$Functions 			<- list(
			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup = NULL,
			# data and function to initialise element at the beginning of each scenario
			#   i.e. how should the element be reset at time 0
			printState    = list(actionMethod = Penguin$FunctionsList$StatePrint$actionMethod,
								actionFile   = Penguin$FunctionsList$StatePrint$actionFile,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Penguin$OutputFlags$PrintState_N,
												  fname  = Penguin$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Penguin$OutputFlags$PrintState_B,
												   fname  = Penguin$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Penguin$OutputFlags$PrintState_Stage,
												  fname  = Penguin$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Penguin$OutputFlags$PrintState_RepCond,
													   fname  = Penguin$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Penguin$OutputFlags$PrintState_Health,
												  fname  = Penguin$OutputFiles$State_Health,
												  path   = NULL)
								)
			),
      
			stateUpdate  = list(actionMethod = Penguin$FunctionsList$StateUpdate$actionMethod,
								actionFile = Penguin$FunctionsList$StateUpdate$actionFile,
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

Penguin$Timesteps 			<- list(
	Summer = list(calday=dayFromDate(31,3),
		actionsN=NULL,  # will be updated below
		actions=list(
			allocate_breeders = list(actionMethod = Penguin$FunctionsList$Allocate_breeders$actionMethod,
									actionFile = Penguin$FunctionsList$Allocate_breeders$actionFile,
									tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "Before",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Penguin$Allocate.breeders
			),

			consume     	  = list(actionMethod = Penguin$FunctionsList$Consume$actionMethod,
									actionFile = Penguin$FunctionsList$Consume$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Penguin$FunctionsList$Consume_setup$actionMethod,
															actionFile = Penguin$FunctionsList$Consume_setup$actionFile,
															dset = NULL),
									relatedElements = Penguin$Consume$relatedElements,
									dset   = Penguin$Consume$dset[[1]]
			),

			mortality   	  = list(actionMethod = Penguin$FunctionsList$Mortality$actionMethod,
									actionFile = Penguin$FunctionsList$Mortality$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Penguin$FunctionsList$Mortality_setup$actionMethod,
															actionFile = Penguin$FunctionsList$Mortality_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Penguin$Mortality[[1]]
			),

			update_rep_health = list(actionMethod = Penguin$FunctionsList$UpdateReprodHealth$actionMethod,
									actionFile = Penguin$FunctionsList$UpdateReprodHealth$actionFile,
									tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    	# "Before","During","After"
									transAction = NULL,
									relatedElements = Penguin$Consume$relatedElements,
									dset   = Penguin$ReprodHealth[[1]]
			),

			update_age 		  = list(actionMethod = Penguin$FunctionsList$Update_age$actionMethod,
									actionFile = Penguin$FunctionsList$Update_age$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Penguin$Update.age
			),

			reproduce   	  = list(actionMethod = Penguin$FunctionsList$Reproduce$actionMethod,
									actionFile = Penguin$FunctionsList$Reproduce$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = list(actionMethod = Penguin$FunctionsList$Reproduce_setup$actionMethod,
															actionFile = Penguin$FunctionsList$Reproduce_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Penguin$Reproduction
			),

			breedersToNonbreeders = list(actionMethod = Penguin$FunctionsList$BreedersToNonbreeders$actionMethod,
										actionFile = Penguin$FunctionsList$BreedersToNonbreeders$actionFile,
										tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    # "Before","During","After"
										transAction = NULL, # list(fn = , dset = )
										relatedElements = NULL,
										dset   = Penguin$Breeders.to.nonbreeders
			),
			printState 		  = list(actionMethod = Penguin$FunctionsList$StatePrint$actionMethod,
									actionFile = Penguin$FunctionsList$StatePrint$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									relatedElements = NULL,
									dset = list(  # List because may need more than one file to print state
											Number = list(output = Penguin$OutputFlags$PrintState_N,
														  fname  = Penguin$OutputFiles$State_N,
														  path   = NULL),
											Biomass = list(output = Penguin$OutputFlags$PrintState_B,
														   fname  = Penguin$OutputFiles$State_B,
														   path   = NULL),
											Stage  = list(output = Penguin$OutputFlags$PrintState_Stage,
														  fname  = Penguin$OutputFiles$State_Stage,
														  path   = NULL),
											Reprod_Cond = list(output = Penguin$OutputFlags$PrintState_RepCond,
															   fname  = Penguin$OutputFiles$State_RepCond,
															   path   = NULL),
											Health = list(output = Penguin$OutputFlags$PrintState_Health,
														  fname  = Penguin$OutputFiles$State_Health,
														  path   = NULL)
									)
			)
		) 
	),
	Winter = list(calday=dayFromDate(30,9),
		actionsN=NULL, # will be updated below
		actions=list(
			consume      = list(actionMethod = Penguin$FunctionsList$Consume$actionMethod,
								actionFile = Penguin$FunctionsList$Consume$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = NULL,
								relatedElements = Penguin$Consume$relatedElements,
								dset   = Penguin$Consume$dset[[2]]
			),

			mortality    = list(actionMethod = Penguin$FunctionsList$Mortality$actionMethod,
								actionFile = Penguin$FunctionsList$Mortality$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = list(actionMethod = Penguin$FunctionsList$Mortality_setup$actionMethod,
														actionFile = Penguin$FunctionsList$Mortality_setup$actionFile,
														dset = NULL),
								relatedElements = NULL,
								dset   = Penguin$Mortality[[2]]
			),

			update_rep_health_cond = list(actionMethod = Penguin$FunctionsList$UpdateReprodHealth$actionMethod,
										actionFile = Penguin$FunctionsList$UpdateReprodHealth$actionFile,
										tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    	# "Before","During","After"
										transAction = NULL,
										relatedElements = Penguin$Consume$relatedElements,
										dset   = Penguin$ReprodHealth[[2]]
			),

			printState   = list(actionMethod = Penguin$FunctionsList$StatePrint$actionMethod,
								actionFile = Penguin$FunctionsList$StatePrint$actionFile,
								tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "After",    # "Before","During","After"
								relatedElements = NULL,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Penguin$OutputFlags$PrintState_N,
												  fname  = Penguin$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Penguin$OutputFlags$PrintState_B,
												   fname  = Penguin$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Penguin$OutputFlags$PrintState_Stage,
												  fname  = Penguin$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Penguin$OutputFlags$PrintState_RepCond,
													   fname  = Penguin$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Penguin$OutputFlags$PrintState_Health,
												  fname  = Penguin$OutputFiles$State_Health,
												  path   = NULL)
								)
			)
		)
	)
)

# declare variable to be sourced
Penguin