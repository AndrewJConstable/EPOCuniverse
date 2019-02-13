# Input data - B.Pr.KPFM.01.data.seals.01.R
# KPFM predator

# notes on development of general function - mark completed as done
#    Allocate_breeders       (completed)  B.Pr.KPFM.Allocate_breeders.01.R
#    Consume                 (completed)      B.Pr.KPFM.Consume.01.R
#    UpdateReprodHealth      (completed)  B.Pr.KPFM.UpdateReprodHealth.01.R
#    Update_age              (completed)  B.Pr.KPFM.Update_age.01.R
#    Reproduce               (completed)  B.Pr.KPFM.Reproduce.01.R
#    Mortality               (completed)  B.Pr.KPFM.Mortality.01.R
#    BreedersToNonbreeders   (completed)  B.Pr.KPFM.BreedersToNonbreeders.01.R
#    Scenario_setup             (completed)  B.Pr.KPFM.Time0.fn.01.R
#    StatePrint              (completed)  B.Pr.KPFM.printState.01.R
#    TransitionSetup         (completed)  B.Pr.KPFM.TransitionSetup.01.R
#    StateUpdate             (completed)  B.Pr.KPFM.update_State.01.R
################################################################################

Seal 						<- list()
#-------------------------------------------------------------------------------
Seal$signature				<- list(
	ClassName 		= "Predator",
	ID             	= 23004,
	Name.full       = "KPFM seal",
	Name.short      = "Seal",
	Morph        	= "KPFM",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "7 July 2008"
)

Seal$polygonsN				<- 8
Seal$Polygons              	<- c(3,4,7,10,11,12,14,15)
                           # reference numbers to polygons in the list of
                           # defined polygons

Seal$Birthdate              <- list(Day = 1, Month = 4)
                           # day and month to be used as time 0 in the year
                           # for the taxon

#-------------------------------------------------------------------------------
Seal$ScaleToTonnes         	<- 0.5
  
#-------------------------------------------------------------------------------
Seal$Init.abundance        	<- c(  12204.0  #3 APDPW
                            ,    211.0  #4 APDPE
                            ,   1002.0  #7 APEI
                            ,     10.0  #10 SOW
                            ,     10.0  #11 SONE
                            ,     10.0  #12 SOSE
                            , 611054.0  #14 SGW
                            ,   6090.0  #15 SGE
)
                             
#-------------------------------------------------------------------------------
Seal$Stage                 	<- list(StageN = 6 # "pups", number of age classes in juveniles + nonbreeders (5) and breeders (6)
                               ,JuveAgeN  = 4 # equivalent to lag in KPFM
                               ,StageStrUnits  = 1 # (1 = N, 2 = B)
                               ,StageStr  = NULL # established as a list by polygon in setup
                               ,StageSize = rep(list(c(0,0.02,0.1,0.2,0.3,0.3)),Seal$polygonsN)
)

#-------------------------------------------------------------------------------
Seal$Mortality          	<- list(summer = list(
                                   # M = nominal mortality over period
                                   # z = max proportion of nominal mortality that is subject to variation
                                   # v= effect of density dependence on dependent variable

                                 pupAge0      = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,juveAge1     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,juveAge2     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,juveAge3     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,nonBreeders  = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,breeders     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))

                                             ) # end summer
                               ,winter = list(
                                 pupAge0      = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,juveAge1     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,juveAge2     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,juveAge3     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,nonBreeders  = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                ,breeders     = list (M = rep(0.130574,Seal$polygonsN)
                                                     ,z = rep(0,Seal$polygonsN)
                                                     ,v = rep(0,Seal$polygonsN))
                                             ) # end winter
)

#-------------------------------------------------------------------------------
Seal$Allocate.breeders     	<- list(
                              StageNonbreeder = 5 # designated stage of nonbreeder - should always be one less than breeder
                             ,StageBreeder    = 6 #  designated stage of breeder
                             ,Phi             = rep(1,Seal$polygonsN)
                             ,maxPropBreeders = rep(1,Seal$polygonsN) # max proportion of non-breeders that can become breeders
                             ,SSMUdest = matrix(c( # proportion of breeders from origin breeding SSMU (rows) going to destination SSMU (cols)
                                  1,0,0,0,0,0,0,0
                                 ,0,1,0,0,0,0,0,0
                                 ,0,0,1,0,0,0,0,0
                                 ,0,0,0,1,0,0,0,0
                                 ,0,0,0,0,1,0,0,0
                                 ,0,0,0,0,0,1,0,0
                                 ,0,0,0,0,0,0,1,0
                                 ,0,0,0,0,0,0,0,1
                                  ),ncol=Seal$polygonsN,byrow=TRUE)
                             ,RepConditionRemaining = 0 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Seal$Reproduction     		<- list(
                              StageBreeder    = 6 #  designated stage of breeder

                          #   offspring mortality parameters - vector for polygons
                             ,M = rep(0.130574,Seal$polygonsN) # nominal mortality of offspring over breeding period
                             ,z = rep(0,Seal$polygonsN) # max proportion of nominal mortality that is subject to variation
                             ,v = rep(1.5,Seal$polygonsN) # effect of density dependence on dependent variable
                             ,alpha = rep(1.14,Seal$polygonsN) # maximum reproductive rate per female
                             ,propfemale = rep(1,Seal$polygonsN) # proportion of breeding population that is female
                             ,RepConditionRemaining = 1 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
Seal$Consume  				<- list(
         relatedElements = matrix(c("Biota", "Krill"),ncol=2,byrow=TRUE) # krill
        ,feeding.SSMUs    = c(1:18)  # reference numbers for polygons in polygon
                                     # list in which feeding can occur by local populations
                                     # this list is used to set up the proportions
                                     # of prey polygons in the feeding polygons below
        ,feeding.SSMU.N   =18
       # reference to related elements below is by relative row number
        ,dset = list(
        
              #-----------------------------------
              summer  = list( # by predator stage - if NULL then no consumption by that stage
                      pup    = NULL
                     ,j_age1 = NULL
                     ,j_age2 = NULL
                     ,j_age3 = NULL

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   =18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  3
                          ,0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  4
                          ,0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  7
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU 10
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 # colony SSMU 11
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 # colony SSMU 12
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 # colony SSMU 14
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 # colony SSMU 15
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 3507360  # maximum per capita consumption of krill
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
                          feeding.SSMU.N   =18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                         # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  3
                          ,0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  4
                          ,0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU  7
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 # colony SSMU 10
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 # colony SSMU 11
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 # colony SSMU 12
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 # colony SSMU 14
                          ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 # colony SSMU 15
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 3507360  # maximum per capita consumption of krill
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
                      pup    = NULL
                     ,j_age1 = NULL
                     ,j_age2 = NULL
                     ,j_age3 = NULL

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.SSMU.N   =18
                         ,PropFeedInPolygon = matrix(c( # rows - local populations,
                                                        # cols - feeding polygons
                        #   1     2  3  4  5     6     7     8     9 10 11 12    13    14   15    16   17   18
                         0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU  3
                        ,0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU  4
                        ,0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU  7
                        ,0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU 10
                        ,0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU 11
                        ,0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU 12
                        ,0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU 14
                        ,0.09, 0.01, 0, 0, 0, 0.01, 0.01, 0.01, 0.16, 0, 0, 0, 0.18, 0.01, 0.01, 0.1, 0.3, 0.1 # colony SSMU 15
                           ),ncol=18,byrow=TRUE)
                         ,Prey = list(
                            Krill = list(
                                    PerCapita       = 2260218  # maximum per capita consumption of krill
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
Seal$ReprodHealth          	<- list(summer = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
                               ,winter = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
)

#-------------------------------------------------------------------------------
Seal$Update.age 			<- NULL
#-------------------------------------------------------------------------------
Seal$Breeders.to.nonbreeders <- list(
                              StageNonbreeder = 5 # designated stage of nonbreeder
                             ,StageBreeder    = 6 #  designated stage of breeder
                             ,Breeders        = matrix(0,nrow=Seal$polygonsN,ncol=Seal$polygonsN)
)

#-------------------------------------------------------------------------------
Seal$Initialise            	<- list(NULL)
Seal$Transition.data       	<- list()
Seal$PrintState            	<- list(OutDir   = NULL, OutFiles = NULL)
Seal$FunctionsList          <- list(Allocate_breeders     = list(actionMethod = "allocateBreeders", 
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
Seal$OutputFiles         	<- list(State_N       = "Biota.Seal.State.N.dat"
								 ,State_B       = "Biota.Seal.State.B.dat"
								 ,State_Stage   = "Biota.Seal.State.Stage.dat"
								 ,State_RepCond = "Biota.Seal.State.RepCond.dat"
								 ,State_Health  = "Biota.Seal.State.Health.dat"
)
#-------------------------------------------------------------------------------
Seal$OutputFlags         	<- list(PrintState_N       = TRUE
								 ,PrintState_B       = TRUE
								 ,PrintState_Stage   = TRUE
								 ,PrintState_RepCond = TRUE
								 ,PrintState_Health  = TRUE
)

Seal$Functions 				<- list(
			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup = NULL,
			# data and function to initialise element at the beginning of each scenario
			#   i.e. how should the element be reset at time 0
			printState    = list(actionMethod = Seal$FunctionsList$StatePrint$actionMethod,
								actionFile   = Seal$FunctionsList$StatePrint$actionFile,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Seal$OutputFlags$PrintState_N,
												  fname  = Seal$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Seal$OutputFlags$PrintState_B,
												   fname  = Seal$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Seal$OutputFlags$PrintState_Stage,
												  fname  = Seal$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Seal$OutputFlags$PrintState_RepCond,
													   fname  = Seal$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Seal$OutputFlags$PrintState_Health,
												  fname  = Seal$OutputFiles$State_Health,
												  path   = NULL)
								)
			),
      
			stateUpdate  = list(actionMethod = Seal$FunctionsList$StateUpdate$actionMethod,
								actionFile = Seal$FunctionsList$StateUpdate$actionFile,
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

Seal$Timesteps 			<- list(
	Summer = list(calday=dayFromDate(31,3),
		actionsN=NULL,  # will be updated below
		actions=list(
			allocate_breeders = list(actionMethod = Seal$FunctionsList$Allocate_breeders$actionMethod,
									actionFile = Seal$FunctionsList$Allocate_breeders$actionFile,
									tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "Before",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Seal$Allocate.breeders
			),

			consume     	  = list(actionMethod = Seal$FunctionsList$Consume$actionMethod,
									actionFile = Seal$FunctionsList$Consume$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Seal$FunctionsList$Consume_setup$actionMethod,
															actionFile = Seal$FunctionsList$Consume_setup$actionFile,
															dset = NULL),
									relatedElements = Seal$Consume$relatedElements,
									dset   = Seal$Consume$dset[[1]]
			),

			mortality   	  = list(actionMethod = Seal$FunctionsList$Mortality$actionMethod,
									actionFile = Seal$FunctionsList$Mortality$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = Seal$FunctionsList$Mortality_setup$actionMethod,
															actionFile = Seal$FunctionsList$Mortality_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Seal$Mortality[[1]]
			),

			update_rep_health = list(actionMethod = Seal$FunctionsList$UpdateReprodHealth$actionMethod,
									actionFile = Seal$FunctionsList$UpdateReprodHealth$actionFile,
									tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    	# "Before","During","After"
									transAction = NULL,
									relatedElements = Seal$Consume$relatedElements,
									dset   = Seal$ReprodHealth[[1]]
			),

			update_age 		  = list(actionMethod = Seal$FunctionsList$Update_age$actionMethod,
									actionFile = Seal$FunctionsList$Update_age$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Seal$Update.age
			),

			reproduce   	  = list(actionMethod = Seal$FunctionsList$Reproduce$actionMethod,
									actionFile = Seal$FunctionsList$Reproduce$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = list(actionMethod = Seal$FunctionsList$Reproduce_setup$actionMethod,
															actionFile = Seal$FunctionsList$Reproduce_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = Seal$Reproduction
			),

			breedersToNonbreeders = list(actionMethod = Seal$FunctionsList$BreedersToNonbreeders$actionMethod,
										actionFile = Seal$FunctionsList$BreedersToNonbreeders$actionFile,
										tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    # "Before","During","After"
										transAction = NULL, # list(fn = , dset = )
										relatedElements = NULL,
										dset   = Seal$Breeders.to.nonbreeders
			),
			printState 		  = list(actionMethod = Seal$FunctionsList$StatePrint$actionMethod,
									actionFile = Seal$FunctionsList$StatePrint$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									relatedElements = NULL,
									dset = list(  # List because may need more than one file to print state
											Number = list(output = Seal$OutputFlags$PrintState_N,
														  fname  = Seal$OutputFiles$State_N,
														  path   = NULL),
											Biomass = list(output = Seal$OutputFlags$PrintState_B,
														   fname  = Seal$OutputFiles$State_B,
														   path   = NULL),
											Stage  = list(output = Seal$OutputFlags$PrintState_Stage,
														  fname  = Seal$OutputFiles$State_Stage,
														  path   = NULL),
											Reprod_Cond = list(output = Seal$OutputFlags$PrintState_RepCond,
															   fname  = Seal$OutputFiles$State_RepCond,
															   path   = NULL),
											Health = list(output = Seal$OutputFlags$PrintState_Health,
														  fname  = Seal$OutputFiles$State_Health,
														  path   = NULL)
									)
			)
		) 
	),
	Winter = list(calday=dayFromDate(30,9),
		actionsN=NULL, # will be updated below
		actions=list(
			consume      = list(actionMethod = Seal$FunctionsList$Consume$actionMethod,
								actionFile = Seal$FunctionsList$Consume$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = NULL,
								relatedElements = Seal$Consume$relatedElements,
								dset   = Seal$Consume$dset[[2]]
			),

			mortality    = list(actionMethod = Seal$FunctionsList$Mortality$actionMethod,
								actionFile = Seal$FunctionsList$Mortality$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = list(actionMethod = Seal$FunctionsList$Mortality_setup$actionMethod,
														actionFile = Seal$FunctionsList$Mortality_setup$actionFile,
														dset = NULL),
								relatedElements = NULL,
								dset   = Seal$Mortality[[2]]
			),

			update_rep_health_cond = list(actionMethod = Seal$FunctionsList$UpdateReprodHealth$actionMethod,
										actionFile = Seal$FunctionsList$UpdateReprodHealth$actionFile,
										tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    	# "Before","During","After"
										transAction = NULL,
										relatedElements = Seal$Consume$relatedElements,
										dset   = Seal$ReprodHealth[[2]]
			),

			printState   = list(actionMethod = Seal$FunctionsList$StatePrint$actionMethod,
								actionFile = Seal$FunctionsList$StatePrint$actionFile,
								tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "After",    # "Before","During","After"
								relatedElements = NULL,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = Seal$OutputFlags$PrintState_N,
												  fname  = Seal$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = Seal$OutputFlags$PrintState_B,
												   fname  = Seal$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = Seal$OutputFlags$PrintState_Stage,
												  fname  = Seal$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = Seal$OutputFlags$PrintState_RepCond,
													   fname  = Seal$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = Seal$OutputFlags$PrintState_Health,
												  fname  = Seal$OutputFiles$State_Health,
												  path   = NULL)
								)
			)
		)
	)
)

# declare variable to be sourced
Seal