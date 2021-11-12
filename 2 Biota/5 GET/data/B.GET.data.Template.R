# Input data - B.GET.data.Template.01.R
# Generalised consumer

GET01 					<- list()
#-------------------------------------------------------------------------------
GET01$signature			<- list(
	ClassName		    = "GET",
	ID             	= 23001,
	Name.full       = "General Consumer Template",
	Name.short      = "GET01",
	Morph        	= "GET01",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "6 June 2015"
)
	
GET01$polygonsN 			<- 6
GET01$Polygons            <- c(3,4,5,7,10,12)
                           # reference numbers to polygons in the list of
                           # defined polygons

GET01$Birthdate           <- list(Day = 1, Month = 4)
                           # day and month to be used as time 0 in the year
                           # for the taxon

#-------------------------------------------------------------------------------
GET01$ScaleToTonnes       <- 0.005
  
#-------------------------------------------------------------------------------
GET01$Init.abundance      <- c(
                             74798    #  3 APDPW
                            ,1084367  #  4 APDPE
                            ,1160224  #  5 APBSW
                            ,1413511  #  7 APEI
                            ,2286     # 10 SOW
                            ,2003958  # 12 SOSE
)

#-------------------------------------------------------------------------------
GET01$Stage               <- list(StageN = 5 # "pups", number of age classes in juveniles + nonbreeders (5) and breeders (6)
                               ,JuveAgeN  = 3 # equivalent to lag in KPFM
                               ,StageStrUnits  = 1 # (1 = N, 2 = B)
                               ,StageStr  = NULL # established as a list by polygon in setup
                               ,StageSize = rep(list(c(0.001 # Age 0
                                                      ,0.002 # Age 1
                                                      ,0.005 # Age 2
                                                      ,0.005 # nonbreeders
                                                      ,0.005 # breeders
                                                      )),GET01$polygonsN)
)

#-------------------------------------------------------------------------------
GET01$Mortality           <- list(summer = list(
                                   # M = nominal mortality over period
                                   # z = threshold of health condition below which survivorship is impacted by health
                                   # v= value of health condition for which survivorship is reduced to 0.5  (v must be less than z)

                                 Age0      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,Age1      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,Age2     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,nonBreeders  = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,breeders     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                             ) # end summer
                               ,winter = list(
                                 Age0      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,Age1      = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,Age2     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,nonBreeders  = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                ,breeders     = list (M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935)
                                                  ,Health = list(TestHealth = FALSE
                                                                ,z = rep(0,GET01$polygonsN)
                                                                ,vprime = rep(0,GET01$polygonsN)  # calculate this value as (-0.693/log(Health$z[pn]/Health$v[pn]))
                                                                ))
                                             ) # end winter
)

#-------------------------------------------------------------------------------
GET01$Allocate.breeders   <- list(
                              StageNonbreeder = 4 # designated stage of nonbreeder - should always be one less than breeder
                             ,StageBreeder    = 5 #  designated stage of breeder
                             ,Phi             = rep(1,GET01$polygonsN)
                             ,maxPropBreeders = rep(1,GET01$polygonsN) # max proportion of non-breeders that can become breeders
                             ,PolygonDest = matrix(c( # proportion of breeders from origin breeding Polygon (rows) going to destination Polygon (cols)
                                  1,0,0,0,0,0
                                 ,0,1,0,0,0,0
                                 ,0,0,1,0,0,0
                                 ,0,0,0,1,0,0
                                 ,0,0,0,0,1,0
                                 ,0,0,0,0,0,1
                                  ),ncol=GET01$polygonsN,byrow=TRUE)
                             ,RepConditionRemaining = 0 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
GET01$Reproduction     	<- list(
                              StageBreeder    = 5 #  designated stage of breeder
                          #   offspring mortality parameters - vector for polygons
                             ,M = c(0.111567,0.111572,0.111572,0.111572,0.111572,0.098935) # nominal mortality of offspring over breeding period
                             ,z = rep(0,GET01$polygonsN) # max proportion of nominal mortality that is subject to variation
                             ,v = rep(1.5,GET01$polygonsN) # effect of density dependence on dependent variable
                                  #calculation of alpha from Watters etal Ralpha
                                  #            print( Ralpha vector *exp(c(M vector))*AgeRec)
                             ,alpha = c(0.4492059, 0.4296881, 0.4296881, 0.4296881, 0.4296881, 0.3621025) # maximum reproductive rate per female
                             ,propfemale = rep(1,GET01$polygonsN) # proportion of breeding population that is female
                             ,RepConditionRemaining = 1 # reproductive condition remaining after allocation to breeders
)

#-------------------------------------------------------------------------------
GET01$Consume  			<- list(
         relatedElements = matrix(c("Biota", "Krill"),ncol=2,byrow=TRUE) # krill
        ,feeding.Polygon    = c(1:18)  # reference numbers for polygons in polygon
                                     # list in which feeding can occur by local populations
                                     # this list is used to set up the proportions
                                     # of prey polygons in the feeding polygons below
        ,feeding.Polygon.N   = 18
       # reference to related elements below is by relative row number
        ,dset = list(
        
              #-----------------------------------
              summer  = list( # by predator stage - if NULL then no consumption by that stage
                      Age0 = NULL
                     ,Age1 = NULL
                     ,Age2 = NULL

                  #-------------------------
                     ,NonBreeder = list(
                          feeding.Polygon.N   = 18
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
                          feeding.Polygon.N   = 18
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
                          feeding.Polygon.N   = 18
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
GET01$ReprodHealth        <- list(summer = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
                               ,winter = list(
                                           FoodValue=c(1) # vector of food values for each prey (in sequence according to list in Consume)
                                           )
)

#-------------------------------------------------------------------------------
GET01$Advance.PupsToJuvenile 	<- list(Stage0 = 1   # uses AdvanceStageNum
                                     ,Stage1 = 2   # across all polygons, advancing one stage to another stage and leaving none in previous stage.

#-------------------------------------------------------------------------------
GET01$Update.age 			<- NULL   # reserve for doing age structured population update age at birthday

#-------------------------------------------------------------------------------
GET01$Breeders.to.nonbreeders <- list(
                              StageNonbreeder = 4 # designated stage of nonbreeder
                             ,StageBreeder    = 5 #  designated stage of breeder
                             ,Breeders        = matrix(0,nrow=GET01$polygonsN,ncol=GET01$polygonsN)
)

#-------------------------------------------------------------------------------
GET01$Initialise          <- list(NULL)
GET01$Transition.data     <- list()
GET01$PrintState          <- list(OutDir   = NULL, OutFiles = NULL)
GET01$FunctionsList       <- list(Allocate_breeders = list(actionMethod = "allocateBreeders",
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
GET01$OutputFiles         <- list(State_N      = "Biota.PengA3.State.N.dat"
                             ,State_B       = "Biota.PengA3.State.B.dat"
                             ,State_Stage   = "Biota.PengA3.State.Stage.dat"
                             ,State_RepCond = "Biota.PengA3.State.RepCond.dat"
                             ,State_Health  = "Biota.PengA3.State.Health.dat"
)
#-------------------------------------------------------------------------------
GET01$OutputFlags         <- list(PrintState_N      = TRUE
                             ,PrintState_B       = TRUE
                             ,PrintState_Stage   = TRUE
                             ,PrintState_RepCond = TRUE
                             ,PrintState_Health  = TRUE
)
	
GET01$Functions 			<- list(
			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup = NULL,
			# data and function to initialise element at the beginning of each scenario
			#   i.e. how should the element be reset at time 0
			printState    = list(actionMethod = GET01$FunctionsList$StatePrint$actionMethod,
								actionFile   = GET01$FunctionsList$StatePrint$actionFile,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = GET01$OutputFlags$PrintState_N,
												  fname  = GET01$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = GET01$OutputFlags$PrintState_B,
												   fname  = GET01$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = GET01$OutputFlags$PrintState_Stage,
												  fname  = GET01$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = GET01$OutputFlags$PrintState_RepCond,
													   fname  = GET01$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = GET01$OutputFlags$PrintState_Health,
												  fname  = GET01$OutputFiles$State_Health,
												  path   = NULL)
								)
			),
      
			stateUpdate  = list(actionMethod = GET01$FunctionsList$StateUpdate$actionMethod,
								actionFile = GET01$FunctionsList$StateUpdate$actionFile,
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

GET01$Timesteps 			<- list(
	Summer = list(calday=dayFromDate(31,3),
		actionsN=NULL,  # will be updated below
		actions=list(
			allocate_breeders = list(actionMethod = GET01$FunctionsList$Allocate_breeders$actionMethod,
									actionFile = GET01$FunctionsList$Allocate_breeders$actionFile,
									tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "Before",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = GET01$Allocate.breeders
			),

			consume     	  = list(actionMethod = GET01$FunctionsList$Consume$actionMethod,
									actionFile = GET01$FunctionsList$Consume$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = GET01$FunctionsList$Consume_setup$actionMethod,
															actionFile = GET01$FunctionsList$Consume_setup$actionFile,
															dset = NULL),
									relatedElements = GET01$Consume$relatedElements,
									dset   = GET01$Consume$dset[[1]]
			),

			mortality   	  = list(actionMethod = GET01$FunctionsList$Mortality$actionMethod,
									actionFile = GET01$FunctionsList$Mortality$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = list(actionMethod = GET01$FunctionsList$Mortality_setup$actionMethod,
															actionFile = GET01$FunctionsList$Mortality_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = GET01$Mortality[[1]]
			),

			update_rep_health = list(actionMethod = GET01$FunctionsList$UpdateReprodHealth$actionMethod,
									actionFile = GET01$FunctionsList$UpdateReprodHealth$actionFile,
									tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    	# "Before","During","After"
									transAction = NULL,
									relatedElements = GET01$Consume$relatedElements,
									dset   = GET01$ReprodHealth[[1]]
			),

			update_age 		  = list(actionMethod = GET01$FunctionsList$Update_age$actionMethod,
									actionFile = GET01$FunctionsList$Update_age$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = GET01$Update.age
			),

			reproduce   	  = list(actionMethod = GET01$FunctionsList$Reproduce$actionMethod,
									actionFile = GET01$FunctionsList$Reproduce$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = list(actionMethod = GET01$FunctionsList$Reproduce_setup$actionMethod,
															actionFile = GET01$FunctionsList$Reproduce_setup$actionFile,
															dset = NULL),
									relatedElements = NULL,
									dset   = GET01$Reproduction
			),

			breedersToNonbreeders = list(actionMethod = GET01$FunctionsList$BreedersToNonbreeders$actionMethod,
										actionFile = GET01$FunctionsList$BreedersToNonbreeders$actionFile,
										tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    # "Before","During","After"
										transAction = NULL, # list(fn = , dset = )
										relatedElements = NULL,
										dset   = GET01$Breeders.to.nonbreeders
			),
			printState 		  = list(actionMethod = GET01$FunctionsList$StatePrint$actionMethod,
									actionFile = GET01$FunctionsList$StatePrint$actionFile,
									tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									relatedElements = NULL,
									dset = list(  # List because may need more than one file to print state
											Number = list(output = GET01$OutputFlags$PrintState_N,
														  fname  = GET01$OutputFiles$State_N,
														  path   = NULL),
											Biomass = list(output = GET01$OutputFlags$PrintState_B,
														   fname  = GET01$OutputFiles$State_B,
														   path   = NULL),
											Stage  = list(output = GET01$OutputFlags$PrintState_Stage,
														  fname  = GET01$OutputFiles$State_Stage,
														  path   = NULL),
											Reprod_Cond = list(output = GET01$OutputFlags$PrintState_RepCond,
															   fname  = GET01$OutputFiles$State_RepCond,
															   path   = NULL),
											Health = list(output = GET01$OutputFlags$PrintState_Health,
														  fname  = GET01$OutputFiles$State_Health,
														  path   = NULL)
									)
			)
		) 
	),
	Winter = list(calday=dayFromDate(30,9),
		actionsN=NULL, # will be updated below
		actions=list(
			consume      = list(actionMethod = GET01$FunctionsList$Consume$actionMethod,
								actionFile = GET01$FunctionsList$Consume$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = NULL,
								relatedElements = GET01$Consume$relatedElements,
								dset   = GET01$Consume$dset[[2]]
			),

			mortality    = list(actionMethod = GET01$FunctionsList$Mortality$actionMethod,
								actionFile = GET01$FunctionsList$Mortality$actionFile,
								tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "During",    # "Before","During","After"
								transAction = list(actionMethod = GET01$FunctionsList$Mortality_setup$actionMethod,
														actionFile = GET01$FunctionsList$Mortality_setup$actionFile,
														dset = NULL),
								relatedElements = NULL,
								dset   = GET01$Mortality[[2]]
			),

			update_rep_health_cond = list(actionMethod = GET01$FunctionsList$UpdateReprodHealth$actionMethod,
										actionFile = GET01$FunctionsList$UpdateReprodHealth$actionFile,
										tsType = "AllPeriods", 	# "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
										tsTiming = "After",    	# "Before","During","After"
										transAction = NULL,
										relatedElements = GET01$Consume$relatedElements,
										dset   = GET01$ReprodHealth[[2]]
			),

			printState   = list(actionMethod = GET01$FunctionsList$StatePrint$actionMethod,
								actionFile = GET01$FunctionsList$StatePrint$actionFile,
								tsType = "LastPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
								tsTiming = "After",    # "Before","During","After"
								relatedElements = NULL,
								dset = list(  # List because may need more than one file to print state
									Number = list(output = GET01$OutputFlags$PrintState_N,
												  fname  = GET01$OutputFiles$State_N,
												  path   = NULL),
									Biomass = list(output = GET01$OutputFlags$PrintState_B,
												   fname  = GET01$OutputFiles$State_B,
												   path   = NULL),
									Stage  = list(output = GET01$OutputFlags$PrintState_Stage,
												  fname  = GET01$OutputFiles$State_Stage,
												  path   = NULL),
									Reprod_Cond = list(output = GET01$OutputFlags$PrintState_RepCond,
													   fname  = GET01$OutputFiles$State_RepCond,
													   path   = NULL),
									Health = list(output = GET01$OutputFlags$PrintState_Health,
												  fname  = GET01$OutputFiles$State_Health,
												  path   = NULL)
								)
			)
		)
	)
)

# declare variable to be sourced
GET01