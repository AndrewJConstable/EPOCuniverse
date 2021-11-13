# Input data - A.Fi.KPFM.01.data.01.R
# KPFM fishery

# notes on development of general function - mark completed as done

#    Catch                   (done)  A.Fi.KPFM.Catch.01.R
#    SetFishingStrategy      (done)  A.Fi.KPFM.SetFishingStrategy.01.R
#    UpdateCatchRecord       (done)  A.Fi.KPFM.UpdateCatchRecord.01.R
#    SeasonAccounting        (done)  A.Fi.KPFM.SeasonAccounting.01.R
#    Trial_setup             (done)  A.Fi.KPFM.Time0.fn.01.R
#    TransitionSetup         (done)  A.Fi.KPFM.TransitionSetup.01.R

#
################################################################################
# start data set

Fishery 				<- list()

#-------------------------------------------------------------------------------
Fishery$signature		<- list(
	ClassName		= "Fishery",
	ID             	= 32001,
	Name.full       = "KPFM Krill fishery",
	Name.short      = "Fishery",
	Morph        	= "KPFM",
	Revision      	= "01",
	Authors      	= "A.Constable",
	Last.edit    	= "7 July 2008"
)
							
Fishery$polygonsN 		<- 15			# number of local populations
Fishery$Polygons        <- c(1:Fishery$polygonsN)
                           # reference numbers to polygons in the list of
                           # defined polygons

Fishery$Birthdate       <- list(Day = 1, Month = 12)
                           # day and month to be used as time 0 in the year
                           # for the taxon

Fishery$TargetN         <- 1 					# number of possible taxa caught in fishery (listed below)
#-------------------------------------------------------------------------------
Fishery$ScaleToTonnes   <- 1
  
#-------------------------------------------------------------------------------
Fishery$Init.Catch.SSMU 		<- matrix(rep(0,Fishery$polygonsN*Fishery$TargetN),ncol=Fishery$polygonsN,byrow=TRUE) # catch limits set to zero (NA would mean no fishing controls)

Fishery$Init.Efficiency.SSMU 	<- rep(list(matrix(rep(c(0,0),Fishery$polygonsN),ncol=2,byrow=TRUE)),Fishery$TargetN) # catch limits set to zero (NA would mean no fishing controls)
#-------------------------------------------------------------------------------
Fishery$Fishery.Elements  			<- matrix(c("Biota", "Krill",
											"Management", "SSMUtac"),ncol=2,byrow=TRUE)
Fishery$FishedTaxa        			<- matrix(c("Biota", "Krill"),ncol=2,byrow=TRUE)
Fishery$Year.Set.Fishery.Strategy 	<- 1952
Fishery$VesselsInFleet    			<- 1

Fishery$SpaceTimePattern  <- list( # for each polygon, relative fishing effort (accounting for distribution of effort
                        # across polygons over the year).  (to be turned into
                        # a matrix - polygons x periods of vessel effort
                           P01 = matrix(c(1,1,1),ncol=3,byrow=TRUE) # day, month, effort
                          ,P02 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P03 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P04 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P05 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P06 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P07 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P08 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P09 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P10 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P11 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P12 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P13 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P14 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
                          ,P15 = matrix(c(1,1,1),ncol=3,byrow=TRUE)
    )
	
Fishery$Catch.dset  <- list(
							 TargetTaxaRef = c(1)  # row refs to relatedElements matrix that contains target taxa
							,TargetTaxaRefN = 1
							,ManagerRef    = c(2)  # reference row to relatedElements matrix that contains the manager element
							,ManagerRefN = 1
							,Target.Taxa   = list( # note in order for the elements referred to in TargetTaxaRef
								Krill = list(
									  PerVessel           = 200  # maximum per vessel catch (number of individuals) per year
									 ,Holling_q            = 1
									 ,Holling_D            = 15
									 ,Holling_units        = 1
									 ,Holling_availability = c(1) # krill stage structure (like selectivity) used to calculate prey density for Holling equation (i.e. what predators can see)
									 ,Target_selectivity     = c(1)
									 ,TargetPn         = list( # for each polygon for fishery (relative reference) give proportion of target polygon in survey polygon
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
													 ) # end Pn list
								)
							)
						)
#-------------------------------------------------------------------------------
Fishery$Initialise      <- list(NULL)
Fishery$Transition.data <- list(NULL)
Fishery$PrintState      <- list(OutDir   = NULL, OutFiles = NULL)
Fishery$FunctionsList   <- list(Catch 				= list(actionMethod = "catch", 
																actionFile = file.path("code", "A.Fi.KPFM.Catch.01.R")),
									SetFishingStrategy 	= list(actionMethod = "setFishingStrategy", 
																actionFile = file.path("code", "A.Fi.KPFM.SetFishingStrategy.01.R")),
									UpdateCatchRecord 	= list(actionMethod = "updateCatchRecord", 
																actionFile = file.path("code", "A.Fi.KPFM.UpdateCatchRecord.01.R")),
									SeasonAccounting 	= list(actionMethod = "seasonAccounting", 
																actionFile = file.path("code", "A.Fi.KPFM.SeasonAccounting.01.R"))
								)
#-------------------------------------------------------------------------------
Fishery$OutputFiles     <- list(CatchRecord = "FisheryCatchRecords.dat")
	
#-------------------------------------------------------------------------------
Fishery$OutputFlags     <- list(Print_CatchRecord = TRUE)

	
Fishery$Functions 		<- list(

			# function to undertake element-specific setup of actions
			# (not including the generalised actions)
			# e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))
			setup = NULL,
			printState = list(actionMethod = NULL,
							  actionFile   = NULL,
                              dset = list(  # List because may need more than one file to print state
                                        Catch = list(output = Fishery$OutputFlags$Print_CatchRecord
                                                      ,fname  = Fishery$OutputFiles$CatchRecord
                                                      ,path   = NULL)
										)
            ),
			stateUpdate  = NULL   # no requirement to update as no mortality etc. only consumption
		)
		

		#   #############################################################
		#   Elmnt$TimeSteps
		#   #############################################################

		#   the characteristics of a time step between the previous time and the specified time (in days)
		#   is given in a list(days in calendar year, number of functions to be carried out, list of named functions)
		#   knife-edge functions can be included by repeating the same day
Fishery$timesteps 		<- list(
	FishingSeason = list(calday=dayFromDate(30,11), # note that the manager manages progress towards catch limits
		actionsN = NULL,  # will be updated below
		actions   = list(
			setFishingStrategy = list(actionMethod = Fishery$FunctionsList$SetFishingStrategy$actionMethod,
									actionFile = Fishery$FunctionsList$SetFishingStrategy$actionFile,
									tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "Before",    # "Before","During","After"
									transAction = NULL,
									relatedElements = Fishery$FishedTaxa,
									dset   = Fishery$SpaceTimePattern
			),

			seasonAccounting   = list(actionMethod = Fishery$FunctionsList$SeasonAccounting$actionMethod,
									actionFile = Fishery$FunctionsList$SeasonAccounting$actionFile,
									tsType = "FirstPeriod", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "Before",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = Fishery$SeasonAccounting$dset
			),

			catch     		   = list(actionMethod = Fishery$FunctionsList$Catch$actionMethod,
									actionFile = Fishery$FunctionsList$Catch$actionFile,
									tsType = "AllPeriods",  # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "During",    # "Before","During","After"
									transAction = NULL,
									relatedElements = Fishery$Fishery.Elements,
									dset   = Fishery$Catch.dset
			),

			updateCatchRecord   = list(actionMethod = Fishery$FunctionsList$UpdateCatchRecord$actionMethod,
									actionFile = Fishery$FunctionsList$UpdateCatchRecord$actionFile,
									tsType = "AllPeriods", # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
									tsTiming = "After",    # "Before","During","After"
									transAction = NULL,
									relatedElements = NULL,
									dset   = NULL
			)
		)
	)
)
	
# declare variable to be sourced	
Fishery