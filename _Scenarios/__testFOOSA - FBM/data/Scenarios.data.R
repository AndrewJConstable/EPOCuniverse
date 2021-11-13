#EC.scenario.KPFM.01<-function (Config,InputData)

# Function:           KPFM.scenario.08.01
# Description:        Environment of parameters for a scenario
# Primary attributes:
#

# Input parameters:

# Returned            list of universe

########################################################
#      Signature <- list(
#        ID           =  12002,
#        Name.full    = "KPFM Scenario.parameters 0801",
#        Name.short   = "KPFM Scenario.par08",
#        Version      = "01",
#        Authors      = "A.Constable",
#        last.edit    = "25 Feb 2008"
#        ) # end Signature


########################################################

AllScenarios <- list(
	
	# default list of input data for scenarios
	scenarioN 			= 1,
    yearStart 			= 1950, # first year of simulation - Year 0 is year before
    yearEnd   			= 1980, # last year of simulation
    firstFishingYear 	= 1960,
    lastFishingYear 	= 1965,
    scenarioDir  		= file.path(getwd(), "runtime"),
	replicateCnt		= 10
)	

# specific details for each unique scenario
Scenarios <- list(
	"Scenario.par" = list(
		#   Signature is a unique identifier for this element
		signature = list(
			ClassName	= "Scenario",
			ID          = 12002,
			Name.full   = "KPFM Scenario parameters 0801",
			Name.short  = "KPFM.Scenario01",
			Revision    = "01",
			Authors     = "A.Constable",
			Last.edit  	= "25 Feb 2008"
		),
		
		yearStart			= AllScenarios$yearStart,
		yearEnd				= AllScenarios$yearEnd,
		yearsN				= AllScenarios$yearEnd-AllScenarios$yearStart+1,
		firstFishingYear 	= AllScenarios$firstFishingYear,
		lastFishingYear 	= AllScenarios$lastFishingYear,
		scenarioDir			= AllScenarios$scenarioDir,
		replicateCnt		= AllScenarios$replicateCnt
	)#,
	# # Add any successive trials after this first item
	# "Scenario.par2" = list(
		# #   Signature is a unique identifier for this element
		# signature = list(
			# ClassName	= "Scenario",
			# ID          = 12003,
			# Name.full   = "KPFM Scenario parameters 0802",
			# Name.short  = "KPFM.Scenario02",
			# Revision    = "01",
			# Authors     = "A.Constable",
			# Last.edit  	= "25 Feb 2008"
		# ),
		
		# yearStart			= 1955,
		# yearEnd				= 1958,
		# yearsN				= 4,
		# firstFishingYear 	= 1956,
		# lastFishingYear 	= 1957,
		# scenarioDir			= file.path(AllScenario$scenarioDir, "Scenario2")
	# )
)

