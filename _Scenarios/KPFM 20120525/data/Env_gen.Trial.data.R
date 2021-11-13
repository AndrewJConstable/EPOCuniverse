#EC.trial.KPFM.01<-function (Config,InputData)

# Function:           KPFM.trial.08.01
# Description:        Environment of parameters for a trial
# Primary attributes:
#

# Input parameters:

# Returned            list of universe

########################################################
#      Signature <- list(
#        ID           =  12002,
#        Name.full    = "Environmental General",
#        Name.short   = "Env_gen",
#        Version      = "01",
#        Authors      = "A.Constable",
#        last.edit    = "28 Jan 2015"
#        ) # end Signature


########################################################

AllTrials <- list(
	
	# default list of input data for trials
	trialsN 			= 1,
    yearStart 			= 1950, # first year of simulation - Year 0 is year before
    yearEnd   			= 1960, # last year of simulation
    firstFishingYear 	= 1952,
    lastFishingYear 	= 1956,
    trialDir  			= file.path(getwd(), "runtime")
)	

# specific details for each unique trial
Trials <- list(
	"Trial.par" = list(
		#   Signature is a unique identifier for this element
		signature = list(
			ClassName	= "Trial",
			ID          = 12002,
			Name.full   = "Environmental General Variable",
			Name.short  = "Env_gen",
			Revision    = "01",
			Authors     = "A.Constable",
			Last.edit  	= "28 Jan 2015"
		),
		
		yearStart			= AllTrials$yearStart,
		yearEnd				= AllTrials$yearEnd,
		yearsN				= AllTrials$yearEnd-AllTrials$yearStart+1,
		firstFishingYear 	= AllTrials$firstFishingYear,
		lastFishingYear 	= AllTrials$lastFishingYear,

		trialDir			= AllTrials$trialDir
	)#,
	# # Add any successive trials after this first item
	# "Trial.par2" = list(
		# #   Signature is a unique identifier for this element
		# signature = list(
			# ClassName	= "Trial",
			# ID          = 12003,
			# Name.full   = "KPFM Trial parameters 0802",
			# Name.short  = "KPFM.Trial02",
			# Revision    = "01",
			# Authors     = "A.Constable",
			# Last.edit  	= "25 Feb 2008"
		# ),
		
		# yearStart			= 1955,
		# yearEnd				= 1958,
		# yearsN				= 4,
		# firstFishingYear 	= 1956,
		# lastFishingYear 	= 1957,
		# trialDir			= file.path(AllTrials$trialDir, "Trial2")
	# )
)

