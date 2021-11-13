# Input data - Universe.R
# Universe

Universe <- list()

	# Universe Signature
Universe$signature			<- list(
	ClassName           = "Universe",
	ID           		= 11003,
	Name.full           = "KPFM Main 0801",
	Name.short          = "KPFM_08",
	Revision            = "01",
	Authors             = "A.Constable, T.Robertson",
	Last.edit           = "28 May 2009"         #"25 Feb 2008"
)
    
	# Module and Element class and data inclusions/paths
Universe$inputPaths 		<- list(
	Config = list(  # ID 11000
		Polygons      = file.path("data", "Spatial.data.R"),
		Scenarios     = file.path("data", "Scenarios.data.R")
	),
	Environment = list(
		 # ID = 12001
		KrillEnv = list(className = "EnvGenVar",
						classFile = file.path("code", "E.O.Env.EnvGenVar.01.R"),
						classData = file.path("data", "E.O.Env.EnvGenVar.data.01.R"))
	),
	Biota = list(
		 # ID = 22001
		Krill = list(className = "Krill",
					 classFile = file.path("code", "B.MI.Es.KPFM.01.R"),
					 classData = file.path("data", "B.MI.Es.KPFM.data.01.R"))
	),
	Activity = list( # ID 30000
		 # ID = 32001
		NULL
	),
	Management = list( # ID 40000
		# ID = 42001
		NULL
	),
	Output = list(  # ID 50000
					NULL
	),
	Presentation = list(  # ID
					NULL
	),
	Controller = list(
					NULL
	)
)
	
# Reporting parameters
Universe$report 		<- list(
	Diagnostics = list(
					Output = list(Level = "normal"),
					Log = list(Level = "verbose", Filename = "EPOC.log", Truncate = TRUE),
					Calendar = list(ToFile = TRUE, Filename = "Calendar.txt"),
					Debug = list()
				),
	HeadingLines = list(
					Heading1 = "##################################################################",
					Heading2 = "------------------------------------------------------------------",
					Heading3 = "+++++++++++++++++++++++++++++++++++++",
					Heading4 = "     ................................"
				)
)
	
# Any global (Universal) Attributes
# Universe$monthDays = c(31,28,31,30,31,30,31,31,30,31,30,31)

Universe