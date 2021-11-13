# Input data - Universe.R
# Universe

Universe <- list()

	# Universe Signature
Universe$signature			<- list(
	ClassName           = "Universe",
	ID           		= 11003,
	Name.full           = "Environment General - trial projections",
	Name.short          = "EnvGenTrial01",
	Revision            = "01",
	Authors             = "A.Constable, T.Robertson",
	Last.edit           = "26 May 2015"         #"25 Feb 2008"
)

	# Module and Element class and data inclusions/paths
Universe$inputPaths 		<- list(
	Config = list(  # ID 11000
		Polygons      = file.path("data", "Spatial.data.R"),
		Scenarios     = file.path("data", "Scenarios.data.R")
	),
	Environment = list(
		 # ID = 12001
		EV01 = list(className = "EnvGenVar",
						classFile = file.path("code", "E.O.Env.EnvGenVar.01.R"),
						classData = file.path("data", "E.O.Env.EnvGenVar.data.01.R"))
	),
	Biota = list(
    PPfood = list(className = "PPgenBMS",
						classFile = file.path("code", "B.PP.GenBms.01.R"),
						classData = file.path("data", "B.PP.GenBms.data.01.R"))
	),
	Activity = list(
  NULL
  ),
	Management = list(
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