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
		Polygons      = file.path("data", "KPFM.Spatial.data.R"),
		Trials        = file.path("data", "KPFM.Trial.data.R")
	),
	Environment = list(
		 # ID = 12001
		KrillEnv = list(className = "KrillEnvironment",
						classFile = file.path("code", "E.O.Env.KPFM.01.R"),
						classData = file.path("data", "KPFM.E.O.Env.data.01.R"))
	),
	Biota = list(
		 # ID = 22001
		Krill = list(className = "Krill",
					 classFile = file.path("code", "B.MI.Es.KPFM.01.R"),
					 classData = file.path("data", "KPFM.B.MI.Es.data.01.R")),
		 # ID = 23001
		PenguinsA3 = list(className = "Predator",
						  classFile = file.path("code", "B.Pr.KPFM.01.R"),
						  classData = file.path("data", "KPFM.B.Pr.01.data.PenguinRecAge3.01.R")),
		 # ID = 23002
		PenguinsA4 = list(className = "Predator",
						  classFile = file.path("code", "B.Pr.KPFM.01.R"),
						  classData = file.path("data", "KPFM.B.Pr.01.data.PenguinRecAge4.01.R")),
		 # ID = 23003
		PenguinsA5 = list(className = "Predator",
						  classFile = file.path("code", "B.Pr.KPFM.01.R"),
						  classData = file.path("data", "KPFM.B.Pr.01.data.PenguinRecAge5.01.R")),
		 # ID = 23004
		Seals    = list(className = "Predator",
						classFile = file.path("code", "B.Pr.KPFM.01.R"),
						classData = file.path("data", "KPFM.B.Pr.01.data.Seal.01.R")),
		 # ID = 23005
		FishA2F1 = list(className = "Predator",
						classFile = file.path("code", "B.Pr.KPFM.01.R"),
						classData = file.path("data", "KPFM.B.Pr.01.data.FishA2F1.01.R")),
		 # ID = 23006
		FishA2F2 = list(className = "Predator",
						classFile = file.path("code", "B.Pr.KPFM.01.R"),
						classData = file.path("data", "KPFM.B.Pr.01.data.FishA2F2.01.R")),
		 # ID = 23007
		FishA2F3 = list(className = "Predator",
						classFile = file.path("code", "B.Pr.KPFM.01.R"),
						classData = file.path("data", "KPFM.B.Pr.01.data.FishA2F3.01.R")),
		 # ID = 23008
		FishA3F1 = list(className = "Predator",
						classFile = file.path("code", "B.Pr.KPFM.01.R"),
						classData = file.path("data", "KPFM.B.Pr.01.data.FishA3F1.01.R")),
		 # ID = 23009
		Whales   = list(className = "Predator",
						classFile = file.path("code", "B.Pr.KPFM.01.R"),
						classData = file.path("data", "KPFM.B.Pr.01.data.Whale.01.R"))
	),
	Activity = list( # ID 30000
		 # ID = 32001
		KrillFishery = list(className = "Fishery",
							classFile = file.path("code", "A.Fi.KPFM.01.R"),
							classData = file.path("data", "KPFM.A.Fi.01.data.01.R"))
	),
	Management = list( # ID 40000
		# ID = 42001
		SSMUtac = list(className = "Manager",
					   classFile = file.path("code", "M.CL.KPFM.manager.01.R"),
					   classData = file.path("data", "KPFM.M.CL.manager.01.data.01.R"))
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