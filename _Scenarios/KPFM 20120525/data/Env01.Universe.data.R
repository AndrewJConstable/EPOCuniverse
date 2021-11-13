# Input data - Universe.R
# Universe

Universe <- list()

	# Universe Signature
Universe$signature			<- list(
	ClassName           = "Universe",
	ID           		= 11003,
	Name.full           = "Environment General variable",
	Name.short          = "Env_gen",
	Revision            = "01",
	Authors             = "A.Constable",
	Last.edit           = "28 Jan 2015"
)
    
	# Module and Element class and data inclusions/paths
Universe$inputPaths 		<- list(
	Config = list(
		            Polygons      = file.path("C:/_work/Modelling/Platforms/EPOC/Universe/07 Spatial domains"
                              ,"Env_gen.Spatial.data.R")
	              ,Trials        = file.path("C:/_work/Modelling/Platforms/EPOC/Universe/07 Spatial domains"
                              ,"Env_gen.Trial.data.R")
	             )
,Environment = list(
		 # ID = 12001
		EnvGen = list(className = "Environment_General",
						classFile = file.path("code", "E.O.Env.Gen.01.R"),
						classData = file.path("data", "E.O.Env.Gen.data.01.R"))
	)
 ,Biota      = list(
					          NULL
	                  )
 ,Activity   = list(
					          NULL
	                  )
 ,Management = list(
					          NULL
	                  )
 ,Output     = list(
					          NULL
	                  )
 ,Presentation = list(
					          NULL
	                  )
 ,Controller = list(
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