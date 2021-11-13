# Input data - E.O.Env.Gen.data.01.R
# GenEnv

# routine to enable processing of input data if needed but without function
Temp.Polygon.N<-1

Env_Gen 				<- list()
Env_Gen$signature 		<- list(
	ClassName	 = "Environment_General",
	ID           = 12001,
	Name.full    = "Environment General Variable",
	Name.short   = "Env_gen",
	Morph        = "Env",
	Revision      = "01",
	Authors      = "A.Constable",
	Last.edit    = "28 Jan 2015"
)
# Polygon reference - which polygons are to be used out of the list of defined polygons
Env_Gen$polygonsN		<- Temp.Polygon.N
Env_Gen$polygons       <- as.vector(c(1:Temp.Polygon.N))

	# Birthdate - specify day and month to be used as time 0 in the year for the variable
Env_Gen$birthdate      <- list(day = 1, month = 6)

	# Krill reproductive environments
Env_Gen$RuntimeFile      	<- "Env_gen_01.dat"
Env_Gen$ReplaceFile        <- TRUE
Env_Gen$EnvParams  <- list(
							   Year0 = 1900
							  ,a = list(slope = 0.0, int = 1.0)
							  ,p = list(slope = 0.0, int = 1.0)
							  ,f = list(slope = 0.0, int = 1.0)
							  ,Xmin = list(slope = 0.0, int = 1)
)
Env_Gen$Region_Variation   <- list(Seed = 2345, UseRandSeq = TRUE, CV = 1.0)
Env_Gen$Polygon_Variation  <- list(Seed = 5000, UseRandSeq = TRUE
							  ,ScaleCoeff = rep(1,Temp.Polygon.N)
							  ,Var = rep(0,Temp.Polygon.N)
							  ,CorrMat = matrix(rep(1,(Temp.Polygon.N*Temp.Polygon.N)),ncol=Temp.Polygon.N)
)
Env_Gen$Initialise         <- list(Records = 1)
Env_Gen$FunctionsList      <- list(

                                  )
       ,Initialise          = list(Records = 1)
       ,Functions           = list(RegionalSeries  = paste(RootPath,"E.O.Env.Gen.regional.series.01.R",sep="")
                                  ,TrialSetup      = paste(RootPath,"E.O.Env.Gen.Time0.fn.01.R",sep="")
                                  ,StatePrint      = paste(RootPath,"E.O.Env.Gen.printState.01.R",sep="")
                                  ,UpdateAnnualEnv = paste(RootPath,"E.O.Env.Gen.annual_update.01.R",sep="")
                                   )
       ,OutputFiles         = list(State = "EnvState.dat")
       ,OutputFlags         = list(PrintState = TRUE)

) # end list EnvAttributes



