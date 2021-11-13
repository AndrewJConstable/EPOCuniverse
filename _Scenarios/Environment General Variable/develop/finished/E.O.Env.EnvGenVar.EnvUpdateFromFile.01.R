# Create S4 method 'EnvUpdateFromFile'
#E.O.Env.EnvGenVar.EnvUpdateFromFile.01<-function(
if (!isGeneric("EnvUpdateFromFile"))
setGeneric("EnvUpdateFromFile", function(element, universe) standardGeneric("EnvUpdateFromFile"))
setMethod("EnvUpdateFromFile", signature(element="EnvGenVar", universe="Universe"),
    function(
		element,
		universe               # access to universe if needed
		)
	{

# Troy's code
#		rYear <- getRTState(universe, "relativeYear")
#		#read next line of file
#		fileName <- getRuntimePath(universe, getAttribute(element, "RuntimeFile"))
#		fileConn <- getFileConnection(element, "RuntimeFile", fileName, "r")
#		envData <- fromCSVCharacter(readFileConnection(element, conn=fileConn, linenum=10+rYear), type="double")

		fileConn <- getAttribute(element, "RuntimeFileConn")
		envData <- fromCSVCharacter(readFileConnection(element, conn=fileConn, linenum=0), type="double")

# the first element of envData is the trial number and the second element is the time of the year.  Check to see the trial and year is the same as in current trial
# if the number of timesteps in the variable is not correct then the years will become offset

		if(floor(envData[1]) != getRTState(universe, "currentTrial") || envData[2] != getRTState(universe, "currentYear")) {
			epocErrorMessage(element, "Environment data in file is being read out of sync with run time scenario-year combination", halt=TRUE)
		}
		
		setState(element, "PolygonEnv", envData[getSlot(element, "recordElements")])
	}
)

