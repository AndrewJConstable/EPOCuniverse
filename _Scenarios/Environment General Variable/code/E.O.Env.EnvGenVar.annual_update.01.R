# Create S4 method 'annualUpdate'
#E.O.Env.EnvGenVar.annual_update.01<-function(
if (!isGeneric("annualUpdate")) 
setGeneric("annualUpdate", function(element, universe) standardGeneric("annualUpdate"))
setMethod("annualUpdate", signature(element="EnvGenVar", universe="Universe"),
    function(
		element,
		universe               # access to universe if needed
		)
	{

		rYear <- getRTState(universe, "relativeYear")
		#read next line of file
		fileName <- getRuntimePath(universe, getAttribute(element, "RuntimeFile"))
		fileConn <- getFileConnection(element, "RuntimeFile", fileName, "r")
		envData <- fromCSVCharacter(readFileConnection(element, conn=fileConn, linenum=10+rYear), type="double")
		if(envData[1] != getRTState(universe, "currentScenario") || envData[2] != getRTState(universe, "currentYear")) {
			epocErrorMessage(element, "Environment data in file is being read out of sync with run time scenario-year combination", halt=TRUE)
		}
		
		setState(element, "PolygonEnv", envData[getSlot(element, "recordElements")])
	}
)

