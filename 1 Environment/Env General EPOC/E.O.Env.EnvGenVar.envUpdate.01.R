# Create S4 method 'envUpdate'
#E.O.Env.EnvGenVar.annual_update.01<-function(
if (!isGeneric("envUpdate"))
setGeneric("envUpdate", function(element, universe) standardGeneric("envUpdate"))
setMethod("envUpdate", signature(element="EnvGenVar", universe="Universe"),
    function(
		element,
		universe               # access to universe if needed
		)
	{

# read one line and leave file connection open

		fileConn <- getFileConnection(element, "RuntimeFile")

		envData <- fromCSVCharacter(readFileConnection(element, conn=fileConn, linenum=0), type="double")

		if(envData[1] != getRTState(universe, "currentReplicate") || envData[2] != getRTState(universe, "currentYear")) {
			epocErrorMessage(element, "Environment data in file is being read out of sync with run time scenario-year combination", halt=TRUE)
		}
		
		setState(element, "PolygonEnv", envData[getSlot(element, "recordElements")])
	}
)

