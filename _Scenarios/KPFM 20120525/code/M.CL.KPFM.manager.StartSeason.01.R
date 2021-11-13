#M.CL.KPFM.manager.StartSeason.01<-function(
if (!isGeneric("startSeason")) 
setGeneric("startSeason", function(element, universe) standardGeneric("startSeason"))
setMethod("startSeason", signature(element="Manager", universe="Universe"), 
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           M.CL.KPFM.manager.StartSeason.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         7 July 2008
		# Description:        Management - SSMU TAC setting process - surveys of nominated SSMUs
		
		# Update state
		setState(element, "SSMU.CatchRemaining", value=getState(element, "SSMU.TAC"))
	}
)