#M.CL.KPFM.manager.EndSeasonReport.01<-function(
if (!isGeneric("endSeasonReport")) 
setGeneric("endSeasonReport", function(element, universe) standardGeneric("endSeasonReport"))
setMethod("endSeasonReport", signature(element="Manager", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           M.CL.KPFM.manager.EndSeasonReport.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         7 July 2008
		# Description:        Management - SSMU TAC setting process - surveys of nominated SSMUs
		# Primary attributes: 
		# Return data:        
		# History:

		#-------------------------------------------------------------------------------
		# dependent elements

		#-------------------------------------------------------------------------------
		# data set (dSet) requirements

		#-------------------------------------------------------------------------------
		# Instructions for use
		#
		#     Note - one survey per year
		#
		################################################################################

#		Elmnt    <-Universe[[Action[1]]][[Action[2]]]
#		dSet     <-Elmnt$TimeSteps[[Action[3]]]$actions[[Action[5]]]$dset
	}
)