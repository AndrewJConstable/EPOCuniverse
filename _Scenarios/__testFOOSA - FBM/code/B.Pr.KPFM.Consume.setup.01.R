# Create S4 method 'consumeSetup'
if (!isGeneric("consumeSetup")) 
setGeneric("consumeSetup", function(element, period="Period", ptSA="list", modulenum="numeric", elementnum="numeric", 
										firstPeriod="numeric", dset="list") standardGeneric("consumeSetup"))
setMethod("consumeSetup", "Predator", 
    function(
		element,			# element environment
		period,				# period
		ptSA,            	# untransformed action for the period derived from timestep of element
							# Note: PtSA is a list retained for concatenating to a list.
							#       Therefore, the action is the first element in the list         
        modulenum,          # reference module number for the element
        elementnum,         # relative number of the element in the universe
        firstPeriod,     	# logical indicating if this is the first period in the timestep
        dset            	# dataset to assist with transformation
		)
	{
		# Function:           B.Pr.KPFM.Consume.setup.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         4 July 2008
		# Description:        General predator function -

		# 1. multiply per capita feeding rate by (duration in year of period)/(duration of timestep)
		#      fraction of timestep of element accounted by period is in calendar under
		#      the period in a list of matrices for each module.
		#      In the matrix - rows = element (by relative number) & cols =
		#           col 1 = timestep for element that is occurring in the calendar period
		#           col 2 = the proportion of the timestep that will be accounted for by the period
		#           col 3 = the number (in sequence) of the period in the timestep
		adj <- getPeriodElementTSData(period, modulenum, elementnum)[2]
		
		for (st in 1:getState(element, "StageN")) {
			if (!is.null(ptSA[[1]]$dset[[st]])){
				for (prey in 1:nrow(ptSA[[1]]$relatedElements)){
					if (!is.null(ptSA[[1]]$dset[[st]]$Prey[[prey]][[1]])) {
						ptSA[[1]]$dset[[st]]$Prey[[prey]][[1]] <- ptSA[[1]]$dset[[st]]$Prey[[prey]][[1]] * adj
					}
				}
			}
		}

		return(ptSA)
	} 
)
###############################################################################
###############################################################################
# test routines
