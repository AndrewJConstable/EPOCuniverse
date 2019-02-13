# Create S4 method 'mortalitySetup'
if (!isGeneric("mortalitySetup")) 
setGeneric("mortalitySetup", function(element, period="Period", ptSA="list", modulenum="numeric", elementnum="numeric", 
										firstPeriod="numeric", dset="list") standardGeneric("mortalitySetup"))
setMethod("mortalitySetup", "Predator", 
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
		# Function:           B.Pr.KPFM.Mortality.setup.01
		# Version             0.01
		# Description:        transform mortality values for timestep to period
		# Primary attributes: 
						
		# 1. multiply matrix by (duration in year of period)/(duration of timestep)
		#      fraction of timestep of element accounted by period is in calendar under
		#      the period in a list of matrices for each module.
		#      In the matrix - rows = element (by relative number) & cols =
		#           col 1 = timestep for element that is occurring in the calendar period
		#           col 2 = the proportion of the timestep that will be accounted for by the period
		#           col 3 = the number (in sequence) of the period in the timestep
		adj <- getPeriodElementTSData(period, modulenum, elementnum)[2]
		for (st in 1:getState(element, "StageN")) ptSA[[1]]$dset[[st]]$M <- ptSA[[1]]$dset[[st]]$M * adj

		# 2. return transformed mortalities
		return(ptSA)
	}
)

