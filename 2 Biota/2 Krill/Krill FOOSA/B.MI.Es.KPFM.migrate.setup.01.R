# Create S4 method 'migrate.setup'
if (!isGeneric("migrateSetup")) 
setGeneric("migrateSetup", function(element, period="Period", ptSA="list", modulenum="numeric", elementnum="numeric", 
										firstPeriod="numeric", dset="list") standardGeneric("migrateSetup"))
setMethod("migrateSetup", "Krill", 
    function(
        element,			# element environment
		period,				# period
		ptSA ,           	# untransformed action for the period derived from timestep of element
							# Note: PtSA is a list retained for concatenating to a list.
							#       Therefore, the action is the first element in the list         
		modulenum,         	# reference module number for the element
		elementnum,        	# relative number of the element in the universe
		firstPeriod,    	# logical indicating if this is the first period in the timestep
		dset            	# dataset to assist with transformation
		)

    # Function:           B.MI.Es.KPFM.migrate.setup.01
    # Version             0.01
    # Description:        Transform migration matrix for timestep into matrix of values for use in migrating Krill
    # Primary attributes: 
                          # 1. multiply matrix by (duration in year of period)/(duration of timestep)
                          # 2. initial calculations to speed up simulation
                          #      for each origin polygon (rows) determine fraction of
                          #      stage class abundance going to destination cells using Baranov equation
                          # 3. return new matrix
    { 
    
		# 1. multiply matrix by (duration in year of period)/(duration of timestep)
		#      fraction of timestep of element accounted by period is in calendar under
		#      the period in a list of matrices for each module.
		#      In the matrix - rows = element (by relative number) & cols =
		#           col 1 = timestep for element that is occurring in the calendar period
		#           col 2 = the proportion of the timestep that will be accounted for by the period
		#           col 3 = the number (in sequence) of the period in the timestep
        adj <- getPeriodElementTSData(period, modulenum, elementnum)[2]
		
        ptSA[[1]]$dset <- ptSA[[1]]$dset * adj
    
        # 2. for each origin polygon (rows) determine fraction of
        #    stage class abundance going to destination cells using Baranov equation
        for (pn in 1:getSlot(element, "polygonsN")){
            TotalRate <- sum(ptSA[[1]]$dset[pn,])
            if(TotalRate>0) ptSA[[1]]$dset[pn,] <- ptSA[[1]]$dset[pn,] / TotalRate*(1-exp(-TotalRate))
        }

        # 3. return transformed action
        return(ptSA)
    }
)
