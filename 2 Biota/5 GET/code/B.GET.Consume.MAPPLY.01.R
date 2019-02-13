#B.GET.Consume.01<-function(
if (!isGeneric("consume")) 
setGeneric("consume", function(element, universe) standardGeneric("consume"))
setMethod("consume", signature(element="GET", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           B.Pr.KPFM.Consume.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         3 July 2008

		# Description:        General consumer function -

		# Primary attributes: 

		# Return data:        

		# History:

		#-------------------------------------------------------------------------------
		# dependent elements

		#-------------------------------------------------------------------------------
		# data set (dSet) requirements
		#   Level 1 - lists of data for each life stage - if not feeding in life stage then NULL
		#   Level 2 - for each life stage
		#       feeding.Polygon.N    - number of feeding polygons for this life stage
		#       PropFeedInPolygon - proportion effort of local population feeding in feeding polygon
		#                         - matrix : rows = local populations; cols = feeding polygons
		#       Prey - list - one record for each prey item in order of "relatedElements"
		#         for each prey: (list)
		#           PerCapitaKrill = maximum per capita consumption of krill
		#           PropInDiet     = proportion of prey in diet
		#           Holling_q
		#           Holling_D
		#           Holling_units  - 1 = number, 2 = biomass
		#           Holling_availability = prey stage structure (like selectivity) used to calculate prey density for Holling equation (i.e. what consumers can see)
		#           Prey_selectivity  = at present, vector of selectivity for each prey stage class
		#           PreyPn = list : for each consumer feeding polygon,
		#                    list
		#                     1. Pns - matrix of col 1 = prey polygons (relative reference)
		#                                  col 2 = prop of prey polygon in feeding polygon
		#                     2. PnN - number of prey polygons for that consumer polygon

		# Get a handle on some necessary universe and element state data
		action <- getRTState(universe, "currentAction")
		# ActionMat row
		# Col  1  = module
		# Col  2  = element
		# Col  3  = period
		# Col  4  = reference day in year
		# Col  5  = action reference number in period (NA if no actions)
		# Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions)
		periodInfo <- getRTState(universe, "currentPeriodInfo")
		# periodInfo  # information about the active period for use in subroutines
		# Number      = eTSD
		# Day         = PropYear[eTSD,1]
		# KnifeEdge   = if(PropYear[eTSD,2]==0) FALSE else TRUE
		# YearPropn   = PropYear[eTSD,3]
		# PeriodStart = PreviousDay/365 # proportion of year passed since 0 Jan
		#                               # to beginning of time period
		# PeriodEnd   = PreviousDay/365+PropYear[eTSD,3]
		elemState <- getState(element)
		elemTrans <- getTransition(element)
		actionTS <- getTimestep(element, action[[3]])$actions[[action[5]]]
		dSet <- actionTS$dset
		consumption <- NULL
		
		for (pop in 1:getSlot(element, "polygonsN")) { 
		
stgPabund <- elemState$Abundance[[elemState$StageStrUnits]][pop] * elemState$Stage[,pop]
		
			for (st in 1:elemState$StageN) {              # for each consumer stage class

				# proportion of effort in feeding polygons by stage class
				prop <- dSet[[st]]$PropFeedInPolygon[pop,]
        
				if(!is.null(prop)) { # for when stage does not consume prey
    
					# abundance of stage class
#					Pabund <- elemState$Abundance[[elemState$StageStrUnits]][pop] * elemState$Stage[[pop]][st,2]
				
					# vector of products for use below
					#propPabund <- prop * Pabund
					propPabund <- prop * stgPabund[st]
					
					# loops to establish consumption.
					# rows (fpn*prey*preyPN*preySt) will be concatenated and turned into a matrix
					# prior to adding to transition matrices

					Result <- NULL
					for (fpn in 1:dSet[[st]]$feeding.Polygon.N) { # loop through feeding polygons and consume prey

						# identify which ones can be skipped
						if (propPabund[fpn] > 0) {
     
							for (prey in 1:actionTS$relatedIndexes.N) {
								preyRE <- actionTS$relatedIndexes[prey,]
								preyElem <- universe$modules[[preyRE[1]]][[preyRE[2]]]
								
								for (preyPN in 1:dSet[[st]]$Prey[[prey]]$PreyPn[[fpn]]$PnN) {

									preyElemState <- getState(preyElem)
									
									# determine the relative reference in prey element for the prey polygon
									preyPolygon <- dSet[[st]]$Prey[[prey]]$PreyPn[[fpn]]$Pns[preyPN,1]
									preyPolygon <- which(getPolygons(preyElem) == preyPolygon)

									# Holling_availability is used to establish density of available prey
									if (dSet[[st]]$Prey[[prey]]$Holling_units == preyElemState$StageStrUnits) {
										AvailablePreyStr <- dSet[[st]]$Prey[[prey]]$Holling_availability *
											preyElemState$Abundance[[dSet[[st]]$Prey[[prey]]$Holling_units]][preyPolygon] *
											preyElemState$Stage[,preyPolygon]
										AvailablePreyTotal <- sum(AvailablePreyStr)
									} else {
										epocErrorMessage(element, "Holling units not the same as prey stage structure units - must change", halt=TRUE)
									}

									# establish Holling value according to available prey
									PreyPNarea <- getSpatial(universe, "polygonAreas")[getPolygons(preyElem,)[preyPolygon]]
									PreyDensity <- AvailablePreyTotal/PreyPNarea

									Holling <- (PreyDensity^(dSet[[st]]$Prey[[prey]]$Holling_q+1))/(dSet[[st]]$Prey[[prey]]$Holling_D+(PreyDensity^(dSet[[st]]$Prey[[prey]]$Holling_q+1)))

									# determine prey consumed by prey stages
									PNconsumeMax <- propPabund[fpn]*  # Stage abundance * proportion of effort in this feeding area
												   dSet[[st]]$Prey[[prey]]$PerCapita* # max per capita consumption
												   dSet[[st]]$Prey[[prey]]$PreyPn[[fpn]]$Pns[preyPN,2] # proportion of prey polygon in consumer polygon

									# protect from divide by 0
									if (AvailablePreyTotal > 0) AvailablePreyStr <- AvailablePreyStr / AvailablePreyTotal

									PNconsumeMax <- PNconsumeMax * AvailablePreyStr
									PNconsumed <- PNconsumeMax * Holling

									# concatenate results for use later
									for (pst in 1:preyElemState$StageN) {
										Result <- c(Result,
													  prey            # 1. number of prey in relatedElements
													 ,preyPolygon     # 2. prey polygon
													 ,pst             # 3. prey stage
													 ,preyElemState$StageStrUnits # 4. stage units
													 ,PNconsumed[pst] # 5. stage quantity
													 ,PNconsumeMax[pst] # 6. maximum stage quantity possible
														   )
									}
								}
							}
						}
					}

					# add to consumption in consumption of consumer
					# only if there are results
					if (!is.null(Result)) {

						Result <- matrix(Result,ncol=6,byrow=TRUE)
#						ResCol1 <- unique(Result[,1])					# Prey count
#						ResCol2 <- unique(Result[,2])					# Prey polygons
#						ResCol3 <- unique(Result[,3])					# Prey stage

eg <- expand.grid(unique(Result[,3]), unique(Result[,2]), unique(Result[,1]))
eg <- matrix(c(eg[,1], eg[,2], eg[,3]), ncol=3)

						#      ResUnique<-matrix(unique(Result[,c(1:4)]),ncol=4)
						#      for (r in 1:nrow(ResUnique)){
#						for (r1 in 1:length(ResCol1)) {
#							for (r2 in 1:length(ResCol2)) {
#								for (r3 in 1:length(ResCol3)) {
consumed <- mapply(FUN=function(preyCnt, preyPoly, preyStg, Result, universe, actionTS, action, periodInfo, pop, st) {				  
#									SumRef <- (Result[,1] == ResCol1[r1] &
#											   Result[,2] == ResCol2[r2] &
#											   Result[,3] == ResCol3[r3])
#print(SumRef)
									SumRef <- (Result[,1] == preyCnt &
											   Result[,2] == preyPoly &
											   Result[,3] == preyStg)
									Sum <- sum(Result[SumRef,5])
									SumMax <- sum(Result[SumRef,6])
									Units <- ifelse(SumMax > 0, mean(Result[SumRef,4]), NA)
									if (SumMax > 0) {
										preyRE <- actionTS$relatedIndexes[preyCnt,]
										
										# Update prey transition data
										preyREElement <- getEPOCElement(universe, preyRE[1], preyRE[2])
										mortality <- getTransition(preyREElement, "Mortality")
										mortality <- rbind(
											mortality,
											c(
											   preyPoly        #    1.   1.  prey subject polygon (relative in element)
											  ,Units              #    2.   2.  units of qnty
											  ,preyStg        #    3.   3.  stage
											  ,Sum                #    4.   4.  stage quantity
											  ,getState(preyREElement, "Cond.S")[preyStg,preyPoly]      #    5.   5.  stage size
											  ,action[1]          #    6.   6.  mortality source module
											  ,action[2]          #    7.   7.  mortality source element
											  ,pop                #    8.   8.  mortality source local population (relative in element)
											  ,st                 #    9.   8.a mortality source stage
											))
										setTransition(preyREElement, "Mortality", value=mortality)
										doUpdate(preyREElement, TRUE)
										
										# Update consumer consumption data
#										elemTrans$Consumption <- rbind(
#											elemTrans$Consumption, c(
return(c(
																  # column from above
											   pop                # mortality source local population (relative in element)
											  ,st                 # mortality source stage
                        ,stgPabund[         # abundance of source stage to determine per capita consumption
											  ,preyRE[1]          # prey module
											  ,preyRE[2]          # prey element
											  ,preyPoly     	    # prey subject polygon (relative in element)
											  ,Units     		      # units of qnty
											  ,preyStg            # stage
											  ,Sum                # stage realised quantity (after competition adjustment in updata_state of prey)
											  ,SumMax             # stage maximum quantity (on first calculation of consumption)
											  ,getState(preyREElement, "Cond.S")[preyStg,preyPoly]      # stage size
											  ,getRTState(universe, "currentYear")        # calendar year
											  ,getRTState(universe, "currentPeriod")      # period
											  ,periodInfo$YearPropn                       # proportion of year
											))
									}
#								}
#							}
#						}
		}, eg[,1], eg[,2], eg[,3], MoreArgs=list(Result=Result, universe=universe, actionTS=actionTS, action=action, periodInfo=periodInfo, pop=pop, st=st))
consumed <- matrix(unlist(consumed), ncol=13, byrow=T)
consumption <- rbind(consumption, consumed)	
					}
				}
			}
		}

		# Update universe element transition consumption data
		setTransition(element, "Consumption", value=rbind(getTransition(element, "Consumption"), consumption))
	}
)
#-------------------------------------------------------------------------------
# save data in transition matrices

    # Transition$Consumption
    #    new  old
    #      1.  1.  mortality source local population (relative in element)
    #      2.  1.a mortality source stage
    #      3.  2.  prey module
    #      4.  3.  prey element
    #      5.  4.  prey polygon (relative in element)
    #      6.  5.  units of qnty
    #      7.  6.  stage
    #      8.  7.  stage realised quantity
    #      9.  7.a stage maximum quantity
    #     10.  8.  stage size
    #     11.  9.  calendar year
    #     12. 10.  period
    #     13. 11.  proportion of year
    #     14.      quantity of source local stage that consumed prey

    # Transition$Mortality for each prey
    #    new  old
    #    1.   1.  prey subject polygon (relative in element)
    #    2.   2.  units of qnty
    #    3.   3.  stage
    #    4.   4.  stage quantity
    #    5.   5.  stage size
    #    6.   6.  mortality source module
    #    7.   7.  mortality source element
    #    8.   8.  mortality source local population (relative in element)
    #    9.   8.a mortality source stage
###############################################################################
# test routines
