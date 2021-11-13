#B.GET.Mortality.01<-function(
if (!isGeneric("mortality")) 
setGeneric("mortality", function(element, universe) standardGeneric("mortality"))
setMethod("mortality", signature(element="GET", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Function:           B.Pr.KPFM.Mortality.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         3 July 2008
		# Description:        General predator function -
	
		# data set (dSet) requirements (vector for polygons in each variable)
		#   list( pup          = list (M =    nominal mortality over period
		#                            , z =    max proportion of nominal mortality that is subject to variation
		#                            , v = )  effect of density dependence on dependent variable
		#        ,juveAge0     =
		#        ,juveAge1     =
		#        ,juveAge2     =
		#        ,nonBreeders  =
		#        ,breeders     =
		#         ) #

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
		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset
		#-------------------------------------------------------------------------------

		for (pn in 1:getSlot(element, "polygonsN")) {   # from origin polygons
			for (st in 1:elemState$StageN){

        Survivorship <- exp(-dSet[[st]]$M[pn])

     if(dSet[[st]]$Health$TestHealth){
				if (elemState$Cond.H[[pn]]$HealthMax[st] > 0) {
					Hbar <- elemState$Cond.H[[pn]]$HealthRealised[st] / elemState$Cond.H[[pn]]$HealthMax[st]
				} else Hbar<-1  # if health is not important

				if (Hbar > 1) Hbar <- 1

      if(Hbar < dSet[[st]]$Health$z[pn]) Survivorship <- Survivorship*(Hbar/dSet[[st]]$Health$z[pn])^dSet[[st]]$Health$vprime[pn]
        } # end do health


				Died <- elemState$Abundance$num.ind[pn] * elemState$Stage[st,pn] * (1-Survivorship)

				elemTrans$Mortality <- rbind(elemTrans$Mortality,
												c(
												  pn                           #  1.  prey subject polygon (relative in element)
												 ,elemState$StageStrUnits      #  2.  units of qnty
												 ,st                           #  3.  stage
												 ,Died                         #  4.  stage quantity
												 ,elemState$Cond.S[st,pn]      #  5.  stage size
												 ,0                            #  6.  mortality source module (do not need to identify where mortality came from)
												 ,0                            #  7.  mortality source element
												 ,0                            #  8.  mortality source local population (relative in element)
												 ,0                            #  9.  mortality source stage
												  ))
			}
		}
	
		# Update state and transition for the element of universe
		setTransition(element, value=elemTrans)
		doUpdate(element, TRUE)
		setState(element, value=elemState)
	}
)
###############################################################################
###############################################################################
# test routines

    # Transition$Mortality for each prey
    #    new
    #    1.  prey subject polygon (relative in element)
    #    2.  units of qnty
    #    3.  stage
    #    4.  stage quantity
    #    5.  stage size
    #    6.  mortality source module
    #    7.  mortality source element
    #    8.  mortality source local population (relative in element)
    #    9.  mortality source stage
