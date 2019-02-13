#B.MI.Es.KPFM.migrate.01<-function(
# Create S4 method 'migrate'
if (!isGeneric("migrate"))
setGeneric("migrate", function(element, universe) standardGeneric("migrate"))
setMethod("migrate", signature(element="Krill", universe="Universe"), 
    function(
		element,
		universe	# access to universe if needed
		)     
    {
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
		elemTimesteps <- getTimestep(element)
        dSet     <- getTimestep(element, action[3])$actions[[action[5]]]$dset
		
		for (pn in 1:getSlot(element, "polygonsN")){ # origin polygons
			for (st in 1:elemState$StageN){

				StQty <- elemState$Stage[[pn]][st,2] * elemState$Abundance[[elemState$StageStrUnits]][pn]
				if(StQty>0){
					for (pnD in 1:getSlot(element, "polygonsN")){ # destination polygons
						if (pn!=pnD){
							elemTrans$Emigration<-rbind(elemTrans$Emigration
								,c(pn
								,pnD
								,elemState$StageStrUnits
								,st
								,StQty*dSet[pn,pnD]
							))
							elemTrans$Immigration<-rbind(elemTrans$Immigration
								,c(pnD
								,pn
								,elemState$StageStrUnits
								,st
								,StQty*dSet[pn,pnD]
							))
						}
					}
				}
			}
		}

		# update universe elements transition data
		setTransition(element, value=elemTrans) 
		doUpdate(element, TRUE)
    }
)

