#B.Pr.KPFM.update_State.01<-function (
setMethod("updateState", signature(element="Predator", universe="Universe"),
    function(element, universe)               # access to universe if needed
	{
   
		# Function:           B.Pr.KPFM.update_State.01
		# Version             0.01
		# Description:        update the State of the element in each polygon
		#                     based on the Transition part of the environment
		#                     based on KPFM krill but a shortened version
		# Input data:         as above
		# Return data:        none at present
		################################################################################
# Transition list
#     Update      = logical (flag to enter this subroutine)
#     Mortality   = holds a vector which is turned into matrix - ncol=9, byrow=TRUE
#                        1.  prey subject polygon (relative in element)
#                        2.  units of qnty
#                        3.  stage
#                        4.  stage quantity
#                        5.  stage size
#                        6.  mortality source module (do not need to identify where mortality came from)
#                        7.  mortality source element
#                        8.  mortality source local population (relative in element)
#                        9.  mortality source stage

elemState <- getState(element)
tChange <- getTransition(element, "Mortality")

# if no mortality then skip adjustments from mortality
if(!is.null(tChange)){
	# by polygons & stage

	for (pn in 1:getSlot(element, "polygonsN")){
		StageQty <- elemState$Abundance$num.ind[pn] * elemState$Stage[[pn]][,2]
		for (st in 1:elemState$StageN){
			StageQty[st] <- StageQty[st] - sum(tChange[tChange[,1]==pn & tChange[,3]==st, 4])
		}
					
		elemState$Abundance$num.ind[pn] <- sum(StageQty)
		elemState$Stage[[pn]][,2] <- StageQty / elemState$Abundance$num.ind[pn]
		elemState$Abundance$mass[pn] <- sum(StageQty * elemState$Cond.S[[pn]])
	}

}

# update element transition
setTransition(element, "Mortality", NULL)
doUpdate(element, FALSE)

# update element State
setState(element, value=elemState)
	}
)

