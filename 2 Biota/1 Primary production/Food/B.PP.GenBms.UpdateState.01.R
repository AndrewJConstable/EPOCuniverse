# Create S4 method 'updateState'
#B.PP.GenBms.update_State.01<-function (
setMethod("updateState", signature(element="PPgenBMS", universe="Universe"),
    function(element, universe)               # access to universe if needed
	{

		# Function:           B.GET.update_State.01
		# Version             0.01
		# Description:        update the State of the element in each polygon
		#                     based on the Transition part of the element
	  #                     Consumption is adjusted if needed and available after the transition
    #                     Mortality and Emigration are reset to NULL at the end of transition
    #                     Consumption and Young are reset when usedd.
		#
		# Notes               all $Mortality records should be in the abundance units relevant to the specific polygon
		#                         it is assumed that conversion from one unit to another has been achieved elsewhere
		#                         an error will be recorded if this is not the case

		################################################################################
		# Transition list
		#     Update      = logical (flag to enter this subroutine)

			# Transition$Mortality for subject element
			#
			#    1.    Subject element polygon (relative in element)
			#    2.    Subject element units of qnty
			#    3.    Subject element stage
			#    4.    Subject element stage quantity lost
			#    5.    Subject element stage size
			#    6.    mortality source module
			#    7.    mortality source element
			#    8.    mortality source local population (relative in element)
			#    9.    mortality source stage

		#     Transition$Emigration  of Subject element

		#                   1.  subject polygon (relative in element)
		#                   2.  destination polygon (relative in element)
		#                   3.  units of qnty
		#                   4.  stage
		#                   5.  stage quantity
    #                   6.  stage size

		#     Transition$Young  = rep(0,PolygonsN)    # for accumulating offspring from reproduction in each polygon of the element

    # Transition$Consumption
    #
    #      1. mortality source local population (relative in element)
    #      2. mortality source stage
    #      3. quantity of source local stage that consumed prey
    #      4. prey module
    #      5. prey element
    #      6. prey polygon (relative in element)
    #      7. units of qnty
    #      8. stage
    #      9. stage realised quantity
    #     10. stage maximum quantity
    #     11. stage size
    #     12. calendar year
    #     13. period
    #     14. proportion of year

################################################################################
# 1. a. transition matrix information

    Cols.TransMortality   <- 9
    Cols.TransEmigration  <- 5
    Cols.TransConsumption <- 14

# 1. b. Subject Element details

		elemState       <- getState(element)
		elemTrans       <- getTransition(element)
		elemListIndexes <- getElementIndexes(universe, element=getSignature(element, "ID"))

    Mortality   <- elemTrans$Mortality
    Emigration  <- elemTrans$Emigration
    if (!is.null(Mortality))  if(!is.matrix(Mortality)) Mortality  <- matrix(Mortality  ,ncol=Cols.TransMortality  ,byrow=TRUE)
    if (!is.null(Emigration)) if(!is.matrix(Emigration)) Emigration <- matrix(Emigration ,ncol=Cols.TransEmigration ,byrow=TRUE)

################################################################################
#  2.	Adjust mortality and emigration as combined rate processes
#     This assumes that losses from a polygon are fixed losses for activities but are rate losses for biota
#     Thus, these routines update the mortality and emigration components of Transition before
#     the state of the element is modified

#     Loop through stages, polygons

	for (st in 1:elemState$StageN) {

		for (pn in 1:getSlot(element, "polygonsN")) {

#-------------------------------------------------------------------------------
#  2.1 set up stage-polygon matrices

		#  calculate abundance of the stage in the current state
		#         (abundance of correct units for a polygon * age structure)
    #  not reserved for losses due to activities in the activities module (these are exempted from the adjustment here)
    #  then subset mortality, emigration for the stage and polygon (not including records from activities)
    #  then estimate log of survivorship rate (M) for each source of loss

       StageQtyInPolygon<-elemState$Abundance[[elemState$StageStrUnits]][pn]*elemState$Stage[st,pn]
       Records.Mort <- 0
       Records.Em   <- 0

    # if StageQtyInPolygon==0 then there would be no mortality or migration

    if(StageQtyInPolygon>0){


       if (!is.null(Mortality)) {
                 StageQtyInPolygon<-StageQtyInPolygon - sum(Mortality[(is.element(Mortality[,1],pn) &
                                                    is.element(Mortality[,3],st) &
                                                    is.element(Mortality[,6],3)),4])
                 MortStage  <- matrix(Mortality[(is.element(Mortality[,1],pn) & is.element(Mortality[,3],st) & !is.element(Mortality[,6],3)),],ncol=Cols.TransMortality)
                 Records.Mort <- nrow(MortStage)
                 #  check mortality quantities are in correct units
                    if (sum(!is.element(MortStage[,2], elemState$StageStrUnits))>0) {
										     epocErrorMessage(element, "transition$change records not all in the same units: Element ",
																				as.character(e)," Polygon ",as.character(pn), halt=TRUE)
									     } # end if
                 M.mort<- ifelse(Records.Mort>0,(-log(1-MortStage[,4] /StageQtyInPolygon)),0)

                  } else M.mort<-0
                   # end !is.null mortality

       if (!is.null(Emigration)) {
                 EmStage    <- matrix(Emigration[(is.element(Emigration[,1],pn) & is.element(Emigration[,4],st)),],ncol=Cols.TransEmigration)
                 Records.Em   <- nrow(EmStage)
                 M.Em  <- ifelse(Records.Em>0,(-log(1-EmStage[,5]/StageQtyInPolygon)),0)
                 } else M.Em<-0 # end !is.null emigration

#-------------------------------------------------------------------------------
#  2.2	Calculate total mortality

       Z     <- sum(c(M.mort,M.Em))

#-------------------------------------------------------------------------------
#  2.3	given StageQtyInPolygon and sum of the log rates, calculate the total loss given the combined rates
       TotalLoss <- StageQtyInPolygon*(1-exp(-Z))

#-------------------------------------------------------------------------------
#  2.4	Determine the loss.adjusted due to each component of loss using Baranov equation
       if(TotalLoss>0){
            Loss.mort <- TotalLoss*M.mort/Z
            Loss.em   <- TotalLoss*M.Em  /Z
            } # end if totalloss

#-------------------------------------------------------------------------------
#  2.4	adjust consumption matrices in transition of consumer elements
       if(Records.Mort>0) for (rc in 1:Records.Mort){
											sm <- MortStage[rc,6] # mortality source module
											se <- MortStage[rc,7]  # mortality source element
              if((sm*se)>0){ # do the following when not natural mortality of  the subject element
											sp <- MortStage[rc,8] # mortality source polygon (relative position)
											sst <- MortStage[rc,9] # mortality source stage

											yr <- getRTState(universe, "currentYear")
											pe <- getRTState(universe, "currentPeriod")

											mortalityScElement <- getEPOCElement(universe, sm, se)
											consumption <- getTransition(mortalityScElement, "Consumption")
                          if(!is.matrix(consumption)){
                            consumption <- matrix(consumption,ncol=Cols.TransConsumption,byrow=TRUE)
                            setTransition(mortalityScElement, "Consumption", value=consumption)
                            }

											consumption[
											   (consumption[,1]==sp &
												consumption[,2]==sst &
												consumption[,4]==elemListIndexes[1] &
												consumption[,5]==elemListIndexes[2] &
												consumption[,6]==pn &
												consumption[,8]==st &
												consumption[,12]==yr &
												consumption[,13]==pe),9] <- Loss.mort[rc]
											setTransition(mortalityScElement, "Consumption", consumption)
                    } # end if sm*se>0
						} # end loop rc and end if

#-------------------------------------------------------------------------------
#  2.5	update quantities for stage

#     replace Emigration loss to matrix for use at end
        if(M.Em>0) Emigration[(is.element(Emigration[,1],pn) & is.element(Emigration[,4],st)),5] <- Loss.em

#     replace Mortality loss to matrix for use at end
        if(M.mort>0) Mortality[(is.element(Mortality[,1],pn) & is.element(Mortality[,3],st) & !is.element(Mortality[,6],3)),4]<-Loss.mort

  } # end if StageQtyInPolygon>0

################################################################################

    } # end polygon
  } # end stage

################################################################################
#  3.	By polygon, update abundance, stage frequencies


		for (pn in 1:getSlot(element, "polygonsN")) {

      # 3.1 Vectors of current quantities and size for each stage in the polygon

          QtyPolygon<-elemState$Abundance[[elemState$StageStrUnits]][pn]
          StQty    <- QtyPolygon*elemState$Stage[,pn]  # vector

      # 3.2 Prepare vectors for new quantities and condition

          NewStQty <- rep(0,elemState$StageN)


      # 3.3 By stage, update number, size, reproductive condition and health

          for (st in 1:elemState$StageN) {
             # Qty-Mort-Em+Im
             # do in two steps
             # step 1 - calculate numbers left after losses (all same size)

          if(StQty[st]>0){

             Remainder = StQty[st]
             if(!is.null(Mortality)) Remainder <- Remainder - sum(Mortality[(is.element(Mortality[,1],pn) & is.element(Mortality[,3],st)),4])
             if(!is.null(Emigration)) {
                 Remainder               <- Remainder -sum(Emigration[(is.element(Emigration[,1],pn) & is.element(Emigration[,4],st)),5])
                 Immigration.N           <- Emigration[(is.element(Emigration[,2],pn) & is.element(Emigration[,4],st)),5]
                 Immigration.origin.poly <- Emigration[(is.element(Emigration[,2],pn) & is.element(Emigration[,4],st)),1]
                 NewStQty[st]  <- Remainder+sum(Immigration.N)
                 }
            } # end if StQty>0
           } # end st

          # put new numbers back in state of element
          # adjust biomass of element if needed

          elemState$Abundance[[elemState$StageStrUnits]][pn]<-sum(NewStQty)
          elemState$Stage[,pn] <- ifelse(elemState$Abundance[[elemState$StageStrUnits]][pn]>0,
                                       NewStQty/elemState$Abundance[[elemState$StageStrUnits]][pn],  # vector
                                       rep((1/elemState$StageN),elemState$StageN)
                                       )

     } # end polygon


		# reset $Transition (Mortality, Emigration) : change to null
		elemTrans["Mortality"] <- list(NULL)
		elemTrans["Emigration"] <- list(NULL)

		# update element transition
		setTransition(element, value=elemTrans)

		doUpdate(element, FALSE)

		# Update state for the element of universe
		setState(element, value=elemState)

	} # end function
)

		#     Transition$Emigration  of Subject element

		#                   1.  subject polygon (relative in element)
		#                   2.  destination polygon (relative in element)
		#                   3.  units of qnty
		#                   4.  stage
		#                   5.  stage quantity
    #                   6.  stage size






			# Transition$Mortality for subject element
			#
			#    1.    Subject element polygon (relative in element)
			#    2.    Subject element units of qnty
			#    3.    Subject element stage
			#    4.    Subject element stage quantity lost
			#    5.    Subject element stage size
			#    6.    mortality source module
			#    7.    mortality source element
			#    8.    mortality source local population (relative in element)
			#    9.    mortality source stage

