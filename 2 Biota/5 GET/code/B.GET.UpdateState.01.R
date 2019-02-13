# Create S4 method 'updateState'
#B.GET.update_State.01<-function (
setMethod("updateState", signature(element="GET", universe="Universe"),
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

   
    Mortality   <- ifelse(!is.matrix(elemTrans$Mortality),matrix(elemTrans$Mortality  ,ncol=Cols.TransMortality  ,byrow=TRUE),elemTrans$Mortality)
    Emigration  <- ifelse(!is.matrix(elemTrans$Emigration) ,matrix(elemTrans$Emigration ,ncol=Cols.TransEmigration ,byrow=TRUE),elemTrans$Emigration)

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

       StageQtyInPolygon<-elemState$Abundance[[elemState$StageStrUnits]][pn]*
							                                            elemState$Stage[st,pn]-
                                     sum(Mortality[(is.element(Mortality[,1],pn) &
                                                    is.element(Mortality[,3],st) &
                                                    is.element(Mortality[,6],3)),4])

    #  subset mortality, emigration for the stage and polygon (not including records from activities)

        MortStage  <- Mortality[(is.element(Mortality[,1],pn) & is.element(Mortality[,3],st) & !is.element(Mortality[,6],3)),]
        EmStage    <- Emigration[(is.element(Emigration[,1],pn) & is.element(Emigration[,4],st)),]

        Records.Mort <- nrow(MortStage)
        Records.Em   <- nrow(EmStage)

    #  check mortality quantities are in correct units
       if (sum(!is.element(MortStage[,2], elemState$StageStrUnits))>0) {
										epocErrorMessage(element, "transition$change records not all in the same units: Element ",
																				as.character(e)," Polygon ",as.character(pn), halt=TRUE)
									} # end if

#-------------------------------------------------------------------------------
#  2.2	estimate log of survivorship rate (M) for each source of loss

       M.mort<- ifelse(Records.Mort>0,(-log(1-MortStage[,4] /StageQtyInPolygon)),NULL)
       M.Em  <- ifelse(Records.Em>0,(-log(1-EmStage[,5]/StageQtyInPolygon)),NULL)
       Z     <- sum(c(M.mort,M.em))

#-------------------------------------------------------------------------------
#  2.3	given StageQtyInPolygon and sum of the log rates, calculate the total loss given the combined rates
       TotalLoss <- StageQtyInPolygon*(1-exp(-sum(c(M.mort,M.em))))

#-------------------------------------------------------------------------------
#  2.4	Determine the loss.adjusted due to each component of loss using Baranov equation
       if(TotalLoss>0){
            Loss.mort <- ifelse(!is.null(M.mort),(TotalLoss*M.mort/Z),NULL)
            Loss.em   <- ifelse(!is.null(M.em)  ,(TotalLoss*M.em  /Z),NULL)
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
        Emigration[(is.element(Emigration[,1],pn) & is.element(Emigration[,4],st)),5] <- Loss.em

#     replace Mortality loss to matrix for use at end
        Mortality[(is.element(Mortality[,1],pn) & is.element(Mortality[,3],st) & !is.element(Mortality[,6],3)),4]<-Loss.mort

################################################################################

    } # end polygon
  } # end stage

################################################################################
#  3.	By polygon, update abundance, stage frequencies, weighted mean size (from immigration), health, reproduction

# generate default MigrateConsumption matrices by stage (rows = origing, cols = destination)

    PN<-getSlot(element, "polygonsN")
    matPN<-matrix(rep(0,PN*PN),ncol=PN)
    for(p in PN) matPN[p,p]<-0         # only fills in matrix if there are values>0
    MigrateConsumption<-rep(list(matPN),elemState$StageN)

    SizePrior<-elemState$Cond.S    # matrix (rows = stage, cols = polygon)
    ReprodPrior<-elemState$Cond.R  # matrix (rows = stage, cols = polygon)
    HealthPrior<-elemState$Cond.H  # matrix (rows = stage, cols = polygon)

		for (pn in 1:getSlot(element, "polygonsN")) {

      # 3.1 Vectors of current quantities and size for each stage in the polygon

          QtyPolygon<-elemState$Abundance[[elemState$StageStrUnits]][pn]
          StQty    <- QtyPolygon*elemState$Stage[,pn]  # vector
          StSize   <- SizePrior[,pn] # vector
          StReprod <- ReprodPrior[,pn] # vector
          StHealth <- HealthPrior[,pn] # vector

      # 3.2 Prepare vectors for new quantities and condition

          NewStQty    <- 0/StQty
          NewStSize   <- StSize
          NewStReprod <- StReprod
          NewStHealth <- StHealth


      # 3.3 By stage, update number, size, reproductive condition and health

          for (st in 1:elemState$StageN) {
             # Qty-Mort-Em+Im
             # do in two steps
             # step 1 - calculate numbers left after losses (all same size)

             Remainder = StQty[st]-
                         sum(Mortality[(is.element(Mortality[,1],pn) & is.element(Mortality[,3],st)),4])-
                         sum(Emigration[(is.element(Emigration[,1],pn) & is.element(Emigration[,4],st)),5])

             Immigration.N           <- Emigration[(is.element(Emigration[,2],pn) & is.element(Emigration[,4],st)),5]
             Immigration.origin.poly <- Emigration[(is.element(Emigration[,2],pn) & is.element(Emigration[,4],st)),1]

             NewStQty[st]  <- Remainder+sum(Immigration.N)

             # step 2 - add values to MigrateConsumption matrix

             MigrateConsumption[[st]][pn,pn]<-Remainder
             MigrateConsumption[[st]][pn,Immigration.origin.poly]<-Immigration.N

             # step 3 - adjust size, reproduction, health conditions as weighted means
             #          based on remainder and origins of immigrants of size of numbers left and sizes of immigrants


             NewStSize[st]   <- (Remainder*StSize[st] +
                                          sum(Immigration.N*SizePrior[st,Immigration.origin.poly])
                                          )/NewStQty[st]

             NewStReprod[st] <- (Remainder*StReprod[st] +
                                          sum(Immigration.N*ReprodPrior[st,Immigration.origin.poly])
                                          )/NewStQty[st]
             NewStHealth[st] <- (Remainder*StHealth[st] +
                                          sum(Immigration.N*HealthPrior[st,Immigration.origin.poly])
                                          )/NewStQty[st]
             } # end st

          # put new numbers and size back in state of element
          # adjust biomass of element if needed

          elemState$Abundance[[elemState$StageStrUnits]][pn]<-sum(NewStQty)
          elemState$Stage[,pn] <- ifelse(elemState$Abundance[[elemState$StageStrUnits]][pn]>0,
                                       NewStQty/elemState$Abundance[[elemState$StageStrUnits]][pn],  # vector
                                       rep((1/elemState$StageN),elemState$StageN)
                                       )

          if(elemState$StageStrUnits==1) {
              elemState$Abundance[[2]][pn] <- sum(NewStQty*NewStSize)
              } else {
              elemState$Abundance[[1]][pn] <- sum(NewStQty/NewStSize)
              }

          elemState$Cond.S[,pn] <- NewStSize # vector
          elemState$Cond.R[,pn] <- NewStReprod # vector
          elemState$Cond.H[,pn] <- NewStHealth # vector

     } # end polygon


		# reset $Transition (Mortality, Emigration) : change to null
		elemTrans["Mortality"] <- list(NULL)
		elemTrans["Emigration"] <- list(NULL)

		# update element transition
		setTransition(element, value=elemTrans)
		setTransition(element, "MigrateConsumption",value=MigrateConsumption)

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

