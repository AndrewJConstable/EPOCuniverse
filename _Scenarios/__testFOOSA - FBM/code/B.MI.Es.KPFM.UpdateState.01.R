# Create S4 method 'updateState'
#B.MI.Es.KPFM.update_State.01<-function (
setMethod("updateState", signature(element="Krill", universe="Universe"),
    function(element, universe)               # access to universe if needed
	{
        
		# Function:           B.MI.Es.KPFM.update_State.01
		# Version             0.01
		# Description:        update the State of the element in each polygon
		#                     based on the Transition part of the environment
		#                     All parts of transition are read (except for consumption)
		#                     and the State is updated accordingly.  Consumption is dealt with directly
		#
		#                     If there is insufficient abundance then an error routine is invoked to make the
		#                     abundance equal to zero and to modify the amounts taken out of the polygon such as
		#                     predator consumption amongst other corrections (migration corrections are not included at present)
		# Notes               all $Mortality records should be in the abundance units relevant to the specific polygon
		#                         it is assumed that conversion from one unit to another has been achieved elsewhere
		#                         an error will be recorded if this is not the case
		#                     have not taken account of potentially different size conditions in different polygons and how
		#                         these will combine to give an appropriate value for B/N.
		#   Elements using this function
		#     E. superba    ID = 22001

		################################################################################
		# Transition list
		#     Update      = logical (flag to enter this subroutine)
		
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

		#     Emigration  = c()
		#                   1.  subject polygon (relative in element)
		#                   2.  destination polygon (relative in element)
		#                   3.  units of qnty
		#                   4.  stage
		#                   5.  stage quantity
		#     Young       = rep(0,PolygonsN)    # for accumulating offspring from reproduction in each polygon of the element

		#     Immigration = c(),
		#                   1.  subject polygon
		#                   2.  origin polygon
		#                   3.  units of qnty
		#                   4.  stage
		#                   5.  stage quantity
		elemState <- getState(element)
		newState <- elemState
		elemTrans <- getTransition(element)
		tChange <- elemTrans$Mortality
		elemListIndexes <- getElementIndexes(universe, element=getSignature(element, "ID"))
print(tChange)
		#-------------------------------------------------------------------------------
		# if no mortality then skip adjustments from mortality
		if(is.null(tChange)){
			tChangePresent <- FALSE
			Removals <- NULL
			FixedRemovals <- 0
			Mort <- NULL
			StageRecordsN <- 0
		} else {

			tChangePresent<-TRUE
			tChangeCols<-ncol(tChange) # number of cols used as a relative measure later
									  # in case more columns are added to Change matrix
			tChangeRows<-nrow(tChange)
		  
			# change for each record
			Removals<-tChange[,4]

			#  determine competition coefficients for sources of mortality for each line in tChange
			#            coefficients > 0 give weight to fixed allocation for the
			#            respective sources of mortality;  initially, the coefficients will be standardised to sum
			#            to one by dividing by the sum of the coefficients.  Sources of mortality
			#            with coefficients equal to 0 are treated as fixed rates of remainder
			#            one coefficient per row
			CompCoeff<-NULL
			for (cc in 1:tChangeRows){
				CCind <- (elemTrans$CompetitorIndexes[,1]==tChange[cc,6] &
						elemTrans$CompetitorIndexes[,2]==tChange[cc,7])
				Coeff <- elemTrans$CompCoeff[CCind,tChange[cc,8]]
				Coeff <- ifelse(length(Coeff)>0,Coeff,0) # if not present then default coeff = 0
				CompCoeff <- c(CompCoeff,Coeff)
			}

			CCindex <- CompCoeff>0 # index for use in fixed allocation

		}

		for (pn in 1:getSlot(element, "polygonsN")) {
			# calculate new attributes of State - polygon, stage, N, B, size
			# for each stage in the polygon, calculate the new state of that
			# stage by removing all losses (migration counts as a non-competitive loss)

			# initialise vector of abundance in each stage
			Abund <- rep(0, elemState$StageN)

			for (st in 1:elemState$StageN) {
				# flag to become True if competitors try to take more than available
				# used to signal need to change amounts consumed by those competitors
				AdjustedFixedRemovals<-FALSE

				#       1. calculate abundance of the stage in the current state
				#         (abundance of correct units for a polygon * age structure)
				AbStageInitial <- elemState$Abundance[[elemState$StageStrUnits]][pn]*
							 elemState$Stage[[pn]][st,2]

				# if AbStageInitial==0 then no mortality in period thus skip all of these calculations
				if(AbStageInitial>0){
					# 2. availability
					AbStageAvailable <- AbStageInitial * getFunctionData(element, "stateUpdate")$dset$Availability[pn]

					if(tChangePresent){
						#    index of mortality records for this stage in this polygon
						StageIndex<-(is.element(tChange[,1],pn) & tChange[,3]==st)

						StageRecordsN<-sum(StageIndex)
						StageIndexLoc<-ifelse(StageRecordsN>0,which(StageIndex),NULL)  # row numbers of relevant records
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

						#     check all records are in the correct abundance units as specified by $State$StageStrUnits
						if (sum(!is.element(tChange[StageIndex,2], elemState$StageStrUnits))>0) {
							epocErrorMessage(element, "transition$change records not all in the same units: Element ",
																	as.character(e)," Polygon ",as.character(pn), halt=TRUE)
						} 
					} 

					# only proceed if there are records present for this stage in the polygon
					# otherwise there is only movement of this stage and therefore no adjustments are necessary
					if(StageRecordsN > 0){

						# 3. for competitive sources of mortality
				        # calculate fixed removals for stage and determine if greater than availability;
						#          if greater than available then do competitive allocation
						RmIndex<-(CCindex & StageIndex)

						FixedRemovals<-ifelse(sum(RmIndex>0),sum(Removals[RmIndex]),0)
				  
						if(FixedRemovals>AbStageAvailable){
							AdjustedFixedRemovals<-TRUE
							#         a. the potential quantity of krill lost to each source of mortality
							#            is determined as a fraction of the available krill.
							Frac <-Removals[RmIndex]/AbStageAvailable

							#         b. the weighted fractions (product of the coefficient and the
							#            fractions) indicate the proportions of krill taken by the
							#            different sources of mortality in a competitive environment,
							#            e.g. assuming each source would have taken equal quantities of
							#            krill but that the competition coefficients ranged from 1 to 1000;
							#            those sources with a coefficient of 1000 would be more likely to
							#            take their quantum well in advance of those that have a
							#            coefficient of 1.  Therefore, the coefficients, once standardised,
							#            serve to identify how much each source will be allocated relative
							#            to the other sources when krill is in short supply.
							RevCC<-CompCoeff[RmIndex]
							RevCC<-RevCC/sum(RevCC)

							WtFrac<-Frac*RevCC

							#         c. the coefficients are then all adjusted so that the sum of the
							#            weighted fractions would equal 1;  this is done by multiplying
							#            the coefficients by the inverse of the sum of the original
							#            weighted fractions.
							#         d. any coefficients that become greater than 1 are made equal to
							#            1 in order that those sources of mortality only take what they
							#            would have taken in the absence of other sources of mortality.
							#         e. steps (c) and (d) are continued for the subsets of mortality
							#            for which the coefficients are less than 1 until the sum of the
							#            weighted fractions equals 1.  In this way, the allocations to the
							#            most competitive sources of mortality are filled first if the
							#            original coefficients are such that one or more sources of
							#            mortality should have their potential krill demand realised.
							while ((abs(1-sum(WtFrac))> 1E-2) & (sum(RevCC)<length(RevCC))) {
								# 1. remove any loss where RevCC>=1
								PropLoss<-1-sum(WtFrac[RevCC>=1])
								# 2. adjust
								RevCC[RevCC<1]<-PropLoss/sum(WtFrac[RevCC<1])*RevCC[RevCC<1]
								RevCC[RevCC>1]<-1
								WtFrac<-RevCC*Frac
							}
							WtFrac<-WtFrac*1/sum(WtFrac)  # correct to 1 if iteration is slightly off
					
							# update Removals
							FixedRemovals <- AbStageAvailable
							Removals[RmIndex] <- WtFrac*AbStageAvailable
						}

						#---------------------------------------------------------------
						# 5. for non-competitive mortality
						#   i)   determine mortality rates for sources of mortality with competition
						#        coefficients = 0 (return vector - NULL if none)
						RmIndex <- (!CCindex & StageIndex)
						Mort <- ifelse(sum(RmIndex)>0,(-log(1-Removals[RmIndex]/AbStageInitial)),NULL)
					}

					# 6. account for movement as non-competitive loss from polygon
					#    determine movement rates from stage in polygon to other polygons (return vector - NULL if none)
					#    generate vector of quantities moved for stage from polygon
					MoveIndex <- (elemTrans$Emigration[,1]==pn
									  & elemTrans$Emigration[,4]==st)
					StageMove <- elemTrans$Emigration[MoveIndex,5]

					# generate vector of rates
					if (length(StageMove) > 0) {
					   Move <- (-log(1-StageMove/AbStageInitial))
				   } else {Move<-NULL}

					# ii)  use root finder to determine mortality rate for fixed allocation
					Rates <- sum(Move)+sum(Mort)
					# Original function to nlm()
					# M_FixedRemovals <- ifelse(FixedRemovals>0,
								 # nlm(function(rootM,Abund,Loss,OtherM)
									   # {(Loss-(rootM/(rootM+OtherM)*
											  # (1-exp(-(rootM+OtherM)))*
											  # Abund))^2}
								   # ,0.001,stepmax = 10.0
								   # ,Loss=FixedRemovals
								   # ,Abund=AbStageInitial
								   # ,OtherM=Rates)$estimate,0)
					# New variant suggested by Ravi Varadhan 14/10/2010, r-devel@R-project.org
					M_FixedRemovals <- ifelse(FixedRemovals>0,
								 nlm(function(rootM,Abund,Loss,OtherM) {
										(Loss/Abund - (rootM/(rootM+OtherM)*
											  (1-exp(-(rootM+OtherM)))))^2
									}
								   ,0.001,stepmax = 10.0
								   ,Loss=FixedRemovals
								   ,Abund=AbStageInitial
								   ,OtherM=Rates)$estimate,0)
								   
					# 6. use Baranov equation to determine quantities of non-competitive losses
					#  as vectors for mortality and movement separately and adjust quantities in other elements
					Z <- M_FixedRemovals + Rates
					TotalLoss <- (1-exp(-Z)) * AbStageInitial

					if(!is.null(Mort)) {
						# determine quantities removed by non-competitive sources
						Removals[RmIndex] <- Mort / Z * TotalLoss
					}

					if(!is.null(Move)) {
						# determine quantities moved
						elemTrans$Emigration[MoveIndex,5] <- Move/Z*TotalLoss
					}

					# 7. adjust records based on changes
					if(AdjustedFixedRemovals){
						Records<-which(CCindex & StageIndex)
						for (rc in 1:length(Records)){
							sm <- tChange[Records[rc],6] # mortality source module
							se <- tChange[Records[rc],7]  # mortality source element
							sp <- tChange[Records[rc],8] # mortality source polygon (relative position)
							sst <- tChange[Records[rc],9] # mortality source stage

							yr <- getRTState(universe, "currentYear")
							pe <- getRTState(universe, "currentPeriod")
							
							mortalityScElement <- getEPOCElement(universe, sm, se)
							consumption <- getTransition(mortalityScElement, "Consumption")
							consumption[
							   (consumption[,1]==sp &
								consumption[,2]==sst &
								consumption[,3]==elemListIndexes[1] &
								consumption[,4]==elemListIndexes[2] &
								consumption[,5]==pn &
								consumption[,7]==st &
								consumption[,11]==yr &
								consumption[,12]==pe),8] <- Removals[Records[rc]]
							setTransition(mortalityScElement, "Consumption", consumption)
						}
					} 

					if(!is.null(Mort)) {
						Records<-which(RmIndex)
						for (rc in 1:length(Records)){
							sm<-tChange[Records[rc],6] # mortality source module
							se<-tChange[Records[rc],7]  # mortality source element
							sp<-tChange[Records[rc],8] # mortality source polygon (relative position)
							sst<-tChange[Records[rc],9] # mortality source stage
							
							yr<-getRTState(universe, "currentYear")
							pe<-getRTState(universe, "currentPeriod")
							
							if(sm>0){ # otherwise natural mortality within own element
								mortalityScElement <- getEPOCElement(universe, sm, se)
								consumption <- getTransition(mortalityScElement, "Consumption")
								consumption[
									(consumption[,2]==elemListIndexes[1] &
									 consumption[,3]==elemListIndexes[2] &
									 consumption[,4]==pn &
									 consumption[,6]==st &
									 consumption[,9]==yr &
									 consumption[,10]==pe),7] <- Removals[Records[rc]]
								setTransition(mortalityScElement, "Consumption", consumption)
							}
						}
					}

					if(!is.null(Move)) {
						Records<-which(MoveIndex)
						for (rc in 1:length(Records)){
							elemTrans$Immigration[
							   (elemTrans$Immigration[,1]==elemTrans$Emigration[Records[rc],2] &
								elemTrans$Immigration[,2]==pn &
								elemTrans$Immigration[,4]==st),5] <- elemTrans$Emigration[Records[rc],5]
						}
					}
					
					# 8. calculate new abundance of stage in polygon
					TotalStageRemovals <- ifelse(tChangePresent,sum(Removals[StageIndex]),0)
					Abund[st] <- AbStageInitial - TotalStageRemovals - sum(elemTrans$Emigration[MoveIndex,5])
				}
			}

			# save abundances at stage temporarily in stage structure (pending addition of immigrants)
			newState$Stage[[pn]][,2] <- Abund
		}
		
		# Update immigration quantities and add to polygon/stage combinations
		Imm <- elemTrans$Immigration
		for (pn in 1:getSlot(element, "polygonsN")) {
			for (st in 1:elemState$StageN){
				ImmIndex <- (Imm[,1]==pn & Imm[,4]==st)
				if(sum(ImmIndex)>0) newState$Stage[[pn]][st,2]<-
								  newState$Stage[[pn]][st,2]+sum(Imm[ImmIndex,5])
			}
		}

		# add Young to first Stage in each polygon
		Young <- elemTrans$Young
		if(sum(Young)>0){
			for (pn in 1:getSlot(element, "polygonsN")) {
				newState$Stage[[pn]][1,2]<-
								  newState$Stage[[pn]][1,2]+Young[pn]
			}
		}
		  
		# update abundances for polygon
		for (pn in 1:getSlot(element, "polygonsN")) {

			SumAbund<-sum(newState$Stage[[pn]][,2])
			  
			if(elemState$StageStrUnits==1) { # if abundances in Number
				newState$Abundance[[1]][pn]<-SumAbund
				newState$Abundance[[2]][pn]<-sum(newState$Stage[[pn]][,2] * elemState$Cond.S[[pn]])
			}

			if(elemState$StageStrUnits==2) { # if abundances in Biomass
				newState$Abundance[[2]][pn] <- SumAbund
				newState$Abundance[[1]][pn] <- sum(newState$Stage[[pn]][,2] / elemState$Cond.S[[pn]])
			}

			# update stage structure
			 newState$Stage[[pn]][,2] <- ifelse(SumAbund>0,
											   newState$Stage[[pn]][,2] / SumAbund,
											   rep(1 / elemState$StageN, elemState$StageN))

		}

		# reset $Transition (Mortality, Emigration and Immigration) : change to null
		elemTrans["Mortality"] <- list(NULL)
		elemTrans["Emigration"] <- list(NULL)
		elemTrans["Immigration"] <- list(NULL)
		elemTrans[["Young"]] <- rep(0, getSlot(element, "polygonsN"))
	
		# update element transition
		setTransition(element, value=elemTrans)
		doUpdate(element, FALSE)

		# update element State
		setState(element, value=newState)
	}
)

