#B.GET.AllocateConsumption.01<-function(
if (!isGeneric("allocateConsumption"))
setGeneric("allocateConsumption", function(element, universe) standardGeneric("allocateConsumption"))
setMethod("allocateConsumption", signature(element="GET", universe="Universe"),
    function(
		element,
		universe	# access to universe if needed
		)     
    {
      
		# Function:           B.GET.AllocateConsumption.01
		#   Version           0.01
		#   Authors           A.Constable
		#   last.edit         7 June 2015 (2 July 2008)
		# Description:        GET function -

		# Condition for Size (Cond.S), Reproduction (Cond.R), Health (Cond.H) is stored as matrices
    # e.g.
		#     Taxon$State$Cond.X  (where X is S, R, H)
		#            matrix (rows = stage, cols = polygon)
		# data are accumulated from Transition$Consumption matrix, which is then set to NULL after use
    #

		# Transition$Consumption
		#    col
		#      1. mortality source local population (relative in element)
		#      2. mortality source stage
		#      3. prey module
		#      4. prey element
		#      5. prey polygon (relative in element)
		#      6. units of qnty
		#      7. stage
		#      8. stage realised quantity
		#      9. stage maximum quantity
		#     10. stage size (mass per unit)
		#     11. calendar year
		#     12. period
		#     13. proportion of year
		################################################################################

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
		preyElement <- getTimestep(element, action[3])$actions[[action[5]]]$relatedIndexes
		dSet <- getTimestep(element, action[3])$actions[[action[5]]]$dset
		Consumed <- elemTrans$Consumption
    MC       <- elemTrans$MigrateConsumption


## removed calculation of maximum food  as health is judged by combined biomass and gonad

################################################################################
# 1. convert food to mass in prey that have units in numbers

      Consumed[Consumed[,7]<2,9]<-Consumed[Consumed[,7]<2,9]*Consumed[Consumed[,7]<2,11]

################################################################################
# 2. Generate Food per capita in each polygon, based on consumption within polygon
#    Matrix (rows = stage; cols = polygon)

    Fpc<-matrix(rep(0,(getSlot(element, "polygonsN")*elemState$StageN)),nrow=elemState$StageN)
    Fpc.adj<-Fpc # for stage after Fpc is generated

		for (pn in 1:getSlot(element, "polygonsN")){
			for (st in 1:elemState$StageN){
      if(dSet$IsConsumer[st]){
				# sum the aggregate per capita prey value consumed by each stage for each polygon of origin

				PerCapitaRealisedFood <- 0
#				PerCapitaMaxFood <- 0
      
				for(prey in 1:length(dSet$FoodValue[[st]])){
            dFood <-Consumed[Consumed[,1] == pn & Consumed[,2] == st
												          & Consumed[,4] == preyElement[prey,1]
												          & Consumed[,5] == preyElement[prey,2],]
            PerCapitaRealisedFood <-PerCapitaRealisedFood +
                                    dSet$FoodValue[prey] *
                                 sum(dFood[,9]/dFood[,3]) # realised prey quantity per capita
#           PerCapitaMaxFood <- PerCapitaMaxFood +
#                                 dSet$FoodValue[prey] *
#                                 sum(dFood[,9]/dFood[,14]) # realised prey quantity per capita
				   } # end prey
       Fpc[st,pn]<-PerCapitaRealisedFood
       } # end if(IsConsumer[st])
       } # end st
       } # end pn

################################################################################
# 3. Adjust food per capita according to the proportion that the origin polygon of immigrants contribute
#    to the per capita food calculation

		for (pn in 1:getSlot(element, "polygonsN")){
			for (st in 1:elemState$StageN){
          Fpc.adj[st,pn] <- (sum(dSet$Cpp[[st]][,pn]*MC[[st]][,pn]*Fpc[st,])+
                            sum((1-dSet$Cpp[[st]][,pn])*MC[[st]][,pn]*Fpc[st,pn]))/sum(MC[[st]][,pn])
          } # end st
      } # end pn

################################################################################
# 4. Allocate between Metabolism, growth and reproduction

    Cond.S.old<-elemState$Cond.S

		for (pn in 1:getSlot(element, "polygonsN")){

			for (st in 1:elemState$StageN){


				# partition between metabolism, growth and reproduction - this is per capita calculations from now on

         Met<-elemState$Cond.S[st,pn]*dSet$dMetab[[st]]*periodInfo$YearPropn
         Food<-Fpc.adj[st,pn]-Met
         Stress<-0

         if(Food<=0){ # lose gonad and then shrink
             if(elemState$maturity>0){
                elemState$Cond.R[st,pn] <- elemState$Cond.R[st,pn]+Food
                if(elemState$Cond.R[st,pn]<0){
                        Food <- elemState$Cond.R[st,pn]
                        elemState$Cond.R[st,pn] <- 0
                   } # end if
                } # end if maturity>0
              shrink<-abs(Food)/dSet$Starve[st]$costShrink
              if(shrink>dSet$Starve[st]$ShrinkMax){
                 Stress <- shrink-dSet$Starve[st]$ShrinkMax
                 shrink <- dSet$Starve[st]$ShrinkMax
                 }
              elemState$Cond.S[st,pn]<-elemState$Cond.S[st,pn]-shrink
              elemState$Cond.H[st,pn]<-elemState$Cond.H[st,pn]-Stress/elemState$Cond.S[st,pn]   # stress is maintained and accumulated with continued periods of food deficits

            } else { # grow gonad and body

           if(dSet$AllocateToReproduction){
              R<-dSet$dReprod[[st]]$pR * Food/dSet$dReprod[[st]]$cR
              Rmax<-dSet$dReprod[[st]]$Rmax*periodInfo$YearPropn

              G<-(1-dSet$dReprod[[st]]$pR) * Food/dSet$dGrowth[[st]]$cG
              Gmax<-dSet$dGrowth[[st]]$Gmax*periodInfo$YearPropn

              if(R>=Rmax & G>=Gmax){
                 R <- Rmax
                 G <- Gmax
                 } else {
                 if(R>=Rmax){
                    Fremain <- (R-Rmax)*dSet$dReprod[[st]]$cR/dSet$dReprod[[st]]$pR
                    R <- Rmax
                    G <- G + Fremain/dSet$dGrowth[[st]]$cG
                    if(G>Gmax) G<-Gmax
                    } else {
                    if(G>=Gmax){
                       Fremain <- (G-Gmax)*dSet$dGrowth[[st]]$cG/(1-dSet$dReprod[[st]]$pR)
                       G <- Gmax
                       R <- R + Fremain/dSet$dReprod[[st]]$cR
                       if(R>Rmax) R<-Rmax
                       } # end G
                    } # end R
                 } # end R & G

              elemState$Cond.S[st,pn]<-elemState$Cond.S[st,pn] + G
              elemState$Cond.R[st,pn]<-elemState$Cond.R[st,pn] + R
              elemState$Cond.H[st,pn]<-1  # condition returns to full health (=0) if growth has occurred
            } else { # if not allocate to reproduction
              G <- G + Food/dSet$dGrowth[[st]]$cG
              if(G>Gmax) G<-Gmax
              elemState$Cond.S[st,pn]<-elemState$Cond.S[st,pn] + G
              elemState$Cond.H[st,pn]<-1  # condition returns to full health (=0) if growth has occurred
            } # end if allocate to reproduction
          } # end if else Food<=0

      } # end if st is consumer
			} # end st

# alter the biomass and frequency if needed of the stage in the polygon

      if(elemState$StageStrUnits==1){  # if primary units are abundance in number
          elemState$Abundance[[2]][pn]<-elemState$Abundance[[1]][pn]*sum(elemState$Stage[,pn]*elemState$Cond.S[,pn])
         } else { # if primary units are abundance in mass
         NatStage<-elemState$Abundance[[2]][pn]*elemState$Stage[,pn]/Cond.S.old[,pn]
         BatStage<-NatStage*elemState$Cond.S[,pn]
         elemState$Abundance[[2]][pn] <- sum(BatStage)
         elemState$Stage[,pn]<-BatStage/elemState$Abundance[[2]][pn]
         }

		} # end pn

		# Update state for the element of universe
		setState(element, value=elemState)
		setTransition(element, "Consumption", value=NULL)
		setTransition(element, "MigrateConsumption", value=NULL)

	}
)






    # Transition$Consumption
    #    new  old (2008)
    #      1.  1.  mortality source local population (relative in element)
    #      2.  2.  mortality source stage
    #      3.      quantity of source local stage that consumed prey
    #      4.  3.  prey module
    #      5.  4.  prey element
    #      6.  5.  prey polygon (relative in element)
    #      7.  6.  units of qnty
    #      8.  7.  stage
    #      9.  8.  stage realised quantity
    #     10.  9.  stage maximum quantity
    #     11. 10.  stage size
    #     12. 11.  calendar year
    #     13. 12.  period
    #     14. 13.  proportion of year


