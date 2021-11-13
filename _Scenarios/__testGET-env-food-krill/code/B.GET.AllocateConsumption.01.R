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

    # Note that Reproduction, growth and starvation are managed in the same units as Cond.S (e.g. wet weight)
    #     metabolism is managed in food units.  the food value converts the quantity from the prey into
    #    food units for these calculations e.g. Carbon
    #    the conversion parameters for gonads and growth allow for the conversion from food units (C) into Cond.S units (WW)
    #    It is preferable that Cond.S and Cond.R are in the same units.

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
		################################################################################
    Cols.TransMortality   <- 9
    Cols.TransEmigration  <- 5
    Cols.TransConsumption <- 14

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
# 2. Generate Food per capita, Fpc, in each polygon, based on consumption within polygon
#    Matrix (rows = stage; cols = polygon)

    Fpc<-matrix(rep(0,(getSlot(element, "polygonsN")*elemState$StageN)),nrow=elemState$StageN)
    Fpc.adj<-Fpc # for stage after Fpc is generated

		for (pn in 1:getSlot(element, "polygonsN")){
			for (st in 1:elemState$StageN){
      if(dSet$IsConsumer[st]){
				# sum the aggregate per capita prey value consumed by each stage for each polygon of origin

				PerCapitaRealisedFood <- 0
#				PerCapitaMaxFood <- 0

        if(!is.null(Consumed)){

	  			for(prey in 1:length(dSet$FoodValue[[st]])){
            dFood <-matrix(Consumed[Consumed[,1] == pn & Consumed[,2] == st
												          & Consumed[,4] == preyElement[prey,1]
												          & Consumed[,5] == preyElement[prey,2],],ncol=Cols.TransConsumption)

            PerCapitaRealisedFood <-PerCapitaRealisedFood +
                                    dSet$FoodValue[[st]][prey] *
                                 sum(dFood[,9]/dFood[,3]) # realised prey quantity per capita
#           PerCapitaMaxFood <- PerCapitaMaxFood +
#                        dSet$FoodValue[prey] *
#                                 sum(dFood[,9]/dFood[,14]) # realised prey quantity per capita
				   } # end prey
         } # end if(!is.null(Consumed))
       Fpc[st,pn]<-PerCapitaRealisedFood
       } # end if(IsConsumer[st])
       } # end st
       } # end pn

################################################################################
# 3. Adjust food per capita according to the proportion that the origin polygon of immigrants contribute
#    to the per capita food calculation
		for (pn in 1:getSlot(element, "polygonsN")){
			for (st in 1:elemState$StageN){
          sumMC<-sum(MC[[st]][,pn])
          Fpc.adj[st,pn] <- if(sumMC>0) (sum(dSet$Cpp[[st]][,pn]*MC[[st]][,pn]*Fpc[st,])+
                            sum((1-dSet$Cpp[[st]][,pn])*MC[[st]][,pn]*Fpc[st,pn]))/sumMC else 0
          } # end st
      } # end pn

################################################################################
# 4. Allocate between Metabolism, growth and reproduction

    Cond.S.old<-elemState$Cond.S

		for (pn in 1:getSlot(element, "polygonsN")){

			for (st in 1:elemState$StageN){

        if(dSet$IsConsumer[st]){
				# partition between metabolism, growth and reproduction - this is per capita calculations from now on

         Met<-elemState$Cond.S[st,pn]*dSet$dMetab[[st]]*periodInfo$YearPropn

         Food<-Fpc.adj[st,pn]-Met
         Stress<-0
         if(Food<=0){ # lose gonad and then shrink
                elemState$Cond.R[st,pn] <- elemState$Cond.R[st,pn]+Food*dSet$dReprod[[st]]$cR
                Food<-0
                if(elemState$Cond.R[st,pn]<0){
                        Food <- elemState$Cond.R[st,pn]/dSet$dReprod[[st]]$cR
                        elemState$Cond.R[st,pn] <- 0
                   } # end if
              shrink<-abs(Food)*dSet$Starve[[st]]$costShrink

              ShrinkMax<-dSet$Starve[[st]]$ShrinkMax*periodInfo$YearPropn*elemState$Cond.S[st,pn]
              if(shrink>ShrinkMax){
                 Stress <- shrink-ShrinkMax
                 shrink <- ShrinkMax
                 }
              elemState$Cond.S[st,pn]<-elemState$Cond.S[st,pn]-shrink
              elemState$Cond.H[st,pn]<-elemState$Cond.H[st,pn]-Stress/elemState$Cond.S[st,pn]   # stress is maintained and accumulated with continued periods of food deficits

            } else { # grow gonad and body

           if(dSet$AllocateToReproduction){
              R<-dSet$dReprod[[st]]$pR * Food*dSet$dReprod[[st]]$cR
              Rmax<-dSet$dReprod[[st]]$Rmax*periodInfo$YearPropn*elemState$Cond.S[st,pn]

              G<-(1-dSet$dReprod[[st]]$pR) * Food*dSet$dGrowth[[st]]$cG
              Gmax<-dSet$dGrowth[[st]]$Gmax*periodInfo$YearPropn*elemState$Cond.S[st,pn]

              if(R>=Rmax & G>=Gmax){
                 R <- Rmax
                 G <- Gmax
                 } else {
                 if(R>=Rmax){
                    Fremain <- (R-Rmax)/dSet$dReprod[[st]]$cR
                    R <- Rmax
                    G <- G + Fremain*dSet$dGrowth[[st]]$cG
                    if(G>Gmax) G<-Gmax
                    } else {
                    if(G>=Gmax){
                       Fremain <- (G-Gmax)/dSet$dGrowth[[st]]$cG
                       G <- Gmax
                       R <- R + Fremain*dSet$dReprod[[st]]$cR
                       if(R>Rmax) R<-Rmax
                       } # end G
                    } # end R
                 } # end R & G

              elemState$Cond.S[st,pn]<-elemState$Cond.S[st,pn] + G
              elemState$Cond.R[st,pn]<-elemState$Cond.R[st,pn] + R
              elemState$Cond.H[st,pn]<-1  # condition returns to full health (=0) if growth has occurred
            } else { # if not allocate to reproduction

              G <- Food*dSet$dGrowth[[st]]$cG
              Gmax<-dSet$dGrowth[[st]]$Gmax*periodInfo$YearPropn*elemState$Cond.S[st,pn]
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
         NatStage<-elemState$Abundance[[2]][pn]*elemState$Stage[,pn]  # first part
         NatStage[Cond.S.old[,pn]>0]<- NatStage[Cond.S.old[,pn]>0]/Cond.S.old[(Cond.S.old[,pn]>0),pn]
         NatStage[!(Cond.S.old[,pn]>0)]<- 0
         BatStage<-NatStage*elemState$Cond.S[,pn]
         elemState$Abundance[[2]][pn] <- sum(BatStage)
         elemState$Stage[,pn]<-if(elemState$Abundance[[2]][pn]>0) BatStage/elemState$Abundance[[2]][pn] else 1/elemState$StageN

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


