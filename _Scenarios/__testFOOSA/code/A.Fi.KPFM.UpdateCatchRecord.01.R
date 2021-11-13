#A.Fi.KPFM.UpdateCatchRecord.01<-function(
if (!isGeneric("updateCatchRecord")) 
setGeneric("updateCatchRecord", function(element, universe) standardGeneric("updateCatchRecord"))
setMethod("updateCatchRecord", signature(element="Fishery", universe="Universe"), 
    function(
		element,
		universe	# access to universe if needed
		)     
    {
		# Description:        General activities fisheries function -
		
		# Instructions for use
		#   Updates Catch vector by polygon
		#   Efficiency is given for each fished taxon in a list
		#        with each taxon having a matrix cols= 1 Realised catch, 2 = potential max catch
		#        and rows = polygons
		#        when reported, this is by taxon,polygon,Realised Catch/Potential max catch
		# 	Data are accumulated from Transition matrix - these data are converted to biomass

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
		#     10. stage size
		#     11. calendar year
		#     12. period
		#     13. proportion of year

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
		catchData <- getTransition(element, "Consumption")
		#-------------------------------------------------------------------------------
		# by fished species by polygon
		for (fi in 1:elemState$TargetN) {
			targetRE <- elemState$TACtaxa[1,]
			catchDataFI <- catchData[catchData[,3]==targetRE[1] & catchData[,4]==targetRE[2],]

			if (!is.null(catchDataFI)) {
				for (pn in 1:getSlot(element, "polygonsN")){
					catchPNrealised <- sum(catchDataFI[catchDataFI[,1]==pn,8] * catchDataFI[catchDataFI[,1]==pn,10])
					catchPNmax <- sum(catchDataFI[catchDataFI[,1]==pn,9] * catchDataFI[catchDataFI[,1]==pn,10])

					# add results to State
					#   Updates Catch vector by polygon in a list of taxa
					#   Efficiency is given for each fished taxon in a list
					#           with each taxon having a matrix cols= 1 Realised catch, 2 = potential max catch
					#           and rows = polygons
					#        when reported, this is by taxon,polygon,Realised Catch/Potential max catch
					elemState$Catch[fi,pn] <- elemState$Catch[fi,pn] + catchPNrealised
					elemState$Efficiency[[fi]][pn,1] <- elemState$Efficiency[[fi]][pn,1] + catchPNrealised
					elemState$Efficiency[[fi]][pn,2] <- elemState$Efficiency[[fi]][pn,1] + catchPNmax
				}
			}
		}

		# Update Transition and State
		setTransition(element, "Consumption", NULL)
		setState(element, value=elemState)
	}
)
