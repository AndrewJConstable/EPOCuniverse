		initAbund <- getAttribute(.Object, "Init.abundance")
  
		Init.Stage.Str<-NULL
		mortality <- getAttribute(.Object, "Mortality")
		stage <- getAttribute(.Object, "Stage")
		
		for (pn in 1:getSlot(.Object, "polygonsN")){
			
			Stage.Str <- getAttribute(.Object, "Reproduction")$alpha[pn] # pups
			for(j in 2: stage$JuveAgeN) {
				Stage.Str <- c(Stage.Str,Stage.Str[j-1]*
                                       exp(-(mortality[[1]][[(j-1)]]$M[pn] +
                                             mortality[[2]][[(j-1)]]$M[pn])))
			}

			# note that the start of the simulation is Jan 1 therefore all population is breeding
			# nonbreeders (sum to infinity from (JuveAgeN+1)
			Stage.Str <- c(Stage.Str,0)

			# breeders
			Stage.Str <- c(Stage.Str,Stage.Str[stage$JuveAgeN]*
                       exp(-(mortality[[1]][[stage$JuveAgeN]]$M[pn]+
                             mortality[[2]][[stage$JuveAgeN]]$M[pn]))/
                       (1-exp(-(mortality[[1]][[(stage$JuveAgeN+1)]]$M[pn]+
                             mortality[[2]][[(stage$JuveAgeN+1)]]$M[pn])))
                       )

			# frequency distribution
			Stage.Str <- Stage.Str/sum(Stage.Str)
			Stage.Str <- cbind(c(1:length(Stage.Str)),Stage.Str)
			Init.Stage.Str <- c(Init.Stage.Str,list(Stage.Str))
      
			# now adjust abundance and biomass to reflect whole population even though abundances in
			# data file are just for breeders/non-breeders
			initAbund[pn] <- initAbund[pn]/sum(Stage.Str[c((stage$JuveAgeN+1):(stage$JuveAgeN+2)),2])
		}

)