Fn <- function(datafram, npred, distr){
       # set global parameters

        dep <- datafram[,1]
        aleatoire <- datafram[,2]
        aleatoire2 <- datafram[,5]
        Lon <- datafram[,3]
        Lat <- datafram[,4]
        dd <- datafram[,6:(npred+5)]

        tt_start <- cbind(dep,aleatoire, aleatoire2, Lon, Lat)

        # for use in creating output vector of variables names
            vnbase <- rep(NA,npred)
            Names<-paste("X", c(1:npred), sep="")
            vnbase<-as.data.frame(matrix(vnbase,nrow=1))
            print(vnbase)
            print(Names)
            names(vnbase)<-Names

            NamesResults1<-NULL
            for (m in 1:npred) NamesResults1<-c(NamesResults1,paste("coef_var", m, sep=""),paste("se", m, sep=""))

      ##########################################################
      # loop (or apply) through models to produce output

        tot <- NULL

        for(m in 0:npred){

           #############################
           # generate formula for model

           if(m==0) Model<-1 else Model<- paste("V", (1:m), sep="")
           Formula<- as.formula(paste("dep ~ ", paste(Model, collapse= "+")))

           print(Formula)

            #############################
            # do null model (m=0) differently (no combinations)
               if (m==0) {
                    vn <- vnbase   # Variables names
                    glmm <- lme(Formula, random= ~1|aleatoire2/aleatoire, correlation=corExp(form=~Lon+Lat|aleatoire2/aleatoire, nugget=TRUE), na.action=na.omit, data=tt)

                    results <- c(vn, AIC=as.numeric(AIC(glmm)),
                        int=as.numeric(glmm$coefficients$fixed[1]),se_int=sqrt(diag(vcov(glmm)))[1])
                    results1<-rep(NA,npred)
                    results1<-data.frame(results1)
                    names(results1)<-NamesResults1

                    tot <- rbind(tot, c(results, results1, data.frame(np=2)))
                    } # end m==0


            #############################
            # for other models (m>0)
                if (m>0) {

                   # generate combinaison
                     it <- combn(colnames(dd), m)
                     nc <- ncol(it) # number of columns

                   # Loops through each combination
                     for(nc1 in 1:nc){
                          # create the dataset
                          vn <- vnbase   # Variables names
                          tt <- tt_start
                          for (m1 in 1:m){
                             xx <- data.frame(dd[,colnames(dd)==it[m1, nc1]])
                             colnames(xx) <-paste("Var", m1, sep="")
                             tt <- cbind(tt, xx)
                             vn[m1] <- it[m1,nc1]
                             } # end m1
                              
                          glmm <- lme(Formula, random= ~1|aleatoire2/aleatoire, correlation=corExp(form=~Lon+Lat|aleatoire2/aleatoire, nugget=TRUE), na.action=na.omit, data=tt)

                          results <- c(vn
                                     , AIC=as.numeric(AIC(glmm))
                                     ,int=as.numeric(glmm$coefficients$fixed[1])
                                     ,se_int=sqrt(diag(vcov(glmm)))[1]
                                     ) # end results 1st line
                            results1<-NULL
                            for (m1 in 1:npred){
                               if(m1 > m) results1 <- c(results1, NA, NA)
                                else       results1 <- c(results1, as.numeric(glmm$coefficients$fixed[m1]), sqrt(diag(vcov(glmm)))[m1])
                                } # end m1
                            results1<-data.frame(results1)
                            names(results1)<-NamesResults1

                            tot <- rbind(tot, c(results, results1, data.frame(np=(m+2))))

                            } # end nc1 - end combination
                      } # end m>0

              } # end m

            tot # return results
            } # end function


