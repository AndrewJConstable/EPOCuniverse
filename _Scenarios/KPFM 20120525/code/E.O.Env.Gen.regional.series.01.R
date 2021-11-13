E.O.Env.Gen.regional.series.01<-function(Year1,YearLast
                  ,EnvParams=list(
                            Year0 = 1900
                           ,a = list(slope = 0.0, int = 1.0)
                           ,p = list(slope = 0.0, int = 1.0)
                           ,f = list(slope = 0.0, int = 1.0)
                           ,Xmin = list(slope = 0.0, int = 1))
                  ) # end call
    { # start function

# Function:           E.O.Env.Gen.regional.series.01.R
# Description:        Generate a time series of X in the generalised environment
# Input parameters
#   Year1 = first year in series
#   YearLast = last year in series
#   EnvParams = parameters to determine scaling value
#          for the region in a given year

#    Factor$Signature <- list(
#      ID           = ,
#      Name.full    = "",
#      Name.short   = "",
#      Morph        = "",
#      Version      = "01",
#      Authors      = "A.Constable",
#      last.edit    = "23 January 2015"
#      ) # end Signature


    YearsN<-YearLast-Year1+1
    YearsToYear0<-(Year1-EnvParams$Year0)
    Series<-rep(1,YearsN)
    tprime_firstYear<-YearsToYear0+2
    tprime<-0.00
    for (i in (YearsToYear0+1):(YearsToYear0+YearsN)){

      # scale parameters
      p<-EnvParams$p$slope*i+EnvParams$p$int
      a<-EnvParams$a$slope*i+EnvParams$a$int
      f<-EnvParams$f$slope*i+EnvParams$f$int
      Xmin<-EnvParams$Xmin$slope*i+EnvParams$Xmin$int

      if((EnvParams$f$slope>0.00 | EnvParams$p$slope>0.00)
          & i>=tprime_firstYear){ # solve for tprime
            # first determine if the (t-1) position was on the rise or fall of the cycle
            # then search for tprime on the respective rise or fall of the new cycle
            # bracketed by the maximum and minumum
              OldPhase<-(((i-1)+tprime+f_old)/p_old)
              OldPiFraction<-OldPhase-2*floor(OldPhase/2)
              NewPhase<-(((i-1)+f)/p)
              NewPiFraction<-NewPhase-2*floor(NewPhase/2)

                if (OldPiFraction>=0.5 & OldPiFraction<=1.5){ # on fall of cycle
                    Fall<-TRUE
                    tprime_min<-(0.5-NewPiFraction)
                    tprime_max<-(1.5-NewPiFraction)
                  } else { # on rise of cycle
                    Fall<-FALSE
                    if (NewPiFraction<0.5) {
                        tprime_min<-(-0.5-NewPiFraction)
                        tprime_max<-(0.5-NewPiFraction)
                      } else {
                        tprime_min<-(1.5-NewPiFraction)
                        tprime_max<-(2.5-NewPiFraction)
                      }
                  }

         if(Series[(i-YearsToYear0-1)]>(2*a+Xmin) | Series[(i-YearsToYear0-1)]<Xmin) { #Xt-1 is outside range of new function then snap to max or min
              if(Series[(i-YearsToYear0-1)]>(2*a+Xmin)) {
                tprime<-ifelse (Fall,tprime_min,tprime_max)
                } else {
                tprime<-ifelse (Fall,tprime_max,tprime_min)
                }
                
              } else { # search for Xt-1 in new function
                 tprime<-optimise(function(tprime,a,PiFraction,Xmin,X_t_minus_1)
                            abs((a*(sin((PiFraction+tprime)*pi)+1)+Xmin)-X_t_minus_1)
                         ,interval = c(tprime_min,tprime_max)
                         ,a=a
                         ,PiFraction=NewPiFraction
                         ,Xmin=Xmin
                         ,X_t_minus_1=Series[(i-YearsToYear0-1)])
              } # end else
         tprime<-tprime[[1]]*p # convert pi fraction into years

         } # end if


      Series[i-YearsToYear0] <- ifelse((p>0),(a*(sin((i+tprime+f)*pi/p)+1)+Xmin),Xmin)
      
      a_old<-a
      p_old<-p
      f_old<-f
      Xmin_old<-Xmin
      
      } #end i
    Series
    } # end function

#######################################################################
  