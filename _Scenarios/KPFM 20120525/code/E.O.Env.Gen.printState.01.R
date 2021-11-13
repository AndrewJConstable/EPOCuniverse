E.O.Env.Gen.printState.01<-function(
                    Action     # ActionMat row
                                  # Col  1  = module
                                  # Col  2  = element
                                  # Col  3  = period
                                  # Col  4  = reference day in year
                                  # Col  5  = action reference number in period (NA if no actions)
                                  # Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions)

                   ,PeriodInfo # information about the active period for use in subroutines
                                  # Number      = eTSD
                                  # Day         = PropYear[eTSD,1]
                                  # KnifeEdge   = if(PropYear[eTSD,2]==0) FALSE else TRUE
                                  # YearPropn   = PropYear[eTSD,3]
                                  # PeriodStart = PreviousDay/365 # proportion of year passed since 0 Jan
                                  #                               # to beginning of time period
                                  # PeriodEnd   = PreviousDay/365+PropYear[eTSD,3]
                   ,Universe               # access to universe if needed
                    ) # end declaration

#    Factor$Signature <- list(
#      ID           = ,
#      Name.full    = "",
#      Name.short   = "",
#      Morph        = "",
#      Version      = "01",
#      Authors      = "A.Constable",
#      last.edit    = "16 April 2008"
#      ) # end Signature

{  # start function

ElState<-Universe[[Action[1]]][[Action[2]]]$Fns$State.print

if(ElState$dset$State$output) {
  ElState$dset$State$path<-file(ElState$dset$State$fname, "a")
  # print line in file - Trial,Year,Day,vector of polygon values of environment

  cat(as.character(c(Universe$Config$RealTimeState$CurrentTrial
                    ,Universe$Config$RealTimeState$CurrentYear
                    ,PeriodInfo$PeriodStart
                    ,Universe[[Action[1]]][[Action[2]]]$State$PolygonEnv
                    ))
     ,sep=",",file = ElState$dset$State$fname,append=TRUE)
  cat("\n"
     ,sep="",file = ElState$dset$State$fname,append=TRUE)

  close(ElState$dset$State$path)
  } # end State
} # end function
