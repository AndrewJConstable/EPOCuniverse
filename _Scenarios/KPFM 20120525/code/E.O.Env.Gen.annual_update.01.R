E.O.Env.Gen.annual_update.01<-function(
                    Action,     # ActionMat row
                                  # Col  1  = module
                                  # Col  2  = element
                                  # Col  3  = period
                                  # Col  4  = reference day in year
                                  # Col  5  = action reference number in period (NA if no actions)
                                  # Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions)

                    PeriodInfo, # information about the active period for use in subroutines
                                  # Number      = eTSD
                                  # Day         = PropYear[eTSD,1]
                                  # KnifeEdge   = if(PropYear[eTSD,2]==0) FALSE else TRUE
                                  # YearPropn   = PropYear[eTSD,3]
                                  # PeriodStart = PreviousDay/365 # proportion of year passed since 0 Jan
                                  #                               # to beginning of time period
                                  # PeriodEnd   = PreviousDay/365+PropYear[eTSD,3]
                    Universe               # access to universe if needed

                    ) # end parse

# function to update the state of the KPFM environment from file
#
{ # begin function
  Factor<-Universe[[Action[1]]][[Action[2]]]

#read next line of file

EnvData<-scan(file = Factor$Data$FilePath, what = double(0), sep = ",",
     dec = ".",nlines = 1)

if(EnvData[1]!=Universe$Config$RealTimeState$CurrentTrial |
   EnvData[2]!=Universe$Config$RealTimeState$CurrentYear) stop("Environment data in file is being read out of sync with run time trial-year combination")

Factor$State$PolygonEnv<-EnvData[Factor$Data$RecordElements]

  } # end function

