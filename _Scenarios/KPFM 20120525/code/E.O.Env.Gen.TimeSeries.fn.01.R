E.O.Env.Gen.TimeSeries.fn.01<-function (Config,EnvAttributes)

# Function:           E.O.Env.Gen.TimeSeries.fn.01.R
# Description:        Environmental Element, Group Ocean,
#                     Environment for modifying General Environmental Variable
# Primary attributes: 


#   ############################################################################
{ # start function
#   ############################################################################

#   Factor - create new R environment & note details of routine

cat("General Environment - create environment","\n",sep="")

    Factor<-new.env()

#   Signature is a unique identifier for this element
#         number applicable to SST where 3000 series

    Factor$Signature <- list(
      ID           = 12001,
      ID.absolute  = 12001,
      Name.full    = "Environment General time series",
      Name.short   = "Env_gen_TS",  # for use in naming file
      Morph        = "Gen",
      Version      = "01",
      Authors      = "A.Constable",
      last.edit    = "23 Jan 2015"
      ) # end Signature

#   ############################################################################
#   Factor$Polygons - definition


    Factor$Polygons<-EnvAttributes$Polygons

#   number of polygons

    Factor$Polygons<-c(Factor$Polygons,list(Polygon.N = length(Factor$Polygons$RefNumbers)))

#   ############################################################################
#   Factor$Birthday - point of origin for beginning of annual cycle
#
    Cday<-EnvAttributes$Birthday$Day
    Cmonth<-EnvAttributes$Birthday$Month

    Factor$YearStart<-U.ST.DayFromDate.01(Cday,Cmonth)/365

#   ############################################################################
#   Routines to prepare for initialising Factor
#   ############################################################################

#   create file of general environments to be used in run time

cat("General Environment - setting file of general environments","\n",sep="")


#   #################################################
#   Trial characteristics

    TrialsN<-Config$Trials$TrialsN
    YearStart<-Config$Trials$YearStart # first year of simulation - Year 0 is year before
    YearEnd<-Config$Trials$YearEnd
    YearsN<-YearEnd-YearStart+1+EnvAttributes$Initialise$Records
    PolygonsN<-Factor$Polygons$Polygon.N
    PolygonNames<-Config$Polygons$Polygon.Names
    
    FileName<-paste(Config$Trials$RootDir,EnvAttributes$RunFileCode,".dat",sep="")

#   ############################################################################
#   establish path by opening file to read - if not present then create file


      cat ("Searching for general environment file at","\n",sep="")
      cat ("   ",FileName,"\n",sep="")
      cat ("\n",sep="")

      FileExists<-file.exists(FileName)

    ########################################
    if (!FileExists | EnvAttributes$ReplaceFile){ # create new/replace file
    
        cat("General Environment - creating new file","\n",sep="")
        FilePath<-file(description = FileName, open = "w")
      # write header information
        cat("General Environment : ",as.character(EnvAttributes$RunFileCode),"\n",sep="",file = FilePath)
        cat("Polygons          : ",as.character(PolygonsN),"\n",sep="",file = FilePath)
        cat("Trials            : ",as.character(TrialsN),"\n",sep="",file = FilePath)
        cat("Years per trial   : ",as.character(YearsN),"\n",sep="",file = FilePath)
        cat("   Initialise     : ",as.character(EnvAttributes$Initialise$Records),"\n",sep="",file = FilePath)
        cat("   First Year     : ",as.character(YearStart),"\n",sep="",file = FilePath)
        cat("   Last Year      : ",as.character(YearEnd),"\n",sep="",file = FilePath)
        cat("Data","\n",sep="",file = FilePath)
        cat("Trial","Year",PolygonNames[Factor$Polygons$RefNumbers],sep=",",file = FilePath)
        cat("\n",file = FilePath)

#       generate region time series of environments

            # region series
            RegionalSeriesFn   = source(file=EnvAttributes$Functions$RegionalSeries)[[1]]
            Env_region<-RegionalSeriesFn((YearStart-EnvAttributes$Initialise$Records),YearEnd,EnvAttributes$EnvParams)

#       generate random variation in region environment
          # - get random number sequence for years in trial if needed
          # - return matrix (rows = trials, columns = years)

            if(EnvAttributes$Region_Variation$UseRandSeq) {
                  if(!is.na(EnvAttributes$Region_Variation$Seed)){
                                 set.seed(EnvAttributes$Region_Variation$Seed)
                                 }
                  LogSD<- sqrt(log(1+EnvAttributes$Region_Variation$CV^2))
                  Rvector<-exp(rnorm((YearsN*TrialsN),mean=0,sd=LogSD))
                } else {
                  Rvector<-exp(rep(0,YearsN*TrialsN))
                }
            Rvector<-matrix(Rvector,ncol=YearsN,nrow=TrialsN)

#       generate polygon scaling coefficients & write to file

            if(!is.na(EnvAttributes$Polygon_Variation$Seed)){
                                 set.seed(EnvAttributes$Polygon_Variation$Seed)
                                 }
            Year0<-YearStart-EnvAttributes$Initialise$Records-1
            for (tr in 1:TrialsN){
              for (yr in 1:YearsN){
                if(EnvAttributes$Polygon_Variation$UseRandSeq) {

                      VarCovMat<-crossprod(diag(EnvAttributes$Polygon_Variation$Var),t(diag(EnvAttributes$Polygon_Variation$Var)%*%EnvAttributes$Polygon_Variation$CorrMat))
                # R version 3.1.2 does not have mvrnorm in the MASS library.  Therefore using mvtnorm library with function rmvnorm
                    #  Pvector<-mvrnorm(n=1,mu=EnvAttributes$Polygon_Variation$ScaleCoeff,VarCovMat)
                      Pvector<-rmvnorm(n=1,mean=EnvAttributes$Polygon_Variation$ScaleCoeff,sigma=VarCovMat)
                      } else {
                      Pvector<-EnvAttributes$Polygon_Variation$ScaleCoeff
                    }
                Evector<-c(tr,(Year0+yr))
                Evector<-c(Evector,Env_region[yr]*Rvector[tr,yr]*Pvector)

                cat(as.character(Evector),sep=",",file = FilePath)
                cat("\n", file = FilePath) # next line

                } # end yr
              } # end tr
              
      close(FilePath)

      } # end create file
      ####################
      
# check file for valid contents by reading header information
#    then read lines until first record for use in trials

   cat("General Environment - checking file and making ready for trials","\n\n",sep="")

   FilePath<-file(description = FileName, open = "r")

   cat("     Opened General Environment File","\n\n",sep="")
   UseFile<-TRUE
   Line<-readLines(con = FilePath, n = 1)
      print(Line)
   Line<-readLines(con = FilePath, n = 1)
      print(Line)
      if (as.integer(substring(Line, 21, last = 1000000))!=PolygonsN) UseFile<-FALSE
   Line<-readLines(con = FilePath, n = 1)
      print(Line)
      if (as.integer(substring(Line, 21, last = 1000000))!=TrialsN) UseFile<-FALSE
   readLines(con = FilePath, n = 2)
   Line<-readLines(con = FilePath, n = 1)
      print(Line)
      if (as.integer(substring(Line, 21, last = 1000000))!=YearStart) UseFile<-FALSE
   Line<-readLines(con = FilePath, n = 1)
      print(Line)
      if (as.integer(substring(Line, 21, last = 1000000))!=YearEnd) UseFile<-FALSE
   readLines(con = FilePath, n = 2)
  if (!UseFile) stop("General Environment file incorrect - program terminated")

  cat("\n","     General Environment - file ready to read","\n\n",sep="")

# add filepath to Outfiles list - for closing at end
  Config$Outfiles<-c(Config$Outfiles,list(EnvAttributes = list(FileName = FileName, Path = FilePath)))

# ##############################################################################
# set up Factor environment with functions
# ##############################################################################


#   ############################################################################
#   Factor$State - initial characteristics

cat("General Environment - declare State","\n",sep="")


# declare State

    Factor$State<-list(
       PolygonEnv = EnvAttributes$Polygons # fill with polygon numbers as placeholders
      ,Transition = FALSE  # is a transition environment used for this element
       ) # end list of State

# declare Data - this is the place for active data and file management

    Factor$Data<-list(
       FilePath = FilePath  # file is open through FilePath
      ,FileName = FileName
      ,RecordElements = c(3:(2+Factor$Polygons$Polygon.N))
       ) # end list of Data


#   #############################################################
#   Factor$TimeSteps

cat("General Environment - set TimeSteps","\n",sep="")

#   the characteristics of a time step between the previous time and the specified time (in days)
#   is given in a list(days in calendar year, number of functions to be carried out, list of named functions)
#   knife-edge functions can be included by repeating the same day

# Action 2 - Update environment at specified time of year


    Factor$TimeSteps<-list(
        Break_point  = list(  # knife-edge step for general env.
                      calday    = U.ST.DayFromDate.01(01,10)
                     ,actions.N = 1
                     ,actions   = list(
                                  Print.State = list(fn     = source(file=EnvAttributes$Functions$StatePrint)[[1]]
                                                    ,TS_type = "FirstPeriod" # "AllPeriods","FirstPeriod","LastPeriod") input KnifeEdge as LastPeriod
                                                    ,TS_timing = "Before"    # "Before","During","After"
                                                    ,related.elements = NULL
                                                    ,dset   = list(  # List because may need more than one file to print state
                                                        State = list(output = EnvAttributes$OutputFlags$PrintState
                                                                    ,fname  = paste(Config$Trials$RootDir,EnvAttributes$OutputFiles$State,sep="")
                                                                    ,path   = NULL
                                                                     ) # end State
                                                                   ) # end data list
                                                      ) # end Print.State

                                  ) # end actions
                      ) # end time step
      ,Set_env  = list(       # NB - this was "Set_Krill_rec_env"  - will appear as a problem in KPFM runs if not changed
                     calday    = U.ST.DayFromDate.01(01,10)
                    ,actions.N = 1
                    ,actions   = list(update.annual.environments =
                                      # function reads next record, checking correct trial and year
                                  list(fn = source(file=EnvAttributes$Functions$UpdateAnnualEnv)[[1]]
                                      ,TS_type = "LastPeriod" # "AllPeriods","FirstPeriod","LastPeriod")
                                      ,TS_timing = "After"    # "Before","During","After"
                                      ,related.elements = NULL # matrix - col 1 = module; col 2 = absolute ID of element; if none then NULL
                                      ,dSet      = NULL)       # pattern for input to function
                                  ) # end actions list
                     ) # end time step

        ) # end list of timesteps

    # number of timesteps

     Factor$TimeSteps.N<-length(Factor$TimeSteps)


#   #############################################################
#   Ancillary functions
#   #############################################################

cat("General Environment - set Ancillary functions","\n",sep="")

    Factor$Fns<-list(

      # function to undertake element-specific setup of actions
      # e.g.  setup         = list (ContEnv = list(fn = NULL, dset = NULL))

        setup         = NULL

      # data and function to initialise element at the beginning of each trial
      #   i.e. how should the element be reset at time 0

       ,Trial.setup = list(fn   = source(file=EnvAttributes$Functions$TrialSetup)[[1]]
                          ,dset = list(
                              Initialise.records = EnvAttributes$Initialise$Records  # number of records in file to read for initialisation
                                ) # end data list
                                ) # end trial.setup
       ,State.print = list(fn     = source(file=EnvAttributes$Functions$StatePrint)[[1]]
                          ,dset   = list(  # List because may need more than one file to print state
                                        State = list(output = EnvAttributes$OutputFlags$PrintState
                                                      ,fname  = paste(Config$Trials$RootDir,EnvAttributes$OutputFiles$State,sep="")
                                                      ,path   = NULL
                                                  )
                                ) # end data list
                                ) # end State.print
        ) # end function list

#   set placeholder for transition states - note that Update is checked at the end of each period to see if
#   the State needs updating. If FALSE then overlooked.

    Factor$Transition<-list(Update = FALSE)
    
#   return Factor environment

cat("General Environment - end setup","\n",sep="")

      Factor

} #     end factor

###############################################################################
