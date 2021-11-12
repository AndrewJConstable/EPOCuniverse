# Ecosim test routines
# by Andrew Constable
# May 2011
#
# derived from Walters & Martell (2004)

SolveODE_AdamsBashforth<-function(
                             DEFUNC             # function of ODEs
                            ,x                  # initial values of x to solve
                            ,Xmin               # vector of minimum values permitted in x
                            ,Params             # parameters that are input to DEFUNC
                            ,DEFUNCvals         # values calculated in DEFUNC that need to be retained for subsequent iterations - the values parsed into this function are the initial values for t0
                            ,Time0=0            # time at start of interval
                            ,TimeEnd=1          # time at end of interval
                            ,TimeStepMin=2E-6   # minimum time step for equal interval
                            ){ # return x, Time (vector of times during interval), ODEresults (col = time step, row = variable)
  # Walters version of Adams-Bashforth - see box 6.1, p 134)

  # notes
  #        ? need to deal with fractions of TimeStep in weighting the derivative in the last time step

    # 1. set up time matrix
        # equal time steps for now

        TimeInterval<-TimeEnd-Time0
        TimeStepsN<-ceiling(TimeInterval/TimeStepMin) # round up and make the adjustment for incomplete timestep at end in the Time matrix
        TimeStepRef<-c(1:(TimeStepsN+1))
        TimeSteps<-TimeStepRef*TimeStepMin
        Time<-cbind(# matrix of time steps
               TimeStepRef   # col 1 = number of the timestep
              ,c(Time0,(Time0+(TimeStepRef[c(1:(TimeStepsN-1))]*TimeStepMin)),TimeEnd)  # col 2 = time at beginning of step
              ,c(rep(TimeStepMin,(TimeStepsN-1)),(TimeInterval-((TimeStepsN-1)*TimeStepMin)),0) # col 3 = length of time step
                 )

    # 2. set up ODE results & add to Data
        ODEresult<-matrix(NA,nrow=length(x),ncol=(TimeStepsN+1))          # create matrix of ODE values for all time steps + 1 (the last row will remain NAs but be compatible with dx at time 0 and no dx at end time).
        XperStep<-matrix(NA,nrow=length(x),ncol=(TimeStepsN))          # create matrix for X after each time step
        Xstart<-x

    # 3. loop through timesteps calling FUNC
         # Timestep 1 - Euler method
                DEFUNCvals<-DEFUNC(x,Params,DEFUNCvals,Time,1)
                ODEresult[,1]<-DEFUNCvals$fx
                x<-x+DEFUNCvals$fx*Time[1,3]
                x[x<Xmin]<-Xmin[x<Xmin]
                XperStep[,1]<-x

         # Timestep 2 - Adams-Bashforth approximation of change
            if(TimeStepsN>1){
              for (TimeStep in 2:TimeStepsN){
              
#              cat('Timestep : ',TimeStep,sep="","\n")
#              cat('X0 =',x,sep="  ","\n")
                DEFUNCvals<-DEFUNC(x,Params,DEFUNCvals,Time,TimeStep)
                ODEresult[,TimeStep]<-DEFUNCvals$fx
                x<-x+Time[TimeStep,3]/2*(3*DEFUNCvals$fx-ODEresult[,(TimeStep-1)])
                x[x<Xmin]<-Xmin[x<Xmin]
#              cat('X1 =',x,sep="  ","\n")
                XperStep[,TimeStep]<-x
                }
              } # end if
        
      list(x = x, Time = Time[,2], XperStep = cbind(Xstart,XperStep),fx = ODEresult)
      
      } # end ODE function
      
      
