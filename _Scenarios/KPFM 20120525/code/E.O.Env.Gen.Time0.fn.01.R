E.O.Env.Gen.Time0.fn.01<-function(
          ec       # module
         ,e        # element
         ,Universe              # access to universe if needed
          )                     # end input parameters

# Function:           E.O.Env.Gen.Time0.fn.01.R
# Description:        Initialise general environment at the beginning of a trial
#                     
# Input parameters
#
#
#    Factor$Signature <- list(
#      ID           = ,
#      Name.full    = "",
#      Name.short   = "",
#      Morph        = "",
#      Version      = "01",
#      Authors      = "A.Constable",
#      last.edit    = "7 April 2008"
#      ) # end Signature

################################################################################
{ # start function
  Elmnt<-Universe[[ec]][[e]]
  Nrec<-Elmnt$Fns$Trial.setup$dset$Initialise.records

  if (Nrec>0){
    for (Rec in 1:Nrec){
      Record<-scan(file = Elmnt$Data$FilePath
              ,sep = ","
              ,nlines = 1)
      Elmnt$State$PolygonEnv<-
          Record[Elmnt$Data$RecordElements]
      }
  } else {
      Elmnt$State$PolygonEnv<-
         rep(NA,Elmnt$Polygons$Polygon.N)
  }
  
} # end function

################################################################################
