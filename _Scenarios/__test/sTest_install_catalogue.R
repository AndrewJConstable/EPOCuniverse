# Scenario Catalogue
# Scenario: test
# Operator: A. Constable
# Date: 2015 (this catalogue generated in 13 November 2021)

# This code includes the following steps:
#    i) designation of the necessary 'code' and 'data' 
#       for use in the scenario.  
#   ii) copying of files to the appropriate directories.
#       Copying only occurs if the files do not exist.  
#       Dates of original files are retained. 

#######################################
# 1. Designation of code and data files
#
# Designation is in a list
# Level 1: folders for the scenario
# Level 2: paths to the original files
# Level 3: vector of file names for copying

EPOCscenario<-list(
   code = list(EnvCodeFiles = c("E.O.Env.EnvGenVar.01.R"  # Main function for generating environmental data
                               ,"E.O.Env.EnvGenVar.envUpdate.01.cpp.R"  # 
                               ,"E.O.Env.EnvGenVar.envUpdate.01.R"  # 
                               ,"E.O.Env.EnvGenVar.printState.01.R"  # 
                                ) # end EnvCodeFiles
              ,paths = c("../../1 Environment/Env General EPOC/")# end paths
              ) # end code
  ,data = list(UniverseDataFiles = c("Universe.data.R","Spatial.data.R","Scenarios.data.R")
              ,EnvDataFiles = c("E.O.Env.EnvGenVar.data.01.R")
              ,paths = c("../../0 Universes/uTest/"
                        ,"../../1 Environment/Env General EPOC/")# end paths
               ) # end data
) # end list


#######################################
# 2. Copy files
#
# copy code and input data files to the appropriate subdirectories in this working directory

sapply(names(EPOCscenario),function(d,Es){
  path_to <- paste("./",d,"/",sep="")
  if(!dir.exists(path_to)) dir.create(path_to)
  EsL<-Es[[d]]
  sapply(seq(1,length(EsL$paths),1)
         ,function(i,EsL,path_to){
           path_from <- EsL$paths[i]
           sapply(EsL[[i]],function(f,pt,pf){
             file.copy(paste(pf,f,sep="")
                       ,paste(pt,f,sep="")
                       ,overwrite = FALSE, copy.date = TRUE)
           },path_to,path_from)
   },EsL,path_to)# end sapply level 2 
  },EPOCscenario) # end sapply Level 1




# loop through list

