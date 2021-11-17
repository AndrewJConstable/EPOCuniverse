# Create S4 method 'envUpdate'
#E.O.Env.EnvGenVar.envUpdate.01.cpp
code<-"
#include <string>

using namespace Rcpp;

int rYear = as<int>(getRTState(universe, \"relativeYear\"));
//read next line of file
const char* fileName = as<const char*>(getRuntimePath(universe, getAttribute(element, \"RuntimeFile\")));
SEXP fileConn = getFileConnection(element, \"RuntimeFile\", fileName, \"r\");
NumericVector envData = fromCSVCharacter(readFileConnection(element, fileConn, 10+rYear), \"double\");
if(envData[0] != as<int>(getRTState(universe, \"currentTrial\")) || envData[1] != as<int>(getRTState(universe, \"currentYear\"))) {
	epocErrorMessage(element, \"Environment data in file is being read out of sync with run time trial-year combination\", TRUE);
}

IntegerVector recElements = getSlot(element, \"recordElements\");
NumericVector polyEnv; 
for ( int i = 0 ; i < recElements.size() ; i++ ) polyEnv.push_back(envData[recElements[i]]);

setState(element, \"PolygonEnv\", polyEnv);
"
setEPOCCPPMethod("envUpdate", "EnvGenVar", body=code)