#inc <- c("#include <EPOC.h>")
#cppa <- c(paste("-I", system.file("include", package="EPOC"), sep=""))
#liba <- c("-lEPOC", paste("-L", system.file("libs", "i386", package="EPOC"), sep=""))
#sig <- signature(sexpObj="Krill", universe="Universe")
code<-"
Rcpp::List action = getRTState(universe, \"currentAction\");
Rcpp::List periodInfo = getRTState(universe, \"currentPeriodInfo\");

SEXP elemState = getState(sexpObj);
SEXP elemTrans = getTransition(sexpObj);
SEXP elemTimesteps = getTimestep(sexpObj);

epocVerboseMessage(universe, \"Doing stuff...\");
epocVerboseMessage(universe, getSignatureMulti(sexpObj, FALSE));
return R_NilValue;
"
setEPOCCPPMethod("doStuff", "Krill", body=code)

#setCPPMethod("doStuff", sig, body=code, includes=inc, cppargs=cppa, libargs=liba, Rcpp=TRUE)