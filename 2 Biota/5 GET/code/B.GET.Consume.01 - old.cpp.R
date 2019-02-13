#B.GET.Consume.01

#####?????? need to change because Stage is no longer in a list by polygon with a 2 column matrix (col 1 = stage number, col 2 = prop at stage)
#####??????    Stage is now a matrix (rows = stage, cols = polygons)

code<-"
using namespace Rcpp;

NumericVector action = getRTState(universe, \"currentAction\");
List periodInfo = getRTState(universe, \"currentPeriodInfo\");
List elemState = getState(sexpObj);
List elemTrans = getTransition(sexpObj);
List actionTS = as<List>(as<List>(getTimestep(sexpObj, action[3]))[\"actions\"])[action[5]];
List dSet = actionTS[\"dset\"];

NumericVector stgUnitsVec = intersect(as<NumericVector>(elemState[\"Abundance\"]), as<NumericVector>(elemState[\"StageStrUnits\"]));

for ( int pop = 0 ; pop < Rcpp::as<int>(getSlot(sexpObj, \"polygonsN\")) ; pop++ ) {
	NumericMatrix stagePop = NumericMatrix((as<List>(elemState[\"Stage\"]))[pop]);
	NumericVector stgPabund = stgUnitsVec[pop] * stagePop.column(2);
	for ( int st = 0 ; st < as<int>(elemState[\"StageN\"]) ; st++ ) {
		NumericMatrix propFeedMat = NumericMatrix(as<NumericVector>(as<List>(dSet[st])[\"PropFeedInPolygon\"]));
		double prop = propFeedMat.row(pop);
		if ( prop >= 0 ) {
			// vector of products for use below
			//SEXP propAbund = prop
			epocMessage(sexpObj, prop);
		}
	}
}

return R_NilValue;
"
setEPOCCPPMethod("consume", "GET", body=code)
