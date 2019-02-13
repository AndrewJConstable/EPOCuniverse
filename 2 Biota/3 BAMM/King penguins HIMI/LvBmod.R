function (vB.Linf=1000,vB.K=0.05,vB.t0=0,LenAge0=0,Age)
# return length from age using modified von Bertalanffy growth curve

# by Andrew Constable
# last edit - 1 September 2005

# input
#       vB.Linf -  von Bertalanffy L infinity
#       vB.K    -  von Bertalanffy Kappa
#       vB.t0   -  von Bertalanffy t0 (LatAge0=0)
#       LenAge0 -  length at Age 0 (t0 = 0)
#       Age     -  input age

{

LenAge0+(vB.Linf-LenAge0)*(1-exp(-vB.K*(Age-vB.t0)))

}

