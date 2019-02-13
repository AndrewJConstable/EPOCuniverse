# Ecosim Kerguelen routines
# by Andrew Constable
# May 2011
#

###??????????????????? notes of what to do next





# 1. Import data from Provost et al 2005

DataTaxa<-read.csv("C:\\0Work\\1AME\\4EPOC\\_proj\\2011 Ecosim Element\\Kerguelen Ecopath Taxa.csv")
DataDiet<-read.csv("C:\\0Work\\1AME\\4EPOC\\_proj\\2011 Ecosim Element\\Kerguelen Ecopath Diet.csv")

GroupsN<-nrow(DataTaxa)

cat("\n","DataTaxa Column Names","\n")
print (names(DataTaxa))
#[1] "Group.No"   "Group.Name" "TrophLevel" "Bms"        "PB.ratio"   "CB.ratio"   "EE"         "PC.ratio"
cat("\n","DataDiet Column Names","\n")
print (names(DataDiet))
# [1] "Group.No"   "Group.Name" "X1"         "X2"         "X3"         "X4"         "X5"         "X6"         "X7"         "X8"         "X9"         "X10"        "X11"        "X12"        "X13"        "X14"        "X15"
#[18] "X16"        "X17"        "X18"        "X19"        "X20"        "X21"        "X22"        "X23"        "X24"

SpNames <- c("Top predators","Filtering marine mammals","Hunting marine mammals","Surface seabirds","Diving seabirds"
            ,"Sharks","Toothfish juveniles","Toothfish adults","Large pelagic fishes","Small pelagic fishes","Large benthic fishes"
            ,"Other benthic fishes","Cephalopods large","Cephalopods small","Deep benthic omnivores","Shallow benthic omnivores"
            ,"Shallow benthic herbivores","Euphausiacea","Zooplankton omnivores","Zooplanton herbivores","Benthic algae"
            ,"Phytoplankton","Detritus","Import")

# 2. Separate 'Import' data from rest of matrix (used separately as a multiplier of biomass)
  BmsImport<-DataDiet[(GroupsN+1),c(3:(GroupsN+2))] # one value for each group
  
# trim DataDiet so as matrix is symmetrical prey (rows) x predators (cols) and Import factor is removed e.g. column X24

  Diet<-DataDiet[c(1:GroupsN),c(3:(GroupsN+2))]

cat("\n","Number of groups = ",GroupsN,sep="","\n")
cat("\n","Annual import of each group","\n")
print(BmsImport)
cat("\n","Diet matrix - prey (rows), predator (cols)","\n")
print(Diet)

################################################################################
# Input Data

#######################################################
# Biomass

Bms <-  DataTaxa$Bms # initialise biomass
BmsN<-length(Bms)
BmsRef<-matrix(c(1:BmsN),nrow=1)
BmsMin<- rep(1E-6,BmsN) # minimum values in ODE

cat("\n","Biomass of each group = ","\n")
print(Bms)

#######################################################
# Vulnerability

# vulnerable prop of each population initially 1 (will change with estimates of escapement giving values for v  & vprime according to E = vprime/(v+vprime)

# matrix is determined from diet matrix - where value is greater than 0 then vulnerability proportion is 1
Vprop <- Diet
Vprop[Vprop>0] <- Vprop[Vprop>0]/Vprop[Vprop>0]

cat("\n","Proportion of each group as prey (rows) vulnerable to predators (cols)","\n")
print(Vprop)

V<-apply(Vprop,2,function(Vp,Bms){Vp*Bms},Bms)
cat("\n","Consequent vulnerable biomasses","\n")
print(V)

# rate parameters for vulnerability - ???? need to work this out from an escapement value given the other parameters see Eqn E3.3prime
Vv <- Vprop
Vvprime <- Vprop*0
Vvprime[Vv==0]<-1  # when Vv=0 Vvprime=1
print("Vvprime")
print(Vvprime)
#######################################################
# Production from consumption

G <-DataTaxa$PC.ratio

#######################################################
# Consumption rate - aij

Va <-apply(BmsRef,2,function(Ref,d,CperBpred,Bms){CperBpred[Ref]*d[,Ref]/Bms},Diet,DataTaxa$CB.ratio,Bms)
cat("\n","consumption rate per predator B per prey B","\n")
print(Va)

#######################################################
# intrinsic growth rate and carrying capacity of primary producers
#    here, assume that standing stock at equilibrium is 0.5K and that production = consumption

r <- 2*DataTaxa$PB.ratio
K <- 2*Bms

cat("\n","Growth parameters in the absence of consumption : r & K","\n")
print(r)
print(K)


#######################################################
# natural mortality
#   this uses the derivative at equilibrium

# *apply(BmsRef,2,function(Ref,a){sum(a[,Ref])},Va)

M <- DataTaxa$PB.ratio-
     apply(BmsRef,2,function(Ref,a,B){sum(a[Ref,]*B)},Va,Bms)
     

cat("\n","Instantaneous natural mortality (M) calculated from equilibrium Ecopath model","\n")
print(M)

# Ecopath formulation is (1-Ecotrophic Efficiency)
MfromEE<-(1-DataTaxa$EE)*DataTaxa$PB.ratio
print(cbind(BmsRef[1,],M,MfromEE,(M/MfromEE)))

# trying M from (1-EE)
# M<-(1-DataTaxa$EE)*DataTaxa$PB.ratio
#######################################################
# other parameters not yet derived
Si = rep(0.0,BmsN)
Hh = matrix(2E-6,nrow=BmsN,ncol=BmsN)


################################################################################
# Parameters

ODEparams <- list(
    G       = G # growth efficiency given biomass eaten (could determine from P/B and per capita consumption : P/B/C (if G=0 then no consumption - driven by surplus production model
   ,r       = r # density dependent growth parameter r when G=0
   ,K       = K # density dependent growth parameter K when G=0
   ,M       = M # mortality rate per year through causes other than predation
   ,Vv      = Vv   # instantaneous flux (per year) from escaped portion of prey biomass to portion vulnerable to predator
   ,Vvprime = Vvprime   # instantaneous flux (per year) from portion of prey biomass vulnerable to predator to portion escaped from predator

   ,Va      = Va # instantaneous per V predation mortality of prey biomass to predator under maximal conditions
   ,Si      = Si # numerical rate adjustment factor (0<S<1)
   ,Hh      = Hh   # handling time (in same units as other rate parameters) by predator per unit of prey (fraction of year per unit prey per unit predator)
   ,BmsRef = BmsRef
   ,BmsN   = BmsN
   ,qbj_0  = NA    # placeholder for initialisation at equilibrium
   ,Mpi_0  = NA    # placeholder for initialisation at equilibrium
   ,PB_0   = DataTaxa$PB.ratio
    ) # end list



