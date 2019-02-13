function (Npop,FirstAge,LastAge,MeanNat,CVnat,NatMort,UseMeanRinB0)
# B0 median test
# Test rule that B0 median is the same as the biomass derived from mean recruitment and the mortality function
# test whether this applies also to spawning biomass

{

# Input parameters:   Npop     = number of populations to use in estimating median
#                     FirstAge = first age class
#                     LastAge  = last age class as a plus class
#                     MeanNat  = Mean of log normal distribution in natural domain
#                     CVnat    = CV of log normal distribution in natural domain
#                     NatMort  = M
#                     UseMeanRinB0 = adjustment to CASAL B0 calculation
#                                   TRUE - use mean recruitment
#                                   FALSE - adjust mean recruitment * exp(-SDlog^2/2)

WatAge<-rep(1,(LastAge-FirstAge+1)) # cosmetic only

Ages<-c(FirstAge:LastAge)
Nages<-length(Ages)
Bpop<-c()

# log parameters
 SDlog   <- ifelse(CVnat==0,0,sqrt(log(1+CVnat^2)))
 MeanLog<- log(MeanNat)-SDlog^2/2



# generate median biomass

for (i in 1:Npop) {
  Cohorts<-rlnorm(Nages, meanlog = MeanLog, sdlog = SDlog)
  Cohorts<-Cohorts*exp(-NatMort*Ages)
  if(NatMort>0) Cohorts[Nages]<-Cohorts[Nages]/(exp(NatMort)-1)
  Bpop[i]<-sum(Cohorts*WatAge)
}

# calculate B0
# 1. adjust R0 if UseMeanRinB0=FALSE
    Rec0<-ifelse(UseMeanRinB0,MeanNat,(MeanNat*exp(-SDlog^2/2)))

# 2. age structure
    B0ageStr<-exp(-NatMort*Ages)
    if(NatMort>0) B0ageStr[Nages]<-B0ageStr[Nages]/(exp(NatMort)-1)

# 3. biomass
    B0fromRec0<-Rec0*sum(B0ageStr*WatAge)

median(Bpop)/B0fromRec0

}

