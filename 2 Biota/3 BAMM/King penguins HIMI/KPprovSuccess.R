function (Bms,yr,KBMSkp,Ap,MSA,MinCoeff)

# King penguin (KP) provisioning success based on time series of MSA index
# the subsequent value is used to adjust reproductive output

# input data
#           Bms   - population size as biomass
#           yr     - year to estimate breeding success (offspring per breeding pair)
#           KBMSkp - total population size indicating biomass at carrying capacity
#           Ap     - degree of density dependence
#           MSA    - MSA index - col 1 year, col 2 MSA

{
# MSA index in previous year is used as an index of food availability over previous summer
#         giving per capita condition
#
# KBMSkp is the carrying capacity as biomass
#
Coeff<-1-(Bms/(MSA[MSA[,1]==(yr-1),2]*KBMSkp))^Ap
if (Rcoeff<0) Rcoeff<-0
Rcoeff
}


