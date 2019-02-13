# EPOC-A - Ecosystem Production Ocean and Climate Modelling Environment - Antarctica
# a Coupled Ocean Ecology Modelling System

# Example set of functions for taxon element (note that fishing and other sources of mortality will have their own set of functions but similar to those below)

# 1. Summary of the status of the taxon at necessary spatial scales

# 2. State and condition of taxon in each polygon relevant to that taxon

#     2.1 FIXED characteristics

#         2.1.1 smallest geographical subdivision

#     2.2 INITIAL characteristics

#         2.2.1 initial polygons identified according to smallest geographical and depth subdivisions

#         2.2.2 initial time steps within year - matrix of start date of each step and purpose/s of time step
#                 2.2.2.1 consumption
#                 2.2.2.2 reproduction
#                 2.2.2.3 movement spatially
#                 2.2.2.4 movement to next stage
#                 2.2.2.5 monitoring
#                 2.2.2.6 update of polygon structure
#                 2.2.2.7 update of time steps
#
#         2.2.3 initial state - options (units in SSI)
#                 2.2.3.1 area - units, magnitude
#                 2.2.3.2 number - units, magnitude
#                 2.2.3.3 biomass - units, magnitude
#                 2.2.3.4 age structure - parameters

#         2.2.4 initial condition - options (units in SSI)
#                 2.2.4.1 size - units, magnitude
#                 2.2.4.2 reproductive condition - units, magnitude
#                 2.2.4.3 health - units, magnitude

#         2.2.5 initial within spatial unit characteristics recorded as a suite of parameters including variation over the temporal unit

#     2.3 VARYING characteristics

#         2.3.1 polygons identified according to smallest geographical and depth subdivisions
#         2.3.2 time steps within year (such as variation in seasonal attributes)
#         2.3.3 state - options (units in SSI)
#                 2.3.3.1 area - units, magnitude
#                 2.3.3.2 number - units, magnitude
#                 2.3.3.3 biomass - units, magnitude
#                 2.3.3.4 age structure - parameters
#         2.3.4 condition - options (units in SSI)
#                 2.3.4.1 size - units, magnitude
#                 2.3.4.2 reproductive condition - units, magnitude
#                 2.3.4.3 health - units, magnitude
#         2.3.5 within spatial unit characteristics

# 3. Within time-step process

#     3.1 Estimate consumption (and consequent mortality for affected taxa)
#     3.2 Accumulate mortality of element
#         3.2.1 predation
#         3.2.2 fishing and/or other human activities
#         3.2.3 other sources of mortality
#     3.3 If total mortality > abundance
#         3.3.1 Pro-rate mortality so that sum equals abundance
#         3.3.2 Re-estimate sources of mortality given competition
#     3.4 Update size condition
#         3.4.1 von Bertalanffy growth with weight-length relationship
#         3.4.2 growth based on mean weight of element, consumption and local conditions of polygon
#         3.4.3 size is based on mean weight, consumption, local conditions of polygon and reproductive condition
#         3.4.4 life history model
#     3.5 Update reproductive condition
#         3.5.1 time of year and size condition
#         3.5.2 consumption, growth, local conditions of polygon and time of year
#         3.5.3 life history model
#     3.6 Update health condition
#         3.6.1 time of year and size condition
#         3.6.2 consumption, growth, local conditions of polygon and time of year
#         3.6.3 life history model
#     3.7 Grow biomass
#     3.8 Move spatially
#     3.9 Move to next stage element (if appropriate)

# 4. End of time-step


# A1. General attributes of the model
#     A1.1 Allow for leap year (T) or not (F)
#     A1.2 Set up order of events during year from the time step function - for those that have the same timing then randomise the order at each occurrence during the simulation
#     A1.3 Set logical flags for when relationships between elements need to be updated due to changing polygons, time steps, movement patterns etc.  These should be done at a specified time.
#     A1.4 conversion formulae for relationship of one element to another (converting to same units) -spatially, -temporally, -next life stage, -consumption

##########################################################
# General attributes of model

# 1. Arena specifications

#      origin coordinates of grid (longitude, latitude)

            arena.grid.origin<-c(0,0)

#      opposing coordinates of grid (longitude, latitude)

            arena.grid.oppose<-c(5,5)

#      grid intervals in longitude and latitude

            arena.grid.interval<-c(1,1)


# 2, Time specifications for modelling "epoch"
#       used for generating lookup tables for each element

            epoch.origin<-list(day0 = 1, month0 = 1, year0 = 2000)

#         will need to change time interval to account for variable days in the month, leap years etc.
#         for the moment just work with fractions of year based on number of intervals in year

            epoch.year.intervals<-12


##########################################################
# combine species using a list

# define ecosystem (taxa are only concerned with their prey and living, not with mortality)
# the user must be confident that all prey are on the list otherwise an error will be generated in next step

Ecosystem<-list(Ocean(),Esuperba=Esuperba(),KrillMortality())

# then work out predator prey relationships


# then work out calendar of events


# Function:           BZ.Esuperba.01
# Description:        Biological Element, Group Zooplankton, Euphausia superba
# Primary attributes: Element incorporates all life stages of taxon
#                     Age structure included in status

function ()
{
#   create new environment for Taxon

    Taxon<-new.env()

#     2.2 INITIAL characteristics

#         2.2.1 initial polygons identified according to smallest geographical and depth subdivisions
#                 grid squares assigned by filling all rows in a column before next column (bottom left is 1,top right is last)

                 Taxon$Polygons<-list(
                    AP     = c(1,6),      # Antarctic Peninsula
                    EI     = c(7,12),     # Elephant Island
                    SOI    = c(13,18),    # South Orkney Islands
                    SGI    = c(19,24),    # South Georgia Island
                    NthWed = c(22,23),    # North Weddell Sea
                    SthWed = c(16,21),    # South Weddell Sea
                    CenWed = c(17),       # Central Weddell Sea
                    EAP    = c(11)        # East Antarctic Peninsula
                    )

                # number of polygons

                 Taxon$Polygons.N<-length(Taxon$Polygons)


#         2.2.2a parameters or function for initialising population

#
#         2.2.3 initial State for each area - options (units in SSI)
#                 2.2.3.1 area - units, magnitude                   "surf.area"
#                 2.2.3.2 number of individuals - magnitude         "num.ind"
#                 2.2.3.3 biomass - units, magnitude                "mass"
#                 2.2.3.4 age structure - parameters                "ageStr"

#                 elements in the vectors correspond to the respective polygons

                   agesN<-3 # number of age classes

                  Taxon$State<-list(
                    num.ind  = vector("numeric",c(1000,1000,1000,1000,1000,1000,1000,1000)),       # if mass is NA then convert from individuals to mass using average weight (see age structure)
                    mass     = list("t",vector("numeric",c(10000,10000,10000,10000,10000,10000,10000,10000))), # if num.ind is NA then convert from mass to individuals using average weight (see age structure)
                    agesN    = agesN,

#                                   default is to apply the same age structure in all areas

                                     Taxon.AgeStructure.Default<-matrix(c(1,0.75,
                                              2,0.30,
                                              3,0.10),nrow=agesN,ncol=2)

                    ageStr   = list(
                            AP     = Taxon.AgeStructure.Default,
                            EI     = Taxon.AgeStructure.Default,
                            SOI    = Taxon.AgeStructure.Default,
                            SGI    = Taxon.AgeStructure.Default,
                            NthWed = Taxon.AgeStructure.Default,
                            SthWed = Taxon.AgeStructure.Default,
                            CenWed = Taxon.AgeStructure.Default,
                            EAP    = Taxon.AgeStructure.Default
                            ) # end ageStr list
                    ) # end list of State


#         2.2.4 initial condition - options (units in SSI)
#                 2.2.4.1 size - units, magnitude                     "ind.size"
#                 2.2.4.2 reproductive condition - units, magnitude   "reprod.cond"
#                 2.2.4.3 health - units, magnitude                   "health.cond"

#                 default is to apply the same conditions in all areas

                      Taxon.Condition.Default<-list(ind.size     = list("g",2),       # could be a function using the mean weight based on the age structure
                                          rep.cond     = list("potl",0.0),  # potential for reproduction (0.0 - 1.0)
                                          health.cond  = list("potl",1.0)  # potential to survive (0.0 - 1.0) without reproducing or growing
                                          )
                  Taxon$Condition<-list(
                            AP     = Taxon.Condition.Default,
                            EI     = Taxon.Condition.Default,
                            SOI    = Taxon.Condition.Default,
                            SGI    = Taxon.Condition.Default,
                            NthWed = Taxon.Condition.Default,
                            SthWed = Taxon.Condition.Default,
                            CenWed = Taxon.Condition.Default,
                            EAP    = Taxon.Condition.Default
                    ) # end list of Condition

#         2.2.5 initial within spatial unit characteristics recorded as a suite of parameters including variation over the temporal unit

                  Taxon$Variability<-list(
                    prob.distn     = "ln",       # turn this into a function for deriving deviates from a probability distribution
                    func.params    = c(0,0.1)    # fake parameters to define the variability - could do this by polygon and timestep if desired
                    ) # end list of variability

# 3. Within time-step process

#     3.1 Estimate consumption (and consequent mortality for affected taxa)

#             not used for this taxon.element

#     3.2 Accumulate mortality of element

#             no procedure needed to be declared here

#     3.3 If total mortality > abundance

#             no procedure needed to be declared here

#     3.4 Update size condition and grow biomass

#         3.4.1 von Bertalanffy growth with weight-length relationship

#             input parameters of different functions - could be one for each area or one for assignment to many areas
                  Taxon$SizeCondition.data<-list(
                      set.01 = list(
                        FNcondition.size  = FN.condition.size.01,
                        vB.Linf           = 70.0,
                        vB.K              = 0.2,
                        vB.t0             = 0.0,
                        vB.grow.start     = 330,
                        vB.grow.end       = 90,
                        WL.a              = 1.0,
                        WL.b              = 3.0)
                   ) # end SizeCondition.params

#             generate lookup tables and save them as data

                  Size.lookup.01<-LU.ind.size(Taxon$SizeCondition.params)

                  Taxon$SizeCondition.data<-list(Size.lookup.01)


#             assign the function (01) to be used to convert lookup tables to size and assign different data sets (in this case, lookup tables) to the respective areas

                  Taxon$SizeCondition.update<-list(
                    FNconvert.size  = FN.convert.size.01,
                            AP     = 1,
                            EI     = 1,
                            SOI    = 1,
                            SGI    = 1,
                            NthWed = 1,
                            SthWed = 1,
                            CenWed = 1,
                            EAP    = 1
                     ) # end Update.SizeCondition


                     # function FN.convert.size.01
                     #      takes the number of individuals in the polygon
                     #      divides them into the respective ages
                     #      determines the size at the time given to the function using a lookup table (interpolating between values if necessary)
                     #      determine the mean weight of an animal to reflect the size condition

                     # biomass of the taxon would then be the product of number of individuals and mean weight


#     3.5 Update reproductive condition

#             not used for this taxon.element

#     3.6 Update health condition

#             not used for this taxon.element

#     3.8 Move spatially
#           matrix of probabilities of moving from one location to another location
#           polygon origins are rows, polygon destinations are columns

                  Taxon$Update.migration<-matrix(c(

#                  Origin (data are filled rows by columns)
#                   1    2    3    4    5    6    7    8
#                  AP  ,EI  ,SOI ,SGI ,NWed,SWed,CWed,EAP
#                                                             # Destination
                   0.55,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,0.3 ,   # AP
                   0.3 ,0.6 ,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,0.4 ,   # EI
                   0.05,0.3 ,0.4 ,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,   # SOI
                   0.0 ,0.0 ,0.4 ,0.8 ,0.1 ,0.0 ,0.0 ,0.0 ,   # SGI
                   0.0 ,0.0 ,0.1 ,0.2 ,0.4 ,0.1 ,0.3 ,0.0 ,   # NWed
                   0.05,0.05,0.0 ,0.0 ,0.4 ,0.4 ,0.2 ,0.1 ,   # SWed
                   0.0 ,0.05,0.1 ,0.0 ,0.1 ,0.1 ,0.4 ,0.1 ,   # CWed
                   0.05,0.0 ,0.0 ,0.0 ,0.0 ,0.4 ,0.1 ,0.1 ))  # EAP

#         Reproduction

#             stock-recruitment relationship & the elements to which new offspring are accumulated



#     3.9 Move to next stage element (if appropriate)


# 4. Management of time steps


#
#         2.2.2 initial time steps within year - matrix of start date of each step and purpose/s of time step
#                 2.2.2.01 update consumption                                 ("consume")
#                 2.2.2.02 update reproductive condition                      ("Upd.cond.reprod")
#                 2.2.2.03 update size condition & biomass if needed          ("Upd.cond.size")
#                 2.2.2.04 update health condition                            ("Upd.cond.health")
#                 2.2.2.05 reproduce                                          ("reproduce")
#                 2.2.2.06 movement spatially                                 ("migrate")
#                 2.2.2.07 movement to next stage (evolution to other element)("evolve")
#                 2.2.2.08 monitoring                                         ("monitor")
#                 2.2.2.09 update of polygon structure                        ("update_polygon")
#                 2.2.2.10 update of time steps                               ("update_timesteps")

#                   the characteristics of a time step between the previous time and the specified time (in days)
#                   is given in a list(days in calendar year, number of functions to be carried out, list of named functions)
#                   knife-edge functions can be included by repeating the same day

                  Taxon$Timesteps<-list(
                    StartYear = list(calday=330,actions.N=0,actions=NA),
                    Spawn     = list(calday=360,actions.N=2,actions=c("reproduce","migrate")),
                    Summer    = list(calday= 90,actions.N=3,actions=c("migrate","Upd.cond.size","monitor")),
                    Autumn    = list(calday=180,actions.N=1,actions=c("migrate")),
                    NextStage = list(calday=180,actions.N=1,actions=c("evolve")),
                    Winter    = list(calday=330,actions.N=2,actions=c("migrate"))
                    ) # end list of timesteps

                # number of timesteps

                 Taxon$Timesteps.N<-length(Taxon$Timesteps)


#   return Taxon environment

      Taxon

} #     end Euphausia superba function 1

