function ()
# Function:           epoc.cat.gen
# Description:        General catalogue of library for EPOC - details of the catalogue and codes
#

# Primary attributes: help

{


########################################################
#      Signature <- list(
#        ID           =  10002,
#        Name.full    = "Help - structure - catalogue and codes",
#        Name.short   = "Catalogue",
#        Version      = "01",
#        Authors      = "A.Constable",
#        last.edit    = "28 May 2005"
#        ) # end Signature


########################################################
#     Nomenclature for function names (including numeric identifiers)
#
#
#
#   C. = Controller

#       10000  - help and descriptions (last assigned ID = 10002)
#       11000  - controller (last assigned ID = 11011 - reassign 11012)
#       12000  - universes to be simulated (last assigned ID = 12001)
#

#     C.00: Development functions

#     C.01: Main controller


#           C.                                  Functions for determining calendar of periods in the year
#           C.Cal.01.01               #  ID = 11004  Generating the calendar of events for the year
#           C.Cal.01.output.01        #  ID = 11012 Output calendar to file

#           C.SE                                    Functions for generally setting up epoc
#           C.SE.Transition.01        #  ID = 11008  establish a list of Transition environments including only $State from each component
#           C.SE.Setup.universe.01    #  ID = 11011  call setup functions in each component to establish parameters, linkages etc.
#                                                       dependent on other aspects of the universe

#           C.PJ                                    Functions for undertaking projections
#           C.PJ.Period.01            #  ID = 11010  Project universe over one time step

#
#   P. = Presentation
#
#       70000  - help (last assigned ID = )
#       71000  - (last assigned ID = )
#       79000  - supporting functions (last assigned ID = )
#
#
#   U. = Utilities
#
#       80000  - help (last assigned ID = )
#       81000  - (last assigned ID = 81003)
#
#     U.ST  : Statistical utilities
#           U.ST.RandLnormNat.01        #ID = 81001  Generate vector of random log-normal deviates based on natural domain mean, CV
#           U.ST.DayFromDate.01         #ID = 81002  Generate calendar day in year from date (day,month)
#           U.element.ref.in.list.01    #ID = 81003  Generate reference number of an element (name) in list
}

