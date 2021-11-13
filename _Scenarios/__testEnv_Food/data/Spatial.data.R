# Polygons / Spatial distribution data file
Polygons <- list()
Polygons$signature			<- list(
								ID          = 15001,
								ClassName	= "Spatial",
								Name.full   = "Polygon Details",
								Name.short  = "Polygons",
								Morph       = "KPFM",
								Revision    = "01",
								Authors     = "A.Constable",
								Last.edit   = "25 Feb 2008"
							)
   
Polygons$polygonNames 		<- c(
								"APPA"   #  1
							   ,"APW"    #  2
							   ,"APDPW"  #  3
							   ,"APDPE"  #  4
							   ,"APBSW"  #  5
							   ,"APBSE"  #  6
							   ,"APEI"   #  7
							   ,"APE"    #  8
							   ,"SOPA"   #  9
							   ,"SOW"    # 10
							   ,"SONE"   # 11
							   ,"SOSE"   # 12
							   ,"SGPA"   # 13
							   ,"SGW"    # 14
							   ,"SGE"    # 15
							   ,"BT1"    # 16
							   ,"BT2"    # 17
							   ,"BT3"    # 18
							   )
	
	# square km (original in m2 from Watters data file - precision kept the same for comparisons)
Polygons$polygonAreas 		<- c( 1 #4.22076E+11 # APPA
                                , 0.35060E+11 # APW
                                , 0.15068E+11 # APDPW
                                , 0.15584E+11 # APDPE
                                , 0.21017E+11 # APBSW
                                , 0.27447E+11 # APBSE
                                , 0.35322E+11 # APEI
                                , 0.58704E+11 # APE
                                , 8.09163E+11 # SOPA
                                , 0.15569E+11 # SOW
                                , 0.10251E+11 # SONE
                                , 0.14954E+11 # SOSE
                                , 9.19534E+11 # SGPA
                                , 0.42119E+11 # SGW
                                , 0.53735E+11 # SGE
                                ,18.80000E+11 # Bathtub 1
                                , 7.80000E+11 # Bathtub 2
                                , 5.24000E+11 # Bathtub 3
                                )

Polygons$coords 			<- list(
								A_1 = c(1)
								,A_2 = c(2)
								,A_3 = c(3)
								,A_4 = c(4)
								,A_5 = c(5)
								,A_6 = c(6)
								,A_7 = c(7)
								,A_8 = c(8)
								,A_9 = c(9)
								,A10 = c(10)
								,A11 = c(11)
								,A12 = c(12)
								,A13 = c(13)
								,A14 = c(14)
								,A15 = c(15)
								,A16 = c(16)
								,A17 = c(17)
								,A18 = c(18)
							)
	
Polygons$coordAreas 		<- list(
								A_1 = 1 #425000.0
								,A_2 = 37459.917545
								,A_3 = 15467.155012
								,A_4 = 16433.877631
								,A_5 = 22266.774353
								,A_6 = 28799.367963
								,A_7 = 36524.401670
								,A_8 = 52208.478973
								,A_9 = 813000.0
								,A10 = 14968.931051
								,A11 = 12077.241055
								,A12 = 15582.556375
								,A13 = 931000.0
								,A14 = 42766.584727
								,A15 = 55231.103064
								,A16 = 188000.0
								,A17 = 780000.0
								,A18 = 524000.0
							)

Polygons$coordProportions 	<- list(
								A_1 = 1
								,A_2 = 1
								,A_3 = 1
								,A_4 = 1
								,A_5 = 1
								,A_6 = 1
								,A_7 = 1
								,A_8 = 1
								,A_9 = 1
								,A10 = 1
								,A11 = 1
								,A12 = 1
								,A13 = 1
								,A14 = 1
								,A15 = 1
								,A16 = 1
								,A17 = 1
								,A18 = 1
							)

	# additional amalgamated polygons
	#    GeneralPredator<-NULL
	#    GenPredGridAreas<-NULL
	#    GenPredPolygonArea<-0
	#    for (pn in 1:Polygon$Polygon.N){
	#       GeneralPredator<-c(GeneralPredator,Polygon$Coords[[pn]])
	#       GenPredGridAreas<-c(GenPredGridAreas,Polygon$Coord.Areas[[pn]])
	#       GenPredPolygonArea<-GenPredPolygonArea+Polygon$Polygon.Areas[pn]
	#      }
	#    Polygon$Polygon.Names<-c(Polygon$Polygon.Names,"GeneralPredator")
	#    Polygon$Polygon.Areas<-c(Polygon$Polygon.Areas,GenPredPolygonArea)
	#    Polygon$Coords<-c(Polygon$Coords,list(GenPredator = GeneralPredator))
	#    Polygon$Coord.Areas<-c(Polygon$Coord.Areas,list(GenPredator = GenPredGridAreas))
	#    Polygon$Polygon.N = length(Polygon$Coords)

	#   Polygon overlap - matrix of proportions of overlap -
	#        proportion of subject polygon (rows) in other polygon (cols)
	#     ????need to write a routine that automatically determines overlap
	#       (need to avoid partial coordinates that are non-overlapping but may
	#        appear overlapping when sorted solely by coordinates.)
	#
Polygons$overlap 			<- matrix(c(
                                  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                 ,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                 ,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                 ,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                 ,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0
                                 ,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0
                                 ,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0
                                 ,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0
                                 ,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0
                                 ,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0
                                 ,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0
                                 ,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0
                                 ,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0
                                 ,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0
                                 ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
                                 ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0
                                 ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0
                                 ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
								),nrow=18,byrow=TRUE)

Polygons

