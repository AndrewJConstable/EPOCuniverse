# Polygons / Spatial distribution data file
Polygons <- list()
Polygons$signature			<- list(
								ID          = 15001,
								ClassName	= "Spatial",
								Name.full   = "Polygon Details",
								Name.short  = "Polygons",
								Morph       = "Env_gen",
								Revision    = "01",
								Authors     = "A.Constable",
								Last.edit   = "28 Jan 2015"
							)
   
Polygons$polygonNames 		<- c(
								"Area_01"   #  1
							   )
	
	# square km (original in m2 from Watters data file - precision kept the same for comparisons)
Polygons$polygonAreas 		<- c( 1 # Area_01
                                )

Polygons$coords 			<- list(
								 A_1 = c(1)
							)
	
Polygons$coordAreas 		<- list(
 								 A_1 = 1
							)

Polygons$coordProportions 	<- list(
								 A_1 = 1
							)


	#   Polygon overlap - matrix of proportions of overlap -
	#        proportion of subject polygon (rows) in other polygon (cols)
	#     ????need to write a routine that automatically determines overlap
	#       (need to avoid partial coordinates that are non-overlapping but may
	#        appear overlapping when sorted solely by coordinates.)
	#
Polygons$overlap 			<- matrix(c(1)
								               ,nrow=1,byrow=TRUE)

Polygons

