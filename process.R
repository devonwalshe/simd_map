### Process

simd = read.csv("../data/import/simd_lookup.csv", stringsAsFactors = FALSE)
simd_mmw = right_join((simd %>% group_by(MMWcode) %>% 
                        summarize(health = mean(SIMD16_Health_Domain_Rank), population = sum(Population), working_pop = sum(Working_Age_Population_revised))),
                      (simd %>% filter(!duplicated(MMWcode) & (LAname == "City of Edinburgh" | LAname == "Midlothian"| LAname=="East Lothian"| LAname=="West Lothian")) %>% 
                         mutate(MMWcode = as.character(MMWcode)) %>% select(MMWcode)), by="MMWcode")
                    

### proj4 strings
### figure out how to reproject
latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"
google = "+init=epsg:3857"
osgb_36 = "+init=epsg:4277"

### load spatial files
mmw = readOGR(dsn = "../data/import/All-Scottish-Census-boundaries(shp)/", layer = "WD_2011_EoR_Scotland")

scotland = readOGR(dsn = "../data/import/", layer = "scotland_osgb")
scotland = spTransform(scotland, CRS(proj4string(mmw)))

### subset edinburgh
mmw_ed = mmw[grep("Lothian|lothian|Edinburgh", mmw$CA_Name),]

### join simd
mmw_ed@data = left_join(mmw_ed@data, simd_mmw, by=c("GSS_CODEWD" = "MMWcode"))

### Add centroids to mmw edinburgh
mmw_ed@data[,c("x","y")] = coordinates(gCentroid(mmw_ed,byid=TRUE))

### disolve into one region for clipping later
edinburgh = gUnaryUnion(mmw_ed, id=mmw_ed$local)
edinburgh_clip = SpatialPolygons(edinburgh@polygons, proj4string= CRS(proj4string(mmw_ed)))

### create points spdf
mmw_ed_points = SpatialPointsDataFrame(coordinates(mmw_ed), mmw_ed@data, 
                                       proj4string = CRS(proj4string(mmw)))

### now over to the tutorial - http://gis.stackexchange.com/questions/158021/plotting-map-resulted-from-kriging-in-r

### compute the grid for our poly
x_length = max(mmw_ed$x - min(mmw_ed$x))+30000
y_length = max(mmw_ed$y - min(mmw_ed$y))+20000

### pixel size
cellsize = 100

### columns
ncol = round(x_length/cellsize,0) #number of columns in grid
nrow = round(y_length/cellsize,0) #number of rows in grid

### already have coordinates for mmw_points

### make grid
mmw_grid = GridTopology(cellcentre.offset=c(min(mmw_ed$x)+500,min(mmw_ed$y))-10000, 
                        cellsize=c(cellsize,cellsize), cells.dim=c(ncol,nrow))

mmw_grid_pixels = SpatialPixelsDataFrame(mmw_grid, data=data.frame(id=1:prod(ncol, nrow)),
                                    proj4string = CRS(proj4string(mmw_ed)))

mmw_grid_points = SpatialPointsDataFrame(mmw_grid, data=data.frame(id=1:prod(ncol, nrow)),
                                         proj4string = CRS(proj4string(mmw_ed)))

### clip the grid
mmw_ed_grid_clipped = mmw_grid_points[!is.na(over(mmw_grid_points, edinburgh_clip)),]

### Krigging
kriging_result = autoKrige(scale(health)~1, mmw_ed_points, mmw_ed_grid_clipped, block=c(10,10), model="Exp")

### try interpolating the results
mmw_interp = interp(x=kriging_result$krige_output, z="var1.pred")

### testing
mmw_ed_points@data %>% 
  ggplot(aes(x, y)) + geom_point(aes(size=health), color="blue", alpha=3/4) + 
  ggtitle("Health outcomes in Edinburgh by location") +  coord_equal() + theme_bw()
