library("rgdal")
library("raster")
library("sf")
library("leafsync")
library("dplyr")
library("ggplot2")
library("stars")
library(spatialEco)
library(spatstat.random)

nsamp=20
point.dist.cond = 50 # want meters between points
tas_utm <- st_crs("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")

# euclidean distance function
eucDistance<-function(P1, P2){
  sqrt((((P2[1]-P1[1])^2)+((P2[2]-P1[2])^2)))
}

# read in raster
landcover<-read_stars("./lc_antler_adventures_area_small.tif") %>%
st_as_sf()

#  read in shp file
farm.full<-st_read("./AntlerAdventuresFull.shp")

# want to buffer farm.full
farm.full
farm.buff<-farm.full %>%
  st_buffer(30) # buffer by 30m

# erase
farm.fenceline.buff<-st_difference(farm.buff, farm.full)

# try masking the raster
lc.fenceline<-st_intersection(landcover, farm.fenceline.buff)

# convert back to raster?
lc.fenceline.rast<-as(st_rasterize(lc.fenceline),"Raster")

# sample fenceline raster
sample.points <- sampleStratified(x=lc.fenceline.rast$lc_antler_adventures_area_small.tif,
                                  size=8,
                                  na.rm=TRUE,
                                  xy=TRUE,
                                  sp=TRUE)
bad.indx.lst <- c()
k<-1
data.points<-list()
for(i in 1:(nrow(sample.points)-1)){
  p1 <- c(sample.points$x[i], sample.points$y[i])
  for(j in (i+1):nrow(sample.points)){
    p2 <- c(sample.points$x[j], sample.points$y[j])
    # last condition will ensure that only points with indices not already in 
    # will go thru this condition; gets around landcover classes with smaller
    # representations
    if((i != j) & (eucDistance(p1, p2) < point.dist.cond) & ((! i %in% bad.indx.lst) | (! j %in% bad.indx.lst))){
       bad.indx.lst[k]<-i
       k<-k+1
      }
    }
}
bad.indx.lst<-unique(bad.indx.lst)
sample.points<-sample.points[-c(bad.indx.lst),]

# check for filtering
(nrow(sample.points))

# filter off excess points
sample.points<-sample.points[-c(nsamp+1:nrow(sample.points)),]

(nrow(sample.points))
# check for number of points
# (nrow(filter.pts))

# convert points to gpx points

# first convert from UTM NAD83 espg:26915 -> long lat
long.lat <- spTransform(sample.points, CRS("+proj=longlat"))

writeOGR(long.lat, dsn="./gpxTEST.gpx",
         dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)

# can plot to check our point distribution

# plot(lc.fenceline.rast$lc_antler_adventures_area_small.tif)
# points(filter.pts, col="black")