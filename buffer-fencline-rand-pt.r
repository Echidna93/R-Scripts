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
earth.radius = 6371
point.dist.cond = 50 # want meters between points
tas_utm <- st_crs("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")

# euclidean distance function
eucDistance<-function(P1, P2, earthRadius){
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
s.pts<-st_as_sf(sample.points)
filter.pts<-data.frame(x<-c(),
                       y<-c())
pt.list <- c()
k<-1
data.points<-list()
for(i in 1:(nrow(s.pts)-1)){
  p1 <- c(s.pts$x[i], s.pts$y[i])
  for(j in (i+1):nrow(s.pts)){
    p2 <- c(s.pts$x[j], s.pts$y[j])
    if((i != j) & (eucDistance(p1, p2, earth.radius) < point.dist.cond)){
       # print(eucDistance(p1,p2,earth.radius))
       # data.point<-list(x=s.pts$x[i],y=s.pts$y[i])
       # filter.pts<-rbind(filter.pts,data.point)
       pt.list[k]<-i
       k<-k+1
      }
    }
}
pt.list<-unique(pt.list)
s.pts<-s.pts[-c(pt.list),]
pts<-list(s.pts$x, s.pts$y)
filter.pts <- rbind(filter.pts, pts)
# filter off excess points
(nrow(filter.pts))
filter.pts<-filter.pts[-c(nsamp+1:nrow(s.pts)),]
(nrow(filter.pts))
# (nrow(s.pts))
# filter.pts<-unique(filter.pts)
# (nrow(filter.pts))
plot(lc.fenceline.rast$lc_antler_adventures_area_small.tif)
points(filter.pts, col="black")