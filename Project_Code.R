# Run this command once to link to my google API account (please do not share this key)
  #register_google(key = "AIzaSyAxPCoK5JzfwK5A-7tvfYrk2L7Aiz99tZA", write=TRUE)

setwd("~/IMGT680")
load("BostonCrimeData2015-2018.RData")

require(stringr)
regex <-"\\((\\d+\\.\\d+), (-\\d+\\.\\d+)\\)"
lat_long <- str_match(data$Location, regex)[,2:3]
lat_long <- apply(lat_long, 2, as.numeric)
colnames(lat_long) <- c("y", "x")
crime_latlong <- cbind(data, lat_long)

head(lat_long) #much better


# plot on a map
#install.packages("ggmap")
require(ggmap)
map.center <- geocode("Boston, MA")
Bos_map <- qmap(c(lon=map.center$lon, lat=map.center$lat), zoom=12)

for(i in as.vector(unique(data$OFFENSE_CODE_GROUP))){
drug_crimes <- crime_latlong[crime_latlong$OFFENSE_CODE_GROUP == i,]
g <- Bos_map + geom_point(aes(x=x, y=y), data=drug_crimes, size=3, alpha=0.2, color="red") + 
                          ggtitle(paste(i, " in Boston by Location (2015-2018)"))
 print(g)
}

#load R geo packages
#install.packages("rgdal")
require(rgdal)
require(sp)

#read the shape files
datadir <- "./Boston_Neighborhoods/"
neighbs <- readOGR(dsn=datadir, layer="Boston_Neighborhoods")

#prepare for plotting
neighbs <- spTransform(neighbs, CRS("+proj=longlat +datum=WGS84"))
neighbs_plt <- fortify(neighbs)

#plot the neighborhoods with ggmap
Bos_map2 <- qmap(c(lon=map.center$lon, lat=map.center$lat), zoom=11)
Bos_map2 + geom_polygon(data=neighbs_plt, aes(x=long, y=lat, group=group), alpha=0.3, color="black", fill='red') + ggtitle("Geographic Extent of Boston")


# plot neighborhoods and crimes
Bos_map2 + geom_polygon(data=neighbs_plt, aes(x=long, y=lat, group=group), alpha=0.3, color="black", fill='red') +geom_point(aes(x=x, y=y), data=drug_crimes, size=2, alpha=0.2, color="black")+ 
  ggtitle("Geographic Extent of Boston with Drug Charges (2011-2014) Overlay")

