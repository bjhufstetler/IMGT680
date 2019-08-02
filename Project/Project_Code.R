# Run this command once to link to my google API account (please do not share this key)
  #register_google(key = "AIzaSyAxPCoK5JzfwK5A-7tvfYrk2L7Aiz99tZA", write=TRUE)

#install.packages("ggmap")
#install.packages("rgdal")

setwd("~/IMGT680/Project")
load("BostonCrimeData2015-2018.RData")

# Convert day of week to integer w/ Monday == 1
data$DAY_OF_WEEK <- factor(data$DAY_OF_WEEK, levels = c("Monday", "Tuesday", "Wednesday", 
                                                        "Thursday", "Friday", "Saturday", 
                                                        "Sunday"),ordered = TRUE)

# plot on a map
require(ggmap)
map.center <- geocode("Boston, MA")
Bos_map <- qmap(c(lon=(map.center$lon-0.025), lat=(map.center$lat-.045)), zoom=12)

for(i in as.vector(unique(data$OFFENSE_CODE_GROUP))){
  print(i)
  crimes <- data[data$OFFENSE_CODE_GROUP == i,]
  p <- Bos_map + geom_point(aes(x=Long, y=Lat), data=crimes, size=3, alpha=0.2, color="red") + 
                            ggtitle(paste(i, " in Boston by Location (2015-2018)"))
  print(p)
}

#load R geo packages
require(rgdal)
require(sp)

#read the shape files
datadir <- "./Boston_Neighborhoods/"
neighbs <- readOGR(dsn=datadir, layer="Boston_Neighborhoods")

#prepare for plotting
neighbs <- spTransform(neighbs, CRS("+proj=longlat +datum=WGS84"))
neighbs_plt <- fortify(neighbs)

#plot the neighborhoods with ggmap
map.neigh <- qmap(c(lon=map.center$lon, lat=map.center$lat), zoom=11)
map.neigh <- map.neigh + geom_polygon(data=neighbs_plt, aes(x=long, y=lat, group=group), alpha=0.3, color="black", fill='red') + ggtitle("Geographic Extent of Boston")


# plot neighborhoods and crimes
map.neigh + geom_polygon(data=neighbs_plt, aes(x=long, y=lat, group=group), 
                         alpha=0.3, color="black", fill='red') +
            geom_point(aes(x=Long, y=Lat), data=crimes, size=2, 
                        alpha=0.2, color="black") + 
            ggtitle("Geographic Extent of Boston with Drug Charges (2011-2014) Overlay")

for(k in 1:13){
par(mfcol = c(5,3),
         pty = "s")
  for(j in 10:12){
    data.table <- table(data[c(3,j)])
    row.names(data.table) <- levels(data$OFFENSE_CODE_GROUP)
    x <- names(data)[j]
    for(i in (5*k):(5*k + 4)){
      data.plot = data.table[i,]
      name = row.names(data.table)[i]
      barplot(data.plot,
              #col = "blue",
              ylab = "Counts",
              xlab = x,
              main = paste("Occurences of",name))
    }
  }
}
missing <- rep(NA,17)
for(i in 1:17){
missing[i] <- sum(is.na(data[i]))
}






