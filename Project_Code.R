# Run this command once to link to my google API account (please do not share this key)
  #register_google(key = "AIzaSyAxPCoK5JzfwK5A-7tvfYrk2L7Aiz99tZA", write=TRUE)

setwd("~/IMGT680")
load("crimedata1.RData")
load("crimedata2.RData")
load("crimedata3.RData")
load("crimedata4.RData")
load("crimedata5.RData")

data <- rbind(crime.data.1,crime.data.2,crime.data.3,crime.data.4,crime.data.5)


require(stringr)
regex <-"\\((\\d+\\.\\d+), (-\\d+\\.\\d+)\\)"
lat_long <- str_match(data$Location, regex)[,2:3]
lat_long <- apply(lat_long, 2, as.numeric)
colnames(lat_long) <- c("y", "x")
crime_latlong <- cbind(data, lat_long)

head(lat_long) #much better

drug_crimes <- crime_latlong[crime_latlong$OFFENSE_DESCRIPTION == "STALKING",]

# plot on a map
#install.packages("ggmap")
require(ggmap)
map.center <- geocode("Boston, MA")
Bos_map <- qmap(c(lon=map.center$lon, lat=map.center$lat), zoom=12)
g <- Bos_map + geom_point(aes(x=x, y=y), data=drug_crimes, size=3, alpha=0.2, color="red") + 
  ggtitle("Drug Charges in Boston by Location (2015-2018)")
print(g)
