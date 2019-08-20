# Run this command once to link to my google API account (please do not share this key)
# register_google(key = "AIzaSyAxPCoK5JzfwK5A-7tvfYrk2L7Aiz99tZA", write=TRUE)

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

for(i in as.vector(unique(data$OFFENSE_CATEGORY))){
  crimes <- data[data$OFFENSE_CATEGORY == i,]
  p <- Bos_map + geom_point(aes(x=Long, y=Lat, colour=factor(HOUR)), 
                            data=crimes, size=3, alpha=0.2) + 
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


######### 
# Category Reduction
#########

crimes.violent <- c("Arson", "Simple Assault", "Ballistics", "Aggravated Assault", "Firearm Violations", "Homicide", "Explosives",
                    "Criminal Harassment", "Biological Threat", "Manslaughter")
crimes.traffic <- c("Towed", "Investigate Property", "Motor Vehicle Accident Response", "License Plate Related Incidents",
                    "Operating Under the Influence", "License Violation", "Evading Fare")
crimes.theft <- c("Larceny", "Auto Theft", "Robbery", "Property Lost", "Larceny From Motor Vehicle", 
                  "Residential Burglary", "Property Found", "Other Burglary", "Auto Theft Recovery", "Commercial Burglary",
                  "HOME INVASION", "Recovered Stolen Property", "Burglary - No Property Taken")
crimes.drugsex <- c("Drug Violation", "Prostitution", "HUMAN TRAFFICKING", "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE")
crimes.money <- c("Fraud", "Counterfeiting", "Confidence Games", "Embezzlement", "Gambling")
crimes.medical <- c("Fire Related Reports", "Medical Assistance")
crimes.other <- c("Vandalism", "Verbal Disputes",  "Other", "Assembly or Gathering Violations",
                  "Restraining Order Violations", "Violations", "Harassment", "Police Service Incidents",
                  "Warrant Arrests", "Disorderly Conduct", "Property Related Damage", "Missing Person Reported", 
                  "Investigate Person",  "Bomb Hoax", "Harbor Related Incidents", "Liquor Violation",
                  "Firearm Discovery", "Landlord/Tenant Disputes", "Missing Person Located", "Service", "Search Warrants",
                  "Offenses Against Child / Family", "Prisoner Related Incidents", "Phone Call Complaints", "Aircraft",
                  "INVESTIGATE PERSON")
for(i in 141898:319073){
  if (data$OFFENSE_CODE_GROUP[i] %in% crimes.violent){
    data$OFFENSE_CATEGORY[i] = "Violent"  
  }else if(data$OFFENSE_CODE_GROUP[i] %in% crimes.traffic){
    data$OFFENSE_CATEGORY[i] = "Traffic"
  }else if(data$OFFENSE_CODE_GROUP[i] %in% crimes.theft){
    data$OFFENSE_CATEGORY[i] = "Theft"
  }else if(data$OFFENSE_CODE_GROUP[i] %in% crimes.drugsex){
    data$OFFENSE_CATEGORY[i] = "Drugsex"
  }else if(data$OFFENSE_CODE_GROUP[i] %in% crimes.money){
    data$OFFENSE_CATEGORY[i] = "Money"
  }else if(data$OFFENSE_CODE_GROUP[i] %in% crimes.medical){
    data$OFFENSE_CATEGORY[i] = "Medical"
  }else if(data$OFFENSE_CODE_GROUP[i] %in% crimes.other){
    data$OFFENSE_CATEGORY[i] = "Other"
  }
  print(i)
}
table(data$OFFENSE_CATEGORY)*100/319073
data$OFFENSE_CATEGORY <- as.factor(data$OFFENSE_CATEGORY)

load("boston_crime_data_grouped.RData")
library(dplyr)
data %>% group_by(INCIDENT_NUMBER) %>% filter(n()>1)

data.month <- table(data[c(18,10)])
data.week <- table(data[c(18,11)])
data.hour <- table(data[c(18,12)])
row.names(data.month) <- unique(data$OFFENSE_CATEGORY)
row.names(data.week) <- unique(data$OFFENSE_CATEGORY)
row.names(data.hour) <- unique(data$OFFENSE_CATEGORY)
x.month <- names(data$MONTH)
x.week <- names(data$DAY_OF_WEEK)
x.hour <- names(data$HOUR)             
for(i in 1:7){
  par(mfcol = c(1,3),
      pty = "s")
  
  plot.month = data.month[i,]
  name = row.names(data.month)[i]
  barplot(plot.month,
          ylab = "Counts",
          xlab = x.month,
          main = paste("Monthly",name," crimes"))
  
  plot.hour = data.hour[i,]
  name = row.names(data.hour)[i]
  barplot(plot.hour,
          ylab = "Counts",
          xlab = x.week,
          main = paste("Hourly ",name," Crimes"))
  
  plot.week = data.week[i,]
  name = row.names(data.week)[i]
  barplot(plot.week,
          ylab = "Counts",
          xlab = x.hour,
          main = paste("Daily ",name," Crimes"))
}
                     

                                 
####################
#code to look at category distributions and graph it
Other <- filter(data, OFFENSE_CATEGORY =="Other")
Medical <- filter(data, OFFENSE_CATEGORY =="Medical")
Theft <- filter(data, OFFENSE_CATEGORY =="Theft")
Traffic <- filter(data, OFFENSE_CATEGORY =="Traffic")
Violent <- filter(data, OFFENSE_CATEGORY =="Violent")
Drugsex <- filter(data, OFFENSE_CATEGORY =="Drugsex")
Money <- filter(data, OFFENSE_CATEGORY =="Money")

barplot(table(Other$OFFENSE_CODE_GROUP), main = "Crime distribution in Other Category", ylab = "Numer of occurences",las=2)
barplot(table(Medical$OFFENSE_CODE_GROUP), main = "Crime distribution in Medical Category", ylab = "Numer of occurences",las=2)                     
barplot(table(Theft$OFFENSE_CODE_GROUP), main = "Crime distribution in Theft Category", ylab = "Numer of occurences",las=2)
barplot(table(Traffic$OFFENSE_CODE_GROUP), main = "Crime distribution in Traffic Category", ylab = "Numer of occurences",las=2)
barplot(table(Violent$OFFENSE_CODE_GROUP), main = "Crime distribution in Violent Category", ylab = "Numer of occurences",las=2)
barplot(table(Drugsexr$OFFENSE_CODE_GROUP), main = "Crime distribution in Drugsex Category", ylab = "Numer of occurences",las=2)
barplot(table(Money$OFFENSE_CODE_GROUP), main = "Crime distribution in Money Category", ylab = "Numer of occurences",las=2)



#########################
#K-means clustering

