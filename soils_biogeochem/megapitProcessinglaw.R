# read in data
# re-set working directory for your purposes
#setwd("/Users/clunch/biogeochemistryIPT/prototype_data/megapit")
#megapit <- read.csv("/Users/clunch/biogeochemistryIPT/prototype_data/megapit/Soil_pit_data_20141113.csv", header=T, fill=T)

options(stringsAsFactors=F, strip.white=T)
megapit <- read.csv("/Users/lwasser/Documents/GitHub/biogeochemistryIPT/prototype_data/megapit/Soil_pit_data_20141113.csv", header=T, fill=T)

# change column headers and stuff for functionality
# plus it looks like a bunch of longitudes are entered incorrectly? I haven't
# fixed them here, but a bunch are >0 and they should all be <0...
colnames(megapit)[c(3,4)] <- c("latitude","longitude")
colnames(megapit)[c(24,27,28)] <- c("horizon","top.depth","bottom.depth")
colnames(megapit)[c(91,120,121)] <- c("total.P", "total.C","total.N")


# let's create a subset with only the data that we want to work with
soilsSubset <- cbind(megapit[2],megapit[3],megapit[4],megapit[24],megapit[27],megapit[28],megapit[91],megapit[120],megapit[121])
#for some reason more than 244 rows populate - remove extra rows
soilsSubset <- soilsSubset[-c(255:504), ]
#check that the data look ok
head(soilsSubset,n=10)

soilsSubset$total.P <- soilsSubset$total.P/1000
#remove '>' from the bottom.depth column
bottom.depth <- gsub(">","",soilsSubset$bottom.depth)
depth <- rowMeans(data.frame(cbind(soilsSubset$top.depth, as.numeric(bottom.depth))))


# first: trying to get to an interesting one-number-per-site thing for the map
# extract all A horizons, then summarize by site (some sites have subhorizons within A)
A.all <- soilsSubset[grep("A", soilsSubset$horizon),]

#get total N and P average values
tot_N_P <- aggregate((A.all[7:8]), by=list(A.all$Site), FUN=mean)
latLong_sum<- aggregate((A.all[2:3]), by=list(A.all$Site), FUN=mean)
#calculate N:P ratio and added it to the NP dataframe
tot_N_P$NPratio <- tot_N_P$total.N/tot_N_P$total.P
#add lat long to the data
tot_N_P$lat <- latLong_sum$latitude
tot_N_P$long <- latLong_sum$longitude
colnames(tot_N_P)[1] <- "site"

#sanity check
head(tot_N_P,n=10)


#this throws errors as it can't mean av sites, lat, long etc.
#A.agg <- aggregate(A.all, by=list(A.all$Site), FUN=mean)
colnames(A.agg)[1] <- "site"

barplot(tot_N_P$total.N, names.arg=tot_N_P$site, ylab="total N (g kg-1)")
barplot(tot_N_P$total.P, names.arg=tot_N_P$site, ylab="total P (g kg-1)")
barplot(tot_N_P$NPratio, names.arg=tot_N_P$site, ylab="N:P ratio",
        main="Soil N:P Ratio for NEON Sites")


#stacked barplot
# Stacked Bar Plot with Colors and Legend
counts <- rbind(tot_N_P$total.N, tot_N_P$total.P)
barplot(counts, main="Stacked N:P Ratio",
        xlab="Site", col=c("darkblue","red"),
        names.arg=tot_N_P$site, legend = rownames(counts))



##################################
#Create a US Map of N:P Ratio 
####################################

#currently there are some lat long errors in the data -- will fix this manually for final plot!


library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggplot2)

#sometimes Rgdal doesn't want to install given the version of R you are running.
#try this:
#install.packages('rgdal', type='source')

# for theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")

#Load US state boundaries
us <- readOGR("C:/Users/lwasser/Documents/GitHub/NEON_HigherEd/code/BubbleMapData/gz_2010_us_040_00_500k.json", layer="OGRGeoJSON")

# Simplify the topology to decrease US Boundary file size 
us <- SpatialPolygonsDataFrame(gSimplify(us, tol=0.1, topologyPreserve=TRUE), 
                               data=us@data)

# Remove extraneous regions from the map
us <- us[!us$NAME %in% c("Alaska", "Hawaii", "Puerto Rico", "District of Columbia"),]

#This function turns a map into a data frame that can more easily be plotted with ggplot2.
map <- fortify(us, region="NAME")

#example data... we'll use soils data
# Pop Data
myData <- data.frame(name=c("Florida", "Colorado", "California", "Harvard", "Yellowstone"),
                     lat=c(28.1, 39, 37, 42, 44.6), 
                     long=c(-81.6, -105.5, -120, -71,-110),
                     pop=c(280, 156, 128, 118, 202))

# the map
#https://source.opennews.org/en-US/learning/choosing-right-map-projection/
gg <- ggplot()
# the base map
gg <- gg + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=group),
                    fill="#ffffff", color="#0e0e0e", size=0.15)

# Add bubbles - we'll use N:P ratio
gg <- gg + geom_point(data=tot_N_P, 
                      aes(x=long, y=lat, size=NPratio), color="#AD655F") 

#these limits are for a us map.
gg <- gg + xlim(-125, -75) + 
  ylim(25, 50) +
  labs(title="Species Occurrence Data") +
  coord_equal()













# N:P looks reasonable - there's a bigger range in N alone, but it's driven by just one site
plot(NPrat~A.agg$latitude, pch=20, xlab="Latitude", ylab="N:P ratio")
# N:P increases with latitude, more or less. weird.
# but we only have latitudes up to 47 or so


# second: plot elements by depth for each (?) site
# let's start with plotting something for all sites (!)
plot(I(-depth)~megapit$total.N, pch=20, cex=0.3, xlab="total N (g kg-1)", ylab="depth (cm)")
# nifty

# C by depth
plot(I(-depth)~megapit$total.C, pch=20, cex=0.3, xlab="total C (g kg-1)", ylab="depth (cm)")

plot(I(-depth)~megapit$total.C)
for(i in as.factor(megapit$Site))


