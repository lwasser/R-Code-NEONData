# read in data
# re-set working directory for your purposes
setwd("/Users/clunch/biogeochemistryIPT/prototype_data/megapit")
options(stringsAsFactors=F, strip.white=T)
megapit <- read.csv("Soil_pit_data_20141113.csv", header=T, fill=T)

# change column headers and stuff for functionality (Ed's headers are impossible)
# plus it looks like a bunch of longitudes are entered incorrectly? I haven't
# fixed them here, but a bunch are >0 and they should all be <0...
colnames(megapit)[c(3,4)] <- c("latitude","longitude")
colnames(megapit)[c(24,27,28)] <- c("horizon","top.depth","bottom.depth")
colnames(megapit)[c(91,121)] <- c("total.P", "total.N")
megapit$total.P <- megapit$total.P/1000
bottom.depth <- gsub(">","",megapit$bottom.depth)
depth <- rowMeans(data.frame(cbind(megapit$top.depth, as.numeric(bottom.depth))))


# first: trying to get to an interesting one-number-per-site thing for the map
# extract all A horizons, then summarize by site (some sites have subhorizons within A)
A.all <- megapit[grep("A", megapit$horizon),]
A.agg <- aggregate(A.all, by=list(A.all$Site), FUN=mean)
colnames(A.agg)[1] <- "site"

# check out N and P, see if we have an interesting range of values
barplot(A.agg$total.N, names.arg=A.agg$site, ylab="total N (g kg-1)")
barplot(A.agg$total.P, names.arg=A.agg$site, ylab="total P (g kg-1)")
NPrat <- A.agg$total.N/A.agg$total.P
barplot(NPrat, names.arg=A.agg$site, ylab="N:P ratio")
# N:P looks reasonable - there's a bigger range in N alone, but it's driven by just one site
plot(NPrat~A.agg$latitude, pch=20, xlab="Latitude", ylab="N:P ratio")
# N:P increases with latitude, more or less. weird.
# but we only have latitudes up to 47 or so


# second: plot elements by depth for each (?) site
# let's start with plotting something for all sites (!)
plot(I(-depth)~megapit$total.N, pch=20, cex=0.3, xlab="total N (g kg-1)", ylab="depth (cm)")
# nifty


