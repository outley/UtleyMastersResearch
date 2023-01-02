#####Cleaning and Wrangling AMKE Prey Delivery Data
#10/04/2019
#Olivia Utley

#steps were as follows
#1. clean and wrangle raw prey data
#note many prey type labels were used over 5 years by multiple researchers
#2. create a label for the value of prey to fruit crops (beneficial, pest, neutral, unknown)
#3. download and collect landscape data for each kestrel nest box
#4. merge landscape with local characteristics data and prey data and continue to clean and wrangle
#5. add number of hours recorded and GDD data, format for analyses
#6. Summary data and visualization of prey value by taxa
#7. analyses, assess collinearity of vars, model selection
#8. model averaging
#9. Visualization
#note data files are saved and reloaded at multiple stages in order to preserve separate, clean data files

#load packages (to start)
library(dplyr)
library(ggplot2)
library(DataCombine)
library(tidyr)

#set working directory and load data
setwd("~/Documents/MSU/Research/Projects/AMKE Prey Delivery Paper/Data")
Data <- read.csv("Raw Delivery Data.csv")

#view data
as_tibble(Data)

#####
#1. Clean and wrangle data for analyses
#####

#relabel inconsistent or unclear general prey taxa labels

#look at all prey type labels
unique(Data$Prey.Type)
Data$Prey.Type <- as.character(Data$Prey.Type) #convert to character

#relabel unidentified prey types
Data$Prey.Type[grepl("Un", Data$Prey.Type)] <- "Unidentified" #any labels including "un" (e.g. Unknown)
Data$Prey.Type[grepl("un", Data$Prey.Type)] <- "Unidentified"
Data$Prey.Type[grepl("or", Data$Prey.Type)] <- "Unidentified" #any labels where multiple prey types were listed as possible
Data$Prey.Type[grepl("OR", Data$Prey.Type)] <- "Unidentified"
Data$Prey.Type[grepl("\\?", Data$Prey.Type)] <- "Unidentified" #any labels including a "?"
Data$Prey.Type[grepl("FU", Data$Prey.Type)] <- "Unidentified"
Data$Prey.Type[grepl("DROP", Data$Prey.Type)] <- "Unidentified" #any labels where kestrel dropped prey
Data$Prey.Type[grepl("^$", Data$Prey.Type)] <- "Unidentified" #any blank labels

unique(Data$Prey.Type)

#Organize prey types by selected taxonomic categories
Data$Prey.Type[grepl("amp", Data$Prey.Type)] <- "Herptile" #categorize amphibians as herptile
Data$Prey.Type[grepl("Amp", Data$Prey.Type)] <- "Herptile"
Data$Prey.Type[grepl("rep", Data$Prey.Type)] <- "Herptile" #reptile as herptile
Data$Prey.Type[grepl("Rep", Data$Prey.Type)] <- "Herptile"
Data$Prey.Type[grepl("herp", Data$Prey.Type)] <- "Herptile"
Data$Prey.Type[grepl("Sna", Data$Prey.Type)] <- "Herptile" #snake as herptile
Data$Prey.Type[grepl("pod", Data$Prey.Type)] <- "Arthropod"
Data$Prey.Type[grepl("ann", Data$Prey.Type)] <- "Annelid"
Data$Prey.Type[grepl("mam", Data$Prey.Type)] <- "Mammal"
Data$Prey.Type[grepl("bir", Data$Prey.Type)] <- "Bird"

unique(Data$Prey.Type) #recheck all prey types listed

#relabel remaining data into selected taxonomic categories

# Create replacements data frame
PTReplace <- data.frame(from = c("grub", " Mammal", "Bird   ", "Mammal ", "mollusc"), 
                       to = c("Arthropod", "Mammal", "Bird", "Mammal", "Mollusk"))

# Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Prey.Type", replaceData = PTReplace,
                     from = "from", to = "to", exact = T)

unique(Data$Prey.Type)

#Fix Stupid Mollusk issue
Data$Prey.Type[grepl("Mol", Data$Prey.Type)] <- "Mollusk"

unique(Data$Prey.Type)

#clean and relabel specific prey type data

Data$Specific.Prey.Type <- as.character(Data$Specific.Prey.Type) #convert variable to character

#check
as_tibble(Data)
unique(Data$Specific.Prey.Type)

#relabel questionable data as "unidentified"
Data$Specific.Prey.Type[grepl("Unkn", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("unkn", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("\\?", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("or ", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("^$", Data$Specific.Prey.Type)] <- "Unidentified"

unique(Data$Specific.Prey.Type) #check

#relabel non-species adjectives
Data$Specific.Prey.Type[grepl("baby bird", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("baby", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("large", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("large and unsure ", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("lightly colored", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("lighter and large", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("dark", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("adult", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("nestling", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("fledgling", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("unkown rodent", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("adult, large", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("obscured by head", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("large, possibly EUST", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("leg", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("bird", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("part", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("small", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("lighter and larger", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("large and lightly colored", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("large baby bird", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("bird", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("large rodent", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("adult.large", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("med", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("looks like just a chunk of something", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("long", Data$Specific.Prey.Type)] <- "Unidentified"
Data$Specific.Prey.Type[grepl("camera didn't catch it", Data$Specific.Prey.Type)] <- "Unidentified"

unique(Data$Specific.Prey.Type) #check

#Cleaning up specific prey types
#multiple rounds of relabeling
SPT1 <- data.frame(from = c("cricket", "grub", "mouse", "moth", "butterfly"), 
                   to = c("Cricket", "Grub", "Mouse", "Moth", "Butterfly"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT1,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 2
SPT2 <- data.frame(from = c("caterpillar", "vole", "beetle", "shrew", "grasshopper"), 
                   to = c("Caterpillar", "Vole", "Beetle", "Shrew", "Grasshopper"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT2,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 3
SPT3 <- data.frame(from = c("chipmunk", "dragonfly", "catapillar", "spider", "caterpilllar"), 
                   to = c("Chipmunk", "Dragonfly", "Caterpillar", "Spider", "Caterpillar"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT3,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 4
SPT4 <- data.frame(from = c("mole", "cricket ", "frog", "ground squirrel", "earthworm", "vole "), 
                   to = c("Mole", "Cricket", "Frog", "Ground Squirrel", "Earthworm", "Vole"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT4,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 5
SPT5 <- data.frame(from = c("Kildeer Chick", "worm", "13 lined ground squirrel", "catapillar "), 
                   to = c("Killdeer", "Earthworm", "Thirteen-lined Ground Squirrel", "Caterpillar"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT5,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 6
SPT6 <- data.frame(from = c("snake", "Ground Squirrel", "praying mantis", "damselfly", "dragonfly or damselfly", "Killdeer Chick"), 
                   to = c("Snake", "Thirteen-lined Ground Squirrel", "Mantis", "Odonata", "Odonata", "Killdeer"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT6,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 7
SPT7 <- data.frame(from = c("Worm", "Preying Mantis", "fly", "grashopper", "chunk of worm", "small grass hopper ", "MEVO", "grub "), 
                   to = c("Earthworm", "Mantis", "Fly", "Grasshopper", "Earthworm", "Grasshopper", "Vole", "Grub"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT7,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 8
SPT8 <- data.frame(from = c("chipmunk foot and tail", "chipmonk", "chipmunk ", "kildeer Chick", "moth ", "Larva", "Mouse Baby", "song sparrow", "CHSP", "beetle ", "grub  ", "dragonfly ", "EUST"), 
                   to = c("Chipmunk", "Chipmunk", "Chipmunk", "Killdeer", "Moth", "Grub", "Mouse", "Sparrow", "Sparrow", "Beetle", "Grub", "Odonata", "Starling"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT8,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 9
SPT9 <- data.frame(from = c("beatle ", "Mantis ", "cicada ", "Killdeer chick", "Bottom half of squirrel", "Sulfer butterfly", "chipmonk ", "small mouse", "cicada", "adult sparrow", "earth worm", "long grub", "snail", "large beetle", "large vole ", "mantis"), 
                   to = c("Beetle", "Mantis", "Cicada", "Killdeer", "Squirrel", "Butterfly", "Chipmunk", "Mouse", "Cicada", "Sparrow", "Earthworm", "Grub", "Snail", "Beetle", "Vole", "Mantis"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT9,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 10
SPT10 <- data.frame(from = c("baby mouse", "killdeer chick", "horsefly", "grasshoper", "catapiller", "sulfur butterfly", "peromyscus", "ground beetle", "jumping mouse", "june beetle", "alatus"), 
                    to = c("Mouse", "Killdeer", "Horsefly", "Grasshopper", "Caterpillar", "Butterfly", "Peromyscus", "Beetle", "Mouse", "Beetle", "Alatus"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT10,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

#Round 11
SPT11 <- data.frame(from = c("chickadee", "sparrow", "DOWO", "catepillar", "grub/borer", "salamander", "stinkbug", "Dragonfly", "vole-female chases male away", "grasshopper ", "butterfly/moth", "chipmunk = groundsquirrel", "chipmunk / ground squirrel", "grub/ catapiller"), 
                    to = c("Chickadee", "Sparrow", "Downy Woodpecker", "Caterpillar", "Grub", "Salamander", "Stinkbug", "Odonata", "Vole", "Grasshopper", "Unidentified", "Unidentified", "Unidentified", "Unidentified"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Specific.Prey.Type", replaceData = SPT11,
                     from = "from", to = "to", exact = T)

unique(Data$Specific.Prey.Type)

as_tibble(Data)

#####
#Assigning value to prey items based on literature
#####

#Add value column
Data$Prey.Value <- Data$Specific.Prey.Type #create new column for value
as_tibble(Data)

#Reorder dataframe
Data <- Data[c(1:3, 6, 7, 11)]
as_tibble(Data)

#multiple rounds of relabeling species as their value in new column 
#Replace with value
#only exact matches
PV1 <- data.frame(from = c("Killdeer", "Cricket", "Mouse", "Grub", "Caterpillar", "Odonata", "Spider", "Vole", "Earthworm", "Thirteen-lined Ground Squirrel", "Grasshopper", "Shrew", "Snake", "Beetle", "Moth", "Mantis", "Chipmunk", "Butterfly", "Frog", "Sparrow", "Peromyscus"), 
                  to = c("Beneficial", "Unknown", "Pest", "Unknown", "Pest", "Beneficial", "Beneficial", "Pest", "Pest", "Unknown", "Pest", "Beneficial", "Beneficial", "Unknown", "Pest", "Beneficial", "Unknown", "Neutral", "Beneficial", "Neutral", "Pest"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Prey.Value", replaceData = PV1,
                     from = "from", to = "to", exact = T)

unique(Data$Prey.Value)

#Replacing value for prey items without identified value
PV2 <- data.frame(from = c("Unidentified", "Duckling", "Fly", "Squirrel", "Starling", "Cicada", "Snail", "Horsefly", "Alatus", "Chickadee", "Downy Woodpecker", "Salamander", "Stinkbug", "Mole", "Large"), 
                  to = c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"))

#Replace patterns and return full data frame
Data <- FindReplace(data = Data, Var = "Prey.Value", replaceData = PV2,
                     from = "from", to = "to", exact = T)

unique(Data$Prey.Value)

as_tibble(Data)

write.csv(Data, "Tidy Delivery Data.csv")

#####
#3. Landscape metrics of kestrel diet data
#adapted from code provided by Mike Crossley and Olivia Smith
#3/12/22
#####

library(raster)
library(rgeos) #for SpatialPointsDataFrame
library(landscapemetrics)
library(lubridate)

#years: 2013-2018
#set working director to hard drive containing rasters
setwd("/Volumes/LaCie Work/AMKE CDL Data")

# read in NLCD rasters
land2013 = raster("2013_30m_cdls.img") #raster file 2013
land2014 = raster("2014_30m_cdls.img")
land2015 = raster("2015_30m_cdls.img")
land2016 = raster("2016_30m_cdls.img")
land2017 = raster("2017_30m_cdls.img")
land2018 = raster("2018_30m_cdls.img")
str(land2014) #land cover data
proj4string(land2014)

#load data
setwd("~/Documents/MSU/Research/Projects/AMKE Prey Delivery Paper/Data/")
Data <- read.csv("Tidy Delivery Data.csv")

BoxLoc = read.csv('All Nest Locations.csv',as.is=T,check.names=F,header=T) #file with coordinates for each location
str(BoxLoc) #sample sites

#Create location/long/lat dataframe
as_tibble(BoxLoc)
locdat = BoxLoc[c(1,3,2)] #sub-setting location, longitude, and latitude from the larger data 

dat.shp = SpatialPointsDataFrame(coords=locdat[,2:3],data=locdat,proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs'))
dat.shp2013 = spTransform(dat.shp,CRS(proj4string(land2014))) ##this is the line we needed
plot(land2013) 
plot(dat.shp2013,add=T,pch=16,col='cyan') #check alignment, takes a moment

rcl = read.csv('cdl_reclass.csv',as.is=T,check.names=F,header=T) #reclassification key for land cover raster

#Add 9999 for empty classes
rcl[rcl == ""] <- 9999
rcl  

#subset rcl
#rclwood = rcl[c(1,4)] #sub-setting rcl for woody/nonwoody
#rclann = rcl[c(1,5)] #sub-setting rcl for annual/perennial
#rclhite = rcl[c(1,6)] #sub-setting rcl for height
rclopen = rcl[c(1,7)] #subsetting rcl for open/forested habitat


rclopen[,1] = as.numeric(rclopen[,1])
str(rclopen) #fix weird character that always shows up in .csv files from Olivia lol!
rclopen2 = rclopen; rclopen2[which(rclopen2[,2]==3),2] = 2 #reclassify 'other' to enable contrast between crops and everything else

###rcl is prepping to look at proportion of cover for reclassification 2
###rcl2 is looking at the amount of edge of reclassification 1 vs. reclassification 2 and 3

locdat$LonLat = paste(locdat$Longitude, locdat$Latitude, sep='_')
lonlats = unique(locdat$LonLat); length(lonlats) #save some time by extracting summaries only from unique lat/lon
#they'll be put back in to the summary 

#2013
locdat$p.open2013 = locdat$cropED2013 = NA
for (i in 1:length(lonlats)){
  print(noquote(i))
  dat.pos = which(locdat$LonLat==lonlats[i]) #which rows (farms) correspond to this lat/lon
  site20131 = dat.shp2013[dat.pos[1],] #spatial point for this lat/lon (in cases where many farms share lat/lon, use first farm)
  buf2013 = buffer(site20131,width=1500) #1500m = 1.5km buffer
  land20131 = crop(land2013,buf2013) #clip land cover raster to buffer extent
  land20132 = mask(land20131,buf2013) #mask pixels outside of 1.5km circular buffer
  land20133 = reclassify(land20132,rclopen) #for prop. open
  land20134 = reclassify(land20132,rclopen2) #for crop edge density
  ed2013 = lsm_l_ed(land20134,count_boundary=F,directions=8) #edge density is reported in units of meters per hectare (m/ha)
  land20133.mat = as.matrix(land20133) #simplifies extracting prop. open
  locdat$p.open2013[dat.pos] = length(which(land20133.mat==1)) / length(which(!is.na(land20133.mat)))
  locdat$cropED2013[dat.pos] = ed2013$value / 1500 #convert m/ha to km/ha
}

#2014
dat.shp2014 = spTransform(dat.shp,CRS(proj4string(land2014))) ##this is the line we needed

locdat$p.open2014 = locdat$cropED2014 = NA
for (i in 1:length(lonlats)){
  print(noquote(i))
  dat.pos = which(locdat$LonLat==lonlats[i]) #which rows (farms) correspond to this lat/lon
  site20141 = dat.shp2014[dat.pos[1],] #spatial point for this lat/lon (in cases where many farms share lat/lon, use first farm)
  buf2014 = buffer(site20141,width=1500) #1500m = 1.5km buffer
  land20141 = crop(land2014,buf2014) #clip land cover raster to buffer extent
  land20142 = mask(land20141,buf2014) #mask pixels outside of 1.5km circular buffer
  land20143 = reclassify(land20142,rclopen) #for prop. open
  land20144 = reclassify(land20142,rclopen2) #for crop edge density
  ed2014 = lsm_l_ed(land20144,count_boundary=F,directions=8) #edge density is reported in units of meters per hectare (m/ha)
  land20143.mat = as.matrix(land20143) #simplifies extracting prop. open
  locdat$p.open2014[dat.pos] = length(which(land20143.mat==1)) / length(which(!is.na(land20143.mat)))
  locdat$cropED2014[dat.pos] = ed2014$value / 1500 #convert m/ha to km/ha
}

#2015
dat.shp2015 = spTransform(dat.shp,CRS(proj4string(land2015))) ##this is the line we needed

locdat$p.open2015 = locdat$cropED2015 = NA
for (i in 1:length(lonlats)){
  print(noquote(i))
  dat.pos = which(locdat$LonLat==lonlats[i]) #which rows (farms) correspond to this lat/lon
  site20151 = dat.shp2015[dat.pos[1],] #spatial point for this lat/lon (in cases where many farms share lat/lon, use first farm)
  buf2015 = buffer(site20151,width=1500) #1500m = 1.5km buffer
  land20151 = crop(land2015,buf2015) #clip land cover raster to buffer extent
  land20152 = mask(land20151,buf2015) #mask pixels outside of 1.5km circular buffer
  land20153 = reclassify(land20152,rclopen) #for prop. open
  land20154 = reclassify(land20152,rclopen2) #for crop edge density
  ed2015 = lsm_l_ed(land20154,count_boundary=F,directions=8) #edge density is reported in units of meters per hectare (m/ha)
  land20153.mat = as.matrix(land20153) #simplifies extracting prop. open
  locdat$p.open2015[dat.pos] = length(which(land20153.mat==1)) / length(which(!is.na(land20153.mat)))
  locdat$cropED2015[dat.pos] = ed2015$value / 1500 #convert m/ha to km/ha
}

#2016
dat.shp2016 = spTransform(dat.shp,CRS(proj4string(land2016))) ##this is the line we needed

locdat$p.open2016 = locdat$cropED2016 = NA
for (i in 1:length(lonlats)){
  print(noquote(i))
  dat.pos = which(locdat$LonLat==lonlats[i]) #which rows (farms) correspond to this lat/lon
  site20161 = dat.shp2016[dat.pos[1],] #spatial point for this lat/lon (in cases where many farms share lat/lon, use first farm)
  buf2016 = buffer(site20161,width=1500) #1500m = 1.5km buffer
  land20161 = crop(land2016,buf2016) #clip land cover raster to buffer extent
  land20162 = mask(land20161,buf2016) #mask pixels outside of 1.5km circular buffer
  land20163 = reclassify(land20162,rclopen) #for prop. open
  land20164 = reclassify(land20162,rclopen2) #for crop edge density
  ed2016 = lsm_l_ed(land20164,count_boundary=F,directions=8) #edge density is reported in units of meters per hectare (m/ha)
  land20163.mat = as.matrix(land20163) #simplifies extracting prop. open
  locdat$p.open2016[dat.pos] = length(which(land20163.mat==1)) / length(which(!is.na(land20163.mat)))
  locdat$cropED2016[dat.pos] = ed2016$value / 1500 #convert m/ha to km/ha
}

#2017
dat.shp2017 = spTransform(dat.shp,CRS(proj4string(land2017))) ##this is the line we needed

locdat$p.open2017 = locdat$cropED2017 = NA
for (i in 1:length(lonlats)){
  print(noquote(i))
  dat.pos = which(locdat$LonLat==lonlats[i]) #which rows (farms) correspond to this lat/lon
  site20171 = dat.shp2017[dat.pos[1],] #spatial point for this lat/lon (in cases where many farms share lat/lon, use first farm)
  buf2017 = buffer(site20171,width=1500) #1500m = 1.5km buffer
  land20171 = crop(land2017,buf2017) #clip land cover raster to buffer extent
  land20172 = mask(land20171,buf2017) #mask pixels outside of 1.5km circular buffer
  land20173 = reclassify(land20172,rclopen) #for prop. open
  land20174 = reclassify(land20172,rclopen2) #for crop edge density
  ed2017 = lsm_l_ed(land20174,count_boundary=F,directions=8) #edge density is reported in units of meters per hectare (m/ha)
  land20173.mat = as.matrix(land20173) #simplifies extracting prop. open
  locdat$p.open2017[dat.pos] = length(which(land20173.mat==1)) / length(which(!is.na(land20173.mat)))
  locdat$cropED2017[dat.pos] = ed2017$value / 1500 #convert m/ha to km/ha
}

#2018
dat.shp2018 = spTransform(dat.shp,CRS(proj4string(land2018))) ##this is the line we needed

locdat$p.open2018 = locdat$cropED2018 = NA
for (i in 1:length(lonlats)){
  print(noquote(i))
  dat.pos = which(locdat$LonLat==lonlats[i]) #which rows (farms) correspond to this lat/lon
  site20181 = dat.shp2018[dat.pos[1],] #spatial point for this lat/lon (in cases where many farms share lat/lon, use first farm)
  buf2018 = buffer(site20181,width=1500) #1500m = 1.5km buffer
  land20181 = crop(land2018,buf2018) #clip land cover raster to buffer extent
  land20182 = mask(land20181,buf2018) #mask pixels outside of 1.5km circular buffer
  land20183 = reclassify(land20182,rclopen) #for prop. open
  land20184 = reclassify(land20182,rclopen2) #for crop edge density
  ed2018 = lsm_l_ed(land20184,count_boundary=F,directions=8) #edge density is reported in units of meters per hectare (m/ha)
  land20183.mat = as.matrix(land20183) #simplifies extracting prop. open
  locdat$p.open2018[dat.pos] = length(which(land20183.mat==1)) / length(which(!is.na(land20183.mat)))
  locdat$cropED2018[dat.pos] = ed2018$value / 1500 #convert m/ha to km/ha
}

#####
#4. Merging landscape data into kestrel diet data
#####

setwd("~/Documents/MSU/Research/Projects/AMKE Prey Delivery Paper/Data/") #shift back to original working directory
Data <- read.csv("Tidy Delivery Data.csv")

#make sure nest box names match between data frames
unique(Data$Box.Name)
unique(locdat$Box.Name)

Data$Box.Name[Data$Box.Name == "Hartmann"] <- "Hartman"
Data$Box.Name[Data$Box.Name == "Calsbeek"] <- "Calsbeak"
Data$Box.Name[Data$Box.Name == "Shady"] <- "Shady Lane"
Data$Box.Name[Data$Box.Name == "Galens South"] <- "Galen South"
Data$Box.Name[Data$Box.Name == "Galens North"] <- "Galen North"
Data$Box.Name[Data$Box.Name == "Grant07"] <- "Grant"
Data$Box.Name[Data$Box.Name == "BahleN"] <- "Bahle North"
Data$Box.Name[Data$Box.Name == "AndersonW"] <- "Anderson West"

unique(Data$Box.Name)

#Merge landscape data (locdat) to prey data (Data)
colnames(locdat)[1] <- "Box.Name"

Data <- merge(Data, locdat, by = "Box.Name",
              all.x = T)

#Add plain year column

#format?
Data <- Data %>%
  mutate(Year = case_when(
    startsWith(Date, "2013") ~ "2013",
    startsWith(Date, "2014") ~ "2014",
    startsWith(Date, "2015") ~ "2015",
    startsWith(Date, "2016") ~ "2016",
    startsWith(Date, "2017") ~ "2017",
    startsWith(Date, "2018") ~ "2018",
  ))


#match landscape data for specific year to location by year
#edge density
Data$openED <- ifelse(Data$Year == 2013, Data$cropED2013,
                      ifelse(Data$Year == 2014, Data$cropED2014,
                             ifelse(Data$Year == 2015, Data$cropED2015,
                                    ifelse(Data$Year == 2016, Data$cropED2016,
                                           ifelse(Data$Year == 2017, Data$cropED2017,
                                                  ifelse(Data$Year == 2018, Data$cropED2018, NA))))))

#percent open land cover
Data$p.open <- ifelse(Data$Year == 2013, Data$p.open2013,
                      ifelse(Data$Year == 2014, Data$p.open2014,
                             ifelse(Data$Year == 2015, Data$p.open2015,
                                    ifelse(Data$Year == 2016, Data$p.open2016,
                                           ifelse(Data$Year == 2017, Data$p.open2017,
                                                  ifelse(Data$Year == 2018, Data$p.open2018, NA))))))


sum(is.na(Data$p.open)) #check for the number of NAs
#should be 0

#Summariesss
range(Data$p.open)

range(Data$openED)

#Merge local characteristic data to data file
LocalChr <- read.csv("Local Characteristics.csv")

colnames(LocalChr)[1] <- "Box.Name" #make Box.Name consistent for merging

Data <- merge(Data, LocalChr, by = "Box.Name",
              all.x = T)

sum(is.na(Data$Distance.to.nearest.linear.or.forest.edge.or.tree.patch..m.)) #count NAs

sum(is.na(Data$Distance.to.forest.edge.of.at.least.50m.x.50m)) #count NAs

as_tibble(Data)
Data <-  Data[c(1,3,15,16,31,4,5,6,7,8,9,10,11,32,33,37,38,39)] #keep important columns

#change a couple headers
colnames(Data)[16] <- "Patch.Distance"
colnames(Data)[17] <- "Forest.Distance"

#some t tests to compare landscapes between blueberry and cherry crops
t.test(p.open ~ Crop.Type, data = Data)
t.test(openED ~ Crop.Type, data = Data)
t.test(Distance.to.forest.edge.of.at.least.50m.x.50m ~ Crop.Type, data = Data)
t.test(Distance.to.nearest.linear.or.forest.edge.or.tree.patch..m. ~ Crop.Type, data = Data)

#Write up/save
write.csv(Data, "Diet Local Landscape Data.csv")

#####
#5. Format for analyses
#####

#reload data point
setwd("~/Documents/MSU/Research/Projects/AMKE Prey Delivery Paper/Data/")
Data <- read.csv("Diet Local Landscape Data.csv")

#create unique identifier for data points (location and year)
Data$BoxYearID <- paste(Data$Box.Name, Data$Year, sep="")

#Sum prey type collected at each box by year
Data <- Data %>%
  group_by(Crop.Type, Box.Name, Year, BoxYearID, p.open, openED,
           Patch.Distance, Forest.Distance, Prey.Type) %>% #all the variables for analyses
  summarize(N = n()) %>% #sum
  ungroup() %>%
  complete(BoxYearID, Prey.Type, #creates 0's for missing prey types for location and year
           fill = list(N = 0))

#Fill in missing data with matching data from above
Data <- Data %>%
  group_by(BoxYearID) %>% 
  fill(Box.Name, Crop.Type, p.open, openED, Year, Patch.Distance, Forest.Distance)

#fill in "up"
Data <- Data %>%
  group_by(BoxYearID) %>% 
  fill(Box.Name, Crop.Type, p.open, openED, Year, Patch.Distance, Forest.Distance, 
       .direction="up")

#add number of hours nest was recorded for offset
RecordHours <- read.csv("Nest Monitoring Data.csv")

RecordHours$Date <- as.character(RecordHours$Date)

RecordHours <- RecordHours %>%
  mutate(Year = case_when(
    startsWith(Date, "2013") ~ "2013",
    startsWith(Date, "2014") ~ "2014",
    startsWith(Date, "2015") ~ "2015",
    startsWith(Date, "2016") ~ "2016",
    startsWith(Date, "2017") ~ "2017",
    startsWith(Date, "2018") ~ "2018",
  ))

unique(Data$Box.Name)
unique(RecordHours$Box.Name)

#make sure box names match
RecordHours$Box.Name[RecordHours$Box.Name == "Hartmann"] <- "Hartman"
RecordHours$Box.Name[RecordHours$Box.Name == "Calsbeek"] <- "Calsbeak"
RecordHours$Box.Name[RecordHours$Box.Name == "Shady"] <- "Shady Lane"
RecordHours$Box.Name[RecordHours$Box.Name == "Galens South"] <- "Galen South"
RecordHours$Box.Name[RecordHours$Box.Name == "Galens North"] <- "Galen North"
RecordHours$Box.Name[RecordHours$Box.Name == "Grant07"] <- "Grant"
RecordHours$Box.Name[RecordHours$Box.Name == "BahleN"] <- "Bahle North"
RecordHours$Box.Name[RecordHours$Box.Name == "AndersonW"] <- "Anderson West"

unique(Data$Box.Name) #check

#add GDD
GDDbb <- read.csv("mawn fennville 2016-18 DD42.csv") #blueberries Fennville
GDDb2 <- read.csv("mawn lawrence 2016-18 DD42.csv") #blueberries Lawrence county
GDDc <- read.csv("2013-2016 Kestrel Prey Deliveries.csv") #cherries

#focus on GDD
as_tibble(GDDc)
as_tibble(GDDbb)
as_tibble(GDDb2)

GDDc <- GDDc[c(1, 5, 6, 11)] #keep box location, date, and GDD
GDDbb <- GDDbb[c(1,5)] #date and GDD
GDDb2 <- GDDb2[c(1,5)] #date and GDD

#check GDDc box names
unique(GDDc$name)

#correct
GDDc$name[GDDc$name == "Shady"] <- "Shady Lane"
GDDc$name[GDDc$name == "Grant07"] <- "Grant"
GDDc$name[GDDc$name == "BahleN"] <- "Bahle North"
GDDc$name[GDDc$name == "AndersonW"] <- "Anderson West"
GDDc$name[GDDc$name == "Stanek2013"] <- "Stanek"

unique(GDDc$name)

#Create "GDD1" variable
#convert characters to dates
GDDc$date <- as.Date(GDDc$date, "%m/%d/%y")
GDDbb$date <- as.Date(GDDbb$date, "%m/%d/%y")
GDDb2$date <- as.Date(GDDb2$date, "%m/%d/%y")

#cherry data already has nextbox locations, blueberry data does not
#need to match GDD from the right location  to appropriate nest boxes

#filter out Fennville locations from record hours
RHbb <- RecordHours %>% filter(Box.Name == "Andy" | 
                                 Box.Name == "Blueberry Valley" |
                                 Box.Name == "Erny" |
                                 Box.Name == "Patterson")

#filter out Lawrence locations from record hours
RHb2 <- RecordHours %>% filter(Box.Name == "Calsbeak" |
                                 Box.Name == "Corner" |
                                 Box.Name == "East Pond" |
                                 Box.Name == "Field" |
                                 Box.Name == "First Pick" |
                                 Box.Name == "Galen North" |
                                 Box.Name == "Galen South" |
                                 Box.Name == "Gray Barn" |
                                 Box.Name == "Hartman" |
                                 Box.Name == "Hoover" |
                                 Box.Name == "New Day North" |
                                 Box.Name == "Red Barn")

#convert RHbb "date" to date
RHbb$Date <- ymd(RHbb$Date)
RHb2$Date <- ymd(RHb2$Date)

#change name to "date"
colnames(RHbb)[which(names(RHbb) == "Date")] <- "date"
colnames(RHb2)[which(names(RHb2) == "Date")] <- "date"

#merge GDD values to recording hours data by appropriate location
RHbb <- merge(RHbb, GDDbb, by = "date",
              all.x = T)
RHb2 <- merge(RHb2, GDDb2, by = "date",
              all.x = T)

#add boxyear identifier
GDDc$BoxYearID <- paste(GDDc$name, GDDc$year, sep="")
RHbb$BoxYearID <- paste(RHbb$Box.Name, RHbb$Year, sep="")
RHb2$BoxYearID <- paste(RHb2$Box.Name, RHb2$Year, sep="")

#filter for start date of nestbox to keep initial GDD (one GDD measurement per year)
#cherries
GDDc <- GDDc %>% 
  group_by(BoxYearID) %>%
  filter(date == min(date))

#Fennville blueberries
RHbb <- RHbb %>% 
  group_by(BoxYearID) %>%
  filter(date == min(date))

#lawrence blueberries
RHb2 <- RHb2 %>% 
  group_by(BoxYearID) %>%
  filter(date == min(date))

#sum recording hours by box and year
RecordHours$Hours <- as.numeric(RecordHours$Hours)

RecordHours <- RecordHours %>% group_by(Box.Name, Year) %>% summarize(Hours = sum(Hours))

#create identifier for record hours
RecordHours$BoxYearID <- paste(RecordHours$Box.Name, RecordHours$Year, sep="")

#combine RHbb and GDDc into one dataframe

as_tibble(RHbb)
as_tibble(GDDc)

#keep GDD and BoxYear ID
RHbb <- RHbb[c(7,8)]

RHb2 <- RHb2[c(7,8)]

GDDc <- GDDc[c(4,5)]

#change bkem_sum header to GDD
colnames(RHbb)[which(names(RHbb) == "bkem_sum")] <- "gdd"

colnames(RHb2)[which(names(RHb2) == "bkem_sum")] <- "gdd"

#combine GDD data
GDD <- rbind(GDDc, RHbb, RHb2)

#merge GDD back into RecordHours with total hours summed
RecordHours <- merge(RecordHours, GDD, by = "BoxYearID",
                     all.x = T)

unique(GDD$BoxYearID)
unique(RecordHours$BoxYearID)

#rename gdd
colnames(RecordHours)[which(names(RecordHours) == "gdd")] <- "gddi"

#Merge to main data
Data <- merge(Data, RecordHours, by = "BoxYearID",
              all.x = T)

as_tibble(Data)

#keep data for analyses
Data <- Data[c(1:10,13,14)]

Data <- Data %>% 
  rename(
    Box.Name = Box.Name.x,
    Year = Year.x
  )

write.csv(Data, "Taxa Analyses Data.csv")

#HAHA data set



#Sum data by box
preytype <- dat %>% group_by(Prey.Type) %>% summarise(n())
preyvalue <- dat %>% group_by(Prey.Value) %>% summarise(n())

preytv <- dat %>% group_by(Prey.Type, Prey.Value) %>% summarise(n())

names(preytype)[2] <- "sum"
names(preyvalue)[2] <- "sum"
names(preytv)[3] <- "sum"

ptvannelid <- preytv[which(preytv$Prey.Type=='Annelid'), ]
ptvarachnid <- preytv[which(preytv$Prey.Type=='Arachnid'), ]
ptvarthropod <- preytv[which(preytv$Prey.Type=='Arthropod'), ]
ptvbird <- preytv[which(preytv$Prey.Type=='Bird'), ]
ptvherptile <- preytv[which(preytv$Prey.Type=='Herptile'), ]
ptvmammal <- preytv[which(preytv$Prey.Type=='Mammal'), ]
ptvmollusk <- preytv[which(preytv$Prey.Type=='Mollusk'), ]
ptvuni <- preytv[which(preytv$Prey.Type=='Unidentified'), ]

#pie charts
#install.packages("ggrepel")
library(ggrepel)

#prey type
tiff("Prey Type Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(preytype, aes(x="", y=sum, fill=Prey.Type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()
dev.off()

#prey value
tiff("Prey Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(preyvalue, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()
dev.off()

#prey type and value
#Annelid
ptvannelid <- ptvannelid %>% 
  mutate(csum = rev(cumsum(rev(sum))), 
         pos = sum/2 + lead(csum, 1),
         pos = if_else(is.na(pos), sum/2, pos))

tiff("Annelid Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(ptvannelid, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Annelids") +
  theme_void() +
  geom_label_repel(data = ptvannelid,
                   aes(y = pos, label = paste0(sum)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)
dev.off()

#Arachnids
ptvarachnid <- ptvarachnid %>% 
  mutate(csum = rev(cumsum(rev(sum))), 
         pos = sum/2 + lead(csum, 1),
         pos = if_else(is.na(pos), sum/2, pos))

tiff("Arachnid Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(ptvarachnid, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Arachnids") +
  theme_void() +
  geom_label_repel(data = ptvarachnid,
                   aes(y = pos, label = paste0(sum)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)
dev.off()

#Arthropod
ptvarthropod <- ptvarthropod %>% 
  mutate(csum = rev(cumsum(rev(sum))), 
         pos = sum/2 + lead(csum, 1),
         pos = if_else(is.na(pos), sum/2, pos))

tiff("Arthropod Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(ptvarthropod, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Arthropods") +
  theme_void() +
  geom_label_repel(data = ptvarthropod,
                   aes(y = pos, label = paste0(sum)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)
dev.off()

#Bird
ptvbird <- ptvbird %>% 
  mutate(csum = rev(cumsum(rev(sum))), 
         pos = sum/2 + lead(csum, 1),
         pos = if_else(is.na(pos), sum/2, pos))

tiff("Bird Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(ptvbird, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Birds") +
  theme_void() +
  geom_label_repel(data = ptvbird,
                   aes(y = pos, label = paste0(sum)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)
dev.off()

#herptile
ptvherptile <- ptvherptile %>% 
  mutate(csum = rev(cumsum(rev(sum))), 
         pos = sum/2 + lead(csum, 1),
         pos = if_else(is.na(pos), sum/2, pos))

tiff("Herptile Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(ptvherptile, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Herptiles") +
  theme_void() +
  geom_label_repel(data = ptvherptile,
                   aes(y = pos, label = paste0(sum)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)
dev.off()

#mammal
ptvmammal <- ptvmammal %>% 
  mutate(csum = rev(cumsum(rev(sum))), 
         pos = sum/2 + lead(csum, 1),
         pos = if_else(is.na(pos), sum/2, pos))

tiff("Mammal Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(ptvmammal, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Mammals") +
  theme_void() +
  geom_label_repel(data = ptvmammal,
                   aes(y = pos, label = paste0(sum)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)
dev.off()

#mollusk
ptvmollusk <- ptvmollusk %>% 
  mutate(csum = rev(cumsum(rev(sum))), 
         pos = sum/2 + lead(csum, 1),
         pos = if_else(is.na(pos), sum/2, pos))

tiff("Mollusk Value Pie.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(ptvmollusk, aes(x="", y=sum, fill=Prey.Value)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Mollusks") +
  theme_void() +
  geom_label_repel(data = ptvmollusk,
                   aes(y = pos, label = paste0(sum)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)
dev.off()









#####
#7. ANALYSES
#####

#install.packages("TMB")
library(glmmTMB)
library(lme4)
#install.packages('DHARMa')
library(DHARMa)
#install.packages("bbmle")
library(bbmle)
#install.packages("performance")
library(performance)

#standardize predictors
p.open <- as.vector(Data$p.open)
mp.open <- mean(p.open, na.rm=TRUE)
sdp.open <- sd(p.open, na.rm=TRUE)
Data$popstdrd <- as.vector((p.open-mp.open)/sdp.open)

openED <- as.vector(Data$openED)
mopenED <- mean(openED, na.rm=TRUE)
sdopenED <- sd(openED, na.rm=TRUE)
Data$edstdrd <- as.vector((openED-mopenED)/sdopenED)

patch <- as.vector(Data$Patch.Distance)
mpatch <- mean(patch, na.rm=TRUE)
sdpatch <- sd(patch, na.rm=TRUE)
Data$patchstdrd <- as.vector((patch-mpatch)/sdpatch)

forest <- as.vector(Data$Forest.Distance)
mforest <- mean(forest, na.rm=TRUE)
sdforest <- sd(forest, na.rm=TRUE)
Data$foreststdrd <- as.vector((forest-mforest)/sdforest)

gddi <- as.vector(Data$gddi)
mgddi <- mean(gddi, na.rm=TRUE)
sdgddi <- sd(gddi, na.rm=TRUE)
Data$gddistdrd <- as.vector((gddi-mgddi)/sdgddi)

unique(Data$Prey.Type)

#make year factor for random effect analyses
Data$Year <- as.factor(Data$Year)

#sort by prey type
DataAnn <- Data[which(Data$Prey.Type=='Annelid'), ]
DataAra <- Data[which(Data$Prey.Type=='Arachnid'), ]
DataArt <- Data[which(Data$Prey.Type=='Arthropod'), ]
DataBir <- Data[which(Data$Prey.Type=='Bird'), ]
DataHer <- Data[which(Data$Prey.Type=='Herptile'), ]
DataMam <- Data[which(Data$Prey.Type=='Mammal'), ]
DataMol <- Data[which(Data$Prey.Type=='Mollusk'), ]
DataUni <- Data[which(Data$Prey.Type=='Unidentified'), ]

#histograms to check distributions
ggplot(DataAnn, aes(N)) +
  geom_histogram()

ggplot(DataArt, aes(N)) +
  geom_histogram()

ggplot(DataBir, aes(N)) +
  geom_histogram()

ggplot(DataHer, aes(N)) +
  geom_histogram()

ggplot(DataMam, aes(N)) +
  geom_histogram()

ggplot(DataUni, aes(N)) +
  geom_histogram()

unique(Data$Box.Name)
unique(Data$Year)

#####
#Arthropods
#####

#single fixed effect (predictor var)
mart1 <- glmmTMB(N ~ -1 + Crop.Type + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart2 <- glmmTMB(N ~ -1 + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart3 <- glmmTMB(N ~ -1 + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart4 <- glmmTMB(N ~ -1 + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart5 <- glmmTMB(N ~ -1 + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
warnings()

summary(mart1)
summary(mart2)
summary(mart3)
summary(mart4)
summary(mart5)

#figuring out over dispersion, checking models for best distribution
mart11 <- glmmTMB(N ~ -1 + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom1)
mart111 <- glmmTMB(N ~ -1 + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart1111 <- glmmTMB(N ~ -1 + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = poisson(log))
mart11111 <- glmmTMB(N ~ -1 + (1|Box.Name) + offset(log(Hours)), data = DataArt, ziformula=~ -1 +1, family = poisson)

summary(mart11)
summary(mart111)
summary(mart1111)
summary(mart11111)

plot(simulateResiduals(mart11))
plot(simulateResiduals(mart111))
plot(simulateResiduals(mart11111))

mart22 <- glmmTMB(N ~ -1 + patchstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom1)
mart33 <- glmmTMB(N ~ -1 + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom1)
mart44 <- glmmTMB(N ~ -1 + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom1)
mart55 <- glmmTMB(N ~ -1 + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom1)

summary(mart22)
summary(mart33)
summary(mart44)
summary(mart55)

#doubles
mart12 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart13 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart14 <- glmmTMB(N ~ -1 + Crop.Type + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart15 <- glmmTMB(N ~ -1 + Crop.Type + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)


summary(mart12)
summary(mart13)
summary(mart14)
summary(mart15)

mart23 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart24 <- glmmTMB(N ~ -1 + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart25 <- glmmTMB(N ~ -1 + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart23)
summary(mart24)
summary(mart25)

mart34 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart35 <- glmmTMB(N ~ -1 + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart34)
summary(mart35)

mart45 <- glmmTMB(N ~ -1 + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart45)

#triples
mart123 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart124 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart125 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart123)
summary(mart124)
summary(mart125)

mart134 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart135 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart134)
summary(mart135)

mart234 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart235 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart234)
summary(mart235)

mart345 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart345)

#quads

mart1234 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)
mart1235 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart1234)
summary(mart1235)

mart1345 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart1345)

mart2345 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart2345)

#global

mart12345 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt, family = nbinom2)

summary(mart12345)

#Random effects check
#martre1 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#martre2 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#martre3 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)) + (1|Box.Name) + offset(log(Hours)), data = DataArt)

#AIC check model fit
AICctab(mart1, mart12, mart123, mart1234, mart12345, mart1235, mart124, mart125, mart13,
        mart134, mart1345, mart135, mart14, mart15, mart2, mart23, mart234, mart2345, 
        mart235, mart24, mart25, mart3, mart34, mart345, mart35, mart4, mart45, mart5, 
        weights=TRUE)

#select models 5% contribution or higher
summary(mart15) #48%
summary(mart125) #17%
summary(mart135) #14%
summary(mart1345) #5%
summary(mart1235) #5%

#check for collinearity
check_collinearity(mart1) #null
check_collinearity(mart12) #low
check_collinearity(mart123) #low
check_collinearity(mart1234) #low
check_collinearity(mart12345)
check_collinearity(mart1235)
check_collinearity(mart124) #low
check_collinearity(mart125)
check_collinearity(mart13)
check_collinearity(mart134)
check_collinearity(mart1345)
check_collinearity(mart135)
check_collinearity(mart14)
check_collinearity(mart15)
check_collinearity(mart2)
check_collinearity(mart23)
check_collinearity(mart234)
check_collinearity(mart2345)
check_collinearity(mart235)
check_collinearity(mart24)
check_collinearity(mart25)
check_collinearity(mart3)
check_collinearity(mart34)
check_collinearity(mart345)
check_collinearity(mart35)
check_collinearity(mart4)
check_collinearity(mart45)
check_collinearity(mart5)

#####
#Birds
#####

#singles
mbir1 <- glmmTMB(N ~ -1 + Crop.Type + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir2 <- glmmTMB(N ~ -1 + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir3 <- glmmTMB(N ~ -1 + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir4 <- glmmTMB(N ~ -1 + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir5 <- glmmTMB(N ~ -1 + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
warnings()

summary(mbir1)
summary(mbir2)
summary(mbir3)
summary(mbir4)
summary(mbir5)

#doubles
mbir12 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir13 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir14 <- glmmTMB(N ~ -1 + Crop.Type + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir15 <- glmmTMB(N ~ -1 + Crop.Type + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir12)
summary(mbir13)
summary(mbir14)
summary(mbir15)

mbir23 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir24 <- glmmTMB(N ~ -1 + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir25 <- glmmTMB(N ~ -1 + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir23)
summary(mbir24)
summary(mbir25)

mbir34 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir35 <- glmmTMB(N ~ -1 + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir34)
summary(mbir35)

mbir45 <- glmmTMB(N ~ -1 + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir45)

#triples
mbir123 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir124 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir125 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir123)
summary(mbir124)
summary(mbir125)

mbir134 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir135 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir134)
summary(mbir135)

mbir234 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir235 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir234)
summary(mbir235)

mbir345 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir345)

#quads

mbir1234 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)
mbir1235 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir1234)
summary(mbir1235)

mbir1345 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir1345)

mbir2345 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir2345)

#global

mbir12345 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataBir, family = nbinom2)

summary(mbir12345)

#Random effects check
#mbirre1 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#mbirre2 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#mbirre3 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)) + (1|Box.Name) + offset(log(Hours)), data = DataArt)

#AIC
AICctab(mbir1, mbir12, mbir123, mbir1234, mbir12345, mbir1235, mbir124, mbir125, mbir13,
        mbir134, mbir1345, mbir135, mbir14, mbir15, mbir2, mbir23, mbir234, mbir2345, 
        mbir235, mbir24, mbir25, mbir3, mbir34, mbir345, mbir35, mbir4, mbir45, mbir5, 
        weights=TRUE)

summary(mbir12) #31%
summary(mbir123) #13%
summary(mbir124) #12%
summary(mbir125) #10%
summary(mbir1) #10%
summary(mbir1234) #6%

#check for collinearity
check_collinearity(mbir1) #null
check_collinearity(mbir12) #low
check_collinearity(mbir123) #low
check_collinearity(mbir1234) #low
check_collinearity(mbir12345)
check_collinearity(mbir1235)
check_collinearity(mbir124) #low
check_collinearity(mbir125)
check_collinearity(mbir13)
check_collinearity(mbir134)
check_collinearity(mbir1345)
check_collinearity(mbir135)
check_collinearity(mbir14)
check_collinearity(mbir15)
check_collinearity(mbir2)
check_collinearity(mbir23)
check_collinearity(mbir234)
check_collinearity(mbir2345)
check_collinearity(mbir235)
check_collinearity(mbir24)
check_collinearity(mbir25)
check_collinearity(mbir3)
check_collinearity(mbir34)
check_collinearity(mbir345)
check_collinearity(mbir35)
check_collinearity(mbir4)
check_collinearity(mbir45)
check_collinearity(mbir5)

#####
#herps
#####

#singles
mher1 <- glmmTMB(N ~ -1 + Crop.Type + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher2 <- glmmTMB(N ~ -1 + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher3 <- glmmTMB(N ~ -1 + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher4 <- glmmTMB(N ~ -1 + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher5 <- glmmTMB(N ~ -1 + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
warnings()

summary(mher1)
summary(mher2)
summary(mher3)
summary(mher4)
summary(mher5)

#doubles
mher12 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher13 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher14 <- glmmTMB(N ~ -1 + Crop.Type + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher15 <- glmmTMB(N ~ -1 + Crop.Type + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher12)
summary(mher13)
summary(mher14)
summary(mher15)

mher23 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher24 <- glmmTMB(N ~ -1 + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher25 <- glmmTMB(N ~ -1 + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher23)
summary(mher24)
summary(mher25)

mher34 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher35 <- glmmTMB(N ~ -1 + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher34)
summary(mher35)

mher45 <- glmmTMB(N ~ -1 + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher45)

#triples
mher123 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher124 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher125 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher123)
summary(mher124)
summary(mher125)

mher134 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher135 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher134)
summary(mher135)

mher234 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher235 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher234)
summary(mher235)

mher345 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher345)

#quads

mher1234 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)
mher1235 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher1234)
summary(mher1235)

mher1345 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher1345)

mher2345 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher2345)

#global

mher12345 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataHer, family = nbinom2)

summary(mher12345)

#Random effects check
#mherre1 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#mherre2 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#mherre3 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)) + (1|Box.Name) + offset(log(Hours)), data = DataArt)

#AIC
AICctab(mher1, mher12, mher123, mher1234, mher12345, mher1235, mher124, mher125, mher13,
        mher134, mher1345, mher135, mher14, mher15, mher2, mher23, mher234, mher2345, 
        mher235, mher24, mher25, mher3, mher34, mher345, mher35, mher4, mher45, mher5, 
        weights=TRUE)

summary(mher15) #34%
summary(mher1) #16%
summary(mher125) #10%
summary(mher135) #10%
summary(mher12) #6%
summary(mher14) #5%
summary(mher13) #5%

#check for collinearity
check_collinearity(mher1) #null
check_collinearity(mher12) #low
check_collinearity(mher123) #low
check_collinearity(mher1234) #low
check_collinearity(mher12345)
check_collinearity(mher1235)
check_collinearity(mher124) #low
check_collinearity(mher125)
check_collinearity(mher13)
check_collinearity(mher134)
check_collinearity(mher1345)
check_collinearity(mher135)
check_collinearity(mher14)
check_collinearity(mher15)
check_collinearity(mher2)
check_collinearity(mher23)
check_collinearity(mher234)
check_collinearity(mher2345)
check_collinearity(mher235)
check_collinearity(mher24)
check_collinearity(mher25)
check_collinearity(mher3)
check_collinearity(mher34)
check_collinearity(mher345)
check_collinearity(mher35)
check_collinearity(mher4)
check_collinearity(mher45)
check_collinearity(mher5)

#####
#MAMMALS
#####

#singles
mmam1 <- glmmTMB(N ~ -1 + Crop.Type + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam2 <- glmmTMB(N ~ -1 + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam3 <- glmmTMB(N ~ -1 + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam4 <- glmmTMB(N ~ -1 + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam5 <- glmmTMB(N ~ -1 + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
warnings()

summary(mmam1)
summary(mmam2)
summary(mmam3)
summary(mmam4)
summary(mmam5)

#doubles
mmam12 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam13 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam14 <- glmmTMB(N ~ -1 + Crop.Type + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam15 <- glmmTMB(N ~ -1 + Crop.Type + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam12)
summary(mmam13)
summary(mmam14)
summary(mmam15)

mmam23 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam24 <- glmmTMB(N ~ -1 + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam25 <- glmmTMB(N ~ -1 + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam23)
summary(mmam24)
summary(mmam25)

mmam34 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam35 <- glmmTMB(N ~ -1 + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam34)
summary(mmam35)

mmam45 <- glmmTMB(N ~ -1 + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam45)

#triples
mmam123 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam124 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam125 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam123)
summary(mmam124)
summary(mmam125)

mmam134 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam135 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam134)
summary(mmam135)

mmam234 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam235 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam234)
summary(mmam235)

mmam345 <- glmmTMB(N ~ -1 + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam345)

#quads

mmam1234 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)
mmam1235 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam1234)
summary(mmam1235)

mmam1345 <- glmmTMB(N ~ -1 + Crop.Type + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam1345)

mmam2345 <- glmmTMB(N ~ -1 + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam2345)

#global

mmam12345 <- glmmTMB(N ~ -1 + Crop.Type + foreststdrd + popstdrd + edstdrd + gddistdrd + (1|Box.Name) + offset(log(Hours)), data = DataMam, family = nbinom2)

summary(mmam12345)

#Random effects check
#mmamre1 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#mmamre2 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)), data = DataArt)
#mmamre3 <- glmer.nb(N ~ -1 + Crop.Type + patchstdrd + foreststdrd + popstdrd + edstdrd + (1|Box.Name) + offset(log(Hours)) + (1|Box.Name) + offset(log(Hours)), data = DataArt)

#AIC
AICctab(mmam1, mmam12, mmam123, mmam1234, mmam12345, mmam1235, mmam124, mmam125, mmam13,
        mmam134, mmam1345, mmam135, mmam14, mmam15, mmam2, mmam23, mmam234, mmam2345, 
        mmam235, mmam24, mmam25, mmam3, mmam34, mmam345, mmam35, mmam4, mmam45, mmam5, 
        weights=TRUE)

summary(mmam1) #14%
summary(mmam1234) #13%
summary(mmam12) #13%
summary(mmam123) #10%
summary(mmam13) #9%
summary(mmam124) #7%
summary(mmam134) #6%
summary(mmam14) #6%
summary(mmam12345) #6%

#check for collinearity
check_collinearity(mmam1) #null
check_collinearity(mmam12) #low
check_collinearity(mmam123) #low
check_collinearity(mmam1234) #low
check_collinearity(mmam12345)
check_collinearity(mmam1235)
check_collinearity(mmam124) #low
check_collinearity(mmam125)
check_collinearity(mmam13)
check_collinearity(mmam134)
check_collinearity(mmam1345)
check_collinearity(mmam135)
check_collinearity(mmam14)
check_collinearity(mmam15)
check_collinearity(mmam2)
check_collinearity(mmam23)
check_collinearity(mmam234)
check_collinearity(mmam2345)
check_collinearity(mmam235)
check_collinearity(mmam24)
check_collinearity(mmam25)
check_collinearity(mmam3)
check_collinearity(mmam34)
check_collinearity(mmam345)
check_collinearity(mmam35)
check_collinearity(mmam4)
check_collinearity(mmam45)
check_collinearity(mmam5)

#####
#8. model averaging
#####

#install.packages('MuMIn')
library(MuMIn)

# Estimate effects by model averaging
#arthropods

#summary(mart15) #48%
#summary(mart125) #17%
#summary(mart135) #14%
#summary(mart1345) #5%
#summary(mart1235) #5%

artmodav = model.avg(list(mart15, mart125, mart135, mart1345, mart1235))
#a lot of people like averaging models with weights > 0.05.
summary(artmodav)
artci <- confint(artmodav, level = 0.95) #95% confidence intervals 

sink("art.csv")
summary(artmodav)
sink() #save as csv

sink("artci.csv")
artci
sink() #save as csv

#Birds

#summary(mbir12) #31%
#summary(mbir123) #13%
#summary(mbir124) #12%
#summary(mbir125) #10%
#summary(mbir1) #10%
#summary(mbir1234) #6%

birmodav = model.avg(list(mbir12, mbir123, mbir124, mbir125, mbir1, mbir1234))
summary(birmodav)
birci <- confint(birmodav, level = 0.95) #95% confidence intervals 

sink("bir.csv")
summary(birmodav)
sink()

sink("birci.csv")
birci
sink()

#herps
#summary(mher15) #34%
#summary(mher1) #16%
#summary(mher125) #10%
#summary(mher135) #10%
#summary(mher12) #6%
#summary(mher14) #5%
#summary(mher13) #5%

hermodav = model.avg(list(mher15, mher1, mher125, mher135, mher12, mher14, mher13))
summary(hermodav)
herci <- confint(hermodav, level = 0.95) #95% confidence intervals 

sink("her.csv")
summary(hermodav)
sink()

sink("herci.csv")
herci
sink()

#mammals
#summary(mmam1) #14%
#summary(mmam1234) #13%
#summary(mmam12) #13%
#summary(mmam123) #10%
#summary(mmam13) #9%
#summary(mmam124) #7%
#summary(mmam134) #6%
#summary(mmam14) #6%
#summary(mmam12345) #6%

mammodav = model.avg(list(mmam1, mmam1234, mmam12, mmam123, mmam13, mmam124, mmam134, 
                          mmam14, mmam12345))
summary(mammodav)
mamci <- confint(mammodav, level = 0.95) #95% confidence intervals 

sink("mam.csv")
summary(mammodav)
sink()

sink("mamci.csv")
mamci
sink()

#####
#9. Visualize data
#####

library(ggplot2)
library(ggpubr)

art <- read.csv("art.csv")
bir <- read.csv("bir.csv")
her <- read.csv("her.csv")
mam <- read.csv("mam.csv")

#arthropods
art <- art[1:6, 1:9]

tiff("arthropods.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(data = art, aes(y = estimate, x = variable, ymin = lower, ymax = upper, 
                       color = variable, label = ifelse(pvalue < 0.001, "***", #denoating significance
                                                        ifelse(pvalue < 0.01, "**", 
                                                               ifelse(pvalue < 0.05, "*", 
                                                                      ifelse(pvalue < 0.1, "m", "")))))) + 
  ggtitle("Arthropods") +
  geom_point() + 
  geom_errorbar() +
  geom_text(aes(hjust = 0.5), vjust = -0.5) +
  theme(legend.position="none") +
  coord_flip()
dev.off()

#birds
bir <- bir[1:11, 1:9]

tiff("birds.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(data = bir, aes(y = estimate, x = variable, ymin = lower, ymax = upper, 
                       color = variable, label = ifelse(pvalue < 0.001, "***", 
                                                        ifelse(pvalue < 0.01, "**", 
                                                               ifelse(pvalue < 0.05, "*", 
                                                                      ifelse(pvalue < 0.1, "m", "")))))) + 
  ggtitle("Birds") +
  geom_point() + 
  geom_errorbar() +
  geom_text(aes(hjust = 0.5), vjust = -0.5) +
  theme(legend.position="none") +
  coord_flip()
dev.off()

#herps
her <- her[1:11, 1:9]

tiff("herptiles.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(data = her, aes(y = estimate, x = variable, ymin = lower, ymax = upper, 
                       color = variable, label = ifelse(pvalue < 0.001, "***", 
                                                        ifelse(pvalue < 0.01, "**", 
                                                               ifelse(pvalue < 0.05, "*", 
                                                                      ifelse(pvalue < 0.1, "m", "")))))) +  ggtitle("Herptiles") +
  geom_point() + 
  geom_errorbar() +
  geom_text(aes(hjust = 0.5), vjust = -0.5) +
  theme(legend.position="none") +
  coord_flip()
dev.off()

#mammals
#herps
mam <- mam[1:6, 1:9]

tiff("mammals.tiff", width = 7.3, height = 4.3, units = 'in', res = 600, compression = 'lzw') 
ggplot(data = mam, aes(y = estimate, x = variable, ymin = lower, ymax = upper, 
                       color = variable, label = ifelse(pvalue < 0.001, "***", 
                                                        ifelse(pvalue < 0.01, "**", 
                                                               ifelse(pvalue < 0.05, "*", 
                                                                      ifelse(pvalue < 0.1, "m", "")))))) +  ggtitle("Mammals") +
  geom_point() + 
  geom_errorbar() +
  geom_text(aes(hjust = 0.5), vjust = -0.5) +
  theme(legend.position="none") +
  coord_flip()
dev.off()


#####
#END
#####
