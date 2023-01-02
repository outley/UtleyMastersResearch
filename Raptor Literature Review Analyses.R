#####Raptor Literature
#Olivia Utley
#Goals 1. produce summary data on study systems (locations and species included)
# 2. summarize variables recorded in 105+ studies of raptor foraging behavior in agriculture
# 3. conduct chi squared analysis on frequency of vars studied and relationships between vars
# 4. meta-analyze raptor selection of cultivated land cover
#started: 1/23/19

#install.packages("dplyr")
#install.packages("ggplot2")
library("dplyr")
library("ggplot2")
library("tidyr")
library("writexl")

setwd("~/Documents/MSU/Research/Projects/Raptor Literature Review/Data")
Data <- read.csv("~/Documents/MSU/Research/Projects/Raptor Literature Review/Data/Review Paper Data.csv")
as_tibble(Data)

#####
#1. Summary data of study systems
#####

#look at variables

unique(Data$Country)

unique(Data$Continent)

unique(Data$Scientific.Name)

#Graph of Continents
#using continents because birds don't typically respect national/international boarder laws
Cont <- Data %>% group_by(Continent) %>% summarise(n()) #get number of studies in each cont

#plot
ContPlot <- ggplot(Cont, aes(Cont$Continent, Cont$`n()`, fill = Continent))
ContPlot + geom_col() + xlab("Continent") + ylab("Frequency") + #axis labels
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1)) #angle data labels

ggsave("Continents.png")

write_xlsx(Continents,"Continents.xlsx")

#Graph of raptor families
RFn <- Data %>% group_by(Raptor.family) %>% summarise(n()) #number of studies on each family

#plot
RaptorPlot <- ggplot(RFn, aes(RFn$Raptor.family, RFn$`n()`, 
                                          fill = Raptor.family))
RaptorPlot + geom_col() + xlab("Raptor Family") + ylab("Frequency") + #axis labels
  theme(legend.position = 'none', panel.background = element_blank(), #remove grid
        axis.text.x = element_text(angle = 45, hjust = 1), #angle data labels
        axis.line.x = element_line(), axis.line.y = element_line()) #add boarder to graph

ggsave("Raptor Family Colors.png")

RSn <- Data %>% group_by(Scientific.Name) %>% summarise(n()) #number of studies on each SPECIES for supplementary material

write_xlsx(Raptor.species.n,"RaptorSpecies.xlsx")

#####Map of countries

#install libraries
#install.packages(c("RgoogleMaps", "ggmap", "mapproj", "sf", "OpenStreetMap", "devtools", "DT"))
#install.packages("RColorBrewer")
library("RgoogleMaps")
library("ggmap")
library("mapproj")
library("sf")
library("OpenStreetMap")
library("devtools")
library("DT")
library("rworldmap")
library("RColorBrewer")

#install package from github
devtools::install_github("dkahle/ggmap", ref = "tidyup")

#sort country data and get count
CountryN <- Data %>% group_by(Country.Code) %>% summarise(n())

#rename columns for clarity
colnames(CountryN) <- c("Country.Code", "Frequency")

#fix Italy and Switzerland counts (because of study with multiple locations ITA/CHE)
CountryN$Frequency[4]=3
CountryN$Frequency[17]=5

#remove combined location study point
CountryN <- CountryN[-c(18), ]

#Map
FrequencyMap <- joinCountryData2Map(CountryN, 
                                    joinCode = "ISO3",
                                    nameJoinColumn = "Country.Code")
#pick pretty colors
display.brewer.all()
colourPalette <- brewer.pal(9,'Spectral')
colourPalette <- rev(colourPalette)

#define map parameters
mapParams <- mapCountryData(FrequencyMap, 
                            nameColumnToPlot="Frequency",
                            oceanCol = "azure2",
                            catMethod = "categorical",
                            missingCountryCol = gray(.8),
                            colourPalette=colourPalette,
                            addLegend = F,
                            mapTitle = "",
                            border = NA)

# add legend and display map

do.call(addMapLegendBoxes, c(mapParams,
                             x = 'bottom',
                             title = "Number of Studies",
                             horiz = T,
                             bg = "transparent",
                             bty = "n",
                             pt.cex = 2))

#####
#2. Frequency of variables used in studies
#####

#Explanatory variable frequency

#create labels for names of variables
ExHead <- c("Land cover", "Season", "Time of day", "Year", "Patch size", "Vegetation structure", 
            "Landscape heterogeneity", "Prey abundance", "Human activity", "Human structures",
            "Perch characteristics", "Distance", "Breeding status", 
            "Sex", "Age", "Size")

ExVarN <- data.frame(colSums(Data[,13:28]), ind = ExHead) #sum frequency of variables and label in new df
colnames(ExVarN) <- c("Frequency", "Explanatory.variable") #rename headers
ExVarN$Proportion <- (ExVarN$Frequency/105)*100 #calculate proportion of total studies

#Response Variable Frequency

#create labels for names of variables
ResHead <- c("Presence/Abundance", "Foraging activity", "Behaviors observed", "Savage Selectivity Index", "Frequency of attack", 
             "Frequency of success", "Hunting strategy", "Home range size", "Home range composition",
             "Directional movement", "Provisioning rate", "Diet", "Prey size")

ResVarN <- data.frame(colSums(Data[,30:42]), ind = ResHead) #sum frequency of variables and label in new df
colnames(ResVarN) <- c("Frequency", "Response.variable") #rename headers
ResVarN$Proportion <- (ResVarN$Frequency/105)*100 #calculate proportion of total studies

#Filter to variables greater than ~20% frequency
ExVarN <- filter(ExVarN, Proportion >= 20)
ResVarN <- filter(ResVarN, Proportion >= 19)

#Remove non manipulable variables (landscape characteristics not within control of farmers)
ExVarN <- ExVarN[-c(2,6,7), ]

#Removing Foraging Use var
ResVarN <- ResVarN[-2, ]

#Graph of Explanatory Variables
ggplot() + geom_col(data = ExVarN, aes(y = Frequency, x = Explanatory.variable, 
                                               fill = Explanatory.variable), show.legend = F) + 
  xlab("Explanatory Variable") + ylab("Frequency") + 
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_line(), axis.line.y = element_line())

ggsave("Explanatory Variables Colors.pdf")

#Graph of Response Variables
ggplot() + geom_col(data = ResVarN, aes(x = Response.variable, y = Frequency, 
                                                fill = Response.variable), show.legend = F)  + 
  xlab("Response Variable") + ylab("Frequency") + 
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_line(), axis.line.y = element_line())

ggsave("Response Variables Colors.png")

#####
# 3. Chi-squared analyses
#####

#looking at response variables: raptor presence/abundance, attack rate, success rate
#and the frequency of their relationships with predictors: land cover, 
#distance to a central location (e.g. nest), prey abundance, and vegetation structure
#i.e. how many positive, neutral, and negative trends were found in 105 studies between
#raptor presence/abundance and each of the predictors?

library("corrplot")
library("colorspace")
library("vcd")
library("grid")

#Upload data
PAData <- read.csv("~/Documents/MSU/Research/Projects/Raptor Literature Review/Data/X2 Presence-Abundance.csv")

#get frequency data (counts of effects of each predictor)
PALandcover <- PAData %>% group_by(Landcover.Type) %>% summarise(n())
PAPreyAbundance <- PAData %>% group_by(Prey.Abundance) %>% summarise(n())
PAVegStructure <- PAData %>% group_by(Vegetation.Structure) %>% summarise(n())
PADistance <- PAData %>% group_by(Distance) %>% summarise(n())

#label columns
colnames(PALandcover) <- c("Effect", "LandCoverFreq")
colnames(PAVegStructure) <- c("Effect", "VegetationStructureFreq")
colnames(PAPreyAbundance) <- c("Effect", "PreyAbundanceFreq")
colnames(PADistance) <- c("Effect", "DistanceFreq")

#Remove counts of NA
PALandcover <- PALandcover[-4,]
PAPreyAbundance <- PAPreyAbundance[-4,]
PAVegStructure <- PAVegStructure[-c(3,4),] #also removes "quadratic" relationship category
PADistance <- PADistance[-3,]

#Add 0's for veg and distance (missing values)
PAVegStructure <- PAVegStructure %>% add_row(Effect = "Neutral", VegetationStructureFreq = 0)
PAVegStructure <- PAVegStructure %>% arrange(Effect)

PADistance <- PADistance %>% add_row(Effect = "Positive", DistanceFreq = 0)
PADistance <- PADistance %>% arrange(Effect)

#Merge variables into one frame
PAchi2table <- cbind(PALandcover, PAPreyAbundance, PAVegStructure, PADistance)

#Rename row
rownames(PAchi2table) <- c("Negative", "Neutral", "Positive")

#Remove repeated columns
PAchi2table <- PAchi2table[-c(1, 3, 5, 7)]

#Rename headers AGAIN for figures later on
colnames(PAchi2table) <- c("CLC", "PA", 
                           "VS", "D")

#transpose for figures
PAchi2table = t(PAchi2table)

#fisher.test(dat)
PAchi.t <- chisq.test(PAchi2table, simulate.p.value = TRUE) #chi-square test
#we simulate the p-value bc the sample size is small

PAchi.t #test results

PAchi.t$observed
PAchi.t$expected #look at the expected counts generated by the test

#graphs and figures

#format for histograms
PAObs <- as.data.frame(PAchi.t$observed)
PAObs$Vars <- c("Cultivated land cover", "Prey abundance", "Vegetation structure", "Distance")
PAObs <- gather(PAObs, Relationship, Frequency, 1:3) #long format
PAObs$Value <- "Observed" #label all data as observed value

PAExp <- as.data.frame(PAchi.t$expected)
PAExp$Vars <- c("Cultivated land cover", "Prey abundance", "Vegetation structure", "Distance")
PAExp <- gather(PAExp, Relationship, Frequency, 1:3) #long format
PAExp$Value <- "Expected" #label all data as expected value

PA <- rbind(PAObs, PAExp) #merge observed and expected
#PA$VarRel <- paste(PA$Vars, PA$Relationship, sep=" ") #combine var and relationship for 

#histogram
pdf("PAHist.pdf") 
plot <- ggplot(PA, aes(interaction(Relationship, Vars), Frequency, fill=Value)) 
#two levels to x axis: predictor variable and relationship
plot <- plot + geom_bar(stat = "identity", position = 'dodge') +
  ggtitle("Expected and Observed Predictor Relationships with Presence/Abundance") +
  coord_cartesian(ylim = c(1.7, 35)) +
  annotate("text", x = 1:12, y = -1,
           label = rep(c("-", "o", "+"), 4)) + #label for each relationship for each var
  annotate("text", x = c(2,5,8,11), y = -4, #position var labels under each "set" of bars for relationships
           label = c("Cultivated land cover", "Distance", "Prey abundance", "Vegetation structure")) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 8, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) #label horizontal

# remove clipping of x axis labels
plot2 <- ggplot_gtable(ggplot_build(plot))
plot2$layout$clip[plot2$layout$name == "panel"] <- "off"
grid.draw(plot2)

dev.off() #finish saving plot

#Mosaic plot for the observed counts
pdf("PAObMosaic.pdf") 
mosaic(PAchi.t$observed, direction = "h",
       main = "Observed Frequency of Relationships Between Predictors and Presence/Abundance",
       labeling = labeling_values,
       labeling_args = list(set_varnames = c(A = "Relationship", B = "Predictor")),
       rot_labels = T, #rotate labels
       pos_varnames = "center",
       shade=T, colorize = T,
       gp = gpar(fill = matrix(c("#F8766D", "#CD9600", "#7CAE00", "#00BFC4", #color matrix
                               "#F8766D", "#CD9600", "#7CAE00", "#00BFC4", 
                               "#F8766D", "#CD9600", "#7CAE00", "#00BFC4"), 4, 3)))
dev.off()

#expected counts mosaic
pdf("PAExMosaic.pdf") 
mosaic(PAchi.t$expected, direction = "h",
       main = "Expected Frequency of Relationships Between Predictors and Presence/Abundance",
       labeling = labeling_values,
       labeling_args = list(set_varnames = c(A = "Relationship", B = "Predictor")),
       rot_labels = T,
       pos_varnames = "center",
       shade=T, colorize = T,
       gp = gpar(fill = matrix(c("#F8766D", "#CD9600", "#7CAE00", "#00BFC4",
                               "#F8766D", "#CD9600", "#7CAE00", "#00BFC4", 
                               "#F8766D", "#CD9600", "#7CAE00", "#00BFC4"), 4, 3)))
dev.off()

#Calculate residuals to examine contribution to chi squared test
PAresi <- round(PAchi.t$residuals, 3) #round to 3 sig figs

#Visualize
pdf("PACorr.pdf") 
corrplot(PAchi.t$residuals, method = "color",
         col = brewer.pal(n = 9, name = 'PRGn'), addCoef.col = 'black', #select colorblind friendly scheme
         cl.pos = 'n', tl.col = 'black', is.cor = FALSE)
dev.off()

#Percent contribution
PAcontrib <- 100*(PAchi.t$residuals^2/PAchi.t$statistic)
round(PAcontrib, 3)

PAcontribTotals <- colSums(PAcontrib[,1:3])

#Visualize percent contribution
corrplot(PAcontrib, col = brewer.pal(n = 9, name = 'YlOrRd'), tl.col = 'black', is.cor = FALSE)

#Attack Rate chi-squared

#load data
ARData <- read.csv("~/Documents/MSU/Research/Projects/Raptor Literature Review/Data/X2 Attack.csv")

#get frequency data (counts of effects of each predictor)
ARLandcover <- ARData %>% group_by(Land.cover) %>% summarise(n())
ARPreyAbundance <- ARData %>% group_by(Prey.abundance) %>% summarise(n())
ARVegStructure <- ARData %>% group_by(Vegetation.structure) %>% summarise(n())
ARDistance <- ARData %>% group_by(Distance) %>% summarise(n())

#label columns
colnames(ARLandcover) <- c("Effect", "LandCoverFreq")
colnames(ARVegStructure) <- c("Effect", "VegetationStructureFreq")
colnames(ARPreyAbundance) <- c("Effect", "PreyAbundanceFreq")
colnames(ARDistance) <- c("Effect", "DistanceFreq")

#Remove NA rows
ARLandcover <- ARLandcover[-4,]
ARVegStructure <- ARVegStructure[-4,]
ARDistance <- ARDistance[-3,]

#Add 0's distance (missing values)
ARDistance <- ARDistance %>% add_row(Effect = "Positive", DistanceFreq = 0)
ARDistance <- ARDistance %>% arrange(Effect)

#Merge variables into one frame
ARchi2table <- cbind(ARLandcover, ARVegStructure, ARDistance)

#Rename Row
rownames(ARchi2table) <- c("Negative", "Neutral", "Positive")

#Remove extra effects columns
ARchi2table <- ARchi2table[-c(1, 3, 5)]

#Rename headers AGAIN
colnames(ARchi2table) <- c("Land cover", "Vegetation structure", "Distance")

#rename headers
#Rename headers AGAIN
colnames(ARchi2table) <- c("CLC", "VS", "D")

#transpose for figures
ARchi2table = t(ARchi2table)

#chi-squared test
ARchi.t <- chisq.test(ARchi2table, simulate.p.value = TRUE) #chi-square test, we simulate the p-value bc the sample size is small

ARchi.t #test results

ARchi.t$expected #look at the expected counts generated by the test

#format data for histograms 
ARObs <- as.data.frame(ARchi.t$observed)
ARObs$Vars <- c("Cultivated land cover", "Vegetation structure", "Distance")
ARObs <- gather(ARObs, Relationship, Frequency, 1:3)
ARObs$Value <- "Observed"

ARExp <- as.data.frame(ARchi.t$expected)
ARExp$Vars <- c("Cultivated land cover", "Vegetation structure", "Distance")
ARExp <- gather(ARExp, Relationship, Frequency, 1:3)
ARExp$Value <- "Expected"

AR <- rbind(ARObs, ARExp)

#histogram of observed and expected relationships
pdf("ARHist.pdf") 
plot <- ggplot(AR, aes(interaction(Relationship, Vars), Frequency, fill=Value))
plot <- plot + geom_bar(stat = "identity", position = 'dodge') +
  ggtitle("Expected vs. Observed Predictor Relationships with Attack Rate") +
  coord_cartesian(ylim = c(0.47, 10)) +
  annotate("text", x = 1:9, y = -0.5,
           label = rep(c("-", "o", "+"), 3)) +
  annotate("text", x = c(2,5,8), y = -1, 
           label = c("Cultivated land cover", "Distance", "Vegetation structure")) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 8, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))

# remove clipping of x axis labels
plot2 <- ggplot_gtable(ggplot_build(plot))
plot2$layout$clip[plot2$layout$name == "panel"] <- "off"
grid.draw(plot2)
dev.off()

#Mosaic plot code for the observed and expected counts generated above
pdf("ARObMosaic.pdf") 
mosaic(ARchi.t$observed, direction = "h",
       main = "Observed Frequency of Relationships Between Predictors and Attack Rate",
       labeling = labeling_values,
       labeling_args = list(set_varnames = c(A="Relationship", B="Predictor")),
       rot_labels = T,
       pos_varnames = "center",
       shade=T, colorize = T,
       gp = gpar(fill=matrix(c("#F8766D", "#7CAE00", "#00BFC4", 
                               "#F8766D", "#7CAE00", "#00BFC4", 
                               "#F8766D", "#7CAE00", "#00BFC4"), 3, 3)))
dev.off()

pdf("ARExMosaic.pdf") 
mosaic(ARchi.t$expected, direction = "h",
       main = "Expected Frequency of Relationships Between Predictors and Attack Rate",
       labeling = labeling_values,
       labeling_args = list(set_varnames = c(A="Relationship", B="Predictor")),
       rot_labels = T,
       pos_varnames = "center",
       shade=T, colorize = T,
       gp = gpar(fill=matrix(c("#F8766D", "#7CAE00", "#00BFC4", 
                               "#F8766D", "#7CAE00", "#00BFC4", 
                               "#F8766D", "#7CAE00", "#00BFC4"), 3, 3)))
dev.off()

#Calculate residuals to examine individual relationships
ARresi <- round(ARchi.t$residuals, 3)

#Visualize residuals
pdf("ARCorr.pdf") 
corrplot(ARchi.t$residuals, method = "color",
         col = brewer.pal(n = 9, name = 'PRGn'), addCoef.col = 'black', 
         cl.pos = 'n', tl.col = 'black', is.cor = FALSE)
dev.off()

#Percent contribution
ARcontrib <- 100*(ARchi.t$residuals^2/ARchi.t$statistic)
round(ARcontrib, 3)

ARcontribTotals <- colSums(ARcontrib[,1:3])

#Visualize percent contribution
corrplot(ARcontrib, col = brewer.pal(n = 9, name = 'YlOrRd'), tl.col = 'black', is.cor = FALSE)

#Success Rate

SRData <- read.csv("~/Documents/MSU/Research/Projects/Raptor Literature Review/Data/X2 Success.csv")

#get frequency data (counts of effects of each predictor)
SRLandcover <- SRData %>% group_by(Land.cover) %>% summarise(n())
SRPreyAbundance <- SRData %>% group_by(Prey.abundance) %>% summarise(n())
SRVegStructure <- SRData %>% group_by(Vegetation.structure) %>% summarise(n())
SRDistance <- SRData %>% group_by(Distance) %>% summarise(n())

#rename columns
colnames(SRLandcover) <- c("Effect", "LandCoverFreq")
colnames(SRVegStructure) <- c("Effect", "VegetationStructureFreq")
colnames(SRPreyAbundance) <- c("Effect", "PreyAbundanceFreq")
colnames(SRDistance) <- c("Effect", "DistanceFreq")

#Remove NA rows
SRLandcover <- SRLandcover[-4,]
SRVegStructure <- SRVegStructure[-4,]
SRDistance <- SRDistance[-4,]

#Merge variables into one frame
SRchi2table <- cbind(SRLandcover, SRVegStructure, SRDistance)

#Rename Row
rownames(SRchi2table) <- c("Negative", "Neutral", "Positive")

#Remove extra effects columns
SRchi2table <- SRchi2table[-c(1, 3, 5)]

#Rename headers AGAIN
colnames(SRchi2table) <- c("Land Cover", "Vegetation Structure", "Distance")

#chi-squared test
SRchi.t <- chisq.test(SRchi2table, simulate.p.value = TRUE) #chi-square test, we simulate the p-value bc the sample size is small

SRchi.t #test results

SRchi.t$expected #look at the expected counts generated by the test

#insignificant results

#####
#4. Meta-analysis
#####

library('metafor')

#load mdat
mdat <- read.csv("Land Cover Use Data.csv")
as_tibble(mdat)

#Creating effect size and variance

#Add cultivated effect size col
mdat$CES <- log(mdat$CultivatedU/mdat$CultivatedA)

#Add variance col
mdat$weight <- sqrt(mdat$N)
mdat$inv.sqrt.N <- 1/sqrt(mdat$N)

#remove NAs
mdat <- mdat %>% drop_na(inv.sqrt.N)

#Check
as_tibble(mdat)

#MODEL

#Estimate, trying to see if log ratio (proportion of use)/(proportion of availability) 
#is significantly different from log ratio where use = availability

?rma.mv

#model includes raptor family as categorical predictor, random effect of "study"
m = rma.mv(yi = CES, V = inv.sqrt.N, data = mdat,
           mods = ~ -1 + Raptor.Family,
           random = ~ 1 | Citation )

#look at model estimates and confidence intervals
m

#calculate estimates back into proportions of use
exp(-1.144)
#Accip uses 31.5% of agriculture
exp(-0.0331)
#Falcons use 96.7%
exp(-.2324)
#owls use 79.2%

#Adding continent as predictor variable
m2 = rma.mv(yi = CES, V = inv.sqrt.N, data = mdat,
            mods = ~ -1 + Raptor.Family + Continent ,
            random = ~ 1 | Citation )

m2







