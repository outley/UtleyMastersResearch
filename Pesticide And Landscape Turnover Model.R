######
#Pesticide and Landscape Turnover Model
#1/27/22
#OJU
######

#the goal of this project is to produce a dynamic occupancy model of kestrel nestboxes
#and understand how nest box turnover is affected by pesticide use, proportion of
#open habitat, and edge density

#nest boxes were considered occupied when they had 1 or more eggs OR 1 or more nestlings

#install.packages('tidyverse')
library('tidyverse')

setwd("~/Documents/MSU/Research/Projects/AMKE Pesticide Project/Data")
dat4 <- read.csv("landscape_pesticide_Kestrel_2021_Complete.csv") #data file with landscape metrics

str(dat4)

#wrangle data first, need 1's and 0's for "occupied" and "unoccupied"
dat4 <- dat4 %>% 
  mutate(Occ = if_else(Count_Eggs > 0, 1, 
                       if_else(Count_Nestlings_Live > 0, 1, 0)))

dat4$Occ

dat4 <- dat4[c("PartnerID", "NestID", "InstallYear", "InstallMonth", "Year", "ObservationEventID",
          "ObservationDataID", "ObservationTime", "Count_AdultKestrels", "Count_Eggs",
          "Count_Nestlings_Live", "Count_Nestlings_Dead", "Occ", "openED", "p.open", "tox")]


#Trying to remove duplicate observations within the same year for simplicity sake

# Remove duplicated rows based on 
# Year and Nest ID and Occ status
dat4 <- dat4 %>% distinct(NestID, Year, Occ, .keep_all = TRUE) 
#PROBLEM, KEEPS FIRST OBS SO DOESN'T CONSIDER IF AT A LTER OBS, BOX WAS OCCUPIED

#make ID for duplicates? Year and NestID ID
dat4$DupID <-paste0(dat4$NestID, dat4$Year) #z is your data.frame

#keep max occ value for each nest ID and year
dat5 <- dat4 %>% group_by(DupID)  %>% filter(Occ==max(Occ))

#expand data for occupancy modeling (wide format data)

#take out unnecessary columns

dat5 <- dat5[c("PartnerID", "NestID", "InstallYear", "InstallMonth", "Year", 
               "ObservationTime", "Occ", "openED", "p.open", "tox")]

#first expand Observation time into columns by year ID
#first, create ID by year

dat5 <-dat5 %>% mutate(Group =
                  case_when(Year == 2002 ~ "OT02", 
                            Year == 2003 ~ "OT03",
                            Year == 2004 ~ "OT04",
                            Year == 2005 ~ "OT05",
                            Year == 2006 ~ "OT06",
                            Year == 2007 ~ "OT07",
                            Year == 2008 ~ "OT08",
                            Year == 2009 ~ "OT09",
                            Year == 2010 ~ "OT10",
                            Year == 2011 ~ "OT11",
                            Year == 2012 ~ "OT12",
                            Year == 2013 ~ "OT13",
                            Year == 2014 ~ "OT14",
                            Year == 2015 ~ "OT15",
                            Year == 2016 ~ "OT16",
                            Year == 2017 ~ "OT17",
                            Year == 2018 ~ "OT18",
                            Year == 2019 ~ "OT19",
                            Year == 2020 ~ "OT20",
                            Year == 2021 ~ "OT21")
)


dat5 <- spread(dat5, Group, ObservationTime) #spread observation time out by year

#then tox

dat5 <-dat5 %>% mutate(Group =
                         case_when(Year == 2002 ~ "tox02", 
                                   Year == 2003 ~ "tox03",
                                   Year == 2004 ~ "tox04",
                                   Year == 2005 ~ "tox05",
                                   Year == 2006 ~ "tox06",
                                   Year == 2007 ~ "tox07",
                                   Year == 2008 ~ "tox08",
                                   Year == 2009 ~ "tox09",
                                   Year == 2010 ~ "tox10",
                                   Year == 2011 ~ "tox11",
                                   Year == 2012 ~ "tox12",
                                   Year == 2013 ~ "tox13",
                                   Year == 2014 ~ "tox14",
                                   Year == 2015 ~ "tox15",
                                   Year == 2016 ~ "tox16",
                                   Year == 2017 ~ "tox17",
                                   Year == 2018 ~ "tox18",
                                   Year == 2019 ~ "tox19",
                                   Year == 2020 ~ "tox20",
                                   Year == 2021 ~ "tox21")
)

dat5 <- spread(dat5, Group, tox) #spread toxicity data out by year

#then ED

dat5 <-dat5 %>% mutate(Group =
                         case_when(Year == 2002 ~ "ED02", 
                                   Year == 2003 ~ "ED03",
                                   Year == 2004 ~ "ED04",
                                   Year == 2005 ~ "ED05",
                                   Year == 2006 ~ "ED06",
                                   Year == 2007 ~ "ED07",
                                   Year == 2008 ~ "ED08",
                                   Year == 2009 ~ "ED09",
                                   Year == 2010 ~ "ED10",
                                   Year == 2011 ~ "ED11",
                                   Year == 2012 ~ "ED12",
                                   Year == 2013 ~ "ED13",
                                   Year == 2014 ~ "ED14",
                                   Year == 2015 ~ "ED15",
                                   Year == 2016 ~ "ED16",
                                   Year == 2017 ~ "ED17",
                                   Year == 2018 ~ "ED18",
                                   Year == 2019 ~ "ED19",
                                   Year == 2020 ~ "ED20",
                                   Year == 2021 ~ "ED21")
)

dat5 <- spread(dat5, Group, openED) #spread edge density out by year

#then p.open

dat5 <-dat5 %>% mutate(Group =
                         case_when(Year == 2002 ~ "p.open02", 
                                   Year == 2003 ~ "p.open03",
                                   Year == 2004 ~ "p.open04",
                                   Year == 2005 ~ "p.open05",
                                   Year == 2006 ~ "p.open06",
                                   Year == 2007 ~ "p.open07",
                                   Year == 2008 ~ "p.open08",
                                   Year == 2009 ~ "p.open09",
                                   Year == 2010 ~ "p.open10",
                                   Year == 2011 ~ "p.open11",
                                   Year == 2012 ~ "p.open12",
                                   Year == 2013 ~ "p.open13",
                                   Year == 2014 ~ "p.open14",
                                   Year == 2015 ~ "p.open15",
                                   Year == 2016 ~ "p.open16",
                                   Year == 2017 ~ "p.open17",
                                   Year == 2018 ~ "p.open18",
                                   Year == 2019 ~ "p.open19",
                                   Year == 2020 ~ "p.open20",
                                   Year == 2021 ~ "p.open21")
)

dat5 <- spread(dat5, Group, p.open) #spread proportion of open land cover out by year

#then expand occupancy by year ID

dat5 <- spread(dat5, Year, Occ)

#Okay try to combine multiple rows into one
library(data.table)

test <- setDT(dat5)[, lapply(.SD, na.omit), by = NestID]
dat6 <- distinct(test)

#####
#STATIC MODELS
#####

# Read in the data

#setwd("~/Documents/MSU/Research/Projects/AMKE Pesticide Project/Data/Class 10")
#data <- read.table("bluebug.txt", header = TRUE)

#Look at the data
head(dat6)

# Collect the data into suitable structures
#y <- as.matrix(data[,4:9])         # as.matrix essential for WinBUGS (occupancy columns)
#y[y>1] <- 1                        # Reduce counts to 0/1 (wish I'd done this for occupancy)
y <- as.matrix(dat6[,85:104])

#edge <- data$forest_edge          #Any vectors

OTcols <- grep("OT", colnames(dat6))
ObsT <- as.matrix(dat6[,5:24])

toxcols <- grep("tox", colnames(dat6))
toxic <- as.matrix(dat6[,25:44])

EDcols <- grep("ED", colnames(dat6))
edge <- as.matrix(dat6[,45:64])

POcols <- grep("open", colnames(dat6))
popen <- as.matrix(dat6[,65:84])

# Standardize covariates
mean.toxic <- mean(toxic, na.rm = TRUE)
sd.toxic <- sd(toxic[!is.na(toxic)])
TOXIC <- (toxic-mean.toxic)/sd.toxic     # Standardise date
#TOXIC[is.na(TOXIC)] <- 0               # Impute zeroes (means) 
#this makes false 0's in our study?

mean.edge <- mean(edge, na.rm = TRUE)
sd.edge <- sd(edge[!is.na(edge)])
EDGE <- (edge-mean.edge)/sd.edge      # Standardise hour
#HOURS[is.na(HOURS)] <- 0                # Impute zeroes (means)

mean.popen <- mean(popen, na.rm = TRUE)
sd.popen <- sd(popen[!is.na(popen)])
POPEN <- (popen-mean.popen)/sd.popen      # Standardise hour
#HOURS[is.na(HOURS)] <- 0                # Impute zeroes (means)

#####
#BASIC MODEL
#####

# Specify model in BUGS language
sink("kestrelpestmodel.txt")
cat("
    model {
    
    # Priors
    alpha.psi ~ dnorm(0, 0.01)
    p ~ dunif(0,1)
    
    # Likelihood
    # Ecological model for the partially observed true state
    for (i in 1:R) {
    z[i] ~ dbern(psi[i])                # True occurrence z at site i
    logit(psi[i]) <- alpha.psi
    
    # Observation model for the observations
    for (j in 1:T) {
    y[i,j] ~ dbern(z[i]*p)  # Detection-nondetection at i and j
    } #j
    mu[i] <- z[i] * p
    } #i
    
    }
    ",fill = TRUE)
sink()

# Bundle data
jags.data <- list(y = y, R = nrow(y), T = ncol(y)) #,  TOXIC = TOXIC)

# Initial values
zst <- apply(y, 1, max, na.rm = TRUE)	# Good starting values crucial
inits <- function(){list(z = zst)}

# Parameters monitored
params <- c("alpha.psi", "p")

# MCMC settings
ni <- 30000
nt <- 10
nb <- 20000
nc <- 3

#Load the correct library
install.packages("jagsUI")
library("jagsUI")

# Call JAGS from R
out <- jags(jags.data, inits, params, "kestrelpestmodel.txt", n.chains = nc, n.thin = nt, 
            n.iter = ni, n.burnin = nb)

dim(y)
dim(TOXIC)

# Summarize posteriors
print(out, digits=3)
plot(out)

#####
#KESTREL TURNOVER MODEL WITH COVARIATES 
#NOT WORKING BECAUSE INDEXING OF COVARIATES
#####

nind <- dim(venados)[1] 
K <- 43 
ntraps <- 13
M <- 150 
nz <- M - nind 
Yaug <- array(0, dim = c(M, ntraps, K))
Yaug[1:nind,,] <- venados
y <- apply(Yaug, c(1,3), sum) 
y[y > 1] <- 1

# Take the rowSum
y_vector <- rowSums(y)

# Use y_vector instead of y
data1 <- list(y = y_vector, nz = nz, nind = nind, K = K, sup = Buffer)

# Specify model in BUGS language
sink("kestrelpestmodel.txt")
cat("
    model {
    
    # Priors
    alpha.psi ~ dnorm(0, 0.01)
    beta.psi ~ dnorm(0, 0.01)
    alpha.p ~ dunif(0,1)
    
    # Likelihood
    # Ecological model for the partially observed true state
    for (i in 1:R) {
      for (j in 1:T) {
    z[i] ~ dbern(psi[i,j])                # True occurrence z at site i
    logit(psi[i,j]) <- alpha.psi + beta.psi * TOXIC[i,j]

    # Observation model for the observations
    
    y[i,j] ~ dbern(z[i]*alpha.p)  # Detection-nondetection at i and j
    } #j
    } #i
    
     # Derived quantities
    occ.fs <- sum(z[])                             # Number of occupied sites
    mean.p <- exp(alpha.p) / (1 + exp(alpha.p))    # Sort of average detection
    
    }
    ",fill = TRUE)
sink()

# Bundle data
jags.data <- list(y = y, R = nrow(y), T = ncol(y), TOXIC = TOXIC)

# Initial values
zst <- apply(y, 1, max, na.rm = TRUE)	# Good starting values crucial
inits <- function(){list(z = zst, alpha.psi=runif(1, -3, 3), alpha.p = runif(1, -3, 3))}

# Parameters monitored
params <- c("alpha.psi", "alpha.p", "beta.psi", "mean.p", "occ.fs")

# MCMC settings
ni <- 30000
nt <- 10
nb <- 20000
nc <- 3

#Load the correct library
install.packages("jagsUI")
library("jagsUI")

# Call JAGS from R
out <- jags(jags.data, inits, params, "kestrelpestmodel.txt", n.chains = nc, n.thin = nt, 
            n.iter = ni, n.burnin = nb)

dim(y)
dim(TOXIC)

# Summarize posteriors
print(out, digits=3)
plot(out)

### Examine the results
# Posterior distribution of the estimated number of occupied sites (woodpiles)
par(mfrow=c(1,1))
hist(out$sims.list$occ.fs, nclass = 30, col = "gray", main = "", 
     xlab = "Number of occupied sites (occ.fs)", xlim = c(9, 27))
## Add a line with the observed number of occuped sites
abline(v = 10, lwd = 2) 

#####
#DYNAMIC MODEL
#####

#example data
install.packages("AHMbook")
library("AHMbook")

data(spottedWoodpecker)  
str(dat <- spottedWoodpecker)

#Data wrangling for dynocc (keep survey data)
library(lubridate)
dat7 <- dat4[c("PartnerID", "NestID", "InstallYear", "InstallMonth", "Year", "ObservationEventID",
               "ObservationDataID", "ObservationTime", "Occ", "openED", "p.open", "tox")]

unique(dat4$Year)

dat7$ObservationTime <-  mdy_hm(dat7$ObservationTime) 

dat7$JD <- yday(dat7$ObservationTime)

write.csv(dat7, "Dynocc Data.csv")

# Check sample sizes in original data set  
nsites <- length(unique(dat7$NestID))  # 3586 sites  
nyears <- length(unique(dat7$Year))  # 20 years  
ndays <- length(unique(dat7$JD))  #340 days

#How many times sites surveyed in a year
length(unique(dat7[c("NestID","Year")]))
head(table((dat7[c("NestID","Year")])), 25)

# Randomly thin out the data set by subsampling 30%  
dat.full <- dat7  # Make a copy of full data set  
prop.data <- 0.3  # Proportion of data to be used  
ncase <- nrow(dat7)  # 36168 
set.seed(1)
sel.cases <- sort(sample(1:ncase, ncase * prop.data))  
dat8 <- dat7[sel.cases,]  # Smaller data set


# Have to renumber the sites (since lost some in subsampling) 
dat8$site <- as.numeric(as.factor(dat8[,"NestID"]))

# Sample sizes in new (subsampled) data set  
nsites <- length(unique(dat8$site))  # 2804 sites  
nyears <- length(unique(dat8$Year))  # 20 years  
ndays <- length(unique(dat8$JD))  # 290 days

# Compute observed occupancy 
zobs <- tapply(dat8$Occ, list(dat8$site, dat8$Year), max, na.rm = TRUE) 
zobs[zobs>1] <- 1 
psiobs <- apply(zobs, 2, mean, na.rm = TRUE)

#adding nsurveys because cheating?
dat8$nsurvey <- as.integer(1)

# Bundle data
str(bdata <- list(y = dat8[,'Occ'], nsurveys = dat8[,'nsurvey'], site = dat8[,'site'], 
                  year = dat8[,'Year']-2001, date = as.numeric(dat8$JD), nsites = nsites, 
                  nyears = nyears, nobs = nrow(dat8)))

zst <- zobs ; zst[is.na(zst)] <- 1
inits <- function(){list(z = zst)}

#Load the correct library
#install.packages("jagsUI")
library("jagsUI")

# Specify model in BUGS language for vertical data format
cat(file = "occmodel5.txt","
model {
  # Specify priors
  psi1 ~ dunif(0, 1)            # Initial occupancy
  for (t in 1:(nyears-1)){       # For survival and persistence
    phi[t] ~ dunif(0, 1)
    gamma[t] ~ dunif(0, 1)
  }
  for (t in 1:nyears){           # For detection parameters
    alpha.lp[t] <- logit(mean.p[t])
    mean.p[t] ~ dunif(0, 1)
    beta.lp.1[t] ~ dnorm(0, 0.001)
    beta.lp.2[t] ~ dnorm(0, 0.001)
  }
  # Ecological submodel: Define state conditional on parameters
  for (i in 1:nsites){
    z[i,1] ~ dbern(psi1)
    for (t in 2:nyears){
      z[i,t] ~ dbern(z[i,t-1]*phi[t-1] + (1-z[i,t-1])*gamma[t-1])
    }
  }
  # Observation model
  for (i in 1:nobs){
    logit(p[i]) <- alpha.lp[year[i]] + beta.lp.1[year[i]] * date[i] + beta.lp.2[year[i]] * pow(date[i],2)
    y[i] ~ dbern(z[site[i],year[i]]*p[i])
  }
  # Derived parameters
  psi[1] <- psi1                # Population occupancy
  n.occ[1] <- sum(z[1:nsites,1]) # Number of occupied sites in sample
  for (t in 2:nyears){
    psi[t] <- psi[t-1]*phi[t-1] + (1-psi[t-1])*gamma[t-1]
    n.occ[t] <- sum(z[1:nsites,t])
  }
}
")

# Parameters monitored
params <- c("psi", "phi", "gamma", "mean.p", "n.occ", "alpha.lp", "beta.lp.1", "beta.lp.2")

# MCMC settings
 na <- 1000  ;  ni <- 100000   ;   nb <- 50000   ;    nt <- 10   ;   nc <- 3  # 4.2 hrs
#na <- 1000
#ni <- 2500
#nb <- 500
#nt <- 2
#nc <- 3  # ~~~ for testing, 20 mins

# Call JAGS from R, check convergence and summarize posteriors
out5 <- jags(bdata, inits, params, "occmodel5.txt", n.adapt = na,
             n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

# par(mfrow = c(3,3))  # ~~~ no longer needed
traceplot(out5)
print(out5, dig = 3)

summary(out5)

out5



















#####
#ORIGINAL
#####


# Specify model in BUGS language
sink("bluebugmodel.txt")
cat("
    model {
    
    # Priors
    alpha.psi ~ dnorm(0, 0.01)
    beta.psi ~ dnorm(0, 0.01)
    alpha.p ~ dnorm(0, 0.01)
    beta1.p ~ dnorm(0, 0.01)
    beta2.p ~ dnorm(0, 0.01)
    beta3.p ~ dnorm(0, 0.01)
    beta4.p ~ dnorm(0, 0.01)
    
    # Likelihood
    # Ecological model for the partially observed true state
    for (i in 1:R) {
    z[i] ~ dbern(psi[i])                # True occurrence z at site i
    logit(psi[i]) <- alpha.psi + beta.psi * edge[i]
    
    # Observation model for the observations
    for (j in 1:T) {
    y[i,j] ~ dbern(mu.p[i,j])  # Detection-nondetection at i and j
    mu.p[i,j] <- z[i] * p[i,j]
    logit(p[i,j]) <- alpha.p + beta1.p * DATES[i,j] + beta2.p * pow(DATES[i,j], 2) + 
               beta3.p * HOURS[i,j] + beta4.p * pow(HOURS[i,j], 2)
    } #j
    } #i
    
    # Derived quantities
    occ.fs <- sum(z[])                             # Number of occupied sites
    mean.p <- exp(alpha.p) / (1 + exp(alpha.p))    # Sort of average detection
    }
    ",fill = TRUE)
sink()

# Bundle data
jags.data <- list(y = y, R = nrow(y), T = ncol(y), edge = edge, DATES = DATES, HOURS = HOURS)

# Initial values
zst <- apply(y, 1, max, na.rm = TRUE)	# Good starting values crucial
inits <- function(){list(z = zst, alpha.psi=runif(1, -3, 3), alpha.p = runif(1, -3, 3))}

# Parameters monitored
params <- c("alpha.psi", "beta.psi", "mean.p", "occ.fs", "alpha.p", 
            "beta1.p", "beta2.p", "beta3.p", "beta4.p")

# MCMC settings
ni <- 30000
nt <- 10
nb <- 20000
nc <- 3

#Load the correct library
library("jagsUI")

# Call JAGS from R
out <- jags(jags.data, inits, params, "bluebugmodel.txt", n.chains = nc, n.thin = nt, 
            n.iter = ni, n.burnin = nb)

# Summarize posteriors
print(out, digits=3)
plot(out)

### Examine the results
# Posterior distribution of the estimated number of occupied sites (woodpiles)
par(mfrow=c(1,1))
hist(out$sims.list$occ.fs, nclass = 30, col = "gray", main = "", 
     xlab = "Number of occupied sites (occ.fs)", xlim = c(9, 27))
## Add a line with the observed number of occuped sites
abline(v = 10, lwd = 2) 

########################

#Plot the posterior distributions of occupancy probabilities for:
#Forest interior locations (beta.psi=0)
#Forest edge locations (beta.psi=1)

par(mfrow = c(2, 1))
hist(plogis(out$sims.list$alpha.psi), nclass = 40, col = "gray", 
     main = "Forest interior", xlab = "Occupancy probability", xlim = c(0, 1))
hist(plogis(out$sims.list$alpha.psi+ out$sims.list$beta.psi), nclass = 40, 
     col = "gray", main = "Forest edge", xlab = "Occupancy probability", xlim = c(0, 1))

#############################


# Look at the effects of date and time of day on detection
# Incorporate the uncertainty estimated in the model
mcmc.sample <- length(out$sims.list$mean.p)

original.date.pred <- seq(0, 60, length.out = 30)
original.hour.pred <- seq(180, 540, length.out = 30)
date.pred <- (original.date.pred - mean.date)/sd.date
hour.pred <- (original.hour.pred - mean.hour)/sd.hour
p.pred.date <- plogis(out$mean$alpha.p + out$mean$beta1.p * date.pred + out$mean$beta2.p * date.pred^2 )
p.pred.hour <- plogis(out$mean$alpha.p + out$mean$beta3.p * hour.pred + out$mean$beta4.p * hour.pred^2 )

array.p.pred.hour <- array.p.pred.date <- array(NA, dim = c(length(hour.pred), mcmc.sample))
for (i in 1:mcmc.sample){
  array.p.pred.date[,i] <- plogis(out$sims.list$alpha.p[i] + out$sims.list$beta1.p[i] * 
                                    date.pred + out$sims.list$beta2.p[i] * date.pred^2)
  array.p.pred.hour[,i] <- plogis(out$sims.list$alpha.p[i] + out$sims.list$beta3.p[i] * 
                                    hour.pred + out$sims.list$beta4.p[i] * hour.pred^2)
}

# Plot for a subsample of MCMC draws (not the full posterior)
sub.set <- sort(sample(1:mcmc.sample, size = 200))

par(mfrow = c(2, 1))
plot(original.date.pred, p.pred.date, main = "", ylab = "Detection probability", 
     xlab = "Date (1 = 1 July)", ylim = c(0, 1), type = "l", lwd = 3, frame.plot = FALSE)
for (i in sub.set){
  lines(original.date.pred, array.p.pred.date[,i], type = "l", lwd = 1, col = "gray")
}
lines(original.date.pred, p.pred.date, type = "l", lwd = 3, col = "blue")

plot(original.hour.pred, p.pred.hour, main = "", ylab = "Detection probability", 
     xlab = "Time of day (mins after noon)", ylim = c(0, 1), type = "l", lwd = 3, frame.plot = FALSE)
for (i in sub.set){
  lines(original.hour.pred, array.p.pred.hour[,i], type = "l", lwd = 1, col = "gray")
}
lines(original.hour.pred, p.pred.hour, type = "l", lwd = 3, col = "blue")