#load Pew Data
library(haven)
PewData <- read_sav("Apr10 political-future public.sav")


#select variables I want from Pew Data 

require(MASS)
require(dplyr)


PewData1 <- PewData %>%
  dplyr::select(psraid,cregion, state, usr, scregion,sstate, smsa, q1, sex, age, educ,racethn, hispcmb,recage2)

PewData1 <- PewData1 %>%
  rename(state_fips = sstate)

#inserting State level indicator data
##subsettin, objname)
##install.packages("miceadds")
##library(miceadds)
##load.Rdata(1979-2016-president.RData, x)

newx <- subset(x, year==2008)

##creating new variable to look at vote share percent for each state for 2008 election
newx$voteshare <- (newx$candidatevotes/newx$totalvotes)*100


newx1 <- subset(newx, candidate == "Obama, Barack H.")
newx2 <- newx1 %>%
  dplyr::select(state, state_fips, voteshare) 


##going to combine this data with State data from MRP tutorial 
library(haven)
State <- read_dta("MRP_Primer_Replication_Files/state_level_update.dta")

State <- State %>%
  dplyr::select(sstate_initnum, sstate, sstatename, pbachelors)

##merge the state level indicators by FIPS

State <- State %>%
  rename(state=sstatename)

state <- merge(State, newx2, by="state")

library(readxl)
state <- read_excel("file_show.xlsx")


##reading in census data
##if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

##library(ipumsr)
##ddi <- read_ipums_ddi("usa_00001.xml")
##data <- read_ipums_micro(ddi)
##census <- data

#reading in census data (this includes both differentially private and standard estimates)
library(readr)
wide_dp1_040 <- read_csv("wide_dp1_040.csv")

newcen <- wide_dp1_040


#select variables I want
newcendp <- newcen %>%
  dplyr::select(name_dp, H7X001_dp, H7X001_dp, H7X002_dp, H7X003_dp, H7Y003_dp, H76007_dp, H76008_dp, H76009_dp, H76010_dp, 
                H76011_dp, H76012_dp, H76013_dp, H76014_dp, H76015_dp, H76016_dp,
                H76017_dp, H76018_dp, H76019_dp, H76020_dp, H76021_dp, H76022_dp, H76023_dp, H76024_dp, H76025_dp,
                H76031_dp, H76032_dp, H76033_dp, H76034_dp, H76035_dp, H76036_dp, H76037_dp, H76038_dp, H76039_dp,
                H76040_dp, H76041_dp, H76042_dp, H76043_dp, H76044_dp, H76045_dp, H76046_dp, H76047_dp, H76048_dp, 
                H76049_dp)

newcendp$white = (newcendp$H7X002_dp/newcendp$H7X001_dp)*100 #white percent
newcendp$black = (newcendp$H7X003_dp/newcendp$H7X001_dp)*100 #black percent
newcendp$hisp = (newcendp$H7Y003_dp/newcendp$H7X001_dp)*100 #hispanic percent

newcendp$totalpop = newcendp$H7X001_dp #total population

#creating my Age/Sex groups for DP data
newcendp$men1829 <- ((newcendp$H76007_dp+newcendp$H76008_dp+newcendp$H76009_dp+newcendp$H76010_dp+newcendp$H76011_dp)
                     /newcendp$totalpop)

newcendp$men3049 <- ((newcendp$H76012_dp+newcendp$H76013_dp+newcendp$H76014_dp+newcendp$H76015_dp)
                     /newcendp$totalpop)

newcendp$men5064 <- ((newcendp$H76016_dp+newcendp$H76017_dp+newcendp$H76018_dp+newcendp$H76019_dp)
                     /newcendp$totalpop)

newcendp$men65 <- ((newcendp$H76020_dp+newcendp$H76021_dp+newcendp$H76022_dp+newcendp$H76023_dp+newcendp$H76024_dp+newcendp$H76025_dp)
                   /newcendp$totalpop)

newcendp$women1829 <- ((newcendp$H76031_dp+newcendp$H76032_dp+newcendp$H76033_dp+newcendp$H76034_dp+newcendp$H76035_dp)
                       /newcendp$totalpop)

newcendp$women3049 <- ((newcendp$H76036_dp+newcendp$H76037_dp+newcendp$H76038_dp+newcendp$H76039_dp)
                       /newcendp$totalpop)

newcendp$women5064 <- ((newcendp$H76040_dp+newcendp$H76041_dp+newcendp$H76042_dp+newcendp$H76043_dp)
                       /newcendp$totalpop)

newcendp$women65 <- ((newcendp$H76044_dp+newcendp$H76045_dp+newcendp$H76046_dp+newcendp$H76047_dp+newcendp$H76048_dp+newcendp$H76049_dp)
                     /newcendp$totalpop)

newcendp1 <- newcendp %>%
  dplyr::select(name_dp, men1829, men3049, men5064, men65, women1829, women3049, women5064, women65)




####FOR STANDARD DATA

newcensf <- newcen %>%
  dplyr::select(name_sf, H7X001_sf, H7X001_sf, H7X002_sf, H7X003_sf, H7Y003_sf, H76007_sf,H76008_sf, H76009_sf, H76010_sf, 
                H76011_sf, H76012_sf, H76013_sf, H76014_sf, H76015_sf, H76016_sf,
                H76017_sf, H76018_sf, H76019_sf, H76020_sf, H76021_sf, H76022_sf, H76023_sf, H76024_sf, H76025_sf,
                H76031_sf, H76032_sf, H76033_sf, H76034_sf, H76035_sf, H76036_sf, H76037_sf, H76038_sf, H76039_sf,
                H76040_sf, H76041_sf, H76042_sf, H76043_sf, H76044_sf, H76045_sf, H76046_sf, H76047_sf, H76048_sf, 
                H76049_sf)


newcensf$white = (newcensf$H7X002_sf/newcensf$H7X001_sf)*100 #white percent
newcensf$black = (newcensf$H7X003_sf/newcensf$H7X001_sf)*100 #black percent
newcensf$hisp = (newcensf$H7Y003_sf/newcensf$H7X001_sf)*100 #hispanic percent

newcensf$totalpop = newcensf$H7X001_sf #total population

newcensf$men1829 <- ((newcensf$H76007_sf+newcensf$H76008_sf+newcensf$H76009_sf+newcensf$H76010_sf+newcensf$H76011_sf)
                     /newcensf$totalpop)

newcensf$men3049 <- ((newcensf$H76012_sf+newcensf$H76013_sf+newcensf$H76014_sf+newcensf$H76015_sf)
                     /newcensf$totalpop)


newcensf$men5064 <- ((newcensf$H76016_sf+newcensf$H76017_sf+newcensf$H76018_sf+newcensf$H76019_sf)
                     /newcensf$totalpop)

newcensf$men65 <- ((newcensf$H76020_sf+newcensf$H76021_sf+newcensf$H76022_sf+newcensf$H76023_sf+newcensf$H76024_sf+newcensf$H76025_sf)
                   /newcensf$totalpop)

newcensf$women1829 <- ((newcensf$H76031_sf+newcensf$H76032_sf+newcensf$H76033_sf+newcensf$H76034_sf+newcensf$H76035_sf)
                       /newcensf$totalpop)

newcensf$women3049 <- ((newcensf$H76036_sf+newcensf$H76037_sf+newcensf$H76038_sf+newcensf$H76039_sf)
                       /newcensf$totalpop)

newcensf$women5064 <- ((newcensf$H76040_sf+newcensf$H76041_sf+newcensf$H76042_sf+newcensf$H76043_sf)
                       /newcensf$totalpop)

newcensf$women65 <- ((newcensf$H76044_sf+newcensf$H76045_sf+newcensf$H76046_sf+newcensf$H76047_sf+newcensf$H76048_sf+newcensf$H76049_sf)
                     /newcensf$totalpop)

newcensf1 <- newcensf %>%
  dplyr::select(name_sf, men1829, men3049, men5064, men65, women1829, women3049, women5064, women65)



##merge state-level census data with info about state aggregate demo features
##creating state fixed effects dataset
mergestate <- newcensf %>%
  dplyr::select(name_sf, totalpop, white, black, hisp)

mergestate <- mergestate %>%
  rename(state = name_sf)

state <- merge(mergestate, state, by="state")


##now recoding age/sex demo variable in Pew dataset for individual level model
PewData1$AgeSex[PewData1$recage2==1 & PewData1$sex ==1] <-1
PewData1$AgeSex[PewData1$recage2==2 & PewData1$sex ==1] <-2
PewData1$AgeSex[PewData1$recage2==3 & PewData1$sex ==1] <-3
PewData1$AgeSex[PewData1$recage2==4 & PewData1$sex ==1] <-4
PewData1$AgeSex[PewData1$recage2==1 & PewData1$sex ==2] <-5
PewData1$AgeSex[PewData1$recage2==2 & PewData1$sex ==2] <-6
PewData1$AgeSex[PewData1$recage2==3 & PewData1$sex ==2] <-7
PewData1$AgeSex[PewData1$recage2==4 & PewData1$sex ==2] <-8




###dropping NA/refused responses
library(car)

PewData1$approve <- as.numeric(PewData1$q1)

PewData1 <- PewData1[which(PewData1$approve <= 8),] 

PewData1$approve <- ifelse(PewData1$approve == 1, "1", "0")


###because I cleaned some of the earlier data Census in excel
library(readxl)
mydata <- read_excel("mydata.xlsx")

dfdata <- read_excel("dfdata.xlsx")


dfdata <- dfdata %>%
  rename(state=name_dp)

mydata <- mydata %>%
  rename(state=name_sf)


##merging state level indicators and census data again
censussf <- merge(mydata, state, by="state")
censusdp <- merge(dfdata, state, by="state")

censussf <- censussf %>%
  rename(p.bachelor=pbachelors)

censusdp <- censusdp %>%
  rename(p.bachelor=pbachelors)


censusdp <- censusdp %>%
  rename(p.obama = voteshare)

censussf <- censussf %>%
  rename(p.obama = voteshare)


statePew <- state %>%
  dplyr::select(state, state_fips, sstate_initnum)

PewData1 <- merge(statePew, PewData1, by="state_fips")


##matching state level aggregate data to individual Pew data by state. 
PewData1$pobama <- state$voteshare[PewData1$sstate_initnum] # obama's 2008 vote share (0-100) 
PewData1$pbachelor <-state$pbachelors[PewData1$sstate_initnum]# proportion of bachelors degrees in respondent's 
PewData1$white.x <- state$white.x[PewData1$sstate_initnum]
PewData1$black.x <- state$black.x[PewData1$sstate_initnum]
PewData1$south <- state$south[PewData1$sstate_initnum]
PewData1$hisp.x <- state$hisp.x[PewData1$sstate_initnum]

PewData1 <- PewData1[order(PewData1$sstate_initnum),]


##making the mixed effects logit model! yay!
library(lme4)

individual.model1 <- glmer(formula = as.numeric(approve) ~ pobama + black.x + south
                           + (1|sstate_initnum) + (1|AgeSex)
                           ,data=PewData1, family=binomial(link="logit"))





display(individual.model1)
summary(individual.model1)



##capturing the random effects of AgeSex and State
state.ranefs <- array(NA,c(51,1))
dimnames(state.ranefs) <- list(c(state$sstate_initnum),"effect")
for(i in state$sstate_initnum){
  state.ranefs[i,1] <- ranef(individual.model1)$sstate_initnum[i,1]
}
state.ranefs[,1][is.na(state.ranefs[,1])] <- 0 

state.ranefs


agesex.ranef <- array(NA, c(8,1))

for(i in PewData1$AgeSex){
  agesex.ranef[i,1] <-ranef(individual.model1)$AgeSex["(Intercept)"][i,1]
}



##generating predictions all 400 cross classifications of agesex and state for dp and sf data

cellpredsf <- invlogit(fixef(individual.model1)["(Intercept)"]
                       + state.ranefs[censu1sf$sstate_initnum,1] 
                       + agesex.ranef[censu1sf$AgeSex,1]
                       + (fixef(individual.model1)["pobama"]*censu1sf$voteshare)
                       + (fixef(individual.model1)["black.x"]*censu1sf$black.x)
                       + (fixef(individual.model1)["south"] *censu1sf$south))


cellpreddp <- invlogit(fixef(individual.model1)["(Intercept)"]
                       + state.ranefs[censu1dp$sstate_initnum,1] 
                       + agesex.ranef[censu1dp$AgeSex,1]
                       + (fixef(individual.model1)["pobama"]*censu1dp$voteshare)
                       + (fixef(individual.model1)["black.x"]*censu1dp$black.x)
                       + (fixef(individual.model1)["south"] *censu1dp$south))




###weighing each prediction by its population frequency in the census datasets
cellpredweightedsf <- cellpredsf * censu1sf$realstratsf
cellpredweighteddp <- cellpreddp * censu1dp$realstratdp


###getting those state wide predictions by adding the weighted opinions from above
statepredsf <- 100*as.vector(tapply(cellpredweightedsf,censu1sf$state_fips,sum))

statepreddp <- 100*as.vector(tapply(cellpredweighteddp,censu1dp$state_fips,sum))


###quanitifying the differences in a table
different <-cbind(statepredsf, statepreddp)
different <- data.frame(different)


number <- array(c(1:50))
newdata <- cbind(number,different)
x2 <- data.frame(newdata)

##because wyoming got dropped in the process :(
x2 <- x2 %>%
  rename(sstate_initnum = number)
x2 <- rbind(x2, c("51","42.43558", "42.43605", "wyoming"))



small <- state %>%
  dplyr::select(state, sstate_initnum)


x2 <- merge(x2, small, by="sstate_initnum")

x2$difference = abs(as.numeric(x2$statepredsf) - as.numeric(x2$statepreddp))


###now to make some chloropleth maps

number <- array(c(1:50))
newdata4 <- cbind(number,statepredsf)
x24 <- data.frame(newdata4)

x2 = merge(x24, x2, by="number")


number <- array(c(1:50))
newdata5 <- cbind(number,statepreddp)
x25 <- data.frame(newdata5)

x3 = merge(x25, x2, by="number")


##for standard data
statedata = cbind(x2, state)

statedata = statedata %>%
  rename(region=state)

library(maps)
library(mapproj)
library(ggplot2)
library(dplyr)
require(maps)
library(viridis)

states <- map_data("state")

statedata[,3]=tolower(statedata[,3])

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)


mapdata <- inner_join(states, x2, by = "region")

ggplot(mapdata, aes(long, lat, group = group))+
  geom_polygon(aes(fill = statpredsf), color = "white")+
  scale_fill_viridis_c(option = "C")




##for differentially private data
statedata = cbind(x3, state)

statedata = statedata %>%
  rename(region=state)

library(maps)
library(mapproj)
library(ggplot2)
library(dplyr)
require(maps)
library(viridis)

states <- map_data("state")

statedata[,3]=tolower(statedata[,3])

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)


mapdata <- inner_join(states, x2, by = "region")

ggplot(mapdata, aes(long, lat, group = group))+
  geom_polygon(aes(fill = statepreddp), color = "white")+
  scale_fill_viridis_c(option = "C")




##for the difference in estimates 


ggplot(mapdata, aes(long, lat, group = group))+
  geom_polygon(aes(fill = different), color = "white")+
  scale_fill_viridis_c(option = "C")
