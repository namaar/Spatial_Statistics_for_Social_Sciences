#
#   title: BB workshop in R
#   author: Natasza Marrouch
#   date: January 24, 2018
#
                                  ### ROADMAP ###
# R Shortcuts  ...................... line 21
# Links to Databases with examples of variables and indicators
# Individual responses  ........ line 27
# Macro  ....................... line 45
# Section 0: Quick R intro (you need to change path=  line 78)  ........ line 73
# SECTION 1: Template maps in R .... line 99
# SECTION 2: Aggregating based on Latitude and Longitude & adding state, county, etc.  ........ line 115
# SECTION 3: Adding Values to polygon shape data for mapping variables. Mapping values of a var   ........ line 136
# SECTION 4: Significance testing of spatial dependencies  ........ line 185
# SECTION 5: Multilevel modeles  ........ line 210
# SECTION 6: Caution & Conclusions  ..... line 241

####################################################################################

                            ###  R shortcuts  ####

# https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts

                    ####  DATA SOURCES WITH EXAMPEL VARIABLES  ####

## INDIVIDUAL LEVEL ##
# https://ropercenter.cornell.edu/polls/
# Gallup
# CBS
# PEW

# World Values Survey: http://www.worldvaluessurvey.org/wvs.jsp
# Schwartz Values Survey (10 item version)
# Country
# Age
# Religiousity
# etc.

# Project Implicit: https://implicit.harvard.edu/implicit/
# race, gender, etc. implicit and explicit measures of stereotypes*


## MACRO LEVEL INDICATORS ##

# UN Data**: http://data.un.org/Explorer.aspx
# Human development Indices
# GINI (income inequality measure)
# GII (gender inequality index)
# Crime
# Child labor

# Fragile State Index: http://fundforpeace.org/fsi/analytics/
# Country instability based on economic, political, societal, etc. indicators
#** Many thanks to Felicia who suggested FSI when I was looking for a measure of this type,
# and reminding about UN database!

# World Bank Data: https://data.worldbank.org/data-catalog
# Population Density
# GDP
# Number of people affected by natural disasters

# Gap Minder***: https://www.gapminder.org/data/
# health
# Life expectency
# Adults with HIV
# Alcohol consumption
# Causes of death in children
#*** Many thanks to Blair for pointing the database!
####################################################################################

                          ##### SECTION 0: A short intro R ####

                                ## Paths & Directories ##

path="enter/path/to/files"
list.files(path, pattern=NULL, all.files=FALSE, full.names=FALSE)
#or
dir(path, pattern=NULL, all.files=FALSE, full.names=FALSE)
# Setting the working directory
setwd(path)
# Getting the working directory

## installing required packages ##

#install.packages(c("maps","ggmap","rgdal","rgeos","maptools","tmap","dplyr","tidyr","sp","ape","nlme","knitr","xtable","lm.beta"))

pckg1=c("maps","ggmap","rgdal","rgeos","maptools","tmap","dplyr","tidyr","sp","ape","nlme","knitr","xtable","lm.beta")
lapply(pckg1, require, character.only = TRUE)

# or

require("maps")
require("ggmaps")

####################################################################################

                    ##### SECTION 1: Template maps in R ####

                                  ## SHAPE FILES ##
# Example 1: World template #
CTRY=readOGR(dsn=paste(path,"ShapeFiles/country",sep=""),"country")
plot(CTRY)

# Example 2: U.S.A. template #

st=readOGR(dsn=paste(path,"ShapeFiles/states",sep=""),"states")
st <- st[st@data[,5]!="AK",]
st <- st[st@data[,5]!="HI",]
plot(st)

####################################################################################

                ##### SECTION 2: Adding geographical variables ####


### FIPS, county, state, and ZIP variables from LAT & LON coordinates ###

#install.packages(c("MazamaSpatialUtils","rworldxtra","rworldmap", "ggmap"))
pckg2=c("MazamaSpatialUtils","rworldxtra","rworldmap", "RColorBrewer")
lapply(pckg2, require, character.only = TRUE)

### Reading in data from qualtrics collected via MTurk ###
MTurk=data.frame(read.csv(paste(path,"input/BB_MTurkQualtrics.csv",sep="")))

## Creating a combined file with the county name, fip number, and state based on lat & lon from QUALTRICS data##
setSpatialDataDir(paste(path,"R_MazamaSpatialUtils/SpatialData",sep=""))
loadSpatialData("USCensusCounties")
MSU_data<-getUSCounty(lon=MTurk[,8], lat=MTurk[,7], dataset = "USCensusCounties", stateCodes = NULL, allData = TRUE)
data_eg<-data.frame(cbind(MTurk,MSU_data[,c(8,9)]))


####################################################################################

                  ##### SECTION 3: Mapping Values of Variables ####

load(paste(path,"input/gallupANDurban.RData",sep=""))

## Using the code below you can map other values, e.g., mental health, obesity, SDO, IAT score

# In this examples I am using the importance of religion to see how well a linear model does in predicting a well studied DV
# 1. Below I regress the DV on the IVs linked in psych lit to the DV: Gender, Age, Education, Income, Ideology

m1=lm(relimp ~ female + age + income + education + moderate + conservative, data=glp2l)
summary(m1)

# You can see from the summary table that things are "as they should": women, older people,
# people with lower education and income, and conservatives are more religious -- time to publish a paper?
# 2. Not so fast -- to see if there may be other factors that LM is overlooking, we can create a new variable using the
# coefficients from the summary table: the predicted level of DV for each subject based on their IVs, then subtract it from the observed level.
# 3. This way we have the residuals for each person (the dataset has it already, its called Res)
# 4. If the model tells the full story (is correctly specified), we should expect that the residuals are independent.
# 5. One way to test it is to look at their averages ResM (better even variance ResVar) by state

## Building a template map ##

cty=readOGR(dsn=paste(path,"ShapeFiles/states",sep=""),"states")
cty <- cty[cty@data[,5]!="AK",]
cty <- cty[cty@data[,5]!="HI",]


## The below can be done more cleanly, but it does the trick ##

ResM<-glp2l[,c("STATE_NAME","resM")]
ResM <- unique(ResM)
# ask me about the two lines below if you are curious
ResM$resM <- ResM$resM-0.03664982
ResM$var <-ResM$resM^2-0.03664982
ctyResM=cty

## Merging the residuals data with a spatial polygon data (the cty template) ##
# *** ResM is a subset of my data that includes the name of the state and the residual mean and var

ctyResM@data<-left_join(ctyResM@data,ResM)

## Creating the Plots ##

qtm(ctyResM,fill="resM",text="STATE_NAME",text.size="AREA",fill.n=44, fill.palette="RdBu", fill.auto.palette.mapping=FALSE,fill.legend.show = F,title="Residual Average by State",title.position = c("left","bottom"))

qtm(ctyResM,fill="var",text="STATE_NAME",text.size="AREA",fill.n=44, fill.palette="OrRd", fill.auto.palette.mapping=FALSE,fill.legend.show = F,title="Residual Variance by State",title.position = c("left","bottom"))

####################################################################################

            ##### SECTION 4: Significance testing of spatial dependencies ####


# The plots show clustering of states where the model over and underestimates the observed levels of the DV
# The color is one thing, but we need a way to see if this is merely an accident

## reading in population centered points for each state to calculate the matrix of distances between them

pop<-read.csv(url("http://www2.census.gov/geo/docs/reference/cenpop2010/CenPop2010_Mean_ST.txt"))
s <- unique(glp2l$STATE_NAME)
popC=pop[pop$STNAME%in%s,]

## caluculating a measure of spatial clustering (there are many others)
locations <- as.matrix(popC[,c(5,4)])
DistancesI <- spDists(locations,longlat=TRUE)
weights=1/DistancesI
weights[is.infinite(weights)] <- 0
Moran.I(x=ResM$resM,weight=weights)

# p<.05 -- we need to assume that the residuals are not spatially independent, and the
# clustering of high versus low values we see on the map is beyond that we would expect if
# this was an expression of a stochastic process
####################################################################################


                    #### SECTION 5: MULTILEVEL MODELS ####

## Temporary function to calculate standardized coefficients if you want them##
coef.lme <- function(mod){
  res <- data.frame(
    "Beta.CI" = paste(round(summary(mod)$coefficients$fixed, 3), " (",round(summary(mod)$coefficients$fixed-1.96*sqrt(diag(mod$varFix)), 2), ",", round(summary(mod)$coefficients$fixed+1.96*sqrt(diag(mod$varFix)), 2),")", sep=""),
    "P.value" = round(2 * pt(-abs(summary(mod)$coefficients$fixed/sqrt(diag(mod$varFix))), summary(mod)$fixDF[[1]]), 3)
  )
  return(res)
}


## Model 01: State as a random intercept ##

glp.int01 <- groupedData( relimp ~  1 + age + female + income + education + moderate + conservative| STATE_NAME, data=glp2l)
mlm01 <- lme(relimp ~ 1 + age + female + income + education + moderate + conservative, data=glp.int01, random = ~ 1 | STATE_NAME, na.action=na.omit,method="REML", corr=NULL)
summary(mlm01)
coef.lme(mlm01)
# Things are still looking pretty well but if you look carefully, not as bright

## Model 02: State as a random intercept, % of population living in urban areas as macro-level IV ##
# * There is theoretical rationale to look at this

glp.int02 <- groupedData( relimp ~  1 + age + female + income + education + moderate + conservative| STATE_NAME, data=glp2l, outer= ~ POPPCT_URBAN)
mlm02 <- lme(relimp ~ female + age + income + moderate + conservative + education + POPPCT_URBAN, data=glp.int02, random = ~ 1 | STATE_NAME, na.action=na.omit)
summary(mlm02)
coef.lme(mlm02)


####################################################################################

                  #### SECTION 6: Caution & Conclusions ####

## This is only meant to be an example to show what you can do with your variables.

# Do not trust the p's .... *
# * kidding (but not entirely)


