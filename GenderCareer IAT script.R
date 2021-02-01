# clear the environment: rm(list = ls())
# clear the console: ctrl+L
# clear all the plots: dev.off()

# Set where plots will be saved to:
setwd("/Users/jonathantownson/Documents/PhD/BBSRC-DTP/PIPS/TU Delft/GenderCareerPlots")

#### Packages ####
# For loading in the data
library(dplyr)
library(haven)
library(tidyr)
# For plotting and statistical analysis
library(ggplot2)
library(ggpubr)
# Install and read in the package containing the sankey function
install.packages("networkD3")
library(networkD3)
#### end ####

#### Read in the data ####
# Read in the Gender-Career data
GenderCareerPath <- file.path("/Users/jonathantownson/Desktop/Gender-Career IAT.public.2005-2019.sav")
GenderCareerIAT <- read_sav(GenderCareerPath)

# Keep data only of people who completed the whole test and remove this as a variable
GenderCareerIAT <- subset(GenderCareerIAT,session_status == "C")
GenderCareerIAT$session_status <- NULL

## Calculate age of participant at time of taking the test
GenderCareerIAT$CalcAge <-
  GenderCareerIAT$year - GenderCareerIAT$birthyear
GenderCareerIAT <- unite(GenderCareerIAT, "Age", c(age,CalcAge), na.rm = TRUE)

## Set country of residence at time of taking the test
# Use codebook to get country names
CountryRes <- read.csv("/Users/jonathantownson/Desktop/GenderScience_CountryRes.csv")
CountryResNum <- read.csv("/Users/jonathantownson/Desktop/GenderScience_CountryResNum.csv")

# Create VLookup function
VLookup <- function(this, data, key, value) {
  m <- match(this, data[[key]])
  data[[value]][m]
}

# Create 3 new columns with VLookup using data in country, countryres, countryres_num
GenderCareerIAT$y <- VLookup(GenderCareerIAT$countryres,CountryRes,"countryres","CountryAct")
GenderCareerIAT$z <- VLookup(GenderCareerIAT$countryres_num,CountryResNum,"countryres_num","CountryAct")

# Combine the 3 new columns into 1 called CountryResAct
GenderCareerIAT <- unite(GenderCareerIAT, "CountryResAct", c(y,z), na.rm = TRUE)

# Delete country, countryres, countryres_num
GenderCareerIAT$country <- NULL
GenderCareerIAT$countryres <- NULL
GenderCareerIAT$countryres_num <- NULL

rm(CountryRes)
rm(CountryResNum)

## Sort the sex, birthsex and gender identity columns
Gender <- read.csv("/Users/jonathantownson/Desktop/GenderScience_Gender.csv")
GenderCareerIAT$sex <- VLookup(GenderCareerIAT$sex,Gender,"sex","GenderAct")
GenderCareerIAT$birthsex <- VLookup(GenderCareerIAT$birthsex,Gender,"birthsex","GenderAct")
GenderCareerIAT$genderidentity <- VLookup(GenderCareerIAT$genderidentity,Gender,"genderidentity","GenderAct")

# Create a new column combining sex and genderidentity
GenderCareerIAT <- unite(GenderCareerIAT, "SexGenComb", c(sex,genderidentity),remove=FALSE,na.rm = TRUE)

rm(Gender)
#### end ####

MaleSubset <- subset(GenderCareerIAT, GenderCareerIAT$SexGenComb == "Male")
FemaleSubset <- subset(GenderCareerIAT, GenderCareerIAT$SexGenComb == "Female")
MaleandFemale <- rbind(MaleSubset,FemaleSubset)

#### Smooth line chart ####
# Smoothed chart, fits a regression line with 95% confidence interval
# Set where the data is retrieved from, the axes
GenderCareerTime <- ggplot(MaleandFemale, aes(x=date, y=D_biep.Male_Career_all, colour = SexGenComb)) +
  geom_smooth() +
  # Adds a title and axis labels
  labs(title="Gender-Career", x = "Year", y = "Increasing bias for Male and Career") + 
  # Centralises the title and removes the background. Can change fonts etc here too
  theme(plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
ggsave("GenderCareerTime.jpg",GenderCareerTime)
rm(GenderCareerTime)
#### end ####

#### Violin plot ####

# Function to calculate the number of observations for each X variable
# The multiplier (1.05) is determining where it will go on the plot in relation to the median
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

# Define lists to do statistical comparisons between
MalevsFemale <- list( c("Male", "Female"))

#Violin plot
# Set where the data is retrieved from, the axes and also will colour in the boxplots (fill) based on X variable
GenderCareerViolin <- ggplot(MaleandFemale, aes(x=SexGenComb, y=D_biep.Male_Career_all, fill=SexGenComb)) +
  # Plots the boxplot of the data and specified axes, legend removed as it just shows the colours.
  geom_violin(show.legend = FALSE, trim = FALSE) + 
  # Adds a title and axis labels
  labs(title="Gender-Career IAT score in Male vs Female", x = "Sex", y = "IAT score") + 
  # Centralises the title and removes the background. Can change fonts etc here too
  theme(plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # Adds p values for statistical analysis, here a t test of the means with two tails and assuming unequal variance
  stat_compare_means(method = "t.test", comparisons = MalevsFemale) +
  # Adds individual points for all data
  #geom_jitter(inherit.aes = TRUE, show.legend = FALSE, size = 0.5) + 
  geom_boxplot(width=0.1, show.legend = FALSE, outlier.shape = NA, notch = TRUE, coef=0)
# Adds the number of observations using the give.n function defined earlier
#stat_summary(fun.data = give.n, geom = "text", fun.y = median)
ggsave("GenderCareerViolin.jpg",GenderCareerViolin)
rm(GenderCareerViolin)
#### end ####

#### European map ####
library(ggplot2)
library(grid)
install.packages("rworldmap")
library(rworldmap)
install.packages("mapproj")
library(mapproj)

# Get the world map
WorldMap <- getMap()
# See names of countries in WorlMap
# WorldMap@data[["NAME"]]

# Select countries in Europe, did not include Russia
EuropeanCountries <- c("Albania","Andorra","Austria","Belarus","Belgium","Bosnia and Herz.","Bulgaria","Croatia","Cyprus",
                       "Czech Rep.","Denmark","Estonia","Finland","France",
                       "Germany","Greece","Holy See","Hungary","Iceland","Ireland","Italy","Kosovo","Latvia","Liechtenstein",
                       "Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro","Netherlands","Macedonia","Norway","Poland",
                       "Portugal","Romania","San Marino","Serbia","Slovakia","Slovenia","Spain",
                       "Sweden","Switzerland","United Kingdom","Ukraine")
# Select only the index of European Countries
IndEurope <- which(WorldMap$NAME%in%EuropeanCountries)

# Extract longitude and latitude border's coordinates of European Countries. 
EuropeCoords <- lapply(IndEurope, function(i){
  df <- data.frame(WorldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(WorldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

EuropeCoords <- do.call("rbind", EuropeCoords)

# Add some data for each country
EuropeanIAT <- select(GenderCareerIAT,CountryResAct,D_biep.Male_Career_all)
EuropeanIAT <- subset(EuropeanIAT, EuropeanIAT$CountryResAct == EuropeanCountries)
EuropeanIAT <- na.omit(EuropeanIAT)
EuropeanIAT <- aggregate(EuropeanIAT$D_biep.Male_Career_all,list(EuropeanIAT$CountryResAct),mean)

EuropeCoords$value <- EuropeanIAT$x[match(EuropeCoords$region,EuropeanIAT$Group.1)]

# Plot the map
EuropePlot <- ggplot() + geom_polygon(data = EuropeCoords, aes(x = long, y = lat, group = region, fill = value),
                                      colour = "black", size = 0.1) + 
  theme(plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


EuropePlot <- EuropePlot + scale_fill_gradient(name = "Gender-Career IAT score", low = "#82d7c6", high = "#0066a2", na.value = "#FFFFFF")


EuropePlot <- EuropePlot + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

EuropePlot <- EuropePlot + scale_fill_gradient(name = "Gender-Career IAT score", low = "#82d7c6", high = "#0066a2", na.value = "#FFFFFF")

ggsave("GenderCareerEurope.jpg",EuropePlot)
rm(EuropePlot)
#### end ####

#### Sankey Plot ####
# Extract relevant data and categorize according to Cohen 1977 for IAT and also into age groups
SankeyIAT <- GenderCareerIAT[, c("SexGenComb","Age", "D_biep.Male_Career_all")]
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all >= 0.8] = "Strong Male-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all < 0.8 & SankeyIAT$D_biep.Male_Career_all >= 0.5] = "Moderate Male-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all < 0.5 & SankeyIAT$D_biep.Male_Career_all >= 0.2] = "Weak Male-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all < 0.2 & SankeyIAT$D_biep.Male_Career_all > -0.2] = "Neutral Gender-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all > -0.5 & SankeyIAT$D_biep.Male_Career_all <= -0.2] = "Weak Female-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all > -0.8 & SankeyIAT$D_biep.Male_Career_all <= -0.5] = "Moderate Female-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all <= -0.8] = "Strong Female-Career bias"

SankeyIAT$AgeCategory[SankeyIAT$Age < 16 & SankeyIAT$Age >= 0] = "< 16"
SankeyIAT$AgeCategory[SankeyIAT$Age < 25 & SankeyIAT$Age >= 16] = "16-24"
SankeyIAT$AgeCategory[SankeyIAT$Age < 35 & SankeyIAT$Age >= 25] = "25-34"
SankeyIAT$AgeCategory[SankeyIAT$Age < 45 & SankeyIAT$Age >= 35] = "35-44"
SankeyIAT$AgeCategory[SankeyIAT$Age < 55 & SankeyIAT$Age >= 45] = "45-54"
SankeyIAT$AgeCategory[SankeyIAT$Age < 65 & SankeyIAT$Age >= 55] = "55-64"
SankeyIAT$AgeCategory[SankeyIAT$Age < 75 & SankeyIAT$Age >= 65] = "65-74"
SankeyIAT$AgeCategory[SankeyIAT$Age >= 75] = "75 +"

# Calculate links values
MalesSankey <- SankeyIAT %>% 
  filter(SexGenComb == "Male") %>% drop_na(IATCategory)
MalesSankey <- MalesSankey %>% 
  count(IATCategory)
AllCounts <- MalesSankey
AllCounts$Males <- MalesSankey$n
AllCounts$n <- NULL
rm(MalesSankey)

FemalesSankey <- SankeyIAT %>% 
  filter(SexGenComb == "Female") %>% drop_na(IATCategory)
FemalesSankey <- FemalesSankey %>% 
  count(IATCategory)
AllCounts$Females <- FemalesSankey$n
rm(FemalesSankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "< 16") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$Under16 <- AgeCategorySankey$n
rm(AgeCategorySankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "16-24") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$From16to24 <- AgeCategorySankey$n
rm(AgeCategorySankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "25-34") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$From25to34 <- AgeCategorySankey$n
rm(AgeCategorySankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "35-44") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$From35to44 <- AgeCategorySankey$n
rm(AgeCategorySankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "45-54") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$From45to54 <- AgeCategorySankey$n
rm(AgeCategorySankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "55-64") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$From55to64 <- AgeCategorySankey$n
rm(AgeCategorySankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "65-74") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$From65to74 <- AgeCategorySankey$n
rm(AgeCategorySankey)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "75 +") %>% drop_na(IATCategory)
AgeCategorySankey <- AgeCategorySankey %>% 
  count(IATCategory)
AllCounts$From75up <- AgeCategorySankey$n
rm(AgeCategorySankey)

AllCounts_pct <- AllCounts
AllCounts_pct$Males <- AllCounts_pct$Males/sum(AllCounts_pct$Males)
AllCounts_pct$Females <- AllCounts_pct$Females/sum(AllCounts_pct$Females)
AllCounts_pct$Under16 <- AllCounts_pct$Under16/sum(AllCounts_pct$Under16)
AllCounts_pct$From16to24 <- AllCounts_pct$From16to24/sum(AllCounts_pct$From16to24)
AllCounts_pct$From25to34 <- AllCounts_pct$From25to34/sum(AllCounts_pct$From25to34)
AllCounts_pct$From35to44 <- AllCounts_pct$From35to44/sum(AllCounts_pct$From35to44)
AllCounts_pct$From45to54 <- AllCounts_pct$From45to54/sum(AllCounts_pct$From45to54)
AllCounts_pct$From55to64 <- AllCounts_pct$From55to64/sum(AllCounts_pct$From55to64)
AllCounts_pct$From65to74 <- AllCounts_pct$From65to74/sum(AllCounts_pct$From65to74)
AllCounts_pct$From75up <- AllCounts_pct$From75up/sum(AllCounts_pct$From75up)


# Set how much data passes from one node to the next or to the one after. 
# Each row represents a link. The first number represents the node being 
# connected from, the second number represents the node connected to, the 
# third number is the value of the node.

## Plot All raw values 
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male", # Node 0
                       "Female", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias", # Node 8
                       "<16", # Node 9
                       "16-24", # Node 10
                       "25-34", # Node 11
                       "35-44", # Node 12
                       "45-54", # Node 13
                       "55-64", # Node 14
                       "65-74", # Node 15
                       "75 +"))# Node 16

links = as.data.frame(matrix(c(
  0, 2, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Males"]),
  0, 3, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Males"]),
  0, 4, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Males"]),
  0, 5, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Males"]),
  0, 6, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Males"]),
  0, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Males"]),
  0, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Males"]),
  
  1, 2, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Females"]),
  1, 3, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Females"]),
  1, 4, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Females"]),
  1, 5, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Females"]),
  1, 6, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Females"]),
  1, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Females"]),
  1, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Females"]),
  
  2, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Under16"]),
  2, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From16to24"]),
  2, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From25to34"]),
  2, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From35to44"]),
  2, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From45to54"]),
  2, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From55to64"]),
  2, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From65to74"]),
  2, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From75up"]),
  
  3, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Under16"]),
  3, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From16to24"]),
  3, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From25to34"]),
  3, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From35to44"]),
  3, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From45to54"]),
  3, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From55to64"]),
  3, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From65to74"]),
  3, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From75up"]),
  
  4, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Under16"]),
  4, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From16to24"]),
  4, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From25to34"]),
  4, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From35to44"]),
  4, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From45to54"]),
  4, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From55to64"]),
  4, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From65to74"]),
  4, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From75up"]),
  
  5, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Under16"]),
  5, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From16to24"]),
  5, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From25to34"]),
  5, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From35to44"]),
  5, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From45to54"]),
  5, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From55to64"]),
  5, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From65to74"]),
  5, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From75up"]),
  
  6, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Under16"]),
  6, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From16to24"]),
  6, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From25to34"]),
  6, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From35to44"]),
  6, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From45to54"]),
  6, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From55to64"]),
  6, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From65to74"]),
  6, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From75up"]),
  
  7, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Under16"]),
  7, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From16to24"]),
  7, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From25to34"]),
  7, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From35to44"]),
  7, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From45to54"]),
  7, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From55to64"]),
  7, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From65to74"]),
  7, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From75up"]),
  
  8, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Under16"]),
  8, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From16to24"]),
  8, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From25to34"]),
  8, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From35to44"]),
  8, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From45to54"]),
  8, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From55to64"]),
  8, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From65to74"]),
  8, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From75up"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllRaw <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllRaw, "GenderCareerAllRaw.html", selfcontained = TRUE)
rm(AllRaw)
## Plot All percentage values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male", # Node 0
                       "Female", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias", # Node 8
                       "<16", # Node 9
                       "16-24", # Node 10
                       "25-34", # Node 11
                       "35-44", # Node 12
                       "45-54", # Node 13
                       "55-64", # Node 14
                       "65-74", # Node 15
                       "75 +"))# Node 16

links = as.data.frame(matrix(c(
  0, 2, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Males"]),
  0, 3, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Males"]),
  0, 4, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Males"]),
  0, 5, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Males"]),
  0, 6, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Males"]),
  0, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Males"]),
  0, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Males"]),
  
  1, 2, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Females"]),
  1, 3, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Females"]),
  1, 4, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Females"]),
  1, 5, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Females"]),
  1, 6, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Females"]),
  1, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Females"]),
  1, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Females"]),
  
  2, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Under16"]),
  2, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From16to24"]),
  2, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From25to34"]),
  2, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From35to44"]),
  2, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From45to54"]),
  2, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From55to64"]),
  2, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From65to74"]),
  2, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From75up"]),
  
  3, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Under16"]),
  3, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From16to24"]),
  3, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From25to34"]),
  3, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From35to44"]),
  3, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From45to54"]),
  3, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From55to64"]),
  3, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From65to74"]),
  3, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From75up"]),
  
  4, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Under16"]),
  4, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From16to24"]),
  4, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From25to34"]),
  4, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From35to44"]),
  4, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From45to54"]),
  4, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From55to64"]),
  4, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From65to74"]),
  4, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From75up"]),
  
  5, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Under16"]),
  5, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From16to24"]),
  5, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From25to34"]),
  5, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From35to44"]),
  5, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From45to54"]),
  5, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From55to64"]),
  5, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From65to74"]),
  5, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From75up"]),
  
  6, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Under16"]),
  6, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From16to24"]),
  6, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From25to34"]),
  6, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From35to44"]),
  6, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From45to54"]),
  6, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From55to64"]),
  6, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From65to74"]),
  6, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From75up"]),
  
  7, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Under16"]),
  7, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From16to24"]),
  7, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From25to34"]),
  7, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From35to44"]),
  7, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From45to54"]),
  7, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From55to64"]),
  7, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From65to74"]),
  7, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From75up"]),
  
  8, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Under16"]),
  8, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From16to24"]),
  8, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From25to34"]),
  8, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From35to44"]),
  8, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From45to54"]),
  8, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From55to64"]),
  8, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From65to74"]),
  8, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From75up"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllPercent <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllPercent, "GenderCareerAllPercent.html", selfcontained = TRUE)
rm(AllPercent)
## Plot MaleFemale values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male", # Node 0
                       "Female", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias")) # Node 8

links = as.data.frame(matrix(c(
  0, 2, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Males"]),
  0, 3, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Males"]),
  0, 4, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Males"]),
  0, 5, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Males"]),
  0, 6, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Males"]),
  0, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Males"]),
  0, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Males"]),
  
  1, 2, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Females"]),
  1, 3, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Females"]),
  1, 4, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Females"]),
  1, 5, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Females"]),
  1, 6, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Females"]),
  1, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Females"]),
  1, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Females"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
MaleFemaleRaw <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(MaleFemaleRaw, "GenderCareerMaleFemaleRaw.html", selfcontained = TRUE)
rm(MaleFemaleRaw)
## Plot MaleFemale percentage values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male", # Node 0
                       "Female", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias")) # Node 8

links = as.data.frame(matrix(c(
  0, 2, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Males"]),
  0, 3, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Males"]),
  0, 4, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Males"]),
  0, 5, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Males"]),
  0, 6, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Males"]),
  0, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Males"]),
  0, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Males"]),
  
  1, 2, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Females"]),
  1, 3, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Females"]),
  1, 4, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Females"]),
  1, 5, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Females"]),
  1, 6, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Females"]),
  1, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Females"]),
  1, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Females"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
MaleFemalePercent <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(MaleFemalePercent, "GenderCareerMaleFemalePercent.html", selfcontained = TRUE)
rm(MaleFemalePercent)

## Plot Age raw values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male", # Node 0
                       "Female", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias", # Node 8
                       "<16", # Node 9
                       "16-24", # Node 10
                       "25-34", # Node 11
                       "35-44", # Node 12
                       "45-54", # Node 13
                       "55-64", # Node 14
                       "65-74", # Node 15
                       "75 +"))# Node 16

links = as.data.frame(matrix(c(
  2, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Under16"]),
  2, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From16to24"]),
  2, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From25to34"]),
  2, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From35to44"]),
  2, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From45to54"]),
  2, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From55to64"]),
  2, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From65to74"]),
  2, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From75up"]),
  
  3, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Under16"]),
  3, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From16to24"]),
  3, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From25to34"]),
  3, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From35to44"]),
  3, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From45to54"]),
  3, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From55to64"]),
  3, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From65to74"]),
  3, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From75up"]),
  
  4, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Under16"]),
  4, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From16to24"]),
  4, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From25to34"]),
  4, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From35to44"]),
  4, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From45to54"]),
  4, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From55to64"]),
  4, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From65to74"]),
  4, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From75up"]),
  
  5, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Under16"]),
  5, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From16to24"]),
  5, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From25to34"]),
  5, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From35to44"]),
  5, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From45to54"]),
  5, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From55to64"]),
  5, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From65to74"]),
  5, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From75up"]),
  
  6, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Under16"]),
  6, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From16to24"]),
  6, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From25to34"]),
  6, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From35to44"]),
  6, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From45to54"]),
  6, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From55to64"]),
  6, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From65to74"]),
  6, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From75up"]),
  
  7, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Under16"]),
  7, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From16to24"]),
  7, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From25to34"]),
  7, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From35to44"]),
  7, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From45to54"]),
  7, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From55to64"]),
  7, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From65to74"]),
  7, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From75up"]),
  
  8, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Under16"]),
  8, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From16to24"]),
  8, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From25to34"]),
  8, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From35to44"]),
  8, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From45to54"]),
  8, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From55to64"]),
  8, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From65to74"]),
  8, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From75up"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllRaw <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllRaw, "GenderCareerAgeRaw.html", selfcontained = TRUE)
rm(AllRaw)
## Plot Age percentage values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male", # Node 0
                       "Female", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias", # Node 8
                       "<16", # Node 9
                       "16-24", # Node 10
                       "25-34", # Node 11
                       "35-44", # Node 12
                       "45-54", # Node 13
                       "55-64", # Node 14
                       "65-74", # Node 15
                       "75 +"))# Node 16

links = as.data.frame(matrix(c(
  2, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Under16"]),
  2, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From16to24"]),
  2, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From25to34"]),
  2, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From35to44"]),
  2, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From45to54"]),
  2, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From55to64"]),
  2, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From65to74"]),
  2, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From75up"]),
  
  3, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Under16"]),
  3, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From16to24"]),
  3, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From25to34"]),
  3, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From35to44"]),
  3, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From45to54"]),
  3, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From55to64"]),
  3, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From65to74"]),
  3, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From75up"]),
  
  4, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Under16"]),
  4, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From16to24"]),
  4, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From25to34"]),
  4, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From35to44"]),
  4, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From45to54"]),
  4, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From55to64"]),
  4, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From65to74"]),
  4, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From75up"]),
  
  5, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Under16"]),
  5, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From16to24"]),
  5, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From25to34"]),
  5, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From35to44"]),
  5, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From45to54"]),
  5, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From55to64"]),
  5, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From65to74"]),
  5, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From75up"]),
  
  6, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Under16"]),
  6, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From16to24"]),
  6, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From25to34"]),
  6, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From35to44"]),
  6, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From45to54"]),
  6, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From55to64"]),
  6, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From65to74"]),
  6, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From75up"]),
  
  7, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Under16"]),
  7, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From16to24"]),
  7, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From25to34"]),
  7, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From35to44"]),
  7, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From45to54"]),
  7, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From55to64"]),
  7, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From65to74"]),
  7, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From75up"]),
  
  8, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Under16"]),
  8, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From16to24"]),
  8, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From25to34"]),
  8, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From35to44"]),
  8, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From45to54"]),
  8, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From55to64"]),
  8, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From65to74"]),
  8, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From75up"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllPercent <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "source", Target = "target",
                            Value = "value", NodeID = "name",
                            fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllPercent, "GenderCareerAgePercent.html", selfcontained = TRUE)
rm(AllPercent)
#### end ####

#### 2009 vs 2019 Sankey Plot ####

#Extract relevant data and categorize according to Cohen 1977 for IAT and also into age groups
SankeyIAT <- GenderCareerIAT[, c("SexGenComb","Age", "D_biep.Male_Career_all", "year")]
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all >= 0.8] = "Strong Male-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all < 0.8 & SankeyIAT$D_biep.Male_Career_all >= 0.5] = "Moderate Male-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all < 0.5 & SankeyIAT$D_biep.Male_Career_all >= 0.2] = "Weak Male-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all < 0.2 & SankeyIAT$D_biep.Male_Career_all > -0.2] = "Neutral Gender-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all > -0.5 & SankeyIAT$D_biep.Male_Career_all <= -0.2] = "Weak Female-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all > -0.8 & SankeyIAT$D_biep.Male_Career_all <= -0.5] = "Moderate Female-Career bias"
SankeyIAT$IATCategory[SankeyIAT$D_biep.Male_Career_all <= -0.8] = "Strong Female-Career bias"

SankeyIAT$AgeCategory[SankeyIAT$Age < 16 & SankeyIAT$Age >= 0] = "< 16"
SankeyIAT$AgeCategory[SankeyIAT$Age < 25 & SankeyIAT$Age >= 16] = "16-24"
SankeyIAT$AgeCategory[SankeyIAT$Age < 35 & SankeyIAT$Age >= 25] = "25-34"
SankeyIAT$AgeCategory[SankeyIAT$Age < 45 & SankeyIAT$Age >= 35] = "35-44"
SankeyIAT$AgeCategory[SankeyIAT$Age < 55 & SankeyIAT$Age >= 45] = "45-54"
SankeyIAT$AgeCategory[SankeyIAT$Age < 65 & SankeyIAT$Age >= 55] = "55-64"
SankeyIAT$AgeCategory[SankeyIAT$Age >= 65] = "65 +"

# Calculate links values
MalesSankey <- SankeyIAT %>% 
  filter(SexGenComb == "Male") %>% drop_na(IATCategory)
MalesSankey2009 <- subset(MalesSankey,year == "2009")
MalesSankey2019 <- subset(MalesSankey,year == "2019")
MalesSankey2009 <- MalesSankey2009 %>% 
  count(IATCategory)
MalesSankey2019 <- MalesSankey2019 %>% 
  count(IATCategory)
AllCounts <- MalesSankey2009
AllCounts$Males2009 <- MalesSankey2009$n
AllCounts$n <- NULL
AllCounts$Males2019 <- MalesSankey2019$n
rm(MalesSankey)
rm(MalesSankey2009)
rm(MalesSankey2019)

FemalesSankey <- SankeyIAT %>% 
  filter(SexGenComb == "Female") %>% drop_na(IATCategory)
FemalesSankey2009 <- subset(FemalesSankey,year == "2009")
FemalesSankey2019 <- subset(FemalesSankey,year == "2019")
FemalesSankey2009 <- FemalesSankey2009 %>% 
  count(IATCategory)
FemalesSankey2019 <- FemalesSankey2019 %>% 
  count(IATCategory)
AllCounts$Females2009 <- FemalesSankey2009$n
AllCounts$Females2019 <- FemalesSankey2019$n
rm(FemalesSankey)
rm(FemalesSankey2009)
rm(FemalesSankey2019)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "< 16") %>% drop_na(IATCategory)
AgeCategorySankey2009 <- subset(AgeCategorySankey,year == "2009")
AgeCategorySankey2019 <- subset(AgeCategorySankey,year == "2019")
AgeCategorySankey2009 <- AgeCategorySankey2009 %>% 
  count(IATCategory)
AgeCategorySankey2019 <- AgeCategorySankey2019 %>% 
  count(IATCategory)
AllCounts$Under16_2009 <- AgeCategorySankey2009$n
AllCounts$Under16_2019 <- AgeCategorySankey2019$n
rm(AgeCategorySankey)
rm(AgeCategorySankey2009)
rm(AgeCategorySankey2019)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "16-24") %>% drop_na(IATCategory)
AgeCategorySankey2009 <- subset(AgeCategorySankey,year == "2009")
AgeCategorySankey2019 <- subset(AgeCategorySankey,year == "2019")
AgeCategorySankey2009 <- AgeCategorySankey2009 %>% 
  count(IATCategory)
AgeCategorySankey2019 <- AgeCategorySankey2019 %>% 
  count(IATCategory)
AllCounts$From16to24_2009 <- AgeCategorySankey2009$n
AllCounts$From16to24_2019 <- AgeCategorySankey2019$n
rm(AgeCategorySankey)
rm(AgeCategorySankey2009)
rm(AgeCategorySankey2019)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "25-34") %>% drop_na(IATCategory)
AgeCategorySankey2009 <- subset(AgeCategorySankey,year == "2009")
AgeCategorySankey2019 <- subset(AgeCategorySankey,year == "2019")
AgeCategorySankey2009 <- AgeCategorySankey2009 %>% 
  count(IATCategory)
AgeCategorySankey2019 <- AgeCategorySankey2019 %>% 
  count(IATCategory)
AllCounts$From25to34_2009 <- AgeCategorySankey2009$n
AllCounts$From25to34_2019 <- AgeCategorySankey2019$n
rm(AgeCategorySankey)
rm(AgeCategorySankey2009)
rm(AgeCategorySankey2019)


AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "35-44") %>% drop_na(IATCategory)
AgeCategorySankey2009 <- subset(AgeCategorySankey,year == "2009")
AgeCategorySankey2019 <- subset(AgeCategorySankey,year == "2019")
AgeCategorySankey2009 <- AgeCategorySankey2009 %>% 
  count(IATCategory)
AgeCategorySankey2019 <- AgeCategorySankey2019 %>% 
  count(IATCategory)
AllCounts$From35to44_2009 <- AgeCategorySankey2009$n
AllCounts$From35to44_2019 <- AgeCategorySankey2019$n
rm(AgeCategorySankey)
rm(AgeCategorySankey2009)
rm(AgeCategorySankey2019)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "45-54") %>% drop_na(IATCategory)
AgeCategorySankey2009 <- subset(AgeCategorySankey,year == "2009")
AgeCategorySankey2019 <- subset(AgeCategorySankey,year == "2019")
AgeCategorySankey2009 <- AgeCategorySankey2009 %>% 
  count(IATCategory)
AgeCategorySankey2019 <- AgeCategorySankey2019 %>% 
  count(IATCategory)
AllCounts$From45to54_2009 <- AgeCategorySankey2009$n
AllCounts$From45to54_2019 <- AgeCategorySankey2019$n
rm(AgeCategorySankey)
rm(AgeCategorySankey2009)
rm(AgeCategorySankey2019)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "55-64") %>% drop_na(IATCategory)
AgeCategorySankey2009 <- subset(AgeCategorySankey,year == "2009")
AgeCategorySankey2019 <- subset(AgeCategorySankey,year == "2019")
AgeCategorySankey2009 <- AgeCategorySankey2009 %>% 
  count(IATCategory)
AgeCategorySankey2019 <- AgeCategorySankey2019 %>% 
  count(IATCategory)
AllCounts$From55to64_2009 <- AgeCategorySankey2009$n
AllCounts$From55to64_2019 <- AgeCategorySankey2019$n
rm(AgeCategorySankey)
rm(AgeCategorySankey2009)
rm(AgeCategorySankey2019)

AgeCategorySankey <- SankeyIAT %>% 
  filter(AgeCategory == "65 +") %>% drop_na(IATCategory)
AgeCategorySankey2009 <- subset(AgeCategorySankey,year == "2009")
AgeCategorySankey2019 <- subset(AgeCategorySankey,year == "2019")
AgeCategorySankey2009 <- AgeCategorySankey2009 %>% 
  count(IATCategory)
AgeCategorySankey2019 <- AgeCategorySankey2019 %>% 
  count(IATCategory)
AllCounts$From65up_2009 <- AgeCategorySankey2009$n
AllCounts$From65up_2019 <- AgeCategorySankey2019$n
rm(AgeCategorySankey)
rm(AgeCategorySankey2009)
rm(AgeCategorySankey2019)

AllCounts_pct <- AllCounts
AllCounts_pct$Males2009 <- AllCounts_pct$Males2009/sum(AllCounts_pct$Males2009)
AllCounts_pct$Females2009 <- AllCounts_pct$Females2009/sum(AllCounts_pct$Females2009)
AllCounts_pct$Under16_2009 <- AllCounts_pct$Under16_2009/sum(AllCounts_pct$Under16_2009)
AllCounts_pct$From16to24_2009 <- AllCounts_pct$From16to24_2009/sum(AllCounts_pct$From16to24_2009)
AllCounts_pct$From25to34_2009 <- AllCounts_pct$From25to34_2009/sum(AllCounts_pct$From25to34_2009)
AllCounts_pct$From35to44_2009 <- AllCounts_pct$From35to44_2009/sum(AllCounts_pct$From35to44_2009)
AllCounts_pct$From45to54_2009 <- AllCounts_pct$From45to54_2009/sum(AllCounts_pct$From45to54_2009)
AllCounts_pct$From55to64_2009 <- AllCounts_pct$From55to64_2009/sum(AllCounts_pct$From55to64_2009)
AllCounts_pct$From65up_2009 <- AllCounts_pct$From65up_2009/sum(AllCounts_pct$From65up_2009)
AllCounts_pct$Males2019 <- AllCounts_pct$Males2019/sum(AllCounts_pct$Males2019)
AllCounts_pct$Females2019 <- AllCounts_pct$Females2019/sum(AllCounts_pct$Females2019)
AllCounts_pct$Under16_2019 <- AllCounts_pct$Under16_2019/sum(AllCounts_pct$Under16_2019)
AllCounts_pct$From16to24_2019 <- AllCounts_pct$From16to24_2019/sum(AllCounts_pct$From16to24_2019)
AllCounts_pct$From25to34_2019 <- AllCounts_pct$From25to34_2019/sum(AllCounts_pct$From25to34_2019)
AllCounts_pct$From35to44_2019 <- AllCounts_pct$From35to44_2019/sum(AllCounts_pct$From35to44_2019)
AllCounts_pct$From45to54_2019 <- AllCounts_pct$From45to54_2019/sum(AllCounts_pct$From45to54_2019)
AllCounts_pct$From55to64_2019 <- AllCounts_pct$From55to64_2019/sum(AllCounts_pct$From55to64_2019)
AllCounts_pct$From65up_2019 <- AllCounts_pct$From65up_2019/sum(AllCounts_pct$From65up_2019)


# Set how much data passes from one node to the next or to the one after. 
# Each row represents a link. The first number represents the node being 
# connected from, the second number represents the node connected to, the 
# third number is the value of the node.

## Plot Gender 2009 vs 2019 raw values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male 2009", # Node 0
                       "Female 2009", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias", # Node 8
                       "Male 2019", # Node 9
                       "Female 2019")) # Node 10

links = as.data.frame(matrix(c(
  0, 2, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Males2009"]),
  0, 3, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Males2009"]),
  0, 4, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Males2009"]),
  0, 5, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Males2009"]),
  0, 6, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Males2009"]),
  0, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Males2009"]),
  0, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Males2009"]),
  
  1, 2, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Females2009"]),
  1, 3, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Females2009"]),
  1, 4, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Females2009"]),
  1, 5, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Females2009"]),
  1, 6, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Females2009"]),
  1, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Females2009"]),
  1, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Females2009"]),
  
  2, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Males2019"]),
  3, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Males2019"]),
  4, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Males2019"]),
  5, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Males2019"]),
  6, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Males2019"]),
  7, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Males2019"]),
  8, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Males2019"]),
  
  2, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Females2019"]),
  3, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Females2019"]),
  4, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Females2019"]),
  5, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Females2019"]),
  6, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Females2019"]),
  7, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Females2019"]),
  8, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Females2019"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllRaw <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllRaw, "GenderCareer2009v2019GenderRaw.html", selfcontained = TRUE)
rm(AllRaw)
## Plot All percentage values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("Male (2009)", # Node 0
                       "Female (2009)", # Node 1
                       "Strong Male-Career bias", # Node 2
                       "Moderate Male-Career bias", # Node 3
                       "Weak Male-Career bias", # Node 4
                       "Neutral Gender-Career bias", # Node 5
                       "Weak Female-Career bias", # Node 6
                       "Moderate Female-Career bias", # Node 7
                       "Strong Female-Career bias", # Node 8
                       "Male (2019)", # Node 9
                       "Female (2019)")) # Node 10

links = as.data.frame(matrix(c(
  0, 2, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Males2009"]),
  0, 3, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Males2009"]),
  0, 4, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Males2009"]),
  0, 5, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Males2009"]),
  0, 6, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Males2009"]),
  0, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Males2009"]),
  0, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Males2009"]),
  
  1, 2, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Females2009"]),
  1, 3, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Females2009"]),
  1, 4, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Females2009"]),
  1, 5, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Females2009"]),
  1, 6, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Females2009"]),
  1, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Females2009"]),
  1, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Females2009"]),
  
  2, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Males2019"]),
  3, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Males2019"]),
  4, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Males2019"]),
  5, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Males2019"]),
  6, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Males2019"]),
  7, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Males2019"]),
  8, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Males2019"]),
  
  2, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Females2019"]),
  3, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Females2019"]),
  4, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Females2019"]),
  5, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Females2019"]),
  6, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Females2019"]),
  7, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Females2019"]),
  8, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Females2019"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllPercent <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "source", Target = "target",
                            Value = "value", NodeID = "name",
                            fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllPercent, "GenderCareer2009v2019GenderPercent.html", selfcontained = TRUE)
rm(AllPercent)

## Plot Age raw values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("<16 (2009)", # Node 0
                       "16-24 (2009)", # Node 1
                       "25-34 (2009)", # Node 2
                       "35-44 (2009)", # Node 3
                       "45-54 (2009)", # Node 4
                       "55-64 (2009)", # Node 5
                       "65+ (2009)", # Node 6
                       "Strong Male-Career bias", # Node 7
                       "Moderate Male-Career bias", # Node 8
                       "Weak Male-Career bias", # Node 9
                       "Neutral Gender-Career bias", # Node 10
                       "Weak Female-Career bias", # Node 11
                       "Moderate Female-Career bias", # Node 12
                       "Strong Female-Career bias", # Node 13
                       "<16 (2019)", # Node 14
                       "16-24 (2019)", # Node 15
                       "25-34 (2019)", # Node 16
                       "35-44 (2019)", # Node 17
                       "45-54 (2019)", # Node 18
                       "55-64 (2019)", # Node 19
                       "65+ (2019)")) # Node 20

links = as.data.frame(matrix(c(
  0, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Under16_2009"]),
  1, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From16to24_2009"]),
  2, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From25to34_2009"]),
  3, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From35to44_2009"]),
  4, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From45to54_2009"]),
  5, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From55to64_2009"]),
  6, 7, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From65up_2009"]),
  
  0, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Under16_2009"]),
  1, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From16to24_2009"]),
  2, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From25to34_2009"]),
  3, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From35to44_2009"]),
  4, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From45to54_2009"]),
  5, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From55to64_2009"]),
  6, 8, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From65up_2009"]),
  
  0, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Under16_2009"]),
  1, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From16to24_2009"]),
  2, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From25to34_2009"]),
  3, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From35to44_2009"]),
  4, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From45to54_2009"]),
  5, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From55to64_2009"]),
  6, 9, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From65up_2009"]),
  
  0, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Under16_2009"]),
  1, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From16to24_2009"]),
  2, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From25to34_2009"]),
  3, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From35to44_2009"]),
  4, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From45to54_2009"]),
  5, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From55to64_2009"]),
  6, 10, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From65up_2009"]),
  
  0, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Under16_2009"]),
  1, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From16to24_2009"]),
  2, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From25to34_2009"]),
  3, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From35to44_2009"]),
  4, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From45to54_2009"]),
  5, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From55to64_2009"]),
  6, 11, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From65up_2009"]),
  
  0, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Under16_2009"]),
  1, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From16to24_2009"]),
  2, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From25to34_2009"]),
  3, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From35to44_2009"]),
  4, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From45to54_2009"]),
  5, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From55to64_2009"]),
  6, 12, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From65up_2009"]),
  
  0, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Under16_2009"]),
  1, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From16to24_2009"]),
  2, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From25to34_2009"]),
  3, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From35to44_2009"]),
  4, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From45to54_2009"]),
  5, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From55to64_2009"]),
  6, 13, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From65up_2009"]),
  
  7, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "Under16_2019"]),
  7, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From16to24_2019"]),
  7, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From25to34_2019"]),
  7, 17, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From35to44_2019"]),
  7, 18, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From45to54_2019"]),
  7, 19, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From55to64_2019"]),
  7, 20, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Male-Career bias", "From65up_2019"]),
  
  8, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "Under16_2019"]),
  8, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From16to24_2019"]),
  8, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From25to34_2019"]),
  8, 17, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From35to44_2019"]),
  8, 18, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From45to54_2019"]),
  8, 19, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From55to64_2019"]),
  8, 20, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Male-Career bias", "From65up_2019"]),
  
  9, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "Under16_2019"]),
  9, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From16to24_2019"]),
  9, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From25to34_2019"]),
  9, 17, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From35to44_2019"]),
  9, 18, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From45to54_2019"]),
  9, 19, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From55to64_2019"]),
  9, 20, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Male-Career bias", "From65up_2019"]),
  
  10, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "Under16_2019"]),
  10, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From16to24_2019"]),
  10, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From25to34_2019"]),
  10, 17, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From35to44_2019"]),
  10, 18, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From45to54_2019"]),
  10, 19, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From55to64_2019"]),
  10, 20, as.numeric(AllCounts[AllCounts$IATCategory == "Neutral Gender-Career bias", "From65up_2019"]),
  
  11, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "Under16_2019"]),
  11, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From16to24_2019"]),
  11, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From25to34_2019"]),
  11, 17, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From35to44_2019"]),
  11, 18, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From45to54_2019"]),
  11, 19, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From55to64_2019"]),
  11, 20, as.numeric(AllCounts[AllCounts$IATCategory == "Weak Female-Career bias", "From65up_2019"]),
  
  12, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "Under16_2019"]),
  12, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From16to24_2019"]),
  12, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From25to34_2019"]),
  12, 17, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From35to44_2019"]),
  12, 18, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From45to54_2019"]),
  12, 19, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From55to64_2019"]),
  12, 20, as.numeric(AllCounts[AllCounts$IATCategory == "Moderate Female-Career bias", "From65up_2019"]),
  
  13, 14, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "Under16_2019"]),
  13, 15, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From16to24_2019"]),
  13, 16, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From25to34_2019"]),
  13, 17, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From35to44_2019"]),
  13, 18, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From45to54_2019"]),
  13, 19, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From55to64_2019"]),
  13, 20, as.numeric(AllCounts[AllCounts$IATCategory == "Strong Female-Career bias", "From65up_2019"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllRaw <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllRaw, "GenderCareer2009v2019AgeRaw.html", selfcontained = TRUE)
rm(AllRaw)
## Plot Age percentage values
# Set what the data will "pass through"
nodes = data.frame("name" = 
                     c("<16 (2009)", # Node 0
                       "16-24 (2009)", # Node 1
                       "25-34 (2009)", # Node 2
                       "35-44 (2009)", # Node 3
                       "45-54 (2009)", # Node 4
                       "55-64 (2009)", # Node 5
                       "65+ (2009)", # Node 6
                       "Strong Male-Career bias", # Node 7
                       "Moderate Male-Career bias", # Node 8
                       "Weak Male-Career bias", # Node 9
                       "Neutral Gender-Career bias", # Node 10
                       "Weak Female-Career bias", # Node 11
                       "Moderate Female-Career bias", # Node 12
                       "Strong Female-Career bias", # Node 13
                       "<16 (2019)", # Node 14
                       "16-24 (2019)", # Node 15
                       "25-34 (2019)", # Node 16
                       "35-44 (2019)", # Node 17
                       "45-54 (2019)", # Node 18
                       "55-64 (2019)", # Node 19
                       "65+ (2019)")) # Node 20

links = as.data.frame(matrix(c(
  0, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Under16_2009"]),
  1, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From16to24_2009"]),
  2, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From25to34_2009"]),
  3, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From35to44_2009"]),
  4, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From45to54_2009"]),
  5, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From55to64_2009"]),
  6, 7, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From65up_2009"]),
  
  0, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Under16_2009"]),
  1, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From16to24_2009"]),
  2, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From25to34_2009"]),
  3, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From35to44_2009"]),
  4, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From45to54_2009"]),
  5, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From55to64_2009"]),
  6, 8, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From65up_2009"]),
  
  0, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Under16_2009"]),
  1, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From16to24_2009"]),
  2, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From25to34_2009"]),
  3, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From35to44_2009"]),
  4, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From45to54_2009"]),
  5, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From55to64_2009"]),
  6, 9, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From65up_2009"]),
  
  0, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Under16_2009"]),
  1, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From16to24_2009"]),
  2, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From25to34_2009"]),
  3, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From35to44_2009"]),
  4, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From45to54_2009"]),
  5, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From55to64_2009"]),
  6, 10, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From65up_2009"]),
  
  0, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Under16_2009"]),
  1, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From16to24_2009"]),
  2, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From25to34_2009"]),
  3, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From35to44_2009"]),
  4, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From45to54_2009"]),
  5, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From55to64_2009"]),
  6, 11, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From65up_2009"]),
  
  0, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Under16_2009"]),
  1, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From16to24_2009"]),
  2, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From25to34_2009"]),
  3, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From35to44_2009"]),
  4, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From45to54_2009"]),
  5, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From55to64_2009"]),
  6, 12, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From65up_2009"]),
  
  0, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Under16_2009"]),
  1, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From16to24_2009"]),
  2, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From25to34_2009"]),
  3, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From35to44_2009"]),
  4, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From45to54_2009"]),
  5, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From55to64_2009"]),
  6, 13, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From65up_2009"]),
  
  7, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "Under16_2019"]),
  7, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From16to24_2019"]),
  7, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From25to34_2019"]),
  7, 17, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From35to44_2019"]),
  7, 18, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From45to54_2019"]),
  7, 19, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From55to64_2019"]),
  7, 20, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Male-Career bias", "From65up_2019"]),
  
  8, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "Under16_2019"]),
  8, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From16to24_2019"]),
  8, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From25to34_2019"]),
  8, 17, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From35to44_2019"]),
  8, 18, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From45to54_2019"]),
  8, 19, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From55to64_2019"]),
  8, 20, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Male-Career bias", "From65up_2019"]),
  
  9, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "Under16_2019"]),
  9, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From16to24_2019"]),
  9, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From25to34_2019"]),
  9, 17, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From35to44_2019"]),
  9, 18, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From45to54_2019"]),
  9, 19, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From55to64_2019"]),
  9, 20, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Male-Career bias", "From65up_2019"]),
  
  10, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "Under16_2019"]),
  10, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From16to24_2019"]),
  10, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From25to34_2019"]),
  10, 17, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From35to44_2019"]),
  10, 18, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From45to54_2019"]),
  10, 19, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From55to64_2019"]),
  10, 20, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Neutral Gender-Career bias", "From65up_2019"]),
  
  11, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "Under16_2019"]),
  11, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From16to24_2019"]),
  11, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From25to34_2019"]),
  11, 17, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From35to44_2019"]),
  11, 18, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From45to54_2019"]),
  11, 19, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From55to64_2019"]),
  11, 20, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Weak Female-Career bias", "From65up_2019"]),
  
  12, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "Under16_2019"]),
  12, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From16to24_2019"]),
  12, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From25to34_2019"]),
  12, 17, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From35to44_2019"]),
  12, 18, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From45to54_2019"]),
  12, 19, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From55to64_2019"]),
  12, 20, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Moderate Female-Career bias", "From65up_2019"]),
  
  13, 14, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "Under16_2019"]),
  13, 15, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From16to24_2019"]),
  13, 16, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From25to34_2019"]),
  13, 17, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From35to44_2019"]),
  13, 18, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From45to54_2019"]),
  13, 19, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From55to64_2019"]),
  13, 20, as.numeric(AllCounts_pct[AllCounts_pct$IATCategory == "Strong Female-Career bias", "From65up_2019"])
),
byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
AllPercent <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "source", Target = "target",
                            Value = "value", NodeID = "name",
                            fontSize= 12, nodeWidth = 30, iterations = 0)
saveNetwork(AllPercent, "GenderCareer2009v2019AgePercent.html", selfcontained = TRUE)
rm(AllPercent)
#### end ####
