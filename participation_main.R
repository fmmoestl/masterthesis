library(haven)
library(tidyverse)
library(tidyr)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(writexl)
library(psych)
library(MASS)
library(stargazer)
library(htmlTable)


setwd("C:/Users/JohnDoe/Desktop/Master/R")

# load eb and input data
eb <- read_dta("ZA7902_v1-0-0.dta")
input <- readxl::read_xlsx("DataOutput.xlsx", col_names = TRUE, sheet = "Test_neu")
country_level <- readxl::read_xlsx("DataOutput.xlsx", col_names = TRUE, sheet = "Calc_1")


# write_xlsx(eb, "eb.xlsx")


## subsetting the data for DV

subset_vector <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", "Greece", "Croatia",
                   "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Sweden", 
                   "EU citizen",	"Company/business",	"Non-governmental organisation (NGO)",	"Public authority",	"Other	Non-EU citizen", "Business association",	"Trade union",	"Consumer organisation",	"Environmental organisation",	"Academic/research Institution",
                   "Name",	"Category",	"SUM_CNTRY"
)

names.use <- names(input)[(names(input) %in% subset_vector)]
input <- input[, names.use]


## creating a country_info df with population information

country_column <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR",
                  "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SI", "SK", "SE")
population_column <- c(8932664,11554767,6916548,896007,10701777,83155031,5840045,1330068,47398695,5533793,67656682,10678632,4036355,
                   9730772,5006324,59236213,2795680,634730,1893223,516100,17475415,37840001,10298252,19201662,2108977,5459781,10379295)
active_citizen <- c(28.3, 20.4, 5.2, 7.2, 12.2, 28.6, 38.7, 16.4, 10.7, 34.1, 23, 11.7, 9.7, 
                    6.9, 29, 12, 16.3, 36.7, 7.3, 8.7, 40.3, 13.8, 9, 3.2, 30.4, 8.3, 35.5)



country_info <- data.frame(country_column, population_column, active_citizen)

names(country_info)[which(names(country_info) == "country_column")] <- "isocntry"
names(country_info)[which(names(country_info) == "population_column")] <- "population"

# Calculating the total population
country_info$pop_share <- NULL

total_population <- sum(country_info$population)  

country_info$pop_share <- country_info$population / total_population  # Calculating the population share

## subsetting the eb datase

cntry <- c("AT", "BE", "BG", "CY", "CZ", "DE-W", "DE-E", "DK", "EE", "ES", "FI", "FR", "GR", "HR",
           "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SI", "SK", "SE")


## Subset the eb dataset based on the country codes
subset_eb <- eb[eb$isocntry %in% cntry, ]

## merge DE-W and DE-E to DE

subset_eb <- subset_eb %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))

## rename the column of input to isocntry
names(input)[which(names(input) == "A2")] <- "isocntry"
names(country_level)[which(names(country_level) == "A2")] <- "isocntry"


# calculate the share of the responses of each country 
# for every legislative proposal in the input data frame

input$Austria <- input$Austria / input$SUM_CNTRY
input$Belgium <- input$Belgium / input$SUM_CNTRY
input$Bulgaria <- input$Bulgaria / input$SUM_CNTRY
input$Cyprus <- input$Cyprus / input$SUM_CNTRY
input$Czechia <- input$Czechia / input$SUM_CNTRY
input$Germany <- input$Germany / input$SUM_CNTRY
input$Denmark <- input$Denmark / input$SUM_CNTRY
input$Estonia <- input$Estonia / input$SUM_CNTRY
input$Spain <- input$Spain / input$SUM_CNTRY
input$Finland  <- input$Finland / input$SUM_CNTRY
input$France <- input$France / input$SUM_CNTRY
input$Greece <- input$Greece / input$SUM_CNTRY
input$Croatia <- input$Croatia / input$SUM_CNTRY
input$Hungary <- input$Hungary / input$SUM_CNTRY
input$Ireland <- input$Ireland / input$SUM_CNTRY
input$Italy <- input$Italy / input$SUM_CNTRY
input$Lithuania <- input$Lithuania / input$SUM_CNTRY
input$Luxembourg <- input$Luxembourg / input$SUM_CNTRY
input$Latvia <- input$Latvia / input$SUM_CNTRY
input$Malta <- input$Malta / input$SUM_CNTRY
input$Netherlands <- input$Netherlands / input$SUM_CNTRY
input$Poland <- input$Poland / input$SUM_CNTRY
input$Portugal <- input$Portugal / input$SUM_CNTRY
input$Romania <- input$Romania / input$SUM_CNTRY
input$Slovenia <- input$Slovenia / input$SUM_CNTRY
input$Slovakia <- input$Slovakia / input$SUM_CNTRY
input$Sweden <- input$Sweden / input$SUM_CNTRY

#rename the column headers to country shortcuts

names(input)[which(names(input) == "Austria")] <- "AT"
names(input)[which(names(input) == "Belgium")] <- "BE"
names(input)[which(names(input) == "Bulgaria")] <- "BG"
names(input)[which(names(input) == "Cyprus")] <- "CY"
names(input)[which(names(input) == "Czechia")] <- "CZ"
names(input)[which(names(input) == "Germany")] <- "DE"
names(input)[which(names(input) == "Denmark")] <- "DK"
names(input)[which(names(input) == "Estonia")] <- "EE"
names(input)[which(names(input) == "Spain")] <- "ES"
names(input)[which(names(input) == "Finland")] <- "FI"
names(input)[which(names(input) == "France")] <- "FR"
names(input)[which(names(input) == "Greece")] <- "GR"
names(input)[which(names(input) == "Croatia")] <- "HR"
names(input)[which(names(input) == "Hungary")] <- "HU"
names(input)[which(names(input) == "Ireland")] <- "IE"
names(input)[which(names(input) == "Italy")] <- "IT"
names(input)[which(names(input) == "Lithuania")] <- "LT"
names(input)[which(names(input) == "Luxembourg")] <- "LU"
names(input)[which(names(input) == "Latvia")] <- "LV"
names(input)[which(names(input) == "Malta")] <- "MT"
names(input)[which(names(input) == "Netherlands")] <- "NL"
names(input)[which(names(input) == "Poland")] <- "PL"
names(input)[which(names(input) == "Portugal")] <- "PT"
names(input)[which(names(input) == "Romania")] <- "RO"
names(input)[which(names(input) == "Slovenia")] <- "SI"
names(input)[which(names(input) == "Slovakia")] <- "SK"
names(input)[which(names(input) == "Sweden")] <- "SE"



#######
#######
#                 Build the "daten" dataframe
#######
#######

# Create an empty dataframe "daten" with the appropriate structure
daten <- data.frame(
  legislation = rep(unique(input$Name), length(unique(colnames(input)[4:30]))),
  isocntry = rep(unique(colnames(input)[4:30]), each = length(unique(input$Name))),
  z = NA
)

# Iterate over each legislation and country combination in "daten"
for (i in 1:nrow(daten)) {
  legislation <- daten$legislation[i]
  isocntry <- daten$isocntry[i]
  
  # Get the corresponding z value from "input"
  z <- input[input$Name == legislation, isocntry]
  
  # Assign the z value to "daten"
  daten$z[i] <- z
}

## add country_info data to "daten"

daten <- merge(daten, country_info, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)

# calculate and add dependent variable "y" for each country and each legislation 

daten$y <- vector("numeric", nrow(daten))
daten$y <- as.numeric(((as.numeric(daten$z) - as.numeric(daten$pop_share)) / as.numeric(daten$pop_share) * 100))

daten$y_1 <- vector("numeric", nrow(daten))
daten$y_2 <- vector("numeric", nrow(daten))
daten$y_3 <- vector("numeric", nrow(daten))
daten$y_4 <- vector("numeric", nrow(daten))
daten$y_5 <- vector("numeric", nrow(daten))

#  absolute difference between expected and observed share
daten$y_1 <- as.numeric(as.numeric(daten$pop_share) - as.numeric(daten$z))
# same as percentage
daten$y_2 <- as.numeric(as.numeric(daten$pop_share) - as.numeric(daten$z))*100
# percentage difference between expected and actual share
daten$y_3 <- as.numeric(((as.numeric(daten$pop_share) - as.numeric(daten$z))/ daten$pop_share))
# absolute difference between the expected and actual share
daten$y_4 <- as.numeric(abs(as.numeric(daten$pop_share) - as.numeric(daten$z)))
# ratio of expected and observed share
daten$y_5 <- as.numeric(as.numeric(daten$z) / as.numeric(daten$pop_share))



#########################################################################
##############   aggregate and add independent variables   ##############
########################################################################

#### (1) trust_pol_parties qa6a_1
subset_eb$qa6a_1 <- replace(subset_eb$qa6a_1, subset_eb$qa6a_1 == 3, NA)
subset_eb$qa6a_1 <- replace(subset_eb$qa6a_1, subset_eb$qa6a_1 == 2, -1)
# aggregate to country-level data
eb_by_country <- aggregate(qa6a_1 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)

#### (2) trust_nat_legal qa6a_2
subset_eb$qa6a_2 <- replace(subset_eb$qa6a_2, subset_eb$qa6a_2 == 3, NA)
subset_eb$qa6a_2 <- replace(subset_eb$qa6a_2, subset_eb$qa6a_2 == 2, -1)
# aggregate to country-level data
eb_by_country <- aggregate(qa6a_2 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)

#### (3) trust_public_admin qa6a_5
subset_eb$qa6a_5 <- replace(subset_eb$qa6a_5, subset_eb$qa6a_5 == 3, NA)
subset_eb$qa6a_5 <- replace(subset_eb$qa6a_5, subset_eb$qa6a_5 == 2, -1)
# aggregate to country-level data
eb_by_country <- aggregate(qa6a_5 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


#### (4) trust_nat_gov: qa6a_8
subset_eb$qa6a_8 <- replace(subset_eb$qa6a_8, subset_eb$qa6a_8 == 3, NA)
subset_eb$qa6a_8 <- replace(subset_eb$qa6a_8, subset_eb$qa6a_8 == 2, -1)
# aggregate to country-level data
eb_by_country <- aggregate(qa6a_8 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


####(5) trust_nat_parl: qa6a_9
subset_eb$qa6a_9 <- replace(subset_eb$qa6a_9, subset_eb$qa6a_9 == 3, NA)
subset_eb$qa6a_9 <- replace(subset_eb$qa6a_9, subset_eb$qa6a_9 == 2, -1)
# aggregate to country-level data
eb_by_country <- aggregate(qa6a_9 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


#### (6) trust_eu: qa6a_10
subset_eb$qa6a_10 <- replace(subset_eb$qa6a_10, subset_eb$qa6a_10 == 3, NA)
subset_eb$qa6a_10 <- replace(subset_eb$qa6a_10, subset_eb$qa6a_10 == 2, -1)
# aggregate to country-level data
eb_by_country <- aggregate(qa6a_10 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


####(7) image_eu: d78
subset_eb$d78 <- replace(subset_eb$d78, subset_eb$d78 == 5, -2)
subset_eb$d78 <- replace(subset_eb$d78, subset_eb$d78 == 4, -1)
subset_eb$d78 <- replace(subset_eb$d78, subset_eb$d78 == 3, 0)
subset_eb$d78 <- replace(subset_eb$d78, subset_eb$d78 == 2, 11)
subset_eb$d78 <- replace(subset_eb$d78, subset_eb$d78 == 1, 2)
subset_eb$d78 <- replace(subset_eb$d78, subset_eb$d78 == 11, 1)
subset_eb$d78 <- replace(subset_eb$d78, subset_eb$d78 == 6, NA)
# aggregate to country-level data
eb_by_country <- aggregate(d78 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


#### (8) discuss_nat: d71_1
subset_eb$d71_1 <- replace(subset_eb$d71_1, subset_eb$d71_1 == 4, NA)
subset_eb$d71_1 <- replace(subset_eb$d71_1, subset_eb$d71_1 == 2, 0)
subset_eb$d71_1 <- replace(subset_eb$d71_1, subset_eb$d71_1 == 3, -1)
### aggregate to country-level data
eb_by_country <- aggregate(d71_1 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


#### (9) discuss_EU: d71_2
subset_eb$d71_2 <- replace(subset_eb$d71_2, subset_eb$d71_2 == 4, NA)
subset_eb$d71_2 <- replace(subset_eb$d71_2, subset_eb$d71_2 == 2, 0)
subset_eb$d71_2 <- replace(subset_eb$d71_2, subset_eb$d71_2 == 3, -1)
### aggregate to country-level data
eb_by_country <- aggregate(d71_2 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


##### (10) discuss_local: d71_3
subset_eb$d71_3 <- replace(subset_eb$d71_3, subset_eb$d71_3 == 4, NA)
subset_eb$d71_3 <- replace(subset_eb$d71_3, subset_eb$d71_3 == 2, 0)
subset_eb$d71_3 <- replace(subset_eb$d71_3, subset_eb$d71_3 == 3, -1)
### aggregate to country-level data
eb_by_country <- aggregate(d71_3 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)

#### (11) attach_EU: qd1a_3
subset_eb$qd1a_3 <- replace(subset_eb$qd1a_3, subset_eb$qd1a_3 == 5, NA)
subset_eb$qd1a_3 <- replace(subset_eb$qd1a_3, subset_eb$qd1a_3 == 4, -3)
subset_eb$qd1a_3 <- replace(subset_eb$qd1a_3, subset_eb$qd1a_3 == 3, -1)
subset_eb$qd1a_3 <- replace(subset_eb$qd1a_3, subset_eb$qd1a_3 == 1, 3)
subset_eb$qd1a_3 <- replace(subset_eb$qd1a_3, subset_eb$qd1a_3 == 2, 1)
### aggregate to country-level data
eb_by_country <- aggregate(qd1a_3 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


#### (12) citizen_EU: qd2_1
subset_eb$qd2_1 <- replace(subset_eb$qd2_1, subset_eb$qd2_1 == 5, NA)
subset_eb$qd2_1 <- replace(subset_eb$qd2_1, subset_eb$qd2_1 == 4, -3)
subset_eb$qd2_1 <- replace(subset_eb$qd2_1, subset_eb$qd2_1 == 3, -1)
subset_eb$qd2_1 <- replace(subset_eb$qd2_1, subset_eb$qd2_1 == 1, 3)
subset_eb$qd2_1 <- replace(subset_eb$qd2_1, subset_eb$qd2_1 == 2, 1)
### aggregate to country-level data
eb_by_country <- aggregate(qd2_1 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)



#### (13) know_right_EU: qd2_2
subset_eb$qd2_2 <- replace(subset_eb$qd2_2, subset_eb$qd2_2 == 5, NA)
subset_eb$qd2_2 <- replace(subset_eb$qd2_2, subset_eb$qd2_2 == 4, -3)
subset_eb$qd2_2 <- replace(subset_eb$qd2_2, subset_eb$qd2_2 == 3, -1)
subset_eb$qd2_2 <- replace(subset_eb$qd2_2, subset_eb$qd2_2 == 1, 3)
subset_eb$qd2_2 <- replace(subset_eb$qd2_2, subset_eb$qd2_2 == 2, 1)
### aggregate to country-level data
eb_by_country <- aggregate(qd2_2 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


####(14) internetuse_home: d62_1
subset_eb$d62_1 <- replace(subset_eb$d62_1, subset_eb$d62_1 == 7, NA)
### aggregate to country-level data
eb_by_country <- aggregate(d62_1 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)



### (15) es_future_outside: qa11_2
subset_eb$qa11_2 <- replace(subset_eb$qa11_2, subset_eb$qa11_2 == 5, NA)
subset_eb$qa11_2 <- replace(subset_eb$qa11_2, subset_eb$qa11_2 == 4, -3)
subset_eb$qa11_2 <- replace(subset_eb$qa11_2, subset_eb$qa11_2 == 3, -1)
subset_eb$qa11_2 <- replace(subset_eb$qa11_2, subset_eb$qa11_2 == 1, 3)
subset_eb$qa11_2 <- replace(subset_eb$qa11_2, subset_eb$qa11_2 == 2, 1)
### aggregate to country-level data
eb_by_country <- aggregate(qa11_2 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


# 16 trust_ep: qa10_1
subset_eb$qa10_1 <- replace(subset_eb$qa10_1, subset_eb$qa10_1 == 3, NA)
subset_eb$qa10_1 <- replace(subset_eb$qa10_1, subset_eb$qa10_1 == 2, -1)
### aggregate to country-level data
eb_by_country <- aggregate(qa10_1 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


# 17  trust_com: qa10_2
subset_eb$qa10_2 <- replace(subset_eb$qa10_2, subset_eb$qa10_2 == 3, NA)
subset_eb$qa10_2 <- replace(subset_eb$qa10_2, subset_eb$qa10_2 == 2, -1)
### aggregate to country-level data
eb_by_country <- aggregate(qa10_2 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


# 18  trust_ecb: qa10_3
subset_eb$qa10_3 <- replace(subset_eb$qa10_3, subset_eb$qa10_3 == 3, NA)
subset_eb$qa10_3 <- replace(subset_eb$qa10_3, subset_eb$qa10_3 == 2, -1)
### aggregate to country-level data
eb_by_country <- aggregate(qa10_3 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


# 19  trust_council: qa10_4
subset_eb$qa10_4 <- replace(subset_eb$qa10_4, subset_eb$qa10_4 == 3, NA)
subset_eb$qa10_4 <- replace(subset_eb$qa10_4, subset_eb$qa10_4 == 2, -1)
### aggregate to country-level data
eb_by_country <- aggregate(qa10_4 ~ isocntry, data = subset_eb, FUN = mean)
daten <- merge(daten, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, eb_by_country, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)

# short detour to test whether the trust in EU variables show collinearity

# Combine the variables into a data frame

eb_by_country_1 <- aggregate(qa10_1 ~ isocntry, data = subset_eb, FUN = mean)
eb_by_country_2 <- aggregate(qa10_2 ~ isocntry, data = subset_eb, FUN = mean)
eb_by_country_3 <- aggregate(qa10_3 ~ isocntry, data = subset_eb, FUN = mean)
eb_by_country_4 <- aggregate(qa10_4 ~ isocntry, data = subset_eb, FUN = mean)

# Merge data for factor analysis of eu institution variables
merged_data <- merge(merge(merge(eb_by_country_1, eb_by_country_2, by = "isocntry"), eb_by_country_3, by = "isocntry"), eb_by_country_4, by = "isocntry")

variables <- merged_data[, c("qa10_1", "qa10_2", "qa10_3", "qa10_4")]

# correlation plot of trust in EU institutions to find possible collinearity of IVs ####
cor_matrix <- cor(variables)
print(cor_matrix)

# Example correlation matrix
cor_matrix <- cor(variables)

# Create correlation plot
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")

# conduct a factor analysis of EU institutions variable ####
factor_analysis <- fa(variables)

# create a variable that summarizes the information for each of the highly correlated variables
factor_loadings <- factor_analysis$loadings
factor_score_MR1 <- as.matrix(variables) %*% factor_loadings[, "MR1"]

# Add the factor score to my datasets
merged_data$trust_EU_institutions <- as.numeric(factor_score_MR1)
merged_subset <- merged_data[c("isocntry", "trust_EU_institutions")]


daten <- merge(daten, merged_subset, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)
daten_country_level <- merge(daten_country_level, merged_subset, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)


#rename column header daten dataframe
daten <- daten %>% rename(
    internet_home = d62_1,
    citizen_feel= qd2_1,
    know_right_EU = qd2_2,
    attach_EU = qd1a_3,
    discuss_local = d71_3,
    discuss_national = d71_1,
    discuss_EU = d71_2,
    image_EU = d78,
    trust_EU = qa6a_10,
    trust_nat_parl= qa6a_9,
    trust_nat_gov= qa6a_8,
    trust_nat_legal= qa6a_2,
    trust_pol_part= qa6a_1,
    trust_public_adm = qa6a_5,
    es_future_outside = qa11_2,
    trust_ep = qa10_1,
    trust_com = qa10_2,
    trust_ecb = qa10_3,
    trust_council = qa10_4
  )

#rename column header daten dataframe
daten_country_level <- daten_country_level %>% rename(
  internet_home = d62_1,
  citizen_feel= qd2_1,
  know_right_EU = qd2_2,
  attach_EU = qd1a_3,
  discuss_local = d71_3,
  discuss_national = d71_1,
  discuss_EU = d71_2,
  image_EU = d78,
  trust_EU = qa6a_10,
  trust_nat_parl= qa6a_9,
  trust_nat_gov= qa6a_8,
  trust_nat_legal= qa6a_2,
  trust_pol_part= qa6a_1,
  trust_public_adm = qa6a_5,
  es_future_outside = qa11_2,
  trust_ep = qa10_1,
  trust_com = qa10_2,
  trust_ecb = qa10_3,
  trust_council = qa10_4
)


#############################
########## Regression Analysis
#########################
#########################
#########################
#########################

# descriptive statistics and correlation matrix ####

# correlation plot of all IVs to find possible collinearity of IVs ####
all_variables <- daten_country_level[, 10:28]

# correlation matrix
cor_matrix <- cor(all_variables)

# Create correlation plot
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")


######   regression models  ######################

# with countries as the unit level ####

# check descriptive statistics of dependent variable
summary(daten_country_level$y)
hist(daten_country_level$y)
plot(daten_country_level$y)


# model 1: alle v
model_1 <- lm(y ~  active_citizen + 
                trust_pol_part + 
                trust_nat_legal + 
                trust_public_adm + 
                trust_nat_gov + 
                trust_nat_parl + 
                trust_EU + 
                trust_ecb + 
                trust_council + 
                trust_ep + 
                trust_com + 
                image_EU + 
                discuss_national + 
                discuss_EU + 
                discuss_local + 
                attach_EU + 
                citizen_feel + 
                know_right_EU + 
                internet_home + 
                es_future_outside, 
              data = daten_country_level)
summary(model_1)

View(daten_country_level)

# # model 2: drop 4 EU trust variables and include factor loaded trust_EU_institutions
model_2 <- lm(y ~  active_citizen + 
                 trust_pol_part + 
                 trust_nat_legal + 
                 trust_public_adm + 
                 trust_nat_gov + 
                 trust_nat_parl + 
                 trust_EU +
                trust_EU_institutions +
                 image_EU + 
                 discuss_national + 
                 discuss_EU + 
                 discuss_local + 
                 attach_EU + 
                 citizen_feel + 
                 know_right_EU + 
                 internet_home + 
                 es_future_outside, 
               data = daten_country_level)
summary(model_2)

# hack the R² of model_1
step_model_1 <- stepAIC(model_1, direction = "both", trace = FALSE)
summary(step_model_1)

# hack the R² of model model_2
step_model_2 <- stepAIC(model_2, direction = "both", trace = FALSE)
summary(step_model_2)

# model 4: dropping some variables according to the stepAIC calculation
model_4 <- lm(y ~  active_citizen + 
                trust_pol_part + 
                trust_nat_legal + 
                trust_nat_gov + 
                trust_nat_parl + 
                trust_EU + 
                trust_ecb + 
                trust_ep + 
                trust_com + 
                image_EU + 
                discuss_national + 
                discuss_EU + 
                discuss_local + 
                citizen_feel + 
                know_right_EU + 
                internet_home + 
                es_future_outside, 
              data = daten_country_level)
summary(model_4)


# with legislative proposals as the the unit level ####

# check descriptive statistics of dependent variable
summary(daten$y)
hist(daten$y)
plot(daten$y)
# log transformation after shifting to positive values 
daten$y_log <- daten$y + 101
daten$y_log <- log(daten$y_log)

# check descriptive statistics of dependent variable after log transformation
summary(daten$y_log)
hist(daten$y_log)

## note to self: great normal distribution now, but deal with the zero response rows...

# model 101: interaktion trust_ep and trust_pol_part
model_101 <- lm(y_log ~  trust_pol_part*trust_com,  data = daten)
summary(model_101)

# model 102: alle v
model_102 <- lm(y_log ~  active_citizen + trust_pol_part + trust_nat_legal + trust_public_adm + trust_nat_gov + trust_nat_parl + trust_EU + trust_ep + trust_council + trust_ecb + trust_com + image_EU + discuss_national + discuss_EU + discuss_local + attach_EU + citizen_feel + know_right_EU + internet_home + es_future_outside, data = daten)
summary(model_102)

# model 103: only with countries
model_103 <- lm(y_log ~ isocntry, data = daten)
summary(model_103)

# optimize for R² with stepAIC
step_model_102 <- stepAIC(model_102, direction = "both", trace = FALSE)
summary(step_model_102)


## another check for multicollinearity

# measuring the VIF ####



