# libraries and setwd ####
library(haven)
library(tidyverse)
library(tidyr)
library(writexl)


setwd("C:/Users/JohnDoe/Desktop/Master/R")

# # load all ebs and input data ####
# eb_04_original <- read_dta("2004_ZA4229_v1-1-0.dta")
# eb_09_original <- read_dta("2009_ZA4973_v3-0-0.dta")
# eb_14_original <- read_dta("2014_ZA5928_v3-0-0.dta")
# eb_19_original <- read_dta("2019_ZA7576_v1-0-0.dta")
# 
# # load two additional ebs for missing data
# eb_09a_original <- read_dta("2009a_ZA4971_v4-0-0.dta")
# eb_14a_original <- read_dta("2013_ZA5875_v2-1-0.dta")


# assign them to the dfs i use in the further coding ####

eb_04 <- eb_04_original
eb_09 <- eb_09_original
eb_14 <- eb_14_original
eb_19 <- eb_19_original
eb_09a <- eb_09a_original
eb_14a <- eb_14a_original

# load the voter turnout excel ####
turnout <- readxl::read_xlsx("panel_eb.xlsx", col_names = TRUE)

# preliminary data manipulation #### 

# just have a vector with all EU-25 

EU25 <- c("AT", "BE", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR",
                   "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "SI", "SK", "SE", "UK")


# subset all eb to only contain EU-25 member states 

cntry <- c("AT", "BE", "CY", "CZ", "DE-W", "DE-E", "DK", "EE", "ES", "FI", "FR", "GR",
           "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "SI", "SK", "SE", "GB-GBN", "GB-NIR")

## rename column headers to unify header names
names(eb_04)[which(names(eb_04) == "v7")] <- "isocntry"
names(eb_09)[which(names(eb_09) == "v7")] <- "isocntry"
names(eb_09a)[which(names(eb_09a) == "v7")] <- "isocntry"


## merge DE-W and DE-E to DE
eb_04 <- eb_04 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
eb_09 <- eb_09 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
eb_14 <- eb_14 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
eb_19 <- eb_19 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
eb_09a <- eb_09a %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
eb_14a <- eb_14a %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))


## merge GB-GBN and GB-NIR to UK
eb_04 <- eb_04 %>% mutate(isocntry=recode(isocntry, "GB-GBN"="UK", "GB-NIR"="UK"))
eb_09 <- eb_09 %>% mutate(isocntry=recode(isocntry, "GB-GBN"="UK", "GB-NIR"="UK"))
eb_14 <- eb_14 %>% mutate(isocntry=recode(isocntry, "GB-GBN"="UK", "GB-NIR"="UK"))
eb_19 <- eb_19 %>% mutate(isocntry=recode(isocntry, "GB"="UK"))
eb_09a <- eb_09a %>% mutate(isocntry=recode(isocntry, "GB-GBN"="UK", "GB-NIR"="UK"))
eb_14a <- eb_14a %>% mutate(isocntry=recode(isocntry, "GB-GBN"="UK", "GB-NIR"="UK"))


## Subset the eb datasets based on the country codes
eb_04 <- eb_04[eb_04$isocntry %in% EU25, ]
eb_09 <- eb_09[eb_09$isocntry %in% EU25, ]
eb_14 <- eb_14[eb_14$isocntry %in% EU25, ]
eb_19 <- eb_19[eb_19$isocntry %in% EU25, ]
eb_09a <- eb_09a[eb_09a$isocntry %in% EU25, ]
eb_14a <- eb_14a[eb_14a$isocntry %in% EU25, ]



# # check countries
# unique(eb_04$isocntry)
# unique(eb_09$isocntry)
# unique(eb_14$isocntry)
# unique(eb_19$isocntry)
# unique(eb_09a$isocntry)
# unique(eb_14a$isocntry)

# initialise dataframes with the wanted eb variables ####
eb_04_input <- data.frame(year = rep(2004, length(EU25)),
                           isocntry = EU25,
                           discuss_pol = rep(0, length(EU25)),
                           trust_pol_part = rep(0, length(EU25)),
                           trust_nat_gov = rep(0, length(EU25)),
                           trust_nat_parl = rep(0, length(EU25)),
                           trust_EU = rep(0, length(EU25)),
                           trust_EP = rep(0, length(EU25)),
                           trust_COM = rep(0, length(EU25)),
                           trust_ECB = rep(0, length(EU25)),
                           es_member_general = rep(0, length(EU25)),
                           es_member_benefit = rep(0, length(EU25)),
                           eu_image = rep(0, length(EU25)),
                           stringsAsFactors = FALSE)

eb_09_input <- data.frame(year = rep(2009, length(EU25)),
                           isocntry = EU25,
                           discuss_pol = rep(0, length(EU25)),
                           trust_pol_part = rep(0, length(EU25)),
                           trust_nat_gov = rep(0, length(EU25)),
                           trust_nat_parl = rep(0, length(EU25)),
                           trust_EU = rep(0, length(EU25)),
                           trust_EP = rep(0, length(EU25)),
                           trust_COM = rep(0, length(EU25)),
                           trust_ECB = rep(0, length(EU25)),
                           es_member_general = rep(0, length(EU25)),
                           es_member_benefit = rep(0, length(EU25)),
                           eu_image = rep(0, length(EU25)),
                           stringsAsFactors = FALSE)

eb_14_input <- data.frame(year = rep(2014, length(EU25)),
                           isocntry = EU25,
                           discuss_pol = rep(0, length(EU25)),
                           trust_pol_part = rep(0, length(EU25)),
                           trust_nat_gov = rep(0, length(EU25)),
                           trust_nat_parl = rep(0, length(EU25)),
                           trust_EU = rep(0, length(EU25)),
                           trust_EP = rep(0, length(EU25)),
                           trust_COM = rep(0, length(EU25)),
                           trust_ECB = rep(0, length(EU25)),
                           es_member_general = rep(0, length(EU25)),
                           es_member_benefit = rep(0, length(EU25)),
                           eu_image = rep(0, length(EU25)),
                           stringsAsFactors = FALSE)

eb_19_input <- data.frame(year = rep(2019, length(EU25)),
                           isocntry = EU25,
                           discuss_pol = rep(0, length(EU25)),
                           trust_pol_part = rep(0, length(EU25)),
                           trust_nat_gov = rep(0, length(EU25)),
                           trust_nat_parl = rep(0, length(EU25)),
                           trust_EU = rep(0, length(EU25)),
                           trust_EP = rep(0, length(EU25)),
                           trust_COM = rep(0, length(EU25)),
                           trust_ECB = rep(0, length(EU25)),
                           es_member_general = rep(0, length(EU25)),
                           es_member_benefit = rep(0, length(EU25)),
                           eu_image = rep(0, length(EU25)),
                           stringsAsFactors = FALSE)

eb_09a_input <- data.frame(year = rep(2009, length(EU25)),
                           isocntry = EU25,
                           discuss_pol = rep(0, length(EU25)),
                           trust_pol_part = rep(0, length(EU25)),
                           trust_nat_gov = rep(0, length(EU25)),
                           trust_nat_parl = rep(0, length(EU25)),
                           trust_EU = rep(0, length(EU25)),
                           trust_EP = rep(0, length(EU25)),
                           trust_COM = rep(0, length(EU25)),
                           trust_ECB = rep(0, length(EU25)),
                           es_member_general = rep(0, length(EU25)),
                           es_member_benefit = rep(0, length(EU25)),
                           eu_image = rep(0, length(EU25)),
                           stringsAsFactors = FALSE)

eb_14a_input <- data.frame(year = rep(2014, length(EU25)),
                          isocntry = EU25,
                          discuss_pol = rep(0, length(EU25)),
                          trust_pol_part = rep(0, length(EU25)),
                          trust_nat_gov = rep(0, length(EU25)),
                          trust_nat_parl = rep(0, length(EU25)),
                          trust_EU = rep(0, length(EU25)),
                          trust_EP = rep(0, length(EU25)),
                          trust_COM = rep(0, length(EU25)),
                          trust_ECB = rep(0, length(EU25)),
                          es_member_general = rep(0, length(EU25)),
                          es_member_benefit = rep(0, length(EU25)),
                          eu_image = rep(0, length(EU25)),
                          stringsAsFactors = FALSE)


# eb_04 variables by country #####

#### (1) trust_pol_part
eb_04$v91 <- replace(eb_04$v91, eb_04$v91 == 3, NA)
eb_04$v91 <- replace(eb_04$v91, eb_04$v91 == 2, -1)
names(eb_04)[which(names(eb_04) == "v91")] <- "trust_pol_part"
# aggregate to country-level data
eb_04_input <- aggregate(trust_pol_part ~ isocntry, data = eb_04, FUN = mean)
eb_04_input$year  <- 2004

#### (2) trust_nat_gov
eb_04$v93 <- replace(eb_04$v93, eb_04$v93 == 3, NA)
eb_04$v93 <- replace(eb_04$v93, eb_04$v93 == 2, -1)
names(eb_04)[which(names(eb_04) == "v93")] <- "trust_nat_gov"
# aggregate to country-level data
eb_04_input$trust_nat_gov <- aggregate(trust_nat_gov ~ isocntry, data = eb_04, FUN = mean)$trust_nat_gov

#### (3) trust_nat_parl
eb_04$v94 <- replace(eb_04$v94, eb_04$v94 == 3, NA)
eb_04$v94 <- replace(eb_04$v94, eb_04$v94 == 2, -1)
names(eb_04)[which(names(eb_04) == "v94")] <- "trust_nat_parl"
# aggregate to country-level data
eb_04_input$trust_nat_parl <- aggregate(trust_nat_parl ~ isocntry, data = eb_04, FUN = mean)$trust_nat_parl

#### (4) trust_EU
eb_04$v95 <- replace(eb_04$v95, eb_04$v95 == 3, NA)
eb_04$v95 <- replace(eb_04$v95, eb_04$v95 == 2, -1)
names(eb_04)[which(names(eb_04) == "v95")] <- "trust_EU"
# aggregate to country-level data
eb_04_input$trust_EU <- aggregate(trust_EU ~ isocntry, data = eb_04, FUN = mean)$trust_EU


#### (5) trust_EP
eb_04$v202 <- replace(eb_04$v202, eb_04$v202 == 3, NA)
eb_04$v202 <- replace(eb_04$v202, eb_04$v202 == 2, -1)
names(eb_04)[which(names(eb_04) == "v202")] <- "trust_EP"
# aggregate to country-level data
eb_04_input$trust_EP <- aggregate(trust_EP ~ isocntry, data = eb_04, FUN = mean)$trust_EP

#### (6) trust_COM
eb_04$v203 <- replace(eb_04$v203, eb_04$v203 == 3, NA)
eb_04$v203 <- replace(eb_04$v203, eb_04$v203 == 2, -1)
names(eb_04)[which(names(eb_04) == "v203")] <- "trust_COM"
# aggregate to country-level data
eb_04_input$trust_COM <- aggregate(trust_COM ~ isocntry, data = eb_04, FUN = mean)$trust_COM

#### (7) trust_ECB
eb_04$v207 <- replace(eb_04$v207, eb_04$v207 == 3, NA)
eb_04$v207 <- replace(eb_04$v207, eb_04$v207 == 2, -1)
names(eb_04)[which(names(eb_04) == "v207")] <- "trust_ECB"
# aggregate to country-level data
eb_04_input$trust_ECB <- aggregate(trust_ECB ~ isocntry, data = eb_04, FUN = mean)$trust_ECB

#### (8) discuss_pol
eb_04$v67 <- replace(eb_04$v67, eb_04$v67 == 4, NA)
eb_04$v67 <- replace(eb_04$v67, eb_04$v67 == 3, -1)
eb_04$v67 <- replace(eb_04$v67, eb_04$v67 == 2, 0)
names(eb_04)[which(names(eb_04) == "v67")] <- "discuss_pol"
# aggregate to country-level data
eb_04_input$discuss_pol <- aggregate(discuss_pol ~ isocntry, data = eb_04, FUN = mean)$discuss_pol

#### (9) member_general
eb_04$v98 <- replace(eb_04$v98, eb_04$v98 == 4, NA)
eb_04$v98 <- replace(eb_04$v98, eb_04$v98 == 3, -1)
eb_04$v98 <- replace(eb_04$v98, eb_04$v98 == 2, 0)
names(eb_04)[which(names(eb_04) == "v98")] <- "member_general"
# aggregate to country-level data
eb_04_input$member_general <- aggregate(member_general ~ isocntry, data = eb_04, FUN = mean)$member_general


#### (10) member_general
eb_04$v99 <- replace(eb_04$v99, eb_04$v99 == 3, NA)
eb_04$v99 <- replace(eb_04$v99, eb_04$v99 == 2, -1)
names(eb_04)[which(names(eb_04) == "v99")] <- "member_benefit"
# aggregate to country-level data
eb_04_input$member_benefit <- aggregate(member_benefit ~ isocntry, data = eb_04, FUN = mean)$member_benefit

#### (11) image_EU
eb_04$v102 <- replace(eb_04$v102, eb_04$v102 == 6, NA)
eb_04$v102 <- replace(eb_04$v102, eb_04$v102 == 5, -2)
eb_04$v102 <- replace(eb_04$v102, eb_04$v102 == 4, -1)
eb_04$v102 <- replace(eb_04$v102, eb_04$v102 == 3, 0)
eb_04$v102 <- replace(eb_04$v102, eb_04$v102 == 1, 11)
eb_04$v102 <- replace(eb_04$v102, eb_04$v102 == 2, 1)
eb_04$v102 <- replace(eb_04$v102, eb_04$v102 == 11, 2)
names(eb_04)[which(names(eb_04) == "v102")] <- "image_EU"
# aggregate to country-level data
eb_04_input$image_EU <- aggregate(image_EU ~ isocntry, data = eb_04, FUN = mean)$image_EU

# eb_09 variables by country  #####

#### (1) trust_pol_part
eb_09$v216 <- replace(eb_09$v216, eb_09$v216 == 3, NA)
eb_09$v216 <- replace(eb_09$v216, eb_09$v216 == 2, -1)
names(eb_09)[which(names(eb_09) == "v216")] <- "trust_pol_part"
# aggregate to country-level data
eb_09_input <- aggregate(trust_pol_part ~ isocntry, data = eb_09, FUN = mean)
eb_09_input$year  <- 2009

#### (2) trust_nat_gov
eb_09$v217 <- replace(eb_09$v217, eb_09$v217 == 3, NA)
eb_09$v217 <- replace(eb_09$v217, eb_09$v217 == 2, -1)
names(eb_09)[which(names(eb_09) == "v217")] <- "trust_nat_gov"
# aggregate to country-level data
eb_09_input$trust_nat_gov <- aggregate(trust_nat_gov ~ isocntry, data = eb_09, FUN = mean)$trust_nat_gov

#### (3) trust_nat_parl
eb_09$v218 <- replace(eb_09$v218, eb_09$v218 == 3, NA)
eb_09$v218 <- replace(eb_09$v218, eb_09$v218 == 2, -1)
names(eb_09)[which(names(eb_09) == "v218")] <- "trust_nat_parl"
# aggregate to country-level data
eb_09_input$trust_nat_parl <- aggregate(trust_nat_parl ~ isocntry, data = eb_09, FUN = mean)$trust_nat_parl

#### (4) trust_EU
eb_09$v219 <- replace(eb_09$v219, eb_09$v219 == 3, NA)
eb_09$v219 <- replace(eb_09$v219, eb_09$v219 == 2, -1)
names(eb_09)[which(names(eb_09) == "v219")] <- "trust_EU"
# aggregate to country-level data
eb_09_input$trust_EU <- aggregate(trust_EU ~ isocntry, data = eb_09, FUN = mean)$trust_EU


#### (5) trust_EP
eb_09$v252 <- replace(eb_09$v252, eb_09$v252 == 3, NA)
eb_09$v252 <- replace(eb_09$v252, eb_09$v252 == 2, -1)
names(eb_09)[which(names(eb_09) == "v252")] <- "trust_EP"
# aggregate to country-level data
eb_09_input$trust_EP <- aggregate(trust_EP ~ isocntry, data = eb_09, FUN = mean)$trust_EP

#### (6) trust_COM
eb_09$v253 <- replace(eb_09$v253, eb_09$v253 == 3, NA)
eb_09$v253 <- replace(eb_09$v253, eb_09$v253 == 2, -1)
names(eb_09)[which(names(eb_09) == "v253")] <- "trust_COM"
# aggregate to country-level data
eb_09_input$trust_COM <- aggregate(trust_COM ~ isocntry, data = eb_09, FUN = mean)$trust_COM

#### (7) trust_ECB
eb_09$v254 <- replace(eb_09$v254, eb_09$v254 == 3, NA)
eb_09$v254 <- replace(eb_09$v254, eb_09$v254 == 2, -1)
names(eb_09)[which(names(eb_09) == "v254")] <- "trust_ECB"
# aggregate to country-level data
eb_09_input$trust_ECB <- aggregate(trust_ECB ~ isocntry, data = eb_09, FUN = mean)$trust_ECB

#### (8) discuss_pol
eb_09a$v87 <- replace(eb_09a$v87, eb_09a$v87 == 4, NA)
eb_09a$v87 <- replace(eb_09a$v87, eb_09a$v87 == 3, -1)
eb_09a$v87 <- replace(eb_09a$v87, eb_09a$v87 == 2, 0)
names(eb_09a)[which(names(eb_09a) == "v87")] <- "discuss_pol"
# aggregate to country-level data
eb_09_input$discuss_pol <- aggregate(discuss_pol ~ isocntry, data = eb_09a, FUN = mean)$discuss_pol

#### (9) member_general
eb_09$v205 <- replace(eb_09$v205, eb_09$v205 == 4, NA)
eb_09$v205 <- replace(eb_09$v205, eb_09$v205 == 3, -1)
eb_09$v205 <- replace(eb_09$v205, eb_09$v205 == 2, 0)
names(eb_09)[which(names(eb_09) == "v205")] <- "member_general"
# aggregate to country-level data
eb_09_input$member_general <- aggregate(member_general ~ isocntry, data = eb_09, FUN = mean)$member_general


#### (10) member_benefit
eb_09$v206 <- replace(eb_09$v206, eb_09$v206 == 3, NA)
eb_09$v206 <- replace(eb_09$v206, eb_09$v206 == 2, -1)
names(eb_09)[which(names(eb_09) == "v206")] <- "member_benefit"
# aggregate to country-level data
eb_09_input$member_benefit <- aggregate(member_benefit ~ isocntry, data = eb_09, FUN = mean)$member_benefit

#### (11) image_EU
eb_09$v221 <- replace(eb_09$v221, eb_09$v221 == 6, NA)
eb_09$v221 <- replace(eb_09$v221, eb_09$v221 == 5, -2)
eb_09$v221 <- replace(eb_09$v221, eb_09$v221 == 4, -1)
eb_09$v221 <- replace(eb_09$v221, eb_09$v221 == 3, 0)
eb_09$v221 <- replace(eb_09$v221, eb_09$v221 == 1, 11)
eb_09$v221 <- replace(eb_09$v221, eb_09$v221 == 2, 1)
eb_09$v221 <- replace(eb_09$v221, eb_09$v221 == 11, 2)
names(eb_09)[which(names(eb_09) == "v221")] <- "image_EU"
# aggregate to country-level data
eb_09_input$image_EU <- aggregate(image_EU ~ isocntry, data = eb_09, FUN = mean)$image_EU


# eb_14 variables by country ######


#### (1) trust_pol_part
eb_14$qa9_1 <- replace(eb_14$qa9_1, eb_14$qa9_1 == 3, NA)
eb_14$qa9_1 <- replace(eb_14$qa9_1, eb_14$qa9_1 == 2, -1)
names(eb_14)[which(names(eb_14) == "qa9_1")] <- "trust_pol_part"
# aggregate to country-level data
eb_14_input <- aggregate(trust_pol_part ~ isocntry, data = eb_14, FUN = mean)
eb_14_input$year  <- 2014

#### (2) trust_nat_gov
eb_14$qa9_2 <- replace(eb_14$qa9_2, eb_14$qa9_2 == 3, NA)
eb_14$qa9_2 <- replace(eb_14$qa9_2, eb_14$qa9_2 == 2, -1)
names(eb_14)[which(names(eb_14) == "qa9_2")] <- "trust_nat_gov"
# aggregate to country-level data
eb_14_input$trust_nat_gov <- aggregate(trust_nat_gov ~ isocntry, data = eb_14, FUN = mean)$trust_nat_gov

#### (3) trust_nat_parl
eb_14$qa9_3 <- replace(eb_14$qa9_3, eb_14$qa9_3 == 3, NA)
eb_14$qa9_3 <- replace(eb_14$qa9_3, eb_14$qa9_3 == 2, -1)
names(eb_14)[which(names(eb_14) == "qa9_3")] <- "trust_nat_parl"
# aggregate to country-level data
eb_14_input$trust_nat_parl <- aggregate(trust_nat_parl ~ isocntry, data = eb_14, FUN = mean)$trust_nat_parl

#### (4) trust_EU
eb_14$qa9_4 <- replace(eb_14$qa9_4, eb_14$qa9_4 == 3, NA)
eb_14$qa9_4 <- replace(eb_14$qa9_4, eb_14$qa9_4 == 2, -1)
names(eb_14)[which(names(eb_14) == "qa9_4")] <- "trust_EU"
# aggregate to country-level data
eb_14_input$trust_EU <- aggregate(trust_EU ~ isocntry, data = eb_14, FUN = mean)$trust_EU


#### (5) trust_EP
eb_14$qa15_1 <- replace(eb_14$qa15_1, eb_14$qa15_1 == 3, NA)
eb_14$qa15_1 <- replace(eb_14$qa15_1, eb_14$qa15_1 == 2, -1)
names(eb_14)[which(names(eb_14) == "qa15_1")] <- "trust_EP"
# aggregate to country-level data
eb_14_input$trust_EP <- aggregate(trust_EP ~ isocntry, data = eb_14, FUN = mean)$trust_EP

#### (6) trust_COM
eb_14$qa15_2 <- replace(eb_14$qa15_2, eb_14$qa15_2 == 3, NA)
eb_14$qa15_2 <- replace(eb_14$qa15_2, eb_14$qa15_2 == 2, -1)
names(eb_14)[which(names(eb_14) == "qa15_2")] <- "trust_COM"
# aggregate to country-level data
eb_14_input$trust_COM <- aggregate(trust_COM ~ isocntry, data = eb_14, FUN = mean)$trust_COM

#### (7) trust_ECB
eb_14$qa15_3 <- replace(eb_14$qa15_3, eb_14$qa15_3 == 3, NA)
eb_14$qa15_3 <- replace(eb_14$qa15_3, eb_14$qa15_3 == 2, -1)
names(eb_14)[which(names(eb_14) == "qa15_3")] <- "trust_ECB"
# aggregate to country-level data
eb_14_input$trust_ECB <- aggregate(trust_ECB ~ isocntry, data = eb_14, FUN = mean)$trust_ECB

#### (8) discuss_pol
eb_14$d71a_1 <- replace(eb_14$d71a_1, eb_14$d71a_1 == 4, NA)
eb_14$d71a_1 <- replace(eb_14$d71a_1, eb_14$d71a_1 == 3, -1)
eb_14$d71a_1 <- replace(eb_14$d71a_1, eb_14$d71a_1 == 2, 0)
names(eb_14)[which(names(eb_14) == "d71a_1")] <- "discuss_pol"
# aggregate to country-level data
eb_14_input$discuss_pol <- aggregate(discuss_pol ~ isocntry, data = eb_14, FUN = mean)$discuss_pol

#### (9) member_general
eb_14a$qp22 <- replace(eb_14a$qp22, eb_14a$qp22 == 4, NA)
eb_14a$qp22 <- replace(eb_14a$qp22, eb_14a$qp22 == 2, -1)
eb_14a$qp22 <- replace(eb_14a$qp22, eb_14a$qp22 == 3, 0)
names(eb_14a)[which(names(eb_14a) == "qp22")] <- "member_general"
# aggregate to country-level data
eb_14_input$member_general <- aggregate(member_general ~ isocntry, data = eb_14a, FUN = mean)$member_general


#### (10) member_benefit
eb_14a$qp23 <- replace(eb_14a$qp23, eb_14a$qp23 == 3, NA)
eb_14a$qp23 <- replace(eb_14a$qp23, eb_14a$qp23 == 2, -1)
names(eb_14a)[which(names(eb_14a) == "qp23")] <- "member_benefit"
# aggregate to country-level data
eb_14_input$member_benefit <- aggregate(member_benefit ~ isocntry, data = eb_14a, FUN = mean)$member_benefit

#### (11) image_EU
eb_14$qa10 <- replace(eb_14$qa10, eb_14$qa10 == 6, NA)
eb_14$qa10 <- replace(eb_14$qa10, eb_14$qa10 == 5, -2)
eb_14$qa10 <- replace(eb_14$qa10, eb_14$qa10 == 4, -1)
eb_14$qa10 <- replace(eb_14$qa10, eb_14$qa10 == 3, 0)
eb_14$qa10 <- replace(eb_14$qa10, eb_14$qa10 == 1, 11)
eb_14$qa10 <- replace(eb_14$qa10, eb_14$qa10 == 2, 1)
eb_14$qa10 <- replace(eb_14$qa10, eb_14$qa10 == 11, 2)
names(eb_14)[which(names(eb_14) == "qa10")] <- "image_EU"
# aggregate to country-level data
eb_14_input$image_EU <- aggregate(image_EU ~ isocntry, data = eb_14, FUN = mean)$image_EU

# eb_19 variables by country #####

#### (1) trust_pol_part
eb_19$qa6a_2 <- replace(eb_19$qa6a_2, eb_19$qa6a_2 == 3, NA)
eb_19$qa6a_2 <- replace(eb_19$qa6a_2, eb_19$qa6a_2 == 2, -1)
names(eb_19)[which(names(eb_19) == "qa6a_2")] <- "trust_pol_part"
# aggregate to country-level data
eb_19_input <- aggregate(trust_pol_part ~ isocntry, data = eb_19, FUN = mean)
eb_19_input$year  <- 2019

#### (2) trust_nat_gov
eb_19$qa6a_8 <- replace(eb_19$qa6a_8, eb_19$qa6a_8 == 3, NA)
eb_19$qa6a_8 <- replace(eb_19$qa6a_8, eb_19$qa6a_8 == 2, -1)
names(eb_19)[which(names(eb_19) == "qa6a_8")] <- "trust_nat_gov"
# aggregate to country-level data
eb_19_input$trust_nat_gov <- aggregate(trust_nat_gov ~ isocntry, data = eb_19, FUN = mean)$trust_nat_gov

#### (3) trust_nat_parl
eb_19$qa6a_9 <- replace(eb_19$qa6a_9, eb_19$qa6a_9 == 3, NA)
eb_19$qa6a_9 <- replace(eb_19$qa6a_9, eb_19$qa6a_9 == 2, -1)
names(eb_19)[which(names(eb_19) == "qa6a_9")] <- "trust_nat_parl"
# aggregate to country-level data
eb_19_input$trust_nat_parl <- aggregate(trust_nat_parl ~ isocntry, data = eb_19, FUN = mean)$trust_nat_parl

#### (4) trust_EU
eb_19$qa6a_10 <- replace(eb_19$qa6a_10, eb_19$qa6a_10 == 3, NA)
eb_19$qa6a_10 <- replace(eb_19$qa6a_10, eb_19$qa6a_10 == 2, -1)
names(eb_19)[which(names(eb_19) == "qa6a_10")] <- "trust_EU"
# aggregate to country-level data
eb_19_input$trust_EU <- aggregate(trust_EU ~ isocntry, data = eb_19, FUN = mean)$trust_EU


#### (5) trust_EP
eb_19$qa14_1 <- replace(eb_19$qa14_1, eb_19$qa14_1 == 3, NA)
eb_19$qa14_1 <- replace(eb_19$qa14_1, eb_19$qa14_1 == 2, -1)
names(eb_19)[which(names(eb_19) == "qa14_1")] <- "trust_EP"
# aggregate to country-level data
eb_19_input$trust_EP <- aggregate(trust_EP ~ isocntry, data = eb_19, FUN = mean)$trust_EP

#### (6) trust_COM
eb_19$qa14_2 <- replace(eb_19$qa14_2, eb_19$qa14_2 == 3, NA)
eb_19$qa14_2 <- replace(eb_19$qa14_2, eb_19$qa14_2 == 2, -1)
names(eb_19)[which(names(eb_19) == "qa14_2")] <- "trust_COM"
# aggregate to country-level data
eb_19_input$trust_COM <- aggregate(trust_COM ~ isocntry, data = eb_19, FUN = mean)$trust_COM

#### (7) trust_ECB
eb_19$qa14_3 <- replace(eb_19$qa14_3, eb_19$qa14_3 == 3, NA)
eb_19$qa14_3 <- replace(eb_19$qa14_3, eb_19$qa14_3 == 2, -1)
names(eb_19)[which(names(eb_19) == "qa14_3")] <- "trust_ECB"
# aggregate to country-level data
eb_19_input$trust_ECB <- aggregate(trust_ECB ~ isocntry, data = eb_19, FUN = mean)$trust_ECB

#### (8) discuss_pol
eb_19$d71a_1 <- replace(eb_19$d71a_1, eb_19$d71a_1 == 4, NA)
eb_19$d71a_1 <- replace(eb_19$d71a_1, eb_19$d71a_1 == 3, -1)
eb_19$d71a_1 <- replace(eb_19$d71a_1, eb_19$d71a_1 == 2, 0)
names(eb_19)[which(names(eb_19) == "d71a_1")] <- "discuss_pol"
# aggregate to country-level data
eb_19_input$discuss_pol <- aggregate(discuss_pol ~ isocntry, data = eb_19, FUN = mean)$discuss_pol

#### (9) member_general
eb_19$qf1 <- replace(eb_19$qf1, eb_19$qf1 == 4, NA)
eb_19$qf1 <- replace(eb_19$qf1, eb_19$qf1 == 2, -1)
eb_19$qf1 <- replace(eb_19$qf1, eb_19$qf1 == 3, 0)
names(eb_19)[which(names(eb_19) == "qf1")] <- "member_general"
# aggregate to country-level data
eb_19_input$member_general <- aggregate(member_general ~ isocntry, data = eb_19, FUN = mean)$member_general


#### (10) member_benefit
eb_19$qf2 <- replace(eb_19$qf2, eb_19$qf2 == 3, NA)
eb_19$qf2 <- replace(eb_19$qf2, eb_19$qf2 == 2, -1)
names(eb_19)[which(names(eb_19) == "qf2")] <- "member_benefit"
# aggregate to country-level data
eb_19_input$member_benefit <- aggregate(member_benefit ~ isocntry, data = eb_19, FUN = mean)$member_benefit

#### (11) image_EU
eb_19$qa7 <- replace(eb_19$qa7, eb_19$qa7 == 6, NA)
eb_19$qa7 <- replace(eb_19$qa7, eb_19$qa7 == 5, -2)
eb_19$qa7 <- replace(eb_19$qa7, eb_19$qa7 == 4, -1)
eb_19$qa7 <- replace(eb_19$qa7, eb_19$qa7 == 3, 0)
eb_19$qa7 <- replace(eb_19$qa7, eb_19$qa7 == 1, 11)
eb_19$qa7 <- replace(eb_19$qa7, eb_19$qa7 == 2, 1)
eb_19$qa7 <- replace(eb_19$qa7, eb_19$qa7 == 11, 2)
names(eb_19)[which(names(eb_19) == "qa7")] <- "image_EU"
# aggregate to country-level data
eb_19_input$image_EU <- aggregate(image_EU ~ isocntry, data = eb_19, FUN = mean)$image_EU



# merging turnout df with the eb by country dfs ####

turnout <- merge(turnout, eb_04_input, by = c("year", "isocntry"), all.x = TRUE)
turnout <- merge(turnout, eb_09_input, by = c("year", "isocntry"), all.x = TRUE)
turnout <- merge(turnout, eb_14_input, by = c("year", "isocntry"), all.x = TRUE)
turnout <- merge(turnout, eb_19_input, by = c("year", "isocntry"), all.x = TRUE)

# clean up turnout df after merge ####
#  prepare the dataframe by shifting all data to the right columns

# 2009 data
# define the range of rows to be shifted
rows_to_shift <- 26:50
# define the range of columns to be shifted
columns_to_shift <- 16:26
# sift the data
turnout[rows_to_shift, columns_to_shift - 11] <- turnout[rows_to_shift, columns_to_shift]

# 2014 data
# define the range of rows to be shifted
rows_to_shift <- 51:75
# define the range of columns to be shifted
columns_to_shift <- 27:37
# shift the data
turnout[rows_to_shift, columns_to_shift - 22] <- turnout[rows_to_shift, columns_to_shift]

# 2019 data
# define the range of rows to be shifted
rows_to_shift <- 76:100
# define the range of columns to be shifted
columns_to_shift <- 38:48
# shift the data
turnout[rows_to_shift, columns_to_shift - 33] <- turnout[rows_to_shift, columns_to_shift]
# drop all leftover columns
turnout <- turnout[, 1:15]

#### restoring the column names
# Get the column names
column_names <- colnames(turnout)
# Modify the column names for all eb variable columns
column_names[5:15] <- substr(column_names[5:15], 1, nchar(column_names[5:15]) - 2)
# Assign the modified column names back to the turnout df
colnames(turnout) <- column_names






# descriptive statistics ####
# regression analysis ####

#model 1: trust 1
model1 <- lm(turnout ~ trust_pol_part + trust_nat_parl + trust_nat_gov + trust_EU,  data = turnout)

summary(model1)

#model 2: trust in EU institutions
model2 <- lm(turnout ~ trust_EP + trust_COM + trust_ECB,  data = turnout)

summary(model2)

# model 3: all trust
model_3 <- lm(turnout ~ trust_EP + trust_COM + trust_ECB + trust_pol_part + trust_nat_parl + trust_nat_gov + trust_EU,  data = turnout)

summary(model_3)

# model 4: image + membership general + membership benefit
model_4 <- lm(turnout ~ image_EU + member_general + member_benefit,  data = turnout)

summary(model_4)

# model 5: discuss
model_5 <- lm(turnout ~ discuss_pol, data = turnout)
summary(model_5)

# model 6: all variables
model_6 <- lm(turnout ~ trust_EP + trust_COM + trust_ECB + trust_pol_part + trust_nat_parl + trust_nat_gov + trust_EU + image_EU + member_general + member_benefit + discuss_pol,  data = turnout)
summary(model_6)

# model 7: interaction
model_7 <- lm(turnout ~ trust_EP*trust_pol_part + trust_ECB + member_general + member_benefit, data = turnout)
summary(model_7)

plot(model_7)

# different model specifications ####

# dropping compulsory voting countries
turnout_no_compulsory <- subset(turnout, !isocntry %in% c("LU", "BE", "GR"))

#model 1: trust 1
model1 <- lm(turnout ~ trust_pol_part + trust_nat_parl + trust_nat_gov + trust_EU,  data = turnout_no_compulsory)

summary(model1)

#model 2: trust in EU institutions
model2 <- lm(turnout ~ trust_EP + trust_COM + trust_ECB,  data = turnout_no_compulsory)

summary(model2)

# model 3: all trust
model_3 <- lm(turnout ~ trust_EP + trust_COM + trust_ECB + trust_pol_part + trust_nat_parl + trust_nat_gov + trust_EU,  data = turnout_no_compulsory)

summary(model_3)

# model 4: image + membership general + membership benefit
model_4 <- lm(turnout ~ image_EU + member_general + member_benefit,  data = turnout_no_compulsory)

summary(model_4)

# model 5: discuss
model_5 <- lm(turnout ~ discuss_pol, data = turnout_no_compulsory)
summary(model_5)

# model 6: all variables
model_6 <- lm(turnout ~ trust_EP + trust_COM + trust_ECB + trust_pol_part + trust_nat_parl + trust_nat_gov + trust_EU + image_EU + member_general + member_benefit + discuss_pol,  data = turnout_no_compulsory)
summary(model_6)

# model 7: interaction + most important variables
model_7 <- lm(turnout ~ trust_EP*trust_pol_part + trust_ECB + member_general + member_benefit, data = turnout_no_compulsory)
summary(model_7)

plot(model_7)




