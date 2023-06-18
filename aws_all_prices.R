# ------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------- PACKAGES ------------------------------------------------------ # 
# ------------------------------------------------------------------------------------------------------------------- #

util.packages.install <- function() {
  install.packages("dplyr")
  install.packages("tidyverse")
}

util.packages.load <- function() {
  library(dplyr)
  library(tidyverse)
}


# ------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- LOADING & PREPARING DATA -------------------------------------------- # 
# ------------------------------------------------------------------------------------------------------------------- #

aws.data.folder <- "./aws_raw_data"


## Load data for region "us-east-1"

aws.OD.prices.load <- function() {
  aws.OD.prices <<- read.csv(paste(aws.data.folder, "OD_rates_2023-06-18.csv", sep = "/"))
}

aws.RI.prices.load <- function() {
  aws.RI.prices.1a <- read.csv(paste(aws.data.folder, "RI_rates_useast1a_2023-06-18.csv", sep = "/"))
  aws.RI.prices.1b <- read.csv(paste(aws.data.folder, "RI_rates_useast1b_2023-06-18.csv", sep = "/"))
  aws.RI.prices.1c <- read.csv(paste(aws.data.folder, "RI_rates_useast1c_2023-06-18.csv", sep = "/"))
  
  aws.RI.prices <<- rbind(aws.RI.prices.1a, aws.RI.prices.1b, aws.RI.prices.1c)
}

aws.SP.prices.load <- function() {
  aws.SP.prices <<- read.csv(paste(aws.data.folder, "SP_rates_2023-06-18.csv", sep = "/"))
}

aws.Spot.prices.load <- function() {
  aws.Spot.prices.1a <- read.csv(paste(aws.data.folder, "Spot_rates_useast1a_2023-06-18.csv", sep = "/"))
  aws.Spot.prices.1b <- read.csv(paste(aws.data.folder, "Spot_rates_useast1b_2023-06-18.csv", sep = "/"))
  aws.Spot.prices.1c <- read.csv(paste(aws.data.folder, "Spot_rates_useast1c_2023-06-18.csv", sep = "/"))
  
  aws.Spot.prices <<- rbind(aws.Spot.prices.1a, aws.Spot.prices.1b, aws.Spot.prices.1c)
}

aws.Spot.interruption.freq.load <- function() {
  aws.Spot.interruption.freq <<- read.csv(paste(aws.data.folder, "Spot_interruption_freq_2023-06-18.csv", sep = "/"))
}


## PROCESS ON DEMAND INSTANCES

aws.OD.prices.process <- function() {
  
  aws.OD.prices.load()
  
  # remove duplicates
  aws.OD.prices <- aws.OD.prices[aws.OD.prices$Hourly.Price != 0, ]
  aws.OD.prices <- distinct(aws.OD.prices, Instance.Type, .keep_all = TRUE)
  
  # rename and remove columns   
  aws.OD.prices <- aws.OD.prices %>%
    rename(API.Name = `Instance.Type`, vCPUs = `vCpus`, Memory.GiB = `Memory`, Network.Gbit = `Network`, 
           OD.costs.hourly = `Hourly.Price`)  %>%
    select(-Region)
  
  # keep only numeric values
  aws.OD.prices$Memory.GiB <- as.numeric(gsub("[^0-9.]", "", aws.OD.prices$Memory.GiB))
  aws.OD.prices$OD.costs.hourly <- as.numeric(aws.OD.prices$OD.costs.hourly)
  aws.OD.prices$vCPUs <- as.numeric(aws.OD.prices$vCPUs)
  
  # remove all burstable instances t.*
  aws.OD.prices <- aws.OD.prices[!grepl("^t", aws.OD.prices$API.Name), ]
  
  # order data set alphabetically 
  aws.OD.prices <<- aws.OD.prices[order(aws.OD.prices$API.Name), ]
  row.names(aws.OD.prices) <- NULL
}


## PROCESS RESERVED INSTANCES

aws.RI.prices.process <- function() {
  
  aws.RI.prices.load()
  
  # keep only necessary columns   
  aws.RI.prices <- aws.RI.prices %>%
    select(c(InstanceType, OfferingClass, OfferingType, Duration, FixedPrice, RecurringCharges)) %>% 
    rename(API.Name = `InstanceType`)
  
  # make duration and prices all numeric, eliminate space in OfferingType
  aws.RI.prices$Duration <- as.numeric(aws.RI.prices$Duration)
  aws.RI.prices$FixedPrice <- as.numeric(aws.RI.prices$FixedPrice)
  aws.RI.prices$RecurringCharges <- as.numeric(gsub("[^0-9.]", "", aws.RI.prices$RecurringCharges))
  aws.RI.prices$OfferingType <- gsub(" ", "", aws.RI.prices$OfferingType)
  
  # remove all burstable instances t.*
  aws.RI.prices <- aws.RI.prices[!grepl("^t", aws.RI.prices$API.Name), ]
  
  # calculate hourly prices based on duration, fixed price and recurring charges
  aws.RI.prices$RI.costs.hourly <- 
    (aws.RI.prices[ , 'FixedPrice'] / (aws.RI.prices[ , 'Duration'] / 60 / 60)) + aws.RI.prices[ , 'RecurringCharges']
  
  # calculate duration from seconds to years
  aws.RI.prices[, 'Duration'] <- ifelse(aws.RI.prices[, 'Duration'] == 31536000, 1, 3)
  
  # take mean over each AZ
  aws.RI.prices <- aggregate(RI.costs.hourly ~ API.Name + OfferingClass + OfferingType + Duration, data = aws.RI.prices, mean)
  
  # New column with full pricing name  
  aws.RI.prices$column.names <- 
    paste("RI.costs.hourly.shared.", aws.RI.prices$OfferingClass, ".", aws.RI.prices$Duration, "year.", aws.RI.prices$OfferingType, sep = "")
  
  # pivot data frame
  aws.RI.prices <<- aws.RI.prices %>% 
    select(c(API.Name, RI.costs.hourly, column.names)) %>% 
    pivot_wider(names_from = column.names, values_from = RI.costs.hourly) %>% 
    na.omit(aws.RI.prices)
}


## PROCESS SAVINGS PLANS

aws.SP.prices.process <- function() {
  
  aws.SP.prices.load()
  
  # keep only necessary columns    
  aws.SP.prices <- aws.SP.prices %>%
    select(c(instanceType, tenancy, savingsPlanOffering.paymentOption, 
             savingsPlanOffering.planType, savingsPlanOffering.durationSeconds, rate)) %>% 
    rename(API.Name = `instanceType`)
  
  # only keep entries with shared tenancy
  aws.SP.prices <- aws.SP.prices[aws.SP.prices$tenancy == 'shared', ]
  
  # calculate duration from seconds to years
  aws.SP.prices[, 'savingsPlanOffering.durationSeconds'] <- 
    ifelse(aws.SP.prices[, 'savingsPlanOffering.durationSeconds'] == 31536000, 1, 3)
  
  # make rate numeric, eliminate space in paymentOption
  aws.SP.prices$rate <- as.numeric(aws.SP.prices$rate)
  aws.SP.prices$savingsPlanOffering.paymentOption <- gsub(" ", "", aws.SP.prices$savingsPlanOffering.paymentOption)
  
  # remove all burstable instances t.*
  aws.SP.prices <- aws.SP.prices[!grepl("^t", aws.SP.prices$API.Name), ]
  
  # New column with full pricing name
  aws.SP.prices$column.names <- 
    paste("SP.costs.hourly.shared.", aws.SP.prices$savingsPlanOffering.planType, ".", 
          aws.SP.prices$savingsPlanOffering.durationSeconds, "year.", aws.SP.prices$savingsPlanOffering.paymentOption, sep = "")

  # remove duplicates
  aws.SP.prices <- aws.SP.prices[!duplicated(aws.SP.prices[ , c("API.Name", "column.names")]), ]
  
  # pivot data frame
  aws.SP.prices <<- aws.SP.prices %>% 
    select(c(API.Name, rate, column.names)) %>% 
    pivot_wider(names_from = column.names, values_from = rate) %>% 
    na.omit(aws.SP.prices)
}


## PROCESS SPOT INSTANCES

aws.Spot.prices.process <- function() {
  
  aws.Spot.prices.load()
  
  aws.Spot.prices <- aws.Spot.prices %>%  
    rename(API.Name = `InstanceType`, Spot.costs.hourly = `SpotPrice`)
  
  # remove all burstable instances t.*
  aws.Spot.prices <- aws.Spot.prices[!grepl("^t", aws.Spot.prices$API.Name), ]
  
  # get one additional dataset with the minimum price of an instance
  aws.Spot.prices.min <<- aggregate(Spot.costs.hourly ~ API.Name, data = aws.Spot.prices, min)
  
  # get mean for each instance over last three months
  aws.Spot.prices <<- aggregate(Spot.costs.hourly ~ API.Name, data = aws.Spot.prices, mean)
}


## PROCESS SPOT INTERRUPTION FREQUENCIES

aws.Spot.interruption.freq.process <- function() {
  
  aws.Spot.interruption.freq.load()
  
  aws.Spot.interruption.freq <- aws.Spot.interruption.freq %>%
    select(c(instanceType, interruptionFrequency)) %>%
    rename(API.Name = instanceType, Interruption.Freq = interruptionFrequency)
  
  # remove all burstable instances t.*
  aws.Spot.interruption.freq <- aws.Spot.interruption.freq[!grepl("^t", aws.Spot.interruption.freq$API.Name), ]
  
  # calculating decimal values for interruption frequencies to use in calculations (adapted for one hour)
  aws.Spot.interruption.freq[, 2] <- case_when(aws.Spot.interruption.freq[, 2] == '<5%' ~ (0.025 / (30 * 24)),
                                               aws.Spot.interruption.freq[, 2] == '5-10%' ~ (0.075 / (30 * 24)),
                                               aws.Spot.interruption.freq[, 2] == '10-15%' ~ (0.125 / (30 * 24)),
                                               aws.Spot.interruption.freq[, 2] == '15-20%' ~ (0.175 / (30 * 24)),
                                               aws.Spot.interruption.freq[, 2] == '>20%' ~ (0.225 / (30 * 24)))
  
  aws.Spot.interruption.freq <<- aws.Spot.interruption.freq
}

create.all.prices.dataset <- function() {
  aws.list <- list(aws.OD.prices, aws.RI.prices, aws.SP.prices, aws.Spot.prices, aws.Spot.interruption.freq)
  aws.all.prices <<- aws.list %>% reduce(inner_join, by = 'API.Name')
}

# dataset with minimum Spot prices
create.all.prices.dataset.min <- function() {
  aws.list <- list(aws.OD.prices, aws.RI.prices, aws.SP.prices, aws.Spot.prices.min, aws.Spot.interruption.freq)
  aws.all.prices.min <<- aws.list %>% reduce(inner_join, by = 'API.Name')
}


# ------------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------- PROCESSING DATA ------------------------------------------------- # 
# ------------------------------------------------------------------------------------------------------------------- #

#util.packages.install()
util.packages.load()

# process the data of all pricing models
aws.OD.prices.process()
aws.RI.prices.process()
aws.SP.prices.process()
aws.Spot.prices.process()
aws.Spot.interruption.freq.process()


## FINAL DATA SET
create.all.prices.dataset()
create.all.prices.dataset.min()

# create subsets for versions of algorithm
aws.all.prices.OD <- aws.all.prices[ , 1:5]
aws.all.prices.OD.RI.SP <- aws.all.prices[ , 1:29]





# Annahmen data
#   - Data was pulled via AWS API on the 12th of June 2023
#   - The data is from region us-east-1, Spot prices are averaged over all 3 AZs in that region
#   - Only instances with SHARED tenancy are included (not dedicated)
#   - Burstable instances are excluded
#   - Instances with incomplete data (e.g. no SP for 3 years) are excluded completely
#   - Spot prices are averaged over the last 3 months since the retrieval of the data
#   - Spot interruption frequency intervals are set to the mean value of lower and upper bound
#   - RI Market prices are not included
#   - vCPUs are equal to real cores -> OFFEN (adapt intel and amd instances)


# write.csv(aws.all.prices, "~/bachelor-thesis/aws.all.prices.csv", row.names=FALSE)
