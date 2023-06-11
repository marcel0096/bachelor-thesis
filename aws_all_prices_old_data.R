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

## ON DEMAND, SPOT AND RESERVED INSTANCES

# function for preparing all RI data to be merged together
OD.Spot.RI.data.prepare.datasets <- function(data, input.tenancy, plan, duration, payment) {
  
  column.name <- paste("RI.costs.", input.tenancy, ".", plan, ".", duration, "year.", gsub(" ", "", payment), sep = "")
  
  df <- data %>%
    select(c(API.Name, Linux.Reserved.cost)) %>%
    rename(!!column.name := Linux.Reserved.cost) %>%
    distinct(API.Name, .keep_all = TRUE)
}

# merging all data frames based on matching API names
OD.Spot.RI.data.merge.datasets <- function(RI.data.list) {
  
  # using the OD.Spot.RI.data.prepare.datasets function to prepare each dataset
  df_list <- list(df0 <- OD.Spot.RI.shared.all.prices,
                  df1 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[1]], "shared", "Standard", 1, "No Upfront")[, 1:2],
                  df2 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[2]], "shared", "Standard", 1, "Partial Upfront")[, 1:2],
                  df3 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[3]], "shared", "Standard", 1, "All Upfront")[, 1:2],
                  df4 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[4]], "shared", "Standard", 3, "No Upfront")[, 1:2],
                  df5 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[5]], "shared", "Standard", 3, "Partial Upfront")[, 1:2],
                  df6 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[6]], "shared", "Standard", 3, "All Upfront")[, 1:2],
                  df7 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[7]], "shared", "Convertible", 1, "No Upfront")[, 1:2],
                  df8 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[8]], "shared", "Convertible", 1, "Partial Upfront")[, 1:2],
                  df9 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[9]], "shared", "Convertible", 1, "All Upfront")[, 1:2],
                  df10 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[10]], "shared", "Convertible", 3, "No Upfront")[, 1:2],
                  df11 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[11]], "shared", "Convertible", 3, "Partial Upfront")[, 1:2],
                  df12 <- OD.Spot.RI.data.prepare.datasets(RI.data.list[[12]], "shared", "Convertible", 3, "All Upfront")[, 1:2]
                  )
  
  OD.Spot.RI.shared.all.prices <- reduce(df_list, merge, by = "API.Name", all = TRUE)
  return(OD.Spot.RI.shared.all.prices)
}

aws.data.OD.RI.Spot.load <- function() {
  
  # On demand, Spot and RI prices retrieved from https://instances.vantage.sh on May 1st 2023 (us-east-2)
  RI.data.shared.standard.1year.noUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_standard_1year_noUpfront.csv", sep = "/"))
  RI.data.shared.standard.1year.partialUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_standard_1year_partialUpfront.csv", sep = "/"))
  RI.data.shared.standard.1year.allUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_standard_1year_allUpfront.csv", sep = "/"))
  RI.data.shared.standard.3year.noUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_standard_3year_noUpfront.csv", sep = "/"))
  RI.data.shared.standard.3year.partialUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_standard_3year_partialUpfront.csv", sep = "/"))
  RI.data.shared.standard.3year.allUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_standard_3year_allUpfront.csv", sep = "/"))
  RI.data.shared.convertible.1year.noUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_convertible_1year_noUpfront.csv", sep = "/"))
  RI.data.shared.convertible.1year.partialUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_convertible_1year_partialUpfront.csv", sep = "/"))
  RI.data.shared.convertible.1year.allUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_convertible_1year_allUpfront.csv", sep = "/"))
  RI.data.shared.convertible.3year.noUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_convertible_3year_noUpfront.csv", sep = "/"))
  RI.data.shared.convertible.3year.partialUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_convertible_3year_partialUpfront.csv", sep = "/"))
  RI.data.shared.convertible.3year.allUpFront <- read.csv(paste(aws.data.folder, "aws_OD_Spot_RI_shared_convertible_3year_allUpfront.csv", sep = "/"))
  
  # copy the first data set to build one big data set
  OD.Spot.RI.shared.all.prices <- RI.data.shared.standard.1year.noUpFront
  
  # removing unnecessary columns and renaming remaining ones    
  OD.Spot.RI.shared.all.prices <<- OD.Spot.RI.shared.all.prices %>%
    select(c(`API.Name`, vCPUs, `Instance.Memory`, `Network.Performance`, `Linux.On.Demand.cost`, `Linux.Spot.Minimum.cost`)) %>% 
    rename(Memory.GiB = `Instance.Memory`, Network.Gbit = `Network.Performance`, 
           OD.costs = `Linux.On.Demand.cost`, Spot.costs = `Linux.Spot.Minimum.cost`)
  
  RI.data.list <- list(RI.data.shared.standard.1year.noUpFront, RI.data.shared.standard.1year.partialUpFront,
                       RI.data.shared.standard.1year.allUpFront, RI.data.shared.standard.3year.noUpFront,
                       RI.data.shared.standard.3year.partialUpFront, RI.data.shared.standard.3year.allUpFront,
                       RI.data.shared.convertible.1year.noUpFront, RI.data.shared.convertible.1year.partialUpFront,
                       RI.data.shared.convertible.1year.allUpFront, RI.data.shared.convertible.3year.noUpFront,
                       RI.data.shared.convertible.3year.partialUpFront, RI.data.shared.convertible.3year.allUpFront)
  
  # merging data sets
  OD.Spot.RI.shared.all.prices <- OD.Spot.RI.data.merge.datasets(RI.data.list)
  
  # Keeping only the numeric values
  OD.Spot.RI.shared.all.prices$Memory.GiB <- as.numeric(gsub("[^0-9.]", "", OD.Spot.RI.shared.all.prices$Memory.GiB))
  OD.Spot.RI.shared.all.prices$OD.costs <- as.numeric(gsub("[^0-9.]", "", OD.Spot.RI.shared.all.prices$OD.costs))
  OD.Spot.RI.shared.all.prices$Spot.costs <- as.numeric(gsub("[^0-9.]", "", OD.Spot.RI.shared.all.prices$Spot.costs))
  OD.Spot.RI.shared.all.prices$vCPUs <- as.numeric(sub(" .*", "", OD.Spot.RI.shared.all.prices$vCPUs))
  OD.Spot.RI.shared.all.prices$Network.Gbit <- as.numeric(gsub("[^0-9.]", "", OD.Spot.RI.shared.all.prices$Network.Gbit))
  OD.Spot.RI.shared.all.prices[, 7:18] <- apply(OD.Spot.RI.shared.all.prices[, 7:18], 2, function(x) as.numeric(gsub("[^0-9.]", "", x)))
  
  # Fixing the rows with x * 100 Gbit
  OD.Spot.RI.shared.all.prices$Network.Gbit[OD.Spot.RI.shared.all.prices$Network.Gbit == 16100] <- 1600
  OD.Spot.RI.shared.all.prices$Network.Gbit[OD.Spot.RI.shared.all.prices$Network.Gbit == 8100] <- 800
  OD.Spot.RI.shared.all.prices$Network.Gbit[OD.Spot.RI.shared.all.prices$Network.Gbit == 4100] <- 400
  
  # Remove rows with N/A entries
  OD.Spot.RI.shared.all.prices <- na.omit(OD.Spot.RI.shared.all.prices)
  
  # remove burstable instances "t"
  OD.Spot.RI.shared.all.prices <- OD.Spot.RI.shared.all.prices[!grepl("^t", OD.Spot.RI.shared.all.prices$API.Name), ]
  
  OD.Spot.RI.shared.all.prices <<- OD.Spot.RI.shared.all.prices
}


## SAVINGS PLANS

# function for splitting up the raw SP_data dataset into seperate data sets for each savings plan option
SP.data.split.dataset <- function(data, input.tenancy, plan, duration, payment) {
  
  column.name <- paste("SP.costs.", input.tenancy, ".", plan, ".", duration, "year.", gsub(" ", "", payment), sep = "")
  
  df <- data %>%
    filter(tenancy == input.tenancy, SP.Plan == plan, SP_Duration == duration, SP.Payment == payment) %>%
    rename(!!column.name := SP.Costs) %>%
    distinct(API.Name, .keep_all = TRUE)
}

# merging all data frames based on matching API names and creating final SP data set
SP.data.merge.datasets <- function(SP.data) {
  
  # using the SP.data.split function to create each dataset
  df_list <- list(df1 <- SP.data.split.dataset(SP.data, "shared", "Compute", 1, "No Upfront")[, 1:2],
                  df2 <- SP.data.split.dataset(SP.data, "shared", "Compute", 1, "Partial Upfront")[, 1:2],
                  df3 <- SP.data.split.dataset(SP.data, "shared", "Compute", 1, "All Upfront")[, 1:2],
                  df4 <- SP.data.split.dataset(SP.data, "shared", "Compute", 3, "No Upfront")[, 1:2],
                  df5 <- SP.data.split.dataset(SP.data, "shared", "Compute", 3, "Partial Upfront")[, 1:2],
                  df6 <- SP.data.split.dataset(SP.data, "shared", "Compute", 3, "All Upfront")[, 1:2],
                  df7 <- SP.data.split.dataset(SP.data, "shared", "EC2Instance", 1, "No Upfront")[, 1:2],
                  df8 <- SP.data.split.dataset(SP.data, "shared", "EC2Instance", 1, "Partial Upfront")[, 1:2],
                  df9 <- SP.data.split.dataset(SP.data, "shared", "EC2Instance", 1, "All Upfront")[, 1:2],
                  df10 <- SP.data.split.dataset(SP.data, "shared", "EC2Instance", 3, "No Upfront")[, 1:2],
                  df11 <- SP.data.split.dataset(SP.data, "shared", "EC2Instance", 3, "Partial Upfront")[, 1:2],
                  df12 <- SP.data.split.dataset(SP.data, "shared", "EC2Instance", 3, "All Upfront")[, 1:2]
  )
  
  SP.shared.all.prices <- reduce(df_list, merge, by = "API.Name", all = TRUE)
  return(SP.shared.all.prices)
}

aws.data.SP.load <- function() {
  
  # savings plans prices for region us-east-2 (retrieved in 2020)
  SP.data <- read.csv(paste(aws.data.folder, "aws_SP_all.csv", sep = "/"))
  
  SP.data <- SP.data %>%
    select(c(rate, instanceType, tenancy, savingsPlanOffering.paymentOption, savingsPlanOffering.planType, savingsPlanOffering.durationSeconds)) %>%
    rename(SP.Costs = rate, API.Name = instanceType, SP.Payment = savingsPlanOffering.paymentOption, SP.Plan = savingsPlanOffering.planType, 
           SP_Duration = savingsPlanOffering.durationSeconds)
  
  # converting seconds to years
  SP.data$SP_Duration <- SP.data$SP_Duration / 31536000
  
  # merge data sets
  SP.shared.all.prices <- SP.data.merge.datasets(SP.data)
  
  # exclude burstable instances "t"
  SP.shared.all.prices <- SP.shared.all.prices[!grepl("^t", SP.shared.all.prices$API.Name), ]
  
  SP.shared.all.prices <<- SP.shared.all.prices
}


## SPOT INTERRUPTION FREQUENCIES

aws.data.spot.interruption.freq.load <- function() {
  
  # spot interruption frequencies for region us-east-2 (retrieved in 2020)
  Spot.interruption.freq <- read.csv(paste(aws.data.folder, "spot_interruption_freq.csv", sep = "/"))
  
  Spot.interruption.freq <- Spot.interruption.freq %>%
    select(c(instanceType, interruptionFrequency)) %>%
    rename(API.Name = instanceType, Interruption.Freq = interruptionFrequency)
  
  # calculating decimal values for interruption frequencies to use in calculations (adapted for one hour)
  Spot.interruption.freq[, 2] <- case_when(Spot.interruption.freq[, 2] == '<5%' ~ (0.025 / (30 * 24)),
                                           Spot.interruption.freq[, 2] == '5-10%' ~ (0.075 / (30 * 24)),
                                           Spot.interruption.freq[, 2] == '10-15%' ~ (0.125 / (30 * 24)),
                                           Spot.interruption.freq[, 2] == '15-20%' ~ (0.175 / (30 * 24)),
                                           Spot.interruption.freq[, 2] == '>20%' ~ (0.225 / (30 * 24)))
  
  Spot.interruption.freq <<- Spot.interruption.freq
}


# ------------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------- PROCESSING DATA ------------------------------------------------- # 
# ------------------------------------------------------------------------------------------------------------------- #

#util.packages.install()
util.packages.load()

# On demand, Spot and Reserved Instances
aws.data.OD.RI.Spot.load()

# Savings plans
aws.data.SP.load()

# Spot interruption frequencies
aws.data.spot.interruption.freq.load()


## FINAL DATASETS 

# Data set with all prices, Spot included 
aws.shared.all.prices <- merge(OD.Spot.RI.shared.all.prices, SP.shared.all.prices, by = "API.Name", all = TRUE)

# omitting all na values for now
aws.shared.all.prices <- na.omit(aws.shared.all.prices)

# Data set with all prices, Spot excluded 
aws.shared.all.prices.without.Spot <- aws.shared.all.prices[ , -6]

# Data set with only on demand prices for basic version of algorithm
aws.all.prices.OD.only <- aws.shared.all.prices[ , 1:5]

# Data set with interruption frequencies included
aws.shared.all.prices.with.interrupt.freq <- merge(aws.shared.all.prices, Spot.interruption.freq, by = "API.Name")
