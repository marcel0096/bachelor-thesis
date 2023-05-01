# ------------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------- DATA PREPARATION ------------------------------------------------ # 
# ------------------------------------------------------------------------------------------------------------------- #


# loading libraries
library(dplyr)
library(tidyverse)


# -------------------------------------- ON DEMAND, SPOT, RESERVED INSTANCES --------------------------------------- #

# importing all raw data as csv files
RI_data_shared_standard_1year_noUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_standard_1year_noUpfront.csv")
RI_data_shared_standard_1year_partialUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_standard_1year_partialUpfront.csv")
RI_data_shared_standard_1year_allUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_standard_1year_allUpfront.csv")
RI_data_shared_standard_3year_noUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_standard_3year_noUpfront.csv")
RI_data_shared_standard_3year_partialUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_standard_3year_partialUpfront.csv")
RI_data_shared_standard_3year_allUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_standard_3year_allUpfront.csv")
RI_data_shared_convertible_1year_noUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_convertible_1year_noUpfront.csv")
RI_data_shared_convertible_1year_partialUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_convertible_1year_partialUpfront.csv")
RI_data_shared_convertible_1year_allUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_convertible_1year_allUpfront.csv")
RI_data_shared_convertible_3year_noUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_convertible_3year_noUpfront.csv")
RI_data_shared_convertible_3year_partialUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_convertible_3year_partialUpfront.csv")
RI_data_shared_convertible_3year_allUpFront <- read.csv("~/bachelor-thesis/aws_raw_data/aws_OD_Spot_RI_shared_convertible_3year_allUpfront.csv")


# copy the first data set to build one big data set
OD_Spot_RI_shared_all_prices <- RI_data_shared_standard_1year_noUpFront

# removing unnecessary columns    
OD_Spot_RI_shared_all_prices <- OD_Spot_RI_shared_all_prices %>%
  select(c(`API.Name`, vCPUs, `Instance.Memory`, `Network.Performance`, `Linux.On.Demand.cost`, `Linux.Spot.Minimum.cost`))

# renaming the columns
OD_Spot_RI_shared_all_prices <- OD_Spot_RI_shared_all_prices %>% 
  rename(API_Name = `API.Name`, Memory_GiB = `Instance.Memory`, Network_Gbit = `Network.Performance`
         , Costs_On_Demand = `Linux.On.Demand.cost`, Costs_Spot = `Linux.Spot.Minimum.cost`)

# now keeping only the RI prices from each data set to avoid overhead
#for (dataset in seq_along(all_RI_files)) {
#  all_RI_files[[dataset]] <- all_RI_files[[dataset]][, "Linux.Reserved.cost", drop = FALSE]
#}

RI_data_shared_standard_1year_noUpFront <- RI_data_shared_standard_1year_noUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_standard_1year_noUpFront = `Linux.Reserved.cost`)

RI_data_shared_standard_1year_partialUpFront <- RI_data_shared_standard_1year_partialUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_standard_1year_partialUpFront = `Linux.Reserved.cost`)

RI_data_shared_standard_1year_allUpFront <- RI_data_shared_standard_1year_allUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_standard_1year_allUpFront = `Linux.Reserved.cost`)

RI_data_shared_standard_3year_noUpFront <- RI_data_shared_standard_3year_noUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_standard_3year_noUpFront = `Linux.Reserved.cost`)

RI_data_shared_standard_3year_partialUpFront <- RI_data_shared_standard_3year_partialUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_standard_3year_partialUpFront = `Linux.Reserved.cost`)

RI_data_shared_standard_3year_allUpFront <- RI_data_shared_standard_3year_allUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_standard_3year_allUpFront = `Linux.Reserved.cost`)

RI_data_shared_convertible_1year_noUpFront <- RI_data_shared_convertible_1year_noUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_convertible_1year_noUpFront = `Linux.Reserved.cost`)

RI_data_shared_convertible_1year_partialUpFront <- RI_data_shared_convertible_1year_partialUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_convertible_1year_partialUpFront = `Linux.Reserved.cost`)

RI_data_shared_convertible_1year_allUpFront <- RI_data_shared_convertible_1year_allUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_convertible_1year_allUpFront = `Linux.Reserved.cost`)

RI_data_shared_convertible_3year_noUpFront <- RI_data_shared_convertible_3year_noUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_convertible_3year_noUpFront = `Linux.Reserved.cost`)

RI_data_shared_convertible_3year_partialUpFront <- RI_data_shared_convertible_3year_partialUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_convertible_3year_partialUpFront = `Linux.Reserved.cost`)

RI_data_shared_convertible_3year_allUpFront <- RI_data_shared_convertible_3year_allUpFront %>% 
  select(c(Linux.Reserved.cost)) %>% 
  rename(Costs_RI_shared_convertible_3year_allUpFront = `Linux.Reserved.cost`)


#all_RI_files <- list(RI_data_shared_convertible_1year_allUpFront, RI_data_shared_standard_1year_partialUpFront, RI_data_shared_standard_1year_allUpFront, 
#                     RI_data_shared_standard_3year_noUpFront, RI_data_shared_standard_3year_partialUpFront, RI_data_shared_standard_3year_allUpFront,
#                     RI_data_shared_convertible_1year_noUpFront, RI_data_shared_convertible_1year_partialUpFront, RI_data_shared_convertible_1year_allUpFront,
#                     RI_data_shared_convertible_3year_noUpFront, RI_data_shared_convertible_3year_partialUpFront, RI_data_shared_convertible_3year_allUpFront)

# binding all prices together
OD_Spot_RI_shared_all_prices <- cbind(OD_Spot_RI_shared_all_prices, RI_data_shared_standard_1year_noUpFront, RI_data_shared_standard_1year_partialUpFront, RI_data_shared_standard_1year_allUpFront, 
                                      RI_data_shared_standard_3year_noUpFront, RI_data_shared_standard_3year_partialUpFront, RI_data_shared_standard_3year_allUpFront,
                                      RI_data_shared_convertible_1year_noUpFront, RI_data_shared_convertible_1year_partialUpFront, RI_data_shared_convertible_1year_allUpFront,
                                      RI_data_shared_convertible_3year_noUpFront, RI_data_shared_convertible_3year_partialUpFront, RI_data_shared_convertible_3year_allUpFront)

# Keeping only the numeric values
OD_Spot_RI_shared_all_prices$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", OD_Spot_RI_shared_all_prices$Memory_GiB))
OD_Spot_RI_shared_all_prices$Costs_On_Demand <- as.numeric(gsub("[^0-9.]", "", OD_Spot_RI_shared_all_prices$Costs_On_Demand))
OD_Spot_RI_shared_all_prices$Costs_Spot <- as.numeric(gsub("[^0-9.]", "", OD_Spot_RI_shared_all_prices$Costs_Spot))
OD_Spot_RI_shared_all_prices$vCPUs <- as.numeric(sub(" .*", "", OD_Spot_RI_shared_all_prices$vCPUs))
OD_Spot_RI_shared_all_prices$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", OD_Spot_RI_shared_all_prices$Network_Gbit))
#OD_Spot_RI_shared_all_prices[, 7:18] <- as.numeric(gsub("[^0-9.]", "", OD_Spot_RI_shared_all_prices[, 7:18]))
OD_Spot_RI_shared_all_prices[, 7:18] <- apply(OD_Spot_RI_shared_all_prices[, 7:18], 2, function(x) as.numeric(gsub("[^0-9.]", "", x)))


# Fixing the rows with x * 100 Gbit
OD_Spot_RI_shared_all_prices$Network_Gbit[OD_Spot_RI_shared_all_prices$Network_Gbit == 16100] <- 1600
OD_Spot_RI_shared_all_prices$Network_Gbit[OD_Spot_RI_shared_all_prices$Network_Gbit == 8100] <- 800
OD_Spot_RI_shared_all_prices$Network_Gbit[OD_Spot_RI_shared_all_prices$Network_Gbit == 4100] <- 400

# Remove rows with N/A entries
OD_Spot_RI_shared_all_prices <- na.omit(OD_Spot_RI_shared_all_prices)

# remove burstable instances "t"
OD_Spot_RI_shared_all_prices <- OD_Spot_RI_shared_all_prices[!grepl("^t", OD_Spot_RI_shared_all_prices$API_Name), ]

# add instances starting with "a"
#aws_data_trimmed_all_prices_2 <- OD_Spot_RI_shared_all_prices[!grepl("^a", OD_Spot_RI_shared_all_prices$API_Name), ]

# Adding vCPU/$ for on demand prices
OD_Spot_RI_shared_all_prices$vCPUs_per_Dollar_On_Demand <- OD_Spot_RI_shared_all_prices$vCPUs / OD_Spot_RI_shared_all_prices$Costs_On_Demand


# ------------------------------------------- SAVINGS PLANS -------------------------------------------- #

# savings plans prices for region us-east-2
SP_data <- read.csv("~/bachelor-thesis/aws_raw_data/aws_SP_all.csv")

# only keep relevant data
SP_data <- SP_data %>%
  select(c(rate, instanceType, tenancy, savingsPlanOffering.paymentOption, savingsPlanOffering.planType, savingsPlanOffering.durationSeconds))

# converting seconds to years
SP_data$savingsPlanOffering.durationSeconds <- SP_data$savingsPlanOffering.durationSeconds / 31536000

# renaming columns
SP_data <- SP_data %>% 
  rename(SP_Costs = rate, API_Name = instanceType, SP_Payment = savingsPlanOffering.paymentOption, SP_Plan = savingsPlanOffering.planType, 
         SP_Duration = savingsPlanOffering.durationSeconds)

# Splitting dataset up into each version
SP_data_shared_compute_1year_noUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "Compute" & SP_data$SP_Duration == 1 
                                                             & SP_data$SP_Payment == "No Upfront") %>% 
  rename(SP_Costs_shared_compute_1year_noUpFront = SP_Costs)

SP_data_shared_compute_1year_partialUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "Compute" & SP_data$SP_Duration == 1 
                                                                  & SP_data$SP_Payment == "Partial Upfront") %>% 
  rename(SP_Costs_shared_compute_1year_partialUpFront = SP_Costs)

SP_data_shared_compute_1year_allUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "Compute" & SP_data$SP_Duration == 1 
                                                              & SP_data$SP_Payment == "All Upfront") %>% 
  rename(SP_Costs_shared_compute_1year_allUpFront = SP_Costs)

SP_data_shared_compute_3year_noUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "Compute" & SP_data$SP_Duration == 3 
                                                             & SP_data$SP_Payment == "No Upfront") %>% 
  rename(SP_Costs_shared_compute_3year_noUpFront = SP_Costs)

SP_data_shared_compute_3year_partialUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "Compute" & SP_data$SP_Duration == 3 
                                                                  & SP_data$SP_Payment == "Partial Upfront") %>% 
  rename(SP_Costs_shared_compute_3year_partialUpFront = SP_Costs)

SP_data_shared_compute_3year_allUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "Compute" & SP_data$SP_Duration == 3 
                                                              & SP_data$SP_Payment == "All Upfront") %>% 
  rename(SP_Costs_shared_compute_3year_allUpFront = SP_Costs)


SP_data_shared_ec2instance_1year_noUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "EC2Instance" & SP_data$SP_Duration == 1 
                                                                 & SP_data$SP_Payment == "No Upfront") %>% 
  rename(SP_Costs_shared_ec2instance_1year_noUpFront = SP_Costs)

SP_data_shared_ec2instance_1year_partialUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "EC2Instance" & SP_data$SP_Duration == 1 
                                                                      & SP_data$SP_Payment == "Partial Upfront") %>% 
  rename(SP_Costs_shared_ec2instance_1year_partialUpFront = SP_Costs)

SP_data_shared_ec2instance_1year_allUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "EC2Instance" & SP_data$SP_Duration == 1 
                                                                  & SP_data$SP_Payment == "All Upfront") %>% 
  rename(SP_Costs_shared_ec2instance_1year_allUpFront = SP_Costs)

SP_data_shared_ec2instance_3year_noUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "EC2Instance" & SP_data$SP_Duration == 3 
                                                                 & SP_data$SP_Payment == "No Upfront") %>% 
  rename(SP_Costs_shared_ec2instance_3year_noUpFront = SP_Costs)

SP_data_shared_ec2instance_3year_partialUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "EC2Instance" & SP_data$SP_Duration == 3 
                                                                      & SP_data$SP_Payment == "Partial Upfront") %>% 
  rename(SP_Costs_shared_ec2instance_3year_partialUpFront = SP_Costs)

SP_data_shared_ec2instance_3year_allUpFront <- SP_data %>% filter(SP_data$tenancy == "shared" & SP_data$SP_Plan == "EC2Instance" & SP_data$SP_Duration == 3 
                                                                  & SP_data$SP_Payment == "All Upfront") %>% 
  rename(SP_Costs_shared_ec2instance_3year_allUpFront = SP_Costs)










  ((3600 / aws_data_trimmed_all_prices_3$vCPUs) + (10 * 8) / (aws_data_trimmed_all_prices_3$Network_Gbit * 0.8)) * (aws_data_trimmed_all_prices_3$On_demand_Costs_Dollar_per_Hour / 3600)