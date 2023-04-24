library(dplyr)

# importing csv export from https://instances.vantage.sh

aws_data <- read_csv("Desktop/TUM - Wirtschaftsinformatik B.Sc./Bachelorarbeit/Amazon EC2 Instance Comparison.csv")

# removing unnecessary columns

aws_data_trimmed <- aws_data %>%
  select(c(`API Name`, `Instance Memory`, vCPUs, `Network Performance`, `Linux On Demand cost`))

# renaming the columns

aws_data_trimmed <- aws_data_trimmed %>% 
  rename(API_Name = `API Name`, Memory_GiB = `Instance Memory`, Network_Gbit = `Network Performance`
         , Linux_Costs_Dollar_per_Hour = `Linux On Demand cost`)

# Keeping only the numeric values

aws_data_trimmed$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed$Memory_GiB))
aws_data_trimmed$Linux_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed$Linux_Costs_Dollar_per_Hour))
aws_data_trimmed$vCPUs <- as.numeric(sub(" .*", "", aws_data_trimmed$vCPUs))
aws_data_trimmed$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed$Network_Gbit))

# Fixing the rows with x * 100 Gbit

aws_data_trimmed$Network_Gbit[aws_data_trimmed$Network_Gbit == 8100] <- 800
aws_data_trimmed$Network_Gbit[aws_data_trimmed$Network_Gbit == 4100] <- 400

# Remove rows with N/A entries

aws_data_trimmed <- na.omit(aws_data_trimmed)

# Adding CPU/$ and GiB/$

aws_data_trimmed$vCPUs_per_Dollar <- aws_data_trimmed$vCPUs / aws_data_trimmed$Linux_Costs_Dollar_per_Hour
aws_data_trimmed$GiB_per_Dollar <- aws_data_trimmed$Memory_GiB / aws_data_trimmed$Linux_Costs_Dollar_per_Hour
aws_data_trimmed$Gbit_per_Dollar <- aws_data_trimmed$Network_Gbit / aws_data_trimmed$Linux_Costs_Dollar_per_Hour

# Example workload price on different instances: Workload running 1 CPU hour and scanning 10 GiB of data

#aws_data_trimmed$Costs_for_one_CPU_hour <- (3600 / aws_data_trimmed$vCPUs) * (aws_data_trimmed$Linux_Costs_Dollar_per_Hour / 3600)

# Approx. handling GiB like GB

aws_data_trimmed$Costs_for_one_CPU_hour_with_Network <- 
  ((3600 / aws_data_trimmed$vCPUs) + (10 * 8) / (aws_data_trimmed$Network_Gbit * 0.8)) * (aws_data_trimmed$Linux_Costs_Dollar_per_Hour / 3600)

# Look at the prices beginning at 32 CPUs

#aws_data_plus_32CPU <- aws_data_trimmed[aws_data_trimmed$vCPUs >= 32, ]

# Look at data with exactly 32 CPUs

#aws_data_32CPU <- aws_data_trimmed[aws_data_trimmed$vCPUs == 32, ]

# Look at data with above 36 CPUs and a minimum of GiB 192

#aws_data_36CPU_192GB <- aws_data_trimmed[aws_data_trimmed$vCPUs >= 36 & aws_data_trimmed$Memory_GiB >= 192, ]

# dataset without burstable instances "t"

aws_data_trimmed_without_burstable <- aws_data_trimmed[!grepl("^t", aws_data_trimmed$API_Name), ]

# dataset with best vCPU/$ instances

aws_best_instances <- aws_data_trimmed[grepl("^m6g\\.|^c6g\\.|^r6g\\.|^a1\\.", aws_data_trimmed$API_Name), ]



# dataset with all price options

aws_data_trimmed_all_prices <- aws_data %>%
  select(c(`API Name`, `Instance Memory`, vCPUs, `Network Performance`, `Linux On Demand cost`, `Linux Reserved cost`, `Linux Spot Minimum cost`))

# renaming the columns

aws_data_trimmed_all_prices <- aws_data_trimmed_all_prices %>% 
  rename(API_Name = `API Name`, Memory_GiB = `Instance Memory`, Network_Gbit = `Network Performance`
         , On_demand_Costs_Dollar_per_Hour = `Linux On Demand cost`, RI_Costs_Dollar_per_Hour = `Linux Reserved cost`, 
         Spot_Costs_Dollar_per_Hour = `Linux Spot Minimum cost`)

# Keeping only the numeric values

aws_data_trimmed_all_prices$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$Memory_GiB))
aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour))
aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour))
aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour))
aws_data_trimmed_all_prices$vCPUs <- as.numeric(sub(" .*", "", aws_data_trimmed_all_prices$vCPUs))
aws_data_trimmed_all_prices$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$Network_Gbit))

# Fixing the rows with x * 100 Gbit

aws_data_trimmed_all_prices$Network_Gbit[aws_data_trimmed_all_prices$Network_Gbit == 8100] <- 800
aws_data_trimmed_all_prices$Network_Gbit[aws_data_trimmed_all_prices$Network_Gbit == 4100] <- 400

# Remove rows with N/A entries

aws_data_trimmed_all_prices <- na.omit(aws_data_trimmed_all_prices)

# Adding CPU/$ and GiB/$ 

aws_data_trimmed_all_prices$vCPUs_per_Dollar_On_Demand <- aws_data_trimmed_all_prices$vCPUs / aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$vCPUs_per_Dollar_RI <- aws_data_trimmed_all_prices$vCPUs / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$vCPUs_per_Dollar_Spot <- aws_data_trimmed_all_prices$vCPUs / aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$GiB_per_Dollar_On_Demand <- aws_data_trimmed_all_prices$Memory_GiB / aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$GiB_per_Dollar_RI <- aws_data_trimmed_all_prices$Memory_GiB / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$GiB_per_Dollar_Spot <- aws_data_trimmed_all_prices$Memory_GiB / aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$Gbit_per_Dollar_On_Demand <- aws_data_trimmed_all_prices$Network_Gbit / aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$Gbit_per_Dollar_RI <- aws_data_trimmed_all_prices$Network_Gbit / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$Gbit_per_Dollar_Spot <- aws_data_trimmed_all_prices$Network_Gbit / aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour

# remove burstable instances "t"

aws_data_trimmed_all_prices <- aws_data_trimmed_all_prices[!grepl("^t", aws_data_trimmed_all_prices$API_Name), ]

