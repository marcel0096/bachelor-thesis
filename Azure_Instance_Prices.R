library(dplyr)

# importing csv export from https://azure-instances.info/?region=us-east

azure_data <- read_csv("Desktop/TUM - Wirtschaftsinformatik B.Sc./Bachelorarbeit/Microsoft Azure Virtual Machine Comparison.csv")

# removing unnecessary columns

azure_data_trimmed <- azure_data %>%
  select(c(Name, Memory, vCPUs, `Expected network bandwidth (Mbps)`, `Linux Pay As You Go cost`))

# renaming the columns

azure_data_trimmed <- azure_data_trimmed %>% 
  rename(API_Name = Name, Memory_GiB = Memory, Network_Gbit = `Expected network bandwidth (Mbps)`, 
         Linux_Costs_Dollar_per_Hour = `Linux Pay As You Go cost`)

# Remove rows with N/A entries

#azure_data_trimmed$Memory_GiB <- na.omit(azure_data_trimmed$Memory_GiB)

# Keeping only the numeric values

azure_data_trimmed$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", azure_data_trimmed$Memory_GiB))
azure_data_trimmed$Linux_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", azure_data_trimmed$Linux_Costs_Dollar_per_Hour))
azure_data_trimmed$vCPUs <- as.numeric(azure_data_trimmed$vCPUs)
azure_data_trimmed$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", azure_data_trimmed$Network_Gbit))

# Convert network bandwidth from Mbit to Gbit

azure_data_trimmed$Network_Gbit <- azure_data_trimmed$Network_Gbit / 1000

# Adding CPU/$ and GiB/$

azure_data_trimmed$vCPUs_per_Dollar <- azure_data_trimmed$vCPUs / azure_data_trimmed$Linux_Costs_Dollar_per_Hour
azure_data_trimmed$GiB_per_Dollar <- azure_data_trimmed$Memory_GiB / azure_data_trimmed$Linux_Costs_Dollar_per_Hour
azure_data_trimmed$Gbit_per_Dollar <- azure_data_trimmed$Network_Gbit / azure_data_trimmed$Linux_Costs_Dollar_per_Hour

# Example workload price on different instances: Workload running 1 CPU hour and scanning 10 GiB of data

#azure_data_trimmed$Costs_for_one_CPU_hour <- (3600 / azure_data_trimmed$vCPUs) * (azure_data_trimmed$Linux_Costs_Dollar_per_Hour / 3600)

# Approx. handling GiB like GB

azure_data_trimmed$Costs_for_one_CPU_hour_with_Network <- 
  ((3600 / azure_data_trimmed$vCPUs) + (10 * 8) / (azure_data_trimmed$Network_Gbit * 0.8)) * (azure_data_trimmed$Linux_Costs_Dollar_per_Hour / 3600)

# Look at the prices beginning at 32 CPUs

#azure_data_plus_32CPU <- azure_data_trimmed[azure_data_trimmed$vCPUs >= 32, ]

# Look at data with exactly 32 CPUs

#azure_data_32CPU <- azure_data_trimmed[azure_data_trimmed$vCPUs == 32, ]

# Look at data with above 36 CPUs and a minimum of GiB 192

#azure_data_36CPU_192GB <- azure_data_trimmed[azure_data_trimmed$vCPUs >= 36 & azure_data_trimmed$Memory_GiB >= 192, ]

# dataset without burstable instances "B"

azure_data_trimmed_without_burstable <- azure_data_trimmed[!grepl("^B", azure_data_trimmed$API_Name), ]

# dataset with best vCPU/$ instances

azure_best_instances <- azure_data_trimmed[grepl("^HB60|^D|^F|^M", azure_data_trimmed$API_Name), ]



# dataset with all price options

azure_data_trimmed_all_prices <- azure_data %>%
  select(c(`Name`, `Memory`, vCPUs, `Expected network bandwidth (Mbps)`, `Linux Pay As You Go cost`, `1 year reserved cost`))

# renaming the columns

azure_data_trimmed_all_prices <- azure_data_trimmed_all_prices %>% 
  rename(API_Name = `Name`, Memory_GiB = `Memory`, Network_Gbit = `Expected network bandwidth (Mbps)`
         , On_demand_Costs_Dollar_per_Hour = `Linux Pay As You Go cost`, RI_Costs_Dollar_per_Hour = `1 year reserved cost`)

# Keeping only the numeric values

azure_data_trimmed_all_prices$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", azure_data_trimmed_all_prices$Memory_GiB))
azure_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", azure_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour))
azure_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", azure_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour))
azure_data_trimmed_all_prices$vCPUs <- as.numeric(sub(" .*", "", azure_data_trimmed_all_prices$vCPUs))
azure_data_trimmed_all_prices$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", azure_data_trimmed_all_prices$Network_Gbit))

# Remove rows with N/A entries

# azure_data_trimmed_all_prices <- na.omit(azure_data_trimmed_all_prices)

# Convert network bandwidth from Mbit to Gbit

azure_data_trimmed_all_prices$Network_Gbit <- azure_data_trimmed_all_prices$Network_Gbit / 1000

# Adding CPU/$ and GiB/$ 

azure_data_trimmed_all_prices$vCPUs_per_Dollar_On_Demand <- azure_data_trimmed_all_prices$vCPUs / azure_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
azure_data_trimmed_all_prices$vCPUs_per_Dollar_RI <- azure_data_trimmed_all_prices$vCPUs / azure_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
azure_data_trimmed_all_prices$GiB_per_Dollar_On_Demand <- azure_data_trimmed_all_prices$Memory_GiB / azure_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
azure_data_trimmed_all_prices$GiB_per_Dollar_RI <- azure_data_trimmed_all_prices$Memory_GiB / azure_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
azure_data_trimmed_all_prices$Gbit_per_Dollar_On_Demand <- azure_data_trimmed_all_prices$Network_Gbit / azure_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
azure_data_trimmed_all_prices$Gbit_per_Dollar_RI <- azure_data_trimmed_all_prices$Network_Gbit / azure_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour

# dataset without burstable instances "B"

azure_data_trimmed_all_prices <- azure_data_trimmed_all_prices[!grepl("^B", azure_data_trimmed_all_prices$API_Name), ]


