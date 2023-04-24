library(dplyr)

# importing csv export from https://gcpinstances.doit-intl.com/?region=us-east1

gcp_data <- read_csv("Desktop/TUM - Wirtschaftsinformatik B.Sc./Bachelorarbeit/GCP Compute Engine Instance Comparison.csv")

# removing unnecessary columns

gcp_data_trimmed <- gcp_data %>%
  select(c(`Machine type`, Memory, vCPUs, `Network performance`, `Linux On Demand cost`))

# renaming the columns

gcp_data_trimmed <- gcp_data_trimmed %>% 
  rename(API_Name = `Machine type`, Memory_GiB = Memory, Network_Gbit = `Network performance`, 
         Linux_Costs_Dollar_per_Hour = `Linux On Demand cost`)

# Keeping only the numeric values

gcp_data_trimmed$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed$Memory_GiB))
gcp_data_trimmed$Linux_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed$Linux_Costs_Dollar_per_Hour))
gcp_data_trimmed$vCPUs <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed$vCPUs))
gcp_data_trimmed$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed$Network_Gbit))

# Remove rows with N/A entries

#gcp_data_trimmed <- na.omit(gcp_data_trimmed)

# Adding CPU/$ and GiB/$

gcp_data_trimmed$vCPUs_per_Dollar <- gcp_data_trimmed$vCPUs / gcp_data_trimmed$Linux_Costs_Dollar_per_Hour
gcp_data_trimmed$GiB_per_Dollar <- gcp_data_trimmed$Memory_GiB / gcp_data_trimmed$Linux_Costs_Dollar_per_Hour
gcp_data_trimmed$Gbit_per_Dollar <- gcp_data_trimmed$Network_Gbit / gcp_data_trimmed$Linux_Costs_Dollar_per_Hour

# Example workload price on different instances: Workload running 1 CPU hour and scanning 10 GiB of data

#gcp_data_trimmed$Costs_for_one_CPU_hour <- (3600 / gcp_data_trimmed$vCPUs) * (gcp_data_trimmed$Linux_Costs_Dollar_per_Hour / 3600)

# Approx. handling GiB like GB

gcp_data_trimmed$Costs_for_one_CPU_hour_with_Network <- 
  ((3600 / gcp_data_trimmed$vCPUs) + (10 * 8) / (gcp_data_trimmed$Network_Gbit * 0.8)) * (gcp_data_trimmed$Linux_Costs_Dollar_per_Hour / 3600)

# Look at the prices beginning at 32 CPUs

#gcp_data_plus_32CPU <- gcp_data_trimmed[gcp_data_trimmed$vCPUs >= 32, ]

# Look at data with exactly 32 CPUs

#gcp_data_32CPU <- gcp_data_trimmed[gcp_data_trimmed$vCPUs == 32, ]

# Look at data with above 36 CPUs and a minimum of GiB 192

#gcp_data_36CPU_192GB <- gcp_data_trimmed[gcp_data_trimmed$vCPUs >= 36 & gcp_data_trimmed$Memory_GiB >= 192, ]



# dataset with all price options

gcp_data_trimmed_all_prices <- gcp_data %>%
  select(c(`Machine type`, `Memory`, vCPUs, `Network performance`, `Linux On Demand cost`, `Linux 1 year CUD cost`, `Linux Preemptible cost`))

# renaming the columns

gcp_data_trimmed_all_prices <- gcp_data_trimmed_all_prices %>% 
  rename(API_Name = `Machine type`, Memory_GiB = `Memory`, Network_Gbit = `Network performance`
         , On_demand_Costs_Dollar_per_Hour = `Linux On Demand cost`, RI_Costs_Dollar_per_Hour = `Linux 1 year CUD cost`, 
         Spot_Costs_Dollar_per_Hour = `Linux Preemptible cost`)

# Keeping only the numeric values

gcp_data_trimmed_all_prices$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed_all_prices$Memory_GiB))
gcp_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour))
gcp_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour))
gcp_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour))
gcp_data_trimmed_all_prices$vCPUs <- as.numeric(sub(" .*", "", gcp_data_trimmed_all_prices$vCPUs))
gcp_data_trimmed_all_prices$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", gcp_data_trimmed_all_prices$Network_Gbit))

# Remove rows with N/A entries

# azure_data_trimmed_all_prices <- na.omit(azure_data_trimmed_all_prices)


# Adding CPU/$ and GiB/$ 

gcp_data_trimmed_all_prices$vCPUs_per_Dollar_On_Demand <- gcp_data_trimmed_all_prices$vCPUs / gcp_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$vCPUs_per_Dollar_RI <- gcp_data_trimmed_all_prices$vCPUs / gcp_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$vCPUs_per_Dollar_Spot <- gcp_data_trimmed_all_prices$vCPUs / gcp_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$GiB_per_Dollar_On_Demand <- gcp_data_trimmed_all_prices$Memory_GiB / gcp_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$GiB_per_Dollar_RI <- gcp_data_trimmed_all_prices$Memory_GiB / gcp_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$GiB_per_Dollar_Spot <- gcp_data_trimmed_all_prices$Memory_GiB / gcp_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$Gbit_per_Dollar_On_Demand <- gcp_data_trimmed_all_prices$Network_Gbit / gcp_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$Gbit_per_Dollar_RI <- gcp_data_trimmed_all_prices$Network_Gbit / gcp_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
gcp_data_trimmed_all_prices$Gbit_per_Dollar_Spot <- gcp_data_trimmed_all_prices$Network_Gbit / gcp_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour


# dataset without burstable instances "B"

gcp_data_trimmed_all_prices <- gcp_data_trimmed_all_prices[!grepl("^B", gcp_data_trimmed_all_prices$API_Name), ]


