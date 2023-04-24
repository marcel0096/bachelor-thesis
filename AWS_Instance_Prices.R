library(dplyr)

# importing csv export from https://instances.vantage.sh, one dataset with 1 year RI prices, one dataset with 3 year RI prices

aws_data1 <- read.csv("~/Desktop/TUM - Wirtschaftsinformatik B.Sc./Bachelorarbeit/Instance data/EC2_data_1_year_RI.csv")
aws_data2 <- read.csv("~/Desktop/TUM - Wirtschaftsinformatik B.Sc./Bachelorarbeit/Instance data/EC2_data_3_year_RI.csv")

# removing unnecessary columns from aws_data1

aws_data_trimmed_all_prices <- aws_data1 %>%
  select(c(`API.Name`, `Instance.Memory`, vCPUs, `Network.Performance`, `Linux.On.Demand.cost`, `Linux.Reserved.cost`, `Linux.Spot.Minimum.cost`))

# renaming the columns

aws_data_trimmed_all_prices <- aws_data_trimmed_all_prices %>% 
  rename(API_Name = `API.Name`, Memory_GiB = `Instance.Memory`, Network_Gbit = `Network.Performance`
         , On_demand_Costs_Dollar_per_Hour = `Linux.On.Demand.cost`, RI_Costs_Dollar_per_Hour = `Linux.Reserved.cost`, 
         Spot_Costs_Dollar_per_Hour = `Linux.Spot.Minimum.cost`)

# keeping only the RI prices from aws_data2

...

# adding the prices to the trimmed dataset

...

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

