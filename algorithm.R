# ---------------------- DATA PREPARATION ---------------------- # 

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

# keeping only the RI prices from aws_data2 and rename the column

aws_data2 <- aws_data2 %>%
  select(c(`Linux.Reserved.cost`))

# adding the prices to the trimmed dataset after RIs for 1 year

aws_data_trimmed_all_prices <- cbind(aws_data_trimmed_all_prices[, 1:6], aws_data2$Linux.Reserved.cost, aws_data_trimmed_all_prices[, 7])
colnames(aws_data_trimmed_all_prices)[7] = "RI_Costs_Dollar_per_Hour_3_years"
colnames(aws_data_trimmed_all_prices)[8] = "Spot_Costs_Dollar_per_Hour"

# Keeping only the numeric values

aws_data_trimmed_all_prices$Memory_GiB <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$Memory_GiB))
aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour))
aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour))
aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour_3_years <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour_3_years))
aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour))
aws_data_trimmed_all_prices$vCPUs <- as.numeric(sub(" .*", "", aws_data_trimmed_all_prices$vCPUs))
aws_data_trimmed_all_prices$Network_Gbit <- as.numeric(gsub("[^0-9.]", "", aws_data_trimmed_all_prices$Network_Gbit))

# Fixing the rows with x * 100 Gbit

aws_data_trimmed_all_prices$Network_Gbit[aws_data_trimmed_all_prices$Network_Gbit == 16100] <- 1600
aws_data_trimmed_all_prices$Network_Gbit[aws_data_trimmed_all_prices$Network_Gbit == 8100] <- 800
aws_data_trimmed_all_prices$Network_Gbit[aws_data_trimmed_all_prices$Network_Gbit == 4100] <- 400

# Remove rows with N/A entries

aws_data_trimmed_all_prices <- na.omit(aws_data_trimmed_all_prices)

# remove burstable instances "t"

aws_data_trimmed_all_prices <- aws_data_trimmed_all_prices[!grepl("^t", aws_data_trimmed_all_prices$API_Name), ]

# Adding CPU/$ and GiB/$ 

aws_data_trimmed_all_prices$vCPUs_per_Dollar_On_Demand <- aws_data_trimmed_all_prices$vCPUs / aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$vCPUs_per_Dollar_RI <- aws_data_trimmed_all_prices$vCPUs / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$vCPUs_per_Dollar_RI_3_years <- aws_data_trimmed_all_prices$vCPUs / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour_3_years
aws_data_trimmed_all_prices$vCPUs_per_Dollar_Spot <- aws_data_trimmed_all_prices$vCPUs / aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour

aws_data_trimmed_all_prices$GiB_per_Dollar_On_Demand <- aws_data_trimmed_all_prices$Memory_GiB / aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$GiB_per_Dollar_RI <- aws_data_trimmed_all_prices$Memory_GiB / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$GiB_per_Dollar_RI_3_years <- aws_data_trimmed_all_prices$Memory_GiB / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour_3_years
aws_data_trimmed_all_prices$GiB_per_Dollar_Spot <- aws_data_trimmed_all_prices$Memory_GiB / aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour

aws_data_trimmed_all_prices$Gbit_per_Dollar_On_Demand <- aws_data_trimmed_all_prices$Network_Gbit / aws_data_trimmed_all_prices$On_demand_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$Gbit_per_Dollar_RI <- aws_data_trimmed_all_prices$Network_Gbit / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour
aws_data_trimmed_all_prices$Gbit_per_Dollar_RI_3_years <- aws_data_trimmed_all_prices$Network_Gbit / aws_data_trimmed_all_prices$RI_Costs_Dollar_per_Hour_3_years
aws_data_trimmed_all_prices$Gbit_per_Dollar_Spot <- aws_data_trimmed_all_prices$Network_Gbit / aws_data_trimmed_all_prices$Spot_Costs_Dollar_per_Hour

# add dataset without instances "a"

aws_data_trimmed_all_prices_2 <- aws_data_trimmed_all_prices[!grepl("^a", aws_data_trimmed_all_prices$API_Name), ]

# add dataset with example workload
# Example workload price on different instances: Workload running 1 CPU hour and scanning 10 GiB of data -> 1h/vCPUs + (10*8 Gbit / networkgbit * 0.8)) * costs

aws_data_trimmed_all_prices_3 <- aws_data_trimmed_all_prices_2
aws_data_trimmed_all_prices_3$Workload <- 
  ((3600 / aws_data_trimmed_all_prices_3$vCPUs) + (10 * 8) / (aws_data_trimmed_all_prices_3$Network_Gbit * 0.8)) * (aws_data_trimmed_all_prices_3$On_demand_Costs_Dollar_per_Hour / 3600)




# ---------------------- ALGORITHM FOR ON DEMAND ---------------------- # 

# Implementing algorithm to find cheapest On demand instance for given CPU/h usage (e.g. 240 CPU hours are required per day) -> using only vCPUs as metric

# base workload ist ein vCPU/h wert, schwankender workload als array von 24 werten

find_cheapest_instance_on_demand <- function(CPU_hours_per_day) {
  
  CPU_hours_per_hour <- CPU_hours_per_day / 24
  current_instance_price <- Inf
  cheapest_price <- Inf
  cheapest_instance <- ""
  result <- list()
  
  # iterate through all rows in data frame to find cheapest instances
  for (i in 1:nrow(aws_data_trimmed_all_prices_2)) {
    
    instance_vCPUs <- aws_data_trimmed_all_prices_2[i, "vCPUs"]
    instance_name <- aws_data_trimmed_all_prices_2[i, "API_Name"]
    instance_on_demand_price <- aws_data_trimmed_all_prices_2[i, "On_demand_Costs_Dollar_per_Hour"]
    
    # does one instance of the instance type alone have enough vCPUs to serve the need?
    if (instance_vCPUs < CPU_hours_per_hour) {
      # if not, calculate how many instances would be required and add the prices
      number_of_instances_required <- ceiling(CPU_hours_per_hour / instance_vCPUs)
      current_instance_price <- instance_on_demand_price * number_of_instances_required
      
      # is it also the cheapest so far?
      if (current_instance_price <= cheapest_price) {
        result <- append(result, paste(instance_name, ": ", current_instance_price))
        cheapest_price <- current_instance_price
      }
      
    } else {
      # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
      current_instance_price <- instance_on_demand_price 
      
      # is it also the cheapest so far?
      if (current_instance_price <= cheapest_price) {
        result <- append(result, paste(instance_name, ": ", current_instance_price))
        cheapest_price <- current_instance_price
      }
    }
  }
  return(result)
}

find_cheapest_instance_on_demand(24000)

# Next:
#   - Savings Plans mit einbauen -> Annahme ist, dass der workload ewig läuft -> dementsprechend sollte 3 Jahresplan am besten sein (was ist mit RIs? Daten dafür?)
#   - Daten sind für RIs -> gleich wie "Instance Savings Plans" -> für compute savings plans brauche ich noch die Daten von AWS -> dann kann ich für alles berechnen außer Spot
#   - Ausgabe verbesseren und verschönern: Wie heißt die Instanz? Wie viele brauche ich davon? Wie teuer wird das? In welchem Plan soll man das kaufen?
#   - Schwankenden Workload einbauen durch das mitgeben eines arrays mit 24 werten
#   - Eingabe direkt mit CPU hours pro Stunde und nicht pro Tag
#   - Bei kleinen Instanzen nicht linear skalieren sondern immer mit einem Faktor sodass sich Leistung z.B. nicht verdoppelt, sondern nur ver-1,8-facht





