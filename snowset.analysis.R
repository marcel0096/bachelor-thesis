source("./util.R")

## ------------------------------------------------------------------------------------------------------------ ##
                                                # Load snowset data 
## ------------------------------------------------------------------------------------------------------------ ##

snowset <- read.csv.sql("~/Downloads/snowset-main.csv", sql = "select * 
                                                               from file 
                                                               WHERE DATE(createdTime) = '2018-02-22'")


## ------------------------------------------------------------------------------------------------------------ ##
                                                  # Analyze data 
## ------------------------------------------------------------------------------------------------------------ ##

# get overview of CPU hour demand of all warehouses
snowset.overview <- aggregate((userCpuTime + systemCpuTime) ~ warehouseId, data = snowset, FUN = sum)
snowset.overview[, 2] <- snowset.overview[, 2] / 1000 / 1000 / 3600
snowset.overview[, 1] <- as.character(snowset.overview[, 1])
mean(snowset.overview[, 2])

# select interesting warehouses based on https://github.com/alexandervanrenen/cab and own analysis

# (1890180489335197062),(8959409253890846472),(2098601964786088346),(7249649289217758973),(2831213012224265896)) - large
# (5217126301665207861),(8063056532299142429),(9115822091017760299),(4702844907506457136),(3674929441415926268)) - small

snowset.filtered <- snowset[(snowset$warehouseId == 8959409253890846472 | 
                             snowset$warehouseId == 1890180489335197062 |
                             snowset$warehouseId == 2098601964786088346 |
                             snowset$warehouseId == 2831213012224265896 |
                             snowset$warehouseId == 7249649289217758973 |
                             snowset$warehouseId == 5217126301665207861 |
                             snowset$warehouseId == 8063056532299142429 |
                             snowset$warehouseId == 9115822091017760299 |
                             snowset$warehouseId == 4702844907506457136 |
                             snowset$warehouseId == 3674929441415926268 |
                             snowset$warehouseId == 3817594216701320192), ]

snowset.filtered <- select(snowset.filtered, c(queryId, warehouseId, createdTime, userCpuTime, systemCpuTime))
snowset.filtered$totalCpuTime <- (snowset.filtered$userCpuTime + snowset.filtered$systemCpuTime) / 1000 / 1000 / 3600
snowset.summed <- aggregate(totalCpuTime ~ warehouseId, data = snowset.filtered, sum)
snowset.summed$query.count <- aggregate(queryId ~ warehouseId, data = snowset.filtered, FUN = length)

# Not used -> Largest warehouse
snowset.largest.cust <- snowset.filtered[snowset.filtered$warehouseId == 1890180489335197062, ]

# Used as large example
snowset.large.cust <- snowset.filtered[snowset.filtered$warehouseId == 7249649289217758973, ]

# Used as middle example
snowset.middle.cust <- snowset.filtered[snowset.filtered$warehouseId == 4702844907506457136, ]

# Used as small example
snowset.small.cust <- snowset.filtered[snowset.filtered$warehouseId == 3817594216701320192, ]
  

## ------------------------------------------------------------------------------------------------------------ ##
                                                # Prepare selected data 
## ------------------------------------------------------------------------------------------------------------ ##

# Make CPU chunks of every hour - large data set
snowset.large.cust$createdTime <- ymd_hms(snowset.large.cust$createdTime, tz = "UTC", quiet = TRUE)
snowset.large.cust$hour_group <- floor_date(snowset.large.cust$createdTime, "hour")
snowset.large.cust.hourly <- aggregate(totalCpuTime ~ hour_group, data = snowset.large.cust, FUN = sum)

# Make CPU chunks of every hour - middle data set
snowset.middle.cust$createdTime <- ymd_hms(snowset.middle.cust$createdTime, tz = "UTC", quiet = TRUE)
snowset.middle.cust$hour_group <- floor_date(snowset.middle.cust$createdTime, "hour")
snowset.middle.cust.hourly <- aggregate(totalCpuTime ~ hour_group, data = snowset.middle.cust, FUN = sum)

# Make CPU chunks of every hour - small data set
snowset.small.cust$createdTime <- ymd_hms(snowset.small.cust$createdTime, tz = "UTC", quiet = TRUE)
snowset.small.cust$hour_group <- floor_date(snowset.small.cust$createdTime, "hour")
snowset.small.cust.hourly <- aggregate(totalCpuTime ~ hour_group, data = snowset.small.cust, FUN = sum)
snowset.small.cust.hourly <- merge(data.frame(hour_group = seq(as.POSIXct("2018-02-22 00:00:00", tz = "UTC"), 
                                                               as.POSIXct("2018-02-22 23:00:00", tz = "UTC"), by = "hour")), 
                                   snowset.small.cust.hourly, by = "hour_group", all.x = TRUE)
snowset.small.cust.hourly$totalCpuTime[is.na(snowset.small.cust.hourly$totalCpuTime)] <- 0

# Make CPU chunks of every hour - largest data set
snowset.largest.cust$createdTime <- ymd_hms(snowset.largest.cust$createdTime, tz = "UTC", quiet = TRUE)
snowset.largest.cust$hour_group <- floor_date(snowset.largest.cust$createdTime, "hour")
snowset.largest.cust.hourly <- aggregate(totalCpuTime ~ hour_group, data = snowset.largest.cust, FUN = sum)