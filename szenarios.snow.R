source("./model.R")
source("./snowset.analysis.R")

## ------------------------------------------------------------------------------------------------------------ ##
                      # VH-0: Largest virtual warehouse in snowset (not used in thesis)
## ------------------------------------------------------------------------------------------------------------ ##

# base workload only
snow.szenario.1.1 <- function() {
  CPU.hours.required <- max(snowset.largest.cust.hourly$totalCpuTime)
  CPU.hours.fluctuating <- 0
  
  snow.szenario.1.1 <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0, amdahl.param = 0.95)
  snow.szenario.1.1.total.daily <<- snow.szenario.1.1[1, 'Total_Costs'] * 24
}
snow.szenario.1.1()

# fluct workload only
snow.szenario.1.2 <- function() {
  CPU.hours.required <- 0
  CPU.hours.fluctuating <- as.list(snowset.largest.cust.hourly$totalCpuTime)
  
  # avg
  snow.szenario.1.2.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  snow.szenario.1.2.total.daily.avg <<- sum(snow.szenario.1.2.avg[2:25, 'Total_Costs']) 
  
  # min
  snow.szenario.1.2.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  snow.szenario.1.2.total.daily.min <<- sum(snow.szenario.1.2.min[2:25, 'Total_Costs']) 
}
snow.szenario.1.2()

# approaching cost-optimality
snow.szenario.1.3 <- function() {
  CPU.hours.required <- 344
  CPU.hours.fluctuating <- as.list(ifelse((snowset.largest.cust.hourly$totalCpuTime - 344) < 0, 
                                          0, (snowset.largest.cust.hourly$totalCpuTime - 344)))

  # avg
  snow.szenario.1.3.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg <- snow.szenario.1.3.avg[1, 'Total_Costs'] * 24 
  total.price.fluct.avg <- sum(snow.szenario.1.3.avg[2:25, 'Total_Costs']) 
  snow.szenario.1.3.total.daily.avg <<- total.price.base.avg + total.price.fluct.avg

  # min
  snow.szenario.1.3.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min <- snow.szenario.1.3.min[1, 'Total_Costs'] * 24 
  total.price.fluct.min <- sum(snow.szenario.1.3.min[2:25, 'Total_Costs'])
  snow.szenario.1.3.total.daily.min <<- total.price.base.min + total.price.fluct.min
}
snow.szenario.1.3()

# cost-optimality
snow.szenario.1.4 <- function() {
  CPU.hours.required <- 472
  CPU.hours.fluctuating <- as.list(ifelse((snowset.largest.cust.hourly$totalCpuTime - 472) < 0, 
                                          0, (snowset.largest.cust.hourly$totalCpuTime - 472)))
  
  snow.szenario.1.4.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg <- snow.szenario.1.4.avg[1, 'Total_Costs'] * 24
  total.price.fluct.avg <- sum(snow.szenario.1.4.avg[2:25, 'Total_Costs']) 
  snow.szenario.1.4.total.daily.avg <<- total.price.base.avg + total.price.fluct.avg
  
  snow.szenario.1.4.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min <- snow.szenario.1.4.min[1, 'Total_Costs'] * 24 
  total.price.fluct.min <- sum(snow.szenario.1.4.min[2:25, 'Total_Costs']) 
  snow.szenario.1.4.total.daily.min <<- total.price.base.min + total.price.fluct.min
}
snow.szenario.1.4()


## ------------------------------------------------------------------------------------------------------------ ##
                                        # VH-1: Large virtual warehouse
## ------------------------------------------------------------------------------------------------------------ ##

# Base workload only
snow.szenario.2.1 <- function() {
  CPU.hours.required <- max(snowset.large.cust.hourly$totalCpuTime)
  CPU.hours.fluctuating <- 0
  
  snow.szenario.2.1 <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0, amdahl.param = 0.95)
  
  snow.szenario.2.1.total.hourly <<- snow.szenario.2.1[1, 'Total_Costs']
  snow.szenario.2.1.total.daily <<- snow.szenario.2.1[1, 'Total_Costs'] * 24 
  snow.szenario.2.1.total.monthly <<- snow.szenario.2.1[1, 'Total_Costs'] * 24 * 30
}
snow.szenario.2.1()

# Fluct workload only
snow.szenario.2.2 <- function() {
  CPU.hours.required <- 0
  CPU.hours.fluctuating <- as.list(snowset.large.cust.hourly$totalCpuTime)
  
  # avg
  snow.szenario.2.2.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  snow.szenario.2.2.total.daily.avg <<- sum(snow.szenario.2.2.avg[2:25, 'Total_Costs'])
  snow.szenario.2.2.total.monthly.avg <<- sum(snow.szenario.2.2.avg[2:25, 'Total_Costs']) * 30
  
  # min
  snow.szenario.2.2.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  snow.szenario.2.2.total.daily.min <<- sum(snow.szenario.2.2.min[2:25, 'Total_Costs'])
  snow.szenario.2.2.total.monthly.min <<- sum(snow.szenario.2.2.min[2:25, 'Total_Costs']) * 30
}
snow.szenario.2.2()

# approaching cost-optimality
snow.szenario.2.3 <- function() {
  CPU.hours.required <- 123
  CPU.hours.fluctuating <- as.list(ifelse((snowset.large.cust.hourly$totalCpuTime - 123) < 0, 
                                          0, (snowset.large.cust.hourly$totalCpuTime - 123)))
  
  # avg
  snow.szenario.2.3.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg.daily <- snow.szenario.2.3.avg[1, 'Total_Costs'] * 24
  total.price.fluct.avg.daily <- sum(snow.szenario.2.3.avg[2:25, 'Total_Costs'])
  snow.szenario.2.3.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.2.3.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.2.3.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.2.3.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
  
  # min
  snow.szenario.2.3.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min.daily <- snow.szenario.2.3.min[1, 'Total_Costs'] * 24
  total.price.fluct.min.daily <- sum(snow.szenario.2.3.min[2:25, 'Total_Costs'])
  snow.szenario.2.3.total.daily.min <<- total.price.base.min.daily + total.price.fluct.min.daily
  
  total.price.base.min.monthly <- snow.szenario.2.3.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min.monthly <- sum(snow.szenario.2.3.min[2:25, 'Total_Costs']) * 30
  snow.szenario.2.3.total.monthly.min <<- total.price.base.min.monthly + total.price.fluct.min.monthly
}
snow.szenario.2.3()

# cost-optimality
# -> with 96 base     # -> with 64 base   # -> with 80 base
# 56.14               # 59.89             # 56.64
# 50.26               # 48.93             # 50.70
snow.szenario.2.4 <- function() {
  CPU.hours.required <- 96
  CPU.hours.fluctuating <- as.list(ifelse((snowset.large.cust.hourly$totalCpuTime - 96) < 0, 
                                          0, (snowset.large.cust.hourly$totalCpuTime - 96)))
  
  # avg
  snow.szenario.2.4.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg.daily <- snow.szenario.2.4.avg[1, 'Total_Costs'] * 24 
  total.price.fluct.avg.daily <- sum(snow.szenario.2.4.avg[2:25, 'Total_Costs']) 
  snow.szenario.2.4.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.2.4.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.2.4.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.2.4.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
  
  # min
  snow.szenario.2.4.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min.daily <- snow.szenario.2.4.min[1, 'Total_Costs'] * 24 
  total.price.fluct.min.daily <- sum(snow.szenario.2.4.min[2:25, 'Total_Costs']) 
  snow.szenario.2.4.total.daily.min <<- total.price.base.min.daily + total.price.fluct.min.daily
  
  total.price.base.min.monthly <- snow.szenario.2.4.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min.monthly <- sum(snow.szenario.2.4.min[2:25, 'Total_Costs']) * 30
  snow.szenario.2.4.total.monthly.min <<- total.price.base.min.monthly + total.price.fluct.min.monthly
}
snow.szenario.2.4()


## ------------------------------------------------------------------------------------------------------------ ##
                                        # VH-2: Middle-sized virtual warehouse
## ------------------------------------------------------------------------------------------------------------ ##

# Base workload only
snow.szenario.3.1 <- function() {
  CPU.hours.required <- max(snowset.middle.cust.hourly$totalCpuTime)
  CPU.hours.fluctuating <- 0
  
  snow.szenario.3.1 <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0)
  
  snow.szenario.3.1.total.hourly <<- snow.szenario.3.1[1, 'Total_Costs']
  snow.szenario.3.1.total.daily <<- snow.szenario.3.1[1, 'Total_Costs'] * 24 
  snow.szenario.3.1.total.monthly <<- snow.szenario.3.1[1, 'Total_Costs'] * 24 * 30
}
snow.szenario.3.1()

# Fluct workload only
snow.szenario.3.2 <- function() {
  CPU.hours.required <- 0
  CPU.hours.fluctuating <- as.list(snowset.middle.cust.hourly$totalCpuTime)
  
  # avg
  snow.szenario.3.2.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 1)
  snow.szenario.3.2.total.daily.avg <<- sum(snow.szenario.3.2.avg[2:25, 'Total_Costs'])
  snow.szenario.3.2.total.monthly.avg <<- sum(snow.szenario.3.2.avg[2:25, 'Total_Costs']) * 30
  
  # min
  snow.szenario.3.2.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 1)
  snow.szenario.3.2.total.daily.min <<- sum(snow.szenario.3.2.min[2:25, 'Total_Costs'])
  snow.szenario.3.2.total.monthly.min <<- sum(snow.szenario.3.2.min[2:25, 'Total_Costs']) * 30
}
snow.szenario.3.2()

# approaching cost-optimality
snow.szenario.3.3 <- function() {
  CPU.hours.required <- 16
  CPU.hours.fluctuating <- as.list(ifelse((snowset.middle.cust.hourly$totalCpuTime - 16) < 0, 
                                          0, (snowset.middle.cust.hourly$totalCpuTime - 16)))
  
  # avg
  snow.szenario.3.3.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.avg.daily <- snow.szenario.3.3.avg[1, 'Total_Costs'] * 24
  total.price.fluct.avg.daily <- sum(snow.szenario.3.3.avg[2:25, 'Total_Costs'])
  snow.szenario.3.3.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.3.3.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.3.3.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.3.3.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
  
  # min
  snow.szenario.3.3.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.min.daily <- snow.szenario.3.3.min[1, 'Total_Costs'] * 24
  total.price.fluct.min.daily <- sum(snow.szenario.3.3.min[2:25, 'Total_Costs'])
  snow.szenario.3.3.total.daily.min <<- total.price.base.min.daily + total.price.fluct.min.daily
  
  total.price.base.min.monthly <- snow.szenario.3.3.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min.monthly <- sum(snow.szenario.3.3.min[2:25, 'Total_Costs']) * 30
  snow.szenario.3.3.total.monthly.min <<- total.price.base.min.monthly + total.price.fluct.min.monthly
}
snow.szenario.3.3()

# cost-optimality
# -> with 16 base     # -> with 12 base   # -> with 8 base  # -> with 4 base  # -> with 2 base  # -> with 32 base  # -> 24 base
# avg: 7.60           # 8.25              # 7.02            # 8.60            # 9.24            # 11.23            # 10.37
# min: 6.42           # 6.96              # 5.80            # 6.62            # 7.05            # 10.31            # 9.24
snow.szenario.3.4 <- function() {
  CPU.hours.required <- 8
  CPU.hours.fluctuating <- as.list(ifelse((snowset.middle.cust.hourly$totalCpuTime - 8) < 0, 
                                          0, (snowset.middle.cust.hourly$totalCpuTime - 8)))
  
  # avg
  snow.szenario.3.4.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.avg.daily <- snow.szenario.3.4.avg[1, 'Total_Costs'] * 24
  total.price.fluct.avg.daily <- sum(snow.szenario.3.4.avg[2:25, 'Total_Costs'])
  snow.szenario.3.4.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.3.4.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.3.4.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.3.4.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
  
  # min
  snow.szenario.3.4.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.min.daily <- snow.szenario.3.4.min[1, 'Total_Costs'] * 24
  total.price.fluct.min.daily <- sum(snow.szenario.3.4.min[2:25, 'Total_Costs'])
  snow.szenario.3.4.total.daily.min <<- total.price.base.min.daily + total.price.fluct.min.daily
  
  total.price.base.min.monthly <- snow.szenario.3.4.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min.monthly <- sum(snow.szenario.3.4.min[2:25, 'Total_Costs']) * 30
  snow.szenario.3.4.total.monthly.min <<- total.price.base.min.monthly + total.price.fluct.min.monthly
}
snow.szenario.3.4()


## ------------------------------------------------------------------------------------------------------------ ##
                                          # VH-3: Small virtual warehouse
## ------------------------------------------------------------------------------------------------------------ ##

# Base workload only
snow.szenario.4.1 <- function() {
  CPU.hours.required <- max(snowset.small.cust.hourly$totalCpuTime)
  CPU.hours.fluctuating <- 0
  
  snow.szenario.4.1 <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0)
  
  snow.szenario.4.1.total.hourly <<- snow.szenario.4.1[1, 'Total_Costs']
  snow.szenario.4.1.total.daily <<- snow.szenario.4.1[1, 'Total_Costs'] * 24 
  snow.szenario.4.1.total.monthly <<- snow.szenario.4.1[1, 'Total_Costs'] * 24 * 30
}
snow.szenario.4.1()

# Fluct workload only
snow.szenario.4.2 <- function() {
  CPU.hours.required <- 0
  CPU.hours.fluctuating <- as.list(snowset.small.cust.hourly$totalCpuTime)
  
  snow.szenario.4.2.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0.5)
  snow.szenario.4.2.total.daily.avg <<- sum(snow.szenario.4.2.avg[2:25, 'Total_Costs'])
  snow.szenario.4.2.total.monthly.avg <<- sum(snow.szenario.4.2.avg[2:25, 'Total_Costs']) * 30
  
  snow.szenario.4.2.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 0.5)
  snow.szenario.4.2.total.daily.min <<- sum(snow.szenario.4.2.min[2:25, 'Total_Costs'])
  snow.szenario.4.2.total.monthly.min <<- sum(snow.szenario.4.2.min[2:25, 'Total_Costs']) * 30
}
snow.szenario.4.2()

# approaching cost-optimality
snow.szenario.4.3 <- function() {
  CPU.hours.required <- 2
  CPU.hours.fluctuating <- as.list(ifelse((snowset.small.cust.hourly$totalCpuTime - 2) < 0, 
                                          0, (snowset.small.cust.hourly$totalCpuTime - 2)))
  
  # avg
  snow.szenario.4.3.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0.5)
  total.price.base.avg.daily <- snow.szenario.4.3.avg[1, 'Total_Costs'] * 24
  total.price.fluct.avg.daily <- sum(snow.szenario.4.3.avg[2:25, 'Total_Costs'])
  snow.szenario.4.3.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.4.3.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.4.3.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.4.3.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
  
  # min
  snow.szenario.4.3.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 0.5)
  total.price.base.min.daily <- snow.szenario.4.3.min[1, 'Total_Costs'] * 24
  total.price.fluct.min.daily <- sum(snow.szenario.4.3.min[2:25, 'Total_Costs'])
  snow.szenario.4.3.total.daily.min <<- total.price.base.min.daily + total.price.fluct.min.daily
  
  total.price.base.min.monthly <- snow.szenario.4.3.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min.monthly <- sum(snow.szenario.4.3.min[2:25, 'Total_Costs']) * 30
  snow.szenario.4.3.total.monthly.min <<- total.price.base.min.monthly + total.price.fluct.min.monthly
}
snow.szenario.4.3()

# cost-optimality
# -> with 1 base       # -> with 2 base  # -> with 4 base  # -> with 3 base
# 1.43                 # 1.14            # 0.95            # 1.28            
# 1.02                 # 0.95            # 0.94            # 1.20           
snow.szenario.4.4 <- function() {
  CPU.hours.required <- 4
  CPU.hours.fluctuating <- as.list(ifelse((snowset.small.cust.hourly$totalCpuTime - 4) < 0, 
                                          0, (snowset.small.cust.hourly$totalCpuTime - 4)))
  
  # avg
  snow.szenario.4.4.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0.5)
  total.price.base.avg.daily <- snow.szenario.4.4.avg[1, 'Total_Costs'] * 24
  total.price.fluct.avg.daily <- sum(snow.szenario.4.4.avg[2:25, 'Total_Costs'])
  snow.szenario.4.4.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.4.4.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.4.4.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.4.4.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
  
  # min
  snow.szenario.4.4.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 0.5)
  total.price.base.min.daily <- snow.szenario.4.4.min[1, 'Total_Costs'] * 24
  total.price.fluct.min.daily <- sum(snow.szenario.4.4.min[2:25, 'Total_Costs'])
  snow.szenario.4.4.total.daily.min <<- total.price.base.min.daily + total.price.fluct.min.daily
  
  total.price.base.min.monthly <- snow.szenario.4.4.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min.monthly <- sum(snow.szenario.4.4.min[2:25, 'Total_Costs']) * 30
  snow.szenario.4.4.total.monthly.min <<- total.price.base.min.monthly + total.price.fluct.min.monthly
}
snow.szenario.4.4()


## ------------------------------------------------------------------------------------------------------------ ##
                                          # Exploring optional parameters
## ------------------------------------------------------------------------------------------------------------ ##

## Alternating pricing parameters for VH-2
snow.szenario.5.1 <- function() {
  CPU.hours.required <- 8
  CPU.hours.fluctuating <- as.list(ifelse((snowset.middle.cust.hourly$totalCpuTime - 8) < 0, 
                                          0, (snowset.middle.cust.hourly$totalCpuTime - 8)))
  
  # avg
  snow.szenario.5.1.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 1, 
                                                         duration = "1", payment = "No", plan = "SP", type = "Compute")
  total.price.base.avg.daily <- snow.szenario.5.1.avg[1, 'Total_Costs'] * 24
  total.price.fluct.avg.daily <- sum(snow.szenario.5.1.avg[2:25, 'Total_Costs'])
  snow.szenario.5.1.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.5.1.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.5.1.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.5.1.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
}
snow.szenario.5.1()


## Alternating amdahl for VH-1
snow.szenario.6.1 <- function() {
  
  CPU.hours.required <- 96
  CPU.hours.fluctuating <- as.list(ifelse((snowset.large.cust.hourly$totalCpuTime - 96) < 0, 
                                          0, (snowset.large.cust.hourly$totalCpuTime - 96)))
  
  # avg
  snow.szenario.6.1.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2, amdahl.param = 0.99)
  total.price.base.avg.daily <- snow.szenario.6.1.avg[1, 'Total_Costs'] * 24 
  total.price.fluct.avg.daily <- sum(snow.szenario.6.1.avg[2:25, 'Total_Costs']) 
  snow.szenario.6.1.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.6.1.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.6.1.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.6.1.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
}
snow.szenario.6.1()


## Using only x86 for VH-2
snow.szenario.7.1 <- function() {
  
  CPU.hours.required <- 8
  CPU.hours.fluctuating <- as.list(ifelse((snowset.middle.cust.hourly$totalCpuTime - 8) < 0, 
                                          0, (snowset.middle.cust.hourly$totalCpuTime - 8)))

  # avg
  snow.szenario.7.1.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 1, processor = "x86")
  total.price.base.avg.daily <- snow.szenario.7.1.avg[1, 'Total_Costs'] * 24 
  total.price.fluct.avg.daily <- sum(snow.szenario.7.1.avg[2:25, 'Total_Costs']) 
  snow.szenario.7.1.total.daily.avg <<- total.price.base.avg.daily + total.price.fluct.avg.daily
  
  total.price.base.avg.monthly <- snow.szenario.7.1.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg.monthly <- sum(snow.szenario.7.1.avg[2:25, 'Total_Costs']) * 30
  snow.szenario.7.1.total.monthly.avg <<- total.price.base.avg.monthly + total.price.fluct.avg.monthly
}
snow.szenario.7.1()