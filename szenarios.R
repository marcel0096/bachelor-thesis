# ----------------------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------- SZENARIOS -------------------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #

## SZENARIO 1: Wikipedia
#     ~ 28mio views per hour on all websites at peak (~ 25mio views on average per hour over the day) 
#       assumption: php needs 0.1 CPU seconds per view = 0.0000278 CPU hours per view

# 1. Approach: Take max value per day but only use base workload (deluxe variant)
# Take max value here because otherwise not comparable with approaches using algorithm as it gives you perfect demand coverage

# Best instance using base only                                      # Comparable on-prem server
# Specs: c6a.48xlarge                                                # Specs: Dell Smart Selection PowerEdge R7625 Rack Server
# Processor: 3rd generation AMD EPYC processors (AMD EPYC 7R13)      # Processor: 1x AMD EPYC 9654 (384 MB Cache, 96 Cores, 192 Threads, 2,40 GHz (360 W), DDR5-4800) 
# vCPUs: 192                                                         # vCPUs: 192
# Memory: 384 GiB                                                    # Memory: 3 x 128 GB, RDIMM, 4.800 MT/s, Quad-Rank 
# Network: Up to 50 Gbps                                             # Network: 1 x Broadcom 57508 Dual Port 100GbE QSFP Adapter, PCIe Low Profile  
# Price: 19.69$ / h (7 instances required)                           # Price: 7x 43500$

szenario1.1 <- function() {
  
  CPU.hours.required <- 28000000 * (0.1 / 60 / 60)
  CPU.hours.fluctuating <- 0
  
  szenario1.1 <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0)
  
  szenario1.1.total.monthly <<- szenario1.1[1, 'Total_Costs'] * 24 * 30
}
szenario1.1()

# Abschreibung über 3 Jahre
# 2x due to direct costs such as power supply, infrastructure and maintainance 
# see paper which proposes TCO = 2x server costs 
# indirect costs such as salaries are not included, but would support the effect in the same direction
on.prem.monthly.1 <- ((7 * 43500) / 36) * 2


# 2. Approach: Use algorithm to only apply base workloads for minimum per day and cover the rest with spot instances
#              New assumption: base load is 17mio views per hour, rest is fluctuating

# If base load is e.g. only 17mio, significant price change (add rest to every spot instance)
szenario1.2 <- function() {
  
  php.CPU.demand.per.view <- (0.1 / 60 / 60)
  CPU.hours.required <- 17000000 * php.CPU.demand.per.view
  # based on rough estimates from graph on slide 63
  CPU.hours.fluctuating <- list(
    1500000 * php.CPU.demand.per.view,
    1000000 * php.CPU.demand.per.view,
    1000000 * php.CPU.demand.per.view,
    1000000 * php.CPU.demand.per.view,
    500000 * php.CPU.demand.per.view,
    0 * php.CPU.demand.per.view,
    1500000 * php.CPU.demand.per.view,
    2000000 * php.CPU.demand.per.view,
    3000000 * php.CPU.demand.per.view,
    4000000 * php.CPU.demand.per.view,
    5000000 * php.CPU.demand.per.view,
    6000000 * php.CPU.demand.per.view,
    7000000 * php.CPU.demand.per.view,
    8000000 * php.CPU.demand.per.view,
    9000000 * php.CPU.demand.per.view,
    10000000 * php.CPU.demand.per.view,
    11000000 * php.CPU.demand.per.view,
    10000000 * php.CPU.demand.per.view,
    10000000 * php.CPU.demand.per.view,
    10000000 * php.CPU.demand.per.view,
    10000000 * php.CPU.demand.per.view,
    9000000 * php.CPU.demand.per.view,
    4000000 * php.CPU.demand.per.view,
    2000000 * php.CPU.demand.per.view
  )
  
  # average spot costs
  szenario1.2.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg <- szenario1.2.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg <- sum(szenario1.2.avg[2:25, 'Total_Costs']) * 30
  szenario1.2.total.monthly.avg <<- total.price.base.avg + total.price.fluct.avg
  
  # minimum spot costs
  szenario1.2.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min <- szenario1.2.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min <- sum(szenario1.2.min[2:25, 'Total_Costs']) * 30
  szenario1.2.total.monthly.min <<- total.price.base.min + total.price.fluct.min
}
szenario1.2()


# 3. Approach: Use algorithm to apply base workloads for less than minimum per day and cover the rest with spot instances
#              New assumption: base load is 10mio views per hour, rest is fluctuating, i.e. put 7mio on top of each fluct value

szenario1.3 <- function() {
  
  php.CPU.demand.per.view <- (0.1 / 60 / 60)
  CPU.hours.required <- 10000000 * php.CPU.demand.per.view
  # based on rough estimates from graph on slide 63
  CPU.hours.fluctuating <- list(
    8500000 * php.CPU.demand.per.view,
    8000000 * php.CPU.demand.per.view,
    8000000 * php.CPU.demand.per.view,
    8000000 * php.CPU.demand.per.view,
    7500000 * php.CPU.demand.per.view,
    7000000 * php.CPU.demand.per.view,
    8500000 * php.CPU.demand.per.view,
    9000000 * php.CPU.demand.per.view,
    10000000 * php.CPU.demand.per.view,
    11000000 * php.CPU.demand.per.view,
    12000000 * php.CPU.demand.per.view,
    13000000 * php.CPU.demand.per.view,
    14000000 * php.CPU.demand.per.view,
    15000000 * php.CPU.demand.per.view,
    16000000 * php.CPU.demand.per.view,
    17000000 * php.CPU.demand.per.view,
    18000000 * php.CPU.demand.per.view,
    17000000 * php.CPU.demand.per.view,
    17000000 * php.CPU.demand.per.view,
    17000000 * php.CPU.demand.per.view,
    17000000 * php.CPU.demand.per.view,
    16000000 * php.CPU.demand.per.view,
    11000000 * php.CPU.demand.per.view,
    9000000 * php.CPU.demand.per.view
  )
  
  # average spot costs
  szenario1.3.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg <- szenario1.3.avg[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.avg <- sum(szenario1.3.avg[2:25, 'Total_Costs']) * 30
  szenario1.3.total.monthly.avg <<- total.price.base.avg + total.price.fluct.avg
  
  # minimum spot costs
  szenario1.3.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min <- szenario1.3.min[1, 'Total_Costs'] * 24 * 30
  total.price.fluct.min <- sum(szenario1.3.min[2:25, 'Total_Costs']) * 30
  szenario1.3.total.monthly.min <<- total.price.base.min + total.price.fluct.min
}
szenario1.3()


## RESULTS SZENARIO 1:
#     - Important insight: More Spot opportunities (i.e. lower base load) do not necessarily mean cheaper prices, even with 
#                          minimum spot prices -> good ratio important -> rule of thumb: set base load to minimum of peaks
#     - Reason for that is: Larger instances (as would be required also by spot if workload is large) have spot prices that are
#       more expensive than SP or RI options
#     - As expected, a middle way is the cheapest option, i.e. set the base load to the lowest fluct value and cover the rest
#       with spot instances
#     - Was passiert wenn man gleich statt einem großen Server immer wieder kleine dazu holt? -> weniger große meistens günstiger



# SZENARIO 2: Middle sized Software Development Company
# Estimate the number of developers working on resource-intensive tasks such as compiling, debugging, and running tests
# Assume an average of 100 developers using the system for 8 hours per day
# Higher CPU utilization of 70% per developer due to resource-demanding development environments -> max value is 100


# 1. Approach: Maximum per hour CPU usage of 100 developers (deluxe variant) 

# Best instance using base only                                      # Comparable on-prem server
# Specs: a1.metal                                                    # Specs: Dell PowerEdge R450 Rack Server
# Processor: AWS Graviton Processor with 64-bit Arm Neoverse cores   # Processor: 1x Intel® Xeon® Silver 4314 2.4G, 16C/32T, 10.4GT/s, 24M Cache, Turbo, HT (135W) DDR4-2666 
# vCPUs: 16 (cores)                                                  # vCPUs: 16 cores / 32 threads
# Memory: 32 GiB                                                     # Memory: 4 x 8GB RDIMM, 3200MT/s, Single Rank 
# Network: Up to 10 Gbps                                             # Network: 1 x Broadcom 57412 Dual Port 10GbE SFP+, OCP NIC 3.0  
# Price: 1.683$ / h (11 instances required)                          # Price: 11x 4300$

szenario2.1 <- function() {

  CPU.hours.required <- 100
  CPU.hours.fluctuating <- 0
  
  szenario2.1 <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0)
  # assuming 8h per day, 21 work days a month (no background jobs)
  szenario2.1.total.monthly <<- szenario2.1[1, 'Total_Costs'] * 8 * 21
}
szenario2.1()

# On prem is much more expensive because dell has no arm processors -> look for new
on.prem.monthly.2 <- ((11 * 4300) / 36) * 2


# 2. Approach: Use algorithm to only apply base workloads for minimum per day and cover the rest with spot instances
#              New assumption: base load is 60 CPU hours/h, fluctuating workload up to 100, 8h a day

szenario2.2 <- function() {
  
  CPU.hours.required <- 60
  CPU.hours.fluctuating <- list(20, 30, 40, 35, 10, 0, 20, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  # average spot costs
  szenario2.2.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg <- szenario2.2.avg[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.avg <- sum(szenario2.2.avg[2:25, 'Total_Costs']) * 21
  szenario2.2.total.monthly.avg <<- total.price.base.avg + total.price.fluct.avg
  
  # minimum spot costs
  szenario2.2.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min <- szenario2.2.min[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.min <- sum(szenario2.2.min[2:25, 'Total_Costs']) * 21
  szenario2.2.total.monthly.min <<- total.price.base.min + total.price.fluct.min
}
szenario2.2()


# 3. Approach: Use algorithm to apply base workloads for less than minimum per day and cover the rest with spot instances
#              New assumption: base load is 40 CPU hours per hour, rest is fluctuating, i.e. put 20 on top of each fluct value

szenario2.3 <- function() {
  
  CPU.hours.required <- 40
  CPU.hours.fluctuating <- list(40, 50, 60, 55, 30, 20, 40, 54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  # average spot costs
  szenario2.3.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.avg <- szenario2.3.avg[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.avg <- sum(szenario2.3.avg[2:25, 'Total_Costs']) * 21
  szenario2.3.total.monthly.avg <<- total.price.base.avg + total.price.fluct.avg
  
  # minimum spot costs
  szenario2.3.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 2)
  total.price.base.min <- szenario2.3.min[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.min <- sum(szenario2.3.min[2:25, 'Total_Costs']) * 21
  szenario2.3.total.monthly.min <<- total.price.base.min + total.price.fluct.min
}
szenario2.3()


## RESULTS USE CASE 2:
#     - Same as Use Case 1, just on smaller scale
#     - Too many spot instances can have a negative effect, even when taking the min prices into account



# USE CASE 3: Small SaaS Company
# 10 software engineers
# High CPU utilization of 80% per user due to frequent database queries and updates -> max value is 10

# 1. Approach: Max value of 10 

# Best instance using base only                                      # Comparable on-prem server
# Specs: a1.large                                                    # Specs: Dell PowerEdge R250 Rack Server
# Processor: AWS Graviton Processor with 64-bit Arm Neoverse cores   # Processor: 1x Intel® Pentium G6505 4.2GHz, 4M Cache, 2C/4T, No Turbo (58W), 2666 MT/s 
# vCPUs: 2 (cores)                                                   # vCPUs: 2 cores / 4 threads
# Memory: 4 GiB                                                      # Memory: 1 x 8GB UDIMM, 3200MT/s, ECC  
# Network: Up to 10 Gbps                                             # Network: 1 x Broadcom 1Gbe  
# Price: 0.134$ / h (7 instances required)                           # Price: 7x 1049$
                                                                     # Very expensive here, better one larger server, e.g. PowerEdge R450 Rack Server for ~4500$

szenario3.1 <- function() {
  
  CPU.hours.required <- 10
  CPU.hours.fluctuating <- 0
  
  szenario3.1 <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0)
  # assuming 8h per day, 21 work days a month (no background jobs for simplicity)
  szenario3.1.total.monthly <<- szenario3.1[1, 'Total_Costs'] * 8 * 21
}
szenario3.1()

on.prem.monthly.3 <- ((4500) / 36) * 2


# 2. Approach: Use algorithm to only apply base workloads for minimum per day and cover the rest with spot instances
#              New assumption: base load is 6 CPU hours/h, fluctuating workload up to 10, 8h a day

szenario3.2 <- function() {
  
  CPU.hours.required <- 6
  CPU.hours.fluctuating <- list(1, 2, 3, 4, 2, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  # average spot costs
  szenario3.2.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.avg <- szenario3.2.avg[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.avg <- sum(szenario3.2.avg[2:25, 'Total_Costs']) * 21
  szenario3.2.total.monthly.avg <<- total.price.base.avg + total.price.fluct.avg
  
  # min spot costs
  szenario3.2.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.min <- szenario3.2.min[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.min <- sum(szenario3.2.min[2:25, 'Total_Costs']) * 21
  szenario3.2.total.monthly.min <<- total.price.base.min + total.price.fluct.min
}
szenario3.2()


# 3. Approach: Use algorithm to apply base workloads for less than minimum per day and cover the rest with spot instances
#              New assumption: base load is 4 CPU hours per hour, rest is fluctuating, i.e. put 2 on top of each fluct value

szenario3.3 <- function() {
  
  CPU.hours.required <- 4
  CPU.hours.fluctuating <- list(3, 4, 5, 6, 4, 2, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  # average spot costs
  szenario3.3.avg <<- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.avg <- szenario3.3.avg[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.avg <- sum(szenario3.3.avg[2:25, 'Total_Costs']) * 21
  szenario3.3.total.monthly.avg <<- total.price.base.avg + total.price.fluct.avg
  
  # min spot costs
  szenario3.3.min <<- find.cheapest.instance.final.min(CPU.hours.required, CPU.hours.fluctuating, 1)
  total.price.base.min <- szenario3.3.min[1, 'Total_Costs'] * 8 * 21
  total.price.fluct.min <- sum(szenario3.3.min[2:25, 'Total_Costs']) * 21
  szenario3.3.total.monthly.min <<- total.price.base.min + total.price.fluct.min
}
szenario3.3()


## RESULTS USE CASE 3:
#     - In general same pattern as above, but as differences are so little, it makes almost no sense to cover such 
#       little workloads with spot, as proposed instance is mostly the same, which is cheaper using a plan (when using averaged prices)
#     - Only when using min spot prices and no migration costs one can get slightly cheaper than with a plan


# TODOS:
#   - Make plots for szenarios
#   - Show what can be saved by commiting / being flexible 
#   - Show savings based on arm vs. intel/amd -> adapt cores/vCPUs in data







