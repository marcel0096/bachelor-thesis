# ----------------------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------- SZENARIOS -------------------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #

## On-prem vs. cloud comparison -> Tanker
## Lift and shift

## USE CASE 1: Wikipedia
#     ~ 28mio views per hour on all websites at peak (~ 25mio views on average per day) 
#       assumption: php needs 0.1 CPU seconds per view = 0.0000278 CPU hours per view

# 1. Approach: Average the fluctuations per day and only use one instance (economy variant -> can't cope with max demand)

views <- 25000000
php.CPU.demand.per.view <- 0.1 / 60 / 60
CPU.hours.required <- views * php.CPU.demand.per.view
CPU.hours.fluctuating <- list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

szenario1 <- find.cheapest.instance.final(CPU.hours.required, CPU.hours.fluctuating, 0)

aws.total.monthly.approach1 <- szenario1[1, 'Total_Costs'] * 24 * 30

# Best instance using base only                                      # Comparable on-prem server
# Specs: c6a.48xlarge                                                # Specs: Dell Smart Selection PowerEdge R7625 Rack Server
# Processor: 3rd generation AMD EPYC processors (AMD EPYC 7R13)      # Processor: 1x AMD EPYC 9654 (384 MB Cache, 96 Cores, 192 Threads, 2,40 GHz (360 W), DDR5-4800) 
# vCPUs: 192                                                         # vCPUs: 192
# Memory: 384 GiB                                                    # Memory: 3 x 128 GB, RDIMM, 4.800 MT/s, Quad-Rank 
# Network: Up to 50 Gbps                                             # Network: 1 x Broadcom 57508 Dual Port 100GbE QSFP Adapter, PCIe Low Profile  
# Price: 14.07$ / h (5 instances required)                           # Price: 5x 43500$

# Abschreibung über 3 Jahre
on.prem.monthly <- (5 * 43500) / 36
# 2x due to maintenance costs etc. -> ALSO INCLUDE SALARY etc. -> see paper which proposes 2x for direct costs
on.prem.monthly.adapted <- on.prem.monthly * 2


# 2. Approach: Use algorithm to only apply base workloads for minimum per day and cover the rest with spot instances
#              New assumption: base load is 17mio views per hour, rest is fluctuating

# If base load is e.g. only 17mio, significant price change (add rest to every spot instance)
CPU.hours.required.approach2 <- 17000000 * php.CPU.demand.per.view

# based on rough estimates from graph on slide 63
CPU.hours.fluctuating.approach2 <- list(
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

# Only few migration costs as only data in caches need to be migrated -> averaged spot costs
szenario2 <- find.cheapest.instance.final(CPU.hours.required.approach2, CPU.hours.fluctuating.approach2, 2)

aws.total.price.base.monthly.approach2 <- szenario2[1, 'Total_Costs'] * 24 * 30
aws.total.price.fluct.monthly.approach2 <- sum(szenario2[2:25, 'Total_Costs']) * 30

aws.total.price.monthly.approach2 <- aws.total.price.base.monthly.approach2 + aws.total.price.fluct.monthly.approach2

# minimum spot costs
szenario2.min <- find.cheapest.instance.final.min(CPU.hours.required.approach2, CPU.hours.fluctuating.approach2, 2)

aws.total.price.base.monthly.approach2.min <- szenario2.min[1, 'Total_Costs'] * 24 * 30
aws.total.price.fluct.monthly.approach2.min <- sum(szenario2.min[2:25, 'Total_Costs']) * 30

aws.total.price.monthly.approach2.min <- aws.total.price.base.monthly.approach2.min + aws.total.price.fluct.monthly.approach2.min


# 3. Approach: Use algorithm to only apply base workloads for less than minimum per day and cover the rest with spot instances
#              New assumption: base load is 10mio views per hour, rest is fluctuating, i.e. put 7mio on top of each fluct value

CPU.hours.required.approach3 <- 10000000 * php.CPU.demand.per.view

# based on rough estimates from graph on slide 63
CPU.hours.fluctuating.approach3 <- list(
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

# Only few migration costs as only data in caches need to be migrated -> averaged spot costs
szenario3 <- find.cheapest.instance.final(CPU.hours.required.approach3, CPU.hours.fluctuating.approach3, 2)

aws.total.price.base.monthly.approach3 <- szenario3[1, 'Total_Costs'] * 24 * 30
aws.total.price.fluct.monthly.approach3 <- sum(szenario3[2:25, 'Total_Costs']) * 30

aws.total.price.monthly.approach3 <- aws.total.price.base.monthly.approach3 + aws.total.price.fluct.monthly.approach3

# minimum spot costs
szenario3.min <- find.cheapest.instance.final.min(CPU.hours.required.approach3, CPU.hours.fluctuating.approach3, 2)

aws.total.price.base.monthly.approach3.min <- szenario3.min[1, 'Total_Costs'] * 24 * 30
aws.total.price.fluct.monthly.approach3.min <- sum(szenario3.min[2:25, 'Total_Costs']) * 30

aws.total.price.monthly.approach3.min <- aws.total.price.base.monthly.approach3.min + aws.total.price.fluct.monthly.approach3.min



## RESULTS USE CASE 1:
#     - Important insight: More Spot opportunities (i.e. lower base load) do not necessarily mean cheaper prices, even with 
#                          minimum spot prices
#     - Reason for that is: Larger instances (as would be required also by spot if workload is large) have spot prices that are
#       more expensive than SP or RI options
#     - As expected, a middle way is the cheapest option, i.e. set the base load to the lowest fluct value and cover the rest
#       with spot instances
#     - Was passiert wenn man gleich statt einem großen Server immer wieder kleine dazu holt
#


# USE CASE 2: Middle sized Software Development Company
# Estimate the number of developers working on resource-intensive tasks such as compiling, debugging, and running tests.
# Assume an average of 100 developers using the system for 8 hours per day.
# Higher CPU utilization of 70% per developer due to resource-demanding development environments.
# Multiply the number of users (e.g., 100 developers) by the daily usage hours (8 hours) and CPU utilization (70%) to estimate the server CPU workload.


# 1. Approach: Average per hour CPU usage assuming 70% utilization of 100 developers, but taking max value of 100
CPU.hours.required.case2 <- 100 
CPU.hours.fluctuating.case2 <- list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

szenario1.case2 <- find.cheapest.instance.final(CPU.hours.required.case2, CPU.hours.fluctuating.case2, 0)

# assuming 8h per day, 21 work days a month (no background jobs for simplicity)
aws.total.monthly.approach1.case2 <- szenario1.case2[1, 'Total_Costs'] * 8 * 21


# Best instance using base only                                      # Comparable on-prem server
# Specs: a1.metal                                                    # Specs: Dell PowerEdge R450 Rack Server
# Processor: AWS Graviton Processor with 64-bit Arm Neoverse cores   # Processor: 1x Intel® Xeon® Silver 4314 2.4G, 16C/32T, 10.4GT/s, 24M Cache, Turbo, HT (135W) DDR4-2666 
# vCPUs: 16 (cores)                                                  # vCPUs: 32 bzw. 16 cores
# Memory: 32 GiB                                                     # Memory: 4 x 8GB RDIMM, 3200MT/s, Single Rank 
# Network: Up to 10 Gbps                                             # Network: 1 x Broadcom 57412 Dual Port 10GbE SFP+, OCP NIC 3.0  
# Price: 1.071$ / h (7 instances required)                           # Price: 7x 4300$

on.prem.monthly.case2 <- (7 * 4300) / 36
on.prem.monthly.aapted.case2 <- on.prem.monthly.case2 * 2


# 2. Approach: Use algorithm to only apply base workloads for minimum per day and cover the rest with spot instances
#              New assumption: base load is 50 CPU hours/h, fluctuating workload up to 100, 8h a day

CPU.hours.required.approach2.case2 <- 50
CPU.hours.fluctuating.approach2.case2 <- list(20, 25, 35, 35, 15, 18, 22, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# Only few migration costs as only data in caches need to be migrated -> averaged spot costs
szenario2.case2 <- find.cheapest.instance.final(CPU.hours.required.approach2.case2, CPU.hours.fluctuating.approach2.case2, 2)

aws.total.price.base.monthly.approach2.case2 <- szenario2.case2[1, 'Total_Costs'] * 8 * 21
aws.total.price.fluct.monthly.approach2.case2 <- sum(szenario2.case2[2:25, 'Total_Costs']) * 21

aws.total.price.monthly.approach2.case2 <- aws.total.price.base.monthly.approach2.case2 + aws.total.price.fluct.monthly.approach2.case2


## RESULTS USE CASE 2:
#     - Same as Use Case 1, just on smaller scale
#     - Too many spot instances can have a negative effect, even when taking the min prices into account



# USE CASE 3: Small SaaS Company
# 10 software engineers
# High CPU utilization of 80% per user due to frequent database queries and updates.
# Multiply the number of users by the daily usage hours (8 hours) and CPU utilization (50%) to estimate the server CPU workload.

# 1. Approach: Average per hour CPU usage assuming 80% utilization of 10 developers, but taking max value of 10
CPU.hours.required.case3 <- 10
CPU.hours.fluctuating.case3 <- list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

szenario1.case3 <- find.cheapest.instance.final(CPU.hours.required.case3, CPU.hours.fluctuating.case3, 0)

# assuming 8h per day, 21 work days a month (no background jobs for simplicity)
aws.total.monthly.approach1.case3 <- szenario1.case3[1, 'Total_Costs'] * 8 * 21


# 2. Approach: Use algorithm to only apply base workloads for minimum per day and cover the rest with spot instances
#              New assumption: base load is 5 CPU hours/h, fluctuating workload up to 10, 8h a day

CPU.hours.required.approach2.case3 <- 5
CPU.hours.fluctuating.approach2.case3 <- list(2, 2, 2, 2, 2, 5, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# Only few migration costs as only data in caches need to be migrated -> averaged spot costs
szenario2.case3 <- find.cheapest.instance.final(CPU.hours.required.approach2.case3, CPU.hours.fluctuating.approach2.case3, 1)

aws.total.price.base.monthly.approach2.case3 <- szenario2.case3[1, 'Total_Costs'] * 8 * 21
aws.total.price.fluct.monthly.approach2.case3 <- sum(szenario2.case3[2:25, 'Total_Costs']) * 21

aws.total.price.monthly.approach2.case3 <- aws.total.price.base.monthly.approach2.case3 + aws.total.price.fluct.monthly.approach2.case3


## RESULTS USE CASE 3:
#     - Makes almost no sense to cover such little workloads with spot, as proposed instance is mostly the same, which is
#       cheaper using a plan (when using averaged prices)
#     - Only when using min spot prices and no migration costs one can get slightly cheaper than with a plan


# -----> Cover as much as possible with plans, even if some resources are wasted, and only cover rare spikes with spot
#           - If spikes are constant, push base load as high as possible so that there are only a few spikes in fluct
#           - Alternate migration costs
#           - Alternate min or averaged prices
#           - Alternate using commitment and plan
#           - Spot instances are mostly not as cheap as one would expect, esp. because they are adapted with their interruption
#             frequencies, which leads to the fact that the theoretically best instances are not the best anymore (e.g. a1...)
#           - As rough estimate, cloud seems to be cheaper in most cases using the algorithm











# Random Comparison: c6i.32xlarge vs. PowerEdge R650xs Rack Server

# Specs: c6i.32xlarge                                                # Specs: Dell PowerEdge R650xs Rack Server
# Processor: 3rd gen. Intel Xeon Scalable (Ice Lake 8375C)           # Processor: 3rd gen. Intel Xeon Scalable (Gold 6338N 2.2G, 32C/64T, 11.2GT/s, 48M Cache, Turbo, HT (185W) DDR4-2666)
# vCPUs: 128                                                         # vCPUs: 2 x 64 = 128
# Memory: 256 GiB                                                    # Memory: 16 x 16GB RDIMM, 3200MT/s, Dual Rank = 256
# Network: Up to 50 Gbps                                             # Network: 2 x Broadcom 57414 Dual Port 10/25GbE SFP28, OCP NIC 3.0 = 50
                                                      
# time.to.run <- 36
# on.prem.price.total <- 17000
# 
# on.prem.price.per.month <- on.prem.price.total / time.to.run
# on.prem.price.per.month.adapted <- 2 * on.prem.price.per.month
# 
# aws.price.per.month <- aws.all.prices[aws.all.prices$API.Name == 'c6i.32xlarge', 'OD.costs.hourly'] * 24 * 30
# aws.price.per.month.adapted <- min(aws.all.prices[aws.all.prices$API.Name == 'c6i.32xlarge', 7:29]) * 24 * 30
# 
#   
# plot_df <- tibble(
#   months = 1:36,
#   on_prem = on.prem.price.per.month.adapted * 36,
#   aws = aws.price.per.month.adapted,
#   max_costs = aws.price.per.month.adapted * 36
# )
# 
# plot_df$on_prem_cum <- cumsum(plot_df$on_prem)
# plot_df$aws_cum <- cumsum(plot_df$aws)
# 
# ggplot(plot_df, aes(x = months)) +
#   geom_line(aes(y = aws_cum, color = "Cloud")) +
#   geom_line(aes(y = on_prem, color = "On-Premises")) +
#   labs(x = "Months", y = "Cumulative Costs") +
#   scale_color_manual(values = c("Cloud" = "blue", "On-Premises" = "red")) +
#   theme_bw()







