source("./model.R")

## ------------------------------------------------------------------------------------------------------------ ##
                                                # Model with Growth
## ------------------------------------------------------------------------------------------------------------ ##

# Assuming a particular growth rate, calculates whether it is better to keep the base load the same and 
# cover the additional growth through spot instances or if it is better to increase the volumes of RIs or SPs
algorithm.with.growth <- function(growth.rate, workload.base, workload.fluct, migration.costs) {
  
  # adapt base and fluct
  new.workload.base.o1 <- (workload.base * (1 + growth.rate))
  new.workload.fluct.o1 <- lapply(workload.fluct, function(x) (x * (1 + growth.rate)-new.workload.base.o1))
  new.workload.fluct.o1 <- ifelse(new.workload.fluct.o1 < 0, 0, new.workload.fluct.o1)
  
  # keep base and adapt fluct
  new.workload.base.o2 <- workload.base
  new.workload.fluct.o2 <- lapply(workload.fluct, function(x) (x * (1 + growth.rate)-new.workload.base.o2))
  new.workload.fluct.o2 <- ifelse(new.workload.fluct.o2 < 0, 0, new.workload.fluct.o2)
  
  # calculate results for both options
  result.with.growth.o1 <<- find.cheapest.instance.final(new.workload.base.o1, new.workload.fluct.o1, migration.costs)
  result.with.growth.o2 <<- find.cheapest.instance.final(new.workload.base.o2, new.workload.fluct.o2, migration.costs)
  
  # check if new used instance in o1 is fully exploited - adapt parallelization fraction as needed
  vCPU <- aws.all.prices$vCPUs[aws.all.prices$API.Name == result.with.growth.o1[1, "Instance_Type"]]
  vCPU <- vCPU * floor(amdahl(0.95, as.numeric(result.with.growth.o1[1, "Num_Instances_Required"])))
  if (new.workload.base.o1 != vCPU) {
    new.workload.base.o1 <- vCPU
    new.workload.fluct.o1 <- lapply(workload.fluct, function(x) (x * (1 + growth.rate)-new.workload.base.o1))
    new.workload.fluct.o1 <- ifelse(new.workload.fluct.o1 < 0, 0, new.workload.fluct.o1)
    
    # recalc result
    result.with.growth.o1 <<- find.cheapest.instance.final(new.workload.base.o1, new.workload.fluct.o1, migration.costs)
  }
  
  # calculate total costs
  o1.base <- result.with.growth.o1[1, 'Total_Costs'] * 24
  o1.fluct <- sum(result.with.growth.o1[2:25, 'Total_Costs'])
  o1.total <<- o1.base + o1.fluct
  
  o2.base <- result.with.growth.o2[1, 'Total_Costs'] * 24
  o2.fluct <- sum(result.with.growth.o2[2:25, 'Total_Costs'])
  o2.total <<- o2.base + o2.fluct
  
  if (o1.total < o2.total) {
    print("Buy additional committed plans")
  } else {
    print("Keep base workload and cover growth with spot instances")
  }
  
  # if (result.with.growth[1, 'Total_Costs'] == result.wo.growth[1, 'Total_Costs'] && 
  #     result.with.growth[1, 'Num_Instances_Required'] == result.wo.growth[1, 'Num_Instances_Required']) {
  #   
  #   message("No new price plan required")
  #   return(result.with.growth)
  #   
  # } else {
  #   new.workload.base <- (workload.base * (1 + growth.rate)) - workload.base
  #   result.with.growth <- find.cheapest.instance.final(new.workload.base, new.workload.fluct, 2)
  #   message("New price plan required")
  #   return(result.with.growth)
  # }
}


## ------------------------------------------------------------------------------------------------------------ ##
                                        # Example usage - Synthetic workload
## ------------------------------------------------------------------------------------------------------------ ##

# Uncomment the following code to run the model with exemplary inputs
# df.example.growth <- algorithm.with.growth(0.50,
#                                            8,
#                                            list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3,
#                                                 6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6),
#                                            2)


## ------------------------------------------------------------------------------------------------------------ ##
                                      # Example usage - Snowset workload (VH-1)
## ------------------------------------------------------------------------------------------------------------ ##

# requires to run snowset.analysis.R
# Uncomment the following code to run the model with exemplary inputs
# df.example.growth2 <- algorithm.with.growth(0.20, 
#                                             96, 
#                                             as.list(snowset.large.cust.hourly$totalCpuTime),
#                                             2)


## ------------------------------------------------------------------------------------------------------------ ##
                                      # Example usage - Snowset workload (VH-2)
## ------------------------------------------------------------------------------------------------------------ ##

# requires to run snowset.analysis.R
# Uncomment the following code to run the model with exemplary inputs
# df.example.growth3 <- algorithm.with.growth(0.20, 
#                                             8, 
#                                             as.list(snowset.middle.cust.hourly$totalCpuTime),
#                                             1)