# ----------------------------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------------ ALGORITHMS ----------------------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #



# --------------------------------------------------- HELPER FUNCTIONS -------------------------------------------------------------- # 

format.plan.string <- function(plan) {
  plan_parts <- strsplit(plan, ".", fixed = TRUE)[[1]]
  
  if (plan_parts[1] == "SP") {
    plan_formatted <- paste0("Savings Plan", " (", paste(plan_parts[3:5], collapse = ", "), ", ", plan_parts[6], ")")
  } else {
    plan_formatted <- paste0("Reserved Instance", " (", paste(plan_parts[3:5], collapse = ", "), ", ", plan_parts[6], ")")
  }
  
  return(plan_formatted)
}

print.base.workload <- function(API.name, costs, required.instances, plan) {
  output = paste("[Base Workload - ", "Instance: ", API.name, "; Total Costs: ", costs, 
                 "$; Number of Instances required: ", required.instances, 
                 "; Plan: ", format.plan.string(plan), "]", sep = "")
  return(output)
}

print.fluct.workload <- function(index, API.name, costs, required.instances, plan) {
  output <- paste("[", "Spot Instance ", index, ": ", API.name, "; Total Costs: ", costs, 
                  "$; Number of Instances required: ", required.instances, 
                  "; Plan: ", plan, "]", sep = "")
  return(output)
}

print.fluct.workload.mig <- function(index, API.name, costs, required.instances, plan, migration.costs, migration.time) {
  output <- paste("[", "Spot Instance ", index, ": ", API.name, "; Total Costs: ", costs, 
                  "$; Number of Instances required: ", required.instances, 
                  "; Plan: ", plan, "; Migration Costs: ", migration.costs, "$; Migration Time: ", migration.time, " CPU hours]", sep = "")
  return(output)
}

# ------------------------------------------- ALGORITHM FOR ON DEMAND w/o AMDAHL ---------------------------------------------------- # 

# Implementing algorithm to find cheapest On demand instance for given required CPU/h usage 
# Basic version assuming that perfect parellelization is possible, i.e. no Amdahls law

find.cheapest.instance.on.demand.v1 <- function(CPU.hours.per.hour) {
  
  current.instance.price <- Inf
  cheapest.price <- Inf
  
  # iterate through all rows in data frame to find cheapest instances
  for (i in 1:nrow(aws.all.prices.OD.only)) {
    
    instance.vCPU <- aws.all.prices.OD.only[i, "vCPUs"]
    instance.name <- aws.all.prices.OD.only[i, "API.Name"]
    instance.OD.costs <- aws.all.prices.OD.only[i, "OD.costs"]
    
    # does one instance of the instance type alone have enough vCPUs to serve the need?
    if (instance.vCPU < CPU.hours.per.hour) {
      # if not, calculate how many instances would be required and add the prices
      number.of.instances.required <- ceiling(CPU.hours.per.hour / instance.vCPU)
      current.instance.price <- instance.OD.costs * number.of.instances.required
      
      # is it also the cheapest so far?
      if (current.instance.price < cheapest.price) {
        # empty list if there is a new cheapest instance 
        result <- list()
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", number.of.instances.required, "]", sep = ""))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        # extend list if there is an equally cheap instance
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", number.of.instances.required, "]", sep = ""))
        cheapest.price <- current.instance.price
      }
      
    } else {
      # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
      current.instance.price <- instance.OD.costs 
      
      # is it also the cheapest so far?
      if (current.instance.price < cheapest.price) {
        result <- list()
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", 1, "]", sep = ""))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", 1, "]", sep = ""))
        cheapest.price <- current.instance.price
      }
    }
  }
  
  # printing the result
  result_final <- paste(paste(result))
  cat(result_final, sep = "\n")
}

# find.cheapest.instance.on.demand.v1(10)



# ------------------------------------------- ALGORITHM FOR ON DEMAND WITH AMDAHL --------------------------------------------------- # 

# Implementing algorithm to find cheapest On demand instance for given required CPU/h usage 
# Advanced version assuming that perfect parellelization is not possible, i.e. with Amdahls law

find.cheapest.instance.on.demand.v2 <- function(CPU.hours.per.hour) {
  
  current.instance.price <- Inf
  cheapest.price <- Inf
  
  # maximum of amdahls formula with p = 0.90 is 10. Thus, maximum number of instances is 9
  amdahl.max = 10
  
  # iterate through all rows in data frame to find cheapest instances
  for (i in 1:nrow(aws.all.prices.OD.only)) {
    
    instance.vCPU <- aws.all.prices.OD.only[i, "vCPUs"]
    instance.name <- aws.all.prices.OD.only[i, "API.Name"]
    instance.OD.costs <- aws.all.prices.OD.only[i, "OD.costs"]
    
    # does one instance of the instance type alone have enough vCPUs to serve the need?
    if (instance.vCPU < CPU.hours.per.hour) {
      # if not, calculate how many instances would be required
      number.of.instances.required <- ceiling(CPU.hours.per.hour / instance.vCPU)
      
      # using amdahls law to determine the number of instances necessary to cope with required workload (90% of parallelization assumed)
      if (number.of.instances.required < amdahl.max) {
        number.of.instances.required.amdahl <- ceiling(amdahl.reversed(0.90, number.of.instances.required))
        current.instance.price <- instance.OD.costs * number.of.instances.required.amdahl
      } else {
        # if the theoretically required instances are 10 or more, amdahls function goes to infinity, thus it is limited by 9
        number.of.instances.required.amdahl <- ceiling(amdahl.reversed(0.90, amdahl.max))
        current.instance.price <- instance.OD.costs * number.of.instances.required.amdahl
      }
      
      # is it also the cheapest so far?
      if (current.instance.price < cheapest.price) {
        # empty list if there is a new cheapest instance 
        result <- list()
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", number.of.instances.required.amdahl, "]", sep = ""))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        # extend list if there is an equally cheap instance
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", number.of.instances.required.amdahl, "]", sep = ""))
        cheapest.price <- current.instance.price
      }
      
    } else {
      # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
      current.instance.price <- instance.OD.costs 
      
      # is it also the cheapest so far?
      if (current.instance.price < cheapest.price) {
        result <- list()
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", 1, "]", sep = ""))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", 1, "]", sep = ""))
        cheapest.price <- current.instance.price
      }
    }
  }
  
  # printing the result
  result_final <- paste(paste(result))
  cat(result_final, sep = "\n")
}

# find.cheapest.instance.on.demand.v2(10)



# ---------------------------- ALGORITHM FOR ON DEMAND, RESERVED INSTANCES, SAVINGS PLANS ------------------------------------------- # 

# extending the find.cheapest.instance.on.demand.v2 function by also searching RI and SP
find.cheapest.instance.OD.RI.SP <- function(CPU.hours.per.hour) {
  
  current.instance.price <- Inf
  cheapest.price <- Inf
  
  # maximum of amdahls formula with p = 0.90 is 10. Thus, maximum number of instances is 9
  amdahl.max = 10
  
  # iterate through all rows in data frame to find cheapest instances
  for (i in 1:nrow(aws.shared.all.prices.without.Spot)) {
    
    instance.vCPU <- aws.shared.all.prices.without.Spot[i, "vCPUs"]
    instance.name <- aws.shared.all.prices.without.Spot[i, "API.Name"]
    # getting cheapest plan in each row and corresponding column name
    instance.min.costs.per.API <- apply(aws.shared.all.prices.without.Spot[i, 5:29], 1, min)
    instance.min.col.name <- names(aws.shared.all.prices.without.Spot)[apply(aws.shared.all.prices.without.Spot[i, 5:29], 1, which.min) + 4]
    
    # does one instance of the instance type alone have enough vCPUs to serve the need?
    if (instance.vCPU < CPU.hours.per.hour) {
      # if not, calculate how many instances would be required
      number.of.instances.required <- ceiling(CPU.hours.per.hour / instance.vCPU)
      
      # using amdahls law to determine the number of instances necessary to cope with required workload (90% of parallelization assumed)
      if (number.of.instances.required < amdahl.max) {
        number.of.instances.required.amdahl <- ceiling(amdahl.reversed(0.90, number.of.instances.required))
        current.instance.price <- instance.min.costs.per.API * number.of.instances.required.amdahl
      } else {
        # if the theoretically required instances are 10 or more, amdahls function goes to infinity, thus it is limited by 9
        number.of.instances.required.amdahl <- ceiling(amdahl.reversed(0.90, amdahl.max))
        current.instance.price <- instance.min.costs.per.API * number.of.instances.required.amdahl
      }
      
      # is it also the cheapest so far?
      if (current.instance.price < cheapest.price) {
        # empty list if there is a new cheapest instance 
        result <- list()
        result <- append(result, print.base.workload(instance.name, current.instance.price, number.of.instances.required.amdahl, instance.min.col.name))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        # extend list if there is an equally cheap instance
        result <- append(result, print.base.workload(instance.name, current.instance.price, number.of.instances.required.amdahl, instance.min.col.name))
        cheapest.price <- current.instance.price
      }
      
    } else {
      # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
      current.instance.price <- instance.min.costs.per.API 
      
      # is it also the cheapest so far?
      if (current.instance.price < cheapest.price) {
        result <- list()
        result <- append(result, print.base.workload(instance.name, current.instance.price, 1, instance.min.col.name))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        result <- append(result, print.base.workload(instance.name, current.instance.price, 1, instance.min.col.name))
        cheapest.price <- current.instance.price
      }
    }
  }
  
  # printing the result
  result_final <- paste(paste(result))
  cat(result_final, sep = "\n")
}

# find.cheapest.instance.OD.RI.SP(96)



# ------- ALGORITHM WITH BASIC WORKLOAD USING ON DEMAND, RESERVED INSTANCES, SAVINGS PLANS + fluctuating WORKLOAD USING SPOT ------- # 

# giving the function the base workload as an int as well as the fluctuating workload as an array of 24 values
# z.B. 10, [0, 2, 3, 5, 3, 2, 6, 15, 18, 20, 19, 12, 11, 13, 15, 14, 17, 19, 24, 18, 18, 18, 13, 5]

# ANNAHME: Spot Instanzen können nicht weggenommen werden + Spot ist immer die billigste Option pro Spalte

find.cheapest.instance.OD.RI.SP.Spot <- function(CPU.hours.per.hour.base, CPU.hours.per.hour.fluct) {
  
  current.instance.price.base <- Inf
  cheapest.price.base <- Inf
  result.base <- list()
  
  # maximum of amdahls formula with p = 0.90 is 10. Thus, maximum number of instances is 9
  amdahl.max = 10
  
  # iterate through array of fluctuating values
  for (j in 1:length(CPU.hours.per.hour.fluct)) {
    
    current.instance.price.fluct <- Inf
    cheapest.price.fluct <- Inf
    result.fluct <- list()
    
    # iterate through all rows in data frame to find cheapest instances
    for (i in 1:nrow(aws.shared.all.prices)) {
      
      instance.vCPU <- aws.shared.all.prices[i, "vCPUs"]
      instance.name <- aws.shared.all.prices[i, "API.Name"]
      
      # searching for the cheapest instance of the base value only in the first iteration
      if (j == 1) {
        
        # getting cheapest plan for base workload in each row and corresponding column name (only searching RIs and SPs)
        instance.min.costs.per.API.base <- apply(aws.shared.all.prices[i, 7:29], 1, min)
        instance.min.col.name.base <- names(aws.shared.all.prices)[apply(aws.shared.all.prices[i, 7:29], 1, which.min) + 6]
        
        # does one instance of the instance type alone have enough vCPUs to serve the need?
        if (instance.vCPU < CPU.hours.per.hour.base) {
          # if not, calculate how many instances would be required
          number.of.instances.required.base <- CPU.hours.per.hour.base / instance.vCPU
          
          # using amdahls law to determine the number of instances necessary to cope with required workload (90% of parallelization assumed)
          if (number.of.instances.required.base < amdahl.max) {
            number.of.instances.required.base.amdahl <- ceiling(amdahl.reversed(0.90, number.of.instances.required.base))
            current.instance.price.base <- instance.min.costs.per.API.base * number.of.instances.required.base.amdahl
          } else {
            # if the theoretically required instances are 10 or more, amdahls function goes to infinity, thus it is limited by 9
            number.of.instances.required.base.amdahl <- ceiling(amdahl.reversed(0.90, amdahl.max))
            current.instance.price.base <- instance.min.costs.per.API.base * number.of.instances.required.base.amdahl
          }
          
          # is it also the cheapest so far?
          if (current.instance.price.base < cheapest.price.base) {
            # empty list if there is a new cheapest instance 
            result.base <- list()
            result.base <- append(result.base, print.base.workload(instance.name, current.instance.price.base, 
                                                                          number.of.instances.required.base.amdahl, instance.min.col.name.base))
            cheapest.price.base <- current.instance.price.base
          } else if (current.instance.price.base == cheapest.price.base) {
            # extend list if there is an equally cheap instance
            result.base <- append(result.base, print.base.workload(instance.name, current.instance.price.base, 
                                                                          number.of.instances.required.base.amdahl, instance.min.col.name.base))
            cheapest.price.base <- current.instance.price.base
          }
          
        } else {
          # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
          current.instance.price.base <- instance.min.costs.per.API.base
          
          # is it also the cheapest so far?
          if (current.instance.price.base < cheapest.price.base) {
            result.base <- list()
            result.base <- append(result.base, print.base.workload(instance.name, current.instance.price.base, 
                                                                          1, instance.min.col.name.base))
            cheapest.price.base <- current.instance.price.base
          } else if (current.instance.price.base == cheapest.price.base) {
            result.base <- append(result.base, print.base.workload(instance.name, current.instance.price.base, 
                                                                          1, instance.min.col.name.base))
            cheapest.price.base <- current.instance.price.base
          }
        }
      }
      
      
      # getting spot price in each row
      instance.min.costs.per.API.fluct <- aws.shared.all.prices[i, "Spot.costs"]
      
      # does one instance of the instance type alone have enough vCPUs to serve the need of the fluctuating demands
      if (instance.vCPU < CPU.hours.per.hour.fluct[[j]]) {
        # if not, calculate how many instances would be required
        number.of.instances.required.fluct <- CPU.hours.per.hour.fluct[[j]] / instance.vCPU
        
        # using amdahls law to determine the number of instances necessary to cope with required workload (90% of parallelization assumed)
        if (number.of.instances.required.fluct < amdahl.max) {
          number.of.instances.required.fluct.amdahl <- ceiling(amdahl.reversed(0.90, number.of.instances.required.fluct))
          current.instance.price.fluct <- instance.min.costs.per.API.fluct * number.of.instances.required.fluct.amdahl
        } else {
          # if the theoretically required instances are 10 or more, amdahls function goes to infinity, thus it is limited by 9
          number.of.instances.required.fluct.amdahl <- ceiling(amdahl.reversed(0.90, amdahl.max))
          current.instance.price.fluct <- instance.min.costs.per.API.fluct * number.of.instances.required.fluct.amdahl
        }
        
        # is it also the cheapest so far?
        if (current.instance.price.fluct < cheapest.price.fluct) {
          # empty list if there is a new cheapest instance 
          result.fluct <- list()
          result.fluct <- append(result.fluct, print.fluct.workload(j, instance.name, current.instance.price.fluct, 
                                                                           number.of.instances.required.fluct.amdahl, "Spot"))
          cheapest.price.fluct <- current.instance.price.fluct
        } else if (current.instance.price.fluct == cheapest.price.fluct) {
          # extend list if there is an equally cheap instance
          result.fluct <- append(result.fluct, print.fluct.workload(j, instance.name, current.instance.price.fluct, 
                                                                           number.of.instances.required.fluct.amdahl, "Spot"))
          cheapest.price.fluct <- current.instance.price.fluct
        }
        
      } else {
        # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
        current.instance.price.fluct <- instance.min.costs.per.API.fluct
        
        # check if the current value in the fluctuating array is 0
        if (CPU.hours.per.hour.fluct[[j]] == 0) {
          result.fluct <- list()
          result.fluct <- append(result.fluct, paste("[Spot Instance ", j, ": No additional Spot instances required]", sep = ""))
        } else if (current.instance.price.fluct < cheapest.price.fluct) {
          result.fluct <- list()
          result.fluct <- append(result.fluct, print.fluct.workload(j, instance.name, current.instance.price.fluct, 
                                                                           1, "Spot"))
          cheapest.price.fluct <- current.instance.price.fluct
        } else if (current.instance.price.fluct == cheapest.price.fluct) {
          result.fluct <- append(result.fluct, print.fluct.workload(j, instance.name, current.instance.price.fluct, 
                                                                           1, "Spot"))
          cheapest.price.fluct <- current.instance.price.fluct
        }
      }
    }
    # printing the result for fluctuating workload in each iteration
    result.final.fluct <- paste(paste(result.fluct))
    cat(result.final.fluct, sep = "\n")
  }
  # printing the result of base workload
  result.final.base <- paste(paste(result.base))
  cat(result.final.base, sep = "\n")
}

#find.cheapest.instance.OD.RI.SP.Spot(50, list(0, 15, 15, 15, 18, 2, 6, 15, 18, 20, 19, 12, 11, 13, 15, 14, 17, 19, 24, 18, 18, 18, 13, 50))



# ------------------------------------------------- SPOT WITH MIGRATION COSTS ----------------------------------------------------- #

find.cheapest.instance.Spot <- function(CPU.hours.per.hour.fluct, migration.costs) {
  
  amdahl.max = 10
  prev.instance.price.fluct <- 0
  prev.instance.price.fluct.wo.amdahl <- 0
  prev.instances.required.fluct <- 0
  prev.instance.vCPU.fluct <- 0
  prev.instance.name.fluct <- ""
  catch.iter <- 1
  
  # iterate through array of fluctuating values
  for (j in 1:length(CPU.hours.per.hour.fluct)) {
    
    current.instance.price.fluct <- Inf
    cheapest.price.fluct <- Inf
    cheapest.name.fluct <- ""
    cheapest.vCPU.fluct <- 0
    cheapest.instances.required <- 0
    cheapest.price.fluct.mig <- Inf
    cheapest.price.fluct.wo.amdahl <- Inf
    current.vCPU.demand.fluct <- CPU.hours.per.hour.fluct[[j]]
    result.fluct <- ""
    
    # iterate through all rows in data frame to find cheapest instance for given value
    for (i in 1:nrow(aws.shared.all.prices)) {
      
      instance.vCPU <- aws.shared.all.prices[i, "vCPUs"]
      instance.name <- aws.shared.all.prices[i, "API.Name"]
      instance.min.costs.per.API.fluct <- aws.shared.all.prices[i, "Spot.costs"]
      
      # does one instance of the instance type alone have enough vCPUs to serve the need of the fluctuating demands
      if (instance.vCPU < current.vCPU.demand.fluct) {
        # if not, calculate how many instances would be required
        number.of.instances.required.fluct <- ceiling(current.vCPU.demand.fluct / instance.vCPU)
        
        # using amdahls law to determine the number of instances necessary to cope with required workload (90% of parallelization assumed)
        if (number.of.instances.required.fluct < amdahl.max) {
          number.of.instances.required.fluct.amdahl <- ceiling(amdahl.reversed(0.90, number.of.instances.required.fluct))
          current.instance.price.fluct <- instance.min.costs.per.API.fluct * number.of.instances.required.fluct.amdahl
        } else {
          # if the theoretically required instances are 10 or more, amdahls function goes to infinity, thus it is limited by 9
          number.of.instances.required.fluct.amdahl <- ceiling(amdahl.reversed(0.90, amdahl.max))
          current.instance.price.fluct <- instance.min.costs.per.API.fluct * number.of.instances.required.fluct.amdahl
        }
        
        # is it also the cheapest so far?
        if (current.instance.price.fluct < cheapest.price.fluct) {
          # updating values
          cheapest.price.fluct <- current.instance.price.fluct
          cheapest.price.fluct.wo.amdahl <- instance.min.costs.per.API.fluct
          cheapest.name.fluct <- instance.name
          cheapest.vCPU.fluct <- instance.vCPU
          cheapest.instances.required <- number.of.instances.required.fluct.amdahl
        } 
        
      } else {
        # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
        current.instance.price.fluct <- instance.min.costs.per.API.fluct
        
        # is it also the cheapest so far?
        if (current.instance.price.fluct < cheapest.price.fluct) {
          # updating values
          cheapest.price.fluct <- current.instance.price.fluct
          cheapest.price.fluct.wo.amdahl <- instance.min.costs.per.API.fluct
          cheapest.name.fluct <- instance.name
          cheapest.vCPU.fluct <- instance.vCPU
          cheapest.instances.required <- 1
        }
      }
    }
    
    # is the demand 0?
    if (current.vCPU.demand.fluct == 0) {
      result.fluct <- paste("[Spot Instance ", j, ": No additional Spot instances required]", sep = "")
      print(result.fluct)
      
      # make sure the next iteration works if this was the first iteration
      if (j == 1) {
        catch.iter <- 2
      }
      next
    }
    
    # No previous instance in the first iteration or in the second if the first had no demand
    if (j == catch.iter) {
      result.fluct <- print.fluct.workload(j, cheapest.name.fluct, cheapest.price.fluct, 
                                                  cheapest.instances.required, "Spot")
      print(result.fluct)
      # updating values
      prev.instance.price.fluct <- cheapest.price.fluct
      prev.instance.price.fluct.wo.amdahl <- cheapest.price.fluct.wo.amdahl
      prev.instance.name.fluct <- cheapest.name.fluct
      prev.instances.required.fluct <- cheapest.instances.required
      prev.instance.vCPU.fluct <- cheapest.vCPU.fluct
      next
    }
    
    # is the best instance different from the one before?
    if (cheapest.name.fluct != prev.instance.name.fluct) {
      # if so, migration costs need to be added (calculated based on previous instance configuration)
      cheapest.price.fluct.mig <- cheapest.price.fluct + prev.instance.price.fluct * 
          (migration.costs / (prev.instance.vCPU.fluct * prev.instances.required.fluct))
    }
    
    # calculating required instances and price for staying at the previous instance
    new.instances.required.with.prev <- ceiling(current.vCPU.demand.fluct / prev.instance.vCPU.fluct)
    new.instances.required.with.prev.amdahl <- 0
    new.price.with.prev <- 0
    
    # using amdahls law to determine the number of instances necessary to cope with required workload (90% of parallelization assumed)
    if (new.instances.required.with.prev < amdahl.max) {
      new.instances.required.with.prev.amdahl <- ceiling(amdahl.reversed(0.90, new.instances.required.with.prev))
      new.price.with.prev <- prev.instance.price.fluct.wo.amdahl * new.instances.required.with.prev.amdahl
    } else {
      # if the theoretically required instances are 10 or more, amdahls function goes to infinity, thus it is limited by 9
      new.instances.required.with.prev.amdahl <- ceiling(amdahl.reversed(0.90, amdahl.max))
      new.price.with.prev <- prev.instance.price.fluct.wo.amdahl * new.instances.required.with.prev.amdahl
    }
  
    # migrate or stay?
    if (cheapest.price.fluct.mig < new.price.with.prev) {
      # if migrate, print the instance with the added migration costs
      result.fluct <- print.fluct.workload(j, cheapest.name.fluct, cheapest.price.fluct.mig, 
                                                  cheapest.instances.required, "Spot")
      
      # updating values
      prev.instance.price.fluct <- cheapest.price.fluct
      prev.instance.price.fluct.wo.amdahl <- cheapest.price.fluct.wo.amdahl
      prev.instance.name.fluct <- cheapest.name.fluct
      prev.instances.required.fluct <- cheapest.instances.required
      prev.instance.vCPU.fluct <- cheapest.vCPU.fluct

    } else {
      # if stay, print the same instance as previous, but with updated values for price and instances required
      result.fluct <- print.fluct.workload(j, prev.instance.name.fluct, new.price.with.prev, 
                                                  new.instances.required.with.prev.amdahl, "Spot")
      
      # updating values
      prev.instance.price.fluct <- new.price.with.prev
      prev.instance.price.fluct.wo.amdahl <- prev.instance.price.fluct.wo.amdahl
      prev.instance.name.fluct <- prev.instance.name.fluct
      prev.instances.required.fluct <- new.instances.required.with.prev.amdahl
      prev.instance.vCPU.fluct <- prev.instance.vCPU.fluct
    }
    
    #print the final instance configuration
    print(result.fluct)
  }
}

find.cheapest.instance.Spot(list(8, 15, 26, 17, 18), 5)



# ---------------------------------------------- SPOT WITH MIGRATION COSTS + BASE LOAD -------------------------------------------- #

create.config.dataset <- function(amdahl.probability, amdahl.max, migration.costs) {
  
  aws.shared.all.configurations <- aws.shared.all.prices.with.interrupt.freq
  
  for (j in 1:nrow(aws.shared.all.prices.with.interrupt.freq)) {
    # calculate new spot prices based on interruption freq -> spot.real = spot + spot * freq * (mig.costs / vCPUs)
    aws.shared.all.configurations[j, "Spot.costs.real"] <- aws.shared.all.prices.with.interrupt.freq[j, "Spot.costs"] + 
      aws.shared.all.prices.with.interrupt.freq[j, "Spot.costs"] * aws.shared.all.prices.with.interrupt.freq[j, "Interruption.Freq"] *
      (migration.costs / aws.shared.all.prices.with.interrupt.freq[j, "vCPUs"])
  }
  
  # duplicate rows to be adapted by amdahl
  aws.shared.all.configurations <- aws.shared.all.configurations[rep(seq_len(nrow(aws.shared.all.configurations)), each = amdahl.max - 1), ]
  
  count = 1
  for (i in 1:nrow(aws.shared.all.configurations)) {
    
    # create column with theoretically required instances
    aws.shared.all.configurations[i, "instances.required.wo.amdahl"] <- count
    count <- count + 1
    
    if (count == amdahl.max) {
      count <- 1
    }
    
    # create column with required instances adapted with amdahl
    aws.shared.all.configurations[i, "instances.required.with.amdahl"] <- 
      ceiling(amdahl.reversed(amdahl.probability, aws.shared.all.configurations[i, "instances.required.wo.amdahl"]))
    
    # create column with final spot price adapted by interruption freq and amdahl
    aws.shared.all.configurations[i, "Spot.costs.final"] <- 
      aws.shared.all.configurations[i, "instances.required.with.amdahl"] * aws.shared.all.configurations[i, "Spot.costs.real"]
  }
  debug.df <<- aws.shared.all.configurations
  return(aws.shared.all.configurations)
}

generate.dataframe <- function() {
  df <- data.frame(
    Workload_Type = character(),
    Instance_Number = character(),
    Instance_Type = character(),
    Total_Costs = numeric(),
    Num_Instances_Required = numeric(),
    Plan = character(),
    Migration_Costs = numeric(),
    Migration_Time = numeric()
  )
  return(df)
}

populate.dataframe <- function(df, workload.tpye, instance.number, instance.type, total.costs, num.instances.req, plan, mig.costs, mig.time) {
  df <- rbind(df, data.frame(
    Workload_Type = workload.tpye,
    Instance_Number = instance.number,
    Instance_Type = instance.type,
    Total_Costs = total.costs,
    Num_Instances_Required = num.instances.req,
    Plan = plan,
    Migration_Costs = mig.costs,
    Migration_Time = mig.time
  ))
  return(df)
}


get.base.workload <- function(CPU.hours.per.hour.base) {
  
  df <- generate.dataframe()
  
  amdahl.prob = 0.95
  amdahl.max = 20
  current.instance.price <- Inf
  cheapest.price <- Inf
  result <- ""
  result.for.df <- list()
  
  # iterate through all rows in data frame to find cheapest instances
  for (i in 1:nrow(aws.shared.all.prices.with.interrupt.freq)) {
    
    instance.vCPU <- aws.shared.all.prices.with.interrupt.freq[i, "vCPUs"]
    instance.name <- aws.shared.all.prices.with.interrupt.freq[i, "API.Name"]
    # getting cheapest plan in each row and corresponding column name
    instance.min.costs.per.API <- apply(aws.shared.all.prices.with.interrupt.freq[i, 7:29], 1, min)
    instance.min.col.name <- names(aws.shared.all.prices.with.interrupt.freq)[apply(aws.shared.all.prices.with.interrupt.freq[i, 7:29], 1, which.min) + 6]
    
    # calculate how many instances would be required
    number.of.instances.required <- ceiling(CPU.hours.per.hour.base / instance.vCPU)
    
    # using amdahls law to determine the number of instances necessary to cope with required workload (limited by amdahl.max - 1)
    if (number.of.instances.required < amdahl.max) {
      number.of.instances.required.amdahl <- ceiling(amdahl.reversed(amdahl.prob, number.of.instances.required))
      current.instance.price <- instance.min.costs.per.API * number.of.instances.required.amdahl
    } else {
      # if the theoretically required instances are amdahl.max or more, amdahls function goes to infinity
      number.of.instances.required.amdahl <- Inf
      current.instance.price <- Inf 
    }
    
    # is it also the cheapest so far?
    if (current.instance.price < cheapest.price) {
      # empty list if there is a new cheapest instance 
      result <- list()
      result <- append(result, print.base.workload(instance.name, current.instance.price, number.of.instances.required.amdahl, instance.min.col.name))
      result.for.df <- list()
      result.for.df <- append(result.for.df, c(instance.name, current.instance.price, number.of.instances.required.amdahl, instance.min.col.name))
      cheapest.price <- current.instance.price
      
    } else if (current.instance.price == cheapest.price) {
      # extend list if there is an equally cheap instance
      result <- append(result, print.base.workload(instance.name, current.instance.price, number.of.instances.required.amdahl, instance.min.col.name))
      result.for.df <- append(result.for.df, c(instance.name, current.instance.price, number.of.instances.required.amdahl, instance.min.col.name))
      cheapest.price <- current.instance.price
    }
  }
  
  if (CPU.hours.per.hour.base == 0) {
    print("No instances for base workload required")
    df <- populate.dataframe(df, "Base", 0, "NA", 0, 0, "NA", 0, 0)
    return(df)
  } else {
    # printing the result
    result_final <- paste(paste(result))
    cat(result_final, sep = "\n")
    
    # put every instance in a dataframe
    for (j in seq(1, length(result.for.df), by = 4)) {
      df <- populate.dataframe(df, "Base", 0, result.for.df[[j]], as.numeric(result.for.df[[j+1]]), result.for.df[[j+2]], result.for.df[[j+3]], 0, 0)
    }
    return(df)
  }
}

#df2 <- get.base.workload(50)


# find.cheapest.instance.OD.RI.SP(50)



find.cheapest.instance.final <- function(CPU.hours.per.hour.base, CPU.hours.per.hour.fluct, migration.costs) {
  
  # print demand warning
  print("Please note that the current implementation takes in workloads up to 2432 CPU hours per hour.")
  
  # calculate and print the base workload
  df <- get.base.workload(CPU.hours.per.hour.base)
  
  # setting amdahl parameters
  amdahl.max <- 20
  amdahl.prob <- 0.95
  
  # create working data set with all configurations 
  aws.shared.all.configurations <- create.config.dataset(amdahl.prob, amdahl.max, migration.costs)
  
  # create local variables 
  prev.instance.price.fluct <- 0
  prev.instance.price.fluct.wo.amdahl <- 0
  prev.instances.required.fluct <- 0
  prev.instance.vCPU.fluct <- 0
  prev.instance.name.fluct <- ""
  catch.iter <- 1
  
  # calculating best configuration for each hourly demand
  for (j in 1:length(CPU.hours.per.hour.fluct)) {
    
    # create temporary variables
    current.instance.price.fluct <- Inf
    cheapest.price.fluct <- Inf
    cheapest.name.fluct <- ""
    cheapest.vCPU.fluct <- 0
    cheapest.instances.required <- 0
    cheapest.instances.required.amdahl <- 0
    cheapest.price.fluct.mig <- Inf
    cheapest.price.fluct.wo.amdahl <- Inf
    current.vCPU.demand.fluct <- CPU.hours.per.hour.fluct[[j]]
    mig.costs.dollar <- 0
    mig.time <- 0
    
    # find the cheapest instance for the current demand
    for (i in 1:nrow(aws.shared.all.configurations)) {
      instance.vCPU <- aws.shared.all.configurations[i, "vCPUs"]
      instance.name <- aws.shared.all.configurations[i, "API.Name"]
      instance.min.costs.per.API.fluct <- aws.shared.all.configurations[i, "Spot.costs.final"]
      number.of.instances.required.fluct <- ceiling(current.vCPU.demand.fluct / instance.vCPU)
      
      # Of each configuration for one instance, get only the spot price of the one that fits the demand
      if (number.of.instances.required.fluct == aws.shared.all.configurations[i, "instances.required.wo.amdahl"]) {
        current.instance.price.fluct <- instance.min.costs.per.API.fluct
      }
      
      # is it the cheapest so far
      if (current.instance.price.fluct < cheapest.price.fluct) {
        # updating values
        cheapest.price.fluct <- current.instance.price.fluct
        cheapest.price.fluct.wo.amdahl <- aws.shared.all.configurations[i, "Spot.costs.real"]
        cheapest.name.fluct <- instance.name
        cheapest.vCPU.fluct <- instance.vCPU
        cheapest.instances.required <- aws.shared.all.configurations[i, "instances.required.wo.amdahl"]
        cheapest.instances.required.amdahl <- aws.shared.all.configurations[i, "instances.required.with.amdahl"]
      }
    }
    
    
    # catching special cases
    if (current.vCPU.demand.fluct == 0) {
      result.fluct <- paste("[Spot Instance ", j, ": No additional Spot instances required]", sep = "")
      print(result.fluct)
      
      df <- populate.dataframe(df, "Fluct", j, "NA", 0, 0, "Spot", 0, 0)
      
      # make sure the next iteration works if this was the first iteration 
      if (j == 1) {
        catch.iter <- 2
      }
      next
    }
    if (j == catch.iter) {
      result.fluct <- print.fluct.workload.mig(j, cheapest.name.fluct, cheapest.price.fluct, 
                                           cheapest.instances.required.amdahl, "Spot", mig.costs.dollar, mig.time)
      print(result.fluct)
      
      df <- populate.dataframe(df, "Fluct", j, cheapest.name.fluct, cheapest.price.fluct, 
                               cheapest.instances.required.amdahl, "Spot", mig.costs.dollar, mig.time)
      
      # updating values
      prev.instance.price.fluct <- cheapest.price.fluct
      prev.instance.price.fluct.wo.amdahl <- cheapest.price.fluct.wo.amdahl
      prev.instance.name.fluct <- cheapest.name.fluct
      prev.instances.required.fluct <- cheapest.instances.required.amdahl
      prev.instance.vCPU.fluct <- cheapest.vCPU.fluct
      next
    }
    
    # is the best instance different from the one before?
    if (cheapest.name.fluct != prev.instance.name.fluct) {
      # if so, migration costs need to be added (calculated based on previous instance configuration)
      mig.costs.dollar <- prev.instance.price.fluct * 
        (migration.costs / (prev.instance.vCPU.fluct * prev.instances.required.fluct))
      mig.time <- migration.costs / (prev.instance.vCPU.fluct * prev.instances.required.fluct)
      
      cheapest.price.fluct.mig <- cheapest.price.fluct + mig.costs.dollar
    }
    
    
    # calculating required instances and price for staying at the previous instance
    new.instances.required.with.prev <- ceiling(current.vCPU.demand.fluct / prev.instance.vCPU.fluct)
    
    # catch case if new.instances.required.with.prev is bigger than amdahl.max -> no values in dataframe
    if (new.instances.required.with.prev < amdahl.max) {
      # get new price with prev based on prev name and new instances required
      new.price.with.prev <- 
        aws.shared.all.configurations$Spot.costs.final[aws.shared.all.configurations$API.Name == prev.instance.name.fluct &
                                                       aws.shared.all.configurations$instances.required.wo.amdahl == new.instances.required.with.prev]
      
      new.instances.required.with.prev.amdahl <- 
        aws.shared.all.configurations$instances.required.with.amdahl[aws.shared.all.configurations$API.Name == prev.instance.name.fluct &
                                                                     aws.shared.all.configurations$instances.required.wo.amdahl == new.instances.required.with.prev]
    } else {
      new.price.with.prev <- Inf
      new.instances.required.with.prev.amdahl <- Inf
    }
    
    
    # migrate or stay?
    if (cheapest.price.fluct.mig < new.price.with.prev) {
      # if migrate, print the instance with the added migration costs
      result.fluct <- print.fluct.workload.mig(j, cheapest.name.fluct, cheapest.price.fluct.mig, 
                                           cheapest.instances.required.amdahl, "Spot", mig.costs.dollar, mig.time)
 
      df <- populate.dataframe(df, "Fluct", j, cheapest.name.fluct, cheapest.price.fluct.mig, 
                               cheapest.instances.required.amdahl, "Spot", mig.costs.dollar, mig.time)
      
      # updating values
      prev.instance.price.fluct <- cheapest.price.fluct
      prev.instance.name.fluct <- cheapest.name.fluct
      prev.instances.required.fluct <- cheapest.instances.required.amdahl
      prev.instance.vCPU.fluct <- cheapest.vCPU.fluct
      
    } else {
      # if stay, print the same instance as previous, but with updated values for price and instances required
      mig.costs.dollar <- 0
      mig.time <- 0
      result.fluct <- print.fluct.workload.mig(j, prev.instance.name.fluct, new.price.with.prev, 
                                           new.instances.required.with.prev.amdahl, "Spot", mig.costs.dollar, mig.time)
      
      df <- populate.dataframe(df, "Fluct", j, prev.instance.name.fluct, new.price.with.prev, 
                               new.instances.required.with.prev.amdahl, "Spot", mig.costs.dollar, mig.time)
      
      # updating values
      prev.instance.price.fluct <- new.price.with.prev
      prev.instance.name.fluct <- prev.instance.name.fluct
      prev.instances.required.fluct <- new.instances.required.with.prev
      prev.instance.vCPU.fluct <- prev.instance.vCPU.fluct
    }
    
    #print the final instance configuration
    print(result.fluct)
  }
  return(df)
}


df.example <- find.cheapest.instance.final(16, list(8, 15, 26, 17, 18, 24, 3, 8, 14, 8, 15, 26, 17, 18, 50, 3, 8, 14, 10, 21, 15, 18, 6, 2), 5)

find.cheapest.instance.Spot(list(8, 15, 26, 17, 18, 50, 3, 8, 14), 5)

find.cheapest.instance.OD.RI.SP(200)


list(8, 15, 26, 17, 18, 50, 3, 8, 14)
list(8, 15, 26, 17, 18, 24, 3, 8, 14, 8, 15, 26, 17, 18, 50, 3, 8, 14, 10, 21, 15, 18, 6, 2)


# Offen im Code:
#   - Data processing anpassen
#   - plots zu Szenarien machen
#   - Szenarien als festgesetzte Liste von Parametern die dann benutzt werden
#   - aktuellere und bessere Daten kriegen
#   - Wieso ist der amdahl parameter bei 17 77 und nicht 76? Rundung? -> check: ganz bisschen über der Zahl -> evtl. nur aufrunden ab .1?
#   - Evtl. Amdahl prob als parameter in die funktion und dann amdahl.max berechnen



# szenarien mit spezifischen Zahlen -> small, medium, large business
# ggplots dazu 
# Annahmen nochmal überprüfen
# funktion dataframe zurückgeben für ggplot -> check
# Zahlen im Beispiel






# Annahmen Spot:
#   - Eine Migration auf eine andere Instanz kostet Geld: Cm = T (in CPU/h, als Parameter in Funktion) * C (Kosten der aktuellen Instanz)
#   - Zusätzlich wird nicht mit normalen sondern angepassten Spot Preisen gerechnet, die die Interruption frequencies berücksichtigen -> check
#   - Keine Migrationskosten, wenn auf der gleichen Instanz skaliert wird (egal ob runter oder hoch) -> innerhalb der gleichen Instantz nur einen
#       Teil (50%) der Migrationskosten
#   - Durch die angepassten Spot Preise sind Migrationskosten auch auf der gleichen Instanz einberechnet
#   - Der Migrationsaufwand ist konstant (egal von welcher auf welche Instanz) 
#   - Bei Interruption freqs die Mitte genommen, passt?
#   - Nachteil neue Herangehensweise: Doppelte Rundung bei Amdahl -> Parallelisierung noch etwas schlechter (grade wieder gleich)
#   - Unterschiedliche Rundung in beiden Herangehensweisen
#   - Double ceiling noch offen


# Annahmen allgemein:
#   - Es sind 95% der workloads parallelisierbar (s. Amdahl erster Parameter)
#   - Laufzeit ist unendlich -> alle Preispläne sind relevant
#   - Nur die Anzahl der vCPUs ist wichtig, da nur der Workload in CPU hours abgearbeitet werden muss
#   - vCPUs werden behandelt wie normale CPUs, nicht wie Hyperthreads
#   - Es ist nicht schlimm, dass es bei SPs keine Garantie gibt, die Instanz auch wirklich zu erhalten
#   - Bisher nur Instanzen inkludiert, die Daten auf allen Preisplänen, interruption freqs etc. haben
#   - Bei Base Workloads sind nur bis zu ca. 2000 CPU Stunden demand möglich -> Maximum was mit einer Instanz und Amdahl machbar ist
#   - Gleiches gilt für fluct workloads

# Aktuelle Fragen:
#   - Wie ist der Ansatz mit unterschiedlichen Instanzen für jede Stunde? Mehr Overhead, dafür weniger wasted resources 
#       -> theoretisch billigste Lösung, aber auch die beste? Realistisch?
#   - Unterschied in datasets -> fehlt z.B. c6a, sollte man vielleicht mit rein nehmen, wie mit NA values umgehen?
#   - Allgemeine Idee für später: Default werte sind unendliche Laufzeit etc., bei Bedarf kann man aber mehr spezifizieren
#   - Berechnung Amdahl angepasst -> Rundungsfehler
#   - Grade noch Spot Preise von einem bestimten Zeitpunkt -> besser Durchschnitt über letzten 3 Monate?
#   - Passt die Rechnung vom Migration price? Zahle ich die alte länger oder die neue früher? -> passt

