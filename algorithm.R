# ------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------- ALGORITHMS ---------------------------------------------------- # 
# ------------------------------------------------------------------------------------------------------------------- #



# ----------------------------------------- HELPER FUNCTIONS -------------------------------------------------------- # 

helper.format.plan.string <- function(plan) {
  plan_parts <- strsplit(plan, ".", fixed = TRUE)[[1]]
  
  if (plan_parts[1] == "SP") {
    plan_formatted <- paste0("Savings Plan", " (", paste(plan_parts[3:5], collapse = ", "), ", ", plan_parts[6], ")")
  } else {
    plan_formatted <- paste0("Reserved Instance", " (", paste(plan_parts[3:5], collapse = ", "), ", ", plan_parts[6], ")")
  }
  
  return(plan_formatted)
}

# --------------------------------- ALGORITHM FOR ON DEMAND w/o AMDAHL ---------------------------------------------- # 

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

find.cheapest.instance.on.demand.v1(10)



# -------------------------------- ALGORITHM FOR ON DEMAND WITH AMDAHL ------------------------------------------- # 

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
      
      # using amdahls law to determine the number of instances necessary to cope with required workload (95% of parallelization assumed)
      if (number.of.instances.required < 10) {
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

find.cheapest.instance.on.demand.v2(64)



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
      if (number.of.instances.required < 10) {
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
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", number.of.instances.required.amdahl, 
                                       "; Plan: ", helper.format.plan.string(instance.min.col.name), 
                                       "]", sep = ""))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        # extend list if there is an equally cheap instance
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", number.of.instances.required.amdahl, 
                                       "; Plan: ", helper.format.plan.string(instance.min.col.name),
                                       "]", sep = ""))
        cheapest.price <- current.instance.price
      }
      
    } else {
      # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
      current.instance.price <- instance.min.costs.per.API 
      
      # is it also the cheapest so far?
      if (current.instance.price < cheapest.price) {
        result <- list()
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", 1, "; Plan: ", helper.format.plan.string(instance.min.col.name), 
                                       "]", sep = ""))
        cheapest.price <- current.instance.price
      } else if (current.instance.price == cheapest.price) {
        result <- append(result, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price, 
                                       "$; Number of Instances required: ", 1, "; Plan: ", helper.format.plan.string(instance.min.col.name), 
                                       "]", sep = ""))
        cheapest.price <- current.instance.price
      }
    }
  }
  
  # printing the result
  result_final <- paste(paste(result))
  cat(result_final, sep = "\n")
}

find.cheapest.instance.OD.RI.SP(128)



# ----------- NOTES: ------------
#   - Alle Daten vorhanden -> check
#   - Ausgabe verbesseren und verschönern: Wie heißt die Instanz? Wie viele brauche ich davon? Wie teuer wird das? In welchem Plan soll man das kaufen? -> check
#   - Eingabe direkt mit CPU hours pro Stunde und nicht pro Tag -> check
#   - Bei kleinen Instanzen nicht linear skalieren sondern immer mit einem Faktor sodass sich Leistung z.B. nicht verdoppelt, sondern nur ver-1,8-facht (s. Amdahls law) -> check

#   - Schwankenden Workload einbauen durch das mitgeben eines arrays mit 24 werten -> open





