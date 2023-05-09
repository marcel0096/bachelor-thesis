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

find.cheapest.instance.on.demand.v2(10)



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

find.cheapest.instance.OD.RI.SP(96)



# --- ALGORITHM WITH BASIC WORKLOAD USING ON DEMAND, RESERVED INSTANCES, SAVINGS PLANS | fluctuating WORKLOAD USING SPOT --- # 

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
      
      # getting spot price in each row
      instance.min.costs.per.API.fluct <- aws.shared.all.prices[i, "Spot.costs"]
      
      # does one instance of the instance type alone have enough vCPUs to serve the need of the fluctuating demands
      if (instance.vCPU < CPU.hours.per.hour.fluct[[j]]) {
        # if not, calculate how many instances would be required
        number.of.instances.required.fluct <- ceiling(CPU.hours.per.hour.fluct[[j]] / instance.vCPU)
        
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
          result.fluct <- append(result.fluct, paste("[", "Spot Instance ", j, ": ", instance.name, "; Total Costs: ", current.instance.price.fluct, 
                                         "$; Number of Instances required: ", number.of.instances.required.fluct.amdahl, 
                                         "; Plan: Spot", "]", sep = ""))
          cheapest.price.fluct <- current.instance.price.fluct
        } else if (current.instance.price.fluct == cheapest.price.fluct) {
          # extend list if there is an equally cheap instance
          result.fluct <- append(result.fluct, paste("[", "Spot Instance ", j, ": ", instance.name, "; Total Costs: ", current.instance.price.fluct, 
                                               "$; Number of Instances required: ", number.of.instances.required.fluct.amdahl, 
                                               "; Plan: Spot", "]", sep = ""))
          cheapest.price.fluct <- current.instance.price.fluct
        }
        
      } else {
        # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
        current.instance.price.fluct <- instance.min.costs.per.API.fluct
        
        # is it also the cheapest so far?
        if (current.instance.price.fluct < cheapest.price.fluct) {
          result.fluct <- list()
          result.fluct <- append(result.fluct, paste("[", "Spot Instance ", j, ": ", instance.name, "; Total Costs: ", current.instance.price.fluct, 
                                               "$; Number of Instances required: ", 1, "; Plan: Spot", "]", sep = ""))
          cheapest.price.fluct <- current.instance.price.fluct
        } else if (current.instance.price.fluct == cheapest.price.fluct) {
          result.fluct <- append(result.fluct, paste("[", "Spot Instance ", j, ": ", instance.name, "; Total Costs: ", current.instance.price.fluct, 
                                               "$; Number of Instances required: ", 1, "; Plan: Spot", "]", sep = ""))
          cheapest.price.fluct <- current.instance.price.fluct
        }
      }
      # FINISHED SEARCHING FOR CHEAPEST SPOT
        
      # searching for the cheapest instance of the base value only in the last iteration
      if (j == length(CPU.hours.per.hour.fluct)) {
        
        # getting cheapest plan for base workload in each row and corresponding column name (only searching RIs and SPs)
        instance.min.costs.per.API.base <- apply(aws.shared.all.prices[i, 7:29], 1, min)
        instance.min.col.name.base <- names(aws.shared.all.prices)[apply(aws.shared.all.prices[i, 7:29], 1, which.min) + 6]
         
        # does one instance of the instance type alone have enough vCPUs to serve the need?
        if (instance.vCPU < CPU.hours.per.hour.base) {
          # if not, calculate how many instances would be required
          number.of.instances.required.base <- ceiling(CPU.hours.per.hour.base / instance.vCPU)
            
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
            result.base <- append(result.base, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price.base, 
                                            "$; Number of Instances required: ", number.of.instances.required.base.amdahl, 
                                            "; Plan: ", helper.format.plan.string(instance.min.col.name.base), 
                                            "]", sep = ""))
            cheapest.price.base <- current.instance.price.base
          } else if (current.instance.price.base == cheapest.price.base) {
            # extend list if there is an equally cheap instance
            result.base <- append(result.base, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price.base, 
                                            "$; Number of Instances required: ", number.of.instances.required.base.amdahl, 
                                            "; Plan: ", helper.format.plan.string(instance.min.col.name.base),
                                            "]", sep = ""))
            cheapest.price.base <- current.instance.price.base
          }
            
        } else {
          # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
          current.instance.price.base <- instance.min.costs.per.API.base
            
          # is it also the cheapest so far?
          if (current.instance.price.base < cheapest.price.base) {
            result.base <- list()
            result.base <- append(result.base, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price.base, 
                                             "$; Number of Instances required: ", 1, "; Plan: ", helper.format.plan.string(instance.min.col.name.base), 
                                             "]", sep = ""))
            cheapest.price.base <- current.instance.price.base
          } else if (current.instance.price.base == cheapest.price.base) {
            result.base <- append(result.base, paste("[", "Instance: ", instance.name, "; Total Costs: ", current.instance.price.base, 
                                             "$; Number of Instances required: ", 1, "; Plan: ", helper.format.plan.string(instance.min.col.name.base), 
                                             "]", sep = ""))
            cheapest.price.base <- current.instance.price.base
          }
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

find.cheapest.instance.OD.RI.SP.Spot(50, list(0, 2, 3, 5, 3, 2, 6, 15, 18, 20, 19, 12, 11, 13, 15, 14, 17, 19, 24, 18, 18, 18, 13, 5))

find.cheapest.instance.OD.RI.SP(50)





# ---- Annahmen bisher ------
#   - Es sind 90% der workloads parallelisierbar (s. Amdahl erster Parameter)
#   - Laufzeit ist unendlich -> alle Preispläne sind relevant
#   - Nur die Anzahl der vCPUs ist wichtig, da nur der Workload in CPU hours abgearbeitet werden muss
#   - vCPUs werden behandelt wie normale CPUs, nicht wie Hyperthreads
#   - Es ist nicht schlimm, dass es bei SPs keine Garantie gibt, die Instanz auch wirklich zu erhalten





# ----------- NOTES: ------------
#   - Alle Daten vorhanden -> check
#   - Ausgabe verbesseren und verschönern: Wie heißt die Instanz? Wie viele brauche ich davon? Wie teuer wird das? In welchem Plan soll man das kaufen? -> check
#   - Eingabe direkt mit CPU hours pro Stunde und nicht pro Tag -> check
#   - Bei kleinen Instanzen nicht linear skalieren sondern immer mit einem Faktor sodass sich Leistung z.B. nicht verdoppelt, sondern nur ver-1,8-facht (s. Amdahls law) -> check

#   - Schwankenden Workload einbauen durch das mitgeben eines arrays mit 24 werten -> open
#   - zweiter parameter für Spot: Wie viele CPU Stunden muss ich aufwenden um eine Inetrruption zu recovern. 
#     Zusammen mit Wahrscheinlichkeit, dass die Interruption eintrifft
#   - Unterschied in datasets anschauen -> fehlen wichtige?
#   - Grade noch Spot Preise von einem bestimmten Zeitpunkt -> besser Durchschnitt über letzten 3 Monate


