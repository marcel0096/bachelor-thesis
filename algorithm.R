# ----------------------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------- FORMATTING OUTPUT STRINGS ---------------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #

format.plan.string <- function(plan) {
  plan_parts <- strsplit(plan, ".", fixed = TRUE)[[1]]

  if (plan_parts[1] == "SP") {
    plan_formatted <- paste0("Savings Plan", " (", paste(plan_parts[4:6], collapse = ", "), ", ", plan_parts[7], ")")
  } else if (plan_parts[1] == "RI") {
    plan_formatted <- paste0("Reserved Instance", " (", paste(plan_parts[4:6], collapse = ", "), ", ", plan_parts[7], ")")
  } else {
    plan_formatted <- "On Demand"
  }

  return(plan_formatted)
}


print.base.workload <- function(API.name, costs, required.instances, plan) {
  output = paste("[Base Workload - ", "Instance: ", API.name, "; Total Costs: ", costs,
                 "$; Number of Instances required: ", required.instances,
                 "; Plan: ", format.plan.string(plan), "]", sep = "")
  return(output)
}


print.fluct.workload.mig <- function(index, API.name, costs, required.instances, plan, migration.costs, migration.time) {
  output <- paste("[", "Spot Instance ", index, ": ", API.name, "; Total Costs: ", costs, 
                  "$; Number of Instances required: ", required.instances, 
                  "; Plan: ", plan, "; Migration Costs: ", migration.costs, 
                  "$; Migration Time: ", migration.time, " CPU hours]", sep = "")
  return(output)
}


# ----------------------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------- FINAL ALGORITHM -------------------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #

## Final version using all pricing data available as well as spot interruption frequencies
## Spot prices are adapted based on interruption frequencies and migration costs
## Scaling with amdahl is more fine grained using a special rounding function
## Base workload and fluctuating workload is printed as well as migration cost and migration time
## Results are printed on console and put into data frame

round.amdahl.instances <- function(amdahl.param, instances.required.wo.amdahl) {
  
  instances.required.with.amdahl <- amdahl.reversed(amdahl.param, instances.required.wo.amdahl)
  
  # only round up if value is at least .05 above the last integer
  cut.off <- instances.required.with.amdahl %% 1
  
  if (cut.off > 0.05) {
    return(ceiling(instances.required.with.amdahl))
  } else {
    return(floor(instances.required.with.amdahl))
  }
}

# cache frequently created config data set to enhance performance
cached.config.dataset <- NULL

create.config.dataset <- function(amdahl.param, amdahl.max, migration.costs) {
  
  aws.shared.all.configurations <- aws.all.prices
  
  for (j in 1:nrow(aws.all.prices)) {
    # calculate new spot prices based on interruption freq -> spot.real = spot + spot * freq * (mig.costs / vCPUs)
    aws.shared.all.configurations[j, "Spot.costs.real"] <- aws.all.prices[j, "Spot.costs.hourly"] + 
      aws.all.prices[j, "Spot.costs.hourly"] * 
      aws.all.prices[j, "Interruption.Freq"] * (migration.costs / aws.all.prices[j, "vCPUs"])
  }
  
  # duplicate rows to be adapted by amdahl
  aws.shared.all.configurations <- 
    aws.shared.all.configurations[rep(seq_len(nrow(aws.shared.all.configurations)), each = amdahl.max - 1), ]
  
  required = 1
  for (i in 1:nrow(aws.shared.all.configurations)) {
    
    # create column with theoretically required instances
    aws.shared.all.configurations[i, "instances.required.wo.amdahl"] <- required
    required <- required + 1
    
    if (required == amdahl.max) {
      required <- 1
    }
    
    # create column with required instances adapted with amdahl
    aws.shared.all.configurations[i, "instances.required.with.amdahl"] <- 
      round.amdahl.instances(amdahl.param, aws.shared.all.configurations[i, "instances.required.wo.amdahl"])
    
    # create column with final spot price adapted by interruption freq and amdahl
    aws.shared.all.configurations[i, "Spot.costs.final"] <- 
      aws.shared.all.configurations[i, "instances.required.with.amdahl"] * 
      aws.shared.all.configurations[i, "Spot.costs.real"]
  }
  
  #assign(paste(c("config.df", amdahl.param), collapse = "."), aws.shared.all.configurations, envir = .GlobalEnv)
  cached.config.dataset <<- aws.shared.all.configurations
  
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


populate.dataframe <- function(df, workload.tpye, instance.number, instance.type, 
                               total.costs, num.instances.req, plan, mig.costs, mig.time) {
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


get.base.workload <- function(CPU.hours.per.hour.base, amdahl.param, amdahl.max, plan, type, duration, payment) {
  
  # catch invalid combinations
  if ((plan == "SP" & (type == "standard" | type == "convertible")) | (plan == "RI" & (type == "Compute" | type == "EC2Instance"))) {
    print("Error: Invalid combination of price plans and options. Base workload cannot be calculated.")
    return()
  }
    
  df <- generate.dataframe()
  
  # Determine which columns to search
  if (plan == "OD") {
    target.columns <- grep(paste(paste0(".*\\b(", "OD", ")"), collapse = ""), 
                           colnames(aws.all.prices), perl = TRUE)
  } else {
    target.columns <- grep(paste(paste0(".*\\b(", c(plan, type, duration, payment), ")"), collapse = ""), 
                           colnames(aws.all.prices), perl = TRUE)
  }
  
  current.instance.price <- Inf
  cheapest.price <- Inf
  result <- ""
  result.for.df <- list()

  for (i in 1:nrow(aws.all.prices)) {

    instance.vCPU <- aws.all.prices[i, "vCPUs"]
    instance.name <- aws.all.prices[i, "API.Name"]
    
    # getting cheapest plan in each row and corresponding column name
    if (length(target.columns) > 1) {
      instance.min.costs.per.API <- apply(aws.all.prices[i, target.columns], 1, min)
    } else {
      instance.min.costs.per.API <- sapply(aws.all.prices[i, target.columns], min)
    }
    min.col.index <- which(aws.all.prices[i, target.columns] == instance.min.costs.per.API)
    instance.min.col.name <- colnames(aws.all.prices)[target.columns[min.col.index]]

    # calculate how many instances would be required
    number.of.instances.required <- ceiling(CPU.hours.per.hour.base / instance.vCPU)

    # using amdahls law to determine the number of instances necessary to cope with required workload (limited by amdahl.max - 1)
    if (number.of.instances.required < amdahl.max) {
      number.of.instances.required.amdahl <- round.amdahl.instances(amdahl.param, number.of.instances.required)
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
    
    # put every instance in a data frame
    for (j in seq(1, length(result.for.df), by = 4)) {
      df <- populate.dataframe(df, "Base", 0, result.for.df[[j]], as.numeric(result.for.df[[j+1]]), result.for.df[[j+2]], result.for.df[[j+3]], 0, 0)
    }
    return(df)
  }
}


find.cheapest.instance.final <- function(CPU.hours.per.hour.base, CPU.hours.per.hour.fluct, migration.costs, 
                                         amdahl.param = 0.95, amdahl.max = 20,
                                         plan = "OD|RI|SP", type = "standard|convertible|Compute|EC2Instance", 
                                         duration = "1|3", payment = "All|Partial|No") {
  
  # get and print demand warning
  max.workload <- max(aws.all.prices$vCPUs) * (amdahl.max - 1)
  print(paste(c("Please note that the current implementation takes in workloads up to", max.workload, "CPU hours per hour."), collapse = " "))
  
  # calculate and print the base workload
  df <- get.base.workload(CPU.hours.per.hour.base, amdahl.param, amdahl.max, plan, type, duration, payment)
  
  # check if needed data set is already in cache
  if (!is.null(cached.config.dataset) && identical(amdahl.param, cached.amdahl.param)) {
    aws.shared.all.configurations <- cached.config.dataset
  } else {
    # Create working dataset
    aws.shared.all.configurations <- create.config.dataset(amdahl.param, amdahl.max, migration.costs)
    cached.amdahl.param <<- amdahl.param
  }
  
  # create local variables 
  prev.instance.price.fluct <- 0
  prev.instance.price.fluct.wo.amdahl <- 0
  prev.instances.required.fluct <- 0
  prev.instance.vCPU.fluct <- 0
  prev.instance.name.fluct <- ""
  catch.iter <- 1
  saw.prev <- 1
  
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
      if (saw.prev == 1) {
        catch.iter <- j + 1
      }
      next
    }
    # print first workload after zeros
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
      saw.prev <- 0
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

df.example <- find.cheapest.instance.final(695, list(0, 0, 0, 17, 18, 24, 3, 8, 2400, 8, 15, 26, 17, 18, 50, 3, 8, 14, 10, 21, 15, 18, 6, 2), 5, 
                                           plan = "RI", type = "standard", duration = "1", payment = "No|Partial|All")


# Notes:
#   - szenarien mit spezifischen Zahlen -> small, medium, large business -> check
#   - plots zu Szenarien machen -> in progress
#   - Szenarien als festgesetzte Liste von Parametern die dann benutzt werden -> offen
#   - Evtl. Amdahl prob als parameter in die funktion und dann amdahl.max berechnen -> offen
#   - Bisher kein Check ob Spot Preis wirklich der billigste ist -> offen
#   - Bisher kein Wachstum mit drinnen -> offen
#   - Anpassen base load mit parameter damit man entscheiden kann für wie viele Jahre man sich committen will, evtl. auch growth rate -> check 
#   - Evtl. auch Memory und Network als config mit rein? -> offen
#   - Bei amdahl 0.99 und 100 ist der algorithmus sehr langsam, da riesiges data set

# Annahmen Spot:
#   - Eine Migration auf eine andere Instanz kostet Geld: Cm = T (in CPU/h, als Parameter in Funktion) * C (Kosten der aktuellen Instanz)
#   - Zusätzlich wird nicht mit normalen sondern angepassten Spot Preisen gerechnet, die die Interruption frequencies berücksichtigen -> check
#   - Keine Migrationskosten, wenn auf der gleichen Instanz skaliert wird (egal ob runter oder hoch) -> innerhalb der gleichen Instantz nur einen
#       Teil (50%) der Migrationskosten
#   - Durch die angepassten Spot Preise sind Migrationskosten auch auf der gleichen Instanz einberechnet
#   - Der Migrationsaufwand ist konstant (egal von welcher auf welche Instanz) 
#   - Bei Interruption freqs Mittelwert aus lower und upper bound
#   - Double ceiling noch offen -> angepasst durch genauere Rundungsfunktion

# Annahmen allgemein:
#   - Es sind 95% der workloads parallelisierbar (s. Amdahl erster Parameter)
#   - Laufzeit ist unendlich -> alle Preispläne sind relevant -> angepasst
#   - Nur die Anzahl der vCPUs ist wichtig, da nur der Workload in CPU hours abgearbeitet werden muss
#   - vCPUs werden behandelt wie normale CPUs, nicht wie Hyperthreads -> evtl. noch anpassen
#   - Es ist nicht schlimm, dass es bei SPs keine Garantie gibt, die Instanz auch wirklich zu erhalten
#   - Nur Instanzen inkludiert, die Daten auf allen Preisplänen, interruption freqs etc. haben
#   - Bei Workloads sind bis zu 3648 CPU Stunden demand möglich -> Maximum was mit einer Instanz und Amdahl machbar ist
