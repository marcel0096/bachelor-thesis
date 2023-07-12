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
  
  # populate the data frame with each potential configuration
  aws.shared.all.configurations <- 
    aws.shared.all.configurations[rep(seq_len(nrow(aws.shared.all.configurations)), each = amdahl.max - 1), ]
  
  required = 1
  for (i in 1:nrow(aws.shared.all.configurations)) {
    
    aws.shared.all.configurations[i, "instances.required.wo.amdahl"] <- required
    
    required <- required + 1
    if (required == amdahl.max) {
      required <- 1
    }
    
    aws.shared.all.configurations[i, "instances.required.with.amdahl"] <- 
      round.amdahl.instances(amdahl.param, aws.shared.all.configurations[i, "instances.required.wo.amdahl"])
    
    aws.shared.all.configurations[i, "Spot.costs.final"] <- 
      aws.shared.all.configurations[i, "instances.required.with.amdahl"] * 
      aws.shared.all.configurations[i, "Spot.costs.real"]
  }
  
  cached.config.dataset <<- aws.shared.all.configurations
  
  return(aws.shared.all.configurations)
}


get.base.workload <- function(workload.base, amdahl.param, amdahl.max, plan, type, duration, payment) {
  
  # catch invalid combinations
  if ((plan == "SP" & (type == "standard" | type == "convertible")) | 
      (plan == "RI" & (type == "Compute" | type == "EC2Instance"))) {
    
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

  # search whole data set and find cheapest option
  for (i in 1:nrow(aws.all.prices)) {

    instance.vCPU <- aws.all.prices[i, "vCPUs"]
    instance.name <- aws.all.prices[i, "API.Name"]
    
    if (length(target.columns) > 1) {
      instance.min.costs.per.API <- apply(aws.all.prices[i, target.columns], 1, min)
    } else {
      instance.min.costs.per.API <- sapply(aws.all.prices[i, target.columns], min)
    }
    min.col.index <- which(aws.all.prices[i, target.columns] == instance.min.costs.per.API)[1]
    instance.min.col.name <- colnames(aws.all.prices)[target.columns[min.col.index]]

    number.of.instances.required <- ceiling(workload.base / instance.vCPU)

    # using amdahls law to determine the number of instances necessary to cope with required workload (limited by amdahl.max - 1)
    if (number.of.instances.required < amdahl.max) {
      number.of.instances.required.amdahl <- round.amdahl.instances(amdahl.param, number.of.instances.required)
      current.instance.price <- instance.min.costs.per.API * number.of.instances.required.amdahl
    } else {
      # if the theoretically required instances are >= amdahl.max -> Inf
      number.of.instances.required.amdahl <- Inf
      current.instance.price <- Inf
    }

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
  
  if (workload.base == 0) {
    print("No instances for base workload required")
    df <- populate.dataframe(df, "Base", 0, "NA", 0, 0, "NA", 0, 0)
    return(df)
  } else {
    result_final <- paste(paste(result))
    cat(result_final, sep = "\n")

    # put every instance in a data frame
    for (j in seq(1, length(result.for.df), by = 4)) {
      df <- populate.dataframe(df, "Base", 0, result.for.df[[j]], as.numeric(result.for.df[[j+1]]), 
                               result.for.df[[j+2]], result.for.df[[j+3]], 0, 0)
    }
    return(df)
  }
}


find.cheapest.instance.final <- function(CPU.hours.per.hour.base, CPU.hours.per.hour.fluct, migration.costs,
                                         plan = "OD|RI|SP", type = "standard|convertible|Compute|EC2Instance",
                                         duration = "1|3", payment = "All|Partial|No", amdahl.param = 0.95) {
  
  # calculate the maximum value for amdahl
  amdahl.max <- ceiling(1 / (1 - amdahl.param))
  
  # Get and print demand warning
  max.workload <- max(aws.all.prices$vCPUs) * (amdahl.max - 1)
  message("Please note that the current implementation takes in workloads up to ", max.workload, " CPU hours per hour.")
  
  # Calculate the base workload
  df <- get.base.workload(CPU.hours.per.hour.base, amdahl.param, amdahl.max, plan, type, duration, payment)
  
  # Check for cached data set
  if (!is.null(cached.config.dataset) && identical(amdahl.param, cached.amdahl.param)) {
    aws.all.configs <- cached.config.dataset
  } else {
    aws.all.configs <- create.config.dataset(amdahl.param, amdahl.max, migration.costs)
    cached.amdahl.param <<- amdahl.param
  }
  
  # Helper variables
  saw.prev <- FALSE
  catch.iter <- 1
  
  # Calculating cost optimal instance configuration for each hourly workload demand
  for (j in 1:length(CPU.hours.per.hour.fluct)) {
    
    current.vCPU.demand.fluct <- CPU.hours.per.hour.fluct[[j]]
    cheapest.instance <- NULL
    current.instance <- NULL
    
    for (i in 1:nrow(aws.all.configs)) {
      
      instance.vCPU <- aws.all.configs[i, "vCPUs"]
      instance.name <- aws.all.configs[i, "API.Name"]
      instance.min.costs.per.API.fluct <- aws.all.configs[i, "Spot.costs.final"]
      number.of.instances.required.fluct <- ceiling(current.vCPU.demand.fluct / instance.vCPU)
      
      if (number.of.instances.required.fluct == aws.all.configs[i, "instances.required.wo.amdahl"]) {
        current.instance <- list(
          name = instance.name,
          price.fluct = instance.min.costs.per.API.fluct,
          vCPU.fluct = instance.vCPU,
          instances.required.amdahl = aws.all.configs[i, "instances.required.with.amdahl"]
        )
      }
      
      if (is.null(cheapest.instance) || current.instance$price.fluct < cheapest.instance$price.fluct) {
        cheapest.instance <- current.instance
      }
    }
    
    # Catch special case when there is no previous instance
    if (current.vCPU.demand.fluct == 0) {
      print(paste("[Spot Instance", j, ": No additional Spot instances required]"))
      
      df <- populate.dataframe(df, "Fluct", j, "NA", 0, 0, "Spot", 0, 0)
      if (!saw.prev) {
        catch.iter <- j + 1
      }
      next
    }
    
    # Print instance configuration for first workload above zero
    if (j == catch.iter) {
      print(print.fluct.workload.mig(j, cheapest.instance$name, cheapest.instance$price.fluct,
                                     cheapest.instance$instances.required.amdahl, "Spot", 0, 0))
      
      df <- populate.dataframe(df, "Fluct", j, cheapest.instance$name, cheapest.instance$price.fluct,
                               cheapest.instance$instances.required.amdahl, "Spot", 0, 0)
      
      prev.instance <- cheapest.instance
      saw.prev <- TRUE
      next
    }
    
    # Migration only possible if proposed instance is different than the previous one
    if (cheapest.instance$name != prev.instance$name) {
      mig.costs.dollar <- prev.instance$price.fluct * 
        (migration.costs / (prev.instance$vCPU.fluct * prev.instance$instances.required.amdahl))
      cheapest.instance$price.fluct.mig <- cheapest.instance$price.fluct + mig.costs.dollar
      mig.time <- migration.costs / (prev.instance$vCPU.fluct * prev.instance$instances.required.amdahl)
      
    } else {
      mig.costs.dollar <- 0
      mig.time <- 0
      cheapest.instance$price.fluct.mig <- cheapest.instance$price.fluct
    }
    
    # Calculate the costs of staying at the previous instance with the new workload
    new.instances.required.with.prev <- ceiling(current.vCPU.demand.fluct / prev.instance$vCPU.fluct)
    
    # Scale with amdahls law
    if (new.instances.required.with.prev < amdahl.max) {
      new.price.with.prev <- aws.all.configs$Spot.costs.final[aws.all.configs$API.Name == prev.instance$name &
                                                                aws.all.configs$instances.required.wo.amdahl == new.instances.required.with.prev]
      
      new.instances.required.with.prev.amdahl <-
        aws.all.configs$instances.required.with.amdahl[aws.all.configs$API.Name == prev.instance$name &
                                                         aws.all.configs$instances.required.wo.amdahl == new.instances.required.with.prev]
    } else {
      new.price.with.prev <- Inf
      new.instances.required.with.prev.amdahl <- Inf
    }
    
    # Migrate or stay
    if (cheapest.instance$price.fluct.mig < new.price.with.prev) {
      print(print.fluct.workload.mig(j, cheapest.instance$name, cheapest.instance$price.fluct.mig,
                                     cheapest.instance$instances.required.amdahl, "Spot", mig.costs.dollar, mig.time))
      
      df <- populate.dataframe(df, "Fluct", j, cheapest.instance$name, cheapest.instance$price.fluct.mig,
                               cheapest.instance$instances.required.amdahl, "Spot", mig.costs.dollar, mig.time)
      
      prev.instance <- cheapest.instance
    } else {
      print(print.fluct.workload.mig(j, prev.instance$name, new.price.with.prev,
                                     new.instances.required.with.prev.amdahl, "Spot", 0, 0))
      
      df <- populate.dataframe(df, "Fluct", j, prev.instance$name, new.price.with.prev,
                               new.instances.required.with.prev.amdahl, "Spot", 0, 0)
    }
  }
  return(df)
}

df.example <- find.cheapest.instance.final(20, list(16, 24, 500, 17, 18, 24, 3, 8, 2400, 8, 15, 26, 17, 18, 50, 3, 8, 14, 10, 21, 15, 18, 6, 2), 2) 
                                    
#df.example <- find.cheapest.instance.final(500, list(0), 24)


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
