# ----------------------------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------ MIN VERSION OF FINAL ALGORITHM --------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #

## Adapting only methods that are required 

# cache frequently created config data set to enhance performance
cached.config.dataset.min <- NULL

create.config.dataset.min <- function(amdahl.param, amdahl.max, migration.costs) {
  
  aws.shared.all.configurations <- aws.all.prices.min
  
  for (j in 1:nrow(aws.all.prices.min)) {
    # calculate new spot prices based on interruption freq -> spot.real = spot + spot * freq * (mig.costs / vCPUs)
    aws.shared.all.configurations[j, "Spot.costs.real"] <- aws.all.prices.min[j, "Spot.costs.hourly"] + 
      aws.all.prices.min[j, "Spot.costs.hourly"] * 
      aws.all.prices.min[j, "Interruption.Freq"] * (migration.costs / aws.all.prices.min[j, "vCPUs"])
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
  
  cached.config.dataset.min <<- aws.shared.all.configurations
  
  return(aws.shared.all.configurations)
}


find.cheapest.instance.final.min <- function(CPU.hours.per.hour.base, CPU.hours.per.hour.fluct, migration.costs,
                                             plan = "OD|RI|SP", type = "standard|convertible|Compute|EC2Instance",
                                             duration = "1|3", payment = "All|Partial|No", amdahl.param = 0.95) {
  
  # calculate the maximum value for amdahl
  amdahl.max <- ceiling(1 / (1 - amdahl.param))
  
  # Get and print demand warning
  max.workload <- max(aws.all.prices.min$vCPUs) * (amdahl.max - 1)
  message("Please note that the current implementation takes in workloads up to ", max.workload, " CPU hours per hour.")
  
  # Calculate the base workload
  df <- get.base.workload(CPU.hours.per.hour.base, amdahl.param, amdahl.max, plan, type, duration, payment)
  
  # Check for cached data set
  if (!is.null(cached.config.dataset.min) && identical(amdahl.param, cached.amdahl.param.min)) {
    aws.all.configs <- cached.config.dataset.min
  } else {
    aws.all.configs <- create.config.dataset.min(amdahl.param, amdahl.max, migration.costs)
    cached.amdahl.param.min <<- amdahl.param
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

df.example.min <- find.cheapest.instance.final.min(695, list(0, 0, 0, 17, 18, 24, 3, 8, 2300, 8, 15, 26, 17, 18, 50, 3, 8, 14, 10, 21, 15, 18, 6, 2), 5, 
                                           plan = "RI", type = "standard", duration = "1", payment = "No|Partial|All", amdahl.param = 0.92)

