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
  
  #assign(paste(c("config.df", amdahl.param), collapse = "."), aws.shared.all.configurations, envir = .GlobalEnv)
  cached.config.dataset.min <<- aws.shared.all.configurations
  
  return(aws.shared.all.configurations)
}


find.cheapest.instance.final.min <- function(CPU.hours.per.hour.base, CPU.hours.per.hour.fluct, migration.costs, 
                                         amdahl.param = 0.95, amdahl.max = 20,
                                         plan = "OD|RI|SP", type = "standard|convertible|Compute|EC2Instance", 
                                         duration = "1|3", payment = "All|Partial|No") {
  
  # get and print demand warning
  max.workload <- max(aws.all.prices.min$vCPUs) * (amdahl.max - 1)
  print(paste(c("Please note that the current implementation takes in workloads up to", max.workload, "CPU hours per hour."), collapse = " "))
  
  # calculate and print the base workload
  df <- get.base.workload(CPU.hours.per.hour.base, amdahl.param, amdahl.max, plan, type, duration, payment)
  
  # check if needed data set is already in cache
  if (!is.null(cached.config.dataset.min) && identical(amdahl.param, cached.amdahl.param.min)) {
    aws.shared.all.configurations <- cached.config.dataset.min
  } else {
    # Create working dataset
    aws.shared.all.configurations <- create.config.dataset.min(amdahl.param, amdahl.max, migration.costs)
    cached.amdahl.param.min <<- amdahl.param
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

df.example.min <- find.cheapest.instance.final.min(695, list(0, 0, 0, 17, 18, 24, 3, 8, 2400, 8, 15, 26, 17, 18, 50, 3, 8, 14, 10, 21, 15, 18, 6, 2), 5, 
                                           plan = "RI", type = "standard", duration = "1", payment = "No|Partial|All")

