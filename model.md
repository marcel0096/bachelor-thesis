# Model 

## Inputs

Workload characterized by:

- required CPU hours per hour: base (1 val), fluctuating( 24 values = hourly )
- migration cost (CPU hours)
- scalability (amdahl factor, e.g., 0.95)


## Assumptions

### Workload
- constant base workload
- fluctuating hourly workload
- base workload independent of fluctuating workload (amdahl applies separately)
  - TODO maybe revisit based on scenarios
- runtime infinite (3yr vs 1yr savings plan)
- migration CPU hour cost is constant (doesn't matter what previous workload was)
  - alternative would be percentage of workload

### Scaling
- perfect scaling per instance, imperfect scaling in cluster (amdahl)
- migration happens on previously running instance (runs for longer)

### AWS mechanics
- interruption frequencies can be scaled-down from monthly to hourly
- took average value from aws-provided interruption frequency values
- not considering probability of not getting an instance


## Input Data

- spot prices + interruption frequencies
  - TODO collect more data, average over month 
- on-demand prices
- reserved instance prices
- savings plans prices
- TODO join over datasets loses a lot of instances
  -> probably b/c RI/SP datasets are old and newer instances  aren't in there

