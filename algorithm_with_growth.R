# ----------------------------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------ GROWTH RATE FOR FINAL ALGORITHM -------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #

## Recalculate every time period where the growth rate applies, e.g. yearly

# checks whether growth is so little that it can be covered by the plan initially bought
# or whether a new plan needs to be acquired (as this is cheaper than buying a larger instance directly)

algorithm.with.growth <- function(growth.rate, workload.base, workload.fluct) {
  
  new.workload.base <- (workload.base * (1 + growth.rate))
    
  new.workload.fluct <- lapply(workload.fluct, function(x) x * (1 + growth.rate))
  
  result.wo.growth <- find.cheapest.instance.final(workload.base, workload.fluct, 2)
  result.with.growth <- find.cheapest.instance.final(new.workload.base, new.workload.fluct, 2)
  
  if (result.with.growth[1, 'Total_Costs'] == result.wo.growth[1, 'Total_Costs'] && 
      result.with.growth[1, 'Num_Instances_Required'] == result.wo.growth[1, 'Num_Instances_Required']) {
    
    message("No new price plan required")
    return(result.with.growth)
    
  } else {
    new.workload.base <- (workload.base * (1 + growth.rate)) - workload.base
    result.with.growth <- find.cheapest.instance.final(new.workload.base, new.workload.fluct, 2)
    message("New price plan required")
    return(result.with.growth)
  }
}

df.example.growth <- algorithm.with.growth(0.20, 100, list(16, 24, 0, 17, 18, 24, 3, 8, 2400, 8, 15, 26, 17, 18, 50, 3, 8, 14, 10, 21, 15, 18, 6, 2))


## RESULTS

## Bei 20% Wachstum pro Jahr -- 100 CPUh/h
# Wenn direkt teurer gekauft wird: c6a.32xlarge für 1.87609$
# Wenn ursprünglicher Preis + dann extra plan für Wachstum: 11x a1.metal für insg. 1.683$ + 7x a1.xlarge für insg. 0.268$ = 1.951$

## Bei 15% Wachstum pro Jahr -- 100 CPUh/h
# Wenn direkt teurer gekauft wird: c6a.32xlarge für 1.87609$
# Wenn ursprünglicher Preis + dann extra plan für Wachstum: 11x a1.metal für insg. 1.683$ + 1x a1.metal für insg. 0.153$ = 1.836$

## Bei 15% Wachstum pro Jahr -- 10 CPUh/h
# Wenn direkt teurer gekauft wird: a1.metal für 0.153$
# Wenn ursprünglicher Preis + dann extra plan für Wachstum: 7x a1.large für insg. 0.134$ + 1x a1.large für insg. 0.019$ = 0.153$

## Bei 10% Wachstum pro Jahr  -- 100 CPUh/h
# 11x a1.metal für insg. 1.683$

## Bei 5% Wachstum pro Jahr -- 100 CPUh/h
# 11x a1.metal für insg. 1.683$

# -> Je größer das Wachstum, desto teurer die zusätzliche Instanzkonfiguration die man kaufen müsste -> Gleich eine größere lohnt mehr

 


