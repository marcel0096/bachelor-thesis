# !!!ADAPT DATA 

# ---------------------- ALGORITHM FOR ON DEMAND ---------------------- # 

# Implementing algorithm to find cheapest On demand instance for given CPU/h usage (e.g. 240 CPU hours are required per day) -> using only vCPUs as metric

# base workload ist ein vCPU/h wert, schwankender workload als array von 24 werten

find_cheapest_instance_on_demand <- function(CPU_hours_per_day) {
  
  CPU_hours_per_hour <- CPU_hours_per_day / 24
  current_instance_price <- Inf
  cheapest_price <- Inf
  cheapest_instance <- ""
  result <- list()
  
  # iterate through all rows in data frame to find cheapest instances
  for (i in 1:nrow(aws_data_trimmed_all_prices_2)) {
    
    instance_vCPUs <- aws_data_trimmed_all_prices_2[i, "vCPUs"]
    instance_name <- aws_data_trimmed_all_prices_2[i, "API_Name"]
    instance_on_demand_price <- aws_data_trimmed_all_prices_2[i, "On_demand_Costs_Dollar_per_Hour"]
    
    # does one instance of the instance type alone have enough vCPUs to serve the need?
    if (instance_vCPUs < CPU_hours_per_hour) {
      # if not, calculate how many instances would be required and add the prices
      number_of_instances_required <- ceiling(CPU_hours_per_hour / instance_vCPUs)
      current_instance_price <- instance_on_demand_price * number_of_instances_required
      
      # is it also the cheapest so far?
      if (current_instance_price <= cheapest_price) {
        result <- append(result, paste(instance_name, ": ", current_instance_price))
        cheapest_price <- current_instance_price
      }
      
    } else {
      # if number of vCPUs of an instance are equal or more that the ones required per hour, one instance is enough to serve usage
      current_instance_price <- instance_on_demand_price 
      
      # is it also the cheapest so far?
      if (current_instance_price <= cheapest_price) {
        result <- append(result, paste(instance_name, ": ", current_instance_price))
        cheapest_price <- current_instance_price
      }
    }
  }
  return(result)
}

find_cheapest_instance_on_demand(24000)

# Next:
#   - Savings Plans mit einbauen -> Annahme ist, dass der workload ewig läuft -> dementsprechend sollte 3 Jahresplan am besten sein (was ist mit RIs? Daten dafür?)
#   - Daten sind für RIs -> gleich wie "Instance Savings Plans" -> für compute savings plans brauche ich noch die Daten von AWS -> dann kann ich für alles berechnen außer Spot
#   - Ausgabe verbesseren und verschönern: Wie heißt die Instanz? Wie viele brauche ich davon? Wie teuer wird das? In welchem Plan soll man das kaufen?
#   - Schwankenden Workload einbauen durch das mitgeben eines arrays mit 24 werten
#   - Eingabe direkt mit CPU hours pro Stunde und nicht pro Tag
#   - Bei kleinen Instanzen nicht linear skalieren sondern immer mit einem Faktor sodass sich Leistung z.B. nicht verdoppelt, sondern nur ver-1,8-facht




