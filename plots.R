# Generate plot for algorithm output

generate.plot <- function(df) {
  
  # get only base load
  df.base <- df[1, ]
  df.base <- df.base[rep(seq_len(nrow(df.base)), each = 24), ]
  df.base$Instance_Number <- 1:24
  
  # get only fluct load
  df.fluct <- df[df$Instance_Number != 0, ]
  df.fluct$group <- cumsum(df.fluct$Instance_Type != lag(df.fluct$Instance_Type, default = ""))
  df.fluct$Total_Costs <- df.fluct$Total_Costs + df.base$Total_Costs
  
  plot <- 
    ggplot(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type, shape = Workload_Type)) +
    #scale_fill_discrete(name = "Fluct") +
    geom_point() +
    geom_segment(df.fluct, mapping = aes(x = lag(Instance_Number), y = lag(Total_Costs), 
                                         xend = Instance_Number, yend = Total_Costs), alpha = 0.5) + 
    geom_point(df.base, mapping = aes(x = Instance_Number, y = Total_Costs)) +
    geom_line(df.base, mapping = aes(x = Instance_Number, y = Total_Costs)) +
    #scale_y_continuous(name = "Prices - base", sec.axis = sec_axis(trans = ~. - df.base[1, "Total_Costs"], name = "Prices - fluct")) +
    scale_x_continuous(breaks = df.fluct$Instance_Number) +
    labs(x = "Time in hours", y = "Costs in $", color = "Instances Types", shape = "Workload Type") +
    theme_bw()
  
  return(plot)
}

generate.plot(szenario1.2.avg)



# only connect point with same color
# ggplot() +
#   geom_point(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
#   geom_line(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type, group = group), alpha = 0.5) +
#   geom_line(df.base, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
#   scale_y_continuous(name = "Prices - base", sec.axis = sec_axis(~. - df.base[1, "Total_Costs"], name = "Prices - fluct")) +
#   labs(x = "Hours", y = "Prices")

