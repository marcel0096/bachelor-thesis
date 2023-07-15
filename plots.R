# ----------------------------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------------------ PLOTS ---------------------------------------------------------------- # 
# ----------------------------------------------------------------------------------------------------------------------------------- #

library(gtable)
library(grid)
library(gridExtra)

## Generate plot for szenario output
CPU.hours.required <- 0

generate.plot.szenario1.2 <- function(df) {
  
  # get only base load
  df.base <<- df[1, ]
  df.base <<- df.base[rep(seq_len(nrow(df.base)), each = 24), ]
  df.base$Instance_Number <<- 1:24
  df.base$workload <<- as.numeric(CPU.hours.required)
  
  # get only fluct load
  df.fluct <<- df[df$Instance_Number != 0, ]
  df.fluct$Instance_config <<- paste(df.fluct$Num_Instances_Required, "x", df.fluct$Instance_Type)
  df.fluct$Instance_config <<- ifelse(df.fluct$Instance_config == "0 x NA", "No additional instance", df.fluct$Instance_config)
  df.fluct$workload <<- as.numeric(CPU.hours.fluctuating) + df.base$workload
  df.fluct$group_id <<- paste(df.fluct$Num_Instances_Required, df.fluct$Instance_Type, df.fluct$workload)
  df.fluct$group <<- cumsum(df.fluct$group_id != lag(df.fluct$group_id, default = ""))
  duplicates <- duplicated(df.fluct$group) | duplicated(df.fluct$group, fromLast = TRUE)
  df.fluct$angle <<- ifelse(duplicates, 0, 90)

  df.fluct.text <<- df.fluct
  df.fluct.text <<- subset(df.fluct.text, !duplicated(group))
  df.fluct.text$x.nudge <<- ifelse(df.fluct.text$angle == 0, 1, 0)
  df.fluct.text$y.nudge <<- ifelse(df.fluct.text$angle == 0, 30, 70)
  df.fluct.text$y.nudge <<- ifelse(df.fluct.text$Instance_config == "No additional instance", 100, df.fluct.text$y.nudge)

  ggplot(data = df.fluct) +
    geom_line(data = df.base, aes(x = Instance_Number, y = workload), color = "azure4", size = 1.2) +
    geom_text(data = subset(df.base, !duplicated(Instance_Type)),
              aes(x = Instance_Number, y = workload, label = Instance_Type),
              size = 2,
              nudge_x = 11,
              vjust = 2,
              show.legend = FALSE,
              color = "azure4") +
    geom_line(aes(x = Instance_Number, y = workload)) +
    geom_text(data = df.fluct.text,
              aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type), 
              size = 2.2, 
              nudge_y = df.fluct.text$y.nudge, 
              nudge_x = df.fluct.text$x.nudge,
              angle = df.fluct.text$angle, 
              show.legend = FALSE) +
    geom_segment(aes(x = Instance_Number - 0.5, y = workload, xend = Instance_Number + 0.5, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    scale_color_manual(values = c("deeppink3", "azure4", "darkgoldenrod3", "deepskyblue3", "darkorchid3")) +
    scale_x_continuous(breaks = seq(0, 25, 2)) +
    ylim(0, 250) +
    xlab("Hour") + 
    ylab("Workload in CPU hours per hour") +
    theme_bw()
}
generate.plot.szenario1.2(snow.szenario.2.2.avg)

result.M1 <- find.cheapest.instance.final(32, list(0), 0)
## Plot for explaining model M1 in paper
generate.plot.M1 <- function() {
  
  workload.M1.M2 <- snowset.middle.cust.hourly
  workload.M1.M2$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                 6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  #workload.M1.M2$hour_group <- hour(workload.M1.M2$hour_group)
  #workload.M1.M2$hour_group <- as.character(strftime(workload.M1.M2$hour_group, format = "%H:%M"))
  
  #result.M1 <- find.cheapest.instance.final(32, list(0), 0)
  result.M1.base <- result.M1[1, ]
  result.M1.base <- result.M1.base[rep(seq_len(nrow(result.M1.base)), each = 24), ]
  result.M1.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M1.base$workload <- 32
  result.M1.base$Instance_config <- paste(result.M1.base$Num_Instances_Required, "x ", result.M1.base$Instance_Type, 
                                          " (", round(result.M1.base$Total_Costs, 4), "$/h)", sep = "")
  
  ggplot() +
    geom_line(data = workload.M1.M2, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black") + 
    geom_line(data = result.M1.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2") +
    geom_text(data = subset(result.M1.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 2,
              nudge_x = 14400,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 35) + 
    #xlab("Time") +
    #ylab("CPU hours") +
    ggtitle("Model M1") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
M1 <- generate.plot.M1()


result.M2 <- find.cheapest.instance.final(8, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 
                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)
## Plot for explaining model M2 in paper
generate.plot.M2 <- function() {
  
  # result.M2 <- find.cheapest.instance.final(8, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 
  #                                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)
  
  workload.M1.M2 <- snowset.middle.cust.hourly
  workload.M1.M2$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                 6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  
  
  result.M2.base <- result.M2[1, ]
  result.M2.base <- result.M2.base[rep(seq_len(nrow(result.M2.base)), each = 24), ]
  result.M2.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.base$workload <- 8
  result.M2.base$Instance_config <- paste(result.M2.base$Num_Instances_Required, "x ", result.M2.base$Instance_Type, 
                                          " (", round(result.M2.base$Total_Costs, 4), "$/h)", sep = "")
  

  result.M2.fluct <<- result.M2[result.M2$Instance_Number != 0, ]
  result.M2.fluct$Instance_Number <<- snowset.middle.cust.hourly$hour_group
  result.M2.fluct$Instance_config <<- paste(result.M2.fluct$Num_Instances_Required, "x ", result.M2.fluct$Instance_Type, 
                                            "\n(", round(result.M2.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.M2.fluct$Instance_config <<- ifelse(result.M2.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M2.fluct$Instance_config)
  result.M2.fluct$workload <<- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", 16 + result.M2.base$workload, 24 + result.M2.base$workload)
  result.M2.fluct$group_id <<- paste(result.M2.fluct$Num_Instances_Required, result.M2.fluct$Instance_Type, result.M2.fluct$workload)
  result.M2.fluct$group <<- cumsum(result.M2.fluct$group_id != lag(result.M2.fluct$group_id, default = ""))
  duplicates <- duplicated(result.M2.fluct$group) | duplicated(result.M2.fluct$group, fromLast = TRUE)
  result.M2.fluct$angle <<- ifelse(duplicates, 0, 90)
  result.M2.fluct <<- result.M2.fluct[result.M2.fluct$Num_Instances_Required > 0, ]
  result.M2.fluct$nudge_x <<- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", -7200, 0)
  
  ggplot() +
    geom_line(data = workload.M1.M2, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black") + 
    geom_line(data = result.M2.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2") +
    geom_text(data = subset(result.M2.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 2,
              nudge_x = 14400,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.M2.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 color = c("#E64B35B2", "#7E6148B2"),
                 #size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = result.M2.fluct, aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 2,
              nudge_x = result.M2.fluct$nudge_x,
              vjust = -0.5,
              show.legend = FALSE,
              color = c("#E64B35B2", "#7E6148B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 35) + 
    #xlab("Time") +
    #ylab("CPU hours") +
    ggtitle("Model M2") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
M2 <- generate.plot.M2()


result.M3 <- find.cheapest.instance.final(8, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 
                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
## Plot for explaining model M3 in paper
generate.plot.M3 <- function() {
  
  # result.M3 <- find.cheapest.instance.final(8, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 
  #                                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
  
  workload.M3 <- snowset.middle.cust.hourly
  workload.M3$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                 6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  

  result.M3.base <- result.M3[1, ]
  result.M3.base <- result.M3.base[rep(seq_len(nrow(result.M3.base)), each = 24), ]
  result.M3.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M3.base$workload <- 8
  result.M3.base$Instance_config <- paste(result.M3.base$Num_Instances_Required, "x ", result.M3.base$Instance_Type, 
                                          " (", round(result.M3.base$Total_Costs, 4), "$/h)", sep = "")
  

  result.M3.fluct <- result.M3[result.M3$Instance_Number != 0, ]
  result.M3.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M3.fluct$Instance_config <- paste(result.M3.fluct$Num_Instances_Required, "x ", result.M3.fluct$Instance_Type, 
                                            "\n(", round(result.M3.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.M3.fluct$Instance_config <- ifelse(result.M3.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M3.fluct$Instance_config)
  result.M3.fluct$workload <- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", 16 + result.M3.base$workload, 24 + result.M3.base$workload)    
  result.M3.fluct$group_id <- paste(result.M3.fluct$Num_Instances_Required, result.M3.fluct$Instance_Type, result.M3.fluct$workload)
  result.M3.fluct$group <- cumsum(result.M3.fluct$group_id != lag(result.M3.fluct$group_id, default = ""))
  duplicates <- duplicated(result.M3.fluct$group) | duplicated(result.M3.fluct$group, fromLast = TRUE)
  result.M3.fluct$angle <- ifelse(duplicates, 0, 90)
  result.M3.fluct$nudge_x <- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", -7200, 0)
  result.M3.fluct <- result.M3.fluct[result.M3.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = result.M3.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2") +
    geom_text(data = subset(result.M3.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 2,
              nudge_x = 14400,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.M3.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 color = c("#E64B35B2", "#7E6148B2"),
                 #size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = result.M3.fluct, aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 2,
              nudge_x = result.M3.fluct$nudge_x,
              vjust = -0.5,
              show.legend = FALSE,
              color = c("#E64B35B2", "#7E6148B2")) +
    geom_line(data = workload.M3, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black") + 
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 35) + 
    #xlab("Time") +
    #ylab("CPU hours") +
    ggtitle("Model M3") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
M3 <- generate.plot.M3()


arrange.M1.M2.M3 <- function() {
  gridExtra::grid.arrange(arrangeGrob(M1, M2, M3, ncol = 3, left = textGrob("CPU hours", rot = 90, vjust = 1)))
}
arrange.M1.M2.M3()


## Costs in $ per CPU/h for every option
generate.plot.costs <- function() {
  
  # M1
  result.M1.base <<- result.M1[1, ]
  result.M1.base <<- result.M1.base[rep(seq_len(nrow(result.M1.base)), each = 24), ]
  result.M1.base$Instance_Number <<- snowset.middle.cust.hourly$hour_group
  result.M1.base$workload <<- 32
  result.M1.base$Instance_config <<- paste(result.M1.base$Num_Instances_Required, "x ", result.M1.base$Instance_Type, 
                                          " (", round(result.M1.base$Total_Costs, 4), "$/h)", sep = "")
  result.M1.base$dollar_per_CPUh <<- result.M1.base$Total_Costs / result.M1.base$workload
  
  # M2
  result.M2.base <- result.M2[1, ]
  result.M2.base <- result.M2.base[rep(seq_len(nrow(result.M2.base)), each = 24), ]
  result.M2.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.base$workload <- 8
  result.M2.base$Instance_config <- paste(result.M2.base$Num_Instances_Required, "x ", result.M2.base$Instance_Type, 
                                          " (", round(result.M2.base$Total_Costs, 4), "$/h)", sep = "")
  
  
  result.M2.fluct <<- result.M2[result.M2$Instance_Number != 0, ]
  result.M2.fluct$Instance_Number <<- snowset.middle.cust.hourly$hour_group
  result.M2.fluct$Instance_config <<- paste(result.M2.fluct$Num_Instances_Required, "x ", result.M2.fluct$Instance_Type, 
                                            " (", round(result.M2.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.M2.fluct$Instance_config <<- ifelse(result.M2.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M2.fluct$Instance_config)
  result.M2.fluct$workload <<- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", 
                                      16 + result.M2.base$workload, ifelse(result.M2.fluct$Instance_Type == "r5.8xlarge", 24 + result.M2.base$workload, 8))
  result.M2.fluct$Total_Costs <<- result.M2.fluct$Total_Costs + result.M2.base$Total_Costs
  result.M2.fluct$dollar_per_CPUh <<- result.M2.fluct$Total_Costs / result.M2.fluct$workload
  
  # M3
  result.M3.base <- result.M3[1, ]
  result.M3.base <- result.M3.base[rep(seq_len(nrow(result.M3.base)), each = 24), ]
  result.M3.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M3.base$workload <- 8
  result.M3.base$Instance_config <- paste(result.M3.base$Num_Instances_Required, "x ", result.M3.base$Instance_Type, 
                                          " (", round(result.M3.base$Total_Costs, 4), "$/h)", sep = "")
  
  
  result.M3.fluct <<- result.M3[result.M3$Instance_Number != 0, ]
  result.M3.fluct$Instance_Number <<- snowset.middle.cust.hourly$hour_group
  result.M3.fluct$Instance_config <<- paste(result.M3.fluct$Num_Instances_Required, "x ", result.M3.fluct$Instance_Type, 
                                            " (", round(result.M3.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.M3.fluct$Instance_config <<- ifelse(result.M3.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M3.fluct$Instance_config)
  result.M3.fluct$workload <<- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", 
                                      16 + result.M3.base$workload, ifelse(result.M3.fluct$Instance_Type == "r5.8xlarge", 24 + result.M3.base$workload, 8))
  result.M3.fluct$Total_Costs <<- result.M3.fluct$Total_Costs + result.M3.base$Total_Costs
  result.M3.fluct$dollar_per_CPUh <<- result.M3.fluct$Total_Costs / result.M3.fluct$workload
  
  
  ggplot() +
    geom_line(data = result.M1.base, aes(x = Instance_Number, y = dollar_per_CPUh), color = "#4DBBD5B2") +
    geom_line(data = result.M2.fluct, aes(x = Instance_Number, y = dollar_per_CPUh), color = "#DC0000B2") +
    geom_line(data = result.M3.fluct, aes(x = Instance_Number, y = dollar_per_CPUh), color = "grey") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.025) + 
    ylab("$ per CPU hour") +
    ggtitle("Test") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))
    
}
generate.plot.costs()


# Insert coniguration and not only instance -> copy from above
# Insert total price

# gemeinsamer Graph mit Preisen $ pro CPU/h -> alle optionen




## Plots for snowsets
ggplot(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime)) +
  geom_line() + 
  ylim(0, 250)

ggplot(data = snowset.middle.cust.hourly, aes(x = hour_group, y = totalCpuTime)) +
  geom_line() + 
  ylim(0, 100)

ggplot(data = snowset.small.cust.hourly, aes(x = hour_group, y = totalCpuTime)) +
  geom_line() + 
  ylim(0, 5)



## NOTES

# Plot mit $/CPUh -> bei manchen spot dann höher als z.B. bei base, da amdahl skaliert s. Foto


# MAKE PLOT THAT SHOWS EXAMPLE SNOWFLAKE WORKLOAD AND DIFFERENT OPTIONS TO COPE WITH IT
# -> ein mal on prem bzw. das gleiche mit nur base load -> alles abdecken -> resources wasted
# -> meine lösung, perfect coverage -> billiger, weniger verschwendet

# noch ein plot mit 1.1 szenario -> gestrichelte Linie in plot
# neuer plot mit y-axis dollar pro cpu stunde 




# only connect point with same color
# ggplot() +
#   geom_point(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
#   geom_line(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type, group = group), alpha = 0.5) +
#   geom_line(df.base, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
#   scale_y_continuous(name = "Prices - base", sec.axis = sec_axis(~. - df.base[1, "Total_Costs"], name = "Prices - fluct")) +
#   labs(x = "Hours", y = "Prices")




# ggplot(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type, shape = Workload_Type)) +
# #scale_fill_discrete(name = "Fluct") +
# geom_point() +
# geom_segment(df.fluct, mapping = aes(x = lag(Instance_Number), y = lag(Total_Costs), 
#                                        xend = Instance_Number, yend = Total_Costs), alpha = 0.5) + 
# geom_point(df.base, mapping = aes(x = Instance_Number, y = Total_Costs)) +
# geom_line(df.base, mapping = aes(x = Instance_Number, y = Total_Costs)) +
# #scale_y_continuous(name = "Prices - base", sec.axis = sec_axis(trans = ~. - df.base[1, "Total_Costs"], name = "Prices - fluct")) +
# scale_x_continuous(breaks = df.fluct$Instance_Number) +
# labs(x = "Time in hours", y = "Costs in $", color = "Instances Types", shape = "Workload Type") +
# theme_bw()

