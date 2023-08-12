source("./model.R")
source("./model.min.R")
source("./model.growth.R")
source("./snowset.analysis.R")
source("./szenarios.snow.R")

## ------------------------------------------------------------------------------------------------------------ ##
                                          # Approach Plot 1: Base only
## ------------------------------------------------------------------------------------------------------------ ##

result.M1 <- find.cheapest.instance.final(32, list(0), 0)
## Plot for explaining model M1 in paper
generate.plot.M1 <- function() {
  
  workload.M1.M2 <- snowset.middle.cust.hourly
  workload.M1.M2$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                 6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))

  result.M1.base <- result.M1[1, ]
  result.M1.base <- result.M1.base[rep(seq_len(nrow(result.M1.base)), each = 24), ]
  result.M1.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M1.base$workload <- 32
  result.M1.base$Instance_config <- paste(result.M1.base$Num_Instances_Required, "x ", result.M1.base$Instance_Type, 
                                          " (", round(result.M1.base$Total_Costs, 4), "$/h)", sep = "")
  
  ggplot() +
    geom_line(data = workload.M1.M2, aes(x = hour_group, y = totalCpuTime), color = "black", linetype = "dotted", size = 1.2) + 
    geom_line(data = result.M1.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.M1.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 9000,
              vjust = -3,
              color = "#3C5488B2") +
    scale_color_manual(values = c("#3C5488B2", "black"), name = "Workload Type") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 40) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 17),
          legend.key.size = unit(1.5, "cm")) +
    annotate("text", x = as.POSIXct("2018-02-22 04:37:00"), y = 34, label = "EC2 Instance SP (3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
M1 <- generate.plot.M1()
ggsave("M1.pdf", plot = M1, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                               # Approach Plot 2: Cost-optimal without migration costs
## ------------------------------------------------------------------------------------------------------------ ##

result.M2 <- find.cheapest.instance.final(8, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 
                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)
## Plot for explaining model M2 in paper
generate.plot.M2 <- function() {
  
  workload.M1.M2 <- snowset.middle.cust.hourly
  workload.M1.M2$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                 6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  
  result.M2.base <- result.M2[1, ]
  result.M2.base <- result.M2.base[rep(seq_len(nrow(result.M2.base)), each = 24), ]
  result.M2.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.base$workload <- 8
  result.M2.base$Instance_config <- paste(result.M2.base$Num_Instances_Required, "x ", result.M2.base$Instance_Type, 
                                          " (", round(result.M2.base$Total_Costs, 4), "$/h)", sep = "")
  
  result.M2.fluct <- result.M2[result.M2$Instance_Number != 0, ]
  result.M2.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.fluct$Instance_config <- paste(result.M2.fluct$Num_Instances_Required, "x ", result.M2.fluct$Instance_Type, 
                                            " (", round(result.M2.fluct$Total_Costs, 4), "$/h)\nSpot Instance", sep = "")
  result.M2.fluct$Instance_config <- ifelse(result.M2.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M2.fluct$Instance_config)
  result.M2.fluct$workload <- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", 16 + result.M2.base$workload, 24 + result.M2.base$workload)
  result.M2.fluct$group_id <- paste(result.M2.fluct$Num_Instances_Required, result.M2.fluct$Instance_Type, result.M2.fluct$workload)
  result.M2.fluct$group <- cumsum(result.M2.fluct$group_id != lag(result.M2.fluct$group_id, default = ""))
  duplicates <- duplicated(result.M2.fluct$group) | duplicated(result.M2.fluct$group, fromLast = TRUE)
  result.M2.fluct$angle <- ifelse(duplicates, 0, 90)
  result.M2.fluct <- result.M2.fluct[result.M2.fluct$Num_Instances_Required > 0, ]
  result.M2.fluct$nudge_x <- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", -8500, 0)
  result.M2.fluct$vjust <- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", 0.3, -0.3)
  
  ggplot() +
    geom_line(data = workload.M1.M2, aes(x = hour_group, y = totalCpuTime,), color = "black", linetype = "dotted", size = 1.2) + 
    geom_line(data = result.M2.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.M2.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 9000,
              vjust = -3,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.M2.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload),
                 color = c("#E64B35B2", "#7E6148B2"),
                 size = 1.2,
                 alpha = 0.8, 
                 ) +
    geom_text(data = result.M2.fluct, aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = result.M2.fluct$nudge_x,
              vjust = result.M2.fluct$vjust,
              show.legend = FALSE,
              color = c("#E64B35B2", "#7E6148B2")) +
    scale_color_manual(values = c("#3C5488B2", "#E64B35B2", "#7E6148B2", "black"), name = "Workload Type") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 40) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 17),
          legend.key.size = unit(1.5, "cm")) +
    annotate("text", x = as.POSIXct("2018-02-22 04:37:00"), y = 10, label = "EC2 Instance SP (3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
M2 <- generate.plot.M2()
ggsave("M2.pdf", plot = M2, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                # Approach Plot 3: Cost-optimized with migration costs
## ------------------------------------------------------------------------------------------------------------ ##

result.M3 <- find.cheapest.instance.final(8, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 
                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
## Plot for explaining model M3 in paper
generate.plot.M3 <- function() {
  
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
                                            " (", round(result.M3.fluct$Total_Costs, 4), "$/h)\nSpot Instance", sep = "")
  result.M3.fluct$Instance_config <- ifelse(result.M3.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M3.fluct$Instance_config)
  result.M3.fluct$workload <- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", 16 + result.M3.base$workload, 24 + result.M3.base$workload)    
  result.M3.fluct$group_id <- paste(result.M3.fluct$Num_Instances_Required, result.M3.fluct$Instance_Type, result.M3.fluct$workload)
  result.M3.fluct$group <- cumsum(result.M3.fluct$group_id != lag(result.M3.fluct$group_id, default = ""))
  duplicates <- duplicated(result.M3.fluct$group) | duplicated(result.M3.fluct$group, fromLast = TRUE)
  result.M3.fluct$angle <- ifelse(duplicates, 0, 90)
  result.M3.fluct$nudge_x <- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", -8500, 0)
  result.M3.fluct$vjust <- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", 0.3, -0.3)
  result.M3.fluct <- result.M3.fluct[result.M3.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = result.M3.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.M3.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 9000,
              vjust = -3,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.M3.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 color = c("#E64B35B2", "#7E6148B2"),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = result.M3.fluct, aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = result.M3.fluct$nudge_x,
              vjust = result.M3.fluct$vjust,
              show.legend = FALSE,
              color = c("#E64B35B2", "#7E6148B2")) +
    geom_line(data = workload.M3, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 40) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 04:37:00"), y = 10, label = "EC2 Instance SP (3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
M3 <- generate.plot.M3()
ggsave("M3.pdf", plot = M3, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                              # Dollar per CPUh plot for M1-M3
## ------------------------------------------------------------------------------------------------------------ ##

generate.plot.costs <- function() {
  # M1
  result.M1.base <- result.M1[1, ]
  result.M1.base <- result.M1.base[rep(seq_len(nrow(result.M1.base)), each = 24), ]
  result.M1.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M1.base$workload <<- 32
  result.M1.base$Instance_config <- paste(result.M1.base$Num_Instances_Required, "x ", result.M1.base$Instance_Type, 
                                          " (", round(result.M1.base$Total_Costs, 4), "$/h)", sep = "")
  result.M1.base$dollar_per_CPUh <- result.M1.base$Total_Costs / result.M1.base$workload
  
  # M2
  result.M2.base <- result.M2[1, ]
  result.M2.base <- result.M2.base[rep(seq_len(nrow(result.M2.base)), each = 24), ]
  result.M2.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.base$workload <- 8
  result.M2.base$Instance_config <- paste(result.M2.base$Num_Instances_Required, "x ", result.M2.base$Instance_Type, 
                                          " (", round(result.M2.base$Total_Costs, 4), "$/h)", sep = "")
  
  result.M2.fluct <- result.M2[result.M2$Instance_Number != 0, ]
  result.M2.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.fluct$Instance_config <- paste(result.M2.fluct$Num_Instances_Required, "x ", result.M2.fluct$Instance_Type, 
                                            " (", round(result.M2.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.M2.fluct$Instance_config <- ifelse(result.M2.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M2.fluct$Instance_config)
  result.M2.fluct$workload <- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", 
                                      16 + result.M2.base$workload, ifelse(result.M2.fluct$Instance_Type == "r5.8xlarge", 24 + result.M2.base$workload, 8))
  result.M2.fluct$Total_Costs <- result.M2.fluct$Total_Costs + result.M2.base$Total_Costs
  result.M2.fluct$dollar_per_CPUh <- result.M2.fluct$Total_Costs / result.M2.fluct$workload
  
  # M3
  result.M3.base <- result.M3[1, ]
  result.M3.base <- result.M3.base[rep(seq_len(nrow(result.M3.base)), each = 24), ]
  result.M3.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M3.base$workload <- 8
  result.M3.base$Instance_config <- paste(result.M3.base$Num_Instances_Required, "x ", result.M3.base$Instance_Type, 
                                          " (", round(result.M3.base$Total_Costs, 4), "$/h)", sep = "")
  
  result.M3.fluct <- result.M3[result.M3$Instance_Number != 0, ]
  result.M3.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M3.fluct$Instance_config <- paste(result.M3.fluct$Num_Instances_Required, "x ", result.M3.fluct$Instance_Type, 
                                            " (", round(result.M3.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.M3.fluct$Instance_config <- ifelse(result.M3.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M3.fluct$Instance_config)
  result.M3.fluct$workload <- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", 
                                      16 + result.M3.base$workload, ifelse(result.M3.fluct$Instance_Type == "r5.8xlarge", 24 + result.M3.base$workload, 8))
  result.M3.fluct$Total_Costs <- result.M3.fluct$Total_Costs + result.M3.base$Total_Costs
  result.M3.fluct$dollar_per_CPUh <- result.M3.fluct$Total_Costs / result.M3.fluct$workload
  
  
  ggplot() +
    geom_line(data = result.M1.base, aes(x = Instance_Number, y = dollar_per_CPUh), color = "#4DBBD5B2") +
    geom_line(data = result.M2.fluct, aes(x = Instance_Number, y = dollar_per_CPUh), linetype = "dashed", color = "#DC0000B2") +
    geom_line(data = result.M3.fluct, aes(x = Instance_Number, y = dollar_per_CPUh), color = "grey") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.025) + 
    ylab("$ per CPU hour") +
    ggtitle("Hourly Costs per CPU hour M1-M3") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))
    
}
generate.plot.costs()


## ------------------------------------------------------------------------------------------------------------ ##
                                                    # Plots VH-1
## ------------------------------------------------------------------------------------------------------------ ##

## workload
generate.plot.S1.workload <- function() {
  S1.workload <<- ggplot() +
    geom_line(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 235, label = "Total workload demand: 2944 CPUh", color = "black", size = 5) 
}
generate.plot.S1.workload()
ggsave("VH-1.pdf", plot = S1.workload, width = 14.7551263472)


## Base only
generate.plot.S1.option1 <- function() {
  
  snow.szenario.2.1.base <- snow.szenario.2.1[1, ]
  snow.szenario.2.1.base <- snow.szenario.2.1.base[rep(seq_len(nrow(snow.szenario.2.1.base)), each = 24), ]
  snow.szenario.2.1.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.1.base$workload <- max(snowset.large.cust.hourly$totalCpuTime)
  snow.szenario.2.1.base$Instance_config <- paste(snow.szenario.2.1.base$Num_Instances_Required, "x ", snow.szenario.2.1.base$Instance_Type, 
                                          " (", round(snow.szenario.2.1.base$Total_Costs, 4), "$/h)", sep = "")
  
  ggplot() +
    geom_line(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black") + 
    geom_line(data = snow.szenario.2.1.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2") +
    geom_text(data = subset(snow.szenario.2.1.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 2,
              nudge_x = 14400,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    xlab("Time of day") +
    ylab("CPU hours") +
    ggtitle("Scenario 1 - Base Workload Only") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5))
}
generate.plot.S1.option1()


## Fluct only
generate.plot.S1.option2 <- function() {
  
  snow.szenario.2.2.avg.base <- snow.szenario.2.2.avg[1, ]
  snow.szenario.2.2.avg.base <- snow.szenario.2.2.avg.base[rep(seq_len(nrow(snow.szenario.2.2.avg.base)), each = 24), ]
  snow.szenario.2.2.avg.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.2.2.avg.base$workload <- 0
  snow.szenario.2.2.avg.base$Instance_Type <- "No base workload required"
  snow.szenario.2.2.avg.base$Instance_config <- paste(snow.szenario.2.2.avg.base$Num_Instances_Required, "x ", snow.szenario.2.2.avg.base$Instance_Type, 
                                                      " (", round(snow.szenario.2.2.avg.base$Total_Costs, 4), "$/h)", sep = "")

  
  snow.szenario.2.2.avg.fluct <- snow.szenario.2.2.avg[snow.szenario.2.2.avg$Instance_Number != 0, ]
  snow.szenario.2.2.avg.fluct$Instance_config <- paste(snow.szenario.2.2.avg.fluct$Num_Instances_Required, "x ", snow.szenario.2.2.avg.fluct$Instance_Type, 
                                                    "\n(", round(snow.szenario.2.2.avg.fluct$Total_Costs, 4), "$/h)", sep = "")
  snow.szenario.2.2.avg.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.2.avg.fluct$Instance_config <- ifelse(snow.szenario.2.2.avg.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", snow.szenario.2.2.avg.fluct$Instance_config)
  snow.szenario.2.2.avg.fluct$workload <- as.numeric(CPU.hours.fluctuating) + snow.szenario.2.2.avg.base$workload
  snow.szenario.2.2.avg.fluct$group_id <- paste(snow.szenario.2.2.avg.fluct$Num_Instances_Required, snow.szenario.2.2.avg.fluct$Instance_Type, snow.szenario.2.2.avg.fluct$workload)
  snow.szenario.2.2.avg.fluct$group <- cumsum(snow.szenario.2.2.avg.fluct$group_id != lag(snow.szenario.2.2.avg.fluct$group_id, default = ""))
  snow.szenario.2.2.avg.fluct$angle <- ifelse(snow.szenario.2.2.avg.fluct$group == 14 |
                                                 snow.szenario.2.2.avg.fluct$group == 15 |
                                                 snow.szenario.2.2.avg.fluct$group == 16, 0, 90)
  
  snow.szenario.2.2.avg.fluct$nudge_x <- ifelse(snow.szenario.2.2.avg.fluct$group == 14, -1000, 0)
  snow.szenario.2.2.avg.fluct$nudge_x <- ifelse(snow.szenario.2.2.avg.fluct$group == 15, 3600, snow.szenario.2.2.avg.fluct$nudge_x)
  snow.szenario.2.2.avg.fluct$nudge_x <- ifelse(snow.szenario.2.2.avg.fluct$group == 16, 3600, snow.szenario.2.2.avg.fluct$nudge_x)
  snow.szenario.2.2.avg.fluct$nudge_x <- ifelse(snow.szenario.2.2.avg.fluct$group == 17, 1800, snow.szenario.2.2.avg.fluct$nudge_x)
  
  snow.szenario.2.2.avg.fluct$nudge_y <- ifelse(snow.szenario.2.2.avg.fluct$workload < 75, -20, 20)
  snow.szenario.2.2.avg.fluct$nudge_y <- ifelse(snow.szenario.2.2.avg.fluct$group == 2 |
                                                   snow.szenario.2.2.avg.fluct$group == 3 |
                                                   snow.szenario.2.2.avg.fluct$group == 11 |
                                                   snow.szenario.2.2.avg.fluct$group == 13, -20, snow.szenario.2.2.avg.fluct$nudge_y)
  snow.szenario.2.2.avg.fluct$nudge_y <- ifelse(snow.szenario.2.2.avg.fluct$group == 14 |
                                                   snow.szenario.2.2.avg.fluct$group == 15 |
                                                   snow.szenario.2.2.avg.fluct$group == 16, 10, snow.szenario.2.2.avg.fluct$nudge_y)
  
  
  ggplot() +
    geom_line(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black") +
    geom_line(data = snow.szenario.2.2.avg.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2") +
    geom_text(data = subset(snow.szenario.2.2.avg.base, !duplicated(Instance_Type)),
              aes(x = Instance_Number, y = workload, label = Instance_Type),
              size = 2,
              nudge_x = 14400,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = snow.szenario.2.2.avg.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = snow.szenario.2.2.avg.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 2,
              nudge_x = snow.szenario.2.2.avg.fluct$nudge_x,
              nudge_y = snow.szenario.2.2.avg.fluct$nudge_y,
              angle = snow.szenario.2.2.avg.fluct$angle,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#E64B35B2", "#7E6148B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    xlab("Time of day") +
    ylab("CPU hours") +
    ggtitle("Scenario 2 - Fluctuating Workload Only") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
generate.plot.S1.option2()


## Cost-optimized
generate.plot.S1.option3 <- function() {
  
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg[1, ]
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg.base[rep(seq_len(nrow(snow.szenario.2.4.avg.base)), each = 24), ]
  snow.szenario.2.4.avg.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.base$workload <- 96
  snow.szenario.2.4.avg.base$Instance_config <- paste(snow.szenario.2.4.avg.base$Num_Instances_Required, "x ", snow.szenario.2.4.avg.base$Instance_Type) 
  
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg[snow.szenario.2.4.avg$Instance_Number != 0, ]
  snow.szenario.2.4.avg.fluct$Instance_config <- paste(snow.szenario.2.4.avg.fluct$Num_Instances_Required, "x ", snow.szenario.2.4.avg.fluct$Instance_Type) 

  snow.szenario.2.4.avg.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.fluct$Instance_config <- ifelse(snow.szenario.2.4.avg.fluct$Instance_config == "0x NA", "No additional instance", snow.szenario.2.4.avg.fluct$Instance_config)
  snow.szenario.2.4.avg.fluct$workload <- ifelse((as.numeric(snowset.large.cust.hourly$totalCpuTime) - snow.szenario.2.4.avg.base$workload) > 0, 
                                                  as.numeric(snowset.large.cust.hourly$totalCpuTime), snow.szenario.2.4.avg.base$workload)
  snow.szenario.2.4.avg.fluct$group_id <- paste(snow.szenario.2.4.avg.fluct$Num_Instances_Required, snow.szenario.2.4.avg.fluct$Instance_Type, snow.szenario.2.4.avg.fluct$workload)
  snow.szenario.2.4.avg.fluct$group <- cumsum(snow.szenario.2.4.avg.fluct$group_id != lag(snow.szenario.2.4.avg.fluct$group_id, default = ""))
  snow.szenario.2.4.avg.fluct$angle <- ifelse(snow.szenario.2.4.avg.fluct$group == 11 |
                                                 snow.szenario.2.4.avg.fluct$group == 12 |
                                                 snow.szenario.2.4.avg.fluct$group == 13 |
                                                 snow.szenario.2.4.avg.fluct$group == 14 |
                                                 snow.szenario.2.4.avg.fluct$group == 15 |
                                                 snow.szenario.2.4.avg.fluct$group == 6 |
                                                 snow.szenario.2.4.avg.fluct$group == 16, 0, 90)
  
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 11, -1000, 0)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 12, 1000, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 13, -1000, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 14, 6000, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 15, 5000, snow.szenario.2.4.avg.fluct$nudge_x) 
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 16, 6000, snow.szenario.2.4.avg.fluct$nudge_x) 
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 1, -3200, snow.szenario.2.4.avg.fluct$nudge_x) 
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 6, 1000, snow.szenario.2.4.avg.fluct$nudge_x) 
  
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 2 |
                                                   snow.szenario.2.4.avg.fluct$group == 8 |
                                                   snow.szenario.2.4.avg.fluct$group == 10 |
                                                   snow.szenario.2.4.avg.fluct$group == 18 |
                                                   snow.szenario.2.4.avg.fluct$group == 19 |
                                                   snow.szenario.2.4.avg.fluct$group == 20, -55, 22)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 3 |
                                                   snow.szenario.2.4.avg.fluct$group == 4, 50, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$angle == 0, 10, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 12, -12, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 22, 55, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 23, 55, snow.szenario.2.4.avg.fluct$nudge_y)
  
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg.fluct[snow.szenario.2.4.avg.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) +
    geom_line(data = snow.szenario.2.4.avg.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(snow.szenario.2.4.avg.base, !duplicated(Instance_Type)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 48000,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = snow.szenario.2.4.avg.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = snow.szenario.2.4.avg.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 6,
              nudge_x = snow.szenario.2.4.avg.fluct$nudge_x,
              nudge_y = snow.szenario.2.4.avg.fluct$nudge_y,
              angle = snow.szenario.2.4.avg.fluct$angle,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#E64B35B2", "#7E6148B2", "#DC0000B2", "#4DBBD5B2", "darkgoldenrod3")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 14:20:00"), y = 75, label = "EC2 Instance SP\n(3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
vh1.instances <- generate.plot.S1.option3()
ggsave("VH-1.instances.pdf", plot = vh1.instances, width = 14.7551263472)


## Cost-optimized with amdahl = 0.99 (recalc snow.szenario.2.4.avg in scenarios.snow with amdahl.param = 0.99 first)
generate.plot.S1.option3.amdahl <- function() {
  
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg[1, ]
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg.base[rep(seq_len(nrow(snow.szenario.2.4.avg.base)), each = 24), ]
  snow.szenario.2.4.avg.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.base$workload <- 96
  snow.szenario.2.4.avg.base$Instance_config <- paste(snow.szenario.2.4.avg.base$Num_Instances_Required, "x ", snow.szenario.2.4.avg.base$Instance_Type) 
  
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg[snow.szenario.2.4.avg$Instance_Number != 0, ]
  snow.szenario.2.4.avg.fluct$Instance_config <- paste(snow.szenario.2.4.avg.fluct$Num_Instances_Required, "x ", snow.szenario.2.4.avg.fluct$Instance_Type) 

  snow.szenario.2.4.avg.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.fluct$Instance_config <- ifelse(snow.szenario.2.4.avg.fluct$Instance_config == "0x NA", "No additional instance", snow.szenario.2.4.avg.fluct$Instance_config)
  snow.szenario.2.4.avg.fluct$workload <- ifelse((as.numeric(snowset.large.cust.hourly$totalCpuTime) - snow.szenario.2.4.avg.base$workload) > 0, 
                                                 as.numeric(snowset.large.cust.hourly$totalCpuTime), snow.szenario.2.4.avg.base$workload)
  snow.szenario.2.4.avg.fluct$group_id <- paste(snow.szenario.2.4.avg.fluct$Num_Instances_Required, snow.szenario.2.4.avg.fluct$Instance_Type, snow.szenario.2.4.avg.fluct$workload)
  snow.szenario.2.4.avg.fluct$group <- cumsum(snow.szenario.2.4.avg.fluct$group_id != lag(snow.szenario.2.4.avg.fluct$group_id, default = ""))
  snow.szenario.2.4.avg.fluct$angle <- ifelse(snow.szenario.2.4.avg.fluct$group == 11 |
                                                snow.szenario.2.4.avg.fluct$group == 12 |
                                                snow.szenario.2.4.avg.fluct$group == 13 |
                                                snow.szenario.2.4.avg.fluct$group == 14 |
                                                snow.szenario.2.4.avg.fluct$group == 15 |
                                                snow.szenario.2.4.avg.fluct$group == 6 |
                                                snow.szenario.2.4.avg.fluct$group == 16, 0, 90)
  
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 11, -1500, 0)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 12, 1000, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 13, -1000, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 14, 6000, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 15, 5000, snow.szenario.2.4.avg.fluct$nudge_x) 
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 16, 6000, snow.szenario.2.4.avg.fluct$nudge_x) 
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 1, -3200, snow.szenario.2.4.avg.fluct$nudge_x) 
  snow.szenario.2.4.avg.fluct$nudge_x <- ifelse(snow.szenario.2.4.avg.fluct$group == 6, 1000, snow.szenario.2.4.avg.fluct$nudge_x) 
  
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 2 |
                                                  snow.szenario.2.4.avg.fluct$group == 8 |
                                                  snow.szenario.2.4.avg.fluct$group == 10 |
                                                  snow.szenario.2.4.avg.fluct$group == 18 |
                                                  snow.szenario.2.4.avg.fluct$group == 20, -55, 22)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 3, 60, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 4, 50, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$angle == 0, 10, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 12, -12, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 19, -60, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 22, 55, snow.szenario.2.4.avg.fluct$nudge_y)
  snow.szenario.2.4.avg.fluct$nudge_y <- ifelse(snow.szenario.2.4.avg.fluct$group == 23, 55, snow.szenario.2.4.avg.fluct$nudge_y)
  
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg.fluct[snow.szenario.2.4.avg.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) +
    geom_line(data = snow.szenario.2.4.avg.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(snow.szenario.2.4.avg.base, !duplicated(Instance_Type)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 48000,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = snow.szenario.2.4.avg.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = snow.szenario.2.4.avg.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 6,
              nudge_x = snow.szenario.2.4.avg.fluct$nudge_x,
              nudge_y = snow.szenario.2.4.avg.fluct$nudge_y,
              angle = snow.szenario.2.4.avg.fluct$angle,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#7E6148B2", "darkgoldenrod3", "#DC0000B2", "#4DBBD5B2", "#E64B35B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) + 
    annotate("text", x = as.POSIXct("2018-02-22 14:20:00"), y = 75, label = "EC2 Instance SP\n(3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
vh1.instances.amdahl <- generate.plot.S1.option3.amdahl()
ggsave("VH-1.instances.amdahl.pdf", plot = vh1.instances.amdahl, width = 14.7551263472)


## Cost distribution all options VH-1
generate.plot.S1.costs <- function() {
  
  # Base only
  snow.szenario.2.1.base <- snow.szenario.2.1[1, ]
  snow.szenario.2.1.base <- snow.szenario.2.1.base[rep(seq_len(nrow(snow.szenario.2.1.base)), each = 24), ]
  snow.szenario.2.1.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.1.base$workload <- snowset.large.cust.hourly$totalCpuTime
  snow.szenario.2.1.base$dollar_per_CPUh <- snow.szenario.2.1.base$Total_Costs / snow.szenario.2.1.base$workload
  snow.szenario.2.1.base$Approach <- "Base workload only"
  
  # Fluct only
  snow.szenario.2.2.avg.fluct <- snow.szenario.2.2.avg[snow.szenario.2.2.avg$Instance_Number != 0, ]
  snow.szenario.2.2.avg.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.2.avg.fluct$workload <- snowset.large.cust.hourly$totalCpuTime
  snow.szenario.2.2.avg.fluct$dollar_per_CPUh <- snow.szenario.2.2.avg.fluct$Total_Costs / snow.szenario.2.2.avg.fluct$workload
  snow.szenario.2.2.avg.fluct$Approach <- "Fluctuating workload only"
  
  # Optimized
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg[1, ]
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg.base[rep(seq_len(nrow(snow.szenario.2.4.avg.base)), each = 24), ]
  snow.szenario.2.4.avg.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.base$workload <- 96
  
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg[snow.szenario.2.4.avg$Instance_Number != 0, ]
  snow.szenario.2.4.avg.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.fluct$workload <- ifelse((as.numeric(snowset.large.cust.hourly$totalCpuTime) - snow.szenario.2.4.avg.base$workload) > 0, 
                                                  as.numeric(snowset.large.cust.hourly$totalCpuTime), snow.szenario.2.4.avg.base$workload)
  snow.szenario.2.4.avg.fluct$Total_Costs <- snow.szenario.2.4.avg.fluct$Total_Costs + snow.szenario.2.4.avg.base$Total_Costs
  snow.szenario.2.4.avg.fluct$dollar_per_CPUh <- snow.szenario.2.4.avg.fluct$Total_Costs / snow.szenario.2.4.avg.fluct$workload
  snow.szenario.2.4.avg.fluct$Approach <- "Cost-optimization"
  
  
  df <- rbind(snow.szenario.2.1.base, snow.szenario.2.2.avg.fluct, snow.szenario.2.4.avg.fluct)
  df$Approach <- factor(df$Approach, levels = c("Base workload only", "Fluctuating workload only", "Cost-optimization"))
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "darkgoldenrod3", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.08) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.079, label = "Base workload only\nTotal Costs: $98.16", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.0345, label = "Fluctuating workload only\nTotal Costs: $85.19", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.0215, label = "Cost-optimized\nTotal Costs: $56.14", color = "#00A087B2", size = 5)
}
S1.costs <- generate.plot.S1.costs()
ggsave("VH-1-costs.pdf", plot = S1.costs, width = 14.7551263472)


## Cost distribution all options VH-1 - ZOOM-IN
generate.plot.S1.costs.zoom <- function() {
  
  # Base only
  snow.szenario.2.1.base <- snow.szenario.2.1[1, ]
  snow.szenario.2.1.base <- snow.szenario.2.1.base[rep(seq_len(nrow(snow.szenario.2.1.base)), each = 24), ]
  snow.szenario.2.1.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.1.base$workload <- snowset.large.cust.hourly$totalCpuTime
  snow.szenario.2.1.base$dollar_per_CPUh <- snow.szenario.2.1.base$Total_Costs / snow.szenario.2.1.base$workload
  snow.szenario.2.1.base$Approach <- "Base workload only"
  
  # Fluct only
  snow.szenario.2.2.avg.fluct <- snow.szenario.2.2.avg[snow.szenario.2.2.avg$Instance_Number != 0, ]
  snow.szenario.2.2.avg.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.2.avg.fluct$workload <- snowset.large.cust.hourly$totalCpuTime
  snow.szenario.2.2.avg.fluct$dollar_per_CPUh <- snow.szenario.2.2.avg.fluct$Total_Costs / snow.szenario.2.2.avg.fluct$workload
  snow.szenario.2.2.avg.fluct$Approach <- "Fluctuating workload only"
  
  # Optimized
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg[1, ]
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg.base[rep(seq_len(nrow(snow.szenario.2.4.avg.base)), each = 24), ]
  snow.szenario.2.4.avg.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.base$workload <- 96
  
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg[snow.szenario.2.4.avg$Instance_Number != 0, ]
  snow.szenario.2.4.avg.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.fluct$workload <- ifelse((as.numeric(snowset.large.cust.hourly$totalCpuTime) - snow.szenario.2.4.avg.base$workload) > 0, 
                                                 as.numeric(snowset.large.cust.hourly$totalCpuTime), snow.szenario.2.4.avg.base$workload)
  snow.szenario.2.4.avg.fluct$Total_Costs <- snow.szenario.2.4.avg.fluct$Total_Costs + snow.szenario.2.4.avg.base$Total_Costs
  snow.szenario.2.4.avg.fluct$dollar_per_CPUh <- snow.szenario.2.4.avg.fluct$Total_Costs / snow.szenario.2.4.avg.fluct$workload
  snow.szenario.2.4.avg.fluct$Approach <- "Cost-optimization"
  
  
  snow.szenario.2.1.base <- snow.szenario.2.1.base[snow.szenario.2.1.base$Instance_Number >= as.POSIXct("2018-02-22 11:00:00") &
                                                     snow.szenario.2.1.base$Instance_Number <= as.POSIXct("2018-02-22 17:00:00"), ]
  snow.szenario.2.2.avg.fluct <- snow.szenario.2.2.avg.fluct[snow.szenario.2.2.avg.fluct$Instance_Number >= as.POSIXct("2018-02-22 11:00:00") &
                                                               snow.szenario.2.2.avg.fluct$Instance_Number <= as.POSIXct("2018-02-22 17:00:00"), ]
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg.base[snow.szenario.2.4.avg.base$Instance_Number >= as.POSIXct("2018-02-22 11:00:00") &
                                                             snow.szenario.2.4.avg.base$Instance_Number <= as.POSIXct("2018-02-22 17:00:00"), ]
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg.fluct[snow.szenario.2.4.avg.fluct$Instance_Number >= as.POSIXct("2018-02-22 11:00:00") &
                                                               snow.szenario.2.4.avg.fluct$Instance_Number <= as.POSIXct("2018-02-22 17:00:00"), ]
  
  
  dfs <- rbind(snow.szenario.2.1.base, snow.szenario.2.2.avg.fluct, snow.szenario.2.4.avg.fluct)
  dfs$Approach <- factor(dfs$Approach, levels = c("Base workload only", "Fluctuating workload only", "Cost-optimization"))
  
  seg <- rbind(snow.szenario.2.1.base[4, ], snow.szenario.2.2.avg.fluct[4, ], snow.szenario.2.4.avg.fluct[4, ])
  seg$nudge_x <- ifelse(seg$Instance_Type == "a1.xlarge", -3600, 4500)
  seg$nudge_y <- ifelse(seg$Instance_Type == "a1.xlarge", 0.004, -0.004)
  
  
  ggplot(data = dfs, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    geom_segment(data = seg,
                 aes(x = Instance_Number - 1800, y = dollar_per_CPUh, xend = Instance_Number + 1800, yend = dollar_per_CPUh, col = Instance_Type),
                 color = c("#E64B35B2", "darkgoldenrod3", "#00A087B2"),
                 size = 1.2,
                 alpha = 0.8,
                 show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "darkgoldenrod3", "#00A087B2")) +
    scale_x_datetime(breaks = seq(from = as.POSIXct("2018-02-22 10:00:00", format = "%Y-%m-%d %H:%M:%S"),
                                  to = as.POSIXct("2018-02-22 17:00:00", format = "%Y-%m-%d %H:%M:%S"),
                                  by = "hour"),
                     date_labels = "%H:%M") +
    ylim(0, 0.045) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 16:45:00"), y = 0.035, label = "Base workload only", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 17:00:00"), y = 0.02, label = "Fluctuating\nworkload only", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 16:45:00"), y = 0.013, label = "Cost-optimized", color = "#00A087B2", size = 5) +
    
    annotate("text", x = as.POSIXct("2018-02-22 14:00:00"), y = 0.014, label = "Base: 5x c6g.16xlarge\nHourly Costs: $4.09", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 14:00:00"), y = 0.036, label = "Fluct: 13x r5.8xlarge\nHourly Costs: $7.43", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 13:45:00"), y = 0.0265, label = "Base: 9x a1.metal\nFluct: 7x r5.8xlarge", color = "#00A087B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 14:35:00"), y = 0.0265, label = "Hourly Costs: $5.38", color = "#00A087B2", size = 5)
}
S1.costs.zoom <- generate.plot.S1.costs.zoom()
ggsave("VH-1-costs-zoom.pdf", plot = S1.costs.zoom, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                                  # Plots VH-2
## ------------------------------------------------------------------------------------------------------------ ##

## workload
generate.plot.S2.workload <- function() {
  ggplot() +
    geom_line(data = snowset.middle.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 100) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 95, label = "Total workload demand: 362 CPUh", color = "black", size = 5)
}
S2.workload <- generate.plot.S2.workload()
ggsave("VH-2.pdf", plot = S2.workload, width = 14.7551263472)


## Cost distribution all options VH-2
generate.plot.S2.costs <- function() {
  # Base only
  snow.szenario.3.1.base <- snow.szenario.3.1[1, ]
  snow.szenario.3.1.base <- snow.szenario.3.1.base[rep(seq_len(nrow(snow.szenario.3.1.base)), each = 24), ]
  snow.szenario.3.1.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.1.base$workload <- snowset.middle.cust.hourly$totalCpuTime
  snow.szenario.3.1.base$dollar_per_CPUh <- snow.szenario.3.1.base$Total_Costs / snow.szenario.3.1.base$workload
  snow.szenario.3.1.base$Approach <- "Base workload only"
  
  # Fluct only
  snow.szenario.3.2.avg.fluct <- snow.szenario.3.2.avg[snow.szenario.3.2.avg$Instance_Number != 0, ]
  snow.szenario.3.2.avg.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.2.avg.fluct$workload <- snowset.middle.cust.hourly$totalCpuTime
  snow.szenario.3.2.avg.fluct$dollar_per_CPUh <- snow.szenario.3.2.avg.fluct$Total_Costs / snow.szenario.3.2.avg.fluct$workload
  snow.szenario.3.2.avg.fluct$Approach <- "Fluctuating workload only"
  
  # Optimized
  snow.szenario.3.4.avg.base <- snow.szenario.3.4.avg[1, ]
  snow.szenario.3.4.avg.base <- snow.szenario.3.4.avg.base[rep(seq_len(nrow(snow.szenario.3.4.avg.base)), each = 24), ]
  snow.szenario.3.4.avg.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.4.avg.base$workload <- 8
  
  snow.szenario.3.4.avg.fluct <- snow.szenario.3.4.avg[snow.szenario.3.4.avg$Instance_Number != 0, ]
  snow.szenario.3.4.avg.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.4.avg.fluct$workload <- ifelse((as.numeric(snowset.middle.cust.hourly$totalCpuTime) - snow.szenario.3.4.avg.base$workload) > 0, 
                                                  as.numeric(snowset.middle.cust.hourly$totalCpuTime), snow.szenario.3.4.avg.base$workload)
  snow.szenario.3.4.avg.fluct$Total_Costs <- snow.szenario.3.4.avg.fluct$Total_Costs + snow.szenario.3.4.avg.base$Total_Costs
  snow.szenario.3.4.avg.fluct$dollar_per_CPUh <- snow.szenario.3.4.avg.fluct$Total_Costs / snow.szenario.3.4.avg.fluct$workload
  snow.szenario.3.4.avg.fluct$Approach <- "Cost-optimized"
  
  df <- rbind(snow.szenario.3.1.base, snow.szenario.3.2.avg.fluct, snow.szenario.3.4.avg.fluct)
  df$Approach <- factor(df$Approach, levels = c("Base workload only", "Fluctuating workload only", "Cost-optimized"))
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "darkgoldenrod3", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.2) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 0.18, label = "Base workload only\nTotal Costs: $25.70", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:25:00"), y = 0.07, label = "Fluctuating\nworkload only\nTotal Costs: $10.59", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 21:35:00"), y = 0.005, label = "Cost-optimized\nTotal Costs: $7.02", color = "#00A087B2", size = 5)
}
S2.costs <- generate.plot.S2.costs()
ggsave("VH-2-costs.pdf", plot = S2.costs, width = 14.7551263472)


# Cost-optimized instance config
generate.plot.S2.option3 <- function() {
  
  snow.szenario.3.4.avg.base <- snow.szenario.3.4.avg[1, ]
  snow.szenario.3.4.avg.base <- snow.szenario.3.4.avg.base[rep(seq_len(nrow(snow.szenario.3.4.avg.base)), each = 24), ]
  snow.szenario.3.4.avg.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.4.avg.base$workload <- 8
  snow.szenario.3.4.avg.base$Instance_config <- paste(snow.szenario.3.4.avg.base$Num_Instances_Required, "x ", snow.szenario.3.4.avg.base$Instance_Type) 
  
  snow.szenario.3.4.avg.fluct <- snow.szenario.3.4.avg[snow.szenario.3.4.avg$Instance_Number != 0, ]
  snow.szenario.3.4.avg.fluct$Instance_config <- paste(snow.szenario.3.4.avg.fluct$Num_Instances_Required, "x ", snow.szenario.3.4.avg.fluct$Instance_Type) 
  snow.szenario.3.4.avg.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.4.avg.fluct$Instance_config <- ifelse(snow.szenario.3.4.avg.fluct$Instance_config == "0x NA", "No additional instance", snow.szenario.3.4.avg.fluct$Instance_config)
  snow.szenario.3.4.avg.fluct$workload <- ifelse((as.numeric(snowset.middle.cust.hourly$totalCpuTime) - snow.szenario.3.4.avg.base$workload) > 0, 
                                                  as.numeric(snowset.middle.cust.hourly$totalCpuTime), snow.szenario.3.4.avg.base$workload)
  snow.szenario.3.4.avg.fluct$group_id <- paste(snow.szenario.3.4.avg.fluct$Num_Instances_Required, snow.szenario.3.4.avg.fluct$Instance_Type, snow.szenario.3.4.avg.fluct$workload)
  snow.szenario.3.4.avg.fluct$group <- cumsum(snow.szenario.3.4.avg.fluct$group_id != lag(snow.szenario.3.4.avg.fluct$group_id, default = ""))
  snow.szenario.3.4.avg.fluct$angle <- ifelse(snow.szenario.3.4.avg.fluct$group == 3 |
                                                 snow.szenario.3.4.avg.fluct$group == 4 |
                                                 snow.szenario.3.4.avg.fluct$group == 5 |
                                                 snow.szenario.3.4.avg.fluct$group == 6 |
                                                 snow.szenario.3.4.avg.fluct$group == 8 |
                                                 snow.szenario.3.4.avg.fluct$group == 9 |
                                                 snow.szenario.3.4.avg.fluct$group == 13 |
                                                 snow.szenario.3.4.avg.fluct$group == 14 |
                                                 snow.szenario.3.4.avg.fluct$group == 15 |
                                                 snow.szenario.3.4.avg.fluct$group == 16, 90, 0)

  snow.szenario.3.4.avg.fluct$nudge_y <- ifelse(snow.szenario.3.4.avg.fluct$group == 1, -5, 5)
  snow.szenario.3.4.avg.fluct$nudge_y <- ifelse(snow.szenario.3.4.avg.fluct$angle == 90, 21, snow.szenario.3.4.avg.fluct$nudge_y)
  
  snow.szenario.3.4.avg.fluct <- snow.szenario.3.4.avg.fluct[snow.szenario.3.4.avg.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = snowset.middle.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) +
    geom_line(data = snow.szenario.3.4.avg.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(snow.szenario.3.4.avg.base, !duplicated(Instance_Type)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 32000,
              vjust = -5,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = snow.szenario.3.4.avg.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = snow.szenario.3.4.avg.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 6,
              nudge_y = snow.szenario.3.4.avg.fluct$nudge_y,
              angle = snow.szenario.3.4.avg.fluct$angle,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#E64B35B2", "#7E6148B2", "#4DBBD5B2", "#DC0000B2", "darkgoldenrod3")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 100) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 09:55:00"), y = 17, label = "EC2 Instance SP\n(3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
vh2.instances <- generate.plot.S2.option3()
ggsave("VH-2.instances.pdf", plot = vh2.instances, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                                    # Plots VH-3
## ------------------------------------------------------------------------------------------------------------ ##

## workload
generate.plot.S3.workload <- function() {
  ggplot() +
    geom_line(data = snowset.small.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    #ylim(0, 5) + 
    scale_y_continuous(labels = scales::label_number(accuracy = .01), 
                       limits = c(0, 5)) +
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 4.75, label = "Total workload demand: 44 CPUh", color = "black", size = 5)
}
S3.workload <- generate.plot.S3.workload()
ggsave("VH-3.pdf", plot = S3.workload, width = 14.7551263472)


## Cost distribution all options VH-3
generate.plot.S3.costs <- function() {
  # Base only
  snow.szenario.4.1.base <- snow.szenario.4.1[1, ]
  snow.szenario.4.1.base <- snow.szenario.4.1.base[rep(seq_len(nrow(snow.szenario.4.1.base)), each = 24), ]
  snow.szenario.4.1.base$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.1.base$workload <- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.1.base$dollar_per_CPUh <- snow.szenario.4.1.base$Total_Costs / snow.szenario.4.1.base$workload
  snow.szenario.4.1.base$Approach <- "Base workload only"
  
  # Fluct only
  snow.szenario.4.2.avg.fluct <- snow.szenario.4.2.avg[snow.szenario.4.2.avg$Instance_Number != 0, ]
  snow.szenario.4.2.avg.fluct$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.2.avg.fluct$workload <- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <- snow.szenario.4.2.avg.fluct$Total_Costs / snow.szenario.4.2.avg.fluct$workload
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <- ifelse(is.na(snow.szenario.4.2.avg.fluct$dollar_per_CPUh), 
                                                         0, snow.szenario.4.2.avg.fluct$dollar_per_CPUh)
  snow.szenario.4.2.avg.fluct$Approach <- "Fluctuating workload only"
  
  # Optimized
  snow.szenario.4.4.avg.base <- snow.szenario.4.4.avg[1, ]
  snow.szenario.4.4.avg.base <- snow.szenario.4.4.avg.base[rep(seq_len(nrow(snow.szenario.4.4.avg.base)), each = 24), ]
  snow.szenario.4.4.avg.base$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.4.avg.base$workload <- 4
  
  snow.szenario.4.4.avg.fluct <- snow.szenario.4.4.avg[snow.szenario.3.4.avg$Instance_Number != 0, ]
  snow.szenario.4.4.avg.fluct$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.4.avg.fluct$workload <- ifelse((as.numeric(snowset.small.cust.hourly$totalCpuTime) - snow.szenario.4.4.avg.base$workload) > 0, 
                                                 as.numeric(snowset.small.cust.hourly$totalCpuTime), snow.szenario.4.4.avg.base$workload)
  snow.szenario.4.4.avg.fluct$Total_Costs <- snow.szenario.4.4.avg.fluct$Total_Costs + snow.szenario.4.4.avg.base$Total_Costs
  snow.szenario.4.4.avg.fluct$dollar_per_CPUh <- snow.szenario.4.4.avg.fluct$Total_Costs / snow.szenario.4.4.avg.fluct$workload
  snow.szenario.4.4.avg.fluct$Approach <- "Cost-optimized"
  
  
  df <- rbind(snow.szenario.4.1.base, snow.szenario.4.2.avg.fluct, snow.szenario.4.4.avg.fluct)
  df$Approach <- factor(df$Approach, levels = c("Base workload only", "Fluctuating workload only", "Cost-optimized"))
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "darkgoldenrod3", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.08) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.07, label = "Base workload only\nTotal Costs: $1.61", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:25:00"), y = 0.04, label = "Fluctuating\nworkload only\nTotal Costs: $1.33", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 0.004, label = "Cost-optimized\nTotal Costs: $0.95", color = "#00A087B2", size = 5)
}
S3.costs <- generate.plot.S3.costs()
ggsave("VH-3-costs.pdf", plot = S3.costs, width = 14.7551263472)


# Cost-optimized instance config
generate.plot.S3.option3 <- function() {
  
  snow.szenario.4.4.avg.base <- snow.szenario.4.4.avg[1, ]
  snow.szenario.4.4.avg.base <- snow.szenario.4.4.avg.base[rep(seq_len(nrow(snow.szenario.4.4.avg.base)), each = 24), ]
  snow.szenario.4.4.avg.base$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.4.avg.base$workload <- 4
  snow.szenario.4.4.avg.base$Instance_config <- paste(snow.szenario.4.4.avg.base$Num_Instances_Required, "x ", snow.szenario.4.4.avg.base$Instance_Type) 
  
  snow.szenario.4.4.avg.fluct <- snow.szenario.4.4.avg[snow.szenario.4.4.avg$Instance_Number != 0, ]
  snow.szenario.4.4.avg.fluct$Instance_config <- paste(snow.szenario.4.4.avg.fluct$Num_Instances_Required, "x ", snow.szenario.4.4.avg.fluct$Instance_Type) 
  snow.szenario.4.4.avg.fluct$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.4.avg.fluct$Instance_config <- ifelse(snow.szenario.4.4.avg.fluct$Instance_config == "0x NA", "No additional instance", snow.szenario.4.4.avg.fluct$Instance_config)
  snow.szenario.4.4.avg.fluct$workload <- ifelse((as.numeric(snowset.small.cust.hourly$totalCpuTime) - snow.szenario.4.4.avg.base$workload) > 0, 
                                                 as.numeric(snowset.small.cust.hourly$totalCpuTime), snow.szenario.4.4.avg.base$workload)
  snow.szenario.4.4.avg.fluct$group_id <- paste(snow.szenario.4.4.avg.fluct$Num_Instances_Required, snow.szenario.4.4.avg.fluct$Instance_Type, snow.szenario.4.4.avg.fluct$workload)
  snow.szenario.4.4.avg.fluct$group <- cumsum(snow.szenario.4.4.avg.fluct$group_id != lag(snow.szenario.4.4.avg.fluct$group_id, default = ""))
  
  snow.szenario.4.4.avg.fluct <- snow.szenario.4.4.avg.fluct[snow.szenario.4.4.avg.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = snowset.small.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) +
    geom_line(data = snow.szenario.4.4.avg.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(snow.szenario.4.4.avg.base, !duplicated(Instance_Type)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 4500,
              vjust = -2.5,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = snow.szenario.4.4.avg.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = snow.szenario.4.4.avg.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 6,
              nudge_y = 0.25,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#E64B35B2", "#7E6148B2", "#4DBBD5B2", "#DC0000B2", "darkgoldenrod3")) +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_y_continuous(labels = scales::label_number(accuracy = .01), 
                       limits = c(0, 5)) +
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) + 
    annotate("text", x = as.POSIXct("2018-02-22 04:08:00"), y = 4.2, label = "Standard RI (3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
vh3.instances <- generate.plot.S3.option3()
ggsave("VH-3.instances.pdf", plot = vh3.instances, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                          # Average vs. Minimum Spot prices
## ------------------------------------------------------------------------------------------------------------ ##

# Comparison between fluct only approach on VH-3 with min and avg spot prices
generate.plot.costs.avg.min <- function() {
  
  # Fluct only avg
  snow.szenario.4.2.avg.fluct <- snow.szenario.4.2.avg[snow.szenario.4.2.avg$Instance_Number != 0, ]
  snow.szenario.4.2.avg.fluct$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.2.avg.fluct$workload <- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <- snow.szenario.4.2.avg.fluct$Total_Costs / snow.szenario.4.2.avg.fluct$workload
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <- ifelse(is.na(snow.szenario.4.2.avg.fluct$dollar_per_CPUh), 
                                                         0, snow.szenario.4.2.avg.fluct$dollar_per_CPUh)
  snow.szenario.4.2.avg.fluct$Approach <- "Average Spot Prices"
  snow.szenario.4.2.avg.fluct$Instance_config <- paste(snow.szenario.4.2.avg.fluct$Num_Instances_Required, "x ", snow.szenario.4.2.avg.fluct$Instance_Type, 
                                                        "\n(", round(snow.szenario.4.2.avg.fluct$Total_Costs, 4), "$/h)", sep = "")
  
  # Fluct only min
  snow.szenario.4.2.min.fluct <- snow.szenario.4.2.min[snow.szenario.4.2.min$Instance_Number != 0, ]
  snow.szenario.4.2.min.fluct$Instance_Number <- snowset.small.cust.hourly$hour_group
  snow.szenario.4.2.min.fluct$workload <- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.2.min.fluct$dollar_per_CPUh <- snow.szenario.4.2.min.fluct$Total_Costs / snow.szenario.4.2.min.fluct$workload
  snow.szenario.4.2.min.fluct$dollar_per_CPUh <- ifelse(is.na(snow.szenario.4.2.min.fluct$dollar_per_CPUh), 
                                                         0, snow.szenario.4.2.min.fluct$dollar_per_CPUh)
  snow.szenario.4.2.min.fluct$Approach <- "Minimum Spot Prices"
  snow.szenario.4.2.min.fluct$Instance_config <- paste(snow.szenario.4.2.min.fluct$Num_Instances_Required, "x ", snow.szenario.4.2.min.fluct$Instance_Type, 
                                                        "\n(", round(snow.szenario.4.2.min.fluct$Total_Costs, 4), "$/h)", sep = "")
  
  seg <- rbind(snow.szenario.4.2.avg.fluct[8, ], snow.szenario.4.2.min.fluct[8, ])
  seg$nudge_x <- ifelse(seg$Instance_Type == "a1.xlarge", -3600, 4500)
  seg$nudge_y <- ifelse(seg$Instance_Type == "a1.xlarge", 0.004, -0.004)
  
  df <- rbind(snow.szenario.4.2.avg.fluct, snow.szenario.4.2.min.fluct)
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    geom_segment(data = seg, 
                 aes(x = Instance_Number - 1800, y = dollar_per_CPUh, xend = Instance_Number + 1800, yend = dollar_per_CPUh, col = Instance_Type),
                 color = c("#E64B35B2", "#00A087B2"),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = seg, aes(x = Instance_Number, y = dollar_per_CPUh, label = Instance_config, col = Instance_Type),
              size = 6,
              color = c("#E64B35B2", "#00A087B2"),
              nudge_x = seg$nudge_x,
              nudge_y = seg$nudge_y,
              show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 23:30:00"), y = 0.033, label = "Average\nSpot Prices", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:30:00"), y = 0.023, label = "Minimum\nSpot Prices", color = "#00A087B2", size = 5)
}
S3.avg.min <- generate.plot.costs.avg.min()
ggsave("VH-3-avg-min-v2.pdf", plot = S3.avg.min, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                              # Growth Plots - Approach
## ------------------------------------------------------------------------------------------------------------ ##

# Workloads with growth
generate.plot.growth <- function() {
  
  workload.wo.growth <- snowset.middle.cust.hourly
  workload.wo.growth$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                              6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  workload.wo.growth$group <- "February 2023"
  
  workload.with.growth <- workload.wo.growth
  workload.with.growth$totalCpuTime <- workload.with.growth$totalCpuTime * 1.5
  workload.wo.growth$group <- "February 2024"
  
  df <- rbind(workload.wo.growth, workload.with.growth)
  
  ggplot() +
    geom_line(data = df, aes(x = hour_group, y = totalCpuTime, color = group), linetype = "dotted", size = 1.2, show.legend = FALSE) +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_color_manual(values = c("darkgrey", "black")) +
    ylim(0, 50) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 14, label = "In one year", color = "darkgrey", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 4, label = "Today", color = "black", size = 5)
}
growth.workload <- generate.plot.growth()
ggsave("growth.workload.pdf", plot = growth.workload, width = 14.7551263472)


# Instance configurations for growth - first option
generate.plot.growth.instances.1 <- function() {
  
  workload.wo.growth <- snowset.middle.cust.hourly
  workload.wo.growth$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                     6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  workload.wo.growth$group <- "Today"
  
  workload.with.growth <- workload.wo.growth
  workload.with.growth$totalCpuTime <- workload.with.growth$totalCpuTime * 1.5
  workload.with.growth$group <- "Future"
  
  df <- rbind(workload.wo.growth, workload.with.growth)
  
  workload.fluct <- list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                         6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6)
  workload.fluct <- as.numeric(lapply(workload.fluct, function(x) (x * 1.5) - 8))
  result.growth <- find.cheapest.instance.final(8, workload.fluct, 2)
  
  result.growth.base <- result.growth[1, ]
  result.growth.base <- result.growth.base[rep(seq_len(nrow(result.growth.base)), each = 24), ]
  result.growth.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.growth.base$workload <- 8
  result.growth.base$Instance_config <- paste(result.growth.base$Num_Instances_Required, "x ", result.growth.base$Instance_Type) 
  
  result.growth.fluct <- result.growth[result.growth$Instance_Number != 0, ]
  result.growth.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.growth.fluct$Instance_config <- paste(result.growth.fluct$Num_Instances_Required, "x ", result.growth.fluct$Instance_Type) 
                                           #"\n(", round(result.growth.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.growth.fluct$Instance_config <- ifelse(result.growth.fluct$Instance_config == "0x NA", "No additional instance", result.growth.fluct$Instance_config)
  result.growth.fluct$workload <- as.numeric(lapply(workload.fluct, function(x) (x + 8)))
  result.growth.fluct$group_id <- paste(result.growth.fluct$Num_Instances_Required, result.growth.fluct$Instance_Type, result.growth.fluct$workload)
  result.growth.fluct$group <- cumsum(result.growth.fluct$group_id != lag(result.growth.fluct$group_id, default = ""))
  duplicates <- duplicated(result.growth.fluct$group) | duplicated(result.growth.fluct$group, fromLast = TRUE)

  result.growth.fluct <- result.growth.fluct[result.growth.fluct$Num_Instances_Required > 0, ]
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$Instance_Type == "r5.8xlarge", 3, 10)
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$Instance_Type == "a1.medium", 12, result.growth.fluct$nudge_y)
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$Instance_Type == "a1.2xlarge", 2, result.growth.fluct$nudge_y)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$Instance_Type == "r5.8xlarge", -5400, 0)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$Instance_Number == 9, -3600, result.growth.fluct$nudge_x)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$Instance_Number == 12, 3600, result.growth.fluct$nudge_x)
  result.growth.fluct$angle <- ifelse(result.growth.fluct$Instance_Type == "r5.8xlarge" |
                                        result.growth.fluct$Instance_Type == "a1.2xlarge", 0, 90)
  
  ggplot() +
    geom_line(data = result.growth.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.growth.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 34000,
              vjust = 2,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.growth.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = result.growth.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 6,
              nudge_x = result.growth.fluct$nudge_x,
              nudge_y = result.growth.fluct$nudge_y,
              angle = result.growth.fluct$angle,
              show.legend = FALSE) +
    scale_color_manual(values = c("#7E6148B2", "#00A087B2", "darkgoldenrod3", "#E64B35B2")) +
    geom_line(data = workload.wo.growth, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) +
    geom_line(data = workload.with.growth, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "darkgrey", size = 1.2) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 50) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 11:00:00"), y = 1, label = "EC2 Instance SP (3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
growth.instances.1 <- generate.plot.growth.instances.1()
ggsave("growth.instances.1.pdf", plot = growth.instances.1, width = 14.7551263472)


# Instance configurations for growth - second option
generate.plot.growth.instances.2 <- function() {
  
  workload.wo.growth <- snowset.middle.cust.hourly
  workload.wo.growth$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                     6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  workload.wo.growth$group <- "Today"
  
  workload.with.growth <- workload.wo.growth
  workload.with.growth$totalCpuTime <- workload.with.growth$totalCpuTime * 1.5
  workload.wo.growth$group <- "Future"
  
  df <- rbind(workload.wo.growth, workload.with.growth)
  
  result.growth <- find.cheapest.instance.final(12, list(0, 0, 0, 0, 0, 0, 0, 0, 0, (24*1.5-12), (32*1.5-12), 0, 
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
  
  result.growth.base <- result.growth[1, ]
  result.growth.base <- result.growth.base[rep(seq_len(nrow(result.growth.base)), each = 24), ]
  result.growth.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.growth.base$workload <- 12
  result.growth.base$Instance_config <- paste(result.growth.base$Num_Instances_Required, "x ", result.growth.base$Instance_Type) 
  
  result.growth.fluct <- result.growth[result.growth$Instance_Number != 0, ]
  result.growth.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.growth.fluct$Instance_config <- paste(result.growth.fluct$Num_Instances_Required, "x ", result.growth.fluct$Instance_Type)

  result.growth.fluct$Instance_config <- ifelse(result.growth.fluct$Instance_config == "0x NA", "No additional instance", result.growth.fluct$Instance_config)
  result.growth.fluct$workload <- ifelse(result.growth.fluct$Instance_Type == "r5.8xlarge", 36, 48)    
  result.growth.fluct$group_id <- paste(result.growth.fluct$Num_Instances_Required, result.growth.fluct$Instance_Type, result.growth.fluct$workload)
  result.growth.fluct$group <- cumsum(result.growth.fluct$group_id != lag(result.growth.fluct$group_id, default = ""))
  duplicates <- duplicated(result.growth.fluct$group) | duplicated(result.growth.fluct$group, fromLast = TRUE)
  result.growth.fluct$angle <- ifelse(duplicates, 0, 90)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$Instance_Type == "r5.8xlarge", -5400, 0)
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$Instance_Type == "r5.8xlarge", 2.5, 2)
  result.growth.fluct <- result.growth.fluct[result.growth.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = result.growth.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.growth.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 4500,
              vjust = -3,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.growth.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 color = c("#E64B35B2", "#7E6148B2"),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = result.growth.fluct, aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = result.growth.fluct$nudge_x,
              nudge_y = result.growth.fluct$nudge_y,
              show.legend = FALSE,
              color = c("#E64B35B2", "#7E6148B2")) +
    geom_line(data = workload.wo.growth, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) +
    geom_line(data = workload.with.growth, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "darkgrey", size = 1.2) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 50) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 04:37:00"), y = 15, label = "EC2 Instance SP (3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
growth.instances.2 <- generate.plot.growth.instances.2()
ggsave("growth.instances.2.pdf", plot = growth.instances.2, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                              # Growth Plots - Discussion
## ------------------------------------------------------------------------------------------------------------ ##

# Workloads with growth
generate.plot.growth.v2 <- function() {
  
  workload.wo.growth <- snowset.large.cust.hourly
  workload.wo.growth$totalCpuTime <- as.numeric(as.list(snowset.large.cust.hourly$totalCpuTime))
  workload.wo.growth$group <- "February 2023"
  
  workload.with.growth <- workload.wo.growth
  workload.with.growth$totalCpuTime <- workload.with.growth$totalCpuTime * 1.2
  workload.wo.growth$group <- "February 2024"
  
  df <- rbind(workload.wo.growth, workload.with.growth)
  
  ggplot() +
    geom_line(data = df, aes(x = hour_group, y = totalCpuTime, color = group), linetype = "dotted", size = 1.2, show.legend = FALSE) +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_color_manual(values = c("darkgrey", "black")) +
    ylim(0, 300) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 140, label = "In one year", color = "darkgrey", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:25:00"), y = 80, label = "Today", color = "black", size = 5)
}
growth.workload.v2 <- generate.plot.growth.v2()
ggsave("growth.workload.v2.pdf", plot = growth.workload.v2, width = 14.7551263472)


# Instance configurations for growth - second option
generate.plot.growth.v2.instances <- function() {
  
  workload.wo.growth <- snowset.large.cust.hourly
  workload.wo.growth$totalCpuTime <- as.numeric(as.list(snowset.large.cust.hourly$totalCpuTime))
  workload.wo.growth$group <- "February 2023"
  
  workload.with.growth <- workload.wo.growth
  workload.with.growth$totalCpuTime <- workload.with.growth$totalCpuTime * 1.2
  workload.wo.growth$group <- "February 2024"
  
  df <- rbind(workload.wo.growth, workload.with.growth)
  
  new.workload.fluct <- lapply(as.numeric(as.list(snowset.large.cust.hourly$totalCpuTime))
                               , function(x) (x * (1 + 0.20)-128))
  new.workload.fluct <- ifelse(new.workload.fluct < 0, 0, new.workload.fluct)
  
  result.growth <- find.cheapest.instance.final(128, new.workload.fluct, 2)
  
  result.growth.base <- result.growth[1, ]
  result.growth.base <- result.growth.base[rep(seq_len(nrow(result.growth.base)), each = 24), ]
  result.growth.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  result.growth.base$workload <- 128
  result.growth.base$Instance_config <- paste(result.growth.base$Num_Instances_Required, "x ", result.growth.base$Instance_Type) 
  
  result.growth.fluct <- result.growth[result.growth$Instance_Number != 0, ]
  result.growth.fluct$Instance_Number <- snowset.large.cust.hourly$hour_group
  result.growth.fluct$Instance_config <- paste(result.growth.fluct$Num_Instances_Required, "x ", result.growth.fluct$Instance_Type)

  result.growth.fluct$Instance_config <- ifelse(result.growth.fluct$Instance_config == "0x NA", "No additional instance", result.growth.fluct$Instance_config)
  result.growth.fluct$workload <- ifelse(workload.with.growth$totalCpuTime - 128 > 0, workload.with.growth$totalCpuTime, 128)  
  result.growth.fluct$group_id <- paste(result.growth.fluct$Num_Instances_Required, result.growth.fluct$Instance_Type, result.growth.fluct$workload)
  result.growth.fluct$group <- cumsum(result.growth.fluct$group_id != lag(result.growth.fluct$group_id, default = ""))
  duplicates <- duplicated(result.growth.fluct$group) | duplicated(result.growth.fluct$group, fromLast = TRUE)
  
  result.growth.fluct$angle <- ifelse(result.growth.fluct$group == 1 |
                                        result.growth.fluct$group == 3 |
                                        result.growth.fluct$group == 4, 90, 0)
  
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 1, -3000, 0)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 6, 2000, result.growth.fluct$nudge_x)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 8, -2000, result.growth.fluct$nudge_x)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 9, 1000, result.growth.fluct$nudge_x)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 10, -1000, result.growth.fluct$nudge_x)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 11, 6000, result.growth.fluct$nudge_x)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 12, 5000, result.growth.fluct$nudge_x)
  result.growth.fluct$nudge_x <- ifelse(result.growth.fluct$group == 13, 5000, result.growth.fluct$nudge_x)
  
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$group == 1, 0, 15)
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$group == 3, 60, result.growth.fluct$nudge_y)
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$group == 4, 60, result.growth.fluct$nudge_y)
  result.growth.fluct$nudge_y <- ifelse(result.growth.fluct$group == 9, -15, result.growth.fluct$nudge_y)
  
  result.growth.fluct <- result.growth.fluct[result.growth.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = result.growth.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.growth.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 48000,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.growth.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = result.growth.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 6,
              nudge_x = result.growth.fluct$nudge_x,
              nudge_y = result.growth.fluct$nudge_y,
              angle = result.growth.fluct$angle,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#E64B35B2", "#7E6148B2", "darkgoldenrod3")) +
    geom_line(data = workload.wo.growth, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) +
    geom_line(data = workload.with.growth, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "darkgrey", size = 1.2) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 300) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.POSIXct("2018-02-22 14:20:00"), y = 100, label = "EC2 Instance SP\n(3 years, all upfront)", 
             color = "#3C5488B2", size = 6)
}
growth.v2.instances <- generate.plot.growth.v2.instances()
ggsave("growth.v2.instances.pdf", plot = growth.v2.instances, width = 14.7551263472)