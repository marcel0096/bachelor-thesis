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
  #workload.M1.M2$group <- "Workload" 
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
    geom_line(data = workload.M1.M2, aes(x = hour_group, y = totalCpuTime), color = "black", linetype = "dotted", size = 1.2) + 
    geom_line(data = result.M1.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.M1.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 11500,
              vjust = -1,
              #show.legend = TRUE,
              color = "#3C5488B2") +
    scale_color_manual(values = c("#3C5488B2", "black"), name = "Workload Type") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 40) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    #ggtitle("Model M1") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 17),
          #legend.spacing.y = unit(5.0, 'cm'),
          legend.key.size = unit(1.5, "cm"))
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
  
  # result.M2 <- find.cheapest.instance.final(8, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 
  #                                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)
  
  workload.M1.M2 <- snowset.middle.cust.hourly
  workload.M1.M2$totalCpuTime <- as.numeric(list(6, 6, 7, 7, 6, 7, 8, 6.5, 7.5, 24, 32, 6.3, 
                                                 6.5, 6, 7, 6.5, 8, 8, 6.3, 6.7, 7.2, 6.5, 7.6, 6))
  #workload.M1.M2$group <- "Workload Demand"
  
  
  result.M2.base <- result.M2[1, ]
  result.M2.base <- result.M2.base[rep(seq_len(nrow(result.M2.base)), each = 24), ]
  result.M2.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.base$workload <- 8
  result.M2.base$Instance_config <- paste(result.M2.base$Num_Instances_Required, "x ", result.M2.base$Instance_Type, 
                                          " (", round(result.M2.base$Total_Costs, 4), "$/h)", sep = "")
  #result.M2.base$Workload_Type <- "Base Workload"
  

  result.M2.fluct <- result.M2[result.M2$Instance_Number != 0, ]
  result.M2.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  result.M2.fluct$Instance_config <- paste(result.M2.fluct$Num_Instances_Required, "x ", result.M2.fluct$Instance_Type, 
                                            "\n(", round(result.M2.fluct$Total_Costs, 4), "$/h)", sep = "")
  result.M2.fluct$Instance_config <- ifelse(result.M2.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", result.M2.fluct$Instance_config)
  result.M2.fluct$workload <- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", 16 + result.M2.base$workload, 24 + result.M2.base$workload)
  result.M2.fluct$group_id <- paste(result.M2.fluct$Num_Instances_Required, result.M2.fluct$Instance_Type, result.M2.fluct$workload)
  result.M2.fluct$group <- cumsum(result.M2.fluct$group_id != lag(result.M2.fluct$group_id, default = ""))
  duplicates <- duplicated(result.M2.fluct$group) | duplicated(result.M2.fluct$group, fromLast = TRUE)
  result.M2.fluct$angle <- ifelse(duplicates, 0, 90)
  result.M2.fluct <- result.M2.fluct[result.M2.fluct$Num_Instances_Required > 0, ]
  result.M2.fluct$nudge_x <- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", -4600, 0)
  #result.M2.fluct$Workload_Type <- ifelse(result.M2.fluct$Instance_Type == "c5.4xlarge", 
  #                                        "Fluctuating Workload 1", 
  #                                        "Fluctuating Workload 2")
  
  ggplot() +
    geom_line(data = workload.M1.M2, aes(x = hour_group, y = totalCpuTime,), color = "black", linetype = "dotted", size = 1.2) + 
    geom_line(data = result.M2.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.M2.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 11500,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = result.M2.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload),
                 color = c("#E64B35B2", "#7E6148B2"),
                 size = 1.2,
                 alpha = 0.8, 
                 #show.legend = FALSE
                 ) +
    geom_text(data = result.M2.fluct, aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = result.M2.fluct$nudge_x,
              vjust = -0.5,
              show.legend = FALSE,
              color = c("#E64B35B2", "#7E6148B2")) +
    scale_color_manual(values = c("#3C5488B2", "#E64B35B2", "#7E6148B2", "black"), name = "Workload Type") +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 40) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    #ggtitle("Model M2") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 17),
          #legend.spacing.y = unit(5.0, 'cm'),
          legend.key.size = unit(1.5, "cm"))
}
M2 <- generate.plot.M2()
ggsave("M2.pdf", plot = M2, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                  # Approach Plot 3: Cost-optimal with migration costs
## ------------------------------------------------------------------------------------------------------------ ##

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
  result.M3.fluct$nudge_x <- ifelse(result.M3.fluct$Instance_Type == "c5.4xlarge", -3600, 0)
  result.M3.fluct <- result.M3.fluct[result.M3.fluct$Num_Instances_Required > 0, ]
  
  ggplot() +
    geom_line(data = result.M3.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2", size = 1.2) +
    geom_text(data = subset(result.M3.base, !duplicated(Instance_config)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 6,
              nudge_x = 8700,
              vjust = -1,
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
              vjust = -0.5,
              show.legend = FALSE,
              color = c("#E64B35B2", "#7E6148B2")) +
    geom_line(data = workload.M3, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 40) + 
    xlab("Time of day") +
    ylab("CPU hours per hour") +
    #ggtitle("Model M3") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5))
}
M3 <- generate.plot.M3()
ggsave("M3.pdf", plot = M3, width = 14.7551263472)


# arrange.M1.M2.M3 <- function() {
#   gridExtra::grid.arrange(arrangeGrob(M1, M2, M3, ncol = 3, left = textGrob("CPU hours", rot = 90, vjust = 1)))
# }
# arrange.M1.M2.M3()


## ------------------------------------------------------------------------------------------------------------ ##
                                              # $ / CPUh plot for M1-M3
## ------------------------------------------------------------------------------------------------------------ ##

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

# workload
generate.plot.S1.workload <- function() {
  S1.workload <<- ggplot() +
    geom_line(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    xlab("Time of day") +
    ylab("CPU hours") +
    #ggtitle("Workload distribution VH-1") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5))
}
generate.plot.S1.workload()

ggsave("VH-1.pdf", plot = S1.workload, width = 14.7551263472)


# Base only
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
    #xlab("Time") +
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


# Fluct only
generate.plot.S1.option2 <- function() {
  
  snow.szenario.2.2.avg.base <- snow.szenario.2.2.avg[1, ]
  snow.szenario.2.2.avg.base <- snow.szenario.2.2.avg.base[rep(seq_len(nrow(snow.szenario.2.2.avg.base)), each = 24), ]
  snow.szenario.2.2.avg.base$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.2.2.avg.base$workload <- 0
  snow.szenario.2.2.avg.base$Instance_Type <- "No base workload required"
  snow.szenario.2.2.avg.base$Instance_config <- paste(snow.szenario.2.2.avg.base$Num_Instances_Required, "x ", snow.szenario.2.2.avg.base$Instance_Type, 
                                                      " (", round(snow.szenario.2.2.avg.base$Total_Costs, 4), "$/h)", sep = "")

  
  snow.szenario.2.2.avg.fluct <<- snow.szenario.2.2.avg[snow.szenario.2.2.avg$Instance_Number != 0, ]
  snow.szenario.2.2.avg.fluct$Instance_config <<- paste(snow.szenario.2.2.avg.fluct$Num_Instances_Required, "x ", snow.szenario.2.2.avg.fluct$Instance_Type, 
                                                    "\n(", round(snow.szenario.2.2.avg.fluct$Total_Costs, 4), "$/h)", sep = "")
  snow.szenario.2.2.avg.fluct$Instance_Number <<- snowset.large.cust.hourly$hour_group
  snow.szenario.2.2.avg.fluct$Instance_config <<- ifelse(snow.szenario.2.2.avg.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", snow.szenario.2.2.avg.fluct$Instance_config)
  snow.szenario.2.2.avg.fluct$workload <<- as.numeric(CPU.hours.fluctuating) + snow.szenario.2.2.avg.base$workload
  snow.szenario.2.2.avg.fluct$group_id <<- paste(snow.szenario.2.2.avg.fluct$Num_Instances_Required, snow.szenario.2.2.avg.fluct$Instance_Type, snow.szenario.2.2.avg.fluct$workload)
  snow.szenario.2.2.avg.fluct$group <<- cumsum(snow.szenario.2.2.avg.fluct$group_id != lag(snow.szenario.2.2.avg.fluct$group_id, default = ""))
  snow.szenario.2.2.avg.fluct$angle <<- ifelse(snow.szenario.2.2.avg.fluct$group == 14 |
                                                 snow.szenario.2.2.avg.fluct$group == 15 |
                                                 snow.szenario.2.2.avg.fluct$group == 16, 0, 90)
  
  snow.szenario.2.2.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.2.avg.fluct$group == 14, -1000, 0)
  snow.szenario.2.2.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.2.avg.fluct$group == 15, 3600, snow.szenario.2.2.avg.fluct$nudge_x)
  snow.szenario.2.2.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.2.avg.fluct$group == 16, 3600, snow.szenario.2.2.avg.fluct$nudge_x)
  snow.szenario.2.2.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.2.avg.fluct$group == 17, 1800, snow.szenario.2.2.avg.fluct$nudge_x)
  
  snow.szenario.2.2.avg.fluct$nudge_y <<- ifelse(snow.szenario.2.2.avg.fluct$workload < 75, -20, 20)
  snow.szenario.2.2.avg.fluct$nudge_y <<- ifelse(snow.szenario.2.2.avg.fluct$group == 2 |
                                                   snow.szenario.2.2.avg.fluct$group == 3 |
                                                   snow.szenario.2.2.avg.fluct$group == 11 |
                                                   snow.szenario.2.2.avg.fluct$group == 13, -20, snow.szenario.2.2.avg.fluct$nudge_y)
  snow.szenario.2.2.avg.fluct$nudge_y <<- ifelse(snow.szenario.2.2.avg.fluct$group == 14 |
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
                 #color = c("#E64B35B2", "#7E6148B2", "darkgoldenrod3"),
                 #size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = snow.szenario.2.2.avg.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 2,
              nudge_x = snow.szenario.2.2.avg.fluct$nudge_x,
              nudge_y = snow.szenario.2.2.avg.fluct$nudge_y,
              angle = snow.szenario.2.2.avg.fluct$angle,
              #vjust = -0.5,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#E64B35B2", "#7E6148B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    #xlab("Time") +
    ylab("CPU hours") +
    ggtitle("Scenario 2 - Fluctuating Workload Only") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          #axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
generate.plot.S1.option2()


# Cost-optimality
generate.plot.S1.option3 <- function() {
  
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg[1, ]
  snow.szenario.2.4.avg.base <- snow.szenario.2.4.avg.base[rep(seq_len(nrow(snow.szenario.2.4.avg.base)), each = 24), ]
  snow.szenario.2.4.avg.base$Instance_Number <- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.base$workload <- 96
  snow.szenario.2.4.avg.base$Instance_config <- paste(snow.szenario.2.4.avg.base$Num_Instances_Required, "x ", snow.szenario.2.4.avg.base$Instance_Type, 
                                                      " (", round(snow.szenario.2.4.avg.base$Total_Costs, 4), "$/h)", sep = "")
  
  
  snow.szenario.2.4.avg.fluct <<- snow.szenario.2.4.avg[snow.szenario.2.4.avg$Instance_Number != 0, ]
  snow.szenario.2.4.avg.fluct$Instance_config <<- paste(snow.szenario.2.4.avg.fluct$Num_Instances_Required, "x ", snow.szenario.2.4.avg.fluct$Instance_Type, 
                                                        "\n(", round(snow.szenario.2.4.avg.fluct$Total_Costs, 4), "$/h)", sep = "")
  snow.szenario.2.4.avg.fluct$Instance_Number <<- snowset.large.cust.hourly$hour_group
  snow.szenario.2.4.avg.fluct$Instance_config <<- ifelse(snow.szenario.2.4.avg.fluct$Instance_config == "0x NA (0$/h)", "No additional instance", snow.szenario.2.4.avg.fluct$Instance_config)
  snow.szenario.2.4.avg.fluct$workload <<- ifelse((as.numeric(snowset.large.cust.hourly$totalCpuTime) - snow.szenario.2.4.avg.base$workload) > 0, 
                                                  as.numeric(snowset.large.cust.hourly$totalCpuTime), snow.szenario.2.4.avg.base$workload)
  snow.szenario.2.4.avg.fluct$group_id <<- paste(snow.szenario.2.4.avg.fluct$Num_Instances_Required, snow.szenario.2.4.avg.fluct$Instance_Type, snow.szenario.2.4.avg.fluct$workload)
  snow.szenario.2.4.avg.fluct$group <<- cumsum(snow.szenario.2.4.avg.fluct$group_id != lag(snow.szenario.2.4.avg.fluct$group_id, default = ""))
  snow.szenario.2.4.avg.fluct$angle <<- ifelse(snow.szenario.2.4.avg.fluct$group == 13 |
                                                 snow.szenario.2.4.avg.fluct$group == 14 |
                                                 snow.szenario.2.4.avg.fluct$group == 15, 0, 90)
  
  snow.szenario.2.4.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.4.avg.fluct$group == 13, -1000, 0)
  snow.szenario.2.4.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.4.avg.fluct$group == 14, 3600, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.4.avg.fluct$group == 15, 3600, snow.szenario.2.4.avg.fluct$nudge_x)
  snow.szenario.2.4.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.4.avg.fluct$group == 16, 1800, snow.szenario.2.4.avg.fluct$nudge_x) 
  snow.szenario.2.4.avg.fluct$nudge_x <<- ifelse(snow.szenario.2.4.avg.fluct$group == 3, -1000, snow.szenario.2.4.avg.fluct$nudge_x) 
  
  snow.szenario.2.4.avg.fluct$nudge_y <<- ifelse(snow.szenario.2.4.avg.fluct$group == 2 |
                                                   snow.szenario.2.4.avg.fluct$group == 10 |
                                                   snow.szenario.2.4.avg.fluct$group == 12, -24, 22)
  snow.szenario.2.4.avg.fluct$nudge_y <<- ifelse(snow.szenario.2.4.avg.fluct$angle == 0, 10, snow.szenario.2.4.avg.fluct$nudge_y)
  
  snow.szenario.2.4.avg.fluct <- snow.szenario.2.4.avg.fluct[snow.szenario.2.4.avg.fluct$Num_Instances_Required > 0, ]
  
  
  ggplot() +
    geom_line(data = snowset.large.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black") +
    geom_line(data = snow.szenario.2.4.avg.base, aes(x = Instance_Number, y = workload), color = "#3C5488B2") +
    geom_text(data = subset(snow.szenario.2.4.avg.base, !duplicated(Instance_Type)),
              aes(x = Instance_Number, y = workload, label = Instance_config),
              size = 2,
              nudge_x = 48000,
              vjust = -1,
              show.legend = FALSE,
              color = "#3C5488B2") +
    geom_segment(data = snow.szenario.2.4.avg.fluct, 
                 aes(x = Instance_Number - 1800, y = workload, xend = Instance_Number + 1800, yend = workload, col = Instance_Type),
                 #color = c("#E64B35B2", "#7E6148B2", "darkgoldenrod3"),
                 #size = 1.2,
                 alpha = 0.8, 
                 show.legend = FALSE) +
    geom_text(data = snow.szenario.2.4.avg.fluct, aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type),
              size = 2,
              nudge_x = snow.szenario.2.4.avg.fluct$nudge_x,
              nudge_y = snow.szenario.2.4.avg.fluct$nudge_y,
              angle = snow.szenario.2.4.avg.fluct$angle,
              #vjust = -0.5,
              show.legend = FALSE) +
    scale_color_manual(values = c("#00A087B2", "#E64B35B2", "#7E6148B2", "#DC0000B2", "#4DBBD5B2", "darkgoldenrod3")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 250) + 
    #xlab("Time") +
    ylab("CPU hours") +
    ggtitle("Scenario 3 - Fluctuating Workload Only") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          #axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
generate.plot.S1.option3()


# Cost distribution all options VH-1
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
  
  # Optimum
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
  snow.szenario.2.4.avg.fluct$Approach <- "Cost-optimality"
  
  
  df <- rbind(snow.szenario.2.1.base, snow.szenario.2.2.avg.fluct, snow.szenario.2.4.avg.fluct)
  df$Approach <- factor(df$Approach, levels = c("Base workload only", "Fluctuating workload only", "Cost-optimality"))
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "darkgoldenrod3", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.08) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    #ggtitle("Hourly Costs per CPU hour for VH-1") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
          #legend.title = element_text(size = 20),
          #legend.text = element_text(size = 17),
          #legend.spacing.y = unit(5.0, 'cm'),
          #legend.key.size = unit(1.5, "cm")) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.078, label = "Base workload only", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.033, label = "Fluctuating workload only", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.02, label = "Cost-optimality", color = "#00A087B2", size = 5)
}
S1.costs <- generate.plot.S1.costs()
ggsave("VH-1-costs.pdf", plot = S1.costs, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                                  # Plots VH-2
## ------------------------------------------------------------------------------------------------------------ ##

generate.plot.S2.workload <- function() {
  ggplot() +
    geom_line(data = snowset.middle.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0.00, 80.00) + 
    xlab("Time of day") +
    ylab("CPU hours") +
    #ggtitle("Workload distribution VH-2") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5))
}
S2.workload <- generate.plot.S2.workload()
ggsave("VH-2.pdf", plot = S2.workload, width = 14.7551263472)


# Cost distribution all options VH-2
generate.plot.S2.costs <- function() {
  # Base only
  snow.szenario.3.1.base <<- snow.szenario.3.1[1, ]
  snow.szenario.3.1.base <<- snow.szenario.3.1.base[rep(seq_len(nrow(snow.szenario.3.1.base)), each = 24), ]
  snow.szenario.3.1.base$Instance_Number <<- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.1.base$workload <<- snowset.middle.cust.hourly$totalCpuTime
  snow.szenario.3.1.base$dollar_per_CPUh <<- snow.szenario.3.1.base$Total_Costs / snow.szenario.3.1.base$workload
  snow.szenario.3.1.base$Approach <<- "Base workload only"
  
  # Fluct only
  snow.szenario.3.2.avg.fluct <- snow.szenario.3.2.avg[snow.szenario.3.2.avg$Instance_Number != 0, ]
  snow.szenario.3.2.avg.fluct$Instance_Number <- snowset.middle.cust.hourly$hour_group
  snow.szenario.3.2.avg.fluct$workload <- snowset.middle.cust.hourly$totalCpuTime
  snow.szenario.3.2.avg.fluct$dollar_per_CPUh <- snow.szenario.3.2.avg.fluct$Total_Costs / snow.szenario.3.2.avg.fluct$workload
  snow.szenario.3.2.avg.fluct$Approach <- "Fluctuating workload only"
  
  # Optimum
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
  snow.szenario.3.4.avg.fluct$Approach <- "Cost-optimality"
  
  
  df <- rbind(snow.szenario.3.1.base, snow.szenario.3.2.avg.fluct, snow.szenario.3.4.avg.fluct)
  df$Approach <- factor(df$Approach, levels = c("Base workload only", "Fluctuating workload only", "Cost-optimality"))
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "darkgoldenrod3", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.2) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    #ggtitle("Hourly Costs per CPU hour for VH-2") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    #legend.title = element_text(size = 20),
    #legend.text = element_text(size = 17),
    #legend.spacing.y = unit(5.0, 'cm'),
    #legend.key.size = unit(1.5, "cm")) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 0.17, label = "Base workload only", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 0.06, label = "Fluctuating\nworkload only", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 0.005, label = "Cost-optimality", color = "#00A087B2", size = 5)
}
S2.costs <- generate.plot.S2.costs()
ggsave("VH-2-costs.pdf", plot = S2.costs, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                                    # Plots VH-3
## ------------------------------------------------------------------------------------------------------------ ##

generate.plot.S3.workload <- function() {
  ggplot() +
    geom_line(data = snowset.small.cust.hourly, aes(x = hour_group, y = totalCpuTime), linetype = "dotted", color = "black", size = 1.2) + 
    scale_x_datetime(date_labels = "%H:%M") +
    #ylim(0, 5) + 
    scale_y_continuous(labels = scales::label_number(accuracy = .01), 
                       limits = c(0, 5)) +
    xlab("Time of day") +
    ylab("CPU hours") +
    #ggtitle("Workload distribution VH-3") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5))
}
S3.workload <- generate.plot.S3.workload()
ggsave("VH-3.pdf", plot = S3.workload, width = 14.7551263472)


# Cost distribution all options VH-3
generate.plot.S3.costs <- function() {
  # Base only
  snow.szenario.4.1.base <<- snow.szenario.4.1[1, ]
  snow.szenario.4.1.base <<- snow.szenario.4.1.base[rep(seq_len(nrow(snow.szenario.4.1.base)), each = 24), ]
  snow.szenario.4.1.base$Instance_Number <<- snowset.small.cust.hourly$hour_group
  snow.szenario.4.1.base$workload <<- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.1.base$dollar_per_CPUh <<- snow.szenario.4.1.base$Total_Costs / snow.szenario.4.1.base$workload
  snow.szenario.4.1.base$Approach <<- "Base workload only"
  
  # Fluct only
  snow.szenario.4.2.avg.fluct <<- snow.szenario.4.2.avg[snow.szenario.4.2.avg$Instance_Number != 0, ]
  snow.szenario.4.2.avg.fluct$Instance_Number <<- snowset.small.cust.hourly$hour_group
  snow.szenario.4.2.avg.fluct$workload <<- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <<- snow.szenario.4.2.avg.fluct$Total_Costs / snow.szenario.4.2.avg.fluct$workload
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <<- ifelse(is.na(snow.szenario.4.2.avg.fluct$dollar_per_CPUh), 
                                                         0, snow.szenario.4.2.avg.fluct$dollar_per_CPUh)
  snow.szenario.4.2.avg.fluct$Approach <<- "Fluctuating workload only"
  
  # Optimum
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
  snow.szenario.4.4.avg.fluct$Approach <- "Cost-optimality"
  
  
  df <- rbind(snow.szenario.4.1.base, snow.szenario.4.2.avg.fluct, snow.szenario.4.4.avg.fluct)
  df$Approach <- factor(df$Approach, levels = c("Base workload only", "Fluctuating workload only", "Cost-optimality"))
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "darkgoldenrod3", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    ylim(0, 0.08) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    #ggtitle("Hourly Costs per CPU hour for VH-2") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    #legend.title = element_text(size = 20),
    #legend.text = element_text(size = 17),
    #legend.spacing.y = unit(5.0, 'cm'),
    #legend.key.size = unit(1.5, "cm")) +
    annotate("text", x = as.POSIXct("2018-02-22 22:00:00"), y = 0.07, label = "Base workload only", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 0.04, label = "Fluctuating\nworkload only", color = "darkgoldenrod3", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:00:00"), y = 0.005, label = "Cost-optimality", color = "#00A087B2", size = 5)
}
S3.costs <- generate.plot.S3.costs()
ggsave("VH-3-costs.pdf", plot = S3.costs, width = 14.7551263472)


# Comparison between fluct only approach on VH-3 with min and avg spot prices
generate.plot.costs.avg.min <- function() {
  
  # Fluct only avg
  snow.szenario.4.2.avg.fluct <<- snow.szenario.4.2.avg[snow.szenario.4.2.avg$Instance_Number != 0, ]
  snow.szenario.4.2.avg.fluct$Instance_Number <<- snowset.small.cust.hourly$hour_group
  snow.szenario.4.2.avg.fluct$workload <<- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <<- snow.szenario.4.2.avg.fluct$Total_Costs / snow.szenario.4.2.avg.fluct$workload
  snow.szenario.4.2.avg.fluct$dollar_per_CPUh <<- ifelse(is.na(snow.szenario.4.2.avg.fluct$dollar_per_CPUh), 
                                                         0, snow.szenario.4.2.avg.fluct$dollar_per_CPUh)
  snow.szenario.4.2.avg.fluct$Approach <<- "Average Spot Prices"
  
  # Fluct only min
  snow.szenario.4.2.min.fluct <<- snow.szenario.4.2.min[snow.szenario.4.2.min$Instance_Number != 0, ]
  snow.szenario.4.2.min.fluct$Instance_Number <<- snowset.small.cust.hourly$hour_group
  snow.szenario.4.2.min.fluct$workload <<- snowset.small.cust.hourly$totalCpuTime
  snow.szenario.4.2.min.fluct$dollar_per_CPUh <<- snow.szenario.4.2.min.fluct$Total_Costs / snow.szenario.4.2.min.fluct$workload
  snow.szenario.4.2.min.fluct$dollar_per_CPUh <<- ifelse(is.na(snow.szenario.4.2.min.fluct$dollar_per_CPUh), 
                                                         0, snow.szenario.4.2.min.fluct$dollar_per_CPUh)
  snow.szenario.4.2.min.fluct$Approach <<- "Minimum Spot Prices"

  
  df <- rbind(snow.szenario.4.2.avg.fluct, snow.szenario.4.2.min.fluct)
  
  ggplot(data = df, aes(x = Instance_Number, y = dollar_per_CPUh, color = Approach)) +
    geom_line(size = 1.2, show.legend = FALSE) +
    scale_color_manual(values = c("#E64B35B2", "#00A087B2")) +
    scale_x_datetime(date_labels = "%H:%M") +
    #ylim(0, 0.08) + 
    ylab("$ per CPU hour demand") +
    xlab("Time of day") +
    #ggtitle("Hourly Costs per CPU hour for VH-2") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5)) +
    #legend.title = element_text(size = 20),
    #legend.text = element_text(size = 17),
    #legend.spacing.y = unit(5.0, 'cm'),
    #legend.key.size = unit(1.5, "cm")) +
    annotate("text", x = as.POSIXct("2018-02-22 23:30:00"), y = 0.033, label = "Average\nSpot Prices", color = "#E64B35B2", size = 5) +
    annotate("text", x = as.POSIXct("2018-02-22 23:30:00"), y = 0.023, label = "Minimum\nSpot Prices", color = "#00A087B2", size = 5)
}
S3.avg.min <- generate.plot.costs.avg.min()
ggsave("VH-3-avg-min.pdf", plot = S3.avg.min, width = 14.7551263472)


## ------------------------------------------------------------------------------------------------------------ ##
                                                  # Growth Plot
## ------------------------------------------------------------------------------------------------------------ ##

generate.plot.growth <- function() {

}
generate.plot.growth()




## NOTES

# Insert coniguration and not only instance -> copy from above
# Insert total price

# gemeinsamer Graph mit Preisen $ pro CPU/h -> alle optionen


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

## Generate plot for szenario output
# CPU.hours.required <- 0
# 
# generate.plot.szenario1.2 <- function(df) {
#   
#   # get only base load
#   df.base <<- df[1, ]
#   df.base <<- df.base[rep(seq_len(nrow(df.base)), each = 24), ]
#   df.base$Instance_Number <<- 1:24
#   df.base$workload <<- as.numeric(CPU.hours.required)
#   
#   # get only fluct load
#   df.fluct <<- df[df$Instance_Number != 0, ]
#   df.fluct$Instance_config <<- paste(df.fluct$Num_Instances_Required, "x", df.fluct$Instance_Type)
#   df.fluct$Instance_config <<- ifelse(df.fluct$Instance_config == "0 x NA", "No additional instance", df.fluct$Instance_config)
#   df.fluct$workload <<- as.numeric(CPU.hours.fluctuating) + df.base$workload
#   df.fluct$group_id <<- paste(df.fluct$Num_Instances_Required, df.fluct$Instance_Type, df.fluct$workload)
#   df.fluct$group <<- cumsum(df.fluct$group_id != lag(df.fluct$group_id, default = ""))
#   duplicates <- duplicated(df.fluct$group) | duplicated(df.fluct$group, fromLast = TRUE)
#   df.fluct$angle <<- ifelse(duplicates, 0, 90)
#   
#   df.fluct.text <<- df.fluct
#   df.fluct.text <<- subset(df.fluct.text, !duplicated(group))
#   df.fluct.text$x.nudge <<- ifelse(df.fluct.text$angle == 0, 1, 0)
#   df.fluct.text$y.nudge <<- ifelse(df.fluct.text$angle == 0, 30, 70)
#   df.fluct.text$y.nudge <<- ifelse(df.fluct.text$Instance_config == "No additional instance", 100, df.fluct.text$y.nudge)
#   
#   ggplot(data = df.fluct) +
#     geom_line(data = df.base, aes(x = Instance_Number, y = workload), color = "azure4", size = 1.2) +
#     geom_text(data = subset(df.base, !duplicated(Instance_Type)),
#               aes(x = Instance_Number, y = workload, label = Instance_Type),
#               size = 2,
#               nudge_x = 11,
#               vjust = 2,
#               show.legend = FALSE,
#               color = "azure4") +
#     geom_line(aes(x = Instance_Number, y = workload)) +
#     geom_text(data = df.fluct.text,
#               aes(x = Instance_Number, y = workload, label = Instance_config, col = Instance_Type), 
#               size = 2.2, 
#               nudge_y = df.fluct.text$y.nudge, 
#               nudge_x = df.fluct.text$x.nudge,
#               angle = df.fluct.text$angle, 
#               show.legend = FALSE) +
#     geom_segment(aes(x = Instance_Number - 0.5, y = workload, xend = Instance_Number + 0.5, yend = workload, col = Instance_Type),
#                  size = 1.2,
#                  alpha = 0.8, 
#                  show.legend = FALSE) +
#     scale_color_manual(values = c("deeppink3", "azure4", "darkgoldenrod3", "deepskyblue3", "darkorchid3")) +
#     scale_x_continuous(breaks = seq(0, 25, 2)) +
#     ylim(0, 250) +
#     xlab("Hour") + 
#     ylab("Workload in CPU hours per hour") +
#     theme_bw()
# }
# generate.plot.szenario1.2(snow.szenario.2.2.avg)


