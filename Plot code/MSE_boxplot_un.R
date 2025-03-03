library(ggplot2)
library(tidyverse)
library(dplyr)
library(patchwork)

# Specific
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO10_S.RData")
plot_list <- vector("list",4)
names(plot_list) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_mse in 1:4) {
  load(v_data[9*i_mse-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_mse]][[1]] <- p1
  plot_list[[i_mse]][[2]] <- p2
  plot_list[[i_mse]][[3]] <- p3
  plot_list[[i_mse]][[4]] <- p4
}
# Boxplot for all scenarios in different random seeds
p_com_25vsgen_0620 <- (plot_list[[1]][[1]])/
  (plot_list[[2]][[1]])

p_com_25vsgen_1217 <- (plot_list[[3]][[1]])/
  (plot_list[[4]][[1]])

p_com_10vsgen_0620 <- (plot_list[[1]][[2]])/
  (plot_list[[2]][[2]])

p_com_10vsgen_1217 <- (plot_list[[3]][[2]])/
  (plot_list[[4]][[2]])

p_com_25vsgen_0620_IO5IO10 <- (plot_list[[1]][[3]])/
  (plot_list[[2]][[3]])

p_com_25vsgen_1217_IO5IO10 <- (plot_list[[3]][[3]])/
  (plot_list[[4]][[3]])

p_com_10vsgen_0620_IO5IO10 <- (plot_list[[1]][[4]])/
  (plot_list[[2]][[4]])

p_com_10vsgen_1217_IO5IO10 <- (plot_list[[3]][[4]])/
  (plot_list[[4]][[4]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/S/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)

# System clustered
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO10_SC.RData")
plot_list <- vector("list",4)
names(plot_list) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_mse in 1:4) {
  load(v_data[9*i_mse-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_mse]][[1]] <- p1
  plot_list[[i_mse]][[2]] <- p2
  plot_list[[i_mse]][[3]] <- p3
  plot_list[[i_mse]][[4]] <- p4
}
# Boxplot for all scenarios in different random seeds
p_com_25vsgen_0620 <- (plot_list[[1]][[1]])/
  (plot_list[[2]][[1]])

p_com_25vsgen_1217 <- (plot_list[[3]][[1]])/
  (plot_list[[4]][[1]])

p_com_10vsgen_0620 <- (plot_list[[1]][[2]])/
  (plot_list[[2]][[2]])

p_com_10vsgen_1217 <- (plot_list[[3]][[2]])/
  (plot_list[[4]][[2]])

p_com_25vsgen_0620_IO5IO10 <- (plot_list[[1]][[3]])/
  (plot_list[[2]][[3]])

p_com_25vsgen_1217_IO5IO10 <- (plot_list[[3]][[3]])/
  (plot_list[[4]][[3]])

p_com_10vsgen_0620_IO5IO10 <- (plot_list[[1]][[4]])/
  (plot_list[[2]][[4]])

p_com_10vsgen_1217_IO5IO10 <- (plot_list[[3]][[4]])/
  (plot_list[[4]][[4]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/SC/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)

# Center
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO10_C.RData")
plot_list <- vector("list",4)
names(plot_list) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_mse in 1:4) {
  load(v_data[9*i_mse-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_mse]][[1]] <- p1
  plot_list[[i_mse]][[2]] <- p2
  plot_list[[i_mse]][[3]] <- p3
  plot_list[[i_mse]][[4]] <- p4
}
# Boxplot for all scenarios in different random seeds
p_com_25vsgen_0620 <- (plot_list[[1]][[1]])/
  (plot_list[[2]][[1]])

p_com_25vsgen_1217 <- (plot_list[[3]][[1]])/
  (plot_list[[4]][[1]])

p_com_10vsgen_0620 <- (plot_list[[1]][[2]])/
  (plot_list[[2]][[2]])

p_com_10vsgen_1217 <- (plot_list[[3]][[2]])/
  (plot_list[[4]][[2]])

p_com_25vsgen_0620_IO5IO10 <- (plot_list[[1]][[3]])/
  (plot_list[[2]][[3]])

p_com_25vsgen_1217_IO5IO10 <- (plot_list[[3]][[3]])/
  (plot_list[[4]][[3]])

p_com_10vsgen_0620_IO5IO10 <- (plot_list[[1]][[4]])/
  (plot_list[[2]][[4]])

p_com_10vsgen_1217_IO5IO10 <- (plot_list[[3]][[4]])/
  (plot_list[[4]][[4]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/C/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)

# U-shape
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO5 PT50IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed0620 TO1.5 PT50IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO5 PT50IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_unmeasured/seed1217 TO1.5 PT50IO10_US.RData")
plot_list <- vector("list",4)
names(plot_list) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_mse in 1:4) {
  load(v_data[9*i_mse-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_mse])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.04)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.5,outlier.size = 2,outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, by = 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_mse]][[1]] <- p1
  plot_list[[i_mse]][[2]] <- p2
  plot_list[[i_mse]][[3]] <- p3
  plot_list[[i_mse]][[4]] <- p4
}
# Boxplot for all scenarios in different random seeds
p_com_25vsgen_0620 <- (plot_list[[1]][[1]])/
  (plot_list[[2]][[1]])

p_com_25vsgen_1217 <- (plot_list[[3]][[1]])/
  (plot_list[[4]][[1]])

p_com_10vsgen_0620 <- (plot_list[[1]][[2]])/
  (plot_list[[2]][[2]])

p_com_10vsgen_1217 <- (plot_list[[3]][[2]])/
  (plot_list[[4]][[2]])

p_com_25vsgen_0620_IO5IO10 <- (plot_list[[1]][[3]])/
  (plot_list[[2]][[3]])

p_com_25vsgen_1217_IO5IO10 <- (plot_list[[3]][[3]])/
  (plot_list[[4]][[3]])

p_com_10vsgen_0620_IO5IO10 <- (plot_list[[1]][[4]])/
  (plot_list[[2]][[4]])

p_com_10vsgen_1217_IO5IO10 <- (plot_list[[3]][[4]])/
  (plot_list[[4]][[4]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/MSE/US/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)