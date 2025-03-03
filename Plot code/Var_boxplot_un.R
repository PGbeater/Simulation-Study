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
plot_list2 <- vector("list",4)
names(plot_list2) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_var in 1:4) {
  load(v_data[9*i_var-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_var]][[1]] <- p1
  plot_list[[i_var]][[2]] <- p2
  plot_list[[i_var]][[3]] <- p3
  plot_list[[i_var]][[4]] <- p4
  
  # changing PT
  # store in a list
  df_list <- mget(paste0("df_25vs10vsgen", 1:9))
  
  # same IO, changing PT
  changingPT_plot_list <- vector("list",3)
  IO_name <- c("IO=0.5%","IO=5%","IO=10%")
  names(changingPT_plot_list) <- IO_name
  
  for (j_var in 1:3) {
    df__changingPT <-  data.frame(rbind(df_list[[j_var]],df_list[[j_var+3]],df_list[[j_var+2*3]]))
    df__changingPT$PT <- rep(c("PT5","PT20","PT50"),each = 60)
    df__changingPT <- df__changingPT %>%
      mutate(PT = factor(PT, levels = c("PT5", "PT20", "PT50")))
    df__changingPT_25 <- df__changingPT %>%
      filter(model %in% c("perc25"))
    df__changingPT_10 <- df__changingPT %>%
      filter(model %in% c("perc10"))
    df__changingPT_gen <- df__changingPT %>%
      filter(model %in% c("gen"))
    
    boxplot_changingPT_per25 <- ggplot(data = df__changingPT_25,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 25% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_per10 <- ggplot(data = df__changingPT_10,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 10% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_gen <- ggplot(data = df__changingPT_gen,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the Generic Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    changingPT_plot_list[[j_var]][[1]] <- boxplot_changingPT_per25
    changingPT_plot_list[[j_var]][[2]] <- boxplot_changingPT_per10
    changingPT_plot_list[[j_var]][[3]] <- boxplot_changingPT_gen
  }
  plot_list2[[i_var]] <- changingPT_plot_list
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

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/S/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)

# Boxplot when changing PT in different random seeds
p_com_25_0620 <- (plot_list2[[1]][[1]][[1]]+plot_list2[[1]][[2]][[1]]+plot_list2[[1]][[3]][[1]])/
  (plot_list2[[2]][[1]][[1]]+plot_list2[[2]][[2]][[1]]+plot_list2[[2]][[3]][[1]])

p_com_25_1217 <- (plot_list2[[3]][[1]][[1]]+plot_list2[[3]][[2]][[1]]+plot_list2[[3]][[3]][[1]])/
  (plot_list2[[4]][[1]][[1]]+plot_list2[[4]][[2]][[1]]+plot_list2[[4]][[3]][[1]])

p_com_10_0620 <- (plot_list2[[1]][[1]][[2]]+plot_list2[[1]][[2]][[2]]+plot_list2[[1]][[3]][[2]])/
  (plot_list2[[2]][[1]][[2]]+plot_list2[[2]][[2]][[2]]+plot_list2[[2]][[3]][[2]])

p_com_10_1217 <- (plot_list2[[3]][[1]][[2]]+plot_list2[[3]][[2]][[2]]+plot_list2[[3]][[3]][[2]])/
  (plot_list2[[4]][[1]][[2]]+plot_list2[[4]][[2]][[2]]+plot_list2[[4]][[3]][[2]])

p_com_gen_0620 <- (plot_list2[[1]][[1]][[3]]+plot_list2[[1]][[2]][[3]]+plot_list2[[1]][[3]][[3]])/
  (plot_list2[[2]][[1]][[3]]+plot_list2[[2]][[2]][[3]]+plot_list2[[2]][[3]][[3]])

p_com_gen_1217 <- (plot_list2[[3]][[1]][[3]]+plot_list2[[3]][[2]][[3]]+plot_list2[[3]][[3]][[3]])/
  (plot_list2[[4]][[1]][[3]]+plot_list2[[4]][[2]][[3]]+plot_list2[[4]][[3]][[3]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/S/per25_0620.png",plot = p_com_25_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/S/per10_0620.png",plot = p_com_10_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/S/gen_0620.png",plot = p_com_gen_0620,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/S/per25_1217.png",plot = p_com_25_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/S/per10_1217.png",plot = p_com_10_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/S/gen_1217.png",plot = p_com_gen_1217,width = 24,height = 12,dpi = 330)

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
plot_list2 <- vector("list",4)
names(plot_list2) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_var in 1:4) {
  load(v_data[9*i_var-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_var]][[1]] <- p1
  plot_list[[i_var]][[2]] <- p2
  plot_list[[i_var]][[3]] <- p3
  plot_list[[i_var]][[4]] <- p4
  
  # changing PT
  # store in a list
  df_list <- mget(paste0("df_25vs10vsgen", 1:9))
  
  # same IO, changing PT
  changingPT_plot_list <- vector("list",3)
  IO_name <- c("IO=0.5%","IO=5%","IO=10%")
  names(changingPT_plot_list) <- IO_name
  
  for (j_var in 1:3) {
    df__changingPT <-  data.frame(rbind(df_list[[j_var]],df_list[[j_var+3]],df_list[[j_var+2*3]]))
    df__changingPT$PT <- rep(c("PT5","PT20","PT50"),each = 60)
    df__changingPT <- df__changingPT %>%
      mutate(PT = factor(PT, levels = c("PT5", "PT20", "PT50")))
    df__changingPT_25 <- df__changingPT %>%
      filter(model %in% c("perc25"))
    df__changingPT_10 <- df__changingPT %>%
      filter(model %in% c("perc10"))
    df__changingPT_gen <- df__changingPT %>%
      filter(model %in% c("gen"))
    
    boxplot_changingPT_per25 <- ggplot(data = df__changingPT_25,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 25% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_per10 <- ggplot(data = df__changingPT_10,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 10% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_gen <- ggplot(data = df__changingPT_gen,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the Generic Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    changingPT_plot_list[[j_var]][[1]] <- boxplot_changingPT_per25
    changingPT_plot_list[[j_var]][[2]] <- boxplot_changingPT_per10
    changingPT_plot_list[[j_var]][[3]] <- boxplot_changingPT_gen
  }
  plot_list2[[i_var]] <- changingPT_plot_list
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

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/SC/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)

# Boxplot when changing PT in different random seeds
p_com_25_0620 <- (plot_list2[[1]][[1]][[1]]+plot_list2[[1]][[2]][[1]]+plot_list2[[1]][[3]][[1]])/
  (plot_list2[[2]][[1]][[1]]+plot_list2[[2]][[2]][[1]]+plot_list2[[2]][[3]][[1]])

p_com_25_1217 <- (plot_list2[[3]][[1]][[1]]+plot_list2[[3]][[2]][[1]]+plot_list2[[3]][[3]][[1]])/
  (plot_list2[[4]][[1]][[1]]+plot_list2[[4]][[2]][[1]]+plot_list2[[4]][[3]][[1]])

p_com_10_0620 <- (plot_list2[[1]][[1]][[2]]+plot_list2[[1]][[2]][[2]]+plot_list2[[1]][[3]][[2]])/
  (plot_list2[[2]][[1]][[2]]+plot_list2[[2]][[2]][[2]]+plot_list2[[2]][[3]][[2]])

p_com_10_1217 <- (plot_list2[[3]][[1]][[2]]+plot_list2[[3]][[2]][[2]]+plot_list2[[3]][[3]][[2]])/
  (plot_list2[[4]][[1]][[2]]+plot_list2[[4]][[2]][[2]]+plot_list2[[4]][[3]][[2]])

p_com_gen_0620 <- (plot_list2[[1]][[1]][[3]]+plot_list2[[1]][[2]][[3]]+plot_list2[[1]][[3]][[3]])/
  (plot_list2[[2]][[1]][[3]]+plot_list2[[2]][[2]][[3]]+plot_list2[[2]][[3]][[3]])

p_com_gen_1217 <- (plot_list2[[3]][[1]][[3]]+plot_list2[[3]][[2]][[3]]+plot_list2[[3]][[3]][[3]])/
  (plot_list2[[4]][[1]][[3]]+plot_list2[[4]][[2]][[3]]+plot_list2[[4]][[3]][[3]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/SC/per25_0620.png",plot = p_com_25_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/SC/per10_0620.png",plot = p_com_10_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/SC/gen_0620.png",plot = p_com_gen_0620,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/SC/per25_1217.png",plot = p_com_25_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/SC/per10_1217.png",plot = p_com_10_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/SC/gen_1217.png",plot = p_com_gen_1217,width = 24,height = 12,dpi = 330)

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
plot_list2 <- vector("list",4)
names(plot_list2) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_var in 1:4) {
  load(v_data[9*i_var-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_var]][[1]] <- p1
  plot_list[[i_var]][[2]] <- p2
  plot_list[[i_var]][[3]] <- p3
  plot_list[[i_var]][[4]] <- p4
  
  # changing PT
  # store in a list
  df_list <- mget(paste0("df_25vs10vsgen", 1:9))
  
  # same IO, changing PT
  changingPT_plot_list <- vector("list",3)
  IO_name <- c("IO=0.5%","IO=5%","IO=10%")
  names(changingPT_plot_list) <- IO_name
  
  for (j_var in 1:3) {
    df__changingPT <-  data.frame(rbind(df_list[[j_var]],df_list[[j_var+3]],df_list[[j_var+2*3]]))
    df__changingPT$PT <- rep(c("PT5","PT20","PT50"),each = 60)
    df__changingPT <- df__changingPT %>%
      mutate(PT = factor(PT, levels = c("PT5", "PT20", "PT50")))
    df__changingPT_25 <- df__changingPT %>%
      filter(model %in% c("perc25"))
    df__changingPT_10 <- df__changingPT %>%
      filter(model %in% c("perc10"))
    df__changingPT_gen <- df__changingPT %>%
      filter(model %in% c("gen"))
    
    boxplot_changingPT_per25 <- ggplot(data = df__changingPT_25,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 25% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_per10 <- ggplot(data = df__changingPT_10,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 10% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_gen <- ggplot(data = df__changingPT_gen,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the Generic Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    changingPT_plot_list[[j_var]][[1]] <- boxplot_changingPT_per25
    changingPT_plot_list[[j_var]][[2]] <- boxplot_changingPT_per10
    changingPT_plot_list[[j_var]][[3]] <- boxplot_changingPT_gen
  }
  plot_list2[[i_var]] <- changingPT_plot_list
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

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/C/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)

# Boxplot when changing PT in different random seeds
p_com_25_0620 <- (plot_list2[[1]][[1]][[1]]+plot_list2[[1]][[2]][[1]]+plot_list2[[1]][[3]][[1]])/
  (plot_list2[[2]][[1]][[1]]+plot_list2[[2]][[2]][[1]]+plot_list2[[2]][[3]][[1]])

p_com_25_1217 <- (plot_list2[[3]][[1]][[1]]+plot_list2[[3]][[2]][[1]]+plot_list2[[3]][[3]][[1]])/
  (plot_list2[[4]][[1]][[1]]+plot_list2[[4]][[2]][[1]]+plot_list2[[4]][[3]][[1]])

p_com_10_0620 <- (plot_list2[[1]][[1]][[2]]+plot_list2[[1]][[2]][[2]]+plot_list2[[1]][[3]][[2]])/
  (plot_list2[[2]][[1]][[2]]+plot_list2[[2]][[2]][[2]]+plot_list2[[2]][[3]][[2]])

p_com_10_1217 <- (plot_list2[[3]][[1]][[2]]+plot_list2[[3]][[2]][[2]]+plot_list2[[3]][[3]][[2]])/
  (plot_list2[[4]][[1]][[2]]+plot_list2[[4]][[2]][[2]]+plot_list2[[4]][[3]][[2]])

p_com_gen_0620 <- (plot_list2[[1]][[1]][[3]]+plot_list2[[1]][[2]][[3]]+plot_list2[[1]][[3]][[3]])/
  (plot_list2[[2]][[1]][[3]]+plot_list2[[2]][[2]][[3]]+plot_list2[[2]][[3]][[3]])

p_com_gen_1217 <- (plot_list2[[3]][[1]][[3]]+plot_list2[[3]][[2]][[3]]+plot_list2[[3]][[3]][[3]])/
  (plot_list2[[4]][[1]][[3]]+plot_list2[[4]][[2]][[3]]+plot_list2[[4]][[3]][[3]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/C/per25_0620.png",plot = p_com_25_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/C/per10_0620.png",plot = p_com_10_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/C/gen_0620.png",plot = p_com_gen_0620,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/C/per25_1217.png",plot = p_com_25_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/C/per10_1217.png",plot = p_com_10_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/C/gen_1217.png",plot = p_com_gen_1217,width = 24,height = 12,dpi = 330)

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
plot_list2 <- vector("list",4)
names(plot_list2) <- c("seed0620 TO5","seed0620 TO1.5","seed1217 TO5","seed1217 TO1.5")
for (i_var in 1:4) {
  load(v_data[9*i_var-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_var])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,3])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,3])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,3])
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 25% model vs Generic model in Variance in Different Scenarios") +
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
    labs(x = "Scenarios", y = "Variance", title = "The 10% model vs Generic model in Variance in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  plot_list[[i_var]][[1]] <- p1
  plot_list[[i_var]][[2]] <- p2
  plot_list[[i_var]][[3]] <- p3
  plot_list[[i_var]][[4]] <- p4
  
  # changing PT
  # store in a list
  df_list <- mget(paste0("df_25vs10vsgen", 1:9))
  
  # same IO, changing PT
  changingPT_plot_list <- vector("list",3)
  IO_name <- c("IO=0.5%","IO=5%","IO=10%")
  names(changingPT_plot_list) <- IO_name
  
  for (j_var in 1:3) {
    df__changingPT <-  data.frame(rbind(df_list[[j_var]],df_list[[j_var+3]],df_list[[j_var+2*3]]))
    df__changingPT$PT <- rep(c("PT5","PT20","PT50"),each = 60)
    df__changingPT <- df__changingPT %>%
      mutate(PT = factor(PT, levels = c("PT5", "PT20", "PT50")))
    df__changingPT_25 <- df__changingPT %>%
      filter(model %in% c("perc25"))
    df__changingPT_10 <- df__changingPT %>%
      filter(model %in% c("perc10"))
    df__changingPT_gen <- df__changingPT %>%
      filter(model %in% c("gen"))
    
    boxplot_changingPT_per25 <- ggplot(data = df__changingPT_25,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 25% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_per10 <- ggplot(data = df__changingPT_10,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the 10% Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    boxplot_changingPT_gen <- ggplot(data = df__changingPT_gen,aes(x = PT,y = value))+
      geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.6,outlier.size = 2,outlier.alpha = 0.5,size = 1)+
      labs(x = "Prevalence of Treatment", y = "Variance", title = paste("The Variance of the Generic Model: Changing Prevalence of Treatment when",IO_name[j_var])) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "black"), 
        plot.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
    
    changingPT_plot_list[[j_var]][[1]] <- boxplot_changingPT_per25
    changingPT_plot_list[[j_var]][[2]] <- boxplot_changingPT_per10
    changingPT_plot_list[[j_var]][[3]] <- boxplot_changingPT_gen
  }
  plot_list2[[i_var]] <- changingPT_plot_list
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

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/25vsgen_0620.png",plot = p_com_25vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/25vsgen_1217.png",plot = p_com_25vsgen_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/10vsgen_0620.png",plot = p_com_10vsgen_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/10vsgen_1217.png",plot = p_com_10vsgen_1217,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/25vsgen_IO5IO10_0620.png",plot = p_com_25vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/25vsgen_IO5IO10_1217.png",plot = p_com_25vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/10vsgen_IO5IO10_0620.png",plot = p_com_10vsgen_0620_IO5IO10,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot/un/Var/US/10vsgen_IO5IO10_1217.png",plot = p_com_10vsgen_1217_IO5IO10,width = 24,height = 12,dpi = 330)

# Boxplot when changing PT in different random seeds
p_com_25_0620 <- (plot_list2[[1]][[1]][[1]]+plot_list2[[1]][[2]][[1]]+plot_list2[[1]][[3]][[1]])/
  (plot_list2[[2]][[1]][[1]]+plot_list2[[2]][[2]][[1]]+plot_list2[[2]][[3]][[1]])

p_com_25_1217 <- (plot_list2[[3]][[1]][[1]]+plot_list2[[3]][[2]][[1]]+plot_list2[[3]][[3]][[1]])/
  (plot_list2[[4]][[1]][[1]]+plot_list2[[4]][[2]][[1]]+plot_list2[[4]][[3]][[1]])

p_com_10_0620 <- (plot_list2[[1]][[1]][[2]]+plot_list2[[1]][[2]][[2]]+plot_list2[[1]][[3]][[2]])/
  (plot_list2[[2]][[1]][[2]]+plot_list2[[2]][[2]][[2]]+plot_list2[[2]][[3]][[2]])

p_com_10_1217 <- (plot_list2[[3]][[1]][[2]]+plot_list2[[3]][[2]][[2]]+plot_list2[[3]][[3]][[2]])/
  (plot_list2[[4]][[1]][[2]]+plot_list2[[4]][[2]][[2]]+plot_list2[[4]][[3]][[2]])

p_com_gen_0620 <- (plot_list2[[1]][[1]][[3]]+plot_list2[[1]][[2]][[3]]+plot_list2[[1]][[3]][[3]])/
  (plot_list2[[2]][[1]][[3]]+plot_list2[[2]][[2]][[3]]+plot_list2[[2]][[3]][[3]])

p_com_gen_1217 <- (plot_list2[[3]][[1]][[3]]+plot_list2[[3]][[2]][[3]]+plot_list2[[3]][[3]][[3]])/
  (plot_list2[[4]][[1]][[3]]+plot_list2[[4]][[2]][[3]]+plot_list2[[4]][[3]][[3]])

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/US/per25_0620.png",plot = p_com_25_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/US/per10_0620.png",plot = p_com_10_0620,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/US/gen_0620.png",plot = p_com_gen_0620,width = 24,height = 12,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/US/per25_1217.png",plot = p_com_25_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/US/per10_1217.png",plot = p_com_10_1217,width = 24,height = 12,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Final_2025_Doc/Final Plot/Boxplot_changing_PT/un/Var/US/gen_1217.png",plot = p_com_gen_1217,width = 24,height = 12,dpi = 330)
