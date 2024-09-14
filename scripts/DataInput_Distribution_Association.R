library(readr)
library(haven)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(patchwork)
library(tidyr)
library(moments)
library(nortest)
library(purrr)
library(broom)
library(blandr)

source('01_Functions_Base.R')

## Distribution results
step.distribution.subtitle <- function(dat.step, folder){
  df = dat.step
  # Row:i, Column:j
  custom_theme <- theme_minimal(base_size = 15) + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 9),
      panel.grid.major = element_line(color = "grey90"),
      panel.border = element_rect(color = "black", fill = NA),
      #panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      axis.ticks = element_blank(),
      plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5))
  
  # Initializes the drawing list
  plot.all <- list()
  
  # Iterate through all step_algorithms, generating a graph for each location
  for (i in 1:length(step_algorithms)) {
    for (j in 1:length(step_algorithms)) {
      
      # Diagonal i = j: histogram
      if (i == j) {
        plot.all[[paste0('H', i, i)]] <- ggplot(df, aes(x = !!sym(step_algorithms[i]))) +
          geom_histogram(
            aes(y = ..count..), 
            bins = 15, 
            fill = "#3D72BE", 
            color = "white"
          ) + labs(
            title = paste0(step_algorithms[i]),
            x = "Step Counts",
            y = "Frequency",
            subtitle = "Histogram"
          ) + custom_theme
        next
      }
      
      # 上三角位置 i < j: 绘制 Bland-Altman 图
      if (i < j) {
        # Calculate Bland-Altman data
        ba_data <- blandr.statistics(df[[step_algorithms[i]]], 
                                     df[[step_algorithms[j]]])
        
        # Dataframe with differences and averages
        plot_data <- data.frame(
          mean = (df[[step_algorithms[i]]] + df[[step_algorithms[j]]]) / 2,
          difference = df[[step_algorithms[i]]] - df[[step_algorithms[j]]]
        )
        # Linear regression
        lm_fit <- lm(difference ~ mean, data = plot_data)
        lm_coef <- tidy(lm_fit)
        
        # Extract Coefficients
        intercept <- lm_coef$estimate[1]
        slope <- lm_coef$estimate[2]
        
        # Create Bland-Altman; size = 0.7
        p <- ggplot(plot_data, aes(x = mean, y = difference)) +
          geom_point(alpha = 0.3, color = 'black') +
          geom_hline(yintercept = ba_data$bias, color = "blue", linetype = "dashed") +
          geom_hline(yintercept = ba_data$upperLOA, color = "red", linetype = "dashed") +
          geom_hline(yintercept = ba_data$lowerLOA, color = "red", linetype = "dashed") +
          geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5),
                      color = "#E74C3C",
                      se = TRUE, span = 2, fill = '#E74C3C') +  
          labs(
            x = paste0("Mean of ", step_algorithms[i], " and ", step_algorithms[j]), 
            y = paste0("Difference (", step_algorithms[i], " - ", step_algorithms[j], ")"),
            title = paste0(step_algorithms[i], " ~ ", step_algorithms[j]),
            subtitle = "Bland-Altman plot"
          ) + custom_theme
        
        plot.all[[paste0('BA', i, j)]] <- p
        next
      }
      
      # Lower triangular position i>j: pairwise scatter plots
      if (i > j) {
        correlation <- cor(df[[step_algorithms[i]]], df[[step_algorithms[j]]])
        
        plot.all[[paste0('PP', i, j)]] <- ggplot(df, aes(
          x = !!sym(step_algorithms[i]), y = !!sym(step_algorithms[j])
        )) +
          geom_point(color = "#E74C3C", alpha = 0.3) +
          geom_smooth(
            method = "gam", formula = y ~ s(x, bs = "cs", k = 5), 
            color = "black", size = 1, se = TRUE, fill = "black"
          ) +
          custom_theme + 
          labs(
            title = paste0(step_algorithms[i], " ~ ", step_algorithms[j]),
            x = step_algorithms[i],
            y = step_algorithms[j],
            subtitle = "Pairwise Distributions"
          ) + 
          annotate("text", 
                   x = min(df[[step_algorithms[i]]]), 
                   y = Inf, 
                   label = paste0("r = ", round(correlation, 2)), 
                   hjust = 0, vjust = 2.0, size = 4, color = "black", fontface = "bold")
          # scale_y_continuous(limits = c(min(df[[step_algorithms[i]]], df[[step_algorithms[j]]]),
          #                               max(df[[step_algorithms[i]]], df[[step_algorithms[j]]]))) + 
          # scale_x_continuous(limits = c(min(df[[step_algorithms[i]]], df[[step_algorithms[j]]]),
          #                               max(df[[step_algorithms[i]]], df[[step_algorithms[j]]])))
        next
      }
    }
  }
  
  combined_plot <- wrap_plots(
    plot.all$H11, plot.all$BA12, plot.all$BA13, plot.all$BA14, plot.all$BA15,
    plot.all$PP21, plot.all$H22, plot.all$BA23, plot.all$BA24, plot.all$BA25,
    plot.all$PP31, plot.all$PP32, plot.all$H33, plot.all$BA34, plot.all$BA35,
    plot.all$PP41, plot.all$PP42, plot.all$PP43, plot.all$H44, plot.all$BA45,
    plot.all$PP51, plot.all$PP52, plot.all$PP53, plot.all$PP54, plot.all$H55,
    ncol = 5
  ) & 
    theme(legend.position = 'bottom')
  
  png(here(paste0('Results/', folder, '/03_Combined.subtitle.png')), 
      width = 12, height = 14, units = 'in', res = 300)
  print(combined_plot)
  dev.off()
}



step.distribution <- function(dat.step, folder){
  df = dat.step
  
  # Row:i, Column:j
  custom_theme <- theme_minimal(base_size = 15) + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 9),
      panel.grid.major = element_line(color = "grey90"),
      panel.border = element_rect(color = "black", fill = NA),
      #panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      axis.ticks = element_blank(),
      plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
      plot.margin = margin(6, 6, 6, 6))
  
  # Initializes the drawing list
  plot.all <- list()
  
  # Iterate through all step_algorithms, generating a graph for each location
  for (i in 1:length(step_algorithms)) {
    for (j in 1:length(step_algorithms)) {
      
      # Diagonal i = j: histogram
      if (i == j) {
        plot.all[[paste0('H', i, i)]] <- ggplot(df, aes(x = !!sym(step_algorithms[i]))) +
          geom_histogram(
            aes(y = ..count..), 
            bins = 15, 
            fill = "#3D72BE",  #skyblue
            color = "white"
          ) + labs(
            title = paste0(step_algorithms[i]),
            x = "Step Counts",
            y = "Frequency"
          ) + custom_theme
        next
      }
      
      # Upper triangular position i<j: Bland-Altman
      if (i < j) {
        # Calculate Bland-Altman data
        ba_data <- blandr.statistics(df[[step_algorithms[i]]], 
                                     df[[step_algorithms[j]]])
        
        # Dataframe with differences and averages
        plot_data <- data.frame(
          mean = (df[[step_algorithms[i]]] + df[[step_algorithms[j]]]) / 2,
          difference = df[[step_algorithms[i]]] - df[[step_algorithms[j]]]
        )
        # Linear regression
        lm_fit <- lm(difference ~ mean, data = plot_data)
        lm_coef <- tidy(lm_fit)
        
        # Extract Coefficients
        intercept <- lm_coef$estimate[1]
        slope <- lm_coef$estimate[2]
        
        # Create Bland-Altman
        p <- ggplot(plot_data, aes(x = mean, y = difference)) +
          geom_point(alpha = 0.3, color = 'black') +
          geom_hline(yintercept = ba_data$bias, color = "blue", linetype = "dashed") +
          geom_hline(yintercept = ba_data$upperLOA, color = "red", linetype = "dashed") +
          geom_hline(yintercept = ba_data$lowerLOA, color = "red", linetype = "dashed") +
          geom_smooth(method = "loess", color = "#E74C3C", se = TRUE, span = 2, fill = '#E74C3C') +  
          labs(
            x = paste0("Mean of ", step_algorithms[i], " and ", step_algorithms[j]), 
            y = paste0("Difference (", step_algorithms[i], " - ", step_algorithms[j], ")"),
            title = paste0(step_algorithms[i], " ~ ", step_algorithms[j])
          ) + custom_theme
        
        plot.all[[paste0('BA', i, j)]] <- p
        next
      }
      
      # Lower triangular position i>j: pairwise scatter plots
      if (i > j) {
        correlation <- cor(df[[step_algorithms[i]]], df[[step_algorithms[j]]])
        
        plot.all[[paste0('PP', i, j)]] <- ggplot(df, aes(
          x = !!sym(step_algorithms[i]), y = !!sym(step_algorithms[j])
        )) +
          geom_point(color = "#E74C3C", alpha = 0.3) +
          geom_smooth(
            method = "gam", formula = y ~ s(x, bs = "cs", k = 5), 
            color = "black", size = 1, se = TRUE, fill = "black"
          ) +
          custom_theme + 
          labs(
            title = paste0(step_algorithms[i], " ~ ", step_algorithms[j]),
            x = step_algorithms[i],
            y = step_algorithms[j]
          ) + annotate("text", 
                       x = min(df[[step_algorithms[i]]]), 
                       y = Inf, 
                       label = paste0("r = ", round(correlation, 2)), 
                       hjust = 0, vjust = 2.0, size = 4, color = "black", fontface = "bold") + 
        scale_y_continuous(limits = c(min(df[[step_algorithms[i]]], df[[step_algorithms[j]]]),
                                      max(df[[step_algorithms[i]]], df[[step_algorithms[j]]]))) + 
        scale_x_continuous(limits = c(min(df[[step_algorithms[i]]], df[[step_algorithms[j]]]),
                                      max(df[[step_algorithms[i]]], df[[step_algorithms[j]]])))
        next
      }
    }
  }
  
  combined_plot <- wrap_plots(
    plot.all$H11, plot.all$BA12, plot.all$BA13, plot.all$BA14, plot.all$BA15,
    plot.all$PP21, plot.all$H22, plot.all$BA23, plot.all$BA24, plot.all$BA25,
    plot.all$PP31, plot.all$PP32, plot.all$H33, plot.all$BA34, plot.all$BA35,
    plot.all$PP41, plot.all$PP42, plot.all$PP43, plot.all$H44, plot.all$BA45,
    plot.all$PP51, plot.all$PP52, plot.all$PP53, plot.all$PP54, plot.all$H55,
    ncol = 5
  ) & 
    theme(legend.position = 'bottom')
  
  png(paste0('Results/', folder, '/03_Combined.png'), 
      width = 12, height = 14, units = 'in', res = 300)
  print(combined_plot)
  dev.off()
}


colors <- c("#ED7720", "#3D72BE", "#E74C3C","#97470C","#4CAF50", "#F4D03F", "#95A5A6")



## Association results
association.result <- function(dt.all, folder){
  dt.all$Hypertension = ifelse(dt.all$Hypertension == 'Yes', 1,0)
  dt.all$Diabetes = ifelse(dt.all$Diabetes == 'Yes', 1,0)
  colnames(dt.all)[which(colnames(dt.all) == 'CES.D')] = 'CES_D'
  
  # locate covariates
  cov_all = colnames(dt.all)
  cov_demo = c(which(cov_all == 'Age'),
               which(cov_all == 'Sex'),
               which(cov_all == 'Race'),
               which(cov_all == 'Education'),
               which(cov_all == 'Center'))
  cov_life_physical = c(which(cov_all == 'Smoking'),
                        which(cov_all == 'Drinking'),
                        which(cov_all == 'APOE_4'))
  
  ## transform Factor
  dt.all$BMI = as.factor(dt.all$BMI)
  dt.all$APOE_4 = as.factor(dt.all$APOE_4)
  dt.all$cogstatus = as.factor(dt.all$cogstatus)
  dt.all$Smoking = as.factor(dt.all$Smoking)
  dt.all$Drinking = as.factor(dt.all$Drinking)
  dt.all$Diabetes = as.factor(dt.all$Diabetes)
  dt.all$Hypertension = as.factor(dt.all$Hypertension)
  dt.all$Race = as.factor(dt.all$Race)
  dt.all$Sex = as.factor(dt.all$Sex)
  dt.all$Education = as.factor(dt.all$Education)
  dt.all$HF = as.factor(dt.all$HF)
  dt.all$MI = as.factor(dt.all$MI)
  dt.all$Stroke = as.factor(dt.all$Stroke)
  dt.all$BMI = as.numeric(as.factor(dt.all$BMI))
  
  ## Scale steps
  # dt.all = dt.all; dt.all[,3:7] = scale(dt.all[, 3:7])
  dt.all
  
  ## Continuous outcomes
  out.list.continuous = c('TG', 'TC', 'FAT_mass', 'HDL_C', 'LDL_C', "BMI",
                          'SPPB', 'Frailty_B', 'CES_D', 'Overall') 

  ## 02.1 Continuous outcome
  ### Model fitting
  num.tmp = which(colnames(dt.all) %in% out.list.continuous)
  dt.all[, num.tmp] = scale(dt.all[,num.tmp]) # Z-transform Outcome variables
  
  continuous_M1_list <- list()
  continuous_M2_list <- list()
  continuous.M1.result = list()
  continuous.M2.result = list()
  
  for (out in out.list.continuous){
    column_names = c('Estimate', 'SE', 'T_value', 'P_value', 'Lower', 'Upper')
    matrix = matrix(NA * length(step_algorithms)*length(column_names), 
                    nrow = length(step_algorithms), 
                    ncol = length(column_names))
    df.M1 <- data.frame(matrix)
    colnames(df.M1) <- column_names
    rownames(df.M1) <- step_algorithms
    
    df.M2 <- df.M1
    
    for (step_var in step_algorithms){
      comb = paste0(c(out, '&', step_var), collapse = '')
      ## Model 1
      formula.M1 = as.formula(paste0(out,'~',step_var,'+',
                                     paste0(c(cov_all[c(cov_demo)]),
                                            collapse = '+')))
      continuous_M1_list[[comb]] = lm(formula.M1, dt.all)
      result.tmp.M1 = data.frame(summary(continuous_M1_list[[comb]])$coefficients)
      result.tmp.M1$Lower = confint(continuous_M1_list[[comb]])[,1]
      result.tmp.M1$Upper = confint(continuous_M1_list[[comb]])[,2]
      df.M1[step_var,] = result.tmp.M1[2,]
      
      ## Model 2
      formula.M2 = as.formula(paste0(out,'~',step_var,'+',
                                     paste0(c(cov_all[c(cov_demo, cov_life_physical)]),
                                            collapse = '+')))
      continuous_M2_list[[comb]] = lm(formula.M2, dt.all)
      result.tmp.M2 = data.frame(summary(continuous_M2_list[[comb]])$coefficients)
      result.tmp.M2$Lower = confint(continuous_M2_list[[comb]])[,1]
      result.tmp.M2$Upper = confint(continuous_M2_list[[comb]])[,2]
      df.M2[step_var,] = result.tmp.M2[2,]
    }
    continuous.M1.result[[out]] = df.M1
    continuous.M2.result[[out]] = df.M2
  }
  
  ### HeatMap-M1
  library(pheatmap)
  
  combined_dt <- do.call(rbind, continuous.M1.result)
  combined_dt$Label = row.names(combined_dt)
  
  combined_dt = combined_dt %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  ## Extract results (T-statistics, P-value) to draw HeatMap
  df_wide.T <- combined_dt %>%
    dplyr::select(algorithm, T_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = T_value)
  
  df_wide.P <- combined_dt %>%
    dplyr::select(algorithm, P_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = P_value)
  
  write.csv(df_wide.P, 
            paste0('Results/', folder, '/LR.M1.Pvalue.csv'))
  write.csv(df_wide.T, 
            paste0('Results/', folder, '/LR.M1.Tvalue.csv'))
  
  dat_t = read.csv(paste0('Results/', folder, '/LR.M1.Tvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_t) = dat_t$outcome; dat_t$outcome = NULL
  dat_p = read.csv(paste0('Results/', folder, '/LR.M1.Pvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_p) = dat_p$outcome; dat_p$outcome = NULL
  
  png(paste0('Results/', folder, '06_LR.M1(HeatMap).png'),
      height = 4, width = 3, units = 'in', res = 600)
  HeatMap_LR(dat_t, dat_p)
  dev.off()
  
  ### HeatMap-M2
  combined_dt <- do.call(rbind, continuous.M2.result)
  combined_dt$Label = row.names(combined_dt)
  
  combined_dt = combined_dt %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  ## Extract results (T-statistics, P-value) to draw HeatMap
  df_wide.T <- combined_dt %>%
    dplyr::select(algorithm, T_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = T_value)
  
  df_wide.P <- combined_dt %>%
    dplyr::select(algorithm, P_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = P_value)
  
  write.csv(df_wide.P, 
            paste0('Results/', folder, '/LR.M2.Pvalue.csv'))
  write.csv(df_wide.T, 
            paste0('Results/', folder, '/LR.M2.Tvalue.csv'))
  
  dat_t = read.csv(paste0('Results/', folder, '/LR.M2.Tvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_t) = dat_t$outcome; dat_t$outcome = NULL
  dat_p = read.csv(paste0('Results/', folder, '/LR.M2.Pvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_p) = dat_p$outcome; dat_p$outcome = NULL
  
  png(paste0('Results/', folder, '06_LR.M2(HeatMap).png'),
      height = 4, width = 3, units = 'in', res = 600)
  HeatMap_LR(dat_t, dat_p)
  dev.off()
  
  ### ForestPlot
  #### Define functions
  library(forestploter)
  library(grid)
  library(ggpubr)
  library(ggplotify)
  
  #### M1
  combined_dt.M1 <- do.call(rbind, continuous.M1.result)
  combined_dt.M1$Label = row.names(combined_dt.M1)
  
  combined_dt.M1 = combined_dt.M1 %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  p1 = forest.function(combined_dt = combined_dt.M1[1:25,], legend = FALSE)
  p2 = forest.function(combined_dt = combined_dt.M1[26:50,])  
  
  g1 = ggplotify::as.ggplot(p1) + theme(legend.position = "none")
  g2 = ggplotify::as.ggplot(p2)
  
  combined_plot = g1 + g2 + 
    plot_layout(widths = c(0.8,0.9), ncol = 2)
  
  png('Results/06_Association_results/06_LR.M1.Scaled.png', height = 4, width = 7.2, units = 'in', res = 600)
  print(combined_plot)
  dev.off()
  
  #### M2
  combined_dt.M2 <- do.call(rbind, continuous.M2.result)
  combined_dt.M2$Label = row.names(combined_dt.M2)
  
  combined_dt.M2 = combined_dt.M2 %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  p1 = forest.function(combined_dt = combined_dt.M2[1:25,], legend = FALSE)
  p2 = forest.function(combined_dt = combined_dt.M2[26:50,])  
  
  g1 = ggplotify::as.ggplot(p1) + theme(legend.position = "none")
  g2 = ggplotify::as.ggplot(p2)
  
  combined_plot = g1 + g2 + 
    plot_layout(widths = c(0.8,0.9), ncol = 2)
  
  png(paste0('Results/', folder, '06_LR.M2.Scaled.png'), height = 4, width = 7.2, units = 'in', res = 600)
  print(combined_plot)
  dev.off()
  
  ### Output Coefficients (M1+M2)
  ## Output Association Coefficients
  ## M1
  output.dt = combined_dt.M1 |>
    mutate(`Conf (95%CI)` = sprintf("%.2f (%.2f, %.2f)", Estimate, Lower, Upper),
           Pvalue = sprintf("%.3f", P_value)) |>
    dplyr::select(algorithm, outcome, `Conf (95%CI)`, Pvalue) |>
    pivot_wider(names_from = algorithm, values_from = c(`Conf (95%CI)`, Pvalue)) |>
    dplyr::select(outcome,
                  `Conf (95%CI)_ADEPT`, Pvalue_ADEPT,
                  `Conf (95%CI)_Oak`, Pvalue_Oak,
                  `Conf (95%CI)_SDT`, Pvalue_SDT,
                  `Conf (95%CI)_Stepcount`, Pvalue_Stepcount,
                  `Conf (95%CI)_Verisense`, Pvalue_Verisense)
  output.dt$outcome = mod_name(output.dt$outcome)
  
  writexl::write_xlsx(output.dt, paste0('Results/', folder, '06_LR.M1.Scaled.Coef.xlsx'))
  
  ## M2
  combined_dt <- do.call(rbind, continuous.M2.result)
  combined_dt$Label = row.names(combined_dt)
  combined_dt = combined_dt %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  output.dt = combined_dt |>
    mutate(`Conf (95%CI)` = sprintf("%.2f (%.2f, %.2f)", Estimate, Lower, Upper),
           Pvalue = sprintf("%.3f", P_value)) |>
    dplyr::select(algorithm, outcome, `Conf (95%CI)`, Pvalue) |>
    pivot_wider(names_from = algorithm, values_from = c(`Conf (95%CI)`, Pvalue)) |>
    dplyr::select(outcome,
                  `Conf (95%CI)_ADEPT`, Pvalue_ADEPT,
                  `Conf (95%CI)_Oak`, Pvalue_Oak,
                  `Conf (95%CI)_SDT`, Pvalue_SDT,
                  `Conf (95%CI)_Stepcount`, Pvalue_Stepcount,
                  `Conf (95%CI)_Verisense`, Pvalue_Verisense)
  output.dt$outcome = mod_name(output.dt$outcome)
  
  writexl::write_xlsx(output.dt, paste0('Results/', folder, '06_LR.M2.Scaled.Coef.xlsx'))
  
  ## 02.2 Binary outcome
  ### Model fitting
  out.list.binary = c('Hypertension', 'Diabetes', 'HF', 'MI', 'FALL')
  dt.all$FALL = as.factor(dt.all$FALL)
  
  num.tmp = which(colnames(dt.all) %in% out.list.binary)
  
  binary_M1_list <- list()
  binary_M2_list <- list()
  binary.M1.result = list()
  binary.M2.result = list()
  
  for (out in out.list.binary){
    column_names = c('OR', 'SE', 'T_value', 'P_value', 'Lower', 'Upper')
    matrix = matrix(NA * length(step_algorithms)*length(column_names), 
                    nrow = length(step_algorithms), 
                    ncol = length(column_names))
    df.M1 <- data.frame(matrix)
    colnames(df.M1) <- column_names
    rownames(df.M1) <- step_algorithms
    
    df.M2 <- df.M1
    
    for (step_var in step_algorithms){
      comb = paste0(c(out, '&', step_var), collapse = '')
      ## Model 1
      formula.M1 = as.formula(paste0(out,'~',step_var,'+',
                                     paste0(c(cov_all[c(cov_demo)]), # cov_life_physical
                                            collapse = '+')))
      binary_M1_list[[comb]] = glm(formula.M1, dt.all, 
                                   family = binomial)
      result.tmp.M1 = data.frame(summary(binary_M1_list[[comb]])$coefficients)
      result.tmp.M1$Lower = confint(binary_M1_list[[comb]])[,1]
      result.tmp.M1$Upper = confint(binary_M1_list[[comb]])[,2]
      result.tmp.M1$Estimate = exp(result.tmp.M1$Estimate)
      result.tmp.M1$Lower = exp(result.tmp.M1$Lower)
      result.tmp.M1$Upper = exp(result.tmp.M1$Upper)
      
      df.M1[step_var,] = result.tmp.M1[2,]
      
      ## Model 2
      formula.M2 = as.formula(paste0(out,'~',step_var,'+',
                                     paste0(c(cov_all[c(cov_demo, cov_life_physical)]),
                                            collapse = '+')))
      binary_M2_list[[comb]] = glm(formula.M2, dt.all, family = binomial)
      result.tmp.M2 = data.frame(summary(binary_M2_list[[comb]])$coefficients)
      result.tmp.M2$Lower = confint(binary_M2_list[[comb]])[,1]
      result.tmp.M2$Upper = confint(binary_M2_list[[comb]])[,2]
      result.tmp.M2$Estimate = exp(result.tmp.M2$Estimate)
      result.tmp.M2$Lower = exp(result.tmp.M2$Lower)
      result.tmp.M2$Upper = exp(result.tmp.M2$Upper)
      
      df.M2[step_var,] = result.tmp.M2[2,]
    }
    binary.M1.result[[out]] = df.M1
    binary.M2.result[[out]] = df.M2
  }
  
  ### HeatMap-M1
  library(pheatmap)
  
  combined_dt <- do.call(rbind, binary.M1.result)
  combined_dt$Label = row.names(combined_dt)
  
  combined_dt = combined_dt %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  ## Extract results (T-statistics, P-value) to draw HeatMap
  df_wide.T <- combined_dt %>%
    dplyr::select(algorithm, T_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = T_value)
  
  df_wide.P <- combined_dt %>%
    dplyr::select(algorithm, P_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = P_value)
  
  write.csv(df_wide.P, 
            paste0('Results/', folder, '/Log.M1.Pvalue.csv'))
  write.csv(df_wide.T, 
            paste0('Results/', folder, '/Log.M1.Tvalue.csv'))
  
  dat_t = read.csv(paste0('Results/', folder, '/Log.M1.Tvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_t) = dat_t$outcome; dat_t$outcome = NULL
  dat_p = read.csv(paste0('Results/', folder, '/Log.M1.Pvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_p) = dat_p$outcome; dat_p$outcome = NULL
  
  
  a <- HeatMap_LR(dat_t, dat_p)
  ggsave(
    filename = paste0('Results/', folder, '/06_Log.M1(HeatMap).png'), # 文件名
    plot = a,                # 要保存的图形
    height = 1.8,            # 图形高度
    width = 2.5,             # 图形宽度
    units = 'in',            # 单位（英寸）
    dpi = 600                # 分辨率
  )
  
  ### HeatMap-M2
  library(pheatmap)
  
  combined_dt <- do.call(rbind, binary.M2.result)
  combined_dt$Label = row.names(combined_dt)
  
  combined_dt = combined_dt %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  ## Extract results (T-statistics, P-value) to draw HeatMap
  df_wide.T <- combined_dt %>%
    dplyr::select(algorithm, T_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = T_value)
  
  df_wide.P <- combined_dt %>%
    dplyr::select(algorithm, P_value, outcome) |>
    pivot_wider(
      names_from = algorithm, # Use value in the algorithm column as the name of the new column
      values_from = P_value)
  
  write.csv(df_wide.P, 
            paste0('Results/', folder, '/Log.M2.Pvalue.csv'))
  write.csv(df_wide.T, 
            paste0('Results/', folder, '/Log.M2.Tvalue.csv'))
  
  dat_t = read.csv(paste0('Results/', folder, '/Log.M2.Tvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_t) = dat_t$outcome; dat_t$outcome = NULL
  dat_p = read.csv(paste0('Results/', folder, '/Log.M2.Pvalue.csv')) |> dplyr::select(-X)
  # row.names(dat_p) = dat_p$outcome; dat_p$outcome = NULL
  
  png(paste0('Results/', folder, '/06_Log.M2(HeatMap).png'), 
      height = 1.8, width = 2.5, units = 'in', res = 600)
  HeatMap_LR(dat_t, dat_p)
  dev.off()
  
  ### ForestPlot
  #### Define functions
  #### M1
  combined_dt.M1 <- do.call(rbind, binary.M1.result)
  combined_dt.M1$Label = row.names(combined_dt.M1)
  
  combined_dt.M1 = combined_dt.M1 %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  p1 = forest.function.binary(combined_dt = combined_dt.M1[1:25,])
  
  png(paste0('Results/', folder, '/06_Log.M1.Scaled.png'),
      height = 4, width = 4, units = 'in', res = 600)
  print(p1)
  dev.off()
  
  #### M2
  combined_dt.M2 <- do.call(rbind, binary.M2.result)
  combined_dt.M2$Label = row.names(combined_dt.M2)
  
  combined_dt.M2 = combined_dt.M2 %>%
    separate(Label, into = c("outcome", "algorithm"), sep = "\\.", remove = FALSE)
  
  p1 = forest.function.binary(combined_dt = combined_dt.M2[1:25,])
  
  png(paste0('Results/', folder, '/06_Log.M2.Scaled.png'),
      height = 4, width = 4, units = 'in', res = 600)
  print(p1)
  dev.off()
  
  ### Output Coefficients (M1+M2)
  ## Output Association Coefficients
  ## M1
  output.dt = combined_dt.M1 |>
    mutate(`Conf (95%CI)` = sprintf("%.2f (%.2f, %.2f)", OR, Lower, Upper),
           Pvalue = sprintf("%.3f", P_value)) |>
    dplyr::select(algorithm, outcome, `Conf (95%CI)`, Pvalue) |>
    pivot_wider(names_from = algorithm, values_from = c(`Conf (95%CI)`, Pvalue)) |>
    dplyr::select(outcome,
                  `Conf (95%CI)_ADEPT`, Pvalue_ADEPT,
                  `Conf (95%CI)_Oak`, Pvalue_Oak,
                  `Conf (95%CI)_SDT`, Pvalue_SDT,
                  `Conf (95%CI)_Stepcount`, Pvalue_Stepcount,
                  `Conf (95%CI)_Verisense`, Pvalue_Verisense)
  output.dt$outcome = mod_name(output.dt$outcome)
  
  writexl::write_xlsx(output.dt, paste0('Results/', folder, '/06_Log.M1.Scaled.Coef.xlsx'))
  
  ## M2
  output.dt = combined_dt.M2 |>
    mutate(`Conf (95%CI)` = sprintf("%.2f (%.2f, %.2f)", OR, Lower, Upper),
           Pvalue = sprintf("%.3f", P_value)) |>
    dplyr::select(algorithm, outcome, `Conf (95%CI)`, Pvalue) |>
    pivot_wider(names_from = algorithm, values_from = c(`Conf (95%CI)`, Pvalue)) |>
    dplyr::select(outcome,
                  `Conf (95%CI)_ADEPT`, Pvalue_ADEPT,
                  `Conf (95%CI)_Oak`, Pvalue_Oak,
                  `Conf (95%CI)_SDT`, Pvalue_SDT,
                  `Conf (95%CI)_Stepcount`, Pvalue_Stepcount,
                  `Conf (95%CI)_Verisense`, Pvalue_Verisense)
  output.dt$outcome = mod_name(output.dt$outcome)
  
  writexl::write_xlsx(output.dt, paste0('Results/', folder, '/06_Log.M2.Scaled.Coef.xlsx'))
}
