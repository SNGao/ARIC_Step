
sort_out_result <- function(result,
                            n_list = c(1:4)){
  result_by_cog = result |>
    arrange(Cognitive)
  result_by_PA = result |>
    arrange(PA)
  
  Detect_4 = result_by_PA |>
    group_by(PA) |>
    summarise(N = n()) |>
    filter(N==4)
  
  Detect_3 = result_by_PA |>
    group_by(PA) |>
    summarise(N = n()) |>
    filter(N==3)
  
  Detect_2 = result_by_PA |>
    group_by(PA) |>
    summarise(N = n()) |>
    filter(N==2)
  
  Detect_1 = result_by_PA |>
    group_by(PA) |>
    summarise(N = n()) |>
    filter(N==1)
  
  print(Detect_4); print(Detect_3); print(Detect_2); print(Detect_1)
}

custom_color_scale <- function(x) {
  ifelse(x == 0, "white", colorRampPalette(c("#4579B2", "#D0745F"))(length(x) - 1)[rank(x[-which(x == 0)])])
}



make_bold_names <- function(mat, rc_fun, rc_names) {
  bold_names <- rc_fun(mat)
  ids <- rc_names %>% match(rc_fun(mat))
  ids %>%
    walk(
      function(i)
        bold_names[i] <<-
        bquote(bold(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  bold_names
}

HeatMap_LR <- function(dat_est, dat_p,
                       show_colnames = TRUE,
                       show_rownames = TRUE,
                       annotation_legend.bool = TRUE,
                       annotation_col = NA,
                       legend = TRUE
                       ){
  
  rownames(dat_est) = dat_est[,1]; dat_est = dat_est[,-1]
  rownames(dat_est) = mod_name(rownames(dat_est))
  rownames(dat_p) = dat_p[,1]; dat_p = dat_p[,-1]
  
  ## 生成显著性列表
  if (!is.null(dat_p)){
    sssmt <- dat_p < 0.001
    dat_p[sssmt] <-'***'
    ssmt <- dat_p >0.001& dat_p <0.01
    dat_p[ssmt] <- '**'
    smt <- dat_p >0.01& dat_p <0.05
    dat_p[smt] <- '*'
    dat_p[!ssmt&!smt&!sssmt]<- ''
  } else {
    dat_p <- F
  }
  
  min_value <- min(dat_est)
  max_value <- max(dat_est)
  equal_range_value <- max(abs(min_value), abs(max_value))
  breaks <- seq(-equal_range_value, equal_range_value, length.out = 100)
  
  # annotation_row <- data.frame(
  #   Category = case_when(grepl('ASTP', row.names(dat_est)) ~ 'Fragmentation',
  #                        grepl('SATP', row.names(dat_est)) ~ 'Fragmentation',
  #                        grepl('IS', row.names(dat_est)) ~ 'Variability',
  #                        grepl('IV', row.names(dat_est)) ~ 'Variability',
  #                        grepl('RMSSD', row.names(dat_est)) ~ 'Variability',
  #                        grepl('TAC', row.names(dat_est)) ~ 'Volume',
  #                        grepl('AT', row.names(dat_est)) ~ 'Volume',
  #                        grepl('ST', row.names(dat_est)) ~ 'Volume',
  #                        grepl('Max', row.names(dat_est)) ~ 'Intensity')
  # )
  # rownames(annotation_row) = row.names(dat_est)
    p1 = pheatmap(dat_est,
                  cluster_cols = FALSE,
                  cluster_rows = FALSE,
                  cellwidth = 20,
                  cellheight = 12,
                  border_color = NA,
                  angle_col = 45,
                  fontsize_row = 8,
                  fontsize_col = 8, # 分别设置行列标签字体大小
                  display_numbers = dat_p,
                  show_rownames = show_rownames,
                  show_colnames = show_colnames,
                  # annotation_row = annotation_row,
                  annotation_names_row = FALSE,
                  annotation_legend = annotation_legend.bool,
                  # annotation_row_fontsize = 3,
                  # annotation_col_fontsize = 3,
                  legend = legend,
                  breaks = seq(-8, 8, length.out = 101),
                  labels_col = make_bold_names(dat_est, colnames, colnames(dat_est)),
                  #breaks = breaks,
                  color = colorRampPalette(c("#4579B2", "white", "#D0745F"))(100)
    )
  return(p1)
}

## Modify name format of data frame
mod_name <- function(name_lists){
  name_lists = gsub('FAT_mass', 'Fat mass', name_lists)
  name_lists = gsub('HDL_C', 'HDL-C', name_lists)
  name_lists = sub(' B', '', gsub('Frailty_B', 'Frailty', name_lists))
  name_lists = gsub('CES_D', 'Depression', name_lists)
  name_lists = gsub('Executive_func', 'Executive function', name_lists)
  name_lists = gsub('Overall', 'Overall cognition', name_lists)
  name_lists = gsub('LDL_C', 'LDL-C', name_lists)
  name_lists = gsub('stroke', 'Stroke', name_lists)
  name_lists = gsub('STroke', 'Stroke', name_lists)
  name_lists = gsub('FALL', 'Fall', name_lists)
  name_lists = gsub('TG', 'Triglycerides', name_lists)
  name_lists = gsub('TC', 'Total cholesterol', name_lists)
  name_lists = gsub('BMI_con', 'BMI', name_lists)
  name_lists = gsub('HF', 'Heart failure', name_lists)
  name_lists = gsub('\\bMI\\b', 'Myocardial Infarction', name_lists)
  name_lists = gsub('Cpm', 'CPM', name_lists)
  name_lists = gsub('0to6only', '(0-6)', name_lists)
  name_lists = gsub('12to18only', '(12-18)', name_lists)
  name_lists = gsub('18to24only', '(18-24)', name_lists)
  name_lists = gsub('6to12only', '(6-12)', name_lists)
  name_lists = gsub('23to5removed', '(rm:23-5)', name_lists)
  name_lists = gsub('Inbedremoved', '(rm:InBed)', name_lists)
  name_lists = gsub('23to5remove', '(rm:23-5)', name_lists)
  name_lists = gsub('Inbedremove', '(rm:InBed)', name_lists)
  name_lists = gsub('Max', 'Max ', name_lists)
  
  return(name_lists)
}


forest.function <- function(combined_dt, legend = TRUE){
  combined_dt$outcome = mod_name(combined_dt$outcome)
  dat.forest = combined_dt |>
    dplyr::select(outcome, algorithm, Estimate, P_value, Lower, Upper) |>
    dplyr::mutate(outcome = paste0(' ', outcome))
  
  dat.forest <- dat.forest %>%
    pivot_wider(
      names_from = algorithm,
      values_from = c(Estimate, P_value, Lower, Upper))
  
  dat.forest$plot <- paste(rep(" ", 25), collapse = " ")
  dat.forest$ci1 <- paste(sprintf("%.2f (%.2f, %.2f)", dat.forest$Estimate_ADEPT, 
                                  dat.forest$Lower_ADEPT, dat.forest$Upper_ADEPT),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$Estimate_Oak,
                                  dat.forest$Lower_Oak, dat.forest$Upper_Oak),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$Estimate_SDT,
                                  dat.forest$Lower_SDT, dat.forest$Upper_SDT),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$Estimate_Stepcount,
                                  dat.forest$Lower_Stepcount, dat.forest$Upper_Stepcount),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$Estimate_Verisense,
                                  dat.forest$Lower_Verisense, dat.forest$Upper_Verisense),
                          sep = "\n")
  # dat.forest = dat.forest |>
  #   mutate(`Coef (95%CI)` = sprintf("%.2f (%.2f, %.2f)", Estimate, Lower, Upper))
  
  ## Drawing
  tm <- forest_theme(base_size = 7,
                     refline_lty = "solid",
                     ci_lwd = 1.2,
                     ci_pch = c(15, 15, 15, 15, 15),
                     ci_col = c("#3D72BE", "#E74C3C","#97470C","#4CAF50", "#FFC107"),
                     footnote_col = "blue",
                     # footnote_cex = 1, # reference line width
                     legend_name = " ", #"Step Algorithm",
                     legend_value = case_when(legend == TRUE ~ step_algorithms,
                                              legend == FALSE ~ rep("", 5)),
                     vertline_lty = c("dashed", "dotted"),
                     vertline_col = c("#d6604d", "#bababa"),
                     # Table cell padding, width 4 and heights 3
                     core = list(padding = unit(c(2, 0.5), "mm")
                     ))
  
  # modify Column Name
  colnames(dat.forest)[which(colnames(dat.forest) == 'outcome')] = 'Outcome'
  colnames(dat.forest)[which(colnames(dat.forest) == 'plot')] = ' '
  colnames(dat.forest)[which(colnames(dat.forest) == 'ci1')] = 'Coef (95%CI)'
  
  p <- forest(dat.forest[,c(1, 22, 23)],
              est = list(dat.forest$Estimate_ADEPT,
                         dat.forest$Estimate_Oak,
                         dat.forest$Estimate_SDT,
                         dat.forest$Estimate_Stepcount,
                         dat.forest$Estimate_Verisense
              ),
              lower = list(dat.forest$Lower_ADEPT,
                           dat.forest$Lower_Oak,
                           dat.forest$Lower_SDT,
                           dat.forest$Lower_Stepcount,
                           dat.forest$Lower_Verisense
              ), 
              upper = list(dat.forest$Upper_ADEPT,
                           dat.forest$Upper_Oak,
                           dat.forest$Upper_SDT,
                           dat.forest$Upper_Stepcount,
                           dat.forest$Upper_Verisense),
              ci_column = c(2),
              sizes = 0.4,
              ref_line = 0,
              vert_line = c(0.5, 2),
              nudge_y = 0.15,
              xlim = c(-0.35, 0.35),
              ticks_at = c(-0.2, 0, 0.2),
              theme = tm) |>
    edit_plot(row = c(1:5),
              col = 1,
              gp = gpar(fontface = "bold", fontsize = 7)) |>
    edit_plot(row = c(1:5),
              col = 3,
              gp = gpar(fontsize = 6)) |>
    add_border(part = "header", where = c("bottom")) |>
    add_border(part = "header", where = c("top"))
  
  return(p)
}

forest.function.binary <- function(combined_dt, legend = TRUE){
  combined_dt$outcome = mod_name(combined_dt$outcome)
  dat.forest = combined_dt |>
    dplyr::select(outcome, algorithm, OR, P_value, Lower, Upper) |>
    dplyr::mutate(outcome = paste0(' ', outcome))
  
  dat.forest <- dat.forest %>%
    pivot_wider(
      names_from = algorithm,
      values_from = c(OR, P_value, Lower, Upper))
  
  dat.forest$plot <- paste(rep(" ", 25), collapse = " ")
  dat.forest$ci1 <- paste(sprintf("%.2f (%.2f, %.2f)", dat.forest$OR_ADEPT, 
                                  dat.forest$Lower_ADEPT, dat.forest$Upper_ADEPT),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$OR_Oak,
                                  dat.forest$Lower_Oak, dat.forest$Upper_Oak),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$OR_SDT,
                                  dat.forest$Lower_SDT, dat.forest$Upper_SDT),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$OR_Stepcount,
                                  dat.forest$Lower_Stepcount, dat.forest$Upper_Stepcount),
                          sprintf("%.2f (%.2f, %.2f)", dat.forest$OR_Verisense,
                                  dat.forest$Lower_Verisense, dat.forest$Upper_Verisense),
                          sep = "\n")
  # dat.forest = dat.forest |>
  #   mutate(`Coef (95%CI)` = sprintf("%.2f (%.2f, %.2f)", Estimate, Lower, Upper))
  
  ## Drawing
  tm <- forest_theme(base_size = 7,
                     refline_lty = "solid",
                     ci_lwd = 1.2,
                     ci_pch = c(15, 15, 15, 15, 15),
                     ci_col = c("#3D72BE", "#E74C3C","#97470C","#4CAF50", "#FFC107"),
                     footnote_col = "blue",
                     # footnote_cex = 1, # reference line width
                     legend_name = " ", #"Step Algorithm",
                     legend_value = case_when(legend == TRUE ~ step_algorithms,
                                              legend == FALSE ~ rep("", 5)),
                     vertline_lty = c("dashed", "dotted"),
                     vertline_col = c("#d6604d", "#bababa"),
                     # Table cell padding, width 4 and heights 3
                     core = list(padding = unit(c(2, 0.5), "mm")
                     ))
  
  # modify Column Name
  colnames(dat.forest)[which(colnames(dat.forest) == 'outcome')] = 'Outcome'
  colnames(dat.forest)[which(colnames(dat.forest) == 'plot')] = ' '
  colnames(dat.forest)[which(colnames(dat.forest) == 'ci1')] = 'OR (95%CI)'
  
  p <- forest(dat.forest[,c(1, 22, 23)],
              est = list(dat.forest$OR_ADEPT,
                         dat.forest$OR_Oak,
                         dat.forest$OR_SDT,
                         dat.forest$OR_Stepcount,
                         dat.forest$OR_Verisense
              ),
              lower = list(dat.forest$Lower_ADEPT,
                           dat.forest$Lower_Oak,
                           dat.forest$Lower_SDT,
                           dat.forest$Lower_Stepcount,
                           dat.forest$Lower_Verisense
              ), 
              upper = list(dat.forest$Upper_ADEPT,
                           dat.forest$Upper_Oak,
                           dat.forest$Upper_SDT,
                           dat.forest$Upper_Stepcount,
                           dat.forest$Upper_Verisense),
              ci_column = c(2),
              sizes = 0.4,
              x_trans = 'log',
              ref_line = 1,
              # vert_line = c(0.5, 2),
              nudge_y = 0.15,
              xlim = c(0.2, 1.3),
              ticks_at = c(0.2, 1, 1.2),
              theme = tm) |>
    edit_plot(row = c(1:5),
              col = 1,
              gp = gpar(fontface = "bold", fontsize = 7)) |>
    edit_plot(row = c(1:5),
              col = 3,
              gp = gpar(fontsize = 6)) |>
    add_border(part = "header", where = c("bottom")) |>
    add_border(part = "header", where = c("top"))
  
  return(p)
}

custom_smooth <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_smooth(method = "lm", color = "blue", ...) + # Set the line color to blue
    labs(title = paste0(mapping$y, " vs ", mapping$x)) + # Add title based on variable names
    theme(plot.title = element_text(size = 10, hjust = 0.5)) # Customize title appearance
}

get_limits <- function(data) {
  min_val <- min(data, na.rm = TRUE)
  max_val <- max(data, na.rm = TRUE)
  return(c(min_val, max_val))
}

cor_fun <- function(data, mapping, color = I("black"), size = 3, ...) {
  ggally_cor(data, mapping, color = color, size = size, ...) +
    theme(
      text = element_text(face = "bold")
    )
}
