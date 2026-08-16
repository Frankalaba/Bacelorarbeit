##############################
# Plotting of calculated Emission asa a function


rm(list = ls())

#install.packages("ggnewscale")


library("rio")
library("here")
library("readxl")
library("tidyr")
library("dplyr")
library("ggplot2")
library("viridis")
library("janitor")
library("purrr") # für map Befehl
library("tidyverse")
library("ggpmisc")
library("patchwork")
library("ggtext")
library("ggnewscale")
library("scales") # maskiert col_factor readr; discard von purrr; viridis_pal von viridis 
#-> muss wenn ich die aus den anderen Pakten haben will direkt ansprechen mit viridis::vidis_pal

#source(file = here("03_Analyses/Ym_und_DE_verändern_Skripte", "Function_Analyses_methane_reduction_simple.r"))

here()

## globale einstellung für alle folgenden plots -> alle textfelder markdown und stil bw

theme_set(
  theme_bw())   # oder theme_minimal(), was du sonst nutzt, als Basis
    



Visual_total_CH4_YB_30_function <- function(df, hs, col_org , Red, lm, br_lm, br_by, CC){
  
  ConfInt_test_final_pred_table <- df
  
  CH4_texst_USA_trainings_data <- hs
  
  pull_org <- ifelse(col_org == "total_CH4_org_calc_in_kt", "total_CH4_org_calc_in_kt", "EnterFe_CH4_emis_org_in_kt")
  pull_upd <- ifelse(col_org == "total_CH4_org_calc_in_kt", "total_CH4_upd_Ym_DE_in_kt", "EnterFe_CH4_emis_upd_in_kt_Ym_DE")
 
  hs_CH4 <- ifelse(col_org == "total_CH4_org_calc_in_kt", "total_CH4_pred_in_kt",  "Cat_EnterFer_pred_in_kt" )
  
  XYZ <- ifelse(col_org == "total_CH4_org_calc_in_kt", "Total CH<sub>4</sub>", "Cattle EnterFe emission")
  
  
  limits_for_CONFin <- tibble(
    Year = ConfInt_test_final_pred_table |>  filter(status == "fit") |> pull(Year),
    Red_in_perc = ConfInt_test_final_pred_table |>  filter(status == "fit") |> pull(Red_in_perc),
    time_sample = ConfInt_test_final_pred_table |>  filter(status == "fit") |> pull(time_sample),
    lwr_total_CH4_org = ConfInt_test_final_pred_table |> filter(status == "lwr") |> pull(.data[[pull_org]]),
    upr_total_CH4_org = ConfInt_test_final_pred_table |> filter(status == "upr") |> pull(.data[[pull_org]]),
    lwr_total_CH4_upd = ConfInt_test_final_pred_table |> filter(status == "lwr") |> pull(.data[[pull_upd]]),
    upr_total_CH4_upd = ConfInt_test_final_pred_table |> filter(status == "upr") |> pull(.data[[pull_upd]]),
    
  )
  
  names(limits_for_CONFin)
  
 Plot_list <- ggplot(ConfInt_test_final_pred_table |>  filter(status == "fit" & Red_in_perc == Red), 
         aes(x = Year, shape = factor(time_sample))) +
    geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == Red & time_sample == 5), 
                aes(x = Year, ymin = lwr_total_CH4_org/1000, ymax = upr_total_CH4_org/1000), 
                fill = "#32648EFF", alpha = 0.175, inherit.aes = FALSE) +
    geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == Red & time_sample == 10), 
                aes(x = Year, ymin = lwr_total_CH4_org/1000, ymax = upr_total_CH4_org/1000), 
                fill = "#32648EFF", alpha = 0.175, inherit.aes = FALSE) +
    geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == Red & time_sample == 15), 
                aes(x = Year, ymin = lwr_total_CH4_org/1000, ymax = upr_total_CH4_org/1000), 
                fill = "#32648EFF", alpha = 0.175, inherit.aes = FALSE) +
    geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == Red & time_sample == 5), 
                aes(x = Year, ymin = lwr_total_CH4_upd/1000, ymax = upr_total_CH4_upd/1000), 
                fill = "#1F968BFF", alpha = 0.175, inherit.aes = FALSE) +
    geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == Red & time_sample == 10), 
                aes(x = Year, ymin = lwr_total_CH4_upd/1000, ymax = upr_total_CH4_upd/1000), 
                fill = "#1F968BFF", alpha = 0.175, inherit.aes = FALSE) +
    geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == Red & time_sample == 15), 
                aes(x = Year, ymin = lwr_total_CH4_upd/1000, ymax = upr_total_CH4_upd/1000), 
                fill = "#1F968BFF", alpha = 0.175, inherit.aes = FALSE) +
    geom_point(aes(y = .data[[pull_org]]/1000, colour = "Prediction without immunization"),
               size = 2.5) +
    geom_point(aes(y = .data[[pull_upd]]/1000, colour = "Prediction with immunization"),
               size = 2.5) +
    geom_point(data = CH4_texst_USA_trainings_data, 
               aes(x = Year, y = .data[[hs_CH4]]/1000, colour = "Historical"), 
               shape = 19,
               size = 2.5, inherit.aes = FALSE) +
    scale_color_manual(name = "Methane Emission",
                       values = c(
                         "Prediction without immunization" = "#32648EFF",
                         "Prediction with immunization" = "#1F968BFF",
                         "Historical" = "#440154FF" 
                       )) + 
    new_scale_colour() + # ermöglicht nochmal legende über colour, die dann aber neue überschrift bekommen kann
    geom_point(data = ConfInt_test_final_pred_table |> filter(Red_in_perc == 50 & time_sample == 5 & Year == 2030 & status == "fit"),
               aes(x = Year, y = ref_2020_in_kt*0.7/1000, colour = "70% of 2020"),
               shape = 17,
               size = 3,
               inherit.aes = FALSE) +
    geom_point(data = ConfInt_test_final_pred_table |> filter(Red_in_perc == 50 & time_sample == 5 & Year == 2030 & status == "fit"),
               aes(x = Year, y = ref_2010_in_kt*0.6/1000, colour = "60% of 2010"),
               shape = 16,
               size = 3, 
               inherit.aes = FALSE) +
    scale_color_manual(
      name = "GMP aim",
      values = 
        c("70% of 2020" = "darkred",
          "60% of 2010" = "darkred")
    ) +
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1990, 2030),
      breaks = seq(1990, 2030, 10)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(0, lm),
      breaks = seq(0, br_lm, br_by),
      labels = label_number(big.mark = " ") # um 1 000 bessser lesen zu können
    ) +
    scale_shape_manual(values = c(
      "5" = 2,
      "10" = 5,
      "15" = 1)) +
    labs(
      title = paste0("Reductionscenario ", Red),
      x = "Year",
      y = "CH<sub>4</sub> in Tg",
      shape = "Time sample for model"
    ) + theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),   # von 15 auf 18
      axis.title.y = element_markdown(size = 16),                         # von 13 auf 16
      axis.title.x = element_markdown(size = 16),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      legend.text = element_markdown(size = 16),
      legend.title = element_markdown(size = 17),
      legend.key.size = unit(0.75, "cm")
    )

  
  return(Plot_list)
}


