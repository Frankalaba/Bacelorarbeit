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
  theme_bw() +   # oder theme_minimal(), was du sonst nutzt, als Basis
    theme(
      plot.title = element_markdown(hjust = 0.5),
      plot.subtitle = element_markdown(),
      axis.title.x = element_markdown(),
      axis.title.y = element_markdown(),
      legend.title = element_markdown(),
      legend.text = element_markdown(),
      strip.text = element_markdown() 
    )
)


test_final_pred_table <- import(file = here("03_Analyses/tables/ExtraPol_data/Prediction_2030", "Final_Pred_table_22_30_USA_10_50_perc.csv"))

View(test_final_pred_table)

names(test_final_pred_table)
# relevante. "Red_in_perc"; "time_sample"; EnterFe_CH4_emis_org_in_kt"; "EnterFe_CH4_emis_upd_in_kt_Ym"; 

CH4_texst_USA_trainings_data <- import(file = here("03_Analyses/tables/ExtraPol_data", "Factor_table_prediction_USA.csv"))

View(CH4_texst_DEU_trainings_data)

names(test_final_pred_table)

ribbon_data <- test_final_pred_table |> 
  filter(Red_in_perc == 50) |> 
  group_by(Year) |> 
  summarise(
    upd_min = min(total_CH4_upd_Ym_DE_in_kt, na.rm = TRUE),
    upd_max = max(total_CH4_upd_Ym_DE_in_kt, na.rm = TRUE),
    org_min = min(total_CH4_org_calc_in_kt, na.rm = TRUE),
    org_max = max(total_CH4_org_calc_in_kt, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(test_final_pred_table |> filter(Red_in_perc == 50), 
       aes(x = Year, group = time_sample, shape = factor(time_sample))) +
  geom_ribbon(data = ribbon_data, 
              aes(x = Year, ymin = upd_min, ymax = upd_max), 
              fill = "steelblue", alpha = 0.15, inherit.aes = FALSE) +
  geom_ribbon(data = ribbon_data, 
              aes(x = Year, ymin = org_min, ymax = org_max), 
              fill = "steelblue", alpha = 0.15, inherit.aes = FALSE) +
  geom_point(aes(y = total_CH4_org_calc_in_kt, colour = "Prediction without immunization")) +
  geom_point(aes(y = total_CH4_upd_Ym_DE_in_kt, colour = "Prediction with immunization")) +
  geom_point(data = CH4_texst_USA_trainings_data, 
             aes(x = Year, y = total_CH4_pred_in_kt, colour = "Historical methane emission"), 
             shape = 16, inherit.aes = FALSE) +
  geom_point(data = test_final_pred_table |> filter(Red_in_perc == 50 & time_sample == 5 & Year == 2030),
             aes(x = Year, y = ref_2020_in_kt*0.7, colour = "70% of 2020"),
             shape = 4,
             size = 2.5,
             inherit.aes = FALSE) +
  geom_point(data = test_final_pred_table |> filter(Red_in_perc == 50 & time_sample == 5 & Year == 2030),
             aes(x = Year, y = ref_2010_in_kt*0.6, colour = "60% of 2010"),
             shape = 4,
             size = 2.5,
             inherit.aes = FALSE) +
  scale_x_continuous(
    expand = expansion(mult = 0.05),
    limits = c(1990, 2030),
    breaks = seq(1990, 2030, 10)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.05),
    limits = c(0, 35*1000),
    breaks = seq(0, 40*1000, 10*1000)
  ) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  labs(
    title = "USA - Reductionscenario 30%",
    x = "Year",
    y = "Methane in kt"
  ) 

#######################################################################################
#####################################################################################
####################################################################################
###################### Jetzt mit den ConfInt
###############################################################################

ConfInt_test_final_pred_table <- import(file = here("03_Analyses/tables/ExtraPol_data/Prediction_2030/Visual_final_Emission_table", "Final_total_CH4_Calc_data_23_30_USA.csv"))

View(ConfInt_test_final_pred_table)

names(test_final_pred_table)
# relevante. "Red_in_perc"; "time_sample"; EnterFe_CH4_emis_org_in_kt"; "EnterFe_CH4_emis_upd_in_kt_Ym"; 

CH4_texst_USA_trainings_data <- import(file = here("03_Analyses/tables/ExtraPol_data", "Factor_table_prediction_USA.csv"))

View(CH4_texst_DEU_trainings_data)

names(test_final_pred_table)

limits_for_CONFin <- tibble(
  Year = ConfInt_test_final_pred_table |>  filter(status == "fit") |> pull(Year),
  Red_in_perc = ConfInt_test_final_pred_table |>  filter(status == "fit") |> pull(Red_in_perc),
  time_sample = ConfInt_test_final_pred_table |>  filter(status == "fit") |> pull(time_sample),
  lwr_total_CH4_org = ConfInt_test_final_pred_table |> filter(status == "lwr") |> pull(total_CH4_org_calc_in_kt),
  upr_total_CH4_org = ConfInt_test_final_pred_table |> filter(status == "upr") |> pull(total_CH4_org_calc_in_kt),
  lwr_total_CH4_upd = ConfInt_test_final_pred_table |> filter(status == "lwr") |> pull(total_CH4_upd_Ym_DE_in_kt),
  upr_total_CH4_upd = ConfInt_test_final_pred_table |> filter(status == "upr") |> pull(total_CH4_upd_Ym_DE_in_kt),
  
)

names(limits_for_CONFin)

ggplot(ConfInt_test_final_pred_table |>  filter(status == "fit" & Red_in_perc == 30), 
       aes(x = Year, shape = factor(time_sample))) +
  geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == 30 & time_sample == 5), 
              aes(x = Year, ymin = lwr_total_CH4_org, ymax = upr_total_CH4_org), 
              fill = "#1F968BFF", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == 30 & time_sample == 10), 
              aes(x = Year, ymin = lwr_total_CH4_org, ymax = upr_total_CH4_org), 
              fill = "#1F968BFF", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == 30 & time_sample == 15), 
              aes(x = Year, ymin = lwr_total_CH4_org, ymax = upr_total_CH4_org), 
              fill = "#1F968BFF", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == 30 & time_sample == 5), 
              aes(x = Year, ymin = lwr_total_CH4_upd, ymax = upr_total_CH4_upd), 
              fill = "#32648EFF", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == 30 & time_sample == 10), 
              aes(x = Year, ymin = lwr_total_CH4_upd, ymax = upr_total_CH4_upd), 
              fill = "#32648EFF", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = limits_for_CONFin |>  filter(Red_in_perc == 30 & time_sample == 15), 
              aes(x = Year, ymin = lwr_total_CH4_upd, ymax = upr_total_CH4_upd), 
              fill = "#32648EFF", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(aes(y = total_CH4_org_calc_in_kt, colour = "Prediction without immunization"),
             size = 2.5) +
  geom_point(aes(y = total_CH4_upd_Ym_DE_in_kt, colour = "Prediction with immunization"),
             size = 2.5) +
  geom_point(data = CH4_texst_USA_trainings_data, 
             aes(x = Year, y = total_CH4_pred_in_kt, colour = "Historical"), 
             shape = 19,
             size = 2.5, inherit.aes = FALSE) +
  scale_color_manual(name = "Methane Emission",
                     values = c(
    "Prediction without immunization" = "#1F968BFF",
    "Prediction with immunization" = "#32648EFF",
    "Historical" = "#440154FF" 
  )) + 
  new_scale_colour() + # ermöglicht nochmal legende über colour, die dann aber neue überschrift bekommen kann
  geom_point(data = ConfInt_test_final_pred_table |> filter(Red_in_perc == 50 & time_sample == 5 & Year == 2030 & status == "fit"),
             aes(x = Year, y = ref_2020_in_kt*0.7, colour = "70% of 2020"),
             shape = 8,
             size = 3,
             inherit.aes = FALSE) +
  geom_point(data = ConfInt_test_final_pred_table |> filter(Red_in_perc == 50 & time_sample == 5 & Year == 2030 & status == "fit"),
             aes(x = Year, y = ref_2010_in_kt*0.6, colour = "60% of 2010"),
             shape = 4,
             size = 3, 
             inherit.aes = FALSE) +
  scale_color_manual(
    name = "GMP aim",
    values = 
      c("70% of 2020" = "darkgrey",
        "60% of 2010" = "darkgrey")
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.05),
    limits = c(1990, 2030),
    breaks = seq(1990, 2030, 10)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.05),
    limits = c(0, 35*1000),
    breaks = seq(0, 40*1000, 10*1000)
  ) +
  scale_shape_manual(values = c(
    "5" = 2,
    "10" = 5,
    "15" = 1)) +
  labs(
    title = "**Total CH<sub>4</sub> - Reductionscenario 30%**",
    x = "Year",
    y = "CH<sub>4</sub> in kt",
    shape = "Time sample for model",
    colour = "Methane emission"
  ) 

viridis(n = 5, option = "D")   # 5 Farben aus der Palette
viridis(n = 20, option = "D")  # 20 Farben aus der Palette

  