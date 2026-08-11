############## Spielerei



rm(list = ls())

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
library("scales") # maskiert col_factor readr; discard von purrr; viridis_pal von viridis 
#-> muss wenn ich die aus den anderen Pakten haben will direkt ansprechen mit viridis::vidis_pal


CH4_texst_DEU <- import(file = here("03_Analyses/tables/ExtraPol_data", "Factor_table_prediction_DEU.csv"))

View(CH4_texst_DEU)

Liste_model_lin <- import(file = here("03_Analyses/tables/ExtraPol_data", "List_linMod_all_fact_DEU.rds"))

summary(Liste_model_lin[["total_CH4_in_kt_5"]])

confint(Liste_model_lin[["total_CH4_in_kt_5"]])

fitted_df_5 <- Liste_model_lin[["total_CH4_in_kt_5"]]$model |> 
  mutate(fitted = Liste_model_lin[["total_CH4_in_kt_5"]]$fitted.values
         )

fitted_df_10 <- Liste_model_lin[["total_CH4_in_kt_10"]]$model |> 
  mutate(fitted = Liste_model_lin[["total_CH4_in_kt_10"]]$fitted.values
  )

fitted_df_15 <- Liste_model_lin[["total_CH4_in_kt_15"]]$model |> 
  mutate(fitted = Liste_model_lin[["total_CH4_in_kt_15"]]$fitted.values
  )

ggplot(CH4_texst_DEU, aes(x = Year)) +
  geom_point(aes(y = total_CH4_in_kt), colour = "darkgrey") +
  geom_line(data = fitted_df_5, aes(x = Year, y = fitted), colour = "black") +
  geom_line(data = fitted_df_10, aes(x = Year, y = fitted), colour = "black") +
  geom_line(data = fitted_df_15, aes(x = Year, y = fitted), colour = "black") 





CH4_texst_NZL <- import(file = here("03_Analyses/tables/ExtraPol_data", "Factor_table_prediction_NZL.csv"))

View(CH4_texst_NZL)

Liste_model_lin_NZL <- import(file = here("03_Analyses/tables/ExtraPol_data", "List_linMod_all_fact_NZL.rds"))

summary(Liste_model_lin_NZL[["NonDairy_Pop_in_1000s"]])

confint(Liste_model_lin_NZL[["NonDairy_Pop_in_1000s_15"]])

NZL_fitted_df_5 <- Liste_model_lin_NZL[["NonDairy_Pop_in_1000s_5"]]$model |> 
  mutate(fitted = Liste_model_lin_NZL[["NonDairy_Pop_in_1000s_5"]]$fitted.values
  )

NZL_fitted_df_10 <- Liste_model_lin_NZL[["NonDairy_Pop_in_1000s_10"]]$model |> 
  mutate(fitted = Liste_model_lin_NZL[["NonDairy_Pop_in_1000s_10"]]$fitted.values
  )

NZL_fitted_df_15 <- Liste_model_lin_NZL[["NonDairy_Pop_in_1000s_15"]]$model |> 
  mutate(fitted = Liste_model_lin_NZL[["NonDairy_Pop_in_1000s_15"]]$fitted.values
  )

ggplot(CH4_texst_NZL, aes(x = Year)) +
  geom_point(aes(y = NonDairy_Pop_in_1000s), colour = "darkgrey") +
  geom_line(data = NZL_fitted_df_5, aes(x = Year, y = fitted), colour = "black") +
  geom_line(data = NZL_fitted_df_10, aes(x = Year, y = fitted), colour = "black") +
  geom_line(data = NZL_fitted_df_15, aes(x = Year, y = fitted), colour = "black") 




CH4_texst_USA <- import(file = here("03_Analyses/tables/ExtraPol_data", "Factor_table_prediction_USA.csv"))

View(CH4_texst_USA)

Liste_model_lin_USA <- import(file = here("03_Analyses/tables/ExtraPol_data", "List_linMod_all_fact_USA.rds"))

summary(Liste_model_lin_USA[["NonDairy_Pop_in_1000s"]])

confint(Liste_model_lin_USA[["NonDairy_Pop_in_1000s_5"]])

USA_fitted_df_5 <- Liste_model_lin_USA[["Dairy_Pop_in_1000s_5"]]$model |> 
  mutate(fitted = Liste_model_lin_USA[["Dairy_Pop_in_1000s_5"]]$fitted.values
  )

USA_fitted_df_10 <- Liste_model_lin_USA[["Dairy_Pop_in_1000s_10"]]$model |> 
  mutate(fitted = Liste_model_lin_USA[["Dairy_Pop_in_1000s_10"]]$fitted.values
  )

USA_fitted_df_15 <- Liste_model_lin_USA[["Dairy_Pop_in_1000s_15"]]$model |> 
  mutate(fitted = Liste_model_lin_USA[["Dairy_Pop_in_1000s_15"]]$fitted.values
  )

ggplot(CH4_texst_USA, aes(x = Year)) +
  geom_point(aes(y = Dairy_Pop_in_1000s), colour = "darkgrey") +
  geom_line(data = USA_fitted_df_5, aes(x = Year, y = fitted), colour = "black") +
  geom_line(data = USA_fitted_df_10, aes(x = Year, y = fitted), colour = "black") +
  geom_line(data = USA_fitted_df_15, aes(x = Year, y = fitted), colour = "black") 

