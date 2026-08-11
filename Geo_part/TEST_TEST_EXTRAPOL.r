################# 1. Funktion bauen für Extrapolation, bzw. Modellierung


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

here()


DEU_CH4_all_Sectors <- import(
  file = here("02_processed/Sector_Emis_data", "CH4_all_Emis_Sectors_DEU_90_24.csv")
) # Testdatensatz



################
#############
#### FUnktionen für die einzelnen modelle
##############
##############


model_linear_fct <- function(y, Year, df){
  lin <- lm( y ~ Year, data = df)
  
  return(lin)
}

model_quadr_fct <- function(Year, y, df){
  qua <- lm(y ~ poly(Year, 2, raw = TRUE), data = df)
  
  return(qua)
}

model_cubic_fct <- function(Year, y, df){
  cub <- lm(y ~ poly(Year, 3, raw = TRUE), data = df)
  
  return(cub)
}

model_asymp_fct <- function(Year, y){
  local_df <- data.frame(Year = Year, y = y)
  asymp <- nls(y ~ SSasymp(Year, Asym, R0, lrc), data = local_df)
  return(asymp)
}


best_fit_function <- function(df, gr, ye){
  
# 1. Schritt: alle gebauten modelle durchlaufen lassen
  
  model_linear <- model_linear_fct(y = gr, Year = ye, df = df)
  model_quadr <- model_quadr_fct(y = gr, Year = ye, df = df)
  model_cub <- model_cubic_fct(y = gr, Year = ye, df = df)
  
# 2. Schritt: liste bauen mit namens zuschreibungen für selektion
  
  name_list_model <- list(
    "linear" = model_linear,
    "quadr" = model_quadr,
    "cubic" = model_cub
  )

# 3. Schritt: Tabelle bauen in dem Indikatoren für Selektion (AIC und BIC) mit model namen in verbindung gebracht werden
  
  tabelle_AIC_BIC <- tibble(
    names = c("linear", "quadr", "cubic"),
    AIC = c(AIC(model_linear), AIC(model_quadr), AIC(model_cub)),
    BIC = c(BIC(model_linear), BIC(model_quadr), BIC(model_cub))
  )

# 4. Schritt: Selektion des besten fits über min(AIC) und min(BIC)
  
best_fit_name <- tabelle_AIC_BIC$names[tabelle_AIC_BIC$AIC == min(tabelle_AIC_BIC$AIC) &
                                         tabelle_AIC_BIC$BIC == min(tabelle_AIC_BIC$BIC)] 

best_fit <- name_list_model[[best_fit_name]] #[[...]] (doppelte Klammer): Extrahiert den Inhalt EINES Elements
# beiner [] wäre es immer noch ne Liste bei [[]] erhält man nur den Wert
  
  
return(best_fit)
  
}


deu_model_total_wo_Agr <- best_fit_function(df = DEU_CH4_all_Sectors, 
                                            gr = DEU_CH4_all_Sectors$Total_CH4_without_Agriculture_in_kt, 
                                            ye = DEU_CH4_all_Sectors$Year)
summary(deu_model_total_wo_Agr)

deu_model_total <- best_fit_function(df = DEU_CH4_all_Sectors,
                                     gr = DEU_CH4_all_Sectors$Total_CH4_in_kt, 
                                     ye = DEU_CH4_all_Sectors$Year)

summary(deu_model_total)

deu_model_Agr <- best_fit_function(df = DEU_CH4_all_Sectors, 
                                   gr = DEU_CH4_all_Sectors$Agriculture_in_kt, 
                                   ye = DEU_CH4_all_Sectors$Year)

summary(deu_model_Agr)

fitted_plot <- DEU_CH4_all_Sectors_plot + geom_line(y = deu_model_total$fitted.values, colour = "grey40") + 
  geom_line(y = deu_model_Agr$fitted.values, colour =  "#4DAF4A") +
  geom_line(y = deu_model_total_wo_Agr$fitted.values, colour = "grey")

fitted_plot

ggsave(
  file = here("04_visual/first_try", "TEST_Emis_Sec_all_DEU_fitted.pdf"),
  plot = fitted_plot
)

###########
########### Prediction table bis 2050 erstellen
##########

Pred_table_DEU <- tibble(
  Year = seq(2024, 2050, 1)
)

Pred_table_DEU <- Pred_table_DEU |> 
  mutate(
    Agr = predict(deu_model_Agr, Pred_table_DEU),
    Total = predict(deu_model_total, Pred_table_DEU),
    Total_wo_Agr = predict(deu_model_total_wo_Agr, Pred_table_DEU)
  ) |> 
  mutate(
    Total_calc = Agr + Total_wo_Agr
  ) |> 
  mutate(
    Genauigkeit = Total_calc/Total * 100
  )
names(Pred_table_DEU)

View(Pred_table_DEU)

############## -> Visualisierung des fits -> Problem wird direkt ersichtlich

names(DEU_CH4_all_Sectors)

Test_plot_extrapol <- ggplot(DEU_CH4_all_Sectors, aes(x = Year)) +
  geom_point(aes(y = Total_CH4_in_kt, colour = "grey40")) +
  geom_point(aes(y = Total_CH4_without_Agriculture_in_kt, colour = "grey")) +
  geom_point(aes(y = Agriculture_in_kt, colour = "#4DAF4A")) +
  geom_line(aes(y = deu_model_total$fitted.values, colour = "grey40")) + 
  geom_line(aes(y = deu_model_Agr$fitted.values, colour =  "#4DAF4A")) +
  geom_line(aes(y = deu_model_total_wo_Agr$fitted.values, colour = "grey")) +
  geom_line(data = Pred_table_DEU, aes( x = Year, y = Agr, colour = "#4DAF4A"), linetype = "dashed") + 
  geom_line(data = Pred_table_DEU, aes( x = Year, y = Total, colour = "grey40"), linetype = "dashed") +
  geom_line(data = Pred_table_DEU,  aes(x = Year, y = Total_wo_Agr, colour = "grey"), linetype = "dashed") +
  scale_x_continuous(
    expand = expansion(mult = 0.05),
    limits = c(1990,2050),
    breaks = seq(1980, 2050, 5)
  ) +
  
  scale_y_continuous(
    expand = c(0,0),
    limits = c(-500, 8*1000),        
    breaks = seq(-2000, 10*1000, by = 1*1000) 
  ) +
  ylab(label = "CH4 in kt") +
  labs(title = paste0("Emissionsectors - ", "DEU"),
       colour = "Sectors") +
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5, # Position horizontal: 0 = links, 0.5 = zentriert, 1 = rechts
      face = "bold",
      size = 10
    ),
    panel.grid.minor = element_line( # minor sind linien im Feld, major achsenlinien
      color = "grey92",
      linewidth = 0.1
    )
  )
Test_plot_extrapol
  