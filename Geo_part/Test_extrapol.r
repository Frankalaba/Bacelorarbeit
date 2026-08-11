
####################################
###################################
##################################
########################################################  Nun Extrapolation um dann die time series bis 2030 weiter zu führen
##################################
#################################
#################################

## Versuche es mit nls
## folgender Aufbau:

####################### nls(model, data, start_values); 

# Bsp. für linear : nls(y ~ a*x+ b, data = df, start = list(a = , b = ))

#Bsp. für quadr. Funkt. ->  nls(y ~ a * x^2 + b * x + c, data = df, start = list(a=0, b=0, c=0))

# Bsp. für exp. -> nls(y ~ a * exp(b * x) + c, data = df, list(a = , b = , c = ))

# Bsp. für cubic -> nls(y ~ a * x^3 + b * x^2 + c * x + d, data = df, list (a = , b = , c = , d = ))

#### da nls für nicht lineare ist, könnte man für linear, quadr. und kubisch auch lm nutzen, weil alle von linear ausgehend

####### lass exp weg, funktioniert nicht gut, müsste da auch daten weiter anpassen

# linear: lm(y ~ x, data = df)

# quadr.: lm(y ~ poly(x, 2, raw = TRUE), data = df)

# cubic: lm(y ~ poly(x, 3, raw = TRUE), data = df)

#####
###
################ Modelle als Funktiuon bauen
####
######

model_linear_fct <- function(y, x, df){
  lin <- lm( y ~ x, data = df)
  
  return(lin)
}

model_quadr_fct <- function(x, y, df){
  qua <- lm(y ~ poly(x, 2, raw = TRUE), data = df)
  
  return(qua)
}

model_cubic_fct <- function(x, y, df){
  cub <- lm(y ~ poly(x, 3, raw = TRUE), data = df)
  
  return(cub)
}

model_asymp_fct <- function(x, y){
  local_df <- data.frame(x = x, y = y)
  asymp <- nls(y ~ SSasymp(x, Asym, R0, lrc), data = local_df)
  return(asymp)
}


DEU_CH4_all_Sectors <- import(
  file = here("02_processed/Sector_Emis_data", "CH4_all_Emis_Sectors_DEU_90_24.csv")
)

glimpse(DEU_CH4_all_Sectors)

filter_test <- DEU_CH4_all_Sectors |> 
  filter(Year %in% seq(2010, 2024, 1))

glimpse(filter_test)

model_linear <- model_linear_fct(x = filter_test$Year, y = filter_test$Energy_in_kt, df = filter_test)

summary(model_linear)

model_linear_org <- model_linear_fct(x = DEU_CH4_all_Sectors$Year, y = DEU_CH4_all_Sectors$Energy_in_kt, df = DEU_CH4_all_Sectors)

View(model_cbc)


s_model_lin <- summary(model_linear)

s_model_lin$adj.r.squared
# Multiple R-squared:  0.9316,	Adjusted R-squared:  0.9295 

model_quadr <- model_quadr_fct(x = DEU_CH4_all_Sectors$Year, y = DEU_CH4_all_Sectors$Energy_in_kt, df = DEU_CH4_all_Sectors)

model_quad_filt <- model_quadr_fct(x = filter_test$Year, y = filter_test$Energy_in_kt, df = filter_test)

summary(model_quadr)
# Multiple R-squared:  0.9877,	Adjusted R-squared:  0.987 

model_cbc <- model_cubic_fct(x = DEU_CH4_all_Sectors$Year, y = DEU_CH4_all_Sectors$Energy_in_kt, df = DEU_CH4_all_Sectors)

model_cbc_filt <- model_cubic_fct(x = filter_test$Year, y = filter_test$Energy_in_kt, df = filter_test)

summary(model_cbc)
# Multiple R-squared:  0.9877,	Adjusted R-squared:  0.9866 

model_asymp <- model_asymp_fct(x = DEU_CH4_all_Sectors$Year, y = DEU_CH4_all_Sectors$Energy_in_kt)



list_model_val_name <- list( # so erzeuge ich Liste, wo ich modellen namen zu ordne, aber modelle komplett erhalte
  "linear" = model_linear_org,
  "quadr" = model_quadr,
  "cbc" = model_cbc
)

table_mod_val <- tibble( # hier nur ein Werte und gleichen Namen wie in Liste -> ermöglicht Selektion des besten Modells
  model_names = c("linear", "quadr", "cbc"),
  BIC_Val = c(BIC(model_linear_org), BIC(model_quadr), BIC(model_cbc)),
  AIC_Val = c(AIC(model_linear_org), AIC(model_quadr), AIC(model_cbc))
)

table_mod_val

best_mod_name <- table_mod_val$model_names[table_mod_val$BIC_Val == min(table_mod_val$BIC_Val) # Selection des besten Modells -> aber nur Name
                                           & table_mod_val$AIC_Val == min(table_mod_val$AIC_Val)]

best_mod <- list_model_val_name[[best_mod_name]] # hier über Name selektion des besten Modells

DEU_CH4_ALL_Sec_fitted <- DEU_CH4_all_Sectors |> 
  mutate(
    Energy_fitted_in_kt_bm = best_mod$fitted.values, .after = Energy_in_kt,
    Energy_fitted_in_kt_cbc = model_cbc$fitted.values,
    Energy_fitted_in_kt_lin_org = model_linear_org$fitted.values
  )

DEU_CH4_ALL_Sec_fitted

summary(DEU_CH4_ALL_Sec_fitted)

ggplot(DEU_CH4_ALL_Sec_fitted, aes(x = Year)) +
  geom_point(aes(y = Energy_in_kt, colour = "Energy")) + 
  geom_line(aes(y = Energy_fitted_in_kt_cbc, colour = "Model")) +
  geom_line(data = test_tab_90_50, aes(x = x, y = pred_bm, colour = "Prediction_bm")) +
  geom_line(data = test_tab_90_50, aes(x = x, y = pred_cbc, colour = "Prediction_cbc")) +
  geom_line(data = test_tab_90_50, aes(x = x, y = pred_asy, colour = "Prediction_asy")) +
  geom_line(data = test_tab_90_50, aes(x = x, y = pred_lin, colour = "Prediction_lin")) +
  geom_line(data = test_tab_90_50, aes(x = x, y = pred_cbc_filt, colour = "Prediction_cbc_filt")) +
  geom_line(data = test_tab_90_50, aes(x = x, y = pred_quad_filt, colour = "Prediction_quadr_filt")) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1989,2051),      
    breaks = seq(1990, 2050, 5)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 2*1000),        
    breaks = seq(0, 6*1000, by = 0.5*1000) 
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

test_tab_90_50 <- tibble(
  x = seq(2024, 2050, 1)
)

predict(best_mod , test_tab_90_50)
test_tab_90_50$pred_bm <- predict(best_mod , test_tab_90_50)
test_tab_90_50$pred_cbc <- predict(model_cbc , test_tab_90_50)
test_tab_90_50$pred_asy <- predict(model_asymp, test_tab_90_50)
test_tab_90_50$pred_lin_org <- predict(model_linear_org , test_tab_90_50)
test_tab_90_50$pred_lin <- predict(model_linear , test_tab_90_50)
test_tab_90_50$pred_cbc_filt <- predict(model_cbc_filt , test_tab_90_50)
test_tab_90_50$pred_quad_filt <- predict(model_quad_filt , test_tab_90_50)

test_tab_90_50

