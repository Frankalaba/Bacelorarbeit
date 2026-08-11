# da extrapolation nicht sinnvoll mit cubischen oder quadratischen Funktionen -> kommt zu komischen Prediktion 
# -> auch wenn die beser für Interpolation sind
## 

### mein fit soll ja aber am besten für Extrapolation sein -> deswegen lineare Interpolation -> über 5 und 10 und 15 Jahre

########### 

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

source(file = here("03_Analyses/Ym_und_DE_verändern_Skripte", "Function_Analyses_methane_reduction_simple.r"))

here()


################
##### Als erste die lineare Funktion/ Modell, welche später nutzen werde
################

model_lin <- function(df, Year, y){
  mod <- lm(y ~ Year, data = df)
  
  return(mod)
}


############################################
############ Funktion bauen, mit der ich alle Parameter in einer Tabelle hab, die ich extrapolieren möchte, muss
###########################################

Table_All_Categories_For_extrapol_function <- function(CC, YE){
  
  tabl_strc_1 <- paste0("CH4_all_Emis_Sectors_", CC, "_90_", YE, ".csv")
  
  tabl_strc_2 <- paste0("CRT_", CC, "_1990_20", YE, "_all.csv")
  
  
  CH4_all_Sec <- import(
    file = here("02_processed/Sector_Emis_data", tabl_strc_1)
  )
  
  CH4_all_cat_EnterFer <- import(
    file = here("02_processed/CRT", tabl_strc_2)
  )
  
  
  #names(CH4_all_cat_EnterFer)
  
  CH4_cat_EnterFer <- CH4_all_cat_EnterFer |> 
    filter(Animal == "Cattle")
  
  CH4_Dairy_EnterFer <- CH4_all_cat_EnterFer |> 
    filter(Animal == "Dairy cattle")
  
  
  CH4_NonDairy_EnterFer <- CH4_all_cat_EnterFer |> 
    filter(Animal == "Non-dairy cattle")
  
  
  
  Input_data_for_ExtrPol <- CH4_all_Sec |> 
    select(Year, total_CH4_pred_in_kt = Total_CH4_in_kt) |> 
    left_join(
      CH4_cat_EnterFer |> select(Year, Cat_EnterFer_pred_in_kt = Ch4_emission_in_kt),
      by = "Year"
    ) |> 
    left_join(
      CH4_Dairy_EnterFer |> 
        transmute(
          Year,
          Dairy_Pop_in_1000s = Population_size_1000s,
          Dairy_Ym_in_perc = Ym_in_percent,
          Dairy_DE_in_perc = Digestibility_of_feed_in_percent,
          Dairy_NEs_total = NEs_function(GE = ø_GE_MJ_per_head_per_day, DE = Digestibility_of_feed_in_percent)
        ),
      by = "Year"
    ) |> 
    left_join(
      CH4_NonDairy_EnterFer |> 
        transmute(
          Year,
          NonDairy_Pop_in_1000s = Population_size_1000s,
          NonDairy_Ym_in_perc = Ym_in_percent,
          NonDairy_DE_in_perc = Digestibility_of_feed_in_percent,
          NonDairy_NEs_total = NEs_function(GE = ø_GE_MJ_per_head_per_day, DE = Digestibility_of_feed_in_percent)
        ),
      by = "Year"
    ) |> 
    mutate(total_CH4_wo_Cat_EnterFe_in_kt = total_CH4_pred_in_kt - Cat_EnterFer_pred_in_kt, .after = total_CH4_pred_in_kt)
  
  
  return(Input_data_for_ExtrPol)
  
}

##############################
############# Nun Funktion bauen für die linearen Model Szenarien 5, 10 und 15 Jahre
############################

List_models_for_factors_function <- function(CC, YE){
  
  table_strc <- paste0("Factor_table_prediction_", CC, ".csv")
  
  Factor_table <- import(file = here("03_Analyses/tables/ExtraPol_data", table_strc))
  
  # Vorbereitungen für die erstellungen der Modelle in einer liste für die 3 Szenarien + alle Faktoren
  
  model_lin <- function(df, Year, y){
    mod <- lm(y ~ Year, data = df)
    
    return(mod)
  }
  
  "5_year" <- seq(YE - 4, YE, 1)
  "10_year" <- seq(YE - 9, YE, 1)
  "15_year" <- seq(YE - 14, YE, 1)
  
  Factor_table_5_year <- Factor_table |> 
    filter(Year %in% `5_year`)
  
  Factor_table_10_year <- Factor_table |> 
    filter(Year %in% `10_year`)
  
  Factor_table_15_year <- Factor_table |> 
    filter(Year %in% `15_year`)
  
  factor_names <- Factor_table |> 
    select(-c(Year)) |> 
    names()
  
  list_5Year_model <- list()
  list_10Year_model <- list()
  list_15Year_model <- list()
  
  ## Nun liste bauen für die einzelnen Jahresszenarien
  
  for (name in factor_names) {
    list_5Year_model[[name]] <-
      model_lin(df = Factor_table_5_year, 
                Year = Factor_table_5_year$Year, 
                y = Factor_table_5_year[[name]])
  }
    
    for (name in factor_names) {
      list_10Year_model[[name]] <-
        model_lin(df = Factor_table_10_year, 
                  Year = Factor_table_10_year$Year, 
                  y = Factor_table_10_year[[name]])
    }
  
  for (name in factor_names) {
    list_15Year_model[[name]] <-
      model_lin(df = Factor_table_15_year, 
                Year = Factor_table_15_year$Year, 
                y = Factor_table_15_year[[name]])
  }
  
  #Suffixe anfügen, um die Listen zusamenfügen zu können
  
  names(list_5Year_model) <- paste0(names(list_5Year_model), "_5")
  names(list_10Year_model) <- paste0(names(list_10Year_model), "_10")
  names(list_15Year_model) <- paste0(names(list_15Year_model), "_15")
  
  list_all <- c(list_5Year_model, list_10Year_model, list_15Year_model)
  
  return(list_all)
}


##########################
######## Funktion bauen, die Tabelle herstellt, welche die Gleichung des Models enthält sowie den R²
##########################


Tabelle_Equation_R2 <- function(ls){
  
  tbl <- tibble(
    names = c("Equation", "R²")
  )
  
  for (nms in names(ls)) {
    
    intercept <- ls[[nms]]$coefficients[1]
    slope <- ls[[nms]]$coefficients[2]
    
    equation <- paste0(
      "y = ", round(slope, 3), "*x ", ifelse(intercept >= 0, "+ ", "- "), round(abs(intercept), 3)
    )
    # round(data, Nachkommastellen)
    
    tbl <- tbl |> 
      mutate(
        !!nms := c(equation, 
                round(summary(ls[[nms]])$r.squared, 3)))
      
  }
  
return(tbl)
}


##############################################
############## Function for determine weak linear models (R² <= 0.6)
#############################################


Filter_weak_model_lin_function <- function(df, th){
  
  # Tabelle muss dabei eine col haben mit names und einer Reihe R²
  
  r2_col_weak <- df |> 
    filter(names == "R²") |> 
    pivot_longer(-names, names_to = "Factors", values_to = "R²") |> 
    filter(`R²` <= th)
  
  names_weak_models <- r2_col_weak$Factors
  
  return(names_weak_models)
}

###############################################
############### Funktion bauen für Prediction table
###############################################


Prediction_2030_model_Lin <- function(ls, YB){
  
  ## 1st step: Creating a empty table with the relevant time period for the prediction
  
  Pred_table <- tibble(
    Year = seq(YB, 2030, 1)
  )
  
  
  ## 2nd step:  Build a loop for all linear models (all the different factors and time sets)
  
  for (nms in names(ls)) {
    Pred_table <- Pred_table |> 
      mutate(
        !!nms := predict(ls[[nms]], Pred_table)
      )
  }
  
  return(Pred_table)
  
}

#########################################
############### Funktion bauen für die Berechnung der Prediction bis 2030 mit confidence intervall
########################################



Prediction_conf_2030_model_Lin <- function(ls, YB){
  
  ## 1st step: Creating a empty table with the relevant time period for the prediction
  
  Pred_table <- tibble(
    Year = seq(YB, 2030, 1)
  )
  
  
  ## 2nd step:  Build a loop for all linear models (all the different factors and time sets)
  
  for (nms in names(ls)) {
    
    pred <- predict(ls[[nms]], newdata = Pred_table, interval = "confidence", level = 0.95)
# pred tabelle weist 3 Spalten auf mit fit, lwr (untergrenze), upr (obergrenze)
    
    Pred_table <- Pred_table |> 
      mutate(
        !!paste0(nms, "_fit") := pred[, "fit"],
        !!paste0(nms, "_lwr") := pred[, "lwr"],
        !!paste0(nms, "_upr") := pred[, "upr"]
      )
  }
  
  return(Pred_table)
  
}



######################################
############# Funktion für Tabelle, die nur die Prediction der Factoren enthält, 
#############die es zur Berechnung der angepassten Emission braucht
######################################


# Um spalten zu selektieren und ich nicht jedes mal den fast gleichen Namen einzugeben, da hier nur Endung sich unterscheidet
# df |> select(starts_with("Value_"))

#bei Zeilen
#df |> filter(str_detect(Spaltenname, "^Value_"))

#dabei gilt
#Der Punkt . bedeutet "ein beliebiges Zeichen", und * bedeutet "beliebig oft
#zsm .* bedeutet es beliebig viele Zeichen
##### um dieses muster zuerkennen, muss aber matches verwendet werden. Andere erkennen Regex nicht

CH4_emission_upd_for_pred <- function(df, Ym_upd, ref_2020, ref_2010){
  
  Faktoren_prdctn_calc_Dairy <- df |> 
    select(c(Year, matches("^Dairy.*"))) |> 
    mutate(
      Animal = rep("Dairy cattle", n()), .after = Year
    ) |>  
    rename_with( #rename_with(.data, .fn, .cols = everything(), ...) -> dabei .fn function, die auf .col angewendet wird
      .fn = ~ substring(.x, 7), #.x ist der Standard-Platzhalter in purrr-Funktionen (wie map(), oder eben rename_with(.fn = ~...)) für "das aktuelle Element, das gerade verarbeitet wird".
      .cols = -c(Year, Animal)
      ) |>  # substring schneidet nach angabe, und übernimmt erst ab da
    pivot_longer(
      cols = -c(Year, Animal),
      names_to = c(".value", "time_sample"),
      names_pattern = "(.*)_(\\d+)$"
    )
  
  Faktoren_prdctn_calc_NonDairy <- df |> 
    select(c(Year, matches("^NonDairy.*"))) |> 
    mutate(
      Animal = rep("Non-dairy cattle", n()), .after = Year
    ) |> 
    rename_with(
      .fn = ~ substring(.x, 10),
      .cols = -c(Year, Animal) 
    ) |> 
    pivot_longer(
     cols =  -c(Year, Animal),
     names_to = c(".value", "time_sample"), # ".value" sorgt dafür, dass man nicht noch ne value spalte brauch, sondern die Variablen neben einander auftauchen sollen
     names_pattern = "(.*)_(\\d+)$" # dabei setzen die () was übernommen ewrden soll -> \\d bedeutet eine einstellige Zahl, \\d+ auch mehrstellige Zahl
    ) ## die korrekte zuordnung erfolgt über die Reihenfolge in names_to

  Faktoren_prdctn_calc_Dairy_NonDairy <- bind_rows(Faktoren_prdctn_calc_Dairy, Faktoren_prdctn_calc_NonDairy)
  
  Pred_upd <- Faktoren_prdctn_calc_Dairy_NonDairy |> 
    transmute( # transmute übernimmt nur Spalten die explizit genannt werden
      Year,
      Animal,
      Pop_in_1000s,
      time_sample,
      Red_in_perc = Ym_upd * 100,
      Ym_org_in_perc = Ym_in_perc,
      Ym_upd_in_perc = Ym_in_perc * (1 - Ym_upd),
      DE_org_in_perc = DE_in_perc,
      DE_upd_in_perc = DE_in_perc + (Ym_in_perc * Ym_upd),
      NEs_total
    ) |> #jetzt müsste alles in der Tabelle sein, um die weiteren Emissionen zu berechnen über die oben niedergeschriebenen Funktionen
    mutate(
      GE_org_in_MJ_per_head_per_day = GE_upd_function_NEs_known(NEs = NEs_total, 
                                                                DE_upd = DE_org_in_perc),
      GE_upd_in_MJ_per_head_per_day = GE_upd_function_NEs_known(NEs = NEs_total, 
                                                      DE_upd = DE_upd_in_perc),  .after = NEs_total
    ) |> 
    mutate(
      EnterFe_CH4_emis_org_in_kt = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                     Ym_t = Ym_org_in_perc, 
                                                     N_t_in_1000s = Pop_in_1000s), 
      EnterFe_CH4_emis_upd_in_kt_Ym = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                        Ym_t = Ym_upd_in_perc, 
                                                        N_t_in_1000s = Pop_in_1000s),
      EnterFe_CH4_emis_upd_in_kt_Ym_DE = emission_total_per_animal(GE = GE_upd_in_MJ_per_head_per_day, 
                                                           Ym_t = Ym_upd_in_perc, 
                                                           N_t_in_1000s = Pop_in_1000s)) 
  
  ### Problem: Cattle Emissionen weichen von sum(Dairy und Non-Dairy) ab, da kein linearer Zusammenhang mehr 
  #-> neu berechnen und korrigieren
  
  result_cattle_summed <- Pred_upd |> 
    group_by(Year, Red_in_perc, time_sample) |> 
    summarise(
      Ym_org_in_perc = weighted.mean(x =  Ym_org_in_perc, w = Pop_in_1000s),
      Ym_upd_in_perc = weighted.mean(x =  Ym_upd_in_perc, w = Pop_in_1000s),
      DE_org_in_perc = weighted.mean(x =  DE_org_in_perc, w = Pop_in_1000s),
      DE_upd_in_perc = weighted.mean(x =  DE_upd_in_perc, w = Pop_in_1000s),
      EnterFe_CH4_emis_org_in_kt = sum(EnterFe_CH4_emis_org_in_kt),
      EnterFe_CH4_emis_upd_in_kt_Ym = sum(EnterFe_CH4_emis_upd_in_kt_Ym),
      NEs_total = weighted.mean(x = NEs_total, w = Pop_in_1000s),
      GE_upd_in_MJ_per_head_per_day = weighted.mean(x = GE_upd_in_MJ_per_head_per_day, w = Pop_in_1000s),
      EnterFe_CH4_emis_upd_in_kt_Ym_DE = sum(EnterFe_CH4_emis_upd_in_kt_Ym_DE),
      Pop_in_1000s = sum(Pop_in_1000s), # muss als letztes, weil summarize sequentiell arbeitet,
      # bei den weighted mean würde sonst falscher Wert für gewicht genommen werden.
      .groups = "drop"
    ) |> 
    mutate(
      Share_change_pred_upd_org_in_perc = 100 - (EnterFe_CH4_emis_upd_in_kt_Ym_DE/EnterFe_CH4_emis_org_in_kt*100)
    ) |> 
    relocate(c(EnterFe_CH4_emis_org_in_kt, EnterFe_CH4_emis_upd_in_kt_Ym), .before = EnterFe_CH4_emis_upd_in_kt_Ym_DE) |> 
    relocate(Pop_in_1000s, .after = Year) |> 
    arrange(Year, # nach Jahr soll zu erst sortiert werden
            factor(time_sample, levels = c("5", "10", "15")))
  
  #### Tabelle anfügen, wo auch total over all sectors drin ist
  
  ### muss auch erst noch aufgeteilt werden mit time_sample
  
  total_time_sample <- df |> 
    select(c(-matches("Dairy.*"))) |> 
    pivot_longer(cols = -Year,
                 names_to = c(".value", "time_sample"),
                 names_pattern = "(.*)_(\\d+)$"
                 )
  
  final_table_withall_prediction <- left_join(result_cattle_summed, total_time_sample, by = c("Year", "time_sample"))
  
  final_table_withall_prediction <- final_table_withall_prediction |> 
    rename(total_CH4_org_pred_in_kt = total_CH4_pred_in_kt ) |> # rename(neuer name = alter Name) 
    mutate(
      total_CH4_org_calc_in_kt = total_CH4_wo_Cat_EnterFe_in_kt + EnterFe_CH4_emis_org_in_kt,
      total_CH4_upd_Ym_DE_in_kt = total_CH4_wo_Cat_EnterFe_in_kt + EnterFe_CH4_emis_upd_in_kt_Ym_DE,
      CH4_emis_reduction_in_kt = total_CH4_org_calc_in_kt - total_CH4_upd_Ym_DE_in_kt,
      ref_2020_in_kt = rep(ref_2020, n()),
      Prozent_emis_red_compared_2020_org_pred = 100 - (total_CH4_org_calc_in_kt/ref_2020)*100,
      Prozent_emis_red_compared_2020_upd_pred = 100 - (total_CH4_upd_Ym_DE_in_kt/ref_2020)*100,
      ref_2010_in_kt = rep(ref_2010, n()),
      Prozent_emis_red_compared_2010_org_pred = 100 - (total_CH4_org_calc_in_kt/ref_2010)*100,
      Prozent_emis_red_compared_2010_upd_pred = 100 - (total_CH4_upd_Ym_DE_in_kt/ref_2010)*100
    )
  
  
  return(final_table_withall_prediction)
  
}

#Pred_table_22_30_USA

#test_result_1 <- CH4_emission_upd_for_pred(Ym_upd = 0.1, df = Pred_table_22_30_USA)
#glimpse(test_result_1)
#View(test_result_1)

# funktoiniert


###############################
############ Table mit berechneten CH4 values with ConfInt
#############################

ConfInt_CH4_emission_upd_for_pred_function <- function(df, Ym_upd, ref_2020, ref_2010){
  
  #### Um berechnung machen zu können, erst al 100 Columns in longformat transformieren
  
  Faktoren_prdctn_calc_Dairy <- df |> 
    select(c(Year, matches("^Dairy.*"))) |> 
    mutate(
      Animal = rep("Dairy cattle", n()), .after = Year
    ) |>  
    rename_with( #rename_with(.data, .fn, .cols = everything(), ...) -> dabei .fn function, die auf .col angewendet wird
      .fn = ~ substring(.x, 7), #.x ist der Standard-Platzhalter in purrr-Funktionen (wie map(), oder eben rename_with(.fn = ~...)) für "das aktuelle Element, das gerade verarbeitet wird".
      .cols = -c(Year, Animal)
    ) |>  # substring schneidet nach angabe, und übernimmt erst ab da
    pivot_longer(
      cols = -c(Year, Animal),
      names_to = c(".value", "time_sample", "status"),
      names_pattern = "(.*)_(\\d+)_(.*)$"
    )
  
  Faktoren_prdctn_calc_NonDairy <- df |> 
    select(c(Year, matches("^NonDairy.*"))) |> 
    mutate(
      Animal = rep("Non-dairy cattle", n()), .after = Year
    ) |> 
    rename_with(
      .fn = ~ substring(.x, 10),
      .cols = -c(Year, Animal) 
    ) |> 
    pivot_longer(
      cols =  -c(Year, Animal),
      names_to = c(".value", "time_sample", "status"), # ".value" sorgt dafür, dass man nicht noch ne value spalte brauch, sondern die Variablen neben einander auftauchen sollen
      names_pattern = "(.*)_(\\d+)_(.*)$" # dabei setzen die () was übernommen ewrden soll -> \\d bedeutet eine einstellige Zahl, \\d+ auch mehrstellige Zahl
    ) ## die korrekte zuordnung erfolgt über die Reihenfolge in names_to
  
  For_Calc_table <- bind_rows(Faktoren_prdctn_calc_Dairy, Faktoren_prdctn_calc_NonDairy)
  
  #### Jetzt tabelle für Calculation vorbereiten
  
  Table_CH4_calculated_withfactors <- For_Calc_table |> 
    transmute(
      Year,
      Animal,
      time_sample,
      status,
      Pop_in_1000s,
      Red_in_perc = Ym_upd * 100,
      Ym_org_in_perc = Ym_in_perc,
      Ym_upd_in_perc = Ym_in_perc * (1 - Ym_upd),
      DE_org_in_perc = DE_in_perc,
      DE_upd_in_perc = DE_in_perc + (Ym_in_perc * Ym_upd),
      NEs_total
    ) |> # GE_upd_function_Nes_known berechnet einfach GE -> also nicht nur GE_upd -> schlecht benannt
    mutate(
      GE_org_in_MJ_per_head_per_day = GE_upd_function_NEs_known(NEs = NEs_total, 
                                                                DE_upd = DE_org_in_perc),
      GE_upd_in_MJ_per_head_per_day = GE_upd_function_NEs_known(NEs = NEs_total, 
                                                                DE_upd = DE_upd_in_perc),  .after = NEs_total
    ) |> 
    mutate(
      EnterFe_CH4_emis_org_in_kt = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                             Ym_t = Ym_org_in_perc, 
                                                             N_t_in_1000s = Pop_in_1000s), 
      EnterFe_CH4_emis_upd_in_kt_Ym = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                                Ym_t = Ym_upd_in_perc, 
                                                                N_t_in_1000s = Pop_in_1000s),
      EnterFe_CH4_emis_upd_in_kt_Ym_DE = emission_total_per_animal(GE = GE_upd_in_MJ_per_head_per_day, 
                                                                   Ym_t = Ym_upd_in_perc, 
                                                                   N_t_in_1000s = Pop_in_1000s)) 
  
  ##### Die berechneten Methanwerte jetzt aber noch nicht für cattle
  
  Table_CH4_calculated_withfactors_for_cat <- Table_CH4_calculated_withfactors |> 
    select(c(Year, time_sample, status, Red_in_perc, Animal, 
             EnterFe_CH4_emis_org_in_kt, EnterFe_CH4_emis_upd_in_kt_Ym, EnterFe_CH4_emis_upd_in_kt_Ym_DE, 
             Pop_in_1000s)) |> 
    group_by(Year, time_sample, status, Red_in_perc) |>  # alles groupen, was bei summarise am Ende noch als einzelne Spalte übrig bleiben soll
    summarise(
      EnterFe_CH4_emis_org_in_kt = sum(EnterFe_CH4_emis_org_in_kt),
      EnterFe_CH4_emis_upd_in_kt_Ym = sum(EnterFe_CH4_emis_upd_in_kt_Ym),
      EnterFe_CH4_emis_upd_in_kt_Ym_DE = sum(EnterFe_CH4_emis_upd_in_kt_Ym_DE),
      Pop_in_1000s = sum(Pop_in_1000s),
      .groups = "drop"
    ) |> 
    relocate(Pop_in_1000s, .after = status) |> 
    mutate(
      Diff_EnterFe_Cat_org_upd_in_kt = EnterFe_CH4_emis_org_in_kt - EnterFe_CH4_emis_upd_in_kt_Ym_DE,
      DE_Share_on_reduction_potential_in_kt = (EnterFe_CH4_emis_org_in_kt - EnterFe_CH4_emis_upd_in_kt_Ym_DE) - (EnterFe_CH4_emis_org_in_kt - EnterFe_CH4_emis_upd_in_kt_Ym),
      DE_Share_on_reduction_potential_in_perc = ((1 - (EnterFe_CH4_emis_org_in_kt - EnterFe_CH4_emis_upd_in_kt_Ym_DE)/(EnterFe_CH4_emis_org_in_kt - EnterFe_CH4_emis_upd_in_kt_Ym))*100)
      ) |> 
    arrange(Year,
            factor(time_sample, levels = c("5", "10", "15")))
    
    
  ###### Um total CH4 berechnen zu können, bedarf es noch der vorhergesagten Werte für total wo Cat_EnterFe
  
  total_time_sample <- df |> 
    select(c(-matches("Dairy.*"))) |> 
    pivot_longer(cols = -Year,
                 names_to = c(".value", "time_sample", "status"),
                 names_pattern = "(.*)_(\\d+)_(.*)$"
    )
  
  Final_table_CH4 <- left_join(Table_CH4_calculated_withfactors_for_cat, total_time_sample, by = c("Year", "time_sample", "status"))
  
  final_table_withall_prediction <- Final_table_CH4 |> 
    rename(total_CH4_org_pred_in_kt = total_CH4_pred_in_kt ) |> # rename(neuer name = alter Name) 
    mutate(
      total_CH4_org_calc_in_kt = total_CH4_wo_Cat_EnterFe_in_kt + EnterFe_CH4_emis_org_in_kt,
      total_CH4_upd_Ym_DE_in_kt = total_CH4_wo_Cat_EnterFe_in_kt + EnterFe_CH4_emis_upd_in_kt_Ym_DE,
      ref_2020_in_kt = rep(ref_2020, n()),
      Prozent_emis_red_compared_2020_org_pred = 100 - (total_CH4_org_calc_in_kt/ref_2020)*100,
      Prozent_emis_red_compared_2020_upd_pred = 100 - (total_CH4_upd_Ym_DE_in_kt/ref_2020)*100,
      ref_2010_in_kt = rep(ref_2010, n()),
      Prozent_emis_red_compared_2010_org_pred = 100 - (total_CH4_org_calc_in_kt/ref_2010)*100,
      Prozent_emis_red_compared_2010_upd_pred = 100 - (total_CH4_upd_Ym_DE_in_kt/ref_2010)*100
    )
  
  
  return(final_table_withall_prediction)
}



##########################################
################ Funktion für Tabelle, die allles beinhaltet aus der Emissionen für EnterFe Dairy und NonDairy berechnet wurde
##########################################


ConfInt_CH4_emission_upd_for_pred_calculation_base_function <- function(df, Ym_upd){ # ref_2020, ref_2010
  
  #### Um berechnung machen zu können, erst al 100 Columns in longformat transformieren
  
  Faktoren_prdctn_calc_Dairy <- df |> 
    select(c(Year, matches("^Dairy.*"))) |> 
    mutate(
      Animal = rep("Dairy cattle", n()), .after = Year
    ) |>  
    rename_with( #rename_with(.data, .fn, .cols = everything(), ...) -> dabei .fn function, die auf .col angewendet wird
      .fn = ~ substring(.x, 7), #.x ist der Standard-Platzhalter in purrr-Funktionen (wie map(), oder eben rename_with(.fn = ~...)) für "das aktuelle Element, das gerade verarbeitet wird".
      .cols = -c(Year, Animal)
    ) |>  # substring schneidet nach angabe, und übernimmt erst ab da
    pivot_longer(
      cols = -c(Year, Animal),
      names_to = c(".value", "time_sample", "status"),
      names_pattern = "(.*)_(\\d+)_(.*)$"
    )
  
  Faktoren_prdctn_calc_NonDairy <- df |> 
    select(c(Year, matches("^NonDairy.*"))) |> 
    mutate(
      Animal = rep("Non-dairy cattle", n()), .after = Year
    ) |> 
    rename_with(
      .fn = ~ substring(.x, 10),
      .cols = -c(Year, Animal) 
    ) |> 
    pivot_longer(
      cols =  -c(Year, Animal),
      names_to = c(".value", "time_sample", "status"), # ".value" sorgt dafür, dass man nicht noch ne value spalte brauch, sondern die Variablen neben einander auftauchen sollen
      names_pattern = "(.*)_(\\d+)_(.*)$" # dabei setzen die () was übernommen ewrden soll -> \\d bedeutet eine einstellige Zahl, \\d+ auch mehrstellige Zahl
    ) ## die korrekte zuordnung erfolgt über die Reihenfolge in names_to
  
  For_Calc_table <- rbind(Faktoren_prdctn_calc_Dairy, Faktoren_prdctn_calc_NonDairy)
  
  #### Jetzt tabelle für Calculation vorbereiten
  
  Table_CH4_calculated_withfactors <- For_Calc_table |> 
    transmute(
      Year,
      Animal,
      time_sample,
      status,
      Pop_in_1000s,
      Red_in_perc = Ym_upd * 100,
      Ym_org_in_perc = Ym_in_perc,
      Ym_upd_in_perc = Ym_in_perc * (1 - Ym_upd),
      DE_org_in_perc = DE_in_perc,
      DE_upd_in_perc = DE_in_perc + (Ym_in_perc * Ym_upd),
      NEs_total
    ) |> # GE_upd_function_Nes_known berechnet einfach GE -> also nicht nur GE_upd -> schlecht benannt
    mutate(
      GE_org_in_MJ_per_head_per_day = GE_upd_function_NEs_known(NEs = NEs_total, 
                                                                DE_upd = DE_org_in_perc),
      GE_upd_in_MJ_per_head_per_day = GE_upd_function_NEs_known(NEs = NEs_total, 
                                                                DE_upd = DE_upd_in_perc),  .after = NEs_total
    ) |> 
    mutate(
      EnterFe_CH4_emis_org_in_kt = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                             Ym_t = Ym_org_in_perc, 
                                                             N_t_in_1000s = Pop_in_1000s), 
      EnterFe_CH4_emis_upd_in_kt_Ym = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                                Ym_t = Ym_upd_in_perc, 
                                                                N_t_in_1000s = Pop_in_1000s),
      EnterFe_CH4_emis_upd_in_kt_Ym_DE = emission_total_per_animal(GE = GE_upd_in_MJ_per_head_per_day, 
                                                                   Ym_t = Ym_upd_in_perc, 
                                                                   N_t_in_1000s = Pop_in_1000s)) 
  
  Table_CH4_calculated_withfactors_for_cat <- Table_CH4_calculated_withfactors |> 
    group_by(Year, time_sample, status, Red_in_perc) |>  # alles groupen, was bei summarise am Ende noch als einzelne Spalte übrig bleiben soll
    summarise(
      Ym_org_in_perc = weighted.mean(x =  Ym_org_in_perc, w = Pop_in_1000s),
      Ym_upd_in_perc = weighted.mean(x =  Ym_upd_in_perc, w = Pop_in_1000s),
      DE_org_in_perc = weighted.mean(x =  DE_org_in_perc, w = Pop_in_1000s),
      DE_upd_in_perc = weighted.mean(x =  DE_upd_in_perc, w = Pop_in_1000s),
      NEs_total = weighted.mean(x = NEs_total, w = Pop_in_1000s),
      GE_org_in_MJ_per_head_per_day = weighted.mean(x = GE_org_in_MJ_per_head_per_day, w = Pop_in_1000s),
      GE_upd_in_MJ_per_head_per_day = weighted.mean(x = GE_upd_in_MJ_per_head_per_day, w = Pop_in_1000s),
      EnterFe_CH4_emis_org_in_kt = sum(EnterFe_CH4_emis_org_in_kt),
      EnterFe_CH4_emis_upd_in_kt_Ym = sum(EnterFe_CH4_emis_upd_in_kt_Ym),
      EnterFe_CH4_emis_upd_in_kt_Ym_DE = sum(EnterFe_CH4_emis_upd_in_kt_Ym_DE),
      Pop_in_1000s = sum(Pop_in_1000s), # muss als letztes, weil summarize sequentiell arbeitet,
      # bei den weighted mean würde sonst falscher Wert für gewicht genommen werden.
      .groups = "drop"
    ) |> 
    relocate(Pop_in_1000s, .after = Red_in_perc) |> 
    mutate(
      Animal = rep("Cattle", n()), .after = Year)
    
  Calc_base_table_plus_avar_cattle <- bind_rows(Table_CH4_calculated_withfactors, Table_CH4_calculated_withfactors_for_cat)
    
    
  return(Calc_base_table_plus_avar_cattle)
}
