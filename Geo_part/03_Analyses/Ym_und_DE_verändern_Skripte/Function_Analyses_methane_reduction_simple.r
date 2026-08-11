###############
###### Funktion für simple Methanreduktion bauen
##############


##### Allgemein - Packages needed


#install.packages("rio")
#install.packages("janitor")

library("rio")
library("here")
library("readxl")
library("tidyr")
library("dplyr")
library("ggplot2")
library("viridis")
library("janitor")
library("purrr") # für map Befehl

here()

#############
##########

##### Funktion für die Berechnung des veränderten CH4-Ausstoßes wenn nur Ym verändert wird

###########
###########

CH4_emission_Ym_function <- function(Ym_upd, CC, YE){
  
  tabl_strc <- paste0("CRT_", CC, "_1990_", YE, "_all.csv")
  CRT_CC_1990_YE_all <- read.csv(here("02_processed/CRT/", tabl_strc))
  
  CRT_CC_1990_YE_all_upd_CH4_Ym <- CRT_CC_1990_YE_all |> 
    transmute(
      Animal, Year, Population_size_1000s,
      Red_in_percent = Ym_upd * 100,
      Ym_org_in_percent = Ym_in_percent,
      Ym_upd_in_percent = Ym_in_percent * (1 - Ym_upd),
      GE_org_in_MJ_per_head_per_day = ø_GE_MJ_per_head_per_day,     # <- ergänzt
      Ch4_emission_org_in_kt_CRT = Ch4_emission_in_kt
    ) |> 
    mutate(
      Ch4_emission_org_in_kt = emission_total_per_animal(
        GE = GE_org_in_MJ_per_head_per_day, 
        Ym_t = Ym_org_in_percent,
        N_t_in_1000s = Population_size_1000s),
      Ch4_emission_upd_in_kt_Ym = emission_total_per_animal(
        GE = GE_org_in_MJ_per_head_per_day, 
        Ym_t = Ym_upd_in_percent,
        N_t_in_1000s = Population_size_1000s),
      .after = Ch4_emission_org_in_kt_CRT
    )
  
  return(CRT_CC_1990_YE_all_upd_CH4_Ym)
}

#CRT_DEU_1990_2024_CH4_reduced_Ym_1_50 <- map_dfr(.x = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5),
 #                                                .f = CH4_emission_Ym_function,
  #                                               CC = "DEU",
   #                                              YE = "2024")

# funktioniert :)


#################
################
###############

## Nun für mit einbezogene Veränderung in DE mit einberechnen

# unter der Annahme das de Tiere die wachsen vernachlässigbar klein sind - sonst könnte ich es aus meinen Werten heraus nicht berechnen

################
###############
###############

###### Zunächst die einzelnen Formeln als Funkton einführen für REM und GE (bzw. hier muss berechnet werden, was der Wert für die NEs ist)


REM_FUNCTION <- function(DE){
  
  REM <- (1.123 - (4.092*10^(-3)*DE) + (1.126 * 10^(-5)*(DE)^2) - (25.4/DE))
 
  return(REM) 
}


NEs_function <- function(GE, DE){
  
  NEs <- GE*(DE/100)*REM_FUNCTION(DE = DE)
  
  return(NEs)
}

GE_upd_function <- function(GE_org, DE_org, DE_upd){
  
  GE_upd <- (NEs_function(GE = GE_org, DE = DE_org)/REM_FUNCTION(DE = DE_upd))/(DE_upd/100)
  
  return(GE_upd)
}

GE_upd_function_NEs_known <- function(NEs, DE_upd){
  
  GE_upd <- (NEs / REM_FUNCTION(DE = DE_upd))/(DE_upd/100)
  
  return(GE_upd)
}


# so erhalte ich jetzt mit Ge_upd_function geupdatete Gross energy wenn ich davon ausgehe,
## dass die für Methan verwendete Energie bei der Kuh ankommt.


### Formeln um mit Ym und GE nun Emission zu berechen

emission_total_per_animal <- function(GE, Ym_t, N_t_in_1000s){
  EF_t <- ((GE*(Ym_t/100)*365)/55.65)
  emission_CH4 <- (EF_t * (N_t_in_1000s*1000 / 10^6)) 
  return(emission_CH4)}

Ef_t_function <- function(GE, Ym_t){
  EF_t <- ((GE*(Ym_t/100)*365)/55.65)
  return(EF_t)}

################
###########
#### Funktion bauen, um das direkt in einer Tabele zu haben
###########
################

CH4_emission_Ym_DE_function <- function(Ym_upd, CC, YE){
  # 1. Schritt: Tabellenstruktur operationalisieren, um verschiedene Tabellen einlesen zu können
  tabl_strc <- paste0("CRT_", CC, "_1990_", YE,"_all.csv")
  
  # 2. Schritt: einlesen der Tabelle
  CRT_CC_1990_YE_all <- read.csv(here("02_processed/CRT/", tabl_strc))
  
  # 3. Schritt: Neue Tabelle erstellen mit den neuen Werten
  
  CRT_CC_1990_YE_all_upd_CH4_Ym_DE <- CRT_CC_1990_YE_all |> 
    transmute( # transmute übernimmt nur Spalten die explizit genannt werden
      Animal,
      Year,
      Population_size_1000s,
      Red_in_perc = Ym_upd * 100,
      Ym_org_in_perc = Ym_in_percent,
      Ym_upd_in_perc = Ym_in_percent * (1 - Ym_upd),
      DE_org_in_perc = Digestibility_of_feed_in_percent,
      DE_upd_in_perc = Digestibility_of_feed_in_percent + (Ym_in_percent * Ym_upd),
      GE_org_in_MJ_per_head_per_day  = ø_GE_MJ_per_head_per_day,
      Ch4_emission_org_in_kt_CRT = Ch4_emission_in_kt
    ) |> #jetzt müsste alles in der Tabelle sein, um die weiteren Emissionen zu berechnen über die oben niedergeschriebenen Funktionen
  mutate(
   GE_upd_in_MJ_per_head_per_day = GE_upd_function(GE_org = GE_org_in_MJ_per_head_per_day, 
                                                   DE_org = DE_org_in_perc, 
                                                   DE_upd = DE_upd_in_perc),  .after = GE_org_in_MJ_per_head_per_day
  ) |> # jetzt sind alle Daten in der Tabelle um die neue Emission zu berechnen für Ym
    
    mutate(
      Ch4_emission_org_in_kt = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                         Ym_t = Ym_org_in_perc,
                                                         N_t_in_1000s = Population_size_1000s),
      Ch4_emission_upd_in_kt_Ym = emission_total_per_animal(GE = GE_org_in_MJ_per_head_per_day, 
                                                         Ym_t = Ym_upd_in_perc,
                                                         N_t_in_1000s = Population_size_1000s),
       .after = Ch4_emission_org_in_kt_CRT
    ) |> # Jetzt für Ym und DE
    mutate(
      CH4_emis_upd_in_kt_Ym_DE = emission_total_per_animal(GE = GE_upd_in_MJ_per_head_per_day, 
                                                           Ym_t = Ym_upd_in_perc, 
                                                           N_t_in_1000s = Population_size_1000s)) 
  
  ### Problem: Cattle Emissionen weichen von sum(Dairy und Non-Dairy) ab, da kein linearer Zusammenhang mehr 
  #-> neu berechnen und korrigieren
  
  result_cattle_summed <- CRT_CC_1990_YE_all_upd_CH4_Ym_DE |> 
    filter(Animal %in% c("Dairy cattle", "Non-dairy cattle")) |> 
    group_by(Year, Red_in_perc) |> 
    summarise(
      Ym_org_in_perc = weighted.mean(x =  Ym_org_in_perc, w = Population_size_1000s),
      Ym_upd_in_perc = weighted.mean(x =  Ym_upd_in_perc, w = Population_size_1000s),
      DE_org_in_perc = weighted.mean(x =  DE_org_in_perc, w = Population_size_1000s),
      DE_upd_in_perc = weighted.mean(x =  DE_upd_in_perc, w = Population_size_1000s),
      Ch4_emission_org_in_kt_CRT = sum(Ch4_emission_org_in_kt_CRT),
      Ch4_emission_org_in_kt = sum(Ch4_emission_org_in_kt),
      Ch4_emission_upd_in_kt_Ym = sum(Ch4_emission_upd_in_kt_Ym),
      GE_org_in_MJ_per_head_per_day = weighted.mean(x = GE_org_in_MJ_per_head_per_day, w = Population_size_1000s),
      GE_upd_in_MJ_per_head_per_day = weighted.mean(x = GE_upd_in_MJ_per_head_per_day, w = Population_size_1000s),
      CH4_emis_upd_in_kt_Ym_DE = sum(CH4_emis_upd_in_kt_Ym_DE),
      Population_size_1000s = sum(Population_size_1000s), # muss als letztes, weil summarize sequentiell arbeitet,
      # bei den weighted mean würde sonst falscher Wert für gewicht genommen werden.
      .groups = "drop"
    ) |> 
    mutate(
      Animal = "Cattle"
    ) |> 
    relocate(Population_size_1000s, .after = Year)
    
  ### jetzt cattle daten rausschmeißen und durch neu berechnete ersetzen
  
    result_final_cattle <- CRT_CC_1990_YE_all_upd_CH4_Ym_DE |>
    filter(Animal != "Cattle") |> 
    bind_rows(result_cattle_summed) |> 
    arrange(Year, 
            factor(Animal, levels = c("Cattle", "Dairy cattle", "Non-dairy cattle")) 
            )
  # arrange, damit Cattle nicht einfach unten an die Tabelle angehängt wird
    
    result_final <- result_final_cattle |> 
      mutate(
        CH4_Ym_in_kt_diff = Ch4_emission_org_in_kt - Ch4_emission_upd_in_kt_Ym, .after = Ch4_emission_upd_in_kt_Ym
      ) |> 
      mutate(
        CH4_Ym_De_in_kt_diff = Ch4_emission_org_in_kt - CH4_emis_upd_in_kt_Ym_DE, .after = CH4_emis_upd_in_kt_Ym_DE
      ) |> 
      mutate(
        CH4_Ym_and_Ym_DE_in_kt_diff = Ch4_emission_upd_in_kt_Ym - CH4_emis_upd_in_kt_Ym_DE, .after = CH4_Ym_De_in_kt_diff
        )
  
   return(result_final)
  
}

#test_result_1 <- CH4_emission_Ym_DE_function(Ym_upd = 0.1, CC = "DEU", YE = "2024")
#glimpse(test_result_1)
#View(test_result_1)

# funktoiniert

