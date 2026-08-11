##################
###### Funktionen für die processed Tabellen, um mit denen weiter zuarbeiten
###################


####
###
########### Allgemeine Funktionen für die verschiedenen Gleichungen
###
###


emission_total_per_animal <- function(GE, Ym_t, N_t){
  EF_t <- ((GE*(Ym_t/100)*365)/55.65)
  emission_CH4 <- (EF_t * (N_t / 10^6)) 
  return(emission_CH4)}

Ef_t_function <- function(GE, Ym_t){
  EF_t <- ((GE*(Ym_t/100)*365)/55.65)
  return(EF_t)}



############## braucht man als Funktion, da die Tabelle getrennt werden muss temporär, 
#um zu verhinden soviele leere, unnötige Reihen zu erzeugen 
#-> könnte dies evtl beheben durch schlauere Grundstruktur der Tabelle
##### 
animal_filter_function <- function(Anml, df){ 
  Anml_gefiltert <- df |> 
    filter(Animal == Anml)
  return(Anml_gefiltert)
}
############

########
########
################## Hauptfunktion für option A Tabellen
########
########

Emission_CH4_calculation_function <- function(CC, YE){
  
  # 1. Schritt: Tabellenstruktur operationalisieren, um verschiedene Tabellen einlesen zu können
  tabl_strc <- paste0("CRT_", CC, "_1990_", YE,"_all.csv")
  
  # 2. Schritt: einlesen der Tabelle
  CRT_CC_1990_YE_all <- read.csv(here("02_processed/CRT/", tabl_strc))
  
  # 3. Schritt: Trennung der Tabelle in dairy & non-dairy
  Dairy <- animal_filter_function(Anml = "Dairy cattle", df = CRT_CC_1990_YE_all)
  Non_dairy <- animal_filter_function(Anml = "Non-dairy cattle", df = CRT_CC_1990_YE_all)
  
  # 4. Schritt: Neue Tabelle bauen mit berechneten Werten
  
  Emission_CH4_CC <- tibble(
    Year = Dairy$Year,
    
    EF_DC = Ef_t_function(
      GE = Dairy$ø_GE_MJ_per_head_per_day, 
      Ym_t = Dairy$Ym_in_percent
    ),
    
    Dairy_cattle_CH4_in_Gg = emission_total_per_animal(
      GE = Dairy$ø_GE_MJ_per_head_per_day, 
      Ym_t = Dairy$Ym_in_percent, 
      N_t = (Dairy$Population_size_1000s * 1000)),
    
    EF_NDC = Ef_t_function(GE = Non_dairy$ø_GE_MJ_per_head_per_day,
                           Ym_t = Non_dairy$Ym_in_percent
    ),
    
    Non_dairy_CH4_in_Gg = emission_total_per_animal(
      GE = Non_dairy$ø_GE_MJ_per_head_per_day,
      Ym_t = Non_dairy$Ym_in_percent,
      N_t = (Non_dairy$Population_size_1000s * 1000)
    )) |>
    mutate(Cattle_CH4_in_Gg = Dairy_cattle_CH4_in_Gg + Non_dairy_CH4_in_Gg) |> 
    
    mutate( Country = rep(CC, n()),
            .after = Year)
  
  return(Emission_CH4_CC)
}
