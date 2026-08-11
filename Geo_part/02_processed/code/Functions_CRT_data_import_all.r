##### Hier werden alle Funktionen gelistet für den Daten-import aus CRT -> um in anderen Skripten drauf zu greifen zu können

###### Für Option A wie in DEU oder NZL


CRT_Country_Year_function <- function(Y, C){
  
  # 1. Schritt: Namensstruktur der Tabellendaten klären und Variable Y (Year) einführen
  
  table_strc <- paste0("CRT_", C, "_", Y, ".xlsx") #Pasteo fügt alles als ein String zusammen ohne Leerzeichen -> ergibt dann Datein Name
  
  # 2. Schritt: Import 1.Teil der Tabelle
  CRT_C_Y <- import(
    file = here("01_Data/Tabellen/", table_strc),
    setclass = "tibble",
    sheet = "Table3.A",
    range = "B8:G13"
  ) |> # Jetzt erfolgt der umbau der 1. Teil-Tabelle
    rename( Animal = ...1, 
            Population_size_1000s = `Population size (1)`, 
            ø_GE_MJ_per_head_per_day = `Average gross energy intake (GE)`, 
            Ym_in_percent = `Average CH4 conversion rate (Ym) (2)`,
            CH4_emission_in_kg_CH4_per_head_per_year = CH4...5, 
            Ch4_emission_in_kt = CH4...6) |> 
    mutate(Animal = recode(Animal,
                           "3.A.1. Cattle" = "Cattle",
                           "3.A.1.a. Dairy cattle" = "Dairy cattle",
                           "3.A.1.b. Non-dairy cattle" = "Non-dairy cattle"
    )) |> 
    drop_na(c(Animal, Population_size_1000s)) |> 
    mutate(across(-c(Animal), as.numeric)) |> 
    mutate(Year = rep(Y, n()), .after = Animal )
  
  # 3. Schritt: 2. Teil-Tabelle importieren und verarbeiten
  CRT_C_AI_Y <- import(
    file = here("01_Data/Tabellen/", table_strc),
    setclass = "tibble",
    sheet = "Table3.A",
    range = "I7:L15") |> 
    rename(
      Indikator = `Disaggregated list of animals (b)`, # (neuer Name = alter Name)
      Unit = ...2 ) |> 
    mutate(Indikator = recode(Indikator,
                              "Weight" = "Weight_kg",
                              "Milk yield" = "Milk_yield_kg_per_day",
                              "Pregnant" = "Pregnant_in_percent",
                              "Digestibility of feed" = "Digestibility_of_feed_in_percent",
                              "Work" = "Work_h_per_day"
    )
    ) |> 
    select(-Unit) |> 
    mutate(across( # across(Spalten, funktion)
      c(`Dairy cattle`, `Non-dairy cattle`), ~ na_if(.x, "NA"))
    ) |>  
    mutate(across(c(`Dairy cattle`, `Non-dairy cattle`),
                  ~ na_if(.x, "NE") # ~ -> minifunktion
    )
    ) |> 
    mutate(across(
      -c(Indikator), as.numeric
    )) |> 
    pivot_longer(-Indikator, names_to = "Animal", values_to = "Wert") |> 
    pivot_wider(names_from = Indikator, values_from = Wert) |> 
    select(-c(`NA`, `Indicators:`, `Feeding situation(c)`))
  
  # 4. Schritt: Tabellen zusammenfügen
  
  CRT_DEU_Y_all <- left_join(CRT_C_Y, CRT_C_AI_Y, by = "Animal")
  
  # 5. schritt: Na bei cattle anpassen für die relevanten Spalten - Ym, Ge, De - über gewichtetes Mittel aus Dairy und Non-dairy cattle
  
  weighted_vals <- CRT_DEU_Y_all |> 
    filter(Animal %in% c("Dairy cattle", "Non-dairy cattle")) |> 
    summarise(
      GE_w = weighted.mean(ø_GE_MJ_per_head_per_day, w = Population_size_1000s, na.rm = TRUE),
      Ym_w = weighted.mean(Ym_in_percent, w = Population_size_1000s, na.rm = TRUE),
      DE_w = weighted.mean(Digestibility_of_feed_in_percent, w = Population_size_1000s, na.rm = TRUE),
      Preg_w = weighted.mean(Pregnant_in_percent, w = Population_size_1000s, na.rm = TRUE),
      Weight_w = weighted.mean(Weight_kg, w = Population_size_1000s, na.rm = TRUE),
      Milk_sum = sum(Milk_yield_kg_per_day, na.rm = TRUE) # ohne na.rm = TRUE berechnet er nix, bzw. setzt alles auf NA
    )
  
  CRT_DEU_Y_all <- CRT_DEU_Y_all |> 
    mutate(
      ø_GE_MJ_per_head_per_day = if_else( #if_else(condition, true, false)
        Animal == "Cattle" & is.na(ø_GE_MJ_per_head_per_day), 
        weighted_vals$GE_w, 
        ø_GE_MJ_per_head_per_day
      ),
      Ym_in_percent = if_else(
        Animal == "Cattle" & is.na(Ym_in_percent), 
        weighted_vals$Ym_w, 
        Ym_in_percent
      ),
      Digestibility_of_feed_in_percent = if_else(
        Animal == "Cattle" & is.na(Digestibility_of_feed_in_percent), 
        weighted_vals$DE_w, 
        Digestibility_of_feed_in_percent
      ),
      Weight_kg = if_else(
        Animal == "Cattle" & is.na(Weight_kg),
        weighted_vals$Weight_w,
        Weight_kg
      ),
      Pregnant_in_percent = if_else(
        Animal == "Cattle" & is.na(Pregnant_in_percent),
        weighted_vals$Preg_w,
        Pregnant_in_percent
      ),
      Milk_yield_kg_per_day = if_else(
        Animal == "Cattle" & is.na(Milk_yield_kg_per_day),
        weighted_vals$Milk_sum,
        Milk_yield_kg_per_day
      )
    )
  
  return(CRT_DEU_Y_all)
}

#############################
###########
##########
#### Umbau von der Option B zu Option A -> damitvergleichbar zu den anderen Tabellen + cattle Werte über gewichtetes Mittel berechnen, wenn notwendig
#########
##########
###############################


Opt_B_to_Opt_A_function <- function(CC, YE){
  
  # 1. Schritt: Tabellenstruktur operationalisieren, um verschiedene Tabellen einlesen zu können
  tabl_strc <- paste0("CRT_", CC, "_1990_", YE,"_all_subcat.csv")
  
  # 2. Schritt: einlesen der Tabelle
  CRT_CC_1990_YE_all_subcat <- read.csv(here("02_processed/CRT/", tabl_strc))
  
  # 3. Schritt wichtige Vektoren und Hilfstabellen bauen, um späteren Umbau der Tabelle zu Opt A zu ermöglichen
  
  ## Vektoren für die Subkategorien von Dairy und Non-dairy (beef) cattle erstellen
  
  Dairy_Cattle_vec <- c("Dairy cows", "Dairy replacements", "Dairy calves")
  
  Non_dairy_cattle_vec <- setdiff(unique(CRT_CC_1990_YE_all_subcat$Animal), c(Dairy_Cattle_vec, "Cattle" )) #setdiff(x,y)
  
  ## Hilfstabelle um Pregnant zu korrigieren
  
  pop_lookup <- CRT_CC_1990_YE_all_subcat |> 
    filter(Animal %in% c("Dairy cows", "Beef cows")) |> 
    select(Year, Animal, Population_size_1000s) |> 
    pivot_wider(names_from = Animal, 
                values_from = Population_size_1000s, 
                names_prefix = "Pop_") #damit bekomme ich spalten mit den Namen der Tiere, vorher nur Zeilennamen mit Tiername
  
  rate_cols <- c("ø_GE_MJ_per_head_per_day", "Ym_in_percent", 
                 "CH4_emission_in_kg_CH4_per_head_per_year", 
                 "Weight_kg", "Digestibility_of_feed_in_percent") # da nicht alle spalten gleich aufsummiert werden sollen -> bei rate_cols sollte gewichtetes Mittelwert berechnet werden -> pregnant kann hierüber aber nicht berechnet werden, da bei gewichtetem mittelwert die Zeilen mit Na komplett rausgenommen werden -> d.h. hier würde % Pregnant auf komplett dairy cow übernommen werden pbwohl nur kleinerer Teil eigtl in dem angegebenem Prozentsatz schwange ist
  
  na_cols <- c("Pregnant_in_percent", 
               "Work_h_per_day") # hier kann man nicht einfach aufsummieren oder weighted mean nehmen, da NAs enthalten und muss eigtl um den Anteil korrigiert werden und nicht einfach aufs ganze bezogen werden
  
  # 4. Schritt: Tabelle umbauen und Subcategorien mit den Werten zu dairy und non-dairy zusammenfassen
  
  CRT_CC_1990_YE_all <- CRT_CC_1990_YE_all_subcat |> 
    mutate(Animal = case_when( 
      # hier werden einfach alle umgeschrieben in Dairy cattle die zu dairy cattle zusammengefasst weren sollen
      Animal %in% Dairy_Cattle_vec ~ "Dairy cattle",
      Animal %in% Non_dairy_cattle_vec ~ "Non-dairy cattle",
      Animal %in% "Cattle" ~ "Cattle",
      TRUE ~ "Sonstige"
    )
    ) |> 
    group_by(Year, Animal) |> 
    summarise(across(all_of(rate_cols), ~ weighted.mean(.x, w = Population_size_1000s, na.rm = TRUE)),
              across(where(is.numeric) & !all_of(rate_cols), ~ sum(.x, na.rm = TRUE)), # lasse Pregnant doch aufsummieren, um im nächsten schritt die Spalte zu korrigieren
              .groups = "drop"
    )  |> 
    left_join(pop_lookup, by = "Year") |>          # verbindet über Jahr, nicht über Position!
    mutate(
      Pregnant_in_percent = case_when(
        Animal == "Dairy cattle" ~ Pregnant_in_percent * `Pop_Dairy cows` / Population_size_1000s,
        Animal == "Non-dairy cattle" ~ Pregnant_in_percent * `Pop_Beef cows` / Population_size_1000s,
        TRUE ~ Pregnant_in_percent
      )
    ) |> 
    select(-starts_with("Pop_"))  # Hilfsspalten wieder entfernen
  
  # Schritt 5: Cattle wird jetzt angepasst und fehlende Werte berechnet
  
  # hier kann nicht wie vorher gerechnet werden, da oben nur ein Jahr verwendet wird, hier aber mehrere 
  #-> muss erst groupen, sonst macht summarise aus allen Jahren eine Spalte
  # dann kann man wieder joinen und weiterarbeiten
  
  weighted_vals <- CRT_CC_1990_YE_all |> 
    filter(Animal %in% c("Dairy cattle", "Non-dairy cattle")) |> 
    group_by(Year) |>                                          # <- FIX: pro Jahr!
    summarise(
      GE_w = weighted.mean(ø_GE_MJ_per_head_per_day, w = Population_size_1000s, na.rm = TRUE),
      Ym_w = weighted.mean(Ym_in_percent, w = Population_size_1000s, na.rm = TRUE),
      DE_w = weighted.mean(Digestibility_of_feed_in_percent, w = Population_size_1000s, na.rm = TRUE),
      Preg_w = weighted.mean(Pregnant_in_percent, w = Population_size_1000s, na.rm = TRUE),
      Weight_w = weighted.mean(Weight_kg, w = Population_size_1000s, na.rm = TRUE),
      Milk_sum = sum(Milk_yield_kg_per_day, na.rm = TRUE),
      .groups = "drop"
    )
  
  CRT_CC_1990_YE_all <- CRT_CC_1990_YE_all |> 
    left_join(weighted_vals, by = "Year") |>                  
    mutate(
      ø_GE_MJ_per_head_per_day = if_else(
        Animal == "Cattle" & is.na(ø_GE_MJ_per_head_per_day), GE_w, ø_GE_MJ_per_head_per_day),
      Ym_in_percent = if_else(
        Animal == "Cattle" & is.na(Ym_in_percent), Ym_w, Ym_in_percent),
      Digestibility_of_feed_in_percent = if_else(
        Animal == "Cattle" & is.na(Digestibility_of_feed_in_percent), DE_w, Digestibility_of_feed_in_percent),
      Weight_kg = if_else(
        Animal == "Cattle" & is.na(Weight_kg), Weight_w, Weight_kg),
      Pregnant_in_percent = if_else(
        Animal == "Cattle" , Preg_w, Pregnant_in_percent),
      Milk_yield_kg_per_day = if_else(
        Animal == "Cattle", Milk_sum, Milk_yield_kg_per_day)
    ) |> 
    select(-c(GE_w, Ym_w, DE_w, Preg_w, Weight_w, Milk_sum))     # Hilfsspalten entfernen
  
  return(CRT_CC_1990_YE_all)
}


#############
### Für die EU -> auch Option A, aber deutlich weniger Daten
############

CRT_EUR_data_all_function <- function(Y){
  
  # 1. Schritt: Namensstruktur der Tabellendaten klären und Variable Y (Year) einführen
  
  table_strc <- paste0("CRT_EUR_", Y, ".xlsx") #Pasteo fügt alles als ein String zusammen ohne Leerzeichen -> ergibt dann Datein Name
  
  # 2. Schritt: Import 1.Teil der Tabelle
  CRT_EUR_Y <- import(
    file = here("01_Data/Tabellen/", table_strc),
    setclass = "tibble",
    sheet = "Table3.A",
    range = "B8:G13") |> 
    rename(Animal = ...1, 
           Population_size_1000s = `Population size (1)`, 
           ø_GE_MJ_per_head_per_day = `Average gross energy intake (GE)`, 
           Ym_in_percent = `Average CH4 conversion rate (Ym) (2)`,
           CH4_emission_in_kg_CH4_per_head_per_year = CH4...5, 
           Ch4_emission_in_kt = CH4...6) |> 
    mutate(Animal = recode(Animal,
                           "3.A.1. Cattle" = "Cattle",
                           "3.A.1.a. Dairy cattle" = "Dairy cattle",
                           "3.A.1.b. Non-dairy cattle" = "Non-dairy cattle",
                           "Option A:" = "NA"
    )) |> 
    mutate(across(Animal, ~ na_if(.x, "NA"))) |> 
    drop_na(Animal) |> 
    mutate(across(-Animal, as.numeric)) |> 
    mutate(Year = rep(Y, n()), .after = "Animal")
  
  # 3. Schritt: AI importieren
  CRT_EUR_Y_AI <- import(
    file = here("01_Data/Tabellen/", table_strc),
    setclass = "tibble",
    sheet = "Table3.A",
    range = "I7:L15") |> 
    rename(Indikator = `Disaggregated list of animals (b)`, # (neuer Name = alter Name)
           Unit = ...2 ) |> 
    mutate(Indikator = recode(Indikator,
                              "Indicators:" = "NA", # ("alter Name" = "neuer NAme")
                              "Weight" = "Weight_kg",
                              "Feeding situation(c)" = "Feeding_situation",
                              "Milk yield" = "Milk_yield_kg_per_day",
                              "Work" =   "Work_h_per_day",
                              "Pregnant" = "Pregnant_in_percent",
                              "Digestibility of feed" = "Digestibility_of_feed_in_percent")) |> 
    mutate(across(Indikator, ~ na_if(.x, "NA"))) |> 
    drop_na(Indikator) |> 
    select(-Unit) |> 
    pivot_longer(-Indikator, names_to = "Animal", values_to = "Wert") |> 
    pivot_wider(names_from = Indikator, values_from = Wert) |> 
    mutate(across(-Animal, as.numeric))
  
  # 4. Schritt: Tabellen zusammenfügen
  left_join(CRT_EUR_Y, CRT_EUR_Y_AI, by = "Animal")
}

##################
#### Für Option B wie bei der USA
################

CRT_Country_Year_function_OP_B <- function(Y, C){
  
  # 1. Schritt: Namensstruktur der Tabellendaten klären und Variable Y (Year) einführen
  
  table_strc <- paste0("CRT_", C, "_", Y, ".xlsx") #Pasteo fügt alles als ein String zusammen ohne Leerzeichen -> ergibt dann Datein Name
  
  # 2. Schritt: Import 1.Teil der Tabelle
  CRT_C_Y <-  import(
    file = here("01_Data/Tabellen/", table_strc),
    setclass = "tibble",
    sheet = "Table3.A",
    range = "B8:G26")  |> 
    rename( Animal = ...1, 
            Population_size_1000s = `Population size (1)`, 
            ø_GE_MJ_per_head_per_day = `Average gross energy intake (GE)`, 
            Ym_in_percent = `Average CH4 conversion rate (Ym) (2)`,
            CH4_emission_in_kg_CH4_per_head_per_year = CH4...5, 
            Ch4_emission_in_kt = CH4...6) |> 
    mutate(Animal = recode(Animal,
                           "3.A.1. Cattle" = "Cattle",
                           "3.A.1.a. Dairy cattle" = "NA",
                           "3.A.1.b. Non-dairy cattle" = "NA",
                           "3.A.1.a.iv. Other (please specify)" = "NA", 
                           "3.A.1.a. Other" = "NA",
                           "Option B (country-specific): (3)" = "NA")
    ) |> 
    mutate(across(, ~ na_if(.x, "NA"))) |> 
    mutate(across(, ~ na_if(.x, "IE"))) |> 
    mutate(across(-c(Animal), as.numeric)) |> 
    drop_na(Animal, Population_size_1000s) |> 
    mutate( Year = rep(Y, n()), .after = Animal)
  
  # 3. Schritt: AI importieren
  
  CRT_C_Y_AI <- import(
    file= here("01_Data/Tabellen/", table_strc),
    sheet = "Table3.A",
    setclass = "tibble",
    range = "I7:W15"
  ) |> 
    rename(Indikator = `Disaggregated list of animals (b)`, # (neuer Name = alter Name)
           Unit = ...2 ) |> 
    mutate(Indikator = recode(Indikator,
                              "Indicators:" = "NA",
                              "Weight" = "Weight_kg",
                              "Feeding situation(c)" = "Feeding_situation",
                              "Milk yield" = "Milk_yield_kg_per_day",
                              "Work" =   "Work_h_per_day",
                              "Pregnant" = "Pregnant_in_percent",
                              "Digestibility of feed" = "Digestibility_of_feed_in_percent")) |> 
    select(-c(Unit, "Dairy cattle", "Non-dairy cattle")) |> 
    mutate(across(, ~na_if(.x, "NA"))) |> 
    mutate(across(, ~na_if(.x, "IE"))) |> 
    mutate(across(, ~na_if(.x, "NO"))) |> 
    drop_na(Indikator) |> 
    pivot_longer(-Indikator, names_to = "Animal", values_to = "Wert") |> 
    pivot_wider(names_from = Indikator, values_from = Wert) |> 
    mutate(across(-c(Animal, Feeding_situation), as.numeric))
  
  # 4. Schritt: Tabellen zusammenführen
  
  CRT_C_Y_final <- left_join(CRT_C_Y, CRT_C_Y_AI, by = "Animal")
  
  return(CRT_C_Y_final)
}
