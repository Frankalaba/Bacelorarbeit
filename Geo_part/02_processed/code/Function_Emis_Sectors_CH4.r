############# Funktion für den Import der Summary Tabelle bauen.

rm(list = ls()) # enviroment säubern

#install.packages("rio")
#install.packages("janitor")
#install.packages("tidyverse")
#install.packages("ggpmisc")
#install.packages("patchwork")
#install.packages("scales")


library("rio")
library("here")
library("readxl")
library("tidyr")
library("dplyr")
library("ggplot2")
library("ggtext")
library("viridis")
library("janitor")
library("purrr") # für map Befehl
library("tidyverse")
library("ggpmisc")
library("patchwork")
library("scales")

here()

#################### Test wieder für DE

#DEU_test_smry  <- import(file = here("01_Data/Tabellen", "CRT_DEU_2024.xlsx"),  # import von rio erkennt Dateityp selber 
#                         # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
##                             sheet = "Table10s3",
#                             range = "B8:AN59",
#                             setclass = "tibble") |> 
#  select(-c("Reference year/period for NDC (1)", "Base year (2)", "Change from 1990 to latest reported year")) |> 
#  rename( "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
#  mutate( Emission_sectors = recode(Emission_sectors,
#    "1. Energy" = "Energy_in_kt",
#    "2.  Industrial processes and product use" = "Industrial_processes_and_product_use_in_kt",
#    "3.  Agriculture" = "Agriculture_in_kt",
#    "4. Land use, land-use change and forestry(4)" = "LULUCF_in_kt",
#    "5.  Waste" = "Waste_in_kt",
#    "6.  Other (as specified in summary 1)" = "Other_in_kt",
#    "Total CH4 emissions without LULUCF" = "Total_CH4_without_LULUCF_in_kt",
#    "Total CH4 emissions with LULUCF" = "Total_CH4__with_LULUCF_in_kt"
#    )) |> 
#  mutate(across(-c(Emission_sectors), as.numeric)) |> 
#  pivot_longer(-c(Emission_sectors), names_to = "Year", values_to = "CH4_in_kt") |> 
#  pivot_wider(names_from = Emission_sectors, values_from = CH4_in_kt) |> 
#  select(c(Year, Energy_in_kt, Industrial_processes_and_product_use_in_kt, Agriculture_in_kt, 
 #          LULUCF_in_kt, Waste_in_kt, Other_in_kt, Total_CH4_without_LULUCF_in_kt, 
#           Total_CH4_without_LULUCF_in_kt, Total_CH4__with_LULUCF_in_kt)) |> 
#  mutate( Country = rep("DEU", n())) |> 
#  relocate(Country, before = Year)

###########################
#########################
##########
#########
################################################# Daraus jetzt funktion bauen für die Übersektoren
#########
#########
##########################
############################

Import_total_CH4_Emis_SECTORS_function <- function(CC, YE){
  # 1. Schritt: Table-Structure bauen, für den folgenden Import
  table_strc <- paste0("CRT_", CC, "_", YE, ".xlsx")
  
  # 2. Schritt: Import und Umbau der Tabelle, sodass mit dieser weiter gearbeitet werden kann.
  
  Total_CH4_Emis_Sectors  <- import(file = here("01_Data/Tabellen", table_strc),  # import von rio erkennt Dateityp selber 
                           # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
                           sheet = "Table10s3",
                           range = "B8:AN59",
                           setclass = "tibble") |> 
    select(-c("Reference year/period for NDC (1)", "Base year (2)", "Change from 1990 to latest reported year")) |> 
    rename( "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
    mutate( Emission_sectors = recode(Emission_sectors,
                                      "1. Energy" = "Energy_in_kt",
                                      "2.  Industrial processes and product use" = "Industrial_processes_and_product_use_in_kt",
                                      "3.  Agriculture" = "Agriculture_in_kt",
                                      "4. Land use, land-use change and forestry(4)" = "LULUCF_in_kt",
                                      "5.  Waste" = "Waste_in_kt",
                                      "6.  Other (as specified in summary 1)" = "Other_in_kt",
                                      "Total CH4 emissions with LULUCF" = "Total_CH4_in_kt"
    )) |> 
    mutate(across(-c(Emission_sectors), as.numeric)) |> 
    pivot_longer(-c(Emission_sectors), names_to = "Year", values_to = "CH4_in_kt") |> 
    pivot_wider(names_from = Emission_sectors, values_from = CH4_in_kt) |> 
    select(c(Year, Energy_in_kt, Industrial_processes_and_product_use_in_kt, Agriculture_in_kt, 
             LULUCF_in_kt, Waste_in_kt, Other_in_kt, Total_CH4_in_kt)) |> 
    mutate( Country = rep(CC, n())) |> 
    relocate(Country, .before = Year) |> 
    mutate(across(-c(Country), as.numeric)) |> 
    drop_na(Energy_in_kt) |>  # da bei USA so gesehen zwei Spalten zu viel importiert werden, wieder entfernen
    mutate(
      Total_CH4_without_Agriculture_in_kt = Total_CH4_in_kt - Agriculture_in_kt, .before = Total_CH4_in_kt
    )
  
  
  return(Total_CH4_Emis_Sectors)
}


#### -> funktioniert

#######################
##################
#####
########################################## Nun Funktion umbauen für extraktion vom Change und alle countrys am Ende zusammen in einer Tabelle
#######
##############
##############

# test für DEU

#Total_CH4_Emis_Sectors  <- import(file = here("01_Data/Tabellen","CRT_DEU_2024.xlsx" ),  # import von rio erkennt Dateityp selber 
#                                 # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
#                                  sheet = "Table10s3",
#                                  range = "B8:AN59",
#                                  setclass = "tibble") |> 
#  rename( "Change_90_22_in_perc" = "Change from 1990 to latest reported year",
#          "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
#  select(c(Emission_sectors, Change_90_22_in_perc)) |> 
 # mutate( Emission_sectors = recode(Emission_sectors,
#                                    "1. Energy" = "Energy_in_kt",
  #                                  "2.  Industrial processes and product use" = "Industrial_processes_and_product_use_in_kt",
 #                                   "3.  Agriculture" = "Agriculture_in_kt",
   #                                 "4. Land use, land-use change and forestry(4)" = "LULUCF_in_kt",
    #                                "5.  Waste" = "Waste_in_kt",
     #                               "6.  Other (as specified in summary 1)" = "Other_in_kt",
      #                              "Total CH4 emissions without LULUCF" = "Total_CH4_without_LULUCF_in_kt",
#       #                             "Total CH4 emissions with LULUCF" = "Total_CH4__with_LULUCF_in_kt"
#  )) |> 
#  mutate(across(-c(Emission_sectors), as.numeric)) |> 
#  filter(Emission_sectors %in% c("Energy_in_kt", "Industrial_processes_and_product_use_in_kt", "Agriculture_in_kt", 
#           "LULUCF_in_kt", "Waste_in_kt", "Other_in_kt", "Total_CH4_without_LULUCF_in_kt", "Total_CH4__with_LULUCF_in_kt")) |>
#  mutate( Country = rep("DEU", n())) |> 
#  relocate(Country, .before = Change_90_22_in_perc)###
#
#glimpse(Total_CH4_Emis_Sectors)

# -> funktioniert


################
#############
###################################### Funktion bauen
##################
#################

Import_CHANGE_CH4_Emis_SECTORS_one_CC_function <- function(CC, YE){
  # 1. Schritt: Table-Structure bauen, für den folgenden Import
  table_strc <- paste0("CRT_", CC, "_", YE, ".xlsx")
  
  # 2. Schritt: Import und Umbau der Tabelle, sodass mit dieser weiter gearbeitet werden kann, 
  #wo nur der in der Ausgangstabelle bereits berechnete Change der totalen Emission übrig bleibt
  
  Change_CH4_Emis_Sectors_one_CC  <- import(file = here("01_Data/Tabellen", table_strc ),  # import von rio erkennt Dateityp selber 
                                    # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
                                    sheet = "Table10s3",
                                    range = "B8:AN59",
                                    setclass = "tibble") |> 
    rename( "Change_90_22_in_perc" = "Change from 1990 to latest reported year",
            "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
    select(c(Emission_sectors, Change_90_22_in_perc)) |> 
    mutate( Emission_sectors = recode(Emission_sectors,
                                      "1. Energy" = "Energy_in_kt",
                                      "2.  Industrial processes and product use" = "Industrial_processes_and_product_use_in_kt",
                                      "3.  Agriculture" = "Agriculture_in_kt",
                                      "4. Land use, land-use change and forestry(4)" = "LULUCF_in_kt",
                                      "5.  Waste" = "Waste_in_kt",
                                      "6.  Other (as specified in summary 1)" = "Other_in_kt",
                                      "Total CH4 emissions without LULUCF" = "Total_CH4_without_LULUCF_in_kt",
                                      "Total CH4 emissions with LULUCF" = "Total_CH4__with_LULUCF_in_kt"
    )) |> 
    mutate(across(-c(Emission_sectors), as.numeric)) |> 
    filter(Emission_sectors %in% c("Energy_in_kt", "Industrial_processes_and_product_use_in_kt", "Agriculture_in_kt", 
                                   "LULUCF_in_kt", "Waste_in_kt", "Other_in_kt", "Total_CH4_without_LULUCF_in_kt", 
                                   "Total_CH4__with_LULUCF_in_kt")) |>
    mutate( Country = rep(CC, n())) |> 
    relocate(Country, .before = Change_90_22_in_perc)
  
  return(Change_CH4_Emis_Sectors_one_CC)
}

#######################
##################
#####
########################################## Nun Funktion umbauen für Sektor Agriculture mit Subcategorien
#####
##############
##############



# Test für DEU

#  Total_CH4_Emis_Sectors  <- import(file = here("01_Data/Tabellen", "CRT_DEU_2024.xlsx"),  # import von rio erkennt Dateityp selber 
#                                    # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
#                                    sheet = "Table10s3",
#                                    range = "B8:AN59",
#                                    setclass = "tibble") |> 
#    select(-c("Reference year/period for NDC (1)", "Base year (2)", "Change from 1990 to latest reported year")) |> 
#    rename( "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
#    mutate( Emission_sectors = recode(Emission_sectors,
#                                      "3.  Agriculture" = "Agriculture_in_kt",
 #                                     "3.A.  Enteric fermentation" = "Enteric_fermentation_in_kt",
#                                      "3.B.  Manure management" = "Manure_management_in_kt",
#                                      "3.C.  Rice cultivation" = "Rice_cultivation_in_kt",
#                                      "3.D.  Agricultural soils" = "Agriculture_soils_in_kt",
#                                      "3.E.  Prescribed burning of savannahs" = "Prescribed_burning_of_savannahs_in_kt",
#                                      "3.F.  Field burning of agricultural residues"  = "Field_burning_of_agricultural_residues_in_kt",
#                                      "3.G. Liming"  = "Liming_in_kt",
#                                      "3.H. Urea application" = "Urea_application_in_kt",
#                                      "3.I.  Other carbon-containing fertilizers" = "Other_carbon_containing_fertilizers_in_kt",
#                                      "3.J.  Other" = "Other_in_kt"
#    )) |> 
#    filter( Emission_sectors %in% c("Agriculture_in_kt", "Enteric_fermentation_in_kt", "Manure_management_in_kt", 
#                                    "Rice_cultivation_in_kt", "Agriculture_soils_in_kt", "Prescribed_burning_of_savannahs_in_kt",
#                                    "Field_burning_of_agricultural_residues_in_kt", "Liming_in_kt", "Urea_application_in_kt", 
#                                    "Other_carbon_containing_fertilizers_in_kt", "Other_in_kt")) |> 
#    mutate(across(-c(Emission_sectors), as.numeric)) |> 
#    pivot_longer(-c(Emission_sectors), names_to = "Year", values_to = "CH4_in_kt") |> 
#    pivot_wider(names_from = Emission_sectors, values_from = CH4_in_kt) |> 
#    mutate( Country = rep("DEU", n())) |> 
 #   relocate(Country, .before = Year) |> 
 #   mutate(across(-c(Country), as.numeric)) |> 
#    drop_na(Agriculture_in_kt) # da bei USA ich so gesehen zwei Spalten zu viel importieren, weider rausnehmen
#  
#glimpse(Total_CH4_Emis_Sectors)
 #-> funktioniert

########
#######
############# Funktion bauen
######
#####

Import_CH4_Emis_SECTOR_AGRICULTURE_function <- function(CC, YE){
  
  # 1. Schritt: Table-Structure bauen, für den folgenden Import
  table_strc <- paste0("CRT_", CC, "_", YE, ".xlsx")
  
  # 2. Schritt: Import und Umbau der Tabelle, sodass mit dieser weiter gearbeitet werden kann.
  
  Total_CH4_Emis_Agri  <- import(file = here("01_Data/Tabellen", table_strc),  # import von rio erkennt Dateityp selber 
                                    # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
                                    sheet = "Table10s3",
                                    range = "B8:AN59",
                                    setclass = "tibble") |> 
    select(-c("Reference year/period for NDC (1)", "Base year (2)", "Change from 1990 to latest reported year")) |> 
    rename( "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
    mutate( Emission_sectors = recode(Emission_sectors,
                                      "3.  Agriculture" = "Agriculture_in_kt",
                                      "3.A.  Enteric fermentation" = "Enteric_fermentation_in_kt",
                                      "3.B.  Manure management" = "Manure_management_in_kt",
                                      "3.C.  Rice cultivation" = "Rice_cultivation_in_kt",
                                      "3.D.  Agricultural soils" = "Agriculture_soils_in_kt",
                                      "3.E.  Prescribed burning of savannahs" = "Prescribed_burning_of_savannahs_in_kt",
                                      "3.F.  Field burning of agricultural residues"  = "Field_burning_of_agricultural_residues_in_kt",
                                      "3.G. Liming"  = "Liming_in_kt",
                                      "3.H. Urea application" = "Urea_application_in_kt",
                                      "3.I.  Other carbon-containing fertilizers" = "Other_carbon_containing_fertilizers_in_kt",
                                      "3.J.  Other" = "Other_in_kt"
    )) |> 
    filter( Emission_sectors %in% c("Agriculture_in_kt", "Enteric_fermentation_in_kt", "Manure_management_in_kt", 
                                    "Rice_cultivation_in_kt", "Agriculture_soils_in_kt", "Prescribed_burning_of_savannahs_in_kt",
                                    "Field_burning_of_agricultural_residues_in_kt", "Liming_in_kt", "Urea_application_in_kt", 
                                    "Other_carbon_containing_fertilizers_in_kt", "Other_in_kt")) |> 
    mutate(across(-c(Emission_sectors), as.numeric)) |> 
    pivot_longer(-c(Emission_sectors), names_to = "Year", values_to = "CH4_in_kt") |> 
    pivot_wider(names_from = Emission_sectors, values_from = CH4_in_kt) |> 
    mutate( Country = rep(CC, n())) |> 
    relocate(Country, .before = Year) |> 
    mutate(across(-c(Country), as.numeric)) |> 
    drop_na(Agriculture_in_kt) |> # da bei USA ich so gesehen zwei Spalten zu viel importieren, weider rausnehmen
    mutate(Agriculture_without_EntFer_in_kt = Agriculture_in_kt - Enteric_fermentation_in_kt)

  return(Total_CH4_Emis_Agri)
  
}


####################################
###################################
########################################################################### Funktion für Anteil von Agric und EntFer an gesamt CH4 Emis und Entfer an Agric Emis
####################################
###################################

Share_Emis_Agric_EntFer_function <- function(CC, YE){
  
  # 1. Schritt: Tabellen Struktur vorgeben
  
  table_strc <- paste0("CRT_", CC, "_", YE, ".xlsx")
  
  # 2. Schritt: Tabelle mit allen Sektoren und den Emissionen herstellen
  
  Total_CH4_Emis_Sectors  <- import(file = here("01_Data/Tabellen", table_strc),  # import von rio erkennt Dateityp selber 
                                    # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
                                    sheet = "Table10s3",
                                    range = "B8:AN59",
                                    setclass = "tibble") |> 
    select(-c("Reference year/period for NDC (1)", "Base year (2)", "Change from 1990 to latest reported year")) |> 
    rename( "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
    mutate( Emission_sectors = recode(Emission_sectors,
                                      "1. Energy" = "Energy_in_kt",
                                      "2.  Industrial processes and product use" = "Industrial_processes_and_product_use_in_kt",
                                      "3.  Agriculture" = "Agriculture_in_kt",
                                      "4. Land use, land-use change and forestry(4)" = "LULUCF_in_kt",
                                      "5.  Waste" = "Waste_in_kt",
                                      "6.  Other (as specified in summary 1)" = "Other_in_kt",
                                      "Total CH4 emissions with LULUCF" = "Total_CH4_in_kt"
    )) |> 
    mutate(across(-c(Emission_sectors), as.numeric)) |> 
    pivot_longer(-c(Emission_sectors), names_to = "Year", values_to = "CH4_in_kt") |> 
    pivot_wider(names_from = Emission_sectors, values_from = CH4_in_kt) |> 
    select(c(Year, Energy_in_kt, Industrial_processes_and_product_use_in_kt, Agriculture_in_kt, 
             LULUCF_in_kt, Waste_in_kt, Other_in_kt, Total_CH4_in_kt)) |> 
    mutate( Country = rep(CC, n())) |> 
    relocate(Country, .before = Year) |> 
    mutate(across(-c(Country), as.numeric)) |> 
    drop_na(Energy_in_kt) |>  # da bei USA so gesehen zwei Spalten zu viel importiert werden, wieder entfernen
    mutate(
      Total_CH4_without_Agriculture_in_kt = Total_CH4_in_kt - Agriculture_in_kt, .before = Total_CH4_in_kt
    )
  
  # 3. Schritt: Tabelle mit dem Agric-Sec. herstellen
  
  Total_CH4_Emis_Agri  <- import(file = here("01_Data/Tabellen", table_strc),  # import von rio erkennt Dateityp selber 
                                 # -> muss nicht read.cs oder _exel eintippen + kann typ beim einladen bestimmen
                                 sheet = "Table10s3",
                                 range = "B8:AN59",
                                 setclass = "tibble") |> 
    select(-c("Reference year/period for NDC (1)", "Base year (2)", "Change from 1990 to latest reported year")) |> 
    rename( "Emission_sectors" = "GREENHOUSE GAS SOURCE AND SINK CATEGORIES") |> 
    mutate( Emission_sectors = recode(Emission_sectors,
                                      "3.  Agriculture" = "Agriculture_in_kt",
                                      "3.A.  Enteric fermentation" = "Enteric_fermentation_in_kt",
                                      "3.B.  Manure management" = "Manure_management_in_kt",
                                      "3.C.  Rice cultivation" = "Rice_cultivation_in_kt",
                                      "3.D.  Agricultural soils" = "Agriculture_soils_in_kt",
                                      "3.E.  Prescribed burning of savannahs" = "Prescribed_burning_of_savannahs_in_kt",
                                      "3.F.  Field burning of agricultural residues"  = "Field_burning_of_agricultural_residues_in_kt",
                                      "3.G. Liming"  = "Liming_in_kt",
                                      "3.H. Urea application" = "Urea_application_in_kt",
                                      "3.I.  Other carbon-containing fertilizers" = "Other_carbon_containing_fertilizers_in_kt",
                                      "3.J.  Other" = "Other_in_kt"
    )) |> 
    filter( Emission_sectors %in% c("Agriculture_in_kt", "Enteric_fermentation_in_kt", "Manure_management_in_kt", 
                                    "Rice_cultivation_in_kt", "Agriculture_soils_in_kt", "Prescribed_burning_of_savannahs_in_kt",
                                    "Field_burning_of_agricultural_residues_in_kt", "Liming_in_kt", "Urea_application_in_kt", 
                                    "Other_carbon_containing_fertilizers_in_kt", "Other_in_kt")) |> 
    mutate(across(-c(Emission_sectors), as.numeric)) |> 
    pivot_longer(-c(Emission_sectors), names_to = "Year", values_to = "CH4_in_kt") |> 
    pivot_wider(names_from = Emission_sectors, values_from = CH4_in_kt) |> 
    mutate( Country = rep(CC, n())) |> 
    relocate(Country, .before = Year) |> 
    mutate(across(-c(Country), as.numeric)) |> 
    drop_na(Agriculture_in_kt) |> # da bei USA ich so gesehen zwei Spalten zu viel importieren, weider rausnehmen
    mutate(Agriculture_without_EntFer_in_kt = Agriculture_in_kt - Enteric_fermentation_in_kt)
  
  # 4. Schritt: Import der bereits erstellten Tabellen für die jeweiligen Länder wo nur Cattle, DC und NDC
  # müssen Cattle extrahieren -> können dann Anteil EnterFer von Cattle an EnterFer und gesamt CH4 berechnen
  
  table_strc2 <- paste0("CRT_", CC, "_1990_", YE, "_all.csv")
  
  Cattle_CH4 <- import(file = here("02_processed/CRT", table_strc2)) |> 
    filter(Animal == "Cattle")
  
  # 5. Schritt: Tabelle mit den Shares herstellen
  
  Share_tab_CC_specific <- tibble(
    Year = Total_CH4_Emis_Agri$Year,
    Agric_total_share_in_perc =  (Total_CH4_Emis_Sectors$Agriculture_in_kt/Total_CH4_Emis_Sectors$Total_CH4_in_kt)*100,
    EntFer_total_share_in_perc = (Total_CH4_Emis_Agri$Enteric_fermentation_in_kt/Total_CH4_Emis_Sectors$Total_CH4_in_kt)*100,
    EntFer_Agric_share_in_kt = (Total_CH4_Emis_Agri$Enteric_fermentation_in_kt/Total_CH4_Emis_Agri$Agriculture_in_kt)*100,
    CatEnFe_total_share_in_perc = (Cattle_CH4$Ch4_emission_in_kt/Total_CH4_Emis_Sectors$Total_CH4_in_kt)*100,
    CatEnFe_Agric_share_in_perc = (Cattle_CH4$Ch4_emission_in_kt/Total_CH4_Emis_Agri$Agriculture_in_kt)*100,
    CatEnFe_EntFer_share_in_perc = (Cattle_CH4$Ch4_emission_in_kt/Total_CH4_Emis_Agri$Enteric_fermentation_in_kt)*100
  )
  
  return(Share_tab_CC_specific)
}
