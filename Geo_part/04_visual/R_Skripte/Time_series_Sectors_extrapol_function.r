############ Biuld function for time series of Emission from diffrent sectors



# habe TOtal without agriculture ausgesetzt in den funktionen für das erstellen der Plots
## weil ich am ende sowieso noch ein plot mit Total, unt total withoout Enterfe_Cat + Enterfe_Cat brauche + plot mit den Indikatoren die ich brauche um Enterfe von cattle zu berechnen

# selbiges bei Agr_wo_EnterFe -> weil ich hier ja nur Cattle_EnterFe brauche


#install.packages("AICcmodavg")

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
library("AICcmodavg")
library("ggtext")
library("scales") # maskiert col_factor readr; discard von purrr; viridis_pal von viridis 
#-> muss wenn ich die aus den anderen Pakten haben will direkt ansprechen mit viridis::vidis_pal



### for creating the same y-axis for each country, take total  Emission with LULUCF as benchmark

## first country specific

#names(DEU_CH4_all_Sectors)

#[1] "Country"                                   
#[2] "Year"                                      
#[3] "Energy_in_kt"                              
#[4] "Industrial_processes_and_product_use_in_kt"
#[5] "Agriculture_in_kt"                         
#[6] "LULUCF_in_kt"                              
#[7] "Waste_in_kt"                               
#[8] "Other_in_kt"                               
#[9] "Total_CH4_without_LULUCF_in_kt"            
#[10] "Total_CH4__with_LULUCF_in_kt" 


#DEU <- ggplot(DEU_CH4_all_Sectors, aes(x = Year,
#                                                y = Energy_in_kt,
#                                                # fill = Animal, 
#                                                colour = "Energy")) +
#  geom_point() + 
#  geom_point(aes(y = Industrial_processes_and_product_use_in_kt, colour = "Industrial processes & product use")) + 
#  geom_point(aes(y = Agriculture_in_kt, colour = "Agriculture")) +
#  geom_point(aes(y = LULUCF_in_kt, colour = "LULUCF")) +
#  geom_point(aes(y = Waste_in_kt, colour = "Waste")) + 
#  geom_point(aes(y = Other_in_kt, colour = "Other")) +
#  geom_point(aes(y = Total_CH4_without_LULUCF_in_kt, colour = "Total without LULUCF")) +
#  geom_point(aes(y = Total_CH4__with_LULUCF_in_kt, colour = "Total with LULUCF")) +
#  scale_x_continuous(
#    expand = c(0, 0),
#    limits = c(1989,2025),      
#    breaks = seq(1990, 2025, 5)
#  ) +
#  scale_y_continuous(
#    expand = c(0,0),
#    limits = c(0, 5.5*1000),        
#    breaks = seq(0, 6*1000, by = 0.5*1000) 
#  ) +
#  ylab(label = "CH4 in kt") +
#  labs(title = "Emissionsectors",
#       colour = "Sectors") +
#  theme_bw() +
#  theme(
#    plot.title = element_text(
#      hjust = 0.5, # Position horizontal: 0 = links, 0.5 = zentriert, 1 = rechts
#      face = "bold",
#      size = 10
#    ),
#    panel.grid.minor = element_line( # minor sind linien im Feld, major achsenlinien
#      color = "grey92",
 ##     linewidth = 0.1
#    )
#  )
#DEU


# Einmal zentral definieren (z.B. ganz am Anfang deines Skripts, außerhalb der Funktion)
sector_colors <- c(
  "Energy" = "#FFD92F",
  "Industrial processes & product use" = "#377EB8",
  "Agriculture" = "#4DAF4A",
  "LULUCF" = "#984EA3",
  "Waste" = "#FF7F00",
  "Other" = "darkred",
  "Total without Agriculture" = "grey",
  "Total" = "grey40"
)

All_sectors_CC_specific_function <- function(df, lm, br_lm, bei, CC){
  # lm -> Max der y-achse; br_lm -> Max des break befehls; bei <- by (alle 3 * 1000), CC <- Country-Code
CC_specific <- ggplot(df, aes(x = Year)) +
    geom_point(aes(y = Energy_in_kt, colour = "Energy")) + 
    geom_point(aes(y = Industrial_processes_and_product_use_in_kt, colour = "Industrial processes & product use")) + 
    geom_point(aes(y = Agriculture_in_kt, colour = "Agriculture")) +
    geom_point(aes(y = LULUCF_in_kt, colour = "LULUCF")) +
    geom_point(aes(y = Waste_in_kt, colour = "Waste")) + 
    geom_point(aes(y = Other_in_kt, colour = "Other")) +
    #geom_point(aes(y = Total_CH4_without_Agriculture_in_kt, colour = "Total without Agriculture")) +
    geom_point(aes(y = Total_CH4_in_kt, colour = "Total")) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(1989,2025),      
      breaks = seq(1990, 2025, 5)
    ) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, lm*1000),        
      breaks = seq(0, br_lm*1000, by = bei*1000) 
    ) +
    ylab(label = "CH<sub>4</sub> in kt") +
    labs(title = paste0("Emissionsectors - ", CC),
         colour = "Sectors") +
  scale_color_manual(
    values = sector_colors,
    limits = c("Energy", "Industrial processes & product use", "Agriculture", # legt Reihenfolge in der Legende an 
               "LULUCF", "Waste", "Other", #"Total without Agriculture", # -> sonst wird alphabetisch angeordnet
               "Total"),
    
  ) +
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
      ),
      axis.title.y = element_markdown()
    )

  return(CC_specific)
}


### Herausfiltern der Sektoren die nur NAs beinhalten!!


All_sectors_CC_specific_NA_Filter_function <- function(df, lm, br_lm, bei, CC){
  
  ### 1. Schritt: definieren welche Spalten ich gleich überprüfen möchte + Liste wo Zusammenhang zwischen Spalte und späterer Name im Plot
  
  Check_NA_list <- list(
    Energy_in_kt = "Energy",
    Industrial_processes_and_product_use_in_kt = "Industrial processes & product use",
    Agriculture_in_kt = "Agriculture",
    LULUCF_in_kt = "LULUCF",
    Waste_in_kt = "Waste",
    Other_in_kt = "Other",
   # Total_CH4_without_Agriculture_in_kt = "Total without Agriculture",
    Total_CH4_in_kt = "Total")
  
  ### 2. Schritt: Vector erstellen, der überprüft ob NAs in den Spalten
  
  valid_cols <- Check_NA_list[sapply(names(Check_NA_list), function(col){ !all(is.na(df[[col]])) })] #sapply(x, FUN(x)) 
  #+ ! vor all, dreht ergebnis um -> fragt ja wo über all nur NA -> will ja aber wissen wo nicht alle NA
  
  ### 3. Schritt: basic plot bauen
  
  basic_plot <- ggplot(df, aes(x = Year))
  
  ### 4. Schritt: Plot erweitern um die Spalten als scatterplot die gültige Werte besitzen
  
  # las Schleife bauen, dass er jedesmal weiteren Scatterplot dazufügt
  
  for (col in names(valid_cols)) { # names(valid_cols) gibt an was col ist -> 
    #könnte col auch durch x ersetzen -> gibt nur an was immer wieder in einer Schleife erstetzt werden soll
    basic_plot <- basic_plot + # hier wichitg, dass 2x gleicher Name verwendet wird, sonst baut er immer wieder neu und nicht kummulativ in eine Datei
      geom_point(aes( y = .data[[col]], colour = !!valid_cols[[col]]))
    #[[ ]] → "Gib mir genau EIN Element/EINE Spalte, und zwar als reinen Inhalt (Vektor), ausgepackt aus der Hülle."
    #[ ] → "Gib mir eine Teilmenge (kann auch mehrere Elemente sein), aber verpackt in derselben Struktur wie das Original (also weiterhin Liste/Dataframe)."
  }
  
  ### 5. Schritt: Plot vervollstädigen
  
  plot_finisched <- basic_plot + scale_x_continuous(
    expand = c(0, 0),
    limits = c(1989,2025),      
    breaks = seq(1990, 2025, 5)
  ) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, lm*1000),        
      breaks = seq(0, br_lm*1000, by = bei*1000) 
    ) +
    ylab( label  = "CH<sub>4</sub> in kt") + 
    labs(
      title = paste0("Emissionsectors - ", CC),
      colour = "Sectors"
    ) +
    scale_color_manual(
      values = sector_colors,
      limits = unname(unlist(valid_cols))
      # hier !! nicht notwenidg weil nur bei Tidy_eval_kontexten nötig, wie aes
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 10
      ),
      axis.title.y = element_markdown(),
      panel.grid.minor = element_line(
        color = "grey92",
        linewidth = 0.1
      )
    )
  
  return(plot_finisched)
}

#DEU_all_Sec_plot <- All_sectors_CC_specific_function(df = DEU_CH4_all_Sectors, lm = 5.5, br_lm = 6, bei = 0.5, CC = "DEU")
#DEU_all_Sec_plot
#-> funktioniert


#########################
###############
###################################### Nun für den Agriculture sector
#############
#######################


#DEU_CH4_Agric_Sect <- import(
#  file = here("02_processed/Sector_Emis_data", "CH4_Emis_Sector_AGRICULTURE_DEU_90_24.csv")
#)

#names(DEU_CH4_Agric_Sect)

#[1] "Country"                                     
#[2] "Year"                                        
#[3] "Agriculture_in_kt"                           
#[4] "Enteric_fermentation_in_kt"                  
#[5] "Manure_management_in_kt"                     
#[6] "Rice_cultivation_in_kt"                      
#[7] "Agriculture_soils_in_kt"                     
#[8] "Prescribed_burning_of_savannahs_in_kt"       
#[9] "Field_burning_of_agricultural_residues_in_kt"
#[10] "Liming_in_kt"                                
#[11] "Urea_application_in_kt"                      
#[12] "Other_carbon_containing_fertilizers_in_kt"   
#[13] "Other_in_kt"   

#summary(DEU_CH4_Agric_Sect)


#### Allgemeine Zuschreibung von Farbwerten zu Subcategorie

Subcategorie_AGR_color <- c(
  "Agriculture" = "#4DAF4A", # Grün - konsistent mit sector_colors
  "Agriculture without EnterFer" = "#1B5E20",
  "Enteric fermentation" = "#377EB8",                      # Blau
  "Manure management" = "#FF7F00",                         # Orange
  "Rice cultivation" = "#984EA3",                          # Lila
  "Agriculture soils" = "#A65628",                         # Braun
  "Prescribed burning of savannahs" = "#E41A1C",           # Rot - Verbrennungsprozesse als Familie...
  "Field burning of agricultural residues" = "#FB9A99",    # ...helleres Rot/Rosa, gleiche Familie
  "Liming" = "#5C2E0D",                                    # Sattelbraun (dunkler, für Bodenbehandlung)
  "Urea application" = "#F781BF",                          # Pink - Düngemittel-Familie...
  "Other carbon containing fertilizers" = "#B8860B",       # ...Gelb, gleiche Düngemittel-Familie
  "Other" = "#999999"                                      # Grau - Sammelkategorie
)

Agriculture_sect_CC_specific_function <- function(df, lm, br_lm, bei, CC){
  # lm -> Max der y-achse; br_lm -> Max des break befehls; bei <- by (alle 3 * 1000), CC <- Country-Code
  CC_specific <- ggplot(df, aes(x = Year,
                                y = Agriculture_in_kt,
                                # fill = Animal, 
                                colour = "Agriculture")) +
    geom_point() + 
    #geom_point(aes(y = Agriculture_without_EntFer_in_kt, colour = "Agriculture without EnterFer")) +
    geom_point(aes(y = Enteric_fermentation_in_kt, colour = "Enteric fermentation")) + 
    geom_point(aes(y = Manure_management_in_kt, colour = "Manure management")) +
    geom_point(aes(y = Rice_cultivation_in_kt, colour = "Rice cultivation")) +
    geom_point(aes(y = Agriculture_soils_in_kt, colour = "Agriculture soils")) + 
    geom_point(aes(y = Prescribed_burning_of_savannahs_in_kt, colour = "Prescribed burning of savannahs")) +
    geom_point(aes(y = Field_burning_of_agricultural_residues_in_kt, colour = "Field burning of agricultural residues")) +
    geom_point(aes(y = Liming_in_kt, colour = "Liming")) +
    geom_point(aes(y = Urea_application_in_kt, colour = "Urea application")) +
    geom_point(aes(y = Other_carbon_containing_fertilizers_in_kt, colour = "Other carbon containing fertilizers")) +
    geom_point(aes(y = Other_in_kt, colour = "Other")) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(1989,2025),      
      breaks = seq(1990, 2025, 5)
    ) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, lm*1000),        
      breaks = seq(0, br_lm*1000, by = bei*1000) 
    ) +
    ylab(label = "CH<sub>4</sub>  in kt") +
    labs(title = paste0("Agriculture - ", CC),
         colour = "Sectors") +
    scale_color_manual(
      values = Subcategorie_AGR_color,
      limits = c("Agriculture", #"Agriculture without EnterFer", 
                 "Enteric fermentation", "Manure management", # legt Reihenfolge in der Legende an 
                 "Rice cultivation", "Agriculture soils", "Prescribed burning of savannahs", "Field burning of agricultural residues", # -> sonst wird alphabetisch angeordnet
                 "Liming", "Urea application", "Other carbon containing fertilizers",
                 "Other"),
      
    ) +
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
      ),
      axis.title.y = element_markdown()
    )
  
  return(CC_specific)
}

#### MIt NA-FIlter Funktion

Agriculture_sect_CC_specific_NA_filter_function <- function(df, lm, br_lm, bei, CC){
  
  # Alle Spalten-Label-Paare definieren
  sector_map <- list(
    Agriculture_in_kt = "Agriculture",
   #Agriculture_without_EntFer_in_kt = "Agriculture without EnterFer",
    Enteric_fermentation_in_kt = "Enteric fermentation",
    Manure_management_in_kt = "Manure management",
    Rice_cultivation_in_kt = "Rice cultivation",
    Agriculture_soils_in_kt = "Agriculture soils",
    Prescribed_burning_of_savannahs_in_kt = "Prescribed burning of savannahs",
    Field_burning_of_agricultural_residues_in_kt = "Field burning of agricultural residues",
    Liming_in_kt = "Liming",
    Urea_application_in_kt = "Urea application",
    Other_carbon_containing_fertilizers_in_kt = "Other carbon containing fertilizers",
    Other_in_kt = "Other"
  )
  
  # Nur die Spalten behalten, die NICHT komplett NA sind
  valid_sectors <- sector_map[sapply(names(sector_map), function(col) { !all(is.na(df[[col]])) }  )]
   # sapply(X, FUN) wendet die Funktion FUN auf jedes Element von X an und fasst die Ergebnisse in einem Vektor zusammen.
   # mit [] hinter einem Objekt überprüfe ich immer, wenn ich ihm logischen Vektor gebe, ob werte TRUE sind, nur diese behält er dann in der LIste
   # Object[] ist grundeinstellung in R
  
  # Basis-Plot ohne geom_point starten
  CC_specific <- ggplot(df, aes(x = Year))
  
  # Für jede gültige Spalte einen geom_point-Layer hinzufügen
  for (col in names(valid_sectors)) {
    CC_specific <- CC_specific + 
      geom_point(aes(y = .data[[col]], colour = !!valid_sectors[[col]]))
    # [[ ]] holt den Wert einer Spalten Spalte hervor
    # !! (sprich: "bang-bang", Un-Quote-Operator) erzwingt die sofortige Auswertung des Ausdrucks an dieser Stelle im Code — 
    # es "friert" den aktuellen Wert von valid_sectors[[col]] ein und setzt ihn direkt als fertigen Wert in den aes()-Aufruf ein, 
    # statt den Ausdruck (mit der sich verändernden Variable col) unausgewertet zu lassen.
  }
  
  CC_specific <- CC_specific +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(1989, 2025),      
      breaks = seq(1990, 2025, 5)
    ) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, lm*1000),        
      breaks = seq(0, br_lm*1000, by = bei*1000) 
    ) +
    ylab(label = "CH<sub>4</sub>  in kt") +
    labs(title = paste0("Agriculture - ", CC), colour = "Sectors") +
    scale_color_manual(
      values =  Subcategorie_AGR_color,
      limits = unname(unlist(valid_sectors))) +
    #unlist() wandelt eine Liste in einen einfachen (atomaren) Vektor um. -> also haben wieder einzelne Namen ("Agriculture_in_kt") + Value ("Sgriculture)
    # unname() entfernt namen, so erhält man nur den Wert -> hier z.B. "Agriculture"
    # -> so hier Ordnung etablierbar in Legende
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.1),
      axis.title.y = element_markdown()
    )
  
  return(CC_specific)
}

