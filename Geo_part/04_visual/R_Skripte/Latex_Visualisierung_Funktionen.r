############ Biuld function for time series of Emission from diffrent sectors



# habe TOtal without agriculture ausgesetzt in den funktionen für das erstellen der Plots
## weil ich am ende sowieso noch ein plot mit Total, unt total withoout Enterfe_Cat + Enterfe_Cat brauche + plot mit den Indikatoren die ich brauche um Enterfe von cattle zu berechnen

# selbiges bei Agr_wo_EnterFe -> weil ich hier ja nur Cattle_EnterFe brauche


#install.packages("AICcmodavg")
#install.packages("MetBrewer")

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
library("MetBrewer") # ür metrewer farbpalette
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

#color_palette_x <- met.brewer("Hiroshige", n = 8)
#scales::show_col(color_palette_x)

# Einmal zentral definieren (z.B. ganz am Anfang deines Skripts, außerhalb der Funktion)
sector_colors <- c(
  "Energy" = "#F0C555",                          # gedämpftes Ocker/Gold statt Neongelb
  "Industrial processes & product use" = "#5B7FA6", # gedecktes Blau
  "Agriculture" = "#6B9E78",                     # gedecktes Grün
  "LULUCF" = "#8C6BA8",                          # gedecktes Violett
  "Waste" = "#C97F4A",                           # gedecktes Orange/Terrakotta
  "Other" = "#8B3A3A",                           # gedecktes Dunkelrot
  "Total without cattle EnterFe" = "#B0AFAA",    # warmes Grau
  "Total" = "#5C5B57"                            # dunkles warmes Grau
)

All_sectors_CC_specific_function <- function(df, lm, br_lm, bei, CC, pt_size){
  # lm -> Max der y-achse; br_lm -> Max des break befehls; bei <- by (alle 3 * 1000), CC <- Country-Code
  CC_specific <- ggplot(df, aes(x = Year)) +
    geom_point(aes(y = Energy_in_kt, colour = "Energy"), size = pt_size) + 
    geom_point(aes(y = Industrial_processes_and_product_use_in_kt, colour = "Industrial processes & product use"), size = pt_size) + 
    geom_point(aes(y = Agriculture_in_kt, colour = "Agriculture"), size = pt_size) +
    geom_point(aes(y = LULUCF_in_kt, colour = "LULUCF"), size = pt_size) +
    geom_point(aes(y = Waste_in_kt, colour = "Waste"), size = pt_size) + 
    geom_point(aes(y = Other_in_kt, colour = "Other"), size = pt_size) +
    geom_point(aes(y = total_CH4_wo_Cat_EnterFe_in_kt, colour = "Total without cattle EnterFe"), size = pt_size) +
    geom_point(aes(y = Total_CH4_in_kt, colour = "Total"), size = pt_size) +
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1989,2025),      
      breaks = seq(1990, 2025, 5)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(0, lm*1000),        
      breaks = seq(0, br_lm*1000, by = bei*1000) 
    ) +
    ylab(label = "CH<sub>4</sub> in kt") +
    labs(title = paste0(CC),
         colour = "Sectors") +
    scale_color_manual(
      values = sector_colors,
      limits = c("Energy", "Industrial processes & product use", "Agriculture", # legt Reihenfolge in der Legende an 
                 "LULUCF", "Waste", "Other", "Total without cattle EnterFe", # -> sonst wird alphabetisch angeordnet
                 "Total")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5, # Position horizontal: 0 = links, 0.5 = zentriert, 1 = rechts
        face = "bold",
        size = 12
      ),
      panel.grid.minor = element_line( # minor sind linien im Feld, major achsenlinien
        color = "grey92",
        linewidth = 0.1
      ),
      axis.title.y = element_markdown(
        size = 11
      ),
      axis.title.x = element_markdown(
        size = 11
      ),
      axis.text.x = element_text(
        size = 10
      ),
      axis.text.y = element_text(
        size = 10
      ),
      legend.text = element_markdown(
        size = 11
      ),
      legend.title = element_markdown(
        size = 12
      ),
      legend.key.size = unit(0.4, "cm")
    )
  
  return(CC_specific)
}

#############################################
#############################################
#############################################
###### Share change plot function
############################################
#############################################
############################################



Share_change_plot_function <- function(df, lm, br_lm, bei, CC){
  XZ <- ggplot(df, aes(x = Year)) + 
    geom_point(aes(y = share_total_of_Cat_enterfe_in_procent, colour = "Total methane emission")) +
    geom_point(aes(y = share_agric_of_Cat_enterfe_in_procent, colour = "Methane emission from agriculture")) + 
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1990,2025),
      breaks = seq(1990, 2025, 5)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(0,lm),
      breaks = seq(0, br_lm, bei)
    ) +
    labs(title = CC,
         y = "Share in %",
         x = "Year",
         colour = "Percentage of:") +
    scale_colour_manual(values = c("Total methane emission" = "#5C5B57",
                                   "Methane emission from agriculture" = "#6B9E78"),
                        limits = c("Methane emission from agriculture", "Total methane emission")) +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5, # Position horizontal: 0 = links, 0.5 = zentriert, 1 = rechts
        face = "bold",
        size = 12
      ),
      panel.grid.minor = element_line( # minor sind linien im Feld, major achsenlinien
        color = "grey92",
        linewidth = 0.1
      ),
      axis.title.y = element_markdown(
        size = 11
      ),
      axis.title.x = element_markdown(
        size = 11
      ),
      axis.text.x = element_text(
        size = 10
      ),
      axis.text.y = element_text(
        size = 10
      ),
      legend.text = element_markdown(
        size = 11,
        vjust = 0.5 #zentriert legendentext -> war hier zu symbol leicht verschoben
      ),
      legend.title = element_markdown(
        size = 12
      ),
      legend.key.size = unit(0.6, "cm")
    )
  
  return(XZ)
}


#############################################
#############################################
#############################################
###### Indicator plot
############################################
#############################################
############################################


Indicator_plot_function <- function(df, lm, br_lm, bei){
  
#  animal_colors <- c(
#    "Cattle" = "#2E4A5E",              
#    "Dairy cattle" = "#5B84A0",        
#    "Non-dairy cattle" = "#8FB1C4"     
#  )
  
  animal_colors <- c(
    "Cattle" = "#26404F",              
    "Dairy cattle" = "#5D8AA0",        
    "Non-dairy cattle" = "#A8CAD6"    
    )
  
  Ym <- ggplot(df, aes(x = Year, y = Ym_org_in_perc, colour = Animal)) +
    geom_point() + 
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1990,2025),      
      breaks = seq(1990, 2025, 10)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(5.9, 7.2),
      breaks = seq(0, 7.5, by = 0.5)
    ) +
    ylab(label = "Ym (%)") +
    labs(title = "Ym", color = "Animal:") +
    scale_color_manual(
      values = animal_colors,
      limits = c("Cattle", "Dairy cattle", "Non-dairy cattle")
    ) +
    theme_bw() +
    theme(
      plot.title = element_markdown(hjust = 0.5, face = "bold", size = 10),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.1),
      legend.title = element_markdown(size = 8),
      legend.text = element_markdown(size = 7)
    )
  
  GE <- ggplot(df, aes(x = Year, y = GE_org_in_MJ_per_head_per_day, colour = Animal)) +
    geom_point() + 
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1990,2025),      
      breaks = seq(1990, 2025, 10)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(0, 400),
      breaks = seq(0, 500, by = 100)
    ) +
    ylab(label = "GE (MJ head<sup>⁻¹</sup> day<sup>⁻¹</sup>)") +
    labs(title = "GE", color = "Animal:") +
    scale_color_manual(
      values = animal_colors,
      limits = c("Cattle", "Dairy cattle", "Non-dairy cattle")
    ) +
    theme_bw() +
    theme(
      plot.title = element_markdown(hjust = 0.5, face = "bold", size = 10),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.1)
      )
  
  DE <- ggplot(df, aes(x = Year, y = DE_org_in_perc, colour = Animal)) +
    geom_point() + 
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1990,2025),      
      breaks = seq(1990, 2025, 10)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(64, 78),
      breaks = seq(0, 100, by = 2)
    ) +
    ylab(label = "DE (%)") +
    labs(title = "DE", color = "Animal:") +
    scale_color_manual(
      values = animal_colors,
      limits = c("Cattle", "Dairy cattle", "Non-dairy cattle")
    ) +
    theme_bw() +
    theme(
      plot.title = element_markdown(hjust = 0.5, face = "bold", size = 10),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.1)
      )
  
  Pop <- ggplot(df, aes(x = Year, y = Population_size_1000s, colour = Animal)) +
    geom_point() + 
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1990,2025),      
      breaks = seq(1990, 2025, 10)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(0, lm*1000),
      breaks = seq(0, br_lm*1000, by = bei*1000)
    ) +
    ylab(label = "Population in 1000s") +
    labs(title = "Population", color = "Animal:") +
    scale_color_manual(
      values = animal_colors,
      limits = c("Cattle", "Dairy cattle", "Non-dairy cattle")
    ) +
    theme_bw() +
    theme(
      plot.title = element_markdown(hjust = 0.5, face = "bold", size = 10),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.1))
  
  Indic_all_plot <- Ym + DE + Pop + GE + plot_layout(ncol = 2, guides = "collect") &
    theme(
      legend.position = "bottom", #### was in den einzel plots steht egal, wird erst hier dann allgemein gesteuer
      legend.direction = "horizontal",
      axis.title.y = element_markdown(size = 11),
      axis.title.x = element_markdown(size = 11),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.text = element_markdown(size = 11, vjust = 0.5),
      legend.title = element_markdown(size = 12)
    )
  
  return(Indic_all_plot)
}

#############################################
#############################################
#############################################
###### EnterFe cattle Emission plot
############################################
#############################################
############################################

EnterFe_emission_plot_function <- function(df, lm, br_lm, bei, CC){
  
  animal_colors <- c(
    "Cattle" = "#26404F",              
    "Dairy cattle" = "#5D8AA0",        
    "Non-dairy cattle" = "#A8CAD6"    
  )
  
EnterFe_emis  <- ggplot(df, aes(x = Year, y = Ch4_emission_org_in_kt, colour = Animal)) +
    geom_point() + 
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      limits = c(1990, 2025),
      breaks = seq(1990, 2030, 10)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.05),
      limits = c(0, lm*1000),
      breaks = seq(0, br_lm*1000, bei*1000)
    ) +
    labs(
      title = CC,
      y = "CH<sub>4</sub> in kt",
      x = "Year",
      colour = "Animal"
    ) +
    scale_colour_manual(
      values = animal_colors,
      limits = c("Cattle", "Dairy cattle", "Non-dairy cattle")
    ) +
    theme_bw() +
    theme(
      plot.title = element_markdown(hjust = 0.5, face = "bold", size = 10),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.1),
      axis.title.y = element_markdown(size = 11),
      axis.title.x = element_markdown(size = 11),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.text = element_markdown(size = 11, vjust = 0.5),
      legend.title = element_markdown(size = 12)
    )
  
  return(EnterFe_emis)
  
}

