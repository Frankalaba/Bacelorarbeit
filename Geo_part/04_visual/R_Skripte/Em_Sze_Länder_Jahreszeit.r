##### Visualizing test


rm(list = ls()) # enviroment säubern

#install.packages("rio")
#install.packages("janitor")
#install.packages("tidyverse")
#install.packages("ggpmisc")
#install.packages("patchwork")


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

here()


CH4_emis_Ym_DE_DEU_1990_2024 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_DEU_1990_2024.csv"))

CH4_emis_Ym_DE_NZL_1990_2024 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_NZL_1990_2024.csv"))

CH4_emis_Ym_DE_USA_1990_2022 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_USA_1990_2022.csv"))



DEU_filterd_CH4_vers_2022 <- CH4_emis_Ym_DE_DEU_1990_2024 |> 
  filter(Year == 2022) |> 
  transmute(
    Animal, Ch4_emission_upd_in_kt_Ym,  CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_org_in_kt, CH4_Ym_in_kt_diff, CH4_Ym_De_in_kt_diff,CH4_Ym_and_Ym_DE_in_kt_diff,
    Country = "DEU", Red_in_perc
  ) 
glimpse(DEU_filterd_CH4_vers_2022)

NZL_filterd_CH4_vers_2022 <- CH4_emis_Ym_DE_NZL_1990_2024 |> 
  filter(Year == 2022) |> 
  transmute(
    Animal, Ch4_emission_upd_in_kt_Ym,  CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_org_in_kt, CH4_Ym_in_kt_diff, CH4_Ym_De_in_kt_diff, CH4_Ym_and_Ym_DE_in_kt_diff,
    Country = "NZL", Red_in_perc
  )

glimpse(NZL_filterd_CH4_vers_2022)

USA_filterd_CH4_vers_2022 <- CH4_emis_Ym_DE_USA_1990_2022 |> 
  filter(Year == 2022) |> 
  transmute(
    Animal, Ch4_emission_upd_in_kt_Ym,  CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_org_in_kt, CH4_Ym_in_kt_diff, CH4_Ym_De_in_kt_diff,CH4_Ym_and_Ym_DE_in_kt_diff,
    Country = "USA", Red_in_perc
  )

glimpse(USA_filterd_CH4_vers_2022)

DEU_USA_NZL_filtered_CH4_vers_2022 <- rbind(DEU_filterd_CH4_vers_2022, NZL_filterd_CH4_vers_2022, USA_filterd_CH4_vers_2022)


###### gut funktionierende Befehle aus dem Test_skript hier rein und den einen als Funktion umbauen.


###############
#############
#### Veränderung bar plot, pro Animal und country
##########
########

df_long <- DEU_USA_NZL_filtered_CH4_vers_2022 |> 
  filter(Country == "DEU" & Animal == "Cattle") |> 
  pivot_longer(
    cols = c(Ch4_emission_org_in_kt, CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_upd_in_kt_Ym), # col gibt an was ich sozusagen betrachte
    names_to = "Szenario_Typ",
    values_to = "Emission_kt"
  ) |> 
  mutate(Szenario_Typ = factor(Szenario_Typ, # factor wandelt character in ordinalwerte um -> ermöglicht ordnung zu
                               levels = c("Ch4_emission_org_in_kt", "Ch4_emission_upd_in_kt_Ym", "CH4_emis_upd_in_kt_Ym_DE"))) 
#View(df_long)

ggplot(df_long, aes(x = factor(Red_in_perc), y = Emission_kt, fill = Szenario_Typ)) +
  geom_col(position = "dodge2") + # geom_col und nicht geom_bar, weil keine Häufigkeit von irgendwas angegeben wird
  labs(
    title = paste0("DEU","-","Cattle"),
    x = "Szenario (%)",
    y = "CH4-Emission (kt)",
    fill = "Szenario"
  ) +
  scale_fill_manual(
    values = c(
      "Ch4_emission_org_in_kt" = "#C6DBEF",
      "CH4_emis_upd_in_kt_Ym_DE" = "#6BAED6",
      "Ch4_emission_upd_in_kt_Ym" = "#3182BD"
    ),
    labels = c("Original", "Ym", "Ym + DE")) +
  theme_bw() +
  theme(
    plot.title = element_text( 
      face = "bold",
      size = 10,
      hjust = 0.5 ),
    panel.grid.minor = element_line( # minor sind linien im Feld, major achsenlinien
      color = "grey92",
      linewidth = 0.1
    )
  )

CH4_Emission_sceanarien_CC_Animal <- function(df, CC, Anml){
  # 1. Schritt: long format Tabelle erstellen
  
  df_long <- df|> 
    filter(Country == CC & Animal == Anml) |> 
    pivot_longer(
      cols = c(Ch4_emission_org_in_kt, CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_upd_in_kt_Ym), # cola gibt an was ich sozusagen betrachte
      names_to = "Szenario_Typ",
      values_to = "Emission_kt"
    ) |> 
    mutate(Szenario_Typ = factor(Szenario_Typ, # factor wandelt character in ordinalwerte um -> ermöglicht ordnung zu
                                 levels = c("Ch4_emission_org_in_kt", "Ch4_emission_upd_in_kt_Ym", "CH4_emis_upd_in_kt_Ym_DE"))) 
  
  # 2. Schritt: plotten
  ggplot(df_long, aes(x = factor(Red_in_perc), y = Emission_kt, fill = Szenario_Typ)) +
    geom_col(position = "dodge2") + # geom_col und nicht geom_bar, weil keine Häufigkeit von irgendwas angegeben wird
    labs(
      title = paste0(CC,"-",Anml),
      x = "Szenario (%)",
      y = "CH4-Emission (kt)",
      fill = "Szenario"
    ) +
    scale_fill_manual(
      values = c(
        "Ch4_emission_org_in_kt" = "#C6DBEF",
        "CH4_emis_upd_in_kt_Ym_DE" = "#6BAED6",
        "Ch4_emission_upd_in_kt_Ym" = "#3182BD"
      ),
      labels = c("Original", "Ym", "Ym + DE")) +
    theme_bw() +
    theme(
      plot.title = element_text( 
        face = "bold",
        size = 10,
        hjust = 0.5 ),
      panel.grid.minor = element_line( # minor sind linien im Feld, major achsenlinien
        color = "grey92",
        linewidth = 0.1
      )
    )
  
}

CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "NZL", Anml = "Dairy cattle")  

plot_DEU_Szen_Anml <- CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "DEU", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "DEU", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "DEU", Anml = "Non-dairy cattle") +
  guide_area() +
  plot_layout(ncol = 2, guides = "collect")

plot_DEU_Szen_Anml

plot_NZL_Szen_Anml <- CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "NZL", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "NZL", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "NZL", Anml = "Non-dairy cattle") +
  guide_area() +
  plot_layout(ncol = 2, guides = "collect")

plot_NZL_Szen_Anml

plot_USA_Szen_Anml <- CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "USA", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "USA", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "USA", Anml = "Non-dairy cattle") +
  guide_area() +
  plot_layout(ncol = 2, guides = "collect")  

plot_USA_Szen_Anml

plot_TOG_Szen_Anml <- CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "DEU", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "DEU", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "DEU", Anml = "Non-dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "USA", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "USA", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "USA", Anml = "Non-dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "NZL", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "NZL", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_2022, CC = "NZL", Anml = "Non-dairy cattle") +
  guide_area() +
  plot_layout(ncol = 3, guides = "collect")

plot_TOG_Szen_Anml # -> zu viele Graphiken sieht scheiße aus


##########################################
##########################################
#########
################################################## Für 1990
#########
###########################################
###########################################

DEU_filterd_CH4_vers_1990 <- CH4_emis_Ym_DE_DEU_1990_2024 |> 
  filter(Year == 1990) |> 
  transmute(
    Animal, Ch4_emission_upd_in_kt_Ym,  CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_org_in_kt, CH4_Ym_in_kt_diff, CH4_Ym_De_in_kt_diff,CH4_Ym_and_Ym_DE_in_kt_diff,
    Country = "DEU", Red_in_perc
  ) 

NZL_filterd_CH4_vers_1990 <- CH4_emis_Ym_DE_NZL_1990_2024 |> 
  filter(Year == 1990) |> 
  transmute(
    Animal, Ch4_emission_upd_in_kt_Ym,  CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_org_in_kt, CH4_Ym_in_kt_diff, CH4_Ym_De_in_kt_diff, CH4_Ym_and_Ym_DE_in_kt_diff,
    Country = "NZL", Red_in_perc
  )

USA_filterd_CH4_vers_1990 <- CH4_emis_Ym_DE_USA_1990_2022 |> 
  filter(Year == 1990) |> 
  transmute(
    Animal, Ch4_emission_upd_in_kt_Ym,  CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_org_in_kt, CH4_Ym_in_kt_diff, CH4_Ym_De_in_kt_diff,CH4_Ym_and_Ym_DE_in_kt_diff,
    Country = "USA", Red_in_perc
  )

DEU_USA_NZL_filtered_CH4_vers_1990 <- rbind(DEU_filterd_CH4_vers_1990, NZL_filterd_CH4_vers_1990, USA_filterd_CH4_vers_1990)

### 
######
######## Plotten
#######
########

plot_DEU_Szen_Anml <- CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "DEU", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "DEU", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "DEU", Anml = "Non-dairy cattle") +
  guide_area() +
  plot_layout(ncol = 2, guides = "collect")

plot_DEU_Szen_Anml

plot_NZL_Szen_Anml <- CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "NZL", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "NZL", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "NZL", Anml = "Non-dairy cattle") +
  guide_area() +
  plot_layout(ncol = 2, guides = "collect")

plot_NZL_Szen_Anml

plot_USA_Szen_Anml <- CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "USA", Anml = "Cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "USA", Anml = "Dairy cattle") +
  CH4_Emission_sceanarien_CC_Animal(df = DEU_USA_NZL_filtered_CH4_vers_1990, CC = "USA", Anml = "Non-dairy cattle") +
  guide_area() +
  plot_layout(ncol = 2, guides = "collect")  

plot_USA_Szen_Anml
