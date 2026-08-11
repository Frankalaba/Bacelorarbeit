
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

#####
####

#  Test for Germany (DEU)
 
#####
###


CH4_emis_Ym_DE_DEU_1990_2024 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_DEU_1990_2024.csv"))

CH4_emis_Ym_DE_NZL_1990_2024 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_NZL_1990_2024.csv"))

CH4_emis_Ym_DE_USA_1990_2022 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_USA_1990_2022.csv"))

# ggplot(data = x, mapping = aes(x = xyz)) + 
# geometrie [geom_xyz()] + scale + theme

 DEU <- ggplot(CH4_emis_Ym_DE_DEU_1990_2024, aes(x = Year,
                                         y = c(Ch4_emission_org_in_kt_CRT),
                                        # fill = Animal, 
                                         colour = Animal)) +
  geom_point() + 
   scale_x_continuous(
     expand = c(0, 0),
     limits = c(1989,2025),
     breaks = seq(1990, 2025, 5)
   ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 1300),
    breaks = seq(0, 1250, by = 250)
  ) +
   ylab(label = "CH4 in kt") +
   labs(title = "DEU") +
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
 
 
 NZL <- ggplot(CH4_emis_Ym_DE_NZL_1990_2024, aes(x = Year,
                                                 y = c(Ch4_emission_org_in_kt_CRT),
                                                 # fill = Animal, 
                                                 colour = Animal)) +
   geom_point() + 
   scale_y_continuous(
     expand = c(0,0),
     limits = c(0, 1300),
     breaks = seq(0, 1250, by = 250)
   ) +
   scale_x_continuous(
     expand = c(0, 0),
     limits = c(1989,2025),
     breaks = seq(1990, 2025, 5)
   ) +
   ylab(label = "CH4 in kt") +
   labs(title = "NZL") + 
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
 
 
USA <- ggplot(CH4_emis_Ym_DE_USA_1990_2022, aes(x = Year,
                                         y = c(Ch4_emission_org_in_kt_CRT),
                                         # fill = Animal,
                                         colour = Animal)) +
  geom_point() + 
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 7200),
    breaks = seq(0, 10000, by = 1000)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1989,2025),
    breaks = seq(1990, 2025, 5)
  ) +
  ylab(label = "CH4 in kt") +
  labs(title = "USA", ) +
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

USA


plots_CH4_org <- DEU + USA + NZL + guide_area() +
  plot_layout(ncol = 2, guides = "collect")

plots_CH4_org

##############################################################
############################################################# Für Jahr 2022 einmal Plot erstellen mit den verschieden Emmissionsszenarien
############################################################

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

unique(DEU_USA_NZL_filtered_CH4_vers_2022$Country)
# [1] "DEU" "NZL" "USA"

df_long <- DEU_USA_NZL_filtered_CH4_vers_2022 |> 
  filter(Country == "DEU" & Animal == "Cattle") |> 
  pivot_longer(
    cols = c(Ch4_emission_org_in_kt, CH4_emis_upd_in_kt_Ym_DE, Ch4_emission_upd_in_kt_Ym), # cola gibt an was ich sozusagen betrachte
    names_to = "Szenario_Typ",
    values_to = "Emission_kt"
  ) |> 
  mutate(Szenario_Typ = factor(Szenario_Typ, # factor wandelt character in ordinalwerte um -> ermöglicht ordnung zu
                               levels = c("Ch4_emission_org_in_kt", "Ch4_emission_upd_in_kt_Ym", "CH4_emis_upd_in_kt_Ym_DE"))) 
View(df_long)

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

# Blautöne:  c("#DEEBF7", "#9ECAE1", "#3182BD") c("#C6DBEF", "#6BAED6", "#08519C")

#####################
###################
###################
####################

df_long_diff <- DEU_USA_NZL_filtered_CH4_vers_2022 |> 
  filter(Country == "DEU" & Animal == "Cattle") |> 
  pivot_longer(cols = c(CH4_Ym_in_kt_diff, CH4_Ym_De_in_kt_diff, CH4_Ym_and_Ym_DE_in_kt_diff),
               names_to = "Szenario_typ",
               values_to = "Emission_kt") |> 
  mutate(
    Szenario_typ = factor(Szenario_typ,
                          levels = c("CH4_Ym_in_kt_diff", "CH4_Ym_De_in_kt_diff", "CH4_Ym_and_Ym_DE_in_kt_diff"))
  )

View(df_long_diff)

ggplot(df_long_diff, aes(x = Red_in_perc,
                        y = Emission_kt,
                        fill = Szenario_typ)) + 
  geom_col(position = "dodge2") +
  labs(
    title = paste0("DEU","-","diffrence"),
    x = "Szenario (%)",
    y = "CH4-Emission (kt)",
    fill = "Szenario"
  ) +
  scale_fill_manual(
    values = c(
      "CH4_Ym_in_kt_diff" = "#C6DBEF",
      "CH4_Ym_De_in_kt_diff" = "#6BAED6",
      "CH4_Ym_and_Ym_DE_in_kt_diff" = "#3182BD"
    ),
    labels = c("Original - Ym", "Original - (Ym + DE)", "Ym - DE")) +
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
# -> sieht scheiße aus, weil die Differenz rasant zunimmt mit den Szenarien 

ggplot(DEU_filterd_CH4_vers_2022, aes(x = Red_in_perc,
                         y = CH4_Ym_and_Ym_DE_in_kt_diff,
                         fill = Country)) + 
  geom_col(position = "dodge2") +
  labs(
    title = paste0("DEU","-","diffrence"),
    x = "Szenario (%)",
    y = "CH4-Emission (kt)",
    fill = "Country"
  ) +
  scale_fill_manual(
    values = c(
      "DEU" = "#C6DBEF",
      "USA" = "#6BAED6",
      "NZL" = "#3182BD"
    ),
    labels = c("Original - Ym", "Original - (Ym + DE)", "Ym - DE")) +
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
# - funktioniert auch nicht schön