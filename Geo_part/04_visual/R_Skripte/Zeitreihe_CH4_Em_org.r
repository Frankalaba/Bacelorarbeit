
rm(list = ls()) # enviroment säubern

#install.packages("rio")
#install.packages("janitor")
#install.packages("tidyverse")
#install.packages("ggpmisc")
#install.packages("patchwork")
#install.packages("scales")
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
library("scales") # maskiert col_factor readr; discard von purrr; viridis_pal von viridis 
#-> muss wenn ich die aus den anderen Pakten haben will direkt ansprechen mit viridis::vidis_pal

here()


############## Befehl für Zeitreihe -> hier nur für orginal CH4 aber auch leicht anpassbar an andere Dinge


CH4_emis_Ym_DE_DEU_1990_2024 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_DEU_1990_2024.csv"))

CH4_emis_Ym_DE_NZL_1990_2024 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_NZL_1990_2024.csv"))

CH4_emis_Ym_DE_USA_1990_2022 <- read.csv(
  file = here("03_Analyses/tables/Ym_and_DE_change_simple", "CH4_emis_DE_Ym_USA_1990_2022.csv"))


names(CH4_emis_Ym_DE_USA_1990_2022)


DEU <- ggplot(CH4_emis_Ym_DE_DEU_1990_2024, aes(x = Year,
                                                y = c(Population_size_1000s),
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
    limits = c(0, 21000),        #  limits = c(0, 1300),  -> für CH4_org
    breaks = seq(0, 21000, by = 5000) # breaks = seq(0, 1250, by = 250) -> für CH4_org
  ) +
  ylab(label = "Population in 1000s") +
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


DEU

NZL <- ggplot(CH4_emis_Ym_DE_NZL_1990_2024, aes(x = Year,
                                                y = c(Population_size_1000s),
                                                # fill = Animal, 
                                                colour = Animal)) +
  geom_point() + 
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 11000),     # limits = c(0, 1300),
    breaks = seq(0, 24000, by = 2000)   # breaks = seq(0, 1250, by = 250)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1989,2025),
    breaks = seq(1990, 2025, 5)
  ) +
  ylab(label = "Population in 1000s") +
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

NZL


USA <- ggplot(CH4_emis_Ym_DE_USA_1990_2022, aes(x = Year,
                                                y = c(Population_size_1000s),
                                                # fill = Animal,
                                                colour = Animal)) +
  geom_point() + 
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 111000.500),   #  limits = c(0, 7200),
    breaks = seq(0, 120000, by = 20000), #    breaks = seq(0, 10000, by = 1000)
    labels = scales::label_number() # scales sorgt dafür, dass label-Zahlen nicht als e+x geschrieben wird sondern ganze Zahl ausgeschrieben
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1989,2025),
    breaks = seq(1990, 2025, 5)
  ) +
  ylab(label = "Population in 1000s") +
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
