
rm(list = ls())

library("tidyr")
library("dplyr")
library("ggplot2")
library("viridis")
library("here")
library("rio")
library("patchwork")
library("purrr")
library("ggtext")

here()


#####################################
#####################################
#### 8-Azahypoxanthin
#####################################
#####################################


#######
# M. bovis
######

###Tabelle mit Rohdaten erstellen

#Mbovis_8aza_Messwerte <- data.frame(
# culture = rep(c("Aza1", "Aza2", "Aza3"), each = 7*10),
#  conc_µg_ml = rep(c("+", "DMSO +", 100, 250, 500, 750, 1000), 3*10),
# day = rep(rep(c(0, 1, 2, 3, 6, 7, 8, 9, 10, 13 ), each= 7), 3), # durch die Verschachtelung, bekomme ich pro Aza-Kultur die verschiedenen tage
#  OD600 = c(0.030, 0.037, 0.023, 0.016, 0.008, 0.005, 0.007, #Aza1 Tag 0
#    0.099, -0.011, 0.023, -0.009, -0.021, -0.017, -0.028, #Tag1
#     0.206, 0.028, 0.065, -0.003, -0.013, -0.017, -0.021, #Tag2
#      0.224, 0.029, 0.096, 0.006, -0.021, -0.026, -0.023, #Tag3
#       0.189, 0.101, 0.119, 0.097, 0.042, 0.035, 0.034, #  Tag6
#        0.162, 0.088, 0.101, 0.125, 0.032, 0.031, 0.029, # Tag 7
#         0.159, 0.086, 0.109, 0.137, 0.039, 0.034, 0.036, # Tag8
#          0.131, 0.073, 0.105, 0.11, 0.035, 0.033, 0.031, # Tag9
#           0.113, 0.061, 0.095, 0.082, 0.038, 0.027, 0.031, # Tag 10
#            0.071, 0.061, 0.078, 0.062, 0.049, 0.037, 0.028, # Tag13
#   
#    0.006, 0.003, -0.007, -0.009, -0.026, -0.023, -0.018, #aza2 Tag0
#     0.052, -0.017, 0.011, -0.005, -0.023, -0.02, -0.025, #Tag1
#      0.190, 0.031, 0.099, 0.022, -0.019, -0.018, -0.022, #Tag2
#       0.245, 0.051, 0.133, 0.036, -0.02, -0.022, -0.027, #Tag3
#        0.172, 0.094, 0.151, 0.084, 0.034, 0.035, 0.034,  #  Tag6
#         0.166, 0.071, 0.132, 0.131, 0.028, 0.019, 0.122, # Tag 7
#          0.16, 0.073, 0.14, 0.158, 0.034, 0.031, 0.119, # Tag8
#           0.137, 0.065, 0.133, 0.143, 0.033, 0.029, 0.105, # Tag9
#            0.134, 0.058, 0.126, 0.111, 0.031, 0.029, 0.1, # Tag 10
#    0.081, 0.039, 0.11, 0.07, 0.033, 0.023, 0.083, # Tag13
#     
#      0.001, 0.013, 0.005, -0.002, -0.004, 0.003, 0.001, # Aza3 Tag 0
#    0.054, -0.002, 0, -0.022, -0.026, -0.026, -0.031, #Tag1
#    0.215, 0.034, 0.051, -0.001, -0.01, -0.016, -0.018, #Tag2
#     0.237, 0.07, 0.082, -0.013, -0.02, -0.025, -0.032, #Tag3
#    0.223, 0.113, 0.11, 0.088, 0.033, 0.033, 0.035,  # Tag6
#      0.181, 0.087, 0.112, 0.161, 0.026, 0.026, 0.027, # Tag7
#       0.174, 0.084, 0.11, 0.142, 0.034, 0.03, 0.03, # Tag8
#        0.156, 0.069, 0.124, 0.109, 0.03, 0.029, 0.03, # Tag9
#         0.136, 0.068, 0.097, 0.089, 0.035, 0.026, 0.025, #Tag 10
#          0.078, 0.039, 0.065, 0.054, 0.04, 0.029, 0.023 # Tag13
#           )
#  )
#View(Mbovis_8aza_Messwerte)

#write.csv(Mbovis_8aza_Messwerte, file = "Mbovis_8azahypoxanthin_roh.csv", row.names = FALSE)

# ab jetzt kann man die Tabelle immer einlesen um zu starten

Mbovis_8aza_roh <- import(
  file = here("Nukleotid-Versuch/Messwerte", "Mbovis_8azahypoxanthin_roh.csv")
)

summary(Mbovis_8aza_roh)

#Mbovis_8aza_log <- Mbovis_8aza_roh |> 
#  mutate(
#    OD600_adj = if_else(OD600 <= 0, 0.001, OD600),
#    OD600_adj_lg = log10(OD600_adj)
#  )

#summary(Mbovis_8aza_log)

#export(Mbovis_8aza_log,
#       file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mbovis_8aza_log10.csv"))

Mbovis_8aza_lg <- import(
  file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mbovis_8aza_log10.csv"))

# neue_Werte_Mb <- data.frame(
#  culture = rep(c("Aza1", "Aza2", "Aza3"), each = 7),
#  conc_µg_ml = rep(c("+", "DSMO", 100, 250, 500, 750, 1000), 3),
#  day = rep(rep(c(0),7),1),
#  OD600 =
# )

# Mbovis_8aza_roh <- rbind(Mbovis_8aza_roh, neue_Werte_Mb)

######################
######################
#Auswertung

#Mittelwerte herstellen

#Funktion des Pipe-Operators (%>%)
#Der Pipe-Operator nimmt den Ausdruck auf der linken Seite und 
#übergibt ihn als erstes Argument an die Funktion auf der rechten Seite.

#Mbovis_8aza_roh_mean <- Mbovis_8aza_roh %>% 
#  group_by(day, conc_µg_ml) %>%  # Gruppierung nach Tag und Konzentration
#  summarize(
#    OD600_mean = mean(OD600, na.rm = TRUE), # Mittelwert der OD600-Werte
#    sd_OD600 = sd(OD600, na.rm = TRUE),
#    .groups = "drop"                          # Gruppierung entfernen
#  )

#View(Mbovis_8aza_roh_mean)

#write.csv(Mbovis_8aza_roh_mean, file = "Mbovis_8aza_mean_sd.csv", row.names = FALSE)

Mbovis_8aza_roh_mean <- read.csv("Mbovis_8aza_mean_sd.csv")

ggplot(data = Mbovis_8aza_roh_mean,
       aes(
         x = day,
         y = OD600_mean,
         color = factor(conc_µg_ml,
                        levels = c("+", "100", "250", "500", "750", "1000", "DMSO +"))
       )) +  
  scale_x_continuous(
    limits = c(0, 13.1),
    breaks = c(0, 2, 4, 6, 8, 10, 12, 14)
  ) +
  scale_y_continuous(
    limits = c(-0.035, 0.25)
  ) +
  #geom_errorbar(aes(ymin = OD600_mean - sd_OD600,
  #                ymax = OD600_mean + sd_OD600),
  #            width = 0.2,
  #            size = 0.8)+
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "D") +  # Option D ist etwas gedeckter
  labs(
    x = "Day",
    y = "Mean OD600",
    color = "Concentration [µg/ml]"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

################################################
##################
############################################## Da mittelwerte wieder schlecht aussieht beim plotten, wieder einzeln die Konz plotten
################
##############################################

Mbovis_8aza_lg <- import(
  file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mbovis_8aza_log10.csv"))

names(Mbovis_8aza_lg)

unique(Mbovis_8aza_lg$conc_µg_ml)
#[1] "+"      "DMSO +" "100"    "250"    "500"    "750"    "1000" 

unique(Mbovis_8aza_lg$culture)
#"Aza1" "Aza2" "Aza3"


Wachstumskurve_plot_einzelne_conc_legende_8aza_function <- function(df, conc, D){
  
 df1 <- df |> 
    mutate(conc_µg_ml = recode(conc_µg_ml,
                               "+" = "0",
                               "DMSO +" = "DMSO"),
           culture = recode(culture,
                            "Aza1" = "Cltr 1",
                            "Aza2" = "Cltr 2",
                            "Aza3" = "Cltr 3"))
  
  
  plot <- ggplot(data = df1[df1$conc_µg_ml == conc, ], # braucht in [] zwingend ein Komma [Zeile, Spalte]
                 aes(
                   x = day, 
                   y = OD600_adj,
                   group = culture)) +
    geom_line(colour = "black",
              linewidth = 0.25) +
    geom_point(
      aes(colour = factor(culture)),
      size = 3,
      shape = 18) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(-0.5, D),
      breaks = seq(0, 20, 2)
    ) +
    scale_y_log10(
      expand = expansion(mult = 0.05), # macht es prozentual
      limits = c(0.001, 1),
      breaks = c(0.001, 0.003, 0.010,  0.03, 0.1, 0.3, 1),
      labels = scales::label_number(accuracy = 0.001) # sonst schnell komische Formatierung der Achseenbschriftung
    ) +
    #scale_y_continuous(
    #  expand = c(0,0),
    #  limits = c(-3.1, 0.1),
    #  breaks = seq(-4, 0, 0.5)
    #) +
    labs(
      title = paste0(conc, " µg/ml 8-Aza"),
      y = "OD 600",
      x = "Day",
      colour = "Culture"
    ) +
    scale_color_viridis_d(option = "D") +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11
      ),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11,
                                  face = "bold"),
      panel.grid.minor.y = element_blank()
    )
  
  return(plot)
}

Wachstumskurve_plot_einzelne_conc_legende_8aza__DMSO_function <- function(df, conc, D){
  
  df1 <- df |> 
    mutate(conc_µg_ml = recode(conc_µg_ml,
                               "+" = "0",
                               "DMSO +" = "DMSO"),
           culture = recode(culture,
                            "Aza1" = "Cltr 1",
                            "Aza2" = "Cltr 2",
                            "Aza3" = "Cltr 3"))
  
  
  plot <- ggplot(data = df1[df1$conc_µg_ml == conc, ], # braucht in [] zwingend ein Komma [Zeile, Spalte]
                 aes(
                   x = day, 
                   y = OD600_adj,
                   group = culture)) +
    geom_line(colour = "black",
              linewidth = 0.25) +
    geom_point(
      aes(colour = factor(culture)),
      size = 3,
      shape = 18) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(-0.5, D),
      breaks = seq(0, 20, 2)
    ) +
    scale_y_log10(
      expand = expansion(mult = 0.05), # macht es prozentual
      limits = c(0.001, 1),
      breaks = c(0.001, 0.003, 0.010, 0.03,  0.1, 0.3, 1),
      labels = scales::label_number(accuracy = 0.001) # sonst schnell komische Formatierung der Achseenbschriftung
    ) +
   # scale_y_continuous(
  #    expand = c(0,0),
  #    limits = c(-3.1, 0.1),
  #    breaks = seq(-4, 0, 0.5)
  #  ) +
    labs(
      title = paste0(conc),
      y = "OD 600",
      x = "Day",
      colour = "Culture"
    ) +
    scale_color_viridis_d(option = "D") +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11
      ),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11,
                                  face = "bold"),
      panel.grid.minor.y = element_blank()
    )
  
  return(plot)
}

plot_mb_all <- map(c(0, 100, 250, 500, 750, 1000), ~ Wachstumskurve_plot_einzelne_conc_legende_8aza_function(df = Mbovis_8aza_lg, conc = .x, D = 16.5)) # erstelle liste an plots

DMSO <- Wachstumskurve_plot_einzelne_conc_legende_8aza__DMSO_function(df = Mbovis_8aza_lg, conc = "DMSO", D = 16.5)

plot_mb_all_DMSO <- c(plot_mb_all, DMSO) # alle als ein Vektor/ Liste zusammengefügt

plot_all_MB_8_aza <- wrap_plots(plot_mb_all_DMSO, ncol = 3) + guide_area() + # wrap paackt die plots nun zusammen -> was sonst durch + geschieht
  plot_layout(guides = "collect") 

plot_all_MB_8_aza

ggsave(
  file = here("Nukleotid-Versuch/graphiken/Mbovis", "Mbovis_8azahypoxanthin_lg10_conc_einzeln.pdf"),
  plot = plot_all_MB_8_aza,
  width = 22,
  height = 18,
  units = "cm"
)


##########################################
##########################################
############# M. millerae
##########################################
##########################################

####Tabelle mit Rohdaten erstellen

#Mmillerae_8aza_Messwerte <- data.frame(
#  culture = rep(c("Aza1", "Aza2", "Aza3"), each = 7*11),
# conc_µg_ml = rep(c("+", "DMSO +", 100, 250, 500, 750, 1000), 3*11),
#  day = rep(rep(c(0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14), each = 7),3),
#OD600 = c(-0.036, -0.039, -0.036, -0.036, -0.037, -0.038, -0.038, # Aza1 Tag0
#           0.309, 0.09, 0.114, 0.055, 0.11, 0.022, 0.09, # Tag3
#            0.287, 0.082, 0.15, 0.046, 0.114, 0.02, 0.082, # Tag 4
# 0.262, 0.079, 0.16, 0.05, 0.116, 0.054, 0.087, # Tag 5
#  0.228, 0.066, 0.147, 0.045, 0.11, 0.133, 0.082, # tag 6
#   0.203, 0.056, 0.132, 0.05, 0.107, 0.186, 0.081, #Tag7
#    0.136, 0.039, 0.083, 0.066, 0.098, 0.201, 0.07, # Tag10
#     0.123, 0.038, 0.08, 0.074, 0.12, 0.202, 0.08, # Tag11
#      0.111, 0.032, 0.07, 0.071, 0.158, 0.194, 0.074, # Tag12
#       0.096, 0.032, 0.069, 0.075, 0.247, 0.184, 0.073, # Tag 13
#        0.076, 0.027, 0.06, 0.092, 0.257, 0.167, 0.071, # Tag14
#         
#          -0.029, -0.019, -0.028, -0.031, -0.028, -0.029, -0.032, # Aza2 Tag0
#           0.241, 0.06, 0.117, 0.028, 0.026, 0.04, 0.074,  # Tag3
#            0.214, 0.055, 0.137, 0.022, 0.022, 0.127, 0.114, # Tag 4
# 0.198, 0.058, 0.133, 0.024, 0.026, 0.118, 0.106, # Tag5
#  0.166, 0.051, 0.12, 0.024, 0.025, 0.099, 0.091, # Tag 6
#   0.142, 0.043, 0.102, 0.021, 0.02, 0.087, 0.08, #Tag7
#    0.08, 0.03, 0.073, 0.052, 0.019, 0.07, 0.066, # Tag10
#     0.074, 0.032, 0.074, 0.065, 0.023, 0.073, 0.069, # Tag11
#      0.065, 0.026, 0.067, 0.074, 0.017, 0.068, 0.065, # Tag12
#       0.059, 0.025, 0.07, 0.086, 0.025, 0.068, 0.064, # Tag13
#        0.058, 0.021, 0.063, 0.085, 0.02, 0.064, 0.061, # Tag14
#         
#          -0.03, -0.025, -0.024, -0.029, -0.032, -0.032, -0.032,  # Aza3 Tag0
#           0.307, 0.066, 0.038, 0.031, 0.027, 0.032, 0.031,  #  Tag3
#            0.306, 0.064, 0.028, 0.023, 0.021, 0.023, 0.022,  # Tag4
#            0.275, 0.06, 0.03, 0.027, 0.025, 0.027, 0.027,  # Tag 5
#            0.236, 0.053, 0.028, 0.027, 0.024, 0.027, 0.026, # Tag 6
#            0.207, 0.048, 0.023, 0.022, 0.02, 0.023, 0.023, # Tag7
#            0.142, 0.031, 0.017, 0.017, 0.016, 0.02, 0.021, # Tag10
#           0.137, 0.033, 0.019, 0.02, 0.02, 0.023, 0.022, # Tag11
#           0.125, 0.027, 0.016, 0.02, 0.017, 0.019, 0.019, # Tag12
#            0.113, 0.027, 0.016, 0.019, 0.018, 0.02, 0.023, # Tag13
#            0.107, 0.024, 0.014, 0.021, 0.015, 0.017, 0.022 # Tag14
#  )
#  )

#View(Mmillerae_8aza_Messwerte)

#write.csv(Mmillerae_8aza_Messwerte, file = "Mmilli_8aza_roh.csv", row.names = FALSE)

Mmilli_8aza_roh <- import(
  file = here("Nukleotid-Versuch/Messwerte", "Mmilli_8aza_roh.csv"))


#Mmilli_8aza_log <- Mmilli_8aza_roh |> 
#  mutate(
#    OD600_adj = if_else(OD600 <= 0, 0.001, OD600),
#    OD600_adj_lg = log10(OD600_adj)
#  )

#summary(Mmilli_8aza_log)

#export(Mmilli_8aza_log,
#       file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mmilli_8aza_log10.csv"))

Mmilli_8aza_lg <- import(
  file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mmilli_8aza_log10.csv"))

summary(Mmilli_8aza_lg)

## Visualisierung

plot_mm_all <- map(c(0, 100, 250, 500, 750, 1000), 
                   ~ Wachstumskurve_plot_einzelne_conc_legende_8aza_function(df = Mmilli_8aza_lg, conc = .x, D = 16.5)) # erstelle liste an plots

DMSO_m <- Wachstumskurve_plot_einzelne_conc_legende_8aza__DMSO_function(df = Mmilli_8aza_lg, conc = "DMSO", D = 16.5)

plot_mm_all_DMSO <- c(plot_mm_all, DMSO_m) # alle als ein Vektor/ Liste zusammengefügt

plot_all_MM_8_aza <- wrap_plots(plot_mm_all_DMSO, ncol = 3) + guide_area() + # wrap paackt die plots nun zusammen -> was sonst durch + geschieht
  plot_layout(guides = "collect")

plot_all_MM_8_aza

ggsave(
  file = here("Nukleotid-Versuch/graphiken/Mmilli", "Mmilli_8azahypoxanthin_lg10_conc_einzeln.pdf"),
  plot = plot_all_MM_8_aza,
  width = 22,
  height = 18,
  units = "cm"
)


#neue_Werte_Mm <- data.frame(
#  culture = rep(c("Aza1", "Aza2", "Aza3"), each = 7),
#  conc_µg_ml = rep(c("+", "DSMO", 100, 250, 500, 750, 1000), 3),
#  day = rep(rep(c(0),7),1),
#  OD600 =
#)

#Mmilli_8aza_roh <- rbind(neue_Werte_Mm, neue_Werte_Mm)

# write.csv(Mmillerae_8aza_Messwerte, file = "Mmilli_8aza_roh.csv", row.names = FALSE)

#########################################
#########################################

#### Analyse

# Mittelwerte herstellen

Mmilli_8aza_roh_mean <- Mmilli_8aza_roh %>%
  group_by(day, conc_µg_ml) %>%
  summarise(
    OD600_mean = mean(OD600, na.rm = TRUE),
    sd_OD600 = sd(OD600, na.rm = TRUE),
    .groups = "drop"
  )

View(Mmilli_8aza_roh_mean)

write.csv(Mmilli_8aza_roh_mean, file = "Mmilli_8aza_mean_sd.csv", row.names = FALSE)

Mmilli_8aza_roh_mean <- read.csv("Mmilli_8aza_mean_sd.csv")

##### Visualisierung

ggplot(data = Mmilli_8aza_roh_mean,
       aes(
         x = day,
         y = OD600_mean,
         color = factor(conc_µg_ml,
                        levels = c("+", "100", "250", "500", "750", "1000", "DMSO +"))
       )) +  
  scale_x_continuous(
    limits = c(0, 14.1),
    breaks = c(0, 2, 4, 6, 8, 10, 12, 14)
  )+
  scale_y_continuous( 
    limits = c(-0.05, 0.31)
  )+
  #geom_errorbar(aes(ymin = OD600_mean - sd_OD600,
  #                ymax = OD600_mean + sd_OD600),
  #             width = 0.2,
  #             size = 0.8)+
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "D") +  # Option D ist etwas gedeckter
  labs(
    x = "Day",
    y = "Mean OD600",
    color = "Concentration [µg/ml]"
  ) +
  theme_bw()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )



###########################
###########################
##########################
####### M. thaueri CW
############################
############################
############################

###Tabelle mit Rohdaten erstellen

#Mthaueri_8aza_roh <- data.frame(
#  culture = rep(c("Aza1", "Aza2", "Aza3"), each = 8*14),
#  conc_µg_ml = rep(c("+", "DMSO +", "DMSO -", 100, 250, 500, 750, 1000), 3*14),
#  day = rep(rep(c(0, 1, 2, 5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 19), each= 8), 3), # durch die Verschachtelung, bekomme ich pro Aza-Kultur die verschiedenen tage
#  OD600 = c(0.039, 0.036, 0.027, 0.042, 0.038, 0.037, 0.04, 0.048, #Aza1 Tag 0
#            0.051, 0.049, 0.032, 0.042, 0.041, 0.04, 0.038, 0.036, #Tag1
#            0.061, 0.051, 0.029, 0.033, 0.03, 0.029, 0.048, 0.028, #Tag2
#            0.102, 0.046, 0.033, 0.025, 0.025, 0.029, 0.029, 0.028, # Tag5
#           0.123, 0.058, 0.038, 0.026, 0.026, 0.027, 0.029, 0.03, # Tag6
#           0.134, 0.046, 0.031, 0.02, 0.022, 0.03, 0.026, 0.037, # Tag7
#          0.168, 0.038, 0.027, 0.026, 0.019, 0.022, 0.024, 0.018, # Tag 8
#         0.174, 0.04, 0.041, 0.023, 0.022, 0.031, 0.026, 0.031, # Tag 9
#        0.166, 0.037, 0.04, 0.024, 0.028, 0.029, 0.032, 0.031, # Tag12
#       0.138, 0.024, 0.031, 0.018, 0.015, 0.018, 0.023, 0.022, # Tag13
#      0.126, 0.032, 0.036, 0.024, 0.022, 0.025, 0.028, 0.025, # Tag 14
#     0.127, 0.043, 0.052, 0.034, 0.037, 0.044, 0.041, 0.039, # Tag15
#    0.107, 0.036, 0.045, 0.025, 0.028, 0.032, 0.031, 0.031, # Tag16
#   0.049, -0.026, -0.028, -0.048, -0.04, -0.006, -0.002, -0.004, # Tag19
#  
# 0.042, 0.038, 0.023, 0.038, 0.043, 0.041, 0.039, 0.042, #aza2 Tag0
#            0.042, 0.038, 0.029, 0.042, 0.043, 0.04, 0.038, 0.042, #Tag 1
#           0.047, 0.042, 0.035, 0.034, 0.034, 0.04, 0.036, 0.039, #Tag 2
#          0.055, 0.038, 0.034, 0.027, 0.03, 0.037, 0.034, 0.036, # Tag 5
#         0.053, 0.045, 0.031, 0.025, 0.032, 0.032, 0.035, 0.029, # Tag 6
#        0.048, 0.028, 0.029, 0.021, 0.026, 0.029, 0.029, 0.03, # Tag 7
#       0.047, 0.021, 0.027, 0.016, 0.02, 0.027, 0.024, 0.021, # Tag 8
#      0.053, 0.031, 0.027, 0.024, 0.026, 0.029, 0.035, 0.032, # Tag 9
#     0.061, 0.027, 0.035, 0.021, 0.025, 0.034, 0.033, 0.03, # Tag12
#    0.065, 0.024, 0.027, 0.016, 0.026, 0.026, 0.024, 0.023, # Tag13
#   0.078, 0.026, 0.03, 0.019, 0.026, 0.027, 0.03, 0.03, # Tag 14
#  0.118, 0.04, 0.042, 0.03, 0.04, 0.045, 0.043, 0.039, # Tag15
# 0.115, 0.03, 0.041, 0.021, 0.029, 0.033, 0.033, 0.027, # Tag16
#            0.044, -0.035, -0.028, -0.041, -0.035, -0.04, -0.032, -0.045, # Tag19
#           
#          0.044, 0.045, 0.034, 0.045, 0.046, 0.05, 0.056, 0.04, # Aza3 Tag 0
#         0.046, 0.045, 0.032, 0.047, 0.044, 0.042, 0.038, 0.038, #Tag1
#        0.05, 0.306, 0.031, 0.038, 0.038, 0.034, 0.036, 0.033, #Tag2
#       0.05, 0.075, 0.033, 0.032, 0.032, 0.031, 0.032, 0.042, # Tag5
#      0.046, 0.059, 0.033, 0.028, 0.032, 0.036, 0.028, 0.032, # Tag6
#     0.044, 0.048, 0.03, 0.024, 0.026, 0.026, 0.025, 0.048,  # Tag7
#    0.048, 0.042, 0.023, 0.019, 0.024, 0.023, 0.019, 0.021, # Tag8
#   0.074, 0.032, 0.047, 0.023, 0.027, 0.033, 0.044, 0.037, # Tag9
#  0.151, 0.05, 0.036, 0.029, 0.027, 0.026, 0.033, 0.029, # Tag12
# 0.104, 0.048, 0.031, 0.017, 0.025, 0.019, 0.022, 0.032, # Tag13
#0.088, 0.051, 0.032, 0.021, 0.027, 0.025, 0.026, 0.026, # Tag 14
#            0.08, 0.069, 0.043, 0.042, 0.039, 0.042, 0.039, 0.045, # Tag15
#           0.063, 0.062, 0.036, 0.027, 0.035, 0.029, 0.03, 0.029, # Tag16
#          -0.025, 0.023, -0.035, -0.049, -0.039, -0.044, -0.044, -0.044 # Tag19
#  )
#)
#View(Mthaueri_8aza_roh)

#write.csv(Mthaueri_8aza_roh, file = "Mthaueri_8azahypoxanthin_roh.csv", row.names = FALSE)

# ab jetzt kann man die Tabelle immer einlesen um zu starten

Mthaueri_8aza_roh <- import(
    file = here("Nukleotid-Versuch/Messwerte", "Mthaueri_8azahypoxanthin_roh.csv"))

summary(Mthaueri_8aza_roh)

#Mthau_8aza_log <- Mthaueri_8aza_roh |> 
#  mutate(
#    OD600_adj = if_else(OD600 <= 0, 0.001, OD600),
#    OD600_adj_lg = log10(OD600_adj)
#  )  |> 
#  filter(day != 19) # nehmen wir raus weil krasser Ausreißer

#summary(Mthau_8aza_log)

#export(Mthau_8aza_log,
#       file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mthau_8aza_log10.csv"))

Mthau_8aza_lg <- import(file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mthau_8aza_log10.csv"))

## Visualisierung


plot_mt_all <- map(c(0, 100, 250, 500, 750, 1000), 
                   ~ Wachstumskurve_plot_einzelne_conc_legende_8aza_function(df = Mthau_8aza_lg, conc = .x, D = 16.5)) # erstelle liste an plots

DMSO_t <- Wachstumskurve_plot_einzelne_conc_legende_8aza__DMSO_function(df = Mthau_8aza_lg, conc = "DMSO", D = 16.5)

plot_mt_all_DMSO <- c(plot_mt_all, DMSO_t) # alle als ein Vektor/ Liste zusammengefügt

plot_all_MT_8_aza <- wrap_plots(plot_mt_all_DMSO, ncol = 3) + guide_area() + # wrap paackt die plots nun zusammen -> was sonst durch + geschieht
  plot_layout(guides = "collect")

plot_all_MT_8_aza

ggsave(
  file = here("Nukleotid-Versuch/graphiken/Mthau", "Mthau_8azahypoxanthin_lg10_conc_einzeln.pdf"),
  plot = plot_all_MT_8_aza,
  width = 22,
  height = 18,
  units = "cm"
)




######################
######################
#Auswertung

#Mittelwerte herstellen

#Funktion des Pipe-Operators (%>%)
#Der Pipe-Operator nimmt den Ausdruck auf der linken Seite und 
#übergibt ihn als erstes Argument an die Funktion auf der rechten Seite.

#Mthaueri_8aza_roh_mean <- Mthaueri_8aza_roh %>% 
#  group_by(day, conc_µg_ml) %>%  # Gruppierung nach Tag und Konzentration
#  summarize(
#    OD600_mean = mean(OD600, na.rm = TRUE), # Mittelwert der OD600-Werte
#   sd_OD600 = sd(OD600, na.rm = TRUE),
#   .groups = "drop"                          # Gruppierung entfernen
#  )

#View(Mthaueri_8aza_roh_mean)

#write.csv(Mthaueri_8aza_roh_mean, file = "Mthaueri_8aza_mean_sd.csv", row.names = FALSE)

Mthaueri_8aza_roh_mean <- read.csv("Mthaueri_8aza_mean_sd.csv")

summary(Mthaueri_8aza_roh_mean)

# Visualisierung

ggplot(data = Mthaueri_8aza_roh_mean,
       aes(
         x = day,
         y = OD600_mean,
         color = factor(conc_µg_ml,
                        levels = c("+", "100", "250", "500", "750", "1000", "DMSO +", "DMSO -"))
       )) +  
  scale_x_continuous(
    limits = c(0, 19.1),
    breaks = c(0, 2, 4, 6, 8, 10, 12, 16, 18)
  ) +
  scale_y_log10() +
  #  geom_errorbar(aes(ymin = OD600_mean - sd_OD600,
  #                  ymax = OD600_mean + sd_OD600),
  #            width = 0.2,
  #          size = 0.8)+
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "D") +  # Option D ist etwas gedeckter
  labs(
    x = "Day",
    y = "Mean OD600",
    color = "Concentration [µg/ml]"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )



#################
################
# 6-Azauracil
###############
###############

###########################
###########################
##########################
####### M. wolinii 
############################
############################
############################

###Tabelle mit Rohdaten erstellen 8.06.26 start

#Mwolinii_6aza_roh <- data.frame(
#  culture = rep(c("Aza1", "Aza2", "Aza3"), each = 7*12),
#  conc_µg_ml = rep(c("+", "DMSO +", 100, 250, 500, 750, 1000), 3*12),
#  day = rep(rep(c(0, 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 15), each= 7), 3), 
#  OD600 = c(0.05, 0.04, 0.046, 0.038, 0.044, 0.034, 0.029, #Aza1 Tag 0
# 0.291, 0.234, 0.262, 0.241, 0.248, 0.256, 0.238, # Tag1
#  0.308, 0.064, 0.1, 0.081, 0.074, 0.08, 0.086, # Tag2
#   0.339, 0.086, 0.055, 0.049, 0.036, 0.036, 0.04, # Tag3
#    0.302, 0.249, 0.05, 0.039, 0.028, 0.026, 0.02, # Tag4
#     0.259, 0.253, 0.052, 0.061, 0.021, 0.037, 0.035, # Tag7
#      0.247, 0.267, 0.149, 0.097, 0.028, 0.047, 0.037, #TAG8
#       0.229, 0.246, 0.3, 0.121, 0.03, 0.043, 0.032, # Tag9
#        0.242, 0.232, 0.278, 0.107, 0.031, 0.04, 0.025, # Tag10
#        0.153, 0.226, 0.274, 0.099, 0.042, 0.036, 0.03, # Tag11 
#          0.224, 0.203, 0.28, 0.307, 0.05, 0.034, 0.032, # Tag14
#           0.202, 0.203, 0.259, 0.283, 0.049, 0.028, 0.031, # Tag15
#            
#            0.041, 0.04, 0.051, 0.039, 0.045, 0.038, 0.044, #aza2 Tag0
#0.248, 0.247, 0.251, 0.244, 0.267, 0.264, 0.251, # Tag1
# 0.097, 0.169, 0.075, 0.069, 0.131, 0.1, 0.101, # Tag2
#  0.115, 0.342, 0.032, 0.041, 0.058, 0.051, 0.04, # Tag3
#   0.485, 0.316, 0.033, 0.034, 0.038, 0.041, 0.027, # Tag4
#    0.41, 0.25, 0.012, 0.028, 0.167, 0.062, 0.044, # Tag7
#     0.387, 0.241, 0.025, 0.051, 0.202, 0.094, 0.049, #TAG8
#      0.168*2, 0.226, 0.125, 0.057, 0.233, 0.118, 0.044, # Tag9
#       0.371, 0.221, 0.235*2, 0.051, 0.251, 0.106, 0.037, # Tag10 
#        0.361, 0.224, 0.242*2, 0.051, 0.266, 0.099, 0.034, # Tag11
#         0.33, 0.218, 0.207*2, 0.261, 0.351, 0.098, 0.039, # Tag14
#          0.321, 0.223, 0.208*2, 0.255, 0.372, 0.099, 0.04, # Tag15
#           
#            0.047, 0.045, 0.044, 0.04, 0.042, 0.04, 0.041, # Aza3 Tag 0
#0.269, 0.257, 0.246, 0.259, 0.264, 0.245, 0.261, # Tag1
# 0.191, 0.155, 0.073, 0.089, 0.073, 0.075, 0.136, # Tag2
#  0.368, 0.318, 0.033, 0.043, 0.044, 0.042, 0.046, # Tag3
#   0.319, 0.431, 0.031, 0.035, 0.038, 0.03, 0.04, # Tag4
#    0.266, 0.314, 0.022, 0.023, 0.029, 0.075, 0.051, # Tag7
#     0.243, 0.291, 0.032, 0.044, 0.05, 0.114, 0.052, #TAG8
#      0.267, 0.275, 0.108, 0.048, 0.056, 0.113, 0.046, # Tag9
#       0.264, 0.257, 0.221*2, 0.042, 0.053, 0.103, 0.04, # Tag10
#        0.262, 0.249, 0.238*2, 0.044, 0.053, 0.097, 0.045, # Tag11
#         0.233, 0.268, 0.198*2, 0.257*2, 0.047, 0.084, 0.044, # Tag14
#          0.245, 0.262, 0.197*2, 0.24*2, 0.054, 0.093, 0.043 # Tag15
# )
#)
#View(Mwolinii_6aza_roh)

#write.csv(Mwolinii_6aza_roh, file = "Mwolinii_6azauracil_roh.csv", row.names = FALSE)

# ab jetzt kann man die Tabelle immer einlesen um zu starten

Mwolinii_6aza_roh <- import(
  file = here("Nukleotid-Versuch/Messwerte", "Mwolinii_6azauracil_roh.csv"))

summary(Mwolinii_6aza_roh)

#Mwolinii_6aza_log <- Mwolinii_6aza_roh |> 
#  mutate(
#    OD600_adj = if_else(OD600 <= 0, 0.001, OD600),
#    OD600_adj_lg = log10(OD600_adj)
#  ) 

#summary(Mwolinii_6aza_log)

#export(Mwolinii_6aza_log,
#       file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mwolini_6aza_log10.csv"))

Mwolinii_6aza_lg <- import(file = here("Nukleotid-Versuch/Messwerte/log_10", "Messwerte_Mwolini_6aza_log10.csv"))


Wachstumskurve_plot_einzelne_conc_legende_6aza_function <- function(df, conc, D){
  
  df1 <- df |> 
    mutate(conc_µg_ml = recode(conc_µg_ml,
                               "+" = "0",
                               "DMSO +" = "DMSO"),
           culture = recode(culture,
                            "Aza1" = "Cltr 1",
                            "Aza2" = "Cltr 2",
                            "Aza3" = "Cltr 3"))
  
  
  plot <- ggplot(data = df1[df1$conc_µg_ml == conc, ], # braucht in [] zwingend ein Komma [Zeile, Spalte]
                 aes(
                   x = day, 
                   y = OD600_adj,
                   group = culture)) +
    geom_line(colour = "black",
              linewidth = 0.25) +
    geom_point(
      aes(colour = factor(culture)),
      size = 3,
      shape = 18) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(-0.5, D),
      breaks = seq(0, 20, 2)
    ) +
   scale_y_log10(
     expand = expansion(mult = 0.05),
     limits = c(0.001, 1),
     breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1),
     label = scales::label_number(accuracy = 0.001)
   ) +
    # scale_y_continuous(
  #    expand = c(0,0),
   #   limits = c(-3.1, 0.1),
  #    breaks = seq(-4, 0, 0.5)
   # ) +
    labs(
      title = paste0(conc, " µg/ml 6-Aza"),
      y = "OD 600",
      x = "Day",
      colour = "Culture"
    ) +
    scale_color_viridis_d(option = "D") +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11
      ),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11,
                                  face = "bold"),
      panel.grid.minor.y = element_blank()
    )
  
  return(plot)
}

Wachstumskurve_plot_einzelne_conc_legende_6aza__DMSO_function <- function(df, conc, D){
  
  df1 <- df |> 
    mutate(conc_µg_ml = recode(conc_µg_ml,
                               "+" = "0",
                               "DMSO +" = "DMSO"),
           culture = recode(culture,
                            "Aza1" = "Cltr 1",
                            "Aza2" = "Cltr 2",
                            "Aza3" = "Cltr 3"))
  
  
  plot <- ggplot(data = df1[df1$conc_µg_ml == conc, ], # braucht in [] zwingend ein Komma [Zeile, Spalte]
                 aes(
                   x = day, 
                   y = OD600_adj,
                   group = culture)) +
    geom_line(colour = "black",
              linewidth = 0.25) +
    geom_point(
      aes(colour = factor(culture)),
      size = 3,
      shape = 18) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(-0.5, D),
      breaks = seq(0, 20, 2)
    ) +
    scale_y_log10(
      expand = expansion(mult = 0.05),
      limits = c(0.001, 1),
      breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1),
      labels = scales::label_number(accuracy = 0.001)
    ) +
    
    #scale_y_continuous(
    #  expand = c(0,0),
    #  limits = c(-3.1, 0.1),
    #  breaks = seq(-4, 0, 0.5)
    #) +
    labs(
      title = paste0(conc),
      y = "OD 600",
      x = "Day",
      colour = "Culture"
    ) +
    scale_color_viridis_d(option = "D") +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11
      ),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11,
                                  face = "bold"),
      panel.grid.minor.y = element_blank()
    )
  
  return(plot)
}

plot_mw_all <- map(c(0, 100, 250, 500, 750, 1000), 
                   ~ Wachstumskurve_plot_einzelne_conc_legende_6aza_function(df = Mwolinii_6aza_lg, conc = .x, D = 16.5)) # erstelle liste an plots

DMSO_w <- Wachstumskurve_plot_einzelne_conc_legende_6aza__DMSO_function(df = Mwolinii_6aza_lg, conc = "DMSO", D = 16.5)

plot_mw_all_DMSO <- c(plot_mw_all, DMSO_w) # alle als ein Vektor/ Liste zusammengefügt

plot_all_MW_6_aza <- wrap_plots(plot_mw_all_DMSO, ncol = 3) + guide_area() + # wrap paackt die plots nun zusammen -> was sonst durch + geschieht
  plot_layout(guides = "collect")

plot_all_MW_6_aza

ggsave(
  file = here("Nukleotid-Versuch/graphiken/Mwolinii", "Mwolini_6azauracil_lg10_conc_einzeln.pdf"),
  plot = plot_all_MW_6_aza,
  width = 22,
  height = 18,
  units = "cm"
)
