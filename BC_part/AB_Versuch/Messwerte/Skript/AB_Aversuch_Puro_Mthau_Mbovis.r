#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("viridis")
#install.packages("ggtext")

library("tidyr")
library("dplyr")
library("ggplot2")
library("viridis")
library("here")
library("rio")
library("patchwork")
library("purrr")
library("ggtext") # um beim plotten Text kursiv, z.B. im titel schreiben zu können

here()

#########################
#### Mbovis
#########################

Mbovis_Puro_data <- import(file = here("AB_Versuch/Messwerte", "Messwerte_Mbovis_Puromycin.csv"))
head(Mbovis_Puro_data)
summary(Mbovis_Puro_data)

#### OD Werte log, um später mit weiter zu rechneen/ plotten für Wachstum

#Mbovis_Puro_data_lg <- Mbovis_Puro_data |> 
#  mutate(
#    OD600_adj = if_else(OD600 <= 0, 0.001, OD600),
#    OD600_adj_lg = log10(OD600_adj)
#  )

#summary(Mbovis_Puro_data_lg)

#export(Mbovis_Puro_data_lg,
#       file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mbovis_Puromycin_log10.csv"))

Mbovis_Puro_data_log <- import(
  file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mbovis_Puromycin_log10.csv")
)

summary(Mbovis_Puro_data_log)

### neue Messwerte ergänzen und Tabelle aktualisieren


#neue_werte_Mb <- data.frame(
#  culture = rep(c("AB1", "AB2", "AB3"), each = 6), #durch das each wird jedes direkt 6 mal 
#  #                             hintereinander wiederholte und nicht in der genannten Reihenfolge
#  concentration.µg.ml = rep(c(0, 1, 2, 5, 10, 20),  3),
#  day = rep(11,18),
#  OD600 = c(0.082, 0.152, 0.066, 0.134, 0.109, 0.111, 
#            0.095, 0.176, 0.148, 0.079, 0.117, -0.002, 
#            0.096, 0.142, 0.159, 0.091, 0.072, 0.130)
#)
#show(neue_werte_Mb)

#Mbovis_Puro_data <- rbind(Mbovis_Puro_data, neue_werte_Mb)
#head(Mbovis_Puro_data)
#show(Mbovis_Puro_data)

#121     AB3                   0   9  0.0161 
#-> hier aus versehen eine 0 zu viel müsste 0.161 sein -> anpassen

#Mbovis_Puro_data$OD600[
#  Mbovis_Puro_data$culture== "AB3" &
#  Mbovis_Puro_data$concentration.µg.ml== 0 &
#  Mbovis_Puro_data$day == 9
#]<- 0.161

#Mbovis_Puro_data[
#  Mbovis_Puro_data$culture == "AB3" &
#    Mbovis_Puro_data$concentration.µg.ml == 0 &
#    Mbovis_Puro_data$day == 9,
#]

#culture concentration.µg.ml day OD600
#121     AB3                   0   9 0.161

## abändern hat funktioniert

#write.csv(Mbovis_Puro_data, file="Messwerte_Mbovis_Puromycin.csv", row.names = FALSE)

ggplot(Mbovis_Puro_data_log, aes(
  x = day,
  y = OD600,
  group = interaction(concentration.µg.ml, culture),  # interaction() erstellt eine neue kombinierte Kategorie, indem es mehrere Variablen zu einer einzigen zusammenfasst
  shape = factor(culture)              # Form nach Kultur
)) +
  geom_line(linewidth = 0.25,
            colour = "black") +
  geom_point(aes(colour = factor(concentration.µg.ml)), size = 3) + # hier erst Farbe für Punkte einführen, damit Linien schwarz sein können
  labs(
    x = "day",
    y = "OD600",
    color = "concentration [µg/ml]",
    shape = "culture"                      # Beschriftung für die Legende
  ) +
  theme_minimal() +
  scale_shape_manual(values = c(16, 17, 15, 18)) + # Manuelle Zuweisung der Formen
  theme(
    line = element_line(colour = "black")
  )
####################################################################################

# Mittelwerte für jede Konzentration berechnen und in einer neuen Tabelle speichern
#Mbovis_Puro_means_lg_1 <- Mbovis_Puro_data_log |> 
#  select(-c(OD600_adj)) |> 
#  group_by(day, concentration.µg.ml) |>   # Gruppierung nach Tag und Konzentration
#  summarize(
#    OD600_mean = mean(OD600, na.rm = TRUE), # Mittelwert der OD600-Werte
#    sd_OD600 = sd(OD600, na.rm = TRUE),
 #   OD600_log_mean = mean(OD600_adj_lg, na.rm = TRUE),
 #   sd_log_OD600 = sd(OD600_adj_lg, na.rm = TRUE),
#    .groups = "drop"                          # Gruppierung entfernen
#  )

#summary(Mbovis_Puro_means_lg_1)

#export(Mbovis_Puro_means_lg_1,
#       file = here("AB_Versuch/Messwerte/log10", "Mbovis_Puromycin_MEAN_log10.csv"))


Mbovis_mean_log10 <- import(
  file = here("AB_Versuch/Messwerte/log10", "Mbovis_Puromycin_MEAN_log10.csv")
)

summary(Mbovis_mean_log10)

Mbovis_plot <- ggplot(data = Mbovis_mean_log10, aes( #_sd anhängen bei Dateinamen, wenn sd mit dargestellt wird
  x = day,
  y = OD600_mean,
  group = concentration.µg.ml 
)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-0.5, 12.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.49),
                     breaks = seq(-0.5, 0.6, 0.1)) +
  geom_line(linewidth = 0.25,
            colour = "black") +
  #geom_errorbar(aes(ymin = OD600_mean - sd_OD600,
  #                  ymax = OD600_mean + sd_OD600,
  #                  colour = factor(concentration.µg.ml)),
  #              width = 0.4,
  #              size = 0.8)+
  geom_point(aes(colour = factor(concentration.µg.ml)), size = 3,
             shape = 18) +

  scale_color_viridis_d(option = "D") +  # Option D ist etwas gedeckter
  labs(
    x = "Day",
    y = "Mean OD600",
    color = "Concentration [µg/ml]"
  ) +
  theme_bw()

Mbovis_sd_plot <- ggplot(data = Mbovis_mean_log10, aes( 
  x = day,
  y = OD600_mean,
  group = concentration.µg.ml 
)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-0.5, 12.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.49),
                     breaks = seq(-0.5, 0.6, 0.1)) +
  geom_line(linewidth = 0.25,
            colour = "black") +
  geom_errorbar(aes(ymin = OD600_mean - sd_OD600,
                    ymax = OD600_mean + sd_OD600,
                    colour = factor(concentration.µg.ml)),
                width = 0.4,
                linewidth = 0.8) +
  geom_point(aes(colour = factor(concentration.µg.ml)), size = 3,
             shape = 18) +
  scale_color_viridis_d(option = "D") +  # Option D ist etwas gedeckter
  labs(
    x = "Day",
    y = "Mean OD600",
    color = "Concentration [µg/ml]"
  ) +
  theme_bw()

Mbovis_plot

Mbovis_sd_plot

ggsave(
  file = here("AB_Versuch/Messwerte/graphs", "AB_Mbovis_Puromycin_mean.pdf"),
  plot = Mbovis_plot
)

ggsave(
  file = here("AB_Versuch/Messwerte/graphs", "AB_Mbovis_Puromycin_mean_sd.pdf"),
  plot = Mbovis_sd_plot
)

##############################################
##### ist geringe Menge Puromycin wachstumsfördernd?
##############################################
# Frage finde ich immer noch spannend, muss ich aber wann anders machen, für BA unwichtig




###########################################
#### log10 der Wachstumskurve plotten
##########################################

summary(Mbovis_mean_log10$OD600_log_mean)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.0000 -2.2190 -1.0089 -1.4932 -0.7729 -0.4742

summary(Mbovis_mean_log10$sd_log_OD600)

#  Min.    1st Qu.  Median  Mean   3rd Qu.  Max. 
#  0.00000 0.04204 0.13028 0.29109 0.42385 1.22177

names(Mbovis_mean_log10)

Mb_plot_lg <- ggplot( Mbovis_mean_log10,
                         aes(
                           x = day,
                           y = OD600_mean,
                           group = concentration.µg.ml
                         )) +
  geom_line( colour = "black",
             linewidth = 0.25) +
  geom_point(aes(colour = factor(concentration.µg.ml)),
             shape = 18,
             size = 3) +
 scale_x_continuous(
    expand = c(0,0),
    limits = c(-0.5, 12.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) + 
  scale_y_log10(
    expand = expansion(mult = 0.05),
    limits = c(0.001, 1),
    breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1),
    labels = scales::label_number(accuracy = 0.001)
  ) +
  labs(
    y = "lg(OD 600)",
    x = "Day",
    colour = "Concentration [µg/ml]"
  ) +
  scale_colour_viridis_d(option = "D") +
  theme_bw()

Mb_plot_lg

ggsave(
  filename = here("AB_Versuch/Messwerte/graphs/log10", "AB_Mbovis_Puromycin_mean_log10.pdf"), 
  plot = Mb_plot_lg
)

Mb_plot_lg_sd <- ggplot( Mbovis_mean_log10,
                      aes(
                        x = day,
                        y = OD600_mean,
                        group = concentration.µg.ml
                      )) +
  geom_line( colour = "black",
            linewidth = 0.25) +
  geom_point(aes(colour = factor(concentration.µg.ml)),
             shape = 18,
             size = 3) +
  geom_errorbar( aes(ymin = OD600_mean - sd_OD600,
                     ymax = OD600_mean + sd_OD600,
                     colour = factor(concentration.µg.ml)),
                 width = 0.4,
                 linewidth = 0.8
                 ) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(-0.5, 12.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) +
  scale_y_log10(
    expand = expansion(mult = 0.05),
    limits = c(0.001, 1),
    breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1),
    labels = scales::label_number(accuracy = 0.001)
  ) +
  labs(
  y = "lg(OD 600)",
  x = "Day",
  colour = "Concentration [µg/ml]"
  ) +
  scale_colour_viridis_d(option = "D") +
  theme_bw()


Mb_plot_lg_sd # geht hier gar nicht, weil wir durch sd im minus bereich landen

ggsave(
  filename = here("AB_Versuch/Messwerte/graphs/log10", "AB_Mbovis_Puromycin_mean_log10_sd.pdf"), 
  plot = Mb_plot_lg_sd
)

################ Funktion um doch nicht als mean darzustellen, aber als einzelne, pro Konzentratiion dann die drei Kulturen zu sehen

Wachstumskurve_plot_einzelne_conc_legende_function <- function(df, conc){
  
  df1 <- df  |> 
    mutate(culture = recode(culture,
                            "AB1" = "Cltr1",
                            "AB2" = "Cltr2",
                            "AB3" = "Cltr3"))
  
  plot <- ggplot(data = df1[df1$concentration.µg.ml == conc, ], # braucht in [] zwingend ein Komma [Zeile, Spalte]
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
             limits = c(-0.5, 11.5),
             breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
           ) +
    scale_y_log10(
      expand = expansion(mult = 0.05),
      limits = c(0.001, 1),
      breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1),
      labels = scales::label_number(accuracy = 0.001)
    ) +
    #scale_y_continuous(
          #   expand = c(0,0),
           #  limits = c(-3.1, 0.1),
          #   breaks = seq(-4, 0, 0.5)
          # ) +
           labs(
             title = paste0(conc, " µg/ml Puromycin"),
             y = "OD 600",
             x = "Day",
             colour = "Culture:"
           ) +
           scale_color_viridis_d(option = "D") +
           theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 12
      ),
      legend.direction = "horizontal",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12,
                                  face = "bold"),
      panel.grid.minor.y = element_blank()
    )
  
  return(plot)
}

Mbovis_Puro_data_log <- import(
  file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mbovis_Puromycin_log10.csv")
)

Mb_all_plot <- map(c(0, 1, 2, 5, 10, 20), 
                   ~ Wachstumskurve_plot_einzelne_conc_legende_function(df = Mbovis_Puro_data_log, conc = .x))

plot_all_MB <- wrap_plots(Mb_all_plot, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

plot_all_MB

ggsave(
  file = here("AB_Versuch/Messwerte/graphs/log10", "AB_Mbovis_Puromycin_log10.pdf"),
  plot = plot_all_MB
)

#########################
#########################
#### Mthau
#########################
#########################

Mthau_puro_data <- import(
  file = here("AB_Versuch/Messwerte", "Messwerte_Mthau_Puromycin.csv")
)

summary(Mthau_puro_data)

#Mthau_puro_data_log_10 <- Mthau_puro_data |> 
#  mutate(
   # OD600_adj = if_else(OD600 <= 0, 0.001, OD600),
  #  OD600_adj_lg = log10(OD600_adj)
 # )

#summary(Mthau_puro_data_log_10)


#export(Mthau_puro_data_log_10,
 #      file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mthau_Puromycin_log10.csv"))

Mthau_Puro_data_log <- import(
  file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mthau_Puromycin_log10.csv")
)

summary(Mthau_Puro_data_log)



#### neue Messwerte ergeänzen

#neue_werte_Mt <- data.frame(
#  culture = c("AB1", "AB1", "AB1", "AB1", "AB1", "AB1", 
#              "AB2", "AB2", "AB2", "AB2", "AB2", "AB2", 
#              "AB3", "AB3", "AB3", "AB3", "AB3", "AB3"),
#  concentration.µg.ml = c(0, 1, 2, 5, 10, 20, 
#                          0, 1, 2, 5, 10, 20, 
#                          0, 1, 2, 5, 10, 20),
#  day = rep(10, 18),
#  OD600 = c(-0.013, -0.009, 0.027, -0.029, -0.037, -0.038, 
#           -0.01, 0.008, -0.012, -0.027, -0.038, -0.024, 
#          0.012, 0.054, 0.037, -0.023, -0.025, -0.039)
#)

#day = rep(10, 18),
#OD600 = c(-0.013, -0.009, 0.027, -0.029, -0.037, -0.038, 
#         -0.01, 0.008, -0.012, -0.027, -0.038, -0.024, 
#        0.012, 0.054, 0.037, -0.023, -0.025, -0.039)

#Mthau_puro_data <- rbind(Mthau_puro_data, neue_werte_Mt)
#show(Mthau_puro_data)

#write.csv(Mthau_puro_data, file = "Messwerte_Mthau_Puromycin.csv", row.names = FALSE)


#Mthau_puro_means_log <- Mthau_Puro_data_log |> 
#  group_by(day, concentration.µg.ml) |> 
#  summarise(
#    OD600_mean = mean(OD600, na.rm = TRUE),
#    sd_OD600 = sd(OD600, na.rm = TRUE),
#    OD600_log_mean = mean(OD600_adj_lg, na.rm = TRUE),
#    sd_log_OD600 = sd(OD600_adj_lg, na.rm = TRUE),
#    .groups = "drop"
#  )



#export(Mthau_puro_means_log,
#       file = here("AB_Versuch/Messwerte/log10", "Mthau_Puromycin_MEAN_log10.csv"))


Mthau_puro_means_lg <- import(
  file = here("AB_Versuch/Messwerte/log10", "Mthau_Puromycin_MEAN_log10.csv"))

Mthau_plot_sd <- ggplot(data = Mthau_puro_means_lg, aes( # sd ergänzen wenn errorbars mit angezeigtt werden
  x = day,
  y = OD600_mean,
  group = concentration.µg.ml 
)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-0.5, 11.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.49),
                     breaks = seq(-0.5, 0.6, 0.1)) +
  geom_line(linewidth = 0.5,
            colour = "black") +
  geom_errorbar(aes(ymin = OD600_mean - sd_OD600,
                   ymax = OD600_mean + sd_OD600,
                   colour = factor(concentration.µg.ml)),
              width = 0.4,
             size = 0.8) +
  geom_point(aes(colour = factor(concentration.µg.ml)), size = 3,
             shape = 18) + 
  scale_color_viridis_d(option = "D") +
  labs(
    x = "Day",
    y = "OD600",
    color = "Concentration [µg/ml]") +
  theme_bw()

Mthau_plot <- ggplot(data = Mthau_puro_means_lg, aes( # sd ergänzen wenn errorbars mit angezeigtt werden
  x = day,
  y = OD600_mean,
  group = concentration.µg.ml 
)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-0.5, 12.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.49),
                     breaks = seq(-0.5, 0.6, 0.1)) +
  geom_line(linewidth = 0.5,
            colour = "black") +
  #geom_errorbar(aes(ymin = OD600_mean - sd_OD600,
  #                  ymax = OD600_mean + sd_OD600,
  #                  colour = factor(concentration.µg.ml)),
  #              width = 0.4,
  #              size = 0.8) +
  geom_point(aes(colour = factor(concentration.µg.ml)), size = 3,
             shape = 18) + 
  scale_color_viridis_d(option = "D") +
  labs(
    x = "Day",
    y = "OD600",
    color = "Concentration [µg/ml]") +
  theme_bw()

Mthau_plot

Mthau_plot_sd

ggsave(
  file = here("AB_Versuch/Messwerte/graphs", "AB_Mthau_Puromycin_mean.pdf"),
  plot = Mthau_plot
)

ggsave(
  file = here("AB_Versuch/Messwerte/graphs", "AB_Mthau_Puromycin_mean_sd.pdf"),
  plot = Mthau_plot_sd
)

Mthau_puro_data[
  Mthau_puro_data$concentration.µg.ml == 5 &
    Mthau_puro_data$day == 3,
]
#culture concentration.µg.ml day  OD600
#16     AB1                   5   3 -0.002
#46     AB2                   5   3  0.001
#76     AB3                   5   3  0.176

#Wert fällt schon krass raus für AB3 und 5 µg/ml -> flasche Probe gemessen?


####################################
######### Für log mean jetzt
###################################

Mt_plot_lg <- ggplot( Mthau_puro_means_lg,
                      aes(
                        x = day,
                        y = OD600_log_mean,
                        group = concentration.µg.ml
                      )) +
  geom_line( colour = "black",
             linewidth = 0.25) +
  geom_point(aes(colour = factor(concentration.µg.ml)),
             shape = 18,
             size = 3) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(-0.5, 12.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(-3.6, 0.1),
    breaks = seq(-4, 0, 0.5)
  ) +
  labs(
    y = "lg(OD 600)",
    x = "Day",
    colour = "Concentration [µg/ml]"
  ) +
  scale_colour_viridis_d(option = "D") +
  theme_bw()

Mt_plot_lg

ggsave(
  filename = here("AB_Versuch/Messwerte/graphs/log10", "AB_Mthau_Puromycin_mean_log10.pdf"), 
  plot = Mt_plot_lg
)

Mt_plot_lg_sd <- ggplot( Mthau_puro_means_lg,
                         aes(
                           x = day,
                           y = OD600_log_mean,
                           group = concentration.µg.ml
                         )) +
  geom_line( colour = "black",
             linewidth = 0.25) +
  geom_point(aes(colour = factor(concentration.µg.ml)),
             shape = 18,
             size = 3) +
  geom_errorbar( aes(ymin = OD600_log_mean - sd_log_OD600,
                     ymax = OD600_log_mean + sd_log_OD600,
                     colour = factor(concentration.µg.ml)),
                 width = 0.4,
                 linewidth = 0.8
  ) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(-0.5, 12.5),
    breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(-3.6, 0.1),
    breaks = seq(-4, 0, 0.5)
  ) +
  labs(
    y = "lg(OD 600)",
    x = "Day",
    colour = "Concentration [µg/ml]"
  ) +
  scale_colour_viridis_d(option = "D") +
  theme_bw()


Mt_plot_lg_sd

ggsave(
  filename = here("AB_Versuch/Messwerte/graphs/log10", "AB_Mthau_Puromycin_mean_log10_sd.pdf"), 
  plot = Mt_plot_lg_sd
)


#############################
######### Jetzt wieder nicht gemittelt, sondern OD direkt für jede einzelne Konzentration
############################


Mthau_Puro_data_log <- import(
  file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mthau_Puromycin_log10.csv")
)

Mt_all_plot <- map(c(0, 1, 2, 5, 10, 20), 
                   ~ Wachstumskurve_plot_einzelne_conc_legende_function(df = Mthau_Puro_data_log, conc = .x))

plot_all_MT <- wrap_plots(Mt_all_plot, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

plot_all_MT

ggsave(
  file = here("AB_Versuch/Messwerte/graphs/log10", "AB_Mthau_Puromycin_log10.pdf"),
  plot = plot_all_MT
)






##############################
########### Jetzt alle zusammen in einem plot
###############################

Wachstumskurve_plot_einzelne_conc_legende_Methano_function <- function(df, conc, Meth){
  
  df1 <- df  |> 
    mutate(culture = recode(culture,
                            "AB1" = "Cltr1",
                            "AB2" = "Cltr2",
                            "AB3" = "Cltr3"))
  
  plot <- ggplot(data = df1[df1$concentration.µg.ml == conc, ], # braucht in [] zwingend ein Komma [Zeile, Spalte]
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
      limits = c(-0.5, 11.5),
      breaks = c(-2 ,0, 2, 4, 6, 8, 10, 12)
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
    #  breaks = seq(-4, 0, 1)
    #) +
    labs(
      title = paste0(conc, " µg/ml Puromycin - *", Meth, "*"),
      y = "OD 600",
      x = "Day",
      colour = "Culture:"
    ) +
    scale_color_viridis_d(option = "D") +
    theme_bw() +
    theme(
      plot.title = element_markdown( # element_markdown, nicht _text damit es ** als kursiv versteht
        hjust = 0.5,
        face = "bold",
        size = 12
      ),
      legend.direction = "horizontal",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12,
                                  face = "bold"),
      panel.grid.minor.y = element_blank()
    )
  
  return(plot)
}
Mb_all_plot_zsm <- map(c(0, 1, 2, 5, 10, 20), ~
                         Wachstumskurve_plot_einzelne_conc_legende_Methano_function(df = Mbovis_Puro_data_log, conc = .x, Meth = "M. bovi"))

Mt_all_plot_zsm <- map(c(0, 1, 2, 5, 10, 20), ~
                         Wachstumskurve_plot_einzelne_conc_legende_Methano_function(df = Mthau_Puro_data_log, conc = .x, Meth = "M. thau"))


plot_all_MT_MB <- wrap_plots(c(Mb_all_plot_zsm, Mt_all_plot_zsm), ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

plot_all_MT_MB

ggsave(
  file = here("AB_Versuch/Messwerte/graphs/log10", "AB_Mthau_&_Mbovi_Puromycin_log10.pdf"),
  plot_all_MT_MB, 
  width = 26,
  height = 24,
  units = "cm"
)
