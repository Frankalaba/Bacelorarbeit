################################
########### Um Tabellen in Latex format umzuwandeln
###############################

rm(list = ls())

#install.packages("xtable")

library("tidyr")
library("dplyr")
library("ggplot2")
library("viridis")
library("here")
library("rio")
library("patchwork")
library("purrr")
library("ggtext")
library("xtable")

here()

################## Für Zellzahl


####### Allgemeine Tabelle

Zellzahl_table <- import(file = here("Zellzahl", "Methanobrevibacter_OD_cellnumber_original.csv"))

Zellzahl_table_latex <- xtable(Zellzahl_table, 
                               label = "tab:cellnumb",
                               caption = "Results of counting chamber",
                               digits = 3)


print(Zellzahl_table_latex,
      file = here("../Text_Production/tables", "Methanos_OD_cellnumber.tex"),
      booktabs = TRUE,
      include.rownames = FALSE,
      caption.placement = "top")

############ Für die einzelnen vor der Berechnung der Zellzahl pro ml

Transform_to_latex_Zellzahl_function  <-  function(df, lab, fl){
test <- import(file = here("Zellzahl", df))

test1 <- test |>
  select(-c(V1, cellnumber_per_ml)) |> 
  mutate(ø_cellnumber = gsub(",", ".", ø_cellnumber),
         chamber = as.character(chamber)) |> 
  rename(
    "Sqr. 1" = Quadrat_1,
    "Sqr. 2" = Quadrat_2,
    "Sqr. 3" = Quadrat_3,
    "Sqr. 4" = Quadrat_4,
    "ø cellnumber" = ø_cellnumber
  )

n_cols <- ncol(test1)

latex_tabelle <- xtable(
  test1,
  label = paste0("tab:", lab),
  caption = "Platzhalter",
  digits = 2,
  align = c("l", rep("c", n_cols))
)

print(latex_tabelle,
      file = here("../Text_Production/tables/04_result", fl),
      booktabs = TRUE,
      include.rownames = FALSE,
      caption.placement = "top"
      )}

Transform_to_latex_Zellzahl_function(df = "Mwoli_OD_0.396_cellnumber_2.csv", 
                                     lab = "cellnmr_wol_Neu_2", 
                                     fl = "Mwoli_OD_0.396_cellnumber_2.tex")              

Transform_to_latex_Zellzahl_function(df = "Mbovis_OD_0.287_1.csv", 
                                     lab = "cellnmr_bovi", 
                                     fl = "Mbovis_OD_0.287_1.tex")              

Transform_to_latex_Zellzahl_function(df = "Mmilli_OD_0.378_1.csv", 
                                     lab = "cellnmr_milli", 
                                     fl = "Mmilli_OD_0.378_1.tex")   

Transform_to_latex_Zellzahl_function(df = "Mthau_OD_0.116_cellnumber_Y.csv", 
                                     lab = "cellnmr_tha_Y", 
                                     fl = "Mthau_OD_0.116_cellnumber_Y.tex")  

Transform_to_latex_Zellzahl_function(df = "Mthau_OD_0.235_cellnumber_X.csv", 
                                     lab = "cellnmr_thau_X", 
                                     fl = "Mthau_OD_0.235_cellnumber_X.tex")   

Transform_to_latex_Zellzahl_function(df = "Mwoli_OD_387_cellnumber_1.csv", 
                                     lab = "cellnmr_wol_Bürk_1", 
                                     fl = "Mwoli_OD_387_cellnumber_1.tex")   





###########################################################
############# AB versuch
###########################################################


Mthau_Puro_data_log <- import(
  file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mthau_Puromycin_log10.csv")
) |> 
  select(-c(OD600_adj_lg)) |> 
  relocate(day, .after = culture) |> 
  rename("conc. [µg/ml]" = concentration.µg.ml,
         "OD600 adj." = OD600_adj) |> 
  mutate(culture = recode(culture, 
                          "AB1" = "Cltr. 1",
                          "AB2" = "Cltr. 2",
                          "AB3" = "Cltr. 3")) |> 
  arrange(culture)

names(Mthau_Puro_data_log)

Mthau_Puro_data_log_latex <- xtable(Mthau_Puro_data_log,
                                     caption = "thau",
                                     digits = 3,
                                     label = "tab:AB_Mthau_sens")
print(Mthau_Puro_data_log_latex,
      file = here("../Text_Production/tables/04_result", "Puro_Mthau_sens.tex"),
      booktabs = TRUE,
      tabular.environment = "longtable",
      floating = FALSE,
      include.rownames = FALSE,
      caption.placement = "top")




################## M bovis

Mbovis_Puro_data_log <- import(
  file = here("AB_Versuch/Messwerte/log10", "Messwerte_Mbovis_Puromycin_log10.csv")
) |> 
  select(-c(OD600_adj_lg)) |> 
  relocate(day, .after = culture) |> 
  rename("conc. [µg/ml]" = concentration.µg.ml,
         "OD600 adj." = OD600_adj) |> 
  mutate(culture = recode(culture, 
                          "AB1" = "Cltr. 1",
                          "AB2" = "Cltr. 2",
                          "AB3" = "Cltr. 3")) |> 
  arrange(culture)

names(Mbovis_Puro_data_log)

Mbovis_Puro_data_log_latex <- xtable(Mbovis_Puro_data_log,
                                     caption = "bovi",
                                     digits = 3,
                                     label = "tab:AB_Mbovi_sens")
print(Mbovis_Puro_data_log_latex,
      file = here("../Text_Production/tables/04_result", "Puro_Mbovis_sens.tex"),
      booktabs = TRUE,
      tabular.environment = "longtable",
      floating = FALSE,
      include.rownames = FALSE,
      caption.placement = "top")


