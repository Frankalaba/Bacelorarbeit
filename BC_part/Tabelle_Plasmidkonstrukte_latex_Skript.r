####################### Tabelle für Plasmide


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


plasmid_table <- tibble(
  name = c("pMt_0321_hpt", "pMt_0231_hpt", "pMt_0331_hpt", "pMt_0241_hpt", "pMt_0341_hpt", "pMb_0231_hpt", "pMb_0331_hpt"),
  "ORI" = rep("NO (0)", 7),
  Promoter = c("PglnA (3)", "hmtB (2)", "PglnA (3)", "hmtB (2)", "PglnA (3)", "hmtB (2)", "PglnA (3)"),
  pac = c("pac_old (2)", "pac_opt (3)", "pac_opt (3)", "pac_org (4)", "pac_org (4)", "pac_opt (3)", "pac_opt (3)"),
  X = rep("hpt (1)", 7)
)

plasmid_table_latex <- xtable(plasmid_table,
                              caption = "Platzhalter",
                              label = "fig:plasmid_konstrukte")

print(plasmid_table_latex,
      file = here("../Text_Production/tables/04_result", "Plasmid_konstrukte_alle.tex"),
      booktabs = TRUE,
      floating = TRUE,
      include.rownames = FALSE,
      caption.placement = "top")
