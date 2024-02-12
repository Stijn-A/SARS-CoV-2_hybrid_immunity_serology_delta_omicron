## packages
lapply(c("tidyverse", "lubridate", "readxl", "writexl", "mgcv", "ggsignif",
         "cowplot", "broom", "RColorBrewer",  "tableone"), 
       require, character.only = TRUE)

# do not use exponetial notation ( 1E+05 instead use 100000)
options(scipen = 999)

source(file = "scripts/01_import/01_InfectieNaVaccinatie_import.R")

# clean data
source(file = "scripts/02_clean/01_InfectieNaVaccinatie_clean_serology.R")
source(file = "scripts/02_clean/02_InfectieNaVaccinatie_clean_symptoms.R")
source(file = "scripts/02_clean/03_InfectieNaVaccinatie_clean_vaccination.R")

# Tab 1
source(file = "scripts/03_figures_tables/04_table_overview.R")

# Fig 1
source(file = "scripts/03_figures_tables/03_figure_tijd_PCR_TEST_Ab_response.R")

source(file = "scripts/03_figures_tables/03_figure_histogram.R")

# Fig 2
source(file = "scripts/04_models/05_model_LR_N_basis.R")
source(file = "scripts/05_model_LR_N_symptoms.R")

source(file = "scripts/05_model_LR_N_other.R")

# Fig 3 and S2
source(file = "scripts/03_figures_tables/03_figure_vacc.R")

# Fig 4 and S3
source(file = "scripts/03_figures_tables/03_Figure_variant_directed_response.R")

# Fig S1
source(file = "scripts/03_figures_tables/03_figure_response_interval_week.R")


# 
# source(file = "scripts/05_model_LR_N_vacc.R")

source(file = "scripts/03_figures_tables/05_numbers_in_txt.R")


source(file = "scripts/05_output/05_InfectieNaVaccinatie_output.R")


