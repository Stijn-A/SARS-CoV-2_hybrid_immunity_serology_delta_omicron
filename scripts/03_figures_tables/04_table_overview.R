
tabel1 <- data_InfectieNaVaccinatie %>% 
  mutate(pre_sample = case_when(!is.na(pre_N_BAU_ml) ~ "present lab",
                                !is.na(seroN) ~  "present STAR",
                                TRUE ~ "missing"),
         post_sample = case_when(!is.na(`N (BAU/ml)`) ~ "present lab",
                                 TRUE ~ "missing"),
         Sex = recode(Geslacht, Man = "Male", Vrouw = "Female"),
         `Number of pre-infection samples` = factor(pre_n_samples),
         Variant = V_Omicron_Delta %>% replace_na("Unknown"),
         symptoms = case_when(
           Coronatest_symptomen_corona_c == "Asymptomatic" ~ "Asymptomatic",
           Coronatest_symptomen_corona_c == "Symptomatic" &
             mild_severe == "Asymptomatic and mild symptoms" ~ "Mild symptomatic",
           Coronatest_symptomen_corona_c == "Symptomatic" &
             mild_severe == "Symptomatic" ~ "Symptomatic",
           TRUE ~ "Unknown"),
         previous_infection = previous_infection %>% replace_na("Unknown")
         ) %>% 
  rename(`Age group` = Leeftijds_groep,
        `COVID-19\nsymptom status` = symptoms,
        `Evidence of\nprevious infection` = previous_infection,
        `Vaccination status` = vacc_status,
        `Vaccine type\nprimary series` = vacc_merk_full,
        `Vaccine type\nbooster series` = vacc_merk_booster,
        `Interval between pre-infection sample/nand SARS-CoV-2 positive test (days)` = interval_bloedafname_pre_PCR,
        `Interval between post-infection sample/nand SARS-CoV-2 positive test (days)` = interval_bloedafname_PCR) %>% 
  CreateTableOne(data = .,
               vars = c("Age group",
                        "Sex",
                        # "Variant",
                        "COVID-19\nsymptom status",
                        "Number of pre-infection samples",
                        "Interval between pre-infection sample/nand SARS-CoV-2 positive test (days)",
                        "Interval between post-infection sample/nand SARS-CoV-2 positive test (days)",
                        "Evidence of\nprevious infection",
                        "Vaccination status",
                        "Vaccine type\nprimary series",
                        "Vaccine type\nbooster series"),
               strata = "Variant"
               )

tabel1_csv <- print(
  tabel1,
  showAllLevels = TRUE,
  printToggle = FALSE,
  catDigits = 1
)

