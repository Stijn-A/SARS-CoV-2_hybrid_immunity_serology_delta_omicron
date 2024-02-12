# Abstract
tabel_ratio_variant %>% 
  filter(comparison == "RBD Omicron BA.1 / RBD Delta") %>% group_by(V_Omicron_Delta) %>% summarise(mean = mean(ratio))

#Variant WGS in methods
59 + 99 + 7
round((59 + 99 + 7) / (59 + 99 + 7 + 279) * 100,1) # variant typed
data_InfectieNaVaccinatie %>% count(!is.na(Variant), !is.na(Clade))
35 / 165 * 100 # by v-pcr
130 / 165 * 100 # by WGS

data_InfectieNaVaccinatie %>% 
  count(V_Omicron_Delta, Variant)

data_InfectieNaVaccinatie %>%
  #mutate(variant = if_else(!is.na(Variant), Variant, V_Omicron_Delta)) %>% 
  tabyl(Variant) %>%
  adorn_totals("col") %>%
  adorn_pct_formatting(digits = 1)

data_InfectieNaVaccinatie %>%
  tabyl(vacc_status) %>%
  adorn_totals("col") %>%
  adorn_pct_formatting(digits = 1)

data_InfectieNaVaccinatie %>%
  tabyl(previous_infection) %>%
  adorn_totals("col") %>%
  adorn_pct_formatting(digits = 1)

data_InfectieNaVaccinatie %>%
  filter(!is.na(pre_N_status_txt )) %>% 
  tabyl(previous_infection, pre_N_status_txt) %>% 
  adorn_totals("col") %>%
  adorn_percentages() %>%
  adorn_pct_formatting(digits = 1)

data_InfectieNaVaccinatie %>% 
  mutate(
    schedule = case_when(
      vacc_dose %in% c("3 dose", "4 dose", "5 dose") |
        (Vaccinatiewelke_1 == "Janssen" & vacc_dose == "2 dose") ~ "booster or more",
      vacc_dose == "2 dose" | (Vaccinatiewelke_1 == "Janssen" & vacc_dose == "1 dose") ~ "primairy",
      TRUE ~ "partilally"
    )
  ) %>% 
  count(schedule) %>% 
  adorn_percentages(denominator = "all") %>% 
  adorn_pct_formatting


data_InfectieNaVaccinatie_pre_serology %>% 
  filter(vacc_dose == "0 dose" & pre_N_BAU_ml < 14.3) %>% 
  summarise(geom_mean = exp(mean(log(pre_S1_BAU_ml))))%>% as.data.frame()

data_InfectieNaVaccinatie_pre_serology %>% 
  filter(vacc_dose != "0 dose" & pre_N_BAU_ml < 14.3) %>% 
  summarise(geom_mean = exp(mean(log(pre_S1_BAU_ml)))) %>% as.data.frame()


data_InfectieNaVaccinatie %>%
  mutate(
    wk_1_3 = case_when(
      interval_bloedafname_PCR %in% seq(1, 7, 1) ~ "wk 1",
      interval_bloedafname_PCR %in% seq(8, 14, 1) ~ "wk 2",
      interval_bloedafname_PCR %in% seq(15, 21, 1) ~ "wk 3",
      interval_bloedafname_PCR %in% seq(15 + 7, 21 + 7, 1) ~ "wk 4",
      interval_bloedafname_PCR %in% seq(15 + 7 + 7, 21 + 7+ 7, 1) ~ "wk 5",
      interval_bloedafname_PCR %in% seq(15 + 7 + 7 + 7, 21 + 7+ 7 + 7, 1) ~ "wk 6",
      TRUE ~ "OTHER"
    )
  ) %>%
  group_by(wk_1_3, previous_infection) %>%
  summarise(geom_mean_S1 = round(exp(mean(log(`S1 (BAU/ml)`))),1),
            geom_mean_N = round(exp(mean(log(`N (BAU/ml)`))),1),
            n = n()) %>% as.data.frame()


data_InfectieNaVaccinatie %>% 
  mutate(pre_N_status_txt = factor(pre_N_status_txt)) %>% 
  filter(pre_N_status == 1) %>% 
  count(post_N_status)

data_InfectieNaVaccinatie %>% 
  mutate(pre_N_status_txt = factor(pre_N_status_txt)) %>% 
  filter(pre_N_status == 1) %>% 
  pull(interval_bloedafname_PCR)

data_InfectieNaVaccinatie %>% count(previous_infection, post_N_status)

data_InfectieNaVaccinatie %>% count(cohorts_bloedname_PCR, previous_infection, post_N_status)

# methods
data_InfectieNaVaccinatie %>% 
  mutate(interval_pos_test_bloedname_pre = DUFS_bloedafname_pre - DUFS_pos_test) %>% 
  summarise(min(interval_pos_test_bloedname_pre, na.rm = T), 
            max(interval_pos_test_bloedname_pre, na.rm = T),
            median(interval_pos_test_bloedname_pre, na.rm = T))

data_InfectieNaVaccinatie %>% 
  mutate(interval_pos_test_bloedname_post = DUFS_bloedafname - DUFS_pos_test) %>% 
  summarise(min(interval_pos_test_bloedname_post, na.rm = T), 
            max(interval_pos_test_bloedname_post, na.rm = T),
            median(interval_pos_test_bloedname_post, na.rm = T))

data_InfectieNaVaccinatie %>% 
  summarise(median(Leeftijd, na.rm = T), quantile(Leeftijd, 1/4, na.rm = T), quantile(Leeftijd, 3/4, na.rm = T))

data_InfectieNaVaccinatie %>% summarise(sum(Geslacht == "Vrouw") / n() * 100)


data_InfectieNaVaccinatie %>% count(vacc_dose, vacc_status, Vaccinatiewelke_1)

