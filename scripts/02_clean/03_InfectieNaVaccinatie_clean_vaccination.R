data_vaccinatie <- data_vaccinatie_vasco %>% 
  filter(StudyId %in% data_InfectieNaVaccinatie$StudyId) %>% 
  select(StudyId, (starts_with("Vaccinatiedatum") | 
                     starts_with("Invuldatum") |
                     starts_with("Vaccinatiewelke")) & !contains("final")) %>% 
  pivot_longer(
    cols = -StudyId,
    names_to = c('.value', 'vragenlijst'), 
    names_pattern = '(.*?)_(.*)') %>% 
  # Filter op vaccinatiedatum. Zonder vaccinatiedatum geen vaccinatiestatus.
  filter(!is.na(Vaccinatiedatum)) %>% 
  arrange(StudyId, Vaccinatiedatum) %>% 
  group_by(StudyId) %>% 
  mutate(interval_vaccinaties = Vaccinatiedatum - lag(Vaccinatiedatum)) %>% 
  # Eerste vacc is NA en daarna moet er 3 weken tussen zitten.
  filter(is.na(interval_vaccinaties) | interval_vaccinaties > (3 * 7)) %>% 
  mutate(vacc_nr = row_number()) %>% 
  ungroup() %>% 
  select(!interval_vaccinaties) %>% 
  pivot_wider(id_cols = StudyId,
              names_from = vacc_nr,
              values_from = c(Vaccinatiedatum, Invuldatum, Vaccinatiewelke, vragenlijst))



data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  left_join(
    # select vacc variables
    data_vaccinatie %>% select(StudyId, 
                               starts_with("Vaccinatiedatum"),
                               starts_with("Vaccinatiewelke")),
    by = "StudyId") %>% 
  # left_join(data_CIMS_vasco,
  #           by = "StudyId") %>% 
  mutate(
    across(
      .cols = starts_with("vacc_dat_"), .fns = ~na_if(., as_date("1900-01-01"))
    ),
    vacc_status = case_when(
      DUFS_bloedafname >= Vaccinatiedatum_3 + 7 |
        (DUFS_bloedafname >= Vaccinatiedatum_2 + 7 & 
           Vaccinatiewelke_1 == "Janssen") ~ "booster",
      
      DUFS_bloedafname >= Vaccinatiedatum_2 + 14 | 
        (DUFS_bloedafname >= Vaccinatiedatum_1 + 28 & 
        Vaccinatiewelke_1 == "Janssen") ~ "full",
      
      DUFS_bloedafname >= Vaccinatiedatum_1 ~ "partially") %>%
      factor(levels = c("partially", "full", "booster")),
    
    vacc_dose =  case_when(
      DUFS_bloedafname > Vaccinatiedatum_5 ~ "5 dose",
      DUFS_bloedafname > Vaccinatiedatum_4 ~ "4 dose",
      DUFS_bloedafname > Vaccinatiedatum_3 ~ "3 dose",
      DUFS_bloedafname > Vaccinatiedatum_2 ~ "2 dose",
      DUFS_bloedafname > Vaccinatiedatum_1 ~ "1 dose",
      DUFS_bloedafname <= Vaccinatiedatum_1 ~ "0 dose",
      TRUE ~ "UNK") %>% 
      factor(levels = c("UNK", "0 dose", "1 dose", "2 dose", "3 dose", "4 dose", "5 dose")),
    vacc_time = case_when(
      DUFS_bloedafname > Vaccinatiedatum_5 ~ DUFS_bloedafname - Vaccinatiedatum_5,
      DUFS_bloedafname > Vaccinatiedatum_4 ~ DUFS_bloedafname - Vaccinatiedatum_4,
      DUFS_bloedafname > Vaccinatiedatum_3 ~ DUFS_bloedafname - Vaccinatiedatum_3,
      DUFS_bloedafname > Vaccinatiedatum_2 ~ DUFS_bloedafname - Vaccinatiedatum_2,
      DUFS_bloedafname > Vaccinatiedatum_1 ~ DUFS_bloedafname - Vaccinatiedatum_1
    ) %>% as.numeric(),
    vacc_merk_laatste = case_when(
      DUFS_bloedafname > Vaccinatiedatum_5 ~ Vaccinatiewelke_5,
      DUFS_bloedafname > Vaccinatiedatum_4 ~ Vaccinatiewelke_4,
      DUFS_bloedafname > Vaccinatiedatum_3 ~ Vaccinatiewelke_3,
      DUFS_bloedafname > Vaccinatiedatum_2 ~ Vaccinatiewelke_2,
      DUFS_bloedafname > Vaccinatiedatum_1 ~ Vaccinatiewelke_1
    ),
    
    vacc_merk_full = case_when(
      vacc_status == "partially" ~ NA_character_,
      Vaccinatiewelke_1 == "Janssen" ~ "Janssen",
      is.na(Vaccinatiewelke_2) ~ Vaccinatiewelke_1,
      is.na(Vaccinatiewelke_1) & is.na(Vaccinatiewelke_2) ~ "UNK",
      TRUE ~ Vaccinatiewelke_2
    ),
    
    vacc_merk_booster = case_when(
      vacc_status == "booster" & (is.na(vacc_merk_laatste) | 
                                    !vacc_merk_laatste %in% 
                                    c("BioNTech/ Pfizer (Comirnaty)", 
                                      "Moderna (Spikevax)")) ~ "UNK",
      vacc_status == "booster" ~ vacc_merk_laatste,
      TRUE ~ NA_character_
    )
  )

data_InfectieNaVaccinatie_pre_serology <- data_InfectieNaVaccinatie_pre_serology %>% 
    left_join(
      # select vacc variables
      data_vaccinatie %>% select(StudyId, 
                                  starts_with("Vaccinatiedatum"),
                                  starts_with("Vaccinatiewelke")),
      by = "StudyId") %>% 
  # left_join(data_CIMS_vasco,
  #           by = "StudyId") %>% 
  mutate(
    across(
      .cols = starts_with("vacc_dat_"), .fns = ~na_if(., as_date("1900-01-01"))
    ),
    vacc_status = case_when(
      DUFS_bloedafname_pre >= Vaccinatiedatum_3 + 7 |
        (DUFS_bloedafname_pre >= Vaccinatiedatum_2 + 7 & 
           Vaccinatiewelke_1 == "Janssen") ~ "booster",
      
      DUFS_bloedafname_pre >= Vaccinatiedatum_2 + 14 | 
        (DUFS_bloedafname_pre >= Vaccinatiedatum_1 + 28 & 
           Vaccinatiewelke_1 == "Janssen") ~ "full",
      
      DUFS_bloedafname_pre > Vaccinatiedatum_1 ~ "partially",
      
      DUFS_bloedafname_pre <= Vaccinatiedatum_1 ~ "unvaccinated") %>%
      factor(levels = c("unvaccinated", "partially", "full", "booster")),
    
    vacc_dose =  case_when(
      DUFS_bloedafname_pre > Vaccinatiedatum_5 ~ "5 dose",
      DUFS_bloedafname_pre > Vaccinatiedatum_4 ~ "4 dose",
      DUFS_bloedafname_pre > Vaccinatiedatum_3 ~ "3 dose",
      DUFS_bloedafname_pre > Vaccinatiedatum_2 ~ "2 dose",
      DUFS_bloedafname_pre > Vaccinatiedatum_1 ~ "1 dose",
      DUFS_bloedafname_pre <= Vaccinatiedatum_1 ~ "0 dose",
      TRUE ~ "UNK") %>% 
      factor(levels = c("UNK", "0 dose", "1 dose", "2 dose", "3 dose", "4 dose", "5 dose")),
    
    vacc_time = case_when(
      DUFS_bloedafname_pre > Vaccinatiedatum_5 ~ DUFS_bloedafname_pre - Vaccinatiedatum_5,
      DUFS_bloedafname_pre > Vaccinatiedatum_4 ~ DUFS_bloedafname_pre - Vaccinatiedatum_4,
      DUFS_bloedafname_pre > Vaccinatiedatum_3 ~ DUFS_bloedafname_pre - Vaccinatiedatum_3,
      DUFS_bloedafname_pre > Vaccinatiedatum_2 ~ DUFS_bloedafname_pre - Vaccinatiedatum_2,
      DUFS_bloedafname_pre > Vaccinatiedatum_1 ~ DUFS_bloedafname_pre - Vaccinatiedatum_1
    ) %>% as.numeric()
      )