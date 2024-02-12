message("clean -- symptoms")

# Questionaire 1 month after infection.
data_1M_infectie <- data_1M_infectie_org %>% 
  select(StudyId, AnswerDate, 
         Coronatest_symptomen_corona_1M_inf = Q01_Heeft_u_in_de_afgelopen_maand_klachten_gehad_die_op_het_coronavirus_kunnen_wijzen___Hierbij_bedoelen_we_zowel_klachten_die_u_had_op_het_moment_dat_u_zich_heeft_laten_testen_als_klachten_die_ontstaan_zijn_nadat_u_een_zelftest_heeft_gedaan_of_u_z,
         Coronatest_symptomen_corona_welke_1M_inf = Q02_Welke_klacht_en__had_u__Er_zijn_meerdere_antwoorden_mogelijk,
         Coronatest_symptomen_wanneer_1M_inf = Q04_Wanneer_begon_nen__deze_klacht_en____Als_u_de_datum_niet_meer_precies_weet__is_een_datum_bij_benadering_ook_goed_,
         Q05_Heeft_u_nog_steeds_last_van_deze_klacht_en__,
         Q09_Bent_u_voor_deze_klacht_en__opgenomen__geweest__in_het_ziekenhuis_,
         Q08_Heeft_u_voor_deze_klacht_en__de_huisarts_bezocht_of_telefonisch_contact_gehad_) %>% 
  left_join(data_InfectieNaVaccinatie %>% select(StudyId, DUFS_pos_test),
            by = "StudyId") %>% 
  # Calculate interval response questionaire and positive test.
  mutate(int_invul_test = as.numeric(as_date(AnswerDate) - DUFS_pos_test)) %>% 
  filter(int_invul_test %in% seq(15,65,1)) %>% 
  # individuals with multiple questionaires, take the first Q
  arrange(AnswerDate) %>% 
  group_by(StudyId) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(!c(int_invul_test, DUFS_pos_test))

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  left_join(
    data_1M_infectie, 
    by = "StudyId"
  ) 




data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  mutate(
    # combine questionaires 1M inf, T and M in correct order
    Coronatest_symptomen_corona_c = case_when(
      # vragenlijsten 1M inf
      Coronatest_symptomen_corona_1M_inf == "Ja" ~ "Symptomatic",
      Coronatest_symptomen_corona_1M_inf == "Nee" ~ "Asymptomatic",
      # vragenlijsten T
      Coronatest_symptomen_corona == "Ja" ~ "Symptomatic",
      Coronatest_symptomen_corona == "Nee" ~ "Asymptomatic",
      # vragenlijsten M
      Symptomen_corona_afgelopen_maand  == "Ja" ~ "Symptomatic",
      Symptomen_corona_afgelopen_maand  == "Nee" ~ "Asymptomatic"
      
      #NCOVVast1eziektedag %in% c("G", "V") ~ "Symptomatic",
      #NCOVVast1eziektedag == "NVT" ~ "Asymptomatic"
    ),
    Coronatest_symptomen_corona_welke_c = case_when(
      !is.na(Coronatest_symptomen_corona_1M_inf) ~ Coronatest_symptomen_corona_welke_1M_inf,
      !is.na(Coronatest_symptomen_corona) ~ Coronatest_symptomen_corona_welke,
      TRUE ~ Symptomen_corona_afgelopen_maand_welke
    ),
    Coronatest_symptomen_wanneer_c = case_when(
      !is.na(Coronatest_symptomen_corona_1M_inf) ~ as_date(Coronatest_symptomen_wanneer_1M_inf),
      !is.na(Coronatest_symptomen_corona) ~ Coronatest_symptomen_wanneer,
      TRUE ~ Symptomen_corona_afgelopen_maand_wanneer),
      
    Coronatest_symptomen_aantal = 
      if_else(Coronatest_symptomen_corona_c == "Asymptomatic",
              0,
              Coronatest_symptomen_corona_welke_c %>% str_count(",") + 1),
    
    Coronatest_symptomen_koorts_malaise = case_when(
      Coronatest_symptomen_corona_c == "Asymptomatic" ~ F,
      Coronatest_symptomen_corona_welke_c %>% str_detect("Koorts/koude rillingen|Algehele malaise") ~ T,
      Coronatest_symptomen_corona_welke_c %>% str_detect("Koorts/koude rillingen|Algehele malaise",negate = T ) ~ F
    )
  )


data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  mutate(
    Klacht_Koorts                          = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,
      Coronatest_symptomen_corona_welke_c %>% 
      str_detect("Koorts/koude rillingen") %>% 
      as.numeric()),
    Klacht_Malaise                         = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,
      Coronatest_symptomen_corona_welke_c %>% 
      str_detect("Algehele malaise") %>% 
      as.numeric()),
    Klacht_Hoesten                         = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Hoesten") %>% 
      as.numeric()),
    Klacht_Keelpijn                        = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Keelpijn") %>% 
      as.numeric()),
    Klacht_Loopneus                        = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Loopneus") %>% 
      as.numeric()),
    Klacht_Kortademigheid                  = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Kortademigheid") %>% 
      as.numeric()),
    Klacht_Diarree                         = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Diarree") %>% 
      as.numeric()),
    Klacht_Misselijkheid                   = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Misselijkheid/overgeven") %>% 
      as.numeric()),
    Klacht_Hoofdpijn                       = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Hoofdpijn") %>% 
      as.numeric()),
    Klacht_Prikkelbaarheid_verwarring      = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Prikkelbaarheid/verwarring") %>% 
      as.numeric()),
    Klacht_Spierpijn                       = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Spierpijn") %>% 
      as.numeric()),
    Klacht_Pijn_ademhaling                 = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Pijn bij ademhaling") %>% 
      as.numeric()),
    Klacht_Buikpijn                        = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Buikpijn") %>% 
      as.numeric()),
    Klacht_Gewrichtspijn                   = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Gewrichtspijn") %>% 
      as.numeric()),
    Klacht_Verlies_Geur_Smaak              = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Verlies van geur- en/of smaakvermogen") %>% 
      as.numeric()),
    Klacht_Extreme_vermoeidheid            = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Extreme vermoeidheid") %>% 
      as.numeric()),
    Klacht_Pijn_ogen                       = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Pijn_achter_de_ogen") %>% 
      as.numeric()),
    Klacht_Andere_klachten                 = if_else(
      Coronatest_symptomen_corona_c == "Asymptomatic", 0,Coronatest_symptomen_corona_welke_c %>% 
      str_detect(pattern = "Andere klachten") %>% 
      as.numeric()),
    
    # Similar classification as in: https://academic.oup.com/cid/article/73/12/2155/6149064
    mild_severe = case_when(
      Klacht_Koorts | Klacht_Kortademigheid | Klacht_Extreme_vermoeidheid |
        Klacht_Spierpijn | 
        Klacht_Malaise | Klacht_Pijn_ademhaling | Klacht_Gewrichtspijn |
        Klacht_Diarree | Klacht_Buikpijn ~ "Symptomatic",
      Coronatest_symptomen_corona_c == "Asymptomatic" | Klacht_Loopneus |
        Klacht_Keelpijn | Klacht_Verlies_Geur_Smaak | Klacht_Hoofdpijn |
        Klacht_Hoesten ~ 
        "Asymptomatic and mild symptoms",
      TRUE ~ "UNK"
    ) %>% factor(levels = 
                   c("Asymptomatic and mild symptoms", "Symptomatic", "UNK"))
    # niet in de lijst Klacht_Prikkelbaarheid_verwarring, Klacht_Pijn_ogen
    #Klacht_Misselijkheid Klacht_Hoesten
  )

