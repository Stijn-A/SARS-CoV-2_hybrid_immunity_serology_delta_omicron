message("clean -- serology")

# Steps data cleaning:
# (1) clean additional samples (provided without StudyId and KitID)
# Koppelen KitIds aan bloedafnamedatums
koppeltabel_KitId_bloedafnamedatum <- data_A10_vingerprik_org %>% 
  # filter op StudyIds die in de substudie zittem
  filter(#StudyId %in% data_InfectieNaVaccinatie$StudyId & 
           !is.na(DateReceived)) %>% 
  select(StudyId, KitId, DateReceived) %>% 
  left_join(data_A8_vingerprik_org %>% 
              select(StudyId, Q02_Wat_is_de_datum_waarop_u_het_bloed_afgenomen_heeft_),
            by = c("StudyId")) %>% 
  mutate(DateReceived = as_date(DateReceived),
         datum_bloedafname_pre = as_date(Q02_Wat_is_de_datum_waarop_u_het_bloed_afgenomen_heeft_),
         # DR: DateReceived, BN: Bloedname
         interval_DR_BN = abs(DateReceived - datum_bloedafname_pre)) %>% 
  filter(datum_bloedafname_pre > DateReceived - 14 & datum_bloedafname_pre < DateReceived + 14) %>% 
  select(StudyId, KitId, datum_bloedafname_pre, DateReceived, interval_DR_BN) %>% 
  distinct(StudyId, KitId, datum_bloedafname_pre, .keep_all = T) %>% 
  arrange(KitId, interval_DR_BN) %>% 
  group_by(KitId) %>% 
  slice(1) %>% 
  ungroup

# koppelen monsters
data_VASCO_TubeID <- data_VASCO_compleet %>% # Deze VASCO compleet is gefilterd op deelnemers van de substudie
  select(StudyId, starts_with("KitID"), starts_with("KitDate")) %>% 
  pivot_longer(cols = starts_with("KitID"),
               values_to = "KitID",
               names_prefix = "KitID_",
               names_to = c("Meetmoment")) %>% 
  pivot_longer(cols = starts_with("KitDate"),
               values_to = "KitDate",
               names_prefix = "KitDate_",
               names_to = c("Meetmoment_Date")) %>% 
  filter(Meetmoment == Meetmoment_Date & !is.na(KitID)) %>% 
  select(!Meetmoment_Date) %>% 
  mutate(Meetmoment = Meetmoment %>% factor(levels = c("0M", "1M_vacc", "6M", "NA"))) %>% 
  left_join(
    koppeltabel_KitId_bloedafnamedatum %>% select(KitId, datum_bloedafname_pre),
    by = c("KitID" = "KitId")
  ) %>% 
  left_join(
    data_A10_vingerprik_org %>% 
      # filter op StudyIds die in de substudie zittem
      filter(#StudyId %in% data_InfectieNaVaccinatie$StudyId & 
               !is.na(DateReceived)) %>% 
      select(KitId, DateReceived) %>% 
      mutate(DateReceived = as_date(DateReceived)),
    by = c("KitID" = "KitId")
  ) %>% 
  rename(DateReceived_pre = DateReceived)

# Hier is het BioID opgegeven als 'SampleCode'
data_InfectieNaVaccinatie_serology_nazending <- data_InfectieNaVaccinatie_serology_nazending_org %>% 
  filter(!is.na(N)) %>% # select samples with measurement
  rename(BioID = SampleCode,
          `RBD-DELTA` = `RBD-delta`,
         `RBD-OMICRON` = `RBD-omicron`,
         `S1-OMICRON` = `S1-omicron`) %>% 
  left_join(koppeltabel_tube_JUL_ID %>% select(BioID, KitID = TubeID) %>% 
              mutate(BioID = as.character(BioID)),
            by = c("BioID")) %>% 
  left_join(data_VASCO_TubeID %>% select(StudyId, KitID)) %>% 
  # Only keep StudyIds of post-measurements (Delta post)
  #mutate(StudyId = if_else(reden == "post-meting Delta infecties", StudyId, NA_character_)) %>% 
  select(BioID, KitID, StudyId, N, S1, RBD, `RBD-DELTA`, 
         `RBD-OMICRON`, `S1-OMICRON`, reden, Coronatest_datum)
  

#(0) Correct remeasurement
# LET OP!!! File is in long format maar: 
# - als SampleCode een KitID bevat is het een post-meting
# - als SampleCode een BioID bevat is het een pre-meting
data_InfectieNaVaccinatie_serology <- data_InfectieNaVaccinatie_serology_org %>%
  mutate(across(
    .cols = c(`N temp AU tov ref-3`, `S1 temp AU tov ref-3`, `RBD temp AU tov ref-3`, 
    `RBD-DELTA`, `RBD-OMICRON`, `S1-OMICRON`, `N (BAU/ml), cut-off 14,3`,  
    `S1 (BAU/ml) cut-off 10,1`),
    ~ .x %>% 
      str_remove_all("\\*") %>% 
      str_replace("#NA", NA_character_) %>% 
      as.numeric())
    ) %>% 
  # Correct remeasurements
  left_join(
    data_InfectieNaVaccinatie_serology_hermetingen,
    by = c("MIA sample" = "Redo samples, weighted results") # JOIN ON KitID
  ) %>% 
  # take correct measurement for re-measured samples.
  mutate(
    `N` = if_else(
      condition = !is.na(`N`),         # remeasurement present?
      true = `N`,                      # yes: take remeasurement
      false = `N temp AU tov ref-3`),  # no: take old measurement
    `S1` = if_else(
      condition = !is.na(S1),
      true = S1,
      false = `S1 temp AU tov ref-3`),
    `RBD` = if_else(
      condition = !is.na(RBD),
      true = RBD,
      false = `RBD temp AU tov ref-3`),
    `RBD-DELTA` = if_else(
      condition = !is.na(RBD_delta),
      true = RBD_delta,
      false = `RBD-DELTA`),
    `RBD-OMICRON` = if_else(
      condition = !is.na(RBD_omicron),
      true = RBD_omicron,
      false = `RBD-OMICRON`),
    `S1-OMICRON` = if_else(
      condition = !is.na(S1_omicron),
      true = S1_omicron,
      false = `S1-OMICRON`),
    StudyId = StudyID) %>%
  bind_rows(
     data_InfectieNaVaccinatie_serology_nazending) %>%
  mutate(
    # BAU/ml conversion
    `S1 (BAU/ml)` = S1 * 
      data_InfectieNaVaccinatie_conversions %>% 
        filter(...1 %>% str_detect("S1")) %>% 
        pull(`totale conversiefactor naar BAU/ml voor INACOVA`),
    `N (BAU/ml)` = N * 
      data_InfectieNaVaccinatie_conversions %>% 
        filter(...1 %>% str_detect("N")) %>% 
        pull(`totale conversiefactor naar BAU/ml voor INACOVA`),
    `RBD (BAU/ml)` = RBD *
      data_InfectieNaVaccinatie_conversions %>% 
      filter(...1 %>% str_detect("RBD")) %>% 
      pull(`totale conversiefactor naar BAU/ml voor INACOVA`)
    ) %>% 
  select(
    StudyId, BioID, KitID, SampleCode, datum,Datum_bloedafname_NA, `bloedafname gewogen (Rob)`, 
    N, S1, RBD, `RBD-DELTA`, `RBD-OMICRON`,`S1-OMICRON`, `S1 (BAU/ml)`, 
    `N (BAU/ml)`,`RBD (BAU/ml)`, reden, Coronatest_datum
  ) %>% 
  # only select participants with MIA measurement
  filter(
    !is.na(`N (BAU/ml)`)
  )


# (1) post meting

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie_serology %>% 
  # Pak alleen de 450 post metingen (SampleCode start met JUL) + extra Delta infecties
  filter(startsWith(SampleCode, "JUL") | reden == "post-meting Delta infecties") %>%
  mutate(
    # Complete KitID with samplecode of post measurements
    KitID = if_else(is.na(KitID), SampleCode, KitID),
    datum = as_date(as.integer(datum), 
                    origin = "1899-12-30"),
    Datum_bloedafname_NA = as_date(Datum_bloedafname_NA)
  ) %>%
  select(!SampleCode) %>% 
  # Let op SGTF gebruiken we ook.
  left_join(data_InfectieNaVaccinatie_variant %>% select(KitID = SampleCode, 
                                                         Datum_pos_test, 
                                                         NCOVdat1eposncov,
                                                         DUFS_1eziektedag,
                                                         ZIE1eZiekteDt,
                                                         NCOVVast1eziektedag,
                                                         Clade, `S result`,
                                                         `Ct ORF1ab`, `Ct S`,
                                                         `Ct N`, PCR_Lab,
                                                         Variant),
            by = "KitID") %>% 
  mutate(V_Omicron_Delta = case_when(
    Variant %>% str_detect("Omicron") ~ "Omicron",
    Variant == "Delta" | 
      # Infecties tussen 1 okt. en 15 nov. 2021
      reden == "post-meting Delta infecties" ~ "Delta"
  ),
  Datum_pos_test = if_else(!is.na(Datum_pos_test), Datum_pos_test, as_date(Coronatest_datum)))

# 403 VASCO compleet matches met positieve testdatum
# nu 443 met M vragenlijst
data_VASCO_compleet_InfectieNaVaccinatie <- data_VASCO_compleet %>% 
  filter(StudyId %in% data_InfectieNaVaccinatie$StudyId) %>% 
  # Selecteer uit VASCO de coronatest datums
  select(StudyId, starts_with("Coronatest_datum"), starts_with("Coronatest_positief_datum"), 
         "Wanneer_test_afgenomen_B" = "Wanneer_test_afgenomen") %>%
  pivot_longer(cols =  !StudyId,
               names_to = "Vragenlijst_veld",
               values_to = "Coronatest_datum",
               names_prefix = "Coronatest_datum_|Coronatest_positief_datum_|Wanneer_test_afgenomen_") %>% 
  # join met de InfectieNaVaccinatie coronatest datums
  full_join(data_InfectieNaVaccinatie %>% select(Datum_pos_test, StudyId),
            by = "StudyId") %>% 
  # filter op gelijke datums om het juiste vragenlijst_veld te achterhalen
  filter(Coronatest_datum == Datum_pos_test | 
           (is.na(Datum_pos_test) & Coronatest_datum %in% seq(as_date("2021-11-30"), as_date("2022-02-28"), 1))) %>% 
  arrange(desc(Vragenlijst_veld)) %>% # T vragenlijst boven M.
  distinct(StudyId, Coronatest_datum, .keep_all = T) %>% 
  #select(StudyId, Vragenlijst_veld) %>% 
  # join de vragenlijst op het juiste vragenlijst_veld ID voor alle T vragenlijst velden
  left_join(
    data_VASCO_compleet %>% 
      select(StudyId, matches("T[1-6]$")) %>% 
      pivot_longer(cols = matches("T[1-6]$"), names_pattern = "(.*)_(T.)$",
                   names_to = c(".value", "Vragenlijst_veld")) %>% 
      select(!Coronatest_datum),
    by = c("StudyId", "Vragenlijst_veld")
  ) %>% 
  # join de vragenlijst op het juiste vragenlijst_veld ID voor alle M vragenlijst velden
  left_join(
    data_VASCO_compleet %>% 
      select(StudyId, matches("M[1-6]$")) %>% 
      pivot_longer(cols = matches("M[1-6]$"), names_pattern = "(.*)_(M.)$",
                   names_to = c(".value", "Vragenlijst_veld")) %>% 
      select(StudyId, Vragenlijst_veld, starts_with("Symptomen_corona"),
             starts_with("Coronatest_positief")),
    by = c("StudyId", "Vragenlijst_veld")
  )

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  select(!Coronatest_datum) %>% 
  left_join(data_VASCO_compleet_InfectieNaVaccinatie %>% 
              select(StudyId, Coronatest_datum, Coronatest_symptomen_corona, 
                     Coronatest_symptomen_corona_welke, Coronatest_symptomen_wanneer,
                     Symptomen_corona_afgelopen_maand, Symptomen_corona_afgelopen_maand_welke,
                     Symptomen_corona_afgelopen_maand_welke_anders, Symptomen_corona_afgelopen_maand_wanneer),
  by= "StudyId") %>% 
  mutate(
    DUFS_pos_test = if_else(!is.na(Coronatest_datum), Coronatest_datum, Datum_pos_test),
    DUFS_symptomen_wanneer = if_else(!is.na(Coronatest_symptomen_wanneer), Coronatest_symptomen_wanneer, ZIE1eZiekteDt)
  )


# (2) pre meting

# Koppelen KitIds aan bloedafnamedatums
koppeltabel_KitId_bloedafnamedatum <- data_A10_vingerprik_org %>% 
  # filter op StudyIds die in de substudie zittem
  filter(StudyId %in% data_InfectieNaVaccinatie$StudyId & !is.na(DateReceived)) %>% 
  select(StudyId, KitId, DateReceived) %>% 
  left_join(data_A8_vingerprik_org %>% 
              select(StudyId, Q02_Wat_is_de_datum_waarop_u_het_bloed_afgenomen_heeft_),
            by = c("StudyId")) %>% 
  mutate(DateReceived = as_date(DateReceived),
         datum_bloedafname_pre = as_date(Q02_Wat_is_de_datum_waarop_u_het_bloed_afgenomen_heeft_),
         # DR: DateReceived, BN: Bloedname
         interval_DR_BN = abs(DateReceived - datum_bloedafname_pre)) %>% 
  filter(datum_bloedafname_pre > DateReceived - 14 & datum_bloedafname_pre < DateReceived + 14) %>% 
  select(StudyId, KitId, datum_bloedafname_pre, DateReceived, interval_DR_BN) %>% 
  distinct(StudyId, KitId, datum_bloedafname_pre, .keep_all = T) %>% 
  arrange(KitId, interval_DR_BN) %>% 
  group_by(KitId) %>% 
  slice(1) %>% 
  ungroup

data_InfectieNaVaccinatie_pre_serology <- data_InfectieNaVaccinatie_serology %>% 
  # Take only the pre measurements
  filter((startsWith(SampleCode, "406") | !BioID %in% data_InfectieNaVaccinatie$BioID) &
           if_all(
            .cols = c(N, S1, RBD, `RBD-DELTA`, `RBD-OMICRON`, `S1-OMICRON`),
            ~!is.na(.x)) # select rows with measurements
  ) %>% 
  # Hernoem om pre en post uit elkaar te kunnen halen, N, S1, RBD neem ik niet meer mee
  select(SampleCode, BioID, pre_RBD_DELTA = `RBD-DELTA`, pre_RBD_OMICRON = `RBD-OMICRON`, pre_S1_OMICRON = `S1-OMICRON`,
         pre_N_BAU_ml = `N (BAU/ml)`, pre_S1_BAU_ml = `S1 (BAU/ml)`, pre_RBD_BAU_ml = `RBD (BAU/ml)`) %>% 
  mutate(BioID_pre  = as.double(if_else(startsWith(SampleCode, "406") & !is.na(SampleCode), SampleCode, BioID))) %>%
  select(!BioID) %>% 
  left_join(koppeltabel_tube_JUL_ID %>% select(BioID, KitID_pre = TubeID),
            by = c("BioID_pre" = "BioID")) %>% 
  left_join(data_VASCO_TubeID, by = c("KitID_pre" = "KitID")) %>% 
  left_join(data_InfectieNaVaccinatie %>% select(StudyId, DUFS_pos_test),
            by = "StudyId") %>% 
  arrange(StudyId, desc(Meetmoment)) %>% 
  mutate(
    DUFS_bloedafname_pre = if_else(!is.na(datum_bloedafname_pre), datum_bloedafname_pre, DateReceived_pre)
  ) %>% 
  # Two individuals have their pre sample at or 1 day after positive test date.
  mutate(interval_pos_test_bloedname_pre = DUFS_bloedafname_pre - DUFS_pos_test) %>% 
  filter(interval_pos_test_bloedname_pre < 0) %>% 
  add_count(StudyId, name = "pre_n_samples") %>% 
  select(!c(DUFS_pos_test, interval_pos_test_bloedname_pre))
  
# link with STAR measurements

data_Ab_STAR <- data_Ab_STAR_org %>% 
  select(StudyId, KitID, Meetmoment, DateReceived_STAR = DateReceived, Resultaat_COVS,resultaatRef_COVNB, Resultaat_COVNB, seroS, seroN) %>% 
  filter(StudyId %in% data_InfectieNaVaccinatie$StudyId ) %>% 
  # Voor nu even dit overslaan
  # left_join(data_A8_vingerprik, 
  #           by = c("StudyId", "Meetmoment"))
  left_join(data_InfectieNaVaccinatie %>% select(StudyId, DUFS_pos_test),
            by = "StudyId") %>% 
  mutate(interval_received_pos_test = as.numeric(DUFS_pos_test - DateReceived_STAR),
         in_star_file = T) %>% 
  filter(interval_received_pos_test >= 30) %>% 
  arrange(is.na(resultaatRef_COVNB), interval_received_pos_test) %>%
  group_by(StudyId) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(!DUFS_pos_test)

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  left_join(
    data_InfectieNaVaccinatie_pre_serology %>% 
      group_by(StudyId) %>% 
      slice(1) %>% 
      ungroup,
    by = "StudyId"
  ) %>% 
  mutate(
    pre_n_samples = pre_n_samples %>% replace_na(0)
  )

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  left_join(data_Ab_STAR %>% select(StudyId, seroN, DateReceived_STAR),
            by = "StudyId") %>% 
  left_join(data_VASCO_TubeID %>% select(KitID, DateReceived_post = DateReceived_pre, datum_bloedafname_post = datum_bloedafname_pre), # Pre changed to post as we now link to post sample ID
            by = "KitID") %>% #LET OP VERANDER DEZE NAAM NAAR KIT ID
  mutate(DateReceived_post = if_else(is.na(DateReceived_post), datum, DateReceived_post),
         DateReceived_pre = if_else(is.na(DateReceived_pre), DateReceived_STAR, DateReceived_pre),
         interval_DateReceived_samples = as.numeric(DateReceived_post - DateReceived_pre)) %>% 
# On average 2 days between bloodsample date and date received 
#data_InfectieNaVaccinatie %>% mutate(int = datum_bloedafname_post - DateReceived_post) %>% summarise(mean(int, na.rm = T), median(int, na.rm = T))
  mutate(DUFS_bloedafname =
           case_when(
             !is.na(datum_bloedafname_post) ~ datum_bloedafname_post, # Date of serum collection, n = 389
             !is.na(DateReceived_post) ~ DateReceived_post - 2,       # Date lab received serum, n = 129
             TRUE ~ as_date(`bloedafname gewogen (Rob)`)),            # imputed date by lab, n = 2
         interval_bloedafname_PCR = as.integer(DUFS_bloedafname - DUFS_pos_test),
         interval_bloedafname_pre_PCR = abs(as.integer(DUFS_bloedafname_pre - DUFS_pos_test)),
         interval_bloedafname_EZD = as.integer(DUFS_bloedafname - DUFS_1eziektedag)  
  )


# 
cohorts_small <-
  c(seq(1,5*7 + 1, 7), data_InfectieNaVaccinatie$interval_bloedafname_PCR %>% max(na.rm = T) + 1)
n_cohorts = cohorts_small %>% length()

cohorts_2wks <-
  c(1, 15, 29, data_InfectieNaVaccinatie$interval_bloedafname_PCR %>% max(na.rm = T) + 1)
n_cohorts_2wks = cohorts_2wks %>% length()

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>%
  mutate(
    pre_N_status = case_when(
      pre_N_BAU_ml >= 14.3 ~ 1,
      pre_N_BAU_ml < 14.3 ~ 0,
      seroN == "ja" ~ 1,
      seroN == "nee" ~ 0
    ),
    pre_S1_status = if_else(pre_S1_BAU_ml > 10.1, 1, 0),
    
    post_N_status = if_else(`N (BAU/ml)` > 14.3, 1, 0) %>% as.factor(),
    post_S1_status = if_else(`S1 (BAU/ml)` > 10.1, 1, 0) %>% as.factor(),
    
    cohorts_bloedname_PCR = interval_bloedafname_PCR %>% cut(
      breaks = cohorts_small,
      right = FALSE,
      include.lowest = TRUE,
      labels = str_c(cohorts_small[1:n_cohorts - 1]  , "-", cohorts_small[2:n_cohorts] - 1)
    ) %>% factor(levels = str_c(cohorts_small[1:n_cohorts - 1]  , "-", cohorts_small[2:n_cohorts] - 1)),
    
    cohorts_bloedname_PCR_2wks = interval_bloedafname_PCR %>% cut(
      breaks = cohorts_2wks,
      right = FALSE,
      include.lowest = TRUE,
      labels = str_c(cohorts_2wks[1:n_cohorts_2wks - 1]  , "-", cohorts_2wks[2:n_cohorts_2wks] - 1)
    ) %>% factor(levels = str_c(cohorts_2wks[1:n_cohorts_2wks - 1]  , "-", cohorts_2wks[2:n_cohorts_2wks] - 1)),
    
    
    pre_N_status_txt = if_else(pre_N_status == 1, "pre N +", "pre N -", missing = NA_character_),
    pre_S1_status_txt = if_else(pre_S1_status == 1, "pre S1 +", "pre S1 -", missing = NA_character_)
  )

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>%
  select(-any_of(
    c(
      "Geboortedatum",
      "Geslacht",
      "Besmetting_coronavirus",
      "Wanneer_test_afgenomen"
    )
  )) %>%
  # add some meta data
  left_join(
    data_VASCO_compleet %>% select(
      StudyId,
      Geboortedatum,
      Geslacht,
      Besmetting_coronavirus,
      Wanneer_test_afgenomen
    ),
    by = "StudyId"
  ) %>%
  mutate(
    eerder_positief = if_else(
      Besmetting_coronavirus == "Ja, dit is bevestigd met een test" &
        Wanneer_test_afgenomen < as_date("2021-10-01"),
      true = "Yes",
      false = "No"
    ),
    # Van iedereen willen een pre-sample voor de infectie
    previous_infection = case_when(
      (eerder_positief == "Yes" &
         !is.na(pre_N_status)) | pre_N_status == 1 ~
        "Evidence of\nprevious infection",
      pre_N_status == 0 ~ "No evidence of\nprevious infection",
      TRUE ~ NA_character_
    )
  )

# Calculate Age
data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  mutate(
    Leeftijd = as.numeric((DUFS_bloedafname - Geboortedatum) / 365.25),
    Leeftijds_groep = Leeftijd %>% cut(
      breaks = c(18, 30, 45, 60, 75, Inf),
      right = FALSE,
      include.lowest = TRUE,
      labels = c(
        "18-29",
        "30 - 44",
        "45 - 59",
        "60 - 74",
        "75+"
      )
    ) %>% as.character() %>% 
      replace_na("UNK") %>%  
      factor(levels = c("18-29", "30 - 44", "45 - 59", "60 - 74", "75+", "UNK")),
  )


# Consent koppeling GGD en IC
data_InfectieNaVaccinatie_IC <- data_InfectieNaVaccinatie_IC %>% 
  # Accepted_Accepted geeft aan of de IC succesvol was.
  # Bij False staat er in ICCategory_ICCategory het gene dat ontbreekt (de datum)
  # Vervolgens krijgen deelnemers een nieuwe oproep om dit correct in te vullen, daarom wil ik de max van Accepted_Accepted.
  group_by(StudyId) %>% 
  mutate(ICQ1_IC_Q1 = max(ICQ1_IC_Q1, na.rm = T),
        Accepted_Accepted = max(Accepted_Accepted, na.rm = T)) %>% 
  ungroup %>% 
  distinct(StudyId, .keep_all = T)

data_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  left_join(data_InfectieNaVaccinatie_IC %>% select(StudyId, ICQ1_IC_Q1, Accepted_Accepted),
                                        by = "StudyId") %>% 
  filter(Accepted_Accepted == T | reden == "post-meting Delta infecties") %>% 
  mutate(Variant = if_else(ICQ1_IC_Q1 == "Ja" | reden == "post-meting Delta infecties", Variant, NA_character_),
         V_Omicron_Delta = if_else(ICQ1_IC_Q1 == "Ja" | reden == "post-meting Delta infecties", V_Omicron_Delta, NA_character_))

data_InfectieNaVaccinatie_pre_serology <- data_InfectieNaVaccinatie_pre_serology %>% 
  filter(StudyId %in% data_InfectieNaVaccinatie$StudyId)
