# importeren
data_InfectieNaVaccinatie_variant <- read_rds("/PATH/data_InfectieNaVaccinatie_VOC_20220623.rds")

data_InfectieNaVaccinatie_serology_org <- read_xlsx("/PATH/InfectieNaVaccinatie interim MIA overzicht 29-6-2022.xlsx",
                                                    sheet = 1) #%>% 

data_InfectieNaVaccinatie_serology_nazending_org <- read_xlsx("/PATH/INACOVA MIA pre-sera - nazending oktober 2022.xlsx",
                                                    sheet = 1) %>%
  # Link type of measurement: post-, pre-Delta or missing pre samples)
  left_join(read_xlsx("/PATH/overzicht_extra_Delta_infectie_deelnemers_20220926_1628.xlsx",
                      sheet = 1) %>% select(BioID, Coronatest_datum) %>% mutate(reden = "post-meting Delta infecties",
                                                              BioID = as.character(BioID)),
            by = c("SampleCode" = "BioID"))

data_InfectieNaVaccinatie_conversions <- read_xlsx("/PATH/InfectieNaVaccinatie interim MIA overzicht 29-6-2022.xlsx",
                                                    sheet = 2)

# Informed consent
data_InfectieNaVaccinatie_IC <- read_xlsx("/PATH/A23_TOESTEMMING_Export_WideFormat_20220829.xlsx") %>% 
  filter(StudyId %in% data_InfectieNaVaccinatie_serology_org$StudyID & !is.na(StudyId))
#21-07-2022 Nu filteren we dit in opschonen
  # Pak alleen de 450 personen
  #filter(startsWith(StudyID, "VS"))


data_InfectieNaVaccinatie_serology_hermetingen <- read_xlsx("PATH/INACOVA REDO selectie final results 21-7-2022.xlsx",
                                                    sheet = 1)

data_VASCO_compleet <- read_delim("PATH/20220816_VASCO_compleet.csv",
                                  #20220620_VASCO_compleet.csv", 
                                       delim = ",", escape_double = FALSE, trim_ws = TRUE) 

data_Ab_STAR_org <- read_csv(
  "PATH/Dataset VASCO serologie_20220921.csv")


# 1 maand na infectie vragenlijst
data_1M_infectie_org <- read_xlsx(
"PATH/A7_Vragenlijst 1 maand na een positieve coronatest_Export_WideFormat_20220815.xlsx")

koppeltabel_tube_JUL_ID <- read_delim("PATH/Biobank (KitID - buisnummers)/02062022_Biobank Julius VASCO.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

data_A8_vingerprik_org <- read_xlsx(
  "PATH/A8_Melding vingerprik_Export_WideFormat_20220815.xlsx") %>%
  select(StudyId, Meetmoment, Q02_Wat_is_de_datum_waarop_u_het_bloed_afgenomen_heeft_)

data_A10_vingerprik_org <- read_xlsx(
  "PATH/A10_Vingerprik_Export_WideFormat_.xlsx")
 

data_CIMS_vasco <- read_csv("PATH/20220829_VASCO_CIMS_final.csv")
data_vaccinatie_vasco <- read_csv("PATH/20220825_VASCO_vacc_final.csv")
