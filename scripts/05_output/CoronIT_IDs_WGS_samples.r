data_InfectieNaVaccinatie %>% 
  # do not output Monsternummer of participants without consent for variant linkage
  filter(!is.na(Clade), !is.na(Variant)) %>% 
  select(Monsternummer) %>% 
  write_xlsx(
    "CoronIT_IDs_20230622.xlsx"
  )
