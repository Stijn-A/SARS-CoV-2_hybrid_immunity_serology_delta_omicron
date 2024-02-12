model_N_vacc <- 
  data_InfectieNaVaccinatie %>% 
  mutate(vacc_status = vacc_status %>% factor(levels = c("full", "part","booster"))) %>% 
  gam(
    formula = post_N_status ~ 
      ti(interval_bloedafname_PCR, bs = "ps", k = 25) + 
      vacc_status +
      pre_N_status,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_N_vacc)



model_N_vacc <- 
  data_InfectieNaVaccinatie %>% 
  mutate(
    vacc_time = as.numeric(vacc_time)
  ) %>% 
  gam(
    formula = post_N_status ~ 
      ti(interval_bloedafname_PCR, bs = "ps", k = 25) + 
      ti(vacc_time, bs = "ps", k = 25) + 
      pre_N_status,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_N_vacc)
