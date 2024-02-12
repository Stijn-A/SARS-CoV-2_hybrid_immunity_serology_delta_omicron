# Variant
model_variant <- 
  data_InfectieNaVaccinatie %>% 
  filter(!is.na(V_Omicron_Delta)) %>% 
  mutate(V_Omicron_Delta = factor(V_Omicron_Delta)) %>% 
  #filter(previous_infection == "No evidence of\nprevious infection") %>% 
  gam(
    formula = post_N_status ~ 
      s(interval_bloedafname_PCR, bs = "ps", k = 25) +
      V_Omicron_Delta +
      previous_infection, #+ 
    
    #s(interval_bloedafname_PCR, by = pre_N_status_txt) +
    #pre_N_status_txt
    #,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)



summary(model_variant)
N.preddata <- expand.grid(
  interval_bloedafname_PCR = seq(5,60,1),
  V_Omicron_Delta = c("Omicron", 
                         "Delta"),
  previous_infection = c("No evidence of\nprevious infection", 
                         "Evidence of\nprevious infection")
)

# Make predictions for each combination using the GAM model
# Do this in a temporary tibble, because two columns are produced: fit and se.fit
tmp <- predict(
  object = model_variant,
  newdata = N.preddata,
  type = "link",
  se.fit = TRUE) %>%
  as_tibble

# Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
N.preddata <- bind_cols(N.preddata, tmp) %>%
  mutate(
    fit_lwr = fit + qnorm(0.025)*se.fit,   
    fit_upr = fit + qnorm(0.975)*se.fit,
    p_inf = fit %>% plogis,                #create vector of probabilities from log odds
    p_inf_lwr = fit_lwr %>% plogis,
    p_inf_upr = fit_upr %>% plogis)

figuur_LR_variant <- ggplot(
  data = N.preddata %>% 
    filter(previous_infection == "No evidence of\nprevious infection"),
  mapping = aes(
    x = interval_bloedafname_PCR, y = p_inf, ymin = p_inf_lwr, ymax = p_inf_upr)) +
  geom_ribbon(
    alpha = 0.25) +
  geom_line(size = 0.8) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), expand = expansion(add = 0)) +
  scale_x_continuous(breaks = seq(0,60,10), limits = c(5,60), expand = expansion(add = 0)) +
  ylab("Probability of N seroconversion") +
  xlab(" ") +
  facet_grid(~V_Omicron_Delta) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = "grey65", size = 0.2),
        strip.background = element_rect(
          color="black", fill="white", linetype = "blank"),
        text = element_text(size=20)
  )



# Vaccin status
model_vaccination_status <- 
  data_InfectieNaVaccinatie %>% 
  
 # filter(previous_infection == "No evidence of\nprevious infection") %>% 
  gam(
    formula = post_N_status ~ 
      s(interval_bloedafname_PCR, bs = "ps", k = 25) +
      s(vacc_status, bs = "re") +
      previous_infection, #+ 
    
    #s(interval_bloedafname_PCR, by = pre_N_status_txt) +
    #pre_N_status_txt
    #,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_vaccination_status)




# Vaccin type
model_vaccin <- 
  data_InfectieNaVaccinatie %>% 
  #filter(previous_infection == "No evidence of\nprevious infection") %>% 
  filter(!is.na(vacc_merk_laatste)) %>% 
  mutate(vacc_merk_laatste = factor(vacc_merk_laatste)) %>% 
  gam(
    formula = post_N_status ~ 
      s(interval_bloedafname_PCR, bs = "ps", k = 25) +
      s(vacc_merk_laatste, bs = "re") +
      previous_infection, #+ 
    
    #s(interval_bloedafname_PCR, by = pre_N_status_txt) +
    #pre_N_status_txt
    #,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_vaccin)

# Vaccin time
model_vaccin_time <- 
  data_InfectieNaVaccinatie %>% 
  #filter(previous_infection == "No evidence of\nprevious infection") %>% 
  #filter(!is.na(vacc_merk_laatste)) %>% 
  mutate(vacc_merk_laatste = factor(vacc_merk_laatste)) %>% 
  gam(
    formula = post_N_status ~ 
      s(interval_bloedafname_PCR, bs = "ps", k = 12) +
      s(vacc_time, bs = "ps", k = 12) + #+ 
    previous_infection,
    #s(interval_bloedafname_PCR, by = pre_N_status_txt) +
    #pre_N_status_txt
    #,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_vaccin_time)
