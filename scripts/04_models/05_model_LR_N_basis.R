
model_N <- 
  data_InfectieNaVaccinatie %>% 
  mutate(pre_N_status_txt = factor(pre_N_status_txt)) %>% 
  filter(!is.na(previous_infection)) %>% 
  gam(
    formula = post_N_status ~ 
      s(interval_bloedafname_PCR, bs = "ps", k = 15) + 
      previous_infection,
      #,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_N)

# Generate some fake data for the model prediction
N.preddata <- expand.grid(
  interval_bloedafname_PCR = seq(5,60,1),
  previous_infection = c("No evidence of\nprevious infection", 
                         "Evidence of\nprevious infection")
)

# Make predictions for each combination using the GAM model
# Do this in a temporary tibble, because two columns are produced: fit and se.fit
tmp <- predict(
  object = model_N,
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

figuur_LR_N <- ggplot(
  data = N.preddata,
  mapping = aes(
    x = interval_bloedafname_PCR, y = p_inf, ymin = p_inf_lwr, ymax = p_inf_upr)) +
  geom_ribbon(
    alpha = 0.25) +
  geom_line(size = 0.8) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), expand = expansion(add = 0)) +
  scale_x_continuous(breaks = seq(0,60,10), limits = c(5,60), expand = expansion(add = 0)) +
  ylab("Probability of N seroconversion") +
  xlab(" ") +
  facet_grid(~previous_infection) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = "grey65", size = 0.2),
        strip.background = element_rect(
          color="black", fill="white", linetype = "blank"),
        strip.text = element_text(size=8)
  )





