
model_N_symptoms <- 
  data_InfectieNaVaccinatie %>% 
  filter(!is.na(Coronatest_symptomen_corona_c), !is.na(previous_infection)) %>% 
  mutate(Coronatest_symptomen_corona_c  = factor(Coronatest_symptomen_corona_c)) %>% 
  gam(
    formula = post_N_status ~ 
      s(interval_bloedafname_PCR, bs = "ps", k = 15) + 
      previous_infection +
      Coronatest_symptomen_corona_c,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_N_symptoms)

#  Similar classification as in: https://academic.oup.com/cid/article/73/12/2155/6149064
model_N_mild_severe <- 
  data_InfectieNaVaccinatie %>% 
  filter(!mild_severe == "UNK") %>% 
  gam(
    formula = post_N_status ~ 
      s(interval_bloedafname_PCR, bs = "ps", k = 15) + 
      previous_infection +
      mild_severe,
    family = binomial,
    method = "REML",  #restricted maximum likelihood 
    data = .)

summary(model_N_mild_severe)

# Generate some fake data for the model prediction
N.symptoms.preddata <- expand.grid(
  interval_bloedafname_PCR = seq(5,60,1),
  previous_infection = c("No evidence of\nprevious infection", 
                         "Evidence of\nprevious infection"),
  mild_severe = c("Symptomatic", "Asymptomatic and mild symptoms")
)

# Make predictions for each combination using the GAM model
# Do this in a temporary tibble, because two columns are produced: fit and se.fit
tmp <- predict(
  object = model_N_mild_severe,
  newdata = N.symptoms.preddata,
  type = "link",
  se.fit = TRUE) %>%
  as_tibble

# Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
N.symptoms.preddata <- bind_cols(N.symptoms.preddata, tmp) %>%
  mutate(
    fit_lwr = fit + qnorm(0.025)*se.fit,   
    fit_upr = fit + qnorm(0.975)*se.fit,
    p_inf = fit %>% plogis,                #create vector of probabilities from log odds
    p_inf_lwr = fit_lwr %>% plogis,
    p_inf_upr = fit_upr %>% plogis)# %>% 
#  filter(previous_infection == "No evidence of\nprevious infection")

figuur_LR_N_symptoms <- ggplot(
  data = N.symptoms.preddata,
  mapping = aes(
    x = interval_bloedafname_PCR, y = p_inf, 
    color = mild_severe, fill = mild_severe)) +
  geom_ribbon(
    aes(ymin = p_inf_lwr, ymax = p_inf_upr),
    alpha = 0.25) +
  geom_line(size = 0.8) +
  ylab("Probability of N seroconversion") +
  xlab(" ") +
  scale_fill_brewer(type = "qual") +
  scale_color_brewer(type = "qual") +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), expand = expansion(add = 0)) +
  scale_x_continuous(breaks = seq(0,60,10), limits = c(5,60), expand = expansion(add = 0)) +
  facet_grid( ~previous_infection) + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey65", 
                                    size = 0.2),
    strip.background = element_rect(
      color="black", fill="white", linetype = "blank"),
    strip.text = element_text(size=8),
    legend.spacing.x = unit(0.5, 'cm')
  )

# model_N_symptoms_nr <- 
#   data_InfectieNaVaccinatie %>% 
#   gam(
#     formula = post_N_status ~ 
#       s(interval_bloedafname_PCR, bs = "ps", k = 25) + 
#       pre_N_status_txt +
#       Coronatest_symptomen_aantal,
#     family = binomial,
#     method = "REML",  #restricted maximum likelihood 
#     data = .)
# 
# summary(model_N_symptoms_nr)


# 
# model_N_symptoms_all <- 
#   data_InfectieNaVaccinatie %>% 
#   # use select function and random effect
#   gam(
#     formula = post_N_status ~ 
#       ti(interval_bloedafname_PCR, bs = "ps", k = 25) + 
#       pre_N_status_txt +
#       Klacht_Koorts + 
#       Klacht_Malaise + 
#       Klacht_Hoesten + 
#       Klacht_Keelpijn + 
#       Klacht_Loopneus + 
#       Klacht_Kortademigheid + 
#       Klacht_Diarree + 
#       Klacht_Misselijkheid + 
#       Klacht_Hoofdpijn + 
#       Klacht_Prikkelbaarheid_verwarring + 
#       Klacht_Spierpijn + 
#       Klacht_Pijn_ademhaling + 
#       Klacht_Buikpijn + 
#       Klacht_Gewrichtspijn + 
#       Klacht_Verlies_Geur_Smaak + 
#       Klacht_Extreme_vermoeidheid + 
#       Klacht_Pijn_ogen + 
#       Klacht_Andere_klachten,
#     family = binomial,
#     method = "REML",  #restricted maximum likelihood 
#     select = T,
#     data = .)
# summary(model_N_symptoms_all)
# 
# model_N_symptoms <- 
#   # use select function and random effect
#   gam(
#     formula = 
#       post_N_status ~ 
#         s(interval_bloedafname_PCR, bs = "ps", k = 15) + 
#         previous_infection + 
#         s(Klacht_Koorts, bs = "re") +
#         s(Klacht_Malaise, bs = "re") +
#         s(Klacht_Hoesten, bs = "re") +
#         s(Klacht_Keelpijn, bs = "re") +
#         s(Klacht_Loopneus, bs = "re") +
#         s(Klacht_Kortademigheid, bs = "re") +
#         s(Klacht_Diarree, bs = "re") +
#         s(Klacht_Misselijkheid, bs = "re") +
#         s(Klacht_Hoofdpijn, bs = "re") +
#         s(Klacht_Prikkelbaarheid_verwarring, bs = "re") +
#         s(Klacht_Spierpijn, bs = "re") +
#         s(Klacht_Pijn_ademhaling, bs = "re") +
#         s(Klacht_Buikpijn, bs = "re") +
#         s(Klacht_Gewrichtspijn, bs = "re") +
#         s(Klacht_Verlies_Geur_Smaak, bs = "re") +
#         s(Klacht_Extreme_vermoeidheid, bs = "re") #+
#         #s(Klacht_Pijn_ogen, bs = "re") +
#         #s(Klacht_Andere_klachten, bs = "re")
#     ,
#       family = binomial,
#       method = "REML",  #restricted maximum likelihood 
#       #select = T,
#     data = data_InfectieNaVaccinatie %>% filter(!is.na(Coronatest_symptomen_corona_c) & !is.na(post_N_status)) #%>% filter(previous_infection ==) 
#     )
# #https://stats.stackexchange.com/questions/546609/low-edf-in-gam-generalized-additive-model
# model_summary <- summary(model_N_symptoms)
# model_summary
# model_summary$s.table
# table_klacht <- 
#   tibble::rownames_to_column(
#   model_summary$s.table %>% as.data.frame(), 
#   "Klacht") %>% 
#   filter(str_detect(Klacht,"Klacht")) %>% 
#   mutate(Klacht = Klacht %>% str_remove(pattern = "s\\(Klacht_") %>% str_remove("\\)")) %>% 
#   select(Klacht,`p-value`)
# 
# 
# 
# N.preddata <- expand.grid(
#   interval_bloedafname_PCR = seq(5,60,5),
#   Klacht_Koorts = c(0,1),
#   Klacht_Malaise  = c(0), 
#   Klacht_Hoesten  = c(0), 
#   Klacht_Keelpijn  = c(0), 
#   Klacht_Loopneus  = c(0), 
#   Klacht_Kortademigheid  = c(0,1), 
#   Klacht_Diarree  = c(0), 
#   Klacht_Misselijkheid  = c(0), 
#   Klacht_Hoofdpijn  = c(0), 
#   Klacht_Prikkelbaarheid_verwarring  = c(0), 
#   Klacht_Spierpijn  = c(0,1), 
#   Klacht_Pijn_ademhaling  = c(0), 
#   Klacht_Buikpijn  = c(0), 
#   Klacht_Gewrichtspijn  = c(0), 
#   Klacht_Verlies_Geur_Smaak  = c(0), 
#   Klacht_Extreme_vermoeidheid  = c(0)#, 
#   #Klacht_Andere_klachten = c(0)
#   #Coronatest_symptomen_corona_c = c("Symptomatic", "Asymptomatic")
# )
# 
# # Make predictions for each combination using the GAM model
# # Do this in a temporary tibble, because two columns are produced: fit and se.fit
# tmp <- predict(
#   object = model_N_symptoms,
#   newdata = N.preddata,
#   type = "link",
#   se.fit = TRUE) %>%
#   as_tibble
# 
# # Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
# N.preddata <- bind_cols(N.preddata, tmp) %>%
#   mutate(
#     fit_lwr = fit + qnorm(0.025)*se.fit,   
#     fit_upr = fit + qnorm(0.975)*se.fit,
#     p_inf = fit %>% plogis,                #create vector of probabilities from log odds
#     p_inf_lwr = fit_lwr %>% plogis,
#     p_inf_upr = fit_upr %>% plogis) %>% 
#   pivot_longer(cols = starts_with("Klacht"), names_to = "Klacht") %>% 
#   mutate(value = factor(value))
# 
# # Losse predicties maken onder de aanname dat alles 0 is behalve de 1e klacht?
# # voor elke klacht
# 
# figuur_LR_N_symptoms <- ggplot(
#   data = N.preddata,
#   mapping = aes(
#     x = interval_bloedafname_PCR, y = p_inf, ymin = p_inf_lwr, ymax = p_inf_upr,
#     color = value, fill = value)) +
#   geom_ribbon(
#     alpha = 0.25) +
#   geom_line() +
#   ylab("Probability of prior N seroconversion") +
#   xlab("interval sample -- testing date") +
#   scale_fill_brewer(type = "qual") +
#   scale_color_brewer(type = "qual") +
#   facet_wrap(~Klacht) + 
#   theme_bw() + 
#   theme(
#     legend.title = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(colour = "grey65", 
#                                     size = 0.2),
#     strip.background = element_rect(
#       color="black", fill="white", linetype = "blank"),
#     strip.text = element_text(size=8)
#   )
# 
# #N.preddata %>% pivot_longer(cols = starts_with("Klacht"), names_to = "Klacht")
# 
# model_N_symptoms_select <- 
#   # use select function and random effect
#   gam(
#     formula = 
#       post_N_status ~ 
#       s(interval_bloedafname_PCR, bs = "ps", k = 5) + 
#       s(pre_N_status, bs = "re") +
#       s(Klacht_Koorts, bs = "re") +
#       #s(Klacht_Malaise, bs = "re") +
#       #s(Klacht_Hoesten, bs = "re") +
#       s(Klacht_Keelpijn, bs = "re") +
#       #s(Klacht_Loopneus, bs = "re") +
#       s(Klacht_Kortademigheid, bs = "re")
#       #s(Klacht_Diarree, bs = "re") +
#       #s(Klacht_Misselijkheid, bs = "re") +
#       #s(Klacht_Hoofdpijn, bs = "re") +
#       #s(Klacht_Prikkelbaarheid_verwarring, bs = "re") +
#       #s(Klacht_Spierpijn, bs = "re") +
#       #s(Klacht_Pijn_ademhaling, bs = "re") +
#       #s(Klacht_Buikpijn, bs = "re") +
#       #s(Klacht_Gewrichtspijn, bs = "re") +
#       #s(Klacht_Verlies_Geur_Smaak, bs = "re") +
#       #s(Klacht_Extreme_vermoeidheid, bs = "re") +
#       #s(Klacht_Pijn_ogen, bs = "re") +
#       #s(Klacht_Andere_klachten, bs = "re")
#     ,
#     family = binomial,
#     method = "REML",  #restricted maximum likelihood 
#     select = T,
#     data = data_InfectieNaVaccinatie %>% filter(!is.na(Klacht_Koorts) & !is.na(post_N_status))
#   )
# 
# summary(model_N_symptoms_select)
# 
# 
# # Generate some fake data for the model prediction
# N.preddata <- expand.grid(
#   interval_bloedafname_PCR = seq(5,60,1),
#   pre_N_status = c(0,1),
#   Klacht_Koorts = c(0,1)
# )
# 
# # Make predictions for each combination using the GAM model
# # Do this in a temporary tibble, because two columns are produced: fit and se.fit
# tmp <- predict(
#   object = model_N_symptoms,
#   newdata = N.preddata,
#   type = "link",
#   se.fit = TRUE) %>%
#   as_tibble
# 
# # Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
# N.preddata <- bind_cols(N.preddata, tmp) %>%
#   mutate(
#     fit_lwr = fit + qnorm(0.025)*se.fit,   
#     fit_upr = fit + qnorm(0.975)*se.fit,
#     p_inf = fit %>% plogis,                #create vector of probabilities from log odds
#     p_inf_lwr = fit_lwr %>% plogis,
#     p_inf_upr = fit_upr %>% plogis)
# 
# figuur_LR_N_symptoms <- ggplot(
#   data = N.preddata,
#   mapping = aes(
#     x = interval_bloedafname_PCR, y = p_inf, ymin = p_inf_lwr, ymax = p_inf_upr,
#     color = Coronatest_symptomen_corona_c, fill = Coronatest_symptomen_corona_c)) +
#   geom_ribbon(
#     alpha = 0.25) +
#   geom_line() +
#   ylab("Probability of prior N seroconversion") +
#   xlab("interval sample -- testing date") +
#   scale_fill_brewer(type = "qual") +
#   scale_color_brewer(type = "qual") +
#   facet_grid(
#     ~pre_N_status_txt) + 
#   theme_bw() + 
#   theme(
#     legend.title = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(colour = "grey65", 
#                                     size = 0.2),
#     strip.background = element_rect(
#       color="black", fill="white", linetype = "blank"),
#     strip.text = element_text(size=8)
#   )
# 
# 
# 
# 
# # vind de NA
# data_InfectieNaVaccinatie %>% filter(!is.na(Klacht_Koorts) & !is.na(post_N_status)) %>% 
#   count(is.na(interval_bloedafname_PCR), is.na(post_N_status), Klacht_Koorts , 
#           Klacht_Malaise , 
#           Klacht_Hoesten , 
#           Klacht_Keelpijn , 
#           Klacht_Loopneus , 
#           Klacht_Kortademigheid , 
#           Klacht_Diarree , 
#           Klacht_Misselijkheid , 
#           Klacht_Hoofdpijn , 
#           Klacht_Prikkelbaarheid_verwarring , 
#           Klacht_Spierpijn , 
#           Klacht_Pijn_ademhaling , 
#           Klacht_Buikpijn , 
#           Klacht_Gewrichtspijn , 
#           Klacht_Verlies_Geur_Smaak , 
#           Klacht_Extreme_vermoeidheid , 
#           Klacht_Pijn_ogen , 
#           Klacht_Andere_klachten) %>% view
# 
# s(bs = "re")
# 
# 
