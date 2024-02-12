# all analysis without prior N positives

tabel_tijd_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  select(interval_bloedafname_PCR, interval_bloedafname_EZD, 
         `N (BAU/ml)`, `S1 (BAU/ml)`, `RBD (BAU/ml)`, `S1-OMICRON`,`RBD-OMICRON`, 
         `RBD-DELTA`, Variant, pre_N_BAU_ml, pre_S1_BAU_ml, pre_S1_BAU_ml, 
         pre_N_status, pre_N_status_txt, pre_S1_status, post_N_status,  
         vacc_time, vacc_dose, V_Omicron_Delta, previous_infection) %>% 
  pivot_longer(
    cols = c(`N (BAU/ml)`, `S1 (BAU/ml)`, `RBD (BAU/ml)`, `S1-OMICRON`, `RBD-OMICRON`, `RBD-DELTA`),
    names_to = "Ag",
    values_to = "(B)AU/mL"
  ) %>% 
  mutate(
    `Log_BAU_mL` = log10(`(B)AU/mL`),
    interval_bloedafname_PCR = as.integer(interval_bloedafname_PCR),
    Ag = Ag %>% factor(levels = c("N (BAU/ml)", "S1 (BAU/ml)", "RBD (BAU/ml)",
                                  "S1-OMICRON", "RBD-OMICRON", "RBD-DELTA")),
    Variant = Variant %>% replace_na("UNK"),
    `pre Ag (BAU/ml)` = case_when(
      Ag == "S1 (BAU/ml)" ~ pre_S1_BAU_ml,
      Ag == "N (BAU/ml)" ~ pre_N_BAU_ml,
    ),
    pre_N_status_txt = factor(pre_N_status_txt, levels = c("pre N -", "pre N +")),
    post_N_status_txt = case_when(post_N_status == 1 ~ "post N +",
                                  post_N_status == 0 ~ "post N -") %>% 
      factor(levels = c("post N -", "post N +"))
  )

# determine vacc_time intervals
data_InfectieNaVaccinatie %>% 
  count(vacc_time) %>% 
  mutate(n_sum = cumsum(n),
         n_sum / 450 * 100) #%>% view

# c(30,60,90,120, 150, 180, 210, 240)

# S1 RBD response
Ag.gam <- gam(Log_BAU_mL ~ 
               te(interval_bloedafname_PCR, vacc_time, k = c(15,15), bs = c("ps", "ps"), by = Ag) +
               post_N_status_txt,
              data = tabel_tijd_InfectieNaVaccinatie %>% 
                filter(Ag %in% c("S1 (BAU/ml)"),
                       previous_infection == "No evidence of\nprevious infection"), 
              method = "REML") # N hierin niet meenemen want dan ga je response met response correleren (N status vs N response)
summary(Ag.gam)

Ag.preddata <- expand.grid(
  Ag = "S1 (BAU/ml)",
  interval_bloedafname_PCR = seq(
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      min(na.rm = T),
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      max(na.rm = T),
    1),
  vacc_time = c(30,60,90,120, 150, 180, 210, 240),
  post_N_status_txt = c("post N -", "post N +")
  
  )

tmp <- predict.gam(
  object = Ag.gam,
  newdata = Ag.preddata,
  se.fit = TRUE) %>%
  as_tibble

# Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
Ag.preddata <- bind_cols(Ag.preddata, tmp) %>%
  mutate(
    fit_lwr = fit + qnorm(0.025)*se.fit,
    fit_upr = fit + qnorm(0.975)*se.fit,
    post_N_status_txt = post_N_status_txt %>% factor(levels =  c("post N -", "post N +"))
  )

Fig_3 <- ggplot() +
  geom_line(
    data = Ag.preddata %>% filter(Ag %in% c("S1 (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, y = 10^fit, color = post_N_status_txt)) +
  geom_ribbon(
    data = Ag.preddata %>% filter(Ag %in% c("S1 (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, ymin = 10^fit_lwr, ymax = 10^fit_upr, fill = post_N_status_txt),
    alpha = 0.5) +
  labs(y = "S1 BAU/mL", x = "Time since infection (days)", color = " ", fill = " ") +
  scale_alpha_continuous(guide="none") +
  scale_y_log10() +
  scale_x_continuous(limits = c(Ag.preddata$interval_bloedafname_PCR %>% min, 60),
                     breaks = c(Ag.preddata$interval_bloedafname_PCR %>% min, seq(10,60,10)),
                     sec.axis = sec_axis(~ . , name = "Time since vaccination (days)", breaks = NULL, labels = NULL)) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) + 
  facet_wrap(vars(vacc_time),
    #pre_N_status_txt ~ vacc_time, 
    scales = "fixed", nrow = 2) +
  theme_minimal() +
  theme(fill=NULL, panel.grid.minor = element_blank())

#RBD
Ag.gam <- gam(Log_BAU_mL ~ 
                te(interval_bloedafname_PCR, vacc_time, k = c(15,15), bs = c("ps", "ps"), by = Ag) +
                post_N_status_txt,
              data = tabel_tijd_InfectieNaVaccinatie %>% 
                filter(Ag %in% c("RBD (BAU/ml)"),
                       previous_infection == "No evidence of\nprevious infection"), 
              method = "REML") # N hierin niet meenemen want dan ga je response met response correleren (N status vs N response)
summary(Ag.gam)

Ag.preddata <- expand.grid(
  Ag = "RBD (BAU/ml)",
  interval_bloedafname_PCR = seq(
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      min(na.rm = T),
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      max(na.rm = T),
    1),
  vacc_time = c(30,60,90,120, 150, 180, 210, 240),
  post_N_status_txt = c("post N -", "post N +")
  
)

tmp <- predict.gam(
  object = Ag.gam,
  newdata = Ag.preddata,
  se.fit = TRUE) %>%
  as_tibble

# Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
Ag.preddata <- bind_cols(Ag.preddata, tmp) %>%
  mutate(
    fit_lwr = fit + qnorm(0.025)*se.fit,
    fit_upr = fit + qnorm(0.975)*se.fit,
    post_N_status_txt = post_N_status_txt %>% factor(levels =  c("post N -", "post N +"))
  )


# RBD 
Fig_S2 <- ggplot() +
  geom_line(
    data = Ag.preddata %>% filter(Ag %in% c("RBD (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, y = 10^fit, color = post_N_status_txt)) +
  geom_ribbon(
    data = Ag.preddata %>% filter(Ag %in% c("RBD (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, ymin = 10^fit_lwr, ymax = 10^fit_upr, fill = post_N_status_txt),
    alpha = 0.5) +
  labs(y = "RBD (BAU/ml)", x = "Time since infection (days)", color = " ", fill = " ") +
  scale_alpha_continuous(guide="none") +
  scale_y_log10() +
  scale_x_continuous(limits = c(Ag.preddata$interval_bloedafname_PCR %>% min, 60),
                     breaks = c(Ag.preddata$interval_bloedafname_PCR %>% min, seq(10,60,10)),
                     sec.axis = sec_axis(~ . , name = "Time since vaccination (days)", breaks = NULL, labels = NULL)) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) + 
  facet_wrap(vars(vacc_time),
             #pre_N_status_txt ~ vacc_time, 
             scales = "fixed", nrow = 2) +
  theme_minimal() +
  theme(fill=NULL, panel.grid.minor = element_blank())


# Numbers in the manuscript
# N response
pre_N.gam <- gam(Log_BAU_mL ~ Ag +
                   s(interval_bloedafname_PCR, k = 15, bs = "ps", by = Ag) +
                   s(vacc_time, k = 15, bs = "ps", by = Ag) +
                   ti(interval_bloedafname_PCR, vacc_time, k = c(15, 15), bs = c("ps", "ps"), by = Ag) +
                s(pre_N_status_txt, bs = "re", by = Ag),
              data = tabel_tijd_InfectieNaVaccinatie %>% filter(Ag %in% c("S1 (BAU/ml)", "RBD (BAU/ml)")), 
              method = "REML") # N hierin niet meenemen want dan ga je response met response correleren (N status vs N response)
summary(pre_N.gam)

Ag.preddata <- expand.grid(
  Ag = factor(c("S1 (BAU/ml)", "RBD (BAU/ml)"), levels = c("N (BAU/ml)", "S1 (BAU/ml)", "RBD (BAU/ml)")),
  interval_bloedafname_PCR = seq(
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      min(na.rm = T),
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      max(na.rm = T),
    1),
  vacc_time = c(30,60,90,120, 150, 180, 210, 240),
  #pre_N_status_txt = c("pre N -", "pre N +"),
  pre_N_status_txt = c("pre N -", "pre N +")
  
)

tmp <- predict.gam(
  object = pre_N.gam,
  newdata = Ag.preddata,
  se.fit = TRUE) %>%
  as_tibble

# Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
Ag.preddata <- bind_cols(Ag.preddata, tmp) %>%
  mutate(
    fit_lwr = fit + qnorm(0.025)*se.fit,
    fit_upr = fit + qnorm(0.975)*se.fit,
    #pre_N_status_txt = pre_N_status_txt %>% factor(levels =  c("pre N -", "pre N +")),
    pre_N_status_txt = pre_N_status_txt %>% factor(levels =  c("pre N -", "pre N +"))
  )

Fig_pre_status <- ggplot() +
  geom_line(
    data = Ag.preddata %>% filter(Ag %in% c("S1 (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, y = 10^fit, color = pre_N_status_txt)) +
  geom_ribbon(
    data = Ag.preddata %>% filter(Ag %in% c("S1 (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, ymin = 10^fit_lwr, ymax = 10^fit_upr, fill = pre_N_status_txt),
    alpha = 0.5) +
  labs(y = "S1 BAU/mL", x = "Time since infection (days)", color = " ", fill = " ") +
  scale_alpha_continuous(guide="none") +
  scale_y_log10() +
  scale_x_continuous(limits = c(Ag.preddata$interval_bloedafname_PCR %>% min, 60),
                     breaks = c(Ag.preddata$interval_bloedafname_PCR %>% min, seq(10,60,10)),
                     sec.axis = sec_axis(~ . , name = "Time since vaccination (days)", breaks = NULL, labels = NULL)) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) + 
  facet_wrap(vars(vacc_time),
             #pre_N_status_txt ~ vacc_time, 
             scales = "fixed", nrow = 2) +
  theme_minimal() +
  theme(fill=NULL, panel.grid.minor = element_blank())

variant.gam <- gam(Log_BAU_mL ~ 
                     Ag +
                     s(interval_bloedafname_PCR, k = 7, bs = "ps", by = Ag) +
                     s(vacc_time, k = 7, bs = "ps", by = Ag) +
                     ti(interval_bloedafname_PCR, vacc_time, k = c(7,7), bs = c("ps","ps"), by = Ag) +
                   #s(pre_N_status_txt, bs = "re", by = Ag) +
                   s(V_Omicron_Delta , bs = "re", by = Ag) +
                     s(pre_N_status_txt, bs = "re", by = Ag)
                   ,
                 data = tabel_tijd_InfectieNaVaccinatie %>% 
                   filter(Ag %in% c("S1 (BAU/ml)", "RBD (BAU/ml)", "N (BAU/ml)") & 
                            !is.na(V_Omicron_Delta)) %>% 
                   mutate(V_Omicron_Delta = factor(V_Omicron_Delta)), 
                 method = "REML") # N hierin niet meenemen want dan ga je response met response correleren (N status vs N response)
summary(variant.gam)


Ag.preddata <- expand.grid(
  Ag = factor(c("S1 (BAU/ml)", "RBD (BAU/ml)"), levels = c("N (BAU/ml)", "S1 (BAU/ml)", "RBD (BAU/ml)")),
  interval_bloedafname_PCR = seq(
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      min(na.rm = T),
    tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>%
      max(na.rm = T),
    1),
  vacc_time = c(30,60,90,120, 150, 180, 210, 240),
  pre_N_status_txt = c("pre N -", "pre N +"),
  V_Omicron_Delta = c("Delta", "Omicron")
  
)

tmp <- predict.gam(
  object = variant.gam,
  newdata = Ag.preddata,
  se.fit = TRUE) %>%
  as_tibble

# Bind tmp to rsv.preddata and calculate the p_inf including 95% lower and upper bound
Ag.preddata <- bind_cols(Ag.preddata, tmp) %>%
  mutate(
    fit_lwr = fit + qnorm(0.025)*se.fit,
    fit_upr = fit + qnorm(0.975)*se.fit,
    #pre_N_status_txt = pre_N_status_txt %>% factor(levels =  c("pre N -", "pre N +")),
    V_Omicron_Delta = V_Omicron_Delta %>% factor(levels =  c("Delta", "Omicron"))
  )

Fig_variant <- ggplot() +
  geom_line(
    data = Ag.preddata %>% filter(Ag %in% c("S1 (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, y = 10^fit, color = V_Omicron_Delta)) +
  geom_ribbon(
    data = Ag.preddata %>% filter(Ag %in% c("S1 (BAU/ml)")),
    aes(x = interval_bloedafname_PCR, ymin = 10^fit_lwr, ymax = 10^fit_upr, fill = V_Omicron_Delta),
    alpha = 0.5) +
  labs(y = "S1 BAU/mL", x = "Time since infection (days)", color = " ", fill = " ") +
  scale_alpha_continuous(guide="none") +
  scale_y_log10() +
  scale_x_continuous(limits = c(Ag.preddata$interval_bloedafname_PCR %>% min, 60),
                     breaks = c(Ag.preddata$interval_bloedafname_PCR %>% min, seq(10,60,10)),
                     sec.axis = sec_axis(~ . , name = "Time since vaccination (days)", breaks = NULL, labels = NULL)) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) + 
  facet_wrap(vars(vacc_time),
             #pre_N_status_txt ~ vacc_time, 
             scales = "fixed", nrow = 2) +
  theme_minimal() +
  theme(fill=NULL, panel.grid.minor = element_blank())



# N response
# the N-specific antibody response develops independent from vaccination
# in text
Ag.gam <- gam(Log_BAU_mL ~ 
                te(interval_bloedafname_PCR, k = 15, bs = "ps") +
                te(vacc_time, k = 15, bs = "ps"),
              data = tabel_tijd_InfectieNaVaccinatie %>% 
                filter(Ag %in% c("N (BAU/ml)"),
                       previous_infection == "No evidence of\nprevious infection"), 
              method = "REML") # N hierin niet meenemen want dan ga je response met response correleren (N status vs N response)
summary(Ag.gam)
