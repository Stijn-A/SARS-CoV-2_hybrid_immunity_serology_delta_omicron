tabel_tijd_InfectieNaVaccinatie <- data_InfectieNaVaccinatie %>% 
  select(interval_bloedafname_PCR, interval_bloedafname_EZD, 
         `N (BAU/ml)`, `S1 (BAU/ml)`, `RBD (BAU/ml)`, `S1-OMICRON`,`RBD-OMICRON`, `RBD-DELTA`, Variant, 
         pre_N_BAU_ml, pre_S1_BAU_ml, pre_RBD_BAU_ml, pre_N_status, pre_S1_status, eerder_positief, previous_infection) %>% 
  pivot_longer(
    cols = c(`N (BAU/ml)`, `S1 (BAU/ml)`, `RBD (BAU/ml)`, `S1-OMICRON`, `RBD-OMICRON`, `RBD-DELTA`),
    names_to = "Ag",
    values_to = "(B)AU/mL"
  ) %>% 
  mutate(
    #`AU_mL` = as.double(`(B)AU/mL`),
    `Log_AU_mL` = log10(`(B)AU/mL`),
    interval_bloedafname_PCR = as.integer(interval_bloedafname_PCR),
    Ag = Ag %>% factor(levels = c("N (BAU/ml)", "S1 (BAU/ml)", "RBD (BAU/ml)",
                                  "S1-OMICRON", "RBD-OMICRON", "RBD-DELTA")),
    Variant = Variant %>% replace_na("UNK"),
    pre_seropos = case_when(
      Ag == "S1 (BAU/ml)" ~ pre_S1_status,
      Ag == "N (BAU/ml)" ~ pre_N_status,
    ),
    `pre Ag (BAU/ml)` = case_when(
      Ag == "S1 (BAU/ml)" & pre_S1_BAU_ml >= 10.1 ~ pre_S1_BAU_ml,
      Ag == "S1 (BAU/ml)" & pre_S1_BAU_ml < 10.1 ~ 10.1,
      Ag == "N (BAU/ml)" & pre_N_BAU_ml >= 14.3 ~ pre_N_BAU_ml,
      Ag == "N (BAU/ml)" & pre_N_BAU_ml < 14.3 ~ 14.3,
      Ag == "RBD (BAU/ml)" ~ pre_RBD_BAU_ml,
    ),
    previous_infection = factor(previous_infection, levels = c("No evidence of\nprevious infection", "Evidence of\nprevious infection"))
  )

#https://stackoverflow.com/questions/67797464/plotting-gam-in-r-setting-custom-x-axis-limits
Ag.gam <- gam(Log_AU_mL ~ Ag + 
                s(interval_bloedafname_PCR, by = Ag, k = 15, bs = "ps"), 
              data = tabel_tijd_InfectieNaVaccinatie %>% filter(pre_N_status == 0 & eerder_positief == "No"), method = "REML") # eventuel fitten aan alleen pre_N_negatieve  %>% filter(pre_N_status == 0)

# number of samples
tabel_tijd_InfectieNaVaccinatie %>% filter(pre_N_status == 0 & eerder_positief == "No") %>% nrow / 6

summary(Ag.gam)
AIC(Ag.gam)

Ag.preddata <- expand.grid(
        Ag = c(tabel_tijd_InfectieNaVaccinatie$Ag %>% levels()),
       interval_bloedafname_PCR = seq(
         tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>% 
           min(na.rm = T),
         tabel_tijd_InfectieNaVaccinatie$interval_bloedafname_PCR %>% 
           max(na.rm = T),
         1)) %>% 
  mutate(
    Ag = Ag %>% factor(levels = c("N (BAU/ml)", "S1 (BAU/ml)", "RBD (BAU/ml)",
                                  "S1-OMICRON", "RBD-OMICRON", "RBD-DELTA"))
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
    Ag = Ag
)

tabel_tijd_InfectieNaVaccinatie = tabel_tijd_InfectieNaVaccinatie %>% 
  mutate(alpha = if_else(Variant != "UNK", 0.9, 0.7))

data_cutoff <- tibble(Ag = factor(tabel_tijd_InfectieNaVaccinatie$Ag %>% levels(),
                                  levels = tabel_tijd_InfectieNaVaccinatie$Ag %>% levels()),
                      cutoff = c(14.3, 10.1,NA,  NA, NA, NA))

figuur_Ab_fit_PCR_pre_levels_N <- ggplot() +
  geom_point(
    data = tabel_tijd_InfectieNaVaccinatie %>% filter(Ag == "N (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, 
        y = `(B)AU/mL`,
        color = previous_infection,
        fill = `pre Ag (BAU/ml)`,
        stroke = 0.4
        ),
    size = 2,
    shape=21
    ) +
  geom_line(
    data = Ag.preddata %>% filter(Ag == "N (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, y = 10^fit)) +
  geom_ribbon(
    data = Ag.preddata %>% filter(Ag == "N (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, ymin = 10^fit_lwr, ymax = 10^fit_upr),
    alpha = 0.4) +
  geom_hline(
    data = data_cutoff %>% filter(Ag == "N (BAU/ml)"),
    aes(yintercept = cutoff), color = "red") +
  scale_alpha_continuous(guide="none", range = c(0.6,1)) +
  scale_x_continuous(breaks = seq(0,80,10), expand = expansion(add = 0.5)) +
  scale_y_log10(breaks = c(0.1,1,10,100,1000,10000), limits = c(0.1, 10000), expand = expansion(add = 0.1)) +
  scale_color_discrete(type = c("black",  "red")) +
  scale_fill_distiller(trans = "log10", 
                       type = "seq", 
                       direction = 1,
                        na.value = "grey50", 
                       palette = 1
                       ) +
  labs(fill ="pre N (BAU/mL) ", color = "Previous infection",  
       x = "interval bloodsample - testing date (days)", title = " ") +
  theme_minimal() +
  theme(legend.box="horizontal", legend.position=c(.8,.17), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(order = 1), 
         fill = guide_colorbar(order = 2))


figuur_pre_levels_N_point <- data_InfectieNaVaccinatie_pre_serology %>% 
  mutate(
    interval_1e_vaccinatie = as.numeric(DUFS_bloedafname_pre - Vaccinatiedatum_1)
  ) %>% 
  ggplot(data = ., aes(y = pre_N_BAU_ml,
                       x = interval_1e_vaccinatie)) +
  geom_line(aes(group = StudyId), alpha = 0.2) +
  geom_point(
    aes(color = vacc_status),
  ) +
  geom_hline(
    data = data_cutoff %>% filter(Ag == "N (BAU/ml)"),
    aes(yintercept = cutoff), color = "red") +
  scale_color_brewer(type= "qual", palette = 5) +
  labs(y = "N (BAU/mL)", x = "interval bloodsample - 1st vaccination (days)", 
       color = "Vaccination status", title = " ") +
  scale_y_log10(breaks = c(0.1,1,10,100,1000,10000), limits = c(0.1, 10000), expand = expansion(add = 0.1)) +
  scale_x_continuous(breaks = seq(0,350,50), expand = expansion(add = 5)) +
  theme_minimal() +
  theme(legend.position=c(.9,.77))

figuur_levels_N_hist <- data_InfectieNaVaccinatie %>% 
  dplyr::select(`pre-infection N` = pre_N_BAU_ml, `post-infection N` = `N (BAU/ml)`) %>% 
  pivot_longer(cols = everything(), names_to = "measurement", values_to = "BAU/mL") %>% 
  mutate(measurement = factor(measurement, levels = c("pre-infection N", "post-infection N"))) %>% 
  ggplot(data = .) +
  geom_histogram(
    aes(x = `BAU/mL`), bins = 100) +
  #scale_y_continuous(limits = c(0,30)) +
  scale_x_log10(breaks = c(0.1,1,10,100,1000,10000, 100000), limits = c(0.1, 150000), expand = expansion(add = 0.2)) +
  #labs(y = "BAU / mL") +
  theme_minimal() +
  facet_wrap(~measurement, nrow = 2)

figuur_Ab_fit_PCR_pre_levels_S1 <- ggplot() +
  geom_point(
    data = tabel_tijd_InfectieNaVaccinatie %>% filter(Ag == "S1 (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, 
        y = `(B)AU/mL`,
        color = previous_infection,
        fill = `pre Ag (BAU/ml)`,
        stroke = 0.4
    ),
    size = 2,
    shape=21
  ) +
  geom_line(
    data = Ag.preddata %>% filter(Ag == "S1 (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, y = 10^fit)) +
  geom_ribbon(
    data = Ag.preddata %>% filter(Ag == "S1 (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, ymin = 10^fit_lwr, ymax = 10^fit_upr),
    alpha = 0.4) +
  geom_hline(
    data = data_cutoff %>% filter(Ag == "S1 (BAU/ml)"),
    aes(yintercept = cutoff), color = "red") +
  scale_alpha_continuous(guide="none", range = c(0.6,1)) +
  scale_x_continuous(breaks = seq(0,80,10), expand = expansion(add = 0.5)) +
  scale_y_log10(breaks = c(0.1,1,10,100,1000,10000,100000), limits = c(0.1, 100000), expand = expansion(add = 0.1)) +
  scale_color_discrete(type = c("black",  "red")) +
  scale_fill_distiller(trans = "log10", 
                       type = "seq", 
                       direction = 1,
                       na.value = "grey50", 
                       palette = 1
  ) +
  labs(fill ="pre S1 (BAU/mL) ", color = "Previous infection",  
       x = "interval bloodsample - testing date (days)", title = "      infection response") +
  theme_minimal() +
  theme(legend.box="horizontal", legend.position=c(.8,.17),  axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(order = 1), 
         fill = guide_colorbar(order = 2))

figuur_pre_levels_S1_point <- data_InfectieNaVaccinatie_pre_serology %>% 
  mutate(
    interval_1e_vaccinatie = as.numeric(DUFS_bloedafname_pre - Vaccinatiedatum_1)
  ) %>% 
  ggplot(data = ., aes(y = pre_S1_BAU_ml,
                       x = interval_1e_vaccinatie)) +
  geom_line(aes(group = StudyId), alpha = 0.2) +
  geom_point(
    aes(color = vacc_status),
  ) +
  geom_hline(
    data = data_cutoff %>% filter(Ag == "S1 (BAU/ml)"),
    aes(yintercept = cutoff), color = "red") +
  labs(y = "S1 (BAU/mL)", x = "interval bloodsample - 1st vaccination (days)", 
       color = "Vaccination status", title = "      vaccine response – prior to studied infection") +
  scale_color_brewer(type= "qual", palette = 5) +
  scale_y_log10(breaks = c(0.1,1,10,100,1000,10000,100000), limits = c(0.1, 100000), expand = expansion(add = 0.1)) +
  scale_x_continuous(breaks = seq(0,350,50), expand = expansion(add = 5)) +
  theme_minimal() +
  theme(legend.position=c(.9,.17))

figuur_levels_S1_hist <- data_InfectieNaVaccinatie %>% 
  dplyr::select(`pre-infection S1` = pre_S1_BAU_ml, `post-infection S1` = `S1 (BAU/ml)`) %>% 
  pivot_longer(cols = everything(), names_to = "measurement", values_to = "BAU/mL") %>% 
  mutate(measurement = factor(measurement, levels = c("pre-infection S1", "post-infection S1"))) %>% 
  ggplot(data = .) +
  geom_histogram(
    aes(x = `BAU/mL`), bins = 100) +
  #scale_y_continuous(limits = c(0,30)) +
  scale_x_log10(breaks = c(0.1,1,10,100,1000,10000, 100000), limits = c(0.1, 150000), expand = expansion(add = 0.2)) +
  #labs(y = "BAU / mL") +
  theme_minimal() +
  facet_wrap(~measurement, nrow = 2)

figuur_Ab_fit_PCR_pre_levels_RBD <- ggplot() +
  geom_point(
    data = tabel_tijd_InfectieNaVaccinatie %>% filter(Ag == "RBD (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, 
        y = `(B)AU/mL`,
        color = previous_infection,
        fill = `pre Ag (BAU/ml)`,
        stroke = 0.4
        #shape = factor(eerder_positief),
        #alpha = alpha
    ),
    size = 2,
    shape=21
  ) +
  geom_line(
    data = Ag.preddata %>% filter(Ag == "RBD (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, y = 10^fit)) +
  geom_ribbon(
    data = Ag.preddata %>% filter(Ag == "RBD (BAU/ml)"),
    aes(x = interval_bloedafname_PCR, ymin = 10^fit_lwr, ymax = 10^fit_upr),
    alpha = 0.4) +
  geom_hline(
    data = data_cutoff %>% filter(Ag == "RBD (BAU/ml)"),
    aes(yintercept = cutoff), color = "red") +
  scale_alpha_continuous(guide="none", range = c(0.6,1)) +
  scale_x_continuous(breaks = seq(0,80,10), expand = expansion(add = 0.5)) +
  scale_y_log10(breaks = c(0.1,1,10,100,1000,10000,100000), limits = c(0.1, 100000), expand = expansion(add = 0.1)) +
  scale_color_discrete(type = c("black",  "red")) +
  scale_fill_distiller(trans = "log10", 
                       type = "seq", 
                       direction = 1,
                       na.value = "grey50", 
                       palette = 1
  ) +
  labs(fill ="pre RBD (BAU/mL) ", color = "Previous infection",  
       x = "interval bloodsample - testing date (days)", title = " ") +
  theme_minimal() +
  theme(legend.box="horizontal", legend.position=c(.8,.17),  axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(order = 1), 
         fill = guide_colorbar(order = 2))



figuur_pre_levels_RBD_point <- data_InfectieNaVaccinatie_pre_serology %>% 
  mutate(
    interval_1e_vaccinatie = as.numeric(DUFS_bloedafname_pre - Vaccinatiedatum_1)
  ) %>% 
  ggplot(data = ., aes(y = pre_RBD_BAU_ml,
                       x = interval_1e_vaccinatie)) +
  geom_line(aes(group = StudyId), alpha = 0.2) +
  geom_point(
    aes(color = vacc_status),
  ) +
  geom_hline(
    data = data_cutoff %>% filter(Ag == "RBD (BAU/ml)"),
    aes(yintercept = cutoff), color = "red") +
  labs(y = "RBD (BAU / mL)", x = "interval bloodsample - 1st vaccination (days)", 
       color = "Vaccination status", title = " ") +
  scale_color_brewer(type= "qual", palette = 5) +
  scale_y_log10(breaks = c(0.1,1,10,100,1000,10000,100000), limits = c(0.1, 100000), expand = expansion(add = 0.1)) +
  scale_x_continuous(breaks = seq(0,350,50), expand = expansion(add = 5)) +
  theme_minimal() +
  theme(legend.position=c(.9,.17))

figuur_levels_RBD_hist <- data_InfectieNaVaccinatie %>% 
  dplyr::select(`pre-infection RBD` = pre_RBD_BAU_ml, `post-infection RBD` = `RBD (BAU/ml)`) %>% 
  pivot_longer(cols = everything(), names_to = "measurement", values_to = "BAU/mL") %>% 
  mutate(measurement = factor(measurement, levels = c("pre-infection RBD", "post-infection RBD"))) %>% 
  ggplot(data = .) +
  geom_histogram(
    aes(x = `BAU/mL`), bins = 100) +
  #scale_y_continuous(limits = c(0,30)) +
  scale_x_log10(breaks = c(0.1,1,10,100,1000,10000, 100000), limits = c(0.1, 150000), expand = expansion(add = 0.2)) +
  #labs(y = "BAU / mL") +
  theme_minimal() +
  facet_wrap(~measurement, nrow = 2)


figuur_Ab_fit_PCR_pre_levels_all <- plot_grid(
  figuur_pre_levels_S1_point, figuur_Ab_fit_PCR_pre_levels_S1,figuur_levels_S1_hist,
  figuur_pre_levels_RBD_point, figuur_Ab_fit_PCR_pre_levels_RBD,figuur_levels_RBD_hist,
  figuur_pre_levels_N_point, figuur_Ab_fit_PCR_pre_levels_N, figuur_levels_N_hist,
  ncol = 3,
  rel_widths = c(0.5,0.5,0.3),
  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
)

