
data_pre_post <- data_InfectieNaVaccinatie %>% 
  select(`N (BAU/ml)`, `S1 (BAU/ml)`,  `RBD (BAU/ml)`, `RBD-OMICRON`,`RBD-DELTA`, `S1-OMICRON`) %>% 
  pivot_longer(
    cols = everything(), names_to = "Ag", values_to = "(B)AU/ml"
  ) %>% 
  mutate(measurement = "post") %>% 
  bind_rows(
    data_InfectieNaVaccinatie %>% 
      select(starts_with("pre_") & !contains("status")) %>% 
      pivot_longer(
        cols = everything(), names_to = "Ag", values_to = "(B)AU/ml"
      ) %>% 
      mutate(measurement = "pre",
             Ag = Ag %>% recode(
                 pre_N_BAU_ml  = "N (BAU/ml)",
                 pre_S1_BAU_ml = "S1 (BAU/ml)",
                 pre_RBD_BAU_ml = "RBD (BAU/ml)",
                 pre_RBD_DELTA = "RBD-DELTA",
                 pre_RBD_OMICRON = "RBD-OMICRON",
                 pre_S1_OMICRON = "S1-OMICRON")
             )
  ) %>% 
  mutate(
    measurement = measurement %>% factor(levels = c("pre", "post"))
  )

hist_N <- data_pre_post %>% 
  filter(Ag == "N (BAU/ml)") %>% 
  ggplot(data = ., aes(x = `(B)AU/ml`)) +
  geom_histogram(bins = 75) +
  scale_x_log10(breaks = c(1,10,100,1000,10000)) +
  geom_vline(
    aes(xintercept = 14.3), color = "red") +
  facet_grid(measurement ~ Ag) +
  theme_minimal() +
  theme(strip.text.y = element_blank())

hist_S1 <- data_pre_post %>% 
  filter(Ag == "S1 (BAU/ml)") %>% 
  ggplot(data = ., aes(x = `(B)AU/ml`)) +
  geom_histogram(bins = 75) +
  geom_vline(
    aes(xintercept = 10.1), color = "red") +
  scale_x_log10(breaks = c(1,10,100,1000,10000, 100000)) +
  facet_grid(measurement ~ Ag) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        strip.text.y = element_blank()
        )

hist_RBD <- data_pre_post %>% 
  filter(Ag == "RBD (BAU/ml)") %>% 
  ggplot(data = ., aes(x = `(B)AU/ml`)) +
  geom_histogram(bins = 75) +
  scale_x_log10(breaks = c(1,10,100,1000,10000, 100000)) +
  facet_grid(measurement ~ Ag) +
  theme_minimal() +
  theme(axis.title.y = element_blank())

figuur_histogram_Ab <- plot_grid(
  hist_N,
  hist_S1,
  hist_RBD,
  ncol = 3
)


hist_N_tot <- data_pre_post %>% 
  filter(Ag == "N (BAU/ml)") %>% 
  ggplot(data = ., aes(x = `(B)AU/ml`, fill = measurement)) +
  geom_histogram(bins = 75) +
  scale_x_log10(breaks = c(1,10,100,1000,10000)) +
  geom_vline(
    aes(xintercept = 14.3), color = "red") +
  facet_grid(~Ag) +
  theme_minimal() +
  theme()

rm(data_pre_post)

#finite mixture regression model
