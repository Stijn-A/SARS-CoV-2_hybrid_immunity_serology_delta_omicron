
tabel_ratio_variant <- data_InfectieNaVaccinatie %>%
  select(
    StudyId,
    starts_with("RBD"),
    starts_with("S1"),
    starts_with("adjusted"),
    Variant,
    V_Omicron_Delta,
    interval_bloedafname_PCR,
    cohorts_bloedname_PCR,
    cohorts_bloedname_PCR_2wks,
    vacc_time,
    vacc_dose,
    previous_infection,
    post_N_status
  ) %>%
  filter(V_Omicron_Delta == "Delta" |
           Variant %in% c("Omicron BA.1", "Delta"),
         previous_infection == "No evidence of\nprevious infection") %>%
  mutate(
    Na_21_days =  if_else(
      interval_bloedafname_PCR >= 21,
      "21+ days after +pcr",
      "<21 days after +pcr"
    ) %>%
      factor(levels = c("<21 days after +pcr", "21+ days after +pcr")),
    
    # `RBD Omicron BA.1 / RBD Delta` = `adjusted\nRBD-OMICRON` / `adjusted\nRBD-DELTA`,
    # `RBD Wildtype / RBD Delta` = `RBD` / `adjusted\nRBD-DELTA`,
    # `RBD Omicron BA.1 / RBD Wildtype` = `adjusted\nRBD-OMICRON` / `RBD`,
    # `RBD Omicron BA.1 / RBD Wildtype` = `adjusted\nRBD-OMICRON` / `RBD`,
    # `S1 Omicron BA.1 / S1 Wildtype` = `S1-OMICRON` / `S1`,
    #
    `RBD Omicron BA.1 / RBD Delta` = `RBD-OMICRON` / `RBD-DELTA`,
    `RBD Wildtype / RBD Delta` = `RBD` / `RBD-DELTA`,
    `RBD Omicron BA.1 / RBD Wildtype` = `RBD-OMICRON` / `RBD`,
    `RBD Omicron BA.1 / RBD Wildtype` = `RBD-OMICRON` / `RBD`,
    `S1 Omicron BA.1 / S1 Wildtype` = `S1-OMICRON` / `S1`,
  ) %>%
  pivot_longer(
    cols = c(
      `RBD Omicron BA.1 / RBD Delta`,
      `RBD Wildtype / RBD Delta`,
      `RBD Omicron BA.1 / RBD Wildtype`,
      `S1 Omicron BA.1 / S1 Wildtype`
    ),
    values_to = "ratio",
    names_to = "comparison"
  ) %>%
  mutate(
    comparison = factor(comparison),
    V_Omicron_Delta = if_else(V_Omicron_Delta == "Omicron", "Omicron BA.1", "Delta") %>%   factor(levels = c("Delta", "Omicron BA.1"))
  )

tabel_n = data_InfectieNaVaccinatie %>%
  filter(V_Omicron_Delta == "Delta" |
           Variant %in% c("Omicron BA.1", "Delta"),
         previous_infection == "No evidence of\nprevious infection") %>%
  mutate(
    V_Omicron_Delta = if_else(V_Omicron_Delta == "Omicron", "Omicron BA.1", "Delta") %>%   factor(levels = c("Delta", "Omicron BA.1"))
  ) %>%
  count(cohorts_bloedname_PCR, V_Omicron_Delta)

data_InfectieNaVaccinatie %>%
  filter(!is.na(V_Omicron_Delta) & !is.na(RBD))

Fig4 <-
  ggplot() +
  geom_violin(
    data = tabel_ratio_variant %>%
      filter(comparison == "RBD Omicron BA.1 / RBD Delta"),
    aes(x = V_Omicron_Delta, y = ratio)
  ) +
  geom_boxplot(
    data = tabel_ratio_variant %>%
      filter(comparison == "RBD Omicron BA.1 / RBD Delta"),
    aes(x = V_Omicron_Delta, y = ratio),
    width = 0.3
  ) +
  geom_signif(
    data = tabel_ratio_variant %>%
      filter(comparison == "RBD Omicron BA.1 / RBD Delta"),
    aes(x = V_Omicron_Delta, y = ratio),
    comparisons = list(c("Delta", "Omicron BA.1")),
    map_signif_level = TRUE,
    vjust = 3
    #test = wilcox.test(paired = F)
  ) +
  geom_jitter(
    data = tabel_ratio_variant %>%
      filter(comparison == "RBD Omicron BA.1 / RBD Delta"),
    aes(x = V_Omicron_Delta, y = ratio),
    alpha = 0.3,
    width = 0.1
  ) +
  geom_text(data = tabel_n,
            aes(x = V_Omicron_Delta, label = str_c("n= ", n)),
            y = 0,) +
  labs(y = "RBD Omicron BA.1 / RBD Delta", x = "Variant of infection") +
  scale_y_continuous(limits = c(-0.5, 11)) +
  scale_color_distiller(type = "div", palette = 3) +
  facet_wrap(vars(cohorts_bloedname_PCR),
             scales = "fixed",
             nrow = 2) +
  theme_minimal()


FigS3_A <- tabel_ratio_variant %>%
  filter(comparison == "RBD Omicron BA.1 / RBD Wildtype") %>%
  ggplot(aes(x = V_Omicron_Delta, y = ratio)) +
  labs(y = "RBD Omicron BA.1 / RBD Wildtype") +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  geom_signif(
    comparisons = list(c("Delta", "Omicron BA.1")),
    map_signif_level = TRUE,
    vjust = 1.5
    #test = wilcox.test(paired = F)
  ) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  scale_color_distiller(type = "div", palette = 3) +
  facet_wrap(vars(cohorts_bloedname_PCR),
             scales = "fixed",
             nrow = 1) +
  theme_minimal()

FigS3_B <- tabel_ratio_variant %>%
  filter(comparison == "RBD Wildtype / RBD Delta") %>%
  ggplot(aes(x = V_Omicron_Delta, y = ratio)) +
  labs(y = "RBD Wildtype / RBD Delta") +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  geom_signif(
    comparisons = list(c("Delta", "Omicron BA.1")),
    map_signif_level = TRUE,
    vjust = 1.5
    #test = wilcox.test(paired = F)
  ) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  scale_color_distiller(type = "div", palette = 3) +
  facet_wrap(vars(cohorts_bloedname_PCR),
             scales = "fixed",
             nrow = 1) +
  theme_minimal()

FigS3_C <- tabel_ratio_variant %>%
  filter(comparison == "S1 Omicron BA.1 / S1 Wildtype") %>%
  ggplot(aes(x = V_Omicron_Delta, y = ratio)) +
  labs(y = "S1 Omicron BA.1 / S1 Wildtype") +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  geom_signif(
    comparisons = list(c("Delta", "Omicron BA.1")),
    map_signif_level = TRUE,
    vjust = 1.5
  ) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  scale_color_distiller(type = "div", palette = 3) +
  facet_wrap(vars(cohorts_bloedname_PCR),
             scales = "fixed",
             nrow = 1) +
  theme_minimal()

Fig4
fig_S3 <- plot_grid(FigS3_A,
                    FigS3_B,
                    FigS3_C,
                    ncol = 1,
                    labels = c("A", "B", "C"))

