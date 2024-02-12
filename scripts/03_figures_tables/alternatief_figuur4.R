library(broom)

sign_Delta_Omicron <- tabel_ratio_variant %>% 
  filter(comparison == "RBD Omicron BA.1 / RBD Delta" & cohorts_bloedname_PCR %in% c("14-20", "21-27")) %>% 
  select(comparison, cohorts_bloedname_PCR, ratio, V_Omicron_Delta) %>% 
  group_by(cohorts_bloedname_PCR) %>% 
  summarise(ttest = list(t.test(ratio ~ V_Omicron_Delta, na.action=na.omit))) %>%
  mutate(ttest = map(ttest, tidy)) %>%
  unnest(cols = c(ttest)) %>%
  select(cohorts_bloedname_PCR, estimate, estimate1, estimate2, p.value)

tabel_ratio_variant %>% 
  filter(comparison == "RBD Omicron BA.1 / RBD Delta" & V_Omicron_Delta %in% c("Omicron") & cohorts_bloedname_PCR %in% c("28-34", "21-27")) %>% 
  select(comparison, cohorts_bloedname_PCR, ratio, V_Omicron_Delta) %>% 
  #group_by(V_Omicron_Delta) %>% 
  summarise(ttest = list(t.test(ratio ~ cohorts_bloedname_PCR))) %>%
  mutate(ttest = map(ttest, tidy)) %>%
  unnest(cols = c(ttest)) %>%
  select(estimate, estimate1, estimate2, p.value)


df %>%
  group_by(factor) %>%
  summarise(ttest = list(t.test(value ~ samediff))) %>%
  mutate(ttest = map(ttest, tidy)) %>%
  unnest() %>%
  select(factor, estimate, estimate1, estimate2, p.value)

Fig4 <-  
  ggplot(data = tabel_ratio_variant %>% 
           filter(comparison == "RBD Omicron BA.1 / RBD Delta"),
         aes(x = cohorts_bloedname_PCR, y = ratio,# fill = V_Omicron_Delta, 
             color = V_Omicron_Delta
             )) +
  ggtitle("RBD Omicron BA.1 / RBD Delta") +
  #geom_violin(alpha = 0.3) +
  geom_boxplot(width = 0.3, 
               alpha = 0.3, 
               position = position_dodge(width = 0.8), 
               outlier.alpha = 0
               ) +
  geom_point(position=position_jitterdodge(dodge.width = 0.8)) +
  #geom_jitter(alpha = 0.3, width = 0.1) +
  geom_signif(comparisons = list(#c("7-13", "14-20"),
                                 c("Delta", "Omicron")),
              map_signif_level = TRUE
  ) +
  geom_text(data = tabel_n, color = "black", 
            aes(x = cohorts_bloedname_PCR, label = str_c("n= ",n),
                group = V_Omicron_Delta, 
                y = 0.1), 
            size = 3,
            position = position_dodge(width = .9)
  ) +
  #scale_y_continuous(limits = c(-0.05,1.5)) +
  scale_color_brewer(type = "qual", palette = 6) +
  #facet_wrap(vars(cohorts_bloedname_PCR), scales = "fixed", nrow = 2) +
  theme_minimal()
