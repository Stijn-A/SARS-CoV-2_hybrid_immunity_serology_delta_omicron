# proportional barplot 
# vacc status

fun_length <- function(x){
  return(data.frame(y=median(x),label= paste0("n=", length(x))))
}

gm_mean <- function(x){exp(mean(log(x)))}

data_InfectieNaVaccinatie %>% filter(!is.na(pre_N_status) &
                                       !is.na(cohorts_bloedname_PCR) & !is.na(`N (BAU/ml)`)) %>% 
  rename(`pre N (BAU/mL)` = pre_N_BAU_ml) %>% 
  group_by(cohorts_bloedname_PCR, previous_infection) %>% 
  summarise(
    n = n(),
    n_pos = sum(post_N_status == 1),
    gm_mean = gm_mean(`N (BAU/ml)`)
  ) %>% as.data.frame()

# mean on this log scale is already the geometric mean
# https://stackoverflow.com/questions/29541853/how-does-ggplot-scale-continuous-work-combined-with-stat-summary
figuur_N_preN_postN <- ggplot(data = data_InfectieNaVaccinatie %>% filter(!is.na(pre_N_status) &
                                                                            !is.na(cohorts_bloedname_PCR) & !is.na(`N (BAU/ml)`)) %>% 
                                rename(`pre N (BAU/mL)` = pre_N_BAU_ml),
                              aes(x = previous_infection, y = `N (BAU/ml)`)) +
  geom_jitter(aes(color = `pre N (BAU/mL)`), width = 0.2) +
  geom_boxplot(width = 0.2) +
  geom_violin(aes(fill = previous_infection),alpha = 0.2,
              trim = T) +
  
  scale_y_log10(breaks = c(0, 1, 10, 100, 1000, 10000, 100000)) +
  scale_color_distiller(
    trans = "log10",
    type = "div",
    na.value = "grey50",
    palette = "RdYlBu"
  ) +
  scale_fill_brewer(type = "div", palette = "RdBu", direction = -1) +
  stat_summary(aes(y = 4000),fun.data = fun_length, geom = "text", size = 3) +
  stat_summary(fun='mean', geom='crossbar', size=0.2, width = 0.2, col='red') +
  geom_signif(aes(x = previous_infection, y = `N (BAU/ml)`),
              comparisons = list(c("Evidence of\nprevious infection", "No evidence of\nprevious infection")), 
              map_signif_level=TRUE,
              vjust = 3
              #test = wilcox.test(paired = F)
  ) +
  labs(title = "interval blood sample-testing date (days)", y = "post N (BAU/ml)") +
  guides(fill=guide_legend(title=" ")) +
  facet_wrap(vars(cohorts_bloedname_PCR), drop = T, nrow = 1) +
  theme_minimal() +
  theme(axis.title.x =  element_blank(),
        axis.text.x = element_text(angle = 30)
        )


figuur_S1_preN_postS <- ggplot(data = data_InfectieNaVaccinatie %>% filter(!is.na(pre_S1_status) &
                                                                             !is.na(cohorts_bloedname_PCR)) %>% 
                                 rename(`pre N (BAU/mL)` = pre_N_BAU_ml),
                               aes(x = previous_infection, y = `S1 (BAU/ml)`)) +
  geom_jitter(aes(color = `pre N (BAU/mL)`), width = 0.2) +
  geom_boxplot(width = 0.2) +
  geom_violin(aes(fill = previous_infection),alpha = 0.2,
              trim = T) +
  scale_y_log10(breaks = c(0, 1, 10, 100, 1000, 10000, 100000)) +
  scale_color_distiller(
    trans = "log10",
    type = "div",
    na.value = "grey50",
    palette = "RdYlBu"
  ) +
  scale_fill_brewer(type = "div", palette = "RdBu", direction = -1) +
  stat_summary(fun='mean', geom='crossbar', size=0.2, width = 0.2, col='red') +
  stat_summary(aes(y = 150000), fun.data = fun_length, geom = "text", size = 3) +
  geom_signif(aes(x = previous_infection, y = `S1 (BAU/ml)`),
              comparisons = list(c("Evidence of\nprevious infection", "No evidence of\nprevious infection")), 
              map_signif_level=TRUE,
              vjust = 3
              #test = wilcox.test(paired = F)
  ) +
  labs(y = "post S1 (BAU/ml)") +
  guides(fill=guide_legend(title=" ")) +
  facet_wrap(vars(cohorts_bloedname_PCR), drop = T, nrow = 1) +
  theme_minimal() +
  theme(axis.title.x =  element_blank(),
        axis.text.x = element_text(angle = 30))


figuur_response_interval_week <- plot_grid(
  figuur_N_preN_postN,
  #figuur_S1_preS_postS,
  figuur_S1_preN_postS,
  labels = c("A", "B"
             #, "C"
             ),
  ncol = 1
)
