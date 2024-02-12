# create directory 'output' if doesnt exist

PATH_figures = "/rivm/n/andewegs/Documents/SARS-CoV-2/07_VASCO_Infectie_Na_Vaccinatie/output/20230913/"
dir.create(PATH_figures, showWarnings = F)

tiff(str_c(PATH_figures, 'F1_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), units="in", width = 18, height = 15, res=300)

figuur_Ab_fit_PCR_pre_levels_all
dev.off()

fig_2 <- plot_grid(figuur_LR_N, figuur_LR_N_symptoms, labels = c("A", "B"), nrow = 2, rel_heights = c(0.45,0.55)) +
  draw_label("Interval blood sample and SARS-CoV-2 testing date (days)", x=0.5, 
             y=  0.06, vjust=-0.5, angle= 0, size=11)


tiff(str_c(PATH_figures, 'F2_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), units="in", width = 10, height = 8, res=300)

fig_2
dev.off()

# Fig 3
tiff(str_c(PATH_figures, 'F3_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), units="in", width = 14, height = 10, res=300)

Fig_3
dev.off()


# Fig 4
tiff(str_c(PATH_figures, 'F4_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), units="in", width = 10, height = 6, res=300)

Fig4
dev.off()


# Fig S1
ggsave(file = str_c(PATH_figures, 'S1_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), 
       plot = figuur_response_interval_week,
       width = 14, height = 8
)

# Fig s2
tiff(str_c(PATH_figures, 'S2_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), units="in", width = 14, height = 10, res=300)

Fig_S2
dev.off()

# Fig S3
# Fig 4
tiff(str_c(PATH_figures, 'S3_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), units="in", width = 10, height = 10, res=300)

fig_S3
dev.off()


# Fig S1
tiff(str_c(PATH_figures, 'S1_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), units="in", width = 10, height = 6, res=300)

figuur_response_interval_week
dev.off()


# Table 1
xlsx::write.xlsx(tabel1_csv, 
                 file = str_c(
                   PATH_figures, "tabel1_characteristics_",
                   format(now(), format = "%Y%m%d_%H%M"),
                   ".xlsx"
                 ))


xlsx::write.xlsx(tabel_N_status_csv, 
                 file = str_c(
                   PATH_figures, "tabel_post_N_status_characteristics_",
                   format(now(), format = "%Y%m%d_%H%M"),
                   ".xlsx"
                 ))
