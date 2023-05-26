# ROBINS FIGURE CODE**

# read in RoB sheet

library(tidyverse)
library(readxl)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
loadfonts()
setwd("Desktop/Simone/Graduate school/Rotator cuff /Systematic Review/")
widerob <- read_excel("ROBRCT.xlsx")
widerob2 <- read_excel("ROBRCTAVG.xlsx")
# break into two dataframes
widerob_scores <- widerob %>%
                    select(Study, ends_with(".RoB"))
widerob_scores2 <- widerob2 %>%
  select(Study, ends_with(".RoB"))

                                 
widerob_questions <- widerob %>%
  select(Study, !ends_with(".RoB"))

widerob_questions2 <- widerob2 %>%
  select(Study, !ends_with(".RoB"))

# make long format
longrob_scores <- widerob_scores %>% 
  gather(Question, Answer, Conf.RoB:Overall.RoB) %>%
  mutate(Question = factor(Question, levels = names(widerob_scores)[-1]),
         Study = factor(Study, levels = levels(widerob$Study)),
         Answer = factor(Answer, levels = c("NA", "Low", "Low/Moderate", "Moderate","Moderate/Serious","Some Consern", "Serious", "Serious/Critical", "High", "Critical")
                         ))

longrob_scores2 <- widerob_scores2 %>% 
  gather(Question, Answer, Conf.RoB:Overall.RoB) %>%
  mutate(Question = factor(Question, levels = names(widerob_scores)[-1]),
         Study = factor(Study, levels = levels(widerob$Study)),
         Answer = factor(Answer, levels = c("NA", "Low", "Low/Moderate", "Moderate","Moderate/Serious","Some Consern", "Serious", "Serious/Critical", "High", "Critical")))


longrob_questions <- widerob_questions %>% 
  gather(Question, Answer, Conf.1:RepMeas.3) %>%
  mutate(Question = factor(Question, levels = names(widerob_questions)[-1]),
         Study = factor(Study, levels= levels(widerob$Study)),
         Answer = ifelse(is.na(Answer), "Not Applicable", Answer),
         Answer = factor(Answer, levels = c("Not Applicable", "No Information",
                                            "No", "Probably No", "Probably Yes", "Yes")))
longrob_questions2 <- widerob_questions2 %>% 
  gather(Question, Answer, Conf.1:RepMeas.3) %>%
  mutate(Question = factor(Question, levels = names(widerob_questions)[-1]),
         Study = factor(Study, levels= levels(widerob$Study)),
         Answer = ifelse(is.na(Answer), "Not Applicable", Answer),
         Answer = factor(Answer, levels = c("Not Applicable", "No Information",
                                            "No", "Probably No", "Probably Yes", "Yes")))


themeheat <- theme(text = element_text(family = "Arial"),
                   axis.text.x = element_text(angle = 45, family = "Arial", size = 8, hjust = 1),
                   axis.text.y = element_text(family = "Arial", size = 8),
                   axis.line = element_blank(),
                   legend.position = "none",
                   legend.key.size = unit(0.5, "cm"),
                   plot.margin = unit(c(0, 5.5, 0, 0), "pt"))

themehist <- theme(text = element_text(family = "Arial", size = 8),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.text.y = element_text(family = "Arial", size = 8),
                   axis.line = element_blank(),
                   legend.position = "left",
                   legend.key.size = unit(0.5, "cm"),
                   plot.margin = unit(c(0, 5.5, 0, 0), "pt"))

# make heatmaps
heatscores <- longrob_scores %>%
  ggplot() +
  geom_tile(aes(x = Question, y = fct_rev(Study), fill = Answer), width = 1) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("NA" = "grey",
                               "Critical" = "black",
                               "Serious/Critical" = "firebrick4",
                               "Serious" = "#7e03a8",
                               "Moderate/Serious" = "#b52f8c",
                               "Moderate" = "#de5f65",
                               "Low/Moderate" = '#f89540',
                               "Low" = "#fbd524")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat

heatscores2 <- longrob_scores2 %>%
  ggplot() +
  geom_tile(aes(x = Question, y = fct_rev(Study), fill = Answer), width = 1) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("NA" = "grey",
                               "Critical" = "black",
                               "Serious/Critical" = "firebrick4",
                               "Serious" = "#7e03a8",
                               "Moderate/Serious" = "#b52f8c",
                               "Moderate" = "#de5f65",
                               "Low/Moderate" = '#f89540',
                               "Low" = "#fbd524")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat

heatquestions <- longrob_questions %>%
  ggplot() +
  geom_tile(aes(x = Question, y = fct_rev(Study), fill = Answer), width = 1) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat 
heatquestions2 <- longrob_questions2 %>%
  ggplot() +
  geom_tile(aes(x = Question, y = fct_rev(Study), fill = Answer), width = 1) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat 

# now make stacked bar plots
countscores <- longrob_scores %>%
  ggplot(aes(x = Question, fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("NA" = "grey",
                               "Critical" = "black",
                               "Serious/Critical" = "firebrick4",
                               "Serious" = "#7e03a8",
                               "Moderate/Serious" = "#b52f8c",
                               "Moderate" = "#de5f65",
                               "Low/Moderate" = '#f89540',
                               "Low" = "#fbd524")) +
  xlab("") + ylab("Count") +
  theme_pubr() +
  themehist

countscores2 <- longrob_scores2 %>%
  ggplot(aes(x = Question, fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("NA" = "grey",
                               "Critical" = "black",
                               "Serious/Critical" = "firebrick4",
                               "Serious" = "#7e03a8",
                               "Moderate/Serious" = "#b52f8c",
                               "Moderate" = "#de5f65",
                               "Low/Moderate" = '#f89540',
                               "Low" = "#fbd524")) +
  xlab("Risk of Bias") + ylab("Count") +
  theme_pubr() +
  themehist


countquestions <- longrob_questions %>%
  ggplot(aes(x = Question, fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("Count") +
  theme_pubr() +
  themehist
countquestions2 <- longrob_questions2 %>%
  ggplot(aes(x = Question, fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("Count") +
  theme_pubr() +
  themehist

# combine plots
scores_plot <- plot_grid(countscores, heatscores,
                         ncol = 1, rel_heights = c(2, 5),
                         align = "v", axis = "l")
scores_plot2 <- plot_grid(countscores2, heatscores2,
                         ncol = 1, rel_heights = c(2, 5),
                         align = "v", axis = "l")

questions_plot <- plot_grid(countquestions, heatquestions, 
                            ncol = 1, rel_heights = c(2, 5),
                            align = "v", axis = "l")
questions_plot2 <- plot_grid(countquestions2, heatquestions2, 
                            ncol = 1, rel_heights = c(2, 5),
                            align = "v", axis = "l")

plot_grid(questions_plot, scores_plot, nrow = 1,
          labels = "auto", label_size = 10,
          label_fontfamily = "Arial",
          rel_widths = c(1.5, 1))
plot_grid(questions_plot2, scores_plot2, nrow = 1,
          labels = "auto", label_size = 10,
          label_fontfamily = "Arial",
          rel_widths = c(1.5, 1))


# try figure three with a different orientation
themeheat1 <- theme(text = element_text(family = "Arial"),
                    axis.text.x = element_text(angle = 45, family = "Arial", size = 8, hjust = 1),
                    axis.text.y = element_text(family = "Arial", size = 8),
                    axis.line = element_blank(),
                    legend.position = "none",
                    legend.key.size = unit(0.5, "cm"),
                    plot.margin = unit(c(0, 0, 0, 0), "pt"))

heatscores_1 <- longrob_scores %>%
  ggplot() +
  geom_tile(aes(y = fct_rev(Question), x = Study, fill = Answer), width = 1) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("NA" = "grey",
                               "Critical" = "black",
                               "Serious/Critical" = "firebrick4",
                               "Serious" = "#7e03a8",
                               "Moderate/Serious" = "#b52f8c",
                               "Moderate" = "#de5f65",
                               "Low/Moderate" = '#f89540',
                               "Low" = "#fbd524")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat1

heatquestions_1 <- longrob_questions %>%
  ggplot() +
  geom_tile(aes(y = fct_rev(Question), x = Study, fill = Answer), width = 1) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat1

themehist1 <- theme(text = element_text(family = "Arial", size = 8),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.x = element_text(family = "Arial", size = 8),
                    axis.line = element_blank(),
                    legend.position = "bottom",
                    legend.key.size = unit(0.5, "cm"),
                    legend.direction = "vertical",
                    plot.margin = unit(c(0, 0, 0, 0), "pt"))

countscores_1 <- longrob_scores %>%
  ggplot(aes(x = fct_rev(Question), fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("NA" = "grey",
                               "Critical" = "black",
                               "Serious/Critical" = "firebrick4",
                               "Serious" = "#7e03a8",
                               "Moderate/Serious" = "#b52f8c",
                               "Moderate" = "#de5f65",
                               "Low/Moderate" = '#f89540',
                               "Low" = "#fbd524")) +
  xlab("") + ylab("Count") +
  theme_pubr() +
  themehist1 + coord_flip()

countquestions_1 <- longrob_questions %>%
  ggplot(aes(x = fct_rev(Question), fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("Count") +
  guides(fill = guide_legend(ncol = 2)) +
  theme_pubr() +
  themehist1 + coord_flip()

# combine plots
scores_plot_1 <- plot_grid(heatscores_1, countscores_1,
                           nrow = 1, rel_widths = c(4, 2),
                           align = "h", axis = "b")
questions_plot_1 <- plot_grid(heatquestions_1, countquestions_1, 
                              nrow = 1, rel_widths = c(4, 2),
                              align = "h", axis = "b")

plot_grid(questions_plot_1, scores_plot_1, ncol = 1,
          labels = "auto", label_size = 10,
          label_fontfamily = "Arial",
          rel_heights = c(1.5, 1))


----- 
ggtexttable(df_widerob_scores,
            theme = ttheme( 
              tbody.style = tbody_style( color = "black",
                fill = c("NA" = "grey",
                         "Critical" = "black",
                         "Serious/Critical" = "firebrick4",
                         "Serious" = "#7e03a8",
                         "Moderate/Serious" = "#b52f8c",
                         "Moderate" = "#de5f65",
                         "Low/Moderate" = '#f89540',
                         "Low" = "#fbd524"))
            )
)
       
-------------------

matrix_widerob_scores <- as.matrix(widerob_scores[,-1L])
rownames(matrix_widerob_scores) <- widerob_scores[[1L]]

widerob_score_colors <- as.character(factor(matrix_widerob_scores, levels=c('Low', 'Low/Moderate','Moderate','Moderate/Serious','Serious',
                                                                            'Serious/Critical', 'Critical','NA'),
                                            labels=c('#fbd524','#f89540','#de5f65','#b53f8c','#7e03a8','firebrick4','black','grey')))

ggtexttable(matrix_widerob_scores,
            theme=ttheme(
              tbody.style=tbody_style(color='black',
                                      fill=widerob_score_colors)))
pdf("ROBtable.pdf", height = 20, width = 10)
ggtexttable(ggtexttable(matrix_widerob_scores,
                        theme=ttheme(
                          tbody.style=tbody_style(color='black',
                                                  fill=widerob_score_colors))))
dev.off()

png(filename="ROBtable.png", 
    width=4800,
    height= 2200, 
    units= "px",
    pointsize= 1,
    bg= "white",
    res= NA)
ggtexttable(matrix_widerob_scores,
            theme=ttheme(
              tbody.style=tbody_style(color='black',
                                      fill=widerob_score_colors)))
dev.off()



--- #robins table with column names 
#overall table
matrix_widerob2_scores <- widerob2 %>%
  select(Study, ends_with(".RoB")) %>%
  mutate(`Confounding Bias`=Conf.RoB,
         `Selection Bias`=Sel.RoB,
         `Missing Data Bias`=MissOut.RoB,
         `Outcome Measurement Bias`=OutMeas.RoB,
         `Reporting Bias`=RepMeas.RoB,
         `Overall Bias`=Overall.RoB,
         .keep='unused') %>%
  as.matrix()
widerob_score_colors <- c(rep.int("white", 2*nrow(matrix_widerob_scores)),
                          as.character(factor(matrix_widerob_scores[,c(-1L,-2L)],
                                              levels=c('Low', 'Low/Moderate','Moderate','Moderate/Serious','Serious',
                                                       'Serious/Critical', 'Critical','NA'),
                                              labels=c('#fbd524','#f89540','#de5f65','#b53f8c','#7e03a8','firebrick4','black','grey'))))


jpeg (filename="ROBtableAVG.png", 
     width=1000,
     height= 1200, 
     units= "px",
     pointsize= 1/100000,
     quality= 100,
     bg= "white",
     res= NA)
ggtexttable(matrix_widerob2_scores,
            theme=ttheme(
              tbody.style=tbody_style(color=ifelse(widerob_score_colors == "black", "white", "black"),
                                      fill=widerob_score_colors)))


dev.off()

countscores2 <- longrob_scores2 %>%
  mutate(`Confounding Bias`=Conf.RoB,
         `Selection Bias`=Sel.RoB,
         `Missing Data Bias`=MissOut.RoB,
         `Outcome Measurement Bias`=OutMeas.RoB,
         `Reporting Bias Bias`=RepMeas.RoB,
         `Overall Bias`=Overall.RoB,
         .keep='unused') %>%
  ggplot(aes(x = Question, fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("NA" = "grey",
                               "Critical" = "black",
                               "Serious/Critical" = "firebrick4",
                               "Serious" = "#7e03a8",
                               "Moderate/Serious" = "#b52f8c",
                               "Moderate" = "#de5f65",
                               "Low/Moderate" = '#f89540',
                               "Low" = "#fbd524")) +
  xlab("Risk of Bias") + ylab("Count") +
  theme_pubr() +
  themehist

##dual review table
matrix_widerob_scores <- widerob %>%
  select(Study, `Data entered by:`, ends_with(".RoB")) %>%
  mutate(Reviewer=`Data entered by:`,
         `Confounding Bias`=Conf.RoB,
         `Selection Bias`=Sel.RoB,
         `Missing Data Bias`=MissOut.RoB,
         `Outcome Measurement Bias`=OutMeas.RoB,
         `Reporting Bias`=RepMeas.RoB,
         `Overall Bias`=Overall.RoB,
         .keep='unused') %>%
  as.matrix()
widerob_score_colors <- c(rep.int("white", 2*nrow(matrix_widerob_scores)),
                          as.character(factor(matrix_widerob_scores[,c(-1L,-2L)],
                                              levels=c('Low', 'Low/Moderate','Moderate','Moderate/Serious','Serious',
                                                       'Serious/Critical', 'Critical','NA'),
                                              labels=c('#fbd524','#f89540','#de5f65','#b53f8c','#7e03a8','firebrick4','black','grey'))))


jpeg (filename="ROBtable2.png", 
    width=1000,
    height= 1200, 
    units= "px",
    pointsize= 1/100000,
    quality= 100,
    bg= "white",
    res= NA)
ggtexttable(matrix_widerob_scores,
            theme=ttheme(
              tbody.style=tbody_style(color=ifelse(widerob_score_colors == "black", "white", "black"),
                                      fill=widerob_score_colors)))


dev.off()