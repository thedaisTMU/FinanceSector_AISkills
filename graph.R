library(ggplot2)
library(DaisTheme)
library(tidyverse)
library(data.table)
library(stringr)
library(scales)

library(plyr)
library(readr)
library(ggrepel)

#######Common elements
graph_fields <- fread("Graph_fields.csv")
graph_attributes <- fread("Graph_attributes.csv")
#############Figure 1

figure_1_data <- fread("Figure_1_data.csv")

finance_highlight <- c("Human resources professionals", "Cashiers",
                       "Administrative assistants","Paralegals and related occupations","Insurance, real estate and financial brokerage managers",
                       "Banking, insurance and other financial clerks","Financial auditors and accountants",
                       "Information systems specialists","Financial advisors","Customer services representatives - financial institutions","Insurance underwriters",
                       "Economists and economic policy researchers and analysts","Accounting technicians and bookkeepers","Graphic designers and illustrators",
                       "Seniors managers - public and private sector","Lawyers and Quebec notaries", "General office support workers",
                       "Other administrative services managers","Customer and information services supervisors","User support technicians")

finance_rename <- c("Human resources professionals","Cashiers",
                    "Administrative assistants","Paralegals","Insurance and financial managers",
                    "Bank and insurance clerks","Auditors and accountants",
                    "Information systems specialists","Financial advisors","Customer service reps, finance","Insurance underwriters",
                    "Economists","Bookkeepers","Graphic designers", "Senior managers", "Lawyers","Office support workers",
                    "Administrative services managers","Customer services supervisors","User support technicians")

figure_1_data$label <- ifelse(figure_1_data$noc_title %in% finance_highlight, figure_1_data$noc_title, "")
figure_1_data[, label  := finance_rename[match(label, finance_highlight)]]


figure_1_finance<-
  ggplot(figure_1_data, aes(x = aioe, y = groupmean, size = TOTAL_WITH_EMP_INC)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(4.7, 6.95), ylim = c(0.406,0.8), clip="off")+
  geom_point(alpha = 0.75,data = figure_1_data[is.na(figure_1_data$label),],color = "#eb0072", shape = 16) + 
  geom_point(alpha = 0.75,data = figure_1_data[!is.na(figure_1_data$label),], color = "#004c9b",shape = 16) +
  scale_size_continuous(range = c(2.5, 12), 
                        name = graph_attributes[Figure_number=="Figure 1",Legends],
                        labels = comma, 
                        limits = c(min(figure_1_data[,TOTAL_WITH_EMP_INC]), NA),
                        breaks = c(1000, 5000, 10000,25000)) +
  geom_text_repel(aes(label = label), 
                  color = "black",
                  family="Replica-Regular",
                  size = 2.5, 
                  box.padding = 0.5, 
                  max.overlaps = 10,
                  segment.color = "transparent")+
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  geom_vline(xintercept = median_AIOE, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  annotate("rect",
           xmax=6.96,
           xmin=6.25,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HEHC",English], 
           nudge_x = 0.2, 
           x = 6.605,
           y = 0.7978,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=6.96,
           xmin=6.25,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HELC",English], 
           nudge_x = 0.2, 
           x = 6.605,
           y = 0.41,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.467,
           xmin=4.777,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LELC",English], 
           nudge_x = 0.2, 
           x = 5.122,
           y = 0.41,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.467,
           xmin=4.777,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LEHC",English], 
           nudge_x = 0.2, 
           x = 5.122,
           y = 0.7978,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  labs(x = graph_attributes[Figure_number=="Figure 1",X_Axis],
       y = graph_attributes[Figure_number=="Figure 1",Y_Axis],
       title = "Figure 1",
       subtitle = "Finance Occupations by Quadrant and Employment Size",
       caption = "Figure 1") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

export.dais.plot("Exported/Figure_1_Finance.pdf",figure_1_finance)




finance_rename_fr <- c("Professionnels des ressources humaines","Caissiers",
                       "Adjoints administratifs","Para-juristes","Gestionnaires en assurances et en finances",
                       "Commis de banque et d’assurance","Vérificateurs et comptables",
                       "Spécialistes des systèmes d’information","Conseillers financiers",
                       "Représentants du service à la clientèle",
                       "Souscripteurs d’assurance","Économistes","Teneurs de livres","Graphistes",
                       "Cadres supérieurs","Avocats","Personnel de soutien de bureau",
                       "Gestionnaires des services administratifs",
                       "Superviseurs du service à la clientèle",
                       "Techniciens du soutien aux utilisateurs")

figure_1_data$label <- ifelse(figure_1_data$noc_title %in% finance_highlight, figure_1_data$noc_title, "")
figure_1_data[, label  := finance_rename_fr[match(label, finance_highlight)]]

figure_1_finance_fr<-
  ggplot(figure_1_data, aes(x = aioe, y = groupmean, size = TOTAL_WITH_EMP_INC)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(4.7, 6.95), ylim = c(0.406,0.8), clip="off")+
  geom_point(alpha = 0.75,data = figure_1_data[is.na(figure_1_data$label),],color = "#eb0072", shape = 16) + 
  geom_point(alpha = 0.75,data = figure_1_data[!is.na(figure_1_data$label),], color = "#004c9b",shape = 16) +
  scale_size_continuous(range = c(2.5, 12), 
                        name = graph_attributes[Figure_number=="Figure 1",Legends],
                        labels = comma, 
                        limits = c(min(figure_1_data[,TOTAL_WITH_EMP_INC]), NA),
                        breaks = c(1000, 5000, 10000,25000)) +
  # geom_text(aes(label = label), color = "black", hjust = 0, nudge_x = -0.12, nudge_y = -0.01, na.rm = TRUE, size = 6) +
  geom_text_repel(aes(label = label), 
                  color = "black",
                  family="Replica-Regular",
                  size = 2.5, 
                  box.padding = 0.5, 
                  max.overlaps = 10,
                  segment.color = "transparent")+
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  geom_vline(xintercept = median_AIOE, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  annotate("rect",
           xmax=6.96,
           xmin=6.2,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HEHC",French], 
           nudge_x = 0.2, 
           x = max(figure_1_data$aioe)-0.18,
           y = max(figure_1_data$groupmean),
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=6.96,
           xmin=6.2,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HELC",French], 
           nudge_x = 0.2, 
           x = max(figure_1_data$aioe)-0.18,
           y = min(figure_1_data$groupmean),
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.517,
           xmin=4.777,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LELC",French], 
           nudge_x = 0.2, 
           x = min(figure_1_data$aioe)+0.3,
           y = min(figure_1_data$groupmean),
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.517,
           xmin=4.777,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LEHC",French], 
           nudge_x = 0.2, 
           x = min(figure_1_data$aioe)+0.3,
           y = max(figure_1_data$groupmean),
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  labs(x = graph_attributes[Figure_number=="Figure 1",X_Axis_FR],
       y = graph_attributes[Figure_number=="Figure 1",Y_Axis_FR],
       title = "Figure 1",
       subtitle = "Professions financières par quadrant et taille de l’emploi",
       caption = "Figure 1") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


export.dais.plot("Exported/Figure_1_Finance_FR.pdf",figure_1_finance_fr)



########Figure 2
figure_2_data_finance <- fread("figure_2_data.csv")
figure_2_finance <- plot.scatter.dais(figure_2_data_finance,AIOE,V1,p.size=pct,group.by=Finance,
                                       plot.title = "Task specific Automation Exposure and Error Tolerance, Finance",
                                       x.axis = "Automation Exposure",
                                       y.axis = "Consequence of Error",
                                       colours = set.colours(2,categorical.choice=c("hot.pink","black")),
                                       plot.fig.num = "Figure 2") +
  scale_y_continuous(limits=c(2.7,4.7)) +
  scale_x_continuous(limits=c(5.2,6.8)) +
  theme(axis.line.x = element_line(linewidth = 0.5,colour="#999999"),axis.line.y = element_line(linewidth=0.5,colour="#999999"))

figure_2_data_finance[,Finance:="Hors finance"]
figure_2_data_finance[DWA_ID_dup %in% finance_dwa,Finance:="Finance"]

figure_2_finance_fr <- plot.scatter.dais(figure_2_data_finance,AIOE,V1,p.size=pct,group.by=Finance,
                                          plot.title = "Exposition à l’automatisation spécifique aux tâches et tolérance à l’erreur, finance",
                                          x.axis = "Exposition à l’automatisation",
                                          y.axis = "Conséquence des erreurs",
                                          colours = set.colours(2,categorical.choice=c("hot.pink","black")),
                                          plot.fig.num = "Figure 2") +
  scale_y_continuous(limits=c(2.7,4.7)) +
  scale_x_continuous(limits=c(5.2,6.8)) +
  theme(axis.line.x = element_line(linewidth = 0.5,colour="#999999"),axis.line.y = element_line(linewidth=0.5,colour="#999999"))


export.dais.plot("Exported/Figure_2_finance.pdf",figure_2_finance)
export.dais.plot("Exported/Figure_2_finance_fr.pdf",figure_2_finance_fr)
