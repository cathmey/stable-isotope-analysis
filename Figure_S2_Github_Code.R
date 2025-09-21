#This code is used to generate figures of pairwise comparisons of group means (28 total) for each isotope based on Bayesian ANOVA results
#Author: Liam Brennan, WHET Lab, OSU
#Created: 13 FEB 2024
#Edited by G. Busquets-Vass and C. E. Meyer for for false killer whale stable isotope Bayesian ANOVA results
#Updated: SEPT 2025

#load libraries 
library(ggplot2)
library(RColorBrewer)

#set working directory 
setwd ("your/directory/path/here")

#import N data 

N_data <- read.csv("N_filename_here.csv")

#order the comparisons

N_data$Parameter <- factor(N_data$Parameter, levels = c("AUS_B - FIS_S",
                                                        "AUS_B - NZSR",
                                                        "AUS_B - NZSM",
                                                        "AUS_B - NZB1E",
                                                        "AUS_B - NZB1L",
                                                        "AUS_B - NZB2",
                                                        "FIS_S - NZSR",
                                                        "FIS_S - NZSM",
                                                        "FIS_S - NZB1E",
                                                        "FIS_S - NZB1L",
                                                        "FIS_S - NZB2",
                                                        "NZSR - NZSM",
                                                        "NZSR - NZB1E",
                                                        "NZSR - NZB1L",
                                                        "NZSR - NZB2",
                                                        "NZSM - NZB1E",
                                                        "NZSM - NZB1L",
                                                        "NZSM - NZB2",
                                                        "NZB1E - NZB1L",
                                                        "NZB1E - NZB2",
                                                        "NZB1L - NZB2"))

#define colour palette for each pairwise comparison 

colors <- c("AUS_B - FIS_S" = "#80002a", 
"AUS_B - NZSR" = "#b3003b",
"AUS_B - NZSM" = "#cc0044",
"AUS_B - NZB1E" = "#f30051",
"AUS_B - NZB1L" = "#e0004b",
"AUS_B - NZB2" = "#ff1a66",
"FIS_S - NZSR" = "#d5c711",
"FIS_S - NZSM" = "#f0e442",
"FIS_S - NZB1E" = "#f4eb71",
"FIS_S - NZB1L" = "#f7f19d",
"FIS_S - NZB2" = "#e0d652",
"NZSR - NZSM" = "#004e62",
"NZSR - NZB1E" = "#00556b",
"NZSR - NZB1L" = "#004151",
"NZSR - NZB2" = "#126180",
"NZSM - NZB1E" = "#00fff2",
"NZSM - NZB1L" = "#00ccff",
"NZSM - NZB2" = "#009dc4",
"NZB1E - NZB1L" = "#bfe895",
"NZB1E - NZB2" = "#8FD744",
"NZB1L - NZB2" = "#00ff66")


#plot the N data 

png(filename = "N_figure_name_here.png", width = 30, height = 20, units = 'cm', res = 400) #save a plot

plotN <- ggplot(N_data, aes(x = Parameter, y = Mean, fill = Parameter, color = Parameter)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, fill = "grey", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.7) +
  geom_point(size = 3, shape = 21, fill = colors, color = "black", stroke = 0.5) +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) +
  scale_color_manual(values = color_palette) +
  labs(x = "", y = expression(Delta^15 * "N (" * "\u2030" * ")"), color = "Group") +
  theme_bw() +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 90, hjust = 1, vjust = 0.5, size = 11), 
        legend.position = "none", axis.text.y = element_text(color = "black", size=11), axis.title = element_text(size=11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x.bottom = element_blank())
  
plotN

dev.off()
  
#import C data 

C_data <- read.csv("C_filename_here.csv")

#order the comparisons

C_data$Parameter <- factor(C_data$Parameter, levels = c("AUS_B - FIS_S",
                                                        "AUS_B - NZSR",
                                                        "AUS_B - NZSM",
                                                        "AUS_B - NZB1E",
                                                        "AUS_B - NZB1L",
                                                        "AUS_B - NZB2",
                                                        "FIS_S - NZSR",
                                                        "FIS_S - NZSM",
                                                        "FIS_S - NZB1E",
                                                        "FIS_S - NZB1L",
                                                        "FIS_S - NZB2",
                                                        "NZSR - NZSM",
                                                        "NZSR - NZB1E",
                                                        "NZSR - NZB1L",
                                                        "NZSR - NZB2",
                                                        "NZSM - NZB1E",
                                                        "NZSM - NZB1L",
                                                        "NZSM - NZB2",
                                                        "NZB1E - NZB1L",
                                                        "NZB1E - NZB2",
                                                        "NZB1L - NZB2"))

#use same colours as above 

colors <- c("AUS_B - FIS_S" = "#80002a", 
            "AUS_B - NZSR" = "#b3003b",
            "AUS_B - NZSM" = "#cc0044",
            "AUS_B - NZB1E" = "#f30051",
            "AUS_B - NZB1L" = "#e0004b",
            "AUS_B - NZB2" = "#ff1a66",
            "FIS_S - NZSR" = "#d5c711",
            "FIS_S - NZSM" = "#f0e442",
            "FIS_S - NZB1E" = "#f4eb71",
            "FIS_S - NZB1L" = "#f7f19d",
            "FIS_S - NZB2" = "#e0d652",
            "NZSR - NZSM" = "#004e62",
            "NZSR - NZB1E" = "#00556b",
            "NZSR - NZB1L" = "#004151",
            "NZSR - NZB2" = "#126180",
            "NZSM - NZB1E" = "#00fff2",
            "NZSM - NZB1L" = "#00ccff",
            "NZSM - NZB2" = "#009dc4",
            "NZB1E - NZB1L" = "#bfe895",
            "NZB1E - NZB2" = "#8FD744",
            "NZB1L - NZB2" = "#00ff66")

#plot the C data 

png(filename = "C_figure_name_here.png", width = 30, height = 20, units = 'cm', res = 400) #save the plot 

plotC <- ggplot(C_data, aes(x = Parameter, y = Mean, fill = Parameter, color = Parameter)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, fill = "grey", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.7) +
  geom_point(size = 3, shape = 21, fill = colors, color = "black", stroke = 0.5) +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) +
  scale_color_manual(values = color_palette) +
  labs(x = "Pairwise group comparisons", y = expression(Delta^13 * "C (" * "\u2030" * ")"), color = "Group") +
  theme_bw() +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 90, hjust = 1, vjust = 0.5, size = 11), 
        legend.position = "none", axis.text.y = element_text(color = "black", size=11), axis.title.y = element_text(size=11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


plotC

dev.off()

#knit the above two graphs together and remove one x-axis

library(ggpubr)

png(filename = "combined_plot_name_here.png", width = 30, height = 30, units = 'cm', res = 400) #save the plot

combined <- ggarrange(plotN, plotC, labels = c("A", "B"), ncol = 1, nrow = 2)
combined

dev.off()
