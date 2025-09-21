## This code is used to generate figures for Bayesian ANOVA output
## Author: Liam Brennan, WHET Lab, OSU
## Created: 13 FEB 2024
## Edited by G. Busquets-Vass and C. E. Meyer for for false killer whale stable isotope Bayesian ANOVA results
## Updated: SEPT 2025

#### CLEAN EVERYTHING ###
rm(list=ls())
graphics.off() # close all;
gc() # Clear memory 

# Load Packages required 
library(tidyverse)
library(ggplot2)

# #set working directory 
setwd ("your/directory/path/here")

##### For nitrogen isotope by group ####

# Load data for nitrogen 
stats <- read.csv("your_nitrogen_filename_here.csv")

#specify order of parameters 

stats$Parameter <- factor(stats$Parameter, levels = c("AUS_B","FIS_S","NZSR","NZSM", 
                                                      "NZB1E","NZB1L","NZB2"))
#CHECK THAT THIS WORKED 
levels(stats$Parameter)
str(stats)
unique(stats$Parameter)
unique(trimws(as.character(stats$Parameter)))

# Define colors for 7 groups using CB palette
pastel_colors <- c("AUS_B" = "#cc0044","FIS_S" = "#f0e442", "NZSR" = "#00556b", "NZSM" = "#00ccff",
                   "NZB1E" = "#8FD744", "NZB1L" = "#00ff66", "NZB2" = "#006600")

# Create the plot for nitrogen: means plus credible intervals for each group and save this

png(filename = "N_filename_here.png", width = 30, height = 20, units = 'cm', res = 400) #save a plot

Results <- ggplot(stats, aes(x = Parameter, y = Mean, fill = Parameter, color = Parameter)) +
  geom_errorbar(aes(ymin = perc_2.5, 
                    ymax = perc_97.5,
                    colour=Parameter),
                position = position_dodge(0.4),
                width = 0,
                alpha= 0.6, #this changes the transparency of the error bar       
                linewidth = 4) +
  geom_errorbar(aes(ymin = perc_25, 
                    ymax = perc_75,
                    colour=Parameter),
                position = position_dodge(0.4),
                width = 0,
                linewidth = 6) +
  geom_boxplot(aes(ymin = perc_50, lower = perc_50, middle = perc_50, upper = perc_50, ymax = perc_50),
               stat = "identity",
               width = 0.12,
               position = position_dodge(0.4),
               fill = gray(0.5),
               color = "black") +
  scale_color_manual(values= c("#cc0044","#F0E442","#00556b","#00ccff", "#8FD744","#00ff66","#006600"))+
  geom_point(aes(y = Mean, 
                 x = Parameter,
                 shape = Parameter),
             color= "black", 
             size = 2, 
             position = position_dodge(0.4)) +
  scale_shape_manual(values=c(17,15,19,19,19,19,19))+
  scale_fill_manual(values = pastel_colors) +  # Apply pastel fill colors
  scale_x_discrete(expand= c(0.05,0.1))+
  scale_y_continuous(limits = c(11, 17), breaks = seq(11, 17, by = 1))+
  labs(x = "",
       y = expression(delta^15 * "N (" * "\u2030" * ")")) +
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, vjust = 1, size = 14), 
        axis.text.y = element_text(color = "black", size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x.bottom = element_blank())

Results

dev.off()

##### For carbon isotope by group ####

# Load data for carbon
stats2 <- read.csv("your_carbon_filename_here.csv")

#specify order of parameters 
stats2$Parameter <- factor(stats2$Parameter, levels = c("AUS_B","FIS_S","NZSR","NZSM", 
                                                         "NZB1E","NZB1L","NZB2"))

# Define colors for 8 groups using CB palette - same as above 
pastel_colors <- c("AUS_B" = "#cc0044", "FIS_S" = "#f0e442","NZSR" = "#00556b", "NZSM" = "#00ccff",
                   "NZB1E" = "#8FD744", "NZB1L" = "#00ff66", "NZB2" = "#006600")


# Create the plot for carbon means plus credible intervals for each group and save this
png(filename = "C_filename_here.png", width = 30, height = 20, units = 'cm', res = 400) #save a plot

Results2 <- ggplot(stats2, aes(x = Parameter, y = Mean, fill = Parameter, color = Parameter)) +
  geom_errorbar(aes(ymin = perc_2.5, 
                    ymax = perc_97.5,
                    colour=Parameter),
                position = position_dodge(0.4),
                width = 0,
                alpha= 0.6,
                linewidth = 4) +
  geom_errorbar(aes(ymin = perc_25, 
                    ymax = perc_75,
                    colour=Parameter),
                position = position_dodge(0.4),
                width = 0,
                linewidth = 6) +
  geom_boxplot(aes(ymin = perc_50, lower = perc_50, middle = perc_50, upper = perc_50, ymax = perc_50),
               stat = "identity",
               width = 0.12,
               position = position_dodge(0.4),
               fill = gray(0.5),
               color = "black") +
  scale_color_manual(values= c("#cc0044","#F0E442","#00556b","#00ccff", "#8FD744","#00ff66","#006600"))+
  geom_point(aes(y = Mean, 
                 x = Parameter,
                 shape=Parameter),
             color= "black", 
             size = 2, 
             position = position_dodge(0.4)) +
  scale_shape_manual(values=c(17,15,19,19,19,19,19))+
  scale_fill_manual(values = pastel_colors) +  # Apply pastel fill colors
  scale_x_discrete(expand= c(0.05,0.1))+
  scale_y_continuous(limits = c(-18, -13), breaks = seq(-18, -13, by = 1))+
  labs(x = "",
       y = expression(delta^13 * "C (" * "\u2030" * ")")) +
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, vjust = 1, size = 14), 
        axis.text.y = element_text(color = "black", size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


Results2

dev.off()

#combine the two graphs above and give one axis (N and then C)

library(ggpubr)
png(filename = "combined_plots.png", width = 30, height = 30, units = 'cm', res = 400) #save a plot
combined1 <- ggarrange(Results, Results2, labels = c("A", "B"), ncol = 1, nrow = 2)
combined1

dev.off()

#this combined plot was edited further in Powerpoint
