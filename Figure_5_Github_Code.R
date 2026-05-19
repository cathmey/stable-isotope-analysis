#this code produces plots of carbon and nitrogen values for putative prey species, with standard deviation 
#isotope values of prey species extracted from: Rosas-Luis et al.,(2016); Handley (2017); Kellett (2021); Rayner et al.,(2021); Hinton (2023); Fontaine et al., (2026).
#see main text for reference details and doi information 

#set working directory
setwd("file/location/here")

#load libraries
library(ggplot2)
library(ggrepel)

#import isotopic data for New Zealand samples and prey 
data1 <- read.csv("Filename_here.csv")

# Ensure numeric columns
data1$d13C_mean <- as.numeric(data1$d13C_mean)
data1$d15N_mean <- as.numeric(data1$d15N_mean)

# Remove NA values
data1 <- na.omit(data1)

#assign colours to each sample partition and potential prey species 
cols <- c(
  "Prey_fish" = "darkgrey",
  "Prey_squid"= "black",
  "NZ_S_Rēkohu" = "#004151",
  "NZ_S_Māhia" = "#00ccff",
  "NZ_B_1_early" = "#8FD744", 
  "NZ_B_1_late" = "#00ff66",
  "NZ_B_2"= "#006600")

#assign shapes to prey type and FKW sample partitions 

shapes <- c(
  "FKW"            = 15,  # filled square
  "Neritic_fish"  = 16,  # filled circle
  "Pelagic_fish" = 18, #filled diamond
  "Neritic_squid"  = 17  # filled triangle (point up)
)

# plot and save NZ biplot
png(filename = "output_file_name_here.png", width = 30, height = 30, units = 'cm', res = 400) 

preyNZ <- ggplot(data = data1, aes(x = d13C_mean, y = d15N_mean)) +
  geom_point(aes(color = Group, shape = Prey_type, fill = after_scale(color)), size = 7) +  # Plot points
  geom_errorbar(aes(ymin = d15N_mean - d15N_sd, ymax = d15N_mean + d15N_sd, color = Group), width = 0) +  # Error bars (y)
  geom_errorbarh(aes(xmin = d13C_mean - d13C_sd, xmax = d13C_mean + d13C_sd, color = Group), height = 0) +  # Error bars (x)
  geom_text_repel(aes(label = Species), size = 6, fontface = "italic", point.padding = 0.7, box.padding = 0.75) + # Avoid overlapping labels
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(limits = c(-24, -14)) +  # X-axis limits
  scale_y_continuous(limits = c(7, 17)) +  # Y-axis limits
  labs(
    x = expression(delta^13*C~("\u2030")),
    y = expression(delta^15*N~("\u2030")),
    title = "",
    color = "",
    shape = ""
  ) +
  guides (shape = guide_legend(override.aes = list(size = 3)))+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.text.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(color = "black", size = 16), 
    axis.title = element_text(size = 16))

print(preyNZ)  # Ensure the plot is printed before closing the device
dev.off()


#repeat the above for potential prey from Falkland Islands/South Atlantic

#import data 
data2<- read.csv("Filename_here.csv")

#assign colours 
cols2 <- c(
  "Prey_fish" = "darkgrey",
  "Prey_squid"= "black",
  "FIS_S" = "#F0E442")

#assign shapes to prey type and FKW sample partition

shapes2 <- c(
  "FKW"            = 15,  # filled square
  "Demersal_fish"  = 16,  # filled circle
  "Neritic_squid"  = 17,  # filled triangle (point up)
  "Oceanic_squid"  = 25   # filled triangle (point down)
)

# Ensure numeric columns
data2$d13C_mean <- as.numeric(data2$d13C_mean)
data2$d15N_mean <- as.numeric(data2$d15N_mean)

# Remove NA values
data2 <- na.omit(data2)

# Save the plot
png(filename = "output_file_name_here.png", width = 30, height = 30, units = 'cm', res = 400) 

preyFI <- ggplot(data = data2, aes(x = d13C_mean, y = d15N_mean)) +
  geom_point(aes(color = Group, shape = Prey_type, fill = after_scale(color)), 
             size = 7) +
  geom_errorbar(aes(ymin = d15N_mean - d15N_sd, ymax = d15N_mean + d15N_sd, color = Group), width = 0) +  # Error bars (y)
  geom_errorbarh(aes(xmin = d13C_mean - d13C_sd, xmax = d13C_mean + d13C_sd, color = Group), height = 0) +  # Error bars (x)
  geom_text_repel(aes(label = Species), size = 6, fontface = "italic", point.padding = 0.7, box.padding = 0.75, segment.color = NA) + # Avoid overlapping labels
  scale_color_manual(values = cols2) +
  scale_shape_manual(values = shapes2) +
  scale_x_continuous(limits = c(-24, -14)) +  # X-axis limits
  scale_y_continuous(limits = c(7, 17)) +  # Y-axis limits
  labs(
    x = expression(delta^13*C~("\u2030")),
    y = expression(delta^15*N~("\u2030")),
    title = "",
    color = "",
    shape = ""
  ) +
  guides (shape = guide_legend(override.aes = list(size = 3)))+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.text.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(color = "black", size = 16), 
    axis.title = element_text(size = 16))


print(preyFI)  # Ensure the plot is printed before closing the device
dev.off()

#knit these two figures together 

library(ggpubr)

png(filename = "knitted_figures.png", width = 50, height = 35, units = 'cm', res = 400) #save a plot

combined <- ggarrange(preyFI, preyNZ, labels = c("A", "B"), ncol = 2, nrow = 1)

print(combined)
dev.off()

#note these plots were further edited in MS PowerPoint 
