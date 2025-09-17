#set working directory
setwd("file_location_here")

#load libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# import file of sampling locations 
samples <-read.csv("Filename_here.csv")

#check data has imported correctly
view(samples)

#get world map data
#returns world country polygons at a specified scale, or points of tiny-countries, sf = simple features 
world <- ne_countries(scale = 50, returnclass = "sf") 

# Adjust world longitudes for Pacific-centered map
#crs = coordinate reference system, needs to be specified for each map due to different focuses, and to ensure scales are accurate 
world <- st_transform(world, crs = "+proj=longlat +datum=WGS84 +lon_wrap=180")

# Plot and save the broad-scale map

png(filename = "All_samples_map.png", width = 30, height = 10, units = 'cm', res = 400)


map <- ggplot(data = world) +
  geom_sf(fill = "gray70", color = "gray70")+
  geom_point(data = samples, aes(x = Long, y = Lat, shape = Type, fill = Location), size = 3.5, color = "black", stroke = 0.7) + # Filled points with black outline
  scale_fill_manual(values = c("AUS_B" = "#cc0044", "FIS_S" = "#F0E442", "NZ_S_Mixed" = "blue" , "NZ_S_Rekohu" = "#004151", "NZ_S_Mahia" = "#00ccff", "NZ_B_1_early" = "#8FD744", "NZ_B_1_late" = "#00ff66","NZ_B_2" = "#006600"), guide = "none")+  #specify colours and remove legend for locations 
  scale_shape_manual(labels = c("Biopsy", "Stranding"), values = c(21, 24)) +
  theme_minimal() + # # Change theme to allow axis lines and tick marks
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_orienteering(line_width = 0.5, text_size= 9)) +
  labs(title = "", x = "", y = "", shape = "Type") +
  coord_sf(xlim = c(110, 320), ylim = c(-60, -5), expand = FALSE)+
  annotation_scale(location = "bl", line_width = 1,
                   height = unit(0.25, "cm"),
                   pad_x = unit(0.3, "cm"),
                   pad_y = unit(0.25, "cm")) + #scale to bottom left 
  scale_x_continuous(breaks = seq(120, 300, by = 20)) + # Custom longitude breaks
  scale_y_continuous(breaks = seq(-60, -10, by = 10)) + # Custom latitude breaks
  theme(legend.position = c(0.6, 0.05), legend.direction = "horizontal",
        legend.key.size = unit (1, "cm"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.ticks = element_line(color = "black"))+ # Add tick marks
  guides(shape = guide_legend(override.aes = list(size = 4))) # Increase symbol size in legend)

dev.off()


#Plot and save NZ inset map 

#import csv file with subset of data 
samples_NZ <-read.csv("Your_filename_here.csv")

# Get world map data
world <- ne_countries(scale = 50, returnclass = "sf") #returns world country polygons at a specified scale, or points of tiny-countries, sf = simple features 

# Adjust world longitudes for Pacific-centered map
#crs = coordinate reference system, needs to be specified for each map due to different focuses, and to ensure scales are accurate 
world <- st_transform(world, crs = "+proj=longlat +datum=WGS84 +lon_wrap=180")

png(filename = "NZ_samples_map_test.png", width = 35, height = 25, units = 'cm', res = 400) #save a plot

mapNZ <- ggplot(data = world) +
  geom_sf(fill = "gray70", color = "gray70") + # Set background to solid gray with no lines
  geom_point(data = samples_NZ, aes(x = Long, y = Lat, shape = Type, fill = Group), size = 8, color = "black", stroke = 1) + # Filled points with black outline
  scale_fill_manual(values = c("NZ_S_Mixed" = "blue" , "NZ_S_Rekohu" = "#004151", "NZ_S_Mahia" = "#00ccff", "NZ_B_1_early" = "#8FD744", "NZ_B_1_late" = "#00ff66","NZ_B_2" = "#006600"), guide = "none")+  #specify colours and remove legend for locations 
  scale_shape_manual(labels = c("Biopsy", "Stranding"), values = c(21, 24), guide = "none") +
  theme_minimal() + # # Change theme to allow axis lines and tick marks
  annotation_north_arrow(height = unit (2, "cm"), width = unit (2, "cm"),location = "tr", which_north = "true", style = north_arrow_orienteering(line_width = 0.5, text_size= 14)) +
  annotation_scale(location = "bl", line_width = 2,
                   height = unit(0.5, "cm"),
                   pad_x = unit(0.4, "cm"),
                   pad_y = unit(0.3, "cm"), text_cex = 2) + #scale to bottom left 
  labs(title = "", x = "", y = "") +
  coord_sf(xlim = c(160, 180), ylim = c(-50, -33), expand = FALSE)+
  scale_x_continuous(breaks = seq(160, 200, by = 10)) + # Custom longitude breaks
  scale_y_continuous(breaks = seq(-55, -30, by = 10))+ # Custom latitude breaks
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth =2),
        axis.ticks = element_line(color = "black"),
        axis.text.y = element_text(color = "black", size = 18),
        axis.text.x = element_text(color = "black", size = 18))

dev.off()

#maps are further edited in PowerPoint
