#this code is to produce a biplot of carbon and nitrogen isotopes 
#the samples have been divided into 'sample partitions' for the analysis
#this is using the QC-ed isotopic dataset 
#carbon values have been Suess-corrected 

#set working directory
setwd("file/location/here")

#load libraries
library(ggpubr)
library(ggplot2)
library(ggExtra)
library(ggsci)

#import isotopic data
biplot_allSC <- read.csv("Filename_here.csv")

#define levels of groups 
biplot_allSC$Group <- factor(biplot_allSC$Group, levels = c("AUS_B", "FIS_S", "NZ_S_Mixed", "NZ_S_Rēkohu", "NZ_S_Māhia", "NZ_B_1_early", "NZ_B_1_late",
                                                      "NZ_B_2"))

#plot and save biplot
png("Figure_name_here", units = "in", width = 10, height = 8, res = 300 )

biplot_allSC_new <- ggplot(data = biplot_allSC)+
  geom_point(mapping = aes(x = d_13C, 
                           y = d_15N, 
                           color = Group,
                           shape = Sex,
                           size = 0.05), position = "jitter")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  theme(legend.background = element_rect(fill="white", 
                                         linewidth = 0.2, linetype="solid"))+
  scale_color_manual(name = "Sample Partition", labels = c( "AUS_B", "FIS_S", "NZ_S_Mixed", "NZ_S_Rēkohu", "NZ_S_Māhia", "NZ_B_1_Early", "NZ_B_1_Late",
                                                            "NZ_B_2"), 
                     values = c( "#cc0044","#F0E442","#0072B2","#004151","#00ccff","#8FD744FF","#00ff66","#006600"))+ # Customise colors here
  theme_minimal() +
  coord_cartesian(xlim =c(-19, -13), ylim = c(10, 18))+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.7))+ #adjust legend position as needed
  theme(axis.text = element_text(size=15))+
  theme(axis.title = element_text(size=15))+
  scale_size(guide="none")+
  theme(legend.text = element_text(size=8))+
  guides(color=guide_legend(override.aes = list(size=3.5)))+
  guides(shape=guide_legend(override.aes = list(size=3.5)))+
  scale_shape_discrete(name = "Sex", labels=c("Female","Male"))+  #specify shapes for male and female samples 
  theme(legend.text = element_text(size = 11))+
  theme(legend.title = element_text(size = 12))

dev.off()
