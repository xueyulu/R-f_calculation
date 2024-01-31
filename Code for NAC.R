rm(list=ls())
library(ggplot2)
library(maps)
getwd()
setwd("D:/0_code_R_/HYSPLIT")
trajectory_file <- read.csv("NAC.csv", header=TRUE, sep=",")
file_lat <- trajectory_file$Lat
file_lon <- trajectory_file$Long

# Create a 2D lat/lon grid
density <- matrix(0, nrow=180, ncol=720)

# Create a 1D array grid for lat and lon
latitude <- seq(-90, -0.5, by=0.5)
longitude <- seq(-180, 179.5, by=0.5)

for (i in 1:nrow(trajectory_file)) {
  traj_lat <- file_lat[i]
  traj_lon <- file_lon[i]
  
  lat_index <- which.min(abs(latitude - traj_lat))
  lon_index <- which.min(abs(longitude - traj_lon))
  
  density[lat_index, lon_index] <- density[lat_index, lon_index] + 1
}

density <- density/sum(density)*100
sum(density)
max(density)
m <- max(density)
contour(longitude, latitude, t(density),levels=c(0.05,0.1,0.25,0.5,1,2,4), xlim=c(150,175), ylim=c(-82,-70))

# Create a data frame for the density data
density_df <- expand.grid(latitude = latitude, longitude = longitude)
density_df$density <- as.vector(density)

p1 <-  ggplot(density_df, aes(x=longitude, y=latitude, fill=density)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "yellow", "orange", "red", "mediumpurple","purple3"), 
                       values = c(0, 0.15/m,0.5/m,1/m,1.5/m, 1)) +
  labs(x=NULL, y=NULL,fill = "Frequency(%)",
       title = "                                      HYSPLIT of NAC Polynya") +
  coord_cartesian(xlim = c(150, 175), ylim = c(-82, -70)) +
  theme_minimal() +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
               color = "black", size= 1.1, fill = NA) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 3)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(size = 14, face ="bold"), 
        axis.text.y = element_text(size = 14, face ="bold")) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = c(150,155,160,165,170), expand = c(0, 0), 
                     labels = c('150¡ãE', '155¡ãE', '160¡ãE','165¡ãE','170¡ãE')) +
  scale_y_continuous(breaks = c(-80,-78,-76,-74,-72,-70), expand = c(0, 0), 
                     labels = c('80¡ãS','78¡ãS','76¡ãS','74¡ãS','72¡ãS','70¡ãS')) +
  geom_point(x=164.12, y=-74.73, color="black", size=5, shape=21, fill="black")
p1

ggsave("NAC2.pdf", plot = p1, device = "pdf", width = 16, height = 22, 
       units = "cm", dpi = 600, family = "Times")
