rm(list=ls())
library(ggplot2)
library(maps)
getwd()
setwd("D:/0_code_R_/HYSPLIT")
trajectory_file <- read.csv("GWS.csv", header=TRUE, sep=",")
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
contour(longitude, latitude, t(density), levels=c(0.1,0.25,0.5,1,1.5), xlim=c(-70,-50), ylim=c(-70,-58))

# Create a data frame for the density data
density_df <- expand.grid(latitude = latitude, longitude = longitude)
density_df$density <- as.vector(density)

p1 <-  ggplot(density_df, aes(x=longitude, y=latitude, fill=density)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "yellow", "orange", "red", "mediumpurple","purple","purple4"), 
                       values = c(0, 0.1/m,0.25/m,0.5/m,1/m,1.5/m, 1)) +
  labs(x=NULL, y=NULL,fill = "Density(%)",
       title = "                                      HYSPLIT of King George Island") +
  coord_cartesian(xlim = c(-75,-50), ylim = c(-70, -58)) +
  theme_minimal() +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
               color = "black", size= 1.1, fill = NA) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 3)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.key.height = unit(4, "cm"), legend.key.width = unit(1, "cm"),
        axis.text.x = element_text(size = 14, face ="bold"), 
        axis.text.y = element_text(size = 14, face ="bold")) +
  theme(legend.text=element_text(size=12, face= "bold"),
        legend.title=element_text(size=14, face= "bold"),
        legend.position="none") +
  scale_x_continuous(breaks = c(-75,-70, -65,-60,-55), expand = c(0, 0), 
                     labels = c('75¡ãW','70¡ãW','65¡ãW', '60¡ãW','55¡ãW')) +
  scale_y_continuous(breaks = c(-68,-66,-64,-62,-60,-58), expand = c(0, 0), 
                     labels = c('68¡ãS','66¡ãS','64¡ãS','62¡ãS','60¡ãS','58¡ãS')) +
  geom_point(x=-58.96, y=-62.22, color="black", size=5, shape=21, fill="black")
p1

ggsave("GWS3.pdf", plot = p1, device = "pdf", width = 16, height = 22, 
       units = "cm", dpi = 600, family = "Times")
