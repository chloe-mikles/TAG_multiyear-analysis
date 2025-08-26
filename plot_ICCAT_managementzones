#ICCAT map code
#Chloe Mikles 
#10.18.23

setwd("~/Desktop/Stanford/Block Lab/ABFT/Multiyear Archival Tracks/Figure 3. Management boxes/")

#Load libraries


#run line 37 in emil's code
ssm$region = assignRegion(ssm$lat, ssm$lon)

#Load ICCAT polygons, Run line 39 & 55 in ICCAT_regions.R

#Manually set limits to match ICCAT boxes
xmin<--100
xmax<-38
ymin<--10.26154
ymax<-75


#Plot blank map w/ ICCAT regions 
ggplot()+
  geom_sf(data = world, fill = "gray") +
  geom_polygon(data=polygonD, aes(x=long, y=lat, group=id), color="black", fill=NA) +
  coord_sf(xlim = c(xmin, xmax ), ylim = c(ymin, ymax)) +
  theme_void() +
  theme( panel.border=element_rect(color="black", fill=NA, size=1) )  #element_blank()") + # omit plot title 

ggsave("blank ICCAT map.jpg", dpi=600)



#Make base
world <- ne_countries(scale = "medium", returnclass = "sf")
base <- ggplot(data = world) + geom_sf() +
  coord_sf(xlim = c(-79, 20), ylim = c(20, 60), expand = FALSE) +
  theme_classic()
base
#set up bathymetry
dt <- data.frame(lon = c(-85, -85, 25, 25), lat = c(15, 70, 70, 15))
basemap(data = dt, bathymetry = TRUE) 

#change limits
buffer <- 5 # original
xmin <- min(tracks$lon) - buffer
xmax <- max(tracks$lon) + buffer
ymin <- min(tracks$lat) - buffer
ymax <- max(tracks$lat) + buffer


#figure 1b draft
basemap(data = dt, bathymetry = TRUE) + 
  geom_sf(data = world, fill = "gray") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(data=df1, aes(x=lon, y=lat),pch=21, size=2.2, fill="#000000", color="#f7f7f7", stroke=0.2)+
  geom_point(data=df2, aes(x=lon, y=lat), pch=21, size=2, fill="#f7f7f7", color="#000000")+
  geom_polygon(data=polygonD, aes(x=long, y=lat, group=id), color="black", fill=NA) +
  #geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  #ggtitle("Combined")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black",fill=NA, size=2)) 

ggsave()

#base+
basemap(data = dt, bathymetry = TRUE) + 
  geom_polygon(data=polygonD, aes(x=long, y=lat, group=id), color="black", fill=NA) +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle(label = "5112003" )+
  # , subtitle = "subtitle") +
  geom_point(data=ssm,
             aes(x =lon, y =lat, 
                 fill=factor(year)), #specify the data
             shape = 21, size = 2.0) +
  #scale_fill_discrete(type=plotcolors, name="Year")+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14, face="bold"),
        title=element_text(size=18, face="bold"))

