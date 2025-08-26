#Figure 1. Multiyear Archival Fish
#Chloe Mikles
#April 12 2023

#Goal: create figure/ map showing transition tracks from immature -> mature fish (based on entry to Med)

#libraries
library(dplyr)
library(lunar)
library(rnaturalearth) #required for world
library(ggplot2)
library(ggOceanMaps) #required for basemap
library(ggspatial)


#fish IDs: 
med_fish<-c(5112003,
            5100135,
            5100205,
            5197031,
            5100143,
            5105027)

#import track data
tracks<- track %>% dplyr::filter(toppid %in%  med_fish) 
deploys<-deploy %>% dplyr::filter(toppid %in% med_fish)
class(tracks$datetime) #character

#Convert columns to datetime 
tracks$datetime <- as.POSIXct(tracks$datetime,format= "%m/%d/%Y %H:%M", tz='UTC')
tracks$toppid <- as.character(tracks$toppid)

#Add columns
tracks$date<-print(paste(format(as.POSIXct(tracks$datetime), format = "%Y/%m/%d")))
tracks$time<-print(paste(format(as.POSIXct(tracks$datetime), format = "%H:%M")))
tracks$year<-print(paste(format(as.POSIXct(tracks$datetime), format = "%Y")))
tracks$month<-print(paste(format(as.POSIXct(tracks$datetime), format = "%m")))
tracks$date<- as.Date(tracks$date)
tracks$Season<- terrestrial.season(tracks$date) #4 seasons

#Make month, year numeric. 
tracks$month<-as.numeric(tracks$month)
tracks$year<-as.numeric(tracks$year)

#Add row numbers
tracks <- tracks %>% group_by(toppid) %>% mutate(day_number = row_number())


#Create pre & post maturity dfs for each indiv based on unique first entry to Med
#5197031 (6th)
pre1<- tracks %>% filter(toppid==5197031 & date < "2000-05-31")
post1<- tracks %>% filter(toppid==5197031) %>% filter(date > "2000-05-31")
#majority of time pre-maturity, post only in Med  

#5100135 (1st)
pre2<- tracks %>% filter(toppid==5100135) %>% filter(date < "2001-06-10")
post2<- tracks %>% filter(toppid==5100135) %>% filter(date > "2001-06-10")

#5100143 (2nd)
pre3<- tracks %>% filter(toppid==5100143) %>% filter(date < "2001-05-01")
post3<- tracks %>% filter(toppid==5100143) %>% filter(date > "2001-05-01" & day_number<854)

#5100205 (3rd)
pre4<- tracks %>% filter(toppid==5100205) %>% filter(date < "1999-07-07")
post4<- tracks %>% filter(toppid==5100205) %>% filter(date > "1999-07-07")

#5105027 (4th)
pre5<- tracks %>% filter(toppid==5105027) %>% filter(date < "2006-05-09")
post5<- tracks %>% filter(toppid==5105027) %>% filter(date > "2006-05-09")

#5112003 (5th)
pre6<- tracks %>% filter(toppid==5112003) %>% filter(date < "2017-05-10")
post6<- tracks %>% filter(toppid==5112003) %>% filter(date > "2017-05-10")

#Join pre's and post's
df1<-rbind(pre1, pre2, pre3, pre4, pre5, pre6)
df2<-rbind(post1, post2, post3, post4, post5, post6)

#set up bathymetry
dt <- data.frame(lon = c(-85, -85, 25, 25), lat = c(15, 70, 70, 15))
#basemap(data = dt, bathymetry = TRUE) 
basemap(data = dt, bathymetry = TRUE, bathy.style = "rcb") 


buffer <- 5 # original
xmin <- min(tracks$lon) - buffer
xmax <- max(tracks$lon) + buffer
ymin <- min(tracks$lat) - buffer
ymax <- max(tracks$lat) + buffer

#Make base
world <- ne_countries(scale = "medium", returnclass = "sf")
base <- ggplot(data = world) + geom_sf() +
  coord_sf(xlim = c(-79, 20), ylim = c(20, 60), expand = FALSE) +
  theme_classic()
base

plotcolors <- c( "#fc8d59", "#fee08b", "#e6f598", "#99d594", "#de77ae", "#d53e4f")


#Map with base (draft figure 1a)
basemap(data = dt, bathymetry = TRUE) + 
  geom_sf(data = world, fill = "gray") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(data=pre1, aes(x=lon, y=lat), pch=21, size=1.75, fill=col1, color="#000000")+
  geom_point(data=post1, aes(x=lon, y=lat),pch=21, size=1.75, fill="#000000", color=col1, stroke=1)+
  geom_point(data=pre2, aes(x=lon, y=lat), pch=21, size=1.75, fill=col2, color="#000000")+
  geom_point(data=post2, aes(x=lon, y=lat),pch=21, size=1.75, fill="#000000", color=col2, stroke=1)+
  geom_point(data=pre3, aes(x=lon, y=lat), pch=21, size=1.75, fill=col3, color="#000000")+
  geom_point(data=post3, aes(x=lon, y=lat),pch=21, size=1.75, fill="#000000", color=col3, stroke=1)+
  geom_point(data=pre4, aes(x=lon, y=lat), pch=21, size=1.75, fill=col4, color="#000000")+
  geom_point(data=post4, aes(x=lon, y=lat),pch=21, size=1.75, fill="#000000", color=col4, stroke=1)+
  geom_point(data=pre5, aes(x=lon, y=lat), pch=21, size=1.75, fill=col5, color="#000000")+
  geom_point(data=post5, aes(x=lon, y=lat),pch=21, size=1.75, fill="#000000", color=col5, stroke=1)+
  geom_point(data=pre6, aes(x=lon, y=lat), pch=21, size=1.75, fill=col6, color="#000000")+
  geom_point(data=post6, aes(x=lon, y=lat),pch=21, size=1.75, fill="#000000", color=col6, stroke=1)+
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  ggtitle("Individual tracks; pre- and post-first entry to Med")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black",fill=NA, size=2)) 
 
col1="#d53e4f"
col2="#fc8d59"
col3="#fee08b"
col4="#e6f598"
col5="#99d594"
col6="#de77ae"


plotcolors <- c( "#fc8d59", "#fee08b", "#e6f598", "#99d594", "#de77ae", "#d53e4f")


#pre
map<-basemap(data = dt, bathymetry = TRUE, bathy.style="rcb") + 
  geom_sf(data = world, fill = "gray") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(data=pre6, aes(x=lon, y=lat), pch=21, size=1.5, fill="#fff7fb", color="#000000")+
  geom_point(data=pre1, aes(x=lon, y=lat), pch=21, size=1.5, fill="#df9ed4", color="#000000")+
  geom_point(data=pre2, aes(x=lon, y=lat), pch=21, size=1.5, fill="#c93f55", color="#000000")+
  geom_point(data=pre3, aes(x=lon, y=lat), pch=21, size=1.5, fill="#eacc62", color="#000000")+
  geom_point(data=pre4, aes(x=lon, y=lat), pch=21, size=1.5, fill="#469d76", color="#000000")+
  geom_point(data=pre5, aes(x=lon, y=lat), pch=21, size=1.5, fill="#924099", color="#000000")+
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  #scale_x_continuous(breaks=c(-70, -45,-20,5), limits = c(-80,15))+
  theme_bw()+
  annotation_scale(location = "bl", width_hint= 0.2, style= "bar", text_cex = 1, height= unit(0.2, "cm") ) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.75, "cm"),
                         width = unit(0.75, "cm"))+
  theme(axis.title = element_blank(),
        axis.text=element_text(color="black", face="bold", size=12),
        panel.border = element_rect(colour = "black",fill=NA, size=2))
map

ggsave("Figure1a_premat_Klimt_Colors.png", dpi=600)

map + 
  new_scale("fill") +
  geom_point(data = pre6, aes(x = lon, y = lat, fill = "pre1"), pch = 21, size = 1.75, color = "#000000") +
  geom_point(data = pre1, aes(x = lon, y = lat, fill = "pre2"), pch = 21, size = 1.75, color = "#000000") +
  geom_point(data = pre2, aes(x = lon, y = lat, fill = "pre3"), pch = 21, size = 1.75, color = "#000000") +
  geom_point(data = pre3, aes(x = lon, y = lat, fill = "pre4"), pch = 21, size = 1.75, color = "#000000") +
  geom_point(data = pre4, aes(x = lon, y = lat, fill = "pre5"), pch = 21, size = 1.75, color = "#000000") +
  geom_point(data = pre5, aes(x = lon, y = lat, fill = "pre6"), pch = 21, size = 1.75, color = "#000000") +
  scale_fill_manual(values = c("#fff7fb","#df9ed4", "#c93f55", "#eacc62","#469d76", "#924099"),
                    name = "Fish ID",
                    labels = c("5112003", "5197031", "5100135", "5100143", "5100205", "5105027"))+
  theme_void() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.key = element_rect(fill = "lightgrey", color = NA)        # Matches key background
  )

ggsave("Figure1a_legend_Klimt_Colors.png", dpi=600)


#post
map2<-basemap(data = dt, bathymetry = TRUE, bathy.style="rcb") + 
  geom_sf(data = world, fill = "gray") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(data=post6, aes(x=lon, y=lat),pch=21, size=1.5, fill="#fff7fb", color="#000000")+
  geom_point(data=post1, aes(x=lon, y=lat),pch=21, size=1.5, fill="#df9ed4", color="#000000")+
  geom_point(data=post2, aes(x=lon, y=lat),pch=21, size=1.5, fill="#c93f55", color="#000000")+
  geom_point(data=post4, aes(x=lon, y=lat),pch=21, size=1.5, fill="#eacc62", color="#000000")+
  geom_point(data=post5, aes(x=lon, y=lat),pch=21, size=1.5, fill="#469d76", color="#000000")+
  geom_point(data=post3, aes(x=lon, y=lat),pch=21, size=1.5, fill="#924099", color="#000000")+
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  #ggtitle("Individual tracks; post-maturity")+
  theme_bw()+
  annotation_scale(location = "bl", width_hint= 0.2, style= "bar", text_cex = 1, height= unit(0.2, "cm") ) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.75, "cm"),
                         width = unit(0.75, "cm"))+
  theme(axis.title = element_blank(),
        axis.text=element_text(color="black", face="bold", size=12),
        panel.border = element_rect(colour = "black",fill=NA, size=2))
map2

ggsave("Figure1b_postmat_Klimt_Colors.png", dpi=600)


map2+new_scale("fill") +
  geom_point(data = post1, aes(x = lon, y = lat, fill = "post1"), pch = 21, size = 1.5, color = "#000000") +
  geom_point(data = post2, aes(x = lon, y = lat, fill = "post2"), pch = 21, size = 1.5, color = "#000000") +
  geom_point(data = post3, aes(x = lon, y = lat, fill = "post3"), pch = 21, size = 1.5, color = "#000000") +
  geom_point(data = post4, aes(x = lon, y = lat, fill = "post4"), pch = 21, size = 1.5, color = "#000000") +
  geom_point(data = post5, aes(x = lon, y = lat, fill = "post5"), pch = 21, size = 1.5, color = "#000000") +
  geom_point(data = post6, aes(x = lon, y = lat, fill = "post6"), pch = 21, size = 1.5, color = "#000000") +
  scale_fill_manual(values = c("#1b9e77","#66a61e", "#7570b3", "#e7298a", "#e6ab02","#d95f02"),
                    name = "Fish ID",
                    labels = c("5197031", "5100135", "5100143", "5100205", "5105027", "5112003"))

ggsave("figure_with_legend.png")

#figure 1b draft
basemap(data = dt, bathymetry = TRUE) + 
  geom_sf(data = world, fill = "gray") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(data=df1, aes(x=lon, y=lat), pch=21, size=2, fill="#f7f7f7", color="#000000")+
  geom_point(data=df2, aes(x=lon, y=lat),pch=21, size=2.2, fill="#000000", color="#f7f7f7", stroke=0.2)+
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  ggtitle("Combined")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black",fill=NA, size=2)) 


#pre
basemap(data = dt, bathymetry = TRUE) + 
  geom_sf(data = world, fill = "gray") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(data=df1, aes(x=lon, y=lat), pch=21, size=2, fill="#f7f7f7", color="#000000")+
  #geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  ggtitle("Combined, pre-maturity")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black",fill=NA, size=2)) 

#post
basemap(data = dt, bathymetry = TRUE) + 
  geom_sf(data = world, fill = "gray") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(data=df2, aes(x=lon, y=lat),pch=21, size=2.2, fill="#000000", color="#f7f7f7", stroke=0.2)+
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  ggtitle("Combined, post-maturity")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black",fill=NA, size=2)) 


#figure 1c draft


#plot lon hist
ggplot(data=tracks, aes(x=lon, fill= toppid)) +
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values=c("#fc8d59", "#fee08b", "#e6f598", "#99d594", "#3288bd", "#d53e4f"))+
  scale_y_reverse()+
  xlab("Longitude")+
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  theme_classic()

#plot lon hist - pre
ggplot(data=df1, aes(x=lon, fill= toppid)) +
  xlim(-80, 20)+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values=c("#fc8d59", "#fee08b", "#e6f598", "#99d594", "#3288bd", "#d53e4f"))+
  scale_y_reverse()+
  xlab("Longitude")+ 
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  theme_classic()
#plot lon hist - post
ggplot(data=df2, aes(x=lon, fill= toppid)) +
  geom_histogram(binwidth = 1)+
  xlim(-80, 20)+
  scale_fill_manual(values=c("#fc8d59", "#fee08b", "#e6f598", "#99d594", "#3288bd", "#d53e4f"))+
  scale_y_reverse()+
  xlab("Longitude")+ 
  geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  theme_classic()


#plot lat hist
ggplot(data=tracks, aes(y=lat, fill = toppid)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values=c("#fc8d59", "#fee08b", "#e6f598", "#99d594", "#3288bd", "#d53e4f"))+
  ylab("Latitude") +
  xlab("Number of days")+
  theme_classic()
#pre lat hist
ggplot(data=df1, aes(y=lat, fill = toppid)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values=c("#fc8d59", "#fee08b", "#e6f598", "#99d594", "#3288bd", "#d53e4f"))+
  ylab("Latitude") +
  xlab("Number of days")+
  ylim(15,65)+
  theme_classic()
#post lat hist
ggplot(data=df2, aes(y=lat, fill = toppid)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values=c("#fc8d59", "#fee08b", "#e6f598", "#99d594", "#3288bd", "#d53e4f"))+
  ylab("Latitude") +
  xlab("Number of days")+
  ylim(20,70)+
  theme_classic()

#just pre-post combined: 
#lat
ggplot(data=df1, aes(y=lat)) +
  geom_histogram(binwidth = 1,fill="#f7f7f7", color="#000000") +
  ylab("Latitude") +
  xlab("Number of days")+
  ylim(20,80)+
  theme_classic()
#post lat hist
ggplot(aes(y=lat, fill = toppid)) +
  geom_histogram(data=df2, binwidth = 1, fill="#000000") +
  ylab("Latitude") +
  xlab("Number of days")+
  ylim(20,60)+
  theme_classic()

#Alternate fig 1c. 
#Re-order layers so autumn is not on top
df2$Season <- factor(df2$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))

#Seasonal combined latitudes. 
ggplot(data=df2, aes(x=lat, fill = Season)) +
  #geom_histogram(binwidth = 1) +
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("#ffb3ba","#ff7f50", "#bae1ff", "#96ceb4" ))+
  xlab("Latitude") +
  ylab("Number of days")+
  scale_x_continuous(breaks=seq(0,70,10))+
  scale_y_continuous(breaks=seq(0,500,100))+
  coord_flip()+
  theme_void()



ggplot(data=df2, aes(x=lon, fill= Season)) +
  #geom_histogram(binwidth = 1)+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("#ffb3ba","#ff7f50", "#bae1ff", "#96ceb4" ))+
  xlab("")+ 
  ylab("")+
  xlim(-85,20)+
  #coord_sf(xlim = c(xmin, xmax)) + #ylim = c(ymin, ymax)) +
  scale_x_continuous(breaks=seq(-85,20,5),limits=c(-85,20))+
  #scale_y_continuous(breaks=seq(0,300,100), limits=c(0,300))+
  #scale_y_reverse(limits=c(300,0))+
  scale_y_reverse()+
  #geom_vline(xintercept = -45, linetype="dashed", size=1.5)+
  theme_void()+
    theme(axis.title = element_blank(),
    panel.grid = element_blank(),
    #panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_line(color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank())
    
