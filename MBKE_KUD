#KUD 

#Movement-based kernel estimation by individual 

#This code will 1).run the KUD
#               2).plot the KUD, 
#               3).calculate overlap using BA. 

#Load libraries
library(dplyr)
library(lunar)
library(lubridate)
library(rnaturalearth) #required for world
library(ggplot2)
library(ggOceanMaps) #required for basemap
library(raster)
library(adehabitatHR)
library(sp)
library(maptools) # required for exporting home range to GIS shapefile
library(geosphere) # required for areaPolygon()
library(rgdal) # required for projection code
library(rgeos)
library(raster)
library(sf)

#Formatting ####

#Run code in "Load and format SSM.R" 

#Working with dataframes of SSM tracks 
df1$id<-paste("pre")
df2$id<-paste("post")

#Combine dfs
c.df<-rbind(df1,df2)

#calculate hmin from 6 fish combined (very similar # per individual so kept it with all)
c.df$elat <- c.df$lat975 - c.df$lat025
c.df$elon <- c.df$lon975 - c.df$lon025
c.df$ei <- sqrt((c.df$elat)^2 + (c.df$elon)^2)
h=sd(c.df$ei)

#Pick one fish 
fish<- c.df %>% filter(toppid==5112003)


#Setup and run KUD ####

#Calculate overlap ####
fish$xy<- cbind(fish$lon, fish$lat)

#Create ltraj object
tr <- as.ltraj(fish$xy, fish$datetime, id=fish$id) 

#basic plot
plot(tr, spixdf=c.df$habitat)

#determine extent -a value indicating the extent of the grid used for the estimation
buffer <- 15 # original
#buffer <- 20 # works for 95% (use 20 for year 6)
xmin <- min(c.df$lon) - buffer
xmax <- max(c.df$lon) + buffer
ymin <- min(c.df$lat) - buffer
ymax <- max(c.df$lat) + buffer
# x <- seq(xmin, xmax, by = .01) # where resolution is the pixel size you want
# y <- seq(ymin, ymax, by = .01)
# byres = 0.01 # original
byres = 0.5 # original

x <- seq(xmin, xmax, by = byres) # where resolution is the pixel size you want
y <- seq(ymin, ymax, by = byres)
xy <- expand.grid(x = x, y = y)
coordinates(xy) <- ~ x + y # takes a while
gridded(xy) <- TRUE

#estimate diffusion coef. 
vv <- BRB.D(tr,Tmax=24*60*60*3,Lmin=0)

#Use BRB to estimate the UD from locations
kud <- BRB(tr,D = vv,Tmax = 24*60*60*3,tau = 24*60*60,Lmin = 0, hmin = h, grid = xy,b = 0,same4all = FALSE,extent = 1)

image(kud)
vud <- getvolumeUD(kud)

#Calculate Bhattacharyya's coef. 
ba<- kerneloverlaphr(kud, method = c("BA"),
                     percent = 95, conditional = FALSE)

ba


#Heat map KUD plot, loop ####

#have to create separate kud object for pre-post each year, so create unique ID for pre/post for each fish. 
c.df$id_mat <- paste(c.df$toppid, c.df$id, sep=" ")
c.df$xy <- cbind(c.df$lon, c.df$lat)

setwd("~/Desktop/Stanford/Block Lab/ABFT/Multiyear Archival Tracks/KUDs/KUD MBKE/Bhattacharyya/Maturity/")


# List of Individual IDs
vector <- (unique(c.df$id_mat))
#troubleshooting
#vector.ctd <- vector[46:63] 
#vector.ctd<- vector.ctd[7:18]

#Create list to loop through 
list <- as.list(vector)

#for loop
for (i in 1:length(list)){ 
  id = list[i] #iterate through each trip in the list 
  print(id)
  fish_id <- c.df[which(c.df$id_mat == id),] #read in only one fish, one year
  #
  buffer <- 30 #works for 95%
  xmin <- min(c.df$lon) - buffer
  xmax <- max(c.df$lon) + buffer
  ymin <- min(c.df$lat) - buffer
  ymax <- max(c.df$lat) + buffer
  # x <- seq(xmin, xmax, by = .01) # where resolution is the pixel size you want
  # y <- seq(ymin, ymax, by = .01)
  # byres = 0.01 # original
  byres = 0.5 
  x <- seq(xmin, xmax, by = byres) # where resolution is the pixel size you want
  y <- seq(ymin, ymax, by = byres)
  xy <- expand.grid(x = x, y = y)
  coordinates(xy) <- ~ x + y # takes a while
  gridded(xy) <- TRUE
  
  ### Calculate MBKE KUD for each trip 
  tr <- as.ltraj(fish_id$xy, fish_id$datetime, id=fish_id$id_mat)
  vv <- BRB.D(tr,Tmax=24*60*60*3,Lmin=0)
  kud1 <- BRB(tr,D = vv,Tmax = 24*60*60*3,tau = 24*60*60,Lmin = 0, hmin = h, grid = xy,b = 0,same4all = FALSE,extent = 1)
  image(kud1) 
  vud<-getvolumeUD(kud1)
  corearea <- getverticeshr(kud1, percent = 50)
  homerange<-getverticeshr(kud1, percent = 95)
  basename <- "~/Desktop/Stanford/Block Lab/ABFT/Multiyear Archival Tracks/KUDs/KUD MBKE/Bhattacharyya/Maturity/"
  name <- paste(id, sep = "") #name output by fishID
  
  #Set up heatmap raster
  kud_raster <- raster(as(vud, "SpatialPixelsDataFrame"))
  kud_raster_df<- as.data.frame(kud_raster, xy = TRUE, na.rm = TRUE)
  kud_raster_NA <- kud_raster_df %>% filter (n <95)
  
  
  #Plot 
  ggplot(data = world) +
    theme_linedraw(base_line_size = NA)+
    geom_raster(data=kud_raster_NA, aes(x=x, y=y, fill=n))+
    scale_fill_viridis_c(option='magma', breaks=c(50,95), name="Utilization Distribution", labels=c("50%","95%"), limits=c(0,100))+
    labs(c(0, 50, 95))+
    geom_sf()+
    coord_sf(xlim = c(xmin,xmax), ylim = c(ymin, ymax), expand = FALSE)+
    labs(x="Longitude", y="Latitude")+
    theme(legend.position = "bottom")+
    ggtitle(name)
  
  
  #save plot
  ggsave(paste0(today(), "_KUD_", id, ".png"),
         plot = last_plot(),
         device = "png",
         path = "~/Desktop/Stanford/Block Lab/ABFT/Multiyear Archival Tracks/KUDs/KUD MBKE/Bhattacharyya/Maturity/",
         scale = 1.5, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 6.32, #NA default. Manually adjust plot box in RStudio after ggplot()
         height = 4, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
         dpi = 300,
         limitsize = TRUE)
  
  #calculate area
  area<- kernel.area(kud1, percent=c(50,95), 
                     unin = c("km"), 
                     unout = c("km2")) 
  area.df<-as.data.frame(area) 
  name = paste("ID", id, sep = "") #name output by fish ID & year
  write.csv(area.df, name) #write table to csv output
  
}


#Combine areas from separate table 
tbl<- read.csv("pre-post_areas.csv", header=T)
mean(tbl$pre50)
mean(tbl$post50)

mean(tbl$pre95)
mean(tbl$post95)

kruskal.test(tbl$pre50, tbl$post50)
kruskal.test(tbl$pre95, tbl$post95)


#Groupings for KW test
fifty<-read.csv("KUD50areas.csv", header=T)
ninety<-read.csv("KUD95areas.csv", header=T)


kruskal.test(KUD50 ~ group, data = fifty)
#Kruskal-Wallis chi-squared = 1.6436, df = 2, p-value = 0.4396

kruskal.test(KUD95 ~ group, data = ninety) #significant 
#Kruskal-Wallis chi-squared = 5.1513, df = 2, p-value = 0.07611

setwd("~/Desktop/Stanford/Block Lab/ABFT/Multiyear Archival Tracks/KUDs/KUD MBKE/Bhattacharyya/Maturity/")
#Plot areas boxplot 
areas1<-read.csv("kud_prepost_comb.csv", header=T)
areas1$Year <- as.character(areas1$Year)
areas1$KUD95<-as.numeric(areas1$KUD95)

areas1$Year <- factor(areas1$Year,  # Change ordering manually
                     levels = c("pre", "post"))

areas1

plotcolors<- c("#FD9567FF", "#9F2F7FFF")

ggplot(data=areas1,  aes(x=Year, y=KUD50)) +
  geom_boxplot(fill=plotcolors)+
  #scale_y_continuous(limits = c(0,2000))+
  #stat_summary(fun = mean, geom="point", shape=23, size=2)+
  labs(x="", y="50% KUD")+
  theme_classic() +
  #theme(element_text())
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave("KUD_areas boxplot_50% KUD_MBKE.png", dpi=600)


ggplot(data=areas1,  aes(x=Year, y=KUD95)) +
  geom_boxplot(fill=plotcolors)+
  #stat_summary(fun = mean, geom="point", shape=23, size=2)+
  labs(x="", y="95% KUD")+
  theme_classic() +
  #theme(element_text())
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave("KUD_areas boxplot_95% KUD_MBKE.png", dpi=600)





