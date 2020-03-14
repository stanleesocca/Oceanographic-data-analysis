
# Access, Download, Process and VIsualize sea surface height and geostrophic current from AVISO in R
# Load library
require(xtractomatic)
require(lubridate)
require(tidyverse)
# check for the data available
daily.dataset = searchData("datasetname:1day")
daily.dataset %>% glimpse()

# coordinate close to the Gulf of Guinea
xpos <- c(8.979167, 0.02083079)
ypos <- c(-8.979167, 8.979177)
# time for which we want the data present
time.lim <- c("2010-12-20", "2010-12-31")

# download and extract the zonal component of data with xtracto_3D 
u <- xtracto_3D(dtype = "erdTAugeo1day", xpos = xpos, ypos = ypos, tpos = time.lim)
u %>% glimpse()

# Extract the longitude, Latitude and Time
lon = u$longitude
lat = u$latitude
time = u$time %>% as.Date()
# Extract the current (zonal flow)
u.current <- u$data
# check the dimension
dim(u.current)


# First get the dimension of dataframe that takes into consideration the coordinate and the flow data
dimension = data.frame(lon, u.current[,,1]) %>% dim()

# convert array into data frame and combine the respective long, lat and time data
u.current.tb <- data.frame(lon, u.current[,,1]) %>% 
  as_tibble() %>% 
  gather(key = "latitude", value = "u", 2:dimension[2]) %>% 
  mutate(lat = rep(lat, each = dimension[1]), 
         time = time[1]) %>% 
  select(lon, lat, time, u)
view(u.current.tb)

# Repeat the same operation for the meriodional component
# get the  meriodional component of the data
v <- xtracto_3D(dtype = "erdTAvgeo1day", xpos = xpos, ypos = ypos, tpos = time.lim)
v %>% glimpse()

lon = v$longitude
lat = v$latitude
time = v$time %>% as.Date()
v.current <- v$data
dim(v.current)

# convert array into data frame and combine the respective long, lat and time data
v.current.tb <- data.frame(lon, v.current[,,1]) %>% 
  as_tibble() %>% 
  gather(key = "latitude", value = "v", 2:dimension[2]) %>% 
  mutate(lat = rep(lat, each = dimension[1]), 
         time = time[1]) %>% 
  select(lon, lat, time, v)
#view(v.current.tb)

# Repeat the same for the SEA_SURFACE HEIGHT
# Now get the sea surface height
ssh <- xtracto_3D(dtype = "erdTAssh1day", xpos = xpos, ypos = ypos, tpos = time.lim)

lon = ssh$longitude
lat = ssh$latitude
time = ssh$time %>% as.Date()
ssh.data <- ssh$data

ssh.tb <- data.frame(lon, ssh.data[,,1]) %>% 
  as_tibble() %>% 
  gather(key = "latitude", value = "ssh", 2:dimension[2]) %>% 
  mutate(lat = rep(lat, each = dimension[1]), 
         time = time[1]) %>% 
  select(lon, lat, time, ssh)
#view(v.current.tb)


# INTERPOLATION
# intepolate the data in sea surface height
ssh.in <- oce::interpBarnes(x = ssh.tb$lon, y = ssh.tb$lat, z = ssh.tb$ssh)
dimension = data.frame(lon = ssh.in$xg, ssh.in$zg) %>% dim()

ssh.in <- data.frame(lon = ssh.in$xg, ssh.in$zg) %>% 
          as_tibble() %>% 
          gather(key = "latitude", value = "ssh", 2:dimension[2]) %>% 
          mutate(lat = rep(ssh.in$yg, each = dimension[1]), time = time[1]) %>% 
          select(lon, lat, time, ssh)
# BIND ALL DATA TOGETHER
aviso <- ssh.tb %>% 
  bind_cols(current.tb %>% select(u), 
            v.current.tb %>% select(v))

# VISUALIZING THE DATA
contour_plot <- ggplot() + 
  metR::geom_contour_fill(data = ssh.in, aes(x = lon, y = lat, z = ssh)) + 
  metR::geom_contour2(data = ssh.in, aes(x = lon, y = lat, z = ssh)) +
  metR::geom_text_contour(data = ssh.in, aes(x = lon, y = lat, z = ssh), 
                          parse = TRUE, check_overlap = TRUE, size = 3.2) + 
  geom_sf(data = spData::world, fill = "grey60", col = "grey20") +
  coord_sf(xlim = xpos, ylim = ypos) + 
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  scale_fill_gradientn(name = "ssh (m)", colours = oce::oceColors9A(120), na.value = "white") +
  scale_x_continuous(breaks = seq(8.9, 0.02, length.out = 4) %>% round(1)) +
  guides(fill = guide_colorbar(title = "Sea surface height (m)", 
                               title.position = "right", title.theme = element_text(angle = 90), 
                               barwidth = 1.25, barheight = 10, draw.ulim = 1.2, draw.llim = 0.6))+
  theme_bw()+
  theme(axis.text = element_text(colour = 1, size = 11))

# CURRENT SPEED SUPER-IMPOSED ON THE MAP

current_plot <- ggplot() + 
  metR::geom_contour_fill(data = ssh.in, aes(x = lon, y = lat, z = ssh)) + 
  metR::geom_vector(data = aviso, aes(x = lon, y = lat, dx = u, dy = v), 
                    arrow.angle = 25, arrow.length = .4, arrow.type = "open") +
  metR::scale_mag(max = .75, name = "Speed", labels = ".75 m/s")+
  geom_sf(data = spData::world, fill = "grey60", col = "grey20") +
  coord_sf(xlim = xpos, ylim = ypos) + 
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  scale_fill_gradientn(name = "ssh (m)", colours = oce::oceColors9A(120), na.value = "white") +
  scale_x_continuous(breaks = seq(8.9, 0.02, length.out = 4) %>% round(1)) +
  guides(fill = guide_colorbar(title = "Sea surface height (m)", 
                               title.position = "right", title.theme = element_text(angle = 90), 
                               barwidth = 1.25, barheight = 10, draw.ulim = 1.2, draw.llim = 0.6))+
  theme_bw()+
  theme(axis.text = element_text(colour = 1, size = 11))

ggsave("contour_plot.png", contour_plot, width = 15, height = 15, units = "cm", dpi = 400)
ggsave("current_plot.png", current_plot, width = 15, height = 15, units = "cm", dpi = 400)
