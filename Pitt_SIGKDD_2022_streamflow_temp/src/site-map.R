# "A spatial map with stream-network and reservoir location will be helpful",
# which is on the dataset description. In the paper we used two subsets, S_1:
# the Lordville subset with 56 segments (covering Cannonsville and Pepacton),
# and S_2: another subset around Neversink with 18 segments. Do you think it is
# possible to quickly create a spatial map with these streams and reservoirs?
#
# Below are the stream ids included in these two subsets:
#
set1 <- c('1435', '1436', '1437', '1438', '1439', '1440', '1441', '1442', '1443',
'1444', '1445', '1446', '1447', '1448', '1449', '1450', '1451', '1452',
'1453', '1454', '1455', '1456', '1457', '1458', '1459', '1460', '1461',
'1462', '1463', '1545', '1546', '1547', '1548', '1549', '1550', '1551',
'1552', '1553', '1554', '1555', '1556', '1557', '1558', '1559', '1560',
'1561', '1562', '1563', '1564', '1565', '1566', '1571', '1572', '1573',
'1574', '1575')

set2 <- c('1634', '1635', '1636', '1637', '1638', '1639', '1641', '1642', '1643',
'1644', '1645', '1646', '1648', '1649', '1650', '1652', '1653', '1657')

segC <- '1566'
segP <- '1450'

library(tidyverse)
library(sf)
library(maps)
library(sbtools)
library(ggthemes)

#### Data release data ####

# as of 11/20/21, sbtools isn't recognizing the zip files, only the xml. So I'm manually downloading instead.
# sbtools::item_list_files('5f6a285d82ce38aaa244912e')
# sb_files <- c('study_monitoring_sites.dbf', 'study_stream_reaches.zip', 'study_reservoirs.zip')
# sbtools::item_file_download(sb_id = '5f6a285d82ce38aaa244912e', names = sb_files, destinations = sb_files)

read_shp <- function(shp_basename) {
  sf::read_sf(sprintf('data/%s/%s.shp', shp_basename, shp_basename))
}
drb_reaches <- read_shp('study_stream_reaches') %>%
  mutate(set = ifelse(segidnat %in% set1, 1, ifelse(segidnat %in% set2, 2, 3)))
study_reaches <- drb_reaches %>%
  filter(set %in% c(1,2))
# canpep <- read_shp('study_reservoirs') # only shows 2; missing Neversink
sites <- read_shp('study_monitoring_sites') %>%
  filter(segidnat %in% union(set1, set2)) %>%
  mutate(set = ifelse(segidnat %in% set1, 1, ifelse(segidnat %in% set2, 2, 3)))

#### Additional reservoir polygon[s] to include Neversink ####

# download canonical_lakes_sf.rds from https://drive.google.com/drive/folders/1eb18a-D0B_21AUsmarjHs2pWJCxAevxh
lakes <- readr::read_rds('data/canonical_lakes_sf.rds') # can't use this without knowing the nhdhr ids
canonical_res <- lakes[lakes$site_id %in% paste0('nhdhr_', c('149269381','120022743','151957878')),] %>%
  mutate(name = c('149269381'='Neversink', '120022743'='Cannonsville', '151957878'='Pepacton')[gsub('nhdhr_', '', site_id)],
         set = ifelse(site_id == 'nhdhr_149269381', 2, 1))

# curl::curl_download('https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/HighResolution/GDB/NHD_H_Pennsylvania_State_GDB.zip', 'NHD_PA.zip')
# manually unzip into data folder (I'm just being lazy here)
# sf::st_layers('data/NHD_H_Pennsylvania_State_GDB.gdb')
# lakes_nhd <- sf::read_sf('data/NHD_H_Pennsylvania_State_GDB.gdb', layer='NHDWaterbody')
# lakes_nhd %>% filter(Permanent_Identifier %in% gsub('nhdhr_','', canonical_res$site_id)) # missing pepacton
# neversink <- lakes_nhd[grep('Neversink Reservoir', lakes_nhd$GNIS_Name),] %>%
#   mutate(site_no = sprintf('nhdhr_%s', Permanent_Identifier), geometry = Shape) %>%
#   sf::st_set_geometry('geometry') %>%
#   select(site_no)
# reservoirs <- bind_rows(canpep, neversink)

#### Background map data ####

map_bbox <- sf::st_bbox(sf::st_buffer(sf::st_transform(study_reaches, crs = proj_str), dist=2000)) +
  c(0, -5000, 0, -1000)

states_all <- st_as_sf(maps::map("state", resolution = 0, fill=TRUE, plot=FALSE))
states <- states_all %>%
  filter(ID %in% c('pennsylvania','new york','delaware','new jersey')) %>%
  mutate(name = c('pennsylvania'='Pennsylvania', 'new york'='New York', 'delaware'='Delaware', 'new jersey'='New Jersey')[ID])

proj_str <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

reach_endpoints <- purrr::map_df(study_reaches$geometry, function(reach) {
  tibble(lon = reach[c(nrow(reach)),1], lat = reach[c(nrow(reach)),2], end=2)
}) %>% sf::st_as_sf(coords=c('lon','lat')) %>%
  mutate(
    segidnat = study_reaches$segidnat,
    specialseg = ifelse(segidnat==segC, 'Seg[C]', ifelse(segidnat == segP, 'Seg[P]', 'Other')),
    set = study_reaches$set) %>%
  sf::st_set_crs(sf::st_crs(study_reaches))

st_bbox(study_reaches)
state_labels <- st_as_sf(tibble(
  name=c('New York', 'Pennsylvania', 'New Jersey'),
  lon = c(-75.33, -75.46, -74.68),
  lat = c(42.3, 41.92, 41.28)
), coords=c('lon','lat')) %>%
  sf::st_set_crs(sf::st_crs(study_reaches))

set_colors <- c(`1`='#7676b1', `2`='#bc7cbe', `3`='#acbed0')

inset_centroid_tbl <- tibble(lon = mean(st_bbox(study_reaches)[c(1,3)]), lat = mean(st_bbox(study_reaches)[c(2,4)]))
inset_centroid <- inset_centroid_tbl %>%
  sf::st_as_sf(coords=c('lon','lat')) %>%
  sf::st_set_crs(sf::st_crs(study_reaches))

#### Plot ####

us_map <- ggplot(states_all) +
  geom_sf(color='gray80', fill='white', size=0.3) +
  geom_sf(data = study_reaches, size=1, aes(color=factor(set))) +
  geom_sf(data = inset_centroid, shape=21, size=7, color=set_colors['2'], fill=set_colors['2'], alpha=0.3) +
  # geom_circle(data = inset_centroid_tbl, aes(x0=lon, y0=lat), size=4) +
  scale_color_manual(
    guide='none', values=set_colors,
    labels=c(`3`='None', `1`=parse(text='S[1]'), `2`=parse(text='S[2]')), aesthetics=c('color','fill')) +
  coord_sf(crs = st_crs(proj_str), datum = NA) +
  ggthemes::theme_map()
us_map

study_map <- ggplot(states) +
  geom_sf(color = "gray80", fill = "white") +
  geom_sf_text(data = state_labels, aes(label=name), color='gray80') +
  geom_sf(data = drb_reaches, aes(color=factor(set)), size=0.4) +
  geom_sf(data = reach_endpoints, aes(color=factor(set), fill=factor(set), shape=specialseg, size=specialseg)) +
  geom_sf_text(data = filter(reach_endpoints, specialseg != 'Other'),
               aes(label=specialseg, color=factor(set)),
               nudge_x = -2000, nudge_y = 3000, size = 3, parse=TRUE) +
  geom_sf(data = canonical_res, aes(color=factor(set), fill=factor(set))) +
  geom_sf_text(
    data = canonical_res, aes(label=name), color=set_colors[c(1,2,1)],
    nudge_x = c(-3000, 9500, 4500), nudge_y = c(-7100, 500, -6500), # label.size=0, label.padding = unit(0, 'lines'),
    size=3) +
  scale_shape_manual(guide='none', values=c('Other'=19, 'Seg[C]'=19, 'Seg[P]'=19)) +
  scale_size_manual(guide='none', values=c('Other'=1, 'Seg[C]'=2, 'Seg[P]'=2)) +
  coord_sf(crs = st_crs(proj_str), datum = NA,
           xlim = map_bbox[c('xmin','xmax')],
           ylim = map_bbox[c('ymin','ymax')]) +
  
  scale_color_manual(
    'Subset', values=set_colors,
    labels=c(`3`='None', `1`=parse(text='S[1]'), `2`=parse(text='S[2]')), aesthetics=c('color','fill')) +
  ggthemes::theme_map() #+ theme(legend.position='bottomleft')
study_map

library(cowplot)
combo_map <- ggdraw() +
  draw_plot(study_map) +
  draw_plot(us_map, x = 0.65, y = 0.75, width = 0.3, height = 0.2)
combo_map

ggsave('map.png', plot=combo_map, width=5, height=6, dpi=300)

