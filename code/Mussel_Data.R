#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Mussel Data                                                                    ##
# Data source: Kasitsna Bay Lab - NCCOS - NOAA                                   ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2024-07-22                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Here is a narrative summary of the script.

# Required Files (check that script is loading latest version):
# mussel_data.csv
# tidalbands_shore_information_translatedPolygon.shp
# Jakolof_Bay_poly.kml

# Associated Scripts:
# FILE.R

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2024/07/22 Script copied from R Introduction files

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES & PREPARE WORKSPACE                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# clear any existing data in the environment
rm(list = ls())

library(tidyverse) # tidyverse is a collection of packages for data manipulation
library(viridis) # viridis is a color-blind friendly palette for graphs

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# command to read in data to R
mussel_data <- read_csv("data/mussel_data.csv", 
                        col_types = cols(polygon_ID = col_character(), 
                                         quadrat = col_character(), 
                                         site = col_character())) 
# look at summary of data structure
str(mussel_data)
glimpse(mussel_data)
head(mussel_data)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPLORATORY SUMMARY FIGURES                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# the simplest ggplot boxplot
ggplot(mussel_data, aes(x = site, 
                        y = length_mm)) +
  geom_boxplot()

# adding themes
ggplot(mussel_data, aes(x = site, 
                        y = length_mm)) +
  geom_boxplot() +
  theme_bw()

# adding color
ggplot(mussel_data, aes(x = site, 
                        y = length_mm,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C") 

# separate by site and quadrat
ggplot(mussel_data, aes(x = quadrat, 
                        y = length_mm,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C") 

# reorder quadrat factor
mussel_data <- mussel_data %>%
  mutate(quadrat = factor(quadrat, levels = c("1", "2", "3", "4", "5",
                                              "6", "7", "8", "9", "10",
                                              "11", "12", "13", "14", "15",
                                              "16", "17", "18", "19", "20")))

# site and quadrat with proper ordering
ggplot(mussel_data, aes(x = quadrat, 
                        y = length_mm,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C")

# total number of mussels per quadrat
ggplot(mussel_data, aes(x = quadrat,
                        fill = site)) +
  stat_count() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C") 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DATA MANIPULATION & VISUALIZATION                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# How much water can mussels filter per hour calculated from their length?

# equation: Rc = 0.0002L^2.19 from Jones et al. 1992

mussel_data <- mussel_data %>%
  mutate(filtration_l_hr = 0.0002 * length_mm^2.19)

# how many liters per hour do the measured mussels filter combined?
sum(mussel_data$filtration_l_hr)

# liters filtered per hour per site
ggplot(mussel_data, aes(x = site,
                        y = filtration_l_hr,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C")

# liters filtered per hour per quadrat
ggplot(mussel_data, aes(x = quadrat,
                        y = filtration_l_hr,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis("Site", 
                     labels = c("One",
                                "Two",
                                "Three",
                                "Four"),
                     discrete = TRUE,
                     option = "C") +
  labs(x = "Quadrat", y = "Filtration Rate L/hr") 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SPATIAL ANALYSIS                                                             ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# terra package works with spatial data
library(terra)

# import the shoreline spatial data
tidelands <- terra::vect("data/tidalbands_shore_information_translated/tidalbands_shore_information_translatedPolygon.shp")

# turn the spatial layer into a data.frame (spreadsheet) and summarize its contents
tidelands_df <- data.frame(values(tidelands))

# plot all the tidelands data by the 'subclass' (substrate type) factor
plot(tidelands, "subclass", col = viridis(nrow(tidelands),
                                          begin = 0,
                                          end = 1,
                                          option = "turbo"))

# import a bounding shape for Jakolof Bay
jako <- terra::vect("data/Jakolof_Bay_poly.kml")

# check if the coordinate reference system (crs) matches the tidelands data
crs(jako)
crs(tidelands)

# extract the tidelands crs
newcrs <- crs(tidelands)

# reproject the Jakolof layer in the new crs
jako_new <- terra:: project(jako, newcrs)

# crop the tidelands layer to just Jakolof
tidelands_jako <- terra::crop(tidelands, jako_new)

# plot tidelands for Jakolof by 'subclass'
plot(tidelands_jako, "subclass", col = viridis(nrow(tidelands_jako),
                                               begin = 0,
                                               end = 1,
                                               option = "turbo"))

# add centroids of polygons
plot(centroids(tidelands_jako, 
               inside = TRUE), add = TRUE)

# extract lat long of centroids
habitat_centroids <- centroids(tidelands_jako, inside = TRUE)
habitat_centroids <- project(habitat_centroids, "+proj=longlat")
coords <- project(habitat_centroids, "+proj=longlat")
plot(coords)
latlong <- extract(habitat_centroids, coords)

points <- crds(centroids(tidelands_jako, inside = TRUE))
jako_df <- data.frame(tidelands_jako)
all_substrate <- data.frame(points, jako_df$subclass)

# calculate total area of the polygons in square meters
sum(terra::expanse(tidelands_jako, unit = "m")) 

#+++++++++++++++++++
# SLOPE CORRECTION #
#+++++++++++++++++++

# FUNCTIONS
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

# tide measurements from https://tidesandcurrents.noaa.gov/datums.html?id=9455517
# highest tide: 7.05 m
# mean low: 0.514 m
# total height of polygons = 6.536

# calculate multiplier to find estimated area of slope plane
# 1/cos(slope) = Multiplier
# Multiplier * Footprint Area = estimated slope area

# averaging high_slope, highmediu0, lowmedium0, low_slope and calculating estimated areas
jako_df <- jako_df %>%
  mutate(temp = str_extract(high_slope, "[0-9]-[0-9]*")) %>%
  mutate(temp = replace_na(temp, "60")) %>%
  separate_wider_delim(temp, "-", names = c("low", "high"), too_few = "align_start") %>%
  mutate(high = replace_na(high, "60")) %>%
  mutate(across(c(high, low), as.numeric)) %>%
  rowwise() %>%
  mutate(high_slope_mean = mean(c(low, high))) %>% # high mean slope
  mutate(temp1 = str_extract(highmediu0, "[0-9]-[0-9]*")) %>%
  mutate(temp1 = replace_na(temp1, "60")) %>%
  separate_wider_delim(temp1, "-", names = c("low1", "high1"), too_few = "align_start") %>%
  mutate(high1 = replace_na(high1, "60")) %>%
  mutate(across(c(high1, low1), as.numeric)) %>%
  rowwise() %>%
  mutate(highmed_slope_mean = mean(c(low1, high1))) %>% # high-med mean slope
  mutate(temp2 = str_extract(lowmedium0, "[0-9]-[0-9]*")) %>%
  mutate(temp2 = replace_na(temp2, "60")) %>%
  separate_wider_delim(temp2, "-", names = c("low2", "high2"), too_few = "align_start") %>%
  mutate(high2 = replace_na(high2, "60")) %>%
  mutate(across(c(high2, low2), as.numeric)) %>%
  rowwise() %>%
  mutate(lowmed_slope_mean = mean(c(low2, high2))) %>% # low-med mean slope
  mutate(temp3 = str_extract(low_slope, "[0-9]-[0-9]*")) %>%
  mutate(temp3 = replace_na(temp3, "60")) %>%
  separate_wider_delim(temp3, "-", names = c("low3", "high3"), too_few = "align_start") %>%
  mutate(high3 = replace_na(high3, "60")) %>%
  mutate(across(c(high3, low3), as.numeric)) %>%
  rowwise() %>%
  mutate(low_slope_mean = mean(c(low3, high3))) %>% # low mean slope
  mutate(average_slope = (high_slope_mean +
                            highmed_slope_mean +
                            lowmed_slope_mean +
                            low_slope_mean)/4) %>% # grand mean slope
  mutate(multiplier = 1/cos(deg2rad(average_slope)))

# standardize substrate names
jako_df <- jako_df %>%
  mutate(substrate = case_when(subclass == "Rubble" ~ "pebble_granule",
                               subclass == "Cobble/Gravel" ~ "pebble_granule",
                               subclass == "Mud/Organic" ~ "mud_organic",
                               subclass == "Bedrock" ~ "bedrock",
                               .default = subclass))

# add multiplier as value to layer  
tidelands_jako[["multiplier"]] <- jako_df$multiplier

# extract raw polygon areas
jako_polygon_area <- expanse(tidelands_jako)  

# create data frame of corrected polygon areas
jako_areas <- data.frame(jako_df$key, jako_df$substrate, jako_polygon_area, jako_df$multiplier) %>%
  mutate(corrected_area = jako_polygon_area * jako_df.multiplier) %>%
  mutate(substrate = jako_df.substrate)

#++++++++++++++++++++++++++++++++++++++++++++++++++
# REDUCE AREA BY CONSTANT FOR EACH SUBSTRATE TYPE #
#++++++++++++++++++++++++++++++++++++++++++++++++++
# mussels are constrained to different depths of the intertidal based on substrate type.
# no intertidal elevation model exists so instead polygons are multiplied by a constant
# determined for each habitat type to represent average proportion of area from highest
# high tide to mean low tide where mussels can be found.

# determine mean tidal heights for upper and lower quadrats between habitat types
mussel_data %>%
  select(position_percent, substrate, tidal_height_ft) %>%
  mutate(tidal_height_m = tidal_height_ft/3.281) %>%
  mutate(position_percent = as.character(position_percent)) %>%
  filter(position_percent %in% c("10", "90")) %>%
  ggplot(aes(x = position_percent, y = tidal_height_m, fill = substrate)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.8) +
  facet_wrap(.~substrate) +
  theme_bw()

mussel_band_elevation <- mussel_data %>%
  select(position_percent, substrate, tidal_height_ft) %>%
  mutate(tidal_height_m = tidal_height_ft/3.281) %>%
  filter(position_percent %in% c(10, 90)) %>%
  group_by(substrate, position_percent) %>%
  summarise(mean(tidal_height_m))
# boulder  3.9810733 - 0.6031698 = 3.377903
# pebble_granule 3.9905430 - 1.3842470 =  2.606296
# height location of polygons: # highest tide: 7.05 m mean low: 0.514 m
# range: 7.05 - 0.514 = 6.536
# proportions:
# boulder = 3.377903/6.536 = 0.516815
# pebble_granule = 2.606296/6.536 = 0.3987601

# CONSTANTS TO APPLY AFTER SLOPE CORRECTION:
BOULDER_cor <- 0.516815
PEBBLE_GRANULE_cor <- 0.3987601

# apply corrections to every polygon based on substrate type
jako_mussel_area <- jako_areas %>%
  mutate(corrected_mussel_area = case_when(substrate == "pebble_granule" ~ corrected_area * PEBBLE_GRANULE_cor,
                                           substrate == "boulder" ~ corrected_area * BOULDER_cor,
                                           .default = corrected_area))
  

# write data as a csv
write_csv(jako_mussel_area, "data/jakolof_mussel_area.csv")

#+++++++++++++++++++++++++++++++++++++++++++
# JOIN SPATIAL DATA WITH MUSSEL FILTRATION #
#+++++++++++++++++++++++++++++++++++++++++++

# Determine mean tidal height for quads within substrates, mean mussel size, 
# mussel count, and filtration rates
mussel_height_summary <- mussel_data %>%
  group_by(substrate, position_percent) %>%
  summarise(across(c(tidal_height_ft,
                     length_mm,
                     filtration_l_hr), mean)) 

mussel_counts <- mussel_data %>%
  group_by(site, substrate, position_percent) %>%
  count() %>%
  ungroup() %>%
  group_by(substrate, position_percent) %>%
  mutate(mean_count = mean(n)) %>%
  ungroup() %>%
  select(substrate, position_percent, mean_count) %>%
  distinct()

mussel_height_summary <- mussel_height_summary %>%
  left_join(mussel_counts, by = c("substrate",
                                  "position_percent")) %>%
  mutate(mean_filtration = filtration_l_hr * mean_count) %>%
  mutate(tidal_height_m = tidal_height_ft/3.281)

# plot mean filtration by position
mussel_height_summary %>%
ggplot(aes(x = tidal_height_m,
                                  y = mean_filtration,
                                  fill = substrate)) +
  geom_col(#position = "dodge",
           width = 0.5) +
  theme_bw() +
  scale_fill_viridis("Substrate Type", discrete = TRUE,
                     option = "E") +
  labs(x = "Tidal Height (m)", y = "Mean Filtration Rate (l / hr)")

# divide area of each mussel area into 5 equal areas and multiply by each mean 
# quadrat filtration rate (does not account for unsurveyed habitat types)
filtration_estimate <- jako_mussel_area %>%
  expand_grid(position_percent = c(10, 30, 50, 70, 90)) %>% # add position percent 
  mutate(band_area = corrected_mussel_area/5) %>% # add 1/5 area of each polygon
  left_join(mussel_height_summary, by = c("substrate", "position_percent")) %>% # add filtration rates
  mutate(quadrat_mulitplier = band_area/0.5) %>% # calculate how many 'quadrats' in each area
  mutate(filtration_per_area = filtration_l_hr * quadrat_mulitplier) # multiply number of quadrats times filtration rate

sum(filtration_estimate$filtration_per_area, na.rm = TRUE)
# 117309.9 l/hr

#++++++++++++++++++++++++++++++++++++++++++
# CALCULATE TIDAL EXCHANGE OF JAKOLOF BAY #
#++++++++++++++++++++++++++++++++++++++++++
bathy <- terra::rast("data/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff")
bathy_mask <- terra::mask(bathy, jako_new)
bathy_crop <- terra::crop(bathy_mask, jako_new)

# test that bathymetry only includes Jakolof
test <- bathy_crop <= 3
plot(test)

bathy_df <- data.frame(bathy_crop) %>%
  filter(KBL.bathymetry_GWA.area_50m_EPSG3338 <= 3) %>%
  mutate(depth_at_3 = KBL.bathymetry_GWA.area_50m_EPSG3338 - 3) %>%
  mutate(volume_3 = abs(depth_at_3) * 50 * 50) %>%
  mutate(volume_0 = case_when(KBL.bathymetry_GWA.area_50m_EPSG3338 > 0 ~ 0,
                              .default = KBL.bathymetry_GWA.area_50m_EPSG3338)) %>%
  mutate(volume_0 = abs(volume_0) * 50 * 50)
 
# volume in cubic meters at 3 m tidal exchange
  sum(bathy_df$volume_3) - sum(bathy_df$volume_0)
  # 6710768 cubic meters
  
# convert l/hr to cubic meters per hour
  sum(filtration_estimate$filtration_per_area, na.rm = TRUE)/1000
# 117.3099 cubic meters per hour

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CREATE EXTREME CASE: MAX MUSSELS SIZE, DENSITY, AREA #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

sum(jako_areas$corrected_area)/0.5 * # total area of all corrected Jakolof intertidal divided by number of quadrats
  ((0.0002 * max(mussel_data$length_mm)^2.19)/1000) * # filtration rate of largest mussel found in cubic meters per hour
  max(mussel_data %>% # maximum density found across all quadrats
        group_by(quadrat) %>%
        count() %>% 
        ungroup() %>%
        select(n))
# 553484 cubic meters per hour
# tidal exchange rate per hour at 3 meter exchange
(sum(bathy_df$volume_3) - sum(bathy_df$volume_0)) / 6
#  1118461 cubic meters per hour

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

library(terra)
library(tidyverse)
library(viridis)

jacko <- terra::vect("C:/Users/Ross.Whippo/Desktop/Jakolof_Bay_poly.kml")

test <- terra::vect("~/git/kbay_filterfeeders/data/tidalbands_shore_information_translated/tidalbands_shore_information_translatedPolygon.shp")

plet(test)

newcrs <- crs(test)

jacko_new <- terra:: project(jacko, newcrs )

new <- terra::crop(test, jacko_new)


plot(new, "subclass", col = viridis(nrow(new),
                                    begin = 0,
                                    end = 1,
                                    option = "turbo"))
new2 <- project(new, "+proj=longlat")
habitat_centroids <- centroids(new, inside = TRUE)
habitat_centroids <- project(habitat_centroids, "+proj=longlat")
coords <- project(habitat_centroids, "+proj=longlat")
plot(coords)
latlong <- extract(habitat_centroids, coords)
points <- crds(centroids(new2, inside = TRUE))
substrate <- data.frame(new)
all_substrate <- data.frame(points, substrate$subclass)


plot(new2, "subclass")
plot(centroids(new2, inside = TRUE), add = TRUE)

plet(new2, "subclass") |> points(centroids(new2, inside = TRUE))

plet(new, "subclass",
     fill = 0.9,
     col = viridis(n = 4 ,begin = 0,
                   end = 1,
                   option = "turbo"))
write_csv(all_substrate, "Substrate_Centroids.csv")

test2 <- new[,"subclass"]
test3 <- data.frame(test2)
seg_ha <- expanse(test2, unit = "ha")
totaldf <- data.frame(test2, seg_ha)

totaldf %>%
  group_by(subclass) %>%
  mutate(subclass = replace_na(subclass, "unknown")) %>%
  summarize(hectares = sum(seg_ha)) %>%
  ggplot(aes(x = subclass, y = hectares, fill = subclass)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, option = "turbo") +
  theme_bw()


testdf <- data.frame(test)

# segment stats

r <- rast(xmin=0, ncols = 1000, nrows = 1000)
test1 <- rasterize(new, bathy, fun = sum)

expanse(test1, unit = "ha", transform = TRUE)
#canopy stats

## proportion suitability 
under_df <- data.frame(newdf)
under_df %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
ggplot(under_df, aes(x=HSI_value)) +
  geom_histogram(binwidth = 0.05) +
  theme_bw()

# printed frequencies
cantab <- as.data.frame(table(cut(under_df$HSI_value,breaks=seq(0,1,by=0.05),
                                  include.lowest = TRUE)))
# values between 0.10 - 0.65
sum(cantab[3:18,2]) # 93420
# total cells: 141036
# proportion 
# 93420/141036 # 0.662
allones <- under_df %>%
  filter(HSI_value == 1)




plot(test)

plot(segs_albers)



dsm <- terra::rast("C:/Users/Ross.Whippo/Desktop/custom_download/dds4/ifsar/dsm/DSM_N5915W15130P/DSM_N5915W15130P.tif")
terra::plot(dsm)
dsm2 <- terra::rast("C:/Users/Ross.Whippo/Desktop/custom_download/dds4/ifsar/dsm/DSM_N5915W15145P/DSM_N5915W15145P.tif")
terra::plot(dsm2, add = TRUE)
bathy8m <- terra::rast("C:/Users/Ross.Whippo/Desktop/Kachemak_bathymetry_8m/Kachemak_bathymetry_8m.tif")
new_crs <- crs(jako_new)
bathy8m_new <- project(bathy8m, new_crs)
masked_bathy <- terra::crop(bathy8m_new, jako_new)

plot(masked_bathy, add = TRUE)

shallow_bathy <- masked_bathy > -1
plot(shallow_bathy, add = TRUE)


masked_dsm2 <- terra::crop(dsm2, jako_new) 
plot(masked_dsm2)
masked_dsm2_0 <- masked_dsm2 < 0.00001
plot(masked_dsm2_0)
plot(tidelands_jako, add = TRUE)

bathy <- terra::rast("C:/Users/Ross.Whippo/Documents/git/kbay_SAV-HSI_model/data/c_submodel_data/canopy_bathymetry_HSI/bathymetryHSI.grd")
terra::plot(bathy)


#++++++++++++++++++++++++++++++++++++++++++++
# CROP OUT UNINHABITED DEPTHS FROM POLYGONS #
#++++++++++++++++++++++++++++++++++++++++++++

bathymetry <- terra::rast("~/git/kbay_SAV-HSI_model/data/a_raw_data/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff")

# constrain bathymetry to roi
bathy_roi <- terra::crop(bathymetry, jako_new)
plot(bathy_roi)
bath_mask <- mask(bathy_roi, jako_new)
plot(bath_mask)
tidal_only <- bath_mask < 7
plot(tidal_only)
