library(terra)
library(tidyverse)
library(viridis)

jacko <- terra::vect("C:/Users/Ross.Whippo/Desktop/Jakolof_Bay_poly.kml")

test <- terra::vect("~/git/kbay_filterfeeders/data/tidalbands_shore_information_translated/tidalbands_shore_information_translatedPolygon.shp")

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










bathy <- terra::rast("C:/Users/Ross.Whippo/Documents/git/kbay_SAV-HSI_model/data/c_submodel_data/canopy_bathymetry_HSI/bathymetryHSI.grd")

