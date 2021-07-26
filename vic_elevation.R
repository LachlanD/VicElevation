devtools::install_github("tylermorganwall/rayshader")
library(raster)
library(rgdal)
library(rayshader)

ras <- raster::raster("vmelev_dem10m_Geotiff_GDA94_Vicgrid.tif")
plot(ras)
res(ras)
low_res <- disaggregate(ras, fact=10)

ex <-as(extent(2592500, 2650000, 2250000, 2305000), 'SpatialPolygons')
crs(ex) <- crs(ras)
wilsons_prom <- crop(ras, ex)
low_res <- disaggregate(ras, factor=10)
plot(wilsons_prom)


elmat <- raster_to_matrix(wilsons_prom)


elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()
