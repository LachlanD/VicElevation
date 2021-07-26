devtools::install_github("tylermorganwall/rayshader")
library(raster)
library(rgdal)
library(rayshader)

ras <- raster::raster("vmelev_dem10m_Geotiff_GDA94_Vicgrid.tif")
plot(ras)
res(ras)

scaling_factor <- 25
low_res <- aggregate(ras, fact=scaling_factor)
res(low_res)

plot(low_res)
elmat <- raster_to_matrix(low_res)

#correct for scaling factor
elmat<-elmat/scaling_factor


elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()

elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))


#Wilsons prom crop
ex <-as(extent(2592500, 2650000, 2250000, 2305000), 'SpatialPolygons')
crs(ex) <- crs(ras)
wilsons_prom <- crop(ras, ex)
plot(wilsons_prom)

#Eleveation matrix for rendering
elmat <- raster_to_matrix(wilsons_prom)


elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()
