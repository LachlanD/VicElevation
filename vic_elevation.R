devtools::install_github("tylermorganwall/rayshader")
library(raster)
library(rgdal)
library(rayshader)

#http://services.land.vic.gov.au/Vicmap_Elevation_DEM10m/vmelev_dem10m_Geotiff_GDA94_Vicgrid.tif (12GB)
ras <- raster::raster("vmelev_dem10m_Geotiff_GDA94_Vicgrid.tif")
plot(ras)
res(ras)

scaling_factor <- 25
low_res <- aggregate(ras, fact=scaling_factor)
res(low_res)

png("vic_elevation_downsampled")
plot(low_res)
dev.off()

#convert raster to elevation matrix
elmat <- raster_to_matrix(low_res)

#correct for scaling factor
elmat<-elmat/scaling_factor

png("vic_downsampled_render.png", width=nrow(elmat), height=ncol(elmat))
elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()
dev.off()

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
wp_mat <- raster_to_matrix(wilsons_prom)

png("wp_10m_render.png", width=nrow(wp_mat),height=ncol(wp_mat))
wp_mat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(wp_mat), 0.5) %>%
  add_shadow(ambient_shade(wp_mat), 0) %>%
  plot_map()
dev.off()

#South Point
ex <-as(extent(2610000, 2627500, 2257500, 2267500), 'SpatialPolygons')
crs(ex) <- crs(ras)
south_point <- crop(ras, ex)
plot(south_point)

#Eleveation matrix for rendering
sp_mat <- raster_to_matrix(south_point)

png("south_point 10m_render.png",width=nrow(sp_mat),height=ncol(sp_mat))
sp_mat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(sp_mat), 0.5) %>%
  add_shadow(ambient_shade(sp_mat), 0) %>%
  plot_map()
dev.off()



require(maptools) 
sf <- readShapeSpatial("layer/sg_geological_unit_250k.shp")
plot(sf)
summary(sf)

replt <- as.character(sf$REPLIT_URI)
replt <- gsub("http://resource.geosciml.org/classifier/cgi/lithology/","",replt)
sf$MAIN_LITH <- as.factor(replt)


#lith<-as.character(sf$LITHOLOGY)
#main_lith <- gsub("^(.*?) [[:punct:]].*","\\1", lith)
#main_lith <- unlist(lapply(main_lith, '[[', 1))
#sf$MAIN_LITH<-as.factor(main_lith)

cl<-topo.colors(unique(sf$MAIN_LITH))

png("vic_lithology.png", width=1920,height=1080)
  plot(sf, col=cl[as.numeric(sf$MAIN_LITH)])
  legend("topright",legend=unique(sf$MAIN_LITH),col=1:length(unique(sf$MAIN_LITH)), fill=cl,ncol=3)
dev.off()
