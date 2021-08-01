devtools::install_github("tylermorganwall/rayshader")
require(raster)
require(rgdal)
require(rayshader)
require(maptools)

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

sf <- readShapeSpatial("layer/GSTRUCZN1M.shp")
plot(sf)
summary(sf)

sf <- readShapeSpatial("layer/geol1m_polygon.shp")

plot(sf)
summary(sf)

cl<-topo.colors(unique(sf$AGEYOUNG))

png("vic_geology_AY.png", width=1920,height=1080)
  plot(sf, col=cl[as.numeric(sf$AGEYOUNG)])
  legend("topright",legend=unique(sf$AGEYOUNG),col=1:length(unique(sf$AGEYOUNG)), fill=cl,ncol=2)
dev.off()

cl<-topo.colors(unique(sf$AGEOLD))

png("vic_geology_AO.png", width=1920,height=1080)
plot(sf, col=cl[as.numeric(sf$AGEOLD)])
legend("topright",legend=unique(sf$AGEOLD),col=1:length(unique(sf$AGEOLD)), fill=cl,ncol=2)
dev.off()

age<-sf$AGEOLD
na <- is.na(age)
age[na]<-sf$AGEYOUNG[na]

ag <-strsplit(as.character(age), '\\(')
ag1 <- unlist(lapply(ag, '[[', 1))
ag2<- unlist(lapply(ag[!is.na(ag)], '[[', 2))
ag2<-gsub("\\).*", "", ag2,)


sf$AGE<-as.factor(ag1)
na <-is.na(sf$AGE)
cl<-topo.colors(unique(sf$AGE[!na]))

png("vic_geology_Age.png", width=1920,height=1080)
  plot(sf, col=cl[as.numeric(sf$AGE)])
  legend("topright",legend=unique(sf$SUBTYPE),col=1:length(unique(sf$SUBTYPE)), fill=cl,ncol=1)
dev.off()


st <- sf$SUBTYPE
summary(st)


sy<-sf$MAP_SYMB

topsymb <- strsplit(as.character(sy), "[a-z]")
sf$TOP_SYM<-as.factor(unlist(lapply(topsymb, '[[', 1)))

subsymb <- strsplit(as.character(sy), "^-?[A-Z]")
sf$SUB_SYM <- as.factor(unlist(lapply(subsymb, '[[', 1)))


top_col<-c("red", "purple", "chocolate", "black")
leg <- gsub("Intrusive", "Intrusive (Igneous)", levels(sf$SUBTYPE))
leg <- gsub("Structural", "Structural (Faults)", leg)


png("vic_geology_RockType.png", width=1920,height=1080)
  plot(sf, col=top_col[as.numeric(sf$SUBTYPE)])
  legend("topright",legend=leg,col=top_col, fill=top_col,ncol=1)
dev.off()

levels(sf$TOP_SYM)
hc <- topo.colors(length(levels(sf$TOP_SYM)))

lv_sy <- levels(sy)

y<-list()

for (i in levels(sf$TOP_SYM)){
  x <- startsWith(lv_sy, i)
  
  y[i]<-paste(lv_sy[x], collapse=" ")
}

png("vic_geology_TopType.png", width=1920,height=1080)
plot(sf, col=hc[as.numeric(sf$TOP_SYM)])
legend("topright",legend=y,col=hc, fill=hc,ncol=1)
dev.off()



sfo<-readOGR("layer/geol1m_polygon")
sfo<-spTransform(sfo, crs(low_res))
sfo_ras<-rasterize(sfo, low_res, st)
plot(sfo_ras, col=top_col)

p<-raster_to_matrix(sfo_ras)

elmat[is.na(p)]<-NA

p[p==1]<-"red"
p[p==2]<-"purple"
p[p==3]<-"chocolate"
p[p==4]<-"black"
#p[is.na(p)]<-"blue"

c<-apply(p, c(1,2), col2rgb)/255

c<-aperm(c, c(3,2,1))




png("overlay.png")
par(mar=c(0, 0, 0, 0))
image(p, useRaster=TRUE, axes=FALSE)
dev.off()

#
#  plot(sfo, col=top_col[as.numeric(as.factor(sfo$SUBTYPE))])
#  legend("topright",legend=leg,col=top_col, fill=top_col,ncol=1)
#dev.off()


#overlay_img <- png::readPNG("overlay.png")


png("rock_overlay_render.png", width=nrow(elmat), height=ncol(elmat))
elmat %>%
  sphere_shade() %>%
  add_overlay(c,alphalayer =0.9) %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()
dev.off()

sf <- readOGR("layer/sg_geological_unit_250k")
replt <- as.character(sf$REPLIT_URI)
replt <- gsub("http://resource.geosciml.org/classifier/cgi/lithology/","",replt)
sf$MAIN_LITH <- as.factor(replt)

sfwp<-spTransform(sf, crs(wilsons_prom))
lt<-rasterize(sfwp, wilsons_prom, as.factor(sf$MAIN_LITH))

lvls<-levels(sf$MAIN_LITH)

lt@data@values<-factor(lvls[lt@data@values])
cl<-topo.colors(nlevels(lt@data@values))


image(lt, col=cl)
legend("bottomright", legend=levels(lt@data@values), col=cl, fill=cl)


lt@data@values<-cl[as.numeric(lt@data@values)]
ltmat<-raster_to_matrix(lt)


z<-apply(ltmat, c(1,2), col2rgb)/255
z<-aperm(z, c(3,2,1))


png("wp_rock_overlay_render.png", width=nrow(wilsons_prom), height=ncol(wilsons_prom))
wp_mat %>%
  sphere_shade() %>%
  add_overlay(z,alphalayer =0.9) %>%
  add_shadow(ray_shade(wp_mat), 0.5) %>%
  add_shadow(ambient_shade(wp_mat), 0) %>%
  plot_map()
dev.off()
