update.packages(ask=FALSE, checkBuilt=TRUE)
install.packages("rgdal")
install.packages("sf")
install.packages("readr")
install.packages("raster")
install.packages("tmap")
install.packages("RColorBrewer")
library(rgdal)
library(sf)
library(readr)
library(tmap)


######################
#funções (abrir e reprojetar)
 
reproject_shp<- function(path_shp){
  
  
  shp_open<- st_read(path_shp)
  new_shp<- st_transform(shp_open, 31983)
}

reproject_raster<- function(path_raster){
  
  raster_open<- raster(path_raster)
  raster_new<- projectRaster(raster_open, crs="+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs ")
}


municipios_sp<- reproject_shp("Municipios-sp.shp")
SaoPaulo<- reproject_shp("SaoPaulo.shp")
AreaEstudo<- reproject_shp("area_estudo_rodoanel.shp")
rodoanel<- reproject_shp("08_Rodoanel.shp")
rural<- reproject_shp('dados/01A_Zona_Rural.shp')
rios<- reproject_shp('dados/05_Hidrografia_Rios.shp')
ucs<- reproject_shp('dados/UC_32723.shp')
declividade<- reproject_raster('dados/DECLIVIDADE-Y-C.tif')  



# visualização rápida
dev.new(height=10,weight=10)
plot(declividade)
plot(st_geometry(rural),add=T)
plot(st_geometry(SaoPaulo),add=T)
plot(st_geometry(AreaEstudo),add=T)
plot(rios,add=T)
plot(st_geometry(ucs),add=T)


######################
# mask, crop e reclassificar (declividade) - quando necessário

crop_reclassify<- function(raster_decliv, area_estudo){
  
  declividade_crop<- crop(raster_decliv, area_estudo)
  # declividade_crop<- projectRaster(declividade_crop, crs="+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs") #descomentar caso nao tenha definido "crs" anteriormente
  declividade_crop_reclassify<- reclassify(declividade_crop,  c(0,3,1,3,8,2,8,20,3,20,45,4,45,100,5))
}

mask_reclassify<- function(raster_decliv, area_estudo){
  
  declividade_mask<- mask(raster_decliv, area_estudo)
  # declividade_mask<- projectRaster(declividade_mask, crs="+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs") #descomentar caso nao tenha definido "crs" anteriormente
  declividade_mask_reclassify<- reclassify(declividade_mask,  c(0,3,1,3,8,2,8,20,3,20,45,4,45,100,5))
}


# decliv_crop<- crop_reclassify(declividade, AreaEstudo)
# decliv_crop
# 
# decliv_mask <- mask_reclassify(declividade, AreaEstudo)
# decliv_mask
  
######################
# rasterizar os shapefiles
 
rasterize_polygon<- function(shp, area_estudo, declividade_raster){
  
  shp_crop<- st_crop(shp, area_estudo)
  shp_crop$fragility<- 1 #onde existir dado, terá peso 1
  shp_raster<- raster(extent(declividade_raster))
  crs(shp_raster)<- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
  res(shp_raster)<- res(declividade_raster)
  shp_rasterize<- rasterize(shp_crop, shp_raster, field="fragility")
}

rasterize_line<- function(shp, area_estudo, declividade_raster){
  
  shp_crop<- st_crop(shp, area_estudo)
  shp_buffer<- st_buffer(shp_crop, 30) # buffer de 30m, por ser o tamanho mínimo exigido no Código Florestal (Lei nº 12.651/12)
  shp_simplify<- st_simplify(shp_buffer, preserveTopology= T)
  shp_simplify$fragility<- 1 #onde existir dado, terá peso 1
  shp_raster<- raster(extent(declividade_raster))
  crs(shp_raster)<- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
  res(shp_raster)<- res(declividade_raster)
  shp_rasterize<- rasterize(shp_simplify, shp_raster, field="fragility")
}


rural_rasterize<- rasterize_shp(rural, AreaEstudo, declividade)
rural_rasterize

rios_rasterize<- rasterize_line(rios, AreaEstudo, declividade)
rios_rasterize

ucs_rasterize<- rasterize_shp(ucs, AreaEstudo, declividade)
ucs_rasterize


######################
# analise multicritério

rasters_all<- brick(ucs_raster, rural_raster, rios_raster)
frag_pot <- stackApply(rasters_all, indices=c(1,1,1), fun=sum, na.rm = T)
frag_pot[frag_pot==0] <- NA
frag_pot_crop<- crop(frag_pot, AreaEstudo)
writeRaster(frag_pot_crop, "fragilidade_potencial.tif", overwrite= T)

# visualização rápida 
dev.new()
plot(frag_pot_crop)
zoom(frag_pot, ext=drawExtent()) #desenhar a área de zoom

######################
# MAPAS

#metodo incremental - mapa de localizacao
dev.new(weight=10,length=10)
plot(st_geometry(municipios_sp), border="grey70",lwd=0.2, main="Estado de São Paulo")
plot(st_geometry(SaoPaulo), col="firebrick1",border="firebrick4",add=TRUE)

#metodo incremental - mapa de localizacao com zoom
dev.new(weight=10,length=10)
plot(st_geometry(SaoPaulo),border="grey90", lwd=1,main="Localização da área de estudo \n Municipio de São Paulo")
plot(st_geometry(AreaEstudo),col="goldenrod1",border="goldenrod4", lwd=2,add=TRUE)
plot(st_geometry(rodoanel),col="firebrick3",lwd=3,add=TRUE)
text(locator(1),"São Paulo",cex=1)

#metodo nao-incremental - mapa de localizacao
dev.new(weight=10,length=10)
tm_shape(municipios_sp) + tm_borders() + tm_grid(
  alpha = 0.2,
  labels.rot = c(0, 90),
  labels.inside.frame = FALSE
) + tm_compass(position = c("right", "top")) + tm_scale_bar() + tm_credits("Projecao SIRGAS 2000 \n Fonte: IBGE") +
  tm_layout(
    main.title = "Mapa de localizacao da area de estudo \n Municipios de Sao Paulo",
    main.title.position = "center",
    outer.margins = 0.05
  ) + tm_add_legend(
    type = "fill",
    col = "white",
    border.col = "black",
    labels = "Municipios de Sao Paulo"
  ) + tm_shape(SaoPaulo) + tm_polygons(col = "firebrick1",
                                       border.col = "firebrick4",
                                       lwd = 2) + tm_add_legend(
                                         type = "fill",
                                         col = "firebrick1",
                                         border.col = "firebrick4",
                                         labels = "Municipios de Sao Paulo"
                                       )

#metodo nao-incremental - mapa de localizacao com zoom
dev.new(wight = 10, lenght = 10)
tm_shape(SaoPaulo) + tm_polygons(col = "grey96", border.col = "grey40") + tm_grid(
  alpha = 0.1,
  labels.rot = c(0, 90),
  labels.inside.frame = F
)  +
  tm_layout(
    main.title = "Mapa de localizacao\n da area de estudo",
    main.title.position = "center",
    outer.margins = 0.05
  ) + tm_add_legend(
    type = "fill",
    col = "grey96",
    border.col = "grey40",
    labels = "Municipio de Sao Paulo"
  ) + tm_shape(AreaEstudo) + tm_polygons(
    col = "khaki1",
    border.col = "gold3", 
    lwd = 2
  ) + tm_add_legend( 
    type = "fill",
    col = "khaki1", 
    border.col = "gold3",
    labels = "Area de estudo"
  ) + tm_shape(rodoanel) +tm_lines(
    col = "firebrick3", 
    lwd = 3
  ) + tm_add_legend( 
    type = "line",
    lwd=3,
    col = "firebrick3",
    labels = "Rodoanel"
  ) + tm_compass(position = c("right", "top")
  ) + tm_scale_bar(position = c("right", "center")
  ) + tm_credits(position = c("right", "center"), "Projecao SIRGAS 2000 \n Fonte: IBGE")

#mapa interativos
tmap_mode("view")
dev.new()
tm_shape(SaoPaulo) + tm_polygons(col = "grey96", border.col = "grey40") + 
  tm_grid(
    alpha = 0.1,
    labels.rot = c(0, 90),
    labels.inside.frame = F
  )  +
  tm_layout(
    main.title = "Mapa de localizacao\n da area de estudo",
    main.title.position = "center",
    outer.margins = 0.05
  ) + 
  tm_add_legend(
    type = "fill",
    col = "grey96",
    border.col = "grey40",
    labels = "Municipio de Sao Paulo"
  ) + 
  tm_shape(AreaEstudo) + 
  tm_polygons(
    col = "khaki1",
    border.col = "gold3",
    lwd = 2) + tm_add_legend(
      type = "fill",
      col = "khaki1",
      border.col = "gold3",
      labels = "?rea de estudo"
    ) + tm_shape(rodoanel) +
  tm_lines(col = "firebrick3", lwd = 3) + 
  tm_add_legend(
    type = "fill",
    lwd = 3,
    col = "firebrick3",
    border.col = NA,
    labels = "Rodoanel"
  ) +  tm_scale_bar(position = c("right", "center"))
tmap_mode("plot")

#mapa de declividade (%)

pal18 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")

tm_shape(SaoPaulo) + tm_polygons(col = "grey96", border.col = "grey40") + tm_grid(
  alpha = 0.1,
  labels.rot = c(0, 90),
  labels.inside.frame = F
)  +
  tm_layout(
    main.title = "Mapa de declividade\n da area de estudo",
    main.title.position = "center",
    outer.margins = 0.05
  ) + tm_add_legend(
    type = "fill",
    col = "grey96",
    border.col = "grey40",
    labels = "Municipio de Sao Paulo"
  ) +  tm_shape(AreaEstudo) + tm_polygons(border.col = "gold3", lwd = 2) + tm_add_legend( type = "fill",col = "khaki1", border.col = "gold3",labels = "Area de estudo")+tm_shape(rodoanel) +tm_lines(col = "firebrick3", lwd = 3)+tm_add_legend( type = "line",lwd=3,col = "firebrick3",labels = "Rodoanel") + tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "center")) + tm_credits(position = c("right", "center"), "Projecao SIRGAS 2000 \n Fonte: IBGE")+tm_shape(decliv_mask)+tm_raster(palette = pal18,title = "Declividade (%)")

#mapa de declividade (pesos)
tm_shape(SaoPaulo) + tm_polygons(col = "grey96", border.col = "grey40") + tm_grid(
  alpha = 0.1,
  labels.rot = c(0, 90),
  labels.inside.frame = F
)  +
  tm_layout(
    main.title = "Mapa de pesos da declividade\n da area de estudo",
    main.title.position = "center",
    outer.margins = 0.05
  ) + tm_add_legend(
    type = "fill",
    col = "grey96",
    border.col = "grey40",
    labels = "Municipio de Sao Paulo"
  ) +  
  tm_shape(AreaEstudo) + tm_polygons(border.col = "gold3", lwd = 5) + tm_add_legend( type = "fill",col = NA, border.col = "gold3",size=2,labels = "Area de estudo")+tm_shape(rodoanel) +tm_lines(col = "firebrick3", lwd = 3)+tm_add_legend( type = "line",border.lwd =2,col = "firebrick3",labels = "Rodoanel") + tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "center")) + tm_credits(position = c("right", "center"), "Projecao SIRGAS 2000 \n Fonte: IBGE")+
  tm_shape(decliv_reclass_mask)+tm_raster(labels = c("1","2","3","4"),legend.show = T,palette = pal18,title = "Declividade (pesos)")

# mapa final - análise multricritério
dev.new()
tm_shape(SaoPaulo) + tm_polygons(col = "grey96", border.col = "grey40") + tm_grid(
  alpha = 0.1,
  labels.rot = c(0, 90),
  labels.inside.frame = F
)  +
  tm_layout(
    main.title = "Mapa de restrições ambientais\n da área de estudo",
    main.title.position = "center",
    outer.margins = 0.05
  ) + tm_add_legend(
    type = "fill",
    col = "grey96",
    border.col = "grey40",
    labels = "Municipio de Sao Paulo"
  ) +  
  tm_shape(AreaEstudo) + tm_polygons(border.col = "gold3", lwd = 5) + tm_add_legend(
    type = "fill",
    col = NA,
    border.col = "gold3",
    size = 2,
    labels = "Area de estudo"
  ) + 
  tm_shape(frag_pot_crop) + tm_raster(
    labels = c("1 - sem ou baixa restricao ambiental", "2 - com restricao ambiental, mas passivel de uso", "3 - com restricao e ocupacao nao recomendada", "4 - ocupação não permitida"),
    legend.show = T,
    palette = "OrRd",
    title = "Grau de restrição"
  ) + 
  tm_shape(rodoanel) + tm_lines(col = "firebrick3", lwd = 3) + tm_add_legend(
    type = "line",
    border.lwd = 2,
    col = "firebrick3",
    labels = "Rodoanel"
  ) + tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "center")) + tm_credits(position = c("right", "center"),"Projecao SIRGAS 2000 \n Fonte: IBGE") 


