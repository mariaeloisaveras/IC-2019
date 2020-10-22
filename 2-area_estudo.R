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
library(RColorBrewer)


###############
#ORGANIZACAO DOS DADOS
setwd("C:/Users/veras_mariaeloisa/Documents/CTA-DadosEspaciais/Area_estudo")
dir("C:/Users/veras_mariaeloisa/Documents/CTA-DadosEspaciais/Area_estudo/dados")

#arquivos
municipios_sp<-st_read("Municipios-sp.shp")
SaoPaulo<-st_read("SaoPaulo.shp")
AreaEstudo<-st_read("area_estudo_rodoanel.shp")
rodoanel<-st_read("08_Rodoanel.shp")
rural<-st_read('dados/01A_Zona_Rural.shp')
rios<-st_read('dados/05_Hidrografia_Rios.shp')
ucs<- st_read('dados/UC_32723.shp')
declividade<- raster('dados/DECLIVIDADE-Y-C.tif')


#projecao
 

municipios_sp<-st_transform(municipios_sp, 31983)
st_crs(municipios_sp)

SaoPaulo<-st_transform(SaoPaulo, 31983) 
st_crs(SaoPaulo)

AreaEstudo<-st_transform(AreaEstudo, 31983)
st_crs(AreaEstudo)

rodoanel<-st_transform(rodoanel, 31983)
st_crs(rodoanel)

ucs<- st_transform(ucs, 31983)
st_crs(ucs)

rios<-st_transform(rios, 31983)
st_crs(rios)

rural<-st_transform(rural, 31983)
st_crs(rural)

declividade<-projectRaster(declividade, crs="+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs ")
crs(declividade)
declividade



#plots
dev.new(height=10,weight=10)
plot(declividade)
plot(st_geometry(rural),add=T)
plot(st_geometry(SaoPaulo),add=T)
plot(st_geometry(AreaEstudo),add=T)
plot(rios,add=T)
plot(st_geometry(ucs),add=T)
par(mfrow=c(1,1))


###############
#MAPAS INICIAIS
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
tm_shape(municipios_sp)+tm_borders()+tm_grid(alpha=0.2,labels.rot= c(0,90),labels.inside.frame = FALSE)+tm_compass(position = c("right","top"))+tm_scale_bar()+tm_credits("Projecao SIRGAS 2000 \n Fonte: IBGE")+tm_layout(main.title = "Mapa de localizacao da area de estudo \n Municipios de Sao Paulo", main.title.position = "center", outer.margins = 0.05)+tm_add_legend(type = "fill",col="white",border.col = "black",labels = "Municipios de Sao Paulo")+tm_shape(SaoPaulo)+tm_polygons(col="firebrick1", border.col = "firebrick4",lwd=2)+tm_add_legend(type="fill", col="firebrick1",border.col = "firebrick4",labels = "Municipios de Sao Paulo")

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
  ) + tm_shape(AreaEstudo) + tm_polygons(col = "khaki1",border.col = "gold3", lwd = 2) + tm_add_legend( type = "fill",col = "khaki1", border.col = "gold3",labels = "Area de estudo")+tm_shape(rodoanel) +tm_lines(col = "firebrick3", lwd = 3)+tm_add_legend( type = "line",lwd=3,col = "firebrick3",labels = "Rodoanel") + tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "center")) + tm_credits(position = c("right", "center"), "Projecao SIRGAS 2000 \n Fonte: IBGE")

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


###############

# mask e crop e reclassificar a declividade
decliv_crop<- crop(declividade,AreaEstudo)
decliv_mask <- mask(declividade, AreaEstudo)

dev.new()
plot(decliv_crop)
plot(decliv_mask)
plot(st_geometry(AreaEstudo),add=T)
writeRaster(decliv_crop,"declividade_mask.tif", overwrite= T)


decliv_crop<-projectRaster(decliv_crop,crs="+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")
decliv_mask<-projectRaster(decliv_mask,crs="+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")

decliv_reclass_crop<-reclassify(decliv_crop, c(0,3,1,3,8,2,8,20,3,20,45,4,45,100,5))
decliv_reclass_mask<-reclassify(decliv_mask, c(0,3,1,3,8,2,8,20,3,20,45,4,45,100,5))

plot(decliv_reclass_crop)
plot(st_geometry(SaoPaulo),border="black",lwd=1.5,main="Declividade (pesos)")
plot(decliv_reclass_crop,add=T)
plot(st_geometry(AreaEstudo),border="red",lwd=3,add=T)

#mapas de declividade
######################
#mapa de declividade (%)
rural_raster

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
  
######################
#rasterizando os demais dados
 
##rural
  
rural_crop<-st_crop(rural,AreaEstudo)
plot(st_geometry(rural_crop),add=T)
rural_crop$fragilidad<- 1 #onde for zona rural, irei atribuir o peso 1
View(rural_crop)
rural_ras<-raster(extent(declividade))
crs(rural_ras)<-"+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
res(rural_ras)<-res(declividade)
rural_raster<-rasterize(rural_crop, rural_ras, field="fragilidad") 
plot(rural_raster)

  ##rios
rios_crop<-st_crop(rios,AreaEstudo)
plot(st_geometry(rios_crop), col="yellow", add=T)
plot(AreaEstudo,add=T)
rios_buffer<- st_buffer(rios_crop,30)
plot(st_geometry(rios_buffer))
rios_simpl<-st_simplify(rios_buffer,preserveTopology = T)
rios_simpl$fragilidad<- 1
plot(rios_simpl)
rios_ras<-raster(extent(declividade))
crs(rios_ras)<- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
res(rios_ras)<-res(declividade)
rios_raster<-rasterize(rios_simpl,rios_ras,field="fragilidad")
plot(rios_raster)  

##UCs
ucs_crop<-st_crop(ucs,AreaEstudo)
plot(st_geometry(ucs_crop))
ucs_crop$fragilidad<-1
ucs_ras<- raster(extent(declividade))
crs(ucs_ras)<- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
res(ucs_ras)<-res(declividade)
ucs_raster<- rasterize(ucs_crop, ucs_ras, field= "fragilidad")  
plot(ucs_raster)
plot(AreaEstudo)
plot(st_geometry(rios),add=T)
plot(rios_crop,col="red",add=T)



## analise multicritério
testee<- crop(ucs_raster+rural_raster+rios_raster,AreaEstudo)
plot(testee)
plot(rural_crop,col= NA, border="green3", lwd=2, add=T)
plot(ucs_crop, col=NA, border="blue",lwd=2,add=T)
plot(AreaEstudo, col=NA, border="red", lwd=2, add=T)


plot(st_geometry(ucs_crop),add=T)

testee_reclass<-reclassify(testee, c(0,1,1,1,2,2,2,3,3,4,5,5))
plot(testee_reclass, col= pal_mult_crit)
testee2<- crop(testee+rios_raster, AreaEstudo)
plot(testee2)
View(testee$layer==1)


frag_pot<- crop(ucs_raster+rios_raster+rural_raster+decliv_reclass_crop, AreaEstudo)
writeRaster(frag_pot,"frag_pot.tif", overwrite= T)



display.brewer.all()
pal_mult_crit <- c("#000000", "#fc3a38", "#0316fa", "#000000", "#acf9fe")
pal_test<- c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4")

dev.new()
paired = brewer.pal(n = 10, name = "Paired")
plot(frag_pot, col= paired)
plot(AreaEstudo, col=NA, border= "Red", lwd= 2,add=T)

#####

tm_shape(SaoPaulo) + tm_polygons(col = "grey96", border.col = "grey40") + tm_grid(
  alpha = 0.1,
  labels.rot = c(0, 90),
  labels.inside.frame = F
)  +
  tm_layout(
    main.title = "Mapa de restricoes ambientais\n da area de estudo",
    main.title.position = "center",
    outer.margins = 0.05
  ) + tm_add_legend(
    type = "fill",
    col = "grey96",
    border.col = "grey40",
    labels = "Municipio de Sao Paulo"
  ) +  
  tm_shape(AreaEstudo) + tm_polygons(border.col = "gold3", lwd = 5) + tm_add_legend( type = "fill",col = NA, border.col = "gold3",size=2,labels = "Area de estudo")+tm_shape(rodoanel) +tm_lines(col = "firebrick3", lwd = 3)+tm_add_legend( type = "line",border.lwd =2,col = "firebrick3",labels = "Rodoanel") + tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "center")) + tm_credits(position = c("right", "center"), "Projecao SIRGAS 2000 \n Fonte: IBGE")+
  tm_shape(frag_pot)+tm_raster(labels = c("1","2","3","4","5","6"),legend.show = T,palette = pal18,title = "Restricoes (pesos)")

plot(frag_pot, col=pal18)
plot(AreaEstudo,col=NA, border="red", lwd=2,add=T)
