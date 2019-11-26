## Para generar los mapas de concentraciones de metales en el río Bacanuchi
## Es importante resaltar que éste script depende de internet ya que los mapas base que provienen de Google
## se obtienen dinámicamente.
# Establece el "working directory" usando la carpeta del presente script
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# si no tienes estos packages hay que instalarlos con las siguientes lineas
# install.packages("ggplot2")
# install.packages("ggmap")
# install.packages("maptools")
# install.packages("manipulate")

## Carga los paquetes requeridos para correr el presente script
library("ggplot2")## paquete para generar gráficas
library("scales")## paquete para establecer la escala de las gráficas (tamaño de los puntos en el mapa)
library("plyr") ## paquete para calcular algunos estadísticos de los valores (media, prmedios, meximos, minimos, desviasión estandard,etc.)
library("reshape2") ## paquete para convertir el formato de las tablas de datos (wide,long)
library("ggmap")## paquete para generar plots con mapas de Google Maps
library("maptools")## paquete que permite la lectura y modificación de objetos espaciales (ejem. shapefiles)
library("manipulate")## paquete para manipular gráficas interactivamente
library(gtable)
library(grid)
library(ggthemes)

## Cambiar el nombre de los ejes "x" y "y"
labels <- c("long"="longitud",
            "lat"="latitud")

#Se carga la tabla de datos 
laTabla <-read.csv("muestrasxy_r2.csv", 
#laTabla <-read.csv("suelos_texturas2.csv",
                   header=TRUE,sep=",")

#Se establecen las columnas que contienen los datos a analizar
lasMalignidades <- colnames(laTabla)[-(1:7)]
losNombres <- function(esta_malignidad){
  if (esta_malignidad=="As"){
    return("Arsénico")
  }
  if (esta_malignidad=="Cd"){
    return("Cadmio")
  }
  if (esta_malignidad=="Cu"){
    return("Cobre")
  }
  if (esta_malignidad=="Fe"){
    return("Hierro")
  }
  if (esta_malignidad=="Hg"){
    return("Mercurio")
  }
  if (esta_malignidad=="Ni"){
    return("Níquel")
  }
  if (esta_malignidad=="Pb"){
    return("Plomo")
  }
  if (esta_malignidad=="Zn"){
    return("Zinc")
  }
  if (esta_malignidad=="Mn"){
    return("Manganeso")
  }
  if (esta_malignidad=="Al"){
    return("Aluminio")
  }
  if (esta_malignidad=="DNA"){
    return("DNA")
  }
  if (esta_malignidad=="CAL"){
    return("Calidad")
  }
  if (esta_malignidad=="pH"){
    return("Potencial de Hidrógeno")
  }
  if (esta_malignidad=="N"){
    return("Nitrógeno")
  }
  if (esta_malignidad=="P"){
    return("Fósforo")
  }
  if (esta_malignidad=="K"){
    return("Potasio")
  }
  if (esta_malignidad=="M.O"){
    return("Materia Orgánica")
  }
  
}
#Se calculan los datos estadísticos para cada sitio de muestreo (media)
df <- ddply(laTabla,.(Sitio,ID2),
            summarise,
            long=mean(long), 
            lat=mean(lat))


#Se convierte a factor (texto) los sitios de muestreo (sitio y ID2)
df$Sitio<-factor(df$Sitio)
df$ID2<-factor(df$ID2)

write.csv(df, file = "sedimentos.csv")
## Función que genera los intervalos que dividen los datos y poder 
## utilizar "scale_size_continuous" correctamente
losBrakes <- function(esta_malignidad){
  subconjuntoB <- subset(df, select=paste0(esta_malignidad,"_mean"))
  colnames(subconjuntoB)[1]<-"este"
  elMaximo <- max(subconjuntoB$este, na.rm = TRUE)
  elMinimo <- min(subconjuntoB$este, na.rm = TRUE)
## Este "if" se utiliza para remover los decimales en caso de que el valor máximo se mayor que 11
## y dejar sólo un decimal para los demás casos.
  if (elMaximo > 11){
    elMaximo <- floor (elMaximo )
    elMinimo <- ceiling (elMinimo)
  }else{
    elMaximo <- (floor (elMaximo * 10)/10)
    elMinimo <- (ceiling (elMinimo * 10)/10)
  }
  elCacho <- signif((elMaximo-elMinimo)/3,digits=1)
  estosBrakes <- c(elMinimo,elMinimo+elCacho,elMinimo+(2*elCacho),elMaximo)
  return(estosBrakes)
}

## Función que genera las nuevas columnas con los cálculos estdísticos para cada transecto
for (i in 1:length(lasMalignidades)){
  meanName = paste(lasMalignidades[i],"_mean",sep="")
  sdName = paste(lasMalignidades[i],"_sd",sep="")
  seName = paste(lasMalignidades[i],"_se",sep="")
  subconjunto <- subset(laTabla, select=c("long","lat","Sitio","ID2",
                                 lasMalignidades[i]))
  colnames(subconjunto)[5] <- "valor"
  meanVector <- ddply(subconjunto,.(Sitio,ID2),
                      summarise,
                      mean=mean(valor))
  df$mean <- meanVector$mean
  colnames(df)[(3*i)+2] <- meanName
  sdVector <- ddply(subconjunto,.(Sitio,ID2),summarise,sd = sd(valor))
  df$sd <- sdVector$sd
  colnames(df)[(3*i)+3] <- sdName
  seVector <- ddply(subconjunto,.(Sitio,ID2),summarise,se = sd(valor)/sqrt(length(valor)))
  df$se <- meanVector$mean
  colnames(df)[(3*i)+4] <- seName
}
# -Bacanuchi (-110.235559, 30.606059)
# -San Felipe de Jesús(-110.240012,29.858725)
# -El Molinito (-110.689983, 29.233621)
derrame <- read.csv(header=T, sep=",",
                    text='nombre,long,lat
                    Represo Tinajas,-110.339249,30.922409
                    ')
citiesA <- read.csv(header=T, sep=",",
                    text='nombre,long,lat
                    Cananea,-110.2892,30.98976
                    Arizpe,-110.1666,30.33594
                    Aconchi,-110.2251,29.82331
                    Ures,-110.3867,29.42744
                    Baviácora,-110.1615,29.71274
                    Banámichi,-110.2133766,30.0093697
                    ')

citiesB <- read.csv(header=T, sep=",",
                    text='nombre,long,lat
                    Bacanuchi,-110.235559,30.606059
                    San Felipe,-110.240012,29.858725
                    El Molinito,-110.689983, 29.233621
                    Hermosillo,-110.9559,29.07297
                    Huépac,-110.2134544,29.9108684
                    ')

## Genera el mapa base de Google (requiere coordenadas de la region)
map1s = get_map(location = c(lon=-110.482, lat=30.111), 
                zoom = 8, 
                maptype = 'satellite',
                scale = 2)
#maptype = 'terrain')
## Genera el ggmap a partir de el mapa base
ggmap1s <- ggmap(map1s, 
                 base_layer=ggplot(aes(x=long,y=lat), 
                                   data=laTabla),
                 extent = "normal", maprange=FALSE) + 
  ## Agrega las subcuencas desde el shapefile 'subcuencas_cg.shp' y
  ## define su éstilo (colour, linetype, alpha, size)
  geom_polygon(aes(x = long, 
                   y = lat, 
                   group = id), 
               data = fortify(readShapeSpatial('subcuencas_cg.shp')),
               colour = 'black', 
               linetype = "dotdash", 
               fill=NA, 
               alpha = .9, 
               size = .5)+
  ## Agrega los rios desde el shapefile 'rios_cg.shp' y define su éstilo (colour, size)
  geom_path(aes(x = long, y = lat, group=id), 
            data = fortify(readShapeSpatial('rios_cg.shp')),
            colour = 'cornflowerblue', 
            size = .9) +
  ## Establece los límites del mapa
  coord_map(xlim=c(-111.401, -109.308),
            ylim=c(29.05, 31.1))+
  ## Agrega los puntos de las ciudades específicas
  geom_point(data=citiesA, 
             shape=18,size=5, 
             color="white")+
  ## Agrega los nombres de las ciudades específicas
  geom_text(data = citiesA, 
            aes(label = nombre), 
            vjust = -0.3, 
            hjust=1.1,
            color="white", 
            size=7)+
  
  ## Agrega los puntos de las ciudades específicas
  geom_point(data=citiesB, 
             shape=18,size=5, 
             color="white")+
  ## Agrega los nombres de las ciudades específicas
  geom_text(data = citiesB, 
            aes(label = nombre), 
            vjust = -0.3, 
            hjust=0.5,
            color="white", 
            size=7)

ploteameEsta <-function(elemento){
  
  
  esteMapa <- ggmap1s + 
    ## Agrea los valores de concentración al mapa y define su estética (size,shape,fill) 
    geom_point(data=df,
               shape=21, 
               alpha=.7,
               colour="black", 
               aes_string(x="long", 
                          y="lat",
                          size=paste0(elemento,"_mean"),
                          bg="Sitio",
                          label="ID2"))+ 
    ## Define la escala cotinua de tamaños de los puntos
    ## los límites y brakes establecidos a partir de la concentración máxima y 
    ## mínima para cada elemento
    scale_size_continuous(range = c(4, 14),
                          #breaks=losBrakes(elemento), name= paste0("Concentración promedio\n",losNombres(elemento)," (mg/kg)"))+ 
                          breaks=losBrakes(elemento), name= paste0("Concentración\n (mg/kg)"))+
                          ## Define la escala de colores de los sitios de muestreo 
#     scale_fill_manual(values=c( 
#                                "firebrick4", 
#                                "red",
#                                "olivedrab1",
#                                "darkorange"),
    scale_fill_manual(values=c( 
      "blue", 
      "darkorange",
      "limegreen",
      "purple"),
                      name = "Sitios de Muestreo")+
    
    geom_point(data=derrame, 
               shape=24,size=5, 
               color="red",
               fill="yellow")+
    
    geom_text(data = derrame, 
              aes(label = nombre), 
              vjust = -0.3, 
              hjust=1.1,
              color="yellow", 
              size=7)+
    guides(size = guide_legend(override.aes = list(colour="black", 
                                                   fill="gray"), order =0),
           fill = guide_legend(override.aes = list(size = 6), order =1))+
    ## Define la estética del título, leyenda y ejes del gráfico
    theme(#axis.title.x=element_text(size=14), 
      #axis.title.y=element_text(size=14),
      line = element_blank(),
      #text = element_blank(),
      #             line = element_blank(),
      #             title = element_blank(),
      axis.line=element_blank(),axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      legend.key=element_blank(), 
      legend.key.width=unit(2,"line"),
      # Define la separación (altura) de los objetos de la leyenda
      
      legend.key.height=unit(2,"line"),
      legend.background=element_rect(linetype="solid",colour="grey60"),
      legend.key.size = unit(10,"lines"),
      legend.text=element_text(size=19),
      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),plot.background=element_blank(),
      legend.title=element_text(size=22),
      #plot.title=element_text(size=18, 
      #                        face="bold", 
      #                        vjust=1),
      #strip.text.x = element_blank(),
      strip.background = element_rect(colour="white", fill="white"),
      legend.position=c(.85,.18))
      

 
  return(esteMapa) 
}
## Agrega el picker de elementos
elementos <- c('Al','As','Cd','Cu','Fe','Hg','Mn','Ni','Pb','Zn')
#Elemento=picker("Al","As","Cd","Cu","Fe","Hg","Mn","Ni","Pb","Zn",initial="As"),

for(elemento in elementos){
  
  
    
    estePlot <- ploteameEsta(elemento)
    
#     gt = ggplotGrob(estePlot)
#     
#     # Get the combined legend
#     leg = gtable_filter(gt, "guide-box")
#     
#     # Get the individual legends - to get their widths and heights
#     leg1 = leg$grobs[[1]]$grobs[[1]]
#     leg2 = leg$grobs[[1]]$grobs[[2]]
#     
#     # Calculate widths and heights for border (Note: some margin widths are dropped from widths)
#     rectHeight = sum(leg1$heights + leg2$heights)
#     rectWidth = sum(unit.pmax(leg1$widths[3:5], leg2$widths[3:5]))
#     
#     # Create border with widths and heights 
#     rect <- rectGrob( width = rectWidth, height = rectHeight,
#                       gp = gpar(col = "black", fill = NA, lwd = 5), name = "gRect")
#     
#     # Create combined legend - i.e., legend plus border
#     leg <- grobTree(leg, rect)
#     
#     # Insert combined legend back into ggplot grob
#     gt$grobs[gt$layout$name == "guide-box"][[1]] <- leg
#     
#     # Draw it
#     grid.newpage()
#     grid.draw(gt)
    
    png(filename=paste0(getwd(),"/M_SedEsc_",elemento,".png"),width = 1100, height = 1100)
    plot(estePlot)
    dev.off()

}
for(elemento in elementos){
  system(paste0("mogrify -crop 932x1055+91+14 ",elemento,"*.png"))
}
## Función que sustituye los acrónimos de cada elemento y agrega su nombre completo 
## para el título de la leyenda






