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

## Cambiar el nombre de los ejes "x" y "y"
labels <- c("long"="longitud",
            "lat"="latitud")

#Se carga la tabla de datos 
laTabla <-read.csv("muestrasxy_r2.csv", 
#laTabla <-read.csv("suelos_texturas2.csv",
                   header=TRUE,sep=",")

#Se establecen las columnas que contienen los datos a analizar
lasMalignidades <- colnames(laTabla)[-(1:7)]

#Se calculan los datos estadísticos para cada sitio de muestreo (media)
df <- ddply(laTabla,.(Sitio,ID2),
            summarise,
            long=mean(long), 
            lat=mean(lat))


#Se convierte a factor (texto) los sitios de muestreo (sitio y ID2)
df$Sitio<-factor(df$Sitio)
df$ID2<-factor(df$ID2)

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

## Genera el mapa de Google de terreno y su estética (zoom y type)
map1t = get_map(location=c(lon=-110.482, 
                           lat=30.111),
               zoom=8,
               maptype="terrain")

## Genera el mapa incluyendo las subcuencas y ríos, así como su estética (colour, linetype,
## size y alpha)
ggmap1t <- ggmap(map1t, 
                 base_layer=ggplot(aes(x=long,
                                       y=lat),
                                   data=df),
                 extent="normal", 
                 maprange=FALSE, 
                 darken=.2)+ 
  ## Agrega las subcuencas desde el shapefile "subcuencas_cg.shp" y
  ## define su éstilo (colour, linetype, alpha, size)
  geom_polygon(aes(x=long, 
                   y=lat, 
                   group=id), 
               data=fortify(readShapeSpatial("subcuencas_cg.shp")),
               colour="black",
               linetype="dotdash", 
               fill=NA, 
               alpha=.9,
               size = .5)+
  ## Agrega los ríos desde el shapefile "rios_cg.shp" y define su éstilo (colour, size)
  geom_path(aes(x=long,
                y=lat,
                group=id), 
            data=fortify(readShapeSpatial("rios_cg.shp")),
            colour="cornflowerblue", 
            size = .9)+
  ## Establece los límites del mapa
  coord_map(xlim=c(-111.401,-109.308),
            ylim=c(29.1, 31.1))

## Genera el mapa de Google de terreno con acercamiento y su estética (zoom y type)
map2t = get_map(location = c(lon=-110.25, lat=30.5),
               zoom=9,
               maptype="terrain")

## Genera el mapa tomando de las subcuencas y ríos, así como su estética (colour, linetype, 
## size y alpha)
ggmap2t <- ggmap(map2t, 
                 base_layer=ggplot(aes(x=long,y=lat), 
                                   data=df),
                extent="normal", 
                maprange=FALSE, 
                darken=.2) +
  ## Agrega las subcuencas desde el shapefile "subcuencas_cg.shp" y
  ## define su estilo (colour, linetype, alpha, size)
  geom_polygon(aes(x=long, 
                   y=lat,
                   group=id), 
               data=fortify(readShapeSpatial("subcuencas_cg.shp")),
               colour="black", 
               linetype="dotdash",
               fill=NA, 
               alpha=.9,
               size=.5)+
  ## Agrega los ríos desde el shapefile "rios_cg.shp" y define su éstilo (colour, size)
  geom_path(aes(x=long,
                y=lat,
                group=id), 
            data=fortify(readShapeSpatial("rios_cg.shp")),
            colour="cornflowerblue",
            size=.9) +
  ## Establece los límites del mapa
  coord_map(xlim=c(-110.8,-109.7),
            ylim=c(30.2, 31))

## Genera el mapa de Google de satélite y su estética (zoom y type)
map1s=get_map(location=c(lon=-110.482, lat=30.111),
                zoom=8,
                maptype="satellite")

# Estos x y s se obtuvieron de http://maps.googleapis.com/maps/api/geocode, y  
# para que no falle en tiempo de ejecución, usamos los valores directamente.
citiesA <- read.csv(header=T, sep=",",
                    text='nombre,long,lat
                    Cananea,-110.2892,30.98976
                    Hermosillo,-110.9559,29.07297
                    Arizpe,-110.1666,30.33594
                    Aconchi,-110.2251,29.82331
                    Ures,-110.3867,29.42744
                    Baviácora,-110.1615,29.71274
                    Banamichi,-110.2133766,30.0093697
                    ')

citiesB <- read.csv(header=T, sep=",",
                    text='nombre,long,lat
                    Tlahuichopa,-110.1621112,30.3451358
                    El Molinote,-110.1316973,29.6512333
                    Mazocahui,-110.1185701,29.5380084
                    La Estancia,-110.2113888,29.7933332
                    La Volantina,-111.0047793,29.0579078
                    Huepac,-110.2134544,29.9108684
                    ')

## Genera el mapa con las subcuencas y ríos, y define la estética (colour, linetype,
## size y alpha)
ggmap1s<-ggmap(map1s,
               base_layer=ggplot(aes(x=long,
                                       y=lat), 
                                   data=df),
                 extent="normal",
                 maprange=FALSE)+
  ## Agrega las subcuencas desde el shapefile 'subcuencas_cg.shp' y
  ## define su estilo (colour, linetype, alpha, size)
  geom_polygon(aes(x=long,
                   y=lat,
                   group=id), 
               data=fortify(readShapeSpatial("subcuencas_cg.shp")),
               colour="grey90",
               linetype="dotdash",
               fill=NA,
               alpha=1,
               size=.5)+
  ## Agrega los ríos desde el shapefile "rios_cg.shp" y 
  ## define su estilo (colour, size)
  geom_path(aes(x=long,
                y=lat,
                group=id), 
            data=fortify(readShapeSpatial("rios_cg.shp")),
            colour="cornflowerblue",
            size=.9)+
  ## Establece los límites del mapa
  coord_map(xlim=c(-111.401,-109.308),
            ylim=c(29.1,31.1))+ 
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
            size=5)+
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
            size=5) 

## Genera el mapa de Google de satélite con acercamiento y su estética (zoom y type)
map2s=get_map(location = c(lon=-110.25, lat=30.5),
                zoom=9,
                maptype="satellite")
## Genera el mapa incluyendo las subcuencas y ríos, así como su estética (colour, linetype,
## size y alpha)
ggmap2s <- ggmap(map2s, 
                 base_layer=ggplot(aes(x=long,y=lat), 
                                   data=df),
                 extent="normal", 
                 maprange=FALSE) +
  ## Agrega las subcuencas desde el shapefile 'subcuencas_cg.shp' y
  ## define su estilo (colour, linetype, alpha, size)
  geom_polygon(aes(x=long,
                   y=lat, 
                   group=id), 
               data=fortify(readShapeSpatial("subcuencas_cg.shp")),
               colour="grey90",
               linetype="dotdash", 
               fill=NA, 
               alpha=1,
               size=.5)+
  ## Agrega los rios desde el shapefile 'rios_cg.shp' y 
  ## define su estilo (colour, size)
  geom_path(aes(x=long,
                y=lat, 
                group=id), 
            data=fortify(readShapeSpatial("rios_cg.shp")),
            colour="cornflowerblue",
            size=.9) +
  ## Establece los límites del mapa
  coord_map(xlim=c(-110.8, -109.7),
            ylim=c(30.2, 31))+
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
            size=5)+
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
            size=5) 

## Función que se utiliza para poder manipular el picker y decidir que tipo de zoom (cerca o lejos) 
## y mapa (terreno o satélite) utilizaremos
cual_map <- function (zoom,base){
  if (zoom=="cerca"){
    if (base=="terrain"){
      return(ggmap2t)
    }else if(base=="satellite"){
      return(ggmap2s)
    }
  }
  if (zoom=="lejos"){
    if (base=="terrain"){
      return(ggmap1t)
    }else if(base=="satellite"){
      return(ggmap1s)
    }
  }
}

## Función que sustituye los acrónimos de cada elemento y agrega su nombre completo 
## para el título de la leyenda
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
    return("Alumnio")
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

## Función que permite manipular interactivamente el mapa, permitiendo 
## escoger el mapa base (satellite o terrain), cambiar el zoom y el elemento
manipulate(
  cual_map(Zoom,Base)+
    ## Agrega los valores del promedio de concentración al mapa y define su estética 
    ## (size,shape,colour, bg) 
          geom_point(data=df,
                     shape=21, 
                     alpha=.7,
                     colour="black", 
                     aes_string(x="long", 
                               y="lat",
                               size=paste0(Elemento,"_mean"),
                               bg="Sitio",
                               label="ID2"))+ 
          ## Define la escala cotinua de tamaños de los puntos
          ## los límites y brakes establecidos a partir de la concentración máxima y 
          ## mínima para cada elemento
          scale_size_continuous(range = c(4, 14),
                          breaks=losBrakes(Elemento))+ 
          ## Define la escala de colores de los sitios de muestreo 
          scale_fill_manual(values=c("yellow2", 
                                     "firebrick4", 
                                     "red",
                                     "olivedrab1",
                                     "darkorange"),
                      name = "Sitios de Muestreo")+
          ## Sustituye la leyenda (default) por una nueva con la siguiente estética 
          ## (size, colour, fill) y establece el orden de las leyendas
          guides(fill=guide_legend(override.aes=list(size = 5), order = 1),
                 size=guide_legend(override.aes=list(colour="black", 
                                                     fill="gray")), order = 2)+
          # Define el título de la leyenda de tamaños
          labs(size="Concentración (mg/kg)")+
    
          ## Define la estética del título, leyenda y ejes del gráfico
          theme(axis.title.x=element_text(size=14,
                                          vjust=0.5), 
                axis.title.y=element_text(size=14,
                                          vjust=1.2),
                axis.text.x=element_text(size=12,
                                         colour="grey30"),
                axis.text.y=element_text(size=12,
                                         colour="grey30"),
                legend.key=element_blank(), 
                legend.text=element_text(size=15),
                legend.title=element_text(size=15),
                plot.title=element_text(size=18,
                                        face="bold", 
                                        vjust=1))+
          ## Define el nombre de los ejes "x" y "y"
          xlab(expression(Longitud))+
          ylab(expression(Latitud))+
          ggtitle(paste(losNombres(Elemento),
                        "(concentración promedio)\n26/05/2015 - 8/06/2015")),
    #Elemento=picker("Al","As","Cd","Cu","Fe","Hg","Mn","Ni","Pb","Zn",initial="As"),
    Elemento=picker(as.list(lasMalignidades)),
    Zoom=picker("lejos","cerca"),
    Base=picker("terrain","satellite"))