#'Get spatial lidar information
#'@description Combines the coordinates of the lidar stations into an sf object.
#'@author Marieke Dirksen
#'@export
get_lidar_locations<-function(){

#FINO website: https://www.fino-offshore.de/en/
#Add FINO2 location (east of Denmark)
#55 Â° 00'24,94'' N 13 Â° 09'15,08'' E

#Look for FINO3 data (west of Denmark)

#Download FINO1 data and location, register at:
#https://www.fino1.de/en/news-data/live-data.html

#TNW epsg 25831
TNWA<-c(name="TNWA",lat=667077,lon=5988551)
TNWB<-c(name="TNWB",lat=667034,lon=5988952)

#HKN epsg 25831
HKNA<-c(name="HKNA",lat=583943,lon=5838265)
HKNB<-c(name="HKNB",lat=583957,lon=5838577)
crs.hkn<-sp::CRS("+init=epsg:25831")

df.hkn<-data.frame(rbind(HKNA,HKNB,TNWA,TNWB))
df.hkn$lat<-as.numeric(as.character(df.hkn$lat))
df.hkn$lon<-as.numeric(as.character(df.hkn$lon))
sp::coordinates(df.hkn)<-~lat+lon
raster::crs(df.hkn)<-crs.hkn
df.hkn<-sp::spTransform(df.hkn,sp::CRS("+init=epsg:4326"))

#HKZ epsg 32631 coordinates (web projection)
HKZA<-c(name="HKZA",lat=568793,lon=5795664)
HKZB<-c(name="HKZB",lat=568792,lon=5793671)
crs.hkz<-sp::CRS("+init=epsg:32631")

df.hkz<-data.frame(rbind(HKZA,HKZB))
df.hkz$lat<-as.numeric(as.character(df.hkz$lat))
df.hkz$lon<-as.numeric(as.character(df.hkz$lon))
sp::coordinates(df.hkz)<-~lat+lon
raster::crs(df.hkz)<-crs.hkz
df.hkz<-sp::spTransform(df.hkz,sp::CRS("+init=epsg:4326"))

#standard WGS84 coordinates
Cabauw<-c(name="Cabauw",lat=51.971,lon=4.927)
ANH<-c(name="ANH",lat=56.59566389,lon=11.15277778)
WMR<-c(name="WMR",lat=56.440677,lon=8.150692)
LEG<-c(name="LEG",lat=51.93,lon=3.67)
EPL<-c(name="EPL",lat=52.00,lon=3.27)
Fino1<-c(name="Fino1",lat=54.01,lon=6.59)
Fino2<-c(name="Fino2",lat=55.007,lon=13.153)
Fino3<-c(name="Fino3",lat=55.195,lon=7.158)
Furgo1<-c(name="Furgo1",lat=51.71556,lon=3.019157)
Furgo2<-c(name="Furgo2",lat=51.650032,lon=2.9422162)
Stellendam<-c(name="Stel.",lat=51.82629,lon=4.03796)
Eemshaven<-c(name="Eem.",lat=53.464833, lon=6.739428)
Daarle<-c(name="Daarle",lat=52.418858, lon=6.536362)
BSA<-c(name="BSA",lat=51.69978, lon=3.05666 )
BSB<-c(name="BSB",lat=51.72636, lon=2.96559)

df.lidar<-data.frame(rbind(Cabauw,ANH,WMR,LEG,EPL,Fino1,Fino2,Fino3,Furgo1,Furgo2,
                           Stellendam,Eemshaven,Daarle,BSA,BSB))
df.lidar$lat<-as.numeric(as.character(df.lidar$lat))
df.lidar$lon<-as.numeric(as.character(df.lidar$lon))
sp::coordinates(df.lidar)<-~lon+lat
raster::crs(df.lidar)<-sp::CRS("+init=epsg:4326")

df.lidar<-rbind(df.lidar,df.hkz,df.hkn)
sf_lidar<-sf::st_as_sf(df.lidar)

return(sf_lidar)
}
