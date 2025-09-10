## Library ----
library(openxlsx)
library(hms)

### Data processing

## Input Data

#raw mammal data
data=read.xlsx("mami-sp.xlsx",1)
names(data)
unique(data$Especie)

  #data independence

str(data) #date and hour are being read as character

  #making date readable to R
head(data$Data)
data$Data <- as.Date(as.numeric(data$Data), origin = "1899-12-30")
head(data$Data)

  #making hour readable to R
head(data$Hora, 10)

data$Hora <- as_hms(as.numeric(data$Hora)* 86400)
data$Hora <- format(as.POSIXct(data$Hora, origin = "1970-01-01"), "%H:%M:%S")
head(data$Hora, 10)

  #joining Data and Hora columns
data$Time <- as.POSIXct(
  paste(data$Data, as.character(data$Hora)),
  format = "%Y-%m-%d %H:%M:%S"
)

View(data)

  #species/hour filter
data.indep=data %>%
  arrange(Especie, Time) %>%
  group_by(Especie) %>%
  filter(
    is.na(lag(Time)) | difftime(Time, lag(Time), units = "hours") >= 1
  ) %>%
  ungroup()

View(data.indep)

#coordinates
latlong=read.xlsx("LatLong.xlsx",1)

#land use and cover
uso=rast("uso-solo/uso_solo_pescan_pema_2023.tif")
plot(uso)

## Creating a base table for analysis

#creating a data frame with site ID, latitude and longitude
base=data.frame(site=latlong$ID, Lat=latlong$Lat, Long=latlong$Long)
base

#extracting the species names from the raw data without replicates using the function unique()
spp=unique(data.indep$Especie)
spp

#adding to the data frame named "base" the species names in columns 
for (especie in spp) {
  base[[especie]] = 0
}
names(base)

#extracting the sites from the raw mammal data without replicates using the function unique()
site.ID=unique(data.indep$ID_ponto)
site.ID

#attributing to each site their respective species frequency
for (p in site.ID){
  pt=data.indep[data.indep$ID_ponto==p,]
  print(table(pt$Especie))
  
}

## Doing the same steps differently

#creating a data frame with site ID and species
freq.species <- as.data.frame(table(data.indep$ID_ponto, data.indep$Especie))
head(freq.species)

#giving names to the columns
colnames(freq.species) <- c("Point", "Species", "Frequency")
head(freq.species)

#rearranging data for each species to be placed in a column

base.expand <- freq.species %>%
  #the function pivot_wider() transpose the data of interest from rows to columns
  pivot_wider(names_from = Species, values_from = Frequency, values_fill = list(Frequency = 0))

base.expand=as.data.frame(base.expand)
names(base.expand)
base.expand$Point=as.character(base.expand$Point)

#sort the site ID or Point (ex.: P1, P2, P3... P20... P33... P56)
base.expand <- base.expand %>%
  mutate(numero = parse_number(Point)) %>%
  arrange(numero) %>%
  select(-numero)

base.expand$Point
View(base.expand)

#adding to the data frame base.expand (Point and species' frequencies per point) the coordinates
base.expand$long=base$Long
base.expand$lat=base$Lat

write.xlsx(base.expand, "site-freq.xlsx")

base=base.expand

  ####

## Processing land use and cover data

classes=unique(uso)
use.cover=data.frame(Point=base$Point,Long=base$long,Lat=base$lat)
classes[,1]
for (var in classes[,1]) {
  use.cover[[var]] <- 0
}
names(use.cover)

#plotting the sampling sites
use.cover$Long=as.numeric(use.cover$Long)
use.cover$Lat=as.numeric(use.cover$Lat)
use.cover_vect=vect(use.cover, geom = c("Long", "Lat"), crs = "EPSG:32722") 
plot(use.cover_vect)

#creating a buffer of land use and cover around each sampling point
for (i in 1:nrow(use.cover)){
  print(i)
  point <- use.cover_vect[i, ] #selects a point
  buffer_point <- buffer(point, width = 1000) #creates a buffer around the point (1 km)
  m=crop(uso,buffer_point);m=mask(m,buffer_point)
  g=unique(m)
  
  #give the proportion of each class of use and cover within each buffer
  for (j in g[,1]){
    use.cover[i,j]=global(m==j,"mean",na.rm=TRUE)
  }
}

write.xlsx(use.cover, "use_val.xlsx")