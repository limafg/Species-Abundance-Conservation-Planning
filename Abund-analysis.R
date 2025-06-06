# Abundance analysis

install.packages("openxlsx")

## Bibliotecas ----
library(openxlsx)
library(terra)
library(tidyr)


## Additional functions--


## Data processing--

# Input Data

#raw mammal data
data=read.xlsx("mami-sp.xlsx",1)
names(data)

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
spp=unique(data$Especie)
spp

#adding to the data frame named "base" the species names in columns 
for (especie in spp) {
  base[[especie]] = 0
}
names(base)

#extracting the sites from the raw mammal data without replicates using the function unique()
site.ID=unique(data$ID_ponto)

#attributing to each site their respective species frequency
for (p in site.ID){
  pt=data[data$ID_ponto==p,]
  print(table(pt$Especie))
  
}

## Doing the same steps differently

#creating a data frame with site ID and species
freq.species <- as.data.frame(table(data$ID_ponto, data$Especie))
head(freq.species)

#giving names to the columns
colnames(freq.species) <- c("Point", "Species", "Frequency")
head(freq.species)

#rearranging data for each species to be placed in a column

base.expand <- freq.species %>%
  #the function pivot_wider() transpose the data of interest from rows to columns
pivot_wider(names_from = Species, values_from = Frequency, values_fill = list(Frequency = 0))
base.expand=as.data.frame(base.expand)
print(base.expand)
