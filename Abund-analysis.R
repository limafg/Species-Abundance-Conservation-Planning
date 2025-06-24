# Abundance analysis

## Library ----
library(openxlsx)
library(terra)
library(readr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(ggplot2)
library(ggrepel)

## Additional functions--

regressao_forestplot <- function(base, baseuso, resposta, preditoras) {
  # Carregar pacotes
  require(ggplot2)
  require(broom)
  require(dplyr)
  require(car)
  require(glue)
  
  # Verificações
  if (!(resposta %in% colnames(base))) stop("A variável resposta não está no dataframe 'base'")
  if (!all(preditoras %in% colnames(baseuso))) stop("Uma ou mais preditoras não estão no dataframe 'baseuso'")
  
  # Unir dataframes
  dados <- cbind(base[resposta], baseuso[preditoras])
  colnames(dados)[1] <- resposta
  
  # Fórmula
  formula_reg <- as.formula(paste(resposta, "~", paste(preditoras, collapse = " + ")))
  
  # Modelo
  modelo <- lm(formula_reg, data = dados)
  sum_model <- summary(modelo)
  
  # R² e ANOVA
  r2 <- round(sum_model$r.squared, 3)
  f_stat <- round(sum_model$fstatistic[1], 2)
  gl1 <- sum_model$fstatistic[2]
  gl2 <- sum_model$fstatistic[3]
  p_valor_anova <- formatC(pf(f_stat, gl1, gl2, lower.tail = FALSE), digits = 3, format = "f")
  
  # Tidy + VIF
  tidy_model <- tidy(modelo, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(significativo = ifelse(p.value < 0.05, "Significativo", "Não significativo"))
  
  vif_valores <- vif(modelo)
  tidy_model$vif <- vif_valores[tidy_model$term]
  
  # Forest plot
  ggplot(tidy_model, aes(x = estimate, y = reorder(term, estimate),
                         xmin = conf.low, xmax = conf.high, color = significativo)) +
    geom_point(size = 3) +
    geom_errorbarh(height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_text(aes(label = paste0("VIF = ", round(vif, 2))), 
              hjust = 0, vjust = 1.5, size = 3.2, color = "black") +
    scale_color_manual(values = c("Significativo" = "darkred", "Não significativo" = "gray40")) +
    labs(
      title = glue("Forest Plot - Regressão de {resposta}"),
      subtitle = glue("R² = {r2} | F({gl1}, {gl2}) = {f_stat}, p = {p_valor_anova}"),
      x = "Estimativa (com IC 95%)",
      y = "Variáveis preditoras"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

### Data processing

## Input Data

#raw mammal data
data=read.xlsx("mami-sp.xlsx",1)
names(data)
unique(data$Especie)

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
names(base.expand)
base.expand$Point=as.character(base.expand$Point)

#adjusting the number of rows; some cameras did not work during the sampling period,
#there were no record of group of interest or there were only false records, 
#so it was added the missing 
nrow(base.expand)

empty=c("P2",as.numeric(rep(0,33)))
base.expand=rbind(base.expand,empty)

empty=c("P11",as.numeric(rep(0,33)))
base.expand=rbind(base.expand,empty)

empty=c("P15",as.numeric(rep(0,33)))
base.expand=rbind(base.expand,empty)

empty=c("P18",as.numeric(rep(0,33)))
base.expand=rbind(base.expand,empty)

empty=c("P22",as.numeric(rep(0,33)))
base.expand=rbind(base.expand,empty)

empty=c("P25",as.numeric(rep(0,33)))
base.expand=rbind(base.expand,empty)

#sort the site ID or Point (ex.: P1, P2, P3... P20... P33... P56)
base.expand <- base.expand %>%
  mutate(numero = parse_number(Point)) %>%
  arrange(numero) %>%
  select(-numero)

#checking if it worked
base.expand$Point
base$site

#adding to the data frame base.expand (Point and species' frequencies per point) the coordinates
base.expand$long=base$Long
base.expand$lat=base$Lat

write.xlsx(base.expand, "site-freq.xlsx")


## Processing land use and cover data
base=read.xlsx("site-freq.xlsx")

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

write.xlsx(use.cover, "uso-val.xlsx")

#Analise preliminar do uso

pca_uso <- PCA(baseuso[,4:14], scale.unit = TRUE, graph = FALSE)

# Extrair scores dos indivíduos e loadings das variáveis
scores=data.frame(pca_uso$ind$coord[, 1:2])  # Primeiro e segundo eixo
uso.val=read.xlsx("uso-val.xlsx")
scores$Local=uso.val$Local

colnames(scores) <- c("PC1", "PC2")
scores$Label <- baseuso$Point  # Usa os nomes das linhas como labels

loadings <- data.frame(pca_uso$var$coord[, 1:2])  # Primeiro e segundo eixo
colnames(loadings) <- c("PC1", "PC2")
loadings$Var <- rownames(loadings)

#plot

# Criar biplot com ggplot2
var_exp <- pca_uso$eig[1:2, 2]
loadings[,1:2]=2*loadings[,1:2]

ggplot() +
  geom_text_repel(data = scores, aes(x = PC1, y = PC2, label = Label, color = Local)) +  # Labels dos pontos
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.03, "npc")), color = "black") +  # Vetores dos loadings
  geom_text_repel(data = loadings, aes(x = PC1, y = PC2, label = Var), color = "black") +  # Labels dos loadings
  theme_minimal() +
  labs(
    x = paste0("PC1 (", round(var_exp[1], 2), "%)"),  # Inclui a variância explicada
    y = paste0("PC2 (", round(var_exp[2], 2), "%)")   # Inclui a variância explicada
  ) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),  # Eixos em preto
    axis.title = element_text(color = "black")  # Labels dos eixos em preto
  ) +
  scale_x_continuous(expand = expansion(mult = 0)) +  # Mantém a origem visível
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_color_manual(values = c("EntPESC" = "darkgoldenrod", "PESCaN" = "darkorange", "Corredor" = "red", "PEMA" = "darkgreen"))

baseuso$pc1=scores$PC1
baseuso$pc2=scores$PC2
names(baseuso)
base

# Local richness
rich=base
names(rich)
View(rich)
rich=rich[,c(4,5,7,9:13,15:18,20:23,25:36)]
View(rich)
rich[,1:28]=1*(rich[,1:28]>0)
S=rowSums(rich)
base$S=S


names(baseuso)
regressao_forestplot(base, baseuso, resposta = "S", preditoras = c("formacao_campestre", "agua", "pastagem","agricultura"))

regressao_forestplot(base, baseuso, resposta = "Canis_lupus_familiaris", preditoras = c("formacao_campestre", "agua", "pastagem","agricultura"))
