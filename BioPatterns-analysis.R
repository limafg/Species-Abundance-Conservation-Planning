### Biodiversity patterns analysis

## Library ----
library(openxlsx)
library(terra)
library(readr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(ggplot2)
library(ggrepel)

# Read here the mammal data
base=read.xlsx("site-freq.xlsx")

# Read here land use and cover
use.cover=read.xlsx("use_val.xlsx")

## Additional functions--

regressao_forestplot <- function(base, use.cover, dependent, predictors) {
  require(ggplot2)
  require(broom)
  require(dplyr)
  require(car)
  require(glue)
  
#checking
  if (!(dependent %in% colnames(base))) stop("não está no dataframe 'base'")
  if (!all(predictors %in% colnames(use.cover))) stop("Uma ou mais preditoras não estão no dataframe 'use.cover'")
  
#unit dataframes
  variables <- cbind(base[dependent], use.cover[predictors])
  #colnames(dados)[1] <- dependent
  
  # Formula
  formula_reg <- as.formula(paste(dependent, "~", paste(predictors, collapse = " + ")))
  
  # Model
  model <- lm(formula_reg, data = variables)
  sum_model <- summary(model)
  
  # R² and ANOVA
  r2 <- round(sum_model$r.squared, 3)
  f_stat <- round(sum_model$fstatistic[1], 2)
  gl1 <- sum_model$fstatistic[2]
  gl2 <- sum_model$fstatistic[3]
  p_value_anova <- formatC(pf(f_stat, gl1, gl2, lower.tail = FALSE), digits = 3, format = "f")
  
  # Tidy + VIF
  tidy_model <- tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(Significant = ifelse(p.value < 0.05, "Significant", "Non-significant"))
  
  vif_valores <- vif(model)
  tidy_model$vif <- vif_valores[tidy_model$term]
  
  # Forest plot
  ggplot(tidy_model, aes(x = estimate, y = reorder(term, estimate),
                         xmin = conf.low, xmax = conf.high, color = Significant)) +
    geom_point(size = 3) +
    geom_errorbarh(height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_text(aes(label = paste0("VIF = ", round(vif, 2))), 
              hjust = 0, vjust = 1.5, size = 3.2, color = "black") +
    scale_color_manual(values = c("Significativo" = "darkred", "Não significativo" = "gray40")) +
    labs(
      title = glue("Forest Plot - Regressão de {dependent}"),
      subtitle = glue("R² = {r2} | F({gl1}, {gl2}) = {f_stat}, p = {p_value_anova}"),
      x = "Estimate (with IC 95%)",
      y = "Predictors"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


### Preliminary land use analysis

pca_use <- PCA(use.cover[,4:14], scale.unit = TRUE, graph = FALSE)

#extracting scores and loadings
scores=data.frame(pca_use$ind$coord[, 1:2]) #two main axis, first and second
head(scores)

#table of land use and cover for each point
use.val=read.xlsx("use_val.xlsx")
use.cover=read.xlsx("use_val.xlsx")

#naming the columns of scores and giving it another one
colnames(scores)=c("PC1", "PC2")
head(scores)

#I created a new column named Site directly in use-val.xlsx
scores$Local=use.val$Local
scores$Label=use.cover$Point #relate the row names with the points (P1, P2... P30)
head(scores)

loadings <- data.frame(pca_use$var$coord[, 1:2]) #first and second axis
colnames(loadings) <- c("PC1", "PC2")

#switching the row names (classes of land use and cover) from Portuguese to English

  # agricultura = Agriculture; agua = Water; APP = PPA; area_urbana = Urban area; 
  # Campo Rupestre = Rupestrian grassland; floresta estacional decidual = Seasonal forest;
  # formacao_campestre = Grassland; formacao_savanica = Savanna;
  # fragmentos_florestais_do_cerrado = Cerrado forest; pastagem = Pastureland;
  # solo_exposto = Exposed soil

rownames(loadings)=c("Agriculture",	"Water",	"PPA",	"Urban area",	"Rupestrian grassland",	"Seasonal forest", "Grassland",	"Savanna",	"Cerrado forest",	"Pastureland",	"Exposed soil")
row.names(loadings)
loadings$Var=rownames(loadings)
loadings

#plot

#creating a biplot with ggplot2
var_exp <- pca_use$eig[1:2, 2]
loadings[,1:2]=2*loadings[,1:2]

biplot.use <- ggplot() +
  geom_text_repel(data = scores, aes(x = PC1, y = PC2, label = Label, color = Local)) + #labels of the points
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.03, "npc")), color = "black") + #vetors of the loadings
  geom_text_repel(data = loadings, aes(x = PC1, y = PC2, label = Var), color = "black") + #labels of the loadings
  theme_minimal() +
  labs(
    x = paste0("PC1 (", round(var_exp[1], 2), "%)"),#includes the explained variance in PC1
    y = paste0("PC2 (", round(var_exp[2], 2), "%)") #includes the explained variance in PC2
  ) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8), #axis in black
    axis.title = element_text(color = "black")  #labels of the axis in black
  ) +
  scale_x_continuous(expand = expansion(mult = 0)) +  #keeps the origin visible
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_color_manual(values = c("Surrounding" = "darkgoldenrod", "PESCaN" = "darkorange", "Corridor" = "red", "PEMA" = "darkgreen"))

biplot.use

ggsave(filename = "Results\\biplot-use.jpeg", plot = biplot.use, width = 25, height = 15, units = "cm", dpi = 300)

#adding the two main axis to use.cover data frame
use.cover$pc1=scores$PC1
use.cover$pc2=scores$PC2

#switching the col names (classes of land use and cover) from Portuguese to English
colnames(use.cover)

colnames(use.cover)=c("Point", "Long", "Lat", "Agriculture",	"Water",	"PPA",	"Urban area",	"Rupestrian grassland",	"Seasonal forest", "Grassland",	"Savanna",	"Cerrado forest",	"Pastureland",	"Exposed soil", "PC1", "PC2")
colnames(use.cover)

# Local richness

#base is a data frame containing: Point, all mammal species and their frequencies for each Point
#and coordinates

rich=base
names(rich)
View(rich)

#selecting the columns containing information on only native mammals

#para facilitar, futuramente criar uma coluna onde as spp. sejam id. com nativas e n-nativas
rich=rich[,c(3,4,6:15,18:36)]
names(rich)

#transforming frequency values into presence/absence
ncol(rich)
rich[,1:31]=1*(rich[,1:31]>0)
S=rowSums(rich)
base$S=S

#now base contains: Point, all mammal species and their frequencies for each Point, coordinates
#and S = native mammal richness for each Point
View(base)

#regression analysis
names(use.cover)
names(base)

sp.richness=regressao_forestplot(base, use.cover, dependent = "S", predictors = c("Grassland", "Water", "Pastureland","Agriculture"))
sp.richness

ggsave(filename = "Results\\riqueza.jpeg", plot = sp.richness, width = 25, height = 15, units = "cm", dpi = 300)

dog=regressao_forestplot(base, use.cover, dependent = "Canis_lupus_familiaris", predictors = c("PPA", "Water", "Pastureland","Agriculture"))
dog

ggsave(filename = "Results\\cachorro.jpeg", plot = dog, width = 25, height = 15, units = "cm", dpi = 300)

catingueiro=regressao_forestplot(base, use.cover, dependent = "Subulo_gouazoubira", predictors = c("Seasonal.forest", "Water", "Cerrado.forest","PPA"))
catingueiro

ggsave(filename = "Results\\catingueiro.jpeg", plot = catingueiro, width = 25, height = 15, units = "cm", dpi = 300)

### Multispecies regression
