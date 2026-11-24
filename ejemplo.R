library(ggplot2)
library(geodata)
################################################################################
#CARGAR DATOS
data <- read.csv("https://raw.githubusercontent.com/ccsosa/R_Examples/refs/heads/master/B_G.csv")

# Ensure 'pais' is treated as a categorical group
data$pais <- as.factor(data$pais)

################################################################################
#PART 0 (MAP)

world_shp <- geodata::world(resolution = 4,level=0,path = tempdir())
cols_5 <- c("green", "yellow", "blue", "orange", "red")

plot(
  world_shp,
  xlim = c(min(data$lon, na.rm = TRUE) - 5, max(data$lon, na.rm = TRUE) + 5),
  ylim = c(min(data$lat, na.rm = TRUE) - 5, max(data$lat, na.rm = TRUE) + 5),
  col = "gray90",
  border = "gray70"
)
points(
  data[, c("lon", "lat")],
  col = cols_5[as.numeric(data$pais)],
  pch = 16,
  cex = 0.8
)




# Scatter plot and correlation (PART 1)

#remove NA
df_clean <- na.omit(data[, c("bio_1", "bio_12", "pais")])
df_clean$pais <- factor(df_clean$pais)

# Convex hull
hulls <- do.call(rbind, lapply(split(df_clean, df_clean$pais), function(subdf) {
  if (nrow(subdf) >= 3) {
    subdf[chull(subdf$bio_1, subdf$bio_12), ]
  } else {
    NULL
  }
}))

ggplot(df_clean, aes(x = bio_1, y = bio_12, color = pais, fill = pais)) +
  geom_polygon(data = hulls, alpha = 0.05, linewidth = 0.01) +
  geom_point(aes(shape = pais), alpha = 0.85, size = 3.5) +
  scale_color_brewer(palette = "Dark2", name = "País") +
  scale_fill_brewer(palette = "Dark2", name = "País") +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3, 4)[seq_len(nlevels(df_clean$pais))], name = "País") +
  labs(
    x = "bio_1 (Temperatura Media Anual)",
    y = "bio_12 (Precipitación Anual)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 13)
  )

# Overall Spearman correlation
cor.test(data$bio_1, data$bio_12, method = "spearman")

# lm_1 <- lm(bio_12 ~ bio_1, data = data)
# 
# summary(lm_1)
# plot(lm_1)
################################################################################
#wilcox.test y boxplots (PART 2)
#subset
data_2_groups <- data[data$pais %in% c("CO","EC"),]
data_2_groups$pais <- factor(data_2_groups$pais,levels = c("CO","EC"))
boxplot(bio_12 ~ pais, data = data_2_groups,col=c("yellow","blue"))
        
wilcox.test(bio_12 ~ pais, data = data_2_groups)

################################################################################
#kruskal y boxplots (PART 3)
boxplot(bio_1 ~ pais, data = data,col=c("green","yellow","blue","orange","red"))
kruskal.test(bio_1 ~ pais, data = data)
################################################################################
#barplots (PART 4)
#initialize
data$bio_1_cat <- NA
#mediana bio12
median(data$bio_1,na.rm=T)

data$bio_1_cat[which(data$bio_1 >= median(data$bio_1,na.rm=T))] <- "TEMP_ALTA"
data$bio_1_cat[which(data$bio_1 < median(data$bio_1,na.rm=T))] <- "TEMP_BAJA"


#initialize
data$bio_12_cat <- NA
#median bio12
median(data$bio_12,na.rm=T) 
data$bio_12_cat[which(data$bio_12 >= median(data$bio_12,na.rm=T))] <- "PREC_ALTA"
data$bio_12_cat[which(data$bio_12 < median(data$bio_12,na.rm=T))] <- "PREC_BAJA"

tab_pais_temp <- table(data$bio_1_cat, data$pais)

print(tab_pais_temp)
barplot(
  tab_pais_temp,
  beside = F, # 
  col = c("#D95F02", "#1B9E77"),
  legend.text = TRUE,
  args.legend = list(x = "topright", title = ""),
  xlab = "País",
  ylab = "Frecuencia",
  main = ""
)

#difference in frequencies for categorical data
fisher.test(tab_pais_temp)

