#import data
data <- read.csv("C:\\Users\\catri\\OneDrive\\SaDa Sem 4\\Analisis Multivariat\\air_quality_dataset.csv")
print(head(data)) 

data_pca <- data[,3:15]  
data_pca <- data_pca[, !(names(data_pca) %in% c("Date", "Time"))] 
print(head(data_pca)) 

# Pre-processing
install.packages("psych")
library(psych)
sum(is.na(data))
p <- ncol(data)

data <- data[complete.cases(data), ]
numeric_data <- data[sapply(data, is.numeric)]  
scale_data <- scale(numeric_data)

# Uji Bartlett
bartlett_result <- cortest.bartlett(cor(numeric_data), n = nrow(numeric_data))
print(bartlett_result)

cor_matrix <- cor(scale_data)

# Uji kecukupan sampel
KMO(cor_matrix)

# korelasi parsial
install.packages("ppcor")
library(ppcor)  
part_corr = pcor(data_pca)
round(part_corr$estimate, 3) 

# --------- Principal Component Analysis
# Menghitung nilai eigen dan vektor eigen
eigen_results <- eigen(cor_matrix)
eigenvalues <- eigen_results$values
eigenvectors <- eigen_results$vectors
eigenvalues

# Varians kumulatif
sumvar <- sum(eigenvalues)
propvar <- (eigenvalues / sumvar) * 100  
cumvar <- cumsum(propvar) 
cumvar

pca_model <- prcomp(scale_data, center = TRUE, scale. = TRUE)
summary(pca_model)  
pca_model$rotation  
head(pca_model$x)   

# ---- **Visualisasi PCA**
library(factoextra)

# Scree Plot
fviz_eig(pca_model, addlabels = TRUE, barfill = "skyblue", 
         barcolor = "darkblue", linecolor = "red")

# Biplot PCA
fviz_pca_biplot(pca_model, geom.ind = "point", addEllipses = TRUE)

# Kontribusi Variabel terhadap PC1, PC2, PC3
fviz_contrib(pca_model, choice = "var", axes = 1, top = 5) + ggtitle("PC1")
fviz_contrib(pca_model, choice = "var", axes = 2, top = 5) + ggtitle("PC2")
fviz_contrib(pca_model, choice = "var", axes = 3, top = 5) + ggtitle("PC3")

# --------- Factor Analysis -----------
varcov <- cov(scale_data)
pc <- eigen(varcov)
print(pc$values)
print(pc$vectors)

# Parallel FA
library(psych)
fa_parallel <- fa.parallel(scale_data, fm = "ml", fa = "fa")
print(fa_parallel)

sp <- sum(pc$values[1:3])

L1 <- sqrt(pc$values[1]) * pc$vectors[, 1]
L2 <- sqrt(pc$values[2]) * pc$vectors[, 2]
L3 <- sqrt(pc$values[3]) * pc$vectors[, 3]

L <- cbind(L1,L2, L3)
print(L)

# Maximum Likelihood
fa_result <- fa(scale_data, nfactors = 3, rotate = "varimax", fm = "ml")

if (!is.null(fa_result$loadings)) {
  load_matrix <- as.matrix(fa_result$loadings)
  print(load_matrix)
  
  # Plot Factor Loadings
  plot(load_matrix[, c(1, 3)], type = "n", main = "Factor Loadings Plot", xlab = "Factor 1", ylab = "Factor 3")
  text(load_matrix[, c(1, 3)], labels = colnames(data), cex = 0.7)
  
  # Diagram Faktor
  fa.diagram(fa_result)
} else {
  print("Factor loadings tidak tersedia. Coba periksa jumlah faktor atau data yang digunakan.")
}

