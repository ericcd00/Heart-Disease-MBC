################################################################################

# TITLE: APLICATION OF MODEL-BASED CLUSTERING IN HEART DISEASE PATIENTS. CODE
# DESCRIPTION: Code used for Eric's Master Thesis. 

################################################################################


################################# Libraries ####################################

if(!require(gridExtra)){
  install.packages("gridExtra") #Export tables
  library(gridExtra)
}

if(!require(ggplot2)){
  install.packages("ggplot2") # Graph creation
  library(ggplot2)
}

if(!require(DataExplorer)){
  install.packages("DataExplorer") # EDA plots
  library(DataExplorer)
}

if(!require(GGally)){
  install.packages("GGally") # Correlation Graph creation
  library(GGally)
}

if(!require(hopkins)){
  install.packages("hopkins") # Hopkins Statistic
  library(hopkins)
}

if(!require(mclust)){
  install.packages("mclust") # Model-Based clustering
  library(mclust)
}

if(!require(stats)){
  install.packages("stats") # K-means and Hierarchical clustering
  library(stats)
}

if(!require(kamila)){ 
  install.packages("kamila") # KAMILA clustering
  library(kamila)
}

if(!require(clustMixType)){
  install.packages("clustMixType") # K-prototypes clustering
  library(clustMixType)
}

if(!require(clusterSim)){
  install.packages("clusterSim") # Calinski-Harabasz index
  library(clusterSim)
}

if(!require(fpc)){
  install.packages("fpc") # Davies-Bouldin index
  library(fpc)
}

if(!require(dplyr)){
  install.packages("dplyr") # Functionality
  library(dplyr)
}

if(!require(ggfortify)){
  install.packages("ggfortify") # Autoplot for PCA
  library(ggfortify)
}

if(!require(factoextra)){
  install.packages("factoextra") # Principal Component Analysis
  library(factoextra)
}

if(!require(PCAmixdata)){
  install.packages("PCAmixdata") # PCA for Mixed Data
  library(PCAmixdata)
}

############################### Import Data ####################################

datadb <- read.table("processed.cleveland.data", sep=",")

# Make a vector that contains all the names of each variable, and placing 
# it as column names
names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
           "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
colnames(datadb) <- names

# Summary of the values in the file
summary(datadb)

datadb$ca <- as.numeric(datadb$ca)
datadb$thal <- as.numeric(datadb$thal)

table(is.na(datadb))


# There are 6 NAs in the database, so we need to delete the rows that contains them
datadb <- na.omit(datadb)

# Correction of the variable types
datadb$sex <- as.factor(datadb$sex)
datadb$cp <- as.factor(datadb$cp)
datadb$fbs <- as.factor(datadb$fbs)
datadb$restecg <- as.factor(datadb$restecg)
datadb$exang <- as.factor(datadb$exang)
datadb$slope <- as.factor(datadb$slope)
datadb$ca <- as.factor(datadb$ca)
datadb$thal <- as.factor(datadb$thal)

# Creation of a new column that classifies patients between having a heart disease
# or not (1=Healthy, 2=Heart Disease)
datadb$hd <- datadb$target
datadb$hd <- ifelse(datadb$hd > 0, 2, 1)

# Transformation of our response variables into factors
datadb$target <- as.factor(datadb$target)
datadb$hd <- as.factor(datadb$hd)

# Creation of a blind data set
datadb_blind <- datadb[, c(-14, -15)]

############################ Creation of table 1 ###############################

# Table 1 was created using this code and assembled the values in an excel file

Group = 3
Continuous_Variable
Factor_Variable

# Group creation
Patients_Selected <- datadb[datadb$target == Group, ]

# N for each group
nrow(Patients_Selected)

# Continuous Variables
mean(Patients_Selected$Continuous_Variable)
sd(Patients_Selected$Continuous_Variable)

# Factor Variables
freq <- table(Patients_Selected$Factor_Variable)
prop <- prop.table(frecuencia)

resultados <- data.frame(
  Clusters = levels(Patients_Selected$Factor_Variable),
  Frequency = as.vector(freq),
  Proportion = as.vector(prop)
)


######################### First values of the dataset ##########################

headdb <- head(datadb)

# Save table as PNG
png("Head_table.png", height = 30*nrow(headdb), width = 45*ncol(headdb))
grid.table(headdb)
dev.off()

############################ Continuous Variables ##############################

cont_variables <- c("age", "trestbps", "chol", "thalach", "oldpeak")

continuous_data <- datadb[, cont_variables]

par(mfrow=c(1,ncol(continuous_data)))
invisible(lapply(1:ncol(continuous_data), function(i) {
  boxplot(continuous_data[, i], main = names(continuous_data)[i], cex.lab = 1.5)
}))
dev.off()


par(mfrow=c(1,ncol(continuous_data)))

# Variable analysis between Clusters (MBC)
boxplot(continuous_data$age, main = "age", ylab = "years")
boxplot(continuous_data$trestbps, main = "trestbps", ylab = "mmHg")
boxplot(continuous_data$chol, main = "chol", ylab = "mg/dl")
boxplot(continuous_data$thalach, main = "thalach", ylab = "beats recorded")
boxplot(continuous_data$oldpeak, main = "oldpeak", ylab = "distance")
dev.off()

data_standardized <- as.data.frame(scale(continuous_data))

############################# Outlier Detection ################################

mahalanobis(continuous_data, colMeans(continuous_data), cov(continuous_data))

patients_mahalanobis <- round(data.frame(Mahalanobis = mahalanobis(continuous_data, colMeans(continuous_data), cov(continuous_data))), 4)
patients_mahalanobis$pvalue <- round(pchisq(patients_mahalanobis$Mahalanobis, df=3, lower.tail=FALSE), 6)

# Save table as txt
write.table(patients_mahalanobis, "patients_mahalanobis.txt")

Outliers <- patients_mahalanobis[patients_mahalanobis$pvalue < 0.001, ]

Outliers

# Save table as PNG
png("Detected_outliers.png", height = 30*nrow(Outliers), width = 160*ncol(Outliers))
grid.table(Outliers)
dev.off()

############################## Correlation Plot ################################

cont_variables_hd <- c("age", "trestbps", "chol", "thalach", "oldpeak", "hd")

continuous_data_hd <- datadb[, cont_variables_hd]

ggpairs(continuous_data_hd,
        columns = c(1:5),
        aes(color = hd,
            alpha = 0.5))

cont_variables_target <- c("age", "trestbps", "chol", "thalach", "oldpeak", "target")

continuous_data_target <- datadb[, cont_variables_target]

ggpairs(continuous_data_target,
        columns = c(1:5),
        aes(color = target,
            alpha = 0.5))

############################# EDA REPORT DATAEXPLORER ##########################

create_report(datadb, output_file = "EDA_report.html")

############################## Categorical Variables ###########################

cat_variables <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")

categorical_data <- datadb[, cat_variables]

################################# Hopkins statistic ############################

set.seed("1")
datainfo <- data.frame("Samples" = nrow(data_standardized),
                       "Variables" = ncol(data_standardized),
                       "Hopkins" = round(hopkins(data_standardized), 4),
                       "Outliers" = nrow(Outliers))

png("data_information.png", height = 70*nrow(datainfo), width = 70*ncol(datainfo))
grid.table(datainfo)
dev.off()


############################# Hierarchical Clustering ##########################

hc_list <- list()
hc_G_values <- 2:10

d <- dist(as.matrix(continuous_data))

set.seed("1")
h <- hclust(d, method = "ward.D2", members = NULL)


# Silhouette score
silhouette_score_hc <- function(G){
  set.seed("1")
  h <- hclust(d, method = "ward.D2", members = NULL)
  cut <- cutree(h, k = G)
  ss <- silhouette(cut, dist(continuous_data))
  mean(ss[, 3])
}


avg_sil_hc <- sapply(hc_G_values, silhouette_score_hc)
plot(hc_G_values, type='b', avg_sil_hc, xlab='Number of clusters', 
     ylab='Average Silhouette Scores', frame=FALSE, 
     main="Silhouette Score (Hierarchical Clustering)")
Max_silhouette_hc <- max(avg_sil_hc)

cut <- cutree(h, k = which.max(avg_sil_hc)+1)

plot(h, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = "Samples")
rect.hclust(h , k = which.max(avg_sil_hc)+1, border = 2:8)

hcut <- hcut(continuous_data, which.max(avg_sil_hc)+1, hc_method = "ward.D2")

continuous_data_hc <- mutate(continuous_data, cluster = cut)
ggplot(continuous_data_hc, aes(x=age, y = chol, color = factor(cluster))) + geom_point() +
  labs(title = "Age ~ Cholesterol. Hierarchical Clustering",
       theme = theme_bw(),
       theme(panel.background = element_blank()),
       x = "Age", y = "Cholesterol",
       guides(color = guide_legend(title = "Cluster"))) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_color_brewer(palette="Set2")

datadb$HCCluster <- as.factor(cut)

# Davies-Bouldin's Index
DB_HC <- index.DB(continuous_data, cut, d=NULL, centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_HC <- calinhara(continuous_data, cut)

fviz_cluster(hcut, data = data_standardized,
             palette = c("#2E9FDF", "#38BB72"), 
             geom = c("point", "text"),
             pointsize = 1.5,
             ellipse.type = "convex",
             main = "Cluster Plot (Hierarchical Clustering)",
             ggtheme = theme_bw()
)

############################# Model-Based Clustering ###########################

MBC_list <- list()
MClust_G_values <- 2:10

for (G in MClust_G_values) {
  set.seed("1")
  MBC <- Mclust(data_standardized, G)
  MBC_list[[as.character(G)]] <- MBC
}


# Silhouette score
silhouette_score_MBC <- function(G){
  set.seed("1")
  MBC <- Mclust(data_standardized, G)
  ss <- silhouette(MBC$classification, dist(data_standardized))
  mean(ss[, 3])
}

avg_sil_MBC <- sapply(MClust_G_values, silhouette_score_MBC)
plot(MClust_G_values, type='b', avg_sil_MBC, xlab='Number of clusters', 
     ylab='Average Silhouette Scores', 
     frame=FALSE, main="Silhouette Score (MBC)")
Max_silhouette_MBC <- max(avg_sil_MBC)

# BIC
for (G in MClust_G_values) {
  set.seed("1")
  BIC_MBC <- lapply(MBC_list, BIC)
}

plot(MClust_G_values, BIC_MBC, xlab = 'Clusters', ylab = 'BIC from MBC')


# Davies-Bouldin's Index
DB_MBC <- index.DB(data_standardized, MBC_list[[as.character(which.max(avg_sil_MBC)+1)]]$classification, 
                   d=NULL, centrotypes="centroids", p=2, q=2)


# Calinski-Harabasz index
CH_MBC <- calinhara(data_standardized, MBC_list[[as.character(which.max(avg_sil_MBC)+1)]]$classification)

# Cluster visualization
plot(MBC_list[[as.character(which.max(avg_sil_MBC)+1)]], what="classification")
summary(MBC_list[[as.character(which.max(avg_sil_MBC)+1)]])

fviz_cluster(MBC_list[[as.character(which.max(avg_sil_MBC)+1)]], data = data_standardized,
             palette = c("#2E9FDF", "#38BB72"), 
             geom = c("point", "text"),
             pointsize = 1.5,
             ellipse.type = "convex",
             main = "Cluster Plot (MBC)",
             ggtheme = theme_bw()
)

#################################### K-means ###################################

kmeans_list <- list()
kmeans_G_values <- 2:10

for (G in kmeans_G_values) {
  set.seed("1")
  k <-kmeans(continuous_data, G)
  kmeans_list[[as.character(G)]] <- k
}

# Silhouette score
silhouette_score_kmeans <- function(kmeans_G_values){
  set.seed("1")
  k <-kmeans(continuous_data, kmeans_G_values, nstart = 50)
  ss <- silhouette(k$cluster, dist(continuous_data))
  mean(ss[, 3])
}

avg_sil_kmeans <- sapply(kmeans_G_values, silhouette_score_kmeans)
plot(kmeans_G_values, type='b', avg_sil_kmeans, xlab='Number of clusters', 
     ylab='Average Silhouette Scores', frame=FALSE, main="Silhouette Score (K-means)")
Max_silhouette_kMeans <- max(avg_sil_kmeans)

# BIC
kmeansBIC <- function(fit){
  m = ncol(fit$centers) 
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + log(n) * m * k)
}

bic_k <- sapply(kmeans_G_values, FUN = 
                  function(k) 
                    kmeansBIC(kmeans(continuous_data, centers = k, nstart = 20, iter.max = 25)))

plot(kmeans_G_values, bic_k, xlab = 'Clusters', ylab = 'BIC from kmeans')

# Davies-Bouldin's Index
DB_Kmeans <- index.DB(continuous_data, kmeans_list[[as.character(which.max(avg_sil_kmeans)+1)]]$cluster, d=NULL, 
                      centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_Kmeans <- calinhara(continuous_data, kmeans_list[[as.character(which.max(avg_sil_kmeans)+1)]]$cluster)

# Cluster visualization
fviz_cluster(kmeans_list[[as.character(which.max(avg_sil_kmeans)+1)]], data = data_standardized,
             palette = c("#2E9FDF", "#38BB72"), 
             geom = c("point", "text"),
             pointsize = 1.5,
             ellipse.type = "convex",
             main = "Cluster Plot (K-means)",
             ggtheme = theme_bw()
)

############################## Cluster comparisons ##############################

Cluster_comparisons <- data.frame("Model" = c("Hierarchical Clustering","MBC", "K-means"),
                                  "Silhouette coefficient" = c(round(Max_silhouette_hc, 3), round(Max_silhouette_MBC, 3), round(Max_silhouette_kMeans, 3)),
                                  "Davies-Bouldin index" = c(round(DB_HC$DB, 3), round(DB_MBC$DB, 3), round(DB_Kmeans$DB, 3)),
                                  "Calinski-Harabasz" = c(round(CH_HC, 3), round(CH_MBC, 3), round(CH_Kmeans, 3)))

# Save table as PNG
png("Cluster_Comparisons.png", height = 50*nrow(Cluster_comparisons), 
    width = 130*ncol(Cluster_comparisons))
grid.table(Cluster_comparisons)
dev.off()


# Comparisons Clusters and Heart Disease
datadb$MBCcluster <- as.factor(MBC_list[[as.character(which.max(avg_sil_MBC)+1)]]$classification)
datadb$Kmeanscluster <- as.factor(kmeans_list[[as.character(which.max(avg_sil_kmeans)+1)]]$cluster)


pca_res <- prcomp(continuous_data, scale. = TRUE)

plot1 <- autoplot(pca_res, data = datadb, colour = "MBCcluster",  
                  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 3)

plot1 + theme(axis.line = element_line(color='black'),
              plot.background = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
  scale_color_brewer(palette="Set2")


plot2 <- autoplot(pca_res, data = datadb, colour = "Kmeanscluster",  
                  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 3)

plot2 + theme(axis.line = element_line(color='black'),
              plot.background = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
  scale_color_brewer(palette="Set2")


plot3 <- autoplot(pca_res, data = datadb, colour = "HCCluster", ggtheme = theme_bw())

plot3 + theme(axis.line = element_line(color='black'),
              plot.background = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
  scale_color_brewer(palette="Set2")

#################################### Kamila ####################################

kamila_list <- list()
numClust_values <- 2:10

for (numClust in numClust_values) {
  set.seed("1")
  kamila <- kamila(conVar = data_standardized, 
                             catFactor = categorical_data,
                             numClust = numClust,
                             numInit = 100)
  kamila_list[[as.character(numClust)]] <- kamila
}


# Silhouette score
silhouette_score_kamila <- function(K){
  set.seed("1")
  kamila <- kamila(conVar = data_standardized, 
                   catFactor = categorical_data,
                   numClust = K,
                   numInit = 100)
  ss <- silhouette(kamila$finalMemb, dist(data_standardized))
  mean(ss[, 3])
}

avg_sil_kamila <- sapply(numClust_values, silhouette_score_kamila)
plot(numClust_values, type='b', avg_sil_kamila, xlab='Number of clusters', 
     ylab='Average Silhouette Scores', frame=FALSE, main="Silhouette Score (Kamila)")
Max_silhouette_kamila <- max(avg_sil_kamila)

# Davies-Bouldin's Index
DB_Kamila <- index.DB(data_standardized, kamila_list[[as.character(which.max(avg_sil_kamila)+1)]]$finalMemb, d=NULL, 
                      centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_Kamila <- calinhara(data_standardized, kamila_list[[as.character(which.max(avg_sil_kamila)+1)]]$finalMemb)


datadb$kamilacluster <- as.factor(kamila_list[[as.character(which.max(avg_sil_kamila)+1)]]$finalMemb)

# PCA for MIXED DATA
split <- splitmix(datadb_blind)
pcamix <- PCAmix(X.quanti=split$X.quanti,
                 X.quali=split$X.quali,
                 rename.level=TRUE, 
                 graph=TRUE, ndim=2)


plot(pcamix, coloring.ind=datadb$kamilacluster)

# PS plot
kamila_ps <- NULL
numClust_values <- 2:10

suppressWarnings(
  for (numClust in numClust_values) {
  set.seed("1")
  kamila <- kamila(conVar = data_standardized, 
                   catFactor = categorical_data,
                   numClust = numClust,
                   calcNumClust = "ps",
                   numInit = 100)
  kamila <- kamila$nClust$psValues
  kamila_ps <- append(kamila_ps, kamila)
})

kamila_ps_df <- data.frame("Clusters" = c((1:length(kamila_ps))+1),
                           "ps" = kamila_ps)

plot(kamila_ps_df$Clusters, kamila_ps_df$ps, type='b', ylab = "Prediction Strength", 
     xlab = "Number of Clusters", frame=FALSE, main="Prediction Strength Values (Kamila)")

################################## k-prototypes ################################

kproto_list <- list()
Kp <- 2:10

for (protoclust in Kp) {
  set.seed("1")
  kproto <- kproto(datadb_blind, protoclust, nstart = 5)
  kproto_list[[as.character(protoclust)]] <- kproto
}

# Silhouette score
silhouette_score_kproto <- function(Kp){
  set.seed("1")
  kproto <- kproto(datadb_blind, Kp, nstart = 5)
  ss <- silhouette(kproto$cluster, dist(datadb_blind))
  mean(ss[, 3])
}

avg_sil_kproto <- sapply(Kp, silhouette_score_kproto)
plot(Kp, type='b', avg_sil_kproto, xlab='Number of clusters', 
     ylab='Average Silhouette Scores', frame=FALSE, main="Silhouette Score (k-Prototypes)")
Max_silhouette_kproto <- max(avg_sil_kproto)

# Davies-Bouldin's Index
DB_Kproto <- index.DB(continuous_data, kproto_list[[as.character(which.max(avg_sil_kproto)+1)]]$cluster, d=NULL, 
                      centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_Kproto <- calinhara(continuous_data, kproto_list[[as.character(which.max(avg_sil_kproto)+1)]]$cluster)


fit_df <- factor(kproto_list[[as.character(which.max(avg_sil_kproto)+1)]]$cluster, order =  TRUE,
                 levels = c(1:2))
fit <- data.frame(data_standardized, fit_df)
result_df <- kproto_list[[as.character(which.max(avg_sil_kproto)+1)]]$centers
Member <- kproto_list[[as.character(which.max(avg_sil_kproto)+1)]]$size
result <- data.frame(Member, result_df)
result


# Cluster visualization
ggplot(fit, aes(x = age, y = chol, color = fit_df)) +
  geom_point() +
  labs(title = "Average Age of the patients (age) ~ Average Cholesterol (chol)",
       theme = theme_bw(),
       theme(panel.background = element_blank()),
       x = "Average Age of the patients (age)", y = "Average Cholesterol (chol)",
       guides(color = guide_legend(title = "Cluster"))) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_color_brewer(palette="Set2")

datadb$kprotocluster <- as.factor(kproto_list[[as.character(which.max(avg_sil_kproto)+1)]]$cluster)

plot(pcamix, coloring.ind=datadb$kprotocluster)

############################## Cluster comparisons #############################

Cluster_comparisons2 <- data.frame("Model" = c("Kamila", "K-Prototypes"),
                                  "Silhouette coefficient" = c(round(Max_silhouette_kamila, 3), round(Max_silhouette_kproto, 3)),
                                  "Davies-Bouldin index" = c(round(DB_Kamila$DB, 3), round(DB_Kproto$DB, 3)),
                                  "Calinski-Harabasz" = c(round(CH_Kamila, 3), round(CH_Kproto, 3)))

# Save table as PNG
png("Cluster_Comparisons2.png", height = 50*nrow(Cluster_comparisons2), 
     width = 130*ncol(Cluster_comparisons2))
grid.table(Cluster_comparisons2)
dev.off()


################################## Post-Analysis ###############################

par(mfrow=c(1,5))

# Variable analysis between Clusters (MBC)
boxplot(age ~ MBCcluster, data = datadb, main = "age", ylab = "years", xlab = "")
boxplot(trestbps ~ MBCcluster, data = datadb, main = "trestbps", ylab = "mmHg", xlab = "")
boxplot(chol ~ MBCcluster, data = datadb, main = "chol", ylab = "mg/dl", xlab = "Cluster")
boxplot(thalach ~ MBCcluster, data = datadb, main = "thalach", ylab = "beats recorded", xlab = "")
boxplot(oldpeak ~ MBCcluster, data = datadb, main = "oldpeak", ylab = "distance", xlab = "")

# Variable analysis between Clusters (kmeans)
boxplot(age ~ Kmeanscluster, data = datadb, main = "age", ylab = "years", xlab = "")
boxplot(trestbps ~ Kmeanscluster, data = datadb, main = "trestbps", ylab = "mmHg", xlab = "")
boxplot(chol ~ Kmeanscluster, data = datadb, main = "chol", ylab = "mg/dl", xlab = "Cluster")
boxplot(thalach ~ Kmeanscluster, data = datadb, main = "thalach", ylab = "beats recorded", xlab = "")
boxplot(oldpeak ~ Kmeanscluster, data = datadb, main = "oldpeak", ylab = "distance", xlab = "")

dev.off()

################################# Final table ##################################

Cluster_comparisons_Final <- data.frame(
  "Model" = c("Hierarchical Clustering", "MBC", "K-means", "Kamila", "K-Prototypes"),
              "Nº of clusters" = c(2, 2, 2, 2, 2),
              "Silhouette coefficient" = c(round(Max_silhouette_hc, 3), round(Max_silhouette_MBC, 3), round(Max_silhouette_kMeans, 3), round(Max_silhouette_kamila, 3), round(Max_silhouette_kproto, 3)),
              "Davies-Bouldin index" = c(round(DB_HC$DB, 3), round(DB_MBC$DB, 3), round(DB_Kmeans$DB, 3), round(DB_Kamila$DB, 3), round(DB_Kproto$DB, 3)),
              "Calinski-Harabasz" = c(round(CH_HC, 3), round(CH_MBC, 3), round(CH_Kmeans, 3), round(CH_Kamila, 3), round(CH_Kproto, 3)))

# Save table as PNG
png("Cluster_comparisons_Final.png", height = 50*nrow(Cluster_comparisons_Final), width = 130*ncol(Cluster_comparisons_Final))
grid.table(Cluster_comparisons_Final)
dev.off()


###################################### ARI #####################################

ARIDF <- data.frame ("Comparisons" = c("HC vs MBC", "HC vs K-means", "HC vs Kamila", "HC vs K-prototypes", 
                                       "MBC vs K-means", "MBC vs Kamila", "MBC vs K-prototypes",
                                       "K-means vs Kamila", "K-means vs K-prototypes",
                                       "Kamila vs K-prototypes"),
                     "ARI" = round(c(adjustedRandIndex(datadb$HCCluster, 
                                                       datadb$MBCcluster), 
                               adjustedRandIndex(datadb$HCCluster, 
                                                 datadb$Kmeanscluster),
                               adjustedRandIndex(datadb$HCCluster, 
                                                 datadb$kamilacluster),
                               adjustedRandIndex(datadb$HCCluster, 
                                                 datadb$kprotocluster),
                               adjustedRandIndex(datadb$MBCcluster, 
                                                 datadb$Kmeanscluster),
                               adjustedRandIndex(datadb$MBCcluster, 
                                                 datadb$kamilacluster),
                               adjustedRandIndex(datadb$MBCcluster, 
                                                 datadb$kprotocluster),
                               adjustedRandIndex(datadb$Kmeanscluster, 
                                                 datadb$kamilacluster),
                               adjustedRandIndex(datadb$Kmeanscluster, 
                                                 datadb$kprotocluster),
                               adjustedRandIndex(datadb$kamilacluster, 
                                                 datadb$kprotocluster)), 4))

ARIDF <- ARIDF[order(ARIDF$ARI, decreasing = TRUE),]


# Save table as PNG
png("ARI_Table.png", height = 25*nrow(ARIDF), width = 130*ncol(ARIDF))
grid.table(ARIDF)
dev.off()


############################## Cluster Profiles #############################

Cluster_Means_Cont <- round(data.frame("HC Age" = c(mean(datadb$age[datadb$HCCluster == 1]),
                                                    mean(datadb$age[datadb$HCCluster == 2])),
                                       "HC trestbps" = c(mean(datadb$trestbps[datadb$HCCluster == 1]),
                                                    mean(datadb$trestbps[datadb$HCCluster == 2])),       
                                       "HC chol" = c(mean(datadb$chol[datadb$HCCluster == 1]),
                                                    mean(datadb$chol[datadb$HCCluster == 2])),
                                       "HC thalach" = c(mean(datadb$thalach[datadb$HCCluster == 1]),
                                                    mean(datadb$thalach[datadb$HCCluster == 2])),
                                       "HC oldpeak" = c(mean(datadb$oldpeak[datadb$HCCluster == 1]),
                                                    mean(datadb$oldpeak[datadb$HCCluster == 2])),
                                       
                                       "MBC Age" = c(mean(datadb$age[datadb$MBCcluster == 1]),
                                                    mean(datadb$age[datadb$MBCcluster == 2])),
                                       "MBC trestbps" = c(mean(datadb$trestbps[datadb$MBCcluster == 1]),
                                                         mean(datadb$trestbps[datadb$MBCcluster == 2])),       
                                       "MBC chol" = c(mean(datadb$chol[datadb$MBCcluster == 1]),
                                                     mean(datadb$chol[datadb$MBCcluster == 2])),
                                       "MBC thalach" = c(mean(datadb$thalach[datadb$MBCcluster == 1]),
                                                        mean(datadb$thalach[datadb$MBCcluster == 2])),
                                       "MBC oldpeak" = c(mean(datadb$oldpeak[datadb$MBCcluster == 1]),
                                                        mean(datadb$oldpeak[datadb$MBCcluster == 2])),
                                       
                                       "Kmeans Age" = c(mean(datadb$age[datadb$Kmeanscluster == 1]),
                                                     mean(datadb$age[datadb$Kmeanscluster == 2])),
                                       "Kmeans trestbps" = c(mean(datadb$trestbps[datadb$Kmeanscluster == 1]),
                                                          mean(datadb$trestbps[datadb$Kmeanscluster == 2])),       
                                       "Kmeans chol" = c(mean(datadb$chol[datadb$Kmeanscluster == 1]),
                                                      mean(datadb$chol[datadb$Kmeanscluster == 2])),
                                       "Kmeans thalach" = c(mean(datadb$thalach[datadb$Kmeanscluster == 1]),
                                                         mean(datadb$thalach[datadb$Kmeanscluster == 2])),
                                       "Kmeans oldpeak" = c(mean(datadb$oldpeak[datadb$Kmeanscluster == 1]),
                                                         mean(datadb$oldpeak[datadb$Kmeanscluster == 2])),
                                       
                                       "kamila Age" = c(mean(datadb$age[datadb$kamilacluster == 1]),
                                                        mean(datadb$age[datadb$kamilacluster == 2])),
                                       "kamila trestbps" = c(mean(datadb$trestbps[datadb$kamilacluster == 1]),
                                                             mean(datadb$trestbps[datadb$kamilacluster == 2])),       
                                       "kamila chol" = c(mean(datadb$chol[datadb$kamilacluster == 1]),
                                                         mean(datadb$chol[datadb$kamilacluster == 2])),
                                       "kamila thalach" = c(mean(datadb$thalach[datadb$kamilacluster == 1]),
                                                            mean(datadb$thalach[datadb$kamilacluster == 2])),
                                       "kamila oldpeak" = c(mean(datadb$oldpeak[datadb$kamilacluster == 1]),
                                                            mean(datadb$oldpeak[datadb$kamilacluster == 2])),
                                       
                                       "kproto Age" = c(mean(datadb$age[datadb$kprotocluster == 1]),
                                                        mean(datadb$age[datadb$kprotocluster == 2])),
                                       "kproto trestbps" = c(mean(datadb$trestbps[datadb$kprotocluster == 1]),
                                                             mean(datadb$trestbps[datadb$kprotocluster == 2])),       
                                       "kproto chol" = c(mean(datadb$chol[datadb$kprotocluster == 1]),
                                                         mean(datadb$chol[datadb$kprotocluster == 2])),
                                       "kproto thalach" = c(mean(datadb$thalach[datadb$kprotocluster == 1]),
                                                            mean(datadb$thalach[datadb$kprotocluster == 2])),
                                       "kproto oldpeak" = c(mean(datadb$oldpeak[datadb$kprotocluster == 1]),
                                                            mean(datadb$oldpeak[datadb$kprotocluster == 2]))), 2)

# Save table as PNG
png("Cluster_Means_Cont.png", height = 35*nrow(Cluster_Means_Cont), width = 90*ncol(Cluster_Means_Cont))
grid.table(Cluster_Means_Cont)
dev.off()


Cluster_Profiles_Cont <- round(data.frame("HC" = c(wilcox.test(subset(datadb, HCCluster == 1)$age, subset(datadb, HCCluster == 2)$age)$p.value,
                                             wilcox.test(subset(datadb, HCCluster == 1)$trestbps, subset(datadb, HCCluster == 2)$trestbps)$p.value,
                                             wilcox.test(subset(datadb, HCCluster == 1)$chol, subset(datadb, HCCluster == 2)$chol)$p.value,
                                             wilcox.test(subset(datadb, HCCluster == 1)$thalach, subset(datadb, HCCluster == 2)$thalach)$p.value,
                                             wilcox.test(subset(datadb, HCCluster == 1)$oldpeak, subset(datadb, HCCluster == 2)$oldpeak)$p.value),
                                   "MBC" = c(wilcox.test(subset(datadb, MBCcluster == 1)$age, subset(datadb, MBCcluster == 2)$age)$p.value,
                                             wilcox.test(subset(datadb, MBCcluster == 1)$trestbps, subset(datadb, MBCcluster == 2)$trestbps)$p.value,
                                             wilcox.test(subset(datadb, MBCcluster == 1)$chol, subset(datadb, MBCcluster == 2)$chol)$p.value,
                                             wilcox.test(subset(datadb, MBCcluster == 1)$thalach, subset(datadb, MBCcluster == 2)$thalach)$p.value,
                                             wilcox.test(subset(datadb, MBCcluster == 1)$oldpeak, subset(datadb, MBCcluster == 2)$oldpeak)$p.value),
                                   "K-means" = c(wilcox.test(subset(datadb, Kmeanscluster == 1)$age, subset(datadb, Kmeanscluster == 2)$age)$p.value,
                                                 wilcox.test(subset(datadb, Kmeanscluster == 1)$trestbps, subset(datadb, Kmeanscluster == 2)$trestbps)$p.value,
                                                 wilcox.test(subset(datadb, Kmeanscluster == 1)$chol, subset(datadb, Kmeanscluster == 2)$chol)$p.value,
                                                 wilcox.test(subset(datadb, Kmeanscluster == 1)$thalach, subset(datadb, Kmeanscluster == 2)$thalach)$p.value,
                                                 wilcox.test(subset(datadb, Kmeanscluster == 1)$oldpeak, subset(datadb, Kmeanscluster == 2)$oldpeak)$p.value),
                                   "Kamila" = c(wilcox.test(subset(datadb, kamilacluster == 1)$age, subset(datadb, kamilacluster == 2)$age)$p.value,
                                                wilcox.test(subset(datadb, kamilacluster == 1)$trestbps, subset(datadb, kamilacluster == 2)$trestbps)$p.value,
                                                wilcox.test(subset(datadb, kamilacluster == 1)$chol, subset(datadb, kamilacluster == 2)$chol)$p.value,
                                                wilcox.test(subset(datadb, kamilacluster == 1)$thalach, subset(datadb, kamilacluster == 2)$thalach)$p.value,
                                                wilcox.test(subset(datadb, kamilacluster == 1)$oldpeak, subset(datadb, kamilacluster == 2)$oldpeak)$p.value),
                                   "K-prototypes" = c(wilcox.test(subset(datadb, kprotocluster == 1)$age, subset(datadb, kprotocluster == 2)$age)$p.value,
                                                      wilcox.test(subset(datadb, kprotocluster == 1)$trestbps, subset(datadb, kprotocluster == 2)$trestbps)$p.value,
                                                      wilcox.test(subset(datadb, kprotocluster == 1)$chol, subset(datadb, kprotocluster == 2)$chol)$p.value,
                                                      wilcox.test(subset(datadb, kprotocluster == 1)$thalach, subset(datadb, kprotocluster == 2)$thalach)$p.value,
                                                      wilcox.test(subset(datadb, kprotocluster == 1)$oldpeak, subset(datadb, kprotocluster == 2)$oldpeak)$p.value)
                                   ), 5)

rownames(Cluster_Profiles_Cont) <- cont_variables


# Save table as PNG
png("Cluster_Profiles_Cont.png", height = 30*nrow(Cluster_Profiles_Cont), width = 80*ncol(Cluster_Profiles_Cont))
grid.table(Cluster_Profiles_Cont)
dev.off()

Cluster_Profiles_Cat <- round(data.frame("Kamila" = c(chisq.test(datadb$sex, datadb$kamilacluster, correct=FALSE)$p.value,
                                                       chisq.test(datadb$cp, datadb$kamilacluster, correct=FALSE)$p.value,
                                                       chisq.test(datadb$fbs, datadb$kamilacluster, correct=FALSE)$p.value,
                                                       chisq.test(datadb$restecg, datadb$kamilacluster, correct=FALSE)$p.value,
                                                       chisq.test(datadb$exang, datadb$kamilacluster, correct=FALSE)$p.value,
                                                       chisq.test(datadb$slope, datadb$kamilacluster, correct=FALSE)$p.value,
                                                       chisq.test(datadb$ca, datadb$kamilacluster, correct=FALSE)$p.value,
                                                       chisq.test(datadb$thal, datadb$kamilacluster, correct=FALSE)$p.value),
                                          "K-prototypes" = c(chisq.test(datadb$sex, datadb$kprotocluster, correct=FALSE)$p.value,
                                                             chisq.test(datadb$cp, datadb$kprotocluster, correct=FALSE)$p.value,
                                                             chisq.test(datadb$fbs, datadb$kprotocluster, correct=FALSE)$p.value,
                                                             chisq.test(datadb$restecg, datadb$kprotocluster, correct=FALSE)$p.value,
                                                             chisq.test(datadb$exang, datadb$kprotocluster, correct=FALSE)$p.value,
                                                             chisq.test(datadb$slope, datadb$kprotocluster, correct=FALSE)$p.value,
                                                             chisq.test(datadb$ca, datadb$kprotocluster, correct=FALSE)$p.value,
                                                             chisq.test(datadb$thal, datadb$kprotocluster, correct=FALSE)$p.value)), 5)

rownames(Cluster_Profiles_Cat) <- cat_variables

# Save table as PNG
png("Cluster_Profiles_Cat.png", height = 30*nrow(Cluster_Profiles_Cat), width = 100*ncol(Cluster_Profiles_Cat))
grid.table(Cluster_Profiles_Cat)
dev.off()
