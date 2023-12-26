################################################################################

# TITLE: APLICATION OF MODEL-BASED CLUSTERING IN HEART DISEASE PATIENTS. CODE
# DESCRIPTION: Code used for Eric's Master Thesis. 

################################################################################


################################# Libraries ####################################

if(!require(gridExtra)){
  install.packages("gridExtra")
  library(gridExtra)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(GGally)){
  install.packages("GGally")
  library(GGally)
}

if(!require(ggfortify)){
  install.packages("ggfortify")
  library(ggfortify)
}

if(!require(mclust)){
  install.packages("mclust")
  library(mclust)
}

if(!require(factoextra)){
  install.packages("factoextra")
  library(factoextra)
}

if(!require(hopkins)){
  install.packages("hopkins")
  library(hopkins)
}

if(!require(kamila)){
  install.packages("kamila")
  library(kamila)
}

if(!require(clustMixType)){
  install.packages("clustMixType")
  library(clustMixType)
}

if(!require(clusterSim)){
  install.packages("clusterSim")
  library(clusterSim)
}

if(!require(fpc)){
  install.packages("fpc")
  library(fpc)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(PCAmixdata)){
  install.packages("PCAmixdata")
  library(PCAmixdata)
}
if(!require(kmodR)){
  install.packages("kmodR")
  library(kmodR)
}
if(!require(stats)){
  install.packages("stats")
  library(stats)
}


############################### Import Data ####################################

datadb <- read.table("processed.cleveland.data", sep=",")

# Make a vector that contains all the names of each variable, and placing 
# it as column names
names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
           "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
colnames(datadb) <- names

#Summary of the values in the file
summary(datadb)

datadb$ca <- as.numeric(datadb$ca)
datadb$thal <- as.numeric(datadb$thal)

table(is.na(datadb))


#There are 6 NAs in the database, so we need to delete the rows that contains them
datadb <- na.omit(datadb)

#Correction of the variable types
datadb$sex <- as.factor(datadb$sex)
datadb$cp <- as.factor(datadb$cp)
datadb$fbs <- as.factor(datadb$fbs)
datadb$restecg <- as.factor(datadb$restecg)
datadb$exang <- as.factor(datadb$exang)
datadb$slope <- as.factor(datadb$slope)
datadb$ca <- as.factor(datadb$ca)
datadb$thal <- as.factor(datadb$thal)

#Create a new column that classifies patients between having a heart disease
#or not (just 1 or 2, 1=Healthy, 2=HD)
datadb$hd <- datadb$target
datadb$hd <- ifelse(datadb$hd > 0, 2, 1)

#Transform our response variables into factors
datadb$target <- as.factor(datadb$target)
datadb$hd <- as.factor(datadb$hd)

# Creation of a blind dataset
datadb_blind <- datadb[, c(-14, -15)]

############################ Creation of table 1 ###############################

Table1 <- data.frame(
  Variable = names(datadb),
  Type = sapply(datadb, function(x) {
    if (is.factor(x)) {
      return("factor")
    } else {
      return(class(x))
    }
  }),
  Range = sapply(datadb, function(x) {
    if (is.factor(x)) {
      return(paste(levels(x), collapse = ", "))
    } else {
      return(paste(range(x), collapse = " - "))
    }
  })
)

print(Table1)

# Save table as PNG
png("Table1.png", height = 25*nrow(Table1), width = 90*ncol(Table1))
grid.table(Table1)
dev.off()


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


par(mfrow=c(1,5))

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

patients_mahalanobis <- data.frame(Mahalanobis = mahalanobis(continuous_data, colMeans(continuous_data), cov(continuous_data)))
patients_mahalanobis$pvalue <- pchisq(patients_mahalanobis$Mahalanobis, df=3, lower.tail=FALSE)

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
plot(MClust_G_values, type='b', avg_sil_MBC, xlab='Number of clusters', ylab='Average Silhouette Scores', 
     frame=FALSE, main="Silhouette Score (MBC)")
Max_silhouette_MBC <- max(avg_sil_MBC)


# Davies-Bouldin's Index
DB_MBC <- index.DB(data_standardized, MBC_list[["2"]]$classification, d=NULL, centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_MBC <- calinhara(data_standardized, MBC_list[["2"]]$classification)

# Cluster visualization
plot(MBC_list[["2"]], what="classification")
summary(MBC_list[["2"]])

fviz_cluster(MBC_list[["2"]], data = data_standardized,
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
plot(kmeans_G_values, type='b', avg_sil_kmeans, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE, main="Silhouette Score (K-means)")
Max_silhouette_kMeans <- max(avg_sil_kmeans)

# Davies-Bouldin's Index
DB_Kmeans <- index.DB(continuous_data, kmeans_list[["2"]]$cluster, d=NULL, centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_Kmeans <- calinhara(continuous_data, kmeans_list[["2"]]$cluster)

# Cluster visualization
fviz_cluster(kmeans_list[["2"]], data = continuous_data,
             palette = c("#2E9FDF", "#38BB72"), 
             geom = c("point", "text"),
             pointsize = 1.5,
             ellipse.type = "convex",
             main = "Cluster Plot (K-means)",
             ggtheme = theme_bw()
)

############################## Cluster comparisons ##############################

Cluster_comparisons <- data.frame("Model" = c("MBC", "K-means"),
                                  "Silhouette coefficient" = c(Max_silhouette_MBC, Max_silhouette_kMeans),
                                  "Davies-Bouldin index" = c(DB_MBC$DB, DB_Kmeans$DB),
                                  "Calinski-Harabasz" = c(CH_MBC, CH_Kmeans))

# Save table as PNG
png("Cluster_Comparisons.png", height = 50*nrow(Cluster_comparisons), width = 130*ncol(Cluster_comparisons))
grid.table(Cluster_comparisons)
dev.off()


# Comparisons Clusters and Heart Disease
continuous_data_hd$MBCcluster <- as.factor(MBC_list[["2"]]$classification)
continuous_data_hd$Kmeanscluster <- as.factor(kmeans_list[["2"]]$cluster)


df <- continuous_data_hd[1:5]

pca_res <- prcomp(df, scale. = TRUE)

plot1 <- autoplot(pca_res, data = continuous_data_hd, colour = "MBCcluster",  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 3)

plot1 + theme(axis.line = element_line(color='black'),
              plot.background = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
  scale_color_brewer(palette="Set2")


plot2 <- autoplot(pca_res, data = continuous_data_hd, colour = "Kmeanscluster",  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 3)

plot2 + theme(axis.line = element_line(color='black'),
              plot.background = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
  scale_color_brewer(palette="Set2")


plot3 <- autoplot(pca_res, data = continuous_data_hd, colour = "hd", ggtheme = theme_bw())

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
plot(numClust_values, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE, main="Silhouette Score (Kamila)")
Max_silhouette_kamila <- max(avg_sil_kamila)

# Davies-Bouldin's Index
DB_Kamila <- index.DB(data_standardized, kamila_list[["2"]]$finalMemb, d=NULL, centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_Kamila <- calinhara(data_standardized, kamila_list[["2"]]$finalMemb)


continuous_data_hd$kamilacluster <- as.factor(kamila_list[["2"]]$finalMemb)

# PCA for MIXED DATA
split <- splitmix(datadb_blind)
pcamix <- PCAmix(X.quanti=split$X.quanti,
                 X.quali=split$X.quali,
                 rename.level=TRUE, 
                 graph=TRUE, ndim=2)


plot(pcamix, coloring.ind=continuous_data_hd$kamilacluster)


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
plot(Kp, type='b', avg_sil_kproto, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE, main="Silhouette Score (k-Prototypes)")
Max_silhouette_kproto <- max(avg_sil_kproto)

# Davies-Bouldin's Index
DB_Kproto <- index.DB(continuous_data, kproto_list[["2"]]$cluster, d=NULL, centrotypes="centroids", p=2, q=2)

# Calinski-Harabasz index
CH_Kproto <- calinhara(continuous_data, kproto_list[["2"]]$cluster)


fit_df <- factor(kproto_list[["2"]]$cluster, order =  TRUE,
                 levels = c(1:2))
fit <- data.frame(data_standardized, fit_df)
result_df <- kproto_list[["2"]]$centers
Member <- kproto_list[["2"]]$size
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

continuous_data_hd$kprotocluster <- as.factor(kproto_list[["2"]]$cluster)

plot(pcamix, coloring.ind=continuous_data_hd$kprotocluster)

############################## Cluster comparisons #############################

Cluster_comparisons2 <- data.frame("Model" = c("Kamila", "K-Prototypes"),
                                  "Silhouette coefficient" = c(Max_silhouette_kamila, Max_silhouette_kproto),
                                  "Davies-Bouldin index" = c(DB_Kamila$DB, DB_Kproto$DB),
                                  "Calinski-Harabasz" = c(CH_Kamila, CH_Kproto))

# Save table as PNG
png("Cluster_Comparisons2.png", height = 50*nrow(Cluster_comparisons2), width = 130*ncol(Cluster_comparisons2))
grid.table(Cluster_comparisons2)
dev.off()


################################## Post-Analysis ###############################


par(mfrow=c(1,5))

# Variable analysis between Clusters (MBC)
boxplot(age ~ MBCcluster, data = continuous_data_hd, main = "age", ylab = "years", xlab = "")
boxplot(trestbps ~ MBCcluster, data = continuous_data_hd, main = "trestbps", ylab = "mmHg", xlab = "")
boxplot(chol ~ MBCcluster, data = continuous_data_hd, main = "chol", ylab = "mg/dl", xlab = "Cluster")
boxplot(thalach ~ MBCcluster, data = continuous_data_hd, main = "thalach", ylab = "beats recorded", xlab = "")
boxplot(oldpeak ~ MBCcluster, data = continuous_data_hd, main = "oldpeak", ylab = "distance", xlab = "")

# Variable analysis between Clusters (kmeans)
boxplot(age ~ Kmeanscluster, data = continuous_data_hd, main = "age", ylab = "years", xlab = "")
boxplot(trestbps ~ Kmeanscluster, data = continuous_data_hd, main = "trestbps", ylab = "mmHg", xlab = "")
boxplot(chol ~ Kmeanscluster, data = continuous_data_hd, main = "chol", ylab = "mg/dl", xlab = "Cluster")
boxplot(thalach ~ Kmeanscluster, data = continuous_data_hd, main = "thalach", ylab = "beats recorded", xlab = "")
boxplot(oldpeak ~ Kmeanscluster, data = continuous_data_hd, main = "oldpeak", ylab = "distance", xlab = "")

dev.off()

################################# Final table ##################################

Cluster_comparisons_Final <- data.frame(
  "Model" = c("MBC", "K-means", "Kamila", "K-Prototypes"),
              "Nº of clusters" = c(2,2,2,2),
              "Silhouette coefficient" = c(Max_silhouette_MBC, Max_silhouette_kMeans, Max_silhouette_kamila, Max_silhouette_kproto),
              "Davies-Bouldin index" = c(DB_MBC$DB, DB_Kmeans$DB, DB_Kamila$DB, DB_Kproto$DB),
              "Calinski-Harabasz" = c(CH_MBC, CH_Kmeans, CH_Kamila, CH_Kproto))

# Save table as PNG
png("Cluster_comparisons_Final.png", height = 50*nrow(Cluster_comparisons_Final), width = 130*ncol(Cluster_comparisons_Final))
grid.table(Cluster_comparisons_Final)
dev.off()


