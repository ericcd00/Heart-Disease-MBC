################################################################################

# TITLE: APLICATION OF MODEL-BASED CLUSTERING IN HEART DISEASE PATIENTS. CODE
# DESCRIPTION: Code used for Eric's Master Thesis. 

################################################################################


### LIBRARIES
library(gridExtra)


#### IMPORT DATA

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


#Creation of table 1
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

# Save plot as PNG
png("Table1.png", height = 25*nrow(Table1), width = 90*ncol(Table1))
grid.table(Table1)
dev.off()

