#install.packages("rstudioapi")
library(rstudioapi)
#install.packages("neuralnet")
library(neuralnet)


################
########Read Dataset
################
getwd()
currentPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(currentPath)
dataset.data <- read.csv("breast-cancer.csv")
dim(dataset.data)

################
########Clean Dataset
################
for (row in 1:nrow(dataset.data)){
  if(10 !=length(dataset.data[row,])){
    print(row)
  }
}
#Data is complete

summary(dataset.data)
# We have some errors in CellClass_2_benign4_malignant it should be just 2 or 4 and we have 41
# lets clean it, all the ones with value diferent to 2 or 4 will be removed
dirty.data <- c()
for (row in 1:nrow(dataset.data)){
  if((4 != dataset.data[row,"CellClass_2_benign4_malignant"]) && (2 != dataset.data[row,"CellClass_2_benign4_malignant"])){
    dirty.data = c(dirty.data,row) 
  }
}
dataset.data.clean <- dataset.data[-dirty.data, ]
summary(dataset.data.clean)
head(dataset.data.clean)

################
########Check correlation
################
## We are going to avoid the correlation bigget to 80%
data.correlation <- cor(dataset.data.clean)
data.Names <- colnames(data.correlation)
for (row in 1:nrow(data.correlation)){
  for (column in 1:ncol(data.correlation)){
    if((0.8 < data.correlation[row,column]) && (row != column)){
      print("Hight correlation in:")
      print(data.Names[row])
      print(data.Names[column])
    }
  }
}

# There is a high correlation between Uniformity_Cell_Size and Uniformity_Cell_Shape
# the rest of the correlations involve the output then are not relevant

################
########Now we need to normalize the information
################
dataset.data.clean.norm = dataset.data.clean/10
for (row in 1:nrow(dataset.data.clean.norm)){
  if(0.2 == dataset.data.clean.norm[row,"CellClass_2_benign4_malignant"]){
    dataset.data.clean.norm[row,"CellClass_2_benign4_malignant"] = 0
  }
  else
  {
    dataset.data.clean.norm[row,"CellClass_2_benign4_malignant"] = 1
  }
}


################
########We need to separate now from train to validation data
################
# We are going to take 20% for validation
set.seed(1234)
val.num <- 140
val.Index <- sample(1:nrow(dataset.data.clean.norm), val.num)
val.sample <- dataset.data.clean.norm[val.Index,]
dataset.data.clean.norm.train <- dataset.data.clean.norm[-val.Index,]
dim(val.sample)
dim(dataset.data.clean.norm.train)
head(dataset.data.clean.norm.train)
summary(dataset.data.clean.norm.train)

################
######## Train
################
formula.cancer <- CellClass_2_benign4_malignant~Clump_Thickness+Uniformity_Cell_Size+Marginal_Adhesion+Single_Epithelial_Cell_Size+Bare_Nuclei+Bland_Chromatin+Normal_Nucleoli+Mitoses
?neuralnet

#set.seed(1234)
#nn.cancer <- neuralnet(formula=formula.cancer,
#                      data = dataset.data.clean.norm.train,
#                      hidden=10, threshold=0.001,
#                      linear.output = T,
#                      stepmax=1e+06,
#                      lifesign = "full")
#plot(nn.cancer)
#saveRDS(nn.cancer, file="modelweights.rds")
nn.cancer <- readRDS("modelweights.rds")

plot(nn.cancer)

cancer.prediction.results <- compute(nn.cancer, val.sample[,c("Clump_Thickness","Uniformity_Cell_Size","Marginal_Adhesion","Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses")])
results <- cancer.prediction.results$net.result

for(element in 1:length(results)){
  if( 0.5 < results[element,1]){
    results[element,1] = 1
  }
  else
  {
    results[element,1] = 0
  }
}

comparacion <- results == val.sample[,"CellClass_2_benign4_malignant"]
comparacion.num <- as.numeric(comparacion)
sum(comparacion.num)
eficient.factor <- sum(comparacion.num) / val.num
