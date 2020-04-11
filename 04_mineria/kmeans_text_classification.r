install.packages("tm")
library(tm)

#########################

getwd()
setwd("C:/Users/ccordero/Desktop/Maestria/09_MAIM/04_mineria/")
# 1 we need to read the document
frases.fut.viajes <- read.csv('textoClase.txt', stringsAsFactors = F, sep="|", header = F)


# frases.fut.viajes is a data frame
str(frases.fut.viajes)
class(frases.fut.viajes)
# frase returns a content with every line
class(frases.fut.viajes$frase)
frases.fut.viajes$frase[2]
colnames(frases.fut.viajes)
colnames(frases.fut.viajes) <- c("frase")


# 2 We need to convert the document in corpus to be able to work
datos.clase.junio.2020.df <- frases.fut.viajes
# we need to convert the data frame into a corpus to manipulate it as lines of text
base.corpus <- Corpus(VectorSource(datos.clase.junio.2020.df$frase))
class(base.corpus)
ndocs <- length(base.corpus)
inspect(base.corpus)
# content returns a content with every line in a corpus
base.corpus$content[2]

# 3 we need to declare some limits and clean the text 
# thresholds - ignore exremelly rare or common words
minTermFreq <- ndocs * 0.01
maxTermFreq <- ndocs * 0.8
stopwords("spanish")
stopwords("english")
# define stopwords that is a list of words that are going to be removed from the text
# by default the stopwords are english then we need to set the spanish in this case
# The third argument just have to be a list not necessarilly predefined stopwords, in fact
# it is much better to define or list of stop words
base.corpous <- tm_map(base.corpus, removeWords, c(stopwords("spanish"), "cualquier"))

# 4 Now we create a tokenized matrix representation of fraces vs word
# Example frace: Hello world world
#         Hello   world   tv
# frace     1       2     0
dtm = DocumentTermMatrix(base.corpous,
                         control = list(
                           stopwords = TRUE,
                           wordLengths= c(1,Inf),
                           removePunctuation=T,
                           removeNumbers=T,
                           bounds = list(global = c(minTermFreq,maxTermFreq))
                           )
                         )
class(dtm)
# dtm will be an object we need to convert into a matrix
dtm.matrix <- as.matrix(dtm)
dim(dtm.matrix)
dtm.matrix[1:5,1:10]

# 5 We apply inverse documnet frequency
# First we need to understand that if a word appear just in few fraces it will be more usefull
# to classify. Then we are going to take the matrix that we have and divide all the numbers over
# the number of fraces that have it, and not only that also the number the times that appears
# in the frace and others.
# Remember that the method receives a DTM object not a matrix
# mathit{idf}_i = \log_2 \frac{|D|}{|\{d \mid t_i \in d\}|
?weightTfIdf
dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix.tf.idf <- as.matrix(dtm)
nrow(dtm.matrix.tf.idf)
dtm.matrix.tf.idf[1:5,1:10]
class(dtm.matrix.tf.idf)
dim(dtm.matrix.tf.idf)
# remove empty rows because maybe after removing stopwords or too frequent or unfrequent words maybe a row is empty now
rowTotals <- apply(dtm.matrix.tf.idf,1,sum)
dtm.matrix.tf.idf <- dtm.matrix.tf.idf[rowTotals > 0,]
dim(dtm.matrix.tf.idf)

# 6 apply Kmeans
# Kmeans is designed to make clusters. We have to define a number  of centroids, and every centroid, this start
# randomly positioned but they will be move to the center of the points closer to such centroid. Many iterations can be performed,
# or it can stops if the error is not changing.
# The error in Kmeans is the square mean error of the distances from the points of a cluster to its centroid. We have one error
# mesurement per cluster but we can add all of them to have a total.
# The most complicated point is to define the number of centroids.
# We are going to start doing an analysis to know how many centroids are ideal.
# we are going to apply kmeans from 1 centroid to 10 and check the error(withinss) then we are going to plot the result
# When we start watching progres we are going to take that result as the best number of centroids. We call it elbow rule.
base.kmeans <- dtm.matrix.tf.idf
# We need to treat it like dataframe
classified.docs <- as.data.frame(base.kmeans)
wss <- vector()
for(i in 1:10){
  set.seed(1234)
  wss[i] <- sum(kmeans(base.kmeans, centers = i, iter.max = 100)$withinss)
}
plot(1:10, wss, type="b", xlab="Groups", ylab="Error")
# We can see that after the point 2 and 4 there is a gradual improvement then we are going to take 2 centers because
# a rule in machine learning is to take the simples model as posible.

# 7 Evaluate the group
set.seed(1000)
kmeans.model <- kmeans(base.kmeans, centers = 2, iter.max = 100)
# kmeans can also provide the cluster that every point belong with the atribute cluster
datos.clase.junio.2020.df$Grupo <- kmeans.model$cluster[1:dim(dtm.matrix.tf.idf)[1]]
# The result is that we have 2 goups and we can group them in this way :  
# 1 1 2 2 2 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# 1 shall be for traveling and 2 for futbol
set.seed(1000)
kmeans.model <- kmeans(base.kmeans, centers = 4, iter.max = 100)
# kmeans can also provide the cluster that every point belong with the atribute cluster
datos.clase.junio.2020.df$Grupo <- kmeans.model$cluster[1:dim(dtm.matrix.tf.idf)[1]]
# The result is that we have 2 goups and we can group them in this way : 
# Results are much better if the lenght of the fraces is the same or similar
# 3 3 2 2 2 3 3 1 3 3 3 2 3 3 3 3 3 3 3 3 4 3 3 3 3 3 3 3 3 3 3
# Applying 4 centers
# 2,4 futbol
# 1,3 for travel
# it is a little bit more precise



