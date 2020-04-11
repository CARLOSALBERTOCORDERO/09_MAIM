# Read the data
getwd()
setwd("C:/Users/ccordero/Desktop/Maestria/09_MAIM/03_filtrado/")
music.usage <- read.csv('lastfm-matrix-germany.csv', stringsAsFactors = F)
# We need to check that all that data is consistent TRUE
complete.cases(music.usage)
# Display data
str(music.usage)
# Check the first elemets
head(music.usage)
# User index
rownames(music.usage)
# colums
colnames(music.usage)
# preferences of user 1, check how the information is displayed, the columns names is also displayed.
music.usage[1,]
# We can check one column using $
music.usage$abba
length(music.usage$abba)

# Remove the user column to have just valuable information
#music.usage.nouser <- music.usage[,2:ncol(music.usage)]
#music.usage.nouser <- music.usage[,-1]
music.usage.nouser <- music.usage
music.usage.nouser$user <- NULL
head(music.usage.nouser)
class(music.usage.nouser)

# Function to get the cosin of the angle between vectors
vector.1 <- c(1,2,3)
magnit.vec <- function(v1) {
  return(sqrt(sum(v1 * v1)))
}
prod.punt <- function(v1,v2) {
  return(sum(v1 * v2))
}
dist.cosine <- function(v1, v2) {
  return(prod.punt(v1,v2)/(magnit.vec(v1)*magnit.vec(v2)))
}

magnit.vec(vector.1)
# How parallel are they, 1 is the maximum
dist.cosine(c(1,1,1),c(1,1,1))
dist.cosine(c(1,0,1),c(0,1,0))
dist.cosine(c(5,10,100),c(-5,-10,-99))
dist.cosine(c(1,1,1,1),c(1,1,1,1000))
dist.cosine(c(1,1,1,1),c(3,3,3,3))

# Now we have to calculate this distance(cos(angle)) between every music group vector 
music.usg.dist <- matrix(NA, nrow=ncol(music.usage.nouser), 
                         ncol=ncol(music.usage.nouser),
                         dimname=list(colnames(music.usage.nouser),
                                      colnames(music.usage.nouser)))

for (i in 1:ncol(music.usage.nouser)) {
  for(j in 1:ncol(music.usage.nouser)) {
    music.usg.dist[i,j] <- dist.cosine(music.usage.nouser[,i],
                                       music.usage.nouser[,j])
  } 
}

dim(music.usg.dist)
# Lets display some results
colnames(music.usage.nouser)
music.usg.dist[1:5,1:5]

# Now lets find the 10 users more similar to a provided user
# We need first to convert again to data frame the matrix created (as matrix it can not be computed)
music.usage.dist.df <- data.frame(music.usg.dist) 

music.usage.similar.artists <- matrix(NA, nrow=ncol(music.usage.dist.df), 
                            ncol=10,
                            dimname=list(colnames(music.usage.nouser)))

dim(music.usage.similar.artists)

# we can agroup by similarity if we just order the  vectors
rownames(music.usage.dist.df[order(music.usage.dist.df[,1], decreasing=T),])[1:10]
# Lets apply this logic but for all collumns the first one is allways the group itself then we are going to take from 2 to 11
for(i in 1:ncol(music.usage.dist.df)) {
  music.usage.similar.artists[i,] <- 
    rownames(
      music.usage.dist.df[
        order(music.usage.dist.df[,i], decreasing=T),])[2:11] 
}

music.usage.similar.artists["aerosmith",]
music.usage.similar.artists["the.beatles",]


# Now we are going to provide recomendations
# Lets map User vs group
user.data <- matrix(NA, nrow=nrow(music.usage), 
                         ncol=ncol(music.usage)-1,
                         dimname=list(music.usage$user,
                                      colnames(music.usage.nouser)))
dim(user.data)

# The score of one artist will consider the cosine distance of the artist that like the user (the value is normalized using all the cosine distances)
# For example if history is all 1s the the recomendation score will be 1.0 
# Apparently this function works better if our data has many 0s
score.recommendation <- function(history, similarities) {
  return(sum(history*similarities)/sum(similarities))
}
# Apparently this function works better if our data has many 1s
#score.recommendation <- function(history, similarities) {
#  return(sum(history*similarities)/length(history))
#}

for (i in 1:nrow(user.data)) {
  for (j in 1:ncol(user.data)){
    user <- rownames(user.data)[i]
    artist <- colnames(user.data)[j]
    # validaci??n 1
    if (music.usage[user,artist]==1) {
      user.data[user,artist] = -1;
    }
    else
    {
      # Take the value  of the cosine distance of the 6 more similar artist to the provided artist
      top.artists <- head(n=6,
                          music.usage.dist.df[order(
                            music.usage.dist.df[,artist], decreasing=T),][artist])
      # Remove the first one because it is the same artist that the provided
      top.artists.names <- rownames(top.artists)[-1]
      top.artists.sim <- (top.artists[,1])[-1]
      
      # Take the answer (1 or 0) from the user for the calculated 5 more similar groups to the group analized
      top.artists.history <-
        music.usage[user,c("user",top.artists.names)][-1]
      # give a score for such artist for the current user
      user.data[user,artist] =
        score.recommendation(top.artists.history, top.artists.sim)
    }
  }
}

# Ignore the error, maybe there is a null in the matrix
# Recomendation for user 1
head(sort(user.data[1,], decreasing = T))

#Recomendations over 0.8
for (i in 1:nrow(user.data)) {
  for (j in 1:ncol(user.data)){
    user <- rownames(user.data)[i]
    artist <- colnames(user.data)[j]
    if(user.data[i,j] > 0.8){
      print(c("We suggest to user: ", user, "the artist: ", artist))
    }
  }
}

# Lets analyze the result user 150, artist limp.bizkit
music.usage["150","limp.bizkit"] # is not liked before = 0
top.artists <- head(n=6,
                    music.usage.dist.df[order(
                      music.usage.dist.df[,"limp.bizkit"], decreasing=T),]["limp.bizkit"])
top.artists.names <- rownames(top.artists)[-1]
top.artists.sim <- (top.artists[,1])[-1]
top.artists.names
top.artists.sim
# "linkin.park"      "papa.roach"       "rammstein"        "disturbed"        "system.of.a.down"

# 0.3482495           0.2704926         0.2352360           0.2282375             0.2238397
top.artists.history <-
  music.usage["150",c("user",top.artists.names)][-1]
#         linkin.park papa.roach rammstein disturbed system.of.a.down
#150           1          1         1         1                0
user150.linkbiskit.recomendation <-  score.recommendation(top.artists.history, top.artists.sim)
# 0.3482495+0.2704926+0.2352360+0.2282375 / 0.3482495+0.2704926+0.2352360+0.2282375+0.2238397
# 0.8286139 Highly recomended

