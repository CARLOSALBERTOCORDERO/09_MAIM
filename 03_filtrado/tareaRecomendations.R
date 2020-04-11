# Read the data
getwd()
setwd("C:/Users/ccordero/Desktop/Maestria/09_MAIM/03_filtrado/steam-video-games/")
music.usage <- read.csv('steam-20k.csv', stringsAsFactors = F)
gamesNames <- vector()
userNames <- vector()
userGameIndex <- 1

# Create Dataset
# Get users
for(user in music.usage[,"user"]){
    if(FALSE ==user %in% userNames){
      userNames[userGameIndex] <- user
      userGameIndex <- userGameIndex + 1
    }
}
userGameIndex <- 1
# Get Games
for(game in music.usage[,"game"]){
  if(FALSE == game %in% gamesNames){
    gamesNames[userGameIndex] <- game
    userGameIndex <- userGameIndex + 1
  }
}
games.usg.dist <- matrix(0, nrow=length(userNames), 
                         ncol=length(gamesNames),
                         dimname=list(userNames,
                                      gamesNames))
dim(games.usg.dist)
# We want to suggest to purchase not to play
for (actionIndex in 1:length(music.usage[,"action"])){
  if("purchase"== music.usage[actionIndex,"action"]){
    games.usg.dist[toString(music.usage[actionIndex,"user"]),music.usage[actionIndex,"game"]] <- 1
  }
}
games.usg.dist["151603712","The Elder Scrolls V Skyrim"]
games.usg.dist["187131847","The Elder Scrolls V Skyrim"]
games.usg.dist["187131847","Dota 2"]
games.usg.dist["128470551","RUSH"]


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
music.usage$game
length(music.usage$game)

# Remove the user column to have just valuable information
#music.usage.nouser <- music.usage[,2:ncol(music.usage)]
#music.usage.nouser <- music.usage[,-1]
#music.usage.nouser <- music.usage
#music.usage.nouser$user <- NULL
#head(music.usage.nouser)
#class(music.usage.nouser)

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


# Now we have to calculate this distance(cos(angle)) between every video game vector 
music.usg.dist <- matrix(NA, nrow=ncol(games.usg.dist), 
                         ncol=ncol(games.usg.dist),
                         dimname=list(colnames(games.usg.dist),
                                      colnames(games.usg.dist)))

for (i in 1:ncol(games.usg.dist)) {
  print(gamesNames[i])
  print(i)
  for(j in 1:ncol(games.usg.dist)) {
    music.usg.dist[i,j] <- dist.cosine(games.usg.dist[,i],
                                       games.usg.dist[,j])
  } 
}

dim(music.usg.dist)
# Lets display some results
#colnames(games.usg.dist)
music.usg.dist[1:5,1:5]

# Now lets find the 10 users more similar to a provided user
# We need first to convert again to data frame the matrix created (as matrix it can not be computed)
music.usage.dist.df <- data.frame(music.usg.dist) 

music.usage.similar.artists <- matrix(NA, nrow=ncol(music.usage.dist.df), 
                            ncol=10,
                            dimname=list(gamesNames))

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

music.usage.similar.artists["The.Elder.Scrolls.V.Skyrim",]
music.usage.similar.artists["Fallout.4",]


# Now we are going to provide recomendations
# Lets map User vs group
user.data <- matrix(NA, nrow=length(userNames), 
                         ncol=length(gamesNames),
                         dimname=list(userNames,
                                      gamesNames))
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

# Clean up

# if(FALSE == ("The.Elder.Scrolls.V.Skyrim" %in% colnames(music.usg.dist))){
#   print("Dragon.Age.Origins.-.Ultimate.Edition")
# }
# 
# if(FALSE == ("The.Elder.Scrolls.V.Skyrim" %in% colnames(music.usage.dist.df))){
#   print("Dragon.Age.Origins.-.Ultimate.Edition")
# }
# 
# i <- 2
# for (j in 1:ncol(user.data)){
#   user <- rownames(user.data)[i]
#   artist <- colnames(user.data)[j]
#   if(FALSE == (artist %in% colnames(music.usage.dist.df))){
#     #print(artist)
#     user.data[user,artist] = -1;
#   }
#   else
#   {
#     if (games.usg.dist[user,artist]==1) {
#       user.data[user,artist] = -1;
#     }
#     else
#     {
#       # Take the value  of the cosine distance of the 6 more similar artist to the provided artist
#       top.artists <- head(n=6,
#                           music.usage.dist.df[order(
#                             music.usage.dist.df[,artist], decreasing=T),][artist])
#       # Remove the first one because it is the same artist that the provided
#       top.artists.names <- rownames(top.artists)[-1]
#       top.artists.sim <- (top.artists[,1])[-1]
#       
#       # Take the answer (1 or 0) from the user for the calculated 5 more similar groups to the group analized
#       top.artists.history <-
#         games.usg.dist[user,top.artists.names]
#       # give a score for such artist for the current user
#       user.data[user,artist] <-
#         score.recommendation(top.artists.history, top.artists.sim)
#     }
#   }
# }
# user.data[user,]

# Clean up

for (i in 1:nrow(user.data)) {
  for (j in 1:ncol(user.data)){
    user <- rownames(user.data)[i]
    artist <- colnames(user.data)[j]
    # validaci??n 1
    if(FALSE == (artist %in% colnames(music.usage.dist.df))){
      #print(artist)
      user.data[user,artist] = -1;
    }
    else
    {
      if (games.usg.dist[user,artist]==1) {
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
          games.usg.dist[user,top.artists.names]
        # give a score for such artist for the current user
        user.data[user,artist] <-
          score.recommendation(top.artists.history, top.artists.sim)
      }
    }
  }
}

head(user.data)

# Ignore the error, maybe there is a null in the matrix
# Recomendation for user 1
head(sort(user.data[4,], decreasing = T))



#Recomendations over 0.8
for (i in 1:nrow(user.data)) {
  for (j in 1:ncol(user.data)){
    user <- rownames(user.data)[i]
    artist <- colnames(user.data)[j]
    if(is.na(user.data[i,j]) == FALSE){
      if(user.data[i,j] > 0.8){
        print(c("We suggest to user: ", user, "the game: ", artist))
      }
    }
  }
}


# Lets analyze the result user 11373749, artist limp.bizkit
games.usg.dist["11373749","Hitman.Sniper.Challenge"] # is not liked before = 0
games.usg.dist["11373749","Hitman.Sniper.Challenge"]
top.artists <- head(n=6,
                    music.usage.dist.df[order(
                      music.usage.dist.df[,"Hitman.Sniper.Challenge"], decreasing=T),]["Hitman.Sniper.Challenge"])
top.artists.names <- rownames(top.artists)[-1]
top.artists.sim <- (top.artists[,1])[-1]
top.artists.names
top.artists.sim

top.artists.history <-
  games.usg.dist["11373749",top.artists.names]
#         linkin.park papa.roach rammstein disturbed system.of.a.down
#11373749           1          1         1         1                0
user.hitman.recomendation <-  score.recommendation(top.artists.history, top.artists.sim)
# 0.8286139 Highly recomended

