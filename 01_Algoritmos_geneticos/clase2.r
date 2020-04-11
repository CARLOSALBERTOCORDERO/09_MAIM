#optimization del uso del agua
library(genalg)
set.seed(1)

vect.1 <- 1:690
vect.2 <- rep(c(0,1),690)
vect.3 <- sample(c(0,1),690, replace = T)
vect.4 <- rep(1,690)
vect.5 <- sample(c(0,1),690, replace = T,prob = c(.8,2))


money.limit <- 1000

limit.clay <- 4
limit.franc <-12
limit.sand <- 15

#Filas
num.rows <- 15
# arboles por fila
num.cols <- 46


mtx.5 <- matrix(vect.5, nrow=15,ncol=46, byrow = T)

#fitness

sensor_price <- 24



check_zeros <- function(vec, n)
{
  zero_count <- 0
  fitness <- 0
  for(i in 1:length(vec))
  {
    if(vec[i] == 0)
    {
      zero_count <- zero_count + 1
    }
    if(vec[i] == 1)
    {
      if(zero_count == n)
      {
        fitness <- fitness + 10
      }
      else if(zero_count == 0)
      {
        fitness <- fitness - 10
      }
      else if(zero_count < n)
      {
        fitness <- fitness + zero_count
      }
      else if(zero_count > n)
      {
        fitness <- fitness + (n - zero_count)
      }
      zero_count <- 0
    }
  }
  return(fitness)
}

density.check <- function(mat, n) {
  x <- dim(mat)[1]
  fitness <- 0 
  for (i in x) {
    fitness <- fitness + check_zeros(mat[i,],n)
  }
  return(fitness)
}

water.optim.1 <- function(c){
  #transform vector to matrix
  fitness <- 0
  if((sum(c)*sensor_price) > money.limit){
    return(0)
  }
  else{
    mtx.all <- matrix(c, nrow=15,ncol=46, byrow = T)
    mtx.clay <- mtx.all[1:limit.clay,]
    fitness <- density.check(mtx.clay,3)
    
    
    mtx.franc <- mtx.all[(limit.clay+1):limit.franc,]
    fitness <- fitness + density.check(mtx.franc,8)
    
    mtx.sand <- mtx.all[(limit.franc+1):limit.sand,]
    fitness <- fitness + density.check(mtx.sand,6)
    
    return(-1 * fitness)
  }
  
}

water.optim.1(vect.5)

cro_1 <- rep(1,690)
cro_2 <- rep(0,690)
cro_3 <- rep(c(1,0),345)
cro_4 <- rep(c(1,0,0),230)
print(water.optim.1(cro_1))
print(water.optim.1(cro_2))
print(water.optim.1(cro_3))
print(water.optim.1(cro_4))



?rbga.bin
ga.tree <- rbga.bin(size=num.rows*num.cols, popSize=500,
                    mutationChance=.01,
                    elitism=50, iters=200, 
                    evalFunc=water.optim.1,
                    verbose = T)
summary(ga.tree, echo = T)

attributes(ga.tree)

ga.tree$population

ga.tree$evaluations

best <- ga.tree$population[ga.tree$evaluations == min(ga.tree$best),][1,]
sum(best) * sensor_price
print(matrix(best, nrow=15,ncol=46, byrow = T))