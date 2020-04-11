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


#####
## Los sensores en case zona Clay, franc, o sand van a tener diferente peso
## Los pesos seran los siguientes:
## Clay: 30
## franc: 20
## sand: 10
## La razon es porque la humedad de la tierra se absorbe de diferente manera 
## y dependiendo la zona hay que estar mas al pendiente de la humedad
## Lo qu ese espera es tener mas sesores en la zona que tenga mas peso en 
## este caso Clay

weigh.clay <- rep(30,num.cols*limit.clay)
print(matrix(weigh.clay, nrow=limit.clay,ncol=46, byrow = T))

weigh.franc <- rep(20,num.cols*(limit.franc - limit.clay))
print(matrix(weigh.franc, nrow=(limit.franc - limit.clay),ncol=46, byrow = T))

weigh.sand <- rep(10,num.cols*(limit.sand- limit.franc))
print(matrix(weigh.sand, nrow=(limit.sand- limit.franc),ncol=46, byrow = T))


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
    
    ######################
    ## Multiplicación de cada sensor por su correspondiente peso
    c.clay <- c[1:(limit.clay * num.cols)]
    c.franc <- c[((limit.clay * num.cols) + 1):(limit.franc * num.cols)]
    c.sand <- c[((limit.franc * num.cols)+1):(limit.sand * num.cols)]
    
    fitness.weigh.clay <- weigh.clay %*% c.clay
    fitness.weigh.franc <- weigh.franc %*% c.franc
    fitness.weigh.sand <- weigh.sand %*% c.sand
    
    ######################
    return(-1 * (fitness + fitness.weigh.clay + fitness.weigh.franc + fitness.weigh.sand))
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