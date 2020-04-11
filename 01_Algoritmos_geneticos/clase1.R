install.packages("genalg")
items <- c("lavar_dientes","ducharme","desayunar","cafe","ejercicio","cambiarme","leer","meditar","perro","regar_el_jardin")
costo <- c(3L,15L,12L,7L,25L,5L,10L,15L,15L,15L)
beneficio <- c(90L, 80L, 40L, 5L, 5L, 100L, 1L, 20L, 50L, 50L)
plan.maiana <- data.frame(
items = items,
 costo = costo,
beneficio = beneficio )
plan.maiana

time.limit = 30

fitness.generic <- function(x){
   items.cost <- x %*% plan.maiana$costo
   items.s.p <- x %*% plan.maiana$beneficio
   if(items.cost > time.limit)
     {
       return(0)
      }
   else
     {
       return(-items.s.p)
     }
}
candidate1 = c(1,0,0,0,0,0,0,0,0,1)

fitness.generic(candidate1)

supercandidate1 = c(1,1,0,1,0,1,0,0,0,0)

fitness.generic(supercandidate1)

library(genalg)
ga.tree <- rbga.bin(size=nrow(plan.maiana), popSize=100, mutationChance=0.01, elitism=20, iters=50, evalFunc=fitness.generic,verbose=TRUE)

class(ga.tree)

summary(ga.tree, echo=TRUE)



