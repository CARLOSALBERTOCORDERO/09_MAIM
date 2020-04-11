install.packages("genalg")
items <- c("Bañarme","Desayunar","Cambiarme","Lavarme_la_boca_después_de_desayuno","Manejar_a_trabajo_ida_vuelta","Pagar_refrendo","Atender_junta_1","Atender_junta_2","Realizar_actividad_1","Realizar_actividad_2","Realizar_actividad_3","Salir_fuera_a_comer","Comer","Lavarme_la_boca_después_de_comer","Platicar_con_colegas","Café_con_colegas","Jugar_Smash_en_la_oficina","Leer_libro","Momento_de_relajación_en_la_oficina","Capacitación_en_línea","Limpiar_cuarto","Ver_series_en_netflix","Visitar_padres","Segundo_baño_antes_de_visita","Cambiarme_para_las_visitas")
costo <- c(20L,25L,10L,5L,50L,40L,120L,60L,180L,210L,270L,20L,60L,5L,25L,20L,60L,50L,25L,120L,90L,60L,180L,15L,10L)
beneficio <- c(50L,60L,80L,50L,30L,20L,70L,50L,80L,70L,80L,20L,90L,30L,40L,20L,10L,20L,30L,40L,30L,5L,30L,70L,70L)
plan.viernes <- data.frame(
items = items,
 costo = costo,
beneficio = beneficio )
plan.viernes

time.limit = 660

fitness.generic <- function(x){
   items.cost <- x %*% plan.viernes$costo
   items.beneficio <- x %*% plan.viernes$beneficio
   if(items.cost > time.limit)
     {
       return(0)
      }
   else
     {
       return(-items.beneficio)
     }
}
candidate1 = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)

fitness.generic(candidate1)

library(genalg)
ga.tree <- rbga.bin(size=nrow(plan.viernes), popSize=100, mutationChance=0.01, elitism=20, iters=100, evalFunc=fitness.generic,verbose=TRUE)

class(ga.tree)

summary(ga.tree, echo=TRUE)



