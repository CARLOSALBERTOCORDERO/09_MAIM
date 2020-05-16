###########################
###########################
#Load information
###########################
###########################
#install.packages("rstudioapi")
library(rstudioapi)
#install.packages("neuralnet")
library(neuralnet)
#install.packages("genalg")
library(genalg)

currentPath <- dirname(rstudioapi::getSourceEditorContext()$path)
#currentPath <- getwd() 
setwd(currentPath)

# 0 miss
# 1 late
# 2 Present
# Matrix of course vs assistance
# 6 courses per semester
load("AsistenciasTotales.R")
# first student
#asistencias.totales[[1]]

load("perfilAlumnos.R")
#head(perfil.alumnos,1)
#summary(perfil.alumnos)

# exam grade over 20
load("ResultadosExamenes.R")
#resultados.examenes.totales[[1000]]

# works over 20
load("ResultadoTrabajos.R")
#resultados.trabajos.totales[[1]]

# times library enters per course
load("UsoBiblioteca.R")
#uso.biblioteca.totales[[1000]]
#uso.biblioteca.totales[[1]]

# times platform is used per course
load("UsoPlataforma.R")
#uso.plataforma.totales[[1]]

# times a book is reserved per course
load("ApartadoDeLibros.R")
#separacion.libros.totales[[1000]]

# 1 beca
# 0 no beca
load("Becas.R")
#distribucion.becas[1]

# Payment per semester
# 2 paid on time
# 1 late
# 0 not paid or paid too late
load("HistorialPagos.R")
#registro.pagos[[1000]]

# Puntuation of the teacher over 10(mean value)
load("EvaluacionProfesorMateria.R")
#encuesta.profesor.materia

# 1 change of career
load("CambioCarrera.R")
#cambio.carrera

###########################
###########################
# Create input values
###########################
###########################
# Define the number of semesters to analyze
semesters.analysis.num <- 3
student.number <- length(perfil.alumnos[[1]])

# Get data to create the input matrix
# one-hot representation for economic level
economic.levels <- c("economic.level.4","economic.level.3","economic.level.2","economic.level.1")
payment.status.number <- 2
elements.analysis <- c()
elements.analysis <- c(elements.analysis,colnames(perfil.alumnos))
elements.analysis <- c(elements.analysis,"scholarship")
elements.analysis <- c(elements.analysis,"prev.change")
elements.analysis <- elements.analysis[elements.analysis != "evalucion.socioeconomica"]
elements.analysis <- c(elements.analysis,economic.levels)
elements.analysis <- elements.analysis[elements.analysis != "payment.mean"]
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("payment.Semester", toString(semester), sep="")
  elements.analysis <- c(elements.analysis, temp.string)
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("assistance", toString(semester), sep="")
  elements.analysis <- c(elements.analysis, temp.string)
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("exams", toString(semester), sep="")
  elements.analysis <- c(elements.analysis, temp.string)
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("works", toString(semester), sep="")
  elements.analysis <- c(elements.analysis, temp.string)
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("library.entrances", toString(semester), sep="")
  elements.analysis <- c(elements.analysis, temp.string)
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("platform.times", toString(semester), sep="")
  elements.analysis <- c(elements.analysis, temp.string)
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("books.reserved", toString(semester), sep="")
  elements.analysis <- c(elements.analysis, temp.string)
}
elements.analysis <- c(elements.analysis, "desertion")

student.data <- length(elements.analysis)
studen.id <- array(1:1000)
student.input <- matrix(0, nrow=student.number, 
                        ncol=student.data,
                        dimname=list(studen.id,
                                     elements.analysis))

############
# Fill in matrix
###########
courses.num <- length(asistencias.totales[[1]][1,])
courses.per.semester <- courses.num / 9
days.per.course <- length(asistencias.totales[[1]][,1])
exam.number <- length(resultados.examenes.totales[[1]][,1])
works.number <- length(resultados.trabajos.totales[[1]][,1])
payments <- length(registro.pagos[[1]][,1])

# Fill in assistance
mean.assistance <- 0
for (student in 1:student.number){
  for(semester in 1:(semesters.analysis.num)){
    mean.assistance <- 0
    for(course in (((semester-1)*courses.per.semester)+1):(courses.per.semester * semester)){
      for(day in 1:days.per.course){
        mean.assistance <- mean.assistance + asistencias.totales[[student]][day,course]
      } 
    }
    mean.assistance <- mean.assistance / ( courses.per.semester * days.per.course)
    column.name <- paste("assistance",toString(semester), sep="")
    student.input[student, column.name] <- mean.assistance
  }
}
# Fill in with student profile
for (student in 1:student.number){
  # one-hot representation for economic level and payment
  for(profile in colnames(perfil.alumnos)){
    if("evalucion.socioeconomica" != profile){
      student.input[student, profile] <- perfil.alumnos[student, profile]
    }
    else{
      colum.name <- paste("economic.level",toString(perfil.alumnos[student, profile]),sep=".")
      student.input[student, colum.name] <- 1
    }
  }
}
# Fill in exam results
mean.exam <- 0
for (student in 1:student.number){
  for(semester in 1:(semesters.analysis.num)){
    mean.exam <- 0
    for(course in (((semester-1)*courses.per.semester)+1):(courses.per.semester * semester)){
      for(exam in 1:exam.number){
        mean.exam <- mean.exam + resultados.examenes.totales[[student]][exam,course]
      } 
    }
    mean.exam <- mean.exam / ( courses.per.semester * exam.number)
    column.name <- paste("exams",toString(semester), sep="")
    student.input[student, column.name] <- mean.exam
  }
}
# Fill in works results
mean.works <- 0
for (student in 1:student.number){
  for(semester in 1:(semesters.analysis.num)){
    mean.works <- 0
    for(course in (((semester-1)*courses.per.semester)+1):(courses.per.semester * semester)){
      for(work in 1:works.number){
        mean.works <- mean.works + resultados.trabajos.totales[[student]][work,course]
      } 
    }
    mean.works <- mean.works / ( courses.per.semester * works.number)
    column.name <- paste("works",toString(semester), sep="")
    student.input[student, column.name] <- mean.works
  }
}

# Fill in times in library
times.library <- 0
for (student in 1:student.number){
  for(semester in 1:(semesters.analysis.num)){
    times.library <- 0
    for(course in (((semester-1)*courses.per.semester)+1):(courses.per.semester * semester)){
      times.library <- times.library + ceiling(uso.biblioteca.totales[[student]][course])
    }
    times.library <- times.library / ( courses.per.semester)
    column.name <- paste("library.entrances",toString(semester), sep="")
    student.input[student, column.name] <- times.library
  }
}

# Fill in platform use
times.platform <- 0
for (student in 1:student.number){
  for(semester in 1:(semesters.analysis.num)){
    times.platform <- 0
    for(course in (((semester-1)*courses.per.semester)+1):(courses.per.semester * semester)){
      times.platform <- times.platform + ceiling(uso.plataforma.totales[[student]][course])
    }
    times.platform <- times.platform / ( courses.per.semester)
    column.name <- paste("platform.times",toString(semester), sep="")
    student.input[student, column.name] <- times.platform
  }
}
separacion.libros.totales[[1]]
# Fill in books Reserved
books.reserved <- 0
for (student in 1:student.number){
  for(semester in 1:(semesters.analysis.num)){
    books.reserved <- 0
    for(course in (((semester-1)*courses.per.semester)+1):(courses.per.semester * semester)){
      books.reserved <- books.reserved + ceiling(separacion.libros.totales[[student]][course])
    }
    books.reserved <- books.reserved / ( courses.per.semester)
    column.name <- paste("books.reserved",toString(semester), sep="")
    student.input[student, column.name] <- books.reserved
  }
}
# Fill in scholarship
for (student in 1:student.number){
  student.input[student, "scholarship"] <- distribucion.becas[student]
}
# Fill in payments
mean.payment <- 0
for (student in 1:student.number){
  for(semester in 1:(semesters.analysis.num)){
    mean.payment <- 0
    for(pay.num in 1:payments){
      mean.payment <- mean.payment + asistencias.totales[[student]][pay.num,semester]
    }
    mean.payment <- mean.payment / ( payments)
    column.name <- paste("payment.Semester",toString(semester), sep="")
    student.input[student, column.name] <- mean.payment
  }
}
# Fill in Change of career
for (student in 1:student.number){
  student.input[student, "prev.change"] <- cambio.carrera[student]
}



############
# Cluster the students to find the group of risk
###########
# Find the number of groups
dtm.matrix.tf.idf <- as.matrix(student.input)
base.kmeans <- dtm.matrix.tf.idf
# We need to treat it like dataframe
classified.docs <- as.data.frame(base.kmeans)
wss <- vector()
for(i in 1:8){
  set.seed(1234)
  wss[i] <- sum(kmeans(base.kmeans, centers = i, iter.max = 1000)$withinss)
}
plot(1:8, wss, type="b", xlab="Groups", ylab="Error")
# Inflection at point 2 and 4

# Evaluate with 2 Groups
set.seed(1000)
kmeans.model <- kmeans(base.kmeans, centers = 2, iter.max = 1000)
# Group 1
student.input[1,]
student.input[2,]
student.input[3,]
# Group 2
student.input[4,]
student.input[6,]
student.input[7,]
# Evaluate with 4 Groups
set.seed(1000)
kmeans.model <- kmeans(base.kmeans, centers = 4, iter.max = 1000)
# Group 1
student.input[1,]
# Group 2
student.input[2,]
# Group 3
student.input[6,]
# Group 4
student.input[4,]
############
# Analyze information
###########
kmeans.model$centers
plot(kmeans.model$withinss)
#Group1:  Best profile, More previews changes than others, group with the highest economic level, the rest of the economic levels 
#         are balanced, balanced in semester payments, balanced in assistances all the semesters, balanced in exams and works,
#         balanced in library assistance and platform usage, balanced in books reserved.
#
#Group2:  The profile is balanced, balanced at all economic levels, the one with more payment complications every semester,
#         the worst in assistance every semester, the worst in exams every every semester, the worst in works every semester,
#         the worst in library assistance and platform usage, worst in books reserved. 
#
#Group3:  The worst profile, many previews changes, the one with more scholarships, most of them in middle economic group, 
#         balanced in exams and works, balanced in library and platform usage, balanced in book reservations.
#        
#Group4:  Balanced profile, the one with less scholarships and previews changes, the group with lowest economic level, the
#         rest of the economic levels are balanced, balanced in payments, the one with better assistance, the best in exams
#         and works, the one with more library entrances, platform usage and books reserved.

# Centroids graphs
plot(student.input[,c("promedio.preparatoria","exams3")], col = kmeans.model$cluster)
points(kmeans.model$centers[,c("promedio.preparatoria","exams3")], col=1:4, pch=6, cex=5)

plot(student.input[,c("economic.level.1","payment.Semester3")], col = kmeans.model$cluster)
points(kmeans.model$centers[,c("economic.level.4","payment.Semester3")], col=1:4, pch=6, cex=5)

plot(student.input[,c("exams3","assistance3")], col = kmeans.model$cluster)
points(kmeans.model$centers[,c("exams3","assistance3")], col=1:4, pch=6, cex=5)


# Group 2 is going to be taken as group of risk
# Set desertions according to analysis
student.index <- 1
for(student.group in kmeans.model$cluster){
  if(2 == student.group){
    student.input[student.index, "desertion"] = 1
  }
  student.index = student.index + 1
}


############
# Normalizing values
###########
student.input.norm <- student.input

assistance.mean.max <- 2
gerder.offset <- 1
profile.grade.max <- 100
age.max <- max(student.input[,"edad.ingreso"], na.rm = FALSE)
behavior.max <- 20
exam.works.max <- 20
library.times.max <- max(student.input[,"library.entrances1"], na.rm = FALSE)
for(semester in 1:(semesters.analysis.num)){
  temp.string <- paste("library.entrances", toString(semester), sep="")
  if(max(student.input[,temp.string], na.rm = FALSE) > library.times.max){
    library.times.max <- max(student.input[,temp.string], na.rm = FALSE)
  }
}
platform.times.max <- max(student.input[,"platform.times1"], na.rm = FALSE)
for(semester in 1:(semesters.analysis.num)){
  temp.string <- paste("platform.times", toString(semester), sep="")
  if(max(student.input[,temp.string], na.rm = FALSE) > platform.times.max){
    platform.times.max <- max(student.input[,temp.string], na.rm = FALSE)
  }
}
books.reserved.max <- max(student.input[,"books.reserved1"], na.rm = FALSE)
for(semester in 1:(semesters.analysis.num)){
  temp.string <- paste("books.reserved", toString(semester), sep="")
  if(max(student.input[,temp.string], na.rm = FALSE) > books.reserved.max){
    books.reserved.max <- max(student.input[,temp.string], na.rm = FALSE)
  }
}
payment.mean.max <- 2

student.input.norm[,"genero"] <- student.input[,"genero"] - gerder.offset
student.input.norm[,"admision.letras"] <- student.input[,"admision.letras"] / profile.grade.max
student.input.norm[,"admision.numeros"] <- student.input[,"admision.numeros"] / profile.grade.max
student.input.norm[,"promedio.preparatoria"] <- student.input[,"promedio.preparatoria"] / profile.grade.max
student.input.norm[,"edad.ingreso"] <- student.input[,"edad.ingreso"] / age.max
student.input.norm[,"nota.conducta"] <- student.input[,"nota.conducta"] / behavior.max
student.input.norm[,"scholarship"] <- student.input[,"scholarship"]
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("payment.Semester", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / payment.mean.max
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("assistance", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / assistance.mean.max
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("exams", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / exam.works.max
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("works", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / exam.works.max
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("library.entrances", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / library.times.max
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("platform.times", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / platform.times.max
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("books.reserved", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / books.reserved.max
}
student.input.norm[,"prev.change"] <- student.input[,"prev.change"]


############
# Separate input data from validation data
###########
val.num <- 200
Students.for.study <- 100
set.seed(1234)
student.selected <- sample(1:nrow(student.input.norm), Students.for.study)
Students.for.study.sample <- student.input.norm[student.selected,]
student.input.norm.sample <- student.input.norm[-student.selected,]
set.seed(1234)
val.index <- sample(1:nrow(student.input.norm.sample), val.num)
val.sample <- student.input.norm.sample[val.index,]
student.input.norm.sample <- student.input.norm.sample[-val.index,]
dim(Students.for.study.sample)
dim(student.input.norm.sample)
dim(val.sample)


################
########Check correlation
################
## We are going to avoid the correlation greater than 80%
data.correlation <- cor(student.input.norm.sample)
data.Names <- colnames(data.correlation)
for (row in 1:nrow(data.correlation)){
  for (column in 1:ncol(data.correlation)){
    if(FALSE == is.na(data.correlation[row,column])){
      if((0.8 < data.correlation[row,column]) && (row != column)){
        print("Hight correlation in:")
        print(data.Names[row])
        print(data.Names[column])
      }
    }
  }
}

#The following correlations were found:
#"economic.level.4" <- "genero"
#"promedio.preparatoria" <- "admision.letras", "admision.numeros", "edad.ingreso", "nota.conducta"
#"exams1" <- "works1", "library.entrances1", "platform.times1", "books.reserved1"
#"exams2" <- "works1", "library.entrances2", "platform.times2", "books.reserved2"
#"exams3" <- "works1", "library.entrances3", "platform.times3", "books.reserved3"

################
########Train
################
formula.students <-desertion~promedio.preparatoria+scholarship+prev.change+economic.level.4+economic.level.3+economic.level.2+economic.level.1+payment.Semester1+payment.Semester2+payment.Semester3+assistance1+assistance2+assistance3+exams1+exams2+exams3

# set.seed(1234)
# nn.students <- neuralnet(formula=formula.students,
#                       data = student.input.norm.sample,
#                       hidden=c(32,8), threshold=0.001,
#                       linear.output = T,
#                       stepmax=8e+06,
#                       lifesign = "full")
# 
# saveRDS(nn.students, file="modelweights.rds")
nn.students <- readRDS("modelweights.rds")
plot(nn.students)

################
######## Evaluate NN eficiency
################
desertion.prediction.results <- compute(nn.students, val.sample[,c("promedio.preparatoria","scholarship","prev.change","economic.level.4","economic.level.3","economic.level.2","economic.level.1","payment.Semester1","payment.Semester2","payment.Semester3","assistance1","assistance2","assistance3","exams1","exams2","exams3")])
results <- desertion.prediction.results$net.result

for(element in 1:length(results)){
  if( 0.5 < results[element,1]){
    results[element,1] = 1
  }
  else
  {
    results[element,1] = 0
  }
}

comparacion <- results == val.sample[,"desertion"]
comparacion.num <- as.numeric(comparacion)
sum(comparacion.num)
eficient.factor <- sum(comparacion.num) / val.num


################
######## Student Recomendations
################
# Limit budget, cost and benefit
budget <- 10000
desertion.prediction.study.results <- compute(nn.students, Students.for.study.sample[,c("promedio.preparatoria","scholarship","prev.change","economic.level.4","economic.level.3","economic.level.2","economic.level.1","payment.Semester1","payment.Semester2","payment.Semester3","assistance1","assistance2","assistance3","exams1","exams2","exams3")])
results.study <- desertion.prediction.study.results$net.result

for(element in 1:length(results.study)){
  if( 0.5 < results.study[element,1]){
    results.study[element,1] = 1
  }
  else
  {
    results.study[element,1] = 0
  }
}

students.risk <- sum(results.study)
item <- c("1 - Dar Beca Estudiantil",
          "2 - Vales de Transporte",
          "3 - Mentoria",
          "4 - Consultoria Psicologica",
          "5 - Boleto a evento Integracion",
          "6 - Asesor Individual",
          "7 - Cursos Remediales",
          "8 - Visita a empresa",
          "9 - Examen Extemporaneo",
          "10 - Platica Motivacional",
          "11 - Viaje recreativo")
cost <- c(500L,100L,200L,400L,50L,250L,2500L,50L,100L,50L,10L)
benefit <- c(100L,50L,70L,75L,40L,80L,80L,40L,70L,30L,25L)

Desercion <- c(results.study)
IDStudent <- c(as.numeric(rownames(results.study)))
PredictedDesercion <- data.frame(
  ID_Student = IDStudent,
  Student_desercion = Desercion
)
PredictedDesercion <- c(subset(PredictedDesercion, Student_desercion==1, select = c("ID_Student")))
nrow(PredictedDesercion)

items.students <- c()
cost.students <- c()
benefit.students <- c()
ID.students <-c()
for (student in 1:(students.risk))
{
  items.students = c(items.students,item)
  cost.students = c(cost.students,cost)
  benefit.students = c(benefit.students,benefit)
  ID.students = c(ID.students,rep(PredictedDesercion$ID_Student[student:student],11))
}

PredictedDesercion$ID_Student

plan.students <- data.frame(
  items = items.students,
  cost = cost.students,
  benefit= benefit.students,
  IDs = ID.students)

student.input[103,]

Dar.Beca.Estudiantil <- 1
Vales.de.Transporte <- 2
Cursos.Remediales <- 7
Examen.Extemporaneo <- 9

recomendations.num <- length(item)

fitness.generic <- function(x){
  items.cost <- x %*% plan.students$cost
  items.beneficio <- x %*% plan.students$benefit
  if(items.cost > budget)
  {
    return(0)
  }
  else
  {
    cnt.student.fitness <- 0
    for (student in PredictedDesercion$ID_Student){
      cnt.student.fitness = cnt.student.fitness + 1
      if((1 == student.input[student, "scholarship"]) && (1 == x[(recomendations.num * (cnt.student.fitness - 1)) + Dar.Beca.Estudiantil])){
        return(0) # For the scholarship
      }
      if((1 == student.input[student, "economic.level.1"]) && 
         ((1 == x[(recomendations.num * (cnt.student.fitness - 1)) + Dar.Beca.Estudiantil]) || 
          (1 == x[(recomendations.num * (cnt.student.fitness - 1)) + Vales.de.Transporte]))){
        items.beneficio = items.beneficio - 80 # For the scholarship
        items.beneficio = items.beneficio - 40 # Vales de transporte
      }
      if((13.9 < student.input[student, "exams3"]) &&
         ((1 == x[(recomendations.num * (cnt.student.fitness - 1)) + Cursos.Remediales]) || 
          (1 == x[(recomendations.num * (cnt.student.fitness - 1)) + Examen.Extemporaneo]))){
        items.beneficio = items.beneficio - 70 # Cursos remediales
        items.beneficio = items.beneficio - 70 # Examen extemporaneo
      }
    }
    return(-items.beneficio)
  }
}

ga.tree <- rbga.bin(size=nrow(plan.students), popSize=100, mutationChance=0.01, elitism=20, iters=1000, evalFunc=fitness.generic,verbose=FALSE)

summary(ga.tree, echo = T)


set.seed(1234)
best <- ga.tree$population[ga.tree$evaluations == min(ga.tree$best),]
if(FALSE == is.null(dim(best))){
  best <- ga.tree$population[ga.tree$evaluations == min(ga.tree$best),][1,]
}

plot(ga.tree)
total.cost <- best %*% cost.students

recomendation.list <- data.frame(
  items = items.students,
  recomendation = best,
  IDs = ID.students)

student.input[986,]
recomendation.list
