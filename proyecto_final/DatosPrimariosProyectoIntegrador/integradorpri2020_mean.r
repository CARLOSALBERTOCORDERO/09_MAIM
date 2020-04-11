###########################
###########################
#Load information
###########################
###########################
getwd()
setwd("C:/Users/ccordero/Desktop/Maestria/09_MAIM/proyecto_final/DatosPrimariosProyectoIntegrador")

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

# 1 change of carrer
load("CambioCarrera.R")
#cambio.carrera

###########################
###########################
# Create input values
###########################
###########################
# Define the number of semesters to analyze
semesters.analysis.num <- 3

# Get data to create the input matrix
student.number <- length(perfil.alumnos[[1]])
elements.analysis <- "assistance.mean"
elements.analysis <- c(elements.analysis,colnames(perfil.alumnos))
elements.analysis <- c(elements.analysis,"exams.mean")
elements.analysis <- c(elements.analysis,"works.mean")
elements.analysis <- c(elements.analysis,"library.times.mean")
elements.analysis <- c(elements.analysis,"platform.times.mean")
elements.analysis <- c(elements.analysis,"books.reserved.mean")
elements.analysis <- c(elements.analysis,"scholarship")
elements.analysis <- c(elements.analysis,"payment.mean")
elements.analysis <- c(elements.analysis,"prev.change")



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
  mean.assistance <- 0
  for(course in 1:(courses.per.semester * semesters.analysis.num)){
    for(day in 1:days.per.course){
      mean.assistance <- mean.assistance + asistencias.totales[[student]][day,course]
    } 
  }
  mean.assistance <- mean.assistance / (courses.per.semester * semesters.analysis.num * days.per.course)
  student.input[student, "assistance.mean"] <- mean.assistance
}
# Fill in wiht student profile
for (student in 1:student.number){
  for(profile in colnames(perfil.alumnos)){
    student.input[student, profile] <- perfil.alumnos[student, profile]
  }
}
# Fill in exam results
mean.exam <- 0
for (student in 1:student.number){
  mean.exam <- 0
  for(course in 1:(courses.per.semester * semesters.analysis.num)){
    for(exam in 1:exam.number){
      mean.exam <- mean.exam + resultados.examenes.totales[[student]][exam,course]
    } 
  }
  mean.exam <- mean.exam / (courses.per.semester * semesters.analysis.num * exam.number)
  student.input[student, "exams.mean"] <- mean.exam
}
# Fill in works results
mean.works <- 0
for (student in 1:student.number){
  mean.works <- 0
  for(course in 1:(courses.per.semester * semesters.analysis.num)){
    for(work in 1:works.number){
      mean.works <- mean.works + resultados.trabajos.totales[[student]][work,course]
    } 
  }
  mean.works <- mean.works / (courses.per.semester * semesters.analysis.num * works.number)
  student.input[student, "works.mean"] <- mean.works
}
# Fill in times in library
times.library <- 0
for (student in 1:student.number){
  times.library <- 0
  for(course in 1:(courses.per.semester * semesters.analysis.num)){
    times.library <- times.library + ceiling(uso.biblioteca.totales[[student]][course])
  }
  times.library <- times.library / (courses.per.semester * semesters.analysis.num )
  student.input[student, "library.times.mean"] <- times.library
}
# Fill in platform use
times.platform <- 0
for (student in 1:student.number){
  times.platform <- 0
  for(course in 1:(courses.per.semester * semesters.analysis.num)){
    times.platform <- times.platform + ceiling(uso.plataforma.totales[[student]][course])
  }
  times.platform <- times.platform / (courses.per.semester * semesters.analysis.num )
  student.input[student, "platform.times.mean"] <- times.platform
}
# Fill in books Reserved
books.reserved <- 0
for (student in 1:student.number){
  books.reserved <- 0
  for(course in 1:(courses.per.semester * semesters.analysis.num)){
    books.reserved <- books.reserved + ceiling(separacion.libros.totales[[student]][course])
  }
  books.reserved <- books.reserved / (courses.per.semester * semesters.analysis.num )
  student.input[student, "books.reserved.mean"] <- books.reserved
}
# Fill in scholarship
for (student in 1:student.number){
  student.input[student, "scholarship"] <- distribucion.becas[student]
}
# Fill in paymets
mean.payment <- 0
for (student in 1:student.number){
  mean.payment <- 0
  for(semester in 1:(semesters.analysis.num)){
    for(pay.num in 1:payments){
      mean.payment <- mean.payment + asistencias.totales[[student]][pay.num,semester]
    } 
  }
  mean.payment <- mean.payment / (semesters.analysis.num * payments)
  student.input[student, "payment.mean"] <- mean.payment
}
# Fill in Change of carrer
for (student in 1:student.number){
  student.input[student, "prev.change"] <- cambio.carrera[student]
}

############
# Normalize values
###########
student.input.norm <- matrix(0, nrow=student.number, 
                        ncol=student.data,
                        dimname=list(studen.id,
                                     elements.analysis))



assistance.mean.max <- 2
gerder.offset <- 1
profile.grade.max <- 100
age.max <- max(student.input[,"edad.ingreso"], na.rm = FALSE)
social.level.max <- 4
behavior.max <- 20
exam.works.max <- 20
library.times.max <- max(student.input[,"library.times.mean"], na.rm = FALSE)
platform.times.max <- max(student.input[,"platform.times.mean"], na.rm = FALSE)
books.reserved.max <- max(student.input[,"books.reserved.mean"], na.rm = FALSE)
 payment.mean.max <- max(student.input[,"payment.mean"], na.rm = FALSE)

student.input.norm[,"assistance.mean"] <- student.input[,"assistance.mean"] / assistance.mean.max
student.input.norm[,"genero"] <- student.input[,"genero"] - gerder.offset
student.input.norm[,"admision.letras"] <- student.input[,"admision.letras"] / profile.grade.max
student.input.norm[,"admision.numeros"] <- student.input[,"admision.numeros"] / profile.grade.max
student.input.norm[,"promedio.preparatoria"] <- student.input[,"promedio.preparatoria"] / profile.grade.max
student.input.norm[,"edad.ingreso"] <- student.input[,"edad.ingreso"] / age.max
student.input.norm[,"evalucion.socioeconomica"] <- student.input[,"evalucion.socioeconomica"] / social.level.max
student.input.norm[,"nota.conducta"] <- student.input[,"nota.conducta"] / behavior.max
student.input.norm[,"exams.mean"] <- student.input[,"exams.mean"] / exam.works.max
student.input.norm[,"works.mean"] <- student.input[,"works.mean"] / exam.works.max
student.input.norm[,"library.times.mean"] <- student.input[,"library.times.mean"] / library.times.max
student.input.norm[,"platform.times.mean"] <- student.input[,"platform.times.mean"] / platform.times.max
student.input.norm[,"books.reserved.mean"] <- student.input[,"books.reserved.mean"] / books.reserved.max
student.input.norm[,"scholarship"] <- student.input[,"scholarship"]
student.input.norm[,"payment.mean"] <- student.input[,"payment.mean"] / payment.mean.max
student.input.norm[,"prev.change"] <- student.input[,"prev.change"]
