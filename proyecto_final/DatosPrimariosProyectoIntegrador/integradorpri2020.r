###########################
###########################
#Load information
###########################
###########################
#install.packages("rstudioapi")
#library(rstudioapi)

getwd()
currentPath <- dirname(rstudioapi::getSourceEditorContext()$path)
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

# 1 change of carrer
load("CambioCarrera.R")
#cambio.carrera

###########################
###########################
# Create input values
###########################
###########################
# Define the number of semesters to analyze
semesters.analysis.num <- 4
student.number <- length(perfil.alumnos[[1]])

# Get data to create the input matrix
# one-hot representation for economic level and payment
economic.levels <- c("economic.level.4","economic.level.3","economic.level.2","economic.level.1")
payment.status.number <- 2
elements.analysis <- c()
elements.analysis <- c(elements.analysis,colnames(perfil.alumnos))
elements.analysis <- c(elements.analysis,"exams.mean")
elements.analysis <- c(elements.analysis,"works.mean")
elements.analysis <- c(elements.analysis,"library.times.mean")
elements.analysis <- c(elements.analysis,"platform.times.mean")
elements.analysis <- c(elements.analysis,"books.reserved.mean")
elements.analysis <- c(elements.analysis,"scholarship")
elements.analysis <- c(elements.analysis,"payment.mean")
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
# Fill in wiht student profile
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
# Fill in Change of carrer
for (student in 1:student.number){
  student.input[student, "prev.change"] <- cambio.carrera[student]
}

############
# Normalize values
###########
student.input.norm <- student.input


assistance.mean.max <- 2
gerder.offset <- 1
profile.grade.max <- 100
age.max <- max(student.input[,"edad.ingreso"], na.rm = FALSE)
behavior.max <- 20
exam.works.max <- 20
library.times.max <- max(student.input[,"library.times.mean"], na.rm = FALSE)
platform.times.max <- max(student.input[,"platform.times.mean"], na.rm = FALSE)
books.reserved.max <- max(student.input[,"books.reserved.mean"], na.rm = FALSE)
payment.mean.max <- 2

student.input.norm[,"genero"] <- student.input[,"genero"] - gerder.offset
student.input.norm[,"admision.letras"] <- student.input[,"admision.letras"] / profile.grade.max
student.input.norm[,"admision.numeros"] <- student.input[,"admision.numeros"] / profile.grade.max
student.input.norm[,"promedio.preparatoria"] <- student.input[,"promedio.preparatoria"] / profile.grade.max
student.input.norm[,"edad.ingreso"] <- student.input[,"edad.ingreso"] / age.max
student.input.norm[,"nota.conducta"] <- student.input[,"nota.conducta"] / behavior.max
student.input.norm[,"exams.mean"] <- student.input[,"exams.mean"] / exam.works.max
student.input.norm[,"works.mean"] <- student.input[,"works.mean"] / exam.works.max
student.input.norm[,"library.times.mean"] <- student.input[,"library.times.mean"] / library.times.max
student.input.norm[,"platform.times.mean"] <- student.input[,"platform.times.mean"] / platform.times.max
student.input.norm[,"books.reserved.mean"] <- student.input[,"books.reserved.mean"] / books.reserved.max
student.input.norm[,"scholarship"] <- student.input[,"scholarship"]
# student.input.norm[,"payment.mean"] <- student.input[,"payment.mean"] / payment.mean.max
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("payment.Semester", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / payment.mean.max
}
for(semester in 1:semesters.analysis.num){
  temp.string <- paste("assistance", toString(semester), sep="")
  student.input.norm[,temp.string] <- student.input[,temp.string] / assistance.mean.max
}

student.input.norm[,"prev.change"] <- student.input[,"prev.change"]



