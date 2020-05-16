# reconcile
# read master file
#install.packages("rstudioapi")
library(rstudioapi)
#install.packages("stringdist")
library(stringdist)
#install.packages("philentropy")
library(philentropy)

currentPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(currentPath)

master.bancos <- read.csv("bancosmaster.csv",stringsAsFactors = F)
str(master.bancos)
master.bancos

# read data to reconcile
primary.data <- read.csv("recon.csv",stringsAsFactors = F)
primary.data
save(primary.data, file="primary.data.R")
rm(primary.data)
primary.data
load("primary.data.R")


# A) Levenshtein
?stringdist
#stringdist(a, b, method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
#                            "cosine", "jaccard", "jw", "soundex"), useBytes = FALSE, weight = c(d = 1,                                                                                               i = 1, s = 1, t = 1), q = 1, p = 0, bt = 0,
#           nthread = getOption("sd_num_thread"))


levenshtein <- stringdist("HSBC","HSBC", method="lv")
adist("HSBC","HSVC")



?adist
stringdist
# B) Jaro, is the complement
jaro <- stringdist("HSBC","HSVC", method="jw", p=0)
# C) Dice
?distance
dice <- distance(x, method = "dice")
# D) Euclidean

# E) Jaccard
jaccard <- stringdist("HSBC","HSVC", method="jaccard")
# F) Longest Common Substring
lcs <- stringdist("Bancomer","Bancomes", method="lcs")
# G) Monge Elkan

# H) Needleman Wunch
# I) Qgrams Distance
qgrams <- stringdist("Bancome","Bancomer", method="qgram", q=2)

# J) Simon White

levenshtein.threshold <- 3
nchar("perro")
reconcile.1 <- function(str1, str2, threshold) {
  length.cad <- max(nchar(str1), nchar(str2))
  levenshtein <- 1 - (stringdist(str1, str2, method="lv") / length.cad)
  if (levenshtein >= threshold)
    return(T)
  else
   
   
    return(F)
}

reconcile.1("HSBC","HSVC",.75)
reconcile.1("perro","perrito",.5)
