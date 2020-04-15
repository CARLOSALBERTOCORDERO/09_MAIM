#library(nerualnet)
#library(neuralnet)

?runif
#runif(1000, min=0, max=100)

set.seed(1234)
training.ser <- as.data.frame(runif(50, min=0, max=100))
training.output <- sqrt(training.ser)
training <- cbind(training.ser, training.output)
head(training, 3)
colnames(training) <- c("input", "output")

formula1 <- output~input
#formula1 <- output~input+input2+input3
class(formula1)
?neuralnet
# threshold is the desired max error
# stepmax is the maximum number of ephocs
# hidden can be an array c(4,8) if we want more layers
neural.net.1 <- neuralnet(formula=formula1,
                          data = training,
                          hidden=10, threshold=0.01,
                          stepmax=1e+06,
                          lifesign = "full")

class(neural.net.1)
plot(neural.net.1)

test.data <- as.data.frame((1:10)*(1:10))
nn.result <- compute(neural.net.1, test.data)
attributes(nn.result)
nn.result$net.result

compare.output <- cbind(test.data, sqrt(test.data), nn.result$net.result)

colnames(compare.output) <- c("input", "expected", "nn.output")
head(compare.output)

compare.output$test <- 1:10
str(compare.output)
compare.output$delta <- compare.output$expected - compare.output$nn.output

getwd()
setwd("C:/Users/ccordero/Desktop/09_MAIM/06_Redes_Neuronales")
credit.data <- read.csv("creditset.csv")
credit.data
class(credit.data)
summary(credit.data)
str(credit.data)
head(credit.data)
rownames(credit.data) <- credit.data$clientid
cor(credit.data[,-1])
# first we check the correlation of the inputs because maybe some of them
# are not going to be giving more information

train.set <- credit.data[1:1600,]
test.set <- credit.data[1601:2000,]

# Because teh correlation it doesnt have sense to use LTI with load or income
formula.loan.1 <- default10yr~income+age+loan
formula.loan.2 <- default10yr~LTI+age
formula.loan.3 <- default10yr~age+loan

# If we want to use formula.loan.1 or formula.loan.3 we need to normalize
set.seed(1234)
nn.loan1 <- neuralnet(formula=formula.loan.1,
                     data = train.set,
                     hidden=10, threshold=0.01,
                     linear.output = T,
                     stepmax=1e+06,
                     lifesign = "full")

set.seed(1234)
nn.loan2 <- neuralnet(formula=formula.loan.2,
                      data = train.set,
                      hidden=10, threshold=0.01,
                      linear.output = F,
                      stepmax=1e+06,
                      lifesign = "full")

set.seed(1234)
nn.loan3 <- neuralnet(formula=formula.loan.3,
                      data = train.set,
                      hidden=10, threshold=0.01,
                      linear.output = F,
                      stepmax=500000,
                      lifesign = "full")

plot(nn.loan2)


credit.results <- 
  compute(nn.loan2, test.set[,c("loan","age")])

credit.results <- 
  compute(nn.loan2, test.set[,c("LTI","age")])


# compare
results <-
  data.frame(actual=test.set$default10yr,
             prediction=round(credit.results$net.result))

head(results,100)
nrow(test.set)

(results$actual == results$prediction) == TRUE
comparacion <- results$actual == results$prediction
comparacion.num <- as.numeric(comparacion)
print("The number of correct answers is(over 400):")
sum(comparacion.num)
sum(comparacion.num) == nrow(test.set)


