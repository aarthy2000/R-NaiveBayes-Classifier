#!/usr/bin/env Rscript
args <- commandArgs()

# read the training set
training.set <- read.table(args[6], header = FALSE, sep = "")

# get the total number of the instances in the training set and number of instances for each class
number.total <- nrow(training.set)
number.spam <- sum(training.set[,ncol(training.set)])
number.nonspam <- number.total - number.spam

# calculate the priors
prior.spam <- number.spam / number.total
prior.nonspam <- number.nonspam / number.total

# split the data into two matrices based on the class label
splitted.data <- split(training.set, training.set$V13)
nonspam.data <- splitted.data[[1]]
spam.data <- splitted.data[[2]]

# get the feature matrices out of the data set (i.e. remove the last column which represents class label)
nonspam.features <- nonspam.data[,1:12]
spam.features <- spam.data[,1:12]

# generate vectors of conditional probabilities (e.g. probs.one.spam stands for prob of one given spam)
# apply dealing with zero method at the same time (only if divisor is zero)
probs.one.spam <- if (number.spam) colSums(spam.features) / number.spam else (colSums(spam.features) + 2) / (number.spam + 1)
probs.zero.spam <- 1 - probs.one.spam
probs.one.nonspam <- if (number.nonspam) colSums(nonspam.features) / number.nonspam else (colSums(nonspam.features) + 2) / (number.spam + 1)
probs.zero.nonspam <- 1 - probs.one.nonspam

print("Probabilities of features being one given spam:")
probs.one.spam
cat("\n")
print("Probabilities of features being zero given spam:")
probs.zero.spam
cat("\n")
print("Probabilities of features being one given nonspam:")
probs.one.nonspam
cat("\n")
print("Probabilities of features being zero given nonspam:")
probs.zero.nonspam
cat("\n")

## start testing

# read the test set
test.set <- read.table(args[7], header = FALSE, sep = "")

# function to calculate scores of spam
prob.given.spam <- function(instance) {
  prob <- prior.spam
  i <- 1
  for (feature in instance) {
    prob <- prob * if (feature) probs.one.spam[i] else probs.zero.spam[i]
    i <- i + 1
  }
  return (prob)
}

# function to calculate scores of nonspam
prob.given.nonspam <- function(instance) {
  prob <- prior.nonspam
  i <- 1
  for (feature in instance) {
    prob <- prob * if (feature) probs.one.nonspam[i] else probs.zero.nonspam[i]
    i <- i + 1
  }
  return (prob)
}

# for each instance in test.set, get the score of the instance being spam
scores.spam <- apply(test.set, 1, prob.given.spam)
# for each instance in test.set, get the score of the instance being nonspam
scores.nonspam <- apply(test.set, 1, prob.given.nonspam)

print("Scores of the each test instance being spam:")
scores.spam
cat("\n")
print("Scores of the each test instance being nonspam:")
scores.nonspam
cat("\n")

# for each instance, if score of spam > score of nonspam, it is spam
results <- (scores.spam - scores.nonspam > 0) * 1

print("Final results(1 represents spam, 0 represents nonspam):")
results
