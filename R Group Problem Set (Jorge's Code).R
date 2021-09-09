rm(list = ls())
set.seed(24)



expected_tests <- function(pop=10000, groupsize=4, positve_rate=0.09){

# For a given population, what pool size will result in the least amount of tests required to test each person



# How many groups can we compose from our population size?
numgroups = pop/groupsize


# This is the probability that a group will have at least one positive test, requiring a re-test
groupf<-(1-dbinom(0,groupsize, positve_rate))

# Based off of this failure rate, how many groups will need to retest?  
failedgroups<- numgroups*groupf

#failedgroups= as.integer(failedgroups)

#For each group that requires a retest, the number of tests required to retest each person in that group is directly tied to the group size (since we will test each member)
newtests<- groupsize*failedgroups

#numgroups<-as.integer(numgroups)

# The total number of tests required to evaluate a given number of people
if(groupsize>1){
totaltests<-newtests+numgroups
}
else{
  totaltests=pop
}

return(totaltests)
#answer<-cat(totaltests, groupsize)
#print (answer)
}


# Depending on the accuracy you'd like to achieve set the population size, greater population equals greater accuracy
pop <- 10000

# Initializing the matrix where we will collect the expected # of tests required to test everyone in the given population size, depending on two variables: the pool size (rows), and the positive test rate of the population (columns)
data <- matrix(nrow=25, ncol=100)

# This loop populates the above matrix with the expected value for number of tests
for(positive_rate in 1:100){
  for(group_size in 1:25){
    data[group_size, positive_rate] = expected_tests(pop,group_size,positive_rate/100)
  }
}

# To abstract the results a bit further, I'd like to see, on average, how many people we can evaluate per test. This way we are achieving the maximum results with each test
tests_per_capita <- pop/data


# I also want to see how different positive test rates in the population affect our optimal pool size
optimal <- 1:100

# For each positive test rate of the population (1%, 2%, 3%, ...) this will tell us what the optimal pool size is for that context
for(positive_rate in 1:100){
  optimal[positive_rate] <- which(tests_per_capita[,positive_rate]==max(tests_per_capita[,positive_rate]))
}

# Since we're still getting used to R, this will allow us to see/confirm the results in Excel
write.csv(tests_per_capita, file="data.csv")











