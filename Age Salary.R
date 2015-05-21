set.seed(100)
age <- rbinom(n = 100, size = 100, prob = .4)
hist(age)

set.seed(100)
salary <- rnorm(n = 100, mean = 100000, sd = 6000)
hist(salary)

dataframe <- data.frame(age, salary)

require(ggplot2)
age.hist <- ggplot(dataframe, aes(x=age)) + geom_histogram() + theme_minimal()
salary.hist <- ggplot(dataframe, aes(x=salary)) + geom_histogram() + theme_minimal()

set.seed(100)
age <- c(rbinom(1500, size = 100, prob = 0.8), rbinom(1500, size = 100, prob = 0.4))
salary <- c(rnorm(15000, mean = 1), rnorm(15000, mean = 1.5))
gender <- as.factor(c(rep(1, 1500), rep(2, 1500)))
dataframe <- data.frame(age, salary, gender)

p2 <-ggplot(dataframe, aes(x=xvar)) + 
  geom_histogram(aes(y = ..density..), color="black", fill=NA) +
  geom_density(color="blue")