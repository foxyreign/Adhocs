require(ggplot2)
require(gridExtra)

set.seed(100)
age <- c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
salary <- c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
gender <- as.factor(c(rep("Female", 1500), rep("Male", 1500)))
dataframe <- data.frame(age, salary, gender)

dots <- ggplot(dataframe, aes(age, salary)) + 
  geom_point(aes(color=gender)) + 
  theme(legend.position=c(1,1),legend.justification=c(1,1)) 

top <- ggplot(dataframe, aes(age, fill=gender)) + 
  geom_density(alpha=.5) + 
  theme(legend.position = "none")

topbox <- ggplot(dataframe, aes(x=gender, y=age, fill=gender)) + 
  geom_boxplot(alpha=.5) + coord_flip() +
  theme(legend.position = "none")

right <- ggplot(dataframe, aes(salary, fill=gender)) + 
  geom_density(alpha=.5) + 
  coord_flip() + 
  theme(legend.position = "none") 

rightbox <- ggplot(dataframe, aes(x=gender, y=salary, fill=gender)) + 
  geom_boxplot(alpha=.5) + 
  theme(legend.position = "none")

empty <- ggplot() + geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

grid.arrange(topbox, empty, empty, top, empty, empty, dots, right, rightbox, 
             sub = "in Z-score", widths=c(6,1.5,2), heights=c(1.5,1,3))
