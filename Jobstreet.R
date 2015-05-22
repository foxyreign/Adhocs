# Load packages
require(ggplot2)
require(scales)
require(rpart)
require(rattle)

# Data set
df <- read.csv('Jobstreet.csv', head=T, sep=",") # Load dataset
df <- na.omit(df) # Exclude missing data

summary(df) # Summarize

# Categorize education variable
df$Education <- factor(df$Education, levels = c(1,2,3,4), 
                       labels=(c("Secondary Sch", "Prof Degree", 
                                 "Bach Degree", "Post Grad Dip")))

# Bin years of experience
df$Experience.Group <- ifelse(df$Experience < 3, "3 Years", 
                              ifelse(df$Experience < 5, "5 Years",
                                     ifelse(df$Experience < 10, "10 Years", "+10 Years")))
df$Experience.Group <- factor(df$Experience.Group, 
                              levels=c("3 Years", "5 Years", "10 Years", "+10 Years"))

# Plot boxplot
ggplot(df, aes(x=factor(0), y=Expected.Salary, fill=Experience.Group)) + 
  facet_wrap(~Position) + geom_boxplot() + xlab(NULL) + 
  scale_y_continuous(labels = comma) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")

# Plot scatter plos
ggplot(df, aes(x=Experience, y=Expected.Salary)) + 
  geom_point(aes(col=Experience.Group)) + 
  facet_wrap(~Position) + 
  stat_smooth(method="lm", fullrange = T) + 
  theme(legend.position="bottom")

# Estimate coefficients of linear regression model
summary(lm(Expected.Salary ~ Experience + Position-1, data=df))

# Diagnose LM
par(mfrow=c(1,2))
plot(lm(Expected.Salary ~ Experience + Position-1, data=df), c(1,2))
par(mfrow=c(1,1))

# CART
cart <- rpart(formula = Expected.Salary ~ Experience + Position, 
              data = df, 
              parms = list(split = "information"), # Uses information gain
              model = T) # Retains model information
print(cart); printcp(cart)

layout(matrix(c(1,2,3,4), nrow = 1, ncol = 2, byrow = TRUE), 
       widths=c(2.5,2)) 
fancyRpartPlot(cart, main=NULL, sub=NULL)
barplot(cart$variable.importance, 
        cex.names = 0.6, cex.axis = 0.5,
        sub = "Variable Importance") 
par(mfrow=c(1,1))
