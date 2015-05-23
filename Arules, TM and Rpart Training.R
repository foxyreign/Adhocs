# Load dataset
train <- read.csv("popcorn-train.csv", head=T, sep=",")
rownames(train) <- train$id
train <- train[,2:3]
train$sentiment <- factor(train$sentiment, levels = c(1,0), labels = c("Pos", "Neg"))

# Prepare tm vector
require(tm, quietly = T)
keyword <- as.data.frame(train$review)
keyword <- Corpus(DataframeSource(keyword)) # Creates corpus
keyword <- tm_map(keyword, removeWords, stopwords(kind = "english")) # Removes stop words
keyword <- tm_map(keyword, stemDocument, language = "porter") # Applies porter stemming
keyword <- tm_map(keyword, stripWhitespace) # Removes whitespace
keyword <- tm_map(keyword, content_transformer(tolower)) # Converts to lowercase
keyword <- tm_map(keyword, removePunctuation) # Removes punctuation marks
keyword <- tm_map(keyword, removeNumbers) # Removes numbers


# Raw Term Frequency
keyword.term <- DocumentTermMatrix(keyword)
keyword.term <- removeSparseTerms(keyword.term, sparse = 0.9)
keyword.term # Checks for sparsity

keyword.matrix <- as.data.frame.matrix(keyword.term)
keyword.matrix <- cbind(keyword.matrix, sentiment = train$sentiment)
rownames(keyword.matrix) <- row.names(train)
head(keyword.matrix[150:170,118:127])


#### Association Rules ####
require(arules, quietly = T)

# Negative
popcorn.neg <- subset(keyword.matrix, sentiment == "Neg")
popcorn.neg <- popcorn.neg[, !(colnames(popcorn.neg) %in% "sentiment")]

set.seed(14344)
popcorn.neg <- popcorn.neg[sample(1:nrow(popcorn.neg), 5000, replace=FALSE),]
popcorn.neg <- as.matrix(popcorn.neg)

popcorn.neg <- as(object = popcorn.neg, Class = "transactions")
popcorn.neg

neg.arules <- apriori(popcorn.neg, parameter=list(support=0.1, conf=0.1))
neg.arules <- sort(neg.arules, decreasing=T, by="lift")

# Negative Prune
neg.prune <- is.subset(neg.arules, neg.arules)
neg.prune[lower.tri(neg.prune, diag=T)] <- NA
neg.redundant <- colSums(neg.prune, na.rm=T) >= 1
neg.prune <- neg.arules[!neg.redundant]

neg.arules.df = data.frame(
  lhs = labels(lhs(neg.prune))$elements,
  rhs = labels(rhs(neg.prune))$elements, 
  neg.prune@quality)
head(neg.arules.df, 20)

require(arulesViz, quietly=T)
par(mfrow=c(1,2)) # Displays two plots in one graphics
plot(head(neg.prune,40), method="graph",
     control=list(type="items", precision=1, cex=.6, main="Items")) # Plots items rules
plot(head(neg.prune,40), method="graph", 
     control=list(type="itemsets", precision=1, cex=.6, main="Itemsets")) # Plots itemsets
par(mfrow=c(1,1)) # Resets graphic device

saveAsGraph(head(sort(neg.prune, by="lift"),200), file="neg.prune.graphml")

# Decision Tree Learning
findFreqTerms(keyword.term, lowfreq = 5000) # Which words are most frequent?

# Convert to bin matrix
keyword.binary <- weightBin(keyword.term) # Applies binary transformation
keyword.binary <- as.data.frame.matrix(keyword.binary) # Converts to data frame
keyword.binary <- cbind(keyword.binary, sentiment = train$sentiment) # Combines response
keyword.binary <- as.data.frame(sapply(keyword.binary, 
                                       FUN = as.factor)) # Converts columns to categorical
rownames(keyword.binary) <- row.names(train) # Applies unique ID


# Divide train and testing set
set.seed(14344) # For reproducibility 
split <- sample(seq_len(nrow(keyword.binary)), # Counts records from 1 to n
                size = 0.8 * nrow(keyword.binary), # Computes for 80% for training set
                replace = FALSE)

popcorn.train <- keyword.binary[split, ] # Creates training set
popcorn.test <- keyword.binary[-split, ] # Created testing set

# Build model
popcorn.tree <- rpart(formula = sentiment ~ ., # tests all predictors against response
                      data = popcorn.train, 
                      method = "class", # Tells model it is a classification tree
                      parms = list(split = "information"), # Uses information gain
                      model = T) # Retains model information


# Visualize tree
# Prints nodes and leaves
print(popcorn.tree) 

# Displays two plots in one graphics
# Prints nodes and leaves
print(popcorn.tree)

layout(matrix(c(1,2,3,4), nrow = 1, ncol = 2, byrow = TRUE), 
       widths=c(2.5,2)) 

# Plots the model
fancyRpartPlot(model = popcorn.tree, 
               main = NULL, 
               sub = NULL) 

# Plots variable importance
barplot(popcorn.tree$variable.importance, 
        cex.names = 0.5, 
        sub = "Variable Importance") 
popcorn.tree$variable.importance

par(mfrow=c(1,1)) # Resets graphic device layout

# Complexity parameter
printcp(popcorn.tree) # Prints CP
# root node error * last xerror = misclassification


# Evaluate model
popcorn.prediction <- predict(object = popcorn.tree, # Tests model
                              newdata = popcorn.test, # with Testing set
                              type = "class") # Tells it is a classification prediction

# Build binary classification confusion matrix
popcorn.confusion <- table(Actual = popcorn.test$sentiment, 
                           Predicted = popcorn.prediction)

tp <- popcorn.confusion[1,1] # True Positive
tn <- popcorn.confusion[2,2] # True Negative
fp <- popcorn.confusion[2,1] # False Positive
fn <- popcorn.confusion[1,2] # False Negative
n <- sum(popcorn.confusion) # Total records of testing set

popcorn.accuracy <- (tp + tn) / n # Accuracy rate = 66.78%
popcorn.error <- (fp + fn) / n # Error rate = 33.22%
popcorn.precision <- tp / (tp + fp) # Precision/Sensitivity = 69.50%
popcorn.recall <- tp / (tp + fn) # Recall/Specificity = 60.21%
popcorn.f1 <- 2 * popcorn.precision * popcorn.recall / (popcorn.precision + popcorn.recall)
popcorn.oddsratio <- (tp * tn) / (fp * fn) # Odds ratio = 4 folds

library(knitcitations)
c(citation(), 
  citation("knitr"),
  citation("arules"),
  citation("arulesViz"),
  citation("rpart"), 
  citation("rattle"), 
  citation("tm"))