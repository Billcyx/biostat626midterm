train_data = read.csv('/Users/yuxichen/biostat626/midterm/training_data.txt', sep="\t", header=FALSE)

#install.packages("stringr")

#####formating the title of the dataframe
library('stringr')
title = train_data$V1[1]
class(title)
title = strsplit(title, split = " ")
head = title[[1]]
head = head[nchar(head)!=0]
head[1] = 'subject activity'
head = head[-2]
#first column is removed (data cleaning)
train_data$V1 = NULL
#first row is also removed(data cleaning)
train_data = train_data[-1,]

#assign head as the column name for train_data frame
colnames(train_data) = head


# split train and validation sample
library('caret')

smp_size <- floor(0.75 * nrow(train_data))

#check for null values(no null value is found)
colSums(is.na(train_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(train_data)), size = smp_size)
train <- train_data[train_ind, ]
valid <- train_data[-train_ind, ]

#binarize the label or train and validation set:
train_binary = transform(train,label=ifelse((0 < train$`subject activity` & train$`subject activity`  < 4), 1, 0))   
train_binary$subject.activity= NULL

valid_binary<-transform(valid,label=ifelse((0 < valid$`subject activity` & valid$`subject activity` < 4), 1, 0))   
valid_binary$subject.activity= NULL

train_x = train_binary[,1:561]
train_x = scale(train_x)[,]
train_y = train_binary[,562]

valid_x = valid_binary[,1:561]
valid_x = scale(valid[,-14])[,]
valid_y = valid_binary[,562]


##use lightgbm for feature importance selection
install.packages('lightgbm')

# load the train and valid data into the LightGBM dataset object. 
library('lightgbm')
dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dvalid <- lgb.Dataset.create.valid(dtrain, data = as.matrix(valid_x), label = valid_y)

# define parameters
params <- list(objective = "multiclass",
               metric = "multi_error",
               seed = 42,
               num_class = 2)

# train model 
lgb_model <- lgb.train(params, dtrain, 200, list(test= dvalid), early_stopping_round=10)

#predict on test data
test_data = read.csv('/Users/yuxichen/biostat626/midterm/test_data.txt', sep="\t", header=FALSE)

#####formating the title of the dataframe for test_data
library('stringr')
title = test_data$V1[1]
class(title)
title = strsplit(title, split = " ")
head = title[[1]]
head = head[nchar(head)!=0]
head = head[-1]
#first column is removed (data cleaning)
test_data$V1 = NULL
#first row is also removed(data cleaning)
test_data = test_data[-1,]

#assign head as the column name for train_data frame
colnames(test_data) = head

test_x = test_data
test_x = scale(test_x)[,]

##let us predidct for test data
# We can predict on test data, identical
my_preds <- predict(lgb_model, test_x)

#convert probablity to category
pred = c()
for (i in 1: length(my_preds))
  if (i %% 2 == 1)
    pred = c(pred, my_preds[i])

categorical_prediction = c()
for (i in 1: length(pred))
  if (pred[i] > 0.5) {
    categorical_prediction = c(categorical_prediction, 0)
  } else 
    categorical_prediction = c(categorical_prediction, 1)

print(categorical_prediction)

##################################################################################################################test for multiclass
#label the class
train_multiclass = data.frame(train) 
train_multiclass$label = train_multiclass$subject.activity
train_multiclass$label[train_multiclass$subject.activity > 6] = 7
train_multiclass$subject.activity= NULL

valid_multiclass = data.frame(valid) 
valid_multiclass$label = valid_multiclass$subject.activity
valid_multiclass$label[valid_multiclass$subject.activity > 6] = 7
valid_multiclass$subject.activity= NULL

train_x = train_multiclass[,1:561]
train_x = scale(train_x)[,]
train_y = train_multiclass[,562] - 1

valid_x = valid_multiclass[,1:561]
valid_x = scale(valid[,-14])[,]
valid_y = valid_multiclass[,562] - 1

# load the train and valid data into the LightGBM dataset object. 
#library('lightgbm')
dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dvalid <- lgb.Dataset.create.valid(dtrain, data = as.matrix(valid_x), label = valid_y)

# define parameters
params <- list(objective = "multiclass",
               metric = "multi_error",
               seed = 42,
               num_class = 7)

# train model 
lgb_model <- lgb.train(params, dtrain, 200, list(test= dvalid), early_stopping_round=10)

##let us predidct for test data
# We can predict on test data, identical
my_preds2 <- predict(lgb_model, test_x)

#convert probablity to category
categorical_prediction2 = c()
list = c()

for (i in 1: length(my_preds2))
  if (i %% 7 == 0){
    list = c(list, my_preds2[i])
    max = which.max(list)
    categorical_prediction2 = c(categorical_prediction2, max)
    list = c()
  } else
    list = c(list, my_preds2[i])
print(categorical_prediction2)

class(categorical_prediction)
print(categorical_prediction)


###write result to file
library(data.table) # install if not installed already
fwrite(list(categorical_prediction), file = "binary_yuxi520.txt")

library(data.table) # install if not installed already
fwrite(list(categorical_prediction2), file = "multiclass_yuxi520.txt")

