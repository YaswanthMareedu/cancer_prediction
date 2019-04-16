# import the dataset
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

#structure of the dataset
str(wbcd)

# drop the id feature
wbcd<-wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))


# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# normalize the wbcd data
wbcd_r<-as.data.frame(lapply(dataset[2:31], normalize))

# confirm that normalization worked
summary(wbcd_r$area_mean)

# create training and test data
wbcd_train <- wbcd_r[1:469, ]
wbcd_test <- wbcd_r[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# Creating the model..

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

#Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)