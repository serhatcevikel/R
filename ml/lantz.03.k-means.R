# implement k-means on wbcd data
# lantz, ch 3


# read data
wbcd <- read.csv(url("https://raw.githubusercontent.com/dataspelunking/MLwR/master/Machine%20Learning%20with%20R%20(2nd%20Ed.)/Chapter%2003/wisc_bc_data.csv"), stringsAsFactors = F)

# first rows
head(wbcd)

# preview structure
str(wbcd)

# delete first id columns
wbcd.1 <- wbcd[-1]

# get the dimensions
dim(wbcd.1)

# get summary of diagnosis results
table(wbcd.1$diagnosis)

# recode diagnosis columns into factors
wbcd.1$diagnosis <- factor(wbcd.1$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

# summarize proportions of diagnosis variable
prop.table(table(wbcd.1$diagnosis))

# selected column labels
selected <- c("radius_mean", "area_mean", "smoothness_mean")

# summary statistics of selected data
summary(wbcd.1[selected])

# function for naive normalization with range
normalize <- function(x)
{
    (x - min(x)) / (max(x) - min(x))
}

# apply normalize function to all columns but the first one
wbcd_n <- as.data.frame(lapply(wbcd.1[-1], normalize))

# get the summary stats of selected columns
summary(wbcd_n[selected])

# define cut point for train data
cutp <- 469

# split data into train and test
wbcd_train <- wbcd_n[1:cutp,]
wbcd_test <- wbcd_n[-(1:cutp),]

# get split data labels
wbcd_train_labels <- wbcd.1[1:cutp,1]
wbcd_test_labels <- wbcd.1[-(1:cutp),1]

# load class library for classification and gmodels for cross tabulation
library(class)
library(gmodels)

# run the model
wbcd_test_pred <- class::knn(
                        train = wbcd_train,
                        test = wbcd_test,
                        cl = wbcd_train_labels,
                        k = 21
)

# cross plot prediction and real values
gmodels::CrossTable(   x = wbcd_test_labels,
                       y = wbcd_test_pred,
                       prop.chisq = F
                   )


