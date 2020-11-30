# Import library
library(dplyr)
library(adabag)
library(caret)

# Import data
data <- read.csv("echocardiogram.csv")
head(data)


data <- data[,-c(8,10:12)]

# Ubah tipe data
for(i in colnames(data)){
  data[,i] <- as.numeric(data[,i])
}
str(data)
summary(data)

# Melihat jumlah missing value
sapply(data, function(x) sum(is.na(x)))


# Menangani nilai yang tidak valid
data <- na_if(data,"?")


# Visualisasi
library(visdat)
vis_miss(data)


# Imputasi data menggunakan MICE
library(mice)
mice_imputes <- mice(data, m=5, maxit = 40)

data <- complete(mice_imputes, 5)


# Melihat lagi jumlah missing value
sapply(data, function(x) sum(is.na(x)))




# Melihat korelasi antar variabel
library(corrplot)
# corrplot(data)
cor(data)
pairs.panels(data)


# Membuat target menjadi factor
data$still.alive <- as.factor(data$still.alive)
str(data)

# Split data
set.seed(1234)
indeks <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[indeks == 1,]
test <- data[indeks == 2,]


# Jumlah dalam masing-masing split
print(paste("Jumlah data training: ", nrow(train)))
print(paste("Jumlah data testing: ", nrow(test)))

# Membuat model
model <- boosting(still.alive~., data=train, 
                mfinal=10, control=rpart.control(maxdepth=1),
                coeflearn='Breiman')
model

# Membuat prediksi pada data testing berdasarkan model
prediksi <- predict(model, test)

#Menampilkan confusion matrix
confusionMatrix(prediksi$confusion)

prediksiSemua <- predict(model, data)
confusionMatrix(prediksiSemua$confusion)



# K-fold cross-validation
set.seed(1234)
control <- trainControl(method = "cv", number = 4)
model2 <- train(still.alive~., data=data, method = "adaboost", trControl = control)
model2
