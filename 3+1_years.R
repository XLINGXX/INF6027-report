#download and load
library(jsonlite)

file_path <- "D:/Sheffield/master/introduction(R)/report/archive/News_Category_Dataset_v3.json"
a <- file(file_path, "r", encoding = "UTF-8")
lines <- readLines(a)
close(a)
data_list <- lapply(lines, fromJSON)
data <- do.call(rbind, data_list)

View(data)
class(data)

#convert
library(tidyverse)
library(dplyr)
library(tibble)
data_tibble <- as_tibble(data)

colnames(data_tibble) <- c("link", "headline", "category", "short_description", "authors", "date")
data_tibble <- data_tibble %>%
  mutate( link = unlist(link),
          headline = unlist(headline), 
          category = unlist(category),
          short_description = unlist(short_description),
          authors = unlist(authors),
          date = unlist(date)   )    
data_tibble$date <- as.Date(data_tibble$date)  

summary(data_tibble)
glimpse(data_tibble)

data_tibble <- data_tibble %>% filter(date >= as.Date("2018-09-23") & date <= as.Date("2022-09-23"))
data_tibble_1 <- data_tibble %>% select(headline, category,date)

summary(data_tibble_1)
View(data_tibble)


#preprocessing
sum(is.na(data_tibble_1$headline))     
sum(data_tibble_1$headline == "")  
sum(is.na(data_tibble_1$category))
sum(data_tibble_1$category == "")


data_tibble_1 <- data_tibble_1 %>% filter(headline != "" & category != "")    
sum(is.na(data_tibble_1$headline))
sum(is.na(data_tibble_1$category))    
sum(is.na(data_tibble_1$date))


#model
library(tm)

train_data <- data_tibble_1 %>% filter(date >= as.Date("2018-09-23") & date <= as.Date("2021-09-23"))
summary(train_data$headline)

corpus_train <- Corpus(VectorSource(train_data$headline))
train_cleaned <- tm_map( corpus_train, tolower )
train_cleaned <- tm_map( train_cleaned, removePunctuation )
train_cleaned <- tm_map( train_cleaned, removeWords, stopwords('en') )
train_cleaned <- tm_map( train_cleaned, stemDocument )

weight_train_cleaned <- DocumentTermMatrix(train_cleaned, control=list(weighting=weightBin) )

removeSparseTerms(weight_train_cleaned, 0.98) 

observed_vocabulary <- unlist(weight_train_cleaned$dimnames)
train_DTM <- DocumentTermMatrix( train_cleaned, control=list( dictionary=observed_vocabulary))



test_data <- data_tibble_1 %>% filter(date >= as.Date("2021-09-24") & date <= as.Date("2022-09-23"))
summary(test_data$headline)
corpus_test <- Corpus(VectorSource(test_data$headline))

test_cleaned <- tm_map( corpus_test, tolower )
test_cleaned <- tm_map( test_cleaned, removePunctuation )
test_cleaned <- tm_map( test_cleaned, removeWords, stopwords('en') )
test_cleaned <- tm_map( test_cleaned, stemDocument )

test_DTM <- DocumentTermMatrix( test_cleaned, control = list( dictionary = observed_vocabulary))

train_category <- train_data$category
test_category <- test_data$category



library(Matrix)
train_matrix_headline <- as(train_DTM, "sparseMatrix")
test_matrix_headline <- as(test_DTM, "sparseMatrix")


  # pick category<2
category_counts <- table(train_category)
low_freq_categories <- category_counts[category_counts < 2]
print(low_freq_categories)
  # combine
valid_categories <- names(category_counts[category_counts >= 2])
train_category <- ifelse(train_category %in% valid_categories, train_category, "Other")


library(glmnet)
model <- glmnet(train_matrix_headline, as.factor(train_category), family = "multinomial")
print(model)


probabilities <- predict(model, newx = test_matrix_headline, s = 0.01, type = "class")
head(probabilities)


accuracy <- mean(probabilities == test_category)
print(paste("Accuracy=", accuracy))





##model_2    increase train data

train_data_2 <- data_tibble %>% filter(date >= as.Date("2018-09-23") & date <= as.Date("2021-12-31"))
summary(train_data_2$headline)

corpus_train_2 <- Corpus(VectorSource(train_data_2$headline))
train_cleaned_2 <- tm_map( corpus_train_2, tolower )
train_cleaned_2 <- tm_map( train_cleaned_2, removePunctuation )
train_cleaned_2 <- tm_map( train_cleaned_2, removeWords, stopwords('en') )
train_cleaned_2 <- tm_map( train_cleaned_2, stemDocument )

weight_train_cleaned_2 <- DocumentTermMatrix(train_cleaned_2, control=list(weighting=weightBin) )

removeSparseTerms(weight_train_cleaned_2, 0.98) 

observed_vocabulary <- unlist(weight_train_cleaned_2$dimnames)
train_DTM_2 <- DocumentTermMatrix( train_cleaned_2, control=list( dictionary=observed_vocabulary))


test_data_2 <- data_tibble %>% filter(date >= as.Date("2021-09-24") & date <= as.Date("2022-09-23"))
summary(test_data_2$headline)
corpus_test_2 <- Corpus(VectorSource(test_data_2$headline))

test_cleaned_2 <- tm_map( corpus_test_2, tolower )
test_cleaned_2 <- tm_map( test_cleaned_2, removePunctuation )
test_cleaned_2 <- tm_map( test_cleaned_2, removeWords, stopwords('en') )
test_cleaned_2 <- tm_map( test_cleaned_2, stemDocument )

test_DTM_2 <- DocumentTermMatrix( test_cleaned_2, control = list( dictionary = observed_vocabulary))

train_category_2 <- train_data_2$category
test_category_2 <- test_data_2$category


library(Matrix)
train_matrix_headline_2 <- as(train_DTM_2, "sparseMatrix")
test_matrix_headline_2 <- as(test_DTM_2, "sparseMatrix")


category_counts_2 <- table(train_category_2)
low_freq_categories_2 <- category_counts_2[category_counts_2 < 2]
print(low_freq_categories_2)
valid_categories_2 <- names(category_counts_2[category_counts_2 >= 2])
train_category_2 <- ifelse(train_category_2 %in% valid_categories_2, train_category_2, "Other")


library(glmnet)
model_2 <- glmnet(train_matrix_headline_2, as.factor(train_category_2), family = "multinomial")
print(model_2)


probabilities_2 <- predict(model_2, newx = test_matrix_headline_2, s = 0.01, type = "class")
head(probabilities_2)

accuracy_2 <- mean(probabilities_2 == test_category_2)
print(paste("Accuracy=", accuracy_2))







##model_3   short description

data_tibble_3 <- data_tibble %>% select(short_description, category,date)
summary(data_tibble_3)

sum(is.na(data_tibble_3$short_description))  
sum(data_tibble_3$short_description == "")  
sum(is.na(data_tibble_3$category))
sum(data_tibble_3$category == "")
data_tibble_3 <- data_tibble_3 %>% filter(short_description != "" & category != "")

train_data_3 <- data_tibble_3 %>% filter(date >= as.Date("2018-09-23") & date <= as.Date("2021-12-31"))
summary(train_data_3$short_description)

corpus_train_3 <- Corpus(VectorSource(train_data_3$short_description))
train_cleaned_3 <- tm_map( corpus_train_3, tolower )
train_cleaned_3 <- tm_map( train_cleaned_3, removePunctuation )
train_cleaned_3 <- tm_map( train_cleaned_3, removeWords, stopwords('en') )
train_cleaned_3 <- tm_map( train_cleaned_3, stemDocument )

weight_train_cleaned_3 <- DocumentTermMatrix(train_cleaned_3, control=list(weighting=weightBin) )

removeSparseTerms(weight_train_cleaned_3, 0.98) 

observed_vocabulary_3 <- unlist(weight_train_cleaned_3$dimnames)
train_DTM_3 <- DocumentTermMatrix( train_cleaned_3, control=list( dictionary=observed_vocabulary_3))


test_data_3 <- data_tibble_3 %>% filter(date >= as.Date("2021-09-24") & date <= as.Date("2022-09-23"))
summary(test_data_3$short_description)
corpus_test_3 <- Corpus(VectorSource(test_data_3$short_description))

test_cleaned_3 <- tm_map( corpus_test_3, tolower )
test_cleaned_3 <- tm_map( test_cleaned_3, removePunctuation )
test_cleaned_3 <- tm_map( test_cleaned_3, removeWords, stopwords('en') )
test_cleaned_3 <- tm_map( test_cleaned_3, stemDocument )

test_DTM_3 <- DocumentTermMatrix( test_cleaned_3, control = list( dictionary = observed_vocabulary_3))

train_category_3 <- train_data_3$category
test_category_3 <- test_data_3$category


library(Matrix)
train_matrix_short_description_3 <- as(train_DTM_3, "sparseMatrix")
test_matrix_short_description_3 <- as(test_DTM_3, "sparseMatrix")


category_counts_3 <- table(train_category_3)
low_freq_categories_3 <- category_counts_3[category_counts_3 < 2]
print(low_freq_categories_3)
valid_categories_3 <- names(category_counts_3[category_counts_3 >= 2])
train_category_3 <- ifelse(train_category_3 %in% valid_categories_3, train_category_3, "Other")


library(glmnet)
model_3 <- glmnet(train_matrix_short_description_3, as.factor(train_category_3), family = "multinomial")
print(model_3)


probabilities_3 <- predict(model_3, newx = test_matrix_short_description_3, s = 0.01, type = "class")
head(probabilities_3)

accuracy_3 <- mean(probabilities_3 == test_category_3)
print(paste("Accuracy=", accuracy_3))



#visualization 1
   #headline length
data_tibble_1 <- data_tibble_1 %>%
  mutate(headline_length = nchar(headline))

ggplot(data_tibble_1, aes(x = headline_length)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Distribution of headline lengths", x = "length of headline", y = "frequency")


#visualization 2
   #compare the accuracy for high frequency category

category_accuracy <- function(predictions, actual) {
  data.frame(Category = unique(actual),
             Accuracy = sapply(unique(actual), function(cat) {
               mean(predictions[actual == cat] == cat, na.rm = TRUE)
             }))
}

accuracy1 <- category_accuracy(probabilities, test_category)
accuracy2 <- category_accuracy(probabilities_2, test_category_2)
accuracy3 <- category_accuracy(probabilities_3, test_category_3)

accuracy1$Model <- "Model 1"
accuracy2$Model <- "Model 2"
accuracy3$Model <- "Model 3"

all_accuracies <- bind_rows(accuracy1, accuracy2, accuracy3)

category_counts_vi <- data_tibble_1 %>%
  group_by(category) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

frequent_categories <- category_counts_vi %>%
  filter(Count >= 100) %>%
  pull(category)

filtered_accuracies <- all_accuracies %>%
  filter(Category %in% frequent_categories)

ggplot(filtered_accuracies, aes(x = Category, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of classification accuracy for high frequency categories", x = "Category", y = "Accuracy")



