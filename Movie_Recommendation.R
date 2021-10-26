#set working directory
setwd("D:/R files/movie recommendation/movie_rec_in_R")
#import libraries
library(recommenderlab)
library(data.table)
library(reshape2)
library(ggplot2)

#import data set 
movies_df <- read.csv("D:/R files/movie recommendation/movie_rec_in_R/movies.csv")
ratings_df <- read.csv("D:/R files/movie recommendation/movie_rec_in_R/ratings.csv")

str(movies_df)
#get summary
summary(movies_df)
summary(ratings_df)

#clean the data

movie_genre<-as.data.frame(movies_df$genres , stringsAsFactors = FALSE)
movie_genre2 <-as.data.frame(tstrsplit(movie_genre[,1],"[|]",type.convert = TRUE),
                             stringsAsFactors = FALSE)

colnames(movie_genre2)<-c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")


genre_matrix1<-matrix(0,10330,18)
genre_matrix1[1,] <-list_genre
colnames(genre_matrix1) <- list_genre


for (index in 1:nrow(movie_genre2)) {
   for (col in 1:nrow(movie_genre2)) {
      gen_col <- which(genre_matrix1[1,] == movie_genre2[index , col])
      genre_matrix1[index + 1 , gen_col] <- 1
   }
}


genre_matrix2 <-as.data.frame(genre_matrix1[-1,] , stringsAsFactors = FALSE)

for (col in 1:ncol(genre_matrix2)) {
   genre_matrix2[,col] <-as.integer(genre_matrix2[,col])
}

str(genre_matrix2)

#create the search matrix
search_matrix <-cbind(movies_df[,1:2] , genre_matrix2)


#use recommendation metrix
rating_matrix <-dcast(ratings_df , ratings_df$userId~ratings_df$movieId  , value.var = "rating" , na.rm = FALSE)
rating_matrix <- as.matrix(rating_matrix[,-1])
#convert rating matrix into a recommendation matrix
rating_matrix <- as(rating_matrix , "realRatingMatrix")
rating_matrix


recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model , "[["  ,"description")

recommendation_model$IBCF_realRatingMatrix$parameters


similarity_mat<-similarity(rating_matrix[1:4,],method = "cosine" , which="users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat) , main = "users similaries")

rating_values <- as.vector(rating_matrix@data)
unique(rating_values) # extracting unique ratings

rating_table <-table(rating_values)
rating_table

movies_view<-colCounts(rating_matrix)
table_view<-data.frame(movie = names(movies_view),views = movies_view)
table_view<-table_view[order(table_view$views , decreasing = TRUE), ]


table_view$title <-NA
for (index in 1:10325) {
   table_view[index , 3] <-as.character(subset(movies_df, movies_df$movieId==table_view[index , 1])$title)
}

table_view[1:6 , ]

ggplot(table_view[1:6, ], aes(x = title, y = views)) +
   geom_bar(stat="identity", fill = 'steelblue') +
   geom_text(aes(label=views), vjust=-0.3, size=3.5) +
   theme(axis.text.x = element_text(angle = 10, hjust = 1)) +
   
   ggtitle("Total Views of the Top Films")


image(rating_matrix[1:20 , 1:20], axes=FALSE , main= "heat map")


movie_ratings<- rating_matrix[rowCounts(rating_matrix)>50,
                              colCounts(rating_matrix)>50]
movie_ratings

minimum_movies<-quantile(rowCounts(movie_ratings) , 0.98)
minimum_users<-quantile(colCounts(movie_ratings) , 0.98)


image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")


average_ratings<-rowMeans(movie_ratings)
qplot(average_ratings , fill=I("steelblue") , col=I("red"))+
   ggtitle("distribution of the average rating per")

normlizied_rating<-normalize(movie_ratings)
sum(rowMeans(normlizied_rating)>0.00001)

image(normlizied_rating[rowCounts(normlizied_rating) > minimum_movies,
                        colCounts(normlizied_rating) > minimum_users],
      main = "normalized rating of the top users")


binary_minimum_movies <- quantile(rowCounts(movie_ratings) ,0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings),0.95)

good_rated_flims<-binarize(movie_ratings , minRating = 3)
image(good_rated_flims[rowCounts(movie_ratings) > binary_minimum_movies , 
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "heatmap of top usera and movies")

sampled_data <-sample(x=c(TRUE , FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE, 
                      prob = c(0.8 , 0.2))
training_data<-movie_ratings[sampled_data,]
testing_data<-movie_ratings[!sampled_data,]

recommendation_syatem<-recommenderRegistry$get_entries(dataType="realRatingMatrix")
recommendation_syatem$IBCF_realRatingMatrix$parameters


recomm_model <- Recommender(data = training_data,
                            method="IBCF",
                            parameter=list(k=30))
recomm_model
class(recomm_model)

model_info<-getModel(recomm_model)
class(model_info$sim)
dim(model_info$sim)
top_items<-20
image(model_info$sim[1:top_items , 1:top_items],
      main="heatmap of the first row and column")


sum_rows<-rowSums(model_info$sim >0)
table(sum_rows)


sum_cols<-colSums(model_info$sim >0)
qplot(sum_cols , fill=I("blue") , col=I("red")) + ggtitle("distribution of the column count")


top_recommendation<-10  # the number of items to recommend to each user
predicted_recommendations<-predict(object = recomm_model,
                                   newdata  = testing_data,
                                   n=top_recommendation)
predicted_recommendations




user1<-predicted_recommendations@items[[1]]
movies_user1<-predicted_recommendations@itemLabels[user1]
movies_user2<-movies_user1
for(index in 1:10){
   movies_user2[index]<-as.character(subset(movies_df,
                                            movies_df$movieId==movies_user1[index])$title)
}

movies_user2

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]


number_of_items <- factor(table(recommendation_matrix))

chart_title <- "Distribution of the Number of Items for IBCF"

qplot(number_of_items, fill=I("steelblue"), col=I("red")) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
for(i in 1:4) {
   table_top[i,1] <- as.character(subset(movies_df,
                                         movies_df$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie Title", "No. of Items")
head(table_top)







