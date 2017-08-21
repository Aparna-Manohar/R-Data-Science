library(data.table)
library(reshape2)
library(proxy)
library(dplyr)
library(recommenderlab)

# There are two types of recommendations one can do
# 1. Content based filtering
# 2. Collaborative filtering

# Content based filtering
dfMovies <- read.csv("/Users/aparnamanohar/Desktop/Project/ml-latest-small/movies.csv",stringsAsFactors = FALSE)
str(dfMovies) #9125 obs. of  3 variables
dfRatings <- read.csv("/Users/aparnamanohar/Desktop/Project/ml-latest-small/ratings.csv",stringsAsFactors = FALSE)
str(dfRatings) #100004 obs. of  4 variables


# Data Preprocessing
# Obtaining the genres by removing the pipes
genres <- as.data.frame(dfMovies$genres, stringsAsFactors=FALSE)
dfGenres <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
str(dfGenres)
colnames(dfGenres) <- c(1:10)
ncol (dfGenres)
genre_list <- c("Action", "Adventure", "Animation", 
                "Children", "Comedy", "Crime",
                "Documentary", "Drama", 
                "Fantasy","Film-Noir", "Horror", "Musical", 
                "Mystery","Romance","Sci-Fi", 
                "Thriller", "War", "Western")

# Creating a movie-genre matrix which will have unique genres and uniques records of movies for it
genre_matrix <- matrix(0,9126,18) # creating an empty matrix
genre_matrix[1,] <- genre_list
colnames(genre_matrix) <- genre_list #set column names to genre list
View(genre_matrix)
for (i in 1:nrow(dfGenres)) {
  for (c in 1:ncol(dfGenres)) {
    genmat_col = which(genre_matrix[1,] == dfGenres[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}
str(genre_matrix)

# We have 18 unique genres and 9125 unique movies. Converting to dataframe and converting characters to integers
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}
View(genre_matrix2)

# Creating user profile matrix using dcast function and first setep is converting to binary format
# ratings of 4 and 5 are mapped to 1 to represent likes, and ratings of 3 and below are mapped 
# to -1 to represent dislikes.
binaryratings <- dfRatings
head(binaryratings)
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}
str(binaryratings)
head(binaryratings)

# Transforming this matrix from long format to wide format and substituting NA values with 0
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
str(binaryratings2)
head(binaryratings2)

for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds
View(binaryratings2)
head(binaryratings2)


# Movies data set and ratings dataset have different number of movies, removing them
#Remove rows that are not rated from movies dataset
movieIds <- length(unique(dfMovies$movieId)) #9125
movieIds
ratingmovieIds <- length(unique(dfRatings$movieId)) #9066
ratingmovieIds
movies2 <- dfMovies[-which((movieIds %in% ratingmovieIds) == FALSE),]
head(movies2)
nrow(movies2)
nrow(dfMovies)
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2
str(genre_matrix2)
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL
head(genre_matrix3)
str(genre_matrix3)
nrow(binaryratings2)

# Now we can calculate the dot product of the genre matrix and the ratings matrix and obtain the user profiles.
#Calculate dot product for User Profiles
result = matrix(0,18,672)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}
#Convert to Binary scale
for (i in 1:nrow(result)){
  for (c in 1:ncol(result)){
    if (result[i,c] < 0){
    result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

# Calculating the Jaccard distance for the first user profile
result2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer
#Calculate Jaccard distance between user profile and all movies
sim_results <- dist(sim_mat, method = "Jaccard")
str(sim_results)
sim_results <- as.data.frame(sim_results)
rows <- which(sim_results == min(sim_results)) #minimum distance


result2
# This is the user profile we created for user 1. User 1 has an inclination towards the following genres: 
# Children, Documentary, Fantasy, Horror, Musical, War
#Recommended movies
dfMovies[rows,]

# These movies returned a similarity score of 0.5. One of the biggest takeaways we can make here is that movieId 4291 and 
# 65585 scored high in terms of similarity because it only had 2 genres recorded. User 1 had only shown a preference for 
# Comedies, and not Crime nor Romance, so these do not seem like quality recommendations.

# Collaborative filtering
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(dfRatings, userId~movieId, value.var = "rating", na.rm=FALSE)
head(ratingmat)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds
View(ratingmat)
str(ratingmat)

#Converting rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalizing the data
ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}

recom_result
# And we have easily obtained the top 10 results for user 1!

