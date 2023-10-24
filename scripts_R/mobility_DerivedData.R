############ Mobility Derived Data ############
library(stringr)

setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#read/write data
setwd("/Users/akpiper/Documents/GitHub/mobility-books/data/derived")
c<-read.csv(gzfile("CONLIT_CharData_AP_5.csv.gz"))

write.csv(c, file="CONLIT_CharData_AP_6.csv")
system("gzip CONLIT_CharData_AP_6.csv") 

##### calculate measures on extracted data #######
setwd("/Users/akpiper/Research/Mobility")
a<-read.csv("book_char_mobility.tsv", sep="\t")

#condtion only on all words
b<-a[a$num_words == 320000,]

c<-NULL
for (i in 1:nlevels(factor(b$book_id))){
  sub<-b[b$book_id == levels(factor(b$book_id))[i],]
  sub<-sub[which(sub$char_count == max(sub$char_count)),]
  c<-rbind(c, sub)
}
#remove dupes
c<-c[-which(duplicated(c$book_id)),]

#match with metadata
c<-c[order(c$book_id),]
meta<-meta[order(meta$ID),]
meta$ID2<-gsub(".txt", "", meta$ID)
which(c$book_id != meta$ID2)
#add metadata
c$Category<-meta$Category
c$Genre<-meta$Genre
c$Tokens<-meta$token_count

#normalize gpe_places_total by tokens
c$num_gpe_places_norm<-c$num_gpe_places/c$Tokens

#normalize nongpe_places_total by tokens
c$num_nongpe_places_norm<-c$num_nongpe_places/c$Tokens

#normalize by char_count
c$num_gpe_places_norm_byCharacter<-c$num_gpe_places/c$char_count
c$num_nongpe_places_norm_byCharacter<-c$num_nongpe_places/c$char_count

#normalize character count by Tokens
c$char_count_norm<-c$char_count/c$Tokens

#create cleaned places (remove "here" and "there" and "it")
c$nongpe_places_cleaned<-gsub("\\bthere\\b", "", c$nongpe_places)
c$nongpe_places_cleaned<-gsub("\\bhere\\b", "", c$nongpe_places_cleaned)
c$nongpe_places_cleaned<-gsub("\\bit\\b", "", c$nongpe_places_cleaned)

#calculate total distance per book
c$dist_miles_allChars_norm_Tokens<-unname(tapply(b$dist_miles, b$book_id, sum))/c$Tokens
c$num_gpe_places_allChars_norm_Tokens<-unname(tapply(b$num_gpe_places, b$book_id, sum))/c$Tokens

#calculate Deixis (rate of here + there)

#function to calculate total number of unique phrases
count_strings <- function(row) {
  # Remove the single quotes and split the row by commas
  strings <- unlist(strsplit(gsub("'", "", row), ","))
  # Remove leading/trailing spaces from each string and count the non-empty ones
  return(sum(nchar(trimws(strings)) > 0))
}

# Apply the function to each row and store the result in a new column
c$nongpe_places_total <- sapply(c$nongpe_places_cleaned, count_strings)
c$gpe_places_total <- sapply(c$gpe_places, count_strings)

#calculate TTR
c$ttr_nongpe<-c$num_nongpe_places/c$nongpe_places_total
c$ttr_gpe<-c$num_gpe_places/c$gpe_places_total

#calculate AVG Distance as total distance per place
c$avg_Distance_GPE<-c$dist_miles/c$gpe_places_total

#calculate AVG Distance as total distance per token
c$avg_Distance_GPE_Tokens<-c$dist_miles/c$Tokens

#NON / GPE ratio
c$non_gpe_ratio<-c$num_nongpe_places/c$num_gpe_places

#Deixis (There+Here Frequency)
c$deixis_count_perplace<-str_count(c$nongpe_places, "'here'|'there'")/c$nongpe_places_total

##### Semantic Distance Functions  #######

#create function to extract all places in sequence
splitElements <- function(input_column) {
  # Remove the square brackets at the beginning and end of each string in the column
  cleaned_text <- gsub("^\\[|\\]$", "", input_column)
  
  # Split each string into a list of elements based on ', ' and remove single quotes
  elements_list <- lapply(strsplit(cleaned_text, "', '"), function(x) gsub("'", "", x))
  
  # Convert the list of elements into a data frame
  result_df <- data.frame(Element = unlist(elements_list))
  
  return(result_df)
}

#function to remove stopwords
remove_words <- function(text_vector, remove) {
  # Construct a single regex pattern that matches any word in the remove vector
  pattern <- paste0("\\b(", paste(remove, collapse="|"), ")\\b")
  
  # Replace matched words with empty string
  cleaned_text <- gsub(pattern, "", text_vector)
  
  # Remove leading and trailing whitespace
  return(trimws(cleaned_text))
}

#functions to compute distance of a route
distRoute <- function(adjmat, route) {
  #route<-route[route %in% colnames(adjmat)]
  d <- 0
  for(n in 2:nrow(adjmat)) {
    d <- d + adjmat[route[n-1],route[n]]
  }
  return(d)
}

distRoute2 <- function(adjmat, route) {
  route<-route[route %in% colnames(adjmat)]
  d <- 0
  for(n in 2:nrow(adjmat)) {
    d <- d + adjmat[route[n-1],route[n]]
  }
  return(d)
}

#function for getting pairwise distances for all words and phrases in a vector
library(data.table)
library(proxy)

# Function to get embeddings
# For each element of a vector this function:
#a. gets the embedding for a single word
#b. takes the average embedding for word phrases of all word vectors in the embedding
get_embedding <- function(token, glove_embeddings) {
  if (!grepl(' ', token)) {
    if (token %in% row.names(glove_embeddings)) {
      return(glove_embeddings[which(row.names(glove_embeddings) %in% token),])
    } else {
      return(NULL)
    }
  }
  
  words <- unlist(strsplit(token, ' '))
  valid_embeddings <- lapply(words, function(word) {
    if (word %in% row.names(glove_embeddings)) {
      return(glove_embeddings[which(row.names(glove_embeddings) %in% word),])
    } else {
      return(NULL)
    }
  })
  
  # Remove NULLs
  valid_embeddings <- Filter(Negate(is.null), valid_embeddings)
  if (length(valid_embeddings) == 0) {
    return(NULL)
  }
  
  avg_embedding <- colMeans(do.call(rbind, valid_embeddings))
  return(avg_embedding)
}

#Function to compute pairwise distance matrix
compute_distances <- function(word_list, glove_embeddings) {
  embeddings_list <- lapply(word_list, get_embedding, glove_embeddings=glove_embeddings)
  embeddings_list <- setNames(lapply(word_list, get_embedding, glove_embeddings=glove_embeddings), word_list)
  embeddings_list <- Filter(Negate(is.null), embeddings_list)
  
  # Convert list of embeddings to matrix
  embedding_matrix <- do.call(rbind, embeddings_list)
  
  # Compute pairwise cosine similarity
  distance_matrix <- as.matrix(dist(embedding_matrix, method="cosine"))
  
  return(distance_matrix)
}

##### Calculate Semantic Distance #######

#load stopwords
library(tm)
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)

#load embedding model
setwd("/Users/akpiper/Data/glove.6B")
glove_embeddings<-read.csv("glove.6B.100d.txt", sep=" ", quote="", header=F)
row.names(glove_embeddings)<-glove_embeddings[,1]
glove_embeddings<-glove_embeddings[,-1]

# Example usage:
#word_list <- c("apple", "fruit", "juice")
#distance_matrix <- compute_distances(word_list, glove_embeddings)

#create empty vectors
semantic_dist_total<-vector(mode="numeric", length=nrow(c))
semantic_dist_mean<-vector(mode="numeric", length=nrow(c))

for (i in 1:nrow(c)){

  print(i)
  
  #for every row create a vector of the sequence of GPEs
  places<-splitElements(c$nongpe_places_cleaned[i])
  
  #lowercase
  places<-tolower(places$Element)
  
  #remove punctuation
  places<-gsub("[[:punct:]]", "", places)
  
  #remove stopwords
  places<-remove_words(places, stop)
  
  #remove white spaces
  places<-gsub("\\s+", " ", places)
  
  #remove blanks
  places<-places[places != ""]
  
  #remove dupes in sequence
  places<-rle(places)$values
  
  #if there are more than 2 places
  if (length(places) > 2){
    
    #calculate distance matrix for all pairwise comparisons
    #first value is the vector of place names, second is the model
    distance_matrix <- compute_distances(places, glove_embeddings)
    
    #make sure there are more than 2 places
    if (!is.null(distance_matrix)){
      if (nrow(distance_matrix) > 2){
        
        #actual distance
        semantic_dist_total[i]<-distRoute(distance_matrix, 1:ncol(distance_matrix))
        semantic_dist_mean[i]<-semantic_dist_total[i]/ncol(distance_matrix)
        
      }
    }
  } else {
    semantic_dist_total[i]<-0
    semantic_dist_mean[i]<-0
  }
}

