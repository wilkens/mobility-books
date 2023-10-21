############ Mobility Analysis ############
library(stringr)

setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

######### DATA GENERATION ###########

##### extract samples for validation #######
setwd("/Users/akpiper/Research/Mobility")
a<-read.csv("book_char_mobility.tsv", sep="\t")

#condition on 10K model
a1<-a[a$num_words == 10000,]

#keep only main characters
a2<-a1[a1$char_rank == 1,]

#minimum 3 places
a3<-a2[a2$num_gpe_places > 2,]

#minimum 100 miles
a4<-a3[a3$dist_miles > 100,]

#sample
a5<-a4[sample(nrow(a4), 30),]

#find main characte name and add to table
root.wd<-c("/Users/akpiper/Data/CONLIT_NLP")
setwd(root.wd)
#create empty vector of names
names.v<-vector(mode="character", length=nrow(a5))
for (i in 1:nrow(a5)){
  print(i)
  setwd(paste(root.wd, paste(a5$book_id[i], ".txt", sep=""), sep="/"))
  ent<-read.csv(paste(paste(a5$book_id[i], ".txt", sep=""), ".entities", sep=""), sep="\t")
  ent<-ent[ent$cat == "PER",]
  names.df<-ent[ent$COREF == as.numeric(names(sort(table(ent$COREF), decreasing = T)[1])),]
  names.df2<-names.df[names.df$prop == "PROP",]
  if (nrow(names.df2) == 0){
    names.df2<-names.df[names.df$prop == "PRON",]
  }
  names.v[i]<-names(sort(table(names.df2$text), decreasing = T)[1])
}
a6<-cbind(names=names.v, a5)

setwd("/Users/akpiper/Research/Mobility")
write.csv(a6, file="Mobility_Validation_Table.csv", row.names = F)

#write samples from main data
source.texts<-c("/Users/akpiper/Data/CONLIT")
target.texts<-c("/Users/akpiper/Research/Mobility/Mobility_Validation_Books")
setwd(source.texts)

#loop through ingest and write
for (i in 1:nrow(a5)){
  #setwd to source texts directory
  setwd(source.texts)
  #ingest
  work<-scan(paste(a5$book_id[i], ".txt", sep=""), what="character", quote="", quiet=T)
  work<-work[1:10000]
  #paste as single string
  work.all<-paste(work, sep=" ", collapse=" ")
  #set working directory to target directory
  setwd(target.texts)
  #write
  write(work.all, file=paste(a5$book_id[i], ".txt", sep=""))
}


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
c$num_gpe_places_norm<-c$gpe_places_total/c$Tokens

#normalize nongpe_places_total by tokens
c$num_nongpe_places_norm<-c$nongpe_places_total/c$Tokens

#normalize by char_count
c$num_gpe_places_norm_byCharacter<-c$gpe_places_total/c$char_count
c$num_nongpe_places_norm_byCharacter<-c$nongpe_places_total/c$char_count

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
c$non_gpe_ratio<-c$nongpe_places_total/c$gpe_places_total

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

##### Semantic Distance #######

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

write.csv(c, file="CONLIT_CharData_AP_3.csv", row.names = F)

######## STATISTICAL ANALYSIS ############
#read new table
setwd("/Users/akpiper/Research/Mobility")

c<-read.csv("CONLIT_CharData_AP_3.csv")

###### Protagonist Only Statistics ######

#Character Count per token
hist(c$char_count_norm)
t.test(char_count_norm ~ Category, data=c)
boxplot(char_count_norm ~ Category, data=c)
wilcox.test(char_count_norm ~ Category, data=c)
summary(lm(char_count_norm ~ Category + Genre, data=c))

#Number GPE per character mention
hist(c$num_gpe_places_norm_byCharacter)
t.test(num_gpe_places_norm_byCharacter ~ Category, data=c)
wilcox.test(num_gpe_places_norm_byCharacter ~ Category, data=c)
boxplot(num_gpe_places_norm_byCharacter ~ Category, data=c)
summary(lm(num_gpe_places_norm_byCharacter ~ Category + Genre, data=c))

#Number NonGPE per character mention
hist(c$num_nongpe_places_norm_byCharacter)
t.test(num_nongpe_places_norm_byCharacter ~ Category, data=c)
wilcox.test(num_nongpe_places_norm_byCharacter ~ Category, data=c)
boxplot(num_nongpe_places_norm_byCharacter ~ Category, data=c)
summary(lm(num_nongpe_places_norm_byCharacter ~ Category + Genre, data=c))

#Distance per GPE
hist(c$avg_Distance_GPE)
t.test(avg_Distance_GPE ~ Category, data=c)
wilcox.test(avg_Distance_GPE ~ Category, data=c)
boxplot(avg_Distance_GPE ~ Category, data=c)
summary(lm(avg_Distance_GPE ~ Category+Genre, data=c))

#Distance per token
hist(c$avg_Distance_GPE_Tokens)
t.test(avg_Distance_GPE_Tokens ~ Category, data=c)
wilcox.test(avg_Distance_GPE_Tokens ~ Category, data=c)
boxplot(avg_Distance_GPE_Tokens ~ Category, data=c)
summary(lm(avg_Distance_GPE_Tokens ~ Category+Genre, data=c))

#type token ratio
c.test<-c[-which(c$ttr_gpe == 0 | c$ttr_gpe == 1),]
summary(lm(ttr_gpe ~ Category+Genre, data=c.test))

#ttr for nongpe
c.test<-c[-which(c$ttr_nongpe == 0 | c$ttr_nongpe == 1),]
summary(lm(ttr_nongpe ~ Category+Genre, data=c.test))

#non/gpe ratio
c.test <- c[!is.infinite(c$non_gpe_ratio),]
summary(lm(non_gpe_ratio ~ Category+Genre, data=c.test))

#deixis
summary(lm(deixis_count_perplace ~ Category+Genre, data=c))

#semantic distance
summary(lm(semantic_dist_mean ~ Category+Genre, data=c))

#Distance by gender
c.test<-c[c$inf_gender %in% c("she/her", "he/him/his"),]
c.fic<-c.test[c.test$Category == "FIC",]
c.fic.adult<-c.fic[!c.fic$Genre %in% c("MID", "YA"),]
summary(lm(avg_Distance_GPE ~ inf_gender + Category+Genre, data=c.test))
summary(lm(avg_Distance_GPE_Tokens ~ inf_gender + Category+Genre, data=c.test))
summary(lm(dist_miles ~ inf_gender + Category+Genre, data=c.test))
summary(lm(dist_miles ~ inf_gender + Genre, data=c.fic.adult))

#physical distance by genre
model <- aov(avg_Distance_GPE_Tokens ~ Genre, data = c)
tukey_result <- TukeyHSD(model)
plot(tukey_result)
sort(tapply(c$avg_Distance_GPE_Tokens, c$Genre, median))
sort(tapply(c$avg_Distance_GPE, c$Genre, median))

#semantic distance by genre
sort(tapply(c$semantic_dist_mean, c$Genre, median))
summary(lm(semantic_dist_mean ~ Genre, data=c))

#correlation matrix of GPE and NON
keep<-c("num_nongpe_places_norm", "num_nongpe_places_norm_byCharacter", "num_gpe_places_norm",
        "num_gpe_places_norm_byCharacter")
c.test<-c[, colnames(c) %in% keep]
correlation_matrix <- cor(c.test)

# Load the 'corrplot' package for plotting
# You may need to install it using install.packages("corrplot") if you haven't already
library(corrplot)

# Create a correlation plot
corrplot(correlation_matrix, method = "color")

#make FIC table
fic<-c[c$Category == "FIC",]
meta.f<-meta[meta$Category == "FIC",]
meta.f<-meta.f[order(meta.f$ID),]
fic<-fic[order(fic$book_id),]
meta.f$ID2<-gsub(".txt", "", meta.f$ID)
which(fic$book_id != meta.f$ID2)
fic$protagonist_concentration<-meta.f$protagonist_concentration
fic$goodreads_avg<-meta.f$goodreads_avg
fic$total_ratings<-meta.f$total_ratings
fic.adult<-fic[!fic$Genre %in% c("MID", "YA"),]

#association of Goodreads ratings with spatial features
#summary(lm(goodreads_avg ~ non_minus_gpe_avg_norm+Genre, data = fic))
#summary(lm(log(total_ratings) ~ non_minus_gpe_avg_norm+Genre, data = fic))
summary(lm(log(total_ratings) ~ avg_Distance_GPE_Tokens+Genre, data = fic))
summary(lm(goodreads_avg ~ avg_Distance_GPE_Tokens+Genre, data = fic))
summary(lm(goodreads_avg ~ dist_miles+Genre+Tokens, data = fic))
summary(lm(goodreads_avg ~ avg_Distance_GPE+Genre, data = fic))

#association of Character concentration with spatial features
summary(lm(avg_Distance_GPE ~ protagonist_concentration + Genre, data = fic))
summary(lm(dist_miles_allChars_norm_Tokens ~ protagonist_concentration + Genre, data = fic))
summary(lm(num_gpe_places_allChars_norm_Tokens ~ protagonist_concentration + Genre, data = fic))

write.csv(fic, file="CONLIT_CharData_AP_FIC.csv", row.names = F)



