############ Mobility Derived Data ############
library(stringr)


#load CONLIT metadata
meta<-read.csv("CONLIT_META.csv")
setwd("/Users/akpiper/Documents/GitHub/mobility-books/data/metadata")
meta<-read.csv("EARLY_META.tsv", sep="\t")

#read/write data
setwd("/Users/akpiper/Documents/GitHub/mobility-books/data/derived")
#c<-read.csv(gzfile("CONLIT_CharData_AP_MW_8.csv.gz"))
c<-read.csv(gzfile("EARLY_CharData_AP_MW_7.csv.gz"))

# c<-c[order(c$book_id),]
# meta<-meta[order(meta$book_id),]
# meta<-meta[!duplicated(meta$book_id),]
# c<-c[c$book_id %in% meta$book_id,]
# which(c$book_id != meta$book_id)
# c<-cbind(meta$source, c)
# colnames(c)[1]<-c("collection")
# c<-cbind(meta$pub_date, c)
# colnames(c)[1]<-c("pub_date")

write.csv(c, file="CONLIT_CharData_AP_MW_8.csv", row.names = F)
system("gzip CONLIT_CharData_AP_MW_8.csv") 

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
#c$dist_miles_allChars_norm_Tokens<-unname(tapply(b$dist_miles, b$book_id, sum))/c$Tokens
#c$num_gpe_places_allChars_norm_Tokens<-unname(tapply(b$num_gpe_places, b$book_id, sum))/c$Tokens

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

#rank by num_nonGPE & dist_miles normalized by tokens
c<-c[order(c$nongpe_places_total/c$Tokens),]
c$non_gpe_total_rank<-seq(1:nrow(c))

#dist_miles
c<-c[order(c$dist_miles/c$Tokens),]
c$dist_miles_rank<-seq(1:nrow(c))
#transform all 0 values into rank 1
c$dist_miles_rank[c$dist_miles == 0] <- 1
#reorder
c<-c[order(c$book_id),]
#reorder
c<-c[order(c$dist_miles_rank, -c$non_gpe_total_rank),]
#subset
d<-c[c$Genre %in% c("PW", "BS", "NYT", "MY"),]
d<-d[1:50,]
write.csv(d, file="TopAdultFiction_NonGPE.csv", row.names = F)

#Deixis (There+Here Frequency)
c$deixis_count_perplace<-str_count(c$nongpe_places, "'here'|'there'")/c$nongpe_places_total

#End Point Similarity
#calculates the distance between the starting location and the final location and calculates a z score
#for that distance (how many standard deviations above or below avg distance in the book is this distance)
#this illustrates the relationship between distance and narrative closure in the sense of circularity

##### Most frequent place calculations ######

#create function to extract all places in sequence
splitElements2 <- function(input_column) {
  # Remove the square brackets at the beginning and end of each string in the column
  cleaned_text <- gsub("^\\[|\\]$", "", input_column)
  
  # Split each string into a list of elements based on ', ' and remove single quotes
  elements_list <- lapply(strsplit(cleaned_text, "', '"), function(x) gsub("'", "", x))
  
  # Convert the list of elements into a data frame
  result_df <- unlist(elements_list)
  
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

#extract every place name and tabulate
fic<-c[c$Category == "FIC",]
fic<-c
list_of_vectors <- lapply(fic$gpe_sequences, splitElements2)
place.v<-unlist(list_of_vectors)
top.v<-names(sort(table(place.v), decreasing = T)[1:5])

#for the most common place extract the next place in sequence
#i.e. where do you go from New York?
top.plus1.v<-vector(mode="character", length=length(top.v))
for (i in 1:length(top.v)){
  next.v<-place.v[(which(place.v == top.v[i])+1)]
  top.plus1.v[i]<-paste(names(sort(table(next.v), decreasing = T)[1:5]), collapse = ",")
}

#create data frame
df<-data.frame(top.v, top.plus1.v)
colnames(df)<-c("Top Places", "Next Places")

#repeat for nonGPEs
library(tm)
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)

#extract vector of places
list_of_vectors <- lapply(fic$nongpe_places_cleaned, splitElements2)
places<-unlist(list_of_vectors)
#clean
places<-tolower(places)
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
#tabulate
top.non<-names(sort(table(places), decreasing = T)[1:5])

#for the most common place extract the next place in sequence
#i.e. where do you go from New York?
top.plus1.non<-vector(mode="character", length=length(top.non))
for (i in 1:length(top.non)){
  next.v<-places[(which(places == top.non[i])+1)]
  top.plus1.non[i]<-paste(names(sort(table(next.v), decreasing = T)[1:5]), collapse = ",")
}

df2<-data.frame(top.non, top.plus1.non)
colnames(df2)<-c("Top Places", "Next Places")

df3<-cbind(df, df2)



