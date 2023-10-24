#######  Mobility Validation Code  #########

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
