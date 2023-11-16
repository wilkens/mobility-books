##########  Mobility Data Analysis   ###########

#read in derived data

setwd("/Users/akpiper/Documents/GitHub/mobility-books/data/derived")
c<-read.csv(gzfile("CONLIT_CharData_AP_MW_11.csv.gz"))

write.table(foo, file="/tmp/foo.csv")
system("gzip /tmp/foo.csv") 

###### Protagonist Only Statistics ######

#Character Count per token
hist(c$char_count_norm)
t.test(char_count_norm ~ Category, data=c)
boxplot(char_count_norm ~ Category, data=c)
wilcox.test(char_count_norm ~ Category, data=c)
summary(lm(char_count_norm ~ Category + Genre, data=c))

#Distance per GPE
hist(c$avg_Distance_GPE)
boxplot(avg_Distance_GPE ~ Category, data=c)
summary(lm(avg_Distance_GPE ~ Category+Genre, data=c))

#Distance per token
hist(c$avg_Distance_GPE_Tokens)
summary(lm(avg_Distance_GPE_Tokens ~ Category+Genre, data=c))

#Number GPE per character mention
summary(lm(num_gpe_places_norm_byCharacter ~ Category + Genre, data=c))

#Number NonGPE per character mention
summary(lm(num_nongpe_places_norm_byCharacter ~ Category + Genre, data=c))

#First / Last semantic distance for GPEs
summary(lm(first_last_SemanticDist ~ Category+Genre, data=c))
fic<-c[c$Category == "FIC",]
summary(lm(first_last_SemanticDist ~ Genre, data=fic))
nrow(fic[fic$first_last_SemanticDist == 0,])/nrow(fic)
fic0<-fic[fic$first_last_SemanticDist == 0,]
table(fic0$Genre)
df<-data.frame(table(fic0$Genre), table(fic$Genre))
df<-df[,-3]
chisq.test(df[,2:3])

#type token ratio GPE
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


#### Make final table for paper #####

#prepare all measures
measures<-c("avg_Distance_GPE_Tokens", "avg_Distance_GPE", "num_gpe_places_norm_byCharacter",
            "num_nongpe_places_norm_byCharacter", "deixis_count_perplace",
            "semantic_dist_mean", "non_gpe_ratio")

measure.names<-c("Distance_per_Token", "Distance_per_GPE", "GPE_per_Character",
                 "nonGPE_per_Character", "Generics_per_nonGPE",
                 "avg_semantic_distance", "nonGPE_GPE_Ratio")

classes<-c("Fictionality", "Prestige", "Youth", "Female Character")

#functions
getr2<-function()

final.df<-NULL
  
for (i in 1:length(classes)){
  #Fictionality
  if (classes[i] == classes[1]){
    for (j in 1:(length(measures)-1)){
      model<-summary(lm(get(measures[j]) ~ Category+Genre, data = c))
      R2<-round(model$r.squared, 3)
      co.df<-model$coefficients
      p<-co.df[2,4]
      p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
      Estimate<-round(co.df[1,1], 3)
      Difference<-round(-co.df[2,1], 3)
      if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
      Category<-classes[i]
      Measure<-measure.names[j]
      temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
      final.df<-rbind(final.df, temp.df)
    }
    c.test <- c[!is.infinite(c$non_gpe_ratio),]
    model<-summary(lm(non_gpe_ratio ~ Category+Genre, data = c.test))
    R2<-round(model$r.squared, 3)
    co.df<-model$coefficients
    p<-co.df[2,4]
    p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
    Estimate<-round(co.df[1,1], 3)
    Difference<-round(-co.df[2,1], 3)
    if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
    Category<-classes[i]
    Measure<-measure.names[j+1]
    temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
    final.df<-rbind(final.df, temp.df)
  }
  
  #Prestige
  if (classes[i] %in% classes[2]){
    d<-c[c$Genre %in% c("PW", "BS"),]
    for (j in 1:(length(measures)-1)){
      model<-summary(lm(get(measures[j]) ~ Genre, data = d))
      R2<-round(model$r.squared, 3)
      co.df<-model$coefficients
      p<-co.df[2,4]
      p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
      Estimate<-round(co.df[1,1], 3)+round(co.df[2,1], 3)
      Difference<-round(co.df[2,1],3)
      if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
      Category<-classes[i]
      Measure<-measure.names[j]
      temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
      final.df<-rbind(final.df, temp.df)
    }
    d.test <- d[!is.infinite(d$non_gpe_ratio),]
    model<-summary(lm(non_gpe_ratio ~ Genre, data = d.test))
    R2<-round(model$r.squared, 3)
    co.df<-model$coefficients
    p<-co.df[2,4]
    p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
    Estimate<-round(co.df[1,1], 3)+round(co.df[2,1], 3)
    Difference<-round(co.df[2,1], 3)
    if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
    Category<-classes[i]
    Measure<-measure.names[j+1]
    temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
    final.df<-rbind(final.df, temp.df)
  }
  
  #Youth
  if (classes[i] %in% classes[3]){
    d<-c[c$Genre %in% c("NYT", "BS", "PW", "MID"),]
    d$Adult<- ifelse(d$Genre == "MID", "MID", "AD")
    for (j in 1:(length(measures)-1)){
      model<-summary(lm(get(measures[j]) ~ Adult, data = d))
      R2<-round(model$r.squared, 3)
      co.df<-model$coefficients
      p<-co.df[2,4]
      p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
      Estimate<-round(co.df[1,1], 3)+round(co.df[2,1], 3)
      Difference<-round(co.df[2,1],3)
      if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
      Category<-classes[i]
      Measure<-measure.names[j]
      temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
      final.df<-rbind(final.df, temp.df)
    }
    d.test <- d[!is.infinite(d$non_gpe_ratio),]
    model<-summary(lm(non_gpe_ratio ~ Adult, data = d.test))
    R2<-round(model$r.squared, 3)
    co.df<-model$coefficients
    p<-co.df[2,4]
    p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
    Estimate<-round(co.df[1,1], 3)+round(co.df[2,1], 3)
    Difference<-round(co.df[2,1], 3)
    if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
    Category<-classes[i]
    Measure<-measure.names[j+1]
    temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
    final.df<-rbind(final.df, temp.df)
  }
  
  #Female Character
  if (classes[i] %in% classes[4]){
    d<-c[c$Category == "FIC",]
    d<-d[d$inf_gender %in% c("she/her", "he/him/his"),]
    for (j in 1:(length(measures)-1)){
      model<-summary(lm(get(measures[j]) ~ inf_gender+Genre, data = d))
      R2<-round(model$r.squared, 3)
      co.df<-model$coefficients
      p<-co.df[2,4]
      p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
      Estimate<-round(co.df[1,1], 3)+round(co.df[2,1], 3)
      Difference<-round(co.df[2,1],3)
      if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
      Category<-classes[i]
      Measure<-measure.names[j]
      temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
      final.df<-rbind(final.df, temp.df)
    }
    d.test <- d[!is.infinite(d$non_gpe_ratio),]
    model<-summary(lm(non_gpe_ratio ~ inf_gender+Genre, data = d.test))
    R2<-round(model$r.squared, 3)
    co.df<-model$coefficients
    p<-co.df[2,4]
    p.code<-if(p<.001){p.code<-c("***")} else if(p<0.01 & p>.001){p.code<-c("**")}else if(p<0.05 & p>.01){p.code<-c("*")}else{p.code<-"."}
    Estimate<-round(co.df[1,1], 3)+round(co.df[2,1], 3)
    Difference<-round(co.df[2,1], 3)
    if(Difference < 0){Valence<-c("-")}else{Valence<-c("+")}
    Category<-classes[i]
    Measure<-measure.names[j+1]
    temp.df<-data.frame(Category, Measure, Estimate, Difference, Valence, R2, p, p.code)
    final.df<-rbind(final.df, temp.df)
  }
}






