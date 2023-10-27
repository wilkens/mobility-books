##########  Mobility Data Analysis   ###########

#read in derived data

setwd("/Users/akpiper/Documents/GitHub/mobility-books/data/derived")
c<-read.csv(gzfile("CONLIT_CharData_AP_MW_7.csv.gz"))

write.table(foo, file="/tmp/foo.csv")
system("gzip /tmp/foo.csv") 

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



