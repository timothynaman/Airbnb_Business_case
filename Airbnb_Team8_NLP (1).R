###############################################################################
## Final Project NLP and Data Visualization - MBAN2 ###########################
###############################################################################

# Download data with Mongolite ----
install.packages("mongolite") 
library(mongolite)

connection_string <- 'mongodb+srv://mdurand:Belcorp2121@cluster0.pcobf.mongodb.net/sample_airbnb?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

airbnb_all <- airbnb_collection$find()
airbnb_all <- as.data.frame(airbnb_all)
save(airbnb_all,file="airbnb_all.Rda")

# Creating data for tableau ----
airbnb_tbl <- data.frame(name = airbnb_all$name,
                         property_type = airbnb_all$property_type,
                         room_type = airbnb_all$room_type,
                         bet_type = airbnb_all$bed_type,
                         min_night = airbnb_all$minimum_nights,
                         max_night = airbnb_all$maximum_nights,
                         number_guests = airbnb_all$accommodates,
                         number_bedrooms = airbnb_all$bedrooms,
                         number_bed = airbnb_all$beds,
                         number_reviews = airbnb_all$number_of_reviews,
                         number_bathroom = airbnb_all$bathrooms,
                         price = airbnb_all$price,
                         security_deposit = airbnb_all$security_deposit,
                         cleanning_fee = airbnb_all$cleaning_fee,
                         extra_people = airbnb_all$extra_people,
                         guest_included = airbnb_all$guests_included,
                         host_superhost = airbnb_all$host$host_is_superhost,
                         host_listing_count = airbnb_all$host$host_listings_count,
                         address_country = airbnb_all$address$country,
                         address_market = airbnb_all$address$market,
                         review_score_accuracy = airbnb_all$review_scores$review_scores_accuracy,
                         review_score_cleanliness = airbnb_all$review_scores$review_scores_cleanliness,
                         review_score_checkin = airbnb_all$review_scores$review_scores_checkin,
                         review_score_communication = airbnb_all$review_scores$review_scores_communication,
                         review_score_location = airbnb_all$review_scores$review_scores_location,
                         review_score_value = airbnb_all$review_scores$review_scores_value,
                         review_score_rating = airbnb_all$review_scores$review_scores_rating,
                         weekly_price = airbnb_all$weekly_price,
                         monthly_price = airbnb_all$monthly_price)

airbnb_tbl$min_night <- as.numeric(airbnb_tbl$min_night)
airbnb_tbl$max_night <- as.numeric(airbnb_tbl$max_night)
airbnb_tbl$number_guests <- as.numeric(airbnb_tbl$number_guests)
airbnb_tbl$number_bedrooms <- as.numeric(airbnb_tbl$number_bedrooms)
airbnb_tbl$number_bed <- as.numeric(airbnb_tbl$number_bed)
airbnb_tbl$number_reviews <- as.numeric(airbnb_tbl$number_reviews)
airbnb_tbl$host_listing_count <- as.numeric(airbnb_tbl$host_listing_count)
airbnb_tbl$review_score_accuracy <- as.numeric(airbnb_tbl$review_score_accuracy)
airbnb_tbl$review_score_cleanliness <- as.numeric(airbnb_tbl$review_score_cleanliness)
airbnb_tbl$review_score_checkin <- as.numeric(airbnb_tbl$review_score_checkin)
airbnb_tbl$review_score_communication <- as.numeric(airbnb_tbl$review_score_communication)
airbnb_tbl$review_score_location <- as.numeric(airbnb_tbl$review_score_location)
airbnb_tbl$review_score_value <- as.numeric(airbnb_tbl$review_score_value)
airbnb_tbl$review_score_rating <- as.numeric(airbnb_tbl$review_score_rating)

index <- is.na(airbnb_tbl$review_score_rating)
airbnb_tbl_rating <- airbnb_tbl[!index,]

# Logistic Regression Superhost----
index      <- sample(1:nrow(airbnb_tbl),size = 0.8*nrow(airbnb_tbl))
airbnb_tbl$host_superhost <- as.numeric(airbnb_tbl$host_superhost)
base_train <- airbnb_tbl[index, ]
base_test  <- airbnb_tbl[-index,]
my_logit   <- glm(host_superhost~ property_type + room_type + bet_type + min_night +
                    max_night + number_guests + number_bedrooms + number_bed + number_reviews + number_bathroom+
                    price + security_deposit + cleanning_fee + extra_people + guest_included + host_listing_count +
                    review_score_rating,
                  data=base_train, family = binomial)
summary(my_logit)
# As we can see, the variables that are relevant for the model number_guests, number_bedrooms, number_reviews,
# number_bathroom, price, cleanning_fee, host_listing_count, review_score_rating
my_logit_new <- glm(host_superhost ~ number_guests + number_bedrooms + number_reviews + number_bathroom + price+
                      cleanning_fee + host_listing_count + review_score_rating, data=base_train, family = binomial)
summary(my_logit_new)
# without Bathroom
my_logit_new_2 <- glm(host_superhost ~ number_guests + number_bedrooms + number_reviews + price+
                      cleanning_fee + host_listing_count + review_score_rating, data=base_train, family = binomial)
summary(my_logit_new_2)
# Evaluating the efficiency of the model

base_test$predict <- predict(my_logit_new_2, base_test, type='response')

library(caret)
my_prediction_testing <- predict(my_logit_new_2, base_test, type="response")
confusionMatrix(data=as.factor(as.numeric(my_prediction_testing > 0.5)), 
                reference = as.factor(as.numeric(base_test$host_superhost)))

# Splitting Data per Country---- 
library(dplyr)
library(stringr)
airbnb_aus <-  airbnb_all[str_detect(airbnb_all$address$country,"Australia"),] ###
airbnb_bra <-  airbnb_all[str_detect(airbnb_all$address$country,"Brazil"),]
airbnb_can <-  airbnb_all[str_detect(airbnb_all$address$country,"Canada"),] ###
airbnb_bra <-  airbnb_all[str_detect(airbnb_all$address$country,"China"),]
airbnb_hk  <-  airbnb_all[str_detect(airbnb_all$address$country,"Hong Kong"),]
airbnb_por <-  airbnb_all[str_detect(airbnb_all$address$country,"Portugal"),]
airbnb_spn <-  airbnb_all[str_detect(airbnb_all$address$country,"Spain"),]
airbnb_spn <-  airbnb_all[str_detect(airbnb_all$address$country,"Turkey"),]
airbnb_usa <-  airbnb_all[str_detect(airbnb_all$address$country,"United States"),] ###

# Extracting comments ----
## USA
airbnb_comment_usa <- data.frame(text = airbnb_usa$reviews[[1]][[6]])
for (i in 2:nrow(airbnb_usa)) {
  try(airbnb_new <- data.frame(text = airbnb_usa$reviews[[i]][[6]]))
  try(airbnb_comment_usa <- rbind(airbnb_comment_usa,airbnb_new))
}
airbnb_comment_usa <- data.frame(country = "United States",text = airbnb_comment_usa)

## Australia
airbnb_comment_aus <- data.frame(text = airbnb_aus$reviews[[1]][[6]])
for (i in 2:nrow(airbnb_aus)) {
  try(airbnb_new <- data.frame(text = airbnb_aus$reviews[[i]][[6]]))
  try(airbnb_comment_aus <- rbind(airbnb_comment_aus,airbnb_new))
}
airbnb_comment_aus <- data.frame(country = "Australia",text = airbnb_comment_aus)

## Canada 
airbnb_comment_can <- data.frame(text = airbnb_can$reviews[[4]][[6]])
for (i in 5:nrow(airbnb_can)) {
  try(airbnb_new <- data.frame(text = airbnb_can$reviews[[i]][[6]]))
  try(airbnb_comment_can <- rbind(airbnb_comment_can,airbnb_new))
}
airbnb_comment_can<- data.frame(country = "Canada",text = airbnb_comment_can)

# Selecting language (fix function)----
# install.packages("textcat")
library(textcat)
## USA
airbnb_comment_usa <- data.frame(country = airbnb_comment_usa$country,
                                 text = airbnb_comment_usa$text,
                                 lan=textcat(airbnb_comment_usa$text, p = ECIMCI_profiles))
airbnb_comment_usa_en <- airbnb_comment_usa %>% filter(lan == "en")

## Australia
airbnb_comment_aus <- data.frame(country = airbnb_comment_aus$country,
                                 text = airbnb_comment_aus$text,
                                 lan=textcat(airbnb_comment_aus$text, p = ECIMCI_profiles))
airbnb_comment_aus_en <- airbnb_comment_aus %>% filter(lan == "en")

## Canada
airbnb_comment_can <- data.frame(country = airbnb_comment_can$country,
                                 text = airbnb_comment_can$text,
                                 lan=textcat(airbnb_comment_can$text, p = ECIMCI_profiles))
airbnb_comment_can_en <- airbnb_comment_can %>% filter(lan == "en")

# Frameworks - USA----
unnest_usa <- airbnb_comment_usa_en %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T)

unnest_usa_export <- data.frame(Country="United States",unnest_usa)

tidy_usa <- airbnb_comment_usa_en %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

# Ngrams
USA_bigrams <- airbnb_comment_usa_en %>%   unnest_tokens(bigram, text, token = "ngrams", n=2)

library(tidyr)
USA_bigrams_separated <- USA_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

USA_bigrams_filtered <- USA_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts_usa <- USA_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts_paste <- data.frame(word=paste(bigram_counts_usa$word1,bigram_counts_usa$word2,sep =" "),
                                  count = bigram_counts_usa$n)
bigrams_usa_export <- data.frame(Country="United States",bigram_counts_paste)

bigram_graph_usa <- bigram_counts_usa %>%
  filter(n>300) %>%
  graph_from_data_frame() %>% 
  na.omit()

library(ggraph)
ggraph(bigram_graph_usa, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =0.5, hjust=0.5)

# Sentiment Analysis
afinn_usa <- tidy_usa %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_usa_exp <- tidy_usa %>%
  inner_join(get_sentiments("afinn"))%>%
  mutate(method="AFINN")

bing_usa_exp <- tidy_usa%>%
  inner_join(get_sentiments("bing"))%>%
  mutate(method = "Bing et al.") %>% 
  filter(sentiment %in% c("positive", "negative"))

bing_usa_exp$value <- gsub("positive",1,bing_usa_exp$sentiment)
bing_usa_exp$value <- as.numeric(gsub("negative",-1,bing_usa_exp$value))
bing_usa_exp <- bing_usa_exp[,c(1,3,4,5,6)]

nrc_usa_exp <- tidy_usa%>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative"))) %>%
  mutate(method = "NRC")

nrc_usa_exp$value <- gsub("positive",1,nrc_usa_exp$sentiment)
nrc_usa_exp$value <- as.numeric(gsub("negative",-1,nrc_usa_exp$value))
nrc_usa_exp <- nrc_usa_exp[,c(1,3,4,5,6)]

nrc_usa_exp_all <- tidy_usa%>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(method = "NRC")

afinn_usa_exp <- data.frame(country=afinn_usa_exp$country, 
                            word=afinn_usa_exp$word, sentiment=NA,
                            method=afinn_usa_exp$method, 
                            value=afinn_usa_exp$value)
sentiments_export_usa <- rbind(afinn_usa_exp, bing_usa_exp, nrc_usa_exp)

# Frameworks - Australia ----
unnest_aus <- airbnb_comment_aus_en %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T)

unnest_aus_export <- data.frame(Country="Australia",unnest_aus)

tidy_aus <- airbnb_comment_aus_en %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

# Ngrams
aus_bigrams <- airbnb_comment_aus_en %>%   unnest_tokens(bigram, text, token = "ngrams", n=2)

library(tidyr)
aus_bigrams_separated <- aus_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

aus_bigrams_filtered <- aus_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts_aus <- aus_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts_paste <- data.frame(word=paste(bigram_counts_aus$word1,bigram_counts_aus$word2,sep =" "),
                                  count = bigram_counts_aus$n)
bigrams_aus_export <- data.frame(Country="Australia",bigram_counts_paste)

bigram_graph_aus <- bigram_counts_aus %>%
  filter(n>120) %>%
  graph_from_data_frame() %>% 
  na.omit()

library(ggraph)
ggraph(bigram_graph_aus, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =0.5, hjust=0.5)

# Sentiment Analysis
afinn_aus <- tidy_aus %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_aus_exp <- tidy_aus %>%
  inner_join(get_sentiments("afinn"))%>%
  mutate(method="AFINN")

bing_aus_exp <- tidy_aus%>%
  inner_join(get_sentiments("bing"))%>%
  mutate(method = "Bing et al.") %>% 
  filter(sentiment %in% c("positive", "negative"))

bing_aus_exp$value <- gsub("positive",1,bing_aus_exp$sentiment)
bing_aus_exp$value <- as.numeric(gsub("negative",-1,bing_aus_exp$value))
bing_aus_exp <- bing_aus_exp[,c(1,3,4,5,6)]

nrc_aus_exp <- tidy_aus%>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative"))) %>%
  mutate(method = "NRC")

nrc_aus_exp$value <- gsub("positive",1,nrc_aus_exp$sentiment)
nrc_aus_exp$value <- as.numeric(gsub("negative",-1,nrc_aus_exp$value))
nrc_aus_exp <- nrc_aus_exp[,c(1,3,4,5,6)]

nrc_aus_exp_all <- tidy_aus%>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(method = "NRC")

afinn_aus_exp <- data.frame(country=afinn_aus_exp$country, 
                            word=afinn_aus_exp$word, sentiment=NA,
                            method=afinn_aus_exp$method, 
                            value=afinn_aus_exp$value)
sentiments_export_aus <- rbind(afinn_aus_exp, bing_aus_exp, nrc_aus_exp)

# Frameworks - Canada ----
unnest_can <- airbnb_comment_can_en %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T)

unnest_can_export <- data.frame(Country="Canada",unnest_can)

tidy_can <- airbnb_comment_can_en %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

# Ngrams
can_bigrams <- airbnb_comment_can_en %>%   unnest_tokens(bigram, text, token = "ngrams", n=2)

library(tidyr)
can_bigrams_separated <- can_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

can_bigrams_filtered <- can_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts_can <- can_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts_paste <- data.frame(word=paste(bigram_counts_can$word1,bigram_counts_can$word2,sep =" "),
                                  count = bigram_counts_can$n)
bigrams_can_export <- data.frame(Country="Canada",bigram_counts_paste)

bigram_graph_can <- bigram_counts_can %>%
  filter(n>80) %>%
  graph_from_data_frame() %>% 
  na.omit()

library(ggraph)
ggraph(bigram_graph_can, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =0.5, hjust=0.5)

# Sentiment Analysis
afinn_can <- tidy_can %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_can_exp <- tidy_can %>%
  inner_join(get_sentiments("afinn"))%>%
  mutate(method="AFINN")

bing_can_exp <- tidy_can%>%
  inner_join(get_sentiments("bing"))%>%
  mutate(method = "Bing et al.") %>% 
  filter(sentiment %in% c("positive", "negative"))

bing_can_exp$value <- gsub("positive",1,bing_can_exp$sentiment)
bing_can_exp$value <- as.numeric(gsub("negative",-1,bing_can_exp$value))
bing_can_exp <- bing_can_exp[,c(1,3,4,5,6)]

nrc_can_exp <- tidy_can%>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative"))) %>%
  mutate(method = "NRC")

nrc_can_exp$value <- gsub("positive",1,nrc_can_exp$sentiment)
nrc_can_exp$value <- as.numeric(gsub("negative",-1,nrc_can_exp$value))
nrc_can_exp <- nrc_can_exp[,c(1,3,4,5,6)]

nrc_can_exp_all <- tidy_can%>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(method = "NRC")

afinn_can_exp <- data.frame(country=afinn_can_exp$country, 
                            word=afinn_can_exp$word, sentiment=NA,
                            method=afinn_can_exp$method, 
                            value=afinn_can_exp$value)
sentiments_export_can <- rbind(afinn_can_exp, bing_can_exp, nrc_can_exp)

# Correlation between all countries -----
library(dplyr)
library(tidytext)
library(tidyr)
frequency_correlogram <- bind_rows(mutate(tidy_usa),
                       mutate(tidy_aus),
                       mutate(tidy_can)
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(country, word) %>%
  group_by(country) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(country, proportion) %>%
  gather(country, proportion, `Australia`, `Canada`) 

# let's plot the correlograms:
library(scales)
ggplot(frequency_correlogram, aes(x=proportion, y=`United States`, 
                      color = abs(`United States`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~country, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "USA", x=NULL)

# doing the cor.test()
cor.test(data=frequency_correlogram[frequency_correlogram$country == "Australia",],
         ~proportion + `United States`)

cor.test(data=frequency_correlogram[frequency_correlogram$country == "Canada",],
         ~proportion + `United States`)

# Exporting csv ----

## Exporting Canada
write.csv(unnest_can_export,"/Users/monicahuaytadurand/Downloads/unnest_can.csv", row.names = FALSE)
write.csv(bigrams_can_export,"/Users/monicahuaytadurand/Downloads/bigrams_can.csv", row.names = FALSE)
write.csv(sentiments_export_can,"/Users/monicahuaytadurand/Downloads/sentiments_can.csv", row.names = FALSE)
write.csv(nrc_can_exp_all,"/Users/monicahuaytadurand/Downloads/nrc_can.csv", row.names = FALSE)

## Exporting USA
write.csv(unnest_usa_export,"/Users/monicahuaytadurand/Downloads/unnest_usa.csv", row.names = FALSE)
write.csv(bigrams_usa_export,"/Users/monicahuaytadurand/Downloads/bigrams_usa.csv", row.names = FALSE)
write.csv(sentiments_export_usa,"/Users/monicahuaytadurand/Downloads/sentiments_usa.csv", row.names = FALSE)
write.csv(nrc_usa_exp_all,"/Users/monicahuaytadurand/Downloads/nrc_usa.csv", row.names = FALSE)

## Exporting AUS
write.csv(unnest_aus_export,"/Users/monicahuaytadurand/Downloads/unnest_aus.csv", row.names = FALSE)
write.csv(bigrams_aus_export,"/Users/monicahuaytadurand/Downloads/bigrams_aus.csv", row.names = FALSE)
write.csv(sentiments_export_aus,"/Users/monicahuaytadurand/Downloads/sentiments_aus.csv", row.names = FALSE)
write.csv(nrc_aus_exp_all,"/Users/monicahuaytadurand/Downloads/nrc_aus.csv", row.names = FALSE)

## Exporting Correlogram
write.csv(frequency_correlogram,"/Users/monicahuaytadurand/Downloads/correlogram.csv", row.names = FALSE)

## Exporting Tableau
write.csv(airbnb_tbl,"/Users/monicahuaytadurand/Downloads/airbnb_tbl.csv", row.names = FALSE)
write.csv(airbnb_tbl_rating,"/Users/monicahuaytadurand/Downloads/airbnb_tbl_rating.csv", row.names = FALSE)