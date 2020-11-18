## Setup
### Load packages
library(ggplot2)
library(dplyr)
library(GGally)
library(tidyr)
### Load data
load("movies.Rdata")

## Part 3: Exploratory data analysis
# Remove duplicates
movies <- movies[!duplicated(movies),]
# There was one duplicate: The movie titled "Man on Wire"

## Check relationships for categorical variables
# title_type - Documentaries and tv movies tend to obtain higher audience scores
movies %>% ggplot(aes(y=audience_score, color=title_type)) + geom_boxplot()

# genre - Again, documentaries tend to have higher scores as well as horror movies
movies %>% group_by(genre) %>% summarise(mean_score=mean(audience_score)) %>%
    ggplot(aes(x=reorder(genre, mean_score), y=mean_score, fill=genre)) +
    geom_bar(stat="identity") + xlab("Genre") + ylab("Mean audience score") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# mpaa_rating - General audiences movies evidentiated greater audience scores
# than restricted and adults only rated movies
movies %>% group_by(mpaa_rating) %>% summarise(mean_score=mean(audience_score)) %>%
    ggplot(aes(x=reorder(mpaa_rating, -mean_score), y=mean_score, fill=mpaa_rating)) + geom_bar(stat="identity") +
    xlab("MPAA rating") + ylab("Mean audience score")

# critics_rating - As the movie is gets a better critic rating
# (Certified fresh, for example) it will also increase its score
movies %>% group_by(critics_rating) %>% summarise(mean_score=mean(audience_score)) %>%
    ggplot(aes(x=reorder(critics_rating, mean_score), y=mean_score, fill=critics_rating)) +
    geom_bar(stat="identity") + xlab("Critics rating") + ylab("Mean audience score")

# audience_rating - Something similar as the critic rating
# But audience rating and audience score may be dependent because they come from the same source
movies %>% group_by(audience_rating) %>% summarise(mean_score=mean(audience_score)) %>%
    ggplot(aes(x=audience_rating,y=mean_score)) + geom_bar(stat="identity") + ylab("Mean audience score") + xlab("Audience rating")

# Whether or not the movie won or was nominated for a best picture Oscar best_pic_nom
# enhances its probability to get a higher audience score. It also
# seems to be important the fact that company is in the Top 200 Box Office list
movies[,c("best_pic_nom","best_pic_win","best_actor_win","best_actress_win","best_dir_win","top200_box", "audience_score")] %>%
   gather(variable,value,-audience_score) %>% group_by(variable,value) %>%
    summarise(mean_score=mean(audience_score)) %>% ggplot(aes(x=reorder(variable, mean_score), y=mean_score, fill=value)) +
    geom_bar(stat="identity", position="dodge") + xlab(element_blank())

# Check relationships for numerical variables and response variable (audience_score)
# runtime: 0.182 - Low
# imdb_rating: 0.865 - High
# imdb_num_votes: 0.290 - Low
# critics_score: 0.703 - High
ggpairs(movies[,c("audience_score", "runtime", "imdb_rating", "imdb_num_votes", "critics_score")])

# There is very low correlation between date related variables and audience score
summarise(na.omit(movies), thtr_rel_year=cor(audience_score, thtr_rel_year),
          thtr_rel_month = cor(audience_score, thtr_rel_month),
          thtr_rel_day = cor(audience_score, thtr_rel_day),
          dvd_rel_year = cor(audience_score, dvd_rel_year),
          dvd_rel_month = cor(audience_score, dvd_rel_month),
          dvd_rel_day = cor(audience_score, dvd_rel_day)
          )

## Part 4: Modeling
# From EDA, variables that seemed to have higher relationship were:
# -title_type
# -genre
# -mpaa_rating
# -critics_rating
# -top200_box
# -best_pic_win
# -best_pic_nom
# -imdb_rating
# -critics_score

# The criteria to include variables for the final model
# will be based using a backwards model selection approach, excluding
# a variable if its p-value is large compared to the rest of
# explanatory variables and also checking if the adjusted R squared increases.

# First fit
mlr1 <- with(movies, lm(audience_score ~ title_type + genre + mpaa_rating +
                                critics_rating + top200_box + best_pic_win +
                                best_pic_nom + imdb_rating + critics_score))
summary(mlr1)
summary(mlr1)$adj.r.squared
# Second fit: critics_score removed
mlr2 <- with(movies, lm(audience_score ~ title_type + genre + mpaa_rating +
                                critics_rating + top200_box + best_pic_win +
                                best_pic_nom + imdb_rating))
summary(mlr2)
summary(mlr2)$adj.r.squared

# Third fit: top200_box removed
mlr3 <- with(movies, lm(audience_score ~ title_type + genre + mpaa_rating +
                                critics_rating + best_pic_win + imdb_rating +
                                best_pic_nom))
summary(mlr3)
summary(mlr3)$adj.r.squared

# Fourth fit: title_type removed
mlr4 <- with(movies, lm(audience_score ~  genre + critics_rating + mpaa_rating +
                                  best_pic_win + imdb_rating + best_pic_nom
                                ))
summary(mlr4)
summary(mlr4)$adj.r.squared

# Fifth fit: mpaa_rating removed
mlr5 <- with(movies, lm(audience_score ~  genre + critics_rating +
                                + imdb_rating + best_pic_win + best_pic_nom))
summary(mlr5)
summary(mlr5)$adj.r.squared

# Sixth fit: best_pic_win removed
mlr6 <- with(movies, lm(audience_score ~ genre + critics_rating +
                                + imdb_rating + best_pic_nom))
summary(mlr6)
summary(mlr6)$adj.r.squared

# Seventh fit: best_pic_nom removed
mlr7 <- with(movies, lm(audience_score ~ genre + critics_rating + imdb_rating))
summary(mlr7)
summary(mlr7)$adj.r.squared

# By removing best_pic_nom the adjusted R squared decreased, so we stick with the sixth model (mlr6).
# Final adjusted R squared: 0.7661102
# The sixth model explains 77% of the variability in audience score

# Diagnostics
# 1. Linearity: Explanatory variables and response variable follow a linear relationship
ggplot(data=movies,aes(x=imdb_rating, y=audience_score)) + geom_point() + geom_smooth(method="lm")

# 2. Nearly normal residuals centered at 0
# Residuals seem to be fairly centered at zero
mlr6 %>% ggplot(aes(x=.resid)) + geom_histogram(binwidth = 5)

# 3. Constant variability (predicted values vs residuals)
# Residuals locate constantly between 25 and -25, however, points
# do not behave in a completely random manner
mlr6 %>% ggplot(aes(x=.fitted,y=.resid)) + geom_point() +
    geom_hline(yintercept = 0, linetype="dashed")

# 4. Independent explanatory variables
# Model variables are: genre, critics_rating, imdb_rating and best_pic_nom

# imdb_rating seems to have a positive relationship with critics_rating
ggplot(movies, aes(x=imdb_rating, fill=critics_rating)) + geom_histogram()


## Part 5: Prediction
# Movie: Doctor Strange
# genre: Adventure, Action, Fantasy or "Action & Adventure"
# critics_rating: Fresh
# imdb_rating: 7.5
# best_pic_nom: "no"
# Real audience score: 86
# Sources: https://www.rottentomatoes.com/m/doctor_strange_2016
# https://en.wikipedia.org/wiki/Doctor_Strange_(2016_film)#Accolades
# https://www.imdb.com/title/tt1211837/
doc_strange <- data.frame(genre="Action & Adventure", critics_rating="Fresh", imdb_rating=7.5, best_pic_nom="no")

# The model predicted an audience score of 78.36
predict(mlr6, doc_strange)

# Prediction interval
# There is a 95% confidence that the movie Doctor Strange, which
# genre belongs to "Action & Adventure", with a "Fresh" critic rating, an imdb_rating of 7.5 and
# that wasn't nominated for a best picture Oscar price will have an audience
# score between 58.94 and 97.78
predict(mlr6, doc_strange, interval = "prediction", level=0.95)


## Part 6: Conclusion
# With these results I realized that some of the most significant
# significant attributes that would make a movie popular are its imdb rating,
# whether the movie was nominated for a best picture Oscar or not, its critics rating on the Rotten Tomatoes website
# and its genre. But I think that there are several limitation of the study
# done here because of the collection of data, as mentioned in Part 1, IMDB and Rotten Tomatoes
# collect data from many visitors on their website, so their opinions can be subjective or biased.
# For example, a movie might get a "Certified Fresh" rating when it might not deserve it.
# Again, these results are not generalizable because it could have dropped a more significant
# that wasn't in the model. For future research I would to test if the total box office
# depends on people rating.

