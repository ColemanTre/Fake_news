# Fake_news
a.	What is the overall purpose of this project?
To predict whether a news article is real or fake use statistical analysis.

b.	What do each file in your repository do?
TopicModeling.R is where I perform the sentiment analysis calculations.
Useful.R is where I perform the xgbTree modeling using the caret library.
FakeNewsDataCleaning.R is where the tf-idf is calculated and formatted for building a model.

c.	What methods did you use to clean the data or do feature engineering?
I used the caret library for modeling and I used the sentiment analysis to engineer several variables
that reflect the overall sentiment of each article.

d.	What methods did you use to generate predictions?
I used xgbTree and tried using ranger. Both are in the caret library.