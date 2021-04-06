### Fake News Workbook

library(tidyverse)
library()

#Combine some languages into same category
fakeNews <- fakeNews %>% 
  mutate(language = fct_collapse(language,
                                 english = c('english', 'middle_frisian')))

#Lump together other languages
fakeNews <- fakeNews %>% 
  mutate(language = fct_explicit_na(language, na_level = "Missing")) %>% 
  mutate(language = fct_lump(language, n=6))
fakeNews <- count(language) %>% 
  arrange(desc(n)) %>% 
  print(n=Inf)

#Calculate tf-df(t) for most common words not including stop words
#(term frequency = % of words in document of term t, document frequency = % of documents that contain term t)

##Create a set of stop words
sw <- bind_rows(get_stopwords(language="en"),
                get_stopwords(language='ru'),
                get_stopwords(language='es'),
                get_stopwords(language='de'),
                get_stopwords(language='fr'))

sw <- sw %>% 
  bind_rows(., data.frame(word=""))

#tidytext format
tidyNews <- fakeNews %>% 
  unnest_tokens(tbl=., output=word, input=text)

#Count of words in each article
news.wc <- tidyNews %>% 
  anti_join(sw) %>% 
  count(id, word, sort=T)