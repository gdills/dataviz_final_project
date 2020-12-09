---
title: "Visualizing Text - Rate my Professor Data"
author: "Greg Dills"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: false
      theme: lumen
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Packages used

```{r message=FALSE, warning=FALSE}
library(rmarkdown)
library(tidyverse)
library(tidytext)
library(wordcloud)
```

***

## Reading and better understanding the data {.tabset .tabset-fade .tabset-pills}

### Getting the data from my Github Repo

Reading in rate my professor data from Mendeley. This dataset is shared by Dr. Jibo HE, founder of the USEE Eye Tracking Inc.  and professor of Tsinghua University."https://data.mendeley.com/datasets/fvtfjyvw7d/2"

```{r}

"He, Jibo (2020), “Big Data Set from RateMyProfessor.com for Professors' Teaching Evaluation”, Mendeley Data, V2, doi: 10.17632/fvtfjyvw7d.2"

professor <- read_csv("https://raw.githubusercontent.com/gdills/dataviz_final_project/main/data/RateMyProfessor_Sample%20data.csv")

head(professor)
```
### Setting the dataframe up to use text analysis


A decent amount of observations to work with.

```{r}
nrow(professor)
```



```{r}
professor_df <- tibble(line = 1:20000, text = professor$`comments`)
```



```{r}
tidy_professor <- professor_df %>%
                  unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,str_detect(word, "[a-z]")) %>% 
                  anti_join(stop_words) %>% 
                  count(word, sort = TRUE)
```
Many of these words don't have much "feeling" or meaning to them when it comes to understanding how students feel about their professors.

```{r}
head(tidy_professor)
```

***

## Wordclouds {.tabset .tabset-fade .tabset-pills}

### First word cloud

This word cloud is great for exploratory analysis with the words and shows that many of these words should be filtered out. These words don't add enough context to the analysis. Words such as class, teacher, professor etc...

```{r message=TRUE, warning=FALSE}

wordcloud(words = tidy_professor$word, freq = tidy_professor$n, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
```
<img src="https://github.com/gdills/dataviz_final_project/raw/main/figures/word_cloud_1.png" width="80%" style="display: block; margin: auto;" />

### Second word cloud

Here, I am removing some of the stopwords that I deemed to not add value to the word cloud.

```{r}
professor_stopwords <- c("class" , "professor" , "teacher" , "time" , "don" , "lectures" , "lot")

tidy_professor2 <- professor_df %>%
                  unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,str_detect(word, "[a-z]")) %>% 
    filter(!word %in% professor_stopwords,str_detect(word, "[a-z]")) %>% 
                  anti_join(stop_words) %>% 
                  count(word, sort = TRUE)

```

```{r warning=FALSE}
wordcloud(words = tidy_professor2$word, freq = tidy_professor2$n, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
```
<img src="https://github.com/gdills/dataviz_final_project/raw/main/figures/word_cloud_2.png" width="80%" style="display: block; margin: auto;" />

## Sentiment Analysis

Separating individual words from the dataset.

```{r}
library(tidytext)
professor_words <- professor %>% 
  unnest_tokens(word, comments)
head(professor_words)
```


Removing stopwords

```{r}
professor_stopwords <- professor %>% 
    unnest_tokens(word, comments, token = "words") %>%
     filter(!word %in% stop_words$word,str_detect(word, "[a-z]")) 
```

Checking the word count for each individual word.

```{r}
professor_stopwords %>% 
  group_by(word) %>% 
  summarise(uses = n()) %>% 
  arrange(desc(uses))
```

Displaying the negative vs positive relationship by implementing the bing lexicon. Very cool to see the ability to "score" someone with positive vs negative attributes.

```{r}

professor_stopwords %>% 
  inner_join(get_sentiments("bing")) %>%
  count(professor_name, school_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  arrange(sentiment)

```

Performing a quick check on the "worst" professor, according to the bing lexicon. Sure enough, there are plenty of negative comments.

```{r}
bad_professor <- professor %>% 
    select(comments, professor_name) %>% 
  filter(professor_name == "Daniel  Vona")

head(bad_professor)
```

Seeing which schools have the most comments. This will allow me to pick a few schools and plot the results

```{r}
professor %>% 
  group_by(school_name) %>% 
  summarize(number_of_professor_reviews = n()) %>% 
  arrange(desc(number_of_professor_reviews))
```


## Sentiment Analysis plot

```{r}
professor_stopwords %>%
  inner_join(get_sentiments("bing")) %>%
  count(professor_name, school_name,  sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  filter(school_name == "University of Alabama") %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(professor_name, sentiment), y = sentiment), stat = "identity") + 
  coord_flip() + 
  labs(x = "", title = "Sentiment Analysis of Professor Reviews", 
       subtitle = "Professors from the University of Alabama")
```
<img src="https://github.com/gdills/dataviz_final_project/raw/main/figures/sentiment_analysis.png" width="80%" style="display: block; margin: auto;" />

Here I am testing to see if the general consensus for John Baker is positive. Overall, yes his reviews are very positive.
```{r}


uab_good_professor <- professor %>% 
  select(comments, professor_name) %>% 
  filter(professor_name == "John  Baker")

head(uab_good_professor)
```

Wow, after looking at the comments, I can see how and why this professor would score badly in the sentiment analysis.

```{r}


uab_bad_professor <- professor %>% 
  select(comments, professor_name) %>% 
  filter(professor_name == "Jamie  Mills")

head(uab_bad_professor)
```


## Conclusion and Next Steps

I would like to explore this data set further. Really cool stuff, being able to analyze individual words from thousands of comments! Who knows, maybe this project could develop into a Shiny app or tool which allows prospective students to search a schools list of professors and dig down quickly into what other students have said about them? 
