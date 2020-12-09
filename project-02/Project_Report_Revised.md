---
title: "Data Visualization Mini-Project 2 R Notebook Revised"
author: "Greg Dills - gregorydills0248@floridapoly.edu"
date: "12/06/2020"
output:
  html_document:
    df_print: paged
---

## Loading Packages

```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(sf)
library(tmap)
library(plotly)
library(readxl)
library(leaflet)
library(ggrepel)
library(scales)
```

```{r}
getwd()
```

```{r}
fl_lakes <- read_sf("C:/Users/Greg_Dills/Desktop/School/Data_Visualization/final_project_repo/dataviz_final_project/data/Florida_Lakes/Florida_Lakes/Florida_Lakes.shp")


```


## Successfully read the lake shapefile

```{r}


head(fl_lakes)
```


## Dataset Summary

```{r}
summary(fl_lakes)
```


```{r}
fl_lakes %>% 
  filter(NAME == "Lake Okeechobee")
```


```{r}
# Generated summary data by county (Lake count and average perimeter)
fl_lakes_summary <- fl_lakes %>% 
  group_by(COUNTY) %>% 
summarize(Average_Perimeter = mean(PERIMETER), Total_Lakes = n())
head(fl_lakes_summary)
```

## Data Exploration


```{r}
#Reviewing the summary data, ensuring that it makes sense. Palm Beach does contain the largest lake (Lake Okeechobee), thus the average perimeter makes sense. However, I am not familiar with Monroe County and intend to explore further.

fl_lakes_summary %>% 
    arrange(desc(Average_Perimeter))
```



[Link to "The Cutoff" lake visual](https://geodata.dep.state.fl.us/datasets/97b765ff2b70400d8bcab23fbe2a5e88_0/data?geometry=-81.214%2C25.183%2C-80.567%2C25.400&where=NAME%20%3D%20%27The%20Cutoff%27)

```{r}
# Interestingly enough, Monroe county is where the Everglades reside and many of the "lakes" in this county, I would not have considered them to be classified as lakes. For example, "The Cutoff" appears to be more like a river/estuary system. A graphical representation can be found in the link above. After reading more about the meta data, the observations can also be reservoirs.


fl_lakes %>% 
  filter(COUNTY == "MONROE")
```

## Interactive Bar Plot


```{r}
# Creating an interactive bar plot to explore which counties have the largest (on average) lakes.

bar_plot <- ggplot(fl_lakes_summary, aes(reorder(COUNTY, -Average_Perimeter), Average_Perimeter)) +
  geom_col(aes(fill = Average_Perimeter)) +
  scale_fill_gradient(low = "grey", 
                       high = "blue") +
  theme_minimal() + 
  theme(plot.title = element_text(size=12)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Distribution of Average Florida Lake Size by County") 
  geom_label(aes(label = COUNTY))

ggplotly(bar_plot)

```

## Plotting the lake shapefiles

```{r}
# Using the sf package to plot all lake shapes within the data file. By not having any filters, we can clearly see these lakes are in Florida.

plot(st_geometry(fl_lakes))
```
## Filtering for just Polk County, FL

```{r}
# I would like to dig deeper and view only lakes in Polk County, FL. This is great, but without a point of reference it is difficult to know where these lakes are located.

polk_lakes <- fl_lakes %>%  filter(COUNTY == "POLK")
tm_shape(polk_lakes) + tm_fill("lightblue") + tm_borders()

```


```{r}
# Using leaflet, I thought it would be interesting to zero in on the lake in Winter Haven, which I live on. In the future I want to expand on using leaflet and shapefiles together for topographical projections.
polk_basemap <- leaflet(polk_lakes) %>% setView(lng = -81.744296, lat = 28.023622, zoom = 14)
polk_basemap %>% addTiles()
```


## Comparing Lake size to Population


```{r}
# Having the lake data by itself is great for projections, however, it would be interesting to explore whether or not there is a relationship between the average lake perimeter (in meters) vs the estimated population for each Florida County. To do this I found data here (https://www.bebr.ufl.edu/population/data) and even though I did not tidy the data in R (I need to practice data wrangling techniques again!), I was able to quickly get the data into a manageable format in Excel.



library(readxl)
fl_county_population <- read_excel("C:/Users/Greg_Dills/Desktop/School/Data_Visualization/final_project_repo/dataviz_final_project/data/estimates_2020_table1_tidy.xlsx")
```

```{r}
# In order to better understand the relation between the average size of lakes and the population in Florida counties.  
head(fl_county_population) 
```


```{r}
# Successfully joined the two tables.

fl_lakes_pop <- fl_lakes_summary %>%  left_join(fl_county_population, by="COUNTY")
head(fl_lakes_pop)
```

```{r}
fl_lakes_pop %>% 
          filter(COUNTY == "POLK")
```

## Analyzing Estimated Florida Population and Lake Size Correlation


```{r message=FALSE, warning=FALSE}

 Very interesting (but not surprising) results from comparing the relationship between the average lake perimeter and the anticipated 2020 population by county. By annotating the clear outliers (above 10,000 meters average lake perimeter), we can see that early on in the model as a County's population is lower, there is an increase in the average size of the lake's perimeter. Aside from other variables this makes sense, the larger the average lake size in a given county, likely there will be less land mass for people to live on. There are exceptions in Florida however. For example, Lake Okeechobee is assigned to Palm Beach County (Even though it neighbors many counties) and Palm Beach has a relatively high estimated population count.


county_lm_model <- ggplot(fl_lakes_pop, mapping = aes(x = `2020_Est_Pop`, y = Average_Perimeter, label = COUNTY)) +
         geom_point() +
  geom_label_repel(aes(label=ifelse(Average_Perimeter > 10000, COUNTY, ifelse(`2020_Est_Pop` > 2000000, COUNTY,ifelse(`2020_Est_Pop` == 715090, COUNTY,""))))) +
  labs(title = "Average Florida Lake Perimeter vs Estimated 2020 Population by County",
       x = "2020 Population",
       y = "Lake Perimeter (Meters)",
       caption = "Source: BEBR & FDEP ") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_smooth() +
  theme(plot.title = element_text(size=10)) +
  theme_minimal()
  
county_lm_model

```


```{r}
county_model <- lm(`2020_Est_Pop` ~ Average_Perimeter, data = fl_lakes_pop)
summary(county_model)
```



```{r message=FALSE, warning=FALSE}

 I figured it would be interesting to analyze total lakes vs population. Keeping the same annotations as the prior plot, we can see where many of the counties shifted, because they simply do not have nearly as many lakes and in the case of Palm Beach (Lake Okeechobee), there are actually very few lakes relative to other counties. Notice how the linear model's peak regression line is further along the x-axis, this is showing that even in counties which the population is higher there are plenty of lakes. Polk county is a good example with over 300 lakes and double the average county Population. Maybe this indicates that people prefer lakes and potentially living on them?

county_lm_model_total_lakes <- ggplot(fl_lakes_pop, mapping = aes(x = `2020_Est_Pop`, y = Total_Lakes, label = COUNTY)) +
         geom_point() +
  geom_label_repel(aes(label=ifelse(Average_Perimeter > 10000, COUNTY, ifelse(`2020_Est_Pop` > 2000000, COUNTY,ifelse(`2020_Est_Pop` == 715090, COUNTY,""))))) +
  labs(title = "Total Number of Lakes vs Estimated 2020 Population by County",
       x = "2020 Population",
       y = "Total Number of Lakes",
       caption = "Source: BEBR & FDEP ") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_smooth() +
  theme(plot.title = element_text(size=10)) +
  theme_minimal()
  
county_lm_model_total_lakes

```

