---
title: "Data Visualization Mini-Project 2 Report"
author: "Greg Dills - gregorydills0248@floridapoly.edu"
date: "11/19/2020"
output:
  html_document:
    df_print: paged
---

Originally, I knew I would be generating spatial visualizations with the lake shape file data. Fortunately, I did not need to clean the data in order to create spatial visualizations. I quickly determined that the lake spatial data alone would be very difficult to work with for anything other than distributions by county. There is simply not enough features in the data set to derive much insight. I overcame this by searching for relevant data at the county level. I ultimately stumbled upon population data for each Florida County. By summarizing the average perimeter for each County and joining the two data sets (lake shape file and population), I could then create a scatter plot to see whether there was any relationship between the average lake size per county and the estimated population.

Once I got started analyzing this data, I realized I would need additional data points in order to ask a question about the data. This lead me to finding population data which helped in creating a linear model. Many of the ggplot2 functions are becoming easier, however I took longer than I would have liked on understanding and figuring out annotations. 

I need to work on my wrangling and cleaning skills. The additional population data was far from tidy and for the sake of quickly being able to work with the data, I made it tidy in Excel in a matter of minutes. Trying to replicate the same in R proved to be difficult and I need to practice wrangling in R and revisit the Data Wrangling course materials more often.

Ultimately, my goals for this project were to continue to simplify my approach, keeping everything organized. I did have some issues with the `saveWidget` function and could not figure it out. Searched for the issue I was having and not successful. The library (`htmlwidgets`) was installed but could not be found.


