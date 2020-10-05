---
categories:
- ""
- ""
date: "2020-09-09"
description: Is mask acceptance high or low in the US? NYT Survey analysis
draft: false
image: pic07.jpg
keywords: ""
slug: blog 5
title: Mask acceptance
---

```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(rvest)    # scrape websites
library(purrr)  
library(lubridate) #to handle dates
library(scales)
```


# Returns of financial stocks

```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- read_csv(here::here("data","nyse.csv"))
```


```{r companies_per_sector}

# YOUR CODE GOES HERE
nyse %>% 
  group_by(sector) %>% 
  summarize(
    companies_per_sector = count(sector)
  ) %>% 
  ggplot(aes(y=reorder(sector, companies_per_sector), x=companies_per_sector)) +
  geom_col() +
  xlab("Amount of companies in sector") +
  ylab("Sector") +
  theme_economist()


```


```{r, tickers_from_wikipedia}

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers + SPY
tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF

```




```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}
# Notice the cache=TRUE argument in the chunk options. Because getting data is time consuming, # cache=TRUE means that once it downloads data, the chunk will not run again next time you knit your Rmd

myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2000-01-01",
         to   = "2020-08-31") %>%
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame
```


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```



```{r summarise_monthly_returns}

# YOUR CODE GOES HERE
L3Y_returns_monthly <- myStocks_returns_monthly %>% 
  filter(date >= as.Date("2017-01-01")) %>% 
  summarize(
    minimum = min(monthly_returns),
    maximum = max(monthly_returns),
    median = median(monthly_returns),
    mean = mean(monthly_returns),
    sdev = sd(monthly_returns)
  )
L3Y_returns_monthly
```


```{r density_monthly_returns}

# YOUR CODE GOES HERE
myStocks_returns_monthly %>% 
  filter(date >= as.Date("2017-01-01")) %>% 
  ggplot(aes(x=monthly_returns, color=symbol)) +
  geom_density() 
  
myStocks_returns_monthly %>% 
  filter(date >= as.Date("2017-01-01")) %>% 
  summarize(
    s_dev = sd(monthly_returns)
  ) %>% 
  arrange(desc(s_dev))
    

```

# What can you infer from this plot? Which stock is the riskiest? The least risky? 

From the plot we cannot identify anything for individual stocks, because the structure of the data is unreadable. However, if we calculate the standard deviation we can see that the DOW JONES INDUSTRIAL AVERAGE has the highest risk (from a financial perspective). The least risky stock/index is the S&P 500. This is due to the fact that the S&P 500 perfectly reflects the American market performance. 



```{r risk_return_plot}
# YOUR CODE GOES HERE
L3Y_returns_monthly %>% 
  ggplot(aes(x=mean, y=sdev)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = symbol)) +
  theme_minimal() +
  xlab("Mean monthly return") +
  ylab("Mean monthly standard deviation")


```

# Answer

Walgreens Boots Alliance is one example for a stock which has less return for the same risk level compared to the other stocks (e.g. Nike) plotted in this chart. You can also infer that the theoretical correlation between risk and return isn't perfectly reflected in the real world. 