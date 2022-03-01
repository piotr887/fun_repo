# http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html

library(rvest)
library(xml2)
library(tidyverse)

load("C:\\Users\\Piotrek\\Desktop\\R Shiny\\Time Series Gielda LSTM_ARIMA\\dzienne_zmiany.Rda")

notowania <- read_html('https://strefainwestorow.pl/notowania/spolki/')

dzienne <- notowania %>% html_nodes("table") %>% .[1] %>% html_table(fill = TRUE)

dzienne <- data.frame(dzienne[[1]])
colnames(dzienne) <- c("Nazwa","Symbol"   ,  "Obrót"   ,   "Cena"     ,  "Zmiana.pkt" ,"Zmiana.Proc"  , "Czas") 

dzienne$Zmiana.Proc <- as.double(gsub("%","", dzienne$Zmiana.Proc))

dzienne <- dzienne %>% arrange(desc(Zmiana.Proc)) %>% filter( nchar(Czas) <= 5) %>%  select(-Czas)

dzienne$report_date <- Sys.Date()

tbls <- bind_rows(dzienne, tbls)

save(tbls, file = "C:\\Users\\Piotrek\\Desktop\\R Shiny\\Time Series Gielda LSTM_ARIMA\\dzienne_zmiany.Rda")

#spolki ze zmiana dwa dni > 10 p.p.
tbls %>% filter(Zmiana.Proc > 10) %>% group_by(Nazwa) %>% summarise(ile = n()) %>% filter(ile > 1)


df <- as.data.frame(iris$Sepal.Width)

df <- df %>% rename(var = "iris$Sepal.Width")

plot_hist <- function(tbl, ile) {
  
    d <- as.data.frame(NULL)

    for ( i in 1:ile) {
      v <- sample_n(df, size = 1)
      d <- rbind.data.frame(d, v)
  
    }
    
    hist(d$var, main =  "Histogram Bootstrap")
    return(d)
}

plot_hist(df, 1000)

