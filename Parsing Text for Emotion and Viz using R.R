### Title: Parsing Text for Emotion Terms: Analysis & Visualization Using R
### Author: Mesfin Gebeyaw, Principal Research Analyst at Capella University 
### https://datascienceplus.com/parsing-text-for-emotion-terms-analysis-visualization-using-r/

## The R code snippet to retrieve the letters was obtained from Michel Toth's post.
library(pdftools)      
library(rvest)       
library(XML)
# Getting & Reading in HTML Letters
urls_77_97 <- paste('http://www.berkshirehathaway.com/letters/', seq(1977, 1997), '.html', sep='')
html_urls <- c(urls_77_97,
               'http://www.berkshirehathaway.com/letters/1998htm.html',
               'http://www.berkshirehathaway.com/letters/1999htm.html',
               'http://www.berkshirehathaway.com/2000ar/2000letter.html',
               'http://www.berkshirehathaway.com/2001ar/2001letter.html')

letters_html <- lapply(html_urls, function(x) read_html(x) %>% html_text())
# Getting & Reading in PDF Letters
urls_03_16 <- paste('http://www.berkshirehathaway.com/letters/', seq(2003, 2016), 'ltr.pdf', sep = '')
pdf_urls <- data.frame('year' = seq(2002, 2016),
                       'link' = c('http://www.berkshirehathaway.com/letters/2002pdf.pdf', urls_03_16))
download_pdfs <- function(x) {
  myfile = paste0(x['year'], '.pdf')
  download.file(url = x['link'], destfile = myfile, mode = 'wb')
  return(myfile)
}
pdfs <- apply(pdf_urls, 1, download_pdfs)
letters_pdf <- lapply(pdfs, function(x) pdf_text(x) %>% paste(collapse=" "))
tmp <- lapply(pdfs, function(x) if(file.exists(x)) file.remove(x)) 
# Combine letters in a data frame
letters <- do.call(rbind, Map(data.frame, year=seq(1977, 2016), text=c(letters_html, letters_pdf)))
letters$text <- as.character(letters$text)

# Load additional required packages

require(tidyverse)
require(tidytext)
require(RColorBrewer)
require(gplots)
theme_set(theme_bw(12))


total_words_count <- letters %>%
  unnest_tokens(word, text) %>%  
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%                        
  group_by(year) %>%
  summarize(total= n()) %>%
  ungroup()

emotion_words_count <- letters %>% 
  unnest_tokens(word, text) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA")) %>%
  group_by(year) %>%
  summarize(emotions= n()) %>%
  ungroup()

emotions_to_total_words <- total_words_count %>%
  left_join(emotion_words_count, by="year") %>%
  mutate(percent_emotions=round((emotions/total)*100,1))

ggplot(emotions_to_total_words, aes(x=year, y=percent_emotions)) +
  geom_line(size=1) +
  scale_y_continuous(limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  xlab("Year") + 
  ylab("Emotion terms / total words (%)") + theme(legend.position="none") +
  ggtitle("Proportion of emotion words usage \n in Mr. Buffett's annual shareholder letters")

### pull emotion words and aggregate by year and emotion terms
emotions <- letters %>% 
  unnest_tokens(word, text) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive")) %>%
  group_by(year, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  ungroup()

### need to convert the data structure to a wide format
emo_box = emotions %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()

### color scheme for the box plots (This step is optional)
cols  <- colorRampPalette(brewer.pal(7, "Set3"), alpha=TRUE)(8)
boxplot2(emo_box[,c(2:9)], col=cols, lty=1, shrink=0.8, textcolor="red",        xlab="Emotion Terms", ylab="Emotion words count (%)", main="Distribution of emotion words count in annual shareholder letters (1978 - 2016")


## yearly line chart
ggplot(emotions, aes(x=year, y=percent, color=sentiment, group=sentiment)) +
  geom_line(size=1) +
  geom_point(size=0.5) +
  xlab("Year") +
  ylab("Emotion words count (%)") +
  ggtitle("Emotion words expressed in Mr. Buffett's \n annual shareholder letters")


### calculate overall averages and standard deviations for each emotion term
overall_mean_sd <- emotions %>%
  group_by(sentiment) %>%
  summarize(overall_mean=mean(percent), sd=sd(percent))
### draw a bar graph with error bars
ggplot(overall_mean_sd, aes(x = reorder(sentiment, -overall_mean), y=overall_mean)) +
  geom_bar(stat="identity", fill="darkgreen", alpha=0.7) + 
  geom_errorbar(aes(ymin=overall_mean-sd, ymax=overall_mean+sd), width=0.2,position=position_dodge(.9)) +
  xlab("Emotion Terms") +
  ylab("Emotion words count (%)") +
  ggtitle("Emotion words expressed in Mr. Buffett's \n annual shareholder letters (1977 – 2016)") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip( )


## Hi / Low plots compared to the 40-years average
emotions_diff <- emotions  %>%
  left_join(overall_mean_sd, by="sentiment") %>%
  mutate(difference=percent-overall_mean)

ggplot(emotions_diff, aes(x=year, y=difference, colour=difference>0)) +
  geom_segment(aes(x=year, xend=year, y=0, yend=difference),
               size=1.1, alpha=0.8) +
  geom_point(size=1.0) +
  xlab("Emotion Terms") +
  ylab("Net emotion words count (%)") +
  ggtitle("Emotion words expressed in Mr. Buffett's \n annual shareholder letters (1977 - 2016)") + 
  theme(legend.position="none") +
  facet_wrap(~sentiment, ncol=4)


