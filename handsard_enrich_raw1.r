######################################################
###  Call Center Data Enrichment
######################################################

# Setup

getwd()
setwd("/Users/ryan/Documents/call_center_instrumentation_analytics/hansard")

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)
library(stringr)
library(splitstackshape)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),httpauth=AUTH_BASIC)) # NOTE - the "httpauth=AUTH_BASIC" piece gets rid of the "Error: UNAUTHORIZED" message 

source("keys.r") ## KEYS has acutal username:password for each IBM service. 

df <- read.csv2("text_samples.csv")
head(df)


##### LET"S ORGANIZE OUR RAW TEXT

# Step 1
data <- read.csv2("text_samples.csv") # Import file (we scrubbed semicolons and commas)
names(data)[1]<-paste("transcript")
dim(data)
data$ID <- seq.int(nrow(data)) # add ID Column for index
data <- data[,c(2,1)] # put ID on left
head(data)

data$wordcount <- sapply(gregexpr("\\W+", data$transcript), length) + 1
# just for troubleshooting # write.table(data,file="temp.csv",sep=",",dec = " ",col.names = TRUE, row.names = FALSE)


# Step 2 - Segment Sentences into Chunks (imagine a 5 minnute audio stream chunked into 15 second intervals or chunked by conversation node)
# https://stackoverflow.com/questions/46260274/split-string-by-n-words-in-r
split_every <- function(x, n, pattern, collapse = pattern) {
  x_split <- strsplit(x, pattern, perl = TRUE)[[1]]
  out <- character(ceiling(length(x_split) / n))
  for (i in seq_along(out)) {
    entry <- x_split[seq((i - 1) * n + 1, i * n, by = 1)]
    out[i] <- paste0(entry[!is.na(entry)], collapse = collapse)}
  out
}

#test
# x <- c("one, two, three, four, five, six, seven, eight, nine, ten")
# split_every(x, 3, pattern = " ")
# split_every(paste(data$transcript[3]), 30, pattern = " ")


# Chunk Transcripts
chunksize <- 30 # how many words in each chunk
# test / sanity check simple loop to run through all rows of data to present line by line wordcount and chunk count
for (i in 1:nrow(data))
  {
    chunkcount <- ceiling(data$wordcount[i]/chunksize)
    print(paste("Index:",i," wordcount: ", data$wordcount[i]," chunkcount ", chunkcount))
  }


# Fancy Loop to create a new data structure DATA2 - that has the chunked data we're going to 
data2 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ID", "chunk", "transcript"))
for (i in 1:nrow(data))
{
  transcipt_temp <- split_every(paste(data$transcript[i]), chunksize, pattern = " ")
  chunkcount <- ceiling(data$wordcount[i]/chunksize)
  for (j in 1:chunkcount){
    df_temp <- data.frame(i,j,transcipt_temp[j])
    data2 <- rbind(data2,df_temp)
  }
  print(paste("Index:",i," wordcount: ", data$wordcount[i]," chunkcount ", chunkcount))
}

data2 <- setNames(data2,c("ID", "chunk", "transcript"))

# clean up
dim(data2)
data3 <- na.omit(data2) # get rid of any NA's (not sure why they show up - but easier to blast)
dim(data3)

### STEP 2 TONE

# LETs add some new columns
data3[c("anger","disgust","fear","joy","sadness","analytical","confident","tentative","openess",
        "conscientiousness","extraversion","agreeableness","emotional_range")] <- NA

base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19"
username_password_TON
#getURL("https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19&text=hello",userpwd = username_password_TON ) 
#getURL("https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19&text=A%20word%20is%20dead%20when%20it%20is%20said,%20some%20say.%20Emily%20Dickinson",userpwd = username_password_TON ) 

### Function to process output from API and table
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data <- data[-c(1), ] # remove dud first row
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),2)
  setnames(data,c("trait","signal"))
  return(data)
}

### POST - Basic Test
process_data_to_tone <- function(text)
{
  response <- POST(url=base_url_TON,
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  abc <- tidyResponse(response_text)
  return(abc)
}

query <- URLencode("I really love to eat stew in the winter because it is delicious - of this I am sure!")
analysis <- process_data_to_tone(query)
analysis

#                 trait signal
# 1              anger   0.13
# 2            disgust   0.11
# 3               fear   0.08
# 4                joy   0.44
# 5            sadness   0.13
# 6         analytical   0.31
# 7          confident   0.57
# 8          tentative   0.00
# 9           openness   0.12
# 10 conscientiousness   0.35
# 11      extraversion   0.87
# 12     agreeableness   0.80
# 13       neuroticism   0.88


### HERE WE HIT TONE API MANY TIMES TO GET DATA AND APPEND TO DATA SET
for (i in 1:nrow(data3))
  {
    print(paste("index:",i)) 
    print(paste(data3$transcript[i]))
    analysis <- process_data_to_tone(URLencode(paste(data3$transcript[i])))
    analysis
    data3[i,4:16] <- as.data.frame(t(analysis$signal)) # transpose the 13 signals in column to row i
    print(data3[i,])
}


write.table(data3,file="hansard_tone_enriched.csv",sep=",",col.names = TRUE, row.names = FALSE)
#data3 <- read.csv2("hansard_tone_enriched.csv", dec=".", sep=",") # if R crashses and you dont want to re-run API enrich - do this
#summary(data3)
#sum(data3$anger)
## Step X - Visualize Basics

### STEP 3 ROUGH PLOTS FOR NOW - MORE LATER
dim(data)
#par(mfrow=c(2, 2))

# WORDCOUNT & SUMMARY
summary(data$wordcount)
hist(data$wordcount, breaks = 100, main = "Histogram: Word Count", col="orange")
hist(data3$chunk, breaks = 60, main = "Histogram: Chunk Count per Utterance", sub = paste("Chunk Size:",chunksize), col="lightblue")
boxplot(data3[,4:16], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   
boxplot(data3[,4:8], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   
boxplot(data3[,9:11], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow")   
boxplot(data3[,12:16], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   

# Scatterplot Matrices from the glus Package
library(gclus)
dta <- data3[4:16]
dta.r <- abs(cor(dta)) # get correlations
dta.r <- round(dta.r,3)

# Variables by Correlation
dta <- data3[c(4,8,9,10,11,15)] # get data
summary(dta)
#dta <- data3[4:16] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.r
dta.col <- dmat.color(dta.r) # get colors # reorder variables so those with highest correlation # are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,main="Variables Ordered and Colored by Correlation" )                                    




### STEP 4 - JOURNEY MAPPING
# Ok - let's grab the longest - multi-chunk utterances and work with them 
data5 <- data[order(-data$wordcount),] 
data5 <- head(data5,10)
summary(data5)

data5$ID # these are our TEN (N) utterances in focus to test - they are long

data6 <- subset(data3,ID %in% data5$ID) # From Data 3, pull out only data that's in our top 10/n list
data6 <- data6[order(data6$ID,data6$chunk),]  # sequence our ID's and our chunks
summary(data)

# WORDCOUNT & SUMMARY
par(mfrow=c(1, 1))
boxplot(data6[,4:16], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   
boxplot(data6[,4:8], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   
boxplot(data6[,9:11], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow")   
boxplot(data6[,12:16], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   

# Let's pick SADNESS (8) ;
  # for (i in 1:9)
  # {
  #   print(paste("Script:",i,"ID: ",colnames(data5[i])))
  #   data_temp <- subset(data6,ID %in% data5$ID[i]) # each time we loop we subset ONLY the 1 of 10 IDs in focus
  #   plot(data_temp$sadness, type="o", main = paste("SADNESS \n Script:",i,"ID: ",data5$ID[i]))
  # }

  convo_plot <- function(){  
  for (i in 1:9)
    {
      #print(paste("Script:",i,"ID: ",toupper(colnames(data_temp[k]))))
      data_temp <- subset(data6,ID %in% data5$ID[i]) # each time we loop we subset ONLY the 1 of 10 IDs in focus
      plot(data_temp[,k], type="o", main = paste(toupper(colnames(data_temp[k]))," \n Script:",i,"ID: ",data5$ID[i]))
    }
  }


  # ANGER (4) 
  k=4
  par(mfrow=c(3, 3), col="red")
  convo_plot()
  
  # SADNESS (8) 
  k=8 
  par(mfrow=c(3, 3), col="blue")
  convo_plot()
  
  # ANALYTICAL (9) 
  k=9 
  par(mfrow=c(3, 3), col="brown")
  convo_plot()
  
  k=14 # Let's pick EXTRAVERSION
  par(mfrow=c(3, 3), col="gold")
  convo_plot()
  
  k=15 # Let's pick AGREEABLENESS
  par(mfrow=c(3, 3), col="green")
  convo_plot()
  
  
  k=16 # Let's pick EMOTIONAL RANGE (16)
  par(mfrow=c(3, 3), col="orange")
  convo_plot()

