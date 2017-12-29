######################################################
###  Call Center Data Enrichment
######################################################

# Setup

getwd()
setwd("/Users/ryan/Documents/call_center_instrumentation_analytics/spark_spot")

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

##### LET"S ORGANIZE OUR RAW TEXT

# Step 1
data <- read.csv2("spark_spot.csv") # Import file (we scrubbed semicolons and commas)
names(data)[1]<-paste("transcript")
dim(data)
data$ID <- seq.int(nrow(data)) # add ID Column for index
data <- data[,c(2,1)] # put ID on left
head(data)

data$wordcount <- sapply(gregexpr("\\W+", data$transcript), length) + 1
# just for troubleshooting # write.table(data,file="temp.csv",sep=",",dec = " ",col.names = TRUE, row.names = FALSE)
total_words <- sum(data$wordcount) # 1460 words
total_words
par(mfrow=c(1, 1))
hist(data$wordcount, breaks=50, main="Words per speech block", col="orange")

# step 1.5 - for www only - let's join up ALL the text into one string then chunk it later
transcript <- paste((data$transcript), collapse = " ")
transcript

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


chunksize <- 50 # how many words in each chunk
chunks <- ceiling(total_words/chunksize)
chunks # number of chunks we should expect

df <- split_every(transcript, chunksize, pattern = " ")
df <- as.data_frame(df)
names(df)[1]<-paste("transcript")
# split_every(paste(data$transcript[3]), 30, pattern = " ")

### STEP 2 TONE

df[c("anger","disgust","fear","joy","sadness","analytical","confident","tentative","openess",
        "conscientiousness","extraversion","agreeableness","emotional_range")] <- NA

base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19"
username_password_TON

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

# API test
query <- URLencode("I really love to eat stew in the winter because it is delicious - of this I am sure!")
analysis <- process_data_to_tone(query)
analysis

#i = 1
### HERE WE HIT TONE API MANY TIMES TO GET DATA AND APPEND TO DATA SET
for (i in 1:chunks)
  {
    print(paste("index:",i)) 
    print(paste(df$transcript[i]))
    analysis <- process_data_to_tone(URLencode(paste(df$transcript[i])))
    analysis
    df[i,2:14] <- as.data.frame(t(analysis$signal)) # transpose the 13 signals in column to row i
    print(data3[i,])
}


write.table(df,file="enriched.csv",sep=",",col.names = TRUE, row.names = FALSE)
data3 <- df
### STEP 3 ROUGH PLOTS FOR NOW - MORE LATER
dim(data3)
#par(mfrow=c(2, 2))

# WORDCOUNT & SUMMARY
boxplot(data3[,2:14], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   
boxplot(data3[,2:6], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   
boxplot(data3[,7:9 ], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow")   
boxplot(data3[,10:14], main = (paste("BoxPlot of Enriched Data \n",nrow(data3),"rows")),col="yellow",las=2)   

data3 <- data.frame(data3)
k <- 12
plot(data3$extraversion, type="o", ylim=c(0, 1),
     main = paste(toupper(colnames(data3[,k])),"\n Spark Talk"))

# manual colors
pigment <- NULL
pigment[1] <- "red"         # anger
pigment[2] <- "yellow4"     # disgust
pigment[3] <- "darkolivegreen"  # fear
pigment[4] <- "yellow"      #joy
pigment[5] <- "deepskyblue4" #sadness
pigment[6] <- "darkorange2"   #analytical
pigment[7] <- "darkblue"    #confident
pigment[8] <- "bisque"    #tentative
pigment[9] <- "chartreuse" #openness
pigment[10] <- "cornflowerblue" #conscientiousness
pigment[11] <- "deeppink"  #extraversion
pigment[12] <- "paleturquoise3" #agreeableness
pigment[13] <- "black" #emotional range


par(mfrow=c(3, 4))
for (k in 2:13)
  {
    par(col=pigment[k-1])
    plot(data3[,k], type="o", ylim=c(0, 1),
    main = paste(toupper(colnames(data3[k])),"\n Spark Talk"))
  }
 
par(mfrow=c(1, 1))
k=14
par(col=pigment[k-1])
plot(data3[,k], type="o", ylim=c(0, 1),
     main = paste(toupper(colnames(data3[k])),"\n Spark Talk"))

