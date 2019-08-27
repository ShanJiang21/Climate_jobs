
## Get the working path
ls()
getwd()
library(stringr)
library(tidyverse)

## set the working directory 
setwd("C:/Users/sj2921/Downloads/Climate_jobs")

## Read in Data 
library(readxl)
library(tm)
library(janeaustenr)
library(tidytext)

job_clean <- read_excel("C:/Users/sj2921/Downloads/Climate_jobs/job_clean.xlsx")
head(job_clean$PosDesc)

## Pre-process the data 

#### 1. Clean the text and create corpus:

library(tm)
library(tau)

Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  # Remove everything that is not a number or letter (may want to keep more 
  # stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  ## Replace all carriage return symbols
  temp <- str_replace_all(temp, "[\r\n]" , "")
  ## remove stop words
  temp <- removeWords(temp, stopwords('en'))
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}


## Create the Date variable by iteration 

library(tm)
library(tidyverse)

job.19 <- job_clean %>% 
  separate(DatePosted, c("year", "month", "day"), "-") %>% 
  filter(year == 2019)

head(job.19$PosDesc)
sapply(job.19$PosDesc, Clean_String)

jobdes_19 =  Clean_String(job.19$PosDesc[1])

# Isolate text from texts:
tomatch <- c("\\<[Cc]limate change\\>", "\\<[Cc]limatic change\\>", 
             "climate-change", "global warming", "global-warming",
             "greenhouse effect", "atmospheric warming",
             "climate crisis"
           )

matches <- unique (grep(paste(tomatch,collapse="|"), 
                        job.19$PosDesc, value=TRUE))

grepl(patterns, job.19$PosDesc) 

count.19 <- grep(pattern, )
length(count.19)

count.19 <- grep("", job.19$PosDesc)
length(count.19)                 
                 

count.18 <- grep("\\<[Cc]limate [Cc]hange\\>",job.19$PosDesc )
length(count.18)

### Iteration for all across years 

gdata <- function(i){job.i = job_clean %>% 
  separate(DatePosted, c("year", "month", "day"), "-") %>% 
  filter(year == i)}

count.19 <- grep("[Gg]lobal",job.19$PosDesc )
count.19




job.18 <- gdata(2018)
job.17 <- gdata(2017)
job.16 <- gdata(2016)
job.15 <- gdata(2015)
job.14 <- gdata(2014)
job.13 <- gdata(2013)
job.12 <- gdata(2012)
job.11 <- gdata(2011)
job.10 <- gdata(2010)
job.09 <- gdata(2009)
job.08 <- gdata(2008)
job.07 <- gdata(2007)
job.06 <- gdata(2006)
job.05 <- gdata(2005)



count.18 <- grep("[Cc]limate change",job.18$PosDesc, value = T)
length(count.18)

count.17 <- grep("[Cc]limate change",job.17$PosDesc, value = T)
length(count.17)

count.16 <- grep("[Cc]limate change",job.16$PosDesc, value = T)
length(count.16)

count.15 <- grep("[Cc]limate change",job.15$PosDesc)
length(count.15)

count.14 <- grep(".*[Cc]limate change",job.14$PosDesc, value = T)
length(count.14)

count.13 <- grep(".*[Cc]limate change",job.13$PosDesc, value = T)
length(count.13)

count.12 <- grep(".*[Cc]limate change",job.12$PosDesc, value = T)
length(count.12)

count.11<- grep(".*[Cc]limate change",job.11$PosDesc, value = T)
length(count.11)

count.09 <- grep(".*[Cc]limate change",job.09$PosDesc, value = T)
length(count.09)

count.08 <- grep(".*[Cc]limate change",job.08$PosDesc, value = T)
length(count.08)

count.07 <- grep(".*[Cc]limate change",job.07$PosDesc, value = T)
length(count.07)




return(job.i)
}

for (j in c(3:19))
{
  job_j = gdata(j)
}  
str(job_j)

count.[i] <- grep(".*[Cc]limate change",job.[i]$PosDesc, value = T)
length(count.[i])