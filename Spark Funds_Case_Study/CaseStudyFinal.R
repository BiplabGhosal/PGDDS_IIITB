
library(tidyr)
library(plyr)
library(dplyr)

######### Checkpoint 1: Data Cleaning 1 ###########
###################################################

#Import companies.txt into companies data frame.
companies <- read.delim('companies.txt',header =TRUE,sep = "\t",na.strings = c("","NA"),stringsAsFactors = F)

#Import rounds2.csv into rounds2 dataframe.
rounds2<- read.csv('rounds2.csv',header =TRUE,na.strings = c("","NA"),stringsAsFactors = F)


#####How many unique companies are present in rounds2?#####

#Calculating the unique companies by Converting company_permalink of round2 
#to lower case for uniformity of data

length(unique(tolower(rounds2$company_permalink))) #66368

#####How many unique companies are present in companies?#####

#Calculating the unique companies by Converting permalink(which is the unique id for company) of companies 
#to lower case for uniformity of data

length(unique(tolower(companies$permalink))) #66368

#####In the companies data frame, which column can be used as the unique key for each company? 
#Write the name of the column.#####

#permalink can be used as an unique key for each company, this column contains distinct records in all the rows.

#####Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N #####

#Checking whether any unique companies of round2 are present in companies or not.
sum(ifelse(unique(tolower(rounds2$company_permalink)) != unique(tolower(companies$permalink)), 1, 0)) 
# It yields zero (0) hence round2 has NO companies which are not present in companies

#####Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. #####
#####Name the merged frame master_frame. How many observations are present in master_frame? #####

rounds2$company_permalink <- tolower(rounds2$company_permalink) #converting to lower case for uniformity
companies$permalink <- tolower(companies$permalink) #converting to lower case for uniformity

#Merging both rounds2 and companies
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

#Calculating the number of rows in master_frame. It has 114949 records.
nrow(master_frame) 


###### Checkpoint 2: Funding Type Analysis ########
###################################################

#Counting the number of NA values in raised_amount_usd column in dataframe master_frame
count <- sum(is.na(master_frame$raised_amount_usd))
#So there are 19990 NA values in the column, which corresponds to 17% of the total data present.
#Since the percent of NA is significant for USD raised, we will be replacing NA values with 0 to increase the dataset to anallyze further. 



#Impute raised_amount_usd by 0.
impute <- function(x) replace(x, is.na(x), 0)
master_frame$raised_amount_usd <- sapply(master_frame$raised_amount_usd,FUN = impute)

#Average funding amount of different investment type.
group <- group_by(master_frame,funding_round_type)

#Create summary dataframe for all funding type.
summary <- summarise(group,average = mean(raised_amount_usd), count = n()) 


#Finding summary for venture type.
summary %>%
  subset(funding_round_type=='venture')

#Average funding amount of venture type : $10634054
#Number of investments in venture type : 55494

#Finding summary for angel type.
summary %>%
  subset(funding_round_type=='angel')

#Average funding amount of angel type : $764564.3
#Number of investments in angel type : 6094

#Finding summary for seed type.
summary %>%
  subset(funding_round_type=='seed')

#Average funding amount of seed type : $556606.7
#Number of investments in seed type : 30524

#Finding summary for private_equity type.
summary %>%
  subset(funding_round_type=='private_equity')

#Average funding amount of private_equity type : $62111788
#Number of investments in private_equity type : 2285


#####Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?

#Considering investment between 5 to 15 million USD , Spark Funds should consider Venture type, since thats where maximum investors have invested, 
#and also the mean of USD invested in venture lies between the investment limit of Spark Funds.

  
######### Checkpoint 3: Country Analysis ##########
###################################################

## Create venture dataframe as most investments occured for venture type and checking for NA country code

venture_frame <- master_frame[which(tolower(master_frame$funding_round_type)=='venture'),] ##55494
na_country <- venture_frame[which(is.na(venture_frame$country_code)),] ##2875

#Since there are 2875 NA country code in venture_frame, which accounts to 5% of the data. 
#Hence we are ignoring the items having country code as NA


#Creating the top9 data frame based on total investment every country have received

top9 <- group_by(venture_frame,country_code) %>% 
  summarise(tot = sum(raised_amount_usd,na.rm=TRUE)) %>%
    arrange(desc(tot)) %>% na.omit() %>% head(9)

#Based on the pdf provided, English speaking countries which are present in top9 dataframe are :
# USA, GBR, IND, CAN  (In order of investment received)

#1. Top English-speaking country - USA (United States)
#2. Second English-speaking country - GBR (United Kingdom)
#3. Third English-speaking country - IND (India)


######### Checkpoint 4: Sector Analysis 1 ##########
####################################################

#Loading the mapping data and removing the "Blanks" from mapping dataframe

data <- read.csv('mapping.csv',header =TRUE,na.strings = c("","NA"),stringsAsFactors = F,check.names=FALSE)
mapping <- gather(data, sector, my_val, 2:10)
mapping <- mapping[!(mapping$my_val == 0 | mapping$sector == 'Blanks'),]
mapping$category_list  <- tolower(mapping$category_list)

#In mapping_long category_list col there are some records having '0' instead of 'na'. Example Energy Ma0gement should be Energy Management
#Hence replacing 0 with 'na'

mapping$category_list  <-gsub("^([^0]*)0(.+)$", "\\1na\\2",mapping$category_list)


##Extract the primary sector of each category list from the category_list column and merging with mapping dataframe

master_frame <- master_frame %>% mutate(primary_sector= tolower(gsub("^([^|]+)\\|*.*", "\\1", category_list)))
master_frame <- merge(master_frame,mapping[,c("category_list","sector")],by.x = 'primary_sector',by.y='category_list',all.x=T)

colnames(master_frame)[17] <- 'main_sector'

######### Checkpoint 5: Sector Analysis 2 ##########
####################################################

#Creating D1, D2 and D3 data frames  

D1 <- 
  master_frame %>%
  filter(country_code == 'USA' & funding_round_type == 'venture' & (raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)) %>%
  group_by(main_sector) %>%  mutate(count = n(), total_amount_invested = sum(raised_amount_usd))


D2 <- 
  master_frame %>%
  filter(country_code == 'GBR' & funding_round_type == 'venture' & (raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)) %>%
  group_by(main_sector) %>%  mutate(count = n(), total_amount_invested = sum(raised_amount_usd))


D3 <- 
  master_frame %>%
  filter(country_code == 'IND' & funding_round_type == 'venture' & (raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)) %>%
  group_by(main_sector) %>%  mutate(count = n(), total_amount_invested = sum(raised_amount_usd))


#1. Total number of investments (count)

#Country 1 - USA 
nrow(D1) #12150
#Country 2 - GBR
nrow(D2) #628
#Country 3 - IND
nrow(D3) #330

#2. Total amount of investment (USD)

#Country 1 - USD
sum(D1$raised_amount_usd)  #$108531347515
#Country 2 - GBR
sum(D2$raised_amount_usd)  #$5436843539
#Country 3 - IND
sum(D3$raised_amount_usd)  #$2976543602

#3. Top sector (based on count of investments)

D1 %>%
  group_by(main_sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Top Sector - USA - Others 

D2 %>%
  group_by(main_sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Top Sector - GBR - Others

D3 %>%
  group_by(main_sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Top Sector - IND - Others

#4. Second-best sector (based on count of investments)

#USA - Social, Finance, Analytics, Advertising
#GBR - Social, Finance, Analytics, Advertising
#IND - Social, Finance, Analytics, Advertising

#5. Third-best sector (based on count of investments)

#USA - Cleantech / Semiconductors
#GBR - Cleantech / Semiconductors
#IND - News, Search and Messaging

#6. Number of investments in the top sector

#USA - Others - 2950
#GBR - Others - 147
#IND - Others - 110

#7. Number of investments in second best sector

#USA - Social, Finance, Analytics, Advertising - 2714
#GBR - Social, Finance, Analytics, Advertising - 133
#IND - Social, Finance, Analytics, Advertising - 60

#8. Number of investments in third best sector

#USA - Cleantech / Semiconductors - 2350
#GBR - Cleantech / Semiconductors - 130
#IND - News, Search and Messaging - 52

#9. For the top sector count-wise (point 3), which company received the highest investment?

D1 %>%
  filter(main_sector== "Others") %>%
  group_by(company_permalink) %>%
  summarise(total = sum(raised_amount_usd, na.rm=TRUE)) %>%
  arrange(desc(total))

#USA - virtustream

D2 %>%
  filter(main_sector== "Others") %>%
  group_by(company_permalink) %>%
  summarise(total = sum(raised_amount_usd, na.rm=TRUE)) %>%
  arrange(desc(total))

#GBR - electric-cloud

D3 %>%
  filter(main_sector== "Others") %>%
  group_by(company_permalink) %>%
  summarise(total = sum(raised_amount_usd, na.rm=TRUE)) %>%
  arrange(desc(total))

#IND - firstcry-com

#10 . For the second-best sector count-wise (point 4), which company received the highest investment?

D1 %>%
  filter(main_sector== "Social, Finance, Analytics, Advertising") %>%
  group_by(company_permalink) %>%
  summarise(total = sum(raised_amount_usd, na.rm=TRUE)) %>%
  arrange(desc(total))

#USA - shotspotter

D2 %>%
  filter(main_sector== "Social, Finance, Analytics, Advertising") %>%
  group_by(company_permalink) %>%
  summarise(total = sum(raised_amount_usd, na.rm=TRUE)) %>%
  arrange(desc(total))

#GBR - celltick-technologies

D3 %>%
  filter(main_sector== "Social, Finance, Analytics, Advertising") %>%
  group_by(company_permalink) %>%
  summarise(total = sum(raised_amount_usd, na.rm=TRUE)) %>%
  arrange(desc(total))

#IND - manthan-systems

