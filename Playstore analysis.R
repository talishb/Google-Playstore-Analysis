library(tidyverse) #Data visualization, extraction and transformation
library(lubridate) #Use for date tranform
library(ggthemes) #Use for data visualization
library(corrplot) #Use for generating correlation matrix
library(dplyr)
library(ggplot2)
library(highcharter) 
library(stringr)
library(xts)
library(psych)
options(scipen=9999999) ##Remove scientific notation


#we start by observing the initial dataset and all the variables we are dealing with

summary(googleplaystore)

#App              Category             Rating          Reviews        
#Length:10841       Length:10841       Min.   : 1.000   Min.   :       0  
#Class :character   Class :character   1st Qu.: 4.000   1st Qu.:      38  
#Mode  :character   Mode  :character   Median : 4.300   Median :    2094  
                                      #Mean   : 4.193   Mean   :  444153  
                                      #3rd Qu.: 4.500   3rd Qu.:   54776  
                                      #Max.   :19.000   Max.   :78158306  
                                      #NA's   :1474     NA's   :1         
  #Size             Installs             Type              Price          
#Length:10841       Length:10841       Length:10841       Length:10841      
#Class :character   Class :character   Class :character   Class :character  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character  




#Content Rating        Genres          Last Updated       Current Ver       
#Length:10841       Length:10841       Length:10841       Length:10841      
#Class :character   Class :character   Class :character   Class :character  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character  


#Android Ver       
#Length:10841      
#Class :character  
#Mode  :character

googleplaystore1 <- googleplaystore

google_app.clean <- googleplaystore1 %>%
  mutate(
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    Size = gsub("M", "", Size),
    Size = ifelse(grepl("k", Size), 0, as.numeric(Size)),
    Rating = as.numeric(Rating),
    Reviews = as.numeric(Reviews),
    Price = as.numeric(gsub("\\$", "", as.character(Price))),
    `Last Updated` = as.POSIXct(googleplaystore1$`Last Updated`,format = "%B %d, %Y",tz=Sys.timezone())
  )%>%
  filter(
    Type %in% c("Free", "Paid")
  )

google_app.clean=google_app.clean[!duplicated(google_app.clean), ]


str(googleplaystore)
#some data cleaning procedures used:-

    # Eliminate some characters to transform Installs to numeric
    # Eliminate M to transform Size to numeric
    # Replace cells with k to 0 since it is < 1MB
    # Transform reviews to numeric
    # Remove currency symbol from Price, change it to numeric
    # Last Updated to date format
    # Replace "Varies with device" to NA since it is unknown
    # Keep only version number to 1 decimal
    # Drop old Android version column
    # Two apps had type as 0 or NA, they will be removed 



#changing the necessary data types

google_app.clean$Type <- as.factor(google_app.clean$Type)
google_app.clean$Category <- as.factor(google_app.clean$Category)
google_app.clean$`Content Rating` <- as.factor(google_app.clean$`Content Rating`)
google_app.clean$Genres <- as.factor(google_app.clean$Genres)
google_app.clean$Installs <- as.factor(google_app.clean$Installs)



str(google_app.clean)
View(google_app.clean)
summary(google_app.clean)


#NA analysis

google_app.clean %>%
  summarise_all(
    funs(sum(is.na(.)))
  ) %>%
  gather() %>%
  # Only show columns with NA
  filter(value> 1) %>%
  arrange(-value) %>%
  hchart('column', hcaes(x = 'key', y = 'value', color = 'key')) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_title(text = "Columns with NA values")


#app

App_occurences = table(google_app.clean$App) # Returns table of occurences of a categorical variable
App_occurences_data_frame = data.frame(App_occurences) # convert table of occurences to dataframe
colnames(App_occurences_data_frame)[1] <- "App" # change the first column name to App
App_occurences_data_frame$Freq <- as.factor(App_occurences_data_frame$Freq) # convert Freq to factor 

ggplot(App_occurences_data_frame, aes(x = Freq, fill = Freq)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  xlab("Frequency of appearence") +
  ylab("Number of application associated") +
  guides(fill = FALSE)


#category


str(google_app.clean$Category)
ggplot(google_app.clean, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1, size = 2.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Number of apps by category", y = "") +
  guides(fill = FALSE)


#rating
summary(google_app.clean$Rating)
ggplot(google_app.clean, aes(y = Rating)) +
  geom_boxplot()

p = google_app.clean %>%filter(!is.na(Rating))%>% ggplot(aes(x=Rating)) + geom_density(color="black",fill="white")
p+scale_x_continuous(limits = c(0, 5.1))+labs(title="Distribution of App Rating",x="Rating",y="Count") + theme_fivethirtyeight() #Theme inspired by fivethirtyeight.com plots

# Automatically Removed 1464 rows containing non-finite values (stat_boxplot) (Earlier Missing values taken care of)


#category by price
google_app.clean %>% 
    group_by(Category, Type) %>%
    summarize(n = n()) %>%
      mutate(perc = round((n /sum(n))*100)) %>%
      hchart('bar', hcaes(x = 'Category', y = 'perc', group = 'Type')) %>%
      hc_plotOptions(series=list(stacking='normal')) %>%
      hc_title(text="Percentage of Free vs Paid by Category") %>%
      hc_add_theme(hc_theme_flat())


#Category by installs
google_app.clean %>%
  count(Category, Installs) %>%
  group_by(Category) %>%
  summarize(
    TotalInstalls = sum(as.numeric(Installs))
  ) %>%
  arrange(-TotalInstalls) %>%
  hchart('scatter', hcaes(x = "Category", y = "TotalInstalls", size = "TotalInstalls", color = "Category")) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Most popular categories (# of installs)")

#Reviews
str(google_app.clean$Reviews)
summary(google_app.clean$Reviews)
describe(google_app.clean$Reviews)

## Histogram of numerical values of Reviews
ggplot(google_app.clean, aes(x = Reviews)) +
  geom_histogram(colour="black", fill="blue", alpha=.5)+
  xlab("Reviews") +
  labs(title="Number of apps by reviews range")

ggplot(google_app.clean,aes(y = Reviews)) +
  geom_boxplot(color='black', fill='white') +
  ylab("Number of reviews")



#installs

ggplot(google_app.clean, aes(Installs)) +
  geom_bar() +
  geom_bar(fill = '#FECBA9', color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-1,size=3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #+ coord_flip() +
  labs(title = "Number of apps by Install")


#size
ggplot(google_app.clean,aes(y = Size)) +
  geom_boxplot(color='black', fill='white') +
  ylab("Size in megabytes")

#Removed 1525 rows containing non-finite values (stat_boxplot). 

#NA values in Size
#On the data cleaning process above,
#I saw “Varies with device” was present under “Size”. 
#There were a total of 1525 applications with “Varies with device” which we transformed to NA. 
#So, that explains NA in this column.

google_app.clean %>%
  filter(
    is.na(Size)
  ) %>% 
  count()


summary(google_app.clean$Size)
google_app.clean %>%
  count(Size) %>%
  hchart('area', hcaes(x = "Size", y = "n")) %>%
  hc_colors("#fb4901") %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Distribution of application size (in MB)")

#size by type
hcboxplot(x = google_app.clean$Size, var = google_app.clean$Type, outliers = TRUE, color = "#fb4901", fillColor = "lightblue") %>%
  hc_chart(type = "column") %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Application size range (in MB) by Application Type")

#There are a lot of apps under 10MB, in general applications are between 5 MB to 30 MB. Paid applications are slightly smaller.

#type
str(google_app.clean$Type)

ggplot(google_app.clean, aes(x = Type, fill = Type)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  labs(title = "Number of apps by type") +
  guides(fill = FALSE)

v <- table(google_app.clean$Type)
prop.table(v)*100

#prices
str(google_app.clean)
summary(google_app.clean$Price)

ggplot(google_app.clean,aes(x = Price)) + 
  geom_histogram(binwidth = 2, color='black', fill='white') +
  scale_x_continuous(breaks = seq(0,400,10), labels = seq(0,400,10)) +
  theme(axis.text.x = element_text(angle = 80, hjust = 1)) +
  labs(title = "Number of apps by Price")+
  xlab("Prices")


#installs of apps with no ratings
google_app.clean %>%
  filter(is.na(Rating)) %>%
  count(Installs) %>%
  arrange(-n) %>%
  hchart('column', hcaes(x = "Installs", y = "n")) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Installations with no rating")

## content rating
ggplot(google_app.clean, aes(x = `Content Rating`, fill = `Content Rating`)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  guides(fill = FALSE) +
  labs(title = "Number of apps by content rating") +
  xlab("Content Rating")



## Bar_Plot For FAMILY Category
ggplot(google_app.clean[google_app.clean$Category %in% c('FAMILY'),], aes(x = Genres, fill = Genres)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Number of apps by genre in category FAMILY") +
  guides(fill = FALSE)

## Bar_Plot For GAME Category
ggplot(google_app.clean[google_app.clean$Category %in% c('GAME'),], aes(x = Genres, fill = Genres)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Number of apps by genre in category GAME") +
  guides(fill = FALSE)

#Last updated
## Split the date into day, month and year.
## With that we could have a good visalization of last updated column
dates <- str_split(str_remove_all(googleplaystore1$`Last Updated`, ",")," ")
months <- sapply(dates, function(t){return (paste(t[1],t[3],sep = " "))})
days <- sapply(dates, function(t){return (t[2])})
years <- sapply(dates, function(t){return (t[3])})
last_update <- data.frame(months = months, years = years)
## BarPlot for year of Last.Updated 
ggplot(last_update, aes(x = years, fill = years)) +
  geom_bar() +
  guides(fill = FALSE) +
  xlab("Year") +
  labs(title = "Number of apps by year of last update")

## BarPlot- ANdroid Ver
ggplot(google_app.clean, aes(x = `Android Ver`, fill = `Android Ver`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(stat='count', aes(label=..count..), vjust=0.5, hjust = -0.1, size=3)+
  coord_flip() + labs(title="Number of apps by supported Android Version") + guides(fill = FALSE) +
  ylab("Android Version")










#apps vs ratings

library(scales)
#scales library for log values to be displayed as natural numbers
ggplot(google_app.clean, aes(x=Reviews, y=Rating)) +
  scale_x_continuous(trans='log10', labels=comma) +
  geom_point(aes(col=Rating)) +
  labs(title="Android App Ratings vs Number of Reviews", subtitle="Google Playstore Dataset", y="Rating from 1 to 5 stars", x="Number of Reviews") +
  theme_linedraw()

#check if last update has impact

googleplaystore1$`Last Updated` <- as.Date(google_app.clean$`Last Updated`, format = "%B %d, %Y")
google_app.clean$Month <- format(google_app.clean$`Last Updated`, "%b")
head(google_app.clean)

months <- factor(googleplaystore1$Month, levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))

ggplot(google_app.clean, aes(x = months, y = Reviews)) +
  scale_y_continuous(trans='log10', labels=comma) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Number of Android app reviews based on month it was last updated",
       y = "Number of Reviews",
       x = "Month it was Updated") + theme_linedraw()

google_app.clean %>% group_by(Month) %>% summarize(Reviews=mean(Reviews))

# Creating a group-means data set
gd <- google_app.clean %>% group_by(Month) %>% summarize(Reviews=mean(Reviews))

ggplot(google_app.clean, aes(x=Month, y=Reviews, group=12)) +
  scale_y_continuous(trans='log10', labels=comma) +
  geom_point(data=gd, color="skyblue") +
  geom_line(data=gd, color="skyblue") +
  scale_x_discrete(limits = month.abb) +
  labs(title = "Average number of Android app reviews and month updated",
       y = "Number of Reviews",
       x = "Month it was Updated") + 
  theme_linedraw()


#category by type

ggplot(google_app.clean, aes(x = Category,
                             fill = Type # map the fill color to caramel           
)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

google_app.clean.app=google_app.clean[!duplicated(google_app.clean$App), ] %>% 
  filter(!is.na(Reviews)) %>% filter(!is.na(Installs))
google_app.clean.review.sort = order(google_app.clean.app$Reviews,decreasing = T)

google_app.clean.app.top20review=google_app.clean.app[google_app.clean.review.sort[1:20],]


extract = c("App","Category","Reviews","Installs", "Type")
google_app.clean.app.top20review[extract]


#bottom 15 apps by rting

google_app.clean.app=google_app.clean[!duplicated(google_app.clean$App), ] %>% filter(!is.na(Reviews)) %>% filter(!is.na(Installs))
google_app.clean.review.sort = order(google_app.clean.app$Reviews,decreasing = F)


google_app.clean.app.bottom20review=google_app.clean.app[google_app.clean.review.sort[1:15],]
extract = c("App","Category","Reviews","Installs","Rating")

View(google_app.clean.app.bottom20review[extract])

#top 15 apps by rating

google_app.clean.app=google_app.clean[!duplicated(google_app.clean$App), ] %>% filter(!is.na(Reviews)) %>% filter(!is.na(Installs))
google_app.clean.review.sort = order(google_app.clean.app$Reviews,decreasing = T)


google_app.clean.app.bottom20review=google_app.clean.app[google_app.clean.review.sort[1:15],]
extract = c("App","Category","Reviews","Installs","Rating")

View(google_app.clean.app.bottom20review[extract])


#median rating
google_app.clean %>%
  filter(Type == "Paid") %>%
  group_by(Category) %>%
  summarize(
    Rating = median(Rating)
  ) %>%
  arrange(-Rating) %>%
  hchart('treemap', hcaes(x = 'Category', value = 'Rating', color = 'Rating')) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_title(text="Median Rating per category")


#Content Rating vs rating
### Looking at the relationship between content rating and average rating
google_app.clean$`Content Rating` <- as.factor(google_app.clean$`Content Rating`)

google_app.clean %>% filter(!is.na(Rating), `Content Rating`!="", !is.na(Installs), `Content Rating`!="Unrated") %>%  group_by(`Content Rating`) %>% summarize(meanRating = mean(Rating)) %>%  ggplot(mapping = aes(x = `Content Rating`, y= meanRating, fill = `Content Rating`)) + geom_col() + geom_line(group = 1, size = 0.5) + ggtitle("Mean rating per content rating") + ylab("Mean rating")

#Category vs rating

### Exploring the ratings given to the apps, categorically
google_app.clean %>%  group_by(Category) %>%  filter(!is.na(Rating), Category!='1.9') %>% summarise(meanRating = mean(Rating)) %>% ggplot(mapping = aes(x = Category, y = meanRating)) + geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + ggtitle("Average rating across categories") + ylab("Average rating") + guides(fill=FALSE)

google_app.clean %>% filter(Category!='1.9') %>% 
  ggplot(mapping = aes(x = Rating, fill = Category)) + geom_histogram(bins = 50, position = "identity") + 
  xlim(0,5) + facet_wrap(~Category) + guides(fill = FALSE) + 
  ggtitle("Distribution of ratings across categories") + ylab("Count")

#rating vs size

ggplot(google_app.clean, aes(x=Size, y=Rating)) +
  scale_x_continuous(trans='log10', labels=comma) +
  geom_point(aes(col=Rating)) +
  labs(title="Android App Ratings vs Size of Apps", subtitle="Google Playstore Dataset", y="Rating from 1 to 5 stars", x="Size of Apps in MB") +
  theme_linedraw()

#rating vs installs

ggplot(google_app.clean, aes(x=Installs, y=Rating)) +
  #scale_x_continuous(trans='log10', labels=comma) +
  geom_point(aes(col=Rating)) +
  labs(title="Android App Ratings vs Number of Installs", subtitle="Google Playstore Dataset", y="Rating from 1 to 5 stars", x="Number of Installs") +
  theme_linedraw()


#Regression of Reviews vs Ratings
google_app.clean %>% filter(!is.na(log(Reviews))) %>% 
  ggplot(mapping = aes(x = log(Reviews), y = Rating)) + geom_smooth(method = "lm") + ggtitle("Relation between the log of the reviews received with the rating for the app") + xlab("Log of the reviews received")


library(xlsx)
# Write the first data set in a new workbook
write.xlsx(google_app.clean, file = "Google Playstore cleaned.xlsx",
           sheetName = "App Data", append = FALSE)

