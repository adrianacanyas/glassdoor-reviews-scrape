# glassdoor-reviews-scrape
A guide on how to build a scraper to obtain reviews data from Glassdoor.com

## How to build a scraper

The first step I always take to build a scraper is checking what pattern do the URLs share when going from page 1 to page 2 to page 3 and so on... 
Taking Google as an example, page 2 and 3 can be accessed through:

https://www.glassdoor.co.uk/Reviews/Company-Reviews-E9079_P2.htm?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng

https://www.glassdoor.co.uk/Reviews/Company-Reviews-E9079_P3.htm?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng

*Note that I am taking the UK version of Glassdoor and sorting the reviewst from Most Recent to Oldest. For different options you might obtain different URLs.*

We can see that the URLs follow a different pattern: they start with https://www.glassdoor.co.uk/Reviews/Company-Reviews-, then we have a company number (in this case E9079), followed by _P and the page number and the last part of the URL configuration (in this case specifying how the reviews are sorted and the language).

With this in mind, we can start building the core function of our scraper:

```r
# Set URLs for scraping
  start_url <- "https://www.glassdoor.co.uk/Reviews/Company-Reviews-"
  settings_url <- ".htm?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
  
  ### Scrape Reviews
  map_df(1:pagesNum, function(i){
    Sys.sleep(3)
    tryCatch({
      pg_reviews <- read_html(GET(paste(start_url, companyNum, "_P", i, settings_url, sep = "")))
      table = pg_reviews %>% 
        html_elements(".mb-0")
      
      data.frame(date = pg_reviews %>% 
                   html_nodes(".middle.common__EiReviewDetailsStyle__newGrey") %>% 
                   html_text2(),
                 
                 summary = pg_reviews %>% 
                   html_nodes(".reviewLink") %>% 
                   html_text(),
                 
                 rating = pg_reviews %>%
                   html_nodes("#ReviewsFeed .mr-xsm") %>%
                   html_text(),
                 
                 employee_type = pg_reviews %>%
                   html_nodes(".eg4psks0") %>%
                   html_text(),
                 
                 pros = pg_reviews %>%
                   html_nodes(".v2__EIReviewDetailsV2__fullWidth:nth-child(1) span") %>%
                   html_text(),
                 
                 cons = pg_reviews %>%
                   html_nodes(".v2__EIReviewDetailsV2__fullWidth:nth-child(2) span") %>%
                   html_text()
                 
      )}, error = function(e){
        NULL
      })
    
  }) -> reviews_df
```

I am using `map_df` to run the function over the pages (pagesNum is the number of pages that we want to use to scrape the reviews) and return a data frame. `Sys.sleep` is set to let 3 seconds of pause between each iteration to not overload the servers and avoid getting an error. On top of that, in case there would be an error with a specific review or page, we want the function to still run over the rest of pages and not stop where the error is. For that, I am using `tryCatch`.

After that, we can define the URLs and start selecting the nodes that we are going to be scrapping. To select the nodes I recommend using Selector Gadget extension for Chrome. It easily gives you the CSS selector by just clicking on it. As you can see, I am scrapping the date, summary (i.e. title of the review), rating, if the employee is a current employee or a former employee, and the pros and cons written as the main part of the review. 

This function will return a data frame, reviews_df, with the columns just specified and as many reviews as you specify in the pagesNum argument (each page has 10 reviews, so if pagesNum = 10 you will get 100 reviews).

To get more information for each review, I follow the same logic to scrape general information about the company, like Industry, Location of the Headquarters or Number of Employees.

```r
 ### Scrapper General Information of a Company
  ## URLs for scraping
  start_overview_url <- "https://www.glassdoor.co.uk/Overview/Working-at-"
  settings_overview_url <- ".htm"
  
  map_df(1, function(i){
    pg <- read_html(GET((paste(start_overview_url, companyName, "-EI_I", companyNum, settings_overview_url, sep = ""))))
    table = pg %>% 
      html_elements(".css-11b1byw:nth-child(1)")
    
    data.frame(company = pg %>% 
                 html_nodes("#DivisionsDropdownComponent") %>% 
                 html_text2(),
               
               Size = pg %>% 
                 html_nodes(".pr-sm-xxsm:nth-child(3) .css-1cnqmgc") %>% 
                 html_text(),
               
               Type = pg %>%
                 html_nodes(".pr-sm-xxsm:nth-child(5) .css-1cnqmgc") %>%
                 html_text(),
               
               Revenue = pg %>%
                 html_nodes(".pr-sm-xxsm:nth-child(7) .css-1cnqmgc") %>%
                 html_text(),
               
               Headquarters = pg %>%
                 html_nodes(".pr-xxsm+ .pl-sm-xxsm .css-1cnqmgc") %>%
                 html_text(),
               
               Founded = pg %>%
                 html_nodes(".pr-sm-xxsm+ .pl-sm-xxsm .css-dwl48b") %>%
                 html_text(),
               
               Industry = pg %>%
                 html_nodes(".pl-sm-xxsm .css-1hg9omi") %>%
                 html_text()
               
    )
    
  }) -> details_company
  ````
I then do a bit of cleaning and reshaping (splitting string variables into to, changing types, removing duplicates, etc...) and I merge both dataframes:

```r
  ### Reshaping reviews_df
  reviews_df <- reviews_df %>%
    separate(date, into = c('date', 'position'), sep = "-") %>% 
    separate(employee_type, into = c("employee_type", "longevity"), sep = ",")
  
  reviews_df$company <- companyName
 
  ### Merging both dfs
   merged_df <- reviews_df %>% 
      left_join(details_company, by = "company")
  
  ## Remove duplicates
  merged_df <- merged_df %>% 
    distinct(pros, cons, .keep_all = TRUE)
  
  ## Change date column into date format
  merged_df$date <- gsub(",", "", merged_df$date)
  merged_df$date <- mdy(merged_df$date)
  
  ## Change rating to numeric
  merged_df$rating <- gsub("\\..*","",merged_df$rating) %>% 
    as.numeric()
  
  ## Factorize employee type, company, industry
  merged_df$employee_type <- as.factor(merged_df$employee_type)
  merged_df$company <- as.factor(merged_df$company)
  merged_df$Industry <- as.factor(merged_df$Industry)
  
  ## Keep only relevant part of Type of company, and factorize
  merged_df$Type <- gsub(".*- ","",merged_df$Type) %>% 
    as.factor()
  
  ## Separate HQ into city and country
  merged_df <- merged_df %>%
    separate(Headquarters, into = c('CityHQ', 'CountryHQ'), sep = ", ") 
  
  merged_df$CityHQ <- as.factor(merged_df$CityHQ)
  merged_df$CountryHQ <- as.factor(merged_df$CountryHQ)
  
  merged_df$pros <- gsub("-","",merged_df$pros)
  merged_df$cons <- gsub("-","",merged_df$cons)
```

As I last step, I put eveything under one function `reviews_scraper` that has the arguments `companyName` (e.g. Google), `companyNum` (e.g.: E9079) and `pagesNum`:

```r
reviews_scraper <- function(companyName, companyNum, pagesNum){
## Add everything that we have just covered
 return(
    write_delim(merged_df, paste("YOUR FOLDER HERE", companyName, ".csv",sep = ""), # Add your folder path in here
                delim = ";", append = FALSE, col_names = TRUE
    
  )
  )
  
}
```
This will save the final merged dataframe as a csv on your selected path, under the name of the company that you are scraping.

And we are done! We have build a function to scrape job reviews on Glassdoor!

*Note: CSS nodes can vary if the current Glassdoor website is modified. Have this into account if you run into an error when using this code.*
