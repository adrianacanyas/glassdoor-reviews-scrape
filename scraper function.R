reviews_scraper <- function(companyName, companyNum, pagesNum){
  
  # URLS for scraping
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
  
  
  ### Reshaping df
  reviews_df <- reviews_df %>%
    separate(date, into = c('date', 'position'), sep = "-") %>% 
    separate(employee_type, into = c("employee_type", "longevity"), sep = ",")
  
  reviews_df$company <- companyName
  
  
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
  
  merged_df <- reviews_df %>% 
    left_join(details_company, by = "company")
  
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
  
    
  return(
    write_delim(merged_df, paste("/Users/adrianacanas/OneDrive - Erasmus University Rotterdam/THESIS/Script Output Files/", companyName, ".csv",sep = ""),
                delim = ";", append = FALSE, col_names = TRUE
    
  )
  )
  
}