# Procedure to create dataset ---------------
# Data source (first page): http://www.waymarking.com/cat/details.aspx?f=1&guid=a89c5a57-86cc-4ae9-a35f-d9f7efcc36aa

# Install packages ---------------
#install.packages("rvest")
#install.packages("xml2")
#install.packages("mefa")
library('rvest')
library('xml2')
library('tidyverse')
library('purrr')

# Specifying and reading url ---------------
# First page
url <- "http://www.waymarking.com/cat/details.aspx?f=1&guid=a89c5a57-86cc-4ae9-a35f-d9f7efcc36aa"
html <- read_html(url)

# Name scraping function -------------
getName <- function(url){
  Sys.sleep(2)
  #Introduces pauses to convince server not robot.
  name <- read_html(url) %>% 
    html_nodes(".wmd_namebold a+ a")%>%           
    html_text() %>% 
    gsub(x = ., pattern = '^\\s+|\\s+$', replacement = '')
  name <- tibble('Name' = name)
}
#Pulls node for post name.

# Location scraping -------------
getLocation <- function(url){
  Sys.sleep(2)
  #Introduces pauses to convince server not robot.
  location <- read_html(url) %>% 
    html_nodes(".wmd_location")%>%           
    html_text() %>% 
    gsub(x = ., pattern = '\\blocation\\b[[:punct:]]\\s', replacement = '')
  location <- tibble('Location' = location)
}
#Pulls node for location.

# Quick Description scraping -------------
getPostDesc <- function(url){
  Sys.sleep(2)
  #Introduces pauses to convince server not robot.
  desc <- read_html(url) %>% 
    html_nodes(".wmd_desc")%>%           
    html_text() 
  desc <- tibble('Quick Description' = desc)
}
#Pulls node for post description.

# Last visit scraping -------------
getLastVisit <- function(url){
  Sys.sleep(2)
  #Introduces pauses to convince server not robot.
  lastvisit <- read_html(url) %>% 
    html_nodes(".wmd_lastvisited")%>%           
    html_text() %>% 
    gsub(x = ., pattern = '\\blast\\b\\s\\bvisited\\b[[:punct:]]\\s', replacement = '')
  lastvisit <- tibble('Last Visit' = lastvisit)
}
#Pulls node for last visit.



# Pull node for url to next page -------------
getNextUrl <- function(url) {
  Sys.sleep(.2)
  read_html(url) %>% 
    html_node("#ctl00_ContentBody_WaymarkDisplayControl1_PagerControl1_lnkNext") %>%
    html_attr("href")
}
#Pulls node for url to next page.

# Scrape function ------------
scrapeBackMap <- function(url, n){
  Sys.sleep(3)
  purrr::map_df(1:n, ~{ # from page 1 to n
    if(!is.na(url)){
      #Only run if URL is not NA
      oUrl <- url
      name <- getName(url) # get names for every node
      loc <- getLocation(url)
      descq <- getPostDesc(url)
      visitl <- getLastVisit(url)
      url <<- getNextUrl(url)
      
      data.frame(curpage = oUrl, 
                 nexturl = url,
                 postname = name,
                 location = loc,
                 qdesc = descq,
                 lvisit = visitl
                 #prepares functions for dataframe
      )}
  })
}



#Scrape data for 30 pages of results
lib_dat <- scrapeBackMap(url, 30)

# Scraping coordinates ------------
coords <- list()
index = 1

# Loop to scrape coordinate data  ------------
for(i in lib_dat$Name[1:length(lib_dat$Name)]){
  placepage <- html_session(lib_dat$curpage[index]) %>% 
    follow_link(i) %>% # Go to the link corresponding with this name
    read_html()
  index = index + 1
  coords[[i]]$place <-  placepage %>% 
    html_nodes("#Table1 span:nth-child(2)") %>% 
    html_text()
  coords[[i]]$coordinate <- placepage %>%
    html_nodes("#wm_coordinates strong") %>% 
    html_text() 
}

# coord <- bind_rows(coords, fill=TRUE) #I don't know how to use bind_rows
coord <- rbind_list(coords) # But rbind_list is deprecated


# Putting it all together ---------
liberty <- cbind(lib_dat, coord)

# Quick reordering, drop "next url" column
liberty <- subset(liberty, select = -c(nexturl, curpage))
liberty <- liberty[c("Name", "Location", "place", "coordinate", "Last.Visit", "Quick.Description")]

# Split coordinate column into latitude and longitude
liberty <- liberty %>%
    mutate(latitude = str_sub(coordinate, 1, -14),
           longitude = str_sub(coordinate, 14, -1))

# Remove leading/trailing white space
liberty$latitude <-
  gsub(pattern = "^\\s+|\\s+$", replacement = "", liberty$latitude)

liberty$longitude <- 
  gsub(pattern = "^\\s+|\\s+$", replacement = "", liberty$longitude)

# Save  ------------
saveRDS(liberty, "liberty.rds")
# Other useful columns:
# Page url for waymarking entry
# Country

# Waymarking site doesn't have UK Statues... 


