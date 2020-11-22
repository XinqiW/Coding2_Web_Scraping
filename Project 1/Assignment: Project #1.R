library(rvest)
library(data.table)
my_url <- 'https://www.ft.com/search?q=carbon'

## Create a function which downloads information from a url to dataframe from FT
get_FT_data <- function(my_url){
    print(my_url)
    t <- read_html(my_url)
    # write_html(t, 't.html')
    boxes <- t %>% html_nodes('.js-teaser')
    
    x <- boxes[[1]]
    boxes_dfs <- lapply(boxes, function(x){
       tl <- list()
       # tl[['title']] <- x %>% html_nodes('.js-teaser-heading-link') %>% html_text()
       tl[['title']] <- paste0(x %>% html_nodes('.o-teaser__heading') %>% html_text(), collapse = ' ')
       tl[['link']] <- paste0('https://www.ft.com', x %>% html_nodes('.o-teaser__heading') %>% html_attr('href'))
       # tl[['description']] <- x %>% html_nodes('.js-teaser-standfirst-link') %>% html_text()
       tl[['description']] <- paste0(x %>% html_nodes('.o-teaser__standfirst') %>% html_text(), collapse = ' ')
    return(tl)
})

df <- rbindlist(boxes_dfs, fill = T)
return(df)
}

# apply my function "get_FT_data()"
df <- get_FT_data(my_url)

# save the outputs into csv file
write.csv(df, 'get_FT_data_output.csv')

# Save a single object to a file
saveRDS(df, "get_FT_data_output.rds")

# Create a function which requires two arguments. First a keyword  --------

get_FT <- function(searchterm, number_of_page) {
  
  # create links
  links_to_get <- 
    paste0('https://www.ft.com/search?q=', searchterm, '&page=', 1:number_of_page, '&sort=relevance' )
  ret_df <- rbindlist(lapply(links_to_get, get_FT_data))
  return(ret_df)
  
}

# apply my function "get_FT()"
df2 <- get_FT(searchterm = 'pollution', 3)

# save the outputs into csv file
write.csv(df2, 'get_FT_output.csv')

# Save a single object to a file
saveRDS(df2, "get_FT_output.rds")
