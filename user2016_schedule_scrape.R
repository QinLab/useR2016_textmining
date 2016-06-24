library("magrittr")
library("dplyr")
library("rvest")
library("xml2")
library("stringr")
library("tm")


schedule_url <- "http://schedule.user2016.org"


schedule <- schedule_url %>% read_html()

schedule_links <- schedule %>% 
  html_nodes(xpath = "//*/div[@class='sched-container-inner']/span/a/@href") %>% 
  html_text(trim = TRUE) %>% 
  paste(schedule_url, ., sep = "")


event_container <- lapply(X = schedule_links, FUN = read_html)

save(event_container, file = "event_container.RData")



event_list <- lapply(
  X = event_container,
  FUN = function(x){
    name <- x %>% 
      html_nodes(xpath = "//*/span/a[@class='name']") %>%
      html_text(trim = TRUE)
    
    type <- x %>% 
      html_nodes(xpath = "//*/div[@class='sched-event-type']/a[1]") %>% 
      html_text(trim = TRUE) %>% 
      str_replace(pattern = "Â[[:space:]]+", replacement = "") %>% 
      tolower()
    
    abstract <- x %>% 
      html_nodes(xpath = "//*/div[@class='tip-description']") %>% 
      html_text(trim = TRUE)
    
    if(length(abstract) == 0) abstract <- NA
    
    speaker <- x %>% 
      html_nodes(xpath = "//*/h2/a[contains(@href,'speaker')]") %>% 
      html_text()
    
    if(length(speaker) == 0) speaker <- NA
    
    out <- data.frame(
      name = name,
      type = type,
      speaker = speaker,
      abstract = abstract,
      stringsAsFactors = FALSE
    )
    
    return(out)
  }
)


event_data <- event_list %>% rbind_all()

save(event_data, file = "event_data.RData")
