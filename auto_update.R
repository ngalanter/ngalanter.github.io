library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# formatting helpers ----------------------

#all of these can work with txt being a vector

it <- function(txt){ paste0("*",txt,"*") }

bf <- function(txt){ paste0("**",txt,"**") }

head <- function(txt,n){ paste0(paste0(rep("#",n),collapse = "")," ",txt) }

#modify to be your actual name
bf_name <- function(txt){ str_replace(txt,"Galanter, N.",bf("Galanter, N.")) }

link_f <- function(txt,link){ paste0("[",txt,"](",link,")")}

paren <- function(txt){ paste0("(",txt,")") }

#adds blank lines before, in between, and after entries
# txt can be a list (multi-line entries) or a vector (one-line entries)
empty_line_wrap <- function(txt){ 
  
  #for a vector add between every line
  if(class(txt) == "character"){
    
  ret <- c("",rbind(txt,rep("",length(txt))))
    
  }else{#for a list add between entries
    
    ret <- lapply(txt,function(x) c(x,""))
    
    ret[[1]] <- c("",ret[[1]])
  }
  
  return(ret)
    
}

#turn text vector into a line block to preserve line breaks
line_block <- function(txt) { paste("|",txt) } 

#turn text vector into a list
list_f <- function(txt) { paste("-",txt) } 

# item helpers ------------------------------

# ... at ends of functions so can apply to dataframe without selecting vars

#publication formatting
pub <- function(authors,year,title,link=NA,journal,subtype,...){
  
  ftitle <- title
  
  if((!is.na(link) & link != '')){
    ftitle <- link_f(title,link)
  }
  
  if(subtype == "Preprints"){
    
    paste0(bf_name(authors)," ",paren("Preprint"),". ",ftitle,".")
    
  }else{
    
    paste0(bf_name(authors)," ",paren(year),". ",ftitle,". ",it(journal),".")
    
  }

  }

#helper to format specific occassion a presentation/poster was presented
pres_instance <- function(year,month,day,location,event,...){
  
  if(day != ""){
    paste0(event,", ",location,", ",month," ",day,", ",year)
  } else{
    paste0(event,", ",location,", ",month," ",year)
  }

}

#formatting for an overall unique presentation/poster
# there can be one or more confrences/events at which it was presented
pres <- function(title,link=NA,dat,..){
  
  ftitle <- title
  
  instances <- dat %>% filter(title == ftitle)
  
  if((!is.na(link) & link != '')){
    ftitle <- link_f(title,link)
  }
  
  return(unlist(c(bf(ftitle), pmap(instances,pres_instance))))
  
}

# page helpers ------------------------------

#splits pages into update and non-update components
#  depends on the section break formatting being very specific
#    for example no lines between ":::"'s and the break string
split_helper <- function(old_page,break_strings){
  
  fun <- function(x){
    
    start_ind <- str_which(old_page,paste0(x,"_start"))
    end_ind <- str_which(old_page,paste0(x,"_end"))
    
    c(start_ind+1,start_ind+2,end_ind-2,end_ind-1)
      
  }
  
  breaks <- c(1,sapply(break_strings,fun),length(old_page))
  
  sapply(seq(from =1,to = length(breaks),by = 2), 
         function(x) old_page[breaks[x]:breaks[(x+1)]])
  
}

#helper to format content as a sequence of sections
#inputs:
#   headings - string vector of sections in order they should appear
#   content - dataframe of the specific content for all the sections
#   format_fun - function that formats each section
#   field - field to look for sections in, if not subtype
#   header_level - numeric between 1 and 5, level of section header
section_helper <- function(headings,content,format_fun,
                           field = "subtype",header_level = 3){
  
  lapply(headings, function(x) content %>% 
           filter(get(field) == x) %>% format_fun() %>%
           c(head(x,header_level),.)) %>% 
    unlist() %>% c()
  
}



#creates updated cv page
cv <- function(old_cv,content){
  
  parts <- split_helper(old_cv,c("pubs_update", "pres_update"))
  
  #updating publications
  
  #helper to format each section
  helper <- function(content){
    content %>% pmap(pub) %>% unlist() %>% empty_line_wrap()
  }
  
  pubs_update <- section_helper(c("Statistical Methods",
                                   "Public Health and Medicine",
                                   "Other"),
                                 content %>% 
                                   filter(type == "Publications"),
                                 format_fun = helper,
                                 field = "theme")
  
  parts[[2]] <- pubs_update
  
  #updating presentations and posters
  #helper to update each presentation/poster section
  helper <- function(content){
    
    unique_items <- content %>% 
      select(title,link) %>% distinct()
    
    formatted <- lapply(1:nrow(unique_items),
                           function(x) pres(unique_items$title[x],
                                            unique_items$link[x],
                                            content)) %>% 
      empty_line_wrap() %>% unlist()
    
    formatted[2:(length(formatted)-1)] <- 
      line_block(formatted[2:(length(formatted)-1)]) 
    
    return(formatted)
    
  }
  
  pres_update <- section_helper(c("Presentations","Posters"),
                                content %>% 
                                  filter(type == "Presentations and Posters"),
                                format_fun = helper)
  
  parts[[4]] <- pres_update
  
  return(do.call(c,parts))
  
}

#creates updated publications page
# includes two different ways sorting papers just as examples
#  for actual website use one or the other (or neither)
publications <- function(old_publications,content){
  
  parts <- split_helper(old_publications,c("pubs_update"))
  
  #helper to format each section
  helper <- function(content){
    content %>% pmap(pub) %>% unlist() %>% empty_line_wrap()
  }
  
  #using a different field than subtype to choose sections so need to specify
  pubs_update <- section_helper(c("Statistical Methods",
                                   "Public Health and Medicine",
                                   "Other"),
                                 content %>% 
                                   filter(type == "Publications"),
                                 format_fun = helper,
                                 field = "theme")
  
  parts[[2]] <- pubs_update
  
  return(do.call(c,parts))
  
}


# run the update ------------------------------

content <- read.csv("Website Content.csv")

#when no month listed, eg for a paper, putting as end of year by default
content <- content %>% 
  mutate(num_month = match(month,month.name),
         num_month = if_else(is.na(num_month),12,num_month)) %>%
  arrange(desc(year),desc(num_month),desc(day))


#updating cv
old_cv <- readLines("cv.qmd")

writeLines(old_cv,"old_cv_backup.qmd")

new_cv <- cv(old_cv,content)

writeLines(new_cv,"cv.qmd")

#updating publications
old_publications <- readLines("publications.qmd")

writeLines(old_publications,"old_publications_backup.qmd")

new_publications <- publications(old_publications,content)

writeLines(new_publications,"publications.qmd")


