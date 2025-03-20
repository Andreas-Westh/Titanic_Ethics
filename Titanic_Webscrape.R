library(rvest)
library(httr)
library(stringr)
library(dplyr)
# originalt blev vi bedt om at finde navnene på passagererne som er mellem k-o, men vi tog alle vi kunne 
library(rvest)
library(httr)
library(stringr)

readRenviron(".Renviron")
cookie <- Sys.getenv("cookie")
username <- Sys.getenv("titanica_username")
password <- Sys.getenv("titanica_password")

url <- "https://www.encyclopedia-titanica.org/titanic-passenger-list/"
headers <- c(
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36",
  "Cookie" = cookie
)

response <- GET(url, add_headers(.headers = headers))
page <- read_html(response)
links <- as.data.frame(page %>% html_node("tbody") %>% html_nodes("td") %>%  html_nodes("a") %>% html_attr("href"))
colnames(links) <- "links"

victims <- links[grepl("/titanic-victim/", links$links), , drop = FALSE]
survivors <- links[grepl("/titanic-survivor/",links$links),,drop=F]

allpassangers_df <- rbind(victims,survivors)


#### big scrape ####
#allpassangers <- allpassangers[1,]
allpassangers <- as.factor(allpassangers_df$links)
#url + heder tilføjelser/sammensætning
Titanic <- "https://www.encyclopedia-titanica.org"

passenger_data <- list()  

for (i in seq_along(allpassangers)) {
  url <- paste0(Titanic, allpassangers[i])
  Sys.sleep(runif(1, 0.2, 0.4))  # be kind to the server :-)
  
  page <- read_html(GET(url, add_headers(.headers = headers), authenticate(username, password)) %>% 
                      httr::content(as = "text", encoding = "UTF-8"))
  
  # takes out the full raw summary
  full_summary <- page %>% html_node("#summary") %>% html_text(trim = TRUE)
  
  # grab all nodes inside #summary (not just immediate siblings)
  summary_nodes <- page %>% html_nodes("#summary *")
  
  passenger_info <- list()
  last_key <- NULL
  
  # loop through all nodes inside #summary
  for (node in summary_nodes) {
    key <- node %>% html_node("strong") %>% html_text(trim = TRUE)  # get field name
    value <- html_text(node, trim = TRUE)  # get corresponding value
    
    if (!is.na(key) && key != "") {
      passenger_info[[key]] <- value
      last_key <- key 
    } else if (!is.null(last_key) && value != "") {
      # if value belongs to the previous key, append it
      passenger_info[[last_key]] <- paste(passenger_info[[last_key]], value, sep = " ")
    }
  }
  
  # take it if titanic victum or survived
  passenger_info[["Survival Status"]] <- page %>% 
    html_node("#summary a[href*='titanic-victims'], #summary a[href*='titanic-survivors']") %>% 
    html_text(trim = TRUE)
  
  # Extract passenger class (1st, 2nd, 3rd Class)
  class_info <- page %>% html_node("a[title*='Class Passengers']") %>% html_text(trim = TRUE)
  
  if (!is.na(class_info) && class_info != "") {
    passenger_info[["Class"]] <- class_info
  }
  passenger_info[["Full Summary"]] <- full_summary
  passenger_info[["Link"]] <- url
  passenger_data[[i]] <- as.data.frame(t(passenger_info), stringsAsFactors = FALSE)
  
  # i like little updates, makes me feel cool
  cat(paste0("Passengers scraped: ", i, " out of ", length(allpassangers), "\n"))
}

passenger_df <- NULL
passenger_df <- bind_rows(lapply(passenger_data, as.data.frame), .id = "Passenger_ID")


#saveRDS(passenger_df,"clutter/passengerLoop.RDS")
passenger_df <- NULL
passenger_df <- readRDS("clutter/passengerLoop.RDS")

# data cleaning:
# get class
passenger_df <- passenger_df %>%
  mutate(ticket = as.factor(str_extract(`Ticket No`, "(?<=Ticket No\\. )\\d+")))
titanicBoats <- titanicBoats %>%
  mutate(ticket = as.factor(str_extract(ticket, "\\d+")))

passenger_df <- passenger_df %>%
  left_join(titanicBoats %>% select(ticket, pclass), by = "ticket")

passenger_df <- passenger_df %>%
  select(-1) %>%  
  relocate(Name, pclass)  
passenger_df <- passenger_df %>%
  select(-ticket)

passenger_df <- passenger_df[!duplicated(passenger_df$Name), ]


saveRDS(passenger_df,"titanica_passengerinfo_raw.RDS")


# Name
passenger_df$Name <- passenger_df$Name %>%
  str_remove("^Name:\\s*") %>%  # Remove leading "Name: "
  str_replace("^([^ ]+ [^ ]+ [^ ]+).*", "\\1")  # Keep only the first full name

# Age
passenger_df$Birthdate <- str_extract(passenger_df$Born, ".*(?= in )") # saves birthdate
passenger_df$Birthplace <- str_remove(passenger_df$Born, ".*? in ") # saves birthplace
passenger_df <- passenger_df %>% select(-Born)
passenger_df <- passenger_df %>% select(Passenger_ID, Name, Birthdate, Birthplace, everything())

# Age / Sex
passenger_df$Age <- str_remove(passenger_df$Age, "^Age:\\s*")
passenger_df <- passenger_df %>%
  mutate(
    Age_Before_Paren = str_extract(Age, "^[^)]+\\)"),   # Everything before and including `)`
    Age_After_Paren = str_extract(Age, "(?<=\\))\\s*.*") # Everything after `)`, keeping spaces
  )
passenger_df <- passenger_df %>%
  mutate(Age_After_Paren = str_extract(Age_After_Paren, "\\[.*?\\]"))

# make into boolean
passenger_df <- passenger_df %>%
  mutate(Is.Child = !is.na(Age_After_Paren))

# split age and sex
passenger_df <- passenger_df %>%
  mutate(Sex = str_extract(Age_Before_Paren, "\\((.*?)\\)"),  # Extract text inside ()
         Age_Before_Paren = str_remove(Age_Before_Paren, "\\s*\\(.*?\\)"))  # Remove (text) from original column




passenger_df$Religion <- as.character(passenger_df$Religion)

i
#### the lucky luke loop ####
passenger_data <- list()

for (i in allpassangers) {
  
  url <- paste0(Titanic, i)
  #Sys.sleep(runif(1, min = 0.2, max = 0.4))

  rawres <- GET(url, add_headers(.headers = headers),authenticate(username, password))
  
  
  rawcontent <- httr::content(rawres, as = "text", encoding = "UTF-8")
  page <- read_html(rawcontent)
  
  
  passenger_info <- page %>%
    html_node("#summary") %>%
    html_text(trim = TRUE)
  
  
  passenger_data[[i]] <- paste(passenger_info, collapse = " | ") # | samler listen i en char-string
}


passenger_df <- data.frame(
  URL = names(passenger_data),
  Info = unlist(passenger_data),
  stringsAsFactors = FALSE
)

saveRDS(passenger_df,"clutter/Scraped.RDS")


##### den endelige scrape - mangler struktur og stringextract's ####

scrapedTitanic <- readRDS("clutter/Scraped.RDS")

#stringR_magic
scrapedTitanic <- scrapedTitanic %>%
  mutate(Name = str_extract(Info, "Name:\\s*([^\\n]+)")) %>%                    # Henter alt efter "Name:"
  mutate(Name = str_remove(Name, "Name:\\s*")) %>%                              # Fjerner selve "Name:"-teksten
  mutate(Born=str_extract(Info, "Born:\\s*([^\\n]+)")) %>% 
  mutate(Born=str_remove(Born, "Born:\\s*")) %>% 
  mutate(Age=str_extract(Info, "Age:\\s*([^\\n]+)")) %>% 
  mutate(Age=str_remove(Age, "Age:\\s*")) %>% 
  mutate(isCHILD=str_extract(Age, "\\[([^]]+)\\]")) %>%                         # Finder tekst mellem [ ]
  mutate(Age=str_remove(Age, "\\[([^]]+)\\]")) %>% 
  mutate(Sex = str_extract(Age, "\\(([^)]+)\\)")) %>%                           # Finder tekst mellem ( )
  mutate(Age=str_remove(Age, "\\(([^)]+)\\)")) %>% 
  mutate(Nationality = str_extract(Info, "Nationality:\\s*([^\\n]+)")) %>% 
  mutate(Nationality=str_remove(Nationality, "Nationality:\\s*")) %>% 
  mutate(Marital_Status= str_extract(Info,"Marital Status:\\s*([^\\n]+)")) %>% 
  mutate(Marital_Status=str_remove(Marital_Status, "Marital Status:\\s*")) %>% 
  mutate(Last_Residence=str_extract(Info,"Last Residence:\\s*([^\\n]+)")) %>% 
  mutate(Last_Residence=str_remove(Last_Residence,"Last Residence:\\s*")) %>% 
  mutate(Occupation=str_extract(Info,"Occupation:\\s*([^\\n]+)")) %>% 
  mutate(Occupation=str_remove(Occupation,"Occupation:\\s*")) %>% 
  mutate(Class = str_extract(Info, "(?<=\\n)[^\\n]+ Class")) %>%                # Finder sidste linje før "Class"
  mutate(Class = str_remove(Class, " Class")) %>%                               # Fjerner selve " Class" 
  mutate(Embarked=str_extract(Info,"Embarked:\\s*([^\\n]+)")) %>% 
  mutate(Embarked=str_remove(Embarked,"Embarked:\\s*")) %>% 
  mutate(Destination=str_extract(Info,"Destination:\\s*([^\\n]+)")) %>% 
  mutate(Destination=str_remove(Destination,"Destination:\\s*")) %>% 
  mutate(Disembarked=str_extract(Info,"Carpathia:\\s*[^\\n]+")) %>% 
  mutate(Disembarked=str_remove(Disembarked,"Carpathia:\\s*")) %>% 
  mutate(Ticket_No=str_extract(Info,"Ticket\\s*([^\\n]+)")) %>% 
  mutate(Ticket_No=str_remove(Ticket_No,"Ticket No.\\s*")) %>% 
  mutate(Ticket_No = str_extract(Ticket_No, "^[^ ]+(?: [^ ]+)*")) %>%           # Matcher alt indtil to mellemrum
  mutate(Ticket_price=str_extract(Ticket_No,"£\\d+")) %>% 
  mutate(Ticket_No=str_remove(Ticket_No,", £\\d+")) %>% 
  mutate(Cabin_No=str_extract(Info,"Cabin No.\\s*([^\\n]+)")) %>% 
  mutate(Rescued=str_extract(Info,"Rescued\\s*([^\\n]+)")) %>% 
  mutate(Rescued = str_remove_all(Rescued, "\\.fa-secondary\\{opacity:\\.4\\}\\s*")) %>% 
  mutate(Boat=str_extract(Rescued,"\\(([^)]+)\\)")) %>% 
  mutate(Died=str_extract(Info,"Died\\s*([^\\n]+)")) %>% 
  mutate(Died=str_remove(Died,"Died\\s*")) %>% 
  mutate(Death_Cause=str_extract(Info,"Cause of Death:\\s*([^\\n]+)")) %>% 
  mutate(Death_Cause=str_remove(Death_Cause,"Cause of Death:\\s*")) %>% 
  mutate(Died=str_remove(Died,":\\s*")) %>% 
  mutate(Body=str_extract(Info,"Body\\s*([^\\n]+)")) %>% 
  mutate(Body=str_remove(Body,"Body \\s*")) %>% 
  mutate(Buried=str_extract(Info,"Buried:\\s*([^\\n]+)")) %>% 
  mutate(Buried=str_remove(Buried,"Buried:\\s*"))


#manuelt giver vi vores double james kelly ekstra identifikation til colnames
scrapedTitanic[526,3] <- "Mr James Kelly (young)"
scrapedTitanic[527,3] <- "Mr James Kelly (old)"
#vi fjerner også Carl Olaf Jansson, da han ikke har summery på: https://www.encyclopedia-titanica.org//titanic-survivor/carl-olof-jansson.html
#potentielt fylde ham ud manuelt da dataen står på hans side.
scrapedTitanic <- scrapedTitanic[-1209,]
names <- as.factor(scrapedTitanic$Name)
rownames(scrapedTitanic) <- names

#Omstrukturer cols
scrapedTitanic <- scrapedTitanic[, c("Name", "Sex", "Age", "isCHILD", "Born", 
                                     "Nationality", "Marital_Status", "Last_Residence", "Destination",
                                     "Occupation", "Class", "Cabin_No", "Embarked", "Disembarked", 
                                     "Rescued", "Boat", "Died", "Death_Cause", "Body", "Buried", 
                                     "Ticket_No", "Ticket_price","URL", "Info")]
#Export
saveRDS(scrapedTitanic,"DIN_PATH_HER")
saveRDS(scrapedTitanic,"Documents/DAL-Projects/2.semester/flow2/titanic/StringR_Titanic.RDS") #<- min path


