library(rvest)
library(httr)
library(stringr)
library(dplyr)
# originalt blev vi bedt om at finde navnene på passagererne som er mellem k-o, men vi tog alle vi kunne 
library(rvest)
library(httr)
library(stringr)

url <- "https://www.encyclopedia-titanica.org/titanic-passenger-list/"
headers <- c(
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  "Accept-Language" = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  "Cache-Control" = "max-age=0",
  "Cookie" = "__cmpcc=1; _ga=GA1.1.1756057943.1741612404; __cmpconsentx86685=CQODSZgQODSZgAfKtBDABgFsAP_gAEPgAAigJ0pF9C5cRSFD4Gp3IJoEIAUHwFBAYEAgBgQBA4ABSBKAMIwEgAAAAACIIAAAAAAAIAAAAAAACAgAAEAAQIAAIQAIAEAAAAACIAAAAAABAAAAAAgAAAAgEAAAAAAAAAAAgAAAAAIAQEgAAAAAAQAAIAAAAAAAAAAAAAAAQAAAQAAIAKgAAAAAAAAAAAAACBAAAAAAAAAAAAAAAQAAAAACCdMDIABAACwAHAAVAAuABwADwAIAASAAqABkADQAHAAPAAiABHACYAFUANAAegA_ACEAEcAJwAVoAwABlADRAHIAOcAdwA_YCDgIQARYAn4BrwDiAHUAO2Ae0A_4CYgFDgKlAXmAyQBlgD5AH7gTpAAA.f_wACHwAAAA; __cmpcccx86685=aCQOFPaZgAAFlpYEyoyqsKGQE1I0RDIkswAUoSkY; _lr_env_src_ats=false; _scor_uid=95611a7dc6e5424a88497992d798a10f; panoramaId_expiry=1742217206010; _cc_id=364e06ceca377e8ac5d59007ca6bd7db; panoramaId=222956d6439cf719343c60b73498185ca02c59e91faa8eab80d668879ec0ecb9; sib_cuid=a8b9ed00-8b9b-40d1-8bf8-fa0b766de97f; po_visitor=QKPCqUL2Kozu; xf_csrf=lE-X3JZIRWBY62VK; xf_user=223038%2C2uQb4VuvuwjnBXky8p3ABFLFbzpfcqY_wBGeuBIQ; xf_session=zfKQhrI71OKsAcX8VEae8TtAxmJrhZHn; etli=jbnlRrdgb9UL987X4NAQkFxKnWm0g6hsUpK; etsc=sjcvfhskjdljeljdjeajfj7uh; __qca=I0-1386231568-1741942272542; sbjs_migrations=1418474375998%3D1; sbjs_current_add=fd%3D2025-03-14%2008%3A56%3A13%7C%7C%7Cep%3Dhttps%3A%2F%2Fwww.encyclopedia-titanica.org%2Fstore%2Fproduct%2Ftitanic-deckplans%2F%7C%7C%7Crf%3Dhttps%3A%2F%2Fwww.encyclopedia-titanica.org%2F; sbjs_first_add=fd%3D2025-03-14%2008%3A56%3A13%7C%7C%7Cep%3Dhttps%3A%2F%2Fwww.encyclopedia-titanica.org%2Fstore%2Fproduct%2Ftitanic-deckplans%2F%7C%7C%7Crf%3Dhttps%3A%2F%2Fwww.encyclopedia-titanica.org%2F; sbjs_current=typ%3Dtypein%7C%7C%7Csrc%3D%28direct%29%7C%7C%7Cmdm%3D%28none%29%7C%7C%7Ccmp%3D%28none%29%7C%7C%7Ccnt%3D%28none%29%7C%7C%7Ctrm%3D%28none%29%7C%7C%7Cid%3D%28none%29%7C%7C%7Cplt%3D%28none%29%7C%7C%7Cfmt%3D%28none%29%7C%7C%7Ctct%3D%28none%29; __stripe_mid=fb3ad2ce-d59e-4651-8a02-f32c1172d719edff8c; sbjs_udata=vst%3D2%7C%7C%7Cuip%3D%28none%29%7C%7C%7Cuag%3DMozilla%2F5.0%20%28Macintosh%3B%20Intel%20Mac%20OS%20X%2010_15_7%29%20AppleWebKit%2F537.36%20%28KHTML%2C%20like%20Gecko%29%20Chrome%2F133.0.0.0%20Safari%2F537.36; __gads=ID=a6884eaa993e662a:T=1741612406:RT=1741951394:S=ALNI_MYTA3ZmXwv0ee6I1UgouNKzMd9-1w; __gpi=UID=00001058ea700fa4:T=1741612406:RT=1741951394:S=ALNI_MabLxGMo7_MTnDFRWzThZHbCFTqGA; __eoi=ID=ca7c88f1af652660:T=1741612406:RT=1741951394:S=AA-AfjZGmzKlBb1RPMVFYW2P2Cka; cf_clearance=UI5BuDNZrYQzwt7yF2QijXAfErch.JI40b3Hh6Pijj0-1741951627-1.2.1.1-OIVHImvQx.ZzHC8Iyb4jCvQx3BcFg150SZtYWA5Ycgxi522A3oJ83SaJDUiq7wySJFX_g3v08MeBd_PIOte4KQZSJRQyYjOqQEnB7Ux7iQwDmZ71AjltrKA1bjlaVDUIGYKqL6d1VsjffLXntGY.6q0dsFFkFRcN_mOZQT2_ZFWH4jnVw.0pGVS3675thUwdbZnxn5QD0numKhrm5UL.4tKS6xr0qiEnmVZcFn3_pae65H5N3wDp6V3p9Aoyj9R92cLsoTmicv7DCnQdPoeCmbMZNh.by6nJL443ZOqPCUhUjtibi0X12WiUs0XLjMX_jZNtKBuKZ6bp3Y28gXclxpH0ImRWQKtzaMD5icUri5E",
  "Priority" = "u=0, i",
  "Referer" = "https://www.encyclopedia-titanica.org/titanic-passenger-list/",
  "Sec-CH-UA" = '"Not(A:Brand";v="99", "Google Chrome";v="133", "Chromium";v="133")',
  "Sec-CH-UA-Mobile" = "?0",
  "Sec-CH-UA-Platform" = '"macOS"',
  "Sec-Fetch-Dest" = "document",
  "Sec-Fetch-Mode" = "navigate",
  "Sec-Fetch-Site" = "same-origin",
  "Sec-Fetch-User" = "?1",
  "Upgrade-Insecure-Requests" = "1",
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36"
)

response <- GET(url, add_headers(.headers = headers))
page <- read_html(response)
links <- as.data.frame(page %>% html_node("tbody") %>% html_nodes("td") %>%  html_nodes("a") %>% html_attr("href"))
colnames(links) <- "links"

victims <- links[grepl("/titanic-victim/", links$links), , drop = FALSE]
survivors <- links[grepl("/titanic-survivor/",links$links),,drop=F]

allpassangers <- rbind(victims,survivors)


### big scrape ###
#allpassangers <- allpassangers[1,]
allpassangers <- as.factor(allpassangers$links)
#url + heder tilføjelser/sammensætning
Titanic <- "https://www.encyclopedia-titanica.org"
username <- "dit_brugernavn"
password <- "din_kode"

#tom liste - fyld den bitch up yo
passenger_data <- list()

for (i in allpassangers) {
  url <- paste0(Titanic, i)
  
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

head(passenger_df)

#gem den til stringer-manipulation
#saveRDS(passenger_df,"Documents/DAL-Projects/2.semester/flow2/titanic/SCRAPEDTITANIC.RDS")

##### den endelige scrape - mangler struktur og stringextract's ####

scrapedTitanic <- readRDS("Documents/DAL-Projects/2.semester/flow2/titanic/SCRAPEDTITANIC.RDS") #<- min path
scrapedTitanic <- readRDS("DIN_PATH_HER")

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


