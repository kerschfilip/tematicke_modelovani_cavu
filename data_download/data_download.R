# script:
# - stáhne z Krameria identifikátory a základní metadata pro vytvoření přehledu o titulu periodika, počtu ročníků, čísel (svazků) a stran 
# - tato data uloží do JSON souboru v pracovním adresáři
# - v průběhu zjišťování struktury titulu zjistí dostupnost data streamů TEXT_OCR a ALTO
# - streamy TEXT_OCR z každé strany stáhne a uloží do pracovního adresáře do složky pod UUID titulu. Vznikne vždy jeden soubor pro jeden svazek (číslo)

library(jsonlite)
library(httr)

kramerius5_api <- "https://kramerius.lib.cas.cz/search/api/v5.0/item/" # API systému Kramerius, verze 5
#kramerius7_api <- "https://kramerius.lib.cas.cz/search/api/client/v7.0/items/" # API systému Kramerius, verze 7

current_working_directory <- getwd() # nastavený pracovní adresář

# identifikátory UUID titulní úrovně periodika z DKAV
#UUID_titul <- "uuid:ae767057-435d-11dd-b505-00145e5790ea" # Rozpravy 1. třídy
#UUID_titul <- "uuid:ae767058-435d-11dd-b505-00145e5790ea" # Rozpravy 2. třídy
UUID_titul <- "uuid:ae74754a-435d-11dd-b505-00145e5790ea" # Rozpravy 3. třídy


# funkce pro získání všech informací o přímých potomcích objektu
getChildren <- function(uuid){
    request <- paste0(kramerius5_api, uuid, "/children") # dotázání se na endpoint /children Krameria 5
    #request <- paste0(kramerius7_api, uuid, "/info/structure") # dotázání se na endpoint /info/structure Krameria 7 
    response <- fromJSON(request) # stažení dat v JSON
    return(response)
}  

# funkce pro stažení datastreamu
getDataStream <- function(uuid, streams){
  request <- paste0(kramerius5_api, uuid, "/streams/", streams) # dotázání se na konkrétní data stream v Kramerius 5
  #request <- paste0(kramerius7_api, uuid, "/ocr/", streams) # dotázání se na konkrétní data stream v Kramerius 7
  response <- GET(request)
  
  if (response$status == 404){ # data stream neexistuje
    data_stream <- "404"
  }else if (response$status == 403){ # chybí oprávnění ke čtení data streamu
    data_stream <- "private"
  }else if (response$status == 200){
    data_stream <- content(response)
  }else{
    data_stream <- "error"
    print(data_stream)
  }
  return(data_stream)
}

#Vytvoření přehledu ročníků
getVolumeInfo <- function(uuid){
  # vytvoření data frame
  columns = c("volume_uuid", "volume_no", "volume_year")
  volume_df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(volume_df) = columns
  
  response_title <- getChildren(uuid) # získá informace o potomcích titulu
  
  # pro každého potomka se uloží do dataframu uuid, číslo ročníku, rok vydání ročníku  
  for (i in 1:length(response_title$pid)) {
    new_row <- data.frame(volume_uuid = response_title$pid[i], 
                          volume_no = response_title$details$volumeNumber[i], 
                          volume_year = response_title$details$year[i])
    volume_df <- rbind(volume_df, new_row)
  }
  return(volume_df)
}

# Vytvoření přehledu svazků (čísel) v jednotlivých ročnících
getIssueInfo <- function(volume_uuid, volume_year){
  # Vytvoření data frame pro čísla
  issue_df <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(issue_df) <- c("issue_uuid", "issue_name", "pages")
  
  response_volume <- getChildren(volume_uuid) # získá informace o potomcích ročníku
  
  #pro každého potomka (číslo) získá jeho identifikátor (issue_uuid), číslo (issue_no) a identifikátory a čísla stran
  for (i in 1:length(response_volume$pid)) {
    issue_uuid <- response_volume$pid[i]
    
    # podmínka pro případ, že v metadatech není číslo vyplněno
    if (length(response_volume$details$partNumber) >= i && !is.null(response_volume$details$partNumber[i])) {
      issue_partNumber <- response_volume$details$partNumber[i]
    } else {
      issue_partNumber <- NA
    }
    
    pages_list <- getPageIdentifiers(volume_uuid, volume_year, issue_uuid, issue_partNumber)
    
    issue_new_row <- data.frame(issue_uuid = issue_uuid, 
                          issue_name = issue_partNumber, 
                          pages = I(list(pages_list)))
    issue_df <- rbind(issue_df, issue_new_row)
  }
  return(issue_df)
}

# Stažení identifikátorů a názvů stran jednotlivých čísel
getPageIdentifiers <- function(volume_uuid, volume_year, issue_uuid, issue_no){
  response_issue <- getChildren(issue_uuid) # získá informace o potomcích čísla (stranách)
  pages_list <- list() # připraví list pro uložení dat
  
  TEXT_OCR_issue <- ""
  
  for (i in 1:length(response_issue$pid)) { # pro každý ročník zjistí uuid strany a číslo (název) strany
    page_uuid <- response_issue$pid[i]
    page_number <- ifelse(!is.null(response_issue$title) && length(response_issue$title) >= i, 
                          response_issue$title[i], 
                          NA)
    
    TEXT_OCR_page <- getDataStream(page_uuid, "TEXT_OCR") #pro K5
    #TEXT_OCR_page <- getDataStream(page_uuid, "text") #pro K7
    
    if(TEXT_OCR_page == "404" | TEXT_OCR_page == "private" | TEXT_OCR_page == "error"){
      TEXT_OCR_exists <- 0
      print(paste("TEXT_OCR pro stranu",page_number, "nedostupné"))
    }else{
      TEXT_OCR_exists <- 1
      print(paste("stahuji TEXT_OCR pro stranu", page_number, "čísla", issue_uuid))
      TEXT_OCR_issue <- paste(TEXT_OCR_issue, TEXT_OCR_page)
    }
    
    ALTO_page <- getDataStream(page_uuid, "ALTO") #pro K5 i K7 stejné
    
    if(any(ALTO_page == "404" | ALTO_page == "private" | ALTO_page == "error")){
      ALTO_exists <- 0
      print(paste("ALTO pro stranu",page_number, "---NEdostupné---"))
    }else{
      ALTO_exists <- 1
      print(paste("ALTO pro stranu",page_number, "+++DOSTUPNÉ+++"))
    }
    
    # zapíše zjištěná data do seznamu
    pages_list[[i]] <- list(page_uuid = page_uuid, page_number = page_number, TEXT_OCR_exists = TEXT_OCR_exists, ALTO_exists = ALTO_exists) 
  }
  
  # Vytvoří složku s názvem uuid_titul
  dir_name <- gsub(":", "_", UUID_titul) # název složky podle UUID_titul
  dir_path <- file.path(current_working_directory, dir_name) # umístění složky v pracovním adresáři
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  # pro každé číslo uloží TEXT_OCR do jednoho .txt souboru
  TEXT_OCR_file_name <- paste0(dir_path, "/TEXT_OCR_vol_", volume_year, "_No_", issue_no, "_",substr(issue_uuid,6,41), ".txt")
  write.csv(TEXT_OCR_issue, file = TEXT_OCR_file_name)
  print(paste("vytvořen soubor",TEXT_OCR_file_name))
  
  return(pages_list)
}

#----

volume_df <- getVolumeInfo(UUID_titul) # vytvoří přehled ročníků (potřeba doplnit UUID titulu)
print(paste("Vytvořen přehled ročníků pro titul",UUID_titul, "Počet ročníků: ",length(volume_df$volume_uuid)))

volume_df$issues <- vector("list", nrow(volume_df)) # připraví sloupec pro data frame s přehledem čísel pod jednotlivými ročníky
for (i in 1:length(volume_df$volume_uuid)){ # iteracemi se sloupec s přehledem čísel vyplní
  issue_df <- getIssueInfo(volume_df$volume_uuid[i], volume_df$volume_year[i])
  volume_df$issues[i] <- list(issue_df) 
  print(paste("zapisuji přehled čísel do ročníku ve volume_df$issues[",i,"]" ))
}

# Uložení získaných dat do JSON souboru
json_filename <- paste0(substr(UUID_titul,6, 41),"_prehled.json") # název souboru
json_file_path <- file.path(current_working_directory, json_filename)
jsonlite::write_json(volume_df, json_file_path, pretty = TRUE) # uložení
print(paste("Přehled uložen do souboru:", json_file_path))