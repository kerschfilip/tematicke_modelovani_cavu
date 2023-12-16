# script postupně odešle všechny txt soubory v nastavené složce na obohacení do nástroje UDPipe na infrastrukturu LINDAT/CLARIN

library(jsonlite)
library(udpipe)
library(dplyr)
current_working_directory <- getwd() # nastavený pracovní adresář
#folder_path <- file.path(current_working_directory, "test") # cesta ke složce s TXT soubory
#folder_path <- file.path(current_working_directory, "rozpravy_trida_2") # cesta ke složce s TXT soubory
folder_path <- file.path(current_working_directory, "test") # cesta ke složce s TXT soubory

# nastaví příkat curl
setCurl <- function(file_name){
  downloaded_file_name <- file_name #funkci mohou vadit spojovníky, pomlčky a mezery mezi nimi v názvu souborů, doporučuje se takové názvy změnit
  curl_command <- paste0("curl -F data=@",downloaded_file_name," -F tokenizer= -F output=conllu -F tagger= -F parser= http://lindat.mff.cuni.cz/services/udpipe/api/process")
  return(curl_command)
}

# pošle dotaz na API
postRequest <- function(curl_command){
  curl_output <- system(curl_command, intern = TRUE)

  # Kontrola stavu dotazu
  if (identical(curl_output[1], "Chyba při uploadu")) {
    cat("curl error :\n")
    cat(curl_output, sep = "\n")
  } else {
    response_content <- fromJSON(curl_output) #když je vše OK získá odpověď v JSON
  }
  return(response_content)
}

# Vytvoří data frame z API odpovědi a uloží
saveConllu <- function(response_content, file_name){
  base_file_name <- basename(file_name)
  df <- udpipe_read_conllu(textConnection(response_content$result))
  df_filename <- paste0("CoNLLU_",base_file_name,".json") # název souboru
  df_file_path <- file.path(folder_path, df_filename)
  write_json(df, df_file_path, pretty = TRUE) # uložení + pretty print
  print(paste("Uložen soubor obsahující CoNLL-U", df_file_path))
  
  return(df)
}

# vyfiltruje podstatná jména z odpovědi
getNouns <- function(df, file_name){
  base_file_name <- basename(file_name)
  filtered_df <- df %>% filter(upos == "NOUN") # vyhledání pouze podstatných jmen
  noun_tokens <- filtered_df$lemma # pouze sloupec obsahující lemma

  # Uložení filtrovaných dat do txt
  txt_file_name <- paste0("NOUNS_",base_file_name,".txt") # název souboru
  txt_file_path <- file.path(folder_path, txt_file_name)
  writeLines(noun_tokens, txt_file_path)
  print(paste("Uložen soubor obsahujíc LEMMA",txt_file_path))
}

enrichFile <- function(file_name){
  print(paste("zpracovávám soubor", file_name))
  request <- setCurl(file_name)
  response <- postRequest(request)
  data <- saveConllu(response, file_name)
  getNouns(data, file_name)
}

processAllTxtFiles <- function() {
  txt_files <- list.files(path = folder_path, pattern = "^PREPARED_TEXT.*\\.txt$") # seznam TXT souborů začínajících na "PREPARED_TEXT" ve složce
  print(paste("Zpracuje se:", length(txt_files), "txt souborů ze složky:", folder_path))
  
  for (file in txt_files) {
    full_file_path <- file.path(folder_path, file) # celá cesta každého TXT souboru
    enrichFile(full_file_path) # zprocesování každého souboru
  }
}

processAllTxtFiles()
