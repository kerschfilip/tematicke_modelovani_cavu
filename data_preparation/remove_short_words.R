# script projde všechny txt soubory v nastavené složce a smaže z nich slova dlouhá 2 znaky nebo méně
# nový TXT soubor uloží do stejné složky se stejným jménem, ale prefixem "PREPARED"


current_working_directory <- getwd() # nastavený pracovní adresář, předpokládá se adresář, kde je uložen tento script
#folder_path <- file.path(current_working_directory, "rozpravy_trida_3") # cesta ke složce s TXT soubory
folder_path <- file.path(current_working_directory, "test") # cesta ke složce s TXT soubory


# Funkce odstraňující slova kratší než 2 znaky
remove_short_words <- function(line) {
  words <- unlist(strsplit(line, " "))
  long_words <- words[nchar(words) > 2] # stanovení hranice 2 znaky
  return(paste(long_words, collapse = " "))
}

prepare_data <- function(full_file_path){
  text <- readLines(full_file_path) # čte věty z TXT souboru
  processed_text <- sapply(text, remove_short_words) # aplikuje funkci remove_short_words na každý řádek
  
  # Uložení do nového txt
  base_file_name <- basename(full_file_path)
  txt_file_name <- paste0("PREPARED_",base_file_name,".txt") # název souboru
  txt_file_path <- file.path(folder_path, txt_file_name)
  writeLines(processed_text, txt_file_path)
  print(paste("Uložen zkrácený soubor",txt_file_path))
}

processAllTxtFiles <- function() {
  txt_files <- list.files(path = folder_path, pattern = "^TEXT.*\\.txt$") # seznam TXT souborů začínajících na "TEXT" ve složce
  print(paste("Zkrátí se:", length(txt_files), "txt souborů ze složky:", folder_path))
  
  for (file in txt_files) {
    full_file_path <- file.path(folder_path, file) # celá cesta každého TXT souboru
    prepare_data(full_file_path) # zprocesování každého souboru
  }
}

processAllTxtFiles()
