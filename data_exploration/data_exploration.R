library(jsonlite)
library(tidyverse)
library(dplyr)
library(scales)
library(knitr)

# identifikátory UUID titulní úrovně periodika z DKAV
files <- list(
                  "ae767057-435d-11dd-b505-00145e5790ea_prehled_public.json", 
                  "ae767058-435d-11dd-b505-00145e5790ea_prehled_public.json", 
                  "ae74754a-435d-11dd-b505-00145e5790ea_prehled_public.json"
              )
# Názvy jednotlivých titulů pro zobrazení v grafu
journal_names <- list("I. třída", "II. třída", "III. třída")

# načtení dat
prepareData <- function(file_name){
  json_file_path <- file_name # název/umístění souboru
  data <- fromJSON(json_file_path)
  
# Počet čísel v ročnících
data$number_of_issues <- sapply(data$issues, nrow) 
  
# Počet stran v číslech
total_pages_per_volume <- numeric(nrow(data)) 
  for (i in 1:nrow(data)) {
    volume_total_pages <- 0
    for (j in 1:nrow(data$issues[[i]])) {
      pages_in_single_issue <- nrow(data$issues[[i]]$pages[[j]])
      volume_total_pages <- volume_total_pages + pages_in_single_issue
    }
    total_pages_per_volume[i] <- volume_total_pages
  }
  data$total_pages_per_volume <- total_pages_per_volume
  
#průměrný počet stran v čísle
average_issue_pages <- numeric(nrow(data))
data$average_issue_pages <- data$total_pages_per_volume%/%data$number_of_issues
  
# procentuální dostupnost TEXT_OCR
volume_TEXT_OCR_coverage <- numeric(nrow(data))
  for (k in 1:nrow(data)) {
    volume_TEXT_OCR_counter <- 0
    for (l in 1:nrow(data$issues[[k]])) {
      TEXT_OCR_coverage <- (data$issues[[k]]$pages[[l]]$TEXT_OCR_exists)
      TEXT_OCR_coverage <- as.numeric(TEXT_OCR_coverage)
      volume_TEXT_OCR_counter <- volume_TEXT_OCR_counter + TEXT_OCR_coverage
    }
    volume_TEXT_OCR_coverage[k] <- volume_TEXT_OCR_counter%/%nrow(data$issues[[k]])
  }
  data$volume_TEXT_OCR_coverage <- percent(volume_TEXT_OCR_coverage)

# procentuální dostupnost ALTO
volume_ALTO_coverage <- numeric(nrow(data))
  for (m in 1:nrow(data)) {
    volume_ALTO_counter <- 0
    for (n in 1:nrow(data$issues[[k]])) {
      ALTO_coverage <- (data$issues[[k]]$pages[[l]]$ALTO_exists)
      ALTO_coverage <- as.numeric(ALTO_coverage)
      volume_ALTO_counter <- volume_ALTO_counter + ALTO_coverage
    }
    volume_ALTO_coverage[k] <- volume_ALTO_counter%/%nrow(data$issues[[k]])
  }
  data$volume_ALTO_coverage <- percent(volume_ALTO_coverage)
  
  return(data)
}

# vytvořit matici číslo - rok vydání
getIssuePublicationYear <- function(data, class){
  # vytvořit data frame
  issues_publication_year <- data.frame(
    issue_uuid = character(),
    issue_name = character(),
    volume_uuid = character(),
    volume_year = numeric(),
    class = character()
  )
    
  for (o in 1:nrow(data)){
    for (p in 1:nrow(data$issues[[o]])){
      # získat data pro každou proměnnou
      issue_uuid <- data$issues[[o]]$issue_uuid[[p]]
      issue_name <- data$issues[[o]]$issue_name[[p]]
      volume_uuid <- data$volume_uuid[[o]]
      volume_year <- data$volume_year[[o]]
      class <- class
      
      # přidat nový řádek do data framu
      issues_publication_year <- rbind(issues_publication_year, data.frame(issue_uuid, issue_name, volume_uuid, volume_year, class, stringsAsFactors = FALSE))
    }    
  }
  return(issues_publication_year)
}  


# Vypočítat souhrn
getSummary <- function(data){
  sum_number_of_issues <- sum(data$number_of_issues)
  sum_total_pages_per_volume <- sum(data$total_pages_per_volume)
  avg_average_issue_pages <- mean(data$average_issue_pages)
  
  # shrnutí
  data_summary <- data.frame(
    volume_uuid = "---------",
    volume_no = "Shrnutí",
    volume_year = "---------",
    issues = "---------",
    number_of_issues = sum_number_of_issues,
    total_pages_per_volume = sum_total_pages_per_volume,
    average_issue_pages = avg_average_issue_pages,
    volume_TEXT_OCR_coverage = "---------",
    volume_ALTO_coverage = "---------"
  )
  return(data_summary)
}

# Spojení dataframů pro jednotlivé třídy
combineData <- function(data, data_summary){
  combined_data <- rbind(data, data_summary)
  return(combined_data)
}



# Tabulka pro přehled dostupnosti dat
getDataTable <- function(data){
  # výběr sloupců
  selected_data <- data %>% 
    select(volume_no, volume_year, number_of_issues,average_issue_pages, total_pages_per_volume, volume_TEXT_OCR_coverage, volume_ALTO_coverage)
  # vytvoření tabulky a pojmenování sloupců
  data_table <- kable(selected_data, 
                      col.names = c("Číslo ročníku", "Rok vydání", "Počet čísel", "Průměrný počet stran v čísle", "Celkový počet stran", "Dostupnost TEXT_OCR", "Dostupnost ALTO")
                      )
  return(data_table)
}

# graf čísel ročníků
getIssuesGraph <- function(data, journal_name){
  xlabels <- sort(unique(data$volume_year))
  xlabels[seq(2, length(xlabels), 2)] <- ""
  issues_graph <- ggplot(data, aes(x = volume_year, y = number_of_issues)) +
    geom_bar(stat = "identity", width = 0.5) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 60) + # fixní nastavení osy y
    xlab("Rok publikování") +
    ylab("Počet svazků") +
    scale_x_discrete(labels = xlabels)+
    #ggtitle("Počet publikovaných čísel v jednotlivých letech")
    ggtitle(journal_name)
  return(issues_graph)
}

#graf stran
getPageGraph <- function(data, journal_name){
  xlabels <- sort(unique(data$volume_year))
  xlabels[seq(2, length(xlabels), 2)] <- ""
  page_graph <- ggplot(data, aes(x = volume_year, y = total_pages_per_volume)) +
    geom_bar(stat = "identity", fill = "#12448B", width = 0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1500) + # fixní nastavení osy y
    xlab("Rok publikování") +
    ylab("Počet stran") +
    scale_x_discrete(labels = xlabels)+
    #ggtitle("Počet stran publikovaných v jednotlivých letech")
    ggtitle(journal_name)
    return(page_graph)
}

# kombinovaný graf
getCombinedGraph <- function(data, journal_name) {
  max_issues <- max(data$number_of_issues, na.rm = TRUE) # maximální počet čísel vydaných v jednom roce
  max_pages <- max(data$total_pages_per_volume, na.rm = TRUE) # maximální počet stran publikovaných v jednom roce
  scale_factor <- max_pages / max_issues # nastavení poměru mezi osami
  
  # přidá scale_factor do data framu
  data$scaled_total_pages <- data$total_pages_per_volume / scale_factor
  # pro seřazení
  data$volume_year <- as.factor(data$volume_year)
  
  #graf
  combined_graph <- ggplot(data, aes(x = volume_year)) +
    geom_bar(aes(y = number_of_issues), stat = "identity", fill = "gray", position = position_dodge()) +
    geom_line(aes(y = scaled_total_pages), group = 1, color = "#12448B", size = 1) +
    scale_y_continuous(
      "Počet čísel",
      sec.axis = sec_axis(~ . * scale_factor, name = "Počet stran")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Rok publikování") +
    ylab("Počet čísel / Počet stran") +
    #ggtitle("Porovnání počtu publikovaných čísel s počtem stran")
    ggtitle(journal_name)
  
  return(combined_graph)
}

# Rozpravy 1
data1 <- prepareData(toString(files[1]))
data_summary1 <- getSummary(data1)
combined_data1 <- combineData(data1, data_summary1)

table_view1 <- getDataTable(data1)
table_summary1 <- getDataTable(combined_data1)

issues_graph1 <- getIssuesGraph(data1, journal_names[[1]])
page_graph1 <- getPageGraph(data1, journal_names[[1]])
combined_graph1 <- getCombinedGraph(data1, journal_names[[1]])

issues_publication_year1 <- getIssuePublicationYear(data1, "1")

# Rozpravy 2
data2 <- prepareData(toString(files[2]))
data_summary2 <- getSummary(data2)
combined_data2 <- combineData(data2, data_summary2)

table_view2 <- getDataTable(data2)
table_summary2 <- getDataTable(combined_data2)

issues_graph2 <- getIssuesGraph(data2, journal_names[[2]])
page_graph2 <- getPageGraph(data2, journal_names[[2]])
combined_graph2 <- getCombinedGraph(data2, journal_names[[2]])

issues_publication_year2 <- getIssuePublicationYear(data2, "2")

# Rozpravy 3
data3 <- prepareData(toString(files[3]))
data_summary3 <- getSummary(data3)
combined_data3 <- combineData(data3, data_summary3)

table_view3 <- getDataTable(data3)
table_summary3 <- getDataTable(combined_data3)

issues_graph3 <- getIssuesGraph(data3, journal_names[[3]])
page_graph3 <- getPageGraph(data3, journal_names[[3]])
combined_graph3 <- getCombinedGraph(data3, journal_names[[3]])

issues_publication_year3 <- getIssuePublicationYear(data3, "3")

# Shrnutí všech dostupných dat:
full_summary <- rbind(data_summary1, data_summary2, data_summary3)

  # Výpočet shrnutí
  sum_total_pages <- sum(full_summary$total_pages_per_volume, na.rm = TRUE) # celkový počet stran
  sum_issues <- sum(full_summary$number_of_issues, na.rm = TRUE) # celkový počet čísel
  sum_avg_issue_pages <- mean(full_summary$average_issue_pages, na.rm = TRUE) # průměrný počet stran
  
  # Přejmenování řádků pro identifikaci jednotlivých tříd:
  full_summary$volume_no[[1]] <- "Rozpravy 1. třídy"
  full_summary$volume_no[[2]] <- "Rozpravy 2. třídy"
  full_summary$volume_no[[3]] <- "Rozpravy 3. třídy"
  
  # vytvoření nového sloupce pro procentuální vyjádření stran
  for (r in 1:length(full_summary$volume_uuid)){
    full_summary$pages_percent[[r]] <- percent(full_summary$total_pages_per_volume[[r]]/sum_total_pages)
  }
  
  # Vytvoření nového řídku do full_summary
  summary_row <- data.frame(
    volume_uuid = NA,
    volume_no = "Shrnutí", 
    volume_year = "---------",
    issues = NA,
    number_of_issues = sum_issues,
    average_issue_pages = sum_avg_issue_pages,
    total_pages_per_volume = sum_total_pages,
    volume_TEXT_OCR_coverage = "---------",
    volume_ALTO_coverage = "---------",
    pages_percent = percent(1)
  )
  full_summary <- rbind(full_summary, summary_row) # Připojení nového řádku

  # Vytvoření tabulky:
  selected_summary <- full_summary %>% 
    select(volume_no, number_of_issues,average_issue_pages, total_pages_per_volume, pages_percent)
  
  full_summary_table <- kable(selected_summary, 
                                            col.names = c("Rozpravy", "Počet čísel", "Průměrný počet stran v čísle", "Celkový počet stran", "Část korpusu"), 
                                            caption = "Přehled dat")

# uložení společné matice číslo - rok vydání pro všechny:
combined_issues_publication_year <- rbind(issues_publication_year1, issues_publication_year2, issues_publication_year3) # spojení dat
json_data <- toJSON(combined_issues_publication_year, pretty = TRUE) # transformace do JSON
write(json_data, file = "combined_issues_publication_year.json") # uložení

