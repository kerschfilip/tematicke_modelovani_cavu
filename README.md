# Postup stažení, analýzy, přípravy dat a modelování témat časopisu _Rozpravy_ České Akademie v letech 1890-1910

Repozitář představuje přílohou k diplomové práci s názvem "Tematické modelování publikační činnosti České akademie věd a umění v letech 1890–1910". Součástí je popis postupu stažení, analýzy a přípravy dat a samotného modelování témat, které bylo provedeno v rámci výzkumné části práce.

Veškerá data jsou dostupná v repozitáři Zenodo: https://doi.org/10.5281/zenodo.10395970

**Reference diplomové práce**

KERSCH, Filip. Tematické modelování publikační činnosti České akademie věd a umění v letech 1890–1910. Online. Praha, 2023. Diplomová práce. Univerzita Karlova. Filozofická fakulta. Ústav informačních studií a knihovnictví. Vedoucí práce Jindřich Marek. Dostupné z: http://hdl.handle.net/20.500.11956/188370

---

Struktura a popis adresáře datasetu:

```r
#model/
#|-- README.Rmd
#|-- README.md
#|-- README.html
#|-- data_download
#|   \-- data_download.R #slouží ke stažení dat z digitální knihovny
#|   \-- uuid_ae767058-435d-11dd-b505-00145e5790ea #stažené textové přepisy Rozprav I. třídy
#|           \-- TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s textovým přepisem
#|   \-- uuid_ae767057-435d-11dd-b505-00145e5790ea #stažené textové přepisy Rozprav II. třídy
#|           \-- TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s textovým přepisem
#|   \-- uuid_ae74754a-435d-11dd-b505-00145e5790ea #stažené textové přepisy Rozprav III. třídy
#|           \-- TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s textovým přepisem
#|   \-- ae767058-435d-11dd-b505-00145e5790ea_prehled.json #struktura a metadata Rozprav I. třídy
#|   \-- ae767057-435d-11dd-b505-00145e5790ea_prehled.json #struktura a metadata Rozprav II. třídy
#|   \-- ae74754a-435d-11dd-b505-00145e5790ea_prehled.json #struktura a metadata Rozprav III. třídy
#|-- data_exploration
#|   \-- data_exploration.R #slouží k vytvoření přehledu o stažených datech
#|   \-- data_exploration_report.Rmd #formátování reportu s přehledem o stažených datech
#|   \-- data_exploration_report.html #report o stažených datech ve formátu HTML
#|   \-- ae767058-435d-11dd-b505-00145e5790ea_prehled_public.json #struktura a metadata Rozprav I. třídy (1890-1910)
#|   \-- ae767057-435d-11dd-b505-00145e5790ea_prehled_public.json #struktura a metadata Rozprav II. třídy (1890-1910)
#|   \-- ae74754a-435d-11dd-b505-00145e5790ea_prehled_public.json #struktura a metadata Rozprav III. třídy (1890-1910)
#|   \-- combined_issues_publication_year.json #společná matice číslo - rok vydání pro všechny Rozpravy
#|-- data_preparation
#|   \-- remove_short_words.R #slouží k odstranění krátkých slov
#|   \-- send_to_udpipe.R #slouží k obohacení dat nástrojem UDPipe2
#|   \-- rozpravy_trida_1 #upravené textové přepisy Rozprav I. třídy
#|           \-- TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s textovým přepisem
#|           \-- PREPARED_TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #zkrácený soubor s textovým přepisem
#|           \-- NOUNS_PREPARED_TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s podstatnými jmény z textového přepisu
#|   \-- rozpravy_trida_2 #upravené textové přepisy Rozprav II. třídy
#|           \-- TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s textovým přepisem
#|           \-- PREPARED_TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #zkrácený soubor s textovým přepisem
#|           \-- NOUNS_PREPARED_TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s podstatnými jmény z textového přepisu
#|   \-- rozpravy_trida_3 #upravené textové přepisy Rozprav III. třídy
#|           \-- TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s textovým přepisem
#|           \-- PREPARED_TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #zkrácený soubor s textovým přepisem
#|           \-- NOUNS_PREPARED_TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s podstatnými jmény z textového přepisu
#|-- topic_model
#|   \-- LDA.Rmd #dokument popisující spuštění LDA a vizualizaci výsledků
#|   \-- LDA.html #dokument popisující spuštění LDA a vizualizaci výsledků ve formátu HTML
#|   \-- combined_issues_publication_year.json #společná matice číslo - rok vydání pro všechny Rozpravy
#|   \-- data #složka s daty
#|           \-- LDA_PREPARED_NOUNS_PREPARED_TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt #soubor s podstatnými jmény připravený na vstup LDA
#|   \-- klasifikace_temat.xlsx #přiřazení rozpoznaných témat ke klasifikaci FORD
#|   \-- topic_model.RData #uložené prostředí R se všemi proměnnými a funkcemi
#|-- slides #prezentace k představení diplomové práce
#\
```

## 1. Stažení dat z digitální knihovny

V softwarovém prostředí R byl vytvořen skript `/data_download/data_download.R`, který stáhne z Digitální knihovny AV ČR identifikátory a základní metadata pro vytvoření přehledu o titulu periodika, počtu ročníků, čísel a stran. Na vstupu předpokládá tento skript nastavení dvou proměnných:

```r
# API systému Kramerius (verze 5)
kramerius5_api <- "https://kramerius.lib.cas.cz/search/api/v5.0/item/"

# identifikátory UUID titulní úrovně periodika z výše nastaveného systému Kramerius
UUID_titul <- "uuid:ae767057-435d-11dd-b505-00145e5790ea" # Rozpravy 1. třídy
#UUID_titul <- "uuid:ae767058-435d-11dd-b505-00145e5790ea" # Rozpravy 2. třídy
#UUID_titul <- "uuid:ae74754a-435d-11dd-b505-00145e5790ea" # Rozpravy 3. třídy
```

Protože jednotlivé třídy ČAVU vydávaly každá "své" _Rozpravy_, byl tento skript použit 3x, vždy s jinak nastavenou proměnnou `UUID_titul`.

Stažená data skript uloží ve formátu JSON pro další analýzu. Pro každý titul je ve složce se skriptem vytvořen soubor `{UUID_titul}_prehled.json`. Uložená data zahrnují informace o všech ročnících a číslech, která se pod daným titulem periodika v digitální knihovně vyskytují. Protože cílem práce je analyzovat pouze publikační činnost v letech 1890-1910, byly ze vzniklých souborů ručně odstraněny informace o ročnících, které nejsou předmětem našeho zájmu. Upravené soubory byly uloženy do složky `/data_exploration` s názvem `{UUID_titul}_prehled_public.json`

V průběhu zjišťování struktury titulu je také ověřena dostupnost data streamů `TEXT_OCR` a `ALTO`. V případě, že je data stream `TEXT_OCR` u jednotlivých stran dostupný, je rovnou uložen. Pro každý svazek (číslo) vznikne jeden textový soubor kombinující textové přepisy všech stran v něm obsažených. Tento soubor je uložen do nově vytvořené složky v adresáři se skriptem. Adresář nese název odpovídající proměnné `UUID_titul`, jednotlivé soubory s textovými přepisy dodržují názvovou konvenci: `TEXT_OCR_vol_{rok vydání}_No_{číslo}_{uuid}.txt`

## 2. Analýza dat

Pro analýzu dat byl vytvořen skript `/data_exploration/data_exploration.R`. Ten předpokládá, že je uložen ve stejné složce jako soubor `{UUID_titul}_prehled.json` vzniklý při stahování dat (je tedy nutné ho přesunout). Názvy souborů je nutné vyplnit do proměnné `files`:

```r
files <- list(
                  "ae767057-435d-11dd-b505-00145e5790ea_prehled_public.json",
                  "ae767058-435d-11dd-b505-00145e5790ea_prehled_public.json",
                  "ae74754a-435d-11dd-b505-00145e5790ea_prehled_public.json"
              )
```

Skript je připraven pro analýzu všech třech titulů _Rozprav_.

V rámci analýzy je vytvořen:

- přehled počtu svazků (čísel) v jednotlivých ročnících,
- přehled počtu stran v jednotlivých svazcích (číslech),
- průměrný počet stran v jednotlivých svazcích (číslech),
- přehled dostupnosti textového přepisu (TEXT_OCR) na úrovni ročníků,
- přehled dostupnosti ALTO na úrovni ročníků,
- souhrn dostupných dat pro každý titul v tabulce,
- graf ukazující počet svazků (čísel) publikovaných v jednotlivých letech,
- graf ukazující počet stran publikovaných v jednotlivých letech,
- graf kombinující počty publikovaných svazků a stran,
- přehled svazků vydaných v jednotlivých letech uložený do souboru `combined_issues_publication_year.json` (bude využit při analýze identifikovaných témat).

Přehledy vytvořené v rámci analýzy je možné si jednoduše prohlédnout pomocí připraveného reportu `data_exploration_report.Rmd`.

## 3. Příprava dat

Stažené textové přepisy jednotlivých čísel _Rozprav_ byly před samotným modelováním upraveny.

### 3.1 Odstranění krátkých slov

Nejprve byla odstraněna slova obsahující dva nebo méně znaků, za účelem odstranění evidentních chyb vzniklých při procesu OCR v rámci digitalizace. K tomu byl použit skript `data_preparation/remove_short_words.R`

Skript vyžaduje nastavení pracovního adresáře (předpokládá se adresář, kde je sám uložen) a cestu k textovým souborům (ty byly z původního umístění do této složky zkopírovány).

```r
current_working_directory <- getwd() # nastavený pracovní adresář, předpokládá se adresář, kde je uložen tento script
folder_path <- file.path(current_working_directory, "rozpravy_trida_3") # cesta ke složce s TXT soubory
```

Při spuštění skript pomocí funkce `processAllTxtFiles()` zpracuje postupně všechny `.txt` soubory v nastavené složce, které mají ve svém názvu prefix `TEXT`:

```r
processAllTxtFiles <- function() {
  txt_files <- list.files(path = folder_path, pattern = "^TEXT.*\\.txt$") # seznam TXT souborů začínajících na "TEXT" ve složce
  print(paste("Zkrátí se:", length(txt_files), "txt souborů ze složky:", folder_path))

  for (file in txt_files) {
    full_file_path <- file.path(folder_path, file) # celá cesta každého TXT souboru
    prepare_data(full_file_path) # zprocesování každého souboru
  }
}
```

Funkce `prepare_data(full_file_path)` načte text ze souboru, odstraní krátká slova a soubor znovu uloží s prefixem `PREPARED`.

### 3.2 UDPipe 2

Zkrácené textové soubory jsou následně obohaceny pomocí nástroje UDPipe2, k čemuž slouží skript `/data_preparation/send_to_udpipe.R`.

K obohacení je použita webová služba [LINDAT UDPipe REST Service](https://lindat.mff.cuni.cz/services/udpipe/). Konkrétně model _czech-pdt-ud-2.12-230707_, který zajistí pro každé slovo z textového přepisu tokenizaci, lemmatizaci a identifikaci slovního druhu.

Obdobně jako při odstraňování krátkých slov je potřeba nejprve načíst data ze pracovního adresáře:

```{r eval=FALSE, include=TRUE, echo=TRUE}
current_working_directory <- getwd() # nastavený pracovní adresář
folder_path <- file.path(current_working_directory, "rozpravy_trida_2") # cesta ke složce s TXT soubory
```

Postupně jsou opět zpracovány všechny `.txt` soubory v nastavené složky, které mají ve svém názvu prefix `PREPARED_TEXT`. O jejich zpracování se postará funkce `processAllTxtFiles()`, která pro každý soubor postupně spustí funkci `enrichFile(file_name)`. Tato funkce zajistí:

- sestavení příkazu pro nástroj _curl_ (`setCurl(filename)`) (funkci mohou vadit spojovníky, pomlčky a mezery mezi nimi v názvu souborů, doporučuje se takové názvy změnit)
- odeslání příkazu na LINDAT UDPipe REST Service a vyčkání na odpověď (`postRequest(curl_command)`)
- uložení získané odpovědi ve formátu CoNLLU (`saveConllu(response_content, file_name)`) - pro každý svazek je uložen jeden JSON soubor s prefixem `CoNLLU_`
- vyfiltrování podstatných jmen (jejich lemmat) ze získaných dat a jejich uložení,

K filtrování je použita funkce `getNouns(df, file_name)`:

```r
getNouns <- function(df, file_name){
  base_file_name <- basename(file_name)
  filtered_df <- df %>% filter(upos == "NOUN") # vyhledání pouze podstatných jmen
  noun_tokens <- filtered_df$lemma # pouze sloupec obsahující lemma
  # [...]
}
```

Získaná podstatná jména (`noun_tokens`) jsou uložena do `.txt` souborů s prefixem `NOUNS_`, ze kterých bude vytvořena matice dokument-slovo, sloužící jako vstup pro tematické modelování.

Protože v rámci procesu obohacení nástrojem UDPipe2 mohly být některé rozsáhlejší chyby vzniklé procesem OCR identifikovány chybně jako podstatná jména a zkrácena na jedno nebo dvouznakovou lemmu, byl nad vzniklými soubory s prefixem `NOUN_` znovu spuštěn skript `data_preparation/remove_short_words.R`. Aby proběhl správně, je ovšem potřeba změnit ve funkci `processAllTxtFiles()` prefix souborů a nastavit nový prefix pro výstupy. Soubory připravené pro proces tematického modelování nesou prefix `LDA_PREPARED_`

```r
# Změna prefixu z "TEXT" na "NOUN" ve funci processAllTxtFiles:

processAllTxtFiles <- function() {
  txt_files <- list.files(path = folder_path, pattern = "^NOUN.*\\.txt$") # seznam TXT souborů začínajících na "NOUN" ve složce
  # [...]
}

# Změna prefixu ukládaných souborů z "PREPARED" na "LDA_PREPARED" ve funkci prepare_data:
prepare_data <- function(full_file_path){
  text <- readLines(full_file_path) # čte věty z TXT souboru
  processed_text <- sapply(text, remove_short_words) # aplikuje funkci remove_short_words na každý řádek

  # Uložení do nového txt
  base_file_name <- basename(full_file_path)
  txt_file_name <- paste0("LDA_PREPARED_",base_file_name,".txt") # název souboru

  #[...]
}
```

## 4. Modelování témat

K modelování témat je používána metoda LDA. Celý proces včetně pojmenování vizualizace témat je k dispozici v souboru `/topic_model/LDA.Rmd`.

Opět je potřeba nejprve načíst soubory, se kterými bude model pracovat - předpokládá se jejich přesunutí do složky `/topic_model/data/`. Předpokládá se také, že do stejné složky, kde je soubor `LDA.Rmd` bude přesunut soubor `combined_issues_publication_year.json`.

```r
current_working_directory <- getwd() # nastavený pracovní adresář, předpokládá se adresář, kde je uložen tento script
relative_path_to_data <- "/data/*.txt" # podadresář se soubory
full_path <- paste0(current_working_directory,relative_path_to_data) # celá cesta k souborům

library(quanteda)
corpus <- corpus(readtext(full_path)) # načtení ve formě corpusu knihovny quanteda
```

Následně je potřeba data ze souborů rozložit na jednotlivé tokeny a vytvořit matici dokument-slovo (_documet-term matrix_). K načtení a manipulaci s daty se používá knihovna `quanteda`.

V rámci transformace do matice je možné odstranit z korpusu tzv. stop slova.

```r
# slova objevující se napříč všemi *Rozpravami* (zanedbatelný vliv na témata)
stop_CAVU <- c("akademie", "strana", "rozprava", "obsah", "fig", "tab", "pag", "hodina", "metr", "pozorování", "maximum", "den", "rok", "datum", "milimetr", "vel", "průměr", "minimum", "maximum", "stupeň", "leden", "únor", "březen", "duben", "květen", "červen", "červenec", "srpen", "září", "říjen", "listopad", "prosinec")

# nevýznamová slova a znaky v korpusu
stop_nonsense <- c("wsw", "nnw", "ssw", "wnw", "ene", "sse", "eše", "nne", "»", "«", "^", "srv", "srv", "ccm", "dod", "vyn", "tim", "vin", "kol", "ott", "oti", "jah", "upd", "rkp", "srvn", "stsl", "ghm", "srva", "pis", "cxo", "mgh")

# rozdělení na tokeny pro quantida
tokens <- tokens(corpus) %>%
  tokens_remove(c(stop_CAVU,stop_nonsense))

# matice dokument-slovo
dfm <- dfm(tokens)
```

K procesu LDA je využita knihovna `topicmodels`. Ta pracuje se specificky strukturovanou maticí dokument-slovo, je proto potřeba výše vytvořenou matici do této podoby převést.

```r
library(topicmodels)
dtm <- convert(dfm, to = "topicmodels")
```

Spuštění LDA si vyžaduje stanovit proměnnou `k`, která určí, kolik témat se má v korpusu identifikovat. Dále je možné nastavit hodnotu `alpha` - čím je vyšší, tím větší množství témat se může objevovat v jednom dokumentu. V našem případě je nastavena na `0.1`, neboť předpokládáme, že jedno číslo _Rozprav_ se pravděpodobně věnovalo jednomu tématu. Hodnoty dalších parametrů jsou ponechány ve výchozím nastavení, počet iterací Gibbsova vzorkování je v takovém případě 2000.

Dále je vhodné nastavit také funkci set.seed, která se používá k vytváření reprodukovatelných výsledků v případech, kdy se vytváří proměnné, které nabývají náhodných hodnot. Zaručuje se tak, že při každém spuštění kódu budou vytvořeny stejné náhodné hodnoty.

```r
set.seed(1234)
topic_model <- LDA(dtm, method = "Gibbs", k = 35,  control = list(alpha = 0.1))
topic_model
```

### 4.1 Matice _slovo-téma_ (_word-topic matrix_)

LDA vytvoří matici _slovo-téma_, ve které je u každého slova z korpusu uvedeno s jakou pravděpodobností bylo vygenerováno z jakého tématu. Tato pravděpodobnost je označena jako `beta`. Model témata nijak nepojmenuje, označena jsou pouze čísly.

```r
word_topics <- tidy(topic_model, matrix="beta")
word_topics
```

Pro každé téma je možné zobrazit stanovený počet slov, která se v něm vyskytují s nejvyšší frekvencí:

```r
topic_number <- 8 # příklad pro téma č. 8

word_topic_posterior <- posterior(topic_model)$terms[topic_number, ]
top_words_for_topicX <- head(sort(word_topic_posterior, decreasing = T), n=50)
head(top_words_for_topicX)
```

Nejfrekventovanější slova tématu je také možné zobrazit jako wordcloud:

```{r word_topic_matrix_wordcloud, include=TRUE}
wordcloud(names(top_words_for_topicX), top_words_for_topicX)
```

### 2.2 Matice _dokument-téma_ (_documet-topic matrix_)

Druhým výstupem LDA je matice popisující pravděpodobnost příslušnosti dokumentu ke konkrétnímu tématu určená proměnnou `gamma`.

LDA přiřadí každému slovu v dokumentu téma. Čím více slov v dokumentu je přiřazeno k jednomu tématu, tím větší hodnotu má proměnná `gamma` a tím spíš pojednává dokument o daném tématu. `gamma` tedy udává odhadovanou proporci slov v dokumentu, která byla vygenerována z konkrétního tématu.

```r
topic_model_documents <- tidy(topic_model, matrix = "gamma")
topic_model_documents

document_to_topic <- topic_model_documents %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()
```

Také je možné vyfiltrovat všechny dokumenty, které patří k vybranému tématu:

```r
topic_number <- 8 # vybrané téma
document_topic_filtr <- filter(document_to_topic, document_to_topic$topic == topic_number)
document_topic_filtr
```

Analýza vzniklého modelu a jeho vizualizace jsou dostupné v souboru `/topic_model/LDA.Rmd`.
