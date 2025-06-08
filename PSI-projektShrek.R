#' ---
#' title: "Shrek a sprawa polska - Projekt PSI autorstwa Macieja Kawelczyka, Szymona Pawłowskiego i Jakuba Maszkowskiego"
#' author: " "
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: breezedark    # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#' ---







#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(lsa) 
library(SentimentAnalysis)
library(readr) 
library(stringr) 

#' # 1.Pobranie i zorganizowanie dokumentów źródłowych 
# 1.Pobranie i zorganizowanie dokumentów źródłowych (SHREK!!!):
docs <- DirSource("Shrek")


corpus <- VCorpus(docs)





#' # 2. Przetwarzanie i oczyszczanie tekstu
# 2. Przetwarzanie i oczyszczanie tekstu- standardowo jak na zajęciach:



corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie (opcjonalnie jakby Pani Profesor chciała pooglądać sobie, ale nie polecam, to są naprawdę długie pliki)
#corpus[[1]][[1]]

# Macierz częstości TDM ----
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)


#' # 3. Zliczanie częstości słów i chmura słów
# 3. Zliczanie częstości słów i chmura słów:


v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)


# Chmura słów (globalna)

wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 25,
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 20
print(head(tdm_df, 20))




#' # 4. Asocjacja w macierzach TDM/DTM
# 4. Asocjacja w macierzach TDM/DTM:

#Przykładowe słowa wytpowane do asocjacji, można popróbwać jakieś inne
#Próg korelacji ustawiony bardzo wysoko ze względu na charakterystykę plików, jest ich mało(4) i są bardzo długie
#Z całej analizy sa to najmniej ciekawe wyniki
findAssocs(tdm,"donkey", 0.93)
findAssocs(tdm,"fiona", 0.93)
findAssocs(tdm,"love", 0.93)



# Wizualizacja asocjacji, tutaj akurat dla osła

target_word <- "donkey"
cor_limit <- 0.92 

associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
assoc_vector <- associations[[target_word]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)

assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
  score = assoc_sorted
)
  
  # Wykres lizakowy (lollipop chart)
  ggplot(assoc_df, aes(x = score, y = reorder(word, score))) +
    geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#a6bddb", size = 1.2) +
    geom_point(color = "#0570b0", size = 4) +
    geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
    scale_x_continuous(limits = c(0, max(assoc_df$score) + 0.1), expand = expansion(mult = c(0, 0.2))) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste0("Asocjacje z terminem: '", target_word, "'"),
      subtitle = paste0("Próg r ≥ ", cor_limit),
      x = "Współczynnik korelacji Pearsona",
      y = "Słowo"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )

  # Wykres lizakowy z natężeniem - niby to samo ale ładniej
  ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
    geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
    geom_point(size = 4) +
    geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
    scale_color_gradient(low = "#a6bddb", high = "#08306b") +
    scale_x_continuous(
      limits = c(0, max(assoc_df$score) + 0.1),
      expand = expansion(mult = c(0, 0.2))
    ) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste0("Asocjacje z terminem: '", target_word, "'"),
      subtitle = paste0("Próg r ≥ ", cor_limit),
      x = "Współczynnik korelacji Pearsona",
      y = "Słowo",
      color = "Natężenie\nskojarzenia"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "right"
    )
#' # 5. Korelacja między filmami
# 5. Korelacja między filmami:
  #Najpierw w sposób prosty, bo patrzymy po słowach i wagach TF-IDF
  dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  
  matrix_tfidf <- as.matrix(dtm_tfidf)
  similarity_matrix <- cosine(t(matrix_tfidf))
  #Suche wyniki
  print("Macierz Podobieństwa:")
  print(round(similarity_matrix, 3))
  
#Wizualizacja na wykresie w postaci macierzy podobieństwa
  doc_names <- paste0("Shrek ", 1:nrow(similarity_matrix))
  colnames(similarity_matrix) <- doc_names
  rownames(similarity_matrix) <- doc_names
  
  similarity_df <- as.data.frame(similarity_matrix) %>%
    rownames_to_column(var = "Doc1") %>%
    pivot_longer(-Doc1, names_to = "Doc2", values_to = "Similarity")
  
  ggplot(similarity_df, aes(x = Doc1, y = Doc2, fill = Similarity)) +
    geom_tile(color = "white") + 
 
    geom_text(aes(label = round(Similarity, 2)), color = "black", size = 4) +
    scale_fill_gradient(low = "#e0f3f8", high = "#08519c", limits = c(0, 1)) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(
      title = "Mapa Ciepła Podobieństwa Dokumentów",
      subtitle = "Obliczone na wagach TF-IDF",
      x = "",
      y = "",
      fill = "Podobieństwo"
    )
  #Modelowanie Tematyczne (LDA), dla naszych danych da zdecydowanie lepsze rezultaty niż poprzednia metoda
  dtm_lda <- DocumentTermMatrix(corpus, control = list(
    bounds = list(global = c(4, length(corpus))) #tu można pozmieniać na 2 lub 3, ale dla 4 wychodzi najsensowniej
  ))

  dtm_lda <- dtm_lda[rowSums(as.matrix(dtm_lda)) > 0, ]

  if (nrow(dtm_lda) < 2) {
    
    print("Analiza LDA niemożliwa: Po odfiltrowaniu słów pozostał tylko jeden (lub mniej) dokument.")
    print("Wskazówka: Spróbuj złagodzić kryteria filtrowania w 'bounds' w funkcji DocumentTermMatrix, np. obniżając dolną granicę 'global'.")
    
  } else {
    
 
    lda_model <- LDA(dtm_lda, k = 3, control = list(seed = 1234))
    doc_topics <- posterior(lda_model)$topics
    
    print("Rozkład tematów w dokumentach (wiersze=dokumenty, kolumny=tematy):")
    print(round(doc_topics, 2))
    similarity_topics <- cosine(t(doc_topics))
    
    print("Macierz Podobieństwa (oparta na tematach LDA):")
    print(round(similarity_topics, 2))
    
    # Wykres! Kolejna mapa ciepła, ale ta z ciekawszymi rezultatami
    doc_names <- paste0("Shrek ", 1:nrow(similarity_topics))
    colnames(similarity_topics) <- doc_names
    rownames(similarity_topics) <- doc_names
    
    similarity_df_lda <- as.data.frame(similarity_topics) %>%
      rownames_to_column(var = "Doc1") %>%
      pivot_longer(-Doc1, names_to = "Doc2", values_to = "Similarity")
    
    ggplot(similarity_df_lda, aes(x = Doc1, y = Doc2, fill = Similarity)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Similarity, 2)), color = "black", size = 4) +
      scale_fill_gradient(low = "#fee8c8", high = "#e34a33", limits = c(0, 1)) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(
        title = "Mapa Ciepła (Metoda LDA)",
        subtitle = paste0("Oparta na ", ncol(doc_topics), " ukrytych tematach"),
        x = "Dokument", y = "Dokument", fill = "Podobieństwo"
      )
  }
  
  
#' # 6. Analiza sentymentu w czasie
# 6. Analiza sentymentu w czasie:

#Aby program działał w sensownym czasie oraz daó się coś wyczytać z wykresu należy podzielić go na fragmenty(okna)
  window_words <- 500
  

  script_files <- list.files("Shrek", pattern = "\\.txt$", full.names = TRUE)
  analyze_part_words <- function(file) {
    text <- readLines(file, encoding = "UTF-8")
    text <- text[nzchar(text)]
    word_count    <- str_count(text, boundary("word"))
    total_words   <- cumsum(word_count)
    sent          <- analyzeSentiment(text)
    data.frame(
      part        = tools::file_path_sans_ext(basename(file)),
      total_words = total_words,
      GI          = sent$SentimentGI,
      QDAP        = sent$SentimentQDAP
    )
  }

  df_words <- map_df(script_files, analyze_part_words) %>%
    mutate(chunk = floor((total_words - 1) / window_words))

  df_agg <- df_words %>%
    pivot_longer(GI:QDAP, names_to = "Dictionary", values_to = "value") %>%
    group_by(part, Dictionary, chunk) %>%
    summarize(
      words_mid = mean(total_words),
      value_avg = mean(value, na.rm = TRUE),
      .groups   = "drop"
    )
  
 #Wykresy GI vs QDAP dla każdego z filmów
  for(p in unique(df_agg$part)) {
    df_tmp <- df_agg %>% filter(part == p)
    plt <- ggplot(df_tmp, aes(x = words_mid, y = value_avg, color = Dictionary)) +
      geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.2) +
      labs(
        title = paste0("Sentyment w czasie – ", p),
        x     = NULL,
        y     = "Średni sentyment"
      ) +
      scale_x_continuous(breaks = NULL) +
      theme_gdocs() +
      theme(legend.position = "bottom")
    print(plt)
  }
#' # 7. Analiza skumulowanego sentymentu 
# 7. Analiza skumulowanego sentymentu:
  

  

  # Tutaj analizujemy sentyment dla kazdego filmu oddzielnie
  analyze_sentiment_per_file <- function(file_path) {
    text <- read_file(file_path)
    words <- unlist(str_split(text, "\\s+"))
    words <- words[words != ""]
    sentiment_scores <- analyzeSentiment(words)
    data.frame(
      Film = tools::file_path_sans_ext(basename(file_path)),
      SentimentGI = convertToDirection(sentiment_scores$SentimentGI),
      SentimentQDAP = convertToDirection(sentiment_scores$SentimentQDAP)
    )
  }
  
  script_files <- list.files("Shrek", pattern = "\\.txt$", full.names = TRUE)
  
  sentiment_summary_per_film <- map_df(script_files, analyze_sentiment_per_file) %>%
    pivot_longer(
      cols = c("SentimentGI", "SentimentQDAP"),
      names_to = "Dictionary",
      values_to = "Sentiment"
    ) %>%
    mutate(Dictionary = str_remove(Dictionary, "Sentiment")) %>%
    group_by(Film, Dictionary, Sentiment) %>%
    summarise(Count = n(), .groups = "drop") %>%
    complete(Film, Dictionary, Sentiment, fill = list(Count = 0))
  
  print("--- Podsumowanie sentymentu dla każdego filmu ---")
  print(sentiment_summary_per_film)
  
  # Wykres dla każdego filmu, dla przejrzystości wyników tylko wyświetla się sentyment pozytywny i negatywny
  sentiment_plot_per_film <- sentiment_summary_per_film %>%
    filter(Sentiment %in% c("positive", "negative"))
  
  ggplot(sentiment_plot_per_film, aes(x = Sentiment, y = Count, fill = Dictionary)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(
      aes(label = Count),
      position = position_dodge(width = 0.9),
      vjust = -0.4, 
      size = 3
    ) +
    facet_wrap(~Film, ncol = 2, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.17))) +
    
    scale_fill_manual(values = c("GI" = "#1f78b4", "QDAP" = "#33a02c")) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Sentyment w poszczególnych częściach Shreka ",
      subtitle = "Liczba słów pozytywnych i negatywnych w każdym filmie",
      x = "Kategoria sentymentu",
      y = "Liczba słów",
      fill = "Słownik"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  
  #Analiza sentymentu dla całej sagi
  all_text <- map_chr(script_files, read_file) %>%
    paste(collapse = " ")
  all_words <- unlist(str_split(all_text, "\\s+"))
  all_words <- all_words[all_words != ""]
  
  sentiment_scores_total <- analyzeSentiment(all_words)
  sentiment_summary_total <- data.frame(
    GI = convertToDirection(sentiment_scores_total$SentimentGI),
    QDAP = convertToDirection(sentiment_scores_total$SentimentQDAP)
  ) %>%
    pivot_longer(cols = c("GI", "QDAP"), names_to = "Dictionary", values_to = "Sentiment") %>%
    group_by(Dictionary, Sentiment) %>%
    summarise(Count = n(), .groups = "drop")
  
  print("--- Podsumowanie sentymentu dla całego korpusu łącznie ---")
  print(sentiment_summary_total)
  
  # Wykres, dla obu słowników odzielnie, w celach porównawczych
  sentiment_plot_total <- sentiment_summary_total %>%
    filter(Sentiment %in% c("positive", "negative"))
  
  ggplot(sentiment_plot_total, aes(x = Sentiment, y = Count, fill = Dictionary)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Count),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = c("GI" = "#1f78b4", "QDAP" = "#33a02c")) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Łączny sentyment w sadze Shrek ",
      subtitle = "Suma słów pozytywnych i negatywnych \n ze wszystkich filmów",
      x = "Kategoria sentymentu",
      y = "Liczba słów",
      fill = "Słownik"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "bottom"
    )
  

  