library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(lubridate)
library(stringi)
library(polyglotr)
library(cld2)
library(tidytext)
library(scales)

# ============================================================
# 1. UČITAVANJE, PARSIRANJE I ČIŠĆENJE EMOTIKONA
# ============================================================

HuD <- readLines("hrvati_u_danskoj.txt")

HuD_tbl <- tibble(line = HuD) |>
  filter(str_detect(line, "\\S")) |>
  mutate(
    label = str_extract(line, "^(OBJAVA|DATUM|SVIĐANJA|KOMENTARI|TEKST)(?=:)"),
    value = str_remove(line, "^[A-ZČĆŠĐŽ]+:\\s*"),
    post_id = cumsum(label == "OBJAVA" & !is.na(label))
  ) |>
  filter(!is.na(label)) |>
  select(post_id, label, value) |>
  pivot_wider(names_from = label, values_from = value) |>
  transmute(
    objava = as.integer(OBJAVA),
    datum = as.Date(DATUM, format = "%m-%d-%Y"),
    svidanja = as.integer(SVIĐANJA),
    komentari = as.integer(KOMENTARI),
    tekst = TEKST
  )

# Čišćenje emotikona
HuD_tbl <- HuD_tbl |>
  mutate(
    tekst = tekst |>
      stri_replace_all_regex("\\p{Emoji_Presentation}", "") |>
      stri_replace_all_regex("[\\p{So}\\p{Sk}]", "") |>
      stri_replace_all_regex("[\\x{FE00}-\\x{FE0F}\\x{1F3FB}-\\x{1F3FF}]", "") |>
      str_squish()
  )

# Pregled
HuD_tbl |> glimpse()

# ============================================================
# 2. DETEKCIJA JEZIKA, ANALIZA I PRIJEVOD
# ============================================================

# Detekcija jezika
HuD_tbl <- HuD_tbl |>
  mutate(
    jezik_raw = detect_language(tekst),
    jezik = case_when(
      is.na(jezik_raw) ~ "hr",
      jezik_raw %in% c("sr", "bs", "sl", "mk") ~ "hr",
      str_detect(jezik_raw, "-") ~ "hr",
      TRUE ~ jezik_raw
    )
  )

# Mapa ISO kodova → puna imena jezika
jezici_imena <- c("hr" = "hrvatski i drugi\njužnoslavenski\njezici", "en" = "engleski", "da" = "danski", "de" = "njemački", "sv" = "švedski", "no" = "norveški", "nl" = "nizozemski", "fr" = "francuski", "es" = "španjolski", "it" = "talijanski", "pl" = "poljski", "ro" = "rumunjski", "tr" = "turski", "sq" = "albanski", "hu" = "mađarski", "cs" = "češki", "sk" = "slovački", "is" = "islandski", "fi" = "finski")

# Analiza distribucije jezika
distribucija_jezika <- HuD_tbl |>
  count(jezik, sort = TRUE) |>
  mutate(
    udio = n / sum(n),
    jezik_ime = recode(jezik, !!!jezici_imena, .default = jezik)
  )

distribucija_jezika |> arrange(desc(n))

distribucija_jezika |>
  slice_max(n, n = 3) |>
  ggplot(aes(x = reorder(jezik_ime, n), y = n)) +
  geom_col(fill = "darkgreen", alpha = 0.5) +
  geom_text(aes(label = paste0(n, " (", percent(udio, accuracy = 0.1), ")")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Distribucija objava po jeziku", x = NULL, y = "Broj objava") +
  theme_minimal() +
  expand_limits(y = max(distribucija_jezika$n[1:3]) * 1.2)

# Prijevod na engleski
safe_google <- possibly(
  \(text, src) {
    if (src == "en") return(text)
    google_translate(text, target_language = "en", source_language = src)
  },
  otherwise = NA_character_
)

HuD_tbl <- HuD_tbl |>
  mutate(tekst_en = map2_chr(tekst, jezik, safe_google, .progress = TRUE)) |>
  drop_na(tekst_en) |>
  filter(nzchar(str_trim(tekst_en)))

# ============================================================
# 3. DULJINA OBJAVA
# ============================================================

HuD_tbl <- HuD_tbl |>
  mutate(
    duljina_znakova = nchar(tekst_en),
    duljina_rijeci = str_count(tekst_en, "\\S+"),
    duljina_recenica = str_count(tekst_en, "[.!?]+")
  )

# Sažeta statistika
duljina_summary <- HuD_tbl |>
  summarise(
    prosj_znakova = round(mean(duljina_znakova), 1),
    medijan_znakova = median(duljina_znakova),
    min_znakova = min(duljina_znakova),
    max_znakova = max(duljina_znakova),
    prosj_rijeci = round(mean(duljina_rijeci), 1),
    medijan_rijeci = median(duljina_rijeci),
    prosj_recenica = round(mean(duljina_recenica), 1)
  ) |> print()

# Histogram distribucije
ggplot(HuD_tbl, aes(x = duljina_rijeci)) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(duljina_rijeci)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(duljina_rijeci)), color = "blue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(HuD_tbl$duljina_rijeci) + 5, y = 50, label = paste("Prosjek:", round(mean(HuD_tbl$duljina_rijeci), 1)), color = "red", hjust = 0) +
  annotate("text", x = median(HuD_tbl$duljina_rijeci) + 5, y = 40, label = paste("Medijan:", median(HuD_tbl$duljina_rijeci)), color = "blue", hjust = 0) +
  labs(title = "Distribucija duljine objava", x = "Broj riječi", y = "Broj objava") +
  theme_minimal()

# ============================================================
# 4. NAJČEŠĆE RIJEČI (UKUPNO) I BIGRAMI
# ============================================================

# Stop riječi za ovu domenu
custom_stop <- tibble(word = c("hello", "everyone", "team", "anyone", "good", "afternoon", "evening", "thanks", "please", "greetings", "wondering", "question", "lp", "pozz", "hi", "dear", "people", "lot", "thank", "morning", "day", "feel", "free", "https", "http", "www"))

# Tokenizacija
sve_rijeci <- HuD_tbl |>
  select(objava, tekst_en) |>
  unnest_tokens(word, tekst_en) |>
  anti_join(stop_words, by = "word") |>
  anti_join(custom_stop, by = "word") |>
  filter(str_length(word) > 2, !str_detect(word, "^\\d+$"))

# Top 30 najčešćih riječi
top_rijeci_ukupno <- sve_rijeci |> count(word, sort = TRUE) |> head(20)
top_rijeci_ukupno

ggplot(top_rijeci_ukupno, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkgreen", alpha = 0.5) +
  coord_flip() +
  labs(title = "20 najčešćih riječi u objavama", x = NULL, y = "Frekvencija") +
  theme_minimal()

# ============================================================
# 5. VREMENSKE VARIJABLE I KOLIČINA OBJAVA KROZ VRIJEME
# ============================================================

HuD_tbl <- HuD_tbl |>
  mutate(
    mjesec = floor_date(datum, "month"),
    godina = year(datum)
  )

# Količina objava po mjesecima
objave_po_mjesecu <- HuD_tbl |> count(mjesec)

ggplot(objave_po_mjesecu, aes(x = mjesec, y = n)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(aes(label = n), vjust = -1, size = 3.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Količina objava po mjesecima", subtitle = paste("Ukupno", nrow(HuD_tbl), "objava"), x = NULL, y = "Broj objava") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = max(objave_po_mjesecu$n) * 1.15)

# Mjesec s najviše objava
objave_po_mjesecu |> slice_max(n, n = 3)

# Statistika aktivnosti
HuD_tbl |>
  summarise(
    prvi_dan = min(datum),
    zadnji_dan = max(datum),
    raspon_dana = as.integer(max(datum) - min(datum)),
    prosj_objava_mjesecno = round(n() / n_distinct(mjesec), 1)
  )

# ============================================================
# 6. KLASIFIKACIJA TEMA, ANALIZA I DISTRIBUCIJA
# ============================================================

topic_dict <- list(
  stanovanje = c("apartment", "room for rent", "rent out", "renting", "accommodation", "flat", "studio", "roommate", "landlord", "looking for a room", "looking for apartment"),
  transport_putovanja = c("traveling", "travel from", "travel to", "drive from", "drive to", "driving from", "driving to", "truck", "van", "transport", "flight", "airport", "plain", "package", "luggage", "carrier", "shipment", "ride to", "going to croatia", "going to denmark"),
  posao = c("looking for a job", "job offer", "we are hiring", "we are looking for", "job advertisement", "salary", "hiring", "employee", "employer", "looking for workers", "experienced", "recruitment", "vacancy", "part-time job", "student job", "full time", "applicant"),
  hrana_prodaja = c("cake", "cakes", "burek", "pastry", "donuts", "for sale", "fresh", "balkan food", "selling", "homemade", "bakery"),
  zdravstvo = c("doctor", "medical", "hospital", "nurse", "physiotherapist", "medicine", "psychologist", "neuropsychologist", "dentist", "health system", "infertility"),
  birokracija = c("cpr", "tax", "skat", "passport", "embassy", "registration", "documents", "visa", "license", "certificate", "papers", "letter of guarantee", "diploma certification"),
  obrtnici_usluge = c("mechanic", "plumber", "electrician", "hairdresser", "lawyer", "store", "carpenter", "service", "repair", "installer", "interpreter", "notary", "accountant", "graphic design", "water installer", "auto service", "bookkeeping"),
  vjera_zajednica = c("mass", "church", "christmas", "easter", "priest", "holy mass", "midnight mass"),
  sport_zabava = c("tickets", "concert", "handball", "water polo", "dinamo", "match", "game in malmo", "bronze", "gold", "silver", "european championship", "celebration", "valentine"),
  jezik_obrazovanje = c("danish course", "danish language", "language course", "study", "studies", "university", "students", "studying", "school", "education", "bachelor", "masters", "phd", "erasmus"),
  drustvo_upoznavanje = c("meet someone", "meet up", "looking to meet", "single mom", "single father", "drop a message", "lonely", "anonymous", "čakula", "hang out", "drinks and", "go out for"),
  tv_internet = c("iptv", "tv channels", "telemach", "eontv", "norlys", "internet", "watch our", "streaming"),
  dogadjaji_promocija = c("grandbalkanevent", "balkan nytårs", "balkan event", "join us", "come and have fun", "caffe g", "venue", "open all days"),
  novac_financije = c("dkk", "euros", "exchange", "leasing", "credit", "kindergarten payment", "real estate")
)

classify_text <- function(text, dict = topic_dict) {
  text_lower <- str_to_lower(text)
  scores <- map_int(dict, \(keywords) {
    sum(map_lgl(keywords, \(kw) str_detect(text_lower, fixed(kw))))
  })
  if (max(scores) == 0) return("ostalo")
  names(scores)[which.max(scores)]
}

HuD_tbl <- HuD_tbl |> mutate(tema = map_chr(tekst_en, classify_text))

# Distribucija po temama
tema_summary <- HuD_tbl |> count(tema, sort = TRUE) |> mutate(udio = n / sum(n))
tema_summary

ggplot(tema_summary, aes(x = reorder(tema, n), y = n)) +
  geom_col(fill = "darkgreen", alpha = 0.5) +
  geom_text(aes(label = paste0(n, " (", percent(udio, accuracy = 0.1), ")")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Distribucija objava po temama", subtitle = paste("Ukupno", nrow(HuD_tbl), "objava"), x = NULL, y = "Broj objava") +
  theme_minimal() +
  expand_limits(y = max(tema_summary$n) * 1.2)

# Najčešće riječi po temama
sve_rijeci_tema <- HuD_tbl |>
  select(tema, tekst_en) |>
  unnest_tokens(word, tekst_en) |>
  anti_join(stop_words, by = "word") |>
  anti_join(custom_stop, by = "word") |>
  filter(str_length(word) > 2, !str_detect(word, "^\\d+$"))

top_rijeci_po_temi <- sve_rijeci_tema |>
  count(tema, word, sort = TRUE) |>
  group_by(tema) |>
  slice_max(n, n = 8) |>
  ungroup()

ggplot(top_rijeci_po_temi, aes(x = reorder_within(word, n, tema), y = n, fill = tema)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~tema, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Najčešće riječi po temama", x = NULL, y = "Frekvencija") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Karakteristične riječi po temi (TF-IDF)
karakteristicne_rijeci <- sve_rijeci_tema |>
  count(tema, word) |>
  bind_tf_idf(word, tema, n) |>
  group_by(tema) |>
  slice_max(tf_idf, n = 5) |>
  ungroup() |>
  arrange(tema, desc(tf_idf))

karakteristicne_rijeci |>
  group_by(tema) |>
  summarise(top_5 = paste(word, collapse = ", "), .groups = "drop") |>
  print(n = Inf)

# Teme kroz vrijeme — heatmapa
tema_kroz_vrijeme <- HuD_tbl |> count(mjesec, tema)

ggplot(tema_kroz_vrijeme, aes(x = mjesec, y = tema, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Objave") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Intenzitet objava po temama kroz vrijeme", x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# 7. NAJLAJKANIJE/NAJKOMENTIRANIJE OBJAVE I ANGAŽMAN PO TEMAMA
# ============================================================

# Top 10 najlajkanijih
top_lajkani <- HuD_tbl |>
  slice_max(svidanja, n = 10) |>
  select(objava, tema, svidanja, komentari, tekst_en)
top_lajkani

# Top 10 najkomentiranijih
top_komentirani <- HuD_tbl |>
  slice_max(komentari, n = 10) |>
  select(objava, tema, svidanja, komentari, tekst_en)
top_komentirani

# Angažman po temama
angazman_tema <- HuD_tbl |>
  group_by(tema) |>
  summarise(
    n_objava = n(),
    prosj_svidanja = round(mean(svidanja, na.rm = TRUE), 1),
    medijan_svidanja = median(svidanja, na.rm = TRUE),
    prosj_komentari = round(mean(komentari, na.rm = TRUE), 1),
    medijan_komentari = median(komentari, na.rm = TRUE),
    ukupno_svidanja = sum(svidanja, na.rm = TRUE),
    ukupno_komentari = sum(komentari, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(prosj_komentari))
angazman_tema

# Vizualizacija angažmana
angazman_tema |>
  pivot_longer(c(prosj_svidanja, prosj_komentari), names_to = "metrika", values_to = "vrijednost") |>
  mutate(metrika = recode(metrika, prosj_svidanja = "Prosj. sviđanja", prosj_komentari = "Prosj. komentari")) |>
  ggplot(aes(x = reorder(tema, vrijednost), y = vrijednost, fill = metrika)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Angažman po temama", x = NULL, y = "Prosječna vrijednost", fill = NULL) +
  theme_minimal()

# ============================================================
# 8. KLASIFIKACIJA TIPA OBJAVE
# ============================================================

classify_type <- function(text) {
  t <- str_to_lower(text)

  if (str_detect(t, paste(c("for sale", "we are hiring", "we are looking for", "selling", "rent out", "renting out", "we offer", "i offer", "join us", "come and", "tickets:", "tickets ", "job advertisement", "we are a", "available for sale", "i recommend"
  ), collapse = "|"))) return("ponuda_oglas")
  if (str_detect(t, paste(c("merry christmas", "happy easter", "congratulations", "reminder", "announcement", "we wish", "blessed", "this year we are holding", "we are organizing"
  ), collapse = "|"))) return("obavijest")
  if (str_detect(t, paste(c("looking for", "i need", "wondering", "does anyone", "is there anyone", "is there a", "how to", "how do", "can someone", "anyone know", "any recommendation", "please help", "any advice", "i would like to know", "\\?"
  ), collapse = "|"))) return("pitanje_potreba")
  
  return("ostalo")
}

HuD_tbl <- HuD_tbl |> mutate(tip = map_chr(tekst_en, classify_type))
HuD_tbl |> count(tip)

# ============================================================
# 9. SENTIMENT ANALIZA
# ============================================================

sentiments_bing <- get_sentiments("bing")

sentiment_po_objavi <- HuD_tbl |>
  select(objava, tekst_en) |>
  unnest_tokens(word, tekst_en) |>
  inner_join(sentiments_bing, by = "word") |>
  count(objava, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(
    sentiment_score = positive - negative,
    sentiment_label = case_when(sentiment_score > 0 ~ "pozitivan", sentiment_score < 0 ~ "negativan", TRUE ~ "neutralan")
  ) |>
  select(objava, sentiment_score, sentiment_label, pozitivnih_rijeci = positive, negativnih_rijeci = negative)

HuD_tbl <- HuD_tbl |>
  left_join(sentiment_po_objavi, by = "objava") |>
  mutate(
    sentiment_label = replace_na(sentiment_label, "neutralan"),
    sentiment_score = replace_na(sentiment_score, 0)
  )

# Distribucija sentimenta
HuD_tbl |> count(sentiment_label, sort = TRUE)

# Sentiment po temi
sentiment_tema <- HuD_tbl |>
  group_by(tema) |>
  summarise(
    prosj_sentiment = round(mean(sentiment_score, na.rm = TRUE), 2),
    udio_pozitivnih = round(mean(sentiment_label == "pozitivan"), 3),
    udio_negativnih = round(mean(sentiment_label == "negativan"), 3),
    .groups = "drop"
  ) |>
  arrange(desc(prosj_sentiment))
sentiment_tema

ggplot(sentiment_tema, aes(x = reorder(tema, prosj_sentiment), y = prosj_sentiment, fill = prosj_sentiment > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"), guide = "none") +
  coord_flip() +
  labs(title = "Prosječni sentiment po temama", subtitle = "Pozitivne vrijednosti = više pozitivnih nego negativnih riječi", x = NULL, y = "Sentiment score (pozitivne − negativne riječi)") +
  theme_minimal()

# ============================================================
# 10. LEKSIČKA RAZNOLIKOST
# ============================================================

# Type-Token Ratio po temama
ttr_po_temi <- sve_rijeci_tema |>
  group_by(tema) |>
  summarise(
    razlicitih_rijeci = n_distinct(word),
    ukupno_rijeci = n(),
    ttr = round(razlicitih_rijeci / ukupno_rijeci, 3),
    .groups = "drop"
  ) |>
  arrange(desc(ttr))
ttr_po_temi

ggplot(ttr_po_temi, aes(x = reorder(tema, ttr), y = ttr)) +
  geom_col(fill = "darkgreen", alpha = 0.5) +
  geom_text(aes(label = ttr), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Leksička raznolikost po temama (TTR)", subtitle = "Viša vrijednost = raznovrsniji vokabular", x = NULL, y = "Type-Token Ratio") +
  theme_minimal() +
  expand_limits(y = max(ttr_po_temi$ttr) * 1.15)

# TTR po pojedinim objavama (raznolikost vokabulara unutar objave)
HuD_tbl <- HuD_tbl |>
  mutate(
    ttr_objava = map_dbl(tekst_en, \(t) {
      rijeci <- str_to_lower(str_extract_all(t, "\\b[a-z]+\\b")[[1]])
      if (length(rijeci) == 0) return(NA_real_)
      n_distinct(rijeci) / length(rijeci)
    })
  )

# ============================================================
# 11. ANALIZA TIPA OBJAVE: DISTRIBUCIJE I KRIŽANJA S TEMOM
# ============================================================

# Distribucija tipova
tip_summary <- HuD_tbl |> count(tip, sort = TRUE) |> mutate(udio = n / sum(n))
tip_summary

ggplot(tip_summary, aes(x = reorder(tip, n), y = n)) +
  geom_col(fill = "darkgreen", alpha = 0.5) +
  geom_text(aes(label = paste0(n, " (", percent(udio, accuracy = 0.1), ")")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Distribucija tipova objava", x = NULL, y = "Broj objava") +
  theme_minimal() +
  expand_limits(y = max(tip_summary$n) * 1.2)

# Tip × tema
tip_tema <- HuD_tbl |>
  count(tema, tip) |>
  group_by(tema) |>
  mutate(udio = n / sum(n)) |>
  ungroup()

ggplot(tip_tema, aes(x = tema, y = udio, fill = tip)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title = "Struktura tipova objava unutar svake teme", x = NULL, y = "Udio", fill = "Tip") +
  theme_minimal()

# Omjer pitanja prema ponudama po temi
omjer_potreba_ponuda <- HuD_tbl |>
  filter(tip %in% c("pitanje_potreba", "ponuda_oglas")) |>
  count(tema, tip) |>
  pivot_wider(names_from = tip, values_from = n, values_fill = 0) |>
  mutate(omjer_potreba_ponuda = round(pitanje_potreba / pmax(ponuda_oglas, 1), 2)) |>
  arrange(desc(omjer_potreba_ponuda))
omjer_potreba_ponuda

# ============================================================
# 12. KONAČNI TIBBLE I SPREMANJE
# ============================================================

HuD_final <- HuD_tbl |>
  select(
    objava, datum, mjesec, godina,
    jezik, tema, tip,
    svidanja, komentari,
    sentiment_score, sentiment_label,
    pozitivnih_rijeci, negativnih_rijeci,
    duljina_znakova, duljina_rijeci, duljina_recenica, ttr_objava,
    tekst, tekst_en
  )

HuD_final |> glimpse()

write_csv(HuD_final, "hrvati_u_danskoj_analiza.csv")
