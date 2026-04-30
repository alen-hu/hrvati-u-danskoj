library(tibble)
library(dplyr)
library(stringr)
library(stringi)
library(polyglotr)
library(cld3)
library(purrr)

HuD <- readLines("~/Documents/sociologija-hrvatske-dijaspore/hrvati_u_danskoj.txt")
head(HuD, 30)

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

HuD_tbl

HuD_tbl <- HuD_tbl |>
  mutate(
    tekst = tekst |>
      # Ukloni sve emoji znakove (Unicode property "Emoji")
      stri_replace_all_regex("\\p{Emoji_Presentation}", "") |>
      # Ukloni i dodatne simbole/piktograme koji nisu pokriveni gore
      stri_replace_all_regex("[\\p{So}\\p{Sk}]", "") |>
      # Ukloni varijacijske selektore (npr. \uFE0F koji prati emojije)
      stri_replace_all_regex("[\\x{FE00}-\\x{FE0F}\\x{1F3FB}-\\x{1F3FF}]", "") |>
      # Spoji višestruke razmake u jedan
      str_squish()
  )

HuD_tbl

HuD_tbl <- HuD_tbl |>
  mutate(
    jezik_raw = detect_language(tekst),
    jezik = case_when(
      is.na(jezik_raw)                         ~ "hr",
      jezik_raw %in% c("sr", "bs", "sl", "mk") ~ "hr",
      TRUE                                      ~ jezik_raw
    )
  )

# 4. Prijevod na engleski
safe_google <- possibly(
  \(text, src) {
    if (src == "en") return(text)
    google_translate(text, target_language = "en", source_language = src)
  },
  otherwise = NA_character_
)

HuD_tbl <- HuD_tbl |>
  mutate(
    tekst_en = map2_chr(tekst, jezik, safe_google, .progress = TRUE)
  )

HuD_tbl
