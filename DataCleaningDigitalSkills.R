# Analyse Skript

library(tidyverse)

source("Data/qualtricshelpers.R")

# Rohdaten laden
raw <- read_csv("data/Datensatz.csv")

## Daten mit Funktion load_qualtrics_csv einlesen: 
Datensatz <- "data/Datensatz.csv"
raw <- load_qualtrics_csv(Datensatz)

#### Daten bereinigen ----
### Schritt 1: Unnötige Spalten löschen. 
## Die ausgeschlossenen Fragen sind Systemdaten, Items von anderen Gruppenmitgliedern etc.:
raw.short <- raw[,c(-1:-17, -19, -28)]

### Schritt 2: Variablen umbenennen 
## Eine eigene Datei mit den Variablennamen erzeugen:
generate_codebook(raw.short, Datensatz, "data/codebook.csv")


### Schritt 3: Variablen den richtigen Typen zuordnen
## Gender zu kategorialer Variable machen:
raw.short$gender <- as.factor(raw.short$gender)

## Schulabschluss zu ordinaler Variable machen:
raw.short$edu1 <- ordered(raw.short$education, levels = c("Kein Schulabschluss",
                                                          "Volks- oder Hauptschulabschluss",
                                                          "Mittlere Reife/Realschulabschluss",
                                                          "Fachhochschulreife/Fachabitur",
                                                          "Allgemeine Hochschulreife/Abitur",
                                                          "Abgeschlossene Berufsausbildung",
                                                          "Bachelor",
                                                          "Master",
                                                          "Promotion",
                                                          "Habilitation",
                                                          "Keiner der hier genannten"))
## Schulabluss zu ordinaler Variable machen:
raw.short$edu2 <- ordered(raw.short$activity, levels = c("Schüler",
                                                       "Student",
                                                       "Auszubildender",
                                                       "Angestellter",
                                                       "Selbstständiger",
                                                       "Rentner",
                                                       "Arbeitssuchender"))


## Skala für Likertskala einmal anlegen...
scale.zustimmung <-c("Stimme gar nicht zu", 
                     "Stimme nicht zu", 
                     "Stimme eher nicht zu", 
                     "Stimme eher zu", 
                     "Stimme zu", 
                     "Stimme völlig zu")

## ... und für jedes Item verwenden:
raw.short$regfoc_1 <- ordered(raw.short$regfoc_1, levels = scale.zustimmung)
raw.short$regfoc_2 <- ordered(raw.short$regfoc_2, levels = scale.zustimmung)
raw.short$regfoc_3 <- ordered(raw.short$regfoc_3, levels = scale.zustimmung)
raw.short$regfoc_4 <- ordered(raw.short$regfoc_4, levels = scale.zustimmung)
raw.short$regfoc_5 <- ordered(raw.short$regfoc_5, levels = scale.zustimmung)
raw.short$regfoc_6 <- ordered(raw.short$regfoc_6, levels = scale.zustimmung)
raw.short$regfoc_7 <- ordered(raw.short$regfoc_7, levels = scale.zustimmung)
raw.short$regfoc_8 <- ordered(raw.short$regfoc_8, levels = scale.zustimmung)
raw.short$regfoc_9 <- ordered(raw.short$regfoc_9, levels = scale.zustimmung)
raw.short$regfoc_10 <- ordered(raw.short$regfoc_10, levels = scale.zustimmung)

raw.short$jc_scen1_question_1 <- ordered(raw.short$jc_scen1_question_1, levels = scale.zustimmung)
raw.short$jc_scen1_question_2 <- ordered(raw.short$jc_scen1_question_2, levels = scale.zustimmung)
raw.short$jc_scen1_question_3 <- ordered(raw.short$jc_scen1_question_3, levels = scale.zustimmung)
raw.short$jc_scen1_question_4 <- ordered(raw.short$jc_scen1_question_4, levels = scale.zustimmung)
raw.short$jc_scen1_question_5 <- ordered(raw.short$jc_scen1_question_5, levels = scale.zustimmung)
raw.short$jc_scen1_question_6 <- ordered(raw.short$jc_scen1_question_6, levels = scale.zustimmung)
raw.short$jc_scen1_question_7 <- ordered(raw.short$jc_scen1_question_7, levels = scale.zustimmung)
raw.short$rjc_scen1_question_8 <- ordered(raw.short$jc_scen1_question_8, levels = scale.zustimmung)
raw.short$jc_scen1_question_9 <- ordered(raw.short$jc_scen1_question_9, levels = scale.zustimmung)

raw.short$oraw1_1 <- ordered(raw.short$oraw1_1, levels = scale.zustimmung)
raw.short$oraw1_2 <- ordered(raw.short$oraw1_2, levels = scale.zustimmung)
raw.short$oraw1_3 <- ordered(raw.short$oraw1_3, levels = scale.zustimmung)
raw.short$oraw1_4 <- ordered(raw.short$oraw1_4, levels = scale.zustimmung)
raw.short$oraw1_5 <- ordered(raw.short$oraw1_5, levels = scale.zustimmung)
raw.short$oraw1_6 <- ordered(raw.short$oraw1_6, levels = scale.zustimmung)
raw.short$oraw1_7 <- ordered(raw.short$oraw1_7, levels = scale.zustimmung)


raw.short$orrpk_1 <- ordered(raw.short$orrpk_1, levels = scale.zustimmung)
raw.short$orrpk_2 <- ordered(raw.short$orrpk_2, levels = scale.zustimmung)

raw.short$orrpz_1 <- ordered(raw.short$orrpz_1, levels = scale.zustimmung)
raw.short$orrpz_2 <- ordered(raw.short$orrpz_2, levels = scale.zustimmung)
raw.short$orrpz_3 <- ordered(raw.short$orrpz_3, levels = scale.zustimmung)

raw.short$orrppe_1 <- ordered(raw.short$orrppe_1, levels = scale.zustimmung)
raw.short$orrppe_2 <- ordered(raw.short$orrppe_2, levels = scale.zustimmung)
raw.short$orrppe_3 <- ordered(raw.short$orrppe_3, levels = scale.zustimmung)


raw.short$orrps_1 <- ordered(raw.short$orrps_1, levels = scale.zustimmung)
raw.short$orrps_2 <- ordered(raw.short$orrps_2, levels = scale.zustimmung)
raw.short$orrps_3 <- ordered(raw.short$orrps_3, levels = scale.zustimmung)
raw.short$orrps_4 <- ordered(raw.short$orrps_4, levels = scale.zustimmung)
raw.short$orrps_5 <- ordered(raw.short$orrps_5, levels = scale.zustimmung)

raw.short$orrpzg_1 <- ordered(raw.short$orrpzg_1, levels = scale.zustimmung)
raw.short$orrpzg_2 <- ordered(raw.short$orrpzg_2, levels = scale.zustimmung)
raw.short$orrpzg_3 <- ordered(raw.short$orrpzg_3, levels = scale.zustimmung)
raw.short$orrpzg_4 <- ordered(raw.short$orrpzg_4, levels = scale.zustimmung)


raw.short$orrpi_1 <- ordered(raw.short$orrpi_1, levels = scale.zustimmung)
raw.short$orrpi_2 <- ordered(raw.short$orrpi_2, levels = scale.zustimmung)
raw.short$orrpi_3 <- ordered(raw.short$orrpi_3, levels = scale.zustimmung)
raw.short$orrpi_4 <- ordered(raw.short$orrpi_4, levels = scale.zustimmung)


raw.short$Q127_1 <- ordered(raw.short$Q127_1, levels = scale.zustimmung)
raw.short$Q127_2<- ordered(raw.short$Q127_2, levels = scale.zustimmung)
raw.short$Q127_3 <- ordered(raw.short$Q127_3, levels = scale.zustimmung)
raw.short$Q127_4 <- ordered(raw.short$Q127_4, levels = scale.zustimmung)
raw.short$Q127_5 <- ordered(raw.short$Q127_5, levels = scale.zustimmung)
raw.short$Q127_6 <- ordered(raw.short$Q127_6, levels = scale.zustimmung)
raw.short$Q127_7 <- ordered(raw.short$Q127_7, levels = scale.zustimmung)


raw.short$Q128_1 <- ordered(raw.short$Q128_1, levels = scale.zustimmung)
raw.short$Q128_2<- ordered(raw.short$Q128_2, levels = scale.zustimmung)
raw.short$Q128_3 <- ordered(raw.short$Q128_3, levels = scale.zustimmung)
raw.short$Q128_4 <- ordered(raw.short$Q128_4, levels = scale.zustimmung)
raw.short$Q128_5 <- ordered(raw.short$Q128_5, levels = scale.zustimmung)
raw.short$Q128_6<- ordered(raw.short$Q128_6, levels = scale.zustimmung)
raw.short$Q128_7 <- ordered(raw.short$Q128_7, levels = scale.zustimmung)
raw.short$Q128_8 <- ordered(raw.short$Q128_8, levels = scale.zustimmung)
raw.short$Q128_9 <- ordered(raw.short$Q128_9, levels = scale.zustimmung)
raw.short$Q128_10<- ordered(raw.short$Q128_10, levels = scale.zustimmung)
raw.short$Q128_11 <- ordered(raw.short$Q128_11, levels = scale.zustimmung)
raw.short$Q128_12 <- ordered(raw.short$Q128_12, levels = scale.zustimmung)
raw.short$Q128_13 <- ordered(raw.short$Q128_13, levels = scale.zustimmung)

raw.short$Q129_1 <- ordered(raw.short$Q129_1, levels = scale.zustimmung)
raw.short$Q129_2<- ordered(raw.short$Q129_2, levels = scale.zustimmung)
raw.short$Q129_3 <- ordered(raw.short$Q129_3, levels = scale.zustimmung)
raw.short$Q129_4 <- ordered(raw.short$Q129_4, levels = scale.zustimmung)


raw.short$Q132_1 <- ordered(raw.short$Q132_1, levels = scale.zustimmung)
raw.short$Q132_2<- ordered(raw.short$Q132_2, levels = scale.zustimmung)


raw.short$Q133_1 <- ordered(raw.short$Q133_1, levels = scale.zustimmung)
raw.short$Q133_2<- ordered(raw.short$Q133_2, levels = scale.zustimmung)
raw.short$Q133_3 <- ordered(raw.short$Q133_3, levels = scale.zustimmung)
raw.short$Q133_4 <- ordered(raw.short$Q133_4, levels = scale.zustimmung)
raw.short$Q133_5 <- ordered(raw.short$Q133_5, levels = scale.zustimmung)
raw.short$Q133_6<- ordered(raw.short$Q133_6, levels = scale.zustimmung)
raw.short$Q133_7 <- ordered(raw.short$Q133_7, levels = scale.zustimmung)
raw.short$Q133_8 <- ordered(raw.short$Q133_8, levels = scale.zustimmung)
raw.short$Q133_9 <- ordered(raw.short$Q133_9, levels = scale.zustimmung)
raw.short$Q133_10<- ordered(raw.short$Q133_10, levels = scale.zustimmung)
raw.short$Q133_11 <- ordered(raw.short$Q133_11, levels = scale.zustimmung)
raw.short$Q133_12 <- ordered(raw.short$Q133_12, levels = scale.zustimmung)
raw.short$Q133_13 <- ordered(raw.short$Q133_13, levels = scale.zustimmung)

raw.short$Q135_1 <- ordered(raw.short$Q135_1, levels = scale.zustimmung)
raw.short$Q135_2<- ordered(raw.short$Q135_2, levels = scale.zustimmung)
raw.short$Q135_3 <- ordered(raw.short$Q135_3, levels = scale.zustimmung)
raw.short$Q135_4 <- ordered(raw.short$Q135_4, levels = scale.zustimmung)
raw.short$Q135_5 <- ordered(raw.short$Q135_5, levels = scale.zustimmung)



## Zweite Skala:
scale.zustimmung2 <-c("sie", 
                      "selten", 
                      "manchmal", 
                      "oft", 
                      "(fast) immer", 
                      "immer")

raw.short$Q83_1 <- ordered(raw.short$Q83_1, levels = scale.zustimmung)
raw.short$Q83_2 <- ordered(raw.short$Q83_2, levels = scale.zustimmung)
raw.short$Q83_3 <- ordered(raw.short$Q83_3, levels = scale.zustimmung)
raw.short$Q83_4 <- ordered(raw.short$Q83_4, levels = scale.zustimmung)

raw.short$Q87_1 <- ordered(raw.short$Q87_1, levels = scale.zustimmung)
raw.short$Q87_2 <- ordered(raw.short$Q87_2, levels = scale.zustimmung)
raw.short$Q87_3 <- ordered(raw.short$Q87_3, levels = scale.zustimmung)
raw.short$Q87_4 <- ordered(raw.short$Q87_4, levels = scale.zustimmung)

raw.short$Q91_1 <- ordered(raw.short$Q91_1, levels = scale.zustimmung)
raw.short$Q91_2 <- ordered(raw.short$Q91_2, levels = scale.zustimmung)
raw.short$Q91_3 <- ordered(raw.short$Q91_3, levels = scale.zustimmung)
raw.short$Q91_4 <- ordered(raw.short$Q91_4, levels = scale.zustimmung)

raw.short$Q95_1 <- ordered(raw.short$Q95_1, levels = scale.zustimmung)
raw.short$Q95_2 <- ordered(raw.short$Q95_2, levels = scale.zustimmung)
raw.short$Q95_3 <- ordered(raw.short$Q95_3, levels = scale.zustimmung)
raw.short$Q95_4 <- ordered(raw.short$Q95_4, levels = scale.zustimmung)

raw.short$Q99_1 <- ordered(raw.short$Q99_1, levels = scale.zustimmung)
raw.short$Q99_2 <- ordered(raw.short$Q99_2, levels = scale.zustimmung)
raw.short$Q99_3 <- ordered(raw.short$Q99_3, levels = scale.zustimmung)
raw.short$Q99_4 <- ordered(raw.short$Q99_4, levels = scale.zustimmung)

raw.short$Q103_1 <- ordered(raw.short$Q103_1, levels = scale.zustimmung)
raw.short$Q103_2 <- ordered(raw.short$Q103_2, levels = scale.zustimmung)
raw.short$Q103_3 <- ordered(raw.short$Q103_3, levels = scale.zustimmung)
raw.short$Q103_4 <- ordered(raw.short$Q103_4, levels = scale.zustimmung)

raw.short$Q107_1 <- ordered(raw.short$Q107_1, levels = scale.zustimmung)
raw.short$Q107_2 <- ordered(raw.short$Q107_2, levels = scale.zustimmung)
raw.short$Q107_3 <- ordered(raw.short$Q107_3, levels = scale.zustimmung)




