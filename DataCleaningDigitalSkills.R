# Analyse Skript

library(tidyverse)

source("data/qualtricshelpers.R")

# Rohdaten laden
#raw <- read_csv("data/DigitalSkillsDaten.csv")

## Daten mit Funktion load_qualtrics_csv einlesen: 
Datensatz <- "data/DigitalSkillsDaten.csv"
raw <- load_qualtrics_csv(Datensatz)

#### Daten bereinigen ----
### Schritt 1: Unnötige Spalten löschen. 
## Die ausgeschlossenen Fragen sind Systemdaten, Items von anderen Gruppenmitgliedern etc.:
raw.short <- raw[,c(-1:-16, -23:-27, -32:-132, -169:-174)]
raw.short2 <- raw.short[,c(-1, -8, -11:-19)]


### Schritt 2: Variablen umbenennen 
## Eine eigene Datei mit den Variablennamen erzeugen:
generate_codebook(raw.short2, Datensatz, "data/codebook.csv")

codebook <- read_codebook("data/codebook_final.csv")

names(raw.short2) <- codebook$variable

### Schritt 3: Variablen den richtigen Typen zuordnen
## Gender zu kategorialer Variable machen:
raw.short2$geschlecht <- as.factor(raw.short2$geschlecht)

## Schulabschluss zu ordinaler Variable machen:
raw.short2$schualabschluss <- ordered(raw.short2$schulabschluss, levels = c("Kein Schulabschluss",
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
raw.short2$taetigkeit <- ordered(raw.short2$taetigkeit, levels = c("Schüler",
                                                       "Student",
                                                       "Auszubildender",
                                                       "Angestellter",
                                                       "Selbstständiger",
                                                       "Rentner",
                                                       "Arbeitssuchender"))


## Skala für Likertskala einmal anlegen...


##Skala:
scale.zustimmung <-c("nie", 
                      "selten", 
                      "manchmal", 
                      "oft", 
                      "(fast) immer", 
                      "immer")

raw.short2$if1 <- ordered(raw.short2$if1, levels = scale.zustimmung)
raw.short2$if2 <- ordered(raw.short2$if2, levels = scale.zustimmung)
raw.short2$if3 <- ordered(raw.short2$if3, levels = scale.zustimmung)
raw.short2$if4 <- ordered(raw.short2$if4, levels = scale.zustimmung)

raw.short2$komf1 <- ordered(raw.short2$komf1, levels = scale.zustimmung)
raw.short2$komf2 <- ordered(raw.short2$komf2, levels = scale.zustimmung)
raw.short2$komf3 <- ordered(raw.short2$komf3, levels = scale.zustimmung)
raw.short2$komf4 <- ordered(raw.short2$komf4, levels = scale.zustimmung)

raw.short2$kollf1 <- ordered(raw.short2$kollf1, levels = scale.zustimmung)
raw.short2$kollf2 <- ordered(raw.short2$kollf2, levels = scale.zustimmung)
raw.short2$kollf3 <- ordered(raw.short2$kollf3, levels = scale.zustimmung)
raw.short2$kollf4 <- ordered(raw.short2$kollf4, levels = scale.zustimmung)

raw.short2$fkd1 <- ordered(raw.short2$fkd1, levels = scale.zustimmung)
raw.short2$fkd2 <- ordered(raw.short2$fkd2, levels = scale.zustimmung)
raw.short2$fkd3 <- ordered(raw.short2$fkd3, levels = scale.zustimmung)
raw.short2$fkd4 <- ordered(raw.short2$fkd4, levels = scale.zustimmung)

raw.short2$fks1 <- ordered(raw.short2$fks1, levels = scale.zustimmung)
raw.short2$fks2 <- ordered(raw.short2$fks2, levels = scale.zustimmung)
raw.short2$fks3 <- ordered(raw.short2$fks3, levels = scale.zustimmung)
raw.short2$fks4 <- ordered(raw.short2$fks4, levels = scale.zustimmung)

raw.short2$pf1 <- ordered(raw.short2$pf1, levels = scale.zustimmung)
raw.short2$pf2 <- ordered(raw.short2$pf2, levels = scale.zustimmung)
raw.short2$pf3 <- ordered(raw.short2$pf3, levels = scale.zustimmung)
raw.short2$pf4 <- ordered(raw.short2$pf4, levels = scale.zustimmung)

raw.short2$ict1 <- ordered(raw.short2$ict1, levels = scale.zustimmung)
raw.short2$ict2 <- ordered(raw.short2$ict2, levels = scale.zustimmung)
raw.short2$ict3 <- ordered(raw.short2$ict3, levels = scale.zustimmung)

### Schritt 4: Skalen berechnen

## Jetzt benötigen wir die psych-bibliothek.
library(psych)

## Der scoreItems-Befehl benötigt eine Liste der folgenden Gestalt. Negative Items sind mit Minus gekennzeichnet.


schluesselliste <- list (ICT = c("ict1", "ict2", "ict3"),
                         Informationsfähigkeit = c("if1", "if2", "if3", "if4"),
                         Kommunikationsfähigkeit = c("komf1", "komf2", "komf3", "komf4"))

                      
## Hier werden die Skalen berechnet: 
scores <- scoreItems(schluesselliste, raw.short2, missing = TRUE, min = 1, max = 6)


## Die errechneten Scores ATI, VBA usw. werden hinten als Spalten an raw.short angefügt:
data <- bind_cols(raw.short2, as_tibble(scores$scores))

## Über den pipe-operator %>% sprechen wir nochmal in Ruhe. 
## Da wir die Konstrukte ja schon berechnet haben, haben wir für die einzelnen Items keine Verwendung mehr. 
## Hierdurch entfernen wir alle Einzelitems. Wörtlich: "Entferne alle Spalten, die mit kleingeschriebenem "ati" beginnen usw. 
data <- data %>% 
  select(-starts_with("ict", ignore.case = F)) %>% 
  select(-starts_with("informationsfähigkeit", ignore.case = F)) %>%
  select(-starts_with("kommunikationsfähigkeit", ignore.case = F))

## data sieht jetzt genau so aus wie in der Musterlösung und kann abgespeichert werden:
saveRDS(data, "data/data.rds")

## Fertig.            
                        
                        
                      


