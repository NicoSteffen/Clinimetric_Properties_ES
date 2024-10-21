# Load Packages -----------------------------------------------------------

library(foreign)
library(tidyverse)
library(easystats)
library(lme4)
library(lmerTest)
library(dendextend)
library(lavaan)
library(tidyr)
library(dplyr)
library(moments)
library(psych)

# Create custom functions -------------------------------------------------

roundallnumerics = function(df, digits){
  for(j in 1:ncol(df)){
    if(is.numeric(df[,j])){
      df[,j] = round(df[,j], digits)
    }
  }
  return(df)
}

p_labeller = function(vec){
  vec = as.numeric(vec)
  for(i in 1:length(vec)){
    if(is.na(vec[i]) == F & vec[i] < .001){
      vec[i] = "<.001***"
    }
    if(is.na(vec[i]) == F & vec[i] >= .001 & vec[i] < .01){
      vec[i] = paste0(vec[i], "**")
    }
    if(is.na(vec[i]) == F & vec[i] > .01 & vec[i] < .05){
      vec[i] = paste0(vec[i], "*")
    }
  }
  return(vec)
}

# Read data ---------------------------------------------------------------

data = read.csv2("data.csv")


# Data cleaning --------------------------------------------------------------

colnames(data)[colnames(data) == "lfdn"] <- "id"

#remove ID-1 (test run)
data <- data[data$id != 1, ]

#exclusion of somatic = 1
data <- data[data$somatic != 1, ]

#exclusion of Schwanger = 1
data <- data[data$Schwanger != 1, ]

#recode answers of ID 4 (changed coding afterwards)
data[data$id == 4, grep("^ES_", colnames(data))] <- 
  lapply(data[data$id == 4, grep("^ES_", colnames(data))], function(x) ifelse(x == 1, 2, ifelse(x == 2, 1, x)))


data[data$id == 4, grep("^MINI_current_|^MINI_past_", colnames(data))] <- 
  lapply(data[data$id == 4, grep("^MINI_", colnames(data))], function(x) ifelse(x == 1, 2, ifelse(x == 2, 1, x)))



# sample characteristics --------------------------------------------------

characteristics = c("sex",
                    "age",
                    "Beziehungsstatus",
                    "Kinder",
                    "Beschaeftigungsverhaeltnis",
                    "Bildungsabschluss",
                    "Einkommen",
                    "Wohnsituation")

# Umkodierung in Faktor
data$sex <- factor(data$sex, levels = c(1, 2, 3), labels = c("weiblich", "männlich", "non-binär/divers"))

data$Beziehungsstatus <- factor(data$Beziehungsstatus, levels = c(1, 2, 3, 4, 5, 6, 7),
                                labels = c("Verheiratet", "Verwitwet", "Geschieden", "Getrennt lebend", "Single",
                                           "In einer festen Partnerschaft (zusammen lebend)", 
                                           "In einer festen Partnerschaft (getrennt lebend)"))

data$Kinder <- factor(data$Kinder, levels = c(1, 2, 3, 4, 5), 
                      labels = c("Nein, keine Kinder", "Ja, 1 Kind", "Ja, 2 Kinder", "Ja, 3 Kinder", "Ja, 4 Kinder oder mehr"))

data$Beschaeftigungsverhaeltnis <- factor(data$Beschaeftigungsverhaeltnis, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 
                                          labels = c("Student/in", "Angestellt (Vollzeit)", "Angestellt (Teilzeit)", 
                                                     "Selbstständig", "Freiberuflich tätig", "Arbeitslos", 
                                                     "In Ausbildung", "Rentner/in", "Hausfrau/Hausmann", 
                                                     "Elternzeit", "Sonstiges Arbeitsverhältnis"))

data$Bildungsabschluss <- factor(data$Bildungsabschluss, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15), 
                                 labels = c("Keinen Schulabschluss", "Hauptschulabschluss", "Realschulabschluss", 
                                            "Fachabitur (FOS, BOS)", "Allgemeines Abitur", "Abgeschlossene Ausbildung", 
                                            "Hochschule (Diplom)", "Hochschule (Magister)", "Hochschule (Bachelor)", 
                                            "Hochschule (Master)", "Hochschule (Promotion)", "Hochschule (Habilitation)", 
                                            "Hochschule (Staatsexamen)", "Sonstiger Abschluss"))

data$Einkommen <- factor(data$Einkommen, levels = c(1, 2, 3, 4, 5, 6, 7), 
                         labels = c("Unter 1.000 €", "1.000 - 1.999 €", "2.000 - 2.999 €", "3.000 - 3.999 €", 
                                    "4.000 - 4.999 €", "5.000 oder mehr", "keine Angabe"))

data$Wohnsituation <- factor(data$Wohnsituation, levels = c(1, 2, 3, 4, 5), 
                             labels = c("Bei meinen Eltern/Verwandten", "Eigene Wohnung/Haus (zur Miete)", 
                                        "Eigene Wohnung/Haus (Eigentum)", "WG (zur Miete)", "WG (Eigentum)"))



flextable::save_as_docx(flextable::flextable(report_sample(
  data[, c(characteristics)],group_by = "sex")), path = "Table1.docx")


# Skalen umkodieren + aggregation ------------------------------------------------------

# ES_dichotom

# Umkodierung der ES_dichotom-Variablen: 1 -> 0 ; 2 -> 1
data[, grep("^ES_(?!likert)", colnames(data), perl = TRUE)] <- 
  lapply(data[, grep("^ES_(?!likert)", colnames(data), perl = TRUE)],
         function(x) ifelse(x == 1, 0, ifelse(x == 2, 1, x)))

data$ES_total <- rowSums(data[, grep("^ES_[1-9]$|^ES_10$", colnames(data))], na.rm = TRUE)


# ES_likert

# Umkodierung der ES_likert_ Variablen: 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4, 6 -> 5
data[, grep("^ES_likert_", colnames(data))] <- lapply(data[, grep("^ES_likert_", colnames(data))], function(x) {
  ifelse(x == 1, 0, ifelse(x == 2, 1, ifelse(x == 3, 2, ifelse(x == 4, 3, ifelse(x == 5, 4, ifelse(x == 6, 5, x))))))
})

data$ES_likert_total = rowSums(data[, grep("ES_likert", colnames(data))], na.rm = TRUE)



# BDI_

# Umkodierung der allgemeinen BDI_ Variablen außer BDI_16 und BDI_18
bdigeneral_vars <- grep("^BDI_", colnames(data), value = TRUE)
bdigeneral_vars <- setdiff(bdigeneral_vars, c("BDI_16", "BDI_18"))

data[, bdigeneral_vars] <- lapply(data[, bdigeneral_vars], function(x) {
  ifelse(x == 1, 0, ifelse(x == 2, 1, ifelse(x == 3, 2, ifelse(x == 4, 3, x))))
})

# Spezielle Umkodierung für BDI_16 und BDI_18
data$BDI_16 <- ifelse(data$BDI_16 == 1, 0, 
                      ifelse(data$BDI_16 %in% c(2, 3), 1, 
                             ifelse(data$BDI_16 %in% c(4, 5), 2, 
                                    ifelse(data$BDI_16 %in% c(6, 7), 3, data$BDI_16))))

data$BDI_18 <- ifelse(data$BDI_18 == 1, 0, 
                      ifelse(data$BDI_18 %in% c(2, 3), 1, 
                             ifelse(data$BDI_18 %in% c(4, 5), 2, 
                                    ifelse(data$BDI_18 %in% c(6, 7), 3, data$BDI_18))))


data$BDI_total = rowSums(data[, grep("BDI_", colnames(data))], na.rm = TRUE)



# MINI Grouping

data[, grep("^MINI_", colnames(data))] <- 
  lapply(data[, grep("^MINI_", colnames(data))], function(x) ifelse(x == 1, 0, ifelse(x == 2, 1, x)))


# Berechnung der Summe der MINI_current und MINI_past Variablen
mini_current_sum <- rowSums(data[, grep("^MINI_current_", colnames(data))], na.rm = TRUE)
mini_past_sum <- rowSums(data[, grep("^MINI_past_", colnames(data))], na.rm = TRUE)

# Codierung für Mini_lifetime_MDE (YES/NO)
data$Mini_lifetime_MDE <- ifelse(mini_past_sum >= 0 & mini_past_sum <= 4, "NO", 
                                 ifelse(mini_past_sum >= 5 & mini_past_sum <= 9, "YES", NA))

# Codierung für Mini_current_MDE (MDE/Subthreshold Depression/None)
data$Mini_current_MDE <- ifelse(mini_current_sum == 0, "None", 
                                ifelse(mini_current_sum >= 1 & mini_current_sum <= 4, "Subthreshold Depression", 
                                       ifelse(mini_current_sum >= 5 & mini_current_sum <= 9, "MDE", NA)))

# Als Faktoren codieren
data$Mini_lifetime_MDE <- factor(data$Mini_lifetime_MDE, levels = c("NO", "YES"))
data$Mini_current_MDE <- factor(data$Mini_current_MDE, levels = c("None", "Subthreshold Depression", "MDE"))

# Erstellung der Gruppen basierend auf Mini_current_MDE und Mini_lifetime_MDE
data$MINI_Group <- ifelse(data$Mini_current_MDE == "None" & data$Mini_lifetime_MDE == "NO", "Group0: No history of MDE - healthy",
                          ifelse(data$Mini_current_MDE == "None" & data$Mini_lifetime_MDE == "YES", "Group1: Full remission",
                                 ifelse(data$Mini_current_MDE == "Subthreshold Depression" & data$Mini_lifetime_MDE == "NO", "Group2: First subthreshold depressive episode",
                                        ifelse(data$Mini_current_MDE == "MDE" & data$Mini_lifetime_MDE == "NO", "Group3: First depressive episode",
                                               ifelse(data$Mini_current_MDE == "Subthreshold Depression" & data$Mini_lifetime_MDE == "YES", "Group4: History of MDE + current subthreshold",
                                                      ifelse(data$Mini_current_MDE == "MDE" & data$Mini_lifetime_MDE == "YES", "Group5: History of MDE + current MDE", NA))))))

# Optional: Als Faktor kodieren, um die Gruppen geordnet darzustellen
data$MINI_Group <- factor(data$MINI_Group, levels = c("Group0: No history of MDE - healthy", 
                                                      "Group1: Full remission", 
                                                      "Group2: First subthreshold depressive episode", 
                                                      "Group3: First depressive episode", 
                                                      "Group4: History of MDE + current subthreshold", 
                                                      "Group5: History of MDE + current MDE"))




# WHOQOL_bref

# Umkodierung der WHOQOL_3, WHOQOL_4 und WHOQOL_26 mit recode()
data$WHOQOL_3 <- recode(data$WHOQOL_3, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
data$WHOQOL_4 <- recode(data$WHOQOL_4, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
data$WHOQOL_26 <- recode(data$WHOQOL_26, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)

# Berechnung der 4 WHOQOL-Domains (Konvertiert 0 - 100)


# Physical Health Domain (konvertiert)
data$WHOQOL_Physical_Health_Converted <- (4 * rowMeans(data[, c("WHOQOL_3", "WHOQOL_4", "WHOQOL_10", "WHOQOL_15", "WHOQOL_16", "WHOQOL_17", "WHOQOL_18")], na.rm = TRUE) - 4) * (100 / 16)

# Psychological Domain (konvertiert)
data$WHOQOL_Psychological_Converted <- (4 * rowMeans(data[, c("WHOQOL_5", "WHOQOL_6", "WHOQOL_7", "WHOQOL_11", "WHOQOL_19", "WHOQOL_26")], na.rm = TRUE) - 4) * (100 / 16)

# Social Relationships Domain (konvertiert)
data$WHOQOL_Social_Relationships_Converted <- (4 * rowMeans(data[, c("WHOQOL_20", "WHOQOL_21", "WHOQOL_22")], na.rm = TRUE) - 4) * (100 / 16)

# Environment Domain (konvertiert)
data$WHOQOL_Environment_Converted <- (4 * rowMeans(data[, c("WHOQOL_8", "WHOQOL_9", "WHOQOL_12", "WHOQOL_13", "WHOQOL_14", "WHOQOL_23", "WHOQOL_24", "WHOQOL_25")], na.rm = TRUE) - 4) * (100 / 16)

# QOL total (Diese Berechnung ist so eigentlich nicht vorgesehen)
data$WHOQOL_total <- rowMeans(data[, c("WHOQOL_Physical_Health_Converted", "WHOQOL_Psychological_Converted", "WHOQOL_Social_Relationships_Converted", "WHOQOL_Environment_Converted")], na.rm = TRUE)


# PWB

#TBA

# CDRISC

# Umkodierung der CDRISC_ Variablen: 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4
data[, grep("^CDRISC_", colnames(data))] <- lapply(data[, grep("^CDRISC_", colnames(data))], function(x) {
  ifelse(x == 1, 0, ifelse(x == 2, 1, ifelse(x == 3, 2, ifelse(x == 4, 3, ifelse(x == 5, 4, x)))))
})

# Erstellen der CDRISC_total Variable als Summenscore der CDRISC_ Items
data$CDRISC_total <- rowSums(data[, grep("^CDRISC_", colnames(data))], na.rm = TRUE)

# WHO5

# Umkodierung der WHO_ Variablen: 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4, 6 -> 5
data[, grep("^WHO_", colnames(data))] <- lapply(data[, grep("^WHO_", colnames(data))], function(x) {
  ifelse(x == 1, 0, ifelse(x == 2, 1, ifelse(x == 3, 2, ifelse(x == 4, 3, ifelse(x == 5, 4, ifelse(x == 6, 5, x))))))
})

data$WHO_total = rowSums(data[, grep("WHO_[1-5]", colnames(data))], na.rm = TRUE)


# Erstellen der Variable ES_likert_WHO als Summe der letzten 5 ES_likert Items
data$ES_likert_WHO <- rowSums(data[, c("ES_likert_6", "ES_likert_7", "ES_likert_8", "ES_likert_9", "ES_likert_10")], na.rm = TRUE)


# BSI

# Umkodierung der BSI_ Variablen: 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4
data[, grep("^BSI_", colnames(data))] <- lapply(data[, grep("^BSI_", colnames(data))], function(x) {
  ifelse(x == 1, 0, ifelse(x == 2, 1, ifelse(x == 3, 2, ifelse(x == 4, 3, ifelse(x == 5, 4, x)))))
})


# Berechnung der GSI (Global Severity Index) als Mittelwert aller BSI-Items von 1 bis 53
data$GSI <- rowSums(data[, paste0("BSI_", 1:53)], na.rm = TRUE)


# Berechnung der Summenwerte für die einzelnen BSI-Skalen

# Somatisierung
data$BSI_Somatisierung <- rowSums(data[, c("BSI_2", "BSI_7", "BSI_23", "BSI_29", "BSI_30", "BSI_33", "BSI_37")], na.rm = TRUE)

# Zwanghaftigkeit
data$BSI_Zwanghaftigkeit <- rowSums(data[, c("BSI_5", "BSI_15", "BSI_26", "BSI_27", "BSI_32", "BSI_36")], na.rm = TRUE)

# Unsicherheit im Sozialkontakt
data$BSI_Sozialkontakt <- rowSums(data[, c("BSI_20", "BSI_21", "BSI_22", "BSI_42")], na.rm = TRUE)

# Depressivität
data$BSI_Depressivität <- rowSums(data[, c("BSI_9", "BSI_16", "BSI_17", "BSI_18", "BSI_35", "BSI_50")], na.rm = TRUE)

# Ängstlichkeit
data$BSI_Ängstlichkeit <- rowSums(data[, c("BSI_1", "BSI_12", "BSI_19", "BSI_38", "BSI_45", "BSI_49")], na.rm = TRUE)

# Aggressivität / Feindseligkeit
data$BSI_Aggressivität <- rowSums(data[, c("BSI_6", "BSI_13", "BSI_40", "BSI_41", "BSI_46")], na.rm = TRUE)

# Phobische Angst
data$BSI_Phobische_Angst <- rowSums(data[, c("BSI_8", "BSI_28", "BSI_31", "BSI_43", "BSI_47")], na.rm = TRUE)

# Paranoides Denken
data$BSI_Paranoides_Denken <- rowSums(data[, c("BSI_4", "BSI_10", "BSI_24", "BSI_48", "BSI_51")], na.rm = TRUE)

# Psychotizismus
data$BSI_Psychotizismus <- rowSums(data[, c("BSI_3", "BSI_14", "BSI_34", "BSI_44", "BSI_53")], na.rm = TRUE)

# Zusatzskala
data$BSI_Zusatz <- rowSums(data[, c("BSI_11", "BSI_25", "BSI_39", "BSI_52")], na.rm = TRUE)







view(data)


cor.test(data$ES_likert_WHO, data$WHO_total)

#stelle des Fragebogens weiter hinten -> negativer weil davor negative Fragebögen abgefragt wurden 
# oder beeinflussen die ersten 5 Fragen der ES die Antwort?

# Interessante Forschungsfrage!!!
mean(data$ES_likert_WHO)
mean(data$WHO_total)

model = lm(ES_total ~ MINI_Group, data = data)
anova(model)

emmeans::emmeans(model, pairwise ~ MINI_Group)











model = lm(BDI_total ~ ES_total, data = data)
summary(model)

model2 = lm(BDI_total ~ ES_likert_total, data = data)
summary(model2)


