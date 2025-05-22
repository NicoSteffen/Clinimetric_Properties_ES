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

library(pROC)

library(easyRasch)
library(grateful)
library(ggrepel)
library(car)
library(kableExtra)
library(readxl)
library(eRm)
library(iarm)
library(mirt)
library(psych)
library(ggplot2)
library(psychotree)
library(matrixStats)
library(reshape)
library(knitr)
library(patchwork)
library(formattable) 
library(glue)
library(foreach)



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
long = read.csv2("long.csv")


# Neue Variable "Population" erstellen und auf "nicht-klinisch" setzen
data$Population <- "nicht-klinisch"
data <- data[, c("Population", setdiff(names(data), "Population"))]

# Data cleaning --------------------------------------------------------------

colnames(data)[colnames(data) == "lfdn"] <- "id"

#remove ID-1 (test run)
data <- data[data$id != 1, ]


#Number of participiants before exlusion: 

#view(data)

#remove ID with missing data
data <- data[data$Teilnahmeerklaerung != 2, ]

#exclusion of somatic = 1
data <- data[data$somatic != 1, ]

#exclusion of Schwanger = 1
data <- data[data$Schwanger != 1, ]

#excludion of age > 75

data <- data[data$age < 76, ]




#recode answers of ID 4 (changed coding afterwards)
data[data$id == 4, grep("^ES_", colnames(data))] <- 
  lapply(data[data$id == 4, grep("^ES_", colnames(data))], function(x) ifelse(x == 1, 2, ifelse(x == 2, 1, x)))


data[data$id == 4, grep("^MINI_current_|^MINI_past_", colnames(data))] <- 
  lapply(data[data$id == 4, grep("^MINI_", colnames(data))], function(x) ifelse(x == 1, 2, ifelse(x == 2, 1, x)))



#Jetzt unnötige Variablen entfernen
# Entferne die nicht benötigten Variablen
data <- data[, !names(data) %in% c("v_205", "v_208", "Versuchspersonenstunden", "Teilnahmeerklaerung", "somatic", "Schwanger")]



# sample characteristics --------------------------------------------------


# Umkodierung in Faktor
data$sex <- factor(data$sex, levels = c(1, 2, 3), labels = c("female", "male", "divers"))




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



# characteristics erstellen 

characteristics_non <- data[, c(
  "Population",
  "age",
  "sex",
  "Beziehungsstatus",
  "Beschaeftigungsverhaeltnis",
  "Sonstiges_Arbeitsverhaeltnis",
  "Bildungsabschluss",
  "Einkommen"
)]

write.csv2(characteristics_non, file = "characteristics_non.csv")

#create Rasch df 

raschnon <- data %>%
  select(starts_with("ES_"), Population, age, sex) %>%
  select(-starts_with("ES_likert"), -ES_total)

write.csv2(raschnon, file = "raschnon.csv")



# Datensatz bereinigen, nur Fragebogendaten -------------------------------

vars_to_remove <- c("age", "sex",
                    "Beziehungsstatus", "Kinder", "Beschaeftigungsverhaeltnis", 
                    "Sonstiges_Arbeitsverhaeltnis", "Bildungsabschluss", "Sonstiger_Abschluss", 
                    "Einkommen", "Wohnsituation")

data <- data[, !names(data) %in% vars_to_remove]


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

#BDI Gruppierung

# Erstellen der Gruppen basierend auf dem BDI_total Score
data$BDI_Group <- cut(data$BDI_total, 
                      breaks = c(-Inf, 13, 19, 28, Inf), 
                      labels = c("Group0: no depression / clinical irrelevant", 
                                 "Group1: mild depression", 
                                 "Group2: moderate depression", 
                                 "Group3: severe depression"), 
                      right = TRUE)

#table(data$BDI_Group)
#data$BDI_total

# MINBDI_total# MINI Grouping

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
                                                      "Group5: History of MDE + current MDE"),
                          ordered = TRUE)




# WHOQOL_bref

# Umkodierung der WHOQOL_3, WHOQOL_4 und WHOQOL_26 mit recode()
data$WHOQOL_3 <- dplyr::recode(data$WHOQOL_3, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
data$WHOQOL_4 <- dplyr::recode(data$WHOQOL_4, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
data$WHOQOL_26 <- dplyr::recode(data$WHOQOL_26, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)



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

data$PWB_1 <- dplyr::recode(data$PWB_1, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
data$PWB_3 <- dplyr::recode(data$PWB_3, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
data$PWB_5 <- dplyr::recode(data$PWB_5, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
data$PWB_6 <- dplyr::recode(data$PWB_6, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
data$PWB_8 <- dplyr::recode(data$PWB_8, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
data$PWB_10 <- dplyr::recode(data$PWB_10, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
data$PWB_11 <- dplyr::recode(data$PWB_11, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
data$PWB_14 <- dplyr::recode(data$PWB_14, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)

data$PWB_Autonomy = rowSums(data[,c("PWB_1", "PWB_9", "PWB_17")])
data$PWB_Environmental_Mastery = rowSums(data[,c("PWB_2", "PWB_11", "PWB_18")])
data$PWB_Personal_Growth = rowSums(data[,c("PWB_4", "PWB_12", "PWB_14")])
data$PWB_Rositive_Relations = rowSums(data[,c("PWB_5", "PWB_10", "PWB_13")])
data$PWB_Purpose_of_life = rowSums(data[,c("PWB_6", "PWB_8", "PWB_15")])
data$PWB_Self_Acceptance = rowSums(data[,c("PWB_3", "PWB_7", "PWB_16")])

data$PWB_total = rowSums(data[,c("PWB_Autonomy", "PWB_Environmental_Mastery", "PWB_Personal_Growth","PWB_Rositive_Relations","PWB_Purpose_of_life", "PWB_Self_Acceptance"  )])

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
data$WHO_total = data$WHO_total * 4

# Erstellen der Variable ES_likert_WHO als Summe der letzten 5 ES_likert Items
data$ES_likert_WHO <- rowSums(data[, c("ES_likert_6", "ES_likert_7", "ES_likert_8", "ES_likert_9", "ES_likert_10")], na.rm = TRUE)


# BSI

# Umkodierung der BSI_ Variablen: 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4
data[, grep("^BSI_", colnames(data))] <- lapply(data[, grep("^BSI_", colnames(data))], function(x) {
  ifelse(x == 1, 0, ifelse(x == 2, 1, ifelse(x == 3, 2, ifelse(x == 4, 3, ifelse(x == 5, 4, x)))))
})


# Berechnung der GSI (Global Severity Index) als Mittelwert aller BSI-Items von 1 bis 53
data$GSI <- rowMeans(data[, paste0("BSI_", 1:53)], na.rm = TRUE)


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


non_variables = data[, c(
  "ES_total",
  "ES_likert_total",
  "BDI_total",
  "WHOQOL_total",
  "PWB_Autonomy",
  "PWB_Environmental_Mastery",
  "PWB_Personal_Growth",
  "PWB_Rositive_Relations",
  "PWB_Purpose_of_life",
  "PWB_Self_Acceptance",
  "PWB_total",
  "CDRISC_total",
  "GSI",
  "WHO_total"
)]

write.csv2(non_variables, file = "non_variables.csv")



# LMU Datensatz hinzufügen ------------------------------------------------

df = read.csv2("cleanlmu_t0.csv")

data <- rbind(data, df)

# Datensatz clean speichern 
write.csv2(data, file = "clean.csv", row.names = FALSE)


# Correlation analyses ----------------------------------------------------

# Question 12 Correlation Analyses-------------------------------------------------------------

cor = data[, c("ES_total", "GSI", "WHOQOL_total", "CDRISC_total", "PWB_total", "BDI_total")]

descriptive_stats = data.frame(
  n = colSums(!is.na(cor)),
  M = sapply(cor, mean, na.rm = TRUE),
  SD = sapply(cor, sd, na.rm = TRUE)
)

correlation_matrix = cor(cor, method = "spearman", use = "pairwise.complete.obs")

final_table = cbind(descriptive_stats,correlation_matrix )


final_table <- roundallnumerics(final_table, 2)
final_table = flextable(final_table)

# p-values
cor_results <- Hmisc::rcorr(as.matrix(cor))

# FDR
p_adj_matrix <- matrix(
  p.adjust(cor_results$P, method = "BH"),
  nrow = nrow(cor_results$P),
  ncol = ncol(cor_results$P),
  dimnames = dimnames(cor_results$P)
)


# normality tests:

# Extract relevant variables
cor <- data[, c("ES_total", "GSI", "WHOQOL_total", "CDRISC_total", "PWB_total", "BDI_total")]

# Shapiro-Wilk tests for normality
shapiro_results <- lapply(cor, shapiro.test)
shapiro_results


# Histograms and QQ plots
par(mfrow = c(2, 2))  # 2x2 plotting grid; adjust if needed

for (varname in names(cor)) {
  hist(cor[[varname]], main = paste("Histogram of", varname), xlab = varname, col = "lightblue", breaks = 20)
  qqnorm(cor[[varname]], main = paste("QQ Plot of", varname))
  qqline(cor[[varname]], col = "red")
}

# Reset plotting layout
par(mfrow = c(1, 1))



# Question 2 Rasch---------------------------------------------------------------




# Question 3 Predict responder---------------------------------------------------------------


# WHO-5 -------------------------------------------------------------------
library(mlr)

library(mlr)

# Sicherstellen, dass Zielvariable ein Faktor ist
long$WHO_responder <- factor(long$WHO_responder, levels = c("No", "Yes"))

# Subsets erstellen
WHO_dichotom <- data.frame(total = long$t0_ES_total, response = long$WHO_responder)
WHO_likert <- data.frame(total = long$t0_ES_likert_total, response = long$WHO_responder)

# Modell erstellen (klassisch)
mod <- glm(response ~ total, data = WHO_dichotom, family = "binomial")
summary(mod)

# Vorhersage testen
predict(mod, newdata = WHO_dichotom, type = "response")

# mlr: Klassifikations-Task
task <- makeClassifTask(target = "response", positive = "Yes", data = WHO_dichotom)
log <- makeLearner("classif.logreg", predict.type = "prob")

# Resampling-Strategie
desc <- makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)

# Metriken definieren
measures <- list(auc, acc, mmce)

# Benchmark starten
bmr1 <- benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)




# Question 4 ---------------------------------------------------------------


# Question 5 ---------------------------------------------------------------

#Group 3 ausschließen da nicht in Japanese Study und nur 3 Pax!
# in lmu 2 x NA in Lifetime!!

d <- data[data$MINI_Group != "Group3: First depressive episode", ]


#dichtotom
model = lm(ES_total ~ MINI_Group, data = d)
anova(model)

emmeans::emmeans(model, pairwise ~ MINI_Group)

install.packages("clinfun")
library(clinfun)


jonckheere.test(x = d$ES_total, g = d$MINI_Group, alternative = "decreasing")


?JonckheereTest

#likert
model = lm(ES_likert_total ~ MINI_Group, data = d)
anova(model)

emmeans::emmeans(model, pairwise ~ MINI_Group)

# Question 6 ---------------------------------------------------------------

#dichtotom
model = lm(ES_total ~ BDI_Group, data = data)
anova(model)
omega_squared(anova(model))

emmeans::emmeans(model, pairwise ~ BDI_Group)

#likert
model = lm(ES_likert_total ~ BDI_Group, data = data)
anova(model)
omega_squared(anova(model))

emmeans::emmeans(model, pairwise ~ BDI_Group)

# Question 7 Cut off Score---------------------------------------------------------------


# Create binary variable for MDE based on BDI cutoff
data$MDE <- ifelse(data$BDI_total >= 13, 1, 0)

# Perform ROC analysis
roc_result <- roc(data$MDE, data$ES_total)


# Plot ROC curve
plot(roc_result, main="ROC Curve for ES Cutoff")

# Determine optimal cutoff using Youden's J statistic
optimal_cutoff <- coords(roc_result, "best", ret = "threshold", best.method = "youden")

# Print the optimal cutoff
print(optimal_cutoff)

cutoff_data <- data.frame(
  Threshold = roc_result$thresholds,
  Sensitivity = roc_result$sensitivities,
  Specificity = roc_result$specificities
)

# Alternative mit MINI

data$MDE_MINI <- ifelse(data$Mini_current_MDE == "MDE", 1, 0)

# Perform ROC analysis
roc_result <- roc(data$MDE_MINI, data$ES_total)

# Plot ROC curve
plot(roc_result, main="ROC Curve for ES Cutoff")

# Determine optimal cutoff using Youden's J statistic
optimal_cutoff <- coords(roc_result, "best", ret = "threshold", best.method = "youden")

# Print the optimal cutoff
print(optimal_cutoff)

test = data.frame(data$MDE, data$MDE_MINI)

#--> Weniger Diagnosen MDE nach MINI als BDI

# Question 8 ---------------------------------------------------------------

long$ES_change = long$t0_ES_total - long$t1_ES_total 
long$ES_likert_change = long$t0_ES_likert_total - long$t1_ES_likert_total 
long$ES_change_cent = long$ES_change - mean(long$ES_change)
long$ES_likert_change_cent = long$ES_likert_change - mean(long$ES_likert_change)
long$BDI_t0_cent = long$t0_BDI_total - mean(long$t0_BDI_total)

model = lm(BDI_change ~ long$ES_change_cent + long$BDI_t0_cent, data = long )
summary(model)

model = lm(BDI_change ~ long$ES_likert_change_cent + long$BDI_t0_cent, data = long )
summary(model)


# Question 9 ---------------------------------------------------------------

#dichotom
model = lm(GSI ~ ES_total, data)
summary(model)

#likert

model = lm(GSI ~ ES_likert_total, data)
summary(model)


# Question 10 ---------------------------------------------------------------


model1 = lm(PWB_total ~ sex + age + Bildungsabschluss + WHO_total, data)
model2 = lm(PWB_total ~ sex + age + Bildungsabschluss + WHO_total + ES_likert_total, data)

anova(model1,model2)

summary(model1)
summary(model2)

view(data)

cor(data$PWB_Autonomy,data$ES_likert_total)

plot(data$PWB_total~data$ES_likert_total)
plot(data$PWB_Autonomy~data$ES_likert_total)
plot(data$PWB_Environmental_Mastery~data$ES_likert_total)

PWB_Autonomy
PWB_Environmental_Mastery

# Komische korrelation zu PWB!!! Anschauen warum? Richtig codiert?



# Question 11 -------------------------------------------------------------









cor.test(data$ES_likert_WHO, data$WHO_total)

t.test(data$ES_likert_WHO,data$WHO_total )

#stelle des Fragebogens weiter hinten -> negativer weil davor negative Fragebögen abgefragt wurden 
# oder beeinflussen die ersten 5 Fragen der ES die Antwort? --> Forschungsidee!!!

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






# Faktorenanalyse ---------------------------------------------------------


# Perform parallel analysis
fa.parallel(data_ES, fa = "fa", n.iter = 100, show.legend = TRUE, main = "Parallel Analysis")



data_ES
data_ES_likert


efa_result <- fa(data_ES_likert, nfactors = 2, rotate = "promax")




# Define the model
model2 <- '
  Factor1 =~ ES_1 + ES_2 + ES_3 + ES_4 + ES_5
  Factor2 =~ ES_6 + ES_7 + ES_8 + ES_9 + ES_10
'

# Assuming 'data' contains your items
cfa_result2 <- cfa(model2, data = data_ES)

# Summarize the results
summary(cfa_result2, fit.measures = TRUE, standardized = TRUE)


# Define the model
model <- '
  Factor1 =~ ES_likert_1 + ES_likert_2 + ES_likert_3 + ES_likert_4 + ES_likert_5
  Factor2 =~ ES_likert_6 + ES_likert_7 + ES_likert_8 + ES_likert_9 + ES_likert_10
'

# Assuming 'data' contains your items
cfa_result <- cfa(model, data = data_ES_likert)

# Summarize the results
summary(cfa_result, fit.measures = TRUE, standardized = TRUE)


model = lm(BDI_total ~ ES_likert_total, data)
summary(model)



# zeug s

mod = lm(PWB_total~sex+age+Beziehungsstatus+Beschaeftigungsverhaeltnis+Bildungsabschluss+Einkommen+Wohnsituation, data = data)
summary(mod)

mod = lm(PWB_Purpose_of_life~sex+age+Beziehungsstatus+Beschaeftigungsverhaeltnis+Bildungsabschluss+Einkommen+Wohnsituation, data = data)
summary(mod)

names(data)



