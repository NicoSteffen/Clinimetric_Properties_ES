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
library(RISEkbmRasch)
library(pROC)
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

# Neue Variable "Population" erstellen und auf "nicht-klinisch" setzen
data$Population <- "nicht-klinisch"
data <- data[, c("Population", setdiff(names(data), "Population"))]

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

#remove ID with missing data
data <- data[data$Teilnahmeerklaerung != 2, ]

#Jetzt unnötige Variablen entfernen
# Entferne die nicht benötigten Variablen
data <- data[, !names(data) %in% c("v_205", "v_208", "Versuchspersonenstunden", "Teilnahmeerklaerung", "somatic", "Schwanger")]



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

table(data$BDI_Group)
data$BDI_total

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
                                                      "Group5: History of MDE + current MDE"))




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




# LMU Datensatz hinzufügen ------------------------------------------------

df = read.csv2("cleanlmu_t0.csv")

data <- rbind(data, df)

# Datensatz clean speichern 
write.csv2(data, file = "clean.csv", row.names = FALSE)

# Question 2 Rasch---------------------------------------------------------------

select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

#ES_likert

#select data

df <- data %>% 
  select(starts_with("ES_likert_"),
         age,sex)  %>% 
  select(!ES_likert_total) %>% 
  select(!ES_likert_WHO)

glimpse(df)

itemlabels <- df %>% 
  select(starts_with("ES_likert_")) %>% 
  names() %>% 
  as_tibble() %>% 
  separate(value, c(NA, "item"), sep ="_[0-9][0-9]_") %>% 
  mutate(itemnr = paste0("ES_likert_",c(1:10)), .before = "item")

#remove non-binär

df <- df %>% 
  filter(sex %in% c("männlich","weiblich"))

#Vektor for sex
dif.sex <- factor(df$sex)

#and remove from df
df$sex <- NULL

#schöne tabelle mit percent
RIdemographics(dif.sex, "Sex")

#weiter mit age
glimpse(df$age)

#Erstelle ggplot für Altersverteilung
ggplot(df) +
  geom_histogram(aes(x = age), 
                 fill = "#009ca6",
                 col = "black") +
  # add the average as a vertical line
  geom_vline(xintercept = mean(df$age), 
             linewidth = 1.5,
             linetype = 2,
             col = "orange") +
  # add a light grey field indicating the standard deviation
  annotate("rect", ymin = 0, ymax = Inf, 
           xmin = (mean(df$age, na.rm = TRUE) - sd(df$age, na.rm = TRUE)), xmax = (mean(df$age, na.rm = TRUE) + sd(df$age, na.rm = TRUE)), 
           alpha = .2) +
  labs(title = "",
       x = "Age in years",
       y = "Number of respondents",
       caption = glue("Note. Mean age is {round(mean(df$age, na.rm = T),1)} years with a standard deviation of {round(sd(df$age, na.rm = T),1)}. Age range is {min(df$age)} to {max(df$age)}.")
  ) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))

#remove age from df
dif.age <- df$age
df$age <- NULL

#check for missing data
RImissing(df)

#check overall responses
RIallresp(df)

#check for floor / ceiling effects
RIrawdist(df)

#While not really necessary, it could be interesting to see whether the response patterns follow a 
#Guttman-like structure. Items and persons are sorted based on lower->higher responses, 
#and we should see the color move from yellow in the lower left corner to blue in the upper right corner.

RIheatmap(df) +
  theme(axis.text.x = element_blank())

#It is usually recommended to have at least ~10 responses 
#in each category for psychometric analysis, no matter which methodology is used.

RItileplot(df)

# item fit 
simfit1 <- RIgetfit(df, iterations = 1000, cpu = 8) 
RIitemfit(df, simfit1)

# residual correlations
simcor1 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor1$p99)

#PCA of residuals
RIpcmPCA(df)

#loadings on first residuals contrast
RIloadLoc(df)

#split data frame
df1 = df[,1:5]
df2 = df[,6:10]

# item fit 
simfit1 <- RIgetfit(df1, iterations = 1000, cpu = 8) 
RIitemfit(df1, simfit1)

# residual correlations
simcor1 <- RIgetResidCor(df2, iterations = 1000, cpu = 8)
RIresidcorr(df2, cutoff = simcor1$p99)

#PCA of residuals
RIpcmPCA(df2)

#loadings on first residuals contrast
RIloadLoc(df2)

RIitemCats(df2, xlims = c(-5,5))


# Other package (https://pgmj.github.io/simcutoffs.html)

simres1 <- RIgetResidCor(df2, iterations = 1000, cpu = 8)

glimpse(simres1)

RIresidcorr(df2, cutoff = simres1$p99)

hist(simres1$results$diff, breaks = 50, col = "lightblue")
abline(v = simres1$p99, col = "red")
abline(v = simres1$p95, col = "orange")

simfit1 <- RIgetfit(df2, iterations = 1000, cpu = 8)
simfit1[[1]]



RIgetfitPlot(simfit1, df2)

library(RASCHplot) 
CICCplot(PCM(df2), which.item = 3)


# Question 3 ---------------------------------------------------------------


# Question 4 ---------------------------------------------------------------


# Question 5 ---------------------------------------------------------------

#Group 3 ausschließen da nicht in Japanese Study und nur 3 Pax!

d <- data[data$MINI_Group != "Group3: First depressive episode", ]


#dichtotom
model = lm(ES_total ~ MINI_Group, data = d)
anova(model)

emmeans::emmeans(model, pairwise ~ MINI_Group)

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


# Question 9 ---------------------------------------------------------------

#dichotom
model = lm(GSI ~ ES_total, data)
summary(model)

#likert

model = lm(GSI ~ ES_likert_total, data)
summary(model)


# Question 10 ---------------------------------------------------------------


model1 = lm(PWB_total ~ WHO_total, data)
model2 = lm(PWB_total ~ WHO_total + ES_likert_total, data)

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



# Question 12 Correlation Analyses-------------------------------------------------------------

# Cor table ES + PWB:

cor.pwb = data[, c("ES_total", "PWB_Autonomy", "PWB_Environmental_Mastery", "PWB_Personal_Growth", "PWB_Rositive_Relations", 
               "PWB_Purpose_of_life", "PWB_Self_Acceptance")]

descriptive_stats_pwb <- data.frame(
  M = sapply(cor.pwb, mean, na.rm = TRUE),
  SD = sapply(cor.pwb, sd, na.rm = TRUE),
  Skew = sapply(cor.pwb, skewness, na.rm = TRUE),
  Kurtosis = sapply(cor.pwb, kurtosis, na.rm = TRUE)
)


#cor table ES + WHOQOL

cor.whoqol = data[, c("ES_total", "WHOQOL_Physical_Health_Converted", "WHOQOL_Psychological_Converted", 
                      "WHOQOL_Social_Relationships_Converted", "WHOQOL_Environment_Converted")]

descriptive_stats_whoqol <- data.frame(
  M = sapply(cor.whoqol, mean, na.rm = TRUE),
  SD = sapply(cor.whoqol, sd, na.rm = TRUE),
  Skew = sapply(cor.whoqol, skewness, na.rm = TRUE),
  Kurtosis = sapply(cor.whoqol, kurtosis, na.rm = TRUE)
)


# cor CDRISC, GSI, BDI, ES Lkert, + WHOQOL Total

cor.df <- data[, c("ES_total", "ES_likert_total", "GSI", "CDRISC_total", "BDI_total", "WHOQOL_total")]

descriptive_stats_df <- data.frame(
  M = sapply(cor.df, mean, na.rm = TRUE),
  SD = sapply(cor.df, sd, na.rm = TRUE),
  Skew = sapply(cor.df, skewness, na.rm = TRUE),
  Kurtosis = sapply(cor.df, kurtosis, na.rm = TRUE)
)


#Überlegung: Correlation Analyses with subscales or total WHOQOL, PWB? 
#cor.df <- data[, c("ES_total", "ES_likert_total", "GSI", "WHOQOL_total", "CDRISC_total", "PWB_total", "BDI_total")]

#hier mit Subscales:
#cor.df <- data[, c("ES_total", "ES_likert_total", "GSI", "BDI_total", "WHOQOL_Physical_Health_Converted", "WHOQOL_Psychological_Converted",
 #                  "WHOQOL_Social_Relationships_Converted", "WHOQOL_Environment_Converted", "WHOQOL_total",
#                   "CDRISC_total", "PWB_Autonomy","PWB_Environmental_Mastery","PWB_Personal_Growth","PWB_Rositive_Relations",
#                   "PWB_Purpose_of_life","PWB_Self_Acceptance", "PWB_total")]



#descriptive_stats <- data.frame(
#  Variable = c("ES", "ES_likert", "GSI", "BDI", "Physical Health", "Psychological", "Social Relationships", "Environment", "WHOQOL_total",
#               "CDRISC", "Autonomy", "Environmental Mastery", "Personal Growth", "Positive Relations", "Purpose of life", "Self Acceptance",
#               "PWB"),
#  M = sapply(cor.df, mean, na.rm = TRUE),
#  SD = sapply(cor.df, sd, na.rm = TRUE),
#  Skew = sapply(cor.df, skewness, na.rm = TRUE),
#  Kurtosis = sapply(cor.df, kurtosis, na.rm = TRUE)
#)

# Erstellen der Datensätze für die einzelnen Fragebögen

# data_ES: Nur die ES_ Variablen (ohne ES_likert)
data_ES <- data[, grep("^ES_[0-9]+$", colnames(data))]

# data_ES_likert: Nur die ES_likert Variablen
data_ES_likert <- data[, grep("^ES_likert_[0-9]+$", colnames(data))]

# data_GSI: Nur die BSI_ Variablen
data_GSI <- data[, grep("^BSI_[0-9]+$", colnames(data))]

# data_BDI: Nur die BDI_ Variablen
data_BDI <- data[, grep("^BDI_[0-9]+$", colnames(data))]

# data_WHOQOL: Nur die WHOQOL_ Variablen
data_WHOQOL <- data[, grep("^WHOQOL_[0-9]+$", colnames(data))]

# data_CDRISC: Nur die CDRISC_ Variablen
data_CDRISC <- data[, grep("^CDRISC_[0-9]+$", colnames(data))]

# data_PWB: Nur die PWB_ Variablen
data_PWB <- data[, grep("^PWB_[0-9]+$", colnames(data))]

# PWB-Dimensionen
data_PWB_Autonomy <- data[, c("PWB_1", "PWB_9", "PWB_17")]
data_PWB_Environmental_Mastery <- data[, c("PWB_2", "PWB_11", "PWB_18")]
data_PWB_Personal_Growth <- data[, c("PWB_4", "PWB_12", "PWB_14")]
data_PWB_Positive_Relations <- data[, c("PWB_5", "PWB_10", "PWB_13")]
data_PWB_Purpose_of_Life <- data[, c("PWB_6", "PWB_8", "PWB_15")]
data_PWB_Self_Acceptance <- data[, c("PWB_3", "PWB_7", "PWB_16")]

# WHOQOL-Dimensionen
data_WHOQOL_Physical_Health <- data[, c("WHOQOL_3", "WHOQOL_4", "WHOQOL_10", "WHOQOL_15", "WHOQOL_16", "WHOQOL_17", "WHOQOL_18")]
data_WHOQOL_Psychological <- data[, c("WHOQOL_5", "WHOQOL_6", "WHOQOL_7", "WHOQOL_11", "WHOQOL_19", "WHOQOL_26")]
data_WHOQOL_Social_Relationships <- data[, c("WHOQOL_20", "WHOQOL_21", "WHOQOL_22")]
data_WHOQOL_Environment <- data[, c("WHOQOL_8", "WHOQOL_9", "WHOQOL_12", "WHOQOL_13", "WHOQOL_14", "WHOQOL_23", "WHOQOL_24", "WHOQOL_25")]

  
  
alpha(data_PWB_Autonomy)


# Berechnung von Cronbach's Alpha-Werten wie zuvor
ES_alpha <- alpha(data_ES)$total$raw_alpha
ES_likert_alpha <- alpha(data_ES_likert)$total$raw_alpha
GSI_alpha <- alpha(data_GSI)$total$raw_alpha
BDI_alpha <- alpha(data_BDI)$total$raw_alpha
Physical_Health_alpha <- alpha(data_WHOQOL_Physical_Health)$total$raw_alpha
Psychological_alpha <- alpha(data_WHOQOL_Psychological)$total$raw_alpha
Social_Relationships_alpha <- alpha(data_WHOQOL_Social_Relationships)$total$raw_alpha
Environment_alpha <- alpha(data_WHOQOL_Environment)$total$raw_alpha
CDRISC_alpha <- alpha(data_CDRISC)$total$raw_alpha
Autonomy_alpha <- alpha(data_PWB_Autonomy)$total$raw_alpha
Environmental_Mastery_alpha <- alpha(data_PWB_Environmental_Mastery)$total$raw_alpha
Personal_Growth_alpha <- alpha(data_PWB_Personal_Growth)$total$raw_alpha
Positive_Relations_alpha <- alpha(data_PWB_Positive_Relations)$total$raw_alpha
Purpose_of_Life_alpha <- alpha(data_PWB_Purpose_of_Life)$total$raw_alpha
Self_Acceptance_alpha <- alpha(data_PWB_Self_Acceptance)$total$raw_alpha
WHOQOL_alpha = alpha(data_WHOQOL)$total$raw_alpha

# Erstellen der Alpha-Spalte in der Tabelle
#descriptive_stats$Alpha <- c(ES_alpha, ES_likert_alpha, GSI_alpha, BDI_alpha, 
 #                            Physical_Health_alpha, Psychological_alpha, 
#                             Social_Relationships_alpha, Environment_alpha, 
#                             WHOQOL_total_alpha, CDRISC_alpha, 
##                             Autonomy_alpha, Environmental_Mastery_alpha, 
#                             Personal_Growth_alpha, Positive_Relations_alpha, 
#                             Purpose_of_Life_alpha, Self_Acceptance_alpha, 
#                             PWB_alpha)


# Alpha in PWB Tabelle erstellen 

descriptive_stats_pwb$Alpha = c(ES_alpha,Autonomy_alpha, Environmental_Mastery_alpha, Personal_Growth_alpha,
                                Positive_Relations_alpha, Purpose_of_Life_alpha, Self_Acceptance_alpha)


# Alpha in whoqol table

descriptive_stats_whoqol$Alpha = c(ES_alpha, Physical_Health_alpha, Psychological_alpha,
                                   Social_Relationships_alpha, Environment_alpha)


# Alpha Tabelle drei

descriptive_stats_df$Alpha = c(ES_alpha, ES_likert_alpha, GSI_alpha,
                               CDRISC_alpha,BDI_alpha, WHOQOL_alpha)



# Correlations for PWB table 
correlation_matrix_pwb <- cor(cor.pwb, use = "pairwise.complete.obs")
correlation_matrix_pwb <- as.data.frame(correlation_matrix_pwb)

final_table_pwb = descriptive_stats_pwb
final_table_pwb$ES_total = correlation_matrix_pwb$ES_total
final_table_pwb$PWB_Autonomy = correlation_matrix_pwb$PWB_Autonomy
final_table_pwb$PWB_Environmental_Mastery = correlation_matrix_pwb$PWB_Environmental_Mastery
final_table_pwb$PWB_Personal_Growth = correlation_matrix_pwb$PWB_Personal_Growth
final_table_pwb$PWB_Rositive_Relations = correlation_matrix_pwb$PWB_Rositive_Relations
final_table_pwb$PWB_Purpose_of_life = correlation_matrix_pwb$PWB_Purpose_of_life


# correlations for whoqol table
correlation_matrix_whoqol <- cor(cor.whoqol, use = "pairwise.complete.obs")
correlation_matrix_whoqol <- as.data.frame(correlation_matrix_whoqol)

final_table_whoqol = descriptive_stats_whoqol
final_table_whoqol$ES_total = correlation_matrix_whoqol$ES_total
final_table_whoqol$WHOQOL_Physical_Health_Converted = correlation_matrix_whoqol$WHOQOL_Physical_Health_Converted
final_table_whoqol$WHOQOL_Psychological_Converted = correlation_matrix_whoqol$WHOQOL_Psychological_Converted
final_table_whoqol$WHOQOL_Social_Relationships_Converted = correlation_matrix_whoqol$WHOQOL_Social_Relationships_Converted
final_table_whoqol$WHOQOL_Environment_Converted = correlation_matrix_whoqol$WHOQOL_Environment_Converted

# correlations for 3rd table
correlation_matrix_df <- cor(cor.df, use = "pairwise.complete.obs")
correlation_matrix_df <- as.data.frame(correlation_matrix_df)

final_table_df = descriptive_stats_df
final_table_df$ES_total = correlation_matrix_df$ES_total
final_table_df$ES_likert_total = correlation_matrix_df$ES_likert_total
final_table_df$GSI = correlation_matrix_df$GSI
final_table_df$CDRISC_total = correlation_matrix_df$CDRISC_total
final_table_df$BDI_total = correlation_matrix_df$BDI_total
final_table_df$WHOQOL_total = correlation_matrix_df$WHOQOL_total


# Combine all information into the final table
#final_table <- descriptive_stats
#final_table$ES_total <- correlation_matrix$ES_total
#final_table$ES_likert_total <- correlation_matrix$ES_likert_total
#final_table$GSI <- correlation_matrix$GSI
#final_table$BDI_total <- correlation_matrix$BDI_total
#final_table$WHOQOL_Physical_Health_Converted <- correlation_matrix$WHOQOL_Physical_Health_Converted
#final_table$WHOQOL_Psychological_Converted <- correlation_matrix$WHOQOL_Psychological_Converted
#final_table$WHOQOL_Social_Relationships_Converted <- correlation_matrix$WHOQOL_Social_Relationships_Converted
#final_table$WHOQOL_Environment_Converted <- correlation_matrix$WHOQOL_Environment_Converted
#final_table$WHOQOL_total <- correlation_matrix$WHOQOL_total
#final_table$CDRISC_total <- correlation_matrix$CDRISC_total
#final_table$PWB_Autonomy <- correlation_matrix$PWB_Autonomy
#final_table$PWB_Environmental_Mastery <- correlation_matrix$PWB_Environmental_Mastery
#final_table$PWB_Personal_Growth <- correlation_matrix$PWB_Personal_Growth
#final_table$PWB_Rositive_Relations <- correlation_matrix$PWB_Rositive_Relations
#final_table$PWB_Purpose_of_life <- correlation_matrix$PWB_Purpose_of_life
#final_table$PWB_Self_Acceptance <- correlation_matrix$PWB_Self_Acceptance
#final_table$PWB_total <- correlation_matrix$PWB_total

# Round all numeric values in the final table
final_table_pwb <- roundallnumerics(final_table_pwb, 2)
final_table_whoqol <- roundallnumerics(final_table_whoqol, 2)
final_table_df <- roundallnumerics(final_table_df, 2)

# PWB tabelle speichern

# Füge eine Spalte mit Variablennamen hinzu
final_table_pwb$Variable <- rownames(final_table_pwb)

# Verschiebe die Spalte "Variable" an den Anfang
final_table_pwb <- final_table_pwb[, c("Variable", setdiff(names(final_table_pwb), "Variable"))]

# Erstelle die flextable aus der Tabelle
formatted_table <- flextable(final_table_pwb)

# Benenne die Spalten entsprechend
formatted_table <- set_header_labels(
  formatted_table,
  M = "M",
  SD = "SD",
  Skew = "Skew",
  Kurtosis = "Kurtosis",
  Alpha = "α",
  ES_total = "I",
  PWB_Autonomy = "II",
  PWB_Environmental_Mastery = "III",
  PWB_Personal_Growth = "IV",
  PWB_Rositive_Relations = "V",
  PWB_Purpose_of_life = "VI",
  PWB_Self_Acceptance = "VII"
)


# Optimiere die Darstellung (automatische Anpassung)
formatted_table <- autofit(formatted_table)

# Optional: Stil hinzufügen
formatted_table <- bold(formatted_table, part = "header")  # Header fett machen
formatted_table <- align(formatted_table, align = "center", part = "all")  # Text zentrieren

# Tabelle anzeigen
formatted_table

# Speichere die Tabelle als Word-Dokument
save_as_docx(formatted_table, path = "table_with_header.docx")


#whoqol tabelle speichern 

# WHOQOL Tabelle vorbereiten

# Füge eine Spalte mit Variablennamen hinzu
final_table_whoqol$Variable <- rownames(final_table_whoqol)

# Verschiebe die Spalte "Variable" an den Anfang
final_table_whoqol <- final_table_whoqol[, c("Variable", setdiff(names(final_table_whoqol), "Variable"))]

# Erstelle die flextable aus der Tabelle
formatted_table_whoqol <- flextable(final_table_whoqol)

# Benenne die Spalten entsprechend
formatted_table_whoqol <- set_header_labels(
  formatted_table_whoqol,
  M = "M",
  SD = "SD",
  Skew = "Skew",
  Kurtosis = "Kurtosis",
  Alpha = "α",
  ES_total = "I",
  WHOQOL_Physical_Health_Converted = "II",
  WHOQOL_Psychological_Converted = "III",
  WHOQOL_Social_Relationships_Converted = "IV",
  WHOQOL_Environment_Converted = "V"
)

# Optimiere die Darstellung (automatische Anpassung)
formatted_table_whoqol <- autofit(formatted_table_whoqol)

# Optional: Stil hinzufügen
formatted_table_whoqol <- bold(formatted_table_whoqol, part = "header")  # Header fett machen
formatted_table_whoqol <- align(formatted_table_whoqol, align = "center", part = "all")  # Text zentrieren

# Tabelle anzeigen
formatted_table_whoqol

# Speichere die Tabelle als Word-Dokument
save_as_docx(formatted_table_whoqol, path = "table_whoqol.docx")


# dritte tabelle fertig 

# Füge eine Spalte mit Variablennamen hinzu
final_table_df$Variable <- rownames(final_table_df)

# Verschiebe die Spalte "Variable" an den Anfang
final_table_df <- final_table_df[, c("Variable", setdiff(names(final_table_df), "Variable"))]

# Erstelle die flextable aus der Tabelle
formatted_table_df <- flextable::flextable(final_table_df)

# Benenne die Spalten entsprechend
formatted_table_df <- set_header_labels(
  formatted_table_df,
  Variable = "Variable",
  M = "M",
  SD = "SD",
  Skew = "Skew",
  Kurtosis = "Kurtosis",
  Alpha = "α",
  ES_total = "I",
  ES_likert_total = "II",
  GSI = "III",
  CDRISC_total = "IV",
  BDI_total = "V"
)

# Optimiere die Darstellung (automatische Anpassung)
formatted_table_df <- autofit(formatted_table_df)

# Optional: Stil hinzufügen
formatted_table_df <- bold(formatted_table_df, part = "header")  # Header fett machen
formatted_table_df <- align(formatted_table_df, align = "center", part = "all")  # Text zentrieren
formatted_table_df <- align(formatted_table_df, j = 1, align = "left", part = "all")  # Erste Spalte linksbündig

# Tabelle anzeigen
formatted_table_df

# Speichere die Tabelle als Word-Dokument
save_as_docx(formatted_table_df, path = "table_final_df.docx")



options(scipen = 999)

# Calculate p-values pwb table
correlation_results_pwb <- Hmisc::rcorr(as.matrix(cor.pwb))
p_values_matrix_pwb <- correlation_results_pwb$P
p_values_df_pwb <- as.data.frame(p_values_matrix_pwb)

# Calculate p-values whoqol table
correlation_results_whoqol <- Hmisc::rcorr(as.matrix(cor.whoqol))
p_values_matrix_whoqol <- correlation_results_whoqol$P
p_values_df_whoqol <- as.data.frame(p_values_matrix_whoqol)

# Calculate p-values 3rd table
correlation_results_df <- Hmisc::rcorr(as.matrix(cor.df))
p_values_matrix_df <- correlation_results_df$P
p_values_df_df <- as.data.frame(p_values_matrix_df)




# Apply Benjamini-Hochberg correction
p_values_vector <- as.vector(p_values_matrix_df)
p_values_bh <- p.adjust(p_values_vector, method = "BH")

# Reshape corrected p-values back into a matrix form and convert to data frame
p_values_bh_matrix <- matrix(p_values_bh, ncol = ncol(p_values_df_df), nrow = nrow(p_values_df_df))
p_values_bh_df <- as.data.frame(p_values_bh_matrix)

# Print the corrected p-values data frame
print(p_values_bh_df)









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
