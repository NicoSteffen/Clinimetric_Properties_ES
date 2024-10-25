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

library(grateful)
library(ggrepel)
library(car)
library(kableExtra)
library(readxl)
library(tidyverse)
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
df2 = df[,1:5]

# item fit 
simfit1 <- RIgetfit(df2, iterations = 1000, cpu = 8) 
RIitemfit(df2, simfit1)

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

#dichtotom
model = lm(ES_total ~ MINI_Group, data = data)
anova(model)

emmeans::emmeans(model, pairwise ~ MINI_Group)

#likert
model = lm(ES_likert_total ~ MINI_Group, data = data)
anova(model)

emmeans::emmeans(model, pairwise ~ MINI_Group)

# Question 6 ---------------------------------------------------------------

#dichtotom
model = lm(ES_total ~ BDI_Group, data = data)
anova(model)

emmeans::emmeans(model, pairwise ~ BDI_Group)

#likert
model = lm(ES_likert_total ~ BDI_Group, data = data)
anova(model)

emmeans::emmeans(model, pairwise ~ BDI_Group)

# Question 7 ---------------------------------------------------------------


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


