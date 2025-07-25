library(dplyr)
library(flextable)




lmu = read.csv2("lmu.csv")
#View(lmu)
#names(lmu)

# Neue Variable "Population" erstellen
lmu$Population <- "klinisch"
lmu <- lmu[, c("Population", setdiff(names(lmu), "Population"))]


# Unnötige Variablen entfernen


vars_to_remove <- c("Code_Studie", "Geburtsdatum",
                    "t0_WHOQOLBREF_3r", "t0_WHOQOLBREF_4r", "t0_WHOQOLBREF_26r", 
                    "t1_WHOQOLBREF_3r", "t1_WHOQOLBREF_4r", "t1_WHOQOLBREF_26r", 
                    "t0_WHO_ges", "t1_WHO_ges", "t0_BDI_ges", "t1_BDI_ges", 
                    "t0_ES_ges", "t1_ES_ges", "t0_ES_likert_ges", "t1_ES_likert_ges", 
                    "t0_CDRISC_ges", "t1_CDRISC_ges", 
                    "t0_WHOQOLBREF_Physical", "t0_WHOQOLBREF_Psychological", "t0_WHOQOLBREF_Environment", "t0_WHOQOLBREF_Social",
                    "t0_WHOQOLBREF_Physical100", "t0_WHOQOLBREF_Psychological100", "t0_WHOQOLBREF_Environment100", "t0_WHOQOLBREF_Social100", "t0_WHOQOLBREF_Total",
                    "t1_WHOQOLBREF_Physical", "t1_WHOQOLBREF_Psychological", "t1_WHOQOLBREF_Environment", "t1_WHOQOLBREF_Social",
                    "t1_WHOQOLBREF_Physical100", "t1_WHOQOLBREF_Psychological100", "t1_WHOQOLBREF_Environment100", "t1_WHOQOLBREF_Social100", "t1_WHOQOLBREF_Total",
                    "t0_BSI_Somatisierung", "t0_BSI_Zwanghaftigkeit", "t0_BSI_Sozialkontakt", "t0_BSI_Depressivität", "t0_BSI_Aengstlichkeit",
                    "t0_BSI_Aggressivitaet", "t0_BSI_Phobisch", "t0_BSI_Paranoid", "t0_BSI_Psychotizismus", "t0_BSI_GSI",
                    "t1_BSI_Somatisierung", "t1_BSI_Zwanghaftigkeit", "t1_BSI_Sozialkontakt", "t1_BSI_Depressivität", "t1_BSI_Aengstlichkeit",
                    "t1_BSI_Aggressivitaet", "t1_BSI_Phobisch", "t1_BSI_Paranoid", "t1_BSI_Psychotizismus", "t1_BSI_GSI")
prefixes_to_remove <- c("t0_PSI", "t0_CEQ", "t0_CTQ", "t0_WB_CU", "t0_MPQ", "t0_UCLA", 
                        "t0_SNQ", "t0_RSES", "t0_BRS", "t0_SOFAS", "t0_CIE", "t0_MADRS", 
                        "t0_ADP", "v4_GQ", "v4_CEQ", "t1_PSI", "t1_CEQ", "t1_CTQ", "t1_WB_CU", "t1_MPQ", "t1_UCLA", 
                        "t1_SNQ", "t1_RSES", "t1_BRS", "t1_SOFAS", "t1_CIE", "t1_MADRS", 
                        "t1_ADP", "t2", "t3")

columns_to_remove <- names(lmu)[sapply(names(lmu), function(x) any(startsWith(x, prefixes_to_remove)))]
columns_to_remove <- c(columns_to_remove, vars_to_remove)  

lmu <- lmu[, !names(lmu) %in% columns_to_remove]

# Entferne Drop Outs ect...
dropouts = table(lmu$Note) # 18 Drop outs 

lmu <- lmu %>%
  filter(!Note %in% c("missing", "Drop-Out", "Unterlagen fehlend", "DROP Out", "nicht vergeben", "DROP out", "Drop out"))


sum(is.na(lmu$t0_ES_1)) 


# Entferne ES = NA == 3 (einer noch nicht eingetragen) raus --> aktuell 32 probandinnen
lmu <- lmu %>%
  filter(!is.na(t0_ES_1))





# sample characteristics --------------------------------------------------

#alter abrunden
lmu$Alter = floor(lmu$Alter)
lmu$Geschlecht

# Demographics codieren
lmu$Geschlecht <- factor(lmu$Geschlecht, levels = c(1, 2, 3), labels = c("female", "male", "divers"))
lmu$Diag_Prim <- factor(
  lmu$Diag_Prim,
  levels = 1:12,
  labels = c(
    "Major Depressive Disorder",
    "Bipolare Störung",
    "Anxiety Disorder",
    "Obsessive-Compulsive Disorder",
    "Schizophrenia",
    "Schizoaffektive Erkrankung",
    "(k)PTBS",
    "Borderline Personality Disorder",
    "Andere Persönlichkeitsstörung",
    "ADHS",
    "Autism Spectrum Disorder",
    "Sonstige"
  )
)

lmu$t0_sozanam_familienstand <- factor(
  lmu$t0_sozanam_familienstand,
  levels = 1:5,
  labels = c("Single", "Partnered", "Married", "Divorced", "Widowed")
)

lmu$t0_sozanam_wohnsituation <- factor(
  lmu$t0_sozanam_wohnsituation,
  levels = 1:5,
  labels = c("Alleine", "Mit Partner", "Bei den Eltern", "WG", "TWG")
)

lmu$t0_sozanam_schulabschluss <- factor(
  lmu$t0_sozanam_schulabschluss,
  levels = 1:5,
  labels = c("Grundschule", "Hauptschule", "Realschule", "Fachabitur", "Abitur")
)

lmu$t0_sozanam_ausbildung <- factor(
  lmu$t0_sozanam_ausbildung,
  levels = 1:4,
  labels = c("keine begonnen", "abgeschlossen", "abgebrochen", "laufend")
)

lmu$t0_sozanam_studium <- factor(
  lmu$t0_sozanam_studium,
  levels = 1:5,
  labels = c("Nein", "Abgebrochen", "Bin dabei", "Bachelor", "Master (Diplom)")
)

lmu$t0_sozanam_geld <- factor(
  lmu$t0_sozanam_geld,
  levels = 1:5,
  labels = c("unter 100€", "100-250€", "250-500€", "500-1000€", "über 1000€")
)

lmu$t0_sozanam_berufstätig <- factor(
  lmu$t0_sozanam_berufstätig,
  levels = 1:4,
  labels = c("Employed", "Retired", "Unemployed", "Student")
)


summary_table <- lmu %>%
  group_by(Geschlecht) %>%
  summarise(
    Alter_Mean = mean(Alter, na.rm = TRUE),
    Alter_SD = sd(Alter, na.rm = TRUE),
    n = n()
  )

sd(lmu$Alter)

flextable::flextable(summary_table)


# characteristics erstellen 

characteristics_lmu <- lmu[, c(
  "Population",
  "Geschlecht",
  "Alter",
  "Diag_Prim",
  "t0_sozanam_familienstand",
  "t0_sozanam_wohnsituation",
  "t0_sozanam_geld",
  "t0_sozanam_schulabschluss",
  "t0_sozanam_ausbildung",
  "t0_sozanam_studium",
  "t0_sozanam_berufstätig"
)]

write.csv2(characteristics_lmu, file = "characteristics_lmu.csv")

#create Rasch df 

raschlmu <- lmu %>%
  select(starts_with("t0_ES_"), Population, Alter, Geschlecht) %>%
  select(-starts_with("t0_ES_likert"))

names(raschlmu) = c("ES_1", "ES_2", "ES_3", "ES_4", "ES_5", "ES_6", "ES_7", "ES_8", "ES_9", "ES_10", "Population", "age", "sex")

write.csv2(raschlmu, file = "raschlmu.csv")

# Create Datensatz für t0 -------------------------------------------------

vars_to_remove <- c("Note", "Alter", "Geschlecht", "Aufnahmedatum", "Entlassdatum", 
                    "Diag_Prim", "Diag_Kom1", "Diag_Kom2", "Diag_Notiz", "Diag_Verlauf",
                    "t0_sozanam_familienstand", "t0_sozanam_wohnsituation", "t0_sozanam_beschwerden",
                    "t0_sozanam_finanziell", "t0_sozanam_geld", "t0_sozanam_schulabschluss", 
                    "t0_sozanam_ausbildung", "t0_sozanam_studium", "t0_sozanam_bildung", 
                    "t0_sozanam_berufstätig", "t0_sozanam_arbeit", "t0_sozanam_konfession",
                    "t0_sozanam_konfession2", "t0_sozanam_konfession3", "t0_sozanam_schlaf", 
                    "t0_sozanam_schlaf2", "t0_sozanam_lebensbereiche1", "t0_sozanam_lebensbereich2",
                    "t0_sozanam_lebensbereich3", "t0_sozanam_lebensbereich4", "t0_sozanam_lebensbereich5",
                    "t0_sozanam_lebensbereich6", "t0_sozanam_lebensbereich7", "t0_sozanam_lebensqualität",
                    "t0_sozanam_erwartung", "t0_sozanam_beschwerdenbeginn", "t0_sozanam_aufenthalte1",
                    "t0_sozanam_aufenthalte2", "t0_sozanam_aufenthalte3", "t0_sozanam_ambulant1", 
                    "t0_sozanam_ambulant2", "t0_sozanam_ambulant3", "t0_sozanam_ambulant4", 
                    "t0_sozanam_ambulant5", "t0_sozanam_therapieverfahren", "t0_sozanam_selbstverletzt",
                    "t0_sozanam_selbstverletzt1", "t0_sozanam_selbstverletzt2", "t0_sozanam_selbstverletzt3",
                    "t0_sozanam_suizid", "t0_sozanam_suizid1", "t0_sozanam_suizid2", "t0_sozanam_suizid3",
                    "t0_sozanam_alkohol", "t0_sozanam_alkohol2", "t0_sozanam_alkohol3", 
                    "t0_sozanam_alkohol4", "t0_sozanam_zigaretten", "t0_sozanam_zigaretten2", 
                    "t0_sozanam_drogen", "t0_sozanam_drogen1")

lmu <- lmu[, !(names(lmu) %in% vars_to_remove)]

# check for missing data

es_t0 = lmu[, grep("^t0_ES_(?!likert)", colnames(lmu), perl = TRUE)]
es_likert_t0 = lmu[, grep("^t0_ES_likert", colnames(lmu), perl = TRUE)]
bdi_t0 = lmu[, grep("^t0_BDI", colnames(lmu), perl = TRUE)]
who_t0 = lmu[, grep("^t0_WHO", colnames(lmu), perl = TRUE)]
cdrisc_t0 = lmu[, grep("^t0_CDRISC", colnames(lmu), perl = TRUE)]
qol_t0 = lmu[, grep("^t0_WHOQOL", colnames(lmu), perl = TRUE)]
bsi_t0 = lmu[, grep("^t0_BSI", colnames(lmu), perl = TRUE)]

RImissing(es_t0)
RImissing(es_likert_t0) #1 Item missing 7
RImissing(bdi_t0)
RImissing(who_t0)
RImissing(cdrisc_t0)
RImissing(qol_t0)
RImissing(bsi_t0) # 1 Item: BSI 34 /  BSI 37 / BSI 38





# clone df for longitudinal alanyses --------------------------------------

long = lmu



# create t0 df ------------------------------------------------------------

lmu <- lmu[, !grepl("^t1_", names(lmu))]


# Neue Namen für die Variablen in lmu definieren
new_names <- c("Population", "id", 
               paste0("WHO_", 1:5), 
               paste0("MINI_current_", 1:9), 
               paste0("MINI_past_", 1:9), 
               paste0("BDI_", 1:21), 
               paste0("ES_", 1:10), 
               paste0("ES_likert_", 1:10), 
               paste0("CDRISC_", 1:10), 
               paste0("WHOQOL_", 1:26), 
               paste0("BSI_", 1:53))

names(lmu) <- new_names


# PWB missing
missing_vars <- c("PWB_1", "PWB_2", "PWB_3", "PWB_4", "PWB_5", "PWB_6", "PWB_7", "PWB_8", 
                  "PWB_9", "PWB_10", "PWB_11", "PWB_12", "PWB_13", "PWB_14", "PWB_15", "PWB_16", "PWB_17", "PWB_18")

for (var in missing_vars) {
  if (!var %in% names(lmu)) {
    lmu[[var]] <- NA
  }
}

desired_order <- c("Population", "id", 
                   grep("^ES_[0-9]", names(lmu), value = TRUE),  # Nur ES_1 bis ES_10, aber nicht ES_likert_
                   grep("^ES_likert_", names(lmu), value = TRUE),
                   grep("^BDI_", names(lmu), value = TRUE),
                   grep("^MINI_current_", names(lmu), value = TRUE),
                   grep("^MINI_past_", names(lmu), value = TRUE),
                   grep("^WHOQOL_", names(lmu), value = TRUE),
                   grep("^PWB_", names(lmu), value = TRUE),
                   grep("^CDRISC_", names(lmu), value = TRUE),
                   grep("^WHO_", names(lmu), value = TRUE),
                   grep("^BSI_", names(lmu), value = TRUE))

lmu <- lmu[, desired_order]
#View(lmu)


# Skalenaggregation und Umkodieren  ---------------------------------------

# 1 missing in ES likert -> 1x mean umputation!

lmu$ES_total <- rowSums(lmu[, grep("^ES_[1-9]$|^ES_10$", colnames(lmu))], na.rm = TRUE)

# ES_likert mit mean imputation
# Auswahl der relevanten Items
es_items <- lmu[, grep("ES_likert", colnames(lmu))]

# Zeilenweise Mittelwert-Imputation
es_imputed <- t(apply(es_items, 1, function(row) {
  if (all(is.na(row))) {
    return(rep(NA, length(row)))  # keine Imputation bei komplett fehlender Zeile
  }
  row[is.na(row)] <- mean(row, na.rm = TRUE)
  return(row)
}))
# In Data Frame umwandeln
es_imputed <- as.data.frame(es_imputed)
colnames(es_imputed) <- colnames(es_items)
# Skalenwert berechnen
lmu$ES_likert_total <- rowSums(es_imputed)




lmu$BDI_total = rowSums(lmu[, grep("BDI_", colnames(lmu))], na.rm = TRUE)

# Erstellen der Gruppen basierend auf dem BDI_total Score
lmu$BDI_Group <- cut(lmu$BDI_total, 
                      breaks = c(-Inf, 13, 19, 28, Inf), 
                      labels = c("Group0: no depression / clinical irrelevant", 
                                 "Group1: mild depression", 
                                 "Group2: moderate depression", 
                                 "Group3: severe depression"), 
                      right = TRUE)

# Berechnung der Summe der MINI_current und MINI_past Variablen
mini_current_sum <- rowSums(lmu[, grep("^MINI_current_", colnames(lmu))])
mini_past_sum <- rowSums(lmu[, grep("^MINI_past_", colnames(lmu))])

# Codierung für Mini_lifetime_MDE (YES/NO)
lmu$Mini_lifetime_MDE <- ifelse(mini_past_sum >= 0 & mini_past_sum <= 4, "NO", 
                                 ifelse(mini_past_sum >= 5 & mini_past_sum <= 9, "YES", NA))

# Codierung für Mini_current_MDE (MDE/Subthreshold Depression/None)
lmu$Mini_current_MDE <- ifelse(mini_current_sum == 0, "None", 
                                ifelse(mini_current_sum >= 1 & mini_current_sum <= 4, "Subthreshold Depression", 
                                       ifelse(mini_current_sum >= 5 & mini_current_sum <= 9, "MDE", NA)))

# Als Faktoren codieren
lmu$Mini_lifetime_MDE <- factor(lmu$Mini_lifetime_MDE, levels = c("NO", "YES"))
lmu$Mini_current_MDE <- factor(lmu$Mini_current_MDE, levels = c("None", "Subthreshold Depression", "MDE"))

# Erstellung der Gruppen basierend auf Mini_current_MDE und Mini_lifetime_MDE
lmu$MINI_Group <- ifelse(lmu$Mini_current_MDE == "None" & lmu$Mini_lifetime_MDE == "NO", "Group0: No history of MDE - healthy",
                          ifelse(lmu$Mini_current_MDE == "None" & lmu$Mini_lifetime_MDE == "YES", "Group1: Full remission",
                                 ifelse(lmu$Mini_current_MDE == "Subthreshold Depression" & lmu$Mini_lifetime_MDE == "NO", "Group2: First subthreshold depressive episode",
                                        ifelse(lmu$Mini_current_MDE == "MDE" & lmu$Mini_lifetime_MDE == "NO", "Group3: First depressive episode",
                                               ifelse(lmu$Mini_current_MDE == "Subthreshold Depression" & lmu$Mini_lifetime_MDE == "YES", "Group4: History of MDE + current subthreshold",
                                                      ifelse(lmu$Mini_current_MDE == "MDE" & lmu$Mini_lifetime_MDE == "YES", "Group5: History of MDE + current MDE", NA))))))


lmu$MINI_Group <- factor(lmu$MINI_Group, levels = c("Group0: No history of MDE - healthy", 
                                                      "Group1: Full remission", 
                                                      "Group2: First subthreshold depressive episode", 
                                                      "Group3: First depressive episode", 
                                                      "Group4: History of MDE + current subthreshold", 
                                                      "Group5: History of MDE + current MDE"))

#lmu$Mini_lifetime_MDE


# WHOQOL_bref

# Umkodierung der WHOQOL_3, WHOQOL_4 und WHOQOL_26 mit recode()
lmu$WHOQOL_3 <- dplyr::recode(lmu$WHOQOL_3, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
lmu$WHOQOL_4 <- dplyr::recode(lmu$WHOQOL_4, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
lmu$WHOQOL_26 <- dplyr::recode(lmu$WHOQOL_26, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)



# Berechnung der 4 WHOQOL-Domains (Konvertiert 0 - 100)


# Physical Health Domain (konvertiert)
lmu$WHOQOL_Physical_Health_Converted <- (4 * rowMeans(lmu[, c("WHOQOL_3", "WHOQOL_4", "WHOQOL_10", "WHOQOL_15", "WHOQOL_16", "WHOQOL_17", "WHOQOL_18")], na.rm = TRUE) - 4) * (100 / 16)

# Psychological Domain (konvertiert)
lmu$WHOQOL_Psychological_Converted <- (4 * rowMeans(lmu[, c("WHOQOL_5", "WHOQOL_6", "WHOQOL_7", "WHOQOL_11", "WHOQOL_19", "WHOQOL_26")], na.rm = TRUE) - 4) * (100 / 16)

# Social Relationships Domain (konvertiert)
lmu$WHOQOL_Social_Relationships_Converted <- (4 * rowMeans(lmu[, c("WHOQOL_20", "WHOQOL_21", "WHOQOL_22")], na.rm = TRUE) - 4) * (100 / 16)

# Environment Domain (konvertiert)
lmu$WHOQOL_Environment_Converted <- (4 * rowMeans(lmu[, c("WHOQOL_8", "WHOQOL_9", "WHOQOL_12", "WHOQOL_13", "WHOQOL_14", "WHOQOL_23", "WHOQOL_24", "WHOQOL_25")], na.rm = TRUE) - 4) * (100 / 16)

# QOL total (Diese Berechnung ist so eigentlich nicht vorgesehen)
lmu$WHOQOL_total <- rowMeans(lmu[, c("WHOQOL_Physical_Health_Converted", "WHOQOL_Psychological_Converted", "WHOQOL_Social_Relationships_Converted", "WHOQOL_Environment_Converted")], na.rm = TRUE)


# PWB

# Leere Variablen mit NA erstellen (gleiche Länge wie lmu)
lmu$PWB_Autonomy <- NA
lmu$PWB_Environmental_Mastery <- NA
lmu$PWB_Personal_Growth <- NA
lmu$PWB_Rositive_Relations <- NA
lmu$PWB_Purpose_of_life <- NA
lmu$PWB_Self_Acceptance <- NA
lmu$PWB_total <- NA



#RISC, WHO
lmu$CDRISC_total <- rowSums(lmu[, grep("^CDRISC_", colnames(lmu))], na.rm = TRUE)
lmu$WHO_total = rowSums(lmu[, grep("WHO_[1-5]", colnames(lmu))], na.rm = TRUE) * 4


# GSI
lmu$GSI <- rowMeans(lmu[, paste0("BSI_", 1:53)], na.rm = TRUE)


# # Berechnung der Summenwerte für die einzelnen BSI-Skalen
# 
# # Somatisierung
# lmu$BSI_Somatisierung <- rowSums(lmu[, c("BSI_2", "BSI_7", "BSI_23", "BSI_29", "BSI_30", "BSI_33", "BSI_37")], na.rm = TRUE)
# 
# # Zwanghaftigkeit
# lmu$BSI_Zwanghaftigkeit <- rowSums(lmu[, c("BSI_5", "BSI_15", "BSI_26", "BSI_27", "BSI_32", "BSI_36")], na.rm = TRUE)
# 
# # Unsicherheit im Sozialkontakt
# lmu$BSI_Sozialkontakt <- rowSums(lmu[, c("BSI_20", "BSI_21", "BSI_22", "BSI_42")], na.rm = TRUE)
# 
# # Depressivität
# lmu$BSI_Depressivität <- rowSums(lmu[, c("BSI_9", "BSI_16", "BSI_17", "BSI_18", "BSI_35", "BSI_50")], na.rm = TRUE)
# 
# # Ängstlichkeit
# lmu$BSI_Ängstlichkeit <- rowSums(lmu[, c("BSI_1", "BSI_12", "BSI_19", "BSI_38", "BSI_45", "BSI_49")], na.rm = TRUE)
# 
# # Aggressivität / Feindseligkeit
# lmu$BSI_Aggressivität <- rowSums(lmu[, c("BSI_6", "BSI_13", "BSI_40", "BSI_41", "BSI_46")], na.rm = TRUE)
# 
# # Phobische Angst
# lmu$BSI_Phobische_Angst <- rowSums(lmu[, c("BSI_8", "BSI_28", "BSI_31", "BSI_43", "BSI_47")], na.rm = TRUE)
# 
# # Paranoides Denken
# lmu$BSI_Paranoides_Denken <- rowSums(lmu[, c("BSI_4", "BSI_10", "BSI_24", "BSI_48", "BSI_51")], na.rm = TRUE)
# 
# # Psychotizismus
# lmu$BSI_Psychotizismus <- rowSums(lmu[, c("BSI_3", "BSI_14", "BSI_34", "BSI_44", "BSI_53")], na.rm = TRUE)
# 
# # Zusatzskala
# lmu$BSI_Zusatz <- rowSums(lmu[, c("BSI_11", "BSI_25", "BSI_39", "BSI_52")], na.rm = TRUE)
# 
# # Datensatz clean speichern 
# write.csv2(lmu, file = "cleanlmu_t0.csv", row.names = FALSE)


#relevant variables for characteristics table 

lmu_variables = lmu[, c(
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

write.csv2(lmu_variables, file = "lmu_variables.csv")


# Longitudinal df ---------------------------------------------------------



#View(long)

#Entferne Probanden ohne t1 Werte im WHO --> 7 !!, bleiben 25 übrig 
long <- long %>%
  filter(!is.na(t1_WHO5_1))

#check missings 

es_t0 = long[, grep("^t0_ES_(?!likert)", colnames(long), perl = TRUE)]
es_likert_t0 = long[, grep("^t0_ES_likert", colnames(long), perl = TRUE)]
bdi_t0 = long[, grep("^t0_BDI", colnames(long), perl = TRUE)]
who_t0 = long[, grep("^t0_WHO", colnames(long), perl = TRUE)]

RImissing(es_t0)
RImissing(es_likert_t0)
RImissing(bdi_t0)
RImissing(who_t0)

es_t1 = long[, grep("^t1_ES_(?!likert)", colnames(long), perl = TRUE)]
es_likert_t1 = long[, grep("^t1_ES_likert", colnames(long), perl = TRUE)]
bdi_t1 = long[, grep("^t1_BDI", colnames(long), perl = TRUE)]
who_t1 = long[, grep("^t1_WHO(?!BREF)", colnames(long), perl = TRUE)]

RImissing(es_t1)
RImissing(es_likert_t1)
RImissing(bdi_t1) # 1 Item BDI 1
RImissing(who_t1)

long$t1_ES_likert_3[1] = 4 # diesen Wert muss du ändern --> zu 4 geändert


# Skalen Aggregieren long Datensatz

# ES
long$t0_ES_total <- rowSums(long[, grep("^t0_ES_[0-9]", names(long))], na.rm = TRUE)
long$t1_ES_total <- rowSums(long[, grep("^t1_ES_[0-9]", names(long))], na.rm = TRUE)

# ES likert keine missings in t1

long$t0_ES_likert_total <- rowSums(long[, grep("^t0_ES_likert_", names(long))], na.rm = TRUE)
long$t1_ES_likert_total <- rowSums(long[, grep("^t1_ES_likert_", names(long))], na.rm = TRUE)

# bdi 1x missing bei t1

long$t0_BDI_total <- rowSums(long[, grep("^t0_BDI_", names(long))], na.rm = TRUE)

bdi_cols <- grep("t1_BDI_", names(long), value = TRUE)
bdi_mat <- long[, bdi_cols]
row_means <- rowMeans(bdi_mat, na.rm = TRUE)

for (i in 1:nrow(bdi_mat)) {
  na_idx <- is.na(bdi_mat[i, ])
  bdi_mat[i, na_idx] <- row_means[i]
}

long[, bdi_cols] <- bdi_mat

long$t1_BDI_total <- rowSums(long[, grep("^t1_BDI_", names(long))], na.rm = TRUE)

long$t0_WHO5_total <- rowSums(long[, grep("^t0_WHO5_", names(long))], na.rm = TRUE)*4
long$t1_WHO5_total <- rowSums(long[, grep("^t1_WHO5_", names(long))], na.rm = TRUE)*4


library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(forcats)

# to long format
long_change <- long %>%
  select(ID_Nummer,
         t0_WHO5_total, t1_WHO5_total,
         t0_BDI_total, t1_BDI_total,
         t0_ES_total, t1_ES_total,
         t0_ES_likert_total, t1_ES_likert_total) %>%
  pivot_longer(
    cols = -ID_Nummer,
    names_to = c("timepoint", "variable"),
    names_pattern = "t(\\d)_(.*)",
    values_to = "value"
  ) %>%
  mutate(
    timepoint = paste0("t", timepoint),
    variable = factor(variable, levels = c("ES_total", "ES_likert_total", "WHO5_total", "BDI_total"))  # custom facet order
  )


scale_labels <- c(
  "ES_total" = "ES-G",
  "ES_likert_total" = "ES-G Likert",
  "WHO5_total" = "WHO-5",
  "BDI_total" = "BDI-II"
)


library(ggplot2)
library(sysfonts)
library(showtext)

font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

ggplot(long_change, aes(x = timepoint, y = value, group = ID_Nummer)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkgreen", size = 1.8) +
  facet_wrap(~ variable, scales = "free_y", labeller = as_labeller(scale_labels), ncol = 2) +
  labs(
    x = "Timepoint",
    y = "Score"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 12),          
    axis.title = element_text(size = 13, face = "bold"),  
    text = element_text(family = "Roboto")
  )






# Mittelwerte und Standardfehler berechnen
summary_stats <- long_change %>%
  group_by(variable, timepoint) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    .groups = "drop"
  )






             


# Objective 6 -------------------------------------------------------------
library(rsq)

long$ES_change = long$t1_ES_total - long$t0_ES_total 
long$ES_likert_change = long$t1_ES_likert_total - long$t0_ES_likert_total 
long$ES_change_cent = long$ES_change - mean(long$ES_change)
long$ES_likert_change_cent = long$ES_likert_change - mean(long$ES_likert_change)
long$BDI_t0_cent = long$t0_BDI_total - mean(long$t0_BDI_total)

model_es = lm(BDI_change ~ ES_change_cent + BDI_t0_cent, data = long )
summary(model_es)

model_likert = lm(BDI_change ~ ES_likert_change_cent + BDI_t0_cent, data = long )
summary(model_likert)


rsq.partial(model_es, adj = FALSE)
rsq.partial(model_likert, adj = FALSE)

model = lm(BDI_change ~ t0_ES_total, data = long)
summary(model)

long$t0_ES_total

model = lm(BDI_change ~ ES_likert_change_cent, data = long )
summary(model)

# Vorraussetzungen

shapiro.test(rstandard(model_es)) # passt!
shapiro.test(rstandard(model_likert)) #passt

plot(model_es, 1, cex = 2) # passt
plot(model_likert, 1, cex = 2) #passt


es = t.test(long$t0_ES_total, long$t1_ES_total, data = long, paired = T)
effectsize::repeated_measures_d(es, method = "rm")

es_likert = t.test(long$t0_ES_likert_total, long$t1_ES_likert_total, data = long, paired = T)
effectsize::repeated_measures_d(es_likert, method = "rm")

who = t.test(long$t0_WHO5_total, long$t1_WHO5_total, data = long, paired = T)
effectsize::repeated_measures_d(who, method = "rm")

bdi = t.test(long$t0_BDI_total, long$t1_BDI_total, data = long, paired = T)
effectsize::repeated_measures_d(bdi, method = "rm")



# Responder ---------------------------------------------------------------

#WHO
long <- long %>%
  mutate(
    WHO_change = t1_WHO5_total - t0_WHO5_total,
    WHO_responder = ifelse(WHO_change >= 10, "Yes", "No")
  )


long <- long %>%
  mutate(
    BDI_change = (t1_BDI_total - t0_BDI_total),
    BDI_change_percent = (t0_BDI_total - t1_BDI_total) / t0_BDI_total,
    BDI_responder = ifelse(BDI_change_percent >= 0.5 & !is.na(BDI_change_percent), "Yes", "No")
  )



# Datensatz clean speichern 
write.csv2(long, file = "long.csv", row.names = FALSE)

table(long$BDI_responder)
table(long$WHO_responder)

long$t0_ES_total
