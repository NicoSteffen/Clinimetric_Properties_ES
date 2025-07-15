library(car)
library(dplyr)
library(tableone)

#LMU

lmu_char = read.csv2("characteristics_lmu.csv")
lmu_variables = read.csv2("lmu_variables.csv")

lmu_characteristics <- cbind(lmu_char, lmu_variables)


names(lmu_characteristics)[1:12] = c("ID","Population", "Sex", "Age", "Diagnosis", "Marital", 
               "Living", "Money", "School", "Apprenticeship", "University", "Employment status"
               )

lmu_characteristics <- lmu_characteristics[, !(names(lmu_characteristics) %in% c("X", "Living"))]

lmu_characteristics$Sex <- factor(lmu_characteristics$Sex, levels = c("female", "male", "divers"))

lmu_characteristics$Highest_educational_level = with(lmu_characteristics, ifelse(
  University %in% c("Bachelor", "Master (Diplom)"),
  "University or postgraduate degree",
  ifelse(
    Apprenticeship == "abgeschlossen",
    "Completed apprenticeship",
    ifelse(
      School == "Abitur",
      "General higher education entrance qualification",
      ifelse(
        School == "Fachabitur",
        "University of applied sciences entrance diploma",
        ifelse(
          School == "Realschule",
          "Intermediate secondary school certificate",
          ifelse(
            School == "Hauptschule",
            "Lower secondary school certificate",
            ifelse(
              School == "Grundschule",
              "Elementary school",
              NA
            ))))))))

lmu_characteristics$Highest_educational_level <- factor(lmu_characteristics$Highest_educational_level, 
                                                        levels = c("Elementary school", "Lower secondary school certificate", 
                                                                   "Intermediate secondary school certificate", "University of applied sciences entrance diploma",
                                                                   "General higher education entrance qualification", "Completed apprenticeship",
                                                                   "University or postgraduate degree"))

lmu_characteristics$Marital_status = with(lmu_characteristics, ifelse(
  Marital == "Single",
  "Single",
  ifelse(
    Marital %in% c("Partnered", "Married"),
    "Married/partnered",
    ifelse(
      Marital == "Divorced",
      "Divorced/widowed",
      "Other"
    ))))

lmu_characteristics$Marital_status <- factor(lmu_characteristics$Marital_status, 
                                                        levels = c("Single", "Married/partnered", "Divorced/widowed", "Other"))

lmu_characteristics$`Employment status` <- factor(
  lmu_characteristics$`Employment status`,
  levels = c("Unemployed", "Student", "Employed", "Self-employed", "Retired", "Other")
)

lmu_characteristics$Household_income = NA

#fetch diagnosis
diagnosis = lmu_characteristics$Diagnosis


lmu_characteristics <- lmu_characteristics[, !(names(lmu_characteristics) %in% c("Diagnosis", "Marital", "Money", "School", "Apprenticeship", "University"))]

lmu_characteristics <- lmu_characteristics[, c(
  "ID", "Population", "Age", "Sex", "Marital_status", "Household_income",
  "Highest_educational_level", "Employment status", "ES_total", "ES_likert_total",
  "BDI_total", "WHOQOL_total", "PWB_Autonomy", "PWB_Environmental_Mastery",
  "PWB_Personal_Growth", "PWB_Rositive_Relations", "PWB_Purpose_of_life",
  "PWB_Self_Acceptance", "PWB_total", "CDRISC_total", "GSI", "WHO_total"
)]





# non clinical ------------------------------------------------------------


non_char = read.csv2("characteristics_non.csv")
non_variables = read.csv2("non_variables.csv")

non_characteristics <- cbind(non_char, non_variables)

names(non_characteristics)[1:9] = c("ID", "Population", "Age", "Sex", "Marital", "Employment status", 
                                     "Sonstiges_Arbeitsverhaeltnis", "Highest_educational_level", "Household_income")


non_characteristics$Sex <- factor(non_characteristics$Sex, levels = c("female", "male", "divers"))

non_characteristics$Marital_status = with(non_characteristics, ifelse(
  Marital == "Single",
  "Single",
  ifelse(
    Marital %in% c("In einer festen Partnerschaft (getrennt lebend)", "In einer festen Partnerschaft (zusammen lebend)", "Verheiratet"),
    "Married/partnered",
    ifelse(
      Marital == "Divorced",
      "Divorced/widowed",
      "Other"
    ))))

non_characteristics$Marital_status <- factor(non_characteristics$Marital_status,
                                                  levels = c("Single", "Married/partnered", "Divorced/widowed","Other"))

non_characteristics$`Employment status` <- with(non_characteristics, ifelse(
  `Employment status` == "Student/in", "Student",
  ifelse(`Employment status` %in% c("Angestellt (Teilzeit)", "Angestellt (Vollzeit)"), "Employed",
         ifelse(`Employment status` %in% c("Freiberuflich tätig", "Selbstständig"), "Self-employed",
                ifelse(`Employment status` == "Sonstiges Arbeitsverhältnis", "Other", NA)))))

non_characteristics$`Employment status` <- factor(non_characteristics$`Employment status`,
                                         levels = c("Unemployed", "Student", "Employed","Self-employed", "Retired", "Other"))


non_characteristics$Highest_educational_level = with(non_characteristics, ifelse(
  Highest_educational_level == "Realschulabschluss", "Intermediate secondary school certificate",
  ifelse(Highest_educational_level == "Fachabitur (FOS, BOS)", "University of applied sciences entrance diploma",
         ifelse(Highest_educational_level == "Allgemeines Abitur", "General higher education entrance qualification",
                ifelse(Highest_educational_level %in% c("Hochschule (Bachelor)", "Hochschule (Diplom)", " Hochschule (Magister)", "Hochschule (Master)", "Hochschule (Promotion)"), "University or postgraduate degree",
                       ifelse(Highest_educational_level == "Abgeschlossene Ausbildung", "Completed apprenticeship", NA))))))

non_characteristics$Highest_educational_level = factor(non_characteristics$Highest_educational_level,
                                                       levels = c("Elementary school", "Lower secondary school certificate", "Intermediate secondary school certificate",
                                                                  "University of applied sciences entrance diploma", "General higher education entrance qualification", 
                                                                  "Completed apprenticeship", "University or postgraduate degree"))


non_characteristics$Household_income = factor(non_characteristics$Household_income,
                                              levels = c("Unter 1.000 €", "1.000 - 1.999 €", "2.000 - 2.999 €", "3.000 - 3.999 €", "4.000 - 4.999 €", 
                                                         "5.000 oder mehr", "keine Angabe"))




non_characteristics <- non_characteristics[, !(names(non_characteristics) %in% c("Marital", "Sonstiges_Arbeitsverhaeltnis", "X"))]

non_characteristics <- non_characteristics[, c(
  "ID", "Population", "Age", "Sex", "Marital_status", "Household_income",
  "Highest_educational_level", "Employment status", "ES_total", "ES_likert_total",
  "BDI_total", "WHOQOL_total", "PWB_Autonomy", "PWB_Environmental_Mastery",
  "PWB_Personal_Growth", "PWB_Rositive_Relations", "PWB_Purpose_of_life",
  "PWB_Self_Acceptance", "PWB_total", "CDRISC_total", "GSI", "WHO_total"
)]



# merge -------------------------------------------------------------------


all_characteristics <- rbind(non_characteristics, lmu_characteristics)




cat_vars <- c("Sex", "Marital_status", "Household_income", 
              "Highest_educational_level", "Employment status")
cont_vars <- c("Age", "ES_total", "ES_likert_total", "BDI_total", 
               "WHOQOL_total", "PWB_Autonomy", "PWB_Environmental_Mastery", 
               "PWB_Personal_Growth", "PWB_Rositive_Relations", 
               "PWB_Purpose_of_life", "PWB_Self_Acceptance", 
               "PWB_total", "CDRISC_total", "GSI", "WHO_total")

# Tabellen
table1 <- CreateTableOne(vars = c(cat_vars, cont_vars), 
                         strata = "Population", 
                         data = all_characteristics, 
                         factorVars = cat_vars)

table2 = CreateTableOne(vars = c(cat_vars, cont_vars), 
                        data = all_characteristics, 
                        factorVars = cat_vars)

print(table1, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE)

#statistical comparisons

# Gender
sex_table <- table(all_characteristics$Sex, all_characteristics$Population)
fisher.test(sex_table)

# Marital

filtered_data <- subset(all_characteristics, !`Marital_status` %in% c("Other"))
filtered_data$Marital_status <- droplevels(filtered_data$Marital_status)
marital_table = table(filtered_data$Marital_status, filtered_data$Population)
fisher.test(marital_table)

# education

filtered_education = subset(all_characteristics, !Highest_educational_level %in% c("Elementary school"))
filtered_education$Highest_educational_level <- droplevels(filtered_education$Highest_educational_level)
education_table = table(filtered_education$Highest_educational_level, filtered_education$Population)
fisher.test(education_table)

#employment
employment_table = table(all_characteristics$`Employment status`, all_characteristics$Population)
fisher.test(employment_table)

#age
t.test(Age ~ Population, data = all_characteristics)

#tests for continuus data 

t.test(ES_total ~ Population,all_characteristics)
t.test(ES_likert_total ~ Population,all_characteristics)
t.test(BDI_total ~ Population,all_characteristics)
t.test(WHOQOL_total ~ Population,all_characteristics)
t.test(CDRISC_total ~ Population,all_characteristics)
t.test(GSI ~ Population,all_characteristics)
t.test(WHO_total ~ Population,all_characteristics)

# non parametric? 

wilcox.test(ES_total ~ Population, data = all_characteristics)
wilcox.test(ES_likert_total ~ Population, data = all_characteristics)
wilcox.test(BDI_total ~ Population, data = all_characteristics)
wilcox.test(WHOQOL_total ~ Population, data = all_characteristics)
wilcox.test(CDRISC_total ~ Population, data = all_characteristics)
wilcox.test(GSI ~ Population, data = all_characteristics)
wilcox.test(WHO_total ~ Population, data = all_characteristics)




vars <- c("ES_total", "ES_likert_total", "BDI_total", "WHOQOL_total",
          "CDRISC_total", "GSI", "WHO_total")

for (v in vars) {
  cat("\nVariable:", v, "\n")
  
  # Levene's Test
  levene_result <- leveneTest(as.formula(paste(v, "~ Population")), data = all_characteristics)
  print(levene_result)
  
  # Shapiro-Wilk Test by group
  for (g in unique(all_characteristics$Population)) {
    group_data <- all_characteristics[all_characteristics$Population == g, v, drop = TRUE]
    cat("  Shapiro-Wilk (", g, "):\n", sep = "")
    print(shapiro.test(group_data))
  }
}

table(diagnosis)

model = lm(PWB_total ~ ES_total + Age + Sex + Highest_educational_level, data = all_characteristics)
summary(model)

