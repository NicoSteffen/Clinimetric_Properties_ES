library(mlr)
library(pROC)

# Daten einlesen
data = read.csv2("clean.csv")

# Depressionsvariable erstellen
data$MDE_MINI <- ifelse(data$Mini_current_MDE == "MDE", 1, 0)
data$MDE <- ifelse(data$BDI_total >= 13, 1, 0)

# Subsets erstellen
MINI_likert = data.frame(total = data$ES_likert_total, MDE = data$MDE_MINI)
MINI_dichotom = data.frame(total = data$ES_total, MDE = data$MDE_MINI)
BDI_likert = data.frame(total = data$ES_likert_total, MDE = data$MDE)
BDI_dichotom = data.frame(total = data$ES_total, MDE = data$MDE)

# Sicherstellen, dass MDE als Faktor vorliegt
datasets <- list(MINI_likert, MINI_dichotom, BDI_likert, BDI_dichotom)
for (i in 1:length(datasets)) {
  datasets[[i]]$MDE <- as.factor(datasets[[i]]$MDE)
}
MINI_likert <- datasets[[1]]
MINI_dichotom <- datasets[[2]]
BDI_likert <- datasets[[3]]
BDI_dichotom <- datasets[[4]]



# Mit MINI likert ---------------------------------------------------------


# Modell mit MINI Likert erstellen
mod = glm(MDE ~ total, data = MINI_likert, family = "binomial")
summary(mod)

# Vorhersage testen
predict(mod, newdata = MINI_likert, type = "response")

task = makeClassifTask(target = "MDE", positive = "1", data = MINI_likert)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(MINI_likert$MDE)[2]) / as.numeric(table(MINI_likert$MDE)[1])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr1 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)


# Cut off: 

# Extrahiere Vorhersagen für die Test-Daten
pred = getBMRPredictions(bmr1, as.df = TRUE)
roc_obj <- roc(pred$truth, pred$prob.1)
plot(roc_obj, main="ROC Curve for Predictive Model")
optimal_cutoff <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
print(optimal_cutoff)

# Erstelle eine Tabelle mit möglichen Cutoff-Werten
cutoff_data <- data.frame(
  Threshold = roc_obj$thresholds,
  Sensitivity = roc_obj$sensitivities,
  Specificity = roc_obj$specificities
)

corrected_threshold = optimal_cutoff / (optimal_cutoff + (1 - optimal_cutoff) * (1 / r))


# Hol die Regressionskoeffizienten
intercept <- coef(mod)[1]
slope <- coef(mod)[2]

# Berechnung des Cutoff Scores für ES_total
cutoff_score1 = (log(corrected_threshold / (1 - corrected_threshold)) - intercept) / slope

print(cutoff_score1)



# Mit MINI dichotom -------------------------------------------------------

mod = glm(MDE ~ total, data = MINI_dichotom, family = "binomial")
summary(mod)

task = makeClassifTask(target = "MDE", positive = "1", data = MINI_dichotom)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(MINI_dichotom$MDE)[2]) / as.numeric(table(MINI_dichotom$MDE)[1])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr2 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)


# Cut off: 

# Extrahiere Vorhersagen für die Test-Daten
pred = getBMRPredictions(bmr2, as.df = TRUE)
roc_obj <- roc(pred$truth, pred$prob.1)
plot(roc_obj, main="ROC Curve for Predictive Model")
optimal_cutoff <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Cutoff ausgeben
print(optimal_cutoff)

# Erstelle eine Tabelle mit möglichen Cutoff-Werten
cutoff_data <- data.frame(
  Threshold = roc_obj$thresholds,
  Sensitivity = roc_obj$sensitivities,
  Specificity = roc_obj$specificities
)

corrected_threshold = optimal_cutoff / (optimal_cutoff + (1 - optimal_cutoff) * (1 / r))


# Hol die Regressionskoeffizienten
intercept <- coef(mod)[1]
slope <- coef(mod)[2]

# Berechnung des Cutoff Scores für ES_total
cutoff_score2 = (log(corrected_threshold / (1 - corrected_threshold)) - intercept) / slope

print(cutoff_score2)


# Mit BDI likert ----------------------------------------------------------

mod = glm(MDE ~ total, data = BDI_likert, family = "binomial")
summary(mod)

task = makeClassifTask(target = "MDE", positive = "1", data = BDI_likert)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(BDI_likert$MDE)[2]) / as.numeric(table(BDI_likert$MDE)[1])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr3 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)

# Cut off: 

pred = getBMRPredictions(bmr3, as.df = TRUE)
roc_obj <- roc(pred$truth, pred$prob.1)
plot(roc_obj, main="ROC Curve for Predictive Model")
optimal_cutoff <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Cutoff ausgeben
print(optimal_cutoff)

# Erstelle eine Tabelle mit möglichen Cutoff-Werten
cutoff_data <- data.frame(
  Threshold = roc_obj$thresholds,
  Sensitivity = roc_obj$sensitivities,
  Specificity = roc_obj$specificities
)

corrected_threshold = optimal_cutoff / (optimal_cutoff + (1 - optimal_cutoff) * (1 / r))

# Hol die Regressionskoeffizienten
intercept <- coef(mod)[1]
slope <- coef(mod)[2]

# Berechnung des Cutoff Scores für ES_total
cutoff_score3 = (log(corrected_threshold / (1 - corrected_threshold)) - intercept) / slope

print(cutoff_score3)



# Mit BDI Dichotom --------------------------------------------------------

mod = glm(MDE ~ total, data = BDI_dichotom, family = "binomial")
summary(mod)

task = makeClassifTask(target = "MDE", positive = "1", data = BDI_dichotom)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(BDI_dichotom$MDE)[2]) / as.numeric(table(BDI_dichotom$MDE)[1])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr4 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)

# Cut off: 

pred = getBMRPredictions(bmr4, as.df = TRUE)
roc_obj <- roc(pred$truth, pred$prob.1)
plot(roc_obj, main="ROC Curve for Predictive Model")
optimal_cutoff <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Cutoff ausgeben
print(optimal_cutoff)

# Erstelle eine Tabelle mit möglichen Cutoff-Werten
cutoff_data <- data.frame(
  Threshold = roc_obj$thresholds,
  Sensitivity = roc_obj$sensitivities,
  Specificity = roc_obj$specificities
)
corrected_threshold = optimal_cutoff / (optimal_cutoff + (1 - optimal_cutoff) * (1 / r))


# Hol die Regressionskoeffizienten
intercept <- coef(mod)[1]
slope <- coef(mod)[2]

# Berechnung des Cutoff Scores für ES_total
cutoff_score4 = (log(corrected_threshold / (1 - corrected_threshold)) - intercept) / slope

print(cutoff_score4)




# Comparing results -------------------------------------------------------



# Liste aller Benchmark-Objekte
bmr_list <- list(bmr1, bmr2, bmr3, bmr4)

# Leerer Dataframe für Ergebnisse
bmr_combined <- data.frame(task.id = character(),
                           bac.test.mean = numeric(),
                           tpr.test.mean = numeric(),
                           tnr.test.mean = numeric(),
                           auc.test.mean = numeric(),
                           stringsAsFactors = FALSE)

# Loop über alle Benchmark-Ergebnisse
for (bmr in bmr_list) {
  result <- bmr$results[[1]][[1]]$aggr  # Zugriff auf Aggregation
  
  # Ergebnisse in Dataframe einfügen
  bmr_combined <- rbind(bmr_combined, data.frame(
    task.id = bmr$results[[1]][[1]]$task.id,
    bac.test.mean = result["bac.test.mean"],
    tpr.test.mean = result["tpr.test.mean"],
    tnr.test.mean = result["tnr.test.mean"],
    auc.test.mean = result["auc.test.mean"]
  ))
}

# Ergebnis ausgeben
print(bmr_combined)



bmr_combined$cutoff = c(cutoff_score1, cutoff_score2, cutoff_score3, cutoff_score4)


bmr_combined = roundallnumerics(bmr_combined, 2)



write.csv2(bmr_combined, "benchmark_results.csv", row.names = FALSE)


