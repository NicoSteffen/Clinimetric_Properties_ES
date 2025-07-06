library(mlr)
library(pROC)

# # Daten einlesen
data = read.csv2("clean.csv")
# 
# # Depressionsvariable erstellen
data$MDE_MINI <- ifelse(data$Mini_current_MDE == "MDE", 1, 0)
data$MDE <- ifelse(data$BDI_total >= 13, 1, 0)
# 
# # Subsets erstellen
MINI_likert = data.frame(total = data$ES_likert_total, MDE = data$MDE_MINI)
MINI_dichotom = data.frame(total = data$ES_total, MDE = data$MDE_MINI)
# BDI_likert = data.frame(total = data$ES_likert_total, MDE = data$MDE)
BDI_dichotom = data.frame(total = data$ES_total, MDE = data$MDE)
# 
# # Sicherstellen, dass MDE als Faktor vorliegt
# datasets <- list(MINI_likert, MINI_dichotom, BDI_likert, BDI_dichotom)
# for (i in 1:length(datasets)) {
#   datasets[[i]]$MDE <- as.factor(datasets[[i]]$MDE)
# }
# MINI_likert <- datasets[[1]]
# MINI_dichotom <- datasets[[2]]
# BDI_likert <- datasets[[3]]
# BDI_dichotom <- datasets[[4]]
# 

BDI_dichotom$MDE = as.factor(BDI_dichotom$MDE)
MINI_dichotom$MDE = as.factor(MINI_dichotom$MDE)
MINI_likert$MDE = as.factor(MINI_likert$MDE)
# 
# # Mit MINI likert ---------------------------------------------------------
# 
# 
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



# # Mit MINI dichotom -------------------------------------------------------
# 
mod = glm(MDE ~ total, data = MINI_dichotom, family = "binomial")
summary(mod)

task = makeClassifTask(target = "MDE", positive = "1", data = MINI_dichotom)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(MINI_dichotom$MDE)[2]) / as.numeric(table(MINI_dichotom$MDE)[1])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr2 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)
table(MINI_dichotom$MDE)


# 
# # Mit BDI likert ----------------------------------------------------------
# 
# mod = glm(MDE ~ total, data = BDI_likert, family = "binomial")
# summary(mod)
# 
# task = makeClassifTask(target = "MDE", positive = "1", data = BDI_likert)
# log = makeLearner("classif.logreg", predict.type = "prob")
# r = as.numeric(table(BDI_likert$MDE)[2]) / as.numeric(table(BDI_likert$MDE)[1])
# log = makeUndersampleWrapper(log, usw.rate = r)
# desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
# bmr3 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)
# 

# 
# 
# # Mit BDI Dichotom --------------------------------------------------------
# 
mod = glm(MDE ~ total, data = BDI_dichotom, family = "binomial")
summary(mod)

task = makeClassifTask(target = "MDE", positive = "1", data = BDI_dichotom)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(BDI_dichotom$MDE)[2]) / as.numeric(table(BDI_dichotom$MDE)[1])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr4 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)




# 
# # Comparing results -------------------------------------------------------
# 
# 
# 
# # Liste aller Benchmark-Objekte
# bmr_list <- list(bmr1, bmr2, bmr3, bmr4)
# 
# # Leerer Dataframe für Ergebnisse
# bmr_combined <- data.frame(task.id = character(),
#                            bac.test.mean = numeric(),
#                            tpr.test.mean = numeric(),
#                            tnr.test.mean = numeric(),
#                            auc.test.mean = numeric(),
#                            stringsAsFactors = FALSE)
# 
# # Loop über alle Benchmark-Ergebnisse
# for (bmr in bmr_list) {
#   result <- bmr$results[[1]][[1]]$aggr  # Zugriff auf Aggregation
#   
#   # Ergebnisse in Dataframe einfügen
#   bmr_combined <- rbind(bmr_combined, data.frame(
#     task.id = bmr$results[[1]][[1]]$task.id,
#     bac.test.mean = result["bac.test.mean"],
#     tpr.test.mean = result["tpr.test.mean"],
#     tnr.test.mean = result["tnr.test.mean"],
#     auc.test.mean = result["auc.test.mean"]
#   ))
# }
# 
# # Ergebnis ausgeben
# print(bmr_combined)
# 
# 
# 
# bmr_combined$cutoff = c(cutoff_score1, cutoff_score2, cutoff_score3, cutoff_score4)
# 
# 
# bmr_combined = roundallnumerics(bmr_combined, 2)
# 
# 
# 
# write.csv2(bmr_combined, "benchmark_results.csv", row.names = FALSE)


# Prediction Responder ----------------------------------------------------
data = read.csv2("long.csv")
data$WHO_responder = as.factor(data$WHO_responder)
data$BDI_responder = as.factor(data$BDI_responder)

measures = list(
  mlr::bac,   
  mlr::auc,   
  mlr::tpr,   
  mlr::tnr    
)


# Modell mit WHO und likert 
ES_likert = data.frame(total = data$t0_ES_likert_total, WHO = data$WHO_responder)

mod = glm(WHO ~ total, data = ES_likert, family = "binomial")
summary(mod)

# prediction
predict(mod, newdata = ES_likert, type = "response")

task = makeClassifTask(target = "WHO", positive = "Yes", data = ES_likert)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(ES_likert$WHO)[1]) / as.numeric(table(ES_likert$WHO)[2])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr1 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)

perf_data_likert_who = getBMRPerformances(bmr1, as.df = TRUE)
mean_bac = mean(perf_data_likert_who$bac, na.rm = TRUE)
mean_auc = mean(perf_data_likert_who$auc, na.rm = TRUE)
mean_tpr = mean(perf_data_likert_who$tpr, na.rm = TRUE)
mean_tnr = mean(perf_data_likert_who$tnr, na.rm = TRUE)

likert_who = as.data.frame(list(bac = mean_bac,
                           auc = mean_auc,
                          tpr = mean_tpr,
                          tnr = mean_tnr))


# Modell mit WHO und ES
ES_dichotom = data.frame(total = data$t0_ES_total, WHO = data$WHO_responder)

mod = glm(WHO ~ total, data = ES_dichotom, family = "binomial")
summary(mod)

# prediction
predict(mod, newdata = ES_dichotom, type = "response")

task = makeClassifTask(target = "WHO", positive = "Yes", data = ES_dichotom)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(ES_dichotom$WHO)[1]) / as.numeric(table(ES_dichotom$WHO)[2])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr1 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)

perf_data_es_who = getBMRPerformances(bmr1, as.df = TRUE)
mean_bac = mean(perf_data_es_who$bac, na.rm = TRUE)
mean_auc = mean(perf_data_es_who$auc, na.rm = TRUE)
mean_tpr = mean(perf_data_es_who$tpr, na.rm = TRUE)
mean_tnr = mean(perf_data_es_who$tnr, na.rm = TRUE)

es_who = as.data.frame(list(bac = mean_bac,
                                auc = mean_auc,
                                tpr = mean_tpr,
                                tnr = mean_tnr))


# Modell mit BDI und likert 
ES_likert = data.frame(total = data$t0_ES_likert_total, BDI = data$BDI_responder)

mod = glm(BDI ~ total, data = ES_likert, family = "binomial")
summary(mod)

# prediction
predict(mod, newdata = ES_likert, type = "response")

task = makeClassifTask(target = "BDI", positive = "Yes", data = ES_likert)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(ES_likert$BDI)[1]) / as.numeric(table(ES_likert$BDI)[2])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr1 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)

perf_data_likert_bdi = getBMRAggrPerformances(bmr1)
# mean_bac = mean(perf_data_likert_who$bac, na.rm = TRUE)
# mean_auc = mean(perf_data_likert_who$auc, na.rm = TRUE)
# mean_tpr = mean(perf_data_likert_who$tpr, na.rm = TRUE)
# mean_tnr = mean(perf_data_likert_who$tnr, na.rm = TRUE)
# 
# likert_who = as.data.frame(list(bac = mean_bac,
#                                 auc = mean_auc,
#                                 tpr = mean_tpr,
#                                 tnr = mean_tnr))

# Modell mit BDI und ES
ES_dichotom = data.frame(total = data$t0_ES_total, BDI = data$BDI_responder)

mod = glm(BDI ~ total, data = ES_dichotom, family = "binomial")
summary(mod)

# prediction
predict(mod, newdata = ES_dichotom, type = "response")

task = makeClassifTask(target = "BDI", positive = "Yes", data = ES_dichotom)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(ES_dichotom$BDI)[1]) / as.numeric(table(ES_dichotom$BDI)[2])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr1 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)

perf_data_es_bdi = getBMRPerformances(bmr1, as.df = TRUE)
mean_bac = mean(perf_data_es_bdi$bac, na.rm = TRUE)
mean_auc = mean(perf_data_es_bdi$auc, na.rm = TRUE)
mean_tpr = mean(perf_data_es_bdi$tpr, na.rm = TRUE)
mean_tnr = mean(perf_data_es_bdi$tnr, na.rm = TRUE)

es_bdi = as.data.frame(list(bac = mean_bac,
                            auc = mean_auc,
                            tpr = mean_tpr,
                            tnr = mean_tnr))


# prediction group --------------------------------------------------------

data = read.csv2("clean.csv")
data$Population = as.factor(data$Population)

#ES likert
ES_likert = data.frame(total = data$ES_likert_total, Group = data$Population)

mod = glm(Group ~ total, data = ES_likert, family = "binomial")
summary(mod)

# prediction
predict(mod, newdata = ES_likert, type = "response")

task = makeClassifTask(target = "Group", positive = "klinisch", data = ES_likert)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(ES_likert$Group)[1]) / as.numeric(table(ES_likert$Group)[2])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr1 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)
Group_likert = getBMRAggrPerformances(bmr1)

#ES 
ES_dichotom = data.frame(total = data$ES_total, Group = data$Population)

mod = glm(Group ~ total, data = ES_dichotom, family = "binomial")
summary(mod)

# prediction
predict(mod, newdata = ES_dichotom, type = "response")

task = makeClassifTask(target = "Group", positive = "klinisch", data = ES_dichotom)
log = makeLearner("classif.logreg", predict.type = "prob")
r = as.numeric(table(ES_dichotom$Group)[1]) / as.numeric(table(ES_dichotom$Group)[2])
log = makeUndersampleWrapper(log, usw.rate = r)
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr1 = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)
Group_dichotom = getBMRAggrPerformances(bmr1)
