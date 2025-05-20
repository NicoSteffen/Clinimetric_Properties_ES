df = data.frame( Group = c("Gruppe1", "Gruppe1", "Gruppe1", "Gruppe2", "Gruppe2", "Gruppe2", "Gruppe2", "Gruppe2", "KG", "KG", "KG", "KG"),
                 Verbesserung = c(9, 11, 7, 7, 5, 9, 10, 4, 4, 5, 8, 5))

model = lm(Verbesserung ~ Group, df)
anova(model)

var(df$Verbesserung[df$Group == "Gruppe2"])
