Value_df$Accuracy <- as.factor(Value_df$Accuracy)
Value_df$ID <- as.factor(Value_df$ID)
Value_df$MemoryType <- as.factor(Value_df$MemoryType)
Value_df$Valence <- as.factor(Value_df$Valence)
logistic_acc <- glmer(Accuracy ~ MemoryType + Valence + AbsDiff + (1|ID),family = binomial(), data=Value_df)
Value_df$MemoryType[Value_df$MemoryType == "NN"] <- "OO"
summary(logistic_acc)
effects(logistic_acc)
plot_model(logistic_acc, type="pred")

exp(coef(logistic_acc)[1]) / (1 + exp(coef(logistic_acc)[1]))
#Intercept (Baseline): 0.50 ~ Perfectly even chance of selecting correct answer
#MemoryTypeOO: 0.28929 ~ 34% increase in likelihood of correctness (p = 9.58e-10)
#ValenceNeutral: 0.12249 ~ 13% increase in likelihood of correctness (p = 0.00838)
#AbsDiff: 0.05356 ~ 5.5% increase in likelihood of correctness (p = 0.00091)
