read_xlsx("1555054100_hospitalcosts.xlsx")->h
h
View(h)
h$RACE
colSums(is.na(h))
table(h$RACE)
h$RACE[is.na(h$RACE)]=1 
h$RACE
h
table(h$RACE)

h$AGE
table(h$AGE)
dev.off()
#1.
barplot(table(h$AGE), main = "Age Vs frequency", 
        xlab = "Age", 
        ylab = "Frequency", ylim = c(0, 400))
barplot(tapply(X=h$TOTCHG, INDEX = list(h$AGE), FUN = sum),
        main = "Age-Group Vs TOTCHG",
        xlab = "Age-Group", 
        ylab = "TOTCHG", ylim = c(0, 8 * (10)^5))

#2. Agency wants to find out the diagnosis related group
# That has maximum hospitilization and expenditure. 

table(h$LOS)
aggregate(h[, c("LOS", "TOTCHG")], by=list(h$APRDRG), FUN = sum)->tab
max(tab)

# 3. 

table(h$RACE)
barplot(tapply(X= h$TOTCHG, INDEX = h$RACE, FUN = mean),
        main = "RACE Vs TOTCHG", 
        xlab = "RACE",
        ylab = "TOTCHG", ylim = c(0, 5000))

aov(formula = TOTCHG~RACE, h)->model
summary(model)

#4.

aov(formula = TOTCHG~AGE+FEMALE, h)->model
summary(model)

lm(TOTCHG~AGE+FEMALE, h)->model
summary(model)

#5.
lm(LOS~AGE + FEMALE + RACE, h)->model
summary(model)

#6.

aov(TOTCHG~., h)->model
summary(model)

#--------------------------------------------------------------------------

h$APRDRG
table(h$APRDRG)
h$TOTCHG
table(h$TOTCHG)
summary(h$LOS)
summary(h$TOTCHG)
h$APRDRG[h$LOS == 41 | h$TOTCHG == 48388]

# To check Malpractise issue: Race of patient is related to hospitalisation costs. 
h$RACE
table(h$RACE)->ra
h$TOTCHG
table(h$TOTCHG)->to
barplot(ra, to, main = "Race Vs ToTCHG",
        xlab = "Race", ylab = "TOTCHG",
        ylim = c(0, 500))

# To properly utilise the cost, hosp needs to analyze by age and gender for proper res.