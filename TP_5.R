# TP 5

# Moyennes mobiles
champ <- read.table("Ressources/champagne.txt", header=T)
champ

champChrono <- ts(champ, frequency=12, start=c(1962,1))
champChrono

x11()
plot(champChrono, main="Ventes de champagne en France", ylab="millions de bouteilles", xlim=c(1962, 1972))

# Moyennes mobiles MM3
MM3 = numeric(105)
for(i in 2:104) { MM3[i] <- mean(champ$Ventes[(i-1):(i+1)]) }

MM3Chrono = ts(MM3[2:104], frequency=12, start=c(1962, 2))
lines(MM3Chrono, col="blue")

# Moyennes mobiles MM7
MM7 = numeric(103)
for(i in 4:102) { MM7[i] <- mean(champ$Ventes[(i-3):(i+3)]) }

MM7Chrono = ts(MM7[4:102], frequency=12, start=c(1962, 4))
lines(MM7Chrono, col="red")

# Moyennes mobiles MM12
MM12 = numeric(100)
for(i in 7:99) { MM12[i] <- mean(c(champ$Ventes[(i-5):(i+5)], mean(c(champ$Ventes[i-6], champ$Ventes[i+6])))) }

MM12Chrono = ts(MM12[7:99], frequency=12, start=c(1962, 7))
lines(MM12Chrono, col="green")

legend("topleft", c("brut", "MM3", "MM7", "MM12"), fill=c("black", "blue", "red", "green"))

#Régression linéaire
t = 1962.5+1/12*0:92
regL = lm(MM12[7:99]~t)
regL
abline(regL, col="purple")
legend("topright", "regL : -563.315 + 0.289 * t", fil="purple")

champChrono.hw=HoltWinters(champChrono)
predict(champChrono.hw, n.ahead=12)
lines(predict(champChrono.hw, n.head=12), col=5, lwd=2)

# Test du Khi2 d'ajustement
de <- read.table("Ressources/de.txt")

table(de)

pth = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
chisq.test(table(de), p = pth)

chisq.test(table(de), p = pth)$residuals

# Test Khi2 d'indépendance
tab <- matrix(c(30, 42, 58, 30, 35, 31), ncol=2)
rownames(tab) <- c("Dose D1", "Dose D2", "Dose D3")
colnames(tab) <- c("Guéris", "Non guéris")

par(mfrow=c(2, 1))
barplot(tab[,1], main="Personnes guéris")
barplot(tab[,2], main="Personnes non guéris")

prop.table(tab, margin=2)
round(100*prop.table(tab, margin=2), 1)

chisq.test(tab)
chisq.test(tab)$expected