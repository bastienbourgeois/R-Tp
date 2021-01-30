#TP 4 Bourgeois Bastien

getwd()

# 1 Régression linéaire

#Chargement de données
ventes<-read.table("Ressources/ventes.txt", header=T)

ventes

x11()

#Nuage de points

plot(ventes, main="Chiffre d'affaire en fonction du budget accordé")
#Ces deux variables semblent linéairement corrélées

cor.test(ventes$pub, ventes$CA)
#La probabilité que ces deux variables ne soient pas corrélées est de 0.2008,
# on peut donc considérer que ces deux variables sont corrélées.

#Régression linéaire
regLin <- lm(CA~pub, data=ventes)
regLin$coef[1]
regLin$coef[2]
abline(regLin, col="firebrick2")
legend("topleft", "Droite de régression : CA = 127.97 + 3.46 * pub", text.col="firebrick2")

#Chiffre d'affaire théorique
CAtheo=regLin$coef[1] + regLin$coef[2] * ventes$pub
CAtheo

#Coefficient de détermination
R2 = sum((CAtheo - mean(ventes$CA))^2) / sum((ventes$CA- mean(ventes$CA))^2)
R2

cor(ventes$pub, ventes$CA)^2

#Budget publicitaire

budget = (198 - 127.97) / 3.46
budget

# 2 Régression non linéaire

#Chargement de données
freinage<-read.table("Ressources/freinage.txt", header=T)

freinage

x11()

#nuage de points
plot(freinage, main="Distance de freinage en fonction de la vitesse")

#Corrélation
cor.test(freinage$V, freinage$C)
# Les variables sont corrélées, et p-value nous le confirme, la probabilité que ces deux variables ne
# soient pas corrélées est quasiment nulle.
# Le modèle linéaire ne semble pas adapté.

#Régression puissance
lnC = log(freinage$C)
lnV = log(freinage$V)

lnC
lnV

cor(lnC, lnV)
cor(freinage$C, freinage$V)

#Le meilleur modèle est le modèle puissance.

regPow <- lm(lnC~lnV, data=freinage)
regPow

C = exp(regPow$coef[1]) * freinage$V ^ regPow$coef[2]
C

#TEST : R2 = sum((C - mean(freinage$C))^2) / sum((freinage$V - mean(freinage$V))^2)
#TEST : R2

det = cor(freinage$C, freinage$V)^2

# Chemin pour 160km/h
chemin = exp(regPow$coef[1]) * 160 ^ regPow$coef[2]
chemin

# 3 Série chronologique et régression exponentielle.

#Chargement de données

mort<-read.table("Ressources/morinfantile.txt", header=T)

mort

x11()

#nuage de points
plot(mort, main="Taux de mortalité infantile par période")

#Le modèle le plus adapté semble être celui exponentiel.

lnM = log(mort$M)
lnT = log(mort$t)

regExp <- lm(lnT~lnM, data=mort)
regExp$coef[1]
regExp$coef[2]

M = exp(regExp$coef[1]) * exp(regExp$coef[2]) ^ mort$t
M

det = cor(mort$t, mort$M)^2
det

# Le modèle le plus adapté semble être celui exponentiel.

lines(mort$t, mort$M, col="red")

prevision = exp(regExp$coef[1]) * (exp(regExp$coef[2]) ^ 2010)
prevision
