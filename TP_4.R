#TP 4 Bourgeois Bastien

getwd()

# 1 R�gression lin�aire

#Chargement de donn�es
ventes<-read.table("Ressources/ventes.txt", header=T)

ventes

x11()

#Nuage de points

plot(ventes, main="Chiffre d'affaire en fonction du budget accord�")
#Ces deux variables semblent lin�airement corr�l�es

cor.test(ventes$pub, ventes$CA)
#La probabilit� que ces deux variables ne soient pas corr�l�es est de 0.2008,
# on peut donc consid�rer que ces deux variables sont corr�l�es.

#R�gression lin�aire
regLin <- lm(CA~pub, data=ventes)
regLin$coef[1]
regLin$coef[2]
abline(regLin, col="firebrick2")
legend("topleft", "Droite de r�gression : CA = 127.97 + 3.46 * pub", text.col="firebrick2")

#Chiffre d'affaire th�orique
CAtheo=regLin$coef[1] + regLin$coef[2] * ventes$pub
CAtheo

#Coefficient de d�termination
R2 = sum((CAtheo - mean(ventes$CA))^2) / sum((ventes$CA- mean(ventes$CA))^2)
R2

cor(ventes$pub, ventes$CA)^2

#Budget publicitaire

budget = (198 - 127.97) / 3.46
budget

# 2 R�gression non lin�aire

#Chargement de donn�es
freinage<-read.table("Ressources/freinage.txt", header=T)

freinage

x11()

#nuage de points
plot(freinage, main="Distance de freinage en fonction de la vitesse")

#Corr�lation
cor.test(freinage$V, freinage$C)
# Les variables sont corr�l�es, et p-value nous le confirme, la probabilit� que ces deux variables ne
# soient pas corr�l�es est quasiment nulle.
# Le mod�le lin�aire ne semble pas adapt�.

#R�gression puissance
lnC = log(freinage$C)
lnV = log(freinage$V)

lnC
lnV

cor(lnC, lnV)
cor(freinage$C, freinage$V)

#Le meilleur mod�le est le mod�le puissance.

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

# 3 S�rie chronologique et r�gression exponentielle.

#Chargement de donn�es

mort<-read.table("Ressources/morinfantile.txt", header=T)

mort

x11()

#nuage de points
plot(mort, main="Taux de mortalit� infantile par p�riode")

#Le mod�le le plus adapt� semble �tre celui exponentiel.

lnM = log(mort$M)
lnT = log(mort$t)

regExp <- lm(lnT~lnM, data=mort)
regExp$coef[1]
regExp$coef[2]

M = exp(regExp$coef[1]) * exp(regExp$coef[2]) ^ mort$t
M

det = cor(mort$t, mort$M)^2
det

# Le mod�le le plus adapt� semble �tre celui exponentiel.

lines(mort$t, mort$M, col="red")

prevision = exp(regExp$coef[1]) * (exp(regExp$coef[2]) ^ 2010)
prevision
