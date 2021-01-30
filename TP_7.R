#TP 7 Bourgeois Bastien
getwd()

# Exercice 1 ------------------------------------

hotel <- read.table("Ressources/hotel.txt", header=T)

x11()
par(mfrow = c(3, 2))

hist(hotel$nuits, main="Nuits", col="red", xlab="Nombre de nuit", ylab="Pourcentage passé")
hist(hotel$numDep, main="Département", col="blue", xlab="Numéro", ylab="Effectif")
hist(hotel$age, main="Age", col="orange", xlab="Age", ylab="Effectif")
#hist(hotel$annee, main="Annee", col="black", xlab="Annee", ylab="Effectif")
pie(table(hotel$annee), main="Effectif 2016/2017", col=c("tomato", "lightblue"), labels=paste(c("2016", "2017"),"  ", round(hotel$annee/40.3,1), "%"))

#pourcentage des client habitant 35
hotel[hotel$numDep == 35,]/length(hotel[hotel$numDep == 35,])

# moyenne 5. mean(hotel[hotel$annee == 2017,2])

boxplot(hotel[hotel$annee == 2016,2],main="Age en 2016", col="pink")
boxplot(hotel[hotel$annee == 2017,2],main="Age en 2017", col="lightblue")

#Execrice 2 --------------------------------------

x11()
par(mfrow = c(1, 1))

genou <- read.table("Ressources/genou.txt", header=T)

plot(genou, main="Récuperation en fonction de la dose administrée")

#fonction linéaire donc on utilise:
cor(genou$dose, genou$récup)
#Test exponentiel
cor(genou$dose, log(genou$récup))
#la corréclation exponentiel semble plus adapté car plus proche de -1

#Regression Linéaire
genou
regLin <- lm(dose~récup, data=genou)
regLin$coef[1]
regLin$coef[2]

lines(sort(genou$dose), exp(sort(genou$dose)), col="red", lwd=2)

#Exercice 3 ---------------------------------------

homme = c(11, 22, 12, 22, 22, 6, 20, 25, 5)
femme = c(12, 26, 12, 26, 16, 8, 20, 25, 5)

chisq.test(homme) # X^2= 0
chisq.test(femme) # X^2= 0

#On peut donc dire qu'il n'y a aucun un lien entre la couleur du yaourt et leurs saveur car le Khi2 est proche de 0

#Exercice 4 ------------------------------------

brasseur <- read.table("Ressources/brasseur.txt", header=T)

sigma <- 0.2
n <- 80

# On à un échantillion de 80 avec un écart type de 0.2
# Intervalle de confiance: [mean(brasseur$Contenance) - 1.96*(sigma/sqrt(n); mean(brasseur$Contenance) + 1.96*(sigma/sqrt(n)]
mean(brasseur$Contenance)
#Soit mean(brasseur$Contenance) est dans l'intervalle de confiance donc notre ingénieur à respecté la conformité de la production