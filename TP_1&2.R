#TP 1-2 Bourgeois Bastien

#Variance
maVar = function(x) {
	a = (1/(length(x)))
	return(a * sum((x-mean(x))^2))
}

maVar(1:10)
var(1:10)

maVar(1:10) == var(1:10)

#Manipulation de mesures
getwd()
tab=read.table("Ressources/mesures.txt", header=T)

monAnalyse = function() {
	sd(tab$poi)
	sd(tab$tai)
}

monAnalyse()

#Individu index 5 10 et 20
tab[5,]
tab[10,]
tab[20,]

#Tout les individus de sexe féminin
tab[tab$sexe == "f",]
nrow(tab[tab$sexe == "f",])

#Induvidus qui mesurent plus de 175cm
tab[tab$tai > 175,]

#Femme qui mesure plus de 175
tab[tab$tai > 175 & tab$sexe == "f",]

#3 indivus les plus lourd
poidec = tab[rev(order(tab[,2])),]
poidec[1:3,]

#Poids Moyen Femme
mean(tab[tab$sexe == "f",2])

#Poids Moyen Homme
mean(tab[tab$sexe == "h",2])

#Histogramme
histo = hist(tab$tai)
hist(tab$tai, main= "Taille du panel", col="mediumpurple4", xlab="Taille en cm", ylab="Effectif")
histo

#Histogramme Poids
histo = hist(tab$poi)
hist(tab$poi, main= "Poids du panel", col="mediumpurple4", xlab="Poids en kg", ylab="Effectif")
#On remarque que plus de 15 personnes pèsent entre 70 et 75 kg

#Histogramme Poids Femme
par(mfrow=c(1,2))
hist(tab[tab$sexe == "f",2], main= "Poids des femmes", col="mediumpurple", xlab="Poids en kg", ylab="Effectif", ylim=c(0, 20))
#Histogramme Poids Homme
hist(tab[tab$sexe == "h",2], main= "Poids des hommes", col="royalblue", xlab="Poids en kg", ylab="Effectif", ylim=c(0, 20))

#Histogramme Prob
hist(tab[tab$sexe == "f",2], main= "Poids des femmes", col="mediumpurple", xlab="Poids en kg", ylab="Effectif", ylim=c(0, 0.1), prob=T)
hist(tab[tab$sexe == "h",2], main= "Poids des hommes", col="royalblue", xlab="Poids en kg", ylab="Effectif", ylim=c(0, 0.1), prob=T)

#Camembert
par(mfrow=c(1,1))
eff=table(tab$sexe)
eff
pie(eff, main="Effectif Homme/Femme", col=c("tomato", "lightblue"), labels=paste(c("Femmes", "Hommes"),"  ", round(eff*100/66,1), "%"))

#Boxplot

par(mfrow=c(2,2))
b = boxplot(tab$poi, main = "Poids tous")
b$stat
boxplot(tab$tai,main="Taille tous")
boxplot(tab$tai~tab$sexe,main="Poids selon sexe")
boxplot(tab$tai~tab$sexe,main="Poids selon sexe", col=c("pink", "lightblue"),names=c("Femmes", "Hommes"))
#On constate que les boxplots font des graphiques personnalisés en fonction des tableaux qu'on leurs donnent (boxplot)

#NUAGE DE POINT
par(mfrow=c(1,1))
plot(0:50,sqrt(0:50), main="Fonction racine carré", col="red", ylab="", xlab="x entier de 0 à 50")

#Nuage de point de la taille en fonction du poids
plot(tab$tai,tab$poi, main="Taille en fonction du poids", col=as.numeric(tab$sexe), ylab="Poids en kg", xlab="Taille en cm")
legend("topleft", legend=c("Femmes", "Hommes"),fill=c(1,2))

#Autre présentation
plot(tab$poi, tab$tai, main=paste("Poids et Taille de", nrow(tab), "individus"), xlab="Poids", ylab="Taille", col=c("pink", "blue")[as.numeric(tab$sexe)], pch=c(17,15)[as.numeric(tab$sexe)], cex=1.5, las=1)
legend("topleft", c("Femmes", "Hommes"), pch=c(17, 20), col=c("pink", "blue"))

#Courbes
x = seq(-pi, pi, length=8)
plot(x, sin(x), type="l")
x=seq(-pi, pi, length=100)
plot(x, sin(x), type="l", main="Fonction Sinus entre -pi et +pi", lwd=10)
lines(c(-3.2, 3.2), c(0,0), lwd=1)

#Loi Normale
x = seq(-5, 5, length=100)
plot(x, dnorm(x), type="l", main="densité de la loi normale N(0, 5)")
lines(x, dnorm(x, sd=2), type="l", col="red")
lines(x, dnorm(x, sd=3), type="l", col="green")
lines(x, dnorm(x, sd=4), type="l", col="blue")
lines(x, dnorm(x, sd=5), type="l", col="orange")
