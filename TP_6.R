#TP 6 Bourgeois Bastien

#PARTIE 1 -------------------------------------------

#Exercice 1

x11()
par(mfrow = c(2, 2))

de1 = floor(runif(10000, 1, 7))
de1
mean(de1) # Somme des valeurs obtenues divisées par 10000
sd(de1)# sqrt((1/n) * somme((xi - mean(X))²))

hist(de1, main="10000 lancers de 1 dé", prob=T, breaks=seq(0.5, 6.5, 1), xlab=paste("moy = ", round(mean(de1), 2), "; sd = ", round(sd(de1), 2)), ylab="Histogramme des fréquences")

de2 = floor(runif(10000, 1, 6.5) + runif(10000, 1, 6.5))
de2
mean(de2)
sd(de2)

hist(de2, main="somme des 10000 lancers de 2 dés", prob=T, breaks=seq(0.5, 12.5, 1), xlab=paste("moy = ", round(mean(de2), 2), "; sd = ", round(sd(de2), 2)), ylab="Histogramme des fréquences")

de3 = floor(runif(10000, 1, 6.33) + runif(10000, 1, 6.33) + runif(10000, 1, 6.33))
de3
mean(de3)
sd(de3)

hist(de3, main="somme des 10000 lancers de 3 dés", prob=T, breaks=seq(0.5, 18.5, 1), xlab=paste("moy = ", round(mean(de3), 2), "; sd = ", round(sd(de3), 2)), ylab="Histogramme des fréquences")

de5 = floor(runif(10000, 1, 6.2) + runif(10000, 1, 6.2) + runif(10000, 1, 6.2) + runif(10000, 1, 6.2) + runif(10000, 1, 6.2))
de5
mean(de5)
sd(de5)

hist(de5, main="somme des 10000 lancers de 5 dés", prob=T, breaks=seq(0.5, 30.5, 1), xlab=paste("moy = ", round(mean(de5), 2), "; sd = ", round(sd(de5), 2)), ylab="Histogramme des fréquences")

# Pour un dé les issues sont à peu près équiprobables, cependant plus le nombre de dés est élevé plus il est probable d'obtenir un nombre proche de la moyenne.

xth = seq(5, 30, 0.1)
yth = dnorm(xth, mean=mean(de5), sd=sd(de5))
lines(xth, yth, col="red", lwd=2)

#Exercice 2

x11()

par(mfrow = c(3,2))

S1<-rexp(10000)
hist(S1,main="Experience 1x (10 000 tirages)", prob=T, xlab=paste("moy = ", round(mean(S1), 2), "; var = ", round(sd(S1) * sd(S1), 2)))

S2<-rexp(10000)+rexp(10000)
hist(S2, main="Experience 2x (10 000 tirages)", prob=T, xlab=paste("moy = ", round(mean(S2), 2), "; var = ", round(sd(S2) * sd(S2), 2)))

S5<-rexp(10000)
for(i in 2:5) S5<-S5 + rexp(10000)
hist(S5, main="Experience 5x (10 000 tirages)", prob=T, xlab=paste("moy = ", round(mean(S5), 2), "; var = ", round(sd(S5) * sd(S5), 2)))

S10<-rexp(10000)
for(i in 2:10) S10<-S10 + rexp(10000)
hist(S10, main="Experience 10x (10 000 tirages)", prob=T, xlab=paste("moy = ", round(mean(S10), 2), "; var = ", round(sd(S10) * sd(S10), 2)))

S50<-rexp(10000)
for(i in 2:50) S50<-S50 + rexp(10000)
hist(S50, main="Experience 50x (10 000 tirages)", prob=T, xlab=paste("moy = ", round(mean(S50), 2), "; var = ", round(sd(S50) * sd(S50), 2)))

S100<-rexp(10000)
for(i in 2:100) S100<-S100 + rexp(10000)
hist(S100, main="Experience 100x (10 000 tirages)", prob=T, xlab=paste("moy = ", round(mean(S100), 2), "; var = ", round(sd(S100) * sd(S100), 2)))

# On remarque que plus on effectue de tirage, plus la moyenne se rapproche de la valeur médianne.
# Quand on augmente le nombre de tirages la courbe passe d'une forme exponentielle à une forme de loi normale

xth = seq(50, 150, 1)
yth = dnorm(xth, mean=100, sd=10)
lines(xth, yth, col="red", lwd=2)

#Exercice 3

x11()

par(mfrow = c(1, 1))

moy = numeric(10000)
for(i in 1:10000) {
	moy[i] <- mean(rexp(100, rate=2))
}
hist(moy, main="Loi de moyenne de 100 variables Exp(2)", prob=T, xlab=paste("Moyenne = ", round(mean(moy), 2)))

#Cette distribution semble se rapprocher d'une loi normale.

xth = seq(0.1, 1, 0.001)
yth = dnorm(xth, mean=1/2, sd=sqrt(1/4 / 100))
lines(xth, yth, col="red", lwd=2)

#PARTIE 2 -------------------------------------------

x11()

x <- seq(-3.5, 3.5, 0.1)
yStu1 <- dt(x, df=1)
plot(x, yStu1, type="l", col="red", ylim=c(0, 0.5), main="Comparaison Student/Gauss", sub=c)
yStu2 <- dt(x, df=2)
lines(x, yStu2, col="orange")
yStu5 <- dt(x, df=5)
lines(x, yStu5, col="blue")
legend("topright", c("Student degré liberté 1", "Student degré liberté 2", "Student degré liberté 5", "Loi Normale"), fil=c("red", "orange", "blue", "black"))
dens = dnorm(x, mean = 0, sd = 1)
lines(x, dens, col="black")

qt(0.975, df=100)
qnorm(0.975)
# On a bien qt(0.975, df=100) > qnorm(0.975) => intervalle de confiance Student > intervalle de confiance Normale

#PARTIE 3 -------------------------------------------

#Exercice 4

# 1)
#	Echantillion:80
#	seuil critique +: 80+1,96*(0.5/10)=80.098
#	seuil critique -: 80-1,96*(0.5/10)=79,002
#
# 2)
#	(0.5/sqrt(X))=0.098/2 <=> (1/2)/sqrt(X) = 0.098/2*1.96
#				   <=> sqrt(X) = 1.96/0.098
#				   <=> X ~= 400
# 	Il faudrait prendre un échantillon de 400. Si on veut doubler la production il faut prendre un échantillion 4 fois  plus grand
#
# 3)	
#	H0: l'échantillion est conforme aux spécifications
#	H1: L'échantillion sort des normes imposés
#
# 4)
#	Voir feuille
#
# 5)
#	Oui car 80.032 est compris dans l'intervalle [79.002, 80.098]
#

#Excercice 5

tomate=read.table("Ressources/tomates.txt", header=T)
tomate

# 1.a
#	H0: Les tomates sont conforme aux diamètre demandé
#	H1: Les tomates ont un diametre plus petit que celui indiqué

# 1.b
#	seuil critique -: 7-1,645*(sd(tomate$diam)/sqrt(100))= [6.886]
#	Or mean(tomate$diam) = 6.642 est < à notre seuil critique donc le fournisseur n'a pas respecté le contrat

# 2.
#	seuil critique Bilatérale +-: 7 +- 1.96*(0.694/sqrt(100)) = [6.864; 7.136]

