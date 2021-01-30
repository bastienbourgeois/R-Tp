#TP 3 Bourgeois Bastien

getwd()
iris<-read.table("Ressources/iris.txt", header=T, dec=",")


# Correlation entre variable quantitative
cor(iris$SeLo, iris$SeLa)

# Matrice de correlation
cor(iris[,-5])

# Les deux variables les plus corrélées sont les PeLo et les PeLa
# Cela signifie que ces deux variables sont fortement semblables

x11()
par(mfrow=(1, 1))

eff = table(iris$Species)
pie(eff, main = "Effectifs des espèces", col=c("red", "green", "orange"), labels=paste(c("Setosa", "Versicolor", "Virginica"), "  ", round(eff*100/150,1), "%"))

#histogramme

x11()
par(mfrow=c(4,4))

summary(iris)


seto = iris[iris$Species == "setosa",]
vers = iris[iris$Species == "versicolor",]
virg = iris[iris$Species == "virginica",]

hist(iris$SeLo, main="SeLo", col="red", xlab="Largeur", ylab="Longueur", xlim=c(4.3, 7.9))
hist(iris$SeLa, main="SeLa", col="blue", xlab="Largeur", ylab="Longueur", xlim=c(2, 4.4))
hist(iris$PeLo, main="PeLo", col="green", xlab="Largeur", ylab="Longueur", xlim=c(1, 6.9))
hist(iris$PeLa, main="PeLa", col="orange", xlab="Largeur", ylab="Longueur", xlim=c(0.1, 2.5))

hist(seto$SeLo, main="SeLo setosa", col="red", xlab="Largeur", ylab="Longueur", xlim=c(4.3, 7.9))
hist(seto$SeLa, main="SeLa setosa", col="blue", xlab="Largeur", ylab="Longueur", xlim=c(2, 4.4))
hist(seto$PeLo, main="PeLo setosa", col="green", xlab="Largeur", ylab="Longueur", xlim=c(1, 6.9))
hist(seto$PeLa, main="PeLa setosa", col="orange", xlab="Largeur", ylab="Longueur", xlim=c(0.1, 2.5))

hist(vers$SeLo, main="SeLo versicolor", col="red", xlab="Largeur", ylab="Longueur", xlim=c(4.3, 7.9))
hist(vers$SeLa, main="SeLa versicolor", col="blue", xlab="Largeur", ylab="Longueur", xlim=c(2, 4.4))
hist(vers$PeLo, main="PeLo versicolor", col="green", xlab="Largeur", ylab="Longueur", xlim=c(1, 6.9))
hist(vers$PeLa, main="PeLa versicolor", col="orange", xlab="Largeur", ylab="Longueur", xlim=c(0.1, 2.5))

hist(virg$SeLo, main="SeLo virginica", col="red", xlab="Largeur", ylab="Longueur", xlim=c(4.3, 7.9))
hist(virg$SeLa, main="SeLa virginica", col="blue", xlab="Largeur", ylab="Longueur", xlim=c(2, 4.4))
hist(virg$PeLo, main="PeLo virginica", col="green", xlab="Largeur", ylab="Longueur", xlim=c(1, 6.9))
hist(virg$PeLa, main="PeLa virginica", col="orange", xlab="Largeur", ylab="Longueur", xlim=c(0.1, 2.5))

# La variable PeLo définie le mieux les différentes espèces
# Affichage des iris triés par la longueur de leurs pétallescolors()


trieSurPeLo = iris[order(iris$PeLo),]
trieSurPeLo

# Boîtes à moustaches

x11()
par(mfrow=c(2,2))

boxplot(iris$SeLo~iris$Species, main="SeLo selon Espèce", col=c("royalblue3", "red", "orange"))
boxplot(iris$SeLa~iris$Species, main="SeLa selon Espèce", col=c("royalblue3", "red", "orange"))
boxplot(iris$PeLo~iris$Species, main="PeLo selon Espèce", col=c("royalblue3", "red", "orange"))
boxplot(iris$PeLa~iris$Species, main="PeLa selon Espèce", col=c("royalblue3", "red", "orange"))

#Nuage de pointswhile()

x11()

plot(iris[,-5], col=as.numeric(iris$Species))

# On remarque que sur les caractères 
