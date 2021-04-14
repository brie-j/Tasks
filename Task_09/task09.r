setwd('~/Desktop/Evolution/Tasks/Task_09')
library('phytools')
trees <- list()
births <-  c()
Fractions <- c()
for(i in 1:100) {
	births[i] <- runif(1)
	Fractions[i] <- runif(1)
	trees[[i]] <- pbtree(b = births[i], d = (births[i] * Fractions[i]),n = 100, = 1)
}
trees
trees[[i]]
plot(trees[[i]])
install.packages('geiger')
library('geiger')