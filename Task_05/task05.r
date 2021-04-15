setwd('C:\\users\\Brie\\Desktop\\Evolution\\Tasks\\Task_05')
library(learnPopGen)
?coalescent.plot
pdf('r05-plot1.pdf', height=6, width=6)
coalescent.plot(n=5, ngen=10, colors=NULL)
dev.off()
pdf('r05-plot2.pdf', height=6, width=6)
coalescent.plot(n=10, ngen=5, colors=NULL)
dev.off()
pdf('r05-plot3.pdf', height=6, width=6)
coalescent.plot(n=10, ngen=10, colors=NULL)
dev.off()
## Question1- begins w/ alleles 1 & 5, 2 & 3 begin w/ allele 10. This can be changed by switching "n" in coalescent.plot().

## Question2- fixation varies depending on the number of alleles being stimulated for allele 5.

## Question3- the avg is between 1 & 2 offspring

## Question4- Fitness allows the individuals in the group to mate more often passing on the allele.

## Question5- Yes.

install.packages("coala")
install.packages("phytools")
install.packages("ape")
install.packages("maps")
library("ape")
library("maps")
library("phytools")
library("coala")
model <- coala::coal_model (sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
	coala::feat_mutation (10) +
	coala::feat_recombination (10) +
	coala::sumstat_trees() + 
	coala::sumstat_nucleotide_div ()
model
stats <- simulate (model, nsim = 1)
Diversity <- stats$pi
Diversity
## Question6- No. Recombination/Mutation.

Nloci <- length (stats$trees)
Nloci
t1 <- read.tree(text=stats$trees [[1]][1])
plot (t1)
axisPhylo ()
## Question7- specific individual has 2 alleles.

Agel <- max (node.height(t1))
Agel
t2 <- read.tree (text=stats$trees [[2]][1])
plot (t2)
axisPhylo()
## Question8- No.

par (mfrow=c(1,2))
plot (t1)
axisPhylo()
plot (t2)
axisPhylo()
comparePhylo(t1, t2)
t1_1 <- read.tree (text=stats$trees [[1]][1])
t1_2 <- read.tree (text=stats$trees [[1]][2])
par (mfrow=c(1,2))
plot (t1_1)
axisPhylo()
plot (t1_2)
axisPhylo()
comparePhylo(t1_1, t1_2)
for (locus in 1:Nloci) {
	ntrees <- length (stats$trees [[locus]])
	for (n in 1:ntrees) {
		if (locus == 1 && n == 1) {
			outPhy <- read.tree (text=stats$trees [[locus]][n])
		}
		else {
			outPhy <- ape:::c.multiPhylo (outPhy, read.tree (text=stats$trees [[locus]][n]))
		}
	}
}

par (mfrow=c(1,1))
phytools::densityTree(outPhy)

model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2) +
	feat_mutation(10) +
	feat_recombination(40) +
	sumstat_trees() +
	sumstat_nucleotide_div()
## Question9- No, more distribution within the alleles is what was expected to be seen here. 

model3 <- coal_model(10, 50) +
	feat_mutation(par_prior('theta', sample.int(100, 1))) +
	sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[['theta']])

pdf("brie.r05-Model3Graph.pdf", height=6, width=6)
plot(mean_pi)
plot(theta)
plot(mean_pi, theta, xlab='Diversity', ylab='Mutation Rate', pch=16, cex=1.3, col='red', main='Diversity due to Varied Mutation Rate')
abline(lm(mean_pi ~ theta), col='black')
dev.off()

+Extra Credit 
install.packages('phyclust')
library('phyclust')
activate_ms(priority = 100)
activate_msms(java = NULL, priority = 500, download = TRUE)

model4 <- coal_model(c(13, 18), loci_number = 10, loci_length = 1000, ploidy = 2) +
feat_selection(10, time = 1, population = 1) +
feat_selection(25, time = 1, population = 2) +
feat_size_change(0.5,
population = 1, 
time = '1', 
locus_group = 'all') +
feat_size_change(0.1, 
population = 2, 
time = '2', 
locus_group = 'all') +
feat_migration(1.2,
pop_to = 1, 
pop_from = 2,
symmetric = FALSE, time = '3', locus_group = 'all') +
feat_mutation(10) +
sumstat_trees() +
sumstat_nucleotide_div()
list_simulators()
stat2 <- simulate(model4, nsim = 40)
** Java wont install it says its no longer supported by Adobe so i guess I stop here for the EC :/