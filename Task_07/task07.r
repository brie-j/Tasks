setwd("~/Desktop/Evolution/Tasks/Task_07")
install.packages("phytools")
library("phytools")
library("ape")
text.string<- "(((((((cow, pig), whale),(bat, (lemur, human))),(robin, iguana)), coelacenth), (gold_fish, trout)), shark) ;"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels (frame="circle", bg='white', cex=1)
vert.tree
str(vert.tree)
tree<-read.tree(text="(((A,B), (C,D)), E) ;")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree<- force.ultrametric
(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col= 'black', border='white', main="", xlab=" edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
tipEdges
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
tree <- read.tree(text='(((A, B), (C, D)), E);')
plot.phylo(tree, type='phylogram', show.tip.label=FALSE, edge.color='red')
Question 4:
plot.phylo(tree, type='radial')
Question 5:
plot.phylo(tree, tip.color = 'red')
Question6-8: Anolis occultis had the shortest edge length.
plot(AnolisTree, cex=0.25) 
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
which(Lengths == min(Lengths))
names(Lengths)
AnolisTree2 <- drop.tip(AnolisTree, 'Anolis_occultus')