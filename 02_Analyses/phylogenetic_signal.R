library(tidyverse)
library(dplyr)
library(picante)
library(geiger)
library(ape)
library(phytools)
library(viridis)
library(viridisLite)
library(patchwork)

#import S&B phylogeny--------------------

#bring in .tre file
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")
SBtree <- read.tree(file = "ALLMB.tre")
is.rooted(SBtree)
is.binary(SBtree)

#check for correct species in pruned tree
specieslist <- pruned.tree$tip.label
specieslist <- as.data.frame(specieslist)

#Blomberg's K (calculated with picante) --------------

##Upper Montane-------

phylosignal(um_win_sig, pruned.tree, reps = 5000, checkdata = TRUE) 


phylosignal(um_c1_sig, pruned.tree, reps = 5000, checkdata = TRUE)


phylosignal(um_c2_sig, pruned.tree, reps = 5000, checkdata = TRUE) 

##Pfeiler-------

phylosignal(pf_win_sig, pruned.tree, reps = 5000, checkdata = TRUE)

phylosignal(pf_c1_sig, pruned.tree, reps = 5000, checkdata = TRUE)

phylosignal(pf_w1_sig, pruned.tree, reps = 5000, checkdata = TRUE)

##Monument-------

phylosignal(mo_win_sig, pruned.tree, reps = 5000, checkdata = TRUE)

phylosignal(mo_w1_sig, pruned.tree, reps = 5000, checkdata = TRUE)

phylosignal(mo_w2_sig, pruned.tree, reps = 5000, checkdata = TRUE)

#Pagels lambda---------------------

##Upper Montane-----
phylosig(pruned.tree, um_win_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

phylosig(pruned.tree, um_c1_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

phylosig(pruned.tree, um_c2_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

##Pfeiler-------
phylosig(pruned.tree, pf_win_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

phylosig(pruned.tree, pf_c1_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

phylosig(pruned.tree, pf_w1_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

##Monument------
phylosig(pruned.tree, mo_win_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

phylosig(pruned.tree, mo_w1_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

phylosig(pruned.tree, mo_w2_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

#make phylogeny figures---------------
##Upper Montante origin site####
###within site#####
phylo_um_win <- contMap(pruned.tree, um_win_sig, res=100, plot=FALSE)
contMap_log_rs <- setMap(contMap_log_rs, viridisLite::viridis(n=8))
plot(contMap_log_rs)





