library(mixOmics)
data(srbct)
X <- srbct$gene

# Outcome y that will be internally coded as dummy:
Y <- srbct$class 
dim(X); length(Y)

pca.srbct <- pca(X, ncomp = 3, scale = TRUE)

plotIndiv(pca.srbct, group = srbct$class, ind.names = FALSE,
          legend = TRUE, 
          title = 'SRBCT, PCA comp 1 - 2')

plsda.srbct <- plsda(X,Y, ncomp = 10)

perf.plsda.srbct <- perf(plsda.srbct, validation = 'Mfold', folds = 3, 
                         progressBar = TRUE,  # Set to TRUE to track progress
                         nrepeat = 50)         # We suggest nrepeat = 50

plot(perf.plsda.srbct, sd = TRUE, legend.position = 'horizontal')

final.plsda.srbct <- plsda(X,Y, ncomp = 3)

plotIndiv(final.plsda.srbct, ind.names = FALSE, legend=TRUE,
          comp=c(1,2), ellipse = TRUE, 
          title = 'PLS-DA on SRBCT comp 1-2',
          X.label = 'PLS-DA comp 1', Y.label = 'PLS-DA comp 2')

plotIndiv(final.plsda.srbct, ind.names = FALSE, legend=TRUE,
          comp=c(2,3), ellipse = TRUE, 
          title = 'PLS-DA on SRBCT comp 2-3',
          X.label = 'PLS-DA comp 2', Y.label = 'PLS-DA comp 3')


perf.final.plsda.srbct <- perf(final.plsda.srbct, validation = 'Mfold', 
                               folds = 3, 
                               progressBar = FALSE, # TRUE to track progress
                               nrepeat = 10) # we recommend 50 

perf.final.plsda.srbct$error.rate$BER[, 'max.dist']

perf.final.plsda.srbct$error.rate.class$max.dist

# Grid of possible keepX values that will be tested for each comp
list.keepX <- c(1:10,  seq(20, 100, 10))
list.keepX

# This chunk takes ~ 2 min to run
# Some convergence issues may arise but it is ok as this is run on CV folds
tune.splsda.srbct <- tune.splsda(X, Y, ncomp = 4, validation = 'Mfold', 
                                 folds = 5, dist = 'max.dist', 
                                 test.keepX = list.keepX, nrepeat = 10)

# To show the error bars across the repeats:
plot(tune.splsda.srbct, sd = TRUE)
# The optimal number of components according to our one-sided t-tests
tune.splsda.srbct$choice.ncomp$ncomp

# Optimal number of components based on t-tests on the error rate
ncomp <- tune.splsda.srbct$choice.ncomp$ncomp 
ncomp

# Optimal number of variables to select
select.keepX <- tune.splsda.srbct$choice.keepX[1:ncomp]  
select.keepX

splsda.srbct <- splsda(X, Y, ncomp = ncomp, keepX = select.keepX) 

perf.splsda.srbct <- perf(splsda.srbct, folds = 5, validation = "Mfold", 
                          dist = "max.dist", progressBar = TRUE, nrepeat = 50)

# perf.splsda.srbct  # Lists the different outputs
perf.splsda.srbct$error.rate

perf.splsda.srbct$error.rate.class
