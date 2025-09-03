#Parameters
data <- iris[1:4]
scale <- TRUE
nfact <- 3
# Remove missing value (casewise)
data <- na.omit(data)

#### PCA (no rotation)
res <- prcomp(data, scale. = scale, rank. = nfact)

# eigenvalues
eigenvalues <- res$sdev**2
eigenvalues

# Total inertia (= number of variables)
inertia <- sum(eigenvalues)
inertia

# % of variance
eigenvalues / inertia
# Cumulative % of variance
cumsum(eigenvalues) / inertia

# Standard Loadings
stdLoadings <- res$rotation
colSums(stdLoadings**2) # = 1

# Principal Loadings  (Standard loadings scaled by singular values = sqrt(eigenvalues))
loadings <- stdLoadings %*% diag(sqrt(eigenvalues[1:nfact]))
loadings
colSums(loadings**2) # = eigenvalues

# Communalities ( = QLT = Extraction) IF scale = TRUE
communalities <- rowSums(loadings**2)
communalities

# Extraction IF scale = FALSE
rowSums(loadings**2) / rapply(data, var)

# Scores (Principal)
scores <- res$x
head(scores)
apply(scores,2,var) # = eigenvalues

# Scores (Standard)
stdScores <- scores %*% diag(1/sqrt(eigenvalues[1:nfact]))
head(stdScores)
apply(stdScores,2,var) # = 1

# Score QLT (Extraction)
zdata <- scale(data, scale = TRUE)
norm2 <- rowSums(zdata**2)
norm2pca <- rowSums(scores**2)
qlt <- norm2pca / norm2
head(qlt)

# Reconstruction
coord <- scores %*% t(stdLoadings)
head(coord)
coord2 <- stdScores %*% t(loadings)
head(coord2)
max(coord-coord2) # < 1e-15
head(zdata)
boxplot(zdata - coord)
max(zdata - coord) # < 1e-15 if nfact = max = 4

plot(zdata[,1:2], col="red", pch=0)
points(coord[,1:2], col="blue", pch=1)


# PCA with rotation
rotatedRes <- GPArotation::Varimax(loadings, normalize = FALSE)
# Principal Loadings
rotatedLoadings <- rotatedRes$loadings
rotatedSSL <- colSums(rotatedLoadings**2)
# =  diag(t(rotatedLoadings) %*% rotatedLoadings)
rotatedSSL
# Standard Loadins
rotatedStdLoadings <- rotatedLoadings %*% diag(1/sqrt(rotatedSSL))
rotatedStdLoadings
colSums(rotatedStdLoadings**2) # = 1

rotatedLoadings
loadings %*% rotatedRes$Th

(stdLoadings %*% rotatedRes$Th) %*% diag(sqrt(rotatedSSL))

plot(rotatedLoadings, col="red", pch=20)
points(stdLoadings %*% rotatedRes$Th, col="blue", pch=20)
abline(v=0)
abline(h=0)

sqrt(eigenvalues[1:3]) %*% rotatedRes$Th

# conclusion ?
# La rotation ne s'applique que au loadings utilisés pour la créer et aux observations
# pour le reste, passer par homothétie sqrt(eigen)


# Scores
# rotatedRes$rotmat <- t(solve(rotatedRes$Th)) # for oblique rotation ?
rotatedRes$rotmat <- rotatedRes$Th
rotatedStdScores <- stdScores %*% rotatedRes$rotmat
rotatedScores <- rotatedStdScores %*% diag(sqrt(rotatedSSL))
apply(rotatedStdScores, 2, var) # = 1
apply(rotatedScores, 2, var) # = eigenvalues

# Reconstruction

coordFromRot <- rotatedStdScores %*% t(rotatedLoadings)
head(coordFromRot)
coordFromRot2 <- rotatedScores %*% t(rotatedStdLoadings)
head(coordFromRot2)
max( coordFromRot2 - coordFromRot) # < 1e-15
head(zdata)
max( zdata - coordFromRot ) # < 1e-15
boxplot(zdata - coordFromRot)
plot(zdata[,1:2], col="red", pch=0)
points(coordFromRot[,1:2], col="blue", pch=1)
plot(zdata[,3:4], col="red", pch=0)
points(coordFromRot[,3:4], col="blue", pch=1)


# PCA with rotation (STATA WAY)
colSums(stdLoadings**2)
StataRotatedRes <- GPArotation::Varimax(stdLoadings, normalize = FALSE)
# Standard Loadings
StataRotatedStdLoadings <- StataRotatedRes$loadings
StataRotatedStdLoadings
colSums(StataRotatedStdLoadings**2)
# Principal Loadings
StataRotatedLoadings <- loadings %*% StataRotatedRes$Th
StataRotatedSSL <- colSums(StataRotatedLoadings**2)
StataRotatedSSL



StataRotatedLoadings
StataRotatedStdLoadings %*% diag(sqrt(StataRotatedSSL)) # NON

stdLoadings %*% StataRotatedRes$Th
StataRotatedStdLoadings

colSums((StataRotatedLoadings %*% diag(1/sqrt(StataRotatedSSL)))**2)

loadings %*% StataRotatedRes$Th
stdLoadings %*% StataRotatedRes$Th

StataRotatedRes$Th

t(t(StataRotatedLoadings) / sqrt(StataRotatedSSL))
