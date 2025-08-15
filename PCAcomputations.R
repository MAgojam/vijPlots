#Parameters
data <- iris[1:4]
scale <- TRUE
nfact <- 4
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
loadings <- stdLoadings %*% diag(res$sdev[1:nfact])
loadings
colSums(loadings**2) # = eigenvalues

# Communalities ( = QLT = Extraction) IF scale = TRUE
communalities <- rowSums(loadings**2)
communalities

# Extraction IF scale = FALSE
rowSums(loadings**2) / rapply(data[1:4], var)

# Scores (Principal)
scores <- res$x
head(scores)
apply(scores,2,var) # = eigenvalues

# Scores (Standard)
stdScores <- scores %*% diag(1/res$sdev[1:nfact])
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
rotatedSSL
# Standard Loadins
rotatedStdLoadings <- rotatedLoadings %*% diag(1/sqrt(rotatedSSL))
colSums(rotatedStdLoadings**2) # = 1
# Scores
rotatedRes$rotmat <- t(solve(rotatedRes$Th))
rotatedStdScores <- scale(res$x[,1:nfact]) %*% rotatedRes$rotmat
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
