
# This file is a generated template, your changes will not be overwritten

principalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "principalClass",
    inherit = principalBase,
    private = list(
        .varName = list(),
        .setVarNames = function(vars) {
            if (self$options$descAsVarName) {
                for (aVar in vars) {
                    aVarName <- attr(self$data[[aVar]], "jmv-desc", TRUE)
                    if (!is.null(aVarName))
                        private$.varName[[aVar]] <- aVarName
                    else
                        private$.varName[[aVar]] <- aVar
                }
            } else {
                private$.varName[vars] <- vars
            }
        },
        .init = function() {
            if (is.null(self$options$vars)) {
                self$results$summaryTable$setVisible(FALSE)
                self$results$kmoTable$setVisible(FALSE)
                self$results$loadingTable$setVisible(FALSE)
                self$results$obsTable$setVisible(FALSE)
                self$results$screePlot$setVisible(FALSE)
                self$results$varPlot$setVisible(FALSE)
                self$results$obsPlot$setVisible(FALSE)
                self$results$biPlot$setVisible(FALSE)
            } else {
                self$results$helpMessage$setVisible(FALSE)
            }

            if (is.null(self$options$groupVar)) {
                extraWidth <- 0
            } else {
                n <- max(nchar(levels(self$data[[self$options$groupVar]])))
                extraWidth <- 50 + n*8
            }
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            image <- self$results$varPlot
            width <- image$width
            height <- image$height

            if (userWidth > 0)
                width <- userWidth
            if (userHeight > 0)
                height <- userHeight
            #image <- self$results$varPlot
            image$setSize(width, height)
            image <- self$results$obsPlot
            image$setSize(width + extraWidth, height)
            image <- self$results$biPlot
            image$setSize(width + extraWidth, height)
        },
        .run = function() {
            if (is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            # check dim values
            nDim <- self$options$dimNum
            if (nDim > length(self$options$vars))
                reject("The number of dimensions cannot be greater than the number of variables")
            if (self$options$xaxis > nDim || self$options$yaxis > nDim)
                reject("X-Axis and Y-Axis cannot be greater than the number of dimensions")
            if (self$options$xaxis == self$options$yaxis)
                reject("X-Axis and Y-Axis cannot be equal")

            # Set variable names
            private$.setVarNames(c(self$options$vars, self$options$labelVar, self$options$groupVar))

            # remove cases with with NA in vars
            data <- self$data[complete.cases(self$data[,self$options$vars]),]
            # Be sure data is numeric (for ordinal data)
            for (aVar in self$options$vars) {
                data[[aVar]] <- as.numeric(data[[aVar]])
            }

            # Verify that cor Matrice is positive definite
            corrMat <- cor(data[,self$options$vars])
            if ( abs(det(corrMat)) < .Machine$double.eps)
                warningMsg <- .("The correlation matrix is not positive definite. Computations may not be accurate.\r\n")
            else
                warningMsg <- ""

            # KMO & Bartlett's test
            if (self$options$showKMO) {
                kmo <- psych::KMO(corrMat)
                bartlett <- psych::cortest.bartlett(corrMat, n = nrow(data))

                self$results$kmoTable$setRow(rowNo = 1,
                                             values = list(test = .("Bartlett's Test of Sphericity"),
                                                           statistic = bartlett$chisq,
                                                           df = bartlett$df, p = bartlett$p.value))
                self$results$kmoTable$setRow(rowNo = 2,
                                             values = list(test = .("Kaiser-Meyer-Olkin Measure of Sampling Adequacy (MSA)"),
                                                            statistic = kmo$MSA,
                                                            df = NULL, p = NULL))
            }

            # PCA computation
            # if (self$options$usePsych) {
            #     res <- private$.pca_psych(data[,self$options$vars], scale = self$options$stdVariables,
            #                          nfact = nDim, rotation = self$options$rotation)
            # } else {
            res <- private$.pca(data[,self$options$vars], scale = self$options$stdVariables,
                               nfact = nDim, rotation = self$options$rotation)
            # }

            if (!is.null(self$options$labelVar))
                rownames(res$scores) <- data[[self$options$labelVar]]
            else
                rownames(res$scores) <- rownames(data)
            if (!is.null(self$options$groupVar))
                res$group <- data[[self$options$groupVar]]

            rotationName <- switch(self$options$rotation,
                                   none = "None",
                                   Varimax = "Varimax",
                                   quartimax = "Quartimax",
                                   equamax = "Equamax",
                                   parsimax = "Parsimax",
                                   varimin = "Varimin",
                                   entropy = "Minimum entropy",
                                   tandemI = "Comrey's Tandem 1",
                                   tandemII = "Comrey's Tandem 2",
                                   bentlerT = "Bentler T"
                            )

            if (res$rotation != self$options$rotation) {
                warningMsg <- paste0(warningMsg, jmvcore::format(.("Unable to use {rotation} rotation."), rotation = rotationName))
                rotationNote <- .("No rotation used.")
            } else if (self$options$rotation != "none") {
                if (self$options$kaiser) {
                    rotationNote <- jmvcore::format(.("{rotation} rotation with Kaiser normalization was used."), rotation = rotationName)
                    res$rotationStr <- jmvcore::format(.("{rotation} rotation with Kaiser normalization"), rotation = rotationName) # for plot
                } else {
                    rotationNote <- jmvcore::format(.("{rotation} rotation was used."), rotation = rotationName)
                    res$rotationStr <- jmvcore::format(.("{rotation} rotation"), rotation = rotationName) # for plot
                }
            } else {
                rotationNote <- NULL
            }

            # Warning (rotation / positive definite)
            if (warningMsg !=""){
                weightsNotice <- jmvcore::Notice$new(self$options, type = NoticeType$WARNING,
                                                     name = '.weights',
                                                     content = warningMsg)
                self$results$insert(1, weightsNotice)
            }

            # Summary Table
            if (self$options$showSummary) {
                eigen <- res$eigenvalues
                eigenSum <- sum(eigen)
                eigenCum <- cumsum(eigen)
                ssl <- res$SSL
                sslCum <- cumsum(ssl)
                for (i in 1:nDim) { # first dimensions
                    self$results$summaryTable$addRow(rowKey = i,
                                                     list(comp = i,
                                                          eigenvalue = eigen[i],
                                                          initVarProp = eigen[i]/eigenSum,
                                                          initVarCum = eigenCum[i]/eigenSum,
                                                          loadings = ssl[i],
                                                          varProp = ssl[i]/eigenSum,
                                                          varCum = sslCum[i]/eigenSum
                                                     ))
                }
                if (length(eigen) > nDim) { # is there more dimensions ?
                    for (i in (nDim+1):length(eigen)) {
                        self$results$summaryTable$addRow(rowKey = i,
                                                         list(comp = i,
                                                              eigenvalue = eigen[i],
                                                              initVarProp = eigen[i]/eigenSum,
                                                              initVarCum = eigenCum[i]/eigenSum,
                                                              loadings = NULL,
                                                              varProp = NULL,
                                                              varCum = NULL
                                                         ))
                    }
                }
                if (!is.null(rotationNote))
                    self$results$summaryTable$setNote('rot', rotationNote)
            }

            # Loading Table
            if (self$options$showLoadings) {
                for(i in 1:nDim) {
                    self$results$loadingTable$addColumn(name = paste0("loading:",i), title = as.character(i), superTitle = "Component", type = "number", format = "zto")
                }
                self$results$loadingTable$addColumn(name = "QLT",
                                                    title = "Extraction", #ifelse(self$options$stdVariables, "Communalities", "Explained"),
                                                    type = "number")
                #loadings <- as.data.frame.array(res$loadings)
                for(aVar in rownames(res$loadings)) {
                    values = list()
                    values[["var"]] <- private$.varName[[aVar]]
                    for(i in 1:nDim) {
                        values[[paste0("loading:",i)]] <- res$loadings[aVar, i]
                    }
                    values[["QLT"]] <- res$communalities[aVar]
                    self$results$loadingTable$setRow(rowKey = aVar, values = values)
                }
                if (!is.null(rotationNote))
                    self$results$loadingTable$setNote('rot', rotationNote)
            }

            # Observation Table
            if (self$options$showObservations) {
                if (is.null(self$options$labelVar))
                    self$results$obsTable$addColumn("obs", title = "Observation", type = "integer")
                else
                    self$results$obsTable$addColumn("obs", title = private$.varName[[self$options$labelVar]], type = "text")
                if (!is.null(self$options$groupVar))
                    self$results$obsTable$addColumn("group", title = private$.varName[[self$options$groupVar]], type = "text")
                for(i in 1:nDim) {
                    self$results$obsTable$addColumn(as.character(i), title = as.character(i), , superTitle = "Component", type = "number", format = "zto")
                }
                self$results$obsTable$addColumn("qlt", title = "Extraction", type = "number", format = "zto")
                for (i in 1:nrow(res$scores)) {
                    values = list()
                    values["obs"] <- rownames(res$scores)[i]
                    if (!is.null(self$options$groupVar))
                        values["group"] <- as.character(res$group[i])
                    values["qlt"] <- res$qlt[i]
                    for(j in 1:nDim)
                        values[as.character(j)] <- res$scores[i,j]
                    self$results$obsTable$addRow(rowKey = i, values = values)
                }
                if (!is.null(rotationNote))
                    self$results$obsTable$setNote('rot', rotationNote)
            }

            # Plots
            if (self$options$showScreePlot) {
                screeplot <- self$results$screePlot
                screeplot$setState(res$eigenvalues)
            }
            if (self$options$showVarPlot) {
                varplot <- self$results$varPlot
                varplot$setState(res)
            }
            if (self$options$showObsPlot) {
                obsplot <- self$results$obsPlot
                obsplot$setState(res)
            }
            if (self$options$showBiplot) {
                biplot <- self$results$biPlot
                biplot$setState(res)
            }
        },
        .screeplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            nd <- length(res)
            plot <- ggplot(NULL,aes(x=1:nd, y=res))
            plot <- plot + geom_line(size=0.8) + geom_point(size=3, color="darkgrey")

            plot <- plot + labs(x = .("Component"), y = .("Eigenvalues"))
            plot <- plot + scale_x_continuous(breaks = 1:nd)
            #plot <- plot + theme(legend.position = c(1, 1), legend.justification = c(1, 1))
            plot <- plot + ggtheme

            title <- self$options$screeTitle
            if (!(title %in% c("", " "))) {
                if (title == "default")
                    title <- .("Scree Plot")
                plot <- plot + labs(title = title)
                plot <- plot + theme(plot.title = element_text(
                                            size = self$options$titleFontSize,
                                            face = self$options$titleFontFace,
                                            hjust = as.numeric(self$options$titleAlign)))
            }
            subtitle <- self$options$screeSubtitle
            if (!(subtitle %in% c("", " ", "default"))) {
                plot <- plot + labs(subtitle = subtitle)
                plot <- plot + theme(plot.subtitle = element_text(
                    size = self$options$subtitleFontSize,
                    face = self$options$subtitleFontFace,
                    hjust = as.numeric(self$options$subtitleAlign),
                    margin = margin(-5, 0, 15, 0)))
            }

            return(plot)
        },
        .varplot = function(image, ggtheme, theme, ...) {
            return(private$.pcaplot("var", image, ggtheme, theme))
        },
        .obsplot = function(image, ggtheme, theme, ...) {
            return(private$.pcaplot("obs", image, ggtheme, theme))
        },
        .biplot = function(image, ggtheme, theme, ...) {
            return(private$.pcaplot("biplot", image, ggtheme, theme))
        },
        .pcaplot = function(plotType, image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            # Axe Titles
            eigenSum <- sum(res$eigenvalues)
            propIn <- round(100*res$SSL/eigenSum,1)
            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis
            c1string <- paste0(.("Component "), dim1, " (", propIn[dim1],"%)")
            c2string <- paste0(.("Component "), dim2, " (", propIn[dim2],"%)")

            type <- self$options$biplotType
            if (plotType == "biplot" && type == "formPlot") {
                res$loadings <- t(t(res$loadings) / sqrt(res$SSL))
                #res$loadings <- res$loadings %*% diag(1/sqrt(res$SSL))
            } else if (plotType == "biplot" && type == "covPlot") {
                res$scores <- t(t(res$scores) / sqrt(res$SSL))
                #res$scores <- res$scores %*% diag(1/sqrt(res$SSL))
            }

            plot <- ggplot()

            # Reference lines
            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)
            # Unit circle
            if (self$options$stdVariables && plotType == "var")
                plot <- plot + ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), linewidth = 0.2, n = 720)

            # Obs Plot
            if (plotType != "var") {

                if (!is.null(self$options$groupVar))
                    res$scores <- cbind(as.data.frame(res$scores), group = res$group)

                obsData <- as.data.frame(res$scores)

                if (self$options$labelColor == "none")
                    labelColor <- self$options$obsColor
                else
                    labelColor <- self$options$labelColor

                c1 <- names(obsData)[dim1]
                c1 <- ensym(c1)
                c2 <- names(obsData)[dim2]
                c2 <- ensym(c2)

                if (!is.null(self$options$groupVar)) {
                    plot <- plot + geom_point(data = obsData, aes(x = !!c1, y = !!c2, label = rownames(obsData), color = group), size = self$options$pointSize)
                } else {
                    plot <- plot + geom_point(data = obsData, aes(x = !!c1, y = !!c2, label = rownames(obsData)), color = self$options$obsColor, size = self$options$pointSize)
                }
                if (!is.null(self$options$labelVar)) {
                    if (!is.null(self$options$groupVar) && self$options$labelColor == "none") {
                        plot <- plot + ggrepel::geom_text_repel(data = obsData, aes(x = !!c1, y = !!c2, label = rownames(obsData), color = group),
                                                                check_overlap = TRUE, box.padding = 0.4, min.segment.length = 0.6,
                                                                size = self$options$obsLabelSize/.pt)
                    } else {
                        plot <- plot + ggrepel::geom_text_repel(data = obsData, aes(x = !!c1, y = !!c2, label = rownames(obsData)),
                                                                check_overlap = TRUE, color = labelColor, box.padding = 0.4, min.segment.length = 0.6,
                                                                size = self$options$obsLabelSize/.pt)
                    }
                }
            }

            # Var Plot
            if (plotType !="obs") {
                if (self$options$biplotStretch && plotType == "biplot")
                    res$loadings <- res$loadings * self$options$biplotStretchFactor
                varData <- as.data.frame.array(res$loadings)

                rownames(varData) <- unlist(private$.varName[rownames(varData)])

                if (self$options$labelColor == "none")
                    labelColor <- self$options$varColor
                else
                    labelColor <- self$options$labelColor

                c1 <- names(varData)[self$options$xaxis]
                c1 <- ensym(c1)
                c2 <- names(varData)[self$options$yaxis]
                c2 <- ensym(c2)
                if (self$options$biplotLines && plotType == "biplot")
                    plot <- plot + geom_abline(data = varData, aes(intercept = 0, slope = !!c2/!!c1), linetype = 3, color="gray")
                plot <- plot + geom_segment(data = varData, aes(x = 0, y = 0, xend = !!c1, yend = !!c2),
                                            arrow = arrow(length = unit(0.05, "inches"), type = "closed"),
                                            color = self$options$varColor, size = 0.8)
                plot <- plot + ggrepel::geom_text_repel(data = varData, aes(x = !!c1, y = !!c2, label = rownames(varData)), check_overlap = TRUE,
                                                        position = ggpp::position_nudge_center(x = 0.2, y = 0.01, center_x = 0, center_y = 0),
                                                        size = self$options$varLabelSize/.pt, color = labelColor, fontface="bold")
             }

            # Finishing Plot
            plot <- plot + ggtheme

            # Axe limits
            if (plotType == "var") {
                xmin = 1.02*min(varData[[dim1]], -1)
                xmax = 1.02*max(varData[[dim1]], 1)
                ymin = 1.02*min(varData[[dim2]], -1)
                ymax = 1.02*max(varData[[dim2]], 1)
            } else if (plotType == "obs") { # floor/ceiling *2 /2 => extend the plot to next 0.5 point
                xmin = 1.01*floor(min(obsData[[dim1]])*2)/2
                xmax = 1.01*ceiling(max(obsData[[dim1]])*2)/2
                ymin = 1.01*floor(min(obsData[[dim2]])*2)/2
                ymax = 1.01*ceiling(max(obsData[[dim2]])*2)/2
            } else { # biplot
                xmin = 1.01*floor(min(obsData[[dim1]], varData[[dim1]])*2)/2
                xmax = 1.01*ceiling(max(obsData[[dim1]], varData[[dim1]])*2)/2
                ymin = 1.01*floor(min(obsData[[dim2]], varData[[dim2]])*2)/2
                ymax = 1.01*ceiling(max(obsData[[dim2]], varData[[dim2]])*2)/2
            }
            plot <- plot + coord_fixed(xlim = c(xmin,xmax), ylim = c(ymin,ymax))

            # Axe & legend titles
            if (!is.null(self$options$groupVar) && plotType != "var")
                plot <- plot + labs(x = c1string, y = c2string, color = private$.varName[[self$options$groupVar]])
            else
                plot <- plot + labs(x = c1string, y = c2string)

            # Axis ticks (be sure there's a tick at each integer)
            if (plotType != "var" && self$options$stdVariables) {
                if( xmin < -2 && xmax > 2)
                    plot <- plot + scale_x_continuous(breaks = c(-5:5))
                if( ymin < -2 && ymax > 2)
                    plot <- plot + scale_y_continuous(breaks = c(-5:5))
            }

            # Group Color Palette (obs and biplot)
            if (!is.null(self$options$groupVar) && plotType != "var") {
                if( self$options$colorPalette == 'jmv' ) {
                    n <- nlevels(obsData[["group"]])
                    plot <- plot + scale_color_manual(values = jmvcore::colorPalette(n=n, theme$palette, type="color"), na.value="grey55")
                } else {
                    plot <- plot + scale_color_brewer(palette = self$options$colorPalette, na.value="grey")
                }
            }

            # Plot frame
            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))

            # Plot title
            title <- self$options$get( paste0(plotType,"Title") )
            defaultTitle <- switch(plotType,
                                var = .("Component Plot"),
                                obs = .("Observation Plot"),
                                biplot = ifelse(type == "covPlot", .("Covariance Biplot"), .("Form Biplot"))
                            )
            if (!(title %in% c("", " "))) {
                if (title == "default")
                    title <- defaultTitle
                plot <- plot + labs(title = title)
                plot <- plot + theme(plot.title = element_text(
                    size = self$options$titleFontSize,
                    face = self$options$titleFontFace,
                    hjust = as.numeric(self$options$titleAlign)))
            }

            # Plot subtitle
            subtitle <- self$options$get( paste0(plotType,"Subtitle") )
            if (!(subtitle %in% c("", " ")) || (subtitle == "default" && res$rotation != "none")) {
                if (subtitle == "default") {
                    subtitle <- res$rotationStr
                }
                plot <- plot + labs(subtitle = subtitle)
                plot <- plot + theme(plot.subtitle = element_text(
                    size = self$options$subtitleFontSize,
                    face = self$options$subtitleFontFace,
                    hjust = as.numeric(self$options$subtitleAlign),
                    margin = margin(-5, 0, 15, 0)))
            }

            return(plot)
        },
        .pca_psych = function(data, scale = TRUE, nfact = 2, rotation = "none") {
            if (self$options$rotation == "varimax")
                res <- psych::principal(data, nfactors = nfact, cor = ifelse(scale,"cor","cov"),
                                        rotate = rotation, use = "complete.obs", eps = 1e-14)
            else
                res <- psych::principal(data, nfactors = nfact, cor = ifelse(scale,"cor","cov"),
                                        rotate = rotation, use = "complete.obs")
            eigenvalues <- res$values
            rotatedSSL <- res$Vaccounted["SS loadings",]
            loadings <- as.data.frame.array(res$loadings)

            # res$scores with principal coordinates
            scores <- res$scores %*% diag(sqrt(rotatedSSL))
            scores <- as.data.frame(scores)
            communalities <- res$communality

            # Score QLT
            zscores <- scale(data, scale = scale)
            norm2 <- rowSums(zscores**2)
            # pca without rotation
            res1 <- psych::principal(data, nfactors = nfact, rotate = "none", use = "complete.obs", cor = ifelse(scale,"cor","cov"))
            eigen1 <- res1$Vaccounted["SS loadings",]
            # Compute norm^2 of projections
            norm2pca <- res1$scores**2 %*% eigen1
            # then QLT
            qlt <- norm2pca / norm2
            return(list(
                eigenvalues = eigenvalues,
                SSL = rotatedSSL,
                loadings = loadings[,],
                scores = scores,
                communalities = communalities,
                qlt = qlt,
                rotation = rotation
            ))
        },
        .pca = function(data, scale = TRUE, nfact = 2, rotation = "none") {
            data <- jmvcore::naOmit(data)
            res <- prcomp(data, scale. = scale)
            # Full solution
            eigenvalues <- res$sdev**2
            # Loadings (principal)
            loadings <- res$rotation[,1:nfact] %*% diag(res$sdev[1:nfact])
            # Loading QLT (Communalities)
            if (scale)
                communalities <- rowSums(loadings**2)
            else
                communalities <- rowSums(loadings**2) / rapply(data, var)
            # Score QLT
            zscores <- scale(data, scale = scale)
            norm2 <- rowSums(zscores**2)
            norm2pca <- rowSums(res$x[,1:nfact]**2)
            qlt <- norm2pca / norm2
            # Rotation
            if (rotation %in% c("quartimax", "equamax", "parsimax", "varimin",
                                    "entropy", "tandemI", "tandemII", "bentlerT", "Varimax")) {
                rotatedRes <-  try(do.call(getFromNamespace(rotation,'GPArotation'),list(loadings, normalize = self$options$kaiser)))
                if (inherits(rotatedRes, as.character("try-error"))) {
                    rotatedLoadings <- loadings
                    rotatedSSL <- eigenvalues[1:nfact]
                    rotatedScores <- res$x[,1:nfact]
                    rotation <- "none"
                } else {
                    rotatedLoadings <- rotatedRes$loadings
                    rotatedRes$rotmat <- t(solve(rotatedRes$Th))
                    rotatedSSL <- colSums(rotatedLoadings**2)
                    rotatedStdScores <- scale(res$x[,1:nfact]) %*% rotatedRes$rotmat
                    rotatedScores <- rotatedStdScores %*% diag(sqrt(rotatedSSL))
                }
            } else { # rotation == "none"
                rotatedLoadings <- loadings
                rotatedSSL <- eigenvalues[1:nfact]
                rotatedScores <- res$x[,1:nfact]
                rotation <- "none"
            }
            # Reoder the dims
            dimOrder <- order(rotatedSSL,decreasing=TRUE)
            rotatedSSL <- rotatedSSL[dimOrder]
            rotatedLoadings <- rotatedLoadings[,dimOrder]
            rotatedScores <- rotatedScores[,dimOrder]
            # Fix axis orientations (from psych::principal)
            sign.tot <- sign(colSums(rotatedLoadings[,]))
            sign.tot[sign.tot==0] <- 1
            rotatedLoadings <- rotatedLoadings %*% diag(sign.tot)
            rotatedScores <- rotatedScores %*% diag(sign.tot)
            #
            return(list(
                eigenvalues = eigenvalues,
                SSL = rotatedSSL,
                loadings = rotatedLoadings,
                scores = rotatedScores,
                communalities = communalities,
                qlt = qlt,
                rotation = rotation
            ))
        }

    )
)

