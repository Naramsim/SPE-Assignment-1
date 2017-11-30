#!/usr/bin/Rscript

# the path where both this script and the .csv files are
# setwd("/home/rstudio")

#---------#
# HELPERS #
#---------#

# operations
as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
}

element.last <- function(set) {
    tail(as.numeric.factor(set), 1)
}

sigfig <- function(number, digits=significantDigits) {
    formatC(signif(number, digits=digits), digits=digits, format="fg", flag="#")
}

# outputs
output.newline <- function() {
    cat("\n")
}

output.dimax <- function(value, probability, object) {
    print(sprintf("D_%i_max = %i", object, value))
    print(sprintf("        error probability = %s", probability))
}

output.alphai <- function(value, correlation, object) {
    print(sprintf("alpha_%i = %s", object, value))
    print(sprintf("        correlation = %f", correlation))
}

output.variance <- function(value, valueLower, valueUpper, confidence, zalphatwo) {
    print(sprintf("sigma^2 = %s [%s, %s]", value, valueLower, valueUpper))
    print(sprintf("        confidence = %s (z = %s)", confidence, zalphatwo))
}

# graphs
plot.new <- function() {
    dev.new()
    par(
        cex=0.6,
        cex.main=1.8,
        cex.lab=3.0,
        cex.axis=2.6,
        lwd=1.2,
        mar = c(6, 6, 6, 2)
        )
}

plot.dimax <- function(data, object) {
    title = "" # paste0("UNIFORM DISTRIBUTION COMPONENT - OBJECT ", object)
    xLabel = "new downloads (n)"
    yLabel = "frequency (n)"
    color = "gray90"
    plot.new()
    barplot(table(data), col=color, main=title, xlab=xLabel, ylab=yLabel)
}

plot.alphai <- function(xData, yData, fit, object) {
    scale = 1000

    title = "" # paste0("LINEAR FIT - OBJECT ", object)
    xLabel = paste0("downloads (n/", scale, ")")
    yLabel = paste0("revenue (n/", scale, ")")
    color = "gray60"
    weight = 2

    plot.new()
    plot(xData/scale, yData/scale, col=color, main=title, xlab=xLabel, ylab=yLabel)
    lines(xData/scale, fit/scale, lwd=weight)
}

plot.alphas <- function(xData, fit, object) {
    scale = 1000
    xLim = c(20, 160)
    yLim = c(0, 2700)

    title = "" # paste0("LINEAR FIT - OBJECT ", object)
    xLabel = paste0("downloads (n/", scale, ")")
    yLabel = paste0("revenue (n/", scale, ")")
    colors = c("gray10", "gray20", "gray30", "gray40", "gray50", "gray60", "gray70", "gray80", "gray90", "gray100")

    if (object == 1) {
        plot.new()
        plot(xData/scale, fit/scale, xlim=xLim, ylim=yLim, type="l", col=colors[object], main=title, xlab=xLabel, ylab=yLabel)
    }
    lines(xData/scale, fit/scale, xlim=xLim, ylim=yLim, type="l", col=colors[object])
}

plot.gaussian <- function(data, bin) {
    title = "" #paste0("NOISE DISTRIBUTION (BIN = ", bin, ")")
    xLabel = "noise"
    yLabel = "density"
    color = "gray90"
    weight = 2

    breaks = (floor(max(data))-ceiling(min(data))) / bin

    plot.new()
    hist(data, freq=FALSE, breaks=breaks, col=color, main=title, xlab=xLabel, ylab=yLabel)
    curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, lwd=weight)
}

#----------#
# SETTINGS #
#----------#

doPlot = TRUE # whether the data is plotted or not
doAll = FALSE # whether the linear fit is plotted for a single object or all of them (irrelevant if doPlot == FALSE)
objectToPlot = 6 # the object of which to plot the D_i_max distribution and the alpha_i fit (the latter in case doAll == FALSE)
doSplit = TRUE # whether the objects noise sets are splitted in two disjointed subsets to calculate sigma or not

significantDigits = 5 # significant digits kept throughout the computation
zalphatwos = list() # list of z_a/2 values for given confidence rates
zalphatwos[["0.900"]] = 1.645
zalphatwos[["0.950"]] = 1.960
zalphatwos[["0.980"]] = 2.326
zalphatwos[["0.990"]] = 2.576
zalphatwos[["0.998"]] = 3.08
zalphatwos[["0.999"]] = 3.27

#------#
# DATA #
#------#

data = read.csv("./data/pezze.csv")
subsets = split(data, data$i)

variances = numeric()

currentObject = 0
for (subset in subsets) {
    currentObject = currentObject+1
    output.newline()

    #--------------------#
    # D_i_max ESTIMATION #
    #--------------------#

    uniformComponent = diff(subset$x)
    uniformDistribution = as.data.frame(table(uniformComponent))

    D_i_max = element.last(uniformDistribution$uniformComponent) # (1)

    possibleValues = D_i_max+1+1 # (2)
    totalEvents = length(uniformComponent)
    neverTurnProb = ((possibleValues-1)/possibleValues)^totalEvents

    output.dimax(D_i_max, format(neverTurnProb), currentObject)
    if (currentObject == objectToPlot)
        if (doPlot && !doAll) plot.dimax(uniformComponent, currentObject)

    #--------------------#
    # alpha_i ESTIMATION #
    #--------------------#

    downloads = subset$x
    revenues = subset$y

    correlation = cor(downloads, revenues)

    fit = lm(revenues ~ downloads) # (3)
    steepness = fit$coefficients[[2]]
    intercept = fit$coefficients[[1]]
    fitLine = downloads*steepness + intercept

    output.alphai(sigfig(steepness), correlation, currentObject)
    if (currentObject == objectToPlot)
        if (doPlot && !doAll) plot.alphai(downloads, revenues, fitLine, currentObject)

    if (doPlot && doAll) plot.alphas(downloads, fitLine, currentObject)

    #------------------#
    # sigma ESTIMATION #
    #------------------#

    gaussianComponent = fitLine-revenues # (4)

    if (doSplit) {
        leftSplit = head(gaussianComponent, length(gaussianComponent)/2)
        rightSplit = tail(gaussianComponent, length(gaussianComponent)/2)

        variances = append(variances, sd(leftSplit))
        variances = append(variances, sd(rightSplit))
    }
    else {
        variances = append(variances, sd(gaussianComponent))
    }

    if (currentObject == objectToPlot)
        if (doPlot && !doAll) plot.gaussian(gaussianComponent, 280)
}

#-----------------------#
# confidence ESTIMATION #
#-----------------------#

output.newline()

degrees = length(variances)-1
varianceMean = mean(variances)
varianceVariance = sd(variances)

if (doPlot && !doAll) plot.gaussian(variances, 5)

for (confidence in names(zalphatwos)) {
    output.newline()

    zalphatwo = zalphatwos[[confidence]]
    varianceLower = varianceMean - zalphatwo*sqrt(varianceVariance/degrees)
    varianceUpper = varianceMean + zalphatwo*sqrt(varianceVariance/degrees)

    output.variance(sigfig(varianceMean), sigfig(varianceLower), sigfig(varianceUpper), confidence, zalphatwo)
}

#-------#
# NOTES #
#-------#

# (1) The largest result found in the distribution is likely to be the desired D_i_max.

# (2) The likelihood of D_i_max actually being the found value plus one can be obtained by calculating the probability
#     this value is actually part of the distribution, but was never extracted:
#
#                n
#         / t-1 \
#     p = | --- |
#         \  t  /
#
#     1 is added to account for '0' being a valid result, and 1 for our real maximum assumption.

# (3) The linear regression fit is estimated using least square mean function.

# (4) The gaussian noise component for each time step is the difference between the real value and the corresponding one
#     on our linear fit.
