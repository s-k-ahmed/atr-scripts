#Installing and loading required packages

packages <- c("car", "contrast", "dplyr", "lsr", "moments", "multcomp", "phonR", "readxl", "Rfolding", "rstatix", "TOSTER", "ufs", "vioplot")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, FUN = require, character.only = TRUE)

#Setting up recurring vectors
vowels <- c("i", "ɪ", "ɛ", "a", "ɔ", "ʊ", "u")
measures <- c("f1", "f2", "b1", "deltab1", "norma1a2", "cog", "cppnofilt", "cppavg")
measures.dur <- c("duration", "f1", "f2", "b1", "deltab1", "norma1a2", "cog", "cppnofilt", "cppavg")
measures.red <- c("f1", "f2", "deltab1", "norma1a2", "cog", "cppavg")

#Importing acoustic measurements from Excel file

BoaData <- read_excel("C:/Users/samue/OneDrive - Universiteit Leiden/Documents/@Leiden/5 Boa VH thesis/BoaData-v8.xlsx")

BoaData$vowel <- factor(BoaData$vowel, levels = vowels)

#Descriptive stats

descstats <- function(data, scale.ext) {
  # Plot F1 and F2 for all vowel tokens
  setwd("C:/Users/samue/OneDrive - Universiteit Leiden/Documents/@Leiden/5 Boa VH thesis")
  png(paste("images/BoaVowels",scale.ext,".png", sep=""), width = 1200, height = 1200, res = 150)
  with(data, plotVowels(f1, f2, vowel, 
                          var.col.by = vowel, 
                          pretty = TRUE, 
                          pch.tokens = vowel, 
                          cex.tokens = 0.7, 
                          alpha.tokens = 0.2, 
                          plot.means = TRUE, 
                          pch.means = vowel, 
                          cex.means = 1.4, 
                          family = "Charis SIL", 
                          ellipse.line = TRUE, 
                          ellipse.conf = 0.95))
  dev.off()

  #Save all diamond plots and statistics
  sink(file = paste("desc-statistics", scale.ext, ".csv", sep = ""))
  paste("Vowel,Measure,Min,LQ,Median,Mean,UQ,Max,Skew,StDev", "\n", sep = "") %>% cat()
  for (f in 1:length(measures.dur)) {
    msr <- measures.dur[f]
    intervalsM <- data.frame(matrix(nrow = 600, ncol = 7))
    colnames(intervalsM) <- rev(vowels)
    for (v in vowels) {
      data.subset <- subset(data, vowel == v)
      paste(paste(v, msr, 
            summary(data.subset[[msr]])[[1]], 
            summary(data.subset[[msr]])[[2]], 
            summary(data.subset[[msr]])[[3]], 
            summary(data.subset[[msr]])[[4]], 
            summary(data.subset[[msr]])[[5]], 
            summary(data.subset[[msr]])[[6]],
            skewness(data.subset[[msr]]),
            sd(data.subset[[msr]]),
            sep = ",", collapse = NULL), "\n", sep = "") %>% cat();
      data.subset[(nrow(data.subset)+1):600,] <- NA
      intervalsM[[v]] = data.subset[[msr]]
    }
    meansDiamondPlot(intervalsM, 
                    jitterHeight = 0.05, 
                    jitterWidth = 0, 
                    showData = TRUE, 
                    conf.level = 0.95, 
                    color = "blue", 
                    alpha = 0, 
                    dataSize = 1, 
                    dataAlpha = 0.1, 
                    dataColor = "black",
                    xlab = c("Duration (s)", "F1 (Hz)", "F2 (Hz)", "B1 (Hz)", "ΔB1 (Hz)", "A1*-A2* (dB)", "Centre of Gravity (Hz)", "Cepstral Peak Prominence - no filter (dB)", "Cepstral Peak Prominence - averaged across filters (dB)")[f],
                    outputFile = paste("C:/Users/samue/OneDrive - Universiteit Leiden/Documents/@Leiden/5 Boa VH thesis/images/diamond/diamond-", msr, scale.ext, ".png", sep = ""))
  }
  sink(file = NULL)
  
  # Derived vs underived diamond plots
  
  for (f in 1:length(measures.dur)) {
    msr <- measures.dur[f]
    uvs <- c("ʊ", "ɔ", "ɪ", "ɛ")
    svs <- c("ʊ", "ʊ", "ɪ", "ɪ")
    intervalsD <- data.frame(matrix(nrow = 600, ncol = length(uvs)))
    for (n in 1:length(uvs)) {
      data.subset <- subset(data, vowel == svs[n] & urvowel == uvs[n])
      data.subset[(nrow(data.subset)+1):600,] <- NA
      intervalsD[n] = data.subset[[msr]]
      colnames(intervalsD)[n] <- paste("/", uvs[n], "/ [", svs[n], "]", sep = "")
    }
    meansDiamondPlot(intervalsD, 
                     jitterHeight = 0.05, 
                     jitterWidth = 0, 
                     showData = TRUE, 
                     conf.level = 0.95, 
                     color = "blue", 
                     alpha = 0, 
                     dataSize = 1, 
                     dataAlpha = 0.1, 
                     dataColor = "black",
                     xlab = c("Duration (s)", "F1 (Hz)", "F2 (Hz)", "B1 (Hz)", "ΔB1 (Hz)", "A1*-A2* (dB)", "Centre of Gravity (Hz)", "Cepstral Peak Prominence - no filter (dB)", "Cepstral Peak Prominence - averaged across filters (dB)")[f],
                     outputFile = paste("C:/Users/samue/OneDrive - Universiteit Leiden/Documents/@Leiden/5 Boa VH thesis/images/deriv/diamond-deriv-", msr, scale.ext, ".png", sep = "")
    )
  }

  #Save all boxplots and violin plots

  for (f in measures) {
    png(paste("images/boxplots/boxplot-", f, scale.ext, ".png", sep = ""), width = 1200, height = 1200, res = 150)
    boxplot(data[[f]] ~ data$vowel, data = data, notch = TRUE, xlab = "Vowel", ylab = f)
    dev.off()
    data$vowel <- factor(data$vowel, levels = rev(vowels))
    png(paste("images/vioplots/vioplot-", f, scale.ext, ".png", sep = ""), width = 1200, height = 1200, res = 150)
    with(data, vioplot(formula = formula(paste(f, "~ vowel")), col=rgb(0.1,0.4,0.7,0.7) , names=rev(vowels), horizontal = TRUE, side = "right"))
    dev.off()
    data$vowel <- factor(data$vowel, levels = vowels)
  }
}

#Unadjusted values/plots

descstats(data = BoaData, scale.ext = "")

#Adjustment

BoaData <- BoaData %>%
  mutate(
    derived = if_else(
      vowel == urvowel, "underived", "derived"
    )
  )

BoaScaled <- BoaData
BoaScaled$b1 <- log10(BoaData$b1)
BoaScaled$deltab1 <- log10(BoaData$deltab1 - min(BoaData$deltab1) + 5)
BoaScaled$cog <- log10(BoaData$cog)

#Adjusted values/plots

descstats(data = BoaScaled, scale.ext = "-trsfm")

#Hypothesis 1

sink(file = "h1-unimodality.csv")
  "Measure,ɪ,ʊ\n" %>% cat();
  h1.F <- folding.statistics(data.matrix(subset(BoaScaled, vowel == "ɪ")[,measures.red]))
  h1.B <- folding.statistics(data.matrix(subset(BoaScaled, vowel == "ʊ")[,measures.red]))
  paste("all,", h1.F, ",", h1.B, "\n", sep = "")  %>% cat();
  for (m in measures.red) {
    phi.F <- folding.statistics(data.matrix(subset(BoaScaled, vowel == "ɪ")[[m]]))
    phi.B <- folding.statistics(data.matrix(subset(BoaScaled, vowel == "ʊ")[[m]]))
    paste(m, ",", phi.F, ",", phi.B, "\n", sep = "") %>% cat();
  }
  paste("n,", nrow(subset(BoaScaled, vowel == "ɪ")), ",", nrow(subset(BoaScaled, vowel == "ʊ")), "\n", sep = "") %>% cat();
sink(file = NULL)

#Hypothesis 2
  
sink(file = "h2-equivalence.csv")
  "Vowel,Measure,meanderiv,meanundrv,sdderiv,sdundrv,nderiv,nundrv,df,TOSTLower-t,TOSTUpper-t,Diff-t,TOSTLower-p,TOSTUpper-p,Diff-p\n" %>% cat();
  tost <- list()
  for (v in c("ɪ", "ʊ")) {
    for (m in measures) {
      dd <- subset(BoaScaled, vowel == v & derived == "derived")[[m]]
      ud <- subset(BoaScaled, vowel == v & derived == "underived")[[m]]
      tost[v] <- tsum_TOST(m1 = mean(dd), m2 = mean(ud), sd1 = sd(dd), sd2 = sd(ud), n1 = length(dd), n2 = length(ud), eqb = 0.880, alpha = 0.05, var.equal = FALSE, eqbound_type = "SMD")
      paste(v, m, mean(dd), mean(ud), sd(dd), sd(ud), length(dd), length(ud), tost[v][[1]]["t-test","df"], tost[v][[1]]["TOST Lower","t"], tost[v][[1]]["TOST Upper","t"], tost[v][[1]]["t-test","t"], tost[v][[1]]["TOST Lower","p.value"], tost[v][[1]]["TOST Upper","p.value"], tost[v][[1]]["t-test","p.value"], sep = ",")  %>% cat();
      "\n" %>% cat();
    }
  }
sink(file = NULL)

#Hypotheses 3-7

BoaScaled.U <- subset(BoaScaled, (vowel != "ɪ" & vowel != "ʊ") | derived == "underived")
BoaScaled.D <- subset(BoaScaled, (vowel != "ɪ" & vowel != "ʊ") | derived == "derived")

h3h7.contrasts <-  c(c(1, -1,  0,  0,  0,  0,  0),
                      c(0,  0,  0,  0,  0, -1,  1),
                      c(1, -2,  1,  0,  0,  0,  0),
                      c(0,  0,  0,  0,  1, -2,  1),
                      c(0,  0,  1,  0,  0, -1,  0))

h3h7.contrast.matrix <- matrix(h3h7.contrasts, nrow = 5, ncol = 7, byrow=TRUE)
colnames(h3h7.contrast.matrix) <- vowels
rownames(h3h7.contrast.matrix) <- c("ivsF", "uvsB", "Fcomp", "Bcomp", "EvsB")

models.U <- data.frame(matrix(nrow = 30))
models.D <- data.frame(matrix(nrow = 30))
for (m in measures){
  models.U[[m]] <- glm(formula(paste(m, "~ vowel")), data=BoaScaled.U, family='gaussian')
  models.D[[m]] <- glm(formula(paste(m, "~ vowel")), data=BoaScaled.D, family='gaussian')
}
h3h7 <- data.frame(matrix(nrow = 10))

sink(file = "h3h7-linear.csv")
  "Test,vowel,deriv,zvalue,pvalue\n"  %>% cat();
  h3h7$h3U <- summary(glht(model = models.U$deltab1, linfct = mcp(vowel = h3h7.contrast.matrix[c("ivsF", "uvsB"),])), test=adjusted("single-step"))
  h3h7$h4U <- summary(glht(model = models.U$norma1a2, linfct = mcp(vowel = h3h7.contrast.matrix[c("ivsF", "uvsB"),])), test=adjusted("single-step"))
  h3h7$h5U <- summary(glht(model = models.U$cog, linfct = mcp(vowel = h3h7.contrast.matrix["Fcomp",])), test=adjusted("single-step"))
  h3h7$h6U <- summary(glht(model = models.U$cppavg, linfct = mcp(vowel = h3h7.contrast.matrix[c("Fcomp", "Bcomp"),])), test=adjusted("single-step"))
  h3h7$h7U <- summary(glht(model = models.U$deltab1, linfct = mcp(vowel = h3h7.contrast.matrix[c("EvsB"),]), alternative = "greater"), test=adjusted("single-step"))
  h3h7$h3D <- summary(glht(model = models.D$deltab1, linfct = mcp(vowel = h3h7.contrast.matrix[c("ivsF", "uvsB"),])), test=adjusted("single-step"))
  h3h7$h4D <- summary(glht(model = models.D$norma1a2, linfct = mcp(vowel = h3h7.contrast.matrix[c("ivsF", "uvsB"),])), test=adjusted("single-step"))
  h3h7$h5D <- summary(glht(model = models.D$cog, linfct = mcp(vowel = h3h7.contrast.matrix["Fcomp",])), test=adjusted("single-step"))
  h3h7$h6D <- summary(glht(model = models.D$cppavg, linfct = mcp(vowel = h3h7.contrast.matrix[c("Fcomp", "Bcomp"),])), test=adjusted("single-step"))
  h3h7$h7D <- summary(glht(model = models.D$deltab1, linfct = mcp(vowel = h3h7.contrast.matrix[c("EvsB"),]), alternative = "greater"), test=adjusted("single-step"))
  for (h in c("h3U", "h4U", "h6U")) {
    mdl <- h3h7[[h]]$test
    for (t in 1:2) {
      paste(h, c("F", "B")[t], "underived", mdl$tstat[[t]], mdl$pvalues[t], sep = ",")  %>% cat();
      "\n" %>% cat();
    }
  }
  for (h in c("h5U", "h7U")) {
    mdl <- h3h7[[h]]$test
    paste(h, "X,underived", mdl$tstat[[1]], mdl$pvalues[1], sep = ",")  %>% cat();
    "\n" %>% cat();
  }
  for (h in c("h3D", "h4D", "h6D")) {
    mdl <- h3h7[[h]]$test
    for (t in 1:2) {
      paste(h, c("F", "B")[t], "derived", mdl$tstat[[t]], mdl$pvalues[t], sep = ",")  %>% cat();
      "\n" %>% cat();
    }
  }
  for (h in c("h5D", "h7D")) {
    mdl <- h3h7[[h]]$test
    paste(h, "X,derived", mdl$tstat[[1]], mdl$pvalues[1], sep = ",")  %>% cat();
    "\n" %>% cat();
  }
sink(file = NULL)

sink(file = "hX-summaries.txt")
  #h2
  "Hypothesis 2\n" %>% cat()
  print(tost["ɪ"])
  print(tost["ʊ"])
  #h3h7
  "Hypothesis 3 (DeltaB1)\n" %>% cat()
  print(h3h7$h3U)
  print(h3h7$h3D)
  "Hypothesis 4 (A1*-A2*)\n" %>% cat()
  print(h3h7$h4U)
  print(h3h7$h4D)
  "Hypothesis 5 (CoG)\n" %>% cat()
  print(h3h7$h5U)
  print(h3h7$h5D)
  "Hypothesis 6 (CPP)\n" %>% cat()
  print(h3h7$h6U)
  print(h3h7$h6D)
  "Hypothesis 7 (DeltaB1 again)\n" %>% cat()
  print(h3h7$h7U)
  print(h3h7$h7D)
sink(file = NULL)