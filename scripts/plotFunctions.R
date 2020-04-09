library(gplots)
library(coda)
library(beastio)
source("../scripts/HPDBeanPlot.R")
source("../scripts/palettes.R")

plotCounts <- function(dates, counts, ylim=c(0,120), 
                       xlab="Date of symptom onset", ylab="Daily number of cases", clab="Cumulative number of cases",
                       col_local = oxCols$green4, col_china = oxCols$blue1, col_other = dark$orange, alpha = 0.5, 
                       beside = FALSE, plotCumSum = FALSE, plotGrid=TRUE, plotLabel="", labelDates=NULL) {
  

    plot(dates, ylim=ylim, type='n', bty='n', axes=FALSE, xlab=xlab, ylab=ylab, xaxs='i', yaxs='i')
    
    if (plotGrid) {
        #grid(nx=0, ny=NULL)
        grid(nx=0, ny=NULL, lwd=0.5, col=mPal(oxCols$gray6))
    }
  
    if (beside) {
      barplot2(counts, beside=beside, names.arg=NULL, width=1/3, offset=0, space=c(0,0), 
               border=c(mPal(col_other), mPal(col_china), mPal(col_local)), 
               col=c(mPal(col_other, alpha), mPal(col_china, alpha), mPal(col_local,alpha)),
               las = 1, add=TRUE)
    } else {
      barplot2(counts, beside=beside, names.arg=NULL, width=1, offset=0, space=0, 
               border=c(mPal(col_other), mPal(col_china), mPal(col_local)), 
               col=c(mPal(col_other, alpha), mPal(col_china, alpha), mPal(col_local,alpha)),
               las = 1, add=TRUE)
    }
    
    #print(format.Date(bigTicks, format="%b %d"))
    #print("nothing")
    bigTicks   <- seq.Date(min(dates), max(dates), by='weeks')
    tickIdxs   <- match(bigTicks, dates)
    smallTicks <- 1:length(dates)
    axis(1, at=tickIdxs, labels=gsub(" 0"," ", format.Date(bigTicks, format="%b %d")), lwd=0, lwd.ticks=1)
    axis(1, at=smallTicks, labels=NA, tcl=-0.25)
    #abline(v=tickIdxs, lty=1, lwd=0.5, col='red', xpd=TRUE)
    
    if (!is.null(labelDates)) {
        idxs <- match(labelDates, dates)
        segments(idxs, rep(0,length(idxs)), idxs, rep(1*ylim[2],length(idxs)), lty=2, lwd=1)
        
        if (is.null(names(labelDates))) {
            #text(x=idxs, y=rep(1*ylim[2],length(idxs)), labels = LETTERS[1:length(idxs)], pos=3, xpd=TRUE, cex=0.8)
            text(x=idxs, y=rep(1*ylim[2],length(idxs)), labels = format.Date(labelDates, format="%b %d"), pos=3, xpd=TRUE, cex=0.8, srt=45)
        } else {
            text(x=idxs, y=rep(1*ylim[2],length(idxs)), labels = names(labelDates), pos=3, srt=45, xpd=TRUE, cex=0.8)
        }
    }
    
    mtext(side=3, plotLabel, line=0, at=-0.12*ncol(counts), cex=1.5)
    
    if (plotCumSum) {
        par(new=TRUE)
        plot(dates, cumsum(colSums(counts)), 
             type='l', bty='n', axes=FALSE, xlab="", ylab="", yaxs='i',
             col=mPal(oxCols$red2), lwd=2, xpd=TRUE)
        #lines(casedb$symptom_onset, cumsum(counts["local_guangdong", ]),    col=mPal(col_local), lwd=2)
        #lines(casedb$symptom_onset, cumsum(counts["import_china_other", ]), col=mPal(col_china), lwd=2)
        #lines(casedb$symptom_onset, cumsum(counts["import_other", ]),       col=mPal(col_other), lwd=2)
        
        axis(4, at = c(0, axTicks(2), round(max(sum(colSums(counts)))/100)*100), las=1)
        mtext(side=4, text = clab, line=3, cex=1)
    }
    
    #abline(v=0:100, col='red')
}


plotScatter <- function(df, x=1, y=2, col=3, cex=1, pt.cex=1, fillMap=NULL, colMap=NULL, 
                        add=FALSE, plotGrid=TRUE, plotLegend=TRUE, legendOffset=-0.2, ...) {
  
  
  colGroups <- levels(df[[col]])
  
  if (!add) {
    plot(df[[x]], df[[y]], type='n', bty='n', axes=FALSE, ...)
    axis(1, at=c(-Inf, axTicks(1), Inf), las=1)
    axis(2, at=c(-Inf, axTicks(2), Inf), las=1)
  }
  
  if (plotGrid) {
    grid(lwd=0.5, col=mPal(oxCols$gray6))
  }
  
  for (group in colGroups)  {
    idxs <- df[[col]] == group
    print(group)
    print(colMap[[group]])
    points(df[idxs, x], df[idxs, y], col=fillMap[[group]], pch=16, cex=pt.cex)
    points(df[idxs, x], df[idxs, y], col=colMap[[group]],  pch=1,  cex=pt.cex)
  }
  
  if (plotLegend) {
    legend("top", horiz = TRUE, inset=c(0,legendOffset), bty='n', xpd=TRUE, pch=16, cex=cex, pt.cex=pt.cex,
           border =sapply(names(colMap), function(x) colMap[[x]]), 
           col    =sapply(names(fillMap), function(x) fillMap[[x]]), 
           legend =names(fillMap), text.col="#00000000")
    
    legend("top", horiz = TRUE, inset=c(0,legendOffset), bty='n', xpd=TRUE, pch=1, cex=cex, pt.cex=pt.cex,
           col    =sapply(names(colMap), function(x) colMap[[x]]), 
           legend =names(fillMap))
  }
}


plotStats <- function(stats, ylab="", ylim=NULL, names=c(), plotGrid=TRUE, plotStrip=TRUE, plotN=TRUE, las=1) {
  
  
  if (is.null(ylim)) {
    ymin <- min(c(0, sapply(stats, min)))
    ymin <- max(ymin, -2)
    
    ymax <- max(sapply(stats, max))
    ymax <- min(ymax, 2)
    
    ylim <- c(ymin, ymax)  
  }
  
  boxplot(stats, ylab = ylab, outline=ifelse(plotStrip, FALSE, TRUE), col=mPal(oxCols$blue1,0.5),
          ylim=ylim, names = names, las = las, lwd = 1, xaxs='i', yaxs='i', xaxt='n')
  
  if (plotGrid) {
    grid(nx=0, ny=NULL, lwd=0.5, col=mPal(oxCols$gray6))
    #abline(h=axTicks(2), lty=2, col=mPal(oxCols$gray3), lwd=0.5)
  }
  
  if (plotStrip) {
    stripchart(stats, vertical = TRUE, method = "jitter", jitter = 0.2, add = TRUE, pch = 16, col = mPal(oxCols$red2, 0.5), cex=1)
  }
  
}






# Go through all files in list of files and plots alignments
# snpFiles and gapFiles should be in the same order!
#
# For each file, plot the regions covered as rectangles in seqCol
# SNPs wrt the reference are plotted as lines in snpCol
# Ambiguous sites wrt the reference are plotted as lines in ambCol
#
# If plotStats are true, the number of SNPs, ambiguous sites and coverage are also plotted
#
# Returns a list of all SNPs in the alignment
plotAlignment <- function(snpFiles, gapFiles, seqlen, cex.axis=0.6, barwidth=0.7, names=NULL, plotGrid=TRUE, plotNames=TRUE, plotStats=TRUE, plotXAxis=TRUE,
                          seqCol=mPal(oxCols$blue1), seqBorder=NA, bgCol=mPal(oxCols$gray3), snpCol=mPal(oxCols$red2), ambCol=mPal(oxCols$orange2)) {
  
  allSNPs <- list()
  nrSeqs  <- length(snpFiles)
  
  plot(1, type='n', ylim=c(0,nrSeqs+1), xlim=c(1,seqlen), xaxs='i', yaxs='i', 
       yaxt='n', xlab="", ylab="", bty='n', axes=FALSE)
  
  if (plotXAxis) {
    axis(1, at=c(0, axTicks(1), seqlen), cex.axis=cex.axis, xpd=TRUE)
  }
  
  if (length(seqCol) < nrSeqs) {
    seqCol <- rep(seqCol, nrSeqs)
  }
  
  if (length(seqBorder) < nrSeqs) {
    seqBorder <- rep(seqBorder, nrSeqs)
  }
  
  barwidth <- barwidth/2
  
  labels=c()
  for (i in 1:nrSeqs) {
    
    #############
    # Read data #
    #############
    
    # Read SNPs
    snps     <- read.csv(snpFiles[i], skip=1, colClasses = c("numeric", "character", "character"))
    snpIdxs  <- which(snps[, 3] %in% c("A","C","G","T"))
    ambIdxs  <- setdiff(1:nrow(snps), snpIdxs)
    nrSnps   <- length(snpIdxs)
    nrAmbig  <- length(ambIdxs)
    
    #print(paste(snpFiles[i], nrow(snps)))
    for (j in 1:nrow(snps)) {
      if (!is.na(snps[j,1]) && length(snps[j,2]) > 0) {
        
        snp <- paste(snps[j,c(2,1,3)], collapse=" ")
        #print(snp)
        #print(names(allSNPs))
        if (snp %in% names(allSNPs)) {
          allSNPs[[snp]] <- allSNPs[[snp]] + 1
        } else {
          allSNPs[[snp]] <- 1
        }
      }
    }
    #print(paste(colnames(snps[3]), nrSnps, nrAmbig))
    
    
    # Read gaps and complement
    gaps     <- read.csv(gapFiles[i], skip=1)
    if (nrow(gaps) == 0) {
      coverage <- 1
      contigs  <- data.frame(start=1, end=seqlen)
    } else {
      coverage <- sum(gaps$length)/seqlen
      
      contigs <- data.frame(start=(gaps$end[1:(nrow(gaps)-1)]+1), end=(gaps$start[2:nrow(gaps)]-1))
      if (gaps$start[1] > 1)
        contigs <- rbind(c(1, gaps$start[1]-1), contigs)
      if (gaps$end[nrow(gaps)] != seqlen)
        contigs <- rbind(contigs, c(gaps$end[nrow(gaps)]+1, seqlen))
      
    }
    
    
    ########
    # Plot #
    ########
    
    # Background
    rect(1, i-0.2, seqlen, i+0.2, col=bgCol, border=NA)
    
    # Draw rectangles
    for (j in 1:nrow(contigs)) {
      rect(contigs$start[j], i-barwidth, contigs$end[j], i+barwidth, col=seqCol[i], border=seqBorder[i], lwd=0.5)
    }
    
    # Draw mutations
    if (nrSnps > 0)  segments(snps$position[snpIdxs], i-barwidth, snps$position[snpIdxs], i+barwidth, col=snpCol, lwd=1.5)
    if (nrAmbig > 0) segments(snps$position[ambIdxs], i-barwidth, snps$position[ambIdxs], i+barwidth, col=ambCol, lwd=1.5)
    
    # Labels
    if (is.null(names)) {
      seqid <- gsub("\\.","-",colnames(snps[3]))
    } else {
      seqid <- names[i]
    }
    
    if (plotStats) {
      seqid <- paste0(seqid, " (", nrSnps, "/", nrAmbig,", ", round((1-coverage)*100,2), "%)")
    }
    labels <- c(labels, seqid)
    
  }
  
  if (plotGrid) {
     abline(v=seq(1000,seqlen,by=1000), lty=3, lwd=0.5, col=mPal(oxCols$gray6))
  }
  
  # Labels
  if (plotNames) {
      axis(4, at=1:nrSeqs, labels=labels, las=1, lwd.ticks=NA, lwd=NA, cex.axis=cex.axis)
  }
  
  return( allSNPs )
  
  
}


plotSNPHist <- function(snpTable, col=mPal(oxCols$gray6), cex.axis=0.8, cutoff=2, ylim=NULL, plotGrid=TRUE) {
  
  x <- y <- c()
  if (length(snpTable) > 0) {
      for (snp in names(snpTable)) {
        x <- c(x, as.numeric(strsplit(snp, split=" ")[[1]][2]))
        y <- c(y, snpTable[[snp]])
      }
    
      if (is.null(ylim)) {
        ylim   <- range(pretty(c(0, round(max(unlist(snpTable))))))
      }
  } else {
      ylim <- c(0,1)
  }
  
  
  
  plot(1, type='n', ylim=ylim, xlim=c(1,seqlen), xaxs='i', yaxs='i', 
       yaxt='n', xlab="", ylab="", bty='n', axes=FALSE)
  
  if (plotGrid) {
    #abline(h=1:(max(ylim)-1), lty=3, lwd=0.5)
    grid(nx=0, ny=NULL, lwd=0.5, col=mPal(oxCols$gray6))
  }
  
  # SNPs
  #print(col)
  if (length(x) > 0) {
      segments(x, rep(0, length(y)), x, y, col=col, lwd=1.5)
  }
  
  axis(1, at=c(0, axTicks(1), seqlen), cex.axis=cex.axis, xpd=TRUE)
  axis(2, las=1, cex.axis=cex.axis)
  
  
  # Label SNPs > cutoff
  if (max(y) > cutoff) {
      text(x[y > cutoff], y[y > cutoff]+1.5, names(snpTable)[y > cutoff], offset = c(0,3), srt=45, cex=cex.axis, col=mPal(oxCols$red2), xpd=TRUE)
  }
  
}




plotTaxonSets <- function(ages, samples, names=NULL, 
                          dateRange=c(as.Date("2019-11-01"),as.Date("2020-03-01")), 
                          maxwidth=1, bw='sj', monophyletic=NULL, col=dark$orange, sampleCol=oxCols$blue1, 
                          plotSymptomOnset = TRUE, plotLabel="", ...) {
  
    n      <- ncol(ages)
    xlim   <- lubridate::decimal_date(dateRange)
    xrange <- diff(xlim)
    
    plot(1, type='n', bty='n', ylim=c(0.9,n+1), xlim=xlim, #xlim=c(xlim[1]-0.04*xrange, xlim[2]+0.04*xrange), 
         axes=FALSE, xlab='', yaxs='i', xaxs='i', ...)
    
    #dateRange  <- as.Date(lubridate::floor_date(lubridate::date_decimal(xlim), unit = 'day'))
    bigTicks   <- seq.Date(dateRange[1], dateRange[2], by='weeks')
    smallTicks <- seq.Date(dateRange[1], dateRange[2], by='days')
    
    for (i in 1:(length(bigTicks)-1)) {
      if (i %% 2 == 1) {
        rect(lubridate::decimal_date(bigTicks[i]), 0.9, lubridate::decimal_date(bigTicks[i+1]), n+1, col="#EDEDED", border=NA)
      }
    }
    
    axis(1, at=lubridate::decimal_date(bigTicks), labels=format.Date(bigTicks, format="%b %d"), lwd=0, lwd.ticks=1)
    axis(1, at=lubridate::decimal_date(smallTicks), labels=NA, tcl=-0.25)
    #abline(v=lubridate::decimal_date(bigTicks), lty=1, lwd=0.5, col='red', xpd=TRUE)
    abline(h = 1:n, lwd=0.5, lty=3, col=mPal(oxCols$gray6))
    
    HPDBeanPlot(ages,  side='right',  fill=mPal(col, 0.5), border=mPal(col), medcol=mPal(oxCols$red2), medwidth=0.25, 
                bw=bw, maxwidth=maxwidth, add=TRUE, axes=FALSE, lwd=c(1,1,NA), horiz=TRUE)
    
    tmrca_hpd <- getHPDMedian(ages)
    samples   <- samples[order(-samples$decimal_date), ]
    for (i in 1:length(names)) {
        taxonSet <- names[i]
        taxonSamples <- samples[which(samples$taxon_set == taxonSet), ]

        if (nrow(taxonSamples) > 0) {
            names[i] <- paste0(names[i],"\n(n = ", nrow(taxonSamples), ")")
            #print(taxonSamples)
          
            dates <- taxonSamples$decimal_date
            
            sampleCols <- rep(mPal(oxCols$gray6),      nrow(taxonSamples))
            fillCols   <- rep(mPal(oxCols$gray6, 0.5), nrow(taxonSamples))
            sampleCols[taxonSamples$province == "Guangdong"] <- mPal(sampleCol)
            fillCols[taxonSamples$province == "Guangdong"]   <- mPal(sampleCol, 0.5)
            
            onsetCols <- rep(mPal(oxCols$gray6),      nrow(taxonSamples))
            onsetFill <- rep(mPal(oxCols$gray6, 0.5), nrow(taxonSamples))
            onsetCols[taxonSamples$infection_location == "guangdong"] <- mPal(sampleCol)
            onsetFill[taxonSamples$infection_location == "guangdong"] <- mPal(sampleCol, 0.5)
            
            # Rect (sampling period)
            rect(min(dates)-(0.5/365), i, max(dates)+(0.5/365), i+0.5, col=mPal(oxCols$blue2,0.5), border=NA)
            
            # Pegs (samples)
            segments(dates, rep(i, length(dates)), dates, rep(i+0.25, length(dates)), col=fillCols, lwd=1)
            points(dates, rep(i+0.25, length(dates)), col="#FFFFFF",  pch=16)
            points(dates, rep(i+0.25, length(dates)), col=fillCols,   pch=16)
            points(dates, rep(i+0.25, length(dates)), col=sampleCols, pch=1)
            
            # Onset of symptoms
            if (plotSymptomOnset) {
                for (j in 1:nrow(taxonSamples)) {
                    if (taxonSamples$province[j] == "Guangdong") {
                        height <- j*(0.4/nrow(taxonSamples))
                        lines(rep(dates[j],2), c(i, i-height), lty=1, lwd=1, col=onsetFill[j])
                        
                        if (!is.na(taxonSamples$symptom_onset[j])) {
                            onset <- lubridate::decimal_date(taxonSamples$symptom_onset[j])
                            lines(c(onset, dates[j]), rep(i-height,2), lty=1, lwd=1, col=onsetFill[j])
                            points(onset, i-height, pch=8, col=onsetCols[j], cex=1)
                        } else {
                            lines(c(tmrca_hpd[i,2], dates[j]), rep(i-height,2), lty=2, lwd=1, col=onsetFill[j])
                        }
                    }
                }
            } else {
                lines(c(min(dates)-(0.5/365), max(dates)+(0.5/365)), rep(i-0.1, 2), col=mPal(sampleCol), lwd=1)
            }
            
        }
    }
    
    axis(4, at=(1:n)+0.1, labels = names, lwd=NA, las=1)
    mtext(side=3, plotLabel, line=0, at=xlim[1]-0.12*(xrange), cex=1.5)
    
    if (plotSymptomOnset) {
        legend("topleft", horiz=FALSE, pch=c(1,1,8,8), bty='n', pt.cex=1, , text.col="#00000000",
               legend = c("Sampled in Guangdong", "Sampled outside Guangdong", "Infected in Guangdong", "Infected outside Guangdong"),
               col=c(mPal(sampleCol), mPal(oxCols$gray6)), xpd=TRUE)
        legend("topleft", horiz=FALSE, pch=16, bty='n', pt.cex=1, 
               legend = c("Sampled in Guangdong", "Sampled outside Guangdong", "Infected in Guangdong", "Infected outside Guangdong"),
               col=c(mPal(sampleCol, 0.5), mPal(oxCols$gray6, 0.5), NA, NA), xpd=TRUE)
    } else {
        legend("topleft", horiz=FALSE, pch=16, bty='n', pt.cex=c(1,1,1.5,1.5),
               legend = c("Sampled in Guangdong", "Sampled outside Guangdong"), text.col="#00000000",
               col=c(mPal(sampleCol, 0.5), mPal(oxCols$gray6, 0.5)), xpd=TRUE)
        legend("topleft", horiz=FALSE, pch=1, bty='n', pt.cex=c(1,1,1.5,1.5),
             legend = c("Sampled in Guangdong", "Sampled outside Guangdong"),
             col=c(mPal(sampleCol), mPal(oxCols$gray6)), xpd=TRUE)
    }

}


plotTaxonSetComparison <- function(ages1, ages2, names=NULL, 
                                   dateRange=c(as.Date("2019-11-01"),as.Date("2020-03-01")), 
                                   maxwidth=0.5, bw='sj', col1=oxCols$blue1, col2=dark$orange,
                                   legend=c("Prior", "Posterior"), ...) {
  
      n      <- ncol(ages1)
      ylim   <- lubridate::decimal_date(dateRange)
      yrange <- diff(ylim)
      
      plot(1, type='n', bty='n', xlim=c(0,n+1), ylim=ylim, #xlim=c(xlim[1]-0.04*xrange, xlim[2]+0.04*xrange), 
           axes=FALSE, xlab='', yaxs='i', xaxs='i', ...)
      
      #dateRange  <- as.Date(lubridate::floor_date(lubridate::date_decimal(xlim), unit = 'day'))
      bigTicks   <- seq.Date(dateRange[1], dateRange[2], by='weeks')
      smallTicks <- seq.Date(dateRange[1], dateRange[2], by='days')
      axis(2, at=lubridate::decimal_date(bigTicks), labels=format.Date(bigTicks, format="%b %d"), lwd=0, lwd.ticks=1, las=1)
      axis(2, at=lubridate::decimal_date(smallTicks), labels=NA, tcl=-0.25)
      
      
      abline(v = 1:n, lwd=0.5, lty=3)
      #segments(x0=rep(xlim[1],n), x1=rep(xlim[2],n), y0=1:n, y1=1:n, lwd=0.5, lty=3)
      #abline(v = seq(xlim[1],xlim[2],by=xtickevery), lwd=0.25, lty=3)
      
      
      
      HPDBeanPlot(ages1,  side='left',  fill=mPal(col1, 0.5), border=mPal(col1), medcol=mPal(oxCols$red2), medwidth=0.25, 
                  bw=bw, maxwidth=maxwidth, add=TRUE, axes=FALSE, lwd=c(1,2,1))
      HPDBeanPlot(ages2,  side='right',  fill=mPal(col2, 0.5), border=mPal(col2), medcol=mPal(oxCols$red2), medwidth=0.25, 
                  bw=bw, maxwidth=maxwidth, add=TRUE, axes=FALSE, lwd=c(1,2,1))
      
      
      #axis(1, at=(1:n), labels = names, lwd=NA, las=1)
      print(names)
      text(x=1:n, y=ylim[1]-0.1*yrange, names, srt=45, xpd=TRUE)
      
      legend("top", horiz=TRUE, inset=c(0,-0.1), bty='n', fill=c(mPal(col1,0.5), mPal(col2, 0.5)), border=c(mPal(col1), mPal(col2)), legend=legend, xpd=TRUE, cex=1)
  
}