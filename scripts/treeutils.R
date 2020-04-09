require(treeio)
require(ggplot2)
require(ggtree)
require(phytools)
require(lubridate)


#' Select an element from a string of tuples separated by `sep`
#' 
#' @param id The string of tuples
#' @param group The index of the tuple to select (use -1 for the last element)
#' @param sep The separator between tuples
#' 
selectFromSeq <- function(id, group, sep="\\|") {
  if (group > 0) {
    return(strsplit(id,split=sep)[[1]][group]) 
  } else {
    parts <- strsplit(id,split=sep)[[1]]
    return(parts[length(parts)])
  }
}

#' Get parts of a list of ordered tuples separated by `sep`
#' 
#' @param labels The list of tuples as strings
#' @param group The element in the tuples to return (use -1 for the last element)
#' @param sep The separator between tuples
#' 
getSeqParts <- function(labels, group=1, sep="\\|") {
  return(sapply(labels, selectFromSeq, group=group, sep=sep))
}



# Process sequence labels into a data frame with metadata
getMetadata <- function(ids, sep="\\|", fmt="%Y-%m-%d") {
  
  name      <- getSeqParts(ids, 1, sep=sep)
  accession <- getSeqParts(ids, 2, sep=sep)
  country   <- getSeqParts(ids, 3, sep=sep)
  province  <- getSeqParts(ids, 4, sep=sep)
  city      <- getSeqParts(ids, 5, sep=sep)
  date      <- as.Date(getSeqParts(ids, 6, sep=sep), format=fmt) 
  
  return(data.frame(taxon=ids, accession=accession, name=name, country=country, province=province, city=city, date=date, row.names = 1:length(ids)))
}



#' Return ggtree object that can be plotted/saved
#' 
#' @param tree          A ggtree object
#' @param metadata      A data frame with metadata for sequences. First column should labeled taxon and contain the taxa in the tree
#' @param size          Line width
#' @param plotIds       Plot sequence labels
#' @param plotNodes     Plot internal node labels
#' @param plotScale     Plot a scale bar
#' @param plotSupport   Plot posterior support on nodes of BEAST trees. No support for plotting bootstrap support at this stage.
#' @param plotCountry   Colour tips by country
#' @param plotTipPoints Plot points on tips
#' @param plotLegend    Add a legend (if there is anything to add)
#' @param plotAxis      If time tree, add an axis from plotStart to plotEnd, with ticks every xtickBy. 
#'                      Otherwise, add an axis with ticks at mutations from the root.
#' @param alignIds      Align sequence labels
#' @param scaleWidth    Length of the scale bar (in s/s)
#' @param nodepal       Palette to use for country colours
#' @param cols List of colours used, in order (foreground/tree, labels, highlights)
#' @param highlighted_taxa Sequence labels to highlight
#' @param highlighted_tips Tips to highlight (with points)
#' @param legendPosition   Where should the legend be
#' @param seqlen           Length of sequences in the alignment. Used to draw axis for non-timetrees
#' @param timeTree         Usually true for BEAST trees
#' @param plotStart, plotEnd, xtickBy Defines the axis for timetrees
#' 
#' 
getTreePlot <- function(tree, metadata, size=0.5, 
                        plotIds=TRUE, plotNodes=FALSE, plotNodeBars=FALSE, plotScale=FALSE, plotSupport=TRUE, 
                        plotCountry=TRUE, plotTipPoints=FALSE, plotLegend=TRUE, plotAxis=TRUE,
                        alignIds=FALSE, scaleWidth=0.005, nodepal=scale_color_aaas(),
                        #cols=c("#002147", "#69913B", "#872434", "#4891DC"), 
                        cols=c(mPal(oxCols$gray6), mPal(oxCols$black), mPal(oxCols$red2), mPal(oxCols$blue1)),
                        highlighted_taxa=c(), highlighted_tips=c(), nodeBars="height_0.95_HPD",
                        legendPosition=c(0.05,0.75), angle=0,
                        seqlen=1000,
                        timeTree=TRUE, plotStart=2009, plotEnd=2019, xtickBy=0.5, ...) {
  
  
  # Base tree
  p <- ggtree(tree, ladderize=TRUE, size=size, color=cols[1], ...) %<+% metadata
  
  # Plot location
  if (plotCountry) {
    p <- p + geom_tippoint(aes(color=location), size=1) + nodePal
  }
  
  # Plot tip points (either all tips, or just a highlighted subset)
  if (plotTipPoints || length(highlighted_tips) > 0) {
    #p <- p + geom_tippoint(aes(subset=chinese), color=cols[3], size=0.5)
    if (length(highlighted_tips) > 0) {
        p <- p + geom_tippoint(aes(subset=(label %in% highlighted_tips)), color=cols[3], size=0.5)
    } else {
        p <- p + geom_tippoint(color=cols[1], size=0.5)
    }
  }
  
  # Add posterior support on nodes
  if (plotSupport) {
      if (timeTree) {
          p <- p + geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.8), color=cols[1], size=1.5) + 
                   geom_text2(aes(subset=(!is.na(posterior) & posterior >= 0.8), label=posterior), vjust=-0.5, hjust=1.25, size=1.75, color=cols[2]) 
            
      } else {
          # Should adjust this for bootstrap, but later, when we actually use bootstrapped trees...
          #p <- p + geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.75 & posterior < 0.95), color=cols[1], size=0.5) + 
          #        #geom_text2(aes(subset=(!is.na(posterior) & posterior >= 0.75 & posterior < 0.95), label=posterior), vjust=-0.5, hjust=1.25, size=1, color=cols[1]) +
          #         geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.95), color=cols[3], size=0.5) + 
          #         geom_text2(aes(subset=(!is.na(posterior) & posterior >= 0.95), label=posterior), vjust=-0.5, hjust=1.25, size=1, color=cols[3]) 
      }
        
  }
  
  # Add sequence ids on tips
  if (plotIds) {
    p <- p + geom_tiplab(aes(subset=!(label %in% highlighted_taxa)), size=1.75, hjust=-0.05, align=alignIds, color=cols[2]) 
    p <- p + geom_tiplab(aes(subset=(label %in% highlighted_taxa)),  size=1.75, hjust=-0.05, align=alignIds, color=cols[3]) #, fontface="bold") 
  }
  
  
  # Add internal node labels (useful for finding clades and debugging)
  if (plotNodes) {
    p <- p + geom_text2(aes(label=node), vjust=1.5, hjust=1.25, size=2, color=cols[2]) 
  }
  
  if (plotNodeBars) {
    p <- p + geom_range(range=nodeBars, color=cols[4], alpha=0.5, size=1, center='height') 
  }
  
  
  # Add legend
  if (plotLegend) {
    p <- p + theme(legend.direction = "vertical", legend.position = legendPosition, 
                   legend.justification = c("left","center"), 
                   legend.text = element_text(size = 10), legend.title = element_text(size=10))
  }
  
  
  # Add scale bar
  if (plotScale) {
    p <- p + geom_treescale(y=-0.02*tree@phylo$Nnode, fontsize=2.5, linesize=0.25, offset=-0.02*tree@phylo$Nnode, width=scaleWidth) 
  }
  
  # Add axis
  if (plotAxis) {
    p <- p + theme_tree2(panel.grid.major = element_line(colour = mPal(oxCols$gray3), size=0.25, linetype = "dotted"), 
                         axis.line.x=element_line(size=0.25), axis.ticks.x=element_line(size=0.25),
                         legend.direction = "vertical",  legend.position = c(1,0.05),
                         legend.justification = c("right","bottom"), legend.background = element_blank() ,
                         legend.text = element_text(size = 10), legend.title = element_blank()) 
    
    if (timeTree) {
        mrca           <- max(getBranchingTimes(tree@phylo))
        mostrecent     <- decimal_date(max(metadata$date))
        
        #plotLimits=c(-(mrca*0.05), mrca*1.05)
        #if (plotNodeBars) {
        #    plotLimits[1] <- 0.5*(mrca - max(range(tree@data[[nodeBars]], na.rm=TRUE)))
        #
        #}
        #if (plotIds) {
        #    plotLimits[2] <- plotLimits[2]*1.5
        #}
        #if (plotIds) {
        #    plotLimits <- c(-(mrca*1.05), mrca*2)
        #} else {
        #    plotLimits=c(-(mrca*0.05), mrca*1.05)
        #}
        
        plotLimits  <- mrca - mostrecent + lubridate::decimal_date(c(plotStart, plotEnd))
        xticks      <- seq.Date(plotStart, plotEnd, by=xtickBy)
        xticklabels <- format.Date(xticks, format="%b %d")
        
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6)) + 
                 scale_y_continuous(breaks=NULL, expand=c(0,1)) + 
                 scale_x_continuous(breaks = (mrca - mostrecent + lubridate::decimal_date(xticks)), 
                                    #labels=function(x) x - mrca + mostrecent, 
                                    labels=xticklabels,  
                                    limits=plotLimits, 
                                    expand=c(0.1,0)) 
    } else {
        mrca  <- max(getBranchingTimes(tree@phylo))
        if (plotIds) {
            mutations <- 0:(1.25*ceiling(mrca*seqlen))
        } else {
            mutations <- 0:ceiling(mrca*seqlen)
        }
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6)) + 
                 scale_y_continuous(breaks=NULL, expand=c(0,1)) + 
                 scale_x_continuous(breaks = mutations/seqlen, 
                                    labels=paste("        ", mutations), 
                                    limits=c(0, max(mutations)/seqlen),
                                    expand=c(0.1,0))
    }
  }
  
  return(p)
}



#' Adds an extra column to the data table for the tree containing information that can be CORRECTLY added to the
#' plot as node bars using geom_range. 
#' 
#' By default, ggtree::geom_range() does not add the correct bars for the height_range and height_0.95_HPD. In 
#' order to get the right upper and lower limits the values in the data table need to be rescaled. 
#'
#' @param tree treedata structure (ggtree structure). This has to be a beast tree.
addNodeHeights <- function(tree, nodes=NULL, inputrange="", center="", outputrange="") {
  
  n <- nrow(tree@data)
  tree@data$height_0.95_HPD_rescaled <- sapply(1:n, function(x) 2*tree@data$height[x] - tree@data$height_0.95_HPD[[x]])

  return(tree)
}


# Select all sequence labels matching a cluster name using cluster lookup table
getClusterIds <- function(clusterName, clusters) as.character(clusters$sequence_label[clusters$cluster == clusterName, drop=TRUE])

# Label clusters
annotateClusters <- function(treeplot, clusternodes, hilightCluster=TRUE, labelCluster=TRUE, alpha=0.4, cols=mPal(oxCols$red2), ...) {
  
  if (length(cols) < length(clusternodes)) {
    cols <- rep(cols, length(clusternodes))
  }
  
  for (i in 1:length(clusternodes)) {
    clustername <- ifelse(is.null(names(clusternodes)), paste("Cluster",i), names(clusternodes)[i])
    
    if (hilightCluster) {
      treeplot <- treeplot + geom_hilight(node=clusternodes[i], fill=cols[i], alpha=alpha)
    }
    
    if (labelCluster) {
        treeplot <- treeplot + geom_cladelabel(node=clusternodes[i], label=clustername, color=cols[i], ...)
    }
  }
  
  return(treeplot)
  
}

#' Get a subset of an MCC tree around a cluster, plot that subset and highlight the cluster
#'
#' @param clusterName   Name of cluster in the cluster lookup table
#' @param clusters      Cluster lookup table (data.frame) with cluster names in column XXX and sequence labels in column XXX (can contain other info too)
#' @param tree          A ggtree object (a phylo object may work with some tweaks)
#' @param metadata      Metadata for tree taxa in a data frame. Must contain a column with name "taxon" and the exact taxa in the tree and a column with name "date" containing Date objects.
#' @param levels_back   The number of levels to go up in the tree past the cluster's root. If negative just plot the cluster in the whole tree
#' @param showParent    Highlight the cluster and iits parent cluster (one level up)
#' @param ...           Options to pass to getTreePlot
#' 
getClusterTreePlot <- function(clusterName, clusters, tree, metadata, shadecol="#69913B",
                               levels_back=-1, shadeClade=FALSE, labelCluster=FALSE, ...) {
  
  clusterIds  <- getClusterIds(clusterName, clusters)
  clusterMRCA <- getMRCA(tree@phylo, clusterIds)
  
  if (levels_back >= 0) {
    clusterTree <- treeio::tree_subset(tree, clusterMRCA, levels_back=levels_back)
  } else {
    clusterTree <- tree
  }
  
  clusterMRCANew  <- getMRCA(clusterTree@phylo, clusterIds)
  parent          <- clusterTree@phylo$edge[clusterTree@phylo$edge[,2] == clusterMRCANew,1]
  clusterTreeMeta <- metadata[match(clusterTree@phylo$tip.label, metadata$taxon), ] 
  
  p <- getTreePlot(clusterTree, clusterTreeMeta, plotCountry=FALSE, highlighted_tips=clusterIds, ...) 
  
  if (shadeClade) {
    names(clusterMRCANew) <- clusterName
    p <- annotateClusters(p, clusterMRCANew, labelCluster=labelCluster, alpha=0.25, cols=shadecol) 
  }
  
  return(p)
}

