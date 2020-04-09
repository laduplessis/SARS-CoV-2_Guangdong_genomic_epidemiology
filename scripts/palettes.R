################################################################################################################################
# Palettes
################################################################################################################################
# Check out wesanderson package for more palettes
# Check out ggsci palettes: https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html

oxCols <- list(oxblue = c(  0, 33, 71),
               blue1  = c( 72,145,220),
               blue2  = c(158,206,235),
               blue3  = c( 68,104,125),
               blue4  = c( 95,155,175),
               blue5  = c(161,196,208),
               green1 = c(  0,119,112),
               green2 = c(123,162,150),
               green3 = c(188,210,195),
               green4 = c(105,145, 59),
               green5 = c(185,207,150),
               green6 = c(206,219,175),
               green7 = c(170,179,  0),
               green8 = c(219,222,114),
               green9 = c(227,229,151),
               orange1= c(207,122, 48),
               orange2= c(245,207, 71),
               orange3= c(243,222,116),
               red1   = c(135, 36, 52),
               red2   = c(190, 15, 52),
               red3   = c(235,196,203),
               purple = c( 94, 80,156),
               gray1  = c(224,222,217),
               gray3  = c(199,194,188),
               gray6  = c(167,157,150),
               silver = c(120,147,149),
               black  = c(  0,  0,  0),
               white  = c(255,255,255))

zikaCols <- list(a = c(158,21,69),
                 b = c(213,63,79),
                 c = c(94,80,156),
                 d = c(246,172,99),
                 e = c(53,135,189),
                 f = c(171,210,161),
                 g = c(236,109,69),
                 h = c(105,190,164))

dark  <- list(blue   = RColorBrewer::brewer.pal(12,"Paired")[2], 
              green  = RColorBrewer::brewer.pal(12,"Paired")[4], 
              red    = RColorBrewer::brewer.pal(12,"Paired")[6], 
              orange = RColorBrewer::brewer.pal(12,"Paired")[8], 
              purple = RColorBrewer::brewer.pal(12,"Paired")[10], 
              gray   = "#777777",
              black  = "#000000",
              white  = "#FFFFFF")

light <- list(blue   = RColorBrewer::brewer.pal(12,"Paired")[1], 
              green  = RColorBrewer::brewer.pal(12,"Paired")[3], 
              red    = RColorBrewer::brewer.pal(12,"Paired")[5], 
              orange = RColorBrewer::brewer.pal(12,"Paired")[7], 
              purple = RColorBrewer::brewer.pal(12,"Paired")[9], 
              gray   = "#777777",
              black  = "#000000",
              white  = "#FFFFFF")

################################################################################################################################

mPal <- function(c, alpha=1.0) {
  if (is.character(c) && substr(c,1,1) == "#") {
      return(paste0(c,format(as.hexmode(round(alpha*255)), width=2)))
  } else {
      return(rgb(red=c[1], green=c[2], blue=c[3], alpha=round(alpha*255), maxColorValue=255))
  }
}


plotPallete <- function(pal, alpha=1.0) {
  
  root <- sqrt(length(pal))
  layout(matrix(1:(round(root)*ceiling(root)), nrow=round(root)))
  
  par(mar=c(2,0,0,0))
  for (col in 1:length(pal)) {
      plot(1,type='n',xlim=c(0,1),ylim=c(0,1), axes=FALSE, ylab="", xlab="")
      rect(0,0,1,1,col=mPal(pal[[col]], alpha=alpha))
      if (is.null(names(pal)[col])) {
          mtext(col,line=0,side=1)
      } else {
          mtext(names(pal)[col],line=0,side=1)
      }
  }
}


