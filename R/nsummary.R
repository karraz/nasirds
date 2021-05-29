
#' Creates histogram, boxplot, and numeric summary

#' @export
#' @param data numeric variable

nsummary <- function(data){
  #Setting display to 2 graph in 1 display
  par(mfrow=c(1,2))
  #Histogram
  hist(data, freq = F, main=NULL, col = rainbow(30), border="#e092fd")
  curve(dnorm(x,
              mean=mean(data),
              sd=sd(data)),
        add=TRUE, col="darkblue",
        lwd=2)

  #Boxplot
  boxplot(data,
          col="green", border = "#2471f5")
  #reset display graph
  par(mfrow=c(1,1))

  #Numerical Summary
  stats <- data.frame (
    Mean=mean(data, na.rm=T),
    Std_Dev = sd(data, na.rm=T),
    Minimum = min(data),
    Maximum = max(data),
    Range = max(data) - min(data),
    Quartile1 = quantile(data, probs=0.25),
    Median = median(data),
    Quartile3 = quantile(data, probs= 0.75),
    IQR = IQR(data))
  stats <- t(stats)
  stats
  colnames(stats) <- c("Numerical Statistics")
  print(stats)

}

