binarydist <- function(cross_table, method = c('Simple', 'phi', 'F1score', 'Sensitivity', 'Specificity', 'Precision') ){
  if(!("0" %in% rownames(cross_table))){
    if(!("0" %in% colnames(cross_table))){
      cross_table <- rbind(0, cross_table)
    }
    else {
      cross_table <- rbind(c(0,0), cross_table)
    }
  }
  if(!("1" %in% rownames(cross_table))){
    if(!("0" %in% colnames(cross_table))){
      cross_table <- rbind(cross_table, 0)
    }
    else {
      cross_table <- rbind(cross_table, c(0,0))
    }
  }

  a <- as.double(cross_table[2,2]) # True positives
  b <- as.double(cross_table[2,1]) # False positive
  c <- as.double(cross_table[1,2]) # False negative
  d <- as.double(cross_table[1,1]) # True negative

  labels <- c()
  calculation <- c()
  for(type in method) {
    if(type == 'Simple'){ # aka accuracy
      calculation <- c(calculation, (a+d)/(a+b+c+d))
      labels <- c(labels, 'Simple')
    }
    else if(type == 'phi'){ # aka Matthews correlation coefficient (MCC), or phi coefficient pearson
      calculation <- c(calculation, ((a+d)-(b+c))/sqrt((a+b)*(a+c)*(b+d)*(c+d)))
      labels <- c(labels, 'phi')
    }
    else if(type == 'F1score'){
      calculation <- c(calculation, (a)/(a+0.5*(b+c)))
      labels <- c(labels, 'F1score')
    }
    else if(type == 'Sensitivity'){ # correctly positive out of those that is in truth positive (aka recall)
      calculation <- c(calculation, (a)/(a+c))
      labels <- c(labels, 'Sensitivity')
    }
    else if(type == 'Specificity'){ # correctly negative out of those that is in truth negative
      calculation <- c(calculation, (d)/(b+d))
      labels <- c(labels, 'Specificity')
    }
    else if(type == 'Precision'){ # correctly positive out of those that is estimated positive
      calculation <- c(calculation, (a)/(a+b))
      labels <- c(labels, 'Precision')
    }
  }
  results <- matrix(calculation, nrow=1)
  colnames(results) <- labels
  return(results)
}


abs_diff = function(est_network, obs_network) {
  return(sum(abs(as.numeric(est_network)-
                   as.numeric(obs_network))/
               (dim(obs_network)[1]*(dim(obs_network)[1]-1))))
}

predBar <- function(posterior_results){
  posterior_counts = apply(posterior_results$Networks, 3, table)
  merged_counts <- merge(table(posterior_results$observed_net), posterior_counts[[1]], by=1, all=TRUE)
  for(i in 2:length(posterior_counts)) {
    suppressWarnings(merged_counts <- merge(merged_counts, posterior_counts[[i]], by=1,all=TRUE))
  }
  merged_counts= merged_counts[order(as.numeric(levels(merged_counts$Var1))),]
  merged_counts = t(merged_counts)
  colnames(merged_counts) = merged_counts[1,]
  # merged_counts <- merged_counts[-1,1:10]
  merged_counts = log(apply(merged_counts, 2, as.numeric))
  full_merged_counts <- merged_counts[-1,]
  ifelse(dim(merged_counts)[2] > 20, merged_counts <- merged_counts[-1,1:10], merged_counts <- merged_counts[-1,])
  vioplot(merged_counts[-1,], drawRect = FALSE, border = NA,
          ylab = "Log requency", xlab = "Observed counts",
          ylim=c(min(merged_counts, na.rm = TRUE),
                 max(merged_counts, na.rm = TRUE)+2))
  points(merged_counts[1,], pch=4, lwd=2.5, col=rgb(red=1, green = 0, blue = 0, alpha=0.5))
  legend("topright", legend = c("Posterior predictive networks", "Observed network"),
         col = c("black", "red"), pch = c(20, 4), cex=0.8)
  return(full_merged_counts)
}
