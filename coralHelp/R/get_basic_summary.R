get_basic_summary <-
function(value = growth, group = site, data = coral_raw){
  data_summary1 <- do.call(data.frame,
                           aggregate(value ~ group,
                                     data,
                                     FUN = function(x) c(mean(x), sum(x), sd(x))))
  colnames(data_summary1) <- c("group", "mean", "sum", "sd")
  return(data_summary1)
}
