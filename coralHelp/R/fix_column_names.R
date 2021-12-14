fix_column_names <-
function(userdata){
  colnames(userdata) <- c("site", "species", "initial_health", "initial_size", "health_status", "growth", "temp", "depth", "outplant_date")
  mydata <- userdata
  mydata
}
