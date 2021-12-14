get_t_tests <-
function(data, variable1, by1, variable2, by2, variable3, by3, ...) {
  t1 <- t.test(data[[variable1]] ~ as.factor(data[[by1]]))$p.value
  t2 <- t.test(data[[variable2]] ~ as.factor(data[[by2]]))$p.value
  t3 <- t.test(data[[variable3]] ~ as.factor(data[[by3]]))$p.value
  p_values <- c(t1, t2, t3)
  t_test <- c("t_test1", "t_test2", "t_test3")
  data_summary <- data.frame(t_test, p_values)
  return(data_summary)
}
