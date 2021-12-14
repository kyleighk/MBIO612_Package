xplore_graph <-
function(df, x, y, z){
  obj1 <- ggplot(df, aes(x = x, y = y)) +
    geom_point()
  obj2 <- ggplot(df, aes(x = x, y = z)) +
    geom_point()
  obj3 <- ggplot(df, aes(x = z, y = x)) +
    geom_boxplot()
  obj4 <- ggplot(df, aes(x = y, y = x)) +
    geom_boxplot()
  return(plot_grid(obj1, obj2, obj3, obj4))
}
