
library(devtools)
setwd("/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp")
getwd()
check()

#' A dataset containing coral data based off data collected from multiple sources
#' @format A tibble with 100 rows and 9 columns
#' \describe{
#' \item{species}{Character values. Species of coral based around Oahu, Hawai'i}
#' \item{site}{Character values. Represent restoration sites or monitoring sites}
#' \item{initial.health}{Integer values. Represent the cm of coral cover at time prior to restoration}
#' \item{initial.size}{Double class. Represent the initial diameter of the coral at time prior to restoration}
#' \item{health.status}{Character values. The health status of the coral: alive, bleached, or dead}
#' \item{growth}{Double class. The growth in cm of the coral since time of restoration}
#' \item{temp}{Double class. The average temperature of the seawater around the coral}
#' \item{depth}{Double class. The depth that the coral is located at}
#' \item{outplant}{Character. The day/month/year that the coral was restored and/or outplanted}
#' }
#' @source randomly generated data based off real variables
"coral_data"

document()


#' Data wrangling Function 1: Fixing column names
#'
#' This function renames the columns in the data set in a way that makes sense and allows the other functions to work unhindered.
#'
#' Renames columns to "site", "species", "initial_health", "initial_size", "health_status", "growth", "temp", "depth", "outplant_date"
#' If data is set up in the above order, only the dataset name needs to be input for the function to work. i.e. fix_column_names(coral_data)
#'
#' @param userdata A dataset in the order: site, species, initial_health, initial_size, health_status, growth, temp, depth, outplant_date
#' @return renamed columns in a tibble
#' @import tidyverse
#' @export
#' @example
#' fix_column_names(coral_data)
fix_column_names <- function(userdata){
  colnames(userdata) <- c("site", "species", "initial_health", "initial_size", "health_status", "growth", "temp", "depth", "outplant_date")
  mydata <- userdata
  mydata
}
dump("fix_column_names", file = "/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp/R/fix_column_names.R")



#' Data wrangling Function 2:cleaning the data
#'
#' This function selects columns that would be useful for general restoration information,
#' corrects date column to a standard format using as.POSIXct,
#' and groups the data by site and species
#'
#' The columns selected are so the other functions will work smoothly and to make the data
#' easy to manipulate for further tests by changing dates to a standard format,
#' making a subset of the data so that it can be manipulated, and grouping by site
#' and species for easy viewing
#'
#'@param data A dataset
#'@param site A character. Site name of restoration, monitoring area. Will be used to group by.
#'@param species A character. A species of coral. Will be used to group by.
#'@param initial_health A variable. The percent cover found prior to restoration.
#'@param initial_size A variable. The size of coral prior to restoration.
#'@param outplant_date A character. Date of outplanting. Will be converted to standard date format. Cannot contain NAs.
#'@param growth A variable. Represents cm of growth since restoration.
#'@param temp A variable. Represents temperature at coral location.
#'@param depth A variable. Represents depth at coral location.
#'@import tidyverse
#'@export
#'@examples
#'organize_data(coral_data)
#'organize_data(coral_data, initial_health = NULL)
organize_data <- function(data, site, species, initial_health, initial_size, outplant_date, growth, temp, depth){
  data %>%
    select(site, species, initial_health, initial_size, health_status, outplant_date, growth, temp, depth) %>%
    group_by(site, species) %>%
    mutate(outplant = as.POSIXct(outplant_date,tryFormats = c("%m/%d/%Y",
                                                              "%Y/%m/%d",
                                                              "%Y-%m-%d",
                                                              "%m-%d-%Y")), outplant_date = NULL)
}
dump("organize_data", file = "/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp/R/organize_data.R")



#' Data wrangling Function 3: Gives basic summary statistics of a group within a specified data frame.
#'
#' This function calculates the mean, sum, and standard deviation of any specified group within a data frame.
#'
#' get_basic_summary uses three different (value, group, data) arguments to calculate the mean, sum, and standard deviation across a group variable in a dataframe.
#' Dataframe being utilized must be specified as an argument. Based on common coral analysis, the value argument has a default of "growth" and the group argument has a default of "site".
#' It returns a data frame with column titles "Group", "Mean", "Sum", and "SD" allowing comparison across a specified variable.
#'
#' @param data A dataframe, cannot contain NA's.
#' @param value A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param growth A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @import dplyr, tidyverse, tidyr
#' @export
#' @examples
#' get_basic_summary(value = growth, group = health_status, data = coral_raw)
#' get_basic_summary(value = temp, group = site, data = coral_raw)
get_basic_summary <- function(value = growth, group = site, data = coral_raw){
  data_summary1 <- do.call(data.frame,
                           aggregate(value ~ group,
                                     data,
                                     FUN = function(x) c(mean(x), sum(x), sd(x))))
  colnames(data_summary1) <- c("group", "mean", "sum", "sd")
  return(data_summary1)
}
dump("get_basic_summary", file = "/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp/R/get_basic_summary.R")

#' Data wrangling Function 4: Site summary data
#'
#' This function aggregates the site data and provides percentages of health status by species
#'
#' This function aggregates the users data, growth, species, depth, health_status, and site info
#' and provides percentages of health info based on the health_status of each species. It is hoped that
#' this function will provide the first step towards visualizing site and species data.
#'
#'@param mydata A dataset
#'@param growth A variable. Represents cm of growth since restoration.
#'@param species A character. A species of coral. Will be used to group by.
#'@param depth A variable. Represents depth at coral location. Will be used to aggregate data.
#'@param health_status A character. Should be 1 of three options: "alive", "bleached", "dead". Cannot contain NAs.Will be used to aggregate data
#'@param site A character. Name of site that monitoring/restoration is occurring. Will be used to aggregate data and group by.
#'@import tidyverse
#'@export
#'@examples
#'site_info(coral_data)
site_info <- function(mydata, growth, species, depth, health_status, site){
  site_agg <- aggregate(growth ~ species + depth + health_status + site, data = mydata, mean)
  site_count <- site_agg %>%
    group_by(site, species) %>%
    count(health_status)
  site_percentage <- site_count %>%
    mutate(percentage = round(n/sum(n) * 100) )
  site_percentage
}
dump("site_info", file = "/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp/R/site_info.R")



#' Modeling Function 1: Gives summary of p-values across three t-tests.
#'
#' This function calculates and reports the p-values of 3 t-tests.
#'
#' get_t_tests uses three different (variable1, by1, data) arguments per t-test to calculate the p-values of three students t-tests.
#' Dataframe being utilized must be specified as an argument. There are no default values for any arguments.
#' It returns a data frame with column titles "t_test" and "p_values" allowing comparison across three t-tests.
#'
#' @param data A dataframe, cannot contain NA's.
#' @param variable1 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param by1 A variable (often denoted as a column in ecological data sets), must contain exactly 2 levels, cannot contain NA's.
#' @param variable2 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param by2 A variable (often denoted as a column in ecological data sets), must contain exactly 2 levels, cannot contain NA's.
#' @param variable3 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param by3 A variable (often denoted as a column in ecological data sets), must contain exactly 2 levels, cannot contain NA's.
#' @import dplyr, tidyverse, tidyr
#' @export
#' @examples
#' get_t_tests(data = coral_binary, variable1 = "growth", by1 = "health_status", variable2 = "temp", by2 = "health_status", variable3 = "depth", by3 = "health_status")
#' get_t_tests(data = coral_binary, variable1 = "growth", by1 = "initial_health", variable2 = "initial_size", by2 = "health_status", variable3 = "temp", by3 = "initial_health")
get_t_tests <- function(data, variable1, by1, variable2, by2, variable3, by3, ...) {
  t1 <- t.test(data[[variable1]] ~ as.factor(data[[by1]]))$p.value
  t2 <- t.test(data[[variable2]] ~ as.factor(data[[by2]]))$p.value
  t3 <- t.test(data[[variable3]] ~ as.factor(data[[by3]]))$p.value
  p_values <- c(t1, t2, t3)
  t_test <- c("t_test1", "t_test2", "t_test3")
  data_summary <- data.frame(t_test, p_values)
  return(data_summary)
}
dump("get_t_tests", file = "/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp/R/get_t_tests.R")

#' Visualization Function 1: Plot four ggplot graphs
#'
#' this function plots two boxplots and two scatter plots using different x and y variables.
#'
#' xplore_graph uses three different (x,y,z) variables to plot the relationship between the environmental factors and focal species.
#' It returns four separate graphs in a 2 by 2 matrix that allows for side by side comparison.
#'
#' @param x A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param y A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param z A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @return graphs of \code{x}, \code{y}, \code{z} in 2x2 plot matrix
#' @import dplyr, ggplot2, cowplot, tidyverse, tidyr
#' @export
#' @examples
#' xplore_graph(data_set, species(x), depth, health)
#' xplore_graph(data_set, site, coral_colony_size, disease_presence)
xplore_graph <- function(df, x, y, z){
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
dump("xplore_graph", file = "/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp/R/xplore_graph.R")

#'Visualization Function 2: plot four ggplot boxplot graphs
#'
#' this function plots four boxplots using four different x and y variables.
#'
#' get_boxplots uses three different (x1, y1, title1) arguments per plot, to plot the relationship between two defined variables and to create a custom title.
#' Dataframe being utilized must be specified as an argument. Titles have default values of 'Boxplot1 - Boxplot4".
#' It returns four boxplots graphs in a 2 by 2 matrix that allows for side by side comparison using cowplot plot_grid() function.
#'
#' @param data A dataframe, cannot contain NA's.
#' @param x1 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param y1 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param title1 Characters to describe plot 1.
#' @param x2 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param y2 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param title2 Characters to describe plot 2.
#' @param x3 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param y3 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param title3 Characters to describe plot 3.
#' @param x4 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param y4 A variable (often denoted as a column in ecological data sets), cannot contain NA's.
#' @param title4 Characters to describe plot 4.
#' @return graphs of \code{x1, y1, title1}, \code{x2, y2, title2}, \code{x3, y3, title3}, \code{x4, y4, title4} in 2x2 plot matrix
#' @import dplyr, ggplot2, cowplot, tidyverse, tidyr
#' @export
#' @examples
#' get_boxplots(data = coral_raw, x1= species, y1= growth, x2 = species, y2 = initial_size, x3 = site, y3 = growth, x4 = site, y4 = temp, title4 = "Site vs Temp")
#' get_boxplots(data = coral_raw, x1= site, y1= temp, x2 = species, y2 = initial_health, x3 = site, y3 = growth, x4 = health_status, y4 = initial_size)
get_boxplots <- function(data, x1, y1, title1 = "Boxplot1", x2, y2, title2 = "Boxplot2",
                         x3, y3, title3 = "Boxplot3", x4, y4, title4 = "Boxplot4"){
  panel_a <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x1 }}, y = {{ y1 }}, fill = {{ x1 }}))+
    geom_point(aes(x = {{ x1 }}, y = {{ y1 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title1)


  panel_b <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x2 }}, y = {{ y2 }}, fill = {{ x2 }}))+
    geom_point(aes(x = {{ x2 }}, y = {{ y2 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title2)

  panel_c <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x3 }}, y = {{ y3 }}, fill = {{ x3 }}))+
    geom_point(aes(x = {{ x3 }}, y = {{ y3 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title3)

  panel_d <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x4 }}, y = {{ y4 }}, fill = {{ x4 }}))+
    geom_point(aes(x = {{ x4 }}, y = {{ y4 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title4)

  combine <- plot_grid(panel_a, panel_b, panel_c, panel_d)

  return(combine)
}

dump("get_boxplots", file = "/Users/kyleighkuball/Desktop/MBIO612Package/coralHelp/R/get_boxplots.R" )

use_mit_license()

check()

use_package("ggplot2")
use_package("dplyr")

options(devtools.desc.author = c("'Kyleigh Kuball <KyleighJ40@gmail.com> [aut, cre]'",
                               "'Aimee Cook McNab <acookmc@hawaii.edu> [aut]'",
                               "'Sophia Rahnke <srahnke@hawaii.edu> [aut]'"))
options("devtools.desc.author")


devtools::load_all()
devtools::document()
build()






