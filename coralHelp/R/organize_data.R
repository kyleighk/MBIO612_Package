organize_data <-
function(data, site, species, initial_health, initial_size, outplant_date, growth, temp, depth){
  data %>%
    select(site, species, initial_health, initial_size, health_status, outplant_date, growth, temp, depth) %>%
    group_by(site, species) %>%
    mutate(outplant = as.POSIXct(outplant_date,tryFormats = c("%m/%d/%Y",
                                                              "%Y/%m/%d",
                                                              "%Y-%m-%d",
                                                              "%m-%d-%Y")), outplant_date = NULL)
}
