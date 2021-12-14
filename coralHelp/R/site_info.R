site_info <-
function(mydata, growth, species, depth, health_status, site){
  site_agg <- aggregate(growth ~ species + depth + health_status + site, data = mydata, mean)
  site_count <- site_agg %>%
    group_by(site, species) %>%
    count(health_status)
  site_percentage <- site_count %>%
    mutate(percentage = round(n/sum(n) * 100) )
  site_percentage
}
