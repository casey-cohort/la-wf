t <- tempfile(fileext = '.json')
download.file('https://apps.npr.org/datawrapper/xse37/13/dataset.json', t)

datawrapper_data <- fromJSON(t)[[1]]

z <- datawrapper_data %>% 
  filter(properties$fill %in% c('#d8472b')) %>% # , "#e38d2c")) %>% 
  filter(type == 'area') %>% 
  as_tibble() 

mandatory <- tibble(
  zone = c('Palisades', 'Hurst', 'Eaton'),
  type = 'Mandatory',
  geometry = list(
    z$feature$features[[1]]$geometry$coordinates[[1]][1, ,] %>% list() %>% st_polygon(),#mandatory zone palisades
    z$feature$features[[1]]$geometry$coordinates[[2]][1, ,] %>% list() %>% st_polygon(),#mandatory zone hurst
    z$feature$features[[1]]$geometry$coordinates[[3]] %>% st_polygon() #mandatory zone eaton
  )
) %>% 
  st_as_sf(crs = 4326) 


z <- datawrapper_data %>% 
  filter(properties$fill %in% c('#e38d2c')) %>% # , "#e38d2c")) %>% 
  filter(type == 'area') %>% 
  as_tibble() 

warning <- tibble(
  zone = c(rep('Palisades', 4), rep('Hurst', 2), rep('Eaton', 4)),
  type = 'Warning',
  geometry = list(
    z$feature$features[[1]]$geometry$coordinates[[1]][1, ,] %>% list() %>% st_polygon(), # palisades warning zone
    z$feature$features[[1]]$geometry$coordinates[[2]][1, ,] %>% list() %>% st_polygon(), # palisades warning zone
    z$feature$features[[1]]$geometry$coordinates[[3]][1, ,] %>% list() %>% st_polygon(), # palisades warning zone
    z$feature$features[[1]]$geometry$coordinates[[4]][1, ,] %>% list() %>% st_polygon(), # palisades warning zone
    z$feature$features[[1]]$geometry$coordinates[[5]][1, ,] %>% list() %>% st_polygon(), # hurst warning zone
    z$feature$features[[1]]$geometry$coordinates[[6]] %>% st_polygon(), # hurst warning zone
    z$feature$features[[1]]$geometry$coordinates[[7]][1, ,] %>% list() %>% st_polygon(), # eaton warning zone
    z$feature$features[[1]]$geometry$coordinates[[8]] %>% st_polygon(), # eaton warning zone
    z$feature$features[[1]]$geometry$coordinates[[9]][1, ,] %>% list() %>% st_polygon(), # eaton warning zone
    z$feature$features[[1]]$geometry$coordinates[[10]][1, ,] %>% list() %>% st_polygon() # eaton warning zone
  )
) %>% 
  st_as_sf(crs = 4326) 


dat <- bind_rows(mandatory, warning) 
mapview(dat, zcol = 'type')

write_sf(dat, 'jan_8_boundaries.geojson')