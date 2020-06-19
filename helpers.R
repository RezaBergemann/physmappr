importGBIF <- function(genuskey = 2264733){
  #API URL
  gbif_occ = 'https://api.gbif.org/v1/occurrence/search'
  
  #Page counter
  n = 0
  
  gbif = NULL
  
  #Loop through data, 300 results at a time
  while(TRUE){
    
    #Get data from API
    r <- GET(gbif_occ, query = list(genusKey = genuskey, offset = 300*n, limit = 300))
    #Transform raw data to tibble
    gbif_json = fromJSON(content(r,"text")) 
    gbif_raw = as_tibble(gbif_json$results)
    
    #Get desired information
    gbif_raw = gbif_raw %>% select(matches("year|month|day|decimalLongitude|decimalLatitude|references"))
    
    #Append
    gbif <- gbif %>% bind_rows(gbif_raw)
    #Break if on last page
    if(nrow(gbif_raw) < 300){break}
    n = n+1
  }
  #Select only relevant info/rename
  gbif %>% select(year, month, day, lat = decimalLatitude, lon = decimalLongitude, references)
}

importJellywatch <- function() {
  
  read_tsv('https://www.jellywatch.org/sightings/tsv?title=&field_description_value=&field_organism_value=Man%20o%20war&_format=csv') %>%
    select(Date, lat = Latitude, lon = Longitude, Title) %>%
    separate(Date, into = c("year", "month", "day"), convert = TRUE) %>%
    add_column(source = 'Jellywatch') %>%
    unite(references, source, Title)
}


importOBIS <- function(taxonId = 135382, resultSize = 5000) {
  obis_occ = 'http://api.obis.org/v3/occurrence'
  
  r <- GET(obis_occ, query = list(taxonid = taxonId, size = resultSize))
  #Transform raw data to tibble
  obis_json = fromJSON(content(r,"text")) 
  
  as_tibble(obis_json$results) %>% 
    select(year = date_year, month, day, lat = decimalLatitude, lon = decimalLongitude, dataset_id) %>%
    add_column(source = 'OBIS') %>%
    mutate_at(c("year", "month", "day"), as.integer) %>%
    unite(references, source, dataset_id)
  
}

importLit <- function(file_name = "lit_physalia_sightings.csv"){
  read_csv(file_name) %>%
    add_column(day = NA) %>%
    select(year = Year, month = Month, day, lat = Latitude, lon = Longitude, references = Publication)
  
}



importERSST <-function(year) {
  
  ersst_base = 'https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v5/netcdf/ersst.v5'
  sst = NULL
  
  #Loop through each month of the year
  for (month in 1:12){
    #Load and open data
    ersst_url = sprintf('%s.%4d%02d.nc', ersst_base, year, month)
    printf('Downloading month: %d', month)
    download.file(ersst_url, 'temp.nc')
    ersst = nc_open('temp.nc')
    lon = ncvar_get(ersst, "lon")
    lat = ncvar_get(ersst, "lat")
    
    #Reorganize longitude to be consistent with other sources
    for (i in 1:length(lon)){
      if (lon[i]>180) {
        lon[i] = -360 + lon[i]
      }
    }
    
    #name columns
    sst_ = ncvar_get(ersst, "sst")
    colnames(sst_) <- lat
    
    #Transform to tibble
    sst_ <- as_tibble(sst_)
    names = colnames(sst_)
    
    #Reshape and add extra data
    sst_ <- sst_ %>% 
      add_column(lon)%>% 
      pivot_longer(names, names_to = "lat", values_to = "temp")  %>%
      add_column(month) %>% 
      mutate(lat = as.numeric(lat))
    
    #Append 
    sst <- sst %>% bind_rows(sst_)
    
  }
  
  sst
  
  
}