library(countrycode)
library(httr)
library(jsonlite)
library(tidygeocoder)
library(tibble)
library(stringr)

GEO <- setRefClass("GEO",
  fields = list(api_key = "character", max_limit = "numeric", base_url = "character"),
  
  methods = list(
    
    initialize = function(api_key = "", max_limit = 10){
      api_key <<- api_key
      max_limit <<- 10
      base_url <<- "http://geodb-free-service.wirefreethought.com/v1/geo/"
      
      if(nchar(api_key) > 0){
        base_url <<- "https://wft-geo-db.p.rapidapi.com/v1/geo/"
      }
    },
    
    makeRequest = function(url){
      request <- GET(url)
      response <- fromJSON(content(request, as = "text", encoding = "UTF-8"))
      
      if(request$status_code >= 300){
        print(paste0("API request status: ",request$status_code, ", for endpoint GET: ", url))
        print("Response Message:")
        print(response)
      }
      
      return (response)
    },
    
    getCountryNamesAndCodes = function(){
      df = (data.frame(
        name = as.character(codelist$country.name.en),
        code2 = as.character(codelist$iso2c),
        code3 = as.character(codelist$iso3c)
      ))
      df$name = tolower(df$name)
      
      return (df)
    },
    
    translateIdsToDelimitedString = function(Ids){
      if(is.atomic(Ids) && length(Ids) == 1 && !is.array(Ids)){
        Ids = c(as.character(Ids))
      }
      
      return (paste(Ids, collapse = ","))
    },
    
    getClosestMatchingCountryNamesOrCodes = function(countries){
      countriesLookup <- getCountryNamesAndCodes()
      
      if(is.atomic(countries) && length(countries) == 1 && !is.array(countries)){
        countries = c(as.character(countries))
      }
      
      codes <- character()
      
      for (country in countries){
        
        matchCode <- toupper(country)
        matchName <- tolower(country)
        
        matches <- countriesLookup[!is.na(countriesLookup$code2) & countriesLookup$code2 == matchCode, ]
        if(nrow(matches) > 0){
          codes = c(codes, as.character(matches[1,2]))
          next
        }
        
        matches <- countriesLookup[!is.na(countriesLookup$name) & countriesLookup$name == matchName, ]
        if(nrow(matches) > 0){
          codes = c(codes, as.character(matches[1,2]))
          next
        }
        
        matches <- countriesLookup[!is.na(countriesLookup$name) & startsWith(countriesLookup$name, matchName), ]
        if(nrow(matches) > 0){
          codes = c(codes, as.character(matches[1,2]))
          next
        }
        
        matches <- countriesLookup[!is.na(countriesLookup$name) & str_detect(countriesLookup$name, matchName), ]
        if(nrow(matches) > 0){
          codes = c(codes, as.character(matches[1,2]))
          next
        }
      }
      
      if(length(codes) > 0){
        return (paste(codes, collapse = ","))
      }
      return (NULL)
    },
    
    convertLongitudeLatitudeToISO = function(lat, long){
      lat <- sprintf("%2.4f", lat)
      long <- sprintf("%3.4f", long)
      
      if(lat >= 0){ lat <- paste0("%2B",lat)}
      if(long >= 0){ long <- paste0("%2B",long)}
      
      print(lat)
      print(long)
      
      return (paste0(lat,long))
    },
    
    getLongitudeLatitude = function(location){
      geocoded_address <- geo(address = location, method='osm', lat=latitude,long=longitude)
      
      return (convertLongitudeLatitudeToISO(geocoded_address[1,2],geocoded_address[1,3]))
    }, 
    
    FindCountries = function(currencyCode = NULL,
                                  includeAllColumns = FALSE, 
                                  columns = NULL,
                                  namePrefix = NULL, 
                                  name = NULL,
                                  offset = 0, 
                                  limit = NULL){
      request_url = paste0(base_url,"countries?offset=",offset)
      
      if(!is.null(limit)){
        request_url <- paste0(request_url,"&limit=",limit)
      }
      #if(!is.null(sort)){
      #request_url <- paste0(request_url,"&sort=",sort)
      #}
      
      if(!is.null(name)){
        request_url <- paste0(request_url,"&namePrefix=",name)
      }
      else if(!is.null(namePrefix)){
        request_url <- paste0(request_url,"&namePrefix=",namePrefix)
      }
      
      if(!is.null(currencyCode)){
        request_url <- paste0(request_url,"&currencyCode=",currencyCode)
      }
      
      response <- makeRequest(request_url)
      data <- as.data.frame(response$data)
      count <- response$metadata$totalCount
      
      if(!is.null(name)){
        data = data[data$name == name, ]
      }
      if(!is.null(columns) && count > 0){
        data <- data[columns]
      }
      else if(!includeAllColumns){
        extraColumns = c("city")
        data <- data[, !(names(data) %in% extraColumns)]
      }
      
      return (list(count = count, data = as.data.frame(data)))
    },
    
    FindPlaces.NearPlace = function(charts = FALSE, placeId = NULL, 
                                         placeName = NULL,
                                         placeAddress = NULL,
                                         includeDistricts = TRUE, 
                                         includeCities = TRUE, 
                                         includeIslands = TRUE,
                                         includeAllColumns = FALSE, 
                                         columns = NULL,
                                         includeDeleted = NULL, 
                                         namePrefix = NULL, 
                                         name = NULL,
                                         maxPopulation = NULL, 
                                         minPopulation = NULL, 
                                         excludedCountries = NULL, 
                                         includedCountries = NULL, 
                                         excludedCountryIds = NULL,
                                         includedCountryIds = NULL,
                                         distanceUnit = "KM", 
                                         radius = NULL, 
                                         longitude = NULL, 
                                         latitude = NULL, 
                                         locationAddress = NULL, 
                                         offset = 0, 
                                         limit = NULL){
      if(is.null(placeAddress) && is.null(placeId) && is.null(placeName)){
        print("ERROR: Either placeId, placeAddress, or placeName is required")
        
        return (NULL)
      }
      
      id <- NULL
      if(!is.null(placeId)){
        id <- placeId
      }
      else{
        places <- FindPlaces(locationAddress = placeAddress, name = placeName)
        
        if(is.null(places) || places$count < 1){
          print(paste0("ERROR: No matching place found."))
          return (NULL)
        }
        id <- (places$data[1,'wikiDataId'])
      }
      
      request_url = paste0(base_url,"places/",id,"/nearbyPlaces?offset=",offset,"&distanceUnit=",distanceUnit)
      
      if(!is.null(limit)){
        request_url <- paste0(request_url,"&limit=",limit)
      }
      #if(!is.null(sort)){
      #request_url <- paste0(request_url,"&sort=",sort)
      #}
      
      if(!is.null(name)){
        request_url <- paste0(request_url,"&namePrefix=",name)
      }
      else if(!is.null(namePrefix)){
        request_url <- paste0(request_url,"&namePrefix=",namePrefix)
      }
      
      if(!is.null(maxPopulation)){
        request_url <- paste0(request_url,"&maxPopulation=",format(maxPopulation, scientific = FALSE))
      }
      if(!is.null(minPopulation)){
        request_url <- paste0(request_url,"&minPopulation=",format(minPopulation, scientific = FALSE))
      }
      if(!is.null(radius)){
        request_url <- paste0(request_url,"&radius=", format(radius, scientific = FALSE))
      }
      
      if(!is.null(latitude) && !is.null(longitude)){
        request_url <- paste0(request_url,"&location=", convertLongitudeLatitudeToISO(latitude, longitude))
        
      }
      else if(!is.null(locationAddress)){
        locationAddress <- getLongitudeLatitude(locationAddress)
        request_url <- paste0(request_url,"&location=",locationAddress)
      }
      
      typeVec = character()
      
      if(includeDistricts){
        typeVec = c(typeVec, as.character(c("ADM2")))
      }
      if(includeCities){
        typeVec = c(typeVec, as.character(c("CITY")))
      }
      if(includeIslands){
        typeVec = c(typeVec, as.character(c("ISLAND")))
      }
      
      if(length(typeVec) == 0){
        return (list(count = 0, data = data.frame()))
      }
      else if(length(typeVec) < 3){
        request_url <- paste0(request_url,"&types=",paste(typeVec, collapse = ","))
      }
      
      if(!is.null(includeDeleted)){
        request_url <- paste0(request_url,"&includeDeleted=", includeDeleted)
      }
      
      iCountries = character()
      
      if(!is.null(includedCountryIds)){
        iCountries = c(iCountries, as.character(c(includedCountryIds)))
      }
      
      if(!is.null(includedCountries)){
        codes = getClosestMatchingCountryNamesOrCodes(includedCountries)
        iCountries = c(iCountries, as.character(c(codes)))
      }
      
      if(length(iCountries) > 0){
        request_url <- paste0(request_url,"&countryIds=",paste(iCountries, collapse = ","))
      }
      
      eCountries = character()
      
      if(!is.null(excludedCountryIds)){
        eCountries = c(eCountries, as.character(c(excludedCountryIds)))
      }
      
      if(!is.null(excludedCountries)){
        codes = getClosestMatchingCountryNamesOrCodes(excludedCountries)
        eCountries = c(eCountries, as.character(c(codes)))
      }
      
      if(length(eCountries) > 0){
        request_url <- paste0(request_url,"&excludedCountryIds=",paste(eCountries, collapse = ","))
      }
      
      response <- makeRequest(request_url)
      data <- as.data.frame(response$data)
      count <- response$metadata$totalCount
      
      if(!is.null(name)){
        data = data[data$name == name, ]
      }
      if(!is.null(columns) && count > 0){
        data <- data[columns]
      }
      else if(!includeAllColumns){
        extraColumns = c("city")
        data <- data[, !(names(data) %in% extraColumns)]
      }
      
      return (list(count = count, data = as.data.frame(data)))
    },
    
    FindPlaces = function(includeDistricts = TRUE, 
                               includeCities = TRUE, 
                               includeIslands = TRUE,
                               includeAllColumns = FALSE, 
                               columns = NULL,
                               includeDeleted = NULL, 
                               namePrefix = NULL, 
                               name = NULL,
                               maxPopulation = NULL, 
                               minPopulation = NULL, 
                               excludedCountries = NULL, 
                               includedCountries = NULL, 
                               excludedCountryIds = NULL,
                               includedCountryIds = NULL,
                               distanceUnit = "KM", 
                               radius = NULL, 
                               longitude = NULL, 
                               latitude = NULL, 
                               locationAddress = NULL, 
                               offset = 0, 
                               limit = NULL){
      
      request_url = paste0(base_url,"places?offset=",offset,"&distanceUnit=",distanceUnit)
      
      if(!is.null(limit)){
        request_url <- paste0(request_url,"&limit=",limit)
      }
      #if(!is.null(sort)){
      #request_url <- paste0(request_url,"&sort=",sort)
      #}
      
      if(!is.null(name)){
        request_url <- paste0(request_url,"&namePrefix=",name)
      }
      else if(!is.null(namePrefix)){
        request_url <- paste0(request_url,"&namePrefix=",namePrefix)
      }
      
      if(!is.null(maxPopulation)){
        request_url <- paste0(request_url,"&maxPopulation=",format(maxPopulation, scientific = FALSE))
      }
      if(!is.null(minPopulation)){
        request_url <- paste0(request_url,"&minPopulation=",format(minPopulation, scientific = FALSE))
      }
      if(!is.null(radius)){
        request_url <- paste0(request_url,"&radius=", format(radius, scientific = FALSE))
      }
      
      if(!is.null(latitude) && !is.null(longitude)){
        request_url <- paste0(request_url,"&location=", convertLongitudeLatitudeToISO(latitude, longitude))
        
      }
      else if(!is.null(locationAddress)){
        locationAddress <- getLongitudeLatitude(locationAddress)
        request_url <- paste0(request_url,"&location=",locationAddress)
      }
      
      typeVec = character()
      
      if(includeDistricts){
        typeVec = c(typeVec, as.character(c("ADM2")))
      }
      if(includeCities){
        typeVec = c(typeVec, as.character(c("CITY")))
      }
      if(includeIslands){
        typeVec = c(typeVec, as.character(c("ISLAND")))
      }
      
      if(length(typeVec) == 0){
        return (list(count = 0, data = data.frame()))
      }
      else if(length(typeVec) < 3){
        request_url <- paste0(request_url,"&types=",paste(typeVec, collapse = ","))
      }
      
      if(!is.null(includeDeleted)){
        request_url <- paste0(request_url,"&includeDeleted=", includeDeleted)
      }
      
      iCountries = character()
      
      if(!is.null(includedCountryIds)){
        iCountries = c(iCountries, as.character(c(includedCountryIds)))
      }
      
      if(!is.null(includedCountries)){
        codes = getClosestMatchingCountryNamesOrCodes(includedCountries)
        iCountries = c(iCountries, as.character(c(codes)))
      }
      
      if(length(iCountries) > 0){
        request_url <- paste0(request_url,"&countryIds=",paste(iCountries, collapse = ","))
      }
      
      eCountries = character()
      
      if(!is.null(excludedCountryIds)){
        eCountries = c(eCountries, as.character(c(excludedCountryIds)))
      }
      
      if(!is.null(excludedCountries)){
        codes = getClosestMatchingCountryNamesOrCodes(excludedCountries)
        eCountries = c(eCountries, as.character(c(codes)))
      }
      
      if(length(eCountries) > 0){
        request_url <- paste0(request_url,"&excludedCountryIds=",paste(eCountries, collapse = ","))
      }
      
      response <- makeRequest(request_url)
      data <- as.data.frame(response$data)
      count <- response$metadata$totalCount
      
      if(!is.null(name)){
        data = data[data$name == name, ]
      }
      if(!is.null(columns) && count > 0){
        data <- data[columns]
      }
      else if(!includeAllColumns){
        extraColumns = c("city")
        data <- data[, !(names(data) %in% extraColumns)]
      }
      
      return (list(count = count, data = as.data.frame(data)))
    },
    
    FindIslands = function(Cities = FALSE, ...){
      return (FindPlaces(includeCities = Cities, includeDistricts = FALSE, ...))
    },
    
    FindDistricts = function(Cities = FALSE, ...){
      return (FindCities(includeCities = Cities, ...))
    },
    
    FindCities = function(includeDistricts = FALSE, 
                               includeCities = TRUE, 
                               includeAllColumns = FALSE, 
                               columns = NULL,
                               includeDeleted = NULL, 
                               namePrefix = NULL, 
                               name = NULL,
                               maxPopulation = NULL, 
                               minPopulation = NULL, 
                               excludedCountries = NULL, 
                               includedCountries = NULL, 
                               excludedCountryIds = NULL,
                               includedCountryIds = NULL,
                               distanceUnit = "KM", 
                               radius = NULL, 
                               longitude = NULL, 
                               latitude = NULL, 
                               locationAddress = NULL, 
                               offset = 0, 
                               limit = NULL){
      
      request_url = paste0(base_url,"cities?offset=",offset,"&distanceUnit=",distanceUnit)
      
      if(!is.null(limit)){
        request_url <- paste0(request_url,"&limit=",limit)
      }
      #if(!is.null(sort)){
      #request_url <- paste0(request_url,"&sort=",sort)
      #}
      
      if(!is.null(name)){
        request_url <- paste0(request_url,"&namePrefix=",name)
      }
      else if(!is.null(namePrefix)){
        request_url <- paste0(request_url,"&namePrefix=",namePrefix)
      }
      
      if(!is.null(maxPopulation)){
        request_url <- paste0(request_url,"&maxPopulation=",format(maxPopulation, scientific = FALSE))
      }
      if(!is.null(minPopulation)){
        request_url <- paste0(request_url,"&minPopulation=",format(minPopulation, scientific = FALSE))
      }
      if(!is.null(radius)){
        request_url <- paste0(request_url,"&radius=", format(radius, scientific = FALSE))
      }
      
      if(!is.null(latitude) && !is.null(longitude)){
        request_url <- paste0(request_url,"&location=", convertLongitudeLatitudeToISO(latitude, longitude))
        
      }
      else if(!is.null(locationAddress)){
        locationAddress <- getLongitudeLatitude(locationAddress)
        request_url <- paste0(request_url,"&location=",locationAddress)
      }
      
      if(!includeCities && !includeDistricts){
        return (list(count = 0, data = data.frame()))
      }
      else if(!includeDistricts && includeCities){
        request_url <- paste0(request_url,"&types=", "CITY")
      }
      else if(includeDistricts && !includeCities){
        request_url <- paste0(request_url,"&types=", "ADM2")
      }
      
      if(!is.null(includeDeleted)){
        request_url <- paste0(request_url,"&includeDeleted=", includeDeleted)
      }
      
      iCountries = character()
      
      if(!is.null(includedCountryIds)){
        iCountries = c(iCountries, as.character(c(includedCountryIds)))
      }
      
      if(!is.null(includedCountries)){
        codes = getClosestMatchingCountryNamesOrCodes(includedCountries)
        iCountries = c(iCountries, as.character(c(codes)))
      }
      
      if(length(iCountries) > 0){
        request_url <- paste0(request_url,"&countryIds=",paste(iCountries, collapse = ","))
      }
      
      eCountries = character()
      
      if(!is.null(excludedCountryIds)){
        eCountries = c(eCountries, as.character(c(excludedCountryIds)))
      }
      
      if(!is.null(excludedCountries)){
        codes = getClosestMatchingCountryNamesOrCodes(excludedCountries)
        eCountries = c(eCountries, as.character(c(codes)))
      }
      
      if(length(eCountries) > 0){
        request_url <- paste0(request_url,"&excludedCountryIds=",paste(eCountries, collapse = ","))
      }
      
      response <- makeRequest(request_url)
      data <- as.data.frame(response$data)
      count <- response$metadata$totalCount
      
      if(!is.null(name)){
        data = data[data$name == name, ]
      }
      if(!is.null(columns) && count > 0){
        data <- data[columns]
      }
      else if(!includeAllColumns){
        extraColumns = c("city")
        data <- data[, !(names(data) %in% extraColumns)]
      }
      
      return (list(count = count, data = as.data.frame(data)))
    },
    
    FindPlaces.By.CountryAndRegion = function(includeDistricts = TRUE, 
                                                   includeCities = TRUE, 
                                                   includeIslands = TRUE,
                                                   includeAllColumns = FALSE, 
                                                   columns = NULL, 
                                                   country = NULL, 
                                                   region = NULL, 
                                                   countryId = NULL, 
                                                   regionId = NULL,
                                                   includeDeleted = NULL, 
                                                   namePrefix = NULL, 
                                                   name = NULL,
                                                   maxPopulation = NULL, 
                                                   minPopulation = NULL, 
                                                   offset = 0, 
                                                   limit = NULL
    ){
      #request_url = paste0(base_url,"places?offset=",offset,"&distanceUnit=",distanceUnit)
      
      if((is.null(country) && is.null(countryId)) || (is.null(region) && is.null(regionId))){
        print("ERROR: either country or countryId is required, and either region or regionId is required.")
        return (NULL)
      }
      
      cId <- NULL
      if(!is.null(countryId)){
        cId <- countryId
      }
      else{
        countryMatches <- FindCountries(name = country)
        
        if(is.null(countryMatches) || countryMatches$count < 1){
          print(paste0("ERROR: No matching country '",country,"' found."))
          return (NULL)
        }
        cId <- (countryMatches$data[1,'wikiDataId'])
      }
      
      rId <- NULL
      if(!is.null(regionId)){
        rId <- regionId
      }
      else{
        regionMatches <- FindRegions.ByCountry(countryId = as.character(cId), name = region)
        
        if(is.null(regionMatches) || regionMatches$count < 1){
          print(paste0("ERROR: No matching region '",region,"' found."))
          return (NULL)
        }
        rId <- (regionMatches$data[1,'isoCode'])
      }
      
      request_url = paste0(base_url,"countries/",cId,"/regions/",rId,"/places?offset=",offset)
      
      if(!is.null(limit)){
        request_url <- paste0(request_url,"&limit=",limit)
      }
      #if(!is.null(sort)){
      #request_url <- paste0(request_url,"&sort=",sort)
      #}
      
      if(!is.null(name)){
        request_url <- paste0(request_url,"&namePrefix=",name)
      }
      else if(!is.null(namePrefix)){
        request_url <- paste0(request_url,"&namePrefix=",namePrefix)
      }
      
      if(!is.null(maxPopulation)){
        request_url <- paste0(request_url,"&maxPopulation=",format(maxPopulation, scientific = FALSE))
      }
      if(!is.null(minPopulation)){
        request_url <- paste0(request_url,"&minPopulation=",format(minPopulation, scientific = FALSE))
      }
      
      typeVec = character()
      
      if(includeDistricts){
        typeVec = c(typeVec, as.character(c("ADM2")))
      }
      if(includeCities){
        typeVec = c(typeVec, as.character(c("CITY")))
      }
      if(includeIslands){
        typeVec = c(typeVec, as.character(c("ISLAND")))
      }
      
      if(length(typeVec) == 0){
        return (list(count = 0, data = data.frame()))
      }
      else if(length(typeVec) < 3){
        request_url <- paste0(request_url,"&types=",paste(typeVec, collapse = ","))
      }
      
      if(!is.null(includeDeleted)){
        request_url <- paste0(request_url,"&includeDeleted=", includeDeleted)
      }
      
      response <- makeRequest(request_url)
      data <- as.data.frame(response$data)
      count <- response$metadata$totalCount
      
      if(!is.null(name)){
        data = data[data$name == name, ]
      }
      if(!is.null(columns) && count > 0){
        data <- data[columns]
      }
      else if(!includeAllColumns){
        extraColumns = c("city")
        data <- data[, !(names(data) %in% extraColumns)]
      }
      
      return (list(count = count, data = as.data.frame(data)))
    },
    
    FindRegions.ByCountry = function(includeAllColumns = FALSE, columns = NULL, country = NULL, countryId = NULL, limit = NULL, offset = 0, name = NULL){
      
      if(is.null(country) && is.null(countryId)){
        print("ERROR: Either 'country' or 'countryId' parameter is required for this function.")
        return (NULL)
      }
      
      id <- NULL
      if(!is.null(countryId)){
        id <- countryId
      }
      else{
        countryMatches <- FindCountries(name = country)
        
        if(is.null(countryMatches) || countryMatches$count < 1){
          print(paste0("ERROR: No matching country '",country,"' found."))
          return (NULL)
        }
        id <- (countryMatches$data[1,'wikiDataId'])
      }
      
      request_url = paste0(base_url,"countries/",id,"/regions/?offset=",offset)
      
      if(!is.null(limit)){
        request_url <- paste0(request_url,"&limit=",limit)
      }
      
      response <- makeRequest(request_url)
      data <- as.data.frame(response$data)
      count <- response$metadata$totalCount
      
      if(!is.null(name)){
        data = data[data$name == name | data$isoCode == name, ]
      }
      if(!is.null(columns) && count > 0){
        data <- data[columns]
      }
      else if(!includeAllColumns){
        extraColumns = c("city")
        data <- data[, !(names(data) %in% extraColumns)]
      }
      
      return (list(count = count, data = as.data.frame(data)))
    },
    
    Distance.Between.Places = function(distanceUnit = "KM",fromId = NULL , fromAddress = NULL, fromName= NULL, toId= NULL, toAddress= NULL, toName= NULL){
      if((is.null(fromId) && is.null(fromName) && is.null(fromAddress)) ||(is.null(toId) && is.null(toName) && is.null(toAddress)) ){
        print("ERROR: Both a from and to parameter are required")
        
        return (NULL)
      }
      
      fid <- NULL
      if(!is.null(fromId)){
        fid <- fromId
      }
      else{
        places <- FindPlaces(locationAddress = fromAddress, name = fromName)
        
        if(is.null(places) || places$count < 1){
          print(paste0("ERROR: No matching from place found."))
          return (NULL)
        }
        fid <- (places$data[1,'wikiDataId'])
      }
      
      tid <- NULL
      if(!is.null(toId)){
        tid <- toId
      }
      else{
        places <- FindPlaces(locationAddress = toAddress, name = toName)
        
        if(is.null(places) || places$count < 1){
          print(paste0("ERROR: No matching to place found."))
          return (NULL)
        }
        tid <- (places$data[1,'wikiDataId'])
      }
      
      request_url = paste0(base_url,"places/",fid,"/distance/?toPlaceId=",tid,"&distanceUnit=",distanceUnit)
      response <- makeRequest(request_url)
      
      return (response$data)
    }
  )
)

