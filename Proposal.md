# Project Proposal
## Group 6: Andre Gnandt, Manpreet Singh, Yihang Wang  

### GitHub Repo: https://github.com/andre-gnandt/DATA-534-Project-Group-6

# OpenMeteoR
We are creating an R wrapper for the open-meteo wordwide weather forecasting, predicting and historical weather data api's, these api's can be found at: [Open-Meteo]("https://open-meteo.com/"). We will be providing various functions to both simplify the calling of the api's, but also to introduce more features and functionality from these api's. Some examples of the functions and features provided will include:  
  
- A catch all function: weather.range(charts, rangeStart, rangeEnd, interval, locations = [], metrics = [], stats = [], stat_interval, units, timezone) used to determine the weather patterns in various locations, over a given range of time, with desired metrics and stats. The data will be returned as a dataframe, with the option to also return charts and graphs of the weather patterns. The parameters include:
    - charts = True or False: used to return charts or not
    - rangeStart: the start date of the weather time range, can be as early as 1940 or as late as 35 days in the futue
    - rangeEnd: The end date of the weather time range
    - interval: The time between succesive weather data points (any number of hours, days, weeks, months, or years), deafult is hourly.
    - locations: An array of locations to obtain the weather data from. Can be an address, city name, name of a place, or longitude and latitude.
    - metrics: Array of metrics to be taken, for example ["temp", "rain", "feels like"], default is all metrics
    - stats: Array of stats taken on the metrics, for example ["mean", "median", "max"] would take mean, median and max of the "temp", "rain", and "feels like" metrics. Default is max and min, or exact value for hourly measurements.
    - stat_interval: the interval time for the stats to be taken, default is equal to interval
    - units: imperial or standard measurements
    - timezone: timezone 
- Other simplified versions of this previous function (weather.range()) will be introduced for specific cases, like weather.forcast() to get the weather range for the next 14 days, weather.now() to get the current weather, weather.forcast.averages(), and more similar cases.
- We will introduce some specific functions that can be used to determine the next days that will have specific weather conditions. For example, a function to determine the next snow day, a function to return all days that will snow above a certain amount in the next 2 weeks, a function to return the next sunny day or all future sunny days, and more similar functions.  
  
The purpose of this R package wrapper is to simplify the usage of the weather api's and provide some enhanced features. For example, the package provides the option to render charts for all of the weather patterns, this allows us to more easily visualize the data returned from the API's. Most of the functions return dataframes which is easier to work with than raw json. This, and the fact that you can simply just pass parameters to these functions to get the data makes it far easier for developers to use, instead of calling the raw API's. These functions also allow more flexibility than the raw API's do. For example, the raw api is rstricted to longitude and latitude for location while the package also accepts address and city name. The api's for previous, current, and future weather data are all from different endpoints, while we create one function call to handle all of this. The api endpoints only accept one location, while the package functions can accept multiple. Our package allows both imperial and standard measurements as well.
  
