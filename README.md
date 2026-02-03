# DATA-534-Project-Group-6
## Members:  
### Andre Gnandt 
### Yihang Wang 
### Manpreet Singh

## GEO Chart Rendering (Yihang's Part)

Some methods in the `GEO` reference class support an optional `chart` argument.

### chart parameter
- `chart = FALSE` (default):  
  The method returns data only (no plot is produced).
- `chart = TRUE`:  
  A ggplot chart is rendered before the method returns its result.

The returned object is always a list:
- `count`: total number of records returned by the API
- `data`: cleaned `data.frame` containing the results

### Automatic chart selection
When `chart = TRUE`, the chart type is chosen automatically based on the returned data:

1. **Geographic scatter plot**
   - Used when both longitude and latitude columns are available  
   - Points are plotted using longitude (x) and latitude (y)  
   - If a population column exists, point size is mapped to population  

2. **Population bar chart**
   - Used when name and population columns are available  
   - Displays the top 10 entries by population  
   - Rendered as a horizontal bar chart  

If the data does not meet either condition, no chart is produced.

### Supported methods
The following methods currently support the `chart` argument:
- `FindCountries`
- `FindPlaces`
- `FindPlaces.NearPlace`
- `FindCities`
- `FindRegions.ByCountry`
- `FindPlaces.By.CountryAndRegion`

### Example usage
```r
g <- GEO$new()

res <- g$FindCities(
  namePrefix = "van",
  limit = 20,
  chart = TRUE
)

df <- res$data