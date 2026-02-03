# DATA-534-Project-Group-6
## Members:  
### Andre Gnandt 
### Yihang Wang 
### Manpreet Singh

## Charts
Some GEO class methods that return data frames accept `chart` (boolean).

- `chart = FALSE` (default): returns the data only
- `chart = TRUE`: prints a ggplot chart before returning the data

The chart type is chosen automatically:
- If the returned data has `longitude` and `latitude`, it plots points (size mapped to `population` if available).
- Otherwise, if it has `name` and `population`, it plots a Top-10 population bar chart.

Example:
```r
g <- GEO$new()
res <- g$FindCities(namePrefix = "van", limit = 20, chart = TRUE)
df <- res$data
```