### About Application

#### Fire Danger Ratings

Fire Danger Ratings (FDR) indicate the threat posed from bush fire. The daily FDR is based on a combination of:
  * Temperature
  * Relative Humidity
  * Wind speed 
  * Fuel dryness factors
  
This application extracts Fire Danger Rating scores and relevant weather forcasts for the coming week. It is designed to be viewed on a mobile device and to indicate the range of FDR scores in a single screen. Additional weather and Emergency Warning / Incident data is also included.


#### Audience

The primary audience is CFA brigade duty officers but the data will likely be useful to anyone living in a area that may be impacted by bush fire. 


#### Location

Location can be set by URL parameters or dropdown list. To set location via URL add `?location=YOUR-TOWN` to the base url. For example:
  * https://mrjoh3.shinyapps.io/fire-weather/?location=mildura
  * https://mrjoh3.shinyapps.io/fire-weather/?location=panton-hill

Data is aggregated by location, usually a town or area. Around 1200 locations are available across Victoria, these are either regional [towns](https://www.data.gov.au/dataset/ds-dga-bdcf5b09-89bc-47ec-9281-6b8e9ee147aa/distribution/dist-dga-53c24b8e-4f55-4eed-a189-2fc0dcca6381/details?q=) or [CFA Brigades](https://discover.data.vic.gov.au/dataset/cfa-fire-station-vmfeat-geomark_point).


### Issues and Errors

Source code for this application is available via [Github](https://github.com/mrjoh3/fdr). If you have any issues or suggestions for new features please log an issue [here](https://github.com/mrjoh3/fdr/issues).



