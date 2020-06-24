# eqviz
NOAA Earthquakes visualization (Coursera Mastering Software Development in R Capstone)

This package is built for Mastering Software Development in R Capstone course using [NOAA earthquakes data](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).

The pacakge _eqviz_ has following functions for visualizations.

### Data handling
* eq_load_data: Load NOAA raw data into dataframe
* eq_clean_data: Do housekeeping of data including type conversions and dropping unused columns
* eq_location_clean: Keep location infos handy
* helper_neg_dates: Convert BC dates into datetime object

### Visualize on a timeline (GeomTimeline, GeomTimelineLabel)
* geom_timeline: Main Geom function to plot earthquakes into a timeline
* geom_timeline_label: Additional Geom function to label locations of the earthquakes

### Mapping using Leaflet
* eq_create_label: Create html text for popup in leaflet maps
* eq_map: Plot leaflet map of earthquakes
