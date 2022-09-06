# nybem v0.2.0 (Release date: 2022-09-06)

## Major Changes
- Updated model specifications based on USACE Eco PCX Model Review comments. See the [report](https://mvr-gis.github.io/NYBEM-Report/) for details. 
- Added the `feature_model_barplot` function to help better visualize model results. 
- Added the `summary_table` function to produce cleanly formatted summary tables for reporting. 
- Added the `summarize_models` function to create tabular summaries of model results. 
- Added the `recode_models` function to assist in removing placeholder values within model result rasters. 
- Added the `save_raster_rdata` function to assist in reliably saving rasters in the `.RData` format. 

## Bug Fixes
- Implemented a more efficient method for calculating area for large rasters in the `summarize_by_poly` function. 


# nybem v0.1.8 (Release date: 2022-07-23)

## Major Changes
- Added the `summarize_by_poly` function to assist in reporting model results. 
- Added the `model_summary` function to provide a quick set of graphs for reviewing model outputs. 
- Added the `na_outside_model` function to assist in combining model outputs and summarizing. 
- Assigned variable names to `SIcalc` function ouput rasters to aid reuse of model outputs.  
- Added the `name_model` function to assist in the automation of model results manipulation. 
- Added the `write_cont_tiff` function to aid in saving rasters to be cross-compatible with R and ESRI (class names, colormaps, layer names, etc.). 

## Bug Fixes
-  Fixed a file naming problem with the write_cont_tiff function. 


# nybem v0.1.4 (Release date: 2022-05-05)

## Major Changes
-  Added package documentation in preparation for [USACE ECO-PCX](https://mvr-gis.github.io/nybem/) model review. 
-  Added `nybem_submodels` containing the ecological models for the New Jersey Back Bay study. 

## Bug Fixes
-  Removed `ecorest` specific functions
