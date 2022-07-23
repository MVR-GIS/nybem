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
