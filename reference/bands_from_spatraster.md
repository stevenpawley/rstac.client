# Extract Raster Band Metadata from a SpatRaster Object

Extracts per-band metadata from a `SpatRaster` object. Creates band
objects with data type and spatial resolution, optionally calculating
statistics.

## Usage

``` r
bands_from_spatraster(spat_rast, calculate_statistics = FALSE)
```

## Arguments

- spat_rast:

  A `SpatRaster` object.

- calculate_statistics:

  (logical, optional) If TRUE, calculates min, max, mean, and standard
  deviation for each layer using
  [`terra::global()`](https://rspatial.github.io/terra/reference/global.html).
  Default is FALSE.

## Value

A list of raster band objects, one per layer.
