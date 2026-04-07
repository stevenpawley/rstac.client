# Generate a Thumbnail PNG from a SpatRaster Object

Renders a `terra` `SpatRaster` to a PNG image and returns a STAC asset
pointing to it. Rasters with 3 or more layers are rendered as an RGB
composite using layers 1–3 via
[`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html);
single-layer rasters are rendered as a greyscale image via
[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html).

## Usage

``` r
thumbnail_from_spatraster(
  spat_rast,
  path,
  width = 256,
  height = 256,
  title = NULL,
  ...
)
```

## Arguments

- spat_rast:

  A `SpatRaster` object.

- path:

  (character, required) File path for the output PNG.

- width:

  (integer) Image width in pixels. Default is 256.

- height:

  (integer) Image height in pixels. Default is 256.

- title:

  (character, optional) Title for the returned asset.

- ...:

  Additional arguments passed to
  [`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html)
  or
  [`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html).

## Value

A STAC asset list with `href`, `type = "image/png"`, and
`roles = c("thumbnail")`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

r <- rast(system.file("ex/logo.tif", package = "terra"))
asset <- thumbnail_from_spatraster(r, path = "thumbnail.png")

item <- item_from_spatraster(r, id = "logo", datetime = "2023-01-01T00:00:00Z")
item <- add_asset(item, key = "thumbnail", asset = asset)
} # }
```
