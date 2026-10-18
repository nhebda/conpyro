# conpyro 0.2.0

## Models and prediction changes

* Added surface ROS models "9c", "11c", 16, 17, "18c", 19, and 20.
* Updated coefficients for default surface ROS models 12 and 13.
  Existing calls using the defaults can therefore produce different
  predictions from version 0.1.0.
* Added stand-dependent surface ROS calculations for model "18c",
  with negative predictions bounded at zero.
* Corrected the standing-dead ladder-fuel calculation to use crown length
  directly rather than half its value, with a minimum effective fuel
  strata gap of 0.5 m. Predictions can differ from version 0.1.0.
* Increased the permitted sapling lower crown base height (`zs`) to 9.5 m,
  matching the documented range. The requirement that `zs < hs` remains.

## Output and input handling

* Composite ROS now preserves the supplied wind-speed order, including
  repeated values.
* Crowning thresholds now return the lowest supplied wind speed in each
  regime, rather than the first matching input position.
* Corrected ROS plot transition endpoints for unsorted wind speeds.
* Explicit passive-crown ROS output now returns the calculated component
  at every supplied wind speed, even when passive crowning is absent.
  Composite fire-regime selection is unchanged.
* Corrected omitted/default scenario ID handling.
* Added warnings for misplaced moisture arguments in fire tools and
  rejection of unknown or unnamed extra arguments in those tools.

## FuelDash integration

* Added an internal full-precision output interface containing FFMC-based
  moisture, stand-adjusted moisture when available, and a wind-aligned
  series of crown fire probability, fire type, and composite ROS.

## Documentation and maintenance

* Updated scientific references for the modelling framework, crown fire
  occurrence models, and surface fire spread models.
* Clarified moisture fallback, intended model applicability, sampled
  thresholds, output precision, and scenario identifiers.
* Documented the existing dependence of smoothed predictions on the
  supplied wind-speed vector. The smoothing formulation is unchanged.
* Corrected example-data documentation and clarified model descriptions, 
  density boundaries, and output units.
* Updated the README with model-selection guidance and a FuelDash link.
