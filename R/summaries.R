
#' Summarized landscape elements
#'
#' Generates landscape analysis for a property, including a natural language
#' description suitable for forest management plans.
#'
#' @param property sf polygon object with property of interest
#' @param yf_size acreage of young forest blocks that qualifies them as young
#'   forest habitat. Defaults to 1 ac.
#' @param yf_dist maximum distance, in feet, between young forest areas that
#'   allows them to function as a single block. Defaults to 100 ft.
#'
#' @returns list of landscape elements, suitable for addition to a forest
#'   inventory. Currently includes:
#'   \describe{
#'     \item{landscape_text}{Character string with natural language description
#'       of the landscape surrounding the property.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Create a property polygon (example: 50-acre parcel)
#' property <- sf::st_sfc(
#'   sf::st_polygon(list(rbind(
#'     c(-72.5, 44.4), c(-72.49, 44.4),
#'     c(-72.49, 44.41), c(-72.5, 44.41),
#'     c(-72.5, 44.4)
#'   ))),
#'   crs = 4326
#' )
#' property <- sf::st_as_sf(data.frame(id = 1, geometry = property))
#'
#' # Generate landscape elements
#' elements <- landscape_elements(property)
#'
#' # Access the landscape description
#' cat(elements$landscape_text)
#' }
landscape_elements <- function(property, yf_size = 1, yf_dist = 100){
  aoi <- aoi(property)

  # Fetch data
  ndsm <- get_ndsm(aoi)
  lc <- get_landcover(aoi)
  wet <- get_wetlands(aoi)
  ttype <- get_treetype(aoi)

 # TODO: Parallel fetching for remote data (requires package to be installed)
  # old_plan <- future::plan(future::multisession)
  # on.exit(future::plan(old_plan), add = TRUE)
  # ndsm_f <- future::future(get_ndsm(aoi), packages = "vthabitat")
  # clr_f <- future::future(get_clr(aoi), packages = "vthabitat")
  # ndsm <- future::value(ndsm_f)
  # clr <- future::value(clr_f)

  # Process
  canopy <- make_canopy(ndsm, lc)
  shrub <- make_shrub(canopy, size = yf_size, distance = yf_dist)

  ls <- land_summary(lc, canopy, wet, shrub, ttype)
  ls_text <- describe_landscape(ls)

  # in future, could return list with landscape text, maps, etc.
  return(list(landscape_text = ls_text))
}

#' Landcover summary
#'
#' Summarizes percentage of land area in each cover type, combining landcover,
#' canopy, wetland, and shrub data into a single comprehensive table.
#'
#' @param landcover \code{SpatRaster} object with landcover classes from
#'   \code{get_landcover}. Must have category labels set.
#' @param canopy \code{SpatRaster} object with canopy classes from
#'   \code{make_canopy}.
#' @param wetland \code{SpatRaster} object with wetland classes from
#'   \code{get_wetlands}.
#' @param shrub \code{sf} object with shrub polygons from \code{make_shrub},
#'   representing wildlife-valuable shrub areas.
#' @param treetype \code{SpatRaster} object with tree type classes from
#'   \code{get_treetype}, showing Coniferous vs Deciduous canopy.
#'
#' @return A list with two data frames:
#'   \describe{
#'     \item{cover}{Land cover classes with \code{cover} and \code{pct} columns,
#'       where pct is percentage of total AOI area (summing to 100)}
#'     \item{treetype}{Tree type classes with \code{type} and \code{pct} columns,
#'       where pct is percentage of tree canopy area (summing to 100)}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get data for area of interest
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 500)
#' lc <- get_landcover(my_aoi)
#' ndsm <- get_ndsm(my_aoi)
#' wetland <- get_wetlands(my_aoi)
#' treetype <- get_treetype(my_aoi)
#' canopy <- make_canopy(ndsm, lc)
#' shrub <- make_shrub(canopy)
#'
#' # Summarize land cover percentages
#' summary <- land_summary(lc, canopy, wetland, shrub, treetype)
#' summary$cover    # cover types as % of AOI
#' summary$treetype # conifer/deciduous as % of tree canopy
#' }
land_summary <- function(landcover, canopy, wetland, shrub, treetype) {
  # --- 1. Get developed, water, bare soil from landcover ---
  lc_freq <- terra::freq(landcover)
  lc_freq <- lc_freq[!is.na(lc_freq$value), ]
  total_lc_cells <- sum(lc_freq$count)

  # Combine developed classes
  dev_classes <- c("Buildings", "Roads", "Railroads", "Other Impervious")
  developed_pct <- 100 * sum(lc_freq$count[lc_freq$value %in% dev_classes]) / total_lc_cells

  # Water and bare soil
  water_pct <- 100 * sum(lc_freq$count[lc_freq$value == "Water"]) / total_lc_cells
  bare_soil_pct <- 100 * sum(lc_freq$count[lc_freq$value == "Bare Soil"]) / total_lc_cells

  # --- 2. Get wetland percentages (as % of total AOI) ---
  # Resample wetland to match canopy resolution
  wetland_resampled <- terra::resample(wetland, canopy, method = "near")

  wet_freq <- terra::freq(wetland_resampled)
  wet_freq <- wet_freq[!is.na(wet_freq$value), ]
  total_canopy_cells <- sum(!is.na(terra::values(canopy)))

  # Calculate wetland percentages scaled to whole AOI
  # First get vegetated % from landcover to scale canopy-resolution counts
  veg_classes <- c("Tree Canopy", "Grass/Shrubs")
  veg_pct <- sum(lc_freq$count[lc_freq$value %in% veg_classes]) / total_lc_cells

  emergent_pct <- 0
  forested_pct <- 0
  scrub_shrub_pct <- 0
  if (nrow(wet_freq) > 0) {
    wet_pcts <- 100 * wet_freq$count / total_canopy_cells * veg_pct
    names(wet_pcts) <- wet_freq$value
    emergent_pct <- ifelse("Emergent" %in% names(wet_pcts), wet_pcts["Emergent"], 0)
    forested_pct <- ifelse("Forested" %in% names(wet_pcts), wet_pcts["Forested"], 0)
    scrub_shrub_pct <- ifelse("Scrub/Shrub" %in% names(wet_pcts), wet_pcts["Scrub/Shrub"], 0)
  }

  # --- 3. Create upland canopy (mask out wetland areas) ---
  is_wetland <- !is.na(wetland_resampled)
  upland_canopy <- terra::mask(canopy, is_wetland, maskvalues = TRUE)

  # --- 4. Split shrub into valuable vs other ---
  # Rasterize shrub polygons to match canopy
  if (nrow(shrub) > 0) {
    shrub_transformed <- sf::st_transform(shrub, terra::crs(canopy))
    valuable_shrub_rast <- terra::rasterize(shrub_transformed, canopy, field = 1, background = 0)
  } else {
    valuable_shrub_rast <- canopy * 0  # All zeros
  }

  # Get upland canopy frequencies
  upland_freq <- terra::freq(upland_canopy)
  upland_freq <- upland_freq[!is.na(upland_freq$value), ]

  # Calculate upland percentages scaled to whole AOI
  groundcover_pct <- 0
  shrub_total_pct <- 0
  pole_pct <- 0
  partial_pct <- 0
  closed_pct <- 0

  if (nrow(upland_freq) > 0) {
    upland_pcts <- 100 * upland_freq$count / total_canopy_cells * veg_pct
    names(upland_pcts) <- upland_freq$value
    groundcover_pct <- ifelse("groundcover" %in% names(upland_pcts), upland_pcts["groundcover"], 0)
    shrub_total_pct <- ifelse("shrub" %in% names(upland_pcts), upland_pcts["shrub"], 0)
    pole_pct <- ifelse("pole" %in% names(upland_pcts), upland_pcts["pole"], 0)
    partial_pct <- ifelse("partial" %in% names(upland_pcts), upland_pcts["partial"], 0)
    closed_pct <- ifelse("closed" %in% names(upland_pcts), upland_pcts["closed"], 0)
  }

  # Split shrub into valuable vs other
  # Mask upland canopy to shrub class only
  upland_shrub_only <- upland_canopy
  upland_shrub_only[upland_shrub_only != 1] <- NA  # 1 = shrub

  # Count valuable vs other shrub cells
  valuable_mask <- valuable_shrub_rast == 1
  valuable_shrub_cells <- sum(!is.na(terra::values(upland_shrub_only)) &
                               terra::values(valuable_mask) == TRUE, na.rm = TRUE)
  other_shrub_cells <- sum(!is.na(terra::values(upland_shrub_only)) &
                            terra::values(valuable_mask) == FALSE, na.rm = TRUE)

  valuable_shrub_pct <- 100 * valuable_shrub_cells / total_canopy_cells * veg_pct
  other_shrub_pct <- 100 * other_shrub_cells / total_canopy_cells * veg_pct

  # --- 5. Calculate treetype percentages (% of tree canopy) ---
  treetype_tab <- pct_type(treetype)

  # --- 6. Assemble output tables ---
  cover_tab <- data.frame(
    cover = c("Developed", "Water", "Bare Soil",
              "Emergent Wetland", "Forested Wetland", "Scrub/Shrub Wetland",
              "Upland Groundcover", "Upland Shrub (valuable)", "Upland Shrub (other)",
              "Upland Pole", "Upland Partial", "Upland Closed"),
    pct = c(developed_pct, water_pct, bare_soil_pct,
            emergent_pct, forested_pct, scrub_shrub_pct,
            groundcover_pct, valuable_shrub_pct, other_shrub_pct,
            pole_pct, partial_pct, closed_pct)
  )

  # Remove any rows with 0 percent
  cover_tab <- cover_tab[cover_tab$pct > 0, ]

  return(list(
    cover = cover_tab,
    treetype = treetype_tab
  ))
}


#' Describe landscape in natural language
#'
#' Generates a descriptive paragraph about the landscape composition suitable
#' for inclusion in forest management plans.
#'
#' @param summary List output from \code{land_summary}, containing cover and
#'   treetype data frames.
#'
#' @return Character string containing a descriptive paragraph about the
#'   landscape area.
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- land_summary(lc, canopy, wetland, shrub, treetype)
#' describe_landscape(summary)
#' }
describe_landscape <- function(summary) {
  cover <- summary$cover
  treetype <- summary$treetype

  # --- Sentence 1: Cover types by abundance ---
  # Aggregate categories for reporting
  agg <- data.frame(
    category = c("upland forests", "fields", "forested wetlands",
                 "emergent wetlands", "shrub wetlands", "water bodies",
                 "developed land", "bare soil"),
    pct = c(
      sum(cover$pct[cover$cover %in% c("Upland Shrub (valuable)",
                                        "Upland Shrub (other)",
                                        "Upland Pole", "Upland Partial",
                                        "Upland Closed")]),
      sum(cover$pct[cover$cover == "Upland Groundcover"]),
      sum(cover$pct[cover$cover == "Forested Wetland"]),
      sum(cover$pct[cover$cover == "Emergent Wetland"]),
      sum(cover$pct[cover$cover == "Scrub/Shrub Wetland"]),
      sum(cover$pct[cover$cover == "Water"]),
      sum(cover$pct[cover$cover == "Developed"]),
      sum(cover$pct[cover$cover == "Bare Soil"])
    )
  )

  # Round to nearest whole number and filter out small values
  agg$pct_round <- round(agg$pct)
  agg <- agg[agg$pct >= 0.5, ]
  agg <- agg[order(-agg$pct), ]

  # Build the list phrase
  if (nrow(agg) == 0) {
    sentence1 <- paste("Landscape analysis shows that the",
                       "landscape area has no significant land cover.")
  } else if (nrow(agg) == 1) {
    sentence1 <- paste0(
      "Landscape analysis shows that in the ",
      "landscape area surrounding the property, some ",
      pct_to_words(agg$pct_round[1]), " percent of the land area is covered by ",
      agg$category[1], "."
    )
  } else {
    # First item: "X percent of the land area is covered by [category]"
    first_item <- paste0(
      pct_to_words(agg$pct_round[1]), " percent of the land area is covered by ",
      agg$category[1]
    )
    # Remaining items: "X percent by [category]"
    rest_items <- paste0(pct_to_words(agg$pct_round[-1]), " percent by ", agg$category[-1])

    if (length(rest_items) == 1) {
      list_phrase <- paste(first_item, "and", rest_items)
    } else {
      list_phrase <- paste0(
        first_item, ", ",
        paste(rest_items[-length(rest_items)], collapse = ", "),
        ", and ", rest_items[length(rest_items)]
      )
    }
    sentence1 <- paste0(
      "Landscape analysis shows that in the ",
      "landscape area surrounding the property, some ", list_phrase, "."
    )
  }

  # --- Sentence 2: Tree type mix and structural diversity ---
  conifer_pct <- treetype$pct[treetype$type == "Coniferous"]
  if (length(conifer_pct) == 0) conifer_pct <- 0

  # Tree type description
  treetype_desc <- if (conifer_pct < 20) {
    "almost entirely hardwoods"
  } else if (conifer_pct < 40) {
    "dominated by hardwoods"
  } else if (conifer_pct <= 60) {
    "a fairly even mix of hardwoods and softwoods"
  } else if (conifer_pct <= 80) {
    "dominated by softwoods"
  } else {
    "almost entirely softwoods"
  }

  # Structural diversity assessment

  # Get component percentages (of AOI)
  get_pct <- function(name) {
    val <- cover$pct[cover$cover == name]
    if (length(val) == 0) 0 else val
  }

  pole_pct <- get_pct("Upland Pole")
  partial_pct <- get_pct("Upland Partial")
  closed_pct <- get_pct("Upland Closed")
  valuable_shrub_pct <- get_pct("Upland Shrub (valuable)")
  other_shrub_pct <- get_pct("Upland Shrub (other)")
  forested_wet_pct <- get_pct("Forested Wetland")
  scrub_shrub_wet_pct <- get_pct("Scrub/Shrub Wetland")

  # Upland forest = all upland canopy classes (shrub, pole, partial, closed)
  upland_shrub_pct <- valuable_shrub_pct + other_shrub_pct
  upland_forest_pct <- upland_shrub_pct + pole_pct + partial_pct + closed_pct

  # All forestland = upland forest + forested wetland + scrub/shrub wetland
  all_forest_pct <- upland_forest_pct + forested_wet_pct + scrub_shrub_wet_pct

  if (all_forest_pct > 0) {
    # Young forest (valuable shrub) as % of all forestland
    young_of_forest <- 100 * valuable_shrub_pct / all_forest_pct

    # Relative percentages within upland forest
    if (upland_forest_pct > 0) {
      pole_rel <- 100 * pole_pct / upland_forest_pct
      partial_rel <- 100 * partial_pct / upland_forest_pct
      closed_rel <- 100 * closed_pct / upland_forest_pct
      shrub_rel <- 100 * upland_shrub_pct / upland_forest_pct
      pole_shrub_rel <- pole_rel + shrub_rel
      pole_partial_rel <- pole_rel + partial_rel
      partial_closed_rel <- partial_rel + closed_rel
    } else {
      pole_rel <- partial_rel <- closed_rel <- shrub_rel <- 0
      pole_shrub_rel <- pole_partial_rel <- partial_closed_rel <- 0
    }

    # Criteria for structural diversity:
    # 1. At least 4.5% of all forestland is young upland forest (valuable shrub)
    # 2. Poles + partial >= 20% of upland forest (midstory development)
    # 3. Partial + closed >= 60% of upland forest (mature forest presence)
    has_young_forest <- young_of_forest >= 4.5
    has_midstory <- pole_partial_rel >= 20
    has_mature <- partial_closed_rel >= 60

    # Check for imbalance: poles + shrub > 40% means little mature forest
    is_immature_heavy <- pole_shrub_rel > 40

    if (has_young_forest && has_midstory && has_mature) {
      structure_desc <- "have good structural diversity with a mix of canopy conditions"
    } else {
      # Build description of what's lacking
      lacking <- c()
      if (!has_young_forest) {
        lacking <- c(lacking, "young forest")
      }
      if (!has_midstory) {
        lacking <- c(lacking, "midstory development")
      }
      if (is_immature_heavy) {
        lacking <- c(lacking, "mature forest")
      } else if (!has_mature && !is_immature_heavy) {
        lacking <- c(lacking, "mature canopy")
      }

      if (length(lacking) == 0) {
        structure_desc <- "have moderate structural diversity"
      } else if (length(lacking) == 1) {
        structure_desc <- paste0("lack structural diversity, with little ", lacking)
      } else {
        structure_desc <- paste0(
          "lack structural diversity, with little ",
          paste(lacking[-length(lacking)], collapse = ", "),
          " or ", lacking[length(lacking)]
        )
      }
    }
  } else {
    structure_desc <- "are minimal in extent"
  }

  sentence2 <- paste0(
    "The forests are ", treetype_desc, ", and they ", structure_desc, "."
  )

  # --- Sentence 3: Young upland forest ---
  # Calculate as % of naturally vegetated land
  # (all upland canopy + all wetlands, excluding developed, water, bare soil)
  nat_veg_pct <- sum(cover$pct[!cover$cover %in% c("Developed", "Water", "Bare Soil")])

  if (nat_veg_pct > 0 && valuable_shrub_pct >= 0.5) {
    young_forest_rel <- round(100 * valuable_shrub_pct / nat_veg_pct)
    # Add "only" if young forest is less than 4.5% of all forestland
    only_qualifier <- if (all_forest_pct > 0 && young_of_forest < 4.5) "only " else ""
    sentence3 <- paste0(
      "Of the naturally vegetated land, ",
      only_qualifier,
      pct_to_words(young_forest_rel),
      " percent is young upland forest (less than 20 feet tall and in patches that cover at least one acre)."
    )
  } else {
    sentence3 <- ""
  }

  sentence4 <-
    paste0("Note that these statistics are based on numerous data sources",
           " dating to 2022 or later. Conditions on the ground may have",
           " changed since data were collected; for example, if the area",
           " experienced significant development or logging.")

  # Combine sentences
  result <- paste(sentence1, sentence2)
  if (nchar(sentence3) > 0) {
    result <- paste(result, sentence3)
  }
  result <- paste(result, sentence4)

  return(result)
}


# Convert percentage to word form for small numbers
pct_to_words <- function(pct) {
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
             "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
             "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
  sapply(pct, function(p) {
    if (p <= 20) {
      words[p + 1]
    } else {
      as.character(p)
    }
  })
}


pct_type <- function(x) {
  freq_tab <- terra::freq(x)
  # Remove NA row (want pct of classed area)
  freq_tab <- freq_tab[!is.na(freq_tab$value), ]

  # terra::freq() returns category labels directly in the value column
  # when the raster has categories set
  total_cells <- sum(freq_tab$count)
  data.frame(
    type = freq_tab$value,
    pct = 100 * freq_tab$count / total_cells
  )
}
