# R/clean_dwc_pipeline.R



#' Clean and validate a mapped Darwin Core data.frame
#'
#' @param df data.frame after mapping (DwC column names)
#' @param coord_epsg_in EPSG code of input coordinates
#' @param target_epsg EPSG code to reproject to (default 4326)
#' @param force_parzer logical; if TRUE, try parzer parsing even when values look numeric
#' @param bathy optional marmap bathy object already loaded/downloaded
#' @param bathy_auto_download logical; if TRUE and bathy is NULL, tries to fetch bathymetry with marmap for the dataset extent
#' @param bathy_depth_tolerance_m numeric; tolerance (meters) to compare reported depth with bathymetry
#' @param swap_auto_fix logical; if TRUE, auto-swap coordinates in clear range-based cases
#' @param preserve_original_coords logical; if TRUE, preserve original decimalLatitude/decimalLongitude
#'   into verbatimLatitude/verbatimLongitude before any parsing/reprojection, when those
#'   verbatim fields do not already exist
#' @param create_geodetic_datum logical; if TRUE, create/fill geodeticDatum
#' @param geodetic_datum_value character; value used to fill geodeticDatum when enabled
#' @param standardize_event_date logical; if TRUE, try to normalize eventDate to ISO-8601
#' @param ambiguous_date_order character; "dmy" or "mdy" for ambiguous numeric dates
#' @return list(data=cleaned_df, issues=issues_df, summary=summary_df)
#' @export
clean_dwc_pipeline <- function(df,
                               coord_epsg_in = 4326,
                               target_epsg = 4326,
                               force_parzer = TRUE,
                               bathy = NULL,
                               bathy_auto_download = TRUE,
                               bathy_depth_tolerance_m = 200,
                               swap_auto_fix = FALSE,
                               preserve_original_coords = FALSE,
                               create_geodetic_datum = FALSE,
                               geodetic_datum_value = "WGS84",
                               standardize_event_date = TRUE,
                               ambiguous_date_order = "dmy") {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(data = df, issues = data.frame(), summary = data.frame()))
  }



  ambiguous_date_order <- tolower(trimws(as.character(ambiguous_date_order %||% "dmy")))
  if (!(ambiguous_date_order %in% c("dmy", "mdy"))) {
    ambiguous_date_order <- "dmy"
  }



  issues <- data.frame(
    row = integer(),
    original_row = integer(),
    original_id = character(),
    field = character(),
    rule = character(),
    severity = character(),
    message = character(),
    stringsAsFactors = FALSE
  )



  out <- df



  add_issue <- function(idx, field, rule, severity, message) {
    idx_int <- suppressWarnings(as.integer(idx)[1])



    has_valid_idx <- !is.na(idx_int) &&
      length(idx_int) == 1 &&
      idx_int >= 1 &&
      idx_int <= nrow(out)



    aurora_row <- NA_integer_
    aurora_id <- NA_character_



    if (has_valid_idx) {
      if (".aurora_origin_row" %in% names(out)) {
        aurora_row <- suppressWarnings(as.integer(out$.aurora_origin_row[idx_int]))
      }
      if (".aurora_origin_id" %in% names(out)) {
        aurora_id <- as.character(out$.aurora_origin_id[idx_int])
      }
    }



    issues <<- rbind(
      issues,
      data.frame(
        row = if (has_valid_idx) idx_int else NA_integer_,
        original_row = aurora_row,
        original_id = aurora_id,
        field = as.character(field),
        rule = as.character(rule),
        severity = as.character(severity),
        message = as.character(message),
        stringsAsFactors = FALSE
      )
    )
  }



  # ------------------------------------------------------------
  # 1) Name parsing / whitespace (scientificName)
  # ------------------------------------------------------------
  if ("scientificName" %in% names(out)) {
    x <- as.character(out$scientificName)
    x0 <- x



    x <- trimws(x)
    x <- gsub("\\s+", " ", x)



    out$scientificName <- x



    empty <- is.na(x) | x == ""
    if (any(empty)) {
      idx <- which(empty)
      for (i in idx) {
        add_issue(i, "scientificName", "empty_scientificName",
                  "ERROR", "scientificName is missing.")
      }
    }



    changed <- !is.na(x0) & x0 != x
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) {
        add_issue(i, "scientificName", "name_trim",
                  "INFO", "scientificName standardized (trim/whitespace).")
      }
    }
  }



  # ------------------------------------------------------------
  # 2) Dates to ISO-8601 (eventDate)
  # ------------------------------------------------------------
  if (isTRUE(standardize_event_date) && "eventDate" %in% names(out)) {
    x <- as.character(out$eventDate)
    x0 <- x



    x <- trimws(x)
    x[x == ""] <- NA_character_



    iso <- vapply(seq_along(x), function(i) {
      .standardize_event_date_value(
        x[i],
        ambiguous_date_order = ambiguous_date_order
      )
    }, character(1))



    out$eventDate <- iso



    bad <- !is.na(x) & is.na(iso)
    if (any(bad)) {
      idx <- which(bad)
      for (i in idx) {
        add_issue(i, "eventDate", "date_parse",
                  "WARNING",
                  paste0("Could not standardize eventDate: '", x[i], "'."))
      }
    }



    changed <- !is.na(x0) & !is.na(iso) & x0 != iso
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) {
        add_issue(i, "eventDate", "date_iso",
                  "INFO", "eventDate standardized to ISO-8601.")
      }
    }
  }



  # ------------------------------------------------------------
  # 3) Coordinate cleanup (decimalLatitude/decimalLongitude)
  # ------------------------------------------------------------
  has_decimal_lat <- "decimalLatitude" %in% names(out)
  has_decimal_lon <- "decimalLongitude" %in% names(out)
  has_vlat <- "verbatimLatitude" %in% names(out)
  has_vlon <- "verbatimLongitude" %in% names(out)
  has_geo_fields <- has_decimal_lat || has_decimal_lon || has_vlat || has_vlon

  if (!has_geo_fields) {
    lat <- rep(NA_real_, nrow(out))
    lon <- rep(NA_real_, nrow(out))
    dmin <- rep(NA_real_, nrow(out))
    dmax <- rep(NA_real_, nrow(out))
  } else {
    if (!has_decimal_lat) {
      out$decimalLatitude <- NA_real_
      add_issue(NA_integer_, "decimalLatitude", "coord_missing_created",
                "INFO", "decimalLatitude is missing.")
      has_decimal_lat <- TRUE
    }
    if (!has_decimal_lon) {
      out$decimalLongitude <- NA_real_
      add_issue(NA_integer_, "decimalLongitude", "coord_missing_created",
                "INFO", "decimalLongitude is missing.")
      has_decimal_lon <- TRUE
    }



    if (isTRUE(preserve_original_coords)) {
      if ("decimalLatitude" %in% names(out) && !("verbatimLatitude" %in% names(out))) {
        out$verbatimLatitude <- as.character(out$decimalLatitude)
        add_issue(
          NA_integer_,
          "verbatimLatitude",
          "verbatim_created_from_decimalLatitude",
          "INFO",
          "verbatimLatitude created from decimalLatitude before transformation."
        )
      }



      if ("decimalLongitude" %in% names(out) && !("verbatimLongitude" %in% names(out))) {
        out$verbatimLongitude <- as.character(out$decimalLongitude)
        add_issue(
          NA_integer_,
          "verbatimLongitude",
          "verbatim_created_from_decimalLongitude",
          "INFO",
          "verbatimLongitude created from decimalLongitude before transformation."
        )
      }
    }



    vlat_raw <- if (has_vlat) out$verbatimLatitude else rep(NA, nrow(out))
    vlon_raw <- if (has_vlon) out$verbatimLongitude else rep(NA, nrow(out))



    lat_raw <- out$decimalLatitude
    lon_raw <- out$decimalLongitude



    lat_num <- suppressWarnings(as.numeric(lat_raw))
    lon_num <- suppressWarnings(as.numeric(lon_raw))



    vlat_chr <- trimws(as.character(vlat_raw))
    vlon_chr <- trimws(as.character(vlon_raw))
    lat_chr  <- trimws(as.character(lat_raw))
    lon_chr  <- trimws(as.character(lon_raw))



    use_vlat <- !is.na(vlat_chr) & vlat_chr != ""
    use_vlon <- !is.na(vlon_chr) & vlon_chr != ""



    lat_candidate <- ifelse(use_vlat, vlat_chr, lat_chr)
    lon_candidate <- ifelse(use_vlon, vlon_chr, lon_chr)



    need_parse_lat <- force_parzer || any(is.na(lat_num) & lat_candidate != "" & !is.na(lat_candidate))
    need_parse_lon <- force_parzer || any(is.na(lon_num) & lon_candidate != "" & !is.na(lon_candidate))



    lat <- lat_num
    lon <- lon_num



    lat_parsed <- rep(NA_real_, nrow(out))
    lon_parsed <- rep(NA_real_, nrow(out))



    if ((need_parse_lat || need_parse_lon) && requireNamespace("parzer", quietly = TRUE)) {
      lat_parsed <- suppressWarnings(parzer::parse_lat(lat_candidate))
      lon_parsed <- suppressWarnings(parzer::parse_lon(lon_candidate))



      lat[is.na(lat) & !is.na(lat_parsed)] <- lat_parsed[is.na(lat) & !is.na(lat_parsed)]
      lon[is.na(lon) & !is.na(lon_parsed)] <- lon_parsed[is.na(lon) & !is.na(lon_parsed)]



      idx_changed <- which(
        (is.na(lat_num) & !is.na(lat_parsed) & lat_candidate != "") |
          (is.na(lon_num) & !is.na(lon_parsed) & lon_candidate != "")
      )
      if (length(idx_changed) > 0) {
        for (i in idx_changed) {
          add_issue(i, "decimalLatitude/decimalLongitude", "coord_parzer",
                    "INFO",
                    "Coordinates converted to decimal degrees from verbatim.")
        }
      }
    } else if (need_parse_lat || need_parse_lon) {
      idx <- which(
        (is.na(lat) & !is.na(lat_candidate) & lat_candidate != "") |
          (is.na(lon) & !is.na(lon_candidate) & lon_candidate != "")
      )
      if (length(idx) > 0) {
        for (i in idx) {
          add_issue(i, "decimalLatitude/decimalLongitude", "coord_parse_missing_pkg",
                    "ERROR", "Could not parse coordinates. Install the 'parzer' package.")
        }
      }
    }



    lat_nonempty <- !is.na(lat_candidate) & lat_candidate != ""
    lon_nonempty <- !is.na(lon_candidate) & lon_candidate != ""



    bad_lat_non_numeric <- lat_nonempty & is.na(suppressWarnings(as.numeric(lat_candidate))) & is.na(lat_parsed)
    bad_lon_non_numeric <- lon_nonempty & is.na(suppressWarnings(as.numeric(lon_candidate))) & is.na(lon_parsed)



    if (any(bad_lat_non_numeric | bad_lon_non_numeric)) {
      idx <- which(bad_lat_non_numeric | bad_lon_non_numeric)
      for (i in idx) {
        add_issue(
          i,
          "decimalLatitude/decimalLongitude",
          "coord_non_numeric",
          "ERROR",
          paste0(
            "Coordinates are not numeric or could not be parsed. latitude='",
            lat_candidate[i], "', longitude='", lon_candidate[i], "'."
          )
        )
      }
    }



    lat_in_range <- !is.na(lat) & lat >= -90 & lat <= 90
    lon_in_range <- !is.na(lon) & lon >= -180 & lon <= 180



    miss <- is.na(lat) | is.na(lon)
    if (any(miss)) {
      idx <- which(miss)
      for (i in idx) {
        add_issue(i, "decimalLatitude/decimalLongitude", "empty_coordinates",
                  "WARNING", "Missing coordinates (lat/lon) after parsing attempt.")
      }
    }



    zero_zero <- !is.na(lat) & !is.na(lon) & lat == 0 & lon == 0
    if (any(zero_zero)) {
      idx <- which(zero_zero)
      for (i in idx) {
        add_issue(
          i,
          "decimalLatitude/decimalLongitude",
          "coords_zero_zero",
          "WARNING",
          "Coordinates at 0,0 detected (Null Island). Review whether this is an error."
        )
      }
    }



    epsg_in <- suppressWarnings(as.integer(coord_epsg_in))
    epsg_target <- suppressWarnings(as.integer(target_epsg))



    if (!is.na(epsg_in) && epsg_in == 4326) {
      oor_lat <- !is.na(lat) & (lat < -90 | lat > 90)
      oor_lon <- !is.na(lon) & (lon < -180 | lon > 180)



      if (any(oor_lat | oor_lon)) {
        idx <- which(oor_lat | oor_lon)
        for (i in idx) {
          add_issue(i, "decimalLatitude/decimalLongitude", "out_of_range",
                    "ERROR", "Coordinates outside the valid range for EPSG:4326.")
        }
      }
    }



    if (!is.na(epsg_in) && !is.na(epsg_target) &&
        epsg_in != epsg_target) {



      if (requireNamespace("sf", quietly = TRUE)) {
        ok <- !is.na(lat) & !is.na(lon)
        if (any(ok)) {
          pts <- sf::st_as_sf(
            data.frame(lon = lon[ok], lat = lat[ok]),
            coords = c("lon", "lat"),
            crs = epsg_in
          )



          pts2 <- tryCatch(sf::st_transform(pts, crs = epsg_target), error = function(e) e)



          if (inherits(pts2, "error")) {
            idx <- which(ok)
            for (i in idx) {
              add_issue(i, "decimalLatitude/decimalLongitude", "reproject_failed",
                        "ERROR",
                        paste0("Failed to reproject to EPSG:[", epsg_target, "]: ", pts2$message))
            }
          } else {
            coords <- sf::st_coordinates(pts2)
            lon[ok] <- coords[, "X"]
            lat[ok] <- coords[, "Y"]



            idx <- which(ok)
            for (i in idx) {
              add_issue(i, "decimalLatitude/decimalLongitude", "reproject_to_target",
                        "INFO",
                        paste0("Reprojected from EPSG:[", epsg_in, "] to EPSG:[", epsg_target, "]."))
            }
          }
        }
      } else {
        idx <- which(!is.na(lat) & !is.na(lon))
        if (length(idx) > 0) {
          for (i in idx) {
            add_issue(i, "decimalLatitude/decimalLongitude", "reproject_missing_pkg",
                      "ERROR", "The 'sf' package is required to reproject coordinates.")
          }
        }
      }
    }



    if (!is.na(epsg_target) && epsg_target == 4326) {
      oor_lat2 <- !is.na(lat) & (lat < -90 | lat > 90)
      oor_lon2 <- !is.na(lon) & (lon < -180 | lon > 180)
      if (any(oor_lat2 | oor_lon2)) {
        idx <- which(oor_lat2 | oor_lon2)
        for (i in idx) {
          add_issue(i, "decimalLatitude/decimalLongitude", "out_of_range_after",
                    "ERROR", "Coordinates outside the valid range after conversion/reprojection.")
        }
      }
    }



    out$decimalLatitude <- lat
    out$decimalLongitude <- lon
  }



  if (isTRUE(create_geodetic_datum)) {
    if (!("geodeticDatum" %in% names(out))) {
      out$geodeticDatum <- NA_character_
      add_issue(
        NA_integer_,
        "geodeticDatum",
        "geodeticDatum_created",
        "INFO",
        "geodeticDatum did not exist and was created."
      )
    }



    idx_fill <- is.na(out$geodeticDatum) | trimws(as.character(out$geodeticDatum)) == ""
    if (any(idx_fill)) {
      out$geodeticDatum[idx_fill] <- geodetic_datum_value
      add_issue(
        NA_integer_,
        "geodeticDatum",
        "geodeticDatum_filled",
        "INFO",
        paste0("geodeticDatum filled with '", geodetic_datum_value, "'.")
      )
    }
  }



  # ------------------------------------------------------------
  # 4) Depth validation
  # ------------------------------------------------------------
  has_min <- "minimumDepthInMeters" %in% names(out)
  has_max <- "maximumDepthInMeters" %in% names(out)



  dmin <- rep(NA_real_, nrow(out))
  dmax <- rep(NA_real_, nrow(out))



  if (has_min || has_max) {
    dmin <- if (has_min) suppressWarnings(as.numeric(out$minimumDepthInMeters)) else rep(NA_real_, nrow(out))
    dmax <- if (has_max) suppressWarnings(as.numeric(out$maximumDepthInMeters)) else rep(NA_real_, nrow(out))



    bad <- !is.na(dmin) & !is.na(dmax) & dmin > dmax
    if (any(bad)) {
      idx <- which(bad)
      for (i in idx) {
        add_issue(i, "minimumDepthInMeters/maximumDepthInMeters", "depth_inconsistent",
                  "ERROR", "minimumDepthInMeters > maximumDepthInMeters.")
      }
    }



    neg_depth <- (!is.na(dmin) & dmin < 0) | (!is.na(dmax) & dmax < 0)
    if (any(neg_depth)) {
      idx <- which(neg_depth)
      for (i in idx) {
        add_issue(i, "minimumDepthInMeters/maximumDepthInMeters", "depth_negative",
                  "WARNING", "Negative depth found. Review the dataset convention.")
      }
    }



    if (has_min) out$minimumDepthInMeters <- dmin
    if (has_max) out$maximumDepthInMeters <- dmax
  }



  bathy_obj <- bathy
  can_bathy <- requireNamespace("marmap", quietly = TRUE)



  need_bathy_check <- can_bathy &&
    any(!is.na(lat) & !is.na(lon)) &&
    (any(!is.na(dmin)) || any(!is.na(dmax)))



  if (need_bathy_check && is.null(bathy_obj) && isTRUE(bathy_auto_download)) {
    ok <- !is.na(lat) & !is.na(lon)
    if (sum(ok) > 0) {
      min_lon <- floor(min(lon[ok], na.rm = TRUE)) - 1
      max_lon <- ceiling(max(lon[ok], na.rm = TRUE)) + 1
      min_lat <- floor(min(lat[ok], na.rm = TRUE)) - 1
      max_lat <- ceiling(max(lat[ok], na.rm = TRUE)) + 1



      bbox_ok <- is.finite(min_lon) && is.finite(max_lon) &&
        is.finite(min_lat) && is.finite(max_lat) &&
        (max_lon - min_lon) <= 10 && (max_lat - min_lat) <= 10



      if (bbox_ok) {
        bathy_try <- tryCatch(
          marmap::getNOAA.bathy(
            lon1 = min_lon, lon2 = max_lon,
            lat1 = min_lat, lat2 = max_lat,
            resolution = 1,
            keep = TRUE
          ),
          error = function(e) e
        )



        if (inherits(bathy_try, "error")) {
          bathy_obj <- NULL
          add_issue(
            NA_integer_,
            "bathymetry",
            "bathymetry_download_failed",
            "WARNING",
            paste0("Failed to automatically download bathymetry: ", bathy_try$message)
          )
        } else {
          bathy_obj <- bathy_try
          add_issue(
            NA_integer_,
            "bathymetry",
            "bathymetry_downloaded",
            "INFO",
            "Bathymetry automatically downloaded with marmap."
          )
        }
      } else {
        add_issue(
          NA_integer_,
          "bathymetry",
          "bathymetry_bbox_too_large",
          "WARNING",
          "Automatic bathymetry cross-reference was skipped because the dataset extent is too large."
        )
      }
    }
  }



  if (need_bathy_check && !is.null(bathy_obj) && can_bathy) {
    ok <- !is.na(lat) & !is.na(lon)
    for (i in which(ok)) {
      bathy_depth <- tryCatch(
        marmap::get.depth(bathy_obj, x = lon[i], y = lat[i]),
        error = function(e) NA_real_
      )



      bathy_depth <- suppressWarnings(as.numeric(bathy_depth))
      if (!is.finite(bathy_depth)) next



      reported_depth <- NA_real_
      if (!is.na(dmin[i]) && !is.na(dmax[i])) {
        reported_depth <- mean(c(dmin[i], dmax[i]))
      } else if (!is.na(dmin[i])) {
        reported_depth <- dmin[i]
      } else if (!is.na(dmax[i])) {
        reported_depth <- dmax[i]
      }



      if (!is.na(reported_depth)) {
        bathy_positive <- abs(bathy_depth)



        if (abs(reported_depth - bathy_positive) > bathy_depth_tolerance_m) {
          add_issue(
            i,
            "minimumDepthInMeters/maximumDepthInMeters",
            "depth_bathymetry_mismatch",
            "WARNING",
            paste0(
              "Reported depth (~", round(reported_depth, 1),
              " m) differs from bathymetry (~", round(bathy_positive, 1),
              " m) above the tolerance of ", bathy_depth_tolerance_m, " m."
            )
          )
        }
      }
    }
  }



  # ------------------------------------------------------------
  # 5) Controlled vocabulary standardization
  # ------------------------------------------------------------
  if ("basisOfRecord" %in% names(out)) {
    x <- tolower(trimws(as.character(out$basisOfRecord)))
    x <- gsub("[ _-]+", "", x)
    x0 <- as.character(out$basisOfRecord)



    map <- c(
      "materialentity" = "MaterialEntity",
      "preservedspecimen" = "PreservedSpecimen",
      "fossilspecimen" = "FossilSpecimen",
      "livingspecimen" = "LivingSpecimen",
      "materialsample" = "MaterialSample",
      "event" = "Event",
      "humanobservation" = "HumanObservation",
      "observation" = "HumanObservation",
      "fieldobservation" = "HumanObservation",
      "visualobservation" = "HumanObservation",
      "machineobservation" = "MachineObservation",
      "sensorobservation" = "MachineObservation",
      "cameraobservation" = "MachineObservation",
      "taxon" = "Taxon",
      "occurrence" = "Occurrence",
      "materialcitation" = "MaterialCitation",
      "museumspecimen" = "PreservedSpecimen",
      "voucher" = "PreservedSpecimen",
      "livingcollection" = "LivingSpecimen",
      "sample" = "MaterialSample",
      "genomicsample" = "MaterialSample"
    )



    y <- x
    hit <- x %in% names(map)
    y[hit] <- unname(map[x[hit]])



    out$basisOfRecord <- y



    changed <- !is.na(x0) & x0 != y
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) {
        add_issue(i, "basisOfRecord", "vocab_standardize",
                  "INFO", "basisOfRecord standardized (controlled vocabulary).")
      }
    }
  }



  if ("sex" %in% names(out)) {
    x <- tolower(trimws(as.character(out$sex)))
    x <- gsub("[ _-]+", "", x)
    x0 <- as.character(out$sex)



    map <- c(
      "m" = "male",
      "male" = "male",
      "masculino" = "male",
      "man" = "male",
      "f" = "female",
      "female" = "female",
      "feminino" = "female",
      "woman" = "female",
      "hermaphrodite" = "hermaphrodite",
      "intersex" = "intersex",
      "unknown" = "unknown",
      "unk" = "unknown",
      "indeterminate" = "unknown",
      "undetermined" = "unknown",
      "na" = "unknown",
      "n/a" = "unknown"
    )



    y <- x
    hit <- x %in% names(map)
    y[hit] <- unname(map[x[hit]])
    out$sex <- y



    changed <- !is.na(x0) & x0 != y
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) {
        add_issue(i, "sex", "vocab_standardize",
                  "INFO", "sex normalized.")
      }
    }
  }



  if ("lifeStage" %in% names(out)) {
    x <- tolower(trimws(as.character(out$lifeStage)))
    x <- gsub("[ _-]+", "", x)
    x0 <- as.character(out$lifeStage)



    map <- c(
      "egg" = "egg",
      "ova" = "egg",
      "embryo" = "embryo",
      "larva" = "larva",
      "larvae" = "larva",
      "juvenile" = "juvenile",
      "juv" = "juvenile",
      "subadult" = "subadult",
      "adult" = "adult",
      "immature" = "immature",
      "mature" = "mature",
      "nauplius" = "nauplius",
      "copepodite" = "copepodite",
      "pupa" = "pupa",
      "pupal" = "pupa",
      "seed" = "seed",
      "seedling" = "seedling",
      "spore" = "spore",
      "gametophyte" = "gametophyte",
      "sporophyte" = "sporophyte",
      "unknown" = "unknown",
      "unk" = "unknown"
    )



    y <- x
    hit <- x %in% names(map)
    y[hit] <- unname(map[x[hit]])
    out$lifeStage <- y



    changed <- !is.na(x0) & x0 != y
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) {
        add_issue(i, "lifeStage", "vocab_standardize",
                  "INFO", "lifeStage standardized")
      }
    }
  }



  # ------------------------------------------------------------
  # 6) Duplicate detection
  # ------------------------------------------------------------
  if (nrow(out) > 1) {
    dup <- duplicated(out)
    if (any(dup)) {
      idx <- which(dup)
      for (i in idx) {
        add_issue(i, "row", "duplicate_exact",
                  "WARNING", "Exact duplicate row detected.")
      }
    }
  }



  if (nrow(issues) == 0) {
    summary <- data.frame(severity = character(), n = integer(), stringsAsFactors = FALSE)
  } else {
    summary <- as.data.frame(table(issues$severity), stringsAsFactors = FALSE)
    names(summary) <- c("severity", "n")
  }



  list(data = out, issues = issues, summary = summary)
}



# ---- internal: null coalesce ----
`%||%` <- function(x, y) if (is.null(x)) y else x



.looks_like_datetime <- function(s) {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(FALSE)



  grepl(
    "^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}(:\\d{2})?(\\.\\d+)?([zZ]|[+-]\\d{2}:?\\d{2})?$",
    s
  ) ||
    grepl(
      "^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}[ T]\\d{1,2}:\\d{2}(:\\d{2})?(\\.\\d+)?$",
      s
    )
}



.format_posix_iso <- function(x) {
  if (inherits(x, c("POSIXct", "POSIXt")) && !is.na(x)) {
    return(format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  NA_character_
}



.standardize_event_date_value <- function(s, ambiguous_date_order = "dmy") {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(NA_character_)



  ambiguous_date_order <- tolower(trimws(as.character(ambiguous_date_order %||% "dmy")))
  if (!(ambiguous_date_order %in% c("dmy", "mdy"))) {
    ambiguous_date_order <- "dmy"
  }



  # Keep already valid reduced ISO forms
  if (grepl("^\\d{4}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$", s)) return(s)



  # Month/year like 01/2033
  if (grepl("^\\d{1,2}/\\d{4}$", s)) {
    p <- strsplit(s, "/", fixed = TRUE)[[1]]
    mm <- suppressWarnings(as.integer(p[1]))
    yy <- suppressWarnings(as.integer(p[2]))
    if (!is.na(mm) && !is.na(yy) && mm >= 1 && mm <= 12) {
      return(sprintf("%04d-%02d", yy, mm))
    }
  }



  # Possible date interval: only treat as interval if both sides look date-like
  if (grepl("/", s, fixed = TRUE) && !.looks_like_datetime(s)) {
    parts <- strsplit(s, "/", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    if (length(parts) == 2 &&
        .is_date_like_token(parts[1]) &&
        .is_date_like_token(parts[2])) {
      a <- .parse_single_date_with_parsedate(parts[1], ambiguous_date_order = ambiguous_date_order)
      b <- .parse_single_date_with_parsedate(parts[2], ambiguous_date_order = ambiguous_date_order)
      if (!is.na(a) && !is.na(b)) {
        return(paste0(a, "/", b))
      }
    }
  }



  .parse_single_date_with_parsedate(s, ambiguous_date_order = ambiguous_date_order)
}



.is_date_like_token <- function(s) {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(FALSE)



  grepl("^\\d{4}$", s) ||
    grepl("^\\d{4}-\\d{2}$", s) ||
    grepl("^\\d{4}-\\d{2}-\\d{2}$", s) ||
    grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{4}$", s) ||
    grepl("^\\d{1,2}\\s+[A-Za-z]{3,9}\\s+\\d{4}$", s) ||
    grepl("^[A-Za-z]{3,9}\\s+\\d{1,2},?\\s+\\d{4}$", s)
}



.parse_single_date_with_parsedate <- function(s, ambiguous_date_order = "dmy") {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(NA_character_)



  ambiguous_date_order <- tolower(trimws(as.character(ambiguous_date_order %||% "dmy")))
  if (!(ambiguous_date_order %in% c("dmy", "mdy"))) {
    ambiguous_date_order <- "dmy"
  }



  # Keep already valid reduced ISO forms
  if (grepl("^\\d{4}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(s)



  # Explicit handling of ambiguous numeric dates before parser guessing
  if (grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{4}$", s)) {
    sep <- if (grepl("/", s, fixed = TRUE)) "/" else "-"
    p <- strsplit(s, sep, fixed = TRUE)[[1]]



    a <- suppressWarnings(as.integer(p[1]))
    b <- suppressWarnings(as.integer(p[2]))
    y <- suppressWarnings(as.integer(p[3]))



    if (!is.na(a) && !is.na(b) && !is.na(y)) {
      if (a > 12 && b >= 1 && b <= 12) {
        return(sprintf("%04d-%02d-%02d", y, b, a))
      }
      if (b > 12 && a >= 1 && a <= 12) {
        return(sprintf("%04d-%02d-%02d", y, a, b))
      }
      if (identical(ambiguous_date_order, "dmy")) {
        return(sprintf("%04d-%02d-%02d", y, b, a))
      } else {
        return(sprintf("%04d-%02d-%02d", y, a, b))
      }
    }
  }



  # Explicit handling of "2 Feb 2023" / "02 February 2023"
  if (grepl("^\\d{1,2}\\s+[A-Za-z]{3,9}\\s+\\d{4}$", s)) {
    # Try lubridate first: it is designed to handle heterogeneous orders
    if (requireNamespace("lubridate", quietly = TRUE)) {
      dt <- suppressWarnings(tryCatch(
        lubridate::parse_date_time(
          s,
          orders = c("d b Y", "d B Y"),
          exact = FALSE,
          quiet = TRUE,
          locale = "C",
          tz = "UTC"
        ),
        error = function(e) NA
      ))



      if (inherits(dt, c("POSIXct", "POSIXt")) && !is.na(dt)) {
        return(format(as.Date(dt, tz = "UTC"), "%Y-%m-%d"))
      }
    }



    # Base R fallback
    dt <- suppressWarnings(tryCatch(
      strptime(s, format = "%d %b %Y", tz = "UTC"),
      error = function(e) NA
    ))
    if (inherits(dt, "POSIXlt") && !is.na(dt)) {
      return(format(as.Date(dt), "%Y-%m-%d"))
    }



    dt <- suppressWarnings(tryCatch(
      strptime(s, format = "%d %B %Y", tz = "UTC"),
      error = function(e) NA
    ))
    if (inherits(dt, "POSIXlt") && !is.na(dt)) {
      return(format(as.Date(dt), "%Y-%m-%d"))
    }
  }



  # Explicit handling of "Feb 2 2023" / "February 2, 2023"
  if (grepl("^[A-Za-z]{3,9}\\s+\\d{1,2},?\\s+\\d{4}$", s)) {
    if (requireNamespace("lubridate", quietly = TRUE)) {
      dt <- suppressWarnings(tryCatch(
        lubridate::parse_date_time(
          s,
          orders = c("b d Y", "B d Y"),
          exact = FALSE,
          quiet = TRUE,
          locale = "C",
          tz = "UTC"
        ),
        error = function(e) NA
      ))



      if (inherits(dt, c("POSIXct", "POSIXt")) && !is.na(dt)) {
        return(format(as.Date(dt, tz = "UTC"), "%Y-%m-%d"))
      }
    }



    dt <- suppressWarnings(tryCatch(
      strptime(s, format = "%b %d %Y", tz = "UTC"),
      error = function(e) NA
    ))
    if (inherits(dt, "POSIXlt") && !is.na(dt)) {
      return(format(as.Date(dt), "%Y-%m-%d"))
    }



    dt <- suppressWarnings(tryCatch(
      strptime(s, format = "%B %d %Y", tz = "UTC"),
      error = function(e) NA
    ))
    if (inherits(dt, "POSIXlt") && !is.na(dt)) {
      return(format(as.Date(dt), "%Y-%m-%d"))
    }



    dt <- suppressWarnings(tryCatch(
      strptime(s, format = "%b %d, %Y", tz = "UTC"),
      error = function(e) NA
    ))
    if (inherits(dt, "POSIXlt") && !is.na(dt)) {
      return(format(as.Date(dt), "%Y-%m-%d"))
    }



    dt <- suppressWarnings(tryCatch(
      strptime(s, format = "%B %d, %Y", tz = "UTC"),
      error = function(e) NA
    ))
    if (inherits(dt, "POSIXlt") && !is.na(dt)) {
      return(format(as.Date(dt), "%Y-%m-%d"))
    }
  }



  # parsedate main path
  if (requireNamespace("parsedate", quietly = TRUE)) {
    s_norm <- gsub("\\s+", " ", s)
    s_norm <- sub("Z$", "+00:00", s_norm, ignore.case = FALSE)
    s_norm <- sub("([+-]\\d{2})(\\d{2})$", "\\1:\\2", s_norm)



    dt <- suppressWarnings(tryCatch(
      parsedate::parse_date(s_norm, approx = FALSE),
      error = function(e) NA
    ))



    if (inherits(dt, c("POSIXct", "POSIXt")) && !is.na(dt)) {
      has_time <- grepl("[T ]\\d{1,2}:\\d{2}", s_norm) ||
        grepl("^\\d{4}-\\d{2}-\\d{2}T", s_norm)



      if (has_time) {
        return(format(dt, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
      } else {
        return(format(as.Date(dt, tz = "UTC"), "%Y-%m-%d"))
      }
    }
  }



  NA_character_
}