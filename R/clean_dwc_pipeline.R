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
                               swap_auto_fixes = FALSE,
                               bathy_auto_download = TRUE,
                               bathy_depth_tolerance_m = 200,
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

.normalize_coord_string <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_

  if (!length(x)) return(x)

  x <- gsub("\u00BA|\u02DA", "°", x, perl = TRUE)
  x <- gsub("[\u2018\u2019\u2032\u00B4\u0060\u02B9]", "'", x, perl = TRUE)
  x <- gsub("[\u201C\u201D\u2033]", "\"", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)

  x
}

.parse_dms_manual <- function(x, type = c("lat", "lon")) {
  type <- match.arg(type)
  x0 <- .normalize_coord_string(x)
  out_num <- rep(NA_real_, length(x0))

  if (!length(x0)) return(out_num)

  for (i in seq_along(x0)) {
    s <- x0[i]
    if (is.na(s) || !nzchar(s)) next

    s2 <- toupper(gsub("\\s+", "", s))

    hemi <- NA_character_
    if (grepl("^[NSEW]", s2)) {
      hemi <- sub("^([NSEW]).*$", "\\1", s2)
      s2 <- sub("^[NSEW]", "", s2)
    } else if (grepl("[NSEW]$", s2)) {
      hemi <- sub("^.*([NSEW])$", "\\1", s2)
      s2 <- sub("([NSEW])$", "", s2)
    }

    m_dms <- regexec("^([+-]?[0-9]+)°([0-9]+)'([0-9]+(?:\\.[0-9]+)?)\"?$", s2, perl = TRUE)
    p_dms <- regmatches(s2, m_dms)[[1]]

    if (length(p_dms) == 4) {
      deg <- suppressWarnings(as.numeric(p_dms[2]))
      minv <- suppressWarnings(as.numeric(p_dms[3]))
      secv <- suppressWarnings(as.numeric(p_dms[4]))

      if (!is.na(deg) && !is.na(minv) && !is.na(secv)) {
        sign_mult <- if (deg < 0) -1 else 1
        val <- abs(deg) + minv / 60 + secv / 3600

        if (!is.na(hemi)) {
          if (hemi %in% c("S", "W")) sign_mult <- -1
          if (hemi %in% c("N", "E")) sign_mult <- 1
        }

        val <- val * sign_mult

        if (type == "lat" && val >= -90 && val <= 90) {
          out_num[i] <- val
        }
        if (type == "lon" && val >= -180 && val <= 180) {
          out_num[i] <- val
        }
        next
      }
    }

    m_ddm <- regexec("^([+-]?[0-9]+)°([0-9]+(?:\\.[0-9]+)?)'?$", s2, perl = TRUE)
    p_ddm <- regmatches(s2, m_ddm)[[1]]

    if (length(p_ddm) == 3) {
      deg <- suppressWarnings(as.numeric(p_ddm[2]))
      minv <- suppressWarnings(as.numeric(p_ddm[3]))

      if (!is.na(deg) && !is.na(minv)) {
        sign_mult <- if (deg < 0) -1 else 1
        val <- abs(deg) + minv / 60

        if (!is.na(hemi)) {
          if (hemi %in% c("S", "W")) sign_mult <- -1
          if (hemi %in% c("N", "E")) sign_mult <- 1
        }

        val <- val * sign_mult

        if (type == "lat" && val >= -90 && val <= 90) {
          out_num[i] <- val
        }
        if (type == "lon" && val >= -180 && val <= 180) {
          out_num[i] <- val
        }
        next
      }
    }
  }

  out_num
}

.parse_coord_fallback <- function(x, type = c("lat", "lon")) {
  type <- match.arg(type)

  x_norm <- .normalize_coord_string(x)
  out_num <- suppressWarnings(as.numeric(x_norm))

  if (requireNamespace("geographiclib", quietly = TRUE)) {
    geo <- tryCatch(
      suppressWarnings(geographiclib::dms_decode(x_norm)),
      error = function(e) NULL
    )

    if (!is.null(geo)) {
      geo_num <- suppressWarnings(as.numeric(geo$angle))
      out_num[is.na(out_num) & !is.na(geo_num)] <- geo_num[is.na(out_num) & !is.na(geo_num)]
    }
  }

  manual_num <- .parse_dms_manual(x_norm, type = type)
  out_num[is.na(out_num) & !is.na(manual_num)] <- manual_num[is.na(out_num) & !is.na(manual_num)]

  if (requireNamespace("parzer", quietly = TRUE)) {
    pz <- if (type == "lat") {
      suppressWarnings(parzer::parse_lat(x_norm))
    } else {
      suppressWarnings(parzer::parse_lon(x_norm))
    }

    pz[is.nan(pz)] <- NA_real_
    out_num[is.na(out_num) & !is.na(pz)] <- pz[is.na(out_num) & !is.na(pz)]
  }

  out_num
}

has_decimal_coords <- has_decimal_lat && has_decimal_lon

if (!has_decimal_coords) {
  lat <- rep(NA_real_, nrow(out))
  lon <- rep(NA_real_, nrow(out))
  dmin <- rep(NA_real_, nrow(out))
  dmax <- rep(NA_real_, nrow(out))

  if (!has_decimal_lat) {
    add_issue(
      NA_integer_,
      "decimalLatitude",
      "coord_missing",
      "INFO",
      "decimalLatitude is missing."
    )
  }

  if (!has_decimal_lon) {
    add_issue(
      NA_integer_,
      "decimalLongitude",
      "coord_missing",
      "INFO",
      "decimalLongitude is missing."
    )
  }

  add_issue(
    NA_integer_,
    "decimalLatitude/decimalLongitude",
    "coord_original_system_disabled",
    "INFO",
    paste(
      "Original coordinate system functions were disabled because both",
      "decimalLatitude and decimalLongitude are required."
    )
  )

  if (!is.null(coord_epsg_in) && !is.na(suppressWarnings(as.integer(coord_epsg_in)))) {
    add_issue(
      NA_integer_,
      "coord_epsg_in",
      "option_blocked",
      "INFO",
      "Input CRS EPSG was ignored because decimalLatitude and decimalLongitude are not both available."
    )
  }

  if (!is.null(target_epsg) && !is.na(suppressWarnings(as.integer(target_epsg)))) {
    add_issue(
      NA_integer_,
      "target_epsg",
      "option_blocked",
      "INFO",
      "Target CRS transformation was ignored because decimalLatitude and decimalLongitude are not both available."
    )
  }

  if (isTRUE(force_parzer)) {
    add_issue(
      NA_integer_,
      "force_parzer",
      "option_blocked",
      "INFO",
      "force_parzer was disabled because decimalLatitude and decimalLongitude are not both available."
    )
  }

  if (isTRUE(preserve_original_coords)) {
    add_issue(
      NA_integer_,
      "preserve_original_coords",
      "option_blocked",
      "INFO",
      "Preserve original coordinates in verbatimLatitude/verbatimLongitude was disabled because decimalLatitude and decimalLongitude are not both available."
    )
  }

  if (isTRUE(create_geodetic_datum)) {
    add_issue(
      NA_integer_,
      "create_geodetic_datum",
      "option_blocked",
      "INFO",
      "Create/fill geodeticDatum was disabled because decimalLatitude and decimalLongitude are not both available."
    )
  }

  coord_epsg_in <- NA_integer_
  target_epsg <- NA_integer_
  force_parzer <- FALSE
  preserve_original_coords <- FALSE
  create_geodetic_datum <- FALSE

} else {

  lat_raw <- out$decimalLatitude
  lon_raw <- out$decimalLongitude

  lat_chr <- .normalize_coord_string(lat_raw)
  lon_chr <- .normalize_coord_string(lon_raw)

  epsg_in <- suppressWarnings(as.integer(coord_epsg_in))
  epsg_target <- suppressWarnings(as.integer(target_epsg))

  reprojection_needed <- !is.na(epsg_in) &&
    !is.na(epsg_target) &&
    epsg_in != epsg_target

  lat_num_raw <- suppressWarnings(as.numeric(lat_chr))
  lon_num_raw <- suppressWarnings(as.numeric(lon_chr))

  lat <- .parse_coord_fallback(lat_chr, type = "lat")
  lon <- .parse_coord_fallback(lon_chr, type = "lon")

  lat_parsed_idx <- !is.na(lat_chr) & !is.na(lat) &
    (is.na(lat_num_raw) | lat != lat_num_raw)
  lon_parsed_idx <- !is.na(lon_chr) & !is.na(lon) &
    (is.na(lon_num_raw) | lon != lon_num_raw)

  idx_changed <- which(lat_parsed_idx | lon_parsed_idx)
  if (length(idx_changed) > 0) {
    for (i in idx_changed) {
      add_issue(
        i,
        "decimalLatitude/decimalLongitude",
        "coord_parsed",
        "INFO",
        "Coordinates converted to decimal degrees from original values."
      )
    }
  }

  if (!requireNamespace("parzer", quietly = TRUE) &&
      !requireNamespace("geographiclib", quietly = TRUE)) {
    idx_need_pkg <- which(
      (!is.na(lat_chr) & is.na(lat_num_raw)) |
      (!is.na(lon_chr) & is.na(lon_num_raw))
    )

    if (length(idx_need_pkg) > 0) {
      for (i in idx_need_pkg) {
        add_issue(
          i,
          "decimalLatitude/decimalLongitude",
          "coord_parse_missing_pkg",
          "ERROR",
          "Could not fully parse coordinates. Install 'parzer' and/or 'geographiclib'."
        )
      }
    }
  }

  reprojection_idx <- reprojection_needed & !is.na(lat) & !is.na(lon)

  lat_preserve_idx <- lat_parsed_idx | reprojection_idx
  lon_preserve_idx <- lon_parsed_idx | reprojection_idx

  if (isTRUE(preserve_original_coords)) {
    if (!has_vlat && any(lat_preserve_idx)) {
      out$verbatimLatitude <- rep(NA_character_, nrow(out))
      out$verbatimLatitude[lat_preserve_idx] <- lat_chr[lat_preserve_idx]

      idx_created <- which(lat_preserve_idx)
      for (i in idx_created) {
        msg <- paste(
          c(
            if (lat_parsed_idx[i]) {
              "verbatimLatitude created from original decimalLatitude because conversion to decimal degrees was needed."
            },
            if (reprojection_idx[i]) {
              paste0(
                "verbatimLatitude preserved because CRS reprojection from EPSG:[",
                epsg_in, "] to EPSG:[", epsg_target, "] was needed."
              )
            }
          ),
          collapse = " "
        )

        add_issue(
          i,
          "verbatimLatitude",
          "verbatim_created_from_decimalLatitude",
          "INFO",
          msg
        )
      }
    }

    if (!has_vlon && any(lon_preserve_idx)) {
      out$verbatimLongitude <- rep(NA_character_, nrow(out))
      out$verbatimLongitude[lon_preserve_idx] <- lon_chr[lon_preserve_idx]

      idx_created <- which(lon_preserve_idx)
      for (i in idx_created) {
        msg <- paste(
          c(
            if (lon_parsed_idx[i]) {
              "verbatimLongitude created from original decimalLongitude because conversion to decimal degrees was needed."
            },
            if (reprojection_idx[i]) {
              paste0(
                "verbatimLongitude preserved because CRS reprojection from EPSG:[",
                epsg_in, "] to EPSG:[", epsg_target, "] was needed."
              )
            }
          ),
          collapse = " "
        )

        add_issue(
          i,
          "verbatimLongitude",
          "verbatim_created_from_decimalLongitude",
          "INFO",
          msg
        )
      }
    }
  }

  lat_nonempty <- !is.na(lat_chr)
  lon_nonempty <- !is.na(lon_chr)

  bad_lat_non_numeric <- lat_nonempty & is.na(lat)
  bad_lon_non_numeric <- lon_nonempty & is.na(lon)

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
          lat_chr[i], "', longitude='", lon_chr[i], "'."
        )
      )
    }
  }

  miss <- is.na(lat) | is.na(lon)
  if (any(miss)) {
    idx <- which(miss)
    for (i in idx) {
      add_issue(
        i,
        "decimalLatitude/decimalLongitude",
        "empty_coordinates",
        "WARNING",
        "Missing coordinates (lat/lon) after parsing attempt."
      )
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

  if (!is.na(epsg_in) && epsg_in == 4326) {
    oor_lat <- !is.na(lat) & (lat < -90 | lat > 90)
    oor_lon <- !is.na(lon) & (lon < -180 | lon > 180)

    if (any(oor_lat | oor_lon)) {
      idx <- which(oor_lat | oor_lon)
      for (i in idx) {
        add_issue(
          i,
          "decimalLatitude/decimalLongitude",
          "out_of_range",
          "ERROR",
          "Coordinates outside the valid range for EPSG:4326."
        )
      }
    }
  }

  if (!is.na(epsg_in) && !is.na(epsg_target) && epsg_in != epsg_target) {
    if (requireNamespace("sf", quietly = TRUE)) {
      ok <- !is.na(lat) & !is.na(lon)

      if (any(ok)) {
        pts <- sf::st_as_sf(
          data.frame(lon = lon[ok], lat = lat[ok]),
          coords = c("lon", "lat"),
          crs = epsg_in
        )

        pts2 <- tryCatch(
          sf::st_transform(pts, crs = epsg_target),
          error = function(e) e
        )

        if (inherits(pts2, "error")) {
          idx <- which(ok)
          for (i in idx) {
            add_issue(
              i,
              "decimalLatitude/decimalLongitude",
              "reproject_failed",
              "ERROR",
              paste0("Failed to reproject to EPSG:[", epsg_target, "]: ", pts2$message)
            )
          }
        } else {
          coords <- sf::st_coordinates(pts2)
          lon[ok] <- coords[, "X"]
          lat[ok] <- coords[, "Y"]

          idx <- which(ok)
          for (i in idx) {
            add_issue(
              i,
              "decimalLatitude/decimalLongitude",
              "reproject_to_target",
              "INFO",
              paste0("Reprojected from EPSG:[", epsg_in, "] to EPSG:[", epsg_target, "].")
            )
          }
        }
      }

    } else {
      idx <- which(!is.na(lat) & !is.na(lon))
      if (length(idx) > 0) {
        for (i in idx) {
          add_issue(
            i,
            "decimalLatitude/decimalLongitude",
            "reproject_missing_pkg",
            "ERROR",
            "The 'sf' package is required to reproject coordinates."
          )
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
        add_issue(
          i,
          "decimalLatitude/decimalLongitude",
          "out_of_range_after",
          "ERROR",
          "Coordinates outside the valid range after conversion/reprojection."
        )
      }
    }
  }

  out$decimalLatitude <- lat
  out$decimalLongitude <- lon

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



  # if (need_bathy_check && is.null(bathy_obj) && isTRUE(bathy_auto_download)) {
  #   ok <- !is.na(lat) & !is.na(lon)
  #   if (sum(ok) > 0) {
  #     min_lon <- floor(min(lon[ok], na.rm = TRUE)) - 1
  #     max_lon <- ceiling(max(lon[ok], na.rm = TRUE)) + 1
  #     min_lat <- floor(min(lat[ok], na.rm = TRUE)) - 1
  #     max_lat <- ceiling(max(lat[ok], na.rm = TRUE)) + 1



  #     bbox_ok <- is.finite(min_lon) && is.finite(max_lon) &&
  #       is.finite(min_lat) && is.finite(max_lat) &&
  #       (max_lon - min_lon) <= 10 && (max_lat - min_lat) <= 10



  #     if (bbox_ok) {
  #       bathy_try <- tryCatch(
  #         marmap::getNOAA.bathy(
  #           lon1 = min_lon, lon2 = max_lon,
  #           lat1 = min_lat, lat2 = max_lat,
  #           resolution = 1,
  #           keep = TRUE
  #         ),
  #         error = function(e) e
  #       )



  #       if (inherits(bathy_try, "error")) {
  #         bathy_obj <- NULL
  #         add_issue(
  #           NA_integer_,
  #           "bathymetry",
  #           "bathymetry_download_failed",
  #           "WARNING",
  #           paste0("Failed to automatically download bathymetry: ", bathy_try$message)
  #         )
  #       } else {
  #         bathy_obj <- bathy_try
  #         add_issue(
  #           NA_integer_,
  #           "bathymetry",
  #           "bathymetry_downloaded",
  #           "INFO",
  #           "Bathymetry automatically downloaded with marmap."
  #         )
  #       }
  #     } else {
  #       add_issue(
  #         NA_integer_,
  #         "bathymetry",
  #         "bathymetry_bbox_too_large",
  #         "WARNING",
  #         "Automatic bathymetry cross-reference was skipped because the dataset extent is too large."
  #       )
  #     }
  #   }
  # }



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


# ------------------------------------------------------------
# 7) Date and time processing
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.normalize_ambiguous_date_order <- function(ambiguous_date_order = "dmy") {
  ambiguous_date_order <- tolower(trimws(as.character(ambiguous_date_order %||% "dmy")))
  if (!ambiguous_date_order %in% c("dmy", "mdy")) {
    ambiguous_date_order <- "dmy"
  }
  ambiguous_date_order
}

# ----------------------------
# CORE ISO 8601 HANDLING (ROBUST)
# ----------------------------
.parse_iso8601 <- function(s) {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(NA_character_)

  if (!grepl("^\\d{4}-\\d{2}-\\d{2}T", s)) return(NA_character_)

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    return(NA_character_)
  }

  # Parse WITHOUT forcing UTC
  dt <- suppressWarnings(lubridate::ymd_hms(s))

  if (is.na(dt)) return(NA_character_)

  # Extract original timezone from string
  tz_str <- regmatches(s, regexpr("(Z|[+-]\\d{2}:?\\d{2})$", s))

  if (length(tz_str) == 0 || tz_str == "") {
    # No timezone provided → return naive ISO
    return(format(dt, "%Y-%m-%dT%H:%M:%S"))
  }

  # Normalize offset format (+0200 → +02:00)
  tz_str <- sub("^([+-]\\d{2})(\\d{2})$", "\\1:\\2", tz_str)

  paste0(format(dt, "%Y-%m-%dT%H:%M:%S"), tz_str)
}

# ----------------------------
# AMBIGUOUS NUMERIC DATES ONLY (dd/mm vs mm/dd)
# ----------------------------
.parse_ambiguous_numeric_date <- function(s, ambiguous_date_order = "dmy") {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(NA_character_)

  ambiguous_date_order <- .normalize_ambiguous_date_order(ambiguous_date_order)

  if (!grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}$", s)) {
    return(NA_character_)
  }

  sep <- if (grepl("/", s, fixed = TRUE)) "/" else "-"
  parts <- strsplit(s, sep, fixed = TRUE)[[1]]

  a <- suppressWarnings(as.integer(parts[1]))
  b <- suppressWarnings(as.integer(parts[2]))
  y <- suppressWarnings(as.integer(parts[3]))

  if (any(is.na(c(a, b, y)))) return(NA_character_)

  # disambiguation logic
  if (a > 12 && b <= 12) {
    return(sprintf("%04d-%02d-%02d", y, b, a))
  }
  if (b > 12 && a <= 12) {
    return(sprintf("%04d-%02d-%02d", y, a, b))
  }

  if (ambiguous_date_order == "dmy") {
    return(sprintf("%04d-%02d-%02d", y, b, a))
  } else {
    return(sprintf("%04d-%02d-%02d", y, a, b))
  }
}

# ----------------------------
# MAIN SINGLE ENTRY PARSER
# ----------------------------
.parse_single_date <- function(s, ambiguous_date_order = "dmy") {

  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(NA_character_)

  ambiguous_date_order <- .normalize_ambiguous_date_order(ambiguous_date_order)

  # 1. ISO 8601 (highest priority)
  iso <- .parse_iso8601(s)
  if (!is.na(iso)) return(iso)

  # 2. Year-only / partial ISO-like tokens (no guessing)
  if (grepl("^\\d{4}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(s)

  # 3. Ambiguous numeric formats (ONLY place where ambiguity is allowed)
  num <- .parse_ambiguous_numeric_date(s, ambiguous_date_order)
  if (!is.na(num)) return(num)

  # 4. Human-readable formats via lubridate (trusted parsing layer)
  if (requireNamespace("lubridate", quietly = TRUE)) {

    dt <- suppressWarnings(lubridate::parse_date_time(
      s,
      orders = c(
        "d b Y", "d B Y",
        "b d Y", "B d Y",
        "dmy", "mdy",
        "Ymd HMS", "Ymd HM"
      ),
      tz = "UTC",
      quiet = TRUE
    ))

    if (inherits(dt, "POSIXct") && !is.na(dt)) {

      # detect if time exists in input
      has_time <- grepl("\\d{1,2}:\\d{2}", s)

      if (has_time) {
        return(format(dt, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
      } else {
        return(format(as.Date(dt, tz = "UTC"), "%Y-%m-%d"))
      }
    }
  }

  # 5. parsedate fallback (last resort)
  if (requireNamespace("parsedate", quietly = TRUE)) {
    s_norm <- gsub("\\s+", " ", s)
    s_norm <- sub("Z$", "+00:00", s_norm)

    dt <- suppressWarnings(parsedate::parse_date(s_norm, approx = FALSE))

    if (inherits(dt, "POSIXct") && !is.na(dt)) {
      return(format(dt, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    }
  }

  NA_character_
}

# ----------------------------
# PUBLIC STANDARDIZER
# ----------------------------
.standardize_event_date_value <- function(s, ambiguous_date_order = "dmy") {
  .parse_single_date(s, ambiguous_date_order)
}

# ----------------------------
# DETECTION HELPERS
# ----------------------------
.looks_like_datetime <- function(s) {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(FALSE)

  grepl("^\\d{4}-\\d{2}-\\d{2}T", s) ||
    grepl("^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}", s) ||
    grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{4}[ T]", s)
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