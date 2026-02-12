# R/clean_dwc_pipeline.R

#' Clean and validate a mapped Darwin Core data.frame
#' @param df data.frame after mapping (DwC column names)
#' @return list(data=cleaned_df, issues=issues_df, summary=summary_df)
#' @export
clean_dwc_pipeline <- function(df,
                               coord_epsg_in = 4326,
                               target_epsg = 4326,
                               force_parzer = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(data = df, issues = data.frame(), summary = data.frame()))
  }

  issues <- data.frame(
    row = integer(),
    field = character(),
    rule = character(),
    severity = character(),  # "ERROR", "WARNING", "INFO"
    message = character(),
    stringsAsFactors = FALSE
  )

  add_issue <- function(idx, field, rule, severity, message) {
    issues <<- rbind(
      issues,
      data.frame(
        row = as.integer(idx),
        field = as.character(field),
        rule = as.character(rule),
        severity = as.character(severity),
        message = as.character(message),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- df

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
      for (i in idx) add_issue(i, "scientificName", "empty_scientificName",
                               "ERROR", "scientificName vazio ou ausente.")
    }

    changed <- !is.na(x0) & x0 != x
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) add_issue(i, "scientificName", "name_trim",
                               "INFO", "scientificName normalizado (trim/espaços).")
    }
  }

  # ------------------------------------------------------------
  # 2) Dates to ISO-8601 (eventDate)
  #    Política: tenta produzir YYYY-MM-DD / YYYY-MM / YYYY ou intervalos start/end
  # ------------------------------------------------------------
  if ("eventDate" %in% names(out)) {
    x <- as.character(out$eventDate)
    x0 <- x

    x <- trimws(x)
    x[x == ""] <- NA_character_

    iso <- vapply(seq_along(x), function(i) {
      s <- x[i]
      if (is.na(s)) return(NA_character_)

      # intervalos já no formato "a/b"
      if (grepl("/", s, fixed = TRUE)) {
        parts <- strsplit(s, "/", fixed = TRUE)[[1]]
        parts <- trimws(parts)
        if (length(parts) == 2) {
          a <- .coerce_date_iso(parts[1])
          b <- .coerce_date_iso(parts[2])
          if (!is.na(a) && !is.na(b)) return(paste0(a, "/", b))
        }
        return(NA_character_)
      }

      .coerce_date_iso(s)
    }, character(1))

    out$eventDate <- iso

    bad <- !is.na(x) & is.na(iso)
    if (any(bad)) {
      idx <- which(bad)
      for (i in idx) add_issue(i, "eventDate", "date_parse",
                              "WARNING", paste0("Não foi possível normalizar eventDate: '", x[i], "'."))
    }

    changed <- !is.na(x0) & !is.na(iso) & x0 != iso
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) add_issue(i, "eventDate", "date_iso",
                              "INFO", "eventDate normalizado para ISO-8601.")
    }
  }

  # ------------------------------------------------------------
  # 3) Coordinate cleanup (decimalLatitude/decimalLongitude)
  #    Regras:
  #    - decimalLatitude/decimalLongitude são obrigatórios -> criar se não existirem
  #    - se verbatimLatitude/verbatimLongitude existirem, usar como fonte para preencher decimals quando necessário
  #    - se decimal* existirem mas estiverem em texto/DMS, converter via parzer
  #    - reprojetar para EPSG:[4326] quando epsg_in != 4326 (usando sf)
  # ------------------------------------------------------------

  # garantir colunas obrigatórias (criar se necessário)
  if (!("decimalLatitude" %in% names(out))) {
    out$decimalLatitude <- NA_real_
    add_issue(NA_integer_, "decimalLatitude", "coord_missing_created",
              "INFO", "decimalLatitude não existia e foi criada.")
  }
  if (!("decimalLongitude" %in% names(out))) {
    out$decimalLongitude <- NA_real_
    add_issue(NA_integer_, "decimalLongitude", "coord_missing_created",
              "INFO", "decimalLongitude não existia e foi criada.")
  }

  has_vlat <- "verbatimLatitude" %in% names(out)
  has_vlon <- "verbatimLongitude" %in% names(out)

  lat_raw <- out$decimalLatitude
  lon_raw <- out$decimalLongitude

  vlat_raw <- if (has_vlat) out$verbatimLatitude else rep(NA, nrow(out))
  vlon_raw <- if (has_vlon) out$verbatimLongitude else rep(NA, nrow(out))

  # 1) tentar numérico direto
  lat_num <- suppressWarnings(as.numeric(lat_raw))
  lon_num <- suppressWarnings(as.numeric(lon_raw))

  # candidatos para parsing:
  # - se verbatim existir e tiver conteúdo, prefere verbatim
  # - senão, usa o próprio decimal* (caso esteja em texto/DMS)
  vlat_chr <- trimws(as.character(vlat_raw))
  vlon_chr <- trimws(as.character(vlon_raw))
  lat_chr  <- trimws(as.character(lat_raw))
  lon_chr  <- trimws(as.character(lon_raw))

  use_vlat <- !is.na(vlat_chr) & vlat_chr != ""
  use_vlon <- !is.na(vlon_chr) & vlon_chr != ""

  lat_candidate <- ifelse(use_vlat, vlat_chr, lat_chr)
  lon_candidate <- ifelse(use_vlon, vlon_chr, lon_chr)

  # decidir se precisa de parzer
  need_parse_lat <- force_parzer || any(is.na(lat_num) & lat_candidate != "" & !is.na(lat_candidate))
  need_parse_lon <- force_parzer || any(is.na(lon_num) & lon_candidate != "" & !is.na(lon_candidate))

  lat <- lat_num
  lon <- lon_num

  if ((need_parse_lat || need_parse_lon) && requireNamespace("parzer", quietly = TRUE)) {
    lat_parsed <- suppressWarnings(parzer::parse_lat(lat_candidate))
    lon_parsed <- suppressWarnings(parzer::parse_lon(lon_candidate))

    # preencher onde está NA numérico
    lat[is.na(lat) & !is.na(lat_parsed)] <- lat_parsed[is.na(lat) & !is.na(lat_parsed)]
    lon[is.na(lon) & !is.na(lon_parsed)] <- lon_parsed[is.na(lon) & !is.na(lon_parsed)]

    idx_changed <- which(
      (is.na(lat_num) & !is.na(lat_parsed) & lat_candidate != "") |
        (is.na(lon_num) & !is.na(lon_parsed) & lon_candidate != "")
    )
    if (length(idx_changed) > 0) {
      for (i in idx_changed) {
        add_issue(i, "decimalLatitude/decimalLongitude", "coord_parzer",
                  "INFO", "Coordenadas convertidas para decimal degrees (parzer) a partir de verbatim e/ou texto.")
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
                  "ERROR", "Não foi possível converter coordenadas. Instala o pacote 'parzer'.")
      }
    }
  }

  # 2) Missing coords
  miss <- (is.na(lat) | is.na(lon))
  if (any(miss)) {
    idx <- which(miss)
    for (i in idx) {
      add_issue(i, "decimalLatitude/decimalLongitude", "empty_coordinates",
                "WARNING", "Coordenadas ausentes (lat/lon) após tentativa de parsing.")
    }
  }

  # 3) Range validation (antes de reprojetar, só é confiável se epsg_in == 4326)
  epsg_in <- suppressWarnings(as.integer(coord_epsg_in))
  epsg_target <- suppressWarnings(as.integer(target_epsg))

  if (!is.na(epsg_in) && epsg_in == 4326) {
    oor_lat <- !is.na(lat) & (lat < -90 | lat > 90)
    oor_lon <- !is.na(lon) & (lon < -180 | lon > 180)

    if (any(oor_lat | oor_lon)) {
      idx <- which(oor_lat | oor_lon)
      for (i in idx) {
        add_issue(i, "decimalLatitude/decimalLongitude", "out_of_range",
                  "ERROR", "Coordenadas fora do intervalo válido para EPSG:[4326].")
      }
    }
  }

  # 4) Reproject to target EPSG (default 4326) if needed
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
                      "ERROR", paste0("Falha ao reprojetar para EPSG:[", epsg_target, "]: ", pts2$message))
          }
        } else {
          coords <- sf::st_coordinates(pts2)
          lon[ok] <- coords[, "X"]
          lat[ok] <- coords[, "Y"]

          idx <- which(ok)
          for (i in idx) {
            add_issue(i, "decimalLatitude/decimalLongitude", "reproject_to_target",
                      "INFO", paste0("Reprojetado de EPSG:[", epsg_in, "] para EPSG:[", epsg_target, "]."))
          }
        }
      }
    } else {
      idx <- which(!is.na(lat) & !is.na(lon))
      if (length(idx) > 0) {
        for (i in idx) {
          add_issue(i, "decimalLatitude/decimalLongitude", "reproject_missing_pkg",
                    "ERROR", "Para reprojetar CRS é necessário instalar o pacote 'sf'.")
        }
      }
    }
  }

  # 5) Final range validation (agora deve estar em target EPSG, tipicamente 4326)
  if (!is.na(epsg_target) && epsg_target == 4326) {
    oor_lat2 <- !is.na(lat) & (lat < -90 | lat > 90)
    oor_lon2 <- !is.na(lon) & (lon < -180 | lon > 180)
    if (any(oor_lat2 | oor_lon2)) {
      idx <- which(oor_lat2 | oor_lon2)
      for (i in idx) {
        add_issue(i, "decimalLatitude/decimalLongitude", "out_of_range_after",
                  "ERROR", "Coordenadas fora do intervalo válido após conversão/reprojeção.")
      }
    }
  }

  out$decimalLatitude <- lat
  out$decimalLongitude <- lon

  # ------------------------------------------------------------
  # 4) Depth validation (minimumDepthInMeters/maximumDepthInMeters)
  # ------------------------------------------------------------
  has_min <- "minimumDepthInMeters" %in% names(out)
  has_max <- "maximumDepthInMeters" %in% names(out)

  if (has_min || has_max) {
    dmin <- if (has_min) suppressWarnings(as.numeric(out$minimumDepthInMeters)) else rep(NA_real_, nrow(out))
    dmax <- if (has_max) suppressWarnings(as.numeric(out$maximumDepthInMeters)) else rep(NA_real_, nrow(out))

    bad <- !is.na(dmin) & !is.na(dmax) & dmin > dmax
    if (any(bad)) {
      idx <- which(bad)
      for (i in idx) add_issue(i, "minimumDepthInMeters/maximumDepthInMeters", "depth_inconsistent",
                              "ERROR", "minimumDepthInMeters > maximumDepthInMeters.")
    }

    if (has_min) out$minimumDepthInMeters <- dmin
    if (has_max) out$maximumDepthInMeters <- dmax
  }

  # ------------------------------------------------------------
  # 5) Controlled vocab standardization (basisOfRecord, sex, lifeStage)
  # ------------------------------------------------------------
  if ("basisOfRecord" %in% names(out)) {
    x <- tolower(trimws(as.character(out$basisOfRecord)))
    x0 <- out$basisOfRecord

    # exemplo mínimo (podes expandir)
    map <- c(
      "humanobservation" = "HumanObservation",
      "machineobservation" = "MachineObservation",
      "preservedspecimen" = "PreservedSpecimen",
      "fossilspecimen" = "FossilSpecimen",
      "living_specimen" = "LivingSpecimen",
      "observation" = "HumanObservation"
    )

    y <- x
    hit <- x %in% names(map)
    y[hit] <- unname(map[x[hit]])

    out$basisOfRecord <- y

    changed <- !is.na(x0) & x0 != y
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) add_issue(i, "basisOfRecord", "vocab_standardize",
                              "INFO", "basisOfRecord normalizado (vocabulário controlado).")
    }
  }

  if ("sex" %in% names(out)) {
    x <- tolower(trimws(as.character(out$sex)))
    x0 <- out$sex
    map <- c("m"="male","male"="male","masculino"="male","f"="female","female"="female","feminino"="female")
    y <- x
    hit <- x %in% names(map)
    y[hit] <- unname(map[x[hit]])
    out$sex <- y

    changed <- !is.na(x0) & x0 != y
    if (any(changed)) {
      idx <- which(changed)
      for (i in idx) add_issue(i, "sex", "vocab_standardize",
                              "INFO", "sex normalizado.")
    }
  }

  # ------------------------------------------------------------
  # 6) Duplicate detection (exact duplicates)
  # ------------------------------------------------------------
  if (nrow(out) > 1) {
    dup <- duplicated(out)
    if (any(dup)) {
      idx <- which(dup)
      for (i in idx) add_issue(i, "row", "duplicate_exact",
                              "WARNING", "Linha duplicada exacta detectada.")
    }
  }

  # summary
  if (nrow(issues) == 0) {
    summary <- data.frame(severity = character(), n = integer(), stringsAsFactors = FALSE)
  } else {
    summary <- as.data.frame(table(issues$severity), stringsAsFactors = FALSE)
    names(summary) <- c("severity", "n")
  }

  list(data = out, issues = issues, summary = summary)
}

# ---- internal: convert single date token to ISO-8601 ----
.coerce_date_iso <- function(s) {
  s <- trimws(s)
  if (!nzchar(s)) return(NA_character_)

  # já ISO
  if (grepl("^\\d{4}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}$", s)) return(s)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(s)

  # tenta com lubridate (silencioso)
  if (requireNamespace("lubridate", quietly = TRUE)) {
    candidates <- c(
      suppressWarnings(lubridate::ymd(s, quiet = TRUE)),
      suppressWarnings(lubridate::dmy(s, quiet = TRUE)),
      suppressWarnings(lubridate::mdy(s, quiet = TRUE))
    )
    candidates <- candidates[!is.na(candidates)]
    if (length(candidates) > 0) {
      return(format(candidates[[1]], "%Y-%m-%d"))
    }
  }

  NA_character_
}
