# Functions GENERIC ------------------------------------------------------------
#' Filters dataframe by a time range
#'
#' Filters a data frame to keep only the data corresponding to a certain time
#' range (between \code{to} and \code{from} (exclude)).
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A data frame with the same variables as \code{df}.
#' @keywords internal
#' @noRd
select_date <- function(df, from, to) {
  if (any(names(df) %in% "month")) {
    df <- transform(df, date = as.Date(paste(df$year, as.numeric(df$month),
                                                01, sep = "-")))
  } else {
    df <- transform(df, date = as.Date(paste(df$year, 01, 01, sep = "-")))
  }
  df <- df[which(df$date >= from & df$date <= to), ]
  df[, which(colnames(df) != "date")]
}


################################################################################
#' Filters and order splits events by a time range
#'
#' Filters a list of splits event to keep only the event corresponding to a
#' certain time range (between \code{to} and \code{from} (exclude)) and order
#' them from the older to the more recent
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A list with the same variables as \code{splits_lst}.
#' @keywords internal
#' @noRd
select_events <- function(splits_lst, from, to) {
  sel0 <- vapply(splits_lst, "[[", 0, 3)
  sel0 <- names(sort(sel0, decreasing = FALSE))
  splits_lst <- splits_lst[sel0]
  sel <- lapply(splits_lst, "[[", 3) > as.Date(from) &
    lapply(splits_lst, "[[", 3) <= as.Date(to)
  splits_lst[sel]
}

################################################################################
#' Vectorises provinces by event
#'
#' Selects the name of the provinces concerned by one or multiple splits events
#' and returns a list with a vector for each event and containing all the
#' province names concerned by this event.
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @return A list of vector, each vector corresponds to one event and contains
#' the province names concerned by this event.
#' @keywords internal
#' @noRd
province_splits <- function(lst_split) {
  provinces <- lapply(names(lst_split), function(x) {
    combined <- unlist(lapply(lst_split[x], "[[", 1))
    elements <- unlist(lapply(lst_split[x], "[[", 2))
    c(combined, elements)
  })
  setNames(provinces, names(lst_split))
}

################################################################################
#' Prepares dataframe
#'
#' Applies for one event on a data frame previously filter to keep only the data
#' of the \code{province} linked to the event.
#' Prepares a data frame who joins all the data together by \code{year} without
#' keeping the \code{province} information.
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @return A data frame with the variables:\code{year}, \code{month},
#' \code{incidence} and \code{mortality}
#' @keywords internal
#' @noRd
prepare_data <- function(df) {
  df <- split(df, as.character(unique(df$province)))
  Reduce(function(x, y) merge(x, y, all = TRUE, by = names(x)), df)
}

################################################################################
#' Does one merging event
#'
#' Applies for one event to a data frame previously filter to keep only the data
#' of the \code{province} linked to the event.
#' Merges multiple provinces together by applying a mathematical function on the
#' data.
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @param FUN A function to apply on the data when merging the province together
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
gather_sum <- function(df, FUN, df2, args, FUN2, ...) {

  # prepare the arguments in a good format
  args2 <- c("value", unlist(args))

  # Prepare the df in a good format and group the data by year and key for the
  # merging event
  df <- reshape(df, varying = "value", direction = "long",
                idvar = names(df)[which(names(df) != "value")],
                v.names = "value", times = "value",
                new.row.names = seq(1, dim(df)[1]))
  df <- df[, which(colnames(df) != "time")]

  if (any(names(df) %in% "month")) {
    df <- split(df, list(df$year, df$month, df$key))
  } else {
    df <- split(df, list(df$year, df$key))
  }

  # if two dfs and the parameter args were provided, apply the merging event on
  # the both of them (with the possibility to apply two different function on
  #  them), if it's not the case, the merging event will be apply only
  # on the first df.
  if (is.data.frame(df2) & is.null(args) == FALSE) {
    df <- lapply(seq_along(df), function(x) {
      tab <- df[[x]]
      tab["value"] <- FUN(tab[, args2[1], drop = TRUE],
                          tab[, args2[2], drop = TRUE], ...)
      tab[args] <- FUN2(tab[, args, drop = TRUE], ...)
      tab
      tab <- tab[, which(names(tab) != "province")]
      unique(tab)
    })
  } else {
      df <- lapply(seq_along(df), function(x) {
        tab <- df[[x]]
        tab["value"] <- FUN(tab[, "value", drop = TRUE], ...)
        tab
        tab <- tab[, which(names(tab) != "province")]
        unique(tab)
      })
  }
  Reduce(rbind, df)
}

################################################################################
#' Merge back together province
#'
#' @param lst  List of data frame named \code{`TRUE`} and \code{`FALSE`} and
#' each should contain at least the variables \code{year}, \code{province}
#' @param names string, new names of the province
#' @param FUN A function to apply on the data when merging the province together
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process.
#' @param ... additional arguments to pass to FUN and FUN2.
#'
#' @return A data frame with the same variables as  the data frames in
#' \code{lst}.
#' @keywords internal
#' @noRd
apply_merge <- function(lst, names, FUN, df2, args, FUN2, ...) {
  lst$`TRUE` <- prepare_data(lst$`TRUE`)
  lst$`TRUE` <- gather_sum(lst$`TRUE`, FUN = FUN, df2 = df2, args = args,
                           FUN2 = FUN2, ...)
  lst$`TRUE` <- transform(lst$`TRUE`, province = names)
  lst <- rbind(lst$`TRUE`, lst$`FALSE`)
  lst$province <- as.character(lst$province)
  lst
}

################################################################################
#' Merging Ha Noi / Ha Son Binh event
#'
#' Applies only if the time range contains the split and the combine event of
#' Ha Noi & Ha Son Binh, does an additional merging on Hanoi and Ha Son Dinh.
#'
#' @param df A  data frame should contain at least the variables \code{year},
#'  \code{province} containing \code{"Ha Noi"} and \code{"Ha Son Binh"}
#' @param FUN A function to apply on the data when merging the province together
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process.
#' @param ... additional arguments to pass to FUN and FUN2.
#'
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
hanoi_function <- function(df, FUN, df2, args, FUN2, ...) {
  tab <- split(df, df$province %in% c("Ha Noi", "Ha Son Binh"))
  tab <- apply_merge(tab, "Ha Noi", FUN = FUN, df2 = df2, args = args,
                     FUN2 = FUN2, ... = ...)
}

################################################################################
#' Merges provinces
#'
#' Merges data accordingly to a time range and by the provinces
#' concerned by a split/combined event and return a data frame for the time
#' range imputed.
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @param FUN A function to apply on the data when merging the province together
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined}, a slot \code{elements} and a slot \code{date}. The first one
#' contains the name of the merged provinces and the second one contains a
#' vector of the names of the provinces to merge.
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process.
#' @param ... additional arguments to pass to FUN and FUN2.
#'
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range) are merged.
#' @keywords internal
#' @noRd
merge_province <- function(df, FUN, from, to, splits_lst,
                           df2, args, FUN2, ...) {
  # select the list of event corresponding at the time range from - to
  lst_events <- select_events(splits_lst, from = from, to = to)

  if (from < as.Date("1992-01-01") & to > as.Date("2008-01-01")) {
    lst_events$`Ha Son Binh`$elements <- c("Hoa Binh", "Ha Noi")
    lst_events$`Ha Son Binh`$combined <- c("Ha Noi")
  }

  # if the list contains some events, merge or split the province concerned
  if (length(lst_events) > 0) {

    for (i in rev(seq_along(lst_events))) {
      province_lst <- province_splits(lst_events[i])
      tmp <- split(df, df$province %in% province_lst[[1]])

      if (length(tmp$`TRUE`) > 0) {

        # Take care of the problem of NA for province before year of creation,
        # some dataset have some for some provinces before their year of
        # creation
        if (anyNA(tmp$`TRUE`) == TRUE &
            sum(!province_lst[[1]] %in% "Ha Tay") /
            length(province_lst[[1]]) == 1) {
          # Add the data of the province existing before the split event to the
          # data frame containing the other province none of interest at this
          # point and filter the data  to keep only the split event data and re-
          # merge them together
          limit <- format(lst_events[i][[1]]$date, "%Y")
          add_df <- tmp$`TRUE`[which(tmp$`TRUE`$year < limit), ]
          add_df <- add_df[add_df$province %in% lst_events[i][[1]]$combined, ]
          tmp$`FALSE` <- rbind(tmp$`FALSE`, add_df)
          tmp$`TRUE` <- tmp$`TRUE`[which(tmp$`TRUE`$year >= limit), ]
        }

        # As Ha Tay is a merging event with Hanoi in 2008, another process is
        # to take care of the problem of the presence of NA for Ha Tay
        # after 2008
        if (anyNA(tmp$`TRUE`) == TRUE &
            sum(!province_lst[[1]] %in% "Ha Tay")
              / length(province_lst[[1]]) != 1) {
          limit <- format(lst_events[i][[1]]$date, "%Y")
          add_df <- tmp$`TRUE`[tmp$`TRUE`$province != "Ha Tay" &
                                 tmp$`TRUE`$year >= limit, ]
          tmp$`FALSE` <- rbind(tmp$`FALSE`, add_df)
          tmp$`TRUE` <- tmp$`TRUE`[which(tmp$`TRUE`$year < limit), ]
        }

        # Merge back together the province
        if (dim(tmp$`TRUE`)[1] > 0) {
          df <- apply_merge(tmp, names(province_lst[1]), FUN = FUN, df2 = df2,
                            args = args, FUN2 = FUN2, ... = ...)
        } else df <- tmp$`FALSE`
      } else df <- tmp$`FALSE`
    }
  } else df

  # if the time range contains the split and the combine event of
  # Ha Noi & Ha Son Binh, does an additional merging on Hanoi and Ha Son Dinh.
  if (from < as.Date("1992-01-01") & to > as.Date("2008-01-01")) {
    df <- hanoi_function(df, FUN, df2 = df2, args = args,
                           FUN2 = FUN2, ... = ...)
  }

  # Problem of Ha Tay, NA value after 2008
  if (from >= as.Date("2008-01-01") & is.element("Ha Tay", df$province)) {
    df <-  df[df$province != "Ha Tay", ]
  }

  return(df)
}


################################################################################
#' Merges provinces
#'
#' Tidy the data and merges data accordingly to a time range and by the
#' provinces concerned by a split/combined event and return a data frame for the
#'  time range imputed.
#'
#' @param df  A data frame containing at least the variables \code{province},
#' \code{year}, accept also the monthly data if the month are in the column
#' \code{month}.
#' @param sel A vector of character to select only the variable to merge.
#'  By default, select all the variables.
#' @param FUN A function to apply on the data when merging the province
#' together. By default, \code{sum}.
#' @param diseases A vector of character used to know which history of Vietnam
#' has to be taken for the merging event as two diseases has different story.
#' Used if you want the same merging event as your infectious disease dataframe,
#'  for more details look at the \code{gdpm package}. By default, \code{NULL}
#' @param from Initial date of the time range selected for the province
#' definition, of the class \code{Date} or \code{numeric}.
#' @param to Final date of the time range selected for the province
#' definition, of the class \code{Date} or \code{numeric}, by default
#'  \code{"2017-12-31"}
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args string vector, column name of the additional arguments
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process. By
#' default \code{sum}
#' @param ... additional arguments to pass to FUN and FUN2.
#'
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range) are merged.
#'
#' @importFrom stats setNames reshape
#'
#' @examples
#'
#' # For all the following examples, we will use the data from the package gso.
#' library(gso) # for the "content"
#' library(magrittr) # for "%>%"
#' library(dplyr)   # for "filter"
#' migration_rate <- gso::content %>% filter(data_name == "demography_12") %>%
#'   .$data %>% .[[1]] %>%
#'   mutate(year = as.numeric(year))
#'
#' # if you want to have the data expressed by province, with the province's
#' # definition of 1992 in Vietnam:
#' merge_prov(migration_rate, from = "1992-01-01")
#' # If you want the province's definition between 1992 and 2010 in Vietnam:
#' merge_prov(migration_rate, from = 1992, to = 2010)
#'
#' # You can change the function
#' merge_prov(migration_rate, from = "1992", FUN = mean)
#'
#'# You can also use weighted mean by providing the weighted in another
#'# data frame
#' pop_size <-  gso::content %>% filter(data_name == "demography_5") %>%
#'   .$data %>% .[[1]] %>% dplyr::select(province, year, total) %>%
#'   mutate(year = as.numeric(year))
#' merge_prov(migration_rate, from = "1992-01-01", FUN = weighted.mean,
#'   df2 = pop_size, args = "total")
#'
#' # You can define the merge_prov function only on certain columns
#' pop_info <- gso::content %>% filter(data_name == "demography_1") %>%
#'   .$data %>% .[[1]] %>%
#'   mutate(year = as.numeric(year))
#' merge_prov(pop_info, sel = "average_population_thous_pers",
#'   from = 1992, FUN = weighted.mean,
#'   df2 = pop_size, args = "total")
#'
#' @export
merge_prov <- function(df, sel = names(df), FUN = sum, from, to = "2017-12-31",
                       diseases = NULL, df2 = NULL, args = NULL, FUN2 = sum,
                       ...) {

  # test df2 format, should be a data frame
  if (!is.null(df2) & !is.data.frame(df2)) {
    stop(paste0("the parameter df2 is not in a good format: ", class(df2),
                ", df2 should be a data frame."))
    }

  # If the df contain at least a column "province" and a column "year",
  # gather and merge the data accordingly with the time serie implemented and
  # the history of Vietnam selected.
  if (length(grep("province|year", names(df))) >= 2) {

    # get from and to in the right format
    from <- as.Date(paste0(from, "-01-01"))
    to <- as.Date(paste0(to, "-12-31"))

    # test year range
    if (from > to ) {
      stop("The time range selected is incorrect (from > to): ",
           from, " > ", to, ".", call. = FALSE)
    }

    # Test Intervals overlaps
    if (any(names(df) %in% "month")) {
      df_date <-  transform(df, date = as.Date(
        paste(df$year, as.numeric(df$month), 01, sep = "-")))
      if (!(min(df_date$date) <= to | max(df_date$date) <= to) |
          max(df_date$date) < from | min(df_date$date) > to) {
        stop("The time range selected is out of bound ", from, " / ", to, ".
The time range should overlap the date range of the data frame inputed: ",
             min(df_date$date), " / ", max(df_date$date), call. = FALSE)
      }
    } else {
      min_df <- as.Date(paste0(min(df$year), "-01-01"))
      max_df <- as.Date(paste0(max(df$year), "-12-31"))
      if (!(min_df <= to | max_df <= to) | max_df < from | min_df > to) {
        stop("The time range selected is out of bound ", from, " / ", to, ".
The time range should overlap the date range of the data frame inputed: ",
             min_df, " / ", max_df, call. = FALSE)
        }
    }

    # select the history of merging/spliting event depending on the parameters
    # diseases
    if (is.null(diseases) == FALSE &
        length(grep(paste(diseases, collapse = "|"),
                    "hepatitis|amoebiasis")) != 0) {
      spl <- ah_splits
    } else {
      spl <- splits
    }

    # Join df2
    if (is.data.frame(df2)) {
      if (length(args) == 1) {
        sel2 <- grep(paste(names(df), collapse = "|"), names(df2), value = TRUE)
        df <- merge(df, df2[, c(args, sel2)], by = sel2, all.x = TRUE)
        sel <- c(sel, args)
      } else {
        stop("The argument 'args' should be of length 1 and should be one ",
             "column name of the data frame 'df2'")
      }
    }

    # Test and select the column containing the information necessary for the
    # merging and the column selected in the parameter 'sel'.
    if (any(sel %in% names(df)) == FALSE) {
      stop("The parameters 'sel' should contain a vector of character ",
           "containing the names of the column to merge", call. = FALSE)
    }
    sel <- unique(c("province", "year", sel))
    gather_sel <-  unique(c("province", "year"))
    if (any(names(df) %in% "month")) {
      gather_sel <- unique(c(gather_sel, "month"))
    }
    if (!is.null(args)) {
      gather_sel <- unique(c(gather_sel, args))
    }
    var_sel <- sel[-which(sel %in% gather_sel)]

    # Merge provinces
    df <- select_date(df, from, to)
    df <- df[, sel]
    df <- reshape(df, varying = var_sel, direction = "long",
                  idvar = gather_sel, v.names = "value", times = var_sel,
                  new.row.names = seq(1, length(var_sel) * dim(df)[1]))
    colnames(df)[which(names(df) == "time")] <- "key"
    df <- transform(df, year = as.integer(df$year))
    df <- merge_province(df, FUN, from = from, to = to, splits_lst = spl,
                         df2 = df2, args = args, FUN2 = FUN2, ... = ...)
    df <- df[which(duplicated(df) == FALSE), ]
    df <- df[order(df$province, df$year), ]
    row.names(df) <- NULL

    # return the data frame containing only the column selected ordered
    if (any(names(df) %in% "month")) {
      df <- df[, c("province", "year", "month", "key", "value")]
    } else {
      df <- df[, c("province", "year", "key", "value")]
    }
  }
  # If the df doesn't contain the two necessary columns, print a warning message
  else {
    df
    warning("If you want to merge the province back together accordingly to
Vietnam history and time, the data frame should contain at least the columns
'province' and 'year'")
  }
  df
}
