### statr_tabulate - replicates one way tabulate as in Stata requires tidyverse


statr_tabulate <- function(data, var, miss=TRUE) {
data %>%
    when(miss=="TRUE" ~ .,
         miss=="FALSE" ~ filter(., !is.na({{var}}))) %>%
    group_by({{var}}, .add=TRUE) %>%
    summarise(Freq.=n(), .groups="drop_last") %>%
    mutate(Total=sum(Freq.)) %>%
    mutate(Percent=(Freq./Total)*100) %>%
    mutate(Cum.=cumsum(Percent))
}


#devtools::source_url("https://raw.githubusercontent.com/frankpopham/frankfunk/master/frankfunk.R")



#' Pare down (summarise) a data frame
#' @description Summarises a data frame (pares down). A count is given for each category of a
#' categorical variables (character, factor, logical) while for numeric variables the
#' mean is returned. Also for categorical variables percentage is given. Counts are given for
#' missing category in categorical variables but  missing are not included in calculation of
#' percentage. Weights are allowed.

#' @param .data A data frame
#' @param .wt A weight variable in the data frame
#' @param ... grouping variable in the data frame
#'
#' @return A tibble with four columns 1) variable name, 2) Variable value (category) or
#' (mean) for numeric variables, 3)  Number of cases (n) - for numeric variables this is number
#' of non-missing cases. 4) Percentage of non missing cases for each category  or the mean.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' pare(iris)
#' iris_t <- iris
#' levels(iris_t$Species) <- c(NA, "versicolor", "virginica")
#' pare(iris_t)
#.wt?????remove
pare <- function(.data , .wt=1) {
    s1 <-  dplyr::select(.data, !where(is.numeric))
    if(ncol(s1)==0) {df1 <- NULL}
    else {
        s1 <- names(s1)
        s2 <-  .data %>%
            dplyr::mutate(.wt={{.wt}}) %>%
            dplyr::select(c(tidyselect::all_of(s1), .wt))
        df1 <-  purrr::map_dfr(s1, ~dplyr::count(s2, .data[[.x]], wt=.wt) %>%
                                   tidyr::pivot_longer(-.data$n, names_to="variable") %>%
                                   dplyr::mutate(dplyr::across(c(.data$variable, .data$value),
                                                               as.character))) %>%
            dplyr::group_by(.data$variable) %>%
            dplyr::mutate(nsum=ifelse(is.na(.data$value), NA, .data$n)) %>%
            dplyr::mutate(pc=ifelse(is.na(.data$value), NA,
                                    (.data$n/ sum(.data$nsum, na.rm=TRUE))*100)) %>%
            dplyr::ungroup()
    }
    df2 <-.data %>%
        dplyr::mutate(.wt={{.wt}}) %>%
        dplyr::select(c(where(is.numeric), .wt))
    df21 <- df2 %>%
        dplyr::summarise(dplyr::across(-.wt, ~sum(as.numeric(!is.na(.x))*.wt),
                                       .names = "n_{.col}"))
    df22 <- df2 %>%
        dplyr::summarise(dplyr::across(-.wt, ~weighted.mean(.x, .wt, na.rm=TRUE),
                                       .names = "pc_{.col}"))
    df2 <- dplyr::bind_cols(df21, df22) %>%
        tidyr::pivot_longer(dplyr::everything(),
                            names_to=c(".value", "variable"),
                            names_sep="_") %>%
        dplyr::mutate(value = "(mean)") %>%
        dplyr::mutate(dplyr::across(c(.data$variable, .data$value), as.character))
    dplyr::bind_rows(df1, df2) %>%
        dplyr::select(.data$variable, .data$value, .data$n, "% or mean"=.data$pc)
}
