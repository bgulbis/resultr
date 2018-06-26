# functions for working with continuous variables

#' Perform significance testing on continuous data
#'
#' @param df data frame
#' @param y column which contains the corresponding groups, as a string or
#'   symbol
#' @param ... selection of column(s) with the data values, supports dplyr
#'   selection rules
#' @param subgrp optional, additional columns used to subgroup the data prior to
#'   testing as a character vector
#' @param normal logical, if TRUE will use t.test, otherwise wilcox.test
#'
#' @return tibble with the results of significance testing
#' @keywords internal
#' @importFrom magrittr "%>%"
p_cont_test <- function(df, y, ..., subgrp = NULL, normal = FALSE) {
    y_var <- rlang::quo_name(rlang::enexpr(y))

    x <- rlang::quos(...)

    f <- rlang::parse_expr(paste0("value ~ ", y_var))
    var_name <- rlang::sym("var_name")

    if (normal) {
        run_test <- stats::t.test
    } else {
        run_test <- stats::wilcox.test
    }

    if (!is.null(subgrp)) {
        var_name <- rlang::quos(!!!rlang::syms(subgrp), !!var_name)
    }

    df %>%
        tidyr::gather("var_name", "value", !!!x) %>%
        dplyr::group_by(!!!var_name) %>%
        dplyr::do(
            broom::tidy(
                run_test(
                    formula = !!f,
                    data = .,
                    conf.int = TRUE
                )
            )
        ) %>%
        dplyr::ungroup()
}

df_cont <- function(df, p, ...) {
    grp <- quos(...)

    var_name <- sym("var_name")
    measure <- sym("measure")
    result <- sym("result")
    year <- sym("year")
    all_measures <- sym("all_measures")

    df %>%
        group_by(!!year) %>%
        summarize_at(
            vars(!!!grp),
            funs(
                median,
                q25 = quantile(., 0.25),
                q75 = quantile(., 0.75)
            ),
            na.rm = TRUE
        ) %>%
        gather(!!var_name, !!result, -!!year) %>%
        separate(
            !!var_name,
            c("var_name", "measure"),
            # c(!!var_name, !!measure),
            sep = "_",
            fill = "left"
        ) %>%
        spread(!!measure, !!result) %>%
        unite(
            !!all_measures,
            !!sym("median"),
            !!sym("q25"),
            !!sym("q75")
        ) %>%
        spread(!!year, !!all_measures) %>%
        separate(
            `2016`,
            c("median_1", "q25_1", "q75_1"),
            sep = "_"
        ) %>%
        separate(
            `2017`,
            c("median_2", "q25_2", "q75_2"),
            sep = "_"
        ) %>%
        left_join(p, by = "var_name") %>%
        mutate_at(
            c(
                "median_1",
                "q25_1",
                "q75_1",
                "median_2",
                "q25_2",
                "q75_2"
            ),
            as.numeric
        )

}

make_cont_table <- function(df, var_name, caption, ...) {
    vars <- quos(...)

    p_df <- df %>%
        p_cont(...)

    df %>%
        df_cont(p = p_df, ...) %>%
        mutate_at(
            "var_name",
            str_replace_all,
            pattern = var_name
        ) %>%
        knitr::kable(
            digits = c(rep(2, 7), rep(3, 3)),
            caption = caption,
            booktabs = TRUE,
            col.names = cont_cols
        ) %>%
        kable_styling() %>%
        add_header_above(make_header(df)) %>%
        add_header_above(hdr_year())
}