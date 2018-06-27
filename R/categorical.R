# functions for categorical variables

#' Perform significance testing on categorical data
#'
#' @param df data frame
#' @param y column which contains the corresponding groups, as a string or
#'   symbol
#' @param ... selection of column(s) with the data values, supports dplyr
#'   selection rules
#' @param subgrp optional, additional columns used to subgroup the data prior to
#'   testing as a character vector
#' @param normal logical, if TRUE will use fisher.test, otherwise chisq.test
#'
#' @return tibble with the results of significance testing
#' @keywords internal
#' @importFrom magrittr "%>%"
p_cat_test <- function(df, y, ..., subgrp = NULL, exact = FALSE) {
    y_var <- rlang::quo_name(rlang::enexpr(y))

    x <- rlang::quos(...)

    var_name <- rlang::sym("var_name")

    if (exact) {
        run_test <- stats::fisher.test
    } else {
        run_test <- stats::chisq.test
    }

    if (!is.null(subgrp)) {
        var_name <- rlang::quos(!!!rlang::syms(subgrp), !!var_name)
    }

    df %>%
        gather("var_name", "value", !!!x) %>%
        group_by(!!!var_name) %>%
        do(
            broom::tidy(
                run_test(
                    x = dplyr::pull(., y_var),
                    y = dplyr::pull(., !!sym("value"))
                )
            )
        ) %>%
        dplyr::ungroup()
}

df_cat_logical <- function(df, p, ...) {
    grp <- quos(...)

    year <- sym("year")
    n_col <- sym("n")
    var_name <- sym("var_name")
    result <- sym("result")
    result_pct <- sym("result_pct")

    df %>%
        ungroup() %>%
        select(!!year, !!!grp) %>%
        add_count(!!year) %>%
        group_by(!!year, !!n_col) %>%
        summarize_if(is.logical, sum, na.rm = TRUE) %>%
        gather(!!var_name, !!result, -!!year, -!!n_col) %>%
        mutate(!!"pct" := !!result / !!n_col * 100) %>%
        select(-!!n_col) %>%
        unite(!!result_pct, !!result, !!sym("pct")) %>%
        spread(!!year, !!result_pct) %>%
        separate(`2016`, c("result1", "pct1"), sep = "_") %>%
        separate(`2017`, c("result2", "pct2"), sep = "_") %>%
        mutate_at(
            c("result1", "pct1", "result2", "pct2"),
            as.numeric
        ) %>%
        left_join(p, by = "var_name")
}

df_cat_char <- function(df, ...) {
    x <- quos(...)

    year <- sym("year")
    n_col <- sym("n")
    var_name <- sym("var_name")
    result <- sym("result")
    result_pct <- sym("result_pct")

    df %>%
        ungroup() %>%
        select(!!sym("millennium.id"), !!year, !!!x) %>%
        add_count(!!year) %>%
        mutate(!!"val" := TRUE) %>%
        spread(!!x[[1]], !!sym("val")) %>%
        group_by(!!year, !!n_col) %>%
        summarize_if(is.logical, sum, na.rm = TRUE) %>%
        gather(!!var_name, !!result, -!!year, -!!n_col) %>%
        mutate(!!"pct" := !!result / !!n_col * 100) %>%
        select(-!!n_col) %>%
        unite(!!result_pct, !!result, !!sym("pct")) %>%
        spread(!!year, !!result_pct) %>%
        separate(`2016`, c("result1", "pct1"), sep = "_") %>%
        separate(`2017`, c("result2", "pct2"), sep = "_") %>%
        mutate_at(
            c("result1", "pct1", "result2", "pct2"),
            as.numeric
        ) %>%
        arrange(desc(!!sym("pct1")))
}

make_cat_table <- function(df, var_name, caption, ...) {
    vars <- quos(...)

    p_df <- p_cat(df, ...)

    df %>%
        df_cat_logical(p = p_df, ...) %>%
        mutate_at(
            "var_name",
            str_replace_all,
            pattern = var_name
        ) %>%
        knitr::kable(
            digits = c(rep(0, 5), 3),
            booktabs = TRUE,
            caption = caption,
            col.names = cat_cols
        ) %>%
        kable_styling() %>%
        add_header_above(
            header = make_header(df, 2, ci = FALSE)
        ) %>%
        add_header_above(hdr_year(2, FALSE))
}

make_cat_char_table <- function(df, var_name, caption, ...) {
    p_df <- p_cat(df, ...)

    tbl <- df_cat_char(df, ...)

    p_df %>%
        bind_rows(tbl) %>%
        select(-!!sym("p.value"), everything()) %>%
        ungroup() %>%
        mutate_at(
            "var_name",
            str_replace_all,
            pattern = var_name
        ) %>%
        knitr::kable(
            digits = c(rep(0, 5), 3),
            booktabs = TRUE,
            caption = caption,
            col.names = cat_cols
        ) %>%
        kable_styling() %>%
        add_header_above(
            header = make_header(df, 2, ci = FALSE)
        ) %>%
        add_header_above(hdr_year(2, FALSE))
    # group_rows("", 2, nrow(tbl) + 1, label_row_css = "")

}