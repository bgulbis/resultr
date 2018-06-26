# functions for categorical variables

p_cat <- function(df, ...) {
    grp <- quos(...)
    var_name <- sym("var_name")

    df %>%
        select(!!sym("year"), !!!grp) %>%
        gather("var_name", "value", !!!grp) %>%
        group_by(!!var_name) %>%
        do(
            broom::tidy(
                chisq.test(
                    .$year, .$value
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