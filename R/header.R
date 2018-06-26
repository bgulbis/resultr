# make table header

cont_cols <- c(
    "Variable",
    "Median",
    "25th",
    "75th",
    "Median",
    "25th",
    "75th",
    "p-value",
    "low",
    "high"
)

cat_cols <- c("Variable", "n", "%", "n", "%", "p-value")


make_header <- function(df, len = 3, ci = TRUE) {
    cnt <- count(df, year)

    x <- paste("n =", cnt$n[[1]])
    y <- paste("n =", cnt$n[[2]])
    z <- "95% CI"

    hdr <- c(" " = 1, x = len, y = len, " " = 1)
    nm <- c(" ", x, y, " ")

    if (ci) {
        hdr <- c(hdr, z = 2)
        names(hdr) <- c(nm, z)
    } else {
        names(hdr) <- nm
    }

    hdr
}

hdr_year <- function(len = 3, ci = TRUE) {
    x <- c(" ", "2016" = len, "2017" = len, " ")
    if (ci) x <- c(x, " ", " ")

    x
}
