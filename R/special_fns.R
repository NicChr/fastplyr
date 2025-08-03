
# Common functions that don't need to be applied on a by-group basis

# Most vectorised binary functions are group unaware
# e.g. `+`, `cheapr::gcd2`, etc

# Function name-namespace pairs
.group_unaware_fns <- list(
  round = "", floor = "", signif = "", `<` = "", `>` = "",
  log = "", `<=` = "", sign = "", `|` = "", abs = "", `>=` = "",
  `&` = "", trunc = "", `*` = "", `+` = "", `-` = "", `/` = "",
  exp = "", `==` = ""
)

# .group_unaware_fns <- new.env(parent = emptyenv())
# .group_unaware_fns[["|"]] <- base::`|`
# .group_unaware_fns[["&"]] <- base::`&`
# .group_unaware_fns[[">="]] <- base::`>=`
# .group_unaware_fns[[">"]] <- base::`>`
# .group_unaware_fns[["<="]] <- base::`<=`
# .group_unaware_fns[["<"]] <- base::`<`
# .group_unaware_fns[["=="]] <- base::`==`
# .group_unaware_fns[["+"]] <- base::`+`
# .group_unaware_fns[["-"]] <- base::`-`
# .group_unaware_fns[["*"]] <- base::`*`
# .group_unaware_fns[["/"]] <- base::`/`
# .group_unaware_fns[["abs"]] <- base::abs
# .group_unaware_fns[["sign"]] <- base::sign
# .group_unaware_fns[["floor"]] <- base::floor
# .group_unaware_fns[["trunc"]] <- base::trunc
# .group_unaware_fns[["round"]] <- base::round
# .group_unaware_fns[["signif"]] <- base::signif
# .group_unaware_fns[["exp"]] <- base::exp
# .group_unaware_fns[["log"]] <- base::log
