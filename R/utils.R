## utils

make_ints <- function(str, o_form, tl0) paste(sapply(str, function(x) o_form[which(tl0 == x)]), collapse = ":")

## calc_multibinom
