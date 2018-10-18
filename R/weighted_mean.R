weighted_mean <- function(x, w,  na.rm = T) {

  if(na.rm) {


    omitted_na <- na.omit(data.frame(x, w))

    return(weighted.mean(omitted_na$x, omitted_na$w))

  }

  weighted.mean(x, w)
}
