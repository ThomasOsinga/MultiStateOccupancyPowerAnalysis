### Function exists to fix the structure of the simulation outputs 
### so it is more easily indexed by the ProcessFiles function
clean_list <- function(x) {
  if (is.list(x)) {
    # Remove NULL elements
    x <- Filter(Negate(is.null), x)
    # Recursively apply the cleaning function to non-NULL elements
    x <- lapply(x, clean_list)
  }
  return(x)
}