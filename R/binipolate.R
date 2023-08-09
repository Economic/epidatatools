#' binipolate: binned interpolated percentiles
#'
#' @param data a data frame
#' @param var variable for analysis
#' @param w weight
#' @param p percentiles to calculate 
#' @return a tibble
#' @export
#' @importFrom dplyr %>%
#' @examples binipolate(mtcars, var = "cyl", binsize = 0.25)
#' @examples binipolate(mtcars, var = "gear", w = wt, binsize = 0.25)
#' @examples binipolate(mtcars, var = "mpg", group_vars = c("cyl", "gear"), w = wt, binsize = 0.25)
binipolate <- function(data, var, p = 50, group_vars, binsize, w = NULL) {
  
  # convert character to symbol for evaluation
  var <- rlang::sym(var)
  
  
  # Check binsize and throw an error if it's less than or equal to 0
  if (binsize <= 0) {
    stop("binsize must be greater than 0.")
  }
  
  # Check data for observations
  if(nrow(data) == 0) {
    stop("no observations, please check sample")
  }
  
  # Check if group_vars is empty, and if so, set it to "all"
  if (missing(group_vars) || length(group_vars) == 0) {
    group_vars <- "all"
  }
  
  # assert group_var as character strings
  if (!is.character(group_vars)) group_vars <- as.character(group_vars)
  
  # ensure data is ungrouped
  df <- ungroup(data) %>% 
    mutate(all = 1)
  
  # cumulative distribution function
  #note: CDF will be used to approximate closest bins around desired percentile
  CDF <- df %>% 
    filter(!is.na({{ var }})) %>% 
    # calculate bin values across groups
    group_by(across(all_of(group_vars))) %>%
    mutate(binvalue = floor((!!var - binsize/2)/binsize) * binsize + binsize + binsize/2) %>% 
    # calculate
    ungroup() %>% group_by(across(all_of(group_vars)), binvalue) %>% 
    #### THIS CAN BE REPLACED WITH ONE STEP TO GET TO RUNNINGSUM??? ####
  summarise(sum = sum({{ w }}, na.rm = TRUE)) %>%
    mutate(runningsum = cumsum(sum), totalsum = sum(sum, na.rm = TRUE), cdf = runningsum/totalsum) %>% 
    ungroup() %>% group_by(across(all_of(group_vars))) %>% 
    mutate(pop = 1, binnumber = cumsum(pop))
  
  # identify maximum number of bins within given percentile breaks
  #note: example, p = 50 ~ max(binnumber) == 90
  map(p, ~ CDF %>% filter(cdf <= .x / 100) %>% 
        group_by(across(all_of(group_vars))) %>%
        summarise(below = max(binnumber, na.rm = TRUE), .groups = "drop") %>% 
        mutate(p = .x)) %>% 
    reduce(bind_rows) %>% 
    # merge with binned data
    left_join(CDF, by = group_vars, relationship = "many-to-many") %>% 
    # within each group, filter out to the bin break or bin break + 1
    group_by(across(all_of(group_vars)), p) %>% 
    filter(binnumber == below | binnumber == (below + 1)) %>% 
    # calculate percentiles
    mutate(value =  binvalue + (lead(binvalue, 1) - binvalue) * ((p/100 - cdf) / (lead(cdf, 1) - cdf))) %>% 
    filter(!is.na(value)) %>% 
    select(all_of(group_vars), value, p) %>% 
    ungroup() %>% 
    mutate(across(.cols = where(~ sjlabelled::is_labelled(.) && !is.character(.)), ~ as.character(as_factor(.x)))) 
  
}