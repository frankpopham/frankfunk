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


