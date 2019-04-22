get_option_list <- function(){

    ## Return dataframe of the ad-hoc option list location
    ##
    ## Args:
    ##
    ## Returns:
    ##     df.option (Dataframe): Dataframe of the whole option list with lot size
    ##
    ## Example:
    ##     df.option =
    ##
    ## Data preview:
    ##     code quantity
    ##      1      500
    ##      2      500
    ##      3     1000

    os = get_os()

    if(os == 'Linux'){
        path = ""
    }else if (os == 'Windows'){
        path = "data/option_quantity.csv"
    }

    df.option <- read.csv(file = path, header = TRUE)
    df.option <- df.option %>% mutate(code = str_pad(code, 5, pad = "0"))
    return(df.option)
}
