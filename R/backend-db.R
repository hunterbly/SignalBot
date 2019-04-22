get_option_list <- function(){

    ## Return dataframe of the ad-hoc option list location
    ##
    ## Args:
    ##
    ## Returns:
    ##     df.option (Dataframe): Dataframe of the whole option list with lot size
    ##
    ## Example:
    ##     df.option = get_option_list()
    ##
    ## Data preview:
    ##     code quantity
    ##    00001      500
    ##    00002      500
    ##    00003     1000

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

get_data <- function(code = NULL){

    ## Return dataframe of the selected stock code
    ##
    ## Args:
    ##     code (str): Stock code e.g. 00001
    ##
    ## Returns:
    ##     df.stock (Dataframe): Dataframe of the selected stock code
    ##
    ## Example:
    ##     df.stock = get_data(code = '00001')
    ##
    ## Data preview:
    ##


    os = get_os()

    if(os == 'Linux'){
        path = ""
    }else if (os == 'Windows'){
        path = "data/option_quantity.csv"
    }

    return(iris)
}
