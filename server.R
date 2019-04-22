library(SignalBot)


calendar       = get_calendar()
df.option.list = get_option_list()


server <- function(input, output, session) {

    ###############################################################################################################
	###																											###
	###	Input + Reactive value																					###
	###																											###
	###		datasetInput - get dataset based on input code and date fields										###
	###																											###
	###############################################################################################################

	datasetInput <- reactive({

	    df.stock = get_data(code = '00001')
        return(df.stock)
	})

	output$plotTab_debug <- renderPrint({

	    df.stock <- datasetInput()

		print(df.stock)
    })
}
