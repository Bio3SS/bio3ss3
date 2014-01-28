## Define UI
shinyUI(pageWithSidebar(
    ## Application title
    headerPanel("population growth models"),

    ## Sidebar with a slider input for number of observations
    sidebarPanel(
        radioButtons("whichPlot","Plot",
                     choices=c("per capita rates"="plot_pc_demog",
                     "total (absolute) rates"="plot_total_demog",
                     "dynamics"="plot_time")),
        sliderInput("b0", 
                    "birth rate at zero [1/time]",
                    min = 0,
                    max = 10, 
                    value = 1,
                    step = 0.1),
        sliderInput("d0", 
                    "death rate at zero [1/time]",
                    min = 0,
                    max = 10, 
                    value = 0.5,
                    step = 0.1),
        sliderInput("bDD", 
                    "birth density-dependence [number]", 
                    min = 0,
                    max = 80, 
                    value = 40,
                    step = 0.1),
        sliderInput("bAllee", 
                    "birth Allee effect [number]", 
                    min = 0,
                    max = 12, 
                    value = 0,
                    step = 0.1),
        conditionalPanel(condition="input.whichPlot == 'plot_time'",
                         sliderInput("N0", 
                                     "Initial pop size [number]", 
                                     min = 0,
                                     max = 100,
                                     value = 1),
                         sliderInput("timeMax",
                                     "Maximum time [time]",
                                     min=2,
                                     max=50,
                                     value=20),
                         sliderInput("timeSteps",
                                     "Number of time steps",
                                     min=10,
                                     max=200,
                                     value=100)),
        conditionalPanel(condition="input.whichPlot != 'plot_time'",
                         checkboxInput('reportDiff', "Plot net growth rate"),
                         checkboxInput('logScale', "Logarithmic y-axis")),
        sliderInput("fSize","Relative font size",
                    min=0.5,max=3.5,value=1,step=0.1),
        textInput('filename', "Filename",value="modelplot"),
        actionButton("printButton","Print")
        ),

    ## Show a plot of the generated distribution
    mainPanel(
        plotOutput("plot")
        )
    ))




