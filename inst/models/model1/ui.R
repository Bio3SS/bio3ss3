## Define UI
shinyUI(pageWithSidebar(
    ## Application title
    headerPanel("population growth models"),

    ## Sidebar with a slider input for number of observations
    sidebarPanel(
        sliderInput("N0", 
                    "Initial pop size", 
                    min = 0,
                    max = 100, 
                    value = 1),
        sliderInput("b0", 
                    "birth rate at zero",
                    min = 0,
                    max = 10, 
                    value = 1),
        sliderInput("d0", 
                    "death rate at zero",
                    min = 0,
                    max = 10, 
                    value = 0.5),
        sliderInput("bDD", 
                    "birth density-dependence", 
                    min = 0,
                    max = 80, 
                    value = 40),
        sliderInput("bAllee", 
                    "birth Allee effect", 
                    min = 0,
                    max = 12, 
                    value = 0),
        radioButtons("whichPlot","Plot",
                     choices=c("per capita rates"="plot_pc_demog",
                     "total (absolute) rates"="plot_total_demog",
                     "dynamics"="plot_time")),
        checkboxInput('reportDiff', "Plot net growth rate"),
        checkboxInput('logScale', "Logarithmic y-axis"),
        sliderInput("fSize","Relative size",
                    min=0.5,max=3.5,value=1,step=0.1),
        textInput('filename', "Filename"),
        checkboxInput('savePlot', "Check to save", FALSE)
        ),

    ## Show a plot of the generated distribution
    mainPanel(
        plotOutput("plot")
        )
    ))




