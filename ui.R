fluidPage(
  titlePanel("Fuel Consumption and Electric Generation in the US"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        'view',
        label = h3('Select view'),
        choices = c('Fuel consumption',
                    'Electric generation'),
        selected = 'Fuel consumption'
      ),
      selectInput(
        'graph',
        label = h3('Select graph'),
        choices = c(
          'Ranked by highest total',
          'Ranked by lowest total',
          'Ranked by highest state population',
          'Ranked by lowest state population',
          'Ranked by highest state GDP',
          'Ranked by lowest state GDP',
          'Timeseries'
        ),
        selected = 1
      ),
      selectInput(
        'year',
        label = h3('Select year'),
        choices = c(
          '2010 - 2019',
          '2019',
          '2018',
          '2017',
          '2016',
          '2015',
          '2014',
          '2013',
          '2012',
          '2011',
          '2010'
        ),
        selected = 1
      ),
      radioButtons(
        "per",
        label = h3("Adjustment factors"),
        choices = c(
          'No adjustment',
          'Per capita, state population',
          'Per state GDP, $ Mils'
        ),
        selected = 'No adjustment'
      ),
      radioButtons(
        "breakdown",
        label = h3("Show breakdown"),
        choices = c('None', 'By political party', 'By fuel source'),
        selected = 'None'
      )
      
    ),
    mainPanel(plotOutput('viz'))
  )
)
