function(input, output) {
  eia.react <- reactive({
    if (input$view == 'Timeseries') {
      eia.time <- eia %>%
        group_by(., YEAR) %>%
        summarise(.,
                  ifelse(
                    input$per == 'Per capita, state population',
                    sum(Elec.MMBtu.Pop),
                    ifelse(
                      input$per == 'Per state GDP, $ Mils',
                      sum(Elec.MMBtu.GDP),
                      sum(Elec.MMBtu)
                    )
                  ))
      colnames(eia.time) <- c('Year', 'Elec.MMBtu')
    } else if (input$year == '2010 - 2019') {
      #Create a unique data frame group (without year filter) if user selects all years to view.
      eia.group <- eia %>%
        group_by(., Plant.State) %>%
        summarise(
          .,
          ifelse(
            #Mutates MMBtu calc if user selects 'per' inputs
            input$per == 'Per capita, state population',
            sum(Elec.MMBtu.Pop),
            ifelse(
              input$per == 'Per state GDP, $ Mils',
              sum(Elec.MMBtu.GDP),
              sum(Elec.MMBtu)
            )
          ),
          sum(Population),
          sum(GDP),
          unique(Overall.Party),
          sum(Coal.MMBtu),
          sum(NG.MMBtu),
          sum(Renewables.MMBtu),
          sum(Nuc.MMBtu),
          sum(Petro.MMBtu)
        )

      colnames(eia.group) <-
        c(
          'State',
          'Elec.MMBtu',
          'Population',
          'GDP',
          'Party',
          'Coal',
          'NG',
          'Renewables',
          'Nuc',
          'Petro'
        )
    } else {
      #create a unique data frame group (with year filter) if user selects specific year
      eia.group <- eia %>%
        filter(., YEAR == input$year) %>%
        group_by(., Plant.State) %>%
        summarise(
          .,
          ifelse(
            #Mutates MMBtu calc if user selects 'per' inputs
            input$per == 'Per capita, state population',
            sum(Elec.MMBtu.Pop),
            ifelse(
              input$per == 'Per state GDP, $ Mils',
              sum(Elec.MMBtu.GDP),
              sum(Elec.MMBtu)
            )
          ),
          unique(Population),
          unique(GDP),
          unique(Party),
          ifelse(input$per == 'No adjustment', sum(Coal.MMBtu), sum(Coal.MMBtu.Pop)),
          ifelse(input$per == 'No adjustment', sum(NG.MMBtu), sum(NG.MMBtu.Pop)),
          ifelse(input$per == 'No adjustment', sum(Renewables.MMBtu), sum(Renewables.MMBtu.Pop)),
          ifelse(input$per == 'No adjustment', sum(Nuc.MMBtu), sum(Nuc.MMBtu.Pop)),
          ifelse(input$per == 'No adjustment', sum(Petro.MMBtu), sum(Petro.MMBtu.Pop))
          
          # Could not get nested ifelse statements to work correctly. Worked around using code above. App does not properly plot fuel breakdown for per GDP weighting.
          
          # ifelse(input$per == 'Per capita, state population', sum(Coal.MMBtu.Pop), ifelse(input$per == 'Per state GDP, $ Mils', sum(Coal.MMBtu.GDP), sum(Coal.MMBtu))),
          # ifelse(input$per == 'Per capita, state population', sum(NG.MMBtu.Pop), ifelse(input$per == 'Per state GDP, $ Mils', sum(NG.MMBtu.GDP), sum(NG.MMBtu))),
          # ifelse(input$per == 'Per capita, state population', sum(Renewables.MMBtu.Pop), ifelse(input$per == 'Per state GDP, $ Mils', sum(Renewables.MMBtu.GDP), sum(Renewables.MMBtu))),
          # ifelse(input$per == 'Per capita, state population', sum(Nuc.MMBtu.Pop), ifelse(input$per == 'Per state GDP, $ Mils', sum(Nuc.MMBtu.GDP), sum(Nuc.MMBtu))),
          # ifelse(input$per == 'Per capita, state population', sum(Petro.MMBtu.Pop), ifelse(input$per == 'Per state GDP, $ Mils', sum(Petro.MMBtu.GDP), sum(Petro.MMBtu)))
        )
  
      colnames(eia.group) <-
        c(
          'State',
          'Elec.MMBtu',
          'Population',
          'GDP',
          'Party',
          'Coal',
          'NG',
          'Renewables',
          'Nuc',
          'Petro'
        )
    }
    
    #Customize ggplot based on user selected view input. Contains all combinations of user input 'view' and 'breakdown'
    if (input$view == 'Timeseries') {
      eia.time %>% ggplot(aes(y = Elec.MMBtu, x = Year)) + geom_line(color = 'grey30', size = 0.9) + scale_x_continuous(name = 'Year',
                                                                                                                        breaks = c(2010, 2012, 2014, 2016, 2018, 2020))
    } else if (input$view == 'Ranked by highest fuel consumption') {
      eia.group <- eia.group %>% top_n(26, Elec.MMBtu)
      if (input$breakdown == 'By political party') {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State,-Elec.MMBtu))) + geom_col(aes(fill = Party)) + scale_fill_manual(values = c('blue2', 'red3')) + xlab('State')
      } else if (input$breakdown == 'By fuel source') {
        eia.group %>% gather(Source, MMBtu, Coal:Petro) %>% ggplot(aes(
          y = MMBtu,
          x = reorder(State,-Elec.MMBtu),
          fill = Source
        )) + geom_col(position = 'stack') + scale_fill_manual(values = c('darkred', 'cyan', 'yellow3', 'grey50', 'forestgreen')) + xlab('State')
      } else {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State,-Elec.MMBtu))) + geom_col() + xlab('State')
      }
    } else if (input$view == 'Ranked by lowest fuel consumption') {
      eia.group <- eia.group %>% top_n(-25, Elec.MMBtu)
      if (input$breakdown == 'By political party') {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, Elec.MMBtu))) + geom_col(aes(fill = Party)) + scale_fill_manual(values = c('blue2', 'red3')) + xlab('State')
      } else if (input$breakdown == 'By fuel source') {
        eia.group %>% gather(Source, MMBtu, Coal:Petro) %>% ggplot(aes(
          y = MMBtu,
          x = reorder(State, Elec.MMBtu),
          fill = Source
        )) + geom_col(position = 'stack') + scale_fill_manual(values = c('darkred', 'cyan', 'yellow3', 'grey50', 'forestgreen')) + xlab('State')
      } else {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, Elec.MMBtu))) + geom_col() + xlab('State')
      }
    } else if (input$view == 'Ranked by highest state population') {
      eia.group <- eia.group %>% top_n(26, Population)
      if (input$breakdown == 'By political party') {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, -Population))) + geom_col(aes(fill = Party)) + scale_fill_manual(values = c('blue2', 'red3')) + xlab('State')
      } else if (input$breakdown == 'By fuel source') {
        eia.group %>% gather(Source, MMBtu, Coal:Petro) %>% ggplot(aes(
          y = MMBtu,
          x = reorder(State, -Population),
          fill = Source
        )) + geom_col(position = 'stack') + scale_fill_manual(values = c('darkred', 'cyan', 'yellow3', 'grey50', 'forestgreen')) + xlab('State')
      } else {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, -Population))) + geom_col() + xlab('State')
      }
    } else if (input$view == 'Ranked by lowest state population') {
      eia.group <- eia.group %>% top_n(-25, Population)
      if (input$breakdown == 'By political party') {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, Population))) + geom_col(aes(fill = Party)) + scale_fill_manual(values = c('blue2', 'red3')) + xlab('State')
      } else if (input$breakdown == 'By fuel source') {
        eia.group %>% gather(Source, MMBtu, Coal:Petro) %>% ggplot(aes(
          y = MMBtu,
          x = reorder(State, Population),
          fill = Source
        )) + geom_col(position = 'stack') + scale_fill_manual(values = c('darkred', 'cyan', 'yellow3', 'grey50', 'forestgreen')) + xlab('State')
      } else {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, Population))) + geom_col() + xlab('State')
      }
    } else if (input$view == 'Ranked by highest state GDP') {
      eia.group <- eia.group %>% top_n(26, GDP)
      if (input$breakdown == 'By political party') {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, -GDP))) + geom_col(aes(fill = Party)) + scale_fill_manual(values = c('blue2', 'red3')) + xlab('State')
      } else if (input$breakdown == 'By fuel source') {
        eia.group %>% gather(Source, MMBtu, Coal:Petro) %>% ggplot(aes(
          y = MMBtu,
          x = reorder(State, -GDP),
          fill = Source
        )) + geom_col(position = 'stack') + scale_fill_manual(values = c('darkred', 'cyan', 'yellow3', 'grey50', 'forestgreen')) + xlab('State')
      } else {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, -GDP))) + geom_col() + xlab('State')
      }
    } else if (input$view == 'Ranked by lowest state GDP') {
      eia.group <- eia.group %>% top_n(-25, GDP)
      if (input$breakdown == 'By political party') {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, GDP))) + geom_col(aes(fill = Party)) + scale_fill_manual(values = c('blue2', 'red3')) + xlab('State')
      } else if (input$breakdown == 'By fuel source') {
        eia.group %>% gather(Source, MMBtu, Coal:Petro) %>% ggplot(aes(
          y = MMBtu,
          x = reorder(State, GDP),
          fill = Source
        )) + geom_col(position = 'stack') + scale_fill_manual(values = c('darkred', 'cyan', 'yellow3', 'grey50', 'forestgreen')) + xlab('State')
      } else {
        eia.group %>% ggplot(aes(y = Elec.MMBtu, x = reorder(State, GDP))) + geom_col() + xlab('State')
      }
    }
  })
  
  output$viz <-
    renderPlot(eia.react() + ggtitle('MMBtu Fuel Consumed')  + theme_economist())
}
