
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(gapminder)
library(maps)
library(repr)
library(ggplot2)
library(broom)
library(gridExtra)
library("cowplot")

app <- Dash$new()

#You can change colours and styles in the app
colors <- list(
  text = '#0013a3'
)

textStyle = list(
  textAlign = 'center',
  color = colors$text
)

type_dict = tibble('gini_index' = 'Income Disparity',
                        'share_unemployed_seasonal'=  'Unemployment rate seasonal',
                        'share_white_poverty'=  'White people poverty rate',
                        'share_non_citizen' = 'Percentage of Non-citizens',
                        'share_population_in_metro_areas' = 'Percentage of people in metro cities')

#let's define our data and make a plot
crime_map <- read_csv('../data/crime_map.csv') %>% 
              filter(region != "district of columbia")

df <- read_csv('../data/hate_crimes.csv') %>%
        filter(state != "District of Columbia")

chart2 <- function(x_val = 'gini_index'){

    type_dict = tibble('gini_index' = 'Income Disparity',
                        'share_unemployed_seasonal'=  'Unemployment rate seasonal',
                        'share_white_poverty'=  'White people poverty rate',
                        'share_non_citizen' = 'Percentage of Non-citizens',
                        'share_population_in_metro_areas' = 'Percentage of people in metro cities')

    df <- df %>%
          select(c(x_val,'avg_hatecrimes_per_100k_fbi','state')) %>% 
          drop_na()

    df <- tibble('x'= df[[1]], 'y' = df[[2]], 'state' = df[[3]])

#    Plot the data points on an interactive axis
#    paste("Variation of hate crimes across ", type_dict[[x_val]]      
    model <- lm(y ~ x, df)

    options(repr.plot.width=10, repr.plot.height=5)
    plot <- ggplot(df, aes(x, y)) +
              geom_point(alpha = 0.6) +
              geom_smooth(method = "lm", formula = y ~ x) +
              labs(title = "Variation of hate crimes across socio-economic factors",
                   x = type_dict[[x_val]],
                   y = "Average annual hate crime per 100K people") +
              geom_text(aes(x = -Inf, y = -Inf), 
                            label = paste0("p-value of slope = ",round(tidy(model)[[2,'p.value']],4)), 
                            hjust = -2, 
                            vjust   = -23,
                            size = 5) +
              theme(plot.title = element_text(face = "bold", size = 12),
                    axis.title = element_text(face = "bold", size = 12)) 
                              
    return(plot)
}

    
p1 <- ggplot(crime_map, aes(long, lat, group = group, fill = avg_hatecrimes_per_100k_fbi))+
     geom_polygon(colour = "white") +   
     scale_fill_gradient(low='#ffe6e6', high='red', guide = "colourbar", name = "Average hate crime \n per 100K polpulation")  + 
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
     panel.background = element_blank(), axis.line = element_blank(), axis.ticks=element_blank(),axis.text=element_blank(), 
           axis.title=element_blank()) + 
     labs(title = 'Average hate crimes per 1OOK population in the United States')


graph1 <- dccGraph(
  id = 'graph1',
  figure= ggplotly(p1, width = 800, height = 400)
)

graph2 <- dccGraph(
  id = 'graph2',
  figure= ggplotly(chart2(), width = 800, height = 400)
)

xaxiskey <- tibble(label = c('Income Disparity', 'Unemployment rate seasonal', 'White people poverty rate', 'Percentage of Non-citizens', 'Percentage of people in metro cities'),
                   value = c('gini_index', 'share_unemployed_seasonal', 'share_white_poverty', 'share_non_citizen', 'share_population_in_metro_areas')
)

Dropdown <- dccDropdown(
  id = 'factors',
  options = map(
            1:nrow(xaxiskey), function(i){
              list(label=xaxiskey$label[i], value=xaxiskey$value[i])
            }),
  value = 'gini_index',
  style=list(width = '70%',
              verticalAlign="middle")
)

line <- htmlDiv('Select the socio-economic factor for x-axis')

app$layout(
  htmlDiv(
    list(
      #See our styles applied to the headers
      htmlH1('U.S. Hate Crime Analysis', style = textStyle),
      htmlH3('Using this App, you can explore the relationship between different socio-economic factors (income, unemployment, \
      education, etc) and hate crime rates. It can also be used to investigte the change of hate crime rates before and after the \
      U.S. president election in 2016.', style = textStyle),

      htmlDiv(children = "Analysis of U.S hate crime rates from 2010-2015", style = textStyle),
      htmlP('The hate crimes are not evenly distributed across all the states in the U.S. \
            Some states have much higher rates than others. This brings up a question that whether \
            there are some factors asscoiated with the occurence of hate crimes. The analysis in this \
            section will help to approach this question.',
            style=list('font-family'='arial','font-size'='16px', 'padding-left'='100px','padding-bottom'='40px','color'='black')),
      # we can add our graph here
      htmlDiv(list(
          htmlDiv(graph1,style= list('display'='inline-block','width'='58%','border-width'='0')),
          htmlDiv(list(line, Dropdown, graph2),
                  style=list('display'='inline-block','width'='42%','border-width'='0'))

      ))  
      
    )
  )
)

app$callback(
  #update figure of gap-graph
  output=list(id = 'graph2', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'factors', property='value')), 
  #this translates your list of params into function arguments
  function(factors_value) {
    chart_2(factors_value)
  })

app$run_server()