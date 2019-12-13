
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

headerStyle = list(
textAlign = 'center',
"color" = 'white',
"font-family" = 'arial',
"background-color" = '#0404B4',
"padding-bottom" = "10px",
"padding-top" = "10px",
"padding-right" = "20px",
"padding-left" = "20px"
)



#let's define our data and make a plot
crime_map <- read_csv('../data/crime_map.csv') %>% 
              filter(region != "district of columbia")

df <- read_csv('../data/hate_crimes.csv') 

chart2 <- function(x_val = 'gini_index'){

    type_dict = tibble( 'gini_index' = 'Income Disparity',
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
              labs(title = paste("Variation of hate crimes across ", type_dict[[x_val]]),
                   x = type_dict[[x_val]],
                   y = "Average annual hate crime per 100K people") +
              geom_text(aes(x = -Inf, y = -Inf), 
                            label = paste("p-value of slope = ",round(tidy(model)[[2,'p.value']],4)), 
                            hjust = -2, 
                            vjust   = -23,
                            size = 5) +
              theme_bw() 
    
    ggplotly(plot, width = 500, height = 400)
}

chart1 <- function(fill_val = 'avg_hatecrimes_per_100k_fbi'){
    
    type_dict2 = tibble('avg_hatecrimes_per_100k_fbi' = 'Average hate crime \n per 100K population',
    'gini_index' = 'Income Disparity',
    'share_unemployed_seasonal'=  'Unemployment rate seasonal',
    'share_white_poverty'=  'White people poverty rate',
    'share_non_citizen' = 'Percentage of Non-citizens',
    'share_population_in_metro_areas' = 'Percentage of people in metro cities')
    

    
    p1 <- ggplot(crime_map, aes(long, lat, group = group, fill = !!sym(fill_val),
    text =paste(type_dict2[[fill_val]],":",!!sym(fill_val),
                                '</br></br> State:', region)))+
     geom_polygon(colour = "white") +   
     scale_fill_gradient(low='#ffe6e6', high='red', guide = "colourbar", name = paste(type_dict2[[fill_val]]), position = "bottom")  + 
     theme(legend.position="bottom", panel.grid.minor = element_blank(),
     panel.background = element_blank(), axis.ticks=element_blank(),axis.text=element_blank(), 
           axis.title=element_blank()) + 
     labs(title = paste(type_dict2[[fill_val]], "in the United States"))

     plot <- ggplotly(p1, width = 700, height = 400, tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1))
    return(plot)
}


graph1 <- dccGraph(
  id = 'graph1',
  figure = chart1(),
 style=list(
'margin-left' = '50px')
)

graph2 <- dccGraph(
  id = 'graph2',
  figure = chart2(),
style=list(
'margin-left' = '200px')

)




######################

Dropdown <- dccDropdown(
  id = 'factors',
  options =  list(
    list(label = 'Income Disparity', value= 'gini_index'),
    list(label = 'Unemployment rate seasonal', value= 'share_unemployed_seasonal'),
    list(label = 'White people poverty rate', value= 'share_white_poverty'),
    list(label = 'Percentage of Non-citizens', value= 'share_non_citizen'),
    list(label = 'Percentage of people in metro cities', value= 'share_population_in_metro_areas')
  ),
  value = 'gini_index',
  style=list('width' = '70%',
              'verticalAlign' = "middle",
              'margin-left' = '120px')
)

line <- htmlP('Select the socio-economic factor for x-axis')

#########
Dropdown1 <- dccDropdown(
id = 'factors_1',
options =  list(
list(label = 'Average hate crime', value= 'avg_hatecrimes_per_100k_fbi'),
list(label = 'Income Disparity', value= 'gini_index'),
list(label = 'Unemployment rate seasonal', value= 'share_unemployed_seasonal'),
list(label = 'White people poverty rate', value= 'share_white_poverty'),
list(label = 'Percentage of Non-citizens', value= 'share_non_citizen'),
list(label = 'Percentage of people in metro cities', value= 'share_population_in_metro_areas')
),
value = 'avg_hatecrimes_per_100k_fbi',
style=list('width' = '70%',
'verticalAlign' = "middle",
'margin-left' = '50px')
)

line <- htmlP('Select the socio-economic factor for group')




app$layout(
  htmlDiv(
    list(
      #See our styles applied to the headers
      htmlDiv(
      list(
      htmlH1('U.S. Hate Crime Analysis', style = headerStyle),
      htmlP('Using this App, you can explore the relationship between different socio-economic factors (income, unemployment, \
      education, etc) and hate crime rates. It can also be used to investigte the change of hate crime rates before and after the \
      U.S. president election in 2016.')),style = headerStyle),
      dccTabs(id="tabs", value='tab-1', children=list(
      dccTab(label='Scoio-Economic factors', value='tab-1'),
      dccTab(label='General Elections' , value='tab-2')
), style = list("padding-bottom" = "10px","padding-top" = "10px")),
      htmlDiv(id='tabs-content-example') 
      
    )
  )
)

app$callback(
  #update figure of gap-graph
  output=list(id = 'graph2', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'factors', property='value')),
  function(factors_value) {
    chart2(factors_value)
  })

app$callback(
#update figure of gap-graph
output=list(id = 'graph1', property='figure'),
#based on values of year, continent, y-axis components
params=list(input(id = 'factors_1', property='value')),
function(factors_value) {
    chart1(factors_value)
})



app$callback(
  #update figure of gap-graph
  output=list(id = 'tabs-content-example', property = 'children'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'tabs', property='value')),
  function(tab = 'tab-1'){
   
    if (tab == 'tab-1'){
        return(htmlDiv(list(
          htmlH2(children = "Analysis of U.S hate crime rates from 2010-2015", style = list('font-family'='arial','font-size'='20px', 'padding-left'='400px', 'padding-top'='50px')),
          htmlP('The hate crimes are not evenly distributed across all the states in the U.S. \
                Some states have much higher rates than others. This brings up a question that whether \
                there are some factors asscoiated with the occurence of hate crimes. The analysis in this \
                section will help to approach this question.',
                style=list('font-family'='arial','font-size'='16px', 'padding-left'='100px','padding-bottom'='40px','color'='black')),
          # we can add our graph here
          htmlDiv(list(
              htmlDiv(list(Dropdown1, graph1),
                       style=list('display'='inline-block','width'='40%','border-width'='0')),
              htmlDiv(list(Dropdown, graph2),
                      style=list('display'='inline-block','width'='40%','border-width'='0'))
            
          )) 
        )))
        }
    else if(tab=='tab-2'){
        return(htmlDiv(list(
          htmlH2('Impact of 2016 U.S. elections on hate crimes',
                  style=list('font-family'='arial','font-size'='20px', 'padding-left'='400px', 'padding-top'='50px')),
          htmlP('Big fluctuations in the hate crime rates have been observed after the U.S. president election in 2016. \
                It is tempting to think that the election may have substantial impact on these rates. However, before any \
                conclusion can be made, it needs to be evaluated whether these changes were impacted by a state\'s voting trend \
                The first bar-chart with heatmap shows all the states with change in hate crimes(post-election - pre-election) denoted \
                by the color of the bar. In the bar charts below, the states were divided in 2 categories low and high baseline \
                where their pre-election crime rates were low and high respectively. Hover over the bars to get information of the states \
                and their pre and post-election hate crime rates. Also clicking on any bar on the heatmap will highlight the same state \
                in either of the 2 charts for low and high-baseline crime rates.',
                style=list('font-family'='arial','font-size'='16px', 'padding-left'='100px','padding-bottom'='40px','color'='black')
        ))))
    }
})

app$run_server()
