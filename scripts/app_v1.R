
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

type_dict = tibble('gini_index' = 'Income Disparity',
                        'share_unemployed_seasonal'=  'Unemployment rate seasonal',
                        'share_white_poverty'=  'White people poverty rate',
                        'share_non_citizen' = 'Percentage of Non-citizens',
                        'share_population_in_metro_areas' = 'Percentage of people in metro cities')

#let's define our data and make a plot
crime_map <- read_csv('../data/crime_map.csv') %>% 
              filter(region != "district of columbia")

df <- read_csv('../data/hate_crimes.csv') 
crime_prop <- read_csv(file = '../data/crime_prop.csv')
crime_trump <- read_csv(file = '../data/crime_trump.csv')



    
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
     scale_fill_gradient(low='#ffe6e6', high='red', name = "", guide = "colourbar")  + 
     theme(legend.title = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks=element_blank(),
        axis.text=element_blank(), 
        axis.title=element_blank()) + 
     labs(title = paste(type_dict2[[fill_val]], "in the United States"))

     plot <- ggplotly(p1, width = 600, height = 400, tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1))
    return(plot)
}

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
   
    model <- lm(y ~ x, df)

    options(repr.plot.width=10, repr.plot.height=5)
    plot <- ggplot(df, aes(x, y)) +
              geom_point(alpha = 0.6) +
              geom_smooth(method = "lm", formula = y ~ x) +
              labs(title = paste("Variation of hate crimes across ", type_dict[[x_val]]),
                   x = type_dict[[x_val]],
                   y = "Average annual hate crime per 100K people") +
              geom_text(aes(x = -Inf, y = -Inf), 
                            label = paste0("p-value of slope = ",round(tidy(model)[[2,'p.value']],4)), 
                            hjust = -0.1, 
                            vjust = -1) +
              theme_bw() 
    
    ggplotly(plot, width = 600, height = 400)
}

make_graph_1 <- function(){
    crime_trump_1 <- crime_trump %>% 
                    select(state, share_voters_voted_trump, diff_hatecrime) %>%
                    drop_na()
    
    heatmap <- crime_trump_1 %>% 
                ggplot(aes(x = reorder(state, -share_voters_voted_trump), y = share_voters_voted_trump, group = 1, 
                           text = paste('State: ', state,
                                     '</br></br> Share of Trump voters (%):', share_voters_voted_trump,
                                     '</br> Difference in hate crime rates:', diff_hatecrime))) +
                geom_bar(stat = "identity", aes(fill = diff_hatecrime)) +
                labs(x = "", y = "Percentage of Trump voters", fill = "Change in hate crime rate (%)") +
                theme(axis.text.x = element_text(angle = 45)) +
                scale_colour_gradient(low = "#ffe6e6", high = "red",
                                     space = "Lab", na.value = "grey50", guide = "colourbar",
                                     aesthetics = "fill")
    ggplotly(heatmap, tooltip = c("text"))
}


make_graph_2 <- function(){
    crime_prop_1 <- crime_prop %>% drop_na() %>%
                    select(state, crime_rate_bracket, prop)
    
    crime_prop_1_high <- crime_prop_1 %>% filter(crime_rate_bracket == 'high baseline crime rate')
    
    bar_high <- crime_prop_1_high %>% 
                ggplot(aes(x = reorder(state, -prop), y = prop,
                      text = paste('Rate of change of hate crimes: ', prop))) +
                geom_bar(stat = "identity", fill = "blue") +
                labs(x = "", y = "Rate of change", title = "Rate of change of hate crimes across states with high baseline") +
                theme(axis.text.x = element_text(angle = 45))
    
    ggplotly(bar_high, tooltip = c("text"))
    }
    
make_graph_3 <- function(){ 
    crime_prop_1 <- crime_prop %>% drop_na() %>%
                    select(state, crime_rate_bracket, prop)
    
    crime_prop_1_low <- crime_prop_1 %>% filter(crime_rate_bracket == 'low baseline crime rate')

    bar_low <- crime_prop_1_low %>% 
                ggplot(aes(x = reorder(state, -prop), y = prop,
                      text = paste('Rate of change of hate crimes: ', prop))) +
                geom_bar(stat = "identity", fill = "blue") +
                labs(x = "", y = "Rate of change", title = "Rate of change of hate crimes across states with low baseline") +
                theme(axis.text.x = element_text(angle = 45))

    ggplotly(bar_low, tooltip = c("text"))
    }   



p1_plot <- ggplotly(p1, width = 700, height = 400)

graph1 <- dccGraph(
  id = 'graph1',
  figure = chart1(),
 style=list(
'margin-left' = '50px')
)

graph2 <- dccGraph(
  id = 'graph2',
  figure = chart2()
)

graph_heatmap <- dccGraph(
  id = 'heatmap-graph',
  figure=make_graph_1()
)

graph_barplot_1 <- dccGraph(
  id = 'boxplot-graph-1',
  figure=make_graph_2()
)

graph_barplot_2 <- dccGraph(
  id = 'boxplot-graph-2',
  figure=make_graph_3()
)

xAxisKey <- tibble(label = c('Income Disparity','Unemployment rate seasonal','White people poverty rate','Percentage of Non-citizens','Percentage of people in metro cities'),
                   value = c('gini_index', 'share_unemployed_seasonal', 'share_white_poverty', 'share_non_citizen', 'share_population_in_metro_areas')
)
# m <- map(
#     1:nrow(xaxiskey), function(i){
#       list(label = xaxiskey$label[i], value=xaxiskey$value[i])
#     })

# print(m)

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

footer <- dccMarkdown(" Note - In first graph- __Change in hate crime rate = (post-election rate - pre-election rate)__, In the second and third graphs - __Rate of change of hate crime rate = (post-election rate - pre-election rate) / pre-election rate__ ", 
                     style = list('font-size'='12px'))

app$layout(
  htmlDiv(
    list(
    htmlDiv(
    list(
      #See our styles applied to the headers
      htmlH1('U.S. Hate Crime Analysis', style = headerStyle),
      htmlH3('Using this App, you can explore the relationship between different socio-economic factors \
        (income, unemployment, education, etc) and hate crime rates. It can also be used to investigte the change of \
        hate crime rates before and after the \
        U.S. president election in 2016.')), style = headerStyle),
      dccTabs(id="tabs", value='tab-1', children=list(
      dccTab(label='Scoio-Economic factors', value='tab-1'),
      dccTab(label='General Elections' , value='tab-2')
      )),
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
          htmlH2("Analysis of U.S hate crime rates from 2010-2015",
                    style = textStyle),
          htmlP('The hate crimes are not evenly distributed across all the states in the U.S. \
                Some states have much higher rates than others. This brings up a question that whether \
                there are some factors asscoiated with the occurence of hate crimes. The analysis in this \
                section will help to approach this question.',
                style=list('font-family'='arial','font-size'='16px', 'padding-left'='100px','padding-bottom'='40px','color'='black')),
          # we can add our graph here
          htmlDiv(list(
              htmlDiv(list(Dropdown1, graph1),
                       style=list('display'='inline-block','width'='50%','border-width'='0')),
              htmlDiv(list(Dropdown, graph2),
                      style=list('display'='inline-block','width'='40%','border-width'='0')) 
        )))))
        }
    else if(tab=='tab-2'){
        return(htmlDiv(list(
        htmlH2('Impact of 2016 U.S. elections on hate crimes', style = textStyle),
        htmlP("Big fluctuations in the hate crime rates have been observed after the U.S. president election in 2016.\
            It is tempting to think that the election may have substantial impact on these rates. However, before any \
            conclusion can be made, it needs to be evaluated whether these changes were impacted by a state's voting trend \
            The first bar-chart with heatmap shows all the states with change in hate crimes(post-election - pre-election) \
            denoted by the color of the bar. In the bar charts below, the states were divided in 2 categories low and high \
            baseline where their pre-election crime rates were low and high respectively. Hover over the bars to get \
            information of the states and their pre and post-election hate crime rates.",
            style=list('font-family'='arial','font-size'='16px', 'padding-left'='100px','padding-bottom'='40px','color'='black')),
        htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
        graph_heatmap,
        htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
        htmlDiv(
            list(
        htmlDiv(graph_barplot_1
        , style = list('display'='inline-block','width'='50%','border-width'='0')),
        htmlDiv(graph_barplot_2
        , style =list('display'='inline-block','width'='50%','border-width'='0'))
    ),),
        footer 
    )))
    }
})

app$run_server()
