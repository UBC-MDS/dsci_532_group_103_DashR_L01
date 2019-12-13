library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(ggpubr)
library(plotly)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

crime_prop <- read_csv(file = 'data/crime_prop.csv')
crime_trump <- read_csv(file = 'data/crime_trump.csv')

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
                scale_colour_gradient(low = "#56B1F7", high = "#132B43",
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
                geom_bar(stat = "identity", fill = "grey") +
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
                geom_bar(stat = "identity", fill = "grey") +
                labs(x = "", y = "Rate of change", title = "Rate of change of hate crimes across states with low baseline") +
                theme(axis.text.x = element_text(angle = 45))

    ggplotly(bar_low, tooltip = c("text"))
    }   
    
 #   ggplotly(ggarrange(box_low, box_high, ncol = 2, nrow = 1))
    

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

app$layout(
  htmlDiv(
    list(
      htmlH1('Impact of 2016 U.S. elections on hate crimes'),
      htmlLabel("Big fluctuations in the hate crime rates have been observed after the U.S. president election in 2016. It is tempting to think that the election may have substantial impact on these rates. However, before any conclusion can be made, it needs to be evaluated whether these changes were impacted by a state's voting trend The first bar-chart with heatmap shows all the states with change in hate crimes(post-election - pre-election) denoted by the color of the bar. In the bar charts below, the states were divided in 2 categories low and high baseline where their pre-election crime rates were low and high respectively. Hover over the bars to get information of the states and their pre and post-election hate crime rates. Also clicking on any bar on the heatmap will highlight the same state in either of the 2 charts for low and high-baseline crime rates."),
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
      graph_heatmap,
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
      htmlDiv(
          list(
        htmlDiv((
            graph_barplot_1
        ), className="six columns"),
        htmlDiv((
            graph_barplot_2
        ), className="six columns")
    ), className="row"),
      htmlIframe(height=20, width=10, style=list(borderWidth = 0)) #space
    )
  )
)

app$run_server()

