# QSS Quarter Projects, Winter 2021. 
# Prof. Robert

# DAGs using ggdag. 
# ggplot implementation of DAGs. 


library(ggdag)
library(tidyverse)
library(ggrepel)

# Our DAG

covid_dag <- dagify(economics ~  compliance + economic_relief + politics + other_econ + covid_rates + lockdown_policy,
                    compliance ~ covid_rates + demographics + politics,
                    economic_relief ~ politics,
                    lockdown_policy ~ covid_rates + politics,
                    covid_rates ~ healthcare,
                    healthcare ~ politics,
                         labels = c("economics" = "Economic\n Effects", 
                                    "other_econ" = "Other\n Economic\n Factors",
                                    "lockdown_policy" = "Lockdown\n Policy",
                                    "compliance" = "Citizen\n Activity",
                                    "oil_price" = "Oil Prices",
                                    "labor_demand" = "Demand for\n Labor",
                                    "economic_relief" ="Provided\n Economic\n Aid",
                                    "stimulus" = "Provided\n Economic\n Stimulus",
                                    "welfare" = "Welfare\n Spending",
                                    "covid_rates" = "COVID Rates",
                                    "demographics" = "Country\n Demographics",
                                    "politics" = "Political\n Factors",
                                    "unified" = "Political\n Unification",
                                    "coalition" = "Political\n Coalition",
                                    "polarization" = "Political\n Polarization",
                                    "trade" = "Economic\n Reliance on\n Trade",
                                    "vaccine" = "Distribution\n and Efficacy\n of Vaccines",
                                    "accessibility" = "Accessibility\n of Healthcare",
                                    "economic_composition" = "Economic\n Composition",
                                    "volatility" = "Price Volatility",
                                    "healthcare" = "Quality of\n Healthcare"
                                    ),
                        exposure ="lockdown_policy",
                        latent = c(),
                        outcome = "economics"
                   )
 
covid_dag %>%
  ggdag() +
  theme_dag()  

# Turn the dag info into a nice table/tibble.

tidycase <- covid_dag %>%
  tidy_dagitty(); tidycase

final_dag = tidycase %>%
  mutate(linetype = ifelse(direction == "<->", "dashed", "solid")) %>%
  mutate(class = "exogenous") %>%
  mutate(class = ifelse(name %in% c("lockdown_policy"), "exposure", class)) %>%
  mutate(class = ifelse(name %in% c("lockdown_efficacy"), "latent", class)) %>%
  mutate(class = ifelse(name %in% c("economics"), "outcome", class)) %>%
  mutate(new_label = label)

# Keep only unique labels 

for(l in 2:length(final_dag[["data"]][["label"]])-1){
  print(l)
  label = final_dag[["data"]][["label"]][l]
  if(label %in% final_dag[["data"]][["label"]][1:l-1]){
    final_dag[["data"]][["new_label"]][l] = NA
  }
}


# Make a new line type variable based on arrow direction variable.

final_dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(color = class)) + 
  geom_dag_edges(aes(edge_linetype = linetype), 
                 show.legend = FALSE) +
  geom_label_repel(aes(label=new_label), 
                   box.padding   = 1, 
                   segment.color = 'grey50',
                   max.overlaps = 20) + 
  theme_dag()



# ggdag_status(covid_dag, text = FALSE, use_labels = "label", node_size = 16, text_size =3) +
  # theme_dag()

