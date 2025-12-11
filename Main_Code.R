library(babynames)
library(shiny)
library(dplyr)
library(ggplot2)
library(tibble)

babynames_grouped <- readRDS("babynames_grouped.rds")

ui <- fluidPage(
  titlePanel("Baby Name Gender Dynamics With Forecast"),
  h4("Enter a name (capitalized) and adjust the slider for your desired year range"),
  textInput("names", "Enter name:", "Reese"),
  sliderInput(
    "years", "Year range:",
    min = min(babynames_grouped$year),
    max = max(babynames_grouped$year),
    value = c(1997, 2017),
    step = 1,
    sep = ""
  ),
  selectInput(
    "sex", "View mode:",
    choices = c("Female", "Male", "Both"),
    selected = "Both"
  ),
  plotOutput("name_plot"),
  hr(),
  h3("Summary statistics for selected name"),
  tableOutput("summary_table"),
  hr(),
  h3("Top 10 Most Volatile Names (Gender Shifts)"),
  plotOutput("volatility_plot"),
  hr(),
  h3("Meaningful Gendering Trends (Names With Directional Change)"),
  p("Only names showing statistically meaningful movement toward more- or less-gendered usage are included. Flat trends are excluded to reduce noise."),
  tableOutput("group_summary_table")
)

server <- function(input, output, session) {
  
  
  volatility_data <- reactive({
    df <- babynames_grouped |>
      filter(
        year >= input$years[1],
        year <= input$years[2],
        total_n > 50,
        !is.na(female_share)
      )
    
    if (nrow(df) == 0) return(tibble())
    
    names_list <- split(df, df$name)
    
    res <- lapply(names_list, function(dn) {
      if (nrow(dn) < 3) return(NULL)
      vol   <- sd(dn$female_share)
      slope <- coef(lm(female_share ~ year, data = dn))[2]
      trend <- ifelse(slope > 0, "Trending Female", "Trending Male")
      data.frame(
        name       = dn$name[1],
        volatility = vol,
        slope      = slope,
        trend      = trend
      )
    })
    
    bind_rows(res) |>
      arrange(desc(volatility)) |>
      head(10)
  })
  
  summary_data <- reactive({
    req(input$names)
    name_vec <- trimws(strsplit(input$names, ",")[[1]])
    
    df <- babynames_grouped |>
      filter(
        name %in% name_vec,
        year >= input$years[1],
        year <= input$years[2],
        !is.na(female_share)
      )
    
    if (nrow(df) == 0) return(tibble())
    
    split_list <- split(df, df$name)
    
    res <- lapply(split_list, function(dn) {
      dn <- dn[order(dn$year), ]
      if (nrow(dn) < 2) return(NULL)
      
      first_year <- min(dn$year)
      peak_year  <- dn$year[which.max(dn$total_n)]
      max_prop   <- max(dn$female_share)
      min_prop   <- min(dn$female_share)
      avg_prop   <- mean(dn$female_share)
      
      state <- ifelse(
        dn$female_share > 0.5, 1,
        ifelse(dn$female_share < 0.5, -1, 0)
      )
      nz <- state[state != 0]
      switches <- if (length(nz) <= 1) 0 else sum(diff(nz) != 0)
      
      vol <- sd(dn$female_share)
      
      data.frame(
        name            = dn$name[1],
        first_year      = as.integer(first_year),
        peak_year       = as.integer(peak_year),
        max_female_prop = max_prop,
        min_female_prop = min_prop,
        avg_female_prop = avg_prop,
        switches        = switches,
        volatility      = vol
      )
    })
    
    bind_rows(res)
  })
  
  group_trend_data <- reactive({
    df <- babynames_grouped |>
      filter(
        year >= input$years[1],
        year <= input$years[2],
        total_n > 50,
        !is.na(dist)
      )
    
    if (nrow(df) == 0) return(tibble())
    
    split_list <- split(df, df$name)
    
    res <- lapply(split_list, function(dn) {
      if (nrow(dn) < 3) return(NULL)
      slope <- coef(lm(dist ~ year, data = dn))[2]
      direction <- dplyr::case_when(
        slope >  0.001 ~ "More gendered",
        slope < -0.001 ~ "Less gendered",
        TRUE           ~ "Flat"
      )
      data.frame(
        name      = dn$name[1],
        slope     = slope,
        direction = direction
      )
    })
    
    res_df <- bind_rows(res) %>% filter(direction != "Flat")
    
    tibble(
      names_with_trends   = nrow(res_df),
      trending_more_gendered = sum(res_df$direction == "More gendered"),
      trending_less_gendered = sum(res_df$direction == "Less gendered"),
      share_more_gendered = round(mean(res_df$direction == "More gendered"), 3),
      share_less_gendered = round(mean(res_df$direction == "Less gendered"), 3)
    )
  })
  
  output$name_plot <- renderPlot({
    req(input$names)
    name_vec <- trimws(strsplit(input$names, ",")[[1]])
    
    df <- babynames_grouped |>
      filter(
        name %in% name_vec,
        year >= input$years[1],
        year <= input$years[2]
      ) |>
      mutate(
        male_n = total_n - female_n,
        female_share = female_n / total_n
      )
    
    if (nrow(df) == 0) return(NULL)
    
    if (input$sex == "Both") {
      df_ratio <- df |> filter(!is.na(female_share))
      
      split_list <- split(df_ratio, df_ratio$name)
      
      forecast_list <- lapply(split_list, function(dn) {
        if (nrow(dn) < 2) return(NULL)
        m <- lm(female_share ~ year, data = dn)
        last_year <- max(dn$year)
        future_years <- seq(last_year + 1, last_year + 10)
        preds <- predict(m, newdata = data.frame(year = future_years))
        preds <- pmin(pmax(preds, 0), 1)
        data.frame(
          name = dn$name[1],
          year = future_years,
          female_share = preds,
          type = "forecast"
        )
      })
      
      forecast_df <- bind_rows(forecast_list)
      hist_df <- df_ratio |> mutate(type = "historical")
      full_df <- bind_rows(hist_df, forecast_df)
      
      return(
        ggplot(full_df, aes(x = year, y = female_share, color = name)) +
          geom_line(aes(linetype = type), linewidth = 1) +
          scale_linetype_manual(values = c(historical = "solid", forecast = "dotted")) +
          geom_hline(yintercept = 0.5, linetype = "dashed") +
          scale_y_continuous(limits = c(0, 1)) +
          labs(x = "Year", y = "Share female (0 = male, 1 = female)")
      )
    }
    
    if (input$sex == "Female") {
      df_f <- df |> filter(female_n > 0)
      
      return(
        ggplot(df_f, aes(x = year, y = female_n, color = name)) +
          geom_line(linewidth = 1) +
          labs(x = "Year", y = "Female births (count)")
      )
    }
    
    if (input$sex == "Male") {
      df_m <- df |> filter(male_n > 0)
      
      return(
        ggplot(df_m, aes(x = year, y = male_n, color = name)) +
          geom_line(linewidth = 1) +
          labs(x = "Year", y = "Male births (count)")
      )
    }
  })
  
  
  output$volatility_plot <- renderPlot({
    df <- volatility_data()
    if (nrow(df) == 0) return(NULL)
    df$label <- round(df$volatility, 3)
    ggplot(df, aes(x = reorder(name, volatility), y = volatility, fill = trend)) +
      geom_col() +
      geom_text(aes(label = label), hjust = -0.1, size = 4) +
      coord_flip() +
      scale_fill_manual(values = c("Trending Male" = "#89CFF0", "Trending Female" = "#FFC0CB")) +
      expand_limits(y = max(df$volatility) * 1.15)
  })
  
  output$summary_table <- renderTable({
    summary_data()
  })
  
  output$group_summary_table <- renderTable({
    group_trend_data()
  })
}

shinyApp(ui, server)


