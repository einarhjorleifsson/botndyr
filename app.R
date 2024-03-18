# load("~/ShinyApps/botndyr/data/data.RData")
load("data/data.RData")
source("R/functions.R")

xlim <- range(st$lon)
ylim <- range(st$lat)

gg_base <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggplot2::geom_path(data = contour, 
                     ggplot2::aes(lon, lat, group = g),
                     colour = "grey") +
  ggplot2::geom_polygon(data = island,
                        ggplot2::aes(lon, lat),
                        fill = "grey") +
  ggplot2::geom_point(data = st,
                      ggplot2::aes(lon, lat),
                      colour = "blue",
                      size = 1) +
  ggplot2::scale_size_area(max_size = 50) +
  ggplot2::coord_quickmap(xlim = xlim, ylim = ylim)

Names <-
  bi |>
  dplyr::count(name) |>
  dplyr::arrange(-n) |>
  dplyr::pull(name)

Verkefni <-
  c("BIOICE", "HAFRO-JON BOGASON", "Rall_botndýr", "HAFRO-ÞISTILFJÖRÐUR", 
    "HAFRO-STEFÁN ÁKI", "AG-COLL-14", "NI-SAFN", "Zoolgy of Iceland", 
    "HAFRO", "JS-COLL-28", "AI-COLL", "JS-COLL-2002", "AI-COLL-1996", 
    "JS-COLL-65", "HAFRO-JÓNBJÖRN", "AI-COLL-2000", "DREKINN", 
    "JS-COLL-34", "AI-COLL-74", "HAFRO-STEINUNN HILMA", "AI-COLL-47", 
    "AG-COLL-9", "JS-COLL-70", "LHI-Helgi Guðmundsson", "JS-COLL-52", 
    "JS-COLL-66", "JS-COLL-1996", "AI-COLL-64", "MAREANO", "THOR", 
    "NI-FISH", "JS-COLL-86", "JS-COLL-55", "THOR-ST.HUGSANL", "Ingolf Expedition", 
    "SKALLAGRÍMUR", "BIOFAR", "Haakon Mosby", "HÍ-MAR-BJÖRK", 
    "IceAge_SENCKENBERG", "FRÁTEKNAR FÆRSLUR", "DANA", "Fjaran_IPA", 
    "Ymer 80", "LÍFFRST-GVH", "KARL-SKIRNIS", "SAM-BOWSER", "BIOICE-ATH", 
    "Matthías Eydal")

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("united"),
  shiny::navbarPage(
    "Botndýr",
    # Page 1 -------------------------------------------------------------------
    shiny::tabPanel(
      "Um gögnin"
    ),
    # Page 2 -------------------------------------------------------------------
    shiny::tabPanel(
      "Útbreiðsla",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput('name', 'Tegundaval', c(Choose='', Names), selectize=TRUE,
                             selected = "Yoldiella lucida"),
          shiny::radioButtons("butt", "Mynd - í vinnslu",
                              c("Magn" = "bubble",
                                "Líkindi" = "prob",
                                #"Kriging" = "krig",
                                "Mörk" = "range")),
          width = 2
        ),
        shiny::mainPanel(
          shiny::plotOutput("plot", height = "100vh"),
          width = 10
        )
      )
    ),
    # Page 3 -------------------------------------------------------------------
    shiny::tabPanel(
      "Rannsóknir og gagnasett",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput('verkefni', 'Verkefni', 
                             choices = Verkefni,
                             selectize=TRUE),
          width = 3),
        shiny::mainPanel(
          knitr::include_url("https://www.ni.is/is/rannsoknir/voktun-og-rannsoknir/botndyr-islandsmidum-bioice/botndyr-islandsmidum-bioice-nanar"),
          width = 9
        )
      )
    ),
    # Page 4 -------------------------------------------------------------------
    shiny::tabPanel(
      "Kortasjá",
      shiny::mainPanel(
        leaflet::leafletOutput("map", height = "100vh"),
        width = 12
      )
    ),
  )
)

server <- function(input, output, session) {
  
  Species <- shiny::reactive({
    input$name
  })
  
  Type <- shiny::reactive({
    input$butt
  })
  
  selected <- shiny::reactive({
    shiny::req(input$name)
    st |>
      dplyr::inner_join(bi |> dplyr::filter(name %in% input$name))
  })
  
  type <- shiny::reactive({
    shiny::req(input$butt)
  })
  
  output$plot <- shiny::renderPlot({
    
    if(type() == "bubble") {
      gg_base +
        add_bubble(st |>
                     dplyr::inner_join(bi |> dplyr::filter(name %in% Species())))
    } else {
      
      if(type() == "prob") {
        p_capture(Species(), st, bi) |>
          dplyr::mutate(p = santoku::chop(p, breaks = c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 1),
                                          drop = FALSE)) |>
          ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::geom_tile(ggplot2::aes(glon, glat, fill = p)) +
          ggplot2::geom_path(data = contour, 
                             ggplot2::aes(lon, lat, group = g),
                             colour = "grey") +
          ggplot2::geom_polygon(data = island,
                                ggplot2::aes(lon, lat),
                                fill = "grey") +
          ggplot2::geom_point(data = st,
                              ggplot2::aes(lon, lat),
                              colour = "blue",
                              size = 1) +
          ggplot2::coord_quickmap(xlim = xlim, ylim = ylim) +
          ggplot2::scale_fill_viridis_d(option = "inferno", direction = -1,
                                        drop = FALSE) +
          ggplot2::labs(fill = "Probability\nof catpture")
        
      } else {
        
        if(type() == "krig") {
          
          d <-
            st |>
            dplyr::select(.sid, x, y) |>
            dplyr::left_join(bi |> dplyr::filter(name == Species()) |> dplyr::select(.sid, n)) |>
            dplyr::mutate(n = tidyr::replace_na(n, 0),
                          n = sqrt(n)) |>
            dplyr::bind_rows(dummies)
          
          plot_idw(d, gr, iceland, eez)
          
          
        } else {
          
          if(type() == "range") {
            st_env <-
              st |>
              dplyr::mutate(z = -z) |>
              dplyr::select(.sid, Dýpi = z, Hiti = zt, Selta = zs) |>
              tidyr::gather(var, val, -.sid)
            
            st |>
              dplyr::inner_join(bi |> dplyr::filter(name == Species())) |>
              dplyr::mutate(z = -z) |>
              dplyr::select(.sid, Dýpi = z, Hiti = zt, Selta = zs) |>
              tidyr::gather(var, val, -.sid) |>
              ggplot2::ggplot(ggplot2::aes(x = 1, y = val)) +
              ggplot2::theme_bw(base_size = 20) +
              ggplot2::geom_violin(data = st_env, fill = "grey") +
              ggplot2::geom_violin(scale = "width", alpha = 0.5, colour = "red") +
              ggplot2::geom_jitter(size = 1, alpha = 0.5, colour = "red") +
              ggplot2::facet_wrap(~ var, scales = "free_y") +
              ggplot2::scale_x_discrete(NULL, NULL) +
              ggplot2::labs(y = NULL)
          }
        }
      }
    }
  })
  
  
  
  output$data <- shiny::renderTable({
    selected()
  })
  
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |> 
      leaflet::setView(lng = -18,
                       lat =  65,
                       zoom = 6) |> 
      leaflet::addTiles() |> 
      leaflet::addTiles(urlTemplate = "https://heima.hafro.is/~einarhj/test_again2/{z}/{x}/{-y}.png",
                        group = "Botnlag",
                        options = leaflet::tileOptions(minZoom = 5, maxZoom = 14))
    
  })
}

shiny::shinyApp(ui, server)


