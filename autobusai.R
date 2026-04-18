library(tidyverse)
library(dplyr)
library(fda)
library(rnaturalearth)
library(sf)
library(leaflet)
library(gridExtra)
library(shiny)
library(ggplot2)
library(htmltools)
library(readr)
library(readxl)

#### Maršrutų žemėlapis ####
stops <- read_csv("stops.txt",show_col_types = FALSE)
trips <- read_csv("trips.txt",show_col_types = FALSE)
routes <- read_csv("routes.txt",show_col_types = FALSE)
stop_times <- read_csv("stop_times.txt",show_col_types = FALSE)
shapes <- read_csv("shapes.txt",show_col_types = FALSE)

stop_routes <- stop_times%>% left_join(trips, by = "trip_id")%>%
  left_join(routes, by = "route_id")%>%
  dplyr::select(stop_id, route_id, route_short_name, route_long_name)%>% distinct()

stop_routes_summary <- stop_routes%>% group_by(stop_id)%>%
  summarise(autobusai = paste(unique(route_short_name), collapse = ", "))

stops_full <- stops%>% left_join(stop_routes_summary, by = "stop_id")

stops_sf <- st_as_sf(stops_full, coords = c("stop_lon", "stop_lat"), crs = 4326)

shape_route <- trips%>% dplyr::select(route_id, shape_id)%>% distinct()

shapes_full <- shapes%>% left_join(shape_route, by = "shape_id")%>%
  left_join(routes, by = "route_id")

shapes_sf <- shapes_full%>% arrange(shape_id, shape_pt_sequence)%>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE)

shapes_lines <- shapes_sf%>%
  group_by(shape_id, route_id, route_short_name, route_color)%>%
  summarise(do_union = FALSE)%>% st_cast("LINESTRING")

shapes_lines$route_color[is.na(shapes_lines$route_color)] <- "0000FF"
shapes_lines$route_color <- paste0("#", shapes_lines$route_color)

leaflet()%>% addProviderTiles("CartoDB.Positron")%>%
  addPolylines(data = shapes_lines,color = ~route_color,weight = 3,
    popup = ~paste("Maršrutas:", route_short_name), group = "Maršrutai")%>%
  addCircleMarkers(data = stops_sf,radius = 4,fillColor = "black",
    color = "white",weight = 1, popup = ~paste0( "<b>", stop_name, "</b><br/>",
      "Autobusai: ", ifelse(is.na(autobusai), "Nėra duomenų", autobusai)))%>%
  addLegend(position = "bottomright",
    colors = c("#DC3131", "#008000", "#0073AC", "#000000"),
    labels = c("Troleibusai", "Greitieji autobusai","Autobusai","Naktiniai autobusai"),
    title = "Maršrutų tipai", opacity = 1)

#Stoteliu sk. kiekvienoje seniunijoje
shp <- st_read("C:/Users/emili/Desktop/Bakalauro darbas/Vilniaus_miesto_seniūnijų_ribos.shp")
shp <- st_transform(shp, 4326)
stops_with_seniunija <- st_join(stops_sf, shp)
stoteliu_skaicius <- stops_with_seniunija %>%
  st_drop_geometry() %>%
  count(SENIUNIJA, sort = TRUE)


#### Maksimalus keleiviu skaicius TOP 10 marsrutu ####
autobusai <- read_excel("duomenys.xlsx", sheet = "Autobusai", range = "A1:E25")
pavad <- autobusai%>% pull(1)
shapes_lines_filtered <- shapes_lines%>% filter(route_short_name %in% pavad)
stoteles <- read.csv("Viešojo transporto stotelės.csv")
duomenys_stoteliu <- st_as_sf(stoteles, coords = c("X", "Y"), crs = 3346)%>%
  st_transform(4326)
autobusai_sf <- autobusai%>%
  rename(route_short_name = 1, max_keleiviai = `Max keleivių sk.`,lat = Y,lon = X)%>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

autobusai_sf <- autobusai%>% rename(route_short_name = 1, max_keleiviai = `Max keleivių sk.`,
    lat = Y, lon = X)%>% st_as_sf(coords = c("lon", "lat"), crs = 4326)%>%
  left_join(duomenys_stoteliu%>% st_drop_geometry()%>%
      dplyr::select(pavadinimas, suolas, svieslente,paviljonas),
    by = c("Stotelė" = "pavadinimas"))

leaflet()%>% addProviderTiles("CartoDB.Positron")%>%
  addPolylines(data = shapes_lines_filtered, color = ~route_color,weight = 3,
    popup = ~paste("Maršrutas:", route_short_name))%>%
  addCircleMarkers(data = autobusai_sf, radius = 4, fillColor = "red", color = "white",
    weight = 1, fillOpacity = 0.9, popup = ~paste0(
      "<b>Stotelė: </b>", Stotelė, "<br/>",
      "<b>Maršrutas: </b>", route_short_name, "<br/>",
      "<b>Max keleivių sk.: </b>", max_keleiviai, "<br/>",
      "<b>Suolo statusas: </b>", suolas, "<br/>",
      "<b>Švieslentes statusas: </b>", svieslente, "<br/>",
      "<b>Pavilijono statusas: </b>", paviljonas))


#### Autobusų keleivių skaičiaus kreivės interaktyvus ####
keleiviai <- read_excel("duomenys.xlsx",sheet = "Autobusu keleiviai")
keleiviai$route_short_name <- as.factor(keleiviai$route_short_name)
ui <- fluidPage(titlePanel("Vilniaus viešojo transporto užimtumas"),
  fluidRow(column(3,wellPanel(
             selectInput("route_short_name","Maršrutas:",choices = unique(keleiviai$route_short_name))))),
  plotOutput("people"))

server <- function(input, output) {
  output$people <- renderPlot({
    df <- keleiviai%>% filter(route_short_name == input$route_short_name)
    ggplot(df, aes(x = Valanda, y = `Max keleivių sk.`)) +
      geom_line(color = "#78003F", linewidth = 1) +
      geom_point(color = "#E64164", size = 2) +
      theme_minimal()})}
shinyApp(ui, server)

#### Funkciniu duomenu analize ####
####SPEC
specialieji <- keleiviai%>% filter(grepl("T|G|N", route_short_name))
s<-ggplot(specialieji, aes(x=Valanda,y=`Max keleivių sk.`, group=route_short_name))+
  geom_line()+
  labs(title = "Specialiųjų maršrutų keleivių sk.")
be_isskirties<- specialieji %>% filter(route_short_name!='"5G"')
spec <- be_isskirties%>% pivot_wider(names_from = route_short_name,
    values_from = `Max keleivių sk.`)
argvals <- spec$Valanda
y <- as.matrix(spec[,-1])
basisobj <- create.bspline.basis(rangeval = range(argvals),nbasis = 10)
spec <- smooth.basis(argvals,y,basisobj)$fd
plot(spec,lwd = 1.3,xlab = "Valanda", ylab = "Keleivių sk.")
spec_vidurkis <- mean.fd(spec)
lines(spec_vidurkis, col = "red", lwd = 2)
spec_sd <- std.fd(spec)
lines(spec_sd, col='blue', lwd=2)
legend("topright",legend = c("Vidurkis", "Standartinis nuokrypis"),
       col = c("red", "blue"),lwd = c(2, 2),cex = 0.5)

spec_var <- var.fd(spec)
t <- seq(1,24, length.out = 50)
cov_mat <- eval.bifd(t, t, spec_var)
par(mfrow = c(1, 2))
persp(t, t, cov_mat,xlab = "Valanda",ylab = "Valanda",zlab = "",
      theta =-30, phi = 25,r=3,expand=0.8,ticktype = 'detailed')
image(t, t, cov_mat,xlab = "Valanda",ylab = "Valanda",col = heat.colors(100))
contour(t, t, cov_mat, col= "black", add = TRUE)

####AUTOBUSAI
autobusai <- keleiviai %>%
  filter(!grepl("T|N|G", route_short_name))
a<-ggplot(autobusai, aes(x=Valanda,y=`Max keleivių sk.`, group=route_short_name))+
  geom_line()+
  labs(title = "Autobuso maršrutų keleivių sk.")
grid.arrange(s,a, nrow=1)
autob <- autobusai %>%pivot_wider(names_from = route_short_name,
                                  values_from = `Max keleivių sk.`)

argvals <- autob$Valanda
y <- as.matrix(autob[,-1])
basisobj <- create.bspline.basis(rangeval = range(argvals), nbasis = 10)
autob <- smooth.basis(argvals,y,basisobj)$fd
plot(autob,lwd = 1.3,xlab = "Valanda", ylab = "Keleivių sk.")
autob_vidurkis <- mean.fd(autob)
lines(autob_vidurkis, col = "red", lwd = 2)
autob_sd <- std.fd(autob)
lines(autob_sd, col='blue', lwd=2)
legend("topright",legend = c("Vidurkis", "Standartinis nuokrypis"),
       col = c("red", "blue"),lwd = c(2, 2),cex = 0.5)


autob_var <- var.fd(autob)
t <- seq(1,24, length.out = 50)
cov_mat <- eval.bifd(t, t, autob_var)
par(mfrow = c(1, 2))
persp(t, t, cov_mat,xlab = "Valanda", ylab = "Valanda", zlab = "",
      theta = -30, phi = 25, r = 3, expand = 0.5, ticktype = "detailed")
image(t, t, cov_mat,xlab = "Valanda", ylab = "Valanda",col = heat.colors(100))
contour(t, t, cov_mat,col= "black", add = TRUE)


#Keleiviu skirtumai tarp grupiu
library(fda.usc)
values_all <- cbind(eval.fd(argvals, spec), eval.fd(argvals, autob))
fd_fdata <- fdata(values_all, argvals = argvals, rangeval = range(argvals))
group_fac <- factor(c(
  rep("Spec", ncol(spec$coefs)),
  rep("Autob", ncol(autob$coefs))))

#skirtumai tarp tipu:
(res_onefactor <- fanova.onefactor(fd_fdata, group_fac))

plot(spec_vidurkis,col = "blue",lwd = 2, type="l", lty=1,
     xlab = "Valanda",ylab = "Keleivių sk.")
lines(autob_vidurkis,col = "red",lwd = 2)
legend("bottom",legend = c("Specialūs", "Autobusai"),
       title = "Maršruto tipas", col = c("blue", "red"),lwd = 2,cex = 0.7)

#skirtumai tarp kiekvieno laiko momento:
#Pointwise testas:
Ztwosample <- function(x, y, t.seq, alpha=0.05) {
  if(class(x) != "fd") stop("X must be fd object")
  if(class(y) != "fd") stop("Y must be fd object")
  k <- length(t.seq)
  mu.x <- mean.fd(x)
  mu.y <- mean.fd(y)
  n <- dim(x$coef)[2]
  m <- dim(y$coef)[2]
  delta <- (mu.x - mu.y)
  delta.t <- eval.fd(t.seq, delta)
  z.x <- center.fd(x)
  z.y <- center.fd(y)
  z.x.t <- eval.fd(t.seq, z.x)
  z.y.t <- eval.fd(t.seq, z.y)
  z.t <- cbind(z.x.t, z.y.t)
  Sigma <- (z.t %*% t(z.t)) / (n + m - 2)
  gamma.t <- diag(Sigma)
  gamma.t[gamma.t == 0] <- 1e-10
  Zpointwise <- sqrt((n*m)/(n+m)) * delta.t / sqrt(gamma.t)
  crit <- qt(1 - alpha/2, n + m - 2)
  crit.val <- rep(crit, k)
  params <- list(critical.value = crit)
  mx <- max(cbind(Zpointwise, crit.val))
  plot(t.seq, Zpointwise, type="l", xlab='Valanda', ylab="Z statistika", ylim=c(0, mx+0.5))
  lines(t.seq, crit.val, lty=2, lwd=1, col="red")
  legend("bottomright", legend = "Kritinė reikšmė 1,983",
         lty = 2, col = "red", bty = "n", cex = 0.7)
  return(list(statistics.pointwise = Zpointwise,
              params = params))}
Ztwosample(spec, autob, argvals)

#keleiviu augimo greitis
par(mfrow = c(1, 2))
(spec_deriv <- deriv.fd(spec))
specialiuju_augimas<-plot(spec_deriv, col = "blue", lwd = 1.2, xlab = "Valanda", ylab = "Augimo greitis", 
                          main = "Specialių maršrutų keleivių augimo greitis", cex.main = 0.7)
abline(h = 0, lty = 2)

(autob_deriv <- deriv.fd(autob))
autobuau_augimas<-plot(autob_deriv, col = "blue", lwd = 1.2, xlab = "Valanda", ylab = "Augimo greitis", 
                          main = "Autobusų maršrutų keleivių augimo greitis", cex.main = 0.7)
abline(h = 0, lty = 2)
time_points <- seq(1, 24, length.out = 200)
eval_mat <- eval.fd(time_points, spec_deriv)
df <- as.data.frame(eval_mat)
df$Valanda <- time_points
df_long <- pivot_longer(df, cols = -Valanda, names_to = "Marsrutas", values_to = "Augimo_greitis")
p <- ggplot(df_long, aes(x = Valanda, y = Augimo_greitis, color = Marsrutas)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Specialių maršrutų keleivių augimo greitis",
       x = "Valanda",y = "Augimo greitis") +
  theme_minimal() +
  theme(legend.position = "none")
ggplotly(p, tooltip = c("Marsrutas", "Valanda", "Augimo_greitis"))

