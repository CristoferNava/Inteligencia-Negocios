# usando la ruta de mi equipo
accidents <- read.csv("/Users/cristofer/Downloads/accidents2004.csv")
casualty <- read.csv("/Users/cristofer/Downloads/casualty2004.csv")
vehicles <- read.csv("/Users/cristofer/Downloads/vehicles2004.csv")

# usando una ruta web
casualty2 <- read.csv("https://raw.githubusercontent.com/CristoferNava/Inteligencia-Negocios/master/data/casualty2004.csv")

# con otro data set carga sin problemas
test <- read.csv("https://raw.githubusercontent.com/CristoferNava/Inteligencia-Negocios/master/data/Ch4_regression_bikes.csv")