
# Librerias ---------------------------------------------------------------
library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(stringr)


# Datos -------------------------------------------------------------------
catalogo <- read.csv(file="Base de datos/Catálogos.csv")
forma_venta <- read.csv(file="Base de datos/FORM_VENTA.csv")
variables <- read.csv(file="Base de datos/Descripcion de variables.csv")
entidad <- read.csv(file = "Base de datos/ENTIDAD.csv")
polizas <- read.csv(file="Base de datos/polizas_recodificadas.csv")
mod_pol <- read.csv(file="Base de datos/MOD_POL.csv")
status_poliza <- read.csv(file="Base de datos/STATUS_POL.csv")
status_siniestro <- read.csv(file="Base de datos/STATUS_SIN.csv")
nom_beneficios <- c("Fallecimiento"	,	
                    "Pérdidas_Orgánicas",
                    "Doble_Indemnización_por_Muerte_Accidental"	,
                    "Triple_Indeminización_por_Muerte_Colectiva",		
                    "Exención_de_Pago_de_Prima_por_Invalidez_Incapacidad_o_Muerte",
                    "Pago_Adicional_por_Invalidez_o_Incapacidad_efectuada_en_una_sola_exhibición",
                    "Rentas_Diferidas_Emisión_Individual_y_Grupo",
                    "Otros_Beneficios_Emisión_Individual_y_Grupo",
                    "Sobrevivencia")
polizas$S_suma <- sum(polizas$S_Doble_Indemnización_por_Muerte_Accidental,polizas$S_Fallecimiento,
                    polizas$`S_Otros Beneficios_Emisión_Individual_y_Grupo`,polizas$S_Pago_Adicional_por_Invalidez_o_Incapacidad_efectuada_en_una_sola_exhibición,
                    polizas$S_Pérdidas_Orgánicas,polizas$S_Sobrevivencia,polizas$S_Triple_Indeminización_por_Muerte_Colectiva)

polizas$suma <- sum(polizas$Doble_Indemnización_por_Muerte_Accidental,polizas$Fallecimiento,
                      polizas$`Otros Beneficios_Emisión_Individual_y_Grupo`,polizas$Pago_Adicional_por_Invalidez_o_Incapacidad_efectuada_en_una_sola_exhibición,
                      polizas$Pérdidas_Orgánicas,polizas$S_Sobrevivencia,polizas$Triple_Indeminización_por_Muerte_Colectiva,
                  polizas$Exención_de_Pago_de_Prima_por_Invalidez_Incapacidad_o_Muerte,polizas$Rentas_Difereidas_Emisión_Individual_y_Grupo
                    )

polizas$SEXO <- as.factor(polizas$SEXO)   

# Declaración de funciones ------------------------------------------------
skewness=function(x) {
  m3=mean((x-mean(x))^3)
  skew=m3/(sd(x)^3)
  skew}
skew2=function(x) {
  p75 = quantile(x,0.75)
  p25 = quantile(x,0.25)
  p50 =quantile(x,0.55)
  num=(p75-p50)-(p50-p25)
  den=p75-p25
  skewinter=num/den
  skewinter}
kurtosis=function(x) {
  m4=mean((x-mean(x))^4)
  kurt=m4/(sd(x)^4)-3 
  kurt}
resumen <- function(dato,nombre){
  sumario <- as.data.frame(cbind(
    nombre,
    min(dato, na.rm=TRUE),
    quantile(dato,0.25, na.rm=TRUE),
    median(dato, na.rm=TRUE),
    mean(dato, na.rm=TRUE),
    mean(dato,trim=5/100, na.rm=TRUE),
    quantile(dato,0.75, na.rm=TRUE),
    max(dato, na.rm=TRUE),
    length(dato),
    IQR(dato, na.rm=TRUE),
    sd(dato),
    var(dato),
    skewness(dato),
    kurtosis(dato)
  ))
  names(sumario) <- c("Variable","Mínimo","Quartil 1","Mediana","Media","Media restringida","Quartil 3","Máximo","Datos","Rango Intercuartilico",
                      "Desviación Estándar","Varianza","Asimetría","Curtosis")
  row.names(sumario) <- nombre
  return(sumario)
  
}
resumenall <- function(matrix){
  res <- c()
  res2 <- c()
  for(i in 1:length(matrix)){
    dato <- matrix[,i]
    nombre <- names(matrix)[i]
    res <- resumen(dato,nombre)
    res2 <- rbind(res2,res)
  }
  res2
}



# SERVER ------------------------------------------------------------------


    server <- function(input, output) {
                    data <- "polizas"
                    
    datos <- reactive ({  
                    # Filtros -----------------------------------------------------------------
                    
                      a <- ifelse(input$venta == "Todas", "filter()" , "filter(FORM_VENTA == input$venta)")
                    b <- ifelse(input$status == "Todas",  "filter()",  "filter(STATUS_POL == input$status)")
                    c <-  ifelse(input$tipo == "Todas", "filter()", "filter(MOD_POL == input$tipo)")
                    d <- ifelse(input$entidad == "Todas", "filter()", "filter(ENTIDAD == input$entidad)")
                    e <- ifelse(input$genero == "Ambos", "filter()", ifelse(input$genero == "Hombres",  "filter(SEXO == 'M')", "filter(SEXO == 'F')"))
                    s <- c()     
                    
                    nomm <- function(numero){
                      s <- c()
                      s2 <- c()
                      for (i in 1:numero) {
                        r <- input$Beneficio[i]
                        r <- as.array(r)
                        r2 <- paste("S_",r,sep="")
                        s <- paste(s," %>% filter(",r,"> 0)",sep = "")
                        s2 <- paste(s2," %>% filter(",r2,"> 0)",sep = "")
                      }
                      s <- paste(s,s2,sep="")
                      return(s)
                    }
                    
                    nomm2 <- function(numero){
                      s <- c()
                      s2 <- c()
                      for (i in 1:numero) {
                        r <- input$Beneficio[i]
                        r <- as.array(r)
                        r2 <- paste("S_",r,sep="")
                        s <- paste(s,r,sep=",")
                        s2 <- paste(s2,r2,sep=",")
                        
                      }
                      s <- paste(" %>% select(1:8",s2,s,",26:31)",sep="")
                      return(s)
                    }
                    
                    letrero <-  ifelse(input$Beneficio[1] == "Todas","x %>% filter()",
                                       paste('x',nomm2(dim(as.array(input$Beneficio))),nomm(dim(as.array(input$Beneficio))),sep=''))
                    
                    
                    filtros <- paste(data,a,b,c,d,e,sep = " %>% ")
                    x <- (eval(parse(text=filtros)))
                    x <- (eval(parse(text=letrero)))
                    x <- as.data.frame(x)
                    x <- as.data.frame(x)
                    
                    
                    graficar <- function(){
                      for (i in 16:24) {
                        f <- paste("+x[,",i,"]", sep = "")
                        s <- paste(s,f,sep="")
                      }
                      return(s)
                    }
                    
                    graficar3 <- function(numero){
                      for (i in 1:numero) {
                        r <- 15+i
                        f <- paste("+x[,input$Beneficio[",i,"]]", sep = "")
                        s <- paste(s,f,sep="")
                      }
                      s <- paste("0",s,sep="")
                      return(s)
                    }
                    
                    graficas <-  ifelse(input$Beneficio=="Todas",graficar(),
                                        graficar3(dim(as.array(input$Beneficio))))
                    
                    sumprimas <- as.numeric(eval(parse(text=graficas)))
                    sumprimas <- sumprimas
                    x$Total_prima <- sumprimas
                    x <- x %>% 
                      filter(Total_prima > input$precio[1]) %>% 
                      filter(Total_prima < input$precio[2])
                    
                    graficara <- function(){
                      for (i in 9:14) {
                        f <- paste("+x[,",i,"]", sep = "")
                        s <- paste(s,f,sep="")
                      }
                      return(s)
                    }
                    
                    graficar3a <- function(numero){
                      s <- c()
                      s2 <- c()
                      for (i in 1:numero) {
                        r <- input$Beneficio[i]
                        r <- as.array(r)
                        r2 <- as.array(paste("S_",r,sep=""))
                        f <- paste("+x[,",r2,"]", sep = "'")
                        s <- paste(s,f,sep="")
                      }
                      s <- paste("0",s,sep="")
                      return(s)
                    }
                    graficas2 <-  ifelse(input$Beneficio=="Todas",graficara(),
                                         graficar3a(dim(as.array(input$Beneficio))))
                    sumprimas2 <- as.numeric(eval(parse(text=graficas2)))
                    sumprimas2 <- sumprimas2
                    x$Total_Suma_Asegurada <- sumprimas2
                    x
                    })
                    
    # Histograma EDAD ---------------------------------------------------------
        output$distPlot <- renderPlot({
  
                  # Gráfica -----------------------------------------------------------------
                               ggplot(data=datos())+
                                   geom_histogram(aes(EDAD,fill=SEXO), color="black",bins=input$bins)+
                                   theme_minimal()+
                                   xlab("")+
                                   ylab("Cantidad")+
                                   ggtitle("Distribución de la edad")+
                                   theme(plot.title = element_text(size=35,vjust=0.8,face="bold"))+
                                   theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
                                   theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) 
                        
                                  
                           })
    # PRIMAS ------------------------------------------------------------------
       output$Primas <- renderPlot({
                # Gáfica ------------------------------------------------------------------
                              ggplot(data=as.data.frame(datos()))+
                                  geom_histogram(aes(as.numeric(Total_prima)),fill="steelblue", color="black",bins=input$bins)+
                                  theme_minimal()+
                                  xlab("")+
                                  ylab("Cantidad")+
                                  ggtitle("Distribución de las Primas Emitidas")+
                                  theme(plot.title = element_text(size=35,vjust=1,lineheight = 1.5,face="bold"))+
                                  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
                                  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) 
                           })

    # SUMAS ASEGURADAS --------------------------------------------------------
       output$Sumas <- renderPlot({
                 # Gráfica -----------------------------------------------------------------
                        datos <-  datos() %>% 
                            filter(Total_Suma_Asegurada < 1.5*IQR(Total_Suma_Asegurada))
                            ggplot(data=datos)+
                                  geom_histogram(aes(Total_Suma_Asegurada),fill="#009E73", color="black",bins=input$bins)+
                                  xlab("")+
                                   theme_minimal()+
                                  ylab("Cantidad")+
                                   ggtitle("Distribución de las Sumas Aseguradas")+
                                   theme(plot.title = element_text(size=35,vjust=1,lineheight = 1.5,face="bold"))+
                                   theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
                                   theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) 
                     
               
       })

    # EDAD --------------------------------------------------------------------
        output$resumen <- renderTable({
     
      # TABLA -------------------------------------------------------------------
          x <- as.data.frame(datos())
          x <- x %>% 
            select(-c("X","MOD_POL","STATUS_POL","ENTIDAD","FORM_VENTA","SUBT_SEG","P_E_DCP","S_F_ADMON",
                      "VENCIMIENTO","RESCATE","DIVIDENDO","SEXO"))
          table <- resumenall(x)
          table
  })
        output$sexo <- renderText({
          data <- as.data.frame(datos())
          data <- data %>% select("SEXO")
          a <- summary(data)
          a
    })
    # DATA --------------------------------------------------------------------
        output$data <- renderDataTable({
         
          # Data Table --------------------------------------------------------------
            datos <- as.data.frame(datos())
            datos
          
                })
       
    # DESCARGAR ---------------------------------------------------------------
      
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(datos(), file)
    }    )
    }

# UI ----------------------------------------------------------------------
   ui <- navbarPage("Seguros de Vida - 2019",
      # Pólizas -----------------------------------------------------------------
                    tabPanel("Pólizas", 
                            fluidPage(
                              theme = bs_theme(version = 4, bootswatch = "minty"),
                                          # Application title
                                  titlePanel("Distribucion de polizas."),
                                          
                                          sidebarLayout(
          # ENTRADAS ----------------------------------------------------------------
                                          sidebarPanel(  
                                            
                                                sliderInput("bins",
                                                              "Numero de segmentos:",
                                                              min =1 ,
                                                              max = 50,
                                                              value = 30),
                                                  
                                                 sliderInput("precio",
                                                              "Rango de la prima",
                                                              min = 1,
                                                              max = 500000,
                                                              value = c(50000,100000),
                                                              dragRange = TRUE,
                                                              round=TRUE),
                                                  
                                                  radioButtons("genero","Género",
                                                               choices = c("Ambos","Hombres","Mujeres")),
                                                  
                                                 selectInput("Beneficio","Tipo de Beneficio",c("Todas",nom_beneficios),selected = "Todas",multiple=TRUE
                                                 ),
                                                 
                                                  selectInput("venta", "Canal de venta",
                                                              choices = c("Todas",forma_venta[,2]),
                                                              selected = "Todas"
                                                  ),
                                                  selectInput("status", "Estatus de póliza",
                                                              choices = c("Todas",status_poliza[,2]),
                                                              selected = "Todas"
                                                  ),
                                                  selectInput("tipo", "Tipo de póliza",
                                                              choices = c("Todas",mod_pol[,2]),
                                                              selected = "Todas"
                                                  ),
                                                  
                                                  selectInput("entidad","Estado",c("Todas",entidad$Descripción),selected = "Todas", multiple = TRUE),
                                                  
                                                  submitButton("Aplicar Cambios"),
                                                
                                                downloadButton("downloadData", "Descargar Datos"),
                                                  
                                              ),
                                              

          # SALIDAS -----------------------------------------------------------------
                                            mainPanel(
                                                  tabsetPanel(
               # Gráficos ----------------------------------------------------------------
                                                    tabPanel("Gráficos", 
                                                             textOutput("sexo"),
                                                             plotOutput("distPlot"),
                                                             plotOutput("Primas"),
                                                             plotOutput("Sumas")
                                                             ),

               # Resumen Estadistico -----------------------------------------------------
                                                    tabPanel("Resumen Estadistico",
                                                             tableOutput("resumen")
                                                             ), 

               # Datos -------------------------------------------------------------------
                                                    tabPanel("Datos",
                                                             dataTableOutput("data")
                                                             )) )
                                              ))),

      # Sienestros --------------------------------------------------------------
                    tabPanel("Siniestros")
            )

# APP ---------------------------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)
