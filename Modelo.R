##########################################################################################

# Autor: Luis Fernado Flores
# Fecha: 13/08/2025
# Descripción: # Este código analiza precios de gasolina en Bolivia usando control sintético
#                y modelos de regresión lineal para proyecciones futuras.
#               - Construye un control sintético con datos históricos y unidades de control,
#                generando predictores, pesos y gráficos de tendencias y diferencias.
#               - Ajusta un modelo MCO para proyectar precios futuros, calculando valores
#                acumulativos y combinándolos con datos históricos.
# -              Genera gráficos comparativos mostrando precios históricos,
#                pronósticos, precios reales y sin subvención, con bandas de predicción y
#                leyendas personalizadas para facilitar la interpretación.





###########################################################################################
##############################          Librerías          ###############################
###########################################################################################
library(readxl)
library(globalmacrodata)
library(tidyverse)
library(countrycode)
library(Synth)
library(imputeTS)
library(tidysynth)
library(ggplot2)

###########################################################################################
##############################         Base de datos       ###############################
###########################################################################################

#Precios de la gasolina corriente

link <- "C:/Users/Fernando Flores/OneDrive/Libros/MEFP/Proyecto"

# Leer cada hoja usando file.path
Pgas <- read_excel(file.path(link, "Precios_consolidada.xlsx"))
pred_WTI <- read_excel(file.path(link, "Precios_consolidada.xlsx"), sheet = "Predicciones del WTI")
pregasolina <- read_excel(file.path(link, "Precios_consolidada.xlsx"), sheet = "pregasolina")


BIGprecios_gasolina <- Pgas %>% 
      pivot_longer(cols = -fecha,
                   names_to = "pais",
                   values_to = "pgas")

paises <- unique(BIGprecios_gasolina$pais)


###########################################################################################
##############################         ISO estándar        ###############################
###########################################################################################

iso3 <- countrycode(paises,
                    origin = "country.name",
                    destination = "iso3c",
                    custom_match = c(
                          "Bolivarian Republic of Venezuela" = "VEN",
                          "Islamic Republic of Iran" = "IRN",
                          "Republic of Türkiye" = "TUR",
                          "People's Republic of China" = "CHN",
                          "Republic of Moldova" = "MDA",
                          "Russian Federation" = "RUS",
                          "Slovak Republic" = "SVK",
                          "Czech Republic" = "CZE",
                          "Cote d'Ivoire" = "CIV",
                          "Korea" = "KOR",
                          "Lao People's Democratic Republic" = "LAO",
                          "United Republic of Tanzania" = "TZA",
                          "Viet Nam" = "VNM",
                          "Chinese Taipei" = "TWN",
                          "United States" = "USA",
                          "United Kingdom" = "GBR",
                          "Greenland" = "GRL",
                          "Palestine" = "PSE",
                          "European Union - 27" = NA,
                          "Europe" = NA,
                          "Africa" = NA,
                          "Americas" = NA,
                          "Asia" = NA,
                          "Oceania" = NA,
                          "OECD" = NA,
                          "IEA and Accession Association countries" = NA,
                          "G20" = NA,
                          "World" = NA
                    ))

# Combinar en un data frame
tabla_iso <- data.frame(pais = paises, iso3 = iso3)

BIGprecios_gasolina <- BIGprecios_gasolina %>% 
      left_join(tabla_iso, by = "pais")

BIGprecios_gasolina <- BIGprecios_gasolina %>% 
      rename(ISO3=iso3)


###########################################################################################
##########################    Limpieza de datos gasolina     ##############################
###########################################################################################

PGAS_LIMPIO <- BIGprecios_gasolina %>%
      filter(fecha >= "2000" & fecha <= "2023") %>%
      group_by(pais) %>%
      filter(sum(is.na(across(everything()))) <= 3) %>%
      ungroup()

# Imputación con interpolación lineal (usa vecinos cercanos)
PGAS_LIMPIO <- PGAS_LIMPIO %>%
      arrange(pais, fecha) %>%
      group_by(pais) %>%
      mutate(pgas = na_ma(pgas, k = 2, weighting = "linear")) %>%
      ungroup()


###########################################################################################
#############################        Variables Macro         ##############################
###########################################################################################

df <- gmd(variables = c("rGDP_USD", "pop", "CA_GDP", "USDfx", "govexp_GDP", "govdebt_GDP", "inv_GDP", "CPI"))

df <-df %>% 
      filter(year>=2000)

df <- df %>% 
      rename(
            pais = countryname,
            fecha = year)

DF_LIMPIO <- df %>%
      filter(fecha >= 2000, fecha <= 2023) %>%
      group_by(pais) %>%
      filter(all(complete.cases(across(everything())))) %>%
      ungroup()

DF_LIMPIO$rGDP_USDpep <- DF_LIMPIO$rGDP_USD/DF_LIMPIO$pop  


###########################################################################################
#############################        Base Consolidada       ##############################
###########################################################################################

DF_LIMPIO <- DF_LIMPIO %>%
      mutate(fecha =as.character(fecha))

Consolidada <-  PGAS_LIMPIO %>% #########################revisar porque hay na
      left_join(DF_LIMPIO, by =c("ISO3","fecha"))%>%
      select(  -pais.y, -pop)

paises_con_na <- Consolidada %>%
      filter(if_any(everything(), is.na)) %>%
      distinct(pais.x)

Consolidada_l <- Consolidada %>%
      filter(!pais.x %in% paises_con_na$pais.x) %>% 
      group_by(pais.x) %>%
      mutate(unit_id = cur_group_id()) %>%
      ungroup()

sum(is.na(Consolidada_l))


###########################################################################################
##############################     Control Sintético      ################################
###########################################################################################
Consolidada_l <- Consolidada_l %>% 
      mutate(fecha=as.integer(fecha))
DConso <- as.data.frame(Consolidada_l)



synthetic_out <- 
      DConso %>% 
      synthetic_control(
            outcome = pgas,                   # Variable dependiente
            unit = pais.x,                  # ID de unidad
            time = fecha,                   # Tiempo (año)
            i_unit = "Bolivia",                     # Unidad tratada
            i_time = 2009,                  # Año de tratamiento
            generate_placebos = TRUE        # Generar placebos para inferencia
      ) %>% 
      # Predictores principales (promedio 2000-2009)
      generate_predictor(
            time_window = 2000:2009,
            CA_GDP = mean(CA_GDP, na.rm = TRUE),
            USDfx = mean(USDfx, na.rm = TRUE),
            govexp_GDP = mean(govexp_GDP, na.rm = TRUE),
            inv_GDP = mean(inv_GDP, na.rm = TRUE),
            govdebt_GDP = mean(govdebt_GDP, na.rm = TRUE)
      ) %>% 
      # Predictores especiales (mediana o promedio en rangos específicos)
      generate_predictor(time_window = 2000:2006, CPI = mean(CPI, na.rm = TRUE)) %>% 
      generate_predictor(time_window = 2000:2010, rGDP_USDpep = mean(rGDP_USDpep, na.rm = TRUE)) %>% 
      generate_predictor(time_window = 2000, pgas2000 = mean(pgas, na.rm = TRUE)) %>% 
      generate_predictor(time_window = 2005, pgas2005 = mean(pgas, na.rm = TRUE)) %>% 
      generate_predictor(time_window = 2008, pgas2008 = mean(pgas, na.rm = TRUE)) %>% 
      
      # Generar pesos, con ventana de optimización 2000-2009
      generate_weights(
            optimization_window = 2000:2009,
            margin_ipop = 0.02,
            sigf_ipop = 7,
            bound_ipop = 6
      ) %>% 
      # Construir control sintético
      generate_control()


###########################################################################################
##############################         Gráficos Tendencias     ############################
###########################################################################################

p <- synthetic_out %>% plot_trends()

# Personalizar
p_custom <- p +
      labs(x = NULL, y = NULL, title = NULL) +   # Sin títulos
      theme(
            legend.position = c(0.8, 0.2),           # Leyenda en esquina (ajusta coordenadas)
            legend.background = element_rect(fill = "white", color = "black"),  # Cuadro blanco con borde negro para leyenda
            legend.title = element_blank(),          # Sin título en leyenda
            axis.title = element_blank(),             # Sin títulos de ejes
            axis.text = element_text(size = 12),
            guides(linetype = guide_legend(title = NULL)),
            panel.grid.minor = element_blank()        # Quitar cuadricula menor si quieres
      ) +
      scale_linetype_manual(values = c("solid", "dashed"))  # Líneas sólidas (para real y sintética)

print(p_custom)



###########################################################################################
##############################        Gráficos Diferencias      ##########################
###########################################################################################

synthetic_out %>% 
      plot_differences() +
      theme(
            legend.position = "none",        # quita la leyenda
            plot.title = element_blank(),    # quita el título principal
            axis.title = element_blank()     # quita los títulos de los ejes (texto "x" y "y")
      )


###########################################################################################
##############################        Tablas y Pesos           ##########################
###########################################################################################

synthetic_out %>% grab_balance_table()
synthetic_out %>% plot_weights()


filtered <- synthetic_out %>% filter(.id == "Bolivia", .type == "treated")
unit_weights_list <- filtered %>% pull(.unit_weights)
print(unit_weights_list)

filtered <- synthetic_out %>% filter(.id == "Bolivia", .type == "treated")
predictor_weights_list <- filtered %>% pull(.predictor_weights)
predictor_weights_list[[1]] %>% arrange(desc(weight))
###################################################################################
###################             #######################
###################################################################################
##################         PREDICCIONES               ##################################

###################################################################################
###################   Preparación para proyecciones             #######################
###################################################################################

precio_sintetico <- data.frame(synthetic_out[[6]][[2]])
precio_sintetico$año <- precio_sintetico$time_unit
base_est <- pred_WTI %>%
      left_join(precio_sintetico, by = "año")
base_est$p_sintético <- base_est$synth_y
base_est <- base_est %>%
      arrange(time_unit) %>%
      mutate(
            varwti    = (WTI - lag(WTI)) / lag(WTI),
            real_y_diff_pct = (real_y - lag(real_y)) / lag(real_y),
            varsin = (synth_y - lag(synth_y)) / lag(synth_y)
      )
Datosresultados_sintetico <- base_est

###########################################################################################
##############################        Preparación de Datos       ##########################
###########################################################################################

# Separar datos históricos (hasta 2023) para estimar
datos_hist <- Datosresultados_sintetico %>%
      filter(año <= 2023)

# Ajustar el modelo MCO
modelo <- lm(varsin ~ varwti-1, data = datos_hist)

# Datos futuros (desde 2024)
datos_fut <- Datosresultados_sintetico %>%
      filter(año > 2022)

# Pronóstico con bandas de predicción
pred <- predict(modelo, newdata = datos_fut, interval = "prediction", level = 0.95)

# Unir predicciones con los datos futuros
pred_df <- cbind(datos_fut, pred)

# Unir históricos y predicciones para graficar
plot_df <- datos_hist %>%
      mutate(tipo = "Histórico") %>%
      bind_rows(
            pred_df %>%
                  select(-varsin) %>%        # quitamos la original con NA
                  rename(varsin = fit) %>%
                  mutate(tipo = "Pronóstico")
      )


###########################################################################################
##############################        Gráfico Estilo EViews       ########################
###########################################################################################

ggplot() +
      geom_point(data = plot_df %>% filter(tipo == "Histórico"),
                 aes(x = año, y = varsin), color = "blue") +
      geom_line(data = plot_df %>% filter(tipo == "Histórico"),
                aes(x = año, y = varsin), color = "blue") +
      geom_line(data = plot_df %>% filter(tipo == "Pronóstico"),
                aes(x = año, y = varsin), color = "red") +
      geom_ribbon(data = pred_df,
                  aes(x = año, ymin = lwr, ymax = upr),
                  fill = "pink", alpha = 0.3) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(plot.title = element_blank())

summary(modelo)


###########################################################################################
##############################        Pronóstico p_sintético      ########################
###########################################################################################

pred_value <- data.frame(fit = pred[, "fit"])

# Último valor conocido de p_sintético
ultimo_valor <- Datosresultados_sintetico %>%
      filter(!is.na(p_sintético)) %>%
      arrange(desc(año)) %>%
      slice(1) %>%
      pull(p_sintético)

# Número de años futuros
n_futuros <- nrow(pred_value)

# Vector para guardar los p_sintético proyectados
p_proyectado <- numeric(n_futuros)

# Calculamos acumulando las variaciones
for (i in 1:n_futuros) {
      if (i == 1) {
            p_proyectado[i] <- ultimo_valor * (1 + pred_value$fit[i])
      } else {
            p_proyectado[i] <- p_proyectado[i-1] * (1 + pred_value$fit[i])
      }
}

# Actualizar dataframe con valores proyectados
Datosresultados_sintetico <- Datosresultados_sintetico %>%
      arrange(año) %>%
      mutate(
            p_sintético_completo = ifelse(!is.na(p_sintético), p_sintético, NA_real_)
      )

# Rellenar los NA con los valores proyectados
Datosresultados_sintetico$p_sintético_completo[Datosresultados_sintetico$año > 2023] <- p_proyectado

# Ver resultado
Datosresultados_sintetico %>%
      select(año, p_sintético, p_sintético_completo)


###########################################################################################
##############################        Gráfico Pronóstico p_sintético      ################
###########################################################################################

ggplot(Datosresultados_sintetico, aes(x = año, y = p_sintético_completo)) +
      geom_line(color = "blue", size = 1) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "black", size = 0.8)+
      labs(title = "", x = "", y = "") +
      theme_minimal() +
      theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
      )


###########################################################################################
##############################        Gráfico con eliminación de subsidios        ################
###########################################################################################

ggplot() +
      geom_line(data = Datosresultados_sintetico, aes(x = año, y = p_sintético_completo, color = "Precio de mercado"), size = 1) +
      geom_line(data = pregasolina, aes(x = Año, y = `Precio actual`, color = "Precio actual"), size = 1) +
      geom_line(data = pregasolina, aes(x = Año, y = `Precio sin subenvión`, color = "Corrección sin subvención"), size = 1) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "gray", size = 1.2) +
      scale_color_manual(values = c(
            "Precio de mercado" = "blue",
            "Corrección sin subvención" = "black",
            "Precio actual" = "grey"
      )) +
      labs(title = "", x = "", y = "", color = NULL) +
      theme_minimal() +
      theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.position = c(0.85, 0.15),
            legend.background = element_rect(fill = "white", color = "black"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size = 10)
      )