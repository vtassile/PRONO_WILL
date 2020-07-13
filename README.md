# PREDICCIÓN DEL CRECIMIENTO DE FRUTOS DE PEPITA

## INTRODUCCIÓN

La presente aplicación permite predecir el crecimiento de frutos de Pepita-Variedad Williams a partír de la fecha posterior a plena floración. La propuesta de predicción se fundamenta en modelos ajustados previamente.

Los modelos ajustados se corresponden a modelos mixtos no lineales que incorporan la termoacumulación de temperaturas por sobre umbrales empíricos a los efectos de considerar el efecto que las temperaturas tienen sobre los procesos biológicos involucrados en el crecimientos de los frutos.

La aplicación se ha construido en Shiny, un paquete de R que posibilita la construcción de aplicaciones web interactivas. Durante el proceso de elaboración tambíen se consideraron las siguientes herramientas tecnologicas:

-HTML: Lenguaje de marcado que se utiliza para el desarrollo de páginas Web.

-CSS: Lenguaje de hojas de estilos creado para controlar el aspecto o presentación de los documentos electrónicos definidos con HTML.

-JAVASCRIPT: lenguaje de programación que se utiliza principalmente para crear páginas web dinámica.

-LATEX: Un sistema de preparación de documentos.

R: Un entorno y lenguaje de programación con un enfoque al análisis estadístico.

## MOTIVACIÓN

El conocimiento del peso final de frutos con antelación a la cosecha es una información de vital importancia por las consecuencias que esta tiene sobre las estrategias de manejo del monte frutal, las tareas de empaque y la logística de comercialización. Es por ello que la predicción de la distribución final del tamaño de frutos, a partir de muestreos previos a la cosecha, es una práctica usual en las principales regiones de producción frutícola.

Para el caso de los frutos de pepita, el proceso de crecimiento del diámetro del fruto a través del tiempo responde a un modelo sigmoideo en forma de S, que parte de algún punto fijo e incrementa su pendiente en forma monótona hasta alcanzar un punto de inflexión, en el que la pendiente comienza a decrecer y la función a aproximarse en forma asintótica al valor definitivo. El modelo utilizado para describir este comportamiento ha sido ampliamente estudiado, **Bramardi et.al**, en sus trabajos de medidas de no linealidad propone el modelo Logístico en la tercer parametrización según *Ratkowski * para describir el crecimiento de frutos de pera de los cultivares William’s y Packam’s Triumph.

Existe además, un nivel de variabilidad estocástica en el crecimiento de los frutos que es gobernada por factores que no son fácilmente observables, como el sitio, la genética, las labores culturales y las condiciones climáticas. La acción simultánea de los factores antes mencionados generan la componente estocástica de la variabilidad que provee la razón fundamental para utilizar el enfoque de los modelos mixtos no lineales (**NLME**) los cuales permiten modelar además de los efectos fijos, los efectos aleatorios generados en el proceso. Los **NLME** se constituyen también como una propuesta superadora por razones de interpretabilidad, parsimonia y, por sobre todo, validación más allá del rango de datos observado.

La información sobre los que se desarrolla el presente trabajo es el producto de una extensa labor desarrollada por los integrantes del Área de Estadística de la Facultad de Ciencias Agrarias de la Universidad Nacional del Comahue (UNCo) . Los datos recopilados han sido material de estudio de varios proyectos de investigación y algunos de ellos, de la tesis de Maestría del Dr. Sergio Bramardi. Posteriormente estos se han integrado a la base de datos del Programa de Pronóstico de Producción de Frutos de Pepita de las Provincias de Rio Negro y Neuquén (PPPRNN).

## PREDICCIÓN DEL TAMAÑO DEL FRUTO

Entre los factores que definen la calidad de los frutos, el tamaño reviste una característica de gran importancia económica, diferencias en pocos gramos en el peso medio de los frutos representan importantes pérdidas en los retornos por exportación. Las diferencias en tamaños entre los frutos, se debe principalmente al número y tamaño de las células de los frutos. En el cultivar William’s es importante disponer, al inicio de la cosecha, de una elevada proporción de frutos con tamaños apropiados a los requerimientos comerciales. El manejo tradicional de la cosecha que se realiza en la región del Alto Valle, es efectuando varias “pasadas” seguidas de un riego para incrementar el tamaño de los frutos, esto incrementa los costos. Además, se deteriora la calidad de la producción por el avance de la madurez de los frutos recolectados en las últimas “pasadas”. Apuntar a realizar la cosecha en dos “pasadas” debería ser considerado como una meta de máxima. Al escenario descrito se suma la concurrencia en las plantas de empaque, a fines del mes de enero, de importantes volúmenes de manzanas Royal Gala y otros clones mejorados, lo que complica sustancialmente su manejo. En la fruticultura actual, los programas de raleo de frutos son de vital importancia para la obtención de fruta de alta calidad que permita al productor competir en el mercado internacional.

Por otra parte el conocimiento anticipado de la distribución de tamaños posibilita la confección de estrategias de manejo en post-cosecha de empaque, conservación y la logística de comercialización ya que el precio diferencial por frutos de distintos tamaños comerciales es muy marcada. La organización de una logística de comercialización acorde a los tamaños predichos crea ventajas competitivas importantes posibilitando reorientar acuerdos comerciales previos a la recolección de los frutos.


## MODELOS NO LINEALES DE EFECTOS MIXTOS

Los Modelos No Lineales de Efectos Mixtos (**NLME**) constituyen un enfoque ampliamente utilizado en la actualidad para el análisis de medidas repetidas cuando el interés se centra en características específicas de los individuos. Históricamente estos modelos han recibido una gran atención en la literatura estadística a finales de 1980, desarrollándose una serie de nuevos métodos computacionales para su tratamiento en la década de 1990. En la actualidad, los **NLME** son ampliamente utilizados en numerosos campos, como biología, agricultura, medio ambiente, medicina y economía, y están disponibles a través de varios paquetes de software.


## MÉTRICAS DE COMPARACIÓN DE MODELOS

Las distribuciones aproximadas de los estimadores se utilizan para producir pruebas de hipótesis e intervalos de confianza para el modelo. La selección del modelo óptimo entre múltiples modelos candidatos representa un importante desafio, esta suele realizarse a partir de los tradicionales **CI** y la prueba **LRT**, los cuales se obtienen como funciones de las verosimilitudes marginales.</p>
Por otra parte, se dispone también de los llamados Criterios Predictivos (**CP**). Estos hacen uso de valores predichos que permiten construir métricas de la capacidad predictiva. Los <b>CP</b> más utilizados son: Coeficiente de Correlación, Coeficiente de Concordancia, Suma de Cuadrados de Errores de Predicción, Raíz del Cuadrado Medio del Error, Sesgo Medio, Sesgo Medio Relativo, Sesgo Medio Absoluto y Sesgo Medio Relativo Absoluto.

## CRITERIOS PREDICTIVOS

En el análisis de regresión por mínimos cuadrados clásico, el coeficiente de determinación , o su forma ajustada  es extensamente utilizado a pesar de sus limitaciones como las descriptas por <i>Lvalseth </i>. R^2 , RMSE, y los gráficos de residuales son probablemente las medidas de bondad de ajuste más comúnmente utilizadas en modelación. Aquí, R^2  es muy popular ya que representa la proporción de la variación total en la variable dependiente que es explicada por un modelo ajustado. Es una medida directa y fácilmente comprensible del éxito en la predicción de la variable dependiente por las variables independientes.
Sin embargo, para los modelos mixtos en general, y los <b>NLME</b> en particular, el concepto de variación total puede ser definida de diferentes maneras dependiendo del criterio y el método utilizado en la estimación. Los modelos mixtos pueden tener varios componentes de varianza, además el modelo ajustado puede promediar un único modelo para la población, o un modelo por cada individuo muestreado dentro de la población. No hay una simple definición de  R^2  para modelos mixtos, y varios  R^2  han sido propuestos. Estos, sin embargo, pueden indicar cosas muy diferentes, con interpretaciones también diferentes.
Mientras que los estadísticos clásicos como  <b>LRT </b>,  <b>AIC </b> y  <b>BIC </b> son utilizados generalmente para comparar  <b>NLME </b> alternativos, los valores actuales de estos estadísticos poco dicen acerca de lo bueno o malo que es un modelo. Ellos son imposibles de interpretar sin un marco de referencia [10].

<H2>MÉTODOS PARA VALIDAR MODELOS </H2>

<p> Para realizar predicciones en un \acs{nlme} es necesario realizar un procedimiento en dos etapas:

<ul>
<li> La <b>PRIMERA ETAPA</b> consiste en calibrar el modelo que se ha ajustado, lo que involucra la predicción de los efectos aleatorios (**EBE**) para los sujetos de interés. Estos sujetos pueden ser nuevos individuos, es decir individuos no utilizados en el procedimiento de ajuste del modelo.
 Para ello se requieren algunos datos de estos individuos para poder calibrar el modelo, pero generalmente la cantidad disponible de estos es menor a la presente en el ajuste del modelo (**Huang**).</li>

<li> La <b>SEGUNDA ETAPA</b> consiste en predecir el valor de la variable de respuesta con las estimaciones de los parámetros de los efectos fijos obtenidos del modelo ajustado, las predicciones de los efectos aleatorios resultantes de la calibración del modelo y el modelo en cuestión.</li>




