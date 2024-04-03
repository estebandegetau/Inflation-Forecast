// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

#show ref: it => locate(loc => {
  let target = query(it.target, loc).first()
  if it.at("supplement", default: none) == none {
    it
    return
  }

  let sup = it.supplement.text.matches(regex("^45127368-afa1-446a-820f-fc64c546b2c5%(.*)")).at(0, default: none)
  if sup != none {
    let parent_id = sup.captures.first()
    let parent_figure = query(label(parent_id), loc).first()
    let parent_location = parent_figure.location()

    let counters = numbering(
      parent_figure.at("numbering"), 
      ..parent_figure.at("counter").at(parent_location))
      
    let subcounter = numbering(
      target.at("numbering"),
      ..target.at("counter").at(target.location()))
    
    // NOTE there's a nonbreaking space in the block below
    link(target.location(), [#parent_figure.at("supplement") #counters#subcounter])
  } else {
    it
  }
})

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      block(
        inset: 1pt, 
        width: 100%, 
        block(fill: white, width: 100%, inset: 8pt, body)))
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[Abstract] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}
#show: doc => article(
  title: [Pronóstico de inflación],
  authors: (
    ( name: [Esteban Degetau],
      affiliation: [],
      email: [esteban.degetau\@imss.gob.mx] ),
    ),
  lang: "es",
  toc_title: [Tabla de contenidos],
  toc_depth: 3,
  cols: 1,
  doc,
)
#import "@preview/fontawesome:0.1.0": *


#block[
#callout(
body: 
[
Este reporte se actualizó por última vez el 02 abril 2024 a las 12:05 hrs CDMX.

]
, 
title: 
[
Nota
]
, 
background_color: 
rgb("#dae6fb")
, 
icon_color: 
rgb("#0758E5")
, 
icon: 
fa-info()
, 
)
]
= Modelos univariados
<modelos-univariados>
Hay al menos 50 maneras distintas de predecir el futuro, lo que implica que todavía no tenemos una herramienta de predicción perfecta #cite(<hamming1997>);. Una manera de generar predicciones automáticamente es con modelos univariados, que solo utilizan una serie de tiempo como insumo de sus predicciones #cite(<hyndman2008>);.

En esta sección, comparamos los modelos ARIMA, ETS, LM, y NNETAR para predecir la inflación mensual en México.#footnote[ARIMA: Modelo autorregresivo integrado de media móvil.

ETS: Suavización exponencial

LM: Modelo lineal

NNETAR: Redes neuronales para series de tiempo] De estos, el modelo ARIMA es el más conocido y utilizado en la literatura. Los modelos ETS y LM son útiles por ser más simples. Por último, el modelo NNETAR es un modelo de redes neuronales para series de tiempo que ha demostrado ser útil en la predicción de series de tiempo #cite(<allen2022>);.

== Probando los modelos
<probando-los-modelos>
Una manera de comparar distintos modelos de predicción es evaluar su precisión en un conjunto de datos externo; es decir, en un conjunto de datos que no se utilizó para ajustar los modelos #cite(<hyndman2021>);#cite(<james2021a>);. En este caso, utilizamos los datos de inflación mensual desde enero 1994 hasta abril 2022 para ajustar los modelos y los datos desde mayo 2022 hasta febrero 2024 para evaluar su precisión. La @fig-timeline muestra la serie de tiempo de inflación mensual \(mes con mes) y los periodos de entrenamiento y prueba.

#block[
#block[
#figure([
#box(width: 864.0pt, image("monthly_forecast_files/figure-typst/fig-timeline-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Línea de tiempo de entrenamiento, prueba y pronóstico
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-timeline>


]
]
La @fig-test muestra las predicciones de los modelos en el conjunto de prueba contra la inflación mensual observada. El modelo NNETAR parece ser el más preciso.

#block[
#block[
#figure([
#box(width: 396.0pt, image("monthly_forecast_files/figure-typst/fig-test-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Prueba de los modelos
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-test>


]
]
#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
#figure(
align(center)[#table(
  columns: 5,
  align: (col, row) => (auto,auto,auto,auto,auto,).at(col),
  inset: 6pt,
  [Modelo], [ME], [RMSE], [MAE], [MPE],
  [ARIMA],
  [−0.01],
  [0.45],
  [0.28],
  [−36.96],
  [ETS],
  [0.00],
  [0.45],
  [0.28],
  [−56.89],
  [LM],
  [0.00],
  [0.72],
  [0.44],
  [−61.10],
  [NNETAR],
  [0.00],
  [0.30],
  [0.20],
  [−3.72],
)]
)
<tbl-comp-1>

], caption: figure.caption(
position: top, 
[
Muestra de entrenamiento
]), 
kind: "quarto-subfloat-tbl", 
supplement: "", 
numbering: "(a)", 
)
<tbl-comp-1>


]
,
  [
#figure([
#figure(
align(center)[#table(
  columns: 5,
  align: (col, row) => (auto,auto,auto,auto,auto,).at(col),
  inset: 6pt,
  [Modelo], [ME], [RMSE], [MAE], [MPE],
  [ARIMA],
  [0.62],
  [0.70],
  [0.62],
  [−44.73],
  [ETS],
  [0.52],
  [0.57],
  [0.52],
  [22.15],
  [LM],
  [−0.54],
  [0.60],
  [0.54],
  [−77.84],
  [NNETAR],
  [0.02],
  [0.28],
  [0.23],
  [−79.03],
)]
)
<tbl-comp-2>

], caption: figure.caption(
position: top, 
[
Muestra de prueba
]), 
kind: "quarto-subfloat-tbl", 
supplement: "", 
numbering: "(a)", 
)
<tbl-comp-2>


]
)
]
], caption: figure.caption(
position: top, 
[
Precisión de los modelos
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-comp>


La @tbl-comp muestra algunas métricas de precisión de los pronósticos dentro y fuera de la muestra de entrenamiento.#footnote[ME: #emph[Median Error.-] Error de estimación promedio.

RMSE: #emph[Root median squared error.-] Raíz cuadrada del error cuadrático medio.

MAE: #emph[Mean absolute error.-] Error absoluto promedio.

MPE: #emph[Mean percentage error.-] Error porcentual promedio.] El modelo más preciso fue NNETAR, puesto que tuvo errores absolutos y cuadráticos más pequeños fuera de la muestra. Estas métricas son útiles para comparar diferentes modelos y las seguiremos utilizando para evaluar la precisión de los modelos en el pronóstico ex-ante.

== Pronóstico ex-ante
<pronóstico-ex-ante>
Sabiendo cuán precisos son los modelos, procedo a pronosticar la inflación mensual en México desde marzo 2024 hasta diciembre 2025. Solo para fines comparativos, utilizaré todos los modelos aunque el mejor fue indiscutiblemente NNETAR.

La @fig-forecast muestra los pronósticos de los modelos para los siguientes 22 meses. Algo importante para notar es que el modelo NNETAR \(@fig-forecast-4[45127368-afa1-446a-820f-fc64c546b2c5%fig-forecast]) no produce intervalos de confianza, puesto que las predicciones por redes neuronales pierden interpretabilidad y no permiten tener una medida de la varianza de la predicción.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
#box(width: 360.0pt, image("monthly_forecast_files/figure-typst/fig-forecast-1.svg"))
], caption: figure.caption(
position: bottom, 
[
ARIMA
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-forecast-1>


]
,
  [
#figure([
#box(width: 360.0pt, image("monthly_forecast_files/figure-typst/fig-forecast-2.svg"))
], caption: figure.caption(
position: bottom, 
[
ETS
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-forecast-2>


]
,
  [
#figure([
#box(width: 360.0pt, image("monthly_forecast_files/figure-typst/fig-forecast-3.svg"))
], caption: figure.caption(
position: bottom, 
[
LM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-forecast-3>


]
,
  [
#figure([
#box(width: 360.0pt, image("monthly_forecast_files/figure-typst/fig-forecast-4.svg"))
], caption: figure.caption(
position: bottom, 
[
NNETAR
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-forecast-4>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Pronósticos ex-ante de inflación mensual
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-forecast>


Una manera de evaluar la precisión de los pronósticos ex-ante es con respecto a la encuesta de expectativas de inflación mensual del Banco de México #cite(<banxico_sie>);. La @fig-banxico muestra los pronósticos de los modelos para los siguientes 12 meses y los compara con la media de las respuestas de la encuesta. Ninguno de los modelos que entrené se ajusta perfectamente al promedio de las expectativas, pero NNETAR y ETS están muy cerca.

#block[
#block[
#figure([
#box(width: 396.0pt, image("monthly_forecast_files/figure-typst/fig-banxico-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Pronóstico de inflación mensual a doce meses vs media de encuesta de expectativas Banxico
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-banxico>


]
]
La @tbl-banxico muestra la precisión de los modelos en la muestra completa respecto a la media de las expectativas de los analistas de Banxico. Es importante notar que el mejor modelo en nuestras pruebas \(NNETAR) no es el que resultó estar más cerca del promedio de los analistas. Sin embargo, esto obliga hacer la pregunta ¿qué tan preciso puede ser el proimedio de analistas para predecir la inflación? La @sec-survey busca responder esta pregunta.

Otra manera de medir la precisión del pronóstico es comparando la inflaicón anual para 2025 con la media de las expectativas de los analistas de Banxico. La @tbl-yearly muestra que el modelo NNETAR tuvo la predicción más cercana a la encuesta de expectativas.

#block[
#figure([
#block[
#figure(
align(center)[#table(
  columns: 5,
  align: (col, row) => (auto,auto,auto,auto,auto,).at(col),
  inset: 6pt,
  [Modelo], [ME], [RMSE], [MAE], [MPE],
  [ARIMA],
  [−0.21],
  [0.25],
  [0.22],
  [−52.73],
  [ETS],
  [−0.16],
  [0.18],
  [0.16],
  [−0.37],
  [LM],
  [−0.34],
  [0.36],
  [0.34],
  [−46.39],
  [NNETAR],
  [−0.01],
  [0.23],
  [0.18],
  [−0.49],
)]
)

]
], caption: figure.caption(
position: top, 
[
Precisión de los modelos en la muestra completa respecto la media de analistas Banxico
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
numbering: "1", 
)
<tbl-banxico>


]
#block[
#figure([
#block[
#figure(
align(center)[#table(
  columns: 5,
  align: (col, row) => (auto,auto,auto,auto,auto,).at(col),
  inset: 6pt,
  [Modelo], [Pronóstico], [Banxico], [Error], [Error porcentual],
  [ARIMA],
  [0.44],
  [3.73],
  [−3.29],
  [−88.15],
  [ETS],
  [1.95],
  [3.73],
  [−1.78],
  [−47.84],
  [LM],
  [−0.63],
  [3.73],
  [−4.36],
  [−116.88],
  [NNETAR],
  [3.16],
  [3.73],
  [−0.57],
  [−15.36],
)]
)

]
], caption: figure.caption(
position: top, 
[
Precisión de los pronósticos para inflación anual 2025 respecto a encuesta de expectativas Banxico
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
numbering: "1", 
)
<tbl-yearly>


]
= Poder predictivo de la encuesta de expectativas
<sec-survey>
Esta sección evalúa la capacidad predictiva de la encuesta de expectativas de inflación del Banco de México.

La @fig-expectations muestra que las predicciones intercuartílicas \(primer cuatil, media, mediana, y tercer cuartil) tienen aproximadamente la misma capacidad predictiva a lo largo de todo el horizonte de predicción \(de cero a doce meses de anticipación). Es importante destacar que los errores absolutos \(MAE y RMSE) son no monotónicos; es decir,. las mejores predicciones son con anticipaciones cercanas a cero y a doce meses. Las peores predicciones son a seis meses de anticipación.

#block[
#block[
#figure([
#box(width: 396.0pt, image("monthly_forecast_files/figure-typst/fig-expectations-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Precisión de la encuesta de expectativas Banxico
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-expectations>


]
]
La @tbl-expectations muestra que las predicciones intercuartílicas tienen un error cuadrático histórico de 0.41 puntos porcentuales. Al comparar con los errores de mis modelos en la @tbl-comp-2[45127368-afa1-446a-820f-fc64c546b2c5%tbl-comp], el modelo NNETAR tuvo mejor desempeño que el histórico de los analistas.

#block[
#figure([
#block[
#figure(
align(center)[#table(
  columns: 5,
  align: (col, row) => (auto,auto,auto,auto,auto,).at(col),
  inset: 6pt,
  [Estadístico], [MAE], [RMSE], [ME], [MPE],
  [Media],
  [0.31],
  [0.41],
  [−0.03],
  [−56.89],
  [Mediana],
  [0.31],
  [0.41],
  [−0.04],
  [−58.95],
  [Máximo],
  [0.37],
  [0.52],
  [0.24],
  [−33.80],
  [Mínimo],
  [0.40],
  [0.51],
  [−0.27],
  [−73.22],
  [Primer cuartil],
  [0.32],
  [0.42],
  [−0.09],
  [−64.32],
  [Tercer cuartil],
  [0.31],
  [0.41],
  [0.02],
  [−52.42],
)]
)

]
], caption: figure.caption(
position: top, 
[
Precisión histórica de la encuesta de expectativas Banxico
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
numbering: "1", 
)
<tbl-expectations>


]
= Referencias
<referencias>



#bibliography("references.bib")

