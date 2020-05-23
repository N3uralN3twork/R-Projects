library(DiagrammeR)

# Source: https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
# Source2 : https://cyberhelp.sesync.org/blog/visualization-with-diagrammeR.html

mermaid("
graph LR
A(Rounded)-->B[Rectangular]
B-->C{A Rhombus}
C-->D[Rectangle One]
C-->E[Rectangle Two]
")


# (()) = circle
# {} = rhombus
# 

mermaid("
graph LR
A((EM Trouble)) -- 0.1 --> F[Adult Incarceration]
B((Trauma)) -- 0.2 --> F[Adult Incarceration]
C((HS Trouble)) -- 0.3 --> F[Adult Incarceration]
D((Delinquency)) -- 0.4 --> F[Adult Incarceration]
E((Crime)) -- 0.5 --> F[Adult Incarceration]
A -- 0.60 --> C
B -- 0.70 --> C
D -- 0.80 --> C
")


library(DiagrammeR)

mermaid("
        graph LR
        A((Salinity))
        A-->B(Barnacles)
        B-.->|-0.10|B1{Mussels}
        A-- 0.30 -->B1

        C[Air Temp]
        C-->B
        C-.->E(Macroalgae)
        E-->B1
        C== 0.89 ==>B1
        style A fill:#FFF, stroke:#333, stroke-width:4px
        style B fill:#9AA, stroke:#9AA, stroke-width:2px
        style B1 fill:#879, stroke:#333, stroke-width:1px
        style C fill:#ADF, stroke:#333, stroke-width:2px
        style E fill:#9C2, stroke:#9C2, stroke-width:2px
        ")
