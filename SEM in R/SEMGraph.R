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



mermaid("
        graph BT
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

library(DiagrammeR)

grViz("
digraph SEM {

graph [layout = neato,
       overlap = true,
       outputorder = edgesfirst]

node [shape = circle]

a [pos = '-4,1!', label = 'e1', shape = square]
b [pos = '-3,1!', label = 'ind_1']
c [pos = '-3,0!', label = 'ind_2']
d [pos = '-3,-1!', label = 'ind_3']
e [pos = '-1,0!', label = 'latent a', shape = ellipse]
f [pos = '1,0!', label = 'latent b', shape = ellipse]
g [pos = '1,1!', label = 'e6', shape = square]
h [pos = '3,1!', label = 'ind_4']
i [pos = '3,-1!', label = 'ind_5']
j [pos = '4,1!', label = 'e4', shape = square]
k [pos = '4,-1!', label = 'e5', shape = square]

a->b
e->b [label = '0.6']
e->c [label = '0.6']
e->d [label = '0.6']

e->f [label = '0.321']
g->f [tailport = 's', headport = 'n']

d->c [dir = both]

f->h [label = '0.6', tailport = 'ne', headport = 'w']
f->i [label = '0.6']

j->h
k->i

}
")
