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


# Creating the tree structure for a system.
mermaid("
        graph TD
        A(Rounded) --> B[On]
        A --> C[Off]
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

mermaid("
        graph LR
        A(Card)-->B(1)
        A-->C(2)
        A-->D(3)
        A-->E(4)
        A-->F(5)
        A-->G(6)
        B--0.1-->H(Red)
        B--1/15-->I(White)
        C--0.1-->J(Red)
        C--1/15-->K(White)
        D--0.1-->L(Red)
        D--1/15-->M(White)
        E--1/15-->N(Red)
        E--0.05-->O(White)
        E--0.05-->P(Black)
        F--0.1-->Q(Red)
        F--1/15-->R(White)
        G--0.1-->S(Red)
        G--1/15-->T(White)")

"Question 3:"
mermaid("
        graph LR
        A(1)--0.99-->B(P)
        A--0.01-->C(F)
        D(2)--0.97-->E(P)
        D--0.03-->F(F)
        G(3)--0.98-->H(P)
        G--0.02-->I(F)
        J(4)--0.99-->K(P)
        J--0.01-->L(F)")
