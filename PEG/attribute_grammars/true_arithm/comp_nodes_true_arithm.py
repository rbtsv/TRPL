#!/usr/bin/python3
import true_arithm
import true_arithm_nodes

#"3*((32/4+3*9)+1+3+25*13*17/13/25)/2/7"
tree = true_arithm.parse(
    "3*(2*3-2)/12*6*6/9".replace(" ", ""),
                         types=true_arithm_nodes)
print(tree.compute())
