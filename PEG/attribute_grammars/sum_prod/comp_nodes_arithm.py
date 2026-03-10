#!/usr/bin/env python3
import arithm
import arithm_nodes

tree = arithm.parse("(4*(1+1)+2) * 2+13".replace(" ", ""),
                     types=arithm_nodes)
print(tree.compute())
