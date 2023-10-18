# Contents

This repository contains the source code for the article arXiv:TODO.

The program in ``sptree.ml`` computes the minimum number of rotations
needed to transform T_1 in T'_1, where T_1 and T'_1 are the trees
defined in section 4.3.

We implemented a quite inefficient algorithm, that first generates the
full reconfiguration graph for the problem, and then computes the
distance using a BFS. Nonetheless, the last instruction should answer
7 and prove Lemma 4.11 within half a minute on a standard laptop.

# Compiling and running

The program can be compiled using the following instruction

``ocamlopt -o sptree sptree.ml``

and then run with

``./sptree``
