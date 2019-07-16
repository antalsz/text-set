# text-set

Use a [DAFSA](https://en.wikipedia.org/wiki/DAFSA) (directed acyclic finite
state automaton; aka a DAWG, a directed acyclic word graph) to implement a set of strings.

The algorithm for building the DAFSA is Algorithm 1 from the paper “Incremental
Construction of Minimal Acyclic Finite-State Automata”, by Jan Daciuk, Stoyan
Mihov, Bruce W. Watson, and Richard E. Watson.  Published in 2000 in
_Computational Linguistics_ 26(1), pp.3-16.  Available online at
<https://www.aclweb.org/anthology/J00-1002>.

The ENABLE wordlist, in `ENABLE-wordlist.txt`, was downloaded from [Peter
Norvig’s page about _Natural Language Corpus Data_](https://norvig.com/ngrams/).
