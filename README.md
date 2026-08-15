# Lince2.0
Animator of Lince 2.0
Rebooting the Lince program using CAOS, using simpler data structures and functions. Initially this will only have the approximated (non-symbolic) execution.

A snapshot of Lince 2.0 can be executed at https://lmf.di.uminho.pt/lince-2.0/

The previous version of Lince (not maintained) can be found at http://http://arcatools.org/lince

Videos
Hands-on tutorial on Lince 2.0, presented at the Shif2SDV European project consortium, April 2026 (11min)
Talk at GAG seminars over Lince, University of Aveiro, Portugal, June 2021 (46min)
Publications
An Adequate While-Language for Stochastic Hybrid Computation, Renato Neves, José Proença, Juliana Souza, PPDP 2025, September 2025
Analyzing Many Simulations of Hybrid Programs in Lince, Reydel Arrieta and, José Proença, Patrick Meumeu Yomsi, FMAS@iFM 2025, November 2025
Formal Simulation and Visualisation of Hybrid Programs, Pedro Mendes, Ricardo Correia, Renato Neves, José Proença, FMAS@iFM 2024, November 2024
Implementing Hybrid Semantics: From Functional to Imperative, Sergey Goncharov, Renato Neves, José Proença, ICTAC 2020, October 2020
An Adequate While-Language for Hybrid Computation, Sergey Goncharov, Renato Neves, PPDP 2019, October 2019
Caos
This project uses and the Caos's framework as a submodule. More information on it can be found online:

Caos' GitHub page: https://github.com/arcalab/CAOS
Caos' tutorial: https://arxiv.org/abs/2304.14901
Caos' demo video: https://youtu.be/Xcfn3zqpubw
Requirements
JVM (>=1.8)
sbt
Before compiling import the CAOS submodule, e.g., using the command:

git submodule update --init

You also need to add the following line to the file lib/caos/tool/index.html, at line 57 (since CAOS does not load Plotly by default):

<script type="text/javascript" src="js/static/plotly.min.js"></script>
Compilation
You need to compile this project using the ScalaJS plug-in, following the steps below. The result will be a JavaScript file that is already being imported by an existing HTML file.

sbt fastLinkJS
open the file lib/caos/tool/index.html
