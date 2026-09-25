# Lince 2.0 – Stream Extension

## Introduction and Scope
Lince 2.0 is an interactive tool for the simulation and visualisation of hybrid programs and Cyber-Physical Systems (CPS).
This repository contains an extended version of Lince 2.0, developed as part of a Master's dissertation at the Faculty of Sciences of the University of Porto (FCUP). The work focuses on integrating streams into the Lince 2.0 simulation environment, allowing sequential and random values to be used during program execution.

## Web Application
A web version of the extended Lince 2.0 is available at:
https://fernando571.github.io/Lince2.0/

## Requirements
The following software is required to compile and run the project:
- Java / JVM
- Scala 3
- sbt
- Git
The project currently uses Scala 3.7.1.

## Compilation
You need to compile this project using the ScalaJS plug-in, following the steps below. The result will be a JavaScript file that is already being imported by an existing HTML file.
sbt fastLinkJS
open the file docs/index.html
