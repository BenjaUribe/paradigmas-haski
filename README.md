# paradigmas-haski
INFO188 tarea 1

---

***Requisitos para compilar y ejecutar el proyecto***
Para compilar y ejecutar el proyecto es necesario tener instalado GHC (Glasgow Haskell Compiler) y Cabal. Asegúrate de tener estas herramientas instaladas en tu sistema.

```bash
# Actualiza e instala las dependencias necesarias
sudo apt-get update
sudo apt-get install -y freeglut3-dev
cabal update
cabal install --lib gloss
cabal install --lib mtl
cabal install --lib random
```
---
***Compilación y ejecución del proyecto***
Para compilar y ejecutar el proyecto, sigue estos pasos:
1. Clona el repositorio o descarga el código fuente.
2. Navega al directorio del proyecto.
3. Compilar el proyecto utilizando `make`
```bash
make
```
4. Ejecuta el programa compilado
```bash
./game
```
---

***Descripción del proyecto***

Este proyecto es un juego simple desarrollado en Haskell utilizando la biblioteca Gloss para gráficos. El juego consiste en un jugador que cuenta con una clase `Warrior | Tank | Rogue`, el cual se enfrenta a diferentes enemigo dentro de una `Dungeon` estilo medieval. Las acciones del jugador estan inspiradas en los juego estilo `Pokemon` y `Dungeons and Dragons`, refiriendonos a la seleccion de acciones y al hecho de que estas estan sujetas a dados para ver efectividad.

