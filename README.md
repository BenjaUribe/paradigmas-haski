# paradigmas-haski
INFO188 tarea 1

---

***Requisitos para compilar y ejecutar el proyecto***
Para compilar y ejecutar el proyecto es necesario tener instalado GHC (Glasgow Haskell Compiler) y Cabal. Asegúrese de tener estas herramientas instaladas en su sistema.

```bash
# Actualizar e instalar las dependencias necesarias
sudo apt-get update
sudo apt-get install -y freeglut3-dev
sudo apt install cabal-install
cabal update
cabal install --lib gloss
cabal install --lib mtl
cabal install --lib random
```
---
***Compilación y ejecución del proyecto***
Para compilar y ejecutar el proyecto, siga estos pasos:
1. Clonar el repositorio o descargar el código fuente.
2. Navegar al directorio del proyecto.
3. Compilar el proyecto utilizando `make`
```bash
make
```
4. Ejecutar el programa compilado
```bash
./game
```
---

***Descripción del proyecto***

Este proyecto consiste en un juego simple desarrollado en Haskell, utilizando la biblioteca Gloss para gráficos. En este juego, el jugador debe escoger una clase entre `Warrior | Tank | Rogue`, cada una con sus propias características de ataque y vida. El jugador debe enfrentarse a diferentes enemigos dentro de una `Dungeon` estilo medieval, siguiendo una progresión de niveles en la que el jugador podrá construir una ruta hacia el jefe final. Además, al completar un nivel, el jugador atravesará un pasillo custodiado por dos moáis, en el que podrá escoger una bonificación de daño o vida para el siguiente nivel. 

Las acciones del jugador estan inspiradas en los juego estilo `Pokemon` y `Dungeons and Dragons`, refiriéndonos a la seleccion de acciones y al hecho de que estas estan sujetas a dados para ver efectividad.

