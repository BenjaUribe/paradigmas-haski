#!/bin/bash
# Script que ejecuta ffplay y muere automáticamente cuando el proceso padre muere

# Configurar para morir cuando el padre muera (usando trap)
trap "exit 0" SIGTERM SIGINT SIGHUP

# El primer argumento es el archivo, el segundo es el volumen
FILE="$1"
VOLUME="${2:-100}"

# Ejecutar ffplay en primer plano
exec ffplay -nodisp -autoexit -loop 0 -loglevel quiet -volume "$VOLUME" "$FILE"
