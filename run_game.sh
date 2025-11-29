#!/bin/bash
# Wrapper que ejecuta el juego y garantiza que ffplay muera al cerrar

cd "$(dirname "$0")"

# Función de limpieza
cleanup() {
    echo " Limpiando procesos de audio..."
    killall -9 ffplay 2>/dev/null
    pkill -9 ffplay 2>/dev/null
    exit 0
}

# Configurar traps para TODAS las señales de terminación
trap cleanup EXIT SIGINT SIGTERM SIGHUP SIGQUIT

# Ejecutar el juego
./game

# Limpieza final (por si acaso)
cleanup
