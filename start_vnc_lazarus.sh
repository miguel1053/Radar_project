#!/bin/bash

# Sincronizar horário
ntpdate -s a.ntp.br || ntpdate -s pool.ntp.org

# Iniciar VNC
vncserver :1 -geometry 1280x800 -depth 24

# Esperar inicialização
sleep 2
export DISPLAY=:1

# Detectar portas seriais
SERIAL_PORTS=""
for port in /dev/ttyS* /dev/ttyUSB* /dev/ttyACM* /dev/tty* ; do
    if [ -c "$port" ]; then
        SERIAL_PORTS="$SERIAL_PORTS $port"
    fi
done
echo "Portas seriais encontradas: $SERIAL_PORTS"

# Iniciar XFCE
startxfce4 &

# Rodar sua aplicação (escolha uma das opções)
# python3 gui_app.py
# /app/your_lazarus_app

# Manter container vivo e matar VNC corretamente
trap "echo 'Encerrando...'; vncserver -kill :1; exit 0" SIGTERM SIGINT
echo "Ambiente iniciado. Pressione Ctrl+C para sair ou pare o container."
while true; do
    sleep 1000
done
