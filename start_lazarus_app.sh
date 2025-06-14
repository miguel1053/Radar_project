#!/bin/bash

# Sincronizar horário
ntpdate -s a.ntp.br || ntpdate -s pool.ntp.org

# Configurar X11 para encaminhamento
# O DISPLAY deve ser configurado pelo host ao rodar o container
# Ex: docker run -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix ...

echo "Configurando ambiente para aplicação Lazarus..."

# Detectar portas seriais
SERIAL_PORTS=""
for port in /dev/ttyS* /dev/ttyUSB* /dev/ttyACM* /dev/tty* ; do
    if [ -c "$port" ]; then
        SERIAL_PORTS="$SERIAL_PORTS $port"
    fi
done
echo "Portas seriais encontradas: $SERIAL_PORTS"

# Rodar a aplicação Lazarus
# Certifique-se de que o executável 'RadarApp' esteja no diretório /app/lazarus_app
/app/lazarus_app/RadarApp

# Manter container vivo (opcional, dependendo do comportamento da aplicação Lazarus)
# Se a aplicação Lazarus for um processo de longa duração, o container permanecerá ativo.
# Se a aplicação Lazarus sair, o container também sairá.
# Para manter o container vivo mesmo após a saída da aplicação, adicione um loop infinito:
# while true; do
#     sleep 1000
# done


