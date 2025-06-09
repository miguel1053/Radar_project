#!/bin/bash

# Sincronizar o horário do container com um servidor NTP
apt-get update && apt-get install -y ntpdate --no-install-recommends
ntpdate -s a.ntp.br || ntpdate -s pool.ntp.org

# Iniciar o servidor VNC
# A resolução pode ser ajustada conforme necessário
vncserver :1 -geometry 1280x800 -depth 24

# Esperar o VNC iniciar
sleep 2

# Definir a variável DISPLAY para a sessão VNC
export DISPLAY=:1

# Detectar portas seriais dinamicamente
SERIAL_PORTS=""
for port in /dev/ttyS* /dev/ttyUSB* /dev/ttyACM* /dev/tty* ; do
    if [ -c "$port" ]; then
        SERIAL_PORTS="$SERIAL_PORTS $port"
    fi
done

# Exibir as portas seriais encontradas (para depuração)
echo "Portas seriais encontradas: $SERIAL_PORTS"

# Iniciar a aplicação Lazarus
# Substitua 'your_lazarus_app' pelo nome do executável da sua aplicação Lazarus
# Se você ainda for usar a aplicação Python, descomente a linha abaixo e comente a do Lazarus
# python3 gui_app.py

# Exemplo de como iniciar o Lazarus IDE (para desenvolvimento)
# lazarus

# Exemplo de como iniciar uma aplicação Lazarus compilada
# Certifique-se de que o executável esteja no diretório /app
# /app/your_lazarus_app

# Mantenha o container rodando
# Este comando é um placeholder. A aplicação Lazarus deve manter o processo ativo.
# Se a aplicação Lazarus sair, o container também sairá.
# Para manter o container ativo para depuração, você pode usar:
# tail -f /dev/null

# Por enquanto, vamos usar um loop simples para manter o container ativo
# até que a aplicação Lazarus seja integrada.
while true; do
    sleep 1000
done

