#!/bin/bash
set -e

# --- Sincronizar horário ---
if command -v ntpdate &>/dev/null; then
    echo "Sincronizando horário com NTP..."
    ntpdate -s a.ntp.br || ntpdate -s pool.ntp.org || echo "Aviso: Falha na sincronização NTP, continuando..."
else
    echo "Aviso: 'ntpdate' não instalado, pulando sincronização NTP."
fi

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
# O executável 'TECSPEEDPRO' deve estar em /app/lazarus_app/
/app/lazarus_app/TECSPEEDPRO

# Manter container vivo (opcional, dependendo do comportamento da aplicação Lazarus)
# Se a aplicação Lazarus for um processo de longa duração, o container permanecerá ativo.
# Se a aplicação Lazarus sair, o container também sairá.
# Para manter o container vivo mesmo após a saída da aplicação, adicione um loop infinito:
# while true; do
#     sleep 1000
# done


