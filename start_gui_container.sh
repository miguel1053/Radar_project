#!/bin/bash

# Diretório onde este script está localizado
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"

# Nome da imagem Docker (deve ser o mesmo usado no build)
IMAGE_NAME="gui-app-container" # <- Certifique-se que este é o nome correto da imagem

# Diretório para logs persistentes no HOST (relativo ao diretório do script)
LOG_DATA_DIR="${SCRIPT_DIR}/project_logs"

# Caminho para o dispositivo serial no HOST (AJUSTE SE NECESSÁRIO!)
# Verifique qual porta seu ESP32 está usando (ex: dmesg | grep tty)
SERIAL_DEVICE="/dev/ttyUSB0" # Ou /dev/ttyACM0, etc.

# --- Verificações ---
if [ ! -c "${SERIAL_DEVICE}" ]; then
    echo "Erro: Dispositivo serial '${SERIAL_DEVICE}' não encontrado no host."
    echo "Verifique se o NANO está conectado e se o caminho está correto neste script."
    exit 1
fi

if ! groups $(whoami) | grep -q '\bdialout\b'; then
   echo "Aviso: Seu usuário ($(whoami)) pode não pertencer ao grupo 'dialout'."
   echo "Pode ser necessário adicioná-lo para acessar a porta serial no host:"
   echo "sudo usermod -a -G dialout $(whoami)"
   echo "(Pode precisar fazer logout/login após adicionar ao grupo)"
fi


# --- Configuração ---
# 1. Permitir conexões locais ao servidor X (com aviso)
echo "Atenção: Permitindo conexões locais ao servidor X via 'xhost +local:'."
xhost +local:

# 2. Criar o diretório de logs persistentes se não existir
mkdir -p "${LOG_DATA_DIR}"
echo "Diretório para logs persistentes no host: ${LOG_DATA_DIR}"

# --- Execução ---
# 3. Executar o container Docker
echo "Iniciando o container ${IMAGE_NAME} com acesso a ${SERIAL_DEVICE}..."
docker run -it --rm \
    -e DISPLAY=$DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    --device=${SERIAL_DEVICE}:${SERIAL_DEVICE} \
    -v "${LOG_DATA_DIR}":"/home/radar/Projetos/ESP32/Logs" \
    ${IMAGE_NAME}

# (Opcional) Desabilitar a permissão xhost após o container parar
# echo "Restaurando permissões do xhost..."
# xhost -local:

echo "Container finalizado."

