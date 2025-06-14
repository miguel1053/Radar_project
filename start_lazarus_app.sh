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

# Diretório onde este script está localizado
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"

# Nome da imagem Docker (deve ser o mesmo usado no build)
IMAGE_NAME="lazarus-x11-app"  # Ajuste se necessário

# Diretório para logs persistentes no HOST (relativo ao diretório do script)
LOG_DATA_DIR="${SCRIPT_DIR}/project_logs"

# Diretório local do Lazarus (pasta no host com seu executável Lazarus)
LAZARUS_APP_HOST_DIR="${SCRIPT_DIR}/lazarus_app\LazarusCLDSpeed"

# Caminho para o dispositivo serial no HOST (AJUSTE SE NECESSÁRIO!)
SERIAL_DEVICE="/dev/ttyUSB0"  # Ou /dev/ttyACM0, etc.

# --- Verificações ---
if [ ! -c "${SERIAL_DEVICE}" ]; then
    echo "Erro: Dispositivo serial \'${SERIAL_DEVICE}\' não encontrado no host."
    echo "Verifique se o dispositivo está conectado e o caminho correto."
    exit 1
fi

if ! groups $(whoami) | grep -q '\bdialout\b'; then
   echo "Aviso: Seu usuário ($(whoami)) pode não pertencer ao grupo \'dialout\'."
   echo "Pode ser necessário adicioná-lo para acessar a porta serial no host:"
   echo "sudo usermod -a -G dialout $(whoami)"
   echo "(Faça logout/login após adicionar ao grupo)"
fi

# --- Permissão para acesso ao X server ---
#echo "Atenção: Permitindo conexões locais ao servidor X via \'xhost +local:\'."
#xhost +local:

# --- Criar diretório para logs persistentes se não existir ---
mkdir -p "${LOG_DATA_DIR}"
echo "Diretório para logs persistentes no host: ${LOG_DATA_DIR}"

# Verificar se a pasta Lazarus existe no host
if [ ! -d "${LAZARUS_APP_HOST_DIR}" ]; then
    echo "Erro: Pasta Lazarus não encontrada no host em ${LAZARUS_APP_HOST_DIR}."
    exit 1
fi

# --- Executar o container Docker ---
echo "Iniciando o container ${IMAGE_NAME} com acesso a ${SERIAL_DEVICE} e pasta Lazarus..."

docker run -it --rm \
    -e DISPLAY=$DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    --device=${SERIAL_DEVICE}:${SERIAL_DEVICE} \
    -v "${LOG_DATA_DIR}":"/home/radar/Projetos/ESP32/Logs" \
    -v "${LAZARUS_APP_HOST_DIR}":"/app/lazarus_app" \
    ${IMAGE_NAME} /app/lazarus_app/project1

# --- Opcional: desabilitar a permissão xhost após o container parar ---
# echo "Restaurando permissões do xhost..."
# xhost -local:

echo "Container finalizado."


