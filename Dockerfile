FROM debian:bookworm

ENV DEBIAN_FRONTEND=noninteractive
ENV USER=root
ENV HOME=/root

# Etapa 1: Instalar pacotes necessários
RUN apt-get update && apt-get install -y \
    libcanberra-gtk-module libcanberra-gtk3-module \
    python3 \
    python3-pip \
    python3-venv \
    python3-tk \
    xauth \
    x11-apps \
    wget \
    curl \
    binutils \
    gcc \
    libgtk2.0-0 \
    libgtk2.0-dev \
    psmisc \
    ntpdate \
    ca-certificates \
    --no-install-recommends && \
    usermod -a -G dialout root && \
    rm -rf /var/lib/apt/lists/*

# Etapa 2: Preparar ambiente Python
WORKDIR /app
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copiar script Python (caso ainda seja usado)
COPY gui_app.py .

# Copiar binário já compilado da aplicação Lazarus
COPY /lazarus_app/LazarusCLDSpeed /app/lazarus_app/

# Copiar script de start
COPY start_lazarus_app.sh /app/start_lazarus_app.sh
RUN chmod +x /app/start_lazarus_app.sh

# Entrypoint
CMD ["/app/start_lazarus_app.sh"]


