FROM debian:bookworm

ENV DEBIAN_FRONTEND=noninteractive
ENV USER=root
ENV HOME=/root

# Etapa 1: Instalar pacotes
RUN apt-get update && apt-get install -y \
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
    tigervnc-standalone-server \
    tigervnc-common \
    tigervnc-tools \   
    xfce4 \
    xfce4-goodies \
    ntpdate \
    ca-certificates \
    --no-install-recommends && \
    usermod -a -G dialout root && \
    rm -rf /var/lib/apt/lists/*

# Etapa 2: Instalar Lazarus e FPC
RUN wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.0/fpc-laz_3.2.2-210709_amd64.deb && \
    dpkg -i fpc-laz_3.2.2-210709_amd64.deb && rm fpc-laz_3.2.2-210709_amd64.deb

RUN wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.0/fpc-src_3.2.2-210709_amd64.deb && \
    dpkg -i fpc-src_3.2.2-210709_amd64.deb && rm fpc-src_3.2.2-210709_amd64.deb

RUN wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.0/lazarus-project_4.0.0-0_amd64.deb && \
    dpkg -i lazarus-project_4.0.0-0_amd64.deb && rm lazarus-project_4.0.0-0_amd64.deb

# Etapa 3: Preparar ambiente Python
WORKDIR /app
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY gui_app.py .
COPY start_vnc_lazarus.sh /app/start_vnc_lazarus.sh
RUN chmod +x /app/start_vnc_lazarus.sh

# Etapa 4: Configurar senha VNC em instrução separada
RUN mkdir -p /root/.vnc
RUN echo "password" | vncpasswd -f > /root/.vnc/passwd
RUN chmod 600 /root/.vnc/passwd

# Expor porta do VNC
EXPOSE 5901

# Entrypoint
CMD ["/app/start_vnc_lazarus.sh"]
