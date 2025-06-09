# Usar imagem Debian slim como base
FROM debian:bookworm-slim

# Evitar prompts interativos
ENV DEBIAN_FRONTEND=noninteractive

# Atualizar e instalar dependências do sistema
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    python3-tk \
    xauth \
    x11-apps \
    wget \
    binutils \
    gcc \
    libgtk2.0-0 \
    libgtk2.0-dev \
    psmisc \
    tigervnc-standalone-server \
    ntpdate \
    --no-install-recommends && \
    usermod -a -G dialout root && \
    rm -rf /var/lib/apt/lists/*

# Instalar Lazarus e FPC (Free Pascal Compiler)
# As URLs foram atualizadas para a versão 4.0.0 do Lazarus e FPC 3.2.2
RUN wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.0/fpc-laz_3.2.2-210709_amd64.deb && \
    dpkg -i fpc-laz_3.2.2-210709_amd64.deb && \
    rm fpc-laz_3.2.2-210709_amd64.deb

RUN wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.0/fpc-src_3.2.2-210709_amd64.deb && \
    dpkg -i fpc-src_3.2.2-210709_amd64.deb && \
    rm fpc-src_3.2.2-210709_amd64.deb

RUN wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.0/lazarus-project_4.0.0-0_amd64.deb && \
    dpkg -i lazarus-project_4.0.0-0_amd64.deb && \
    rm lazarus-project_4.0.0-0_amd64.deb

# Limpar pacotes desnecessários
RUN apt-get clean && apt-get autoremove -y

# Definir diretório de trabalho
WORKDIR /app

# Criar e ativar ambiente virtual Python
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Copiar arquivo de requisitos e instalar dependências Python via pip
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copiar o código da aplicação GUI Python (se ainda for usada)
COPY gui_app.py .

# Criar diretório para logs dentro do container
RUN mkdir -p /app/logs

# Configurar VNC
ENV USER=root
ENV HOME=/root
RUN mkdir -p /root/.vnc
RUN echo "password" | vncpasswd -f > /root/.vnc/passwd
RUN chmod 600 /root/.vnc/passwd

# Expor porta VNC
EXPOSE 5901

# Comando padrão para iniciar o VNC e a aplicação Lazarus (ou Python GUI)
# Este script será criado no próximo passo
CMD ["/app/start_vnc_lazarus.sh"]


