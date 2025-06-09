# Projeto Radar com Lazarus, VNC e Comunicação TCP

Este projeto visa evoluir uma aplicação gráfica em container Linux, adicionando suporte a aplicações Lazarus, comunicação de rede TCP, acesso via VNC, detecção dinâmica de portas seriais e sincronização de horário.

## Estrutura do Projeto

- `Dockerfile`: Define o ambiente do container, incluindo Lazarus, VNC, Python e as dependências necessárias.
- `start_vnc_lazarus.sh`: Script de inicialização do container, responsável por:
    - Sincronizar o horário do container com um servidor NTP.
    - Iniciar o servidor VNC.
    - Detectar dinamicamente as portas seriais disponíveis (TTY e USB).
    - Iniciar a aplicação Lazarus (ou a aplicação Python, se preferir).
- `gui_app.py`: (Opcional) Sua aplicação gráfica original em Python.
- `requirements.txt`: Dependências Python do projeto.
- `persistent_data/`: Diretório para dados persistentes (ex: logs).
- `project_logs/`: Diretório para logs do projeto.

## Como Usar

### Pré-requisitos

- Docker instalado em sua máquina.

### Construindo a Imagem Docker

Navegue até o diretório `radar` (onde o `Dockerfile` está localizado) e execute o seguinte comando:

```bash
sudo docker build -t lazarus-vnc-app .
```

### Executando o Container

Para executar o container, você precisará mapear as portas e, opcionalmente, os dispositivos seriais. Substitua `/dev/ttyUSB0` pelo caminho correto do seu dispositivo serial, se houver.

```bash
sudo docker run -it --rm \
    -p 5901:5901 \
    --device=/dev/ttyUSB0:/dev/ttyUSB0 \
    --name lazarus-gui-container \
    lazarus-vnc-app
```

- `-p 5901:5901`: Mapeia a porta VNC do container para a porta 5901 do seu host.
- `--device=/dev/ttyUSB0:/dev/ttyUSB0`: Mapeia um dispositivo serial do host para o container. Repita esta flag para cada dispositivo serial que você precisar.
- `--name lazarus-gui-container`: Define um nome para o seu container.

### Acessando a Aplicação via VNC

Após iniciar o container, você pode se conectar a ele usando um cliente VNC (ex: RealVNC Viewer, TightVNC Viewer) no endereço `localhost:5901` (ou `seu_ip_do_host:5901`). A senha do VNC é `password` (definida no Dockerfile).

### Detecção de Portas Seriais

O script `start_vnc_lazarus.sh` detectará automaticamente as portas seriais disponíveis no container (TTY e USB) e as exibirá no log. Sua aplicação Lazarus precisará ser configurada para listar e utilizar essas portas dinamicamente.

### Sincronização de Horário

O container sincronizará seu horário com um servidor NTP na inicialização, garantindo que o horário esteja sempre atualizado.

## Próximos Passos

1.  **Desenvolvimento da Aplicação Lazarus:** Implemente a lógica da sua aplicação Lazarus, incluindo a comunicação TCP e a utilização das portas seriais detectadas dinamicamente.
2.  **Comunicação TCP:** Dentro da sua aplicação Lazarus, implemente a comunicação TCP (cliente ou servidor, conforme sua necessidade).
3.  **Otimização:** Otimize o Dockerfile e o script de inicialização conforme suas necessidades específicas de produção.

## Contribuição

Sinta-se à vontade para contribuir com este projeto, enviando pull requests ou abrindo issues no GitHub.


