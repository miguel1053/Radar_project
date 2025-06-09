import tkinter as tk
from tkinter import ttk
from tkinter import scrolledtext
from tkinter import messagebox
import datetime
import os
import time
import serial
import serial.tools.list_ports
import threading

class SerialApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Comunicação Serial")
        
        # Variáveis de controle
        self.ser = None
        self.running = False
        self.log_file = None
        
        # Diretório onde os logs serão armazenados fora do container
        self.log_dir = "/home/radar/Projetos/ESP32/Logs"
        
        # Label e Dropdown para seleção de porta
        self.port_label = tk.Label(root, text="Porta Serial:")
        self.port_label.pack()
        
        self.port_var = tk.StringVar()
        self.port_dropdown = ttk.Combobox(root, textvariable=self.port_var, state="readonly")
        self.update_ports()
        self.port_dropdown.pack()
        
        # Label e Dropdown para configuração de baud rate
        self.baud_label = tk.Label(root, text="Baud Rate:")
        self.baud_label.pack()
        
        self.baud_var = tk.IntVar(value=115200)
        self.baud_dropdown = ttk.Combobox(root, textvariable=self.baud_var, state="readonly", values=[9600, 19200, 38400, 57600, 115200, 230400, 460800, 921600])
        self.baud_dropdown.pack()

        # Botão para conectar
        self.connect_button = tk.Button(root, text="Conectar", command=self.toggle_connection)
        self.connect_button.pack(pady=5)

        # Label para exibir a porta em uso
        self.status_label = tk.Label(root, text="Desconectado", fg="red")
        self.status_label.pack()

        # Área de texto para exibir as respostas
        self.text_area = scrolledtext.ScrolledText(root, width=50, height=20)
        self.text_area.pack(pady=10)

        # Campo de entrada para comandos
        self.entry = tk.Entry(root, width=40)
        self.entry.pack(pady=5)

        # Botão de envio
        self.send_button = tk.Button(root, text="Enviar", command=self.send_command, state="disabled")
        self.send_button.pack(pady=5)

    def update_ports(self):
        """Atualiza a lista de portas seriais disponíveis"""
        ports = [port.device for port in serial.tools.list_ports.comports()]
        self.port_dropdown["values"] = ports
        if ports:
            self.port_var.set(ports[0])  # Define a primeira porta encontrada como padrão

    def toggle_connection(self):
        """Conecta ou desconecta a porta serial"""
        if self.ser and self.ser.is_open:
            self.disconnect_serial()
        else:
            self.connect_serial()

    def connect_serial(self):
        """Conecta ao dispositivo serial"""
        selected_port = self.port_var.get()
        baud_rate = self.baud_var.get()
        
        if not selected_port:
            messagebox.showwarning("Erro", "Nenhuma porta selecionada!")
            return
        if baud_rate is None:
            messagebox.showwarning("Erro", "Baud rate não selecionado!")
            return

        try:
            self.ser = serial.Serial(selected_port, baud_rate, timeout=1)
            self.running = True
            self.status_label.config(text=f"Conectado em {selected_port}", fg="green")
            self.text_area.insert(tk.END, f"[INFO] Conectado à {selected_port} a {baud_rate} baud.\n")
            self.text_area.see(tk.END)
            self.send_button.config(state="normal")
            self.connect_button.config(text="Desconectar")
            
            # Iniciar a gravação do log
            self.start_logging()

            threading.Thread(target=self.read_serial, daemon=True).start()

        except serial.SerialException as e:
            messagebox.showerror("Erro", f"Erro ao conectar: {e}")

    def disconnect_serial(self):
        """Desconecta do dispositivo serial e fecha o arquivo de log"""
        if self.ser and self.ser.is_open:
            self.running = False
            self.ser.close()
            self.status_label.config(text="Desconectado", fg="red")
            self.text_area.insert(tk.END, "[INFO] Dispositivo desconectado.\n")
            self.text_area.see(tk.END)

            # Fechar o arquivo de log
            if self.log_file:
                self.log_file.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} - [INFO] Dispositivo desconectado.\n")
                self.log_file.flush()
                self.log_file.close()
                self.log_file = None

            self.send_button.config(state="disabled")
            self.connect_button.config(text="Conectar")

    def send_command(self):
        """Envia um comando via serial"""
        command = self.entry.get()

        if command and self.ser and self.ser.is_open:
            self.ser.write((command + "\n").encode("utf-8"))
            self.text_area.insert(tk.END, f"[ENVIADO] {command}\n")
            self.text_area.see(tk.END)
            self.entry.delete(0, tk.END)

    def read_serial(self):
        """Lê os dados recebidos do dispositivo serial"""
        while self.running:
            try:
                if self.ser and self.ser.is_open:
                    data = self.ser.readline().decode("utf-8").strip()
                    if data:
                        self.text_area.insert(tk.END, f"[RECEBIDO] {data}\n")
                        self.text_area.see(tk.END)
                        if self.log_file:
                            self.log_file.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} - [RECEBIDO] {data}\n")
                            self.log_file.flush()
            except Exception as e:
                self.text_area.insert(tk.END, f"[ERRO] {e}\n")
                self.text_area.see(tk.END)
                if self.log_file:
                    self.log_file.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} - [ERRO] {e}\n")
                    self.log_file.flush()
                self.running = False
                break

    def start_logging(self):
        """Inicia a gravação do log"""
        os.makedirs(self.log_dir, exist_ok=True)
        log_file_path = os.path.join(self.log_dir, f"log_{int(time.time())}.txt")
        self.log_file = open(log_file_path, "a")
        self.text_area.insert(tk.END, f"[INFO] Gravando log em: {log_file_path}\n")
        self.text_area.see(tk.END)
        if self.log_file:
            self.log_file.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} - [INFO] Conectado à {self.ser.port} a {self.ser.baudrate} baud.\n")
            self.log_file.flush()

    def on_closing(self):
        """Fecha a interface e encerra a comunicação"""
        self.disconnect_serial()
        self.root.destroy()

if __name__ == "__main__":
    root = tk.Tk()
    app = SerialApp(root)
    root.protocol("WM_DELETE_WINDOW", app.on_closing)
    root.mainloop()

LOG_FILE = "/data/log.txt"

def log_timestamp():
    """Appends the current timestamp to the log file."""
    try:
        # Ensure the /data directory exists (Docker volume should handle this, but good practice)
        os.makedirs(os.path.dirname(LOG_FILE), exist_ok=True)

        now = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        with open(LOG_FILE, "a") as f:
            f.write(f"Button clicked at: {now}\n")
        messagebox.showinfo("Success", f"Timestamp logged to {LOG_FILE}")
    except Exception as e:
        messagebox.showerror("Error", f"Failed to write to log file {LOG_FILE}:\n{e}")

# Create the main window
root = tk.Tk()
root.title("GUI App in Container")
root.geometry("300x150")

# Create a label
label = tk.Label(root, text="Click the button to log timestamp.")
label.pack(pady=20)

# Create a button
log_button = tk.Button(root, text="Log Timestamp", command=log_timestamp)
log_button.pack(pady=10)

# Start the Tkinter event loop
root.mainloop()

