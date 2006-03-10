package gov.nih.mipav.view;


import java.io.*;
import java.net.*;
import javax.net.*;
import javax.net.ssl.*;
import java.util.StringTokenizer;


/**
 * Client to connect to specified server.
 */
public class ChatClient extends Thread {
    private static int DEFAULT_PORT = 9030; // default port for server

    private String host = null; // server's ip address

    private int port = DEFAULT_PORT; // port to connect

    private Socket socket = null; // socket for connection (SSL)

    private String clientName; // client's username

    private String clientIP; // client's IP

    private JChatFrame chatFrame; // Chat Frame from which the client was spawned

    private BufferedReader in = null; // reader for data from server

    private PrintWriter out = null; // writer to send data to server

    private boolean keepGoing = true; // boolean true while running (looking for input)

    /**
     * constructor
     * @param name Client user-name
     * @param ip client's IP address
     * @param chatFrame parent chatframe
     * @param host (server to connect)
     * @param port (server's port)
     */
    public ChatClient(String name, String ip, JChatFrame chatFrame, String host, int port) {
        this.clientName = name;
        this.clientIP = ip;
        this.chatFrame = chatFrame;
        this.host = host;
        this.port = port;
        chatFrame.notifyLogin(login());
    }

    /**
     * cleans up
     */
    protected void finalize() {
        try {
            if (out != null) {
                out.close();
                out = null;
            }
            if (in != null) {
                in.close();
                in = null;
            }
            if (socket != null) {
                socket.close();
                socket = null;
            }
        } catch (IOException e) {
            System.err.println("ChatClient.finalize(): " + e.toString());
        }
    }

    /**
     * logs client into the server
     * @return success on login
     */
    public boolean login() {
        try {
            if (socket == null) {
                SocketFactory socketFactory = SSLSocketFactory.getDefault();
                socket = socketFactory.createSocket(host, port);
                // System.err.println("Socket created");
                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    if (socket instanceof SSLSocket) { // of course it is!
                        SSLSession session = ((SSLSocket) socket).getSession();
                        try {
                            javax.security.cert.X509Certificate[] x509 = session.getPeerCertificateChain();
                            for (int i = 0; i < x509.length; i++ )
                                System.out.println("Certs: " + x509[i].toString());
                        } catch (SSLPeerUnverifiedException ex) {
                            System.err.println(ex.toString());
                            return false;
                        }
                    }
                }
                if (socket.isConnected()) {
                    in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                    out = new PrintWriter(socket.getOutputStream(), true);
                    out.println(clientName);
                    out.println(clientIP);
                    start();
                } else {
                    println("Unable to make connection with server");
                    return false;
                }
            }
        } catch (ConnectException e) {
            println("Unable to connect.  Server is most likely down.");
            return false;
        } catch (UnknownHostException e) {
            println("Unknown host, please notify admin.");
            return false;
        } catch (IOException e) {
            println("I/O socket error.");
            e.printStackTrace();
            return false;
        } catch (Exception e) {
            println("Cannot log in, please try again later.");
            return false;
        }
        return true;
    }

    /**
     * sends the given message through the socket to the server
     * @param message to send
     */
    public void send(String message) {
        String command;
        if ( !message.startsWith("/") || message.length() == 1 || message.startsWith("/ ")) {
            command = "MESG" + " " + message;
        } else {
            StringTokenizer tokens = new StringTokenizer(message);
            command = tokens.nextToken("/").toUpperCase();
            if (command.equals("MESG") && tokens.hasMoreTokens()) {
                command = command + " " + tokens.nextToken("\n").trim();
            } else {
                while (tokens.hasMoreTokens()) {
                    command = command + " " + tokens.nextToken(" \n");
                }
            }
        }
        if (out != null) {
            out.println(command);
        } else {
            keepGoing = false;
            finalize();
        }
    }

    /**
     * prints the given message to the chatframe
     * @param message to print
     */
    public void println(String message) {
        chatFrame.println(message);
    }

    /**
     * Run the chat reader.
     */
    public void run() {
        String message;
        try {
            while (keepGoing && in != null && (message = in.readLine()) != null) {
                println(message);
            }
        } catch (IOException e) {
            finalize();
        }
    }
}
