package gov.nih.mipav.view;

import java.io.*;
import java.net.*;
import java.util.*;
import java.text.DateFormat;
import javax.net.*;
import javax.net.ssl.*;

/**
 * <p>Title: ServeOneClient</p>
 * <p>Description: For each connection, there will be one ServeOneClient created</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

class ServeOneClient
    extends Thread {
  private static DateFormat dateFormat = DateFormat.getDateTimeInstance(
      DateFormat.FULL,
      DateFormat.LONG);

  private Socket socket; // socket through which to connect
  private String clientName = null; // client's name (username)
  private String clientIP = null; // client's ip address
  private BufferedReader in; // for input sent from client
  private PrintWriter out; // for output to go to client
  private boolean hadSameName = false;  // if the client logging in has a non-unique username (clientName)

  public ServeOneClient(Socket s) throws IOException {
    socket = s;
    in = new BufferedReader(new InputStreamReader(s.getInputStream()));
    out = new PrintWriter(s.getOutputStream(), true);

    start();
  }

  private void login() throws IOException, SSLHandshakeException {

    clientName = in.readLine();
    clientIP = in.readLine();

    //check to see if that username is already taken
    Enumeration e = ChatServer.clients.keys();
    String name;
    while (e.hasMoreElements()) {
      name = (String) e.nextElement();
      if (clientName.equalsIgnoreCase(name)) {
        // can't have two users with same name
        out.println("User with the name " + clientName + " is already logged into server.");
        out.println("Change username and re-login.");
        out.println("CLIENTBOOT");
        try {
          socket.close();
          in.close();
          out.close();
        }
        catch (IOException ex) {
          System.out.println("ServeOneClient.login(): " + ex.toString());
        }
        hadSameName = true;
        return;
      }
    }

    ChatServer.clients.put(clientName, this);

    Date date = new Date(System.currentTimeMillis());
    String message = clientName + " logs in at " + dateFormat.format(date) +
        " from " + clientIP;

    relayMessage(message);
    sendClientListString();
  }

  /**
   * notifies all clients that server is shutting down, and then shuts the server down
   */
  public void notifyAndLogout() {
    out.println("Server is shutting down.....logging out");
    //System.err.println("SERVER IS SHUTTING DOWN");

    relayMessage("SERVERDISCONNECT");
    try {
      socket.close();
      in.close();
      out.close();
    }
    catch (IOException ex) {
      System.out.println("ServeOneClient.notifyAndLogout(): " + ex.toString());
    }
  }

  /**
   * logs the client out of the server
   */
  private void logout() {
    ChatServer.clients.remove(clientName);
    Date date = new Date(System.currentTimeMillis());
    String message = clientName + " logs out at " + dateFormat.format(date) +
        ".";

    relayMessage(message);
    sendClientListString();

    try {
      socket.close();
      in.close();
      out.close();
    }
    catch (IOException ex) {
      System.out.println("ServeOneClient.logout(): " + ex.toString());
    }
  }
  /**
   * relays the message to all connected clients
   * @param message
   */
  private void relayMessage(String message) {
    ServeOneClient client;
    String name;
    Enumeration names = ChatServer.clients.keys();

    while (names.hasMoreElements()) {
      name = (String) names.nextElement();
      client = (ServeOneClient) ChatServer.clients.get(name);
      client.out.println(message);
    }
  }

  /**
   *  relays a string of messages to connected clients with the sender's name in brackets
   * @param tokens
   */
  private void printMessage(StringTokenizer tokens) {
    if (tokens.hasMoreTokens()) {
      String message = tokens.nextToken("\n");
      relayMessage("[" + clientName + "] " + message);
    }
  }

  /**
   * sends connected clients a string that has each client name and IP
   */
  private void sendClientListString() {
    Enumeration names = ChatServer.clients.keys();
    String name;
    String clientList = new String("CLIENTLIST");

    while (names.hasMoreElements()) {
      name = (String) names.nextElement();
      try {
        clientList += " " + name + " " + ((ServeOneClient)ChatServer.clients.get(name)).clientIP;
      }
      catch(Exception ex) {
        System.err.println(ex.toString());
      }
    }
    relayMessage(clientList);
  }

  /**
   * lists the currently connected clients
   */
  private void listClients() {
    String name, ip;
    Enumeration names = ChatServer.clients.keys();

    out.println("------------------------------------");
    out.println("Current Users:");

    while (names.hasMoreElements()) {
      name = (String) names.nextElement();
      ip = ((ServeOneClient)ChatServer.clients.get(name)).clientIP;
      out.println(name + " from " + ip);
    }
    out.println("------------------------------------");
  }

  /**
   * sends a private message from one client to another
   * @param tokens
   */
  private void tellClient(StringTokenizer tokens) {
    System.out.println("in tell client");
    ServeOneClient client = null;
    try {
      String talker = tokens.nextToken(" ");
      String name;
      Enumeration names = ChatServer.clients.keys();
      boolean found = false;

      while (!found && names.hasMoreElements()) {
        name = (String) names.nextElement();
        if (talker.equalsIgnoreCase(name)) {
          client = (ServeOneClient) ChatServer.clients.get(name);
          found = true;
        }

        if (found) {
          String message =  tokens.nextToken("\n").substring(1);
          out.println("[To " + client.clientName + "] " + message);
          client.out.println("[From " + clientName + "] " + message);
        }
        else {
          out.println("User " + talker + " not found.");
        }
      }
    }
    catch (NoSuchElementException ex) {
      System.err.println(ex.toString());
    }
  }

  /**
   * if the /SEND command is received, the client that will receive the files
   * is sent a message telling it to start its file receiver
   * @param tokens
   */
  private void startFileReceiver(StringTokenizer tokens) {
    ServeOneClient client = null;
    try {
      String talker = tokens.nextToken(" ");
      String name;
      Enumeration names = ChatServer.clients.keys();
      boolean found = false;

      //System.err.println("Looking for: " + talker);
      while (!found && names.hasMoreElements()) {
        name = (String) names.nextElement();
        if (talker.equalsIgnoreCase(name)) {
          client = (ServeOneClient) ChatServer.clients.get(name);
          found = true;
        }
      }
      if (found) {
        String fileSendMessage = "FILESEND " + clientName;
        //System.err.println(fileSendMessage);
        client.out.println(fileSendMessage);
      }
      else {
        out.println("User " + talker + " not found.");
      }

    }
    catch (NoSuchElementException ex){
    }
  }

  /**
   * checks to see if the command has the correct # of arguments
   * @param command
   * @param numArgs
   * @param tokens
   * @return
   */
  private boolean hasEnoughArgs(String command, int numArgs,
                                StringTokenizer tokens) {
    boolean enough = (tokens.countTokens() < numArgs);
    if (!enough) {
      out.println("Command " + command + " needs " + numArgs + " arguments.");
    }
    return enough;
  }

  /**
   * a Command was sent through that was not recognized
   * @param command (the command being sent to the server)
   */
  private void invalidCommand(String command) {
    out.println("Command " + command + " is not available.");
  }

  /**
   * run method for serveoneclient
   */
  public void run() {
    String readStr;
    String command;
    StringTokenizer tokens;

    try {
      login();
      while ( (readStr = in.readLine()) != null) {
       // System.out.println("Get message --> " + readStr);
        tokens = new StringTokenizer(readStr);
        command = tokens.nextToken();
        //System.err.println("Command is: " + command + "******");

        if (command.equals("MESG")) {
          printMessage(tokens);
        }
        else if (command.equals("LIST")) {
          listClients();
        }
        else if (command.equals("TELL")) {
          tellClient(tokens);
        }
        else if (command.equals("SEND")) {
          startFileReceiver(tokens);
        }
        else {
          invalidCommand(command);
        }
      }
    }
    catch (SocketException se) {
      System.err.println(se.toString());
    }
    catch (SSLHandshakeException ex) {
      System.err.println(ex.toString());
    }
    catch (IOException e) {
      // do nada
    }
    finally {
      if (!hadSameName) {
        if (clientName != null) {
          //System.err.println("Client is null..logging out of server");
          logout();
        }
        try {
          socket.close();
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
  }
}

/**
 * Public class to host a chat server... holds a hashtable of clients
 * <p>Title: ChatServer</p>
 * <p>Description: A chat server!</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */
public class ChatServer
    extends Thread {
  public static int PORT = 9030;
  public static String DELIM = "\\";

  static Hashtable clients = new Hashtable();
  private ServerSocket serverSocket;
  private boolean keepgoing = true;

  public ChatServer(int port) throws IOException {

    if (port > 1024 && port <= 9999) {
      PORT = port;
    }

    ServerSocketFactory ssocketFactory = SSLServerSocketFactory.getDefault();
    serverSocket = ssocketFactory.createServerSocket(PORT);

    String ip = "Unknown IP";
    try {
      ip = InetAddress.getLocalHost().getHostAddress();
    }
    catch (UnknownHostException error) {

    }
    System.err.println("Started Chat Server at " + ip + " on port " + PORT);
  }

  /**
   * stops the server
   */
  public void stopServer() {

      Enumeration e = clients.keys();
      String name;
      while (e.hasMoreElements()) {
        name = (String) e.nextElement();
        ServeOneClient client = (ServeOneClient)clients.get(name);
        client.notifyAndLogout();
      }
      try {
        serverSocket.close();
      }
      catch (IOException ex) {
        System.out.println("ChatServer.stopServer(): " + ex.toString());
      }
  }

  /**
   * run method for chatserver:
   * continue to look for connections while server is up
   */
  public void run() {

    try {
      while (true) {
        Socket socket = serverSocket.accept();
        try {
          new ServeOneClient(socket);
        }
        catch (IOException e) {
          socket.close();
        }
      }
    }
    catch (IOException e) {

    }
    System.out.println("At end of run() in ChatServer");
  }

}