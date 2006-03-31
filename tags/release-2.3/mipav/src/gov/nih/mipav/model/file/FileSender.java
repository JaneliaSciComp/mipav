package gov.nih.mipav.model.file;

import java.io.*;
import java.net.*;
import javax.net.*;
import javax.net.ssl.*;

/**
 * <p>Title: FileSender</p>
 * <p>Description: Sends a single zipped file to the given server</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class FileSender extends Thread {

  private static int DEFAULT_PORT = 9130;

  private String host = null;
  private int port = DEFAULT_PORT;
  private Socket socket = null;
  private String filename;
  private FileInputStream fis = null;
  private PrintWriter pwriter = null;
  private OutputStream out = null;
  private boolean keepGoing = true;
  private int availableBytes = 0;

  private boolean isDone = false;

  public FileSender(String filename, String host, int port) {
    this.filename = filename;
    this.host = host;
    this.port = port;
  }

  public boolean login() {
    try {
      if (socket == null) {

        SocketFactory socketFactory = SSLSocketFactory.getDefault();
        socket = socketFactory.createSocket(host, port);
      }

      //sends the BEGINFILETRANSFER COMMAND and the # of bytes it will be sending
      if (socket.isConnected()) {
        pwriter = new PrintWriter(socket.getOutputStream(), true);
        fis = new FileInputStream(filename);
        availableBytes = fis.available();
        pwriter.println("BEGINFILETRANSFER " + availableBytes);
        out = socket.getOutputStream();
        start();
      }
      else {
        System.err.println("FileSender socket is not connected");
        return false;
      }
    }

    catch (UnknownHostException e) {
      System.err.println("Got an exception: " + e.toString());
      return false;
    }
    catch (IOException e) {
      e.printStackTrace();
      return false;
    }
    catch (Exception e) {
      System.err.println("Got an exception: " + e.toString());
      return false;
    }
    return true;
  }

  public boolean isDone() {
    return isDone;
  }

  public void run() {
    int b;
    int counter = 0;

    try {
      byte [] j = new byte[availableBytes];
      fis.read(j);
      out.write(j);

      fis.close();
      out.close();
      pwriter.close();
      socket.close();
      fis = null;
      socket = null;
      out = null;
      pwriter = null;

      isDone = true;

    }
    catch (IOException ioex) {
      // end of file
      System.err.println("FileSender Problem: " + ioex.toString());
    }
  }
}
