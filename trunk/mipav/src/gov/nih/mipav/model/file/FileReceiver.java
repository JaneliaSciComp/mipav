package gov.nih.mipav.model.file;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.zip.*;
import javax.net.*;
import javax.net.ssl.*;
import java.awt.*;

import gov.nih.mipav.view.JChatFrame;
import gov.nih.mipav.view.ViewJProgressBar;

/**
 * <p>Title: File Receiver</p>
 * <p>Description: Allows user to receive a single zipped file, then unzip it</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class FileReceiver
    extends Thread {

  public static String HOST = "";
  public static int PORT = 9130;
  public static String DELIM = "\\";

  private ServerSocket serverSocket;
  private String directory;
  private String tempFileName;
  private ViewJProgressBar progressBar;
  private JChatFrame chatFrame;

  public FileReceiver(JChatFrame chatFrame, String directory, int port) throws
      IOException {
    this.chatFrame = chatFrame;
    this.directory = directory;
    this.tempFileName = directory + "\\temp.zip";
    try {
      HOST = InetAddress.getLocalHost().getHostAddress();
    }
    catch (UnknownHostException error) {
    }

    if (port > 1024 && port <= 9999) {
      PORT = port;
    }

    try {
      // creates a server socket
      ServerSocketFactory ssocketFactory = SSLServerSocketFactory.getDefault();
      serverSocket = ssocketFactory.createServerSocket(PORT);

      ssocketFactory = null;
    }
    catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  public void run() {

    System.out.println("Start FileReceiver at " + HOST + ":" + PORT + "...");

    Socket socket;
    InputStream is;
    BufferedReader in;
    File newFile = new File(tempFileName);
    DataInputStream dis;
    boolean stillSending = false;
    String readStr;

    try {
      socket = serverSocket.accept();


      is = socket.getInputStream();
      in = new BufferedReader(new InputStreamReader(is));

      progressBar = new ViewJProgressBar("Receiving file(s)",
                                         "Receiving...", 0, 100, false, null, null);

      progressBar.updateValue(0, false);
      progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2,
                              50);
      progressBar.setVisible(true);



      try {
        while ( (readStr = in.readLine()) != null) {
          //first look for the BEGINFILETRANSFER command because it tells us how many bytes we are receiving
          if (readStr.startsWith("BEGINFILETRANSFER")) {

            StringTokenizer tokens = new StringTokenizer(readStr);
            tokens.nextToken();
            int available = Integer.parseInt(tokens.nextToken());
            int mod = available / 100;

            dis = new DataInputStream(is);
            byte[] byteBuffer = new byte[available];
            RandomAccessFile raFile = new RandomAccessFile(newFile, "rw");

            try {
              int counter = 0;
              while (true) {
                byteBuffer[counter] = dis.readByte();
                counter++;
                if (counter % mod == 0) {
                  int value = (int)((float) counter / (float)available * 100.0f);
                  progressBar.updateValue(value, false);
                }
              }
            }
            catch (IOException io) {
              socket.close();
              socket = null;
              serverSocket.close();
              serverSocket = null;

              raFile.write(byteBuffer);
              raFile.close();
              raFile = null;
              dis.close();
              dis = null;

              progressBar.setVisible(false);
              progressBar = null;
              if (unzipFiles()) {
                System.out.println("Unzipped files and closed all sockets");
              }
              return;

            }
          }
        }
      }
      catch (IOException ex) {
        //let this one go
        System.err.println("Got IOException in inner loop in file receiver.java: " + ex.toString());
      }
    }
    catch (IOException ex) {
      System.err.println("Caught IOException in file receiver.java: " + ex.toString());
      return;
    }
  }

  /**
   * Unzips the files (creating new directory structure if needed) and deletes the received zip file
   * @return success on unzipping
   */
  private boolean unzipFiles() {
    try {
      // Open the ZIP file
      ZipInputStream in = new ZipInputStream(new FileInputStream(tempFileName));

      ZipFile tempZip = new ZipFile(tempFileName);
      float numEntries = tempZip.size();
      tempZip.close();
      tempZip = null;

      ZipEntry entry;
      OutputStream out;
      int len;
      byte[] buf;
      long compressedSize;
      long decompressedSize;

      progressBar = new ViewJProgressBar("File(s) received",
                                         "Unzipping file(s)", 0, 100, false, null, null);

      progressBar.updateValue(0, true);
      progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2,
                              50);
      progressBar.setMessage("");
      progressBar.updateValue(0, true);
      progressBar.setVisible(true);

      float counter = 1.0f;
      while ( (entry = in.getNextEntry()) != null) {
        progressBar.setMessage("Unzipping file: " + entry.getName());

        if (entry.isDirectory()) {
          String dirname = directory + File.separator +
              entry.getName().substring(0, entry.getName().length() - 1);
          new File(dirname).mkdir();
        }
        else {
          out = new FileOutputStream(directory + File.separator + entry.getName());

          // Transfer bytes from the ZIP file to the output file
          buf = new byte[1024];

          while ( (len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
          }
          out.close();
        }
        int value = (int)(counter / numEntries * 100.0f);
        progressBar.updateValue(value, true);
        counter++;
      }

      progressBar.setVisible(false);
      progressBar = null;

      in.close();
      in = null;
      out = null;

      //delete zip file after decompressing
      FileDeleter fd = new FileDeleter(tempFileName);
      fd.start();
    }
    catch (IOException e) {
      System.err.println("IOException while unzipping files: " + e.toString());
    }

    return true;
  }

}
