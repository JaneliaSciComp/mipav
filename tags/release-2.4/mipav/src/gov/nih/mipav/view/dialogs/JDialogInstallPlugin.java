package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.util.zip.*;
import java.io.*;

import javax.swing.*;

/**
 *   Simple dialog to install a plugin.  The user selects which .class
 *   file to install using a file chooser.  The file is copied into
 *   MIPAV's class directory and the mipav.preferences file is updated
 *   accordingly.  The menubars are also updated.
 *
 *		@version    1.0 July 19, 2000
 *		@author     Harman Singh
 *
 */

public class JDialogInstallPlugin extends JDialogBase implements ActionListener {

    private JTextField textName;
    private JButton browseButton;
    private Vector files = new Vector();
    private String pluginDir = System.getProperty("user.home") + File.separator +
                               "mipav" + File.separator + "plugins" +
                               File.separator;
    private String docsDir = pluginDir + File.separator + "docs";
    private ViewUserInterface ui;

    /**
     *   Creates new dialog.
     *   @param theParentFrame   Parent frame
     *   @param userInterface    Pointer to the user interface.
     */
    public JDialogInstallPlugin(JFrame theParentFrame,
                                ViewUserInterface userInterface) {
        super(theParentFrame, true);
        ui = userInterface;
        init();
    }

    /**
     *   Sets up GUI dialog.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("Install Plugin");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Install Plugin Parameters"));

        JLabel labelType = new JLabel("Plugin Type");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        textName = new JTextField(15);
        textName.setText(".class, .jar, .zip");
        textName.setFont(serif12);
        textName.setEnabled(false);

        browseButton = new JButton("Browse");
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.addActionListener(this);

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gbc.anchor = gbc.WEST;

        mainPanel.add(browseButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(textName, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        panel.add(mainPanel);

        mainDialogPanel.add(panel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
    }

    //************************************************************************
     //**************************** Action Events *****************************
      //************************************************************************

       /**
        *   Calls various methods depending on the action
        *   @param event      event that triggered function
        */
       public void actionPerformed(ActionEvent event) {
           Object source = event.getSource();

           if (source == browseButton) {

               try {
                   // Check if this is running on a Windows machine:
                   JFileChooser chooser = new JFileChooser();
                   chooser.setDialogTitle("Select Plugin File(s)");
                   chooser.addChoosableFileFilter(new ViewImageFileFilter(
                           ViewImageFileFilter.PLUGIN)); //shows only files with .class, .jar, .zip extensions
                   chooser.setCurrentDirectory(new File(System.getProperty(
                           "user.dir") +
                           File.separator + "plugins"));
                   chooser.setMultiSelectionEnabled(true);
                   int returnValue = chooser.showOpenDialog(this);

                   if (returnValue == JFileChooser.APPROVE_OPTION) {
                       files.removeAllElements();
                       textName.setText("");
                       textName.setToolTipText(null);

                       File[] fileArray = chooser.getSelectedFiles();
                       String fileNames = new String();

                       for (int i = 0; i < fileArray.length; i++) {
                           files.add(fileArray[i]);
                           fileNames += fileArray[i].getName() + " ";
                       }
                       if (files.size() > 0) {
                           textName.setText(fileNames);
                           if (fileNames.length() > 100) {
                               textName.setToolTipText(fileNames.substring(0,
                                       99) + "...");
                           } else {
                               textName.setToolTipText(fileNames);
                           }
                       }

                   } else return;
               } catch (OutOfMemoryError e) {
                   MipavUtil.displayError("Out of memory");
                   return;
               }

           }

           else if (source == OKButton) {
               String userHome;
               int i;

               if (files.size() == 0) {
                   MipavUtil.displayError("Please select PlugIn file(s)");
                   return;
               }

               //make the plugins directory if it does not exist
               if (!new File(pluginDir).isDirectory()) {
                   new File(pluginDir).mkdirs();
               }

               FileOutputStream fw = null; //for outputting copied file
               FileInputStream fr = null; //for inputting the source plugin file
               BufferedInputStream br = null; //buffers are used to speed up the process
               BufferedOutputStream bw = null;
               ZipFile zFile = null;
               ZipEntry entry = null;
               ZipInputStream zIn = null;

               byte[] buf = null;
               int len;

               for (i = 0; i < files.size(); i++) {
                   File currentFile = (File) files.elementAt(i);
                   if (currentFile.getName().endsWith(".class")) {
                       try {
                           fr = new FileInputStream(currentFile); //sets the fileinput to the directory chosen from the browse option
                           fw = new FileOutputStream(pluginDir +
                                   File.separatorChar +
                                   currentFile.getName()); // the location to be copied is MIPAV's class path

                           br = new BufferedInputStream(fr);
                           bw = new BufferedOutputStream(fw);

                           int fileLength = (int) currentFile.length();

                           byte byteBuff[] = new byte[fileLength];

                           if (fileLength != 0) {
                               while (br.read(byteBuff, 0, fileLength) != -1) {
                                   bw.write(byteBuff, 0, fileLength);
                               }
                           }

                       } catch (FileNotFoundException fnfe) {
                           MipavUtil.displayError("InstallPlugin: " + fnfe);
                           dispose();
                           return;
                       }

                       catch (IOException ioe) {
                           MipavUtil.displayError(
                                   "Error reading/writing plugin files.  Try manually copying .class files to " +
                                   pluginDir);
                           dispose();
                           return;
                       }

                       finally {
                           try {
                               if (br != null) br.close();
                               if (bw != null) bw.close();
                           } catch (IOException ioe) {
                               ioe.printStackTrace();
                           }
                       }
                   }
                   //must be a .jar or .zip so extract files
                   else {
                       try {
                           zFile = new ZipFile(currentFile);
                           zIn = new ZipInputStream(new FileInputStream(
                                   currentFile));
                           entry = null;

                           int numEntries = zFile.size();

                           //if the entry is a directory of is a class file, extract it
                           while ((entry = zIn.getNextEntry()) != null) {

                               if (entry.isDirectory()) {
                                   String dirname = pluginDir + File.separator +
                                           entry.getName().substring(0,
                                           entry.getName().length() - 1);
                                   new File(dirname).mkdir();
                               } else {
                                   if (entry.getName().endsWith(".class")) {

                                       try {
                                           new File(pluginDir +
                                                   File.separator +
                                                   entry.getName()).
                                                   getParentFile().mkdirs();
                                       } catch (Exception ex) {
                                           //do nothing...no parent dir here
                                       }
                                       fw = new FileOutputStream(pluginDir +
                                               File.separator +
                                               entry.getName());

                                       // Transfer bytes from the ZIP file to the output file
                                       buf = new byte[1024];

                                       while ((len = zIn.read(buf)) > 0) {
                                           fw.write(buf, 0, len);
                                       }
                                       fw.close();
                                   } else {
                                       //these are not .class files so put them in pluginDir/docs
                                       String fileName = null;
                                       if (new File(entry.getName()).
                                           getParentFile() != null) {
                                           fileName = new File(entry.getName()).
                                                   getName();
                                       } else {
                                           fileName = entry.getName();
                                       }
                                       new File(docsDir).mkdirs();

                                       fw = new FileOutputStream(docsDir +
                                               File.separator + fileName);

                                       // Transfer bytes from the ZIP file to the output file
                                       buf = new byte[1024];

                                       while ((len = zIn.read(buf)) > 0) {
                                           fw.write(buf, 0, len);
                                       }
                                       fw.close();

                                   }
                               }
                           }

                       } catch (Exception e) {
                           e.printStackTrace();
                       } finally {
                           try {
                               if (zIn != null) zIn.close();
                               if (fw != null) fw.close();
                           } catch (IOException ioe) {
                               ioe.printStackTrace();
                           }
                       }

                   }
               }

               try {
                   if (zIn != null) zIn.close();
                   if (fw != null) fw.close();
                   if (bw != null) bw.close();
                   if (br != null) br.close();
               } catch (IOException ioe) {
                   ioe.printStackTrace();
               }

               //updates menubar for each image
               Vector imageFrames = ui.getImageFrameVector();
               if (imageFrames.size() < 1) {
                   ui.buildMenu();
                   ui.setControls();
               } else {
                   for (i = 0; i < imageFrames.size(); i++) {
                       ((ViewJFrameImage) (imageFrames.elementAt(i))).
                               updateMenubar();
                   }
               }

               dispose();
               return;
           }

           else if (source == cancelButton) {
               dispose();
           }
       }

}
