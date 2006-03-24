import gov.nih.mipav.plugins.PlugInFile;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.gui.*;

import java.io.File;
import java.io.IOException;
import java.net.*;
import java.util.*;
import javax.swing.*;

import java.awt.*;
import java.awt.event.*;

/**
 * Demo plugin to read/write image files using SRB.
 *
 * @author mccreedy
 */
public class PlugInFileSRB implements PlugInFile {
    private String srbTempDir = "file:/C:/temp/srb-temp/";
    private JDialog uriDialog;
    
    /**
     * @inheritDoc
     */
    public void readImage() {
        uriDialog = new JDialogGetURI();
    }
    
    /**
     * Retrieves an image located at an URI, saves it in a local temp directory, and opens the image with MIPAV.
     * @param uri the URI of the image to copy locally and open
     */
    public void readImageFromURI(URI uri) {
        Vector fileList = new Vector();
        fileList.add(uri);
        
        String extension = getExtension(uri);
        try {
            if (extension.equalsIgnoreCase("")) {
                // TODO: maybe assume multifile?  get all in sequence?
            } else if (extension.equalsIgnoreCase(".xml")) {
                fileList.add(new URI(uri.toASCIIString().replaceFirst("\\.xml$", ".raw")));
            } else if (extension.equalsIgnoreCase(".img")) {
                fileList.add(new URI(uri.toASCIIString().replaceFirst("\\.img$", ".hdr")));
            }
        } catch (URISyntaxException urie) {
            urie.printStackTrace(System.err);
            MipavUtil.displayError("URI is malformed: " + urie.getMessage());
            return;
        }
        
        try {
            GeneralFile tempDir = FileFactory.newFile(new URI(srbTempDir));
            if (!tempDir.exists()) {
                tempDir.mkdirs();
            }
            
            for (int i = 0; i < fileList.size(); i++) {
                GeneralFile file = FileFactory.newFile((URI)fileList.elementAt(i));
                if (!file.isFile()) {
                    // TODO handle retrieval of directories from srb
                    MipavUtil.displayError(uri.toASCIIString() + " is not a file and cannot be copied locally.");
                    return;
                }
                
                GeneralFile localFile = FileFactory.newFile(new URI(srbTempDir + "/" + file.getName()));
                if (localFile.exists()) {
                    if (Preferences.is(Preferences.PREF_SAVE_PROMPT_OVERWRITE)) {
                        int response = JOptionPane.showConfirmDialog(uriDialog, localFile + " exists.  Overwrite?", "File exists", JOptionPane.YES_NO_OPTION);
                        if (response == JOptionPane.NO_OPTION) {
                            continue;
                        }
                    }
                }
            
                // force overwriting of temp files
                file.copyTo(localFile, true);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered copying file: " + ioe.getMessage());
            return;
        } catch (URISyntaxException urie) {
            urie.printStackTrace(System.err);
            MipavUtil.displayError("SRB temp dir URI is malformed: " + urie.getMessage());
            return;
        }
        
        try {
            GeneralFile localFile = FileFactory.newFile((URI)fileList.elementAt(0));
            String localDir = localFile.getParent();
            String localFileName = localFile.getName();
            
            ViewUserInterface.getReference().openImageFrame(localFileName, localDir);
        } catch (IOException ioe) {
            ioe.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered copying file: " + ioe.getMessage());
            return;
        }
    }

    /**
     * @inheritDoc
     */
    public void writeImage(ModelImage image) {
        // TODO Auto-generated method stub
    }

    /**
     * @inheritDoc
     */
    public boolean canReadImages() {
        return true;
    }

    /**
     * @inheritDoc
     */
    public boolean canWriteImages() {
        return false;
    }

    /**
     * @inheritDoc
     */
    public boolean isExtensionSupported(String ext) {
        return true;
    }
    
    private static final String getExtension(URI uri) {
        try {
            String name = uri.toURL().getFile();
            
            int index = name.lastIndexOf(".");
            
            // no extension
            if (index == -1) {
                return new String("");
            }
            
            return name.substring(index);
        } catch (MalformedURLException urle) {
            urle.printStackTrace(System.err);
            MipavUtil.displayError("Malformed url: " + urle.getMessage());
            return "";
        }
    }
    
    private class JDialogGetURI extends JDialog implements ActionListener {
        private JTextField uriField;
        
        /**
         * Set up the URI input GUI.
         */
        public JDialogGetURI() {
            super(ViewUserInterface.getReference().getMainFrame(), false);
            
            PanelManager manager = new PanelManager("srb:// or file:/ URI");
            manager.getConstraints().insets = new Insets(3,3,3,3);
            manager.add(WidgetFactory.buildLabel("Enter URI "));
            uriField = WidgetFactory.buildTextField("file:/");
            uriField.setColumns(40);
            manager.add(uriField);
            manager.add(WidgetFactory.buildTextButton("Browse", "Browse srb or local filesystem (determined by which is in URI field).", "Browse", this));
            manager.add(WidgetFactory.buildTextButton("Get file", "Get file indicated by URI", "ReadURI", this));
            
            getContentPane().add(manager.getPanel());
            
            pack();
            setVisible(true);
            MipavUtil.centerOnScreen(this);
        }
        
        /**
         * Changes the URI displayed in the URI field. 
         * @param uri the url to put into the URI text field
         */
        public void setURI(URI uri) {
            uriField.setText(uri.toASCIIString());
        }
        
        /**
         * Return the current uri that is in the uri text field.
         * @return the current uri in the uri text field, or null if it is malformed
         */
        public URI getURI() {
            try {
                return new URI(uriField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("SRB URI is malformed: " + urie.getMessage());
                return null;
            }
        }
        
        /**
         * @inheritDoc
         */
        public void actionPerformed(ActionEvent event) {
            String command = event.getActionCommand();
            
            if (command.equals("ReadURI")) {
                readImageFromURI(getURI());
                
                dispose();
            } else if (command.equals("Browse")) {
                new JDialogBrowse(this);
            }
        }
    }
    
    private class JDialogBrowse extends JDialog implements ActionListener {
        private JDialogGetURI parent;
        
        /**
         * Create the filesystem browser GUI.
         * @param dialog the parent dialog to this browser dialog
         */
        public JDialogBrowse(JDialogGetURI dialog) {
            super(dialog, false);
            parent = dialog;
            
            if (parent.getURI().toASCIIString().startsWith("file:/")) {
                // local browser
                try {
                    JFileChooser chooser = new JFileChooser();
                    chooser.setFont(MipavUtil.defaultMenuFont);
                    
                    Dimension d = new Dimension(700, 400);
                    chooser.setMinimumSize(d);
                    chooser.setPreferredSize(d);

                    MipavUtil.setFonts(chooser.getComponents());
                    
                    chooser.setLocation(new Point(ViewUserInterface.getReference().getNewFrameLocation().width, ViewUserInterface.getReference().getNewFrameLocation().height));

                    // TODO default to current dir in uri field
                    
                    if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                        File file = new File(ViewUserInterface.getReference().getDefaultDirectory());
                        if (file != null) {
                            chooser.setCurrentDirectory(file);
                        } else {
                            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                        }
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
    
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));
                    chooser.setFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
    
                    chooser.setDialogTitle("Select Image");
                    int returnValue = chooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());
    
                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        parent.setURI(chooser.getSelectedFile().toURI());
                    }
                    else {
                        return;
                    }
                }
                catch (OutOfMemoryError e) {
                    MipavUtil.displayError("Out of memory!");
                    return;
                }
            } else {
                // srb browser
                GeneralFile filesysRoot;
                JargonTree filesysTree;
                try {
                    filesysRoot = FileFactory.newFile(parent.getURI());
                    filesysTree = new JargonTree(filesysRoot);
                    
                    
                } catch (IOException ioe) {
                    ioe.printStackTrace(System.err);
                    MipavUtil.displayError("Error encountered copying file: " + ioe.getMessage());
                    return;
                }
                
                getContentPane().add(filesysTree);
                
                pack();
                setVisible(true);
                setSize(800, 800);
                MipavUtil.centerOnScreen(this);
            }
        }
        
        /**
         * @inheritDoc
         */
        public void actionPerformed(ActionEvent event) {
            String command = event.getActionCommand();
            
            System.out.println("cmd: " + command);
        }
    }
}
