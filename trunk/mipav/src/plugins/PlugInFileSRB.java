import gov.nih.mipav.plugins.PlugInFile;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.gui.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

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
    //Used to indicate whether it is the source or the destination.
    private static final int SOURCE = 0;
    private static final int DESTINATION = 1;
    
    private String srbTempDir = "file:/C:/temp/srb-temp/";
    
    private JDialog uriDialog;
    
    private GeneralFileSystem sourceFileSystem;
    private GeneralFileSystem destinationFileSystem;
    
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
                // TODO: maybe assume multifile? get all in sequence?
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
        
        if (sourceFileSystem == null)
            return;
        // the local temporary file.
        GeneralFile firstLocalTempFile = null;
        GeneralFile localTempFile = null;
        try {
            GeneralFile localTempDir = FileFactory.newFile(new URI(srbTempDir));
            if (!localTempDir.exists()) {
                localTempDir.mkdirs();
            }

            for (int i = 0; i < fileList.size(); i++) {
                GeneralFile srbFile = null;
                if (sourceFileSystem instanceof LocalFileSystem) {
                    srbFile = FileFactory.newFile((URI) fileList.elementAt(i));
                } else {
                    srbFile = new SRBFile((SRBFileSystem) sourceFileSystem,
                            ((URI) fileList.elementAt(i)).getPath());
                }
                if (!srbFile.isFile()) {
                    // TODO handle retrieval of directories from srb
                    MipavUtil.displayError(uri.toASCIIString()
                            + " is not a file and cannot be copied locally.");
                    return;
                }

                localTempFile = new LocalFile(new File(localTempDir.getPath()
                        + File.separator + srbFile.getName()));
                if (i == 0)
                    firstLocalTempFile = localTempFile;
                /*
                 * if (localTempFile.exists()) { if
                 * (Preferences.is(Preferences.PREF_SAVE_PROMPT_OVERWRITE)) {
                 * int response = JOptionPane.showConfirmDialog(uriDialog,
                 * localTempFile + " exists. Overwrite?", "File exists",
                 * JOptionPane.YES_NO_OPTION); if (response ==
                 * JOptionPane.NO_OPTION) { continue; } } } // force overwriting
                 * of temp files file.copyTo(localTempFile, true);
                 */
                if (!localTempFile.exists())
                    srbFile.copyTo(localTempFile, true);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered copying file: "
                    + ioe.getMessage());
            return;
        } catch (URISyntaxException urie) {
            urie.printStackTrace(System.err);
            MipavUtil.displayError("SRB temp dir URI is malformed: "
                    + urie.getMessage());
            return;
        }

        ViewUserInterface.getReference().openImageFrame(
                firstLocalTempFile.getPath());

        // The local temporary file will deleted when MIPAV exits.
        localTempFile.deleteOnExit();
        firstLocalTempFile.deleteOnExit();
    }
    
    public void writeImageToURI(URI sourceURI, URI destinationURI){
        Vector fileList = new Vector();
        fileList.add(sourceURI);
        
        String extension = getExtension(sourceURI);
        try {
            if (extension.equalsIgnoreCase("")) {
                // TODO: maybe assume multifile? get all in sequence?
            } else if (extension.equalsIgnoreCase(".xml")) {
                fileList.add(new URI(sourceURI.toASCIIString().replaceFirst("\\.xml$", ".raw")));
            } else if (extension.equalsIgnoreCase(".img")) {
                fileList.add(new URI(sourceURI.toASCIIString().replaceFirst("\\.img$", ".hdr")));
            }
        } catch (URISyntaxException urie) {
            urie.printStackTrace(System.err);
            MipavUtil.displayError("URI is malformed: " + urie.getMessage());
            return;
        }
        
        if (sourceFileSystem == null || destinationFileSystem == null)
            return;

        try {
            for (int i = 0; i < fileList.size(); i++) {
                GeneralFile sourceFile = null;
                if (sourceFileSystem instanceof LocalFileSystem) {
                    sourceFile = FileFactory.newFile((URI) fileList.elementAt(i));
                } else {
                    sourceFile = new SRBFile((SRBFileSystem) sourceFileSystem,
                            ((URI) fileList.elementAt(i)).getPath());
                }
                if (!sourceFile.isFile()) {
                    // TODO handle retrieval of directories from srb
                    MipavUtil.displayError(sourceURI.toASCIIString()
                            + " is not a file and cannot be copied locally or remotely.");
                    return;
                }

                GeneralFile destinationDirectory = null;
                if (destinationFileSystem instanceof LocalFileSystem) {
                    destinationDirectory = FileFactory.newFile(destinationURI);
                } else {
                    destinationDirectory = new SRBFile((SRBFileSystem) destinationFileSystem,
                            destinationURI.getPath());
                }
               if(destinationDirectory.isFile())
                   destinationDirectory = destinationDirectory.getParentFile();
               
               GeneralFile destinationFile = null;
               if (destinationFileSystem instanceof LocalFileSystem) {
                   destinationFile = new LocalFile(getLocalPathFromURI(destinationDirectory.toURI())+ "\\" + sourceFile.getName());
               } else {
                   destinationFile = new SRBFile((SRBFileSystem) destinationFileSystem,
                           destinationDirectory.getPath()+"/"+sourceFile.getName());
               }
               
               copy(sourceFile, destinationFile);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered copying file: "
                    + ioe.getMessage());
            return;
        }
     }

    private String getLocalPathFromURI(URI uri){
        return uri.getPath().substring(1);
    }
    
    private boolean copy(GeneralFile sourceFile, GeneralFile destinationFile){
        GeneralRandomAccessFile sourceRandomAccessFile;
        GeneralRandomAccessFile destinationRandomAccessFile;
        try {
            if (sourceFile instanceof LocalFile) {
                sourceRandomAccessFile = new LocalRandomAccessFile(
                        (LocalFile) sourceFile, "r");
            } else {
                sourceRandomAccessFile = new SRBRandomAccessFile(
                        (SRBFile) sourceFile, "r");
            }
            if (destinationFile instanceof LocalFile) {
                destinationRandomAccessFile = new LocalRandomAccessFile(
                        (LocalFile) destinationFile, "rw");
            } else {
                destinationRandomAccessFile = new SRBRandomAccessFile(
                        (SRBFile) destinationFile, "rw");
            }
            long length = sourceRandomAccessFile.length();
            byte[] buffer = new byte[(int)length];
            sourceRandomAccessFile.read(buffer, 0, (int)length);
            destinationRandomAccessFile.write(buffer, 0, (int)length);

            sourceRandomAccessFile.close();
            destinationRandomAccessFile.close();
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("File I/O error: " + e.getMessage());
            return false;
        }
    }
    /**
     * @inheritDoc
     */
    public void writeImage(ModelImage image) {
        // TODO Auto-generated method stub
        uriDialog = new JDialogPushSRB();
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
        return true;
    }

    /**
     * @inheritDoc
     */
    public boolean isExtensionSupported(String ext) {
        return true;
    }
    
    private static final String getExtension(URI uri) {
//        try {
            String name = uri.toString();
            
            int index = name.lastIndexOf(".");
            
            // no extension
            if (index == -1) {
                return new String("");
            }
            
            return name.substring(index);
//        } catch (MalformedURLException urle) {
//            urle.printStackTrace(System.err);
//            MipavUtil.displayError("Malformed url: " + urle.getMessage());
//            return "";
//        }
    }
    
    private class JDialogGetURI extends JDialogURI implements ActionListener {
        private JTextField sourceURIField;
        
        /**
         * Set up the URI input GUI.
         */
        public JDialogGetURI() {
            super(ViewUserInterface.getReference().getMainFrame(), false);
            
            PanelManager manager = new PanelManager("srb:// or file:/ URI");
            manager.getConstraints().insets = new Insets(3,3,3,3);
            manager.add(WidgetFactory.buildLabel("Enter URI "));
            sourceURIField = WidgetFactory.buildTextField("srb://hwang.nih:NIHBIRN@ncmir-gpop.ucsd.edu:5825/home/hwang.nih");
            sourceURIField.setColumns(40);
            manager.add(sourceURIField);
            manager.add(WidgetFactory.buildTextButton("Browse", "Browse srb or local filesystem (determined by which is in URI field).", "Browse", this));
            manager.add(WidgetFactory.buildTextButton("Get file", "Get file indicated by URI", "Read", this));
            
            getContentPane().add(manager.getPanel());
            this.setTitle("Pull Data");
            pack();
            setVisible(true);
            MipavUtil.centerOnScreen(this);
        }
        
        /**
         * Changes the URI displayed in the URI field. 
         * @param uri the url to put into the URI text field
         */
        public void setSourceURI(URI uri) {
            String strURI = uri.toString();
            int index = strURI.indexOf("?");
            if(index >= 0)
                strURI = strURI.substring(0, index);
            sourceURIField.setText(strURI);
        }
        
        /**
         * Return the current uri that is in the uri text field.
         * @return the current uri in the uri text field, or null if it is malformed
         */
        public URI getSourceURI() {
            try {
                return new URI(sourceURIField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("SRB URI is malformed: " + urie.getMessage());
                return null;
            }
        }
        
        public URI getDestinationURI(){
            throw new UnsupportedOperationException();
        }
        
        public void setDestinationURI(URI uri){
            throw new UnsupportedOperationException();
        }
        
        /**
         * @inheritDoc
         */
        public void actionPerformed(ActionEvent event) {
            String command = event.getActionCommand();
            
            if (command.equals("Read")) {
                readImageFromURI(getSourceURI());
                
                dispose();
            } else if (command.equals("Browse")) {
                URI newURI = getURI(sourceURIField.getText(), SOURCE);
                setSourceURI(newURI);
            }
        }
    }
    
    private abstract class JDialogURI extends JDialog {
        public JDialogURI(JFrame frame, boolean modal){
            super(frame, modal);
        }
        public URI getSourceURI(){
            return null;
        }
        public URI getDestinationURI(){
            return null;
        }
        public void setSourceURI(URI uri){
            
        }
        public void setDestinationURI(URI uri){
            
        }
    }
    private class JDialogPushSRB extends JDialogURI implements ActionListener {
        private JTextField sourceURIField;
        
        private JTextField destinationURIField;
        
        /**
         * Set up the URI input GUI.
         */
        public JDialogPushSRB() {
            super(ViewUserInterface.getReference().getMainFrame(), false);
            
            PanelManager manager = new PanelManager("srb:// or file:/ URI");
            manager.getConstraints().insets = new Insets(3,3,3,3);
            manager.add(WidgetFactory.buildLabel("Enter Source URI "));
            sourceURIField = WidgetFactory.buildTextField("file:/");
            sourceURIField.setColumns(40);
            manager.add(sourceURIField);
            manager.add(WidgetFactory.buildTextButton("Browse", "Browse srb or local filesystem (determined by which is in URI field).", "sourceBrowse", this));
            manager.addOnNextLine(WidgetFactory.buildLabel("Enter Destination URI "));
            destinationURIField = WidgetFactory.buildTextField("srb://hwang.nih:NIHBIRN@ncmir-gpop.ucsd.edu:5825/home/hwang.nih");
            destinationURIField.setColumns(40);
            manager.add(destinationURIField);
            manager.add(WidgetFactory.buildTextButton("Browse", "Browse srb or local filesystem (determined by which is in URI field).", "destinationBrowse", this));
            manager.add(WidgetFactory.buildTextButton("Push file", "Push file from source URI to destination URI", "Write", this));
            
            getContentPane().add(manager.getPanel());
            this.setTitle("Push Data");
            pack();
            setVisible(true);
            MipavUtil.centerOnScreen(this);
        }
        
        /**
         * Changes the URI displayed in the URI field. 
         * @param uri the url to put into the URI text field
         */
        public void setSourceURI(URI uri) {
            String strURI = uri.toString();
            int index  = strURI.indexOf("?");
            if(index >= 0)
                strURI = strURI.substring(0, index);
            sourceURIField.setText(strURI);
        }
        
        public void setDestinationURI(URI uri) {
            String strURI = uri.toString();
            strURI = strURI.substring(0, strURI.indexOf("?"));
            destinationURIField.setText(strURI);
        }
        
        /**
         * Return the current uri that is in the uri text field.
         * @return the current uri in the uri text field, or null if it is malformed
         */
        public URI getSourceURI() {
            try {
                return new URI(sourceURIField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("URI is malformed: " + urie.getMessage());
                return null;
            }
        }
        
        public URI getDestinationURI() {
            try {
                return new URI(destinationURIField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("URI is malformed: " + urie.getMessage());
                return null;
            }
        }
        
         /**
         * @inheritDoc
         */
        public void actionPerformed(ActionEvent event) {
            String command = event.getActionCommand();
            
            if (command.equals("Write")) {
                writeImageToURI(getSourceURI(), getDestinationURI());
                dispose();
            } else if (command.equals("sourceBrowse")) {
                URI newURI = getURI(sourceURIField.getText(), SOURCE);
                setSourceURI(newURI);
            } else if (command.equals("destinationBrowse")){
                URI newURI = getURI(destinationURIField.getText(), DESTINATION);
                setDestinationURI(newURI);
            }
        }
    }
    
    private URI getURI(String uri, int use){
        if (uri.startsWith("file:/")) {
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
                    } else {
                        file = new File(System.getProperty("user.dir"));
                    }
                    LocalFile lf = new LocalFile(file);
                    if(use == SOURCE)
                        sourceFileSystem = lf.getFileSystem();
                    else
                        destinationFileSystem = lf.getFileSystem();
                    chooser.setCurrentDirectory(file);
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
                    return chooser.getSelectedFile().toURI();
                }
                else {
                    return (URI)null;
                }
            }
            catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");
                return (URI)null;
            }
        } else if(uri.startsWith("srb://")) {
            // srb browser
            try {
                URI uri2 = new URI(uri);
                JargonFileChooser chooser = new JargonFileChooser(uri2);
                int returnValue = chooser.showDialog();
                if(returnValue == JargonFileChooser.APPROVE_OPTION){
                    SRBFile f = chooser.getSelectedFile();
                    if(f != null){
                        if(use == SOURCE)
                            sourceFileSystem = f.getFileSystem();
                        else
                            destinationFileSystem = f.getFileSystem();
                        return f.toURI();
                    }
                }
            }
            catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");
                return (URI)null;
            }
            catch(IOException e){
                MipavUtil.displayError(e.getMessage());
                return (URI)null;
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("URI is malformed: " + urie.getMessage());
                return (URI)null;
            }
        } else {
            MipavUtil.displayError("URI is malformed: " + uri);
            return (URI)null;
        }
        return (URI)null;
    }
}
