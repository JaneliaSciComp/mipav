package gov.nih.mipav.view.dialogs;


import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.TreeNode;
import javax.xml.parsers.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.scripting.*;


/**
 * @author Nathan Pollack -- Contractor (SSAI)
 * @version 0.1 May 24, 2006
 * @see JDialogRunScriptView
 * @see JDialogRunScriptModel
 */
public class JDialogRunScriptController implements ActionListener {
    private JDialogRunScriptView view;

    private JDialogRunScriptModel model;

    /**
     * JDialogRunScriptController Constructor.
     * 
     * @param  scriptFile  The name of the scriptFile to use , this is a script that has defined the structure of the script the user will populate
     */
    public JDialogRunScriptController(String scriptFile) {
        this.model = new JDialogRunScriptModel();
        model.setScriptFile(scriptFile);
        populateModel(scriptFile);
        this.view = new JDialogRunScriptView(this, model);
        this.model.addObserver(view);
    }

    /**
     * Calls methods to populate model and direct view to draw itself
     * 
     * @param  scriptFile  The name of the script file to use
     */
    private void populateModel(String scriptFile) {
        populateAvailableObjectsLists();
        populateInitialScriptTree(scriptFile);
        // view.displayView(scriptFile);
    }

    /**
     * Populates image and voi lists by calling ViewUserInterface and getting all images currently open, then getting
     * all open VOIs associated with those images
     */
    private void populateAvailableObjectsLists() {
        Enumeration images = ViewUserInterface.getReference().getRegisteredImages();
        while (images.hasMoreElements()) {
            model.addToAvailableImageList((ModelImage) images.nextElement());
        }
    }

    /**
     * Creates the tree structure from the parser code
     * 
     * @param  scriptFile  The name of the script file to use
     */
    private void populateInitialScriptTree(String scriptFile) {
        try {
            model.setScriptImageVars(Parser.getImageVarsUsedInScript(scriptFile));
            int[] numberOfVOIs = new int[model.getScriptImageVars().length];
            for (int i = 0; i < model.getScriptImageVars().length; i++ ) {
                numberOfVOIs[i] = Parser.getNumberOfVOIsRequiredForImageVar(scriptFile, model.getScriptImageVars()[i]);
            }
            model.setNumberOfRequiredVOIsForScriptImages(numberOfVOIs);
        } catch (ParserException pe) {
            MipavUtil.displayError("Unable to get the number of images and VOIs required for the script.\n\n" + pe);
        }
    }

    /**
     * Main event handler for MIPAV scripting tool
     * 
     * @see  #actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        
        if (command.equalsIgnoreCase("Add new script executer")) {
            view.addExecuter(model.getScriptImageVars(), model.getNumberOfRequiredVOIsForScriptImages());
        } else if (command.equalsIgnoreCase("Run script")) {
            runScript();
        } else if (command.equalsIgnoreCase("Add image from file")) {
            ModelImage newImage = openImageWithoutFrame();
            if (newImage != null) {
                model.addToAvailableImageList(newImage.getImageName(), newImage.getImageDirectory() + newImage.getImageFileName(), newImage.getExtents(), newImage.getVOIs());
                newImage.disposeLocal();
            }
        } else if (command.equalsIgnoreCase("Add VOI from file")) {
            if ( ( ((JList) ((JScrollPane) view.getComponentByName("Images List: scroll")).getViewport().getView())
                    .getSelectedValue()) == null) {
                MipavUtil.displayWarning("Please select an image to load VOIS from.");
                return;
            }
            // JDialogRunScriptView.ModelImageForScripting imageScript = ((JDialogRunScriptView.ModelImageForScripting)
            // ((JList) ((JScrollPane) view.getComponentByName("Images List: scroll")).getViewport().getView()).getSelectedValue());
            // VOI[] vois = ViewUserInterface.getReference().getActiveImageFrame().openVOI(imageScript.getModelImage(), false);
            ScriptImage imageScript = ((ScriptImage) ((JList) ((JScrollPane) view
                    .getComponentByName("Images List: scroll")).getViewport().getView()).getSelectedValue());
            int selectedIndex = ((JList) ((JScrollPane) view.getComponentByName("Images List: scroll")).getViewport()
                    .getView()).getSelectedIndex();
            ScriptVOI[] vois = imageScript.getScriptVOIs();
            String fileName;
            String directory;
            // this code is reused, might want to make a helper method
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Open VOI");
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {"xml", "voi"}));
            int returnValue = chooser.showOpenDialog(view.getFrame());
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else {
                return;
            }
            model.addVOI(fileName, directory, selectedIndex);
        } else if (command.equalsIgnoreCase("Save...")) {
            String xmlTree = parseTreeToXML(view.getTreeRoot());
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Save script to XML");
            chooser.showSaveDialog(view.getFrame());
            File saveFile = new File(chooser.getCurrentDirectory(), chooser.getSelectedFile().getName());
            try {
                PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(saveFile)));
                out.write(xmlTree);
                out.flush();
                out.close();
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        } else if (command.equalsIgnoreCase("Open Script...")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Open script");
            chooser.showOpenDialog(view.getFrame());
            try {
                File openFile = new File(chooser.getCurrentDirectory(), chooser.getSelectedFile().getName());
                org.w3c.dom.Document savedScript = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(
                        openFile);
                view.createScriptTree(savedScript);
            } catch (IOException ioe) {
                ioe.printStackTrace();
            } catch (IllegalArgumentException iae) {
                Preferences.debug("run dialog:\tScript file we have been told to open is null.", Preferences.DEBUG_SCRIPTING);
                iae.printStackTrace();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    /**
     * Opens a file chooser, allowing the user to select an image which should be opened. Exactly the same as
     * ViewUserInterface.openImageFrame() but skips image frame creation.
     * 
     * @return  The new image from the file the user selects, may be null if there is a problem opening the file.
     * 
     * @see     ViewUserInterface#openImageFrame()
     */
    public ModelImage openImageWithoutFrame() {
        ViewOpenFileUI openFile = new ViewOpenFileUI(true);
        boolean stackFlag = ViewUserInterface.getReference().getLastStackFlag();
        // set the filter type to the preferences saved filter
        int filter = ViewImageFileFilter.TECH;
        try {
            filter = Integer.parseInt(Preferences.getProperty("FilenameFilter"));
        } catch (NumberFormatException nfe) {
            // an invalid value was set in preferences -- so fix it!
            filter = ViewImageFileFilter.TECH;
            Preferences.setProperty("FilenameFilter", Integer.toString(filter));
        }
        openFile.setFilterType(filter);
        openFile.setPutInFrame(false);
        
        // Matt through in a _false_ to get it to compile - 12/31/2002
        Vector openImageNames = openFile.open(stackFlag, false);
        
        // if open failed, then imageNames will be null
        if (openImageNames == null) {
            return null;
        }
        
        boolean sizeChanged = false;
        // if the SaveAllOnSave preference flag is set, then
        // load all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {
            Enumeration e = openImageNames.elements();
            while (e.hasMoreElements()) {
                try {
                    String name = (String) e.nextElement();
                    ModelImage img = ViewUserInterface.getReference().getRegisteredImageByName(name);
                    // get frame for image
                    ViewJFrameImage imgFrame = img.getParentFrame();
                    // if the image size was changed to FLOAT, then don't
                    // load any luts (chances are they won't work)
                    if ( !sizeChanged) {
                        // load any luts
                        imgFrame.loadLUT(true, true);
                    }
                    // load any vois
                    imgFrame.loadAllVOIs(true);
                } catch (IllegalArgumentException iae) {
                    // MipavUtil.displayError("There was a problem with the supplied name.\n" );
                    Preferences.debug("Illegal Argument Exception in " + "ViewUserInterface.openImageFrame(). "
                            + "Somehow the Image list sent an incorrect name to " + "the image image hashtable. "
                            + "\n", 1);
                    Preferences.debug("Bad argument.");
                }
            }
        }
        
        if (openFile.getImage2() != null) {
            MipavUtil.displayWarning("Found that a second image was automatically opened along with the file selected.  It has been automatically closed.");
            openFile.getImage2().disposeLocal();
        }
        
        return openFile.getImage();
    }
    
    private String openImageWithFrame(String imageLocation) {
        ViewOpenFileUI openFile = new ViewOpenFileUI(true);
        boolean stackFlag = ViewUserInterface.getReference().getLastStackFlag();
        
        String imageName = openFile.open(imageLocation, stackFlag, null);

        // if open failed, then imageName will be null
        if (imageName == null) {
            return null;
        }
        
        boolean sizeChanged = false;
        // if the SaveAllOnSave preference flag is set, then
        // load all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {
            try {
                ModelImage img = ViewUserInterface.getReference().getRegisteredImageByName(imageName);
                // get frame for image
                ViewJFrameImage imgFrame = img.getParentFrame();
                // if the image size was changed to FLOAT, then don't
                // load any luts (chances are they won't work)
                if ( !sizeChanged) {
                    // load any luts
                    imgFrame.loadLUT(true, true);
                }
                // load any vois
                imgFrame.loadAllVOIs(true);
            } catch (IllegalArgumentException iae) {
                // MipavUtil.displayError("There was a problem with the supplied name.\n" );
                Preferences.debug("Illegal Argument Exception in " + "ViewUserInterface.openImageFrame(). "
                        + "Somehow the Image list sent an incorrect name to " + "the image image hashtable. "
                        + "\n", 1);
                Preferences.debug("Bad argument.");
            }
        }
        
        return imageName;
    }

    private String parseTreeToXML(TreeNode root) {
        StringBuffer xmlDoc = new StringBuffer();
        xmlDoc.append("<" + "root" + ">\n");
        for (int i = 0; i < root.getChildCount(); i++ ) {
            xmlDoc.append("<" + ((TreeNode) root.getChildAt(i)).toString().trim() + ">\n"); // script executer
            for (int j = 0; j < ((TreeNode) root.getChildAt(i)).getChildCount(); j++ ) {
                ScriptTreeNode node = (ScriptTreeNode) root.getChildAt(i).getChildAt(j);
                // System.out.println("file--: " +
                // model.getScriptModelImage((String)node.getUserObject()).getFileLocation());
                String imageName = ((TreeNode) root.getChildAt(i).getChildAt(j)).toString().trim();
                xmlDoc.append("<Image");
                xmlDoc.append(" name=\"" + imageName + "\"");
                xmlDoc.append(" defaultName= \"" + node.getDefaultName() + "\"");
                xmlDoc.append(" filePath= \""
                        + model.getScriptImage((String) node.getUserObject()).getFileLocation() + " \" " + ">\n"); // images
                // xmlDoc.append("<filePath>" +
                // model.getScriptModelImage((String)node.getUserObject()).getFileLocation() + "</filePath>\n");
                // VOIs
                for (int k = 0; k < ((TreeNode) root.getChildAt(i).getChildAt(j)).getChildCount(); k++ ) {
                    String voiName = ((TreeNode) root.getChildAt(i).getChildAt(j).getChildAt(k)).toString().trim();
                    String filePath = model.getScriptImage(imageName).getScriptVOI(voiName).getVoiFileLocation();
                    if (filePath == null) {
                        String imageFilePath = model.getScriptImage((String) node.getUserObject())
                                .getFileLocation();
                        String voiFileLocation = imageFilePath.substring(0, imageFilePath
                                .lastIndexOf(File.separator) + 1)
                                + "defaultVOIs_" + imageName + File.separator;
                        String fileName = voiFileLocation + voiName + ".xml";
                        if (new java.io.File(fileName).exists()) {
                            filePath = fileName;
                        } else {
                            Object[] options = {"Save", "Set File Location"};
                            int userSelection = JOptionPane
                                    .showOptionDialog(
                                            null,
                                            "Uable to locate VOI: "
                                                    + voiName
                                                    + ", in default MIPAV location either save it now, or set its location on the file system",
                                            "Question", JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null,
                                            options, options[0]);
                            System.out.println("userSelection: " + userSelection);
                            if (userSelection == 0) {
                                String savedFile = ((ViewJFrameImage) ViewUserInterface.getReference().getImageFrameVector().firstElement()).saveVOIAs();
                                filePath = savedFile;
                            }
                            if (userSelection == 1) {
                                JFileChooser chooser = new JFileChooser();
                                chooser.setDialogTitle("Set VOI file location");
                                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                                chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {"xml", "voi"}));
                                int returnValue = chooser.showOpenDialog(view.getFrame());
                                if (returnValue == JFileChooser.APPROVE_OPTION) {
                                    String file = chooser.getSelectedFile().getName();
                                    String directory = String.valueOf(chooser.getCurrentDirectory())
                                            + File.separatorChar;
                                    filePath = directory + file;
                                }
                            }
                        }
                    } else {
                        filePath += voiName;
                    }
                    xmlDoc.append("<VOI");
                    xmlDoc.append(" name=\"" + voiName + "\"");// vois
                    xmlDoc.append(" filePath= \"" + filePath + " \"" + " />");
                    // xmlDoc.append("</VOI>\n");
                }
                // xmlDoc.append("</" + ((TreeNode) root.getChildAt(i).getChildAt(j)).toString().trim() + ">\n");
                xmlDoc.append("</Image>\n");
            }
            xmlDoc.append("</" + ((TreeNode) root.getChildAt(i)).toString().trim() + ">\n");
        }
        xmlDoc.append("</" + "root" + ">\n");
        // xmlDoc.toString().replace("Script Executer", "Script_Executer").replace("VOI Needed","VOI_Needed");
        // return xmlDoc.toString().replace(" ", "_").replace("$", "__").replace("'", "___");
        return xmlDoc.toString().replace("Script Executer", "Script_Executer");
        /*
         * return xmlDoc.toString().replace ("Script Executer", "Script__Executer").replace ("VOI
         * Needed","VOI_Needed").replace ("$", "___").replace ("'", "____");
         */
    }
    
    private void runScript() {
        if (view.isTreeReadyForScriptExecution()) {
            view.getFrame().setVisible(false);
            
            String[] scriptVars = model.getScriptImageVars();
            
            Vector scriptExecutors = view.getUserSelectedImages();
            Vector scriptExecutorsVOIs = view.getUserSelectedVOIs();
            
            for (int i = 0; i < scriptExecutors.size(); i++ ) {
                Vector scriptImages = (Vector) scriptExecutors.elementAt(i);
                Vector imagesOpenedByDialog = new Vector();
                Preferences.debug("run dialog:\tScript execution #" + i + " images to be used:\n", Preferences.DEBUG_SCRIPTING);
                for (int j = 0; j < scriptImages.size(); j++ ) {
                    // open any images which were selected from disk in this dialog, then replace their filepath with their new image name
                    String imageElement = (String) scriptImages.elementAt(j);
                    if (imageElement.indexOf(File.separator) != -1) {
                        String imageName = openImageWithFrame(imageElement);
                        scriptImages.setElementAt(imageName, j);
                        imagesOpenedByDialog.addElement(ViewUserInterface.getReference().getRegisteredImageByName(imageName));
                    }
                    Preferences.debug("run dialog:\tScript execution #" + i + "\t" + scriptVars[j] + " -> " + scriptImages.elementAt(j) + "\n", Preferences.DEBUG_SCRIPTING);
                }
                
                Vector scriptVOIs = (Vector) scriptExecutorsVOIs.elementAt(i);
                Preferences.debug("run dialog:\tScript execution #" + i + " VOIs to be used:\n", Preferences.DEBUG_SCRIPTING);
                for (int j = 0; j < scriptVOIs.size(); j++ ) {
                    Preferences.debug("run dialog:\tScript execution #" + i + "\t" + scriptVars[j] + " VOI " + j + " -> " + scriptVOIs.elementAt(j) + "\n", Preferences.DEBUG_SCRIPTING);
                }
                
                Preferences.debug("run dialog:\tStarting script execution #" + i + "\n", Preferences.DEBUG_SCRIPTING);
                ScriptRunner.getReference().runScript(model.getScriptFile(), scriptImages, scriptVOIs);
                Preferences.debug("run dialog:\tFinished script execution #" + i + "\n", Preferences.DEBUG_SCRIPTING);
                
                for (int j = 0; j < imagesOpenedByDialog.size(); j++) {
                    ((ModelImage) imagesOpenedByDialog.elementAt(j)).getParentFrame().close();
                }
                System.gc();
            }
            
            view.getFrame().dispose();
        }
    }
}
