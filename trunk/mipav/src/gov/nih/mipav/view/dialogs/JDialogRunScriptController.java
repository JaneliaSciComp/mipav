package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.event.*;

import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.tree.*;

import javax.xml.parsers.*;


/**
 * @author   Nathan Pollack -- Contractor (SSAI)
 * @version  0.1 May 24, 2006
 * @see      JDialogRunScriptView
 * @see      JDialogRunScriptModel
 */
public class JDialogRunScriptController implements ActionListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JDialogRunScriptModel model;

    /** DOCUMENT ME! */
    private JDialogRunScriptView view;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * JDialogRunScriptController Constructor.
     *
     * @param  scriptFile  The name of the scriptFile to use , this is a script that has defined the structure of the
     *                     script the user will populate
     */
    public JDialogRunScriptController(String scriptFile) {
        this.model = new JDialogRunScriptModel();
        model.setScriptFile(scriptFile);
        populateModel(scriptFile);
        this.view = new JDialogRunScriptView(this, model);
       // this.model.addObserver(view);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Main event handler for MIPAV scripting tool.
     *
     * @see  #actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equalsIgnoreCase("Run script")) {
            runScript();
        } else if (command.equalsIgnoreCase("Add image from file")) {
        	ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
        	JFileChooser chooser = fileChooser.getFileChooser();
        	chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        	int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	boolean isMultiFile = fileChooser.isMulti();
            	
            	File [] files = chooser.getSelectedFiles();
            	for (int i = 0; i < files.length; i++) {
            		model.addToAvailableImageList(files[i].getName(), 
            				files[i].getPath(), isMultiFile);
            	}
            	view.update();
            }
        } else if (command.equalsIgnoreCase("Add VOI from file")) {

            if ((((JList) ((JScrollPane) view.getComponentByName("Images List: scroll")).getViewport().getView())
                     .getSelectedValue()) == null) {
                MipavUtil.displayWarning("Please select an image to load VOIS from.");

                return;
            }

            // JDialogRunScriptView.ModelImageForScripting imageScript = ((JDialogRunScriptView.ModelImageForScripting)
            // ((JList) ((JScrollPane) view.getComponentByName("Images List:
            // scroll")).getViewport().getView()).getSelectedValue()); VOI[] vois =
            // ViewUserInterface.getReference().getActiveImageFrame().openVOI(imageScript.getModelImage(), false);
            ScriptImage imageScript = ((ScriptImage)
                                           ((JList)
                                                ((JScrollPane) view.getComponentByName("Images List: scroll"))
                                                    .getViewport().getView()).getSelectedValue());
            int selectedIndex = ((JList)
                                     ((JScrollPane) view.getComponentByName("Images List: scroll")).getViewport().getView())
                                    .getSelectedIndex();
            ScriptVOI[] vois = imageScript.getScriptVOIs();
            String fileName;
            String directory;

            // this code is reused, might want to make a helper method
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Open VOI");
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "xml", "voi" }));

            int returnValue = chooser.showOpenDialog(view.getFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
            	File [] files = chooser.getSelectedFiles();
            	for (int i = 0; i < files.length; i++) {
            		model.addVOI(files[i].getName(), files[i].getPath(), selectedIndex);
            	}
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else {
                return;
            }

            
        } else if (command.equalsIgnoreCase("Save current image and VOI selections")) {
        	if (view.isTreeReadyForScriptExecution()) {
            String xmlTree = parseTreeToXML(view.getTreeRoot());
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Save script to XML");
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
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
        	}
        } else if (command.equalsIgnoreCase("Open saved image and VOI selections")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Open saved image and VOI selections");
            chooser.showOpenDialog(view.getFrame());

            try {
            	if (chooser.getSelectedFile() != null) {
            		
            	
                File openFile = new File(chooser.getCurrentDirectory(), chooser.getSelectedFile().getName());
                org.w3c.dom.Document savedScript = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(openFile);
                view.createScriptTree(savedScript);
            	}
            } catch (IOException ioe) {
                ioe.printStackTrace();
            } catch (IllegalArgumentException iae) {
                Preferences.debug("run dialog:\tScript file we have been told to open is null.",
                                  Preferences.DEBUG_SCRIPTING);
                iae.printStackTrace();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } else if (command.equalsIgnoreCase("View current script contents")) {
            JDialog scriptContentsDialog = new JDialog(view.getFrame(), "Script contents: " + model.getScriptFile(),
                                                       false);

            String contents = new String();

            try {
                BufferedReader scriptReader = new BufferedReader(new FileReader(model.getScriptFile()));

                while (scriptReader.ready()) {
                    contents += scriptReader.readLine() + "\n";
                }
            } catch (FileNotFoundException fnfe) {
                MipavUtil.displayError("File not found: " + model.getScriptFile());
                fnfe.printStackTrace();
            } catch (IOException ioe) {
                MipavUtil.displayError("Error encountered while reading script file contents: " +
                                       model.getScriptFile());
                ioe.printStackTrace();
            }

            JTextArea contentsArea = new JTextArea(contents, 20, 80);
            contentsArea.setEditable(false);

            JScrollPane scrollPane = new JScrollPane(contentsArea);

            scriptContentsDialog.getContentPane().add(scrollPane);
            scriptContentsDialog.pack();
            MipavUtil.centerInWindow(view.getFrame(), scriptContentsDialog);
            scriptContentsDialog.setVisible(true);
        } else if (command.equalsIgnoreCase("Close")) {
            view.getFrame().dispose();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   imageLocation  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String openImageWithFrame(String imageLocation, boolean doMulti) {
        ViewOpenFileUI openFile = new ViewOpenFileUI(true);
        
        String imageName = openFile.open(imageLocation, doMulti, null);

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
                if (!sizeChanged) {

                    // load any luts
                    imgFrame.loadLUT(true, true);
                }

                // load any vois
                imgFrame.loadAllVOIs(true);
            } catch (IllegalArgumentException iae) {

                // MipavUtil.displayError("There was a problem with the supplied name.\n" );
                Preferences.debug("Illegal Argument Exception in " + "ViewUserInterface.openImageFrame(). " +
                                  "Somehow the Image list sent an incorrect name to " + "the image image hashtable. " +
                                  "\n", 1);
                Preferences.debug("Bad argument.");
            }
        }

        return imageName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   root  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String parseTreeToXML(TreeNode root) {
    	
    	ScriptTreeNode scriptNode = (ScriptTreeNode)root.getChildAt(0);
    	int numImagePlaceHolders = scriptNode.getChildCount();
    	
//    	find out how many times the script will run
    	int numExecuters = ((ScriptTreeNode)scriptNode.getChildAt(0)).getChildCount();
    	
    	ScriptTreeNode imagePHNode = null;
    	ScriptTreeNode imageNode = null;
    	ScriptTreeNode voiNode = null;
    	
    	int numVOIs;
    	
    	
        StringBuffer xmlDoc = new StringBuffer();
        
        xmlDoc.append("<" + "root" + ">\n");
     
        int numImages = 0;
        
        for (int i = 0; i < numImagePlaceHolders; i++) {
        	xmlDoc.append("<Image_Placeholder>\n");
        	imagePHNode = (ScriptTreeNode) scriptNode.getChildAt(i);
        	numImages = imagePHNode.getChildCount();
        	
        	for (int j = 0; j < numImages; j++) {
        		imageNode = (ScriptTreeNode)imagePHNode.getChildAt(j);
        		String imageName = ((String)imageNode.getUserObject()).trim();
                xmlDoc.append("<Image");
                xmlDoc.append(" name=\"" + imageName + "\"");
                xmlDoc.append(" filePath= \"" + model.getScriptImage(imageName).getFileLocation() +
                              " \" ");
                xmlDoc.append(" isMulti=\"" + model.getScriptImage(imageName).isMultiFile() + " \" " + ">\n"); // images
                
                numVOIs = imageNode.getChildCount();
                                
                for (int k = 0; k < numVOIs; k++) {
                	voiNode = (ScriptTreeNode)imageNode.getChildAt(k);
                    String voiName = ((String)voiNode.getUserObject()).trim();
                    System.err.println("VOIName: " + voiName);
                    if (model.getScriptImage(imageName) ==  null) {
                    	System.err.println("scriptimage null");
                    } else if (model.getScriptImage(imageName).getScriptVOI(voiName) == null) {
                    	System.err.println("script image VOI is null...");
                    }
                    String filePath = model.getScriptImage(imageName).getScriptVOI(voiName).getVoiFileLocation();

                    if (filePath == null) {
                        String imageFilePath = model.getScriptImage(imageName).getFileLocation();
                        String voiFileLocation = imageFilePath.substring(0,
                                                                         imageFilePath.lastIndexOf(File.separator) + 1) +
                                                 "defaultVOIs_" + imageName + File.separator;
                        String fileName = voiFileLocation + voiName + ".xml";

                        if (new java.io.File(fileName).exists()) {
                            filePath = fileName;
                        } else {
                            Object[] options = { "Save", "Set File Location" };
                            int userSelection = JOptionPane.showOptionDialog(null,
                                                                             "Unable to locate VOI: " + voiName +
                                                                             ", in default MIPAV location either save it now, or set its location on the file system",
                                                                             "Question", JOptionPane.DEFAULT_OPTION,
                                                                             JOptionPane.QUESTION_MESSAGE, null,
                                                                             options, options[0]);
                            System.out.println("userSelection: " + userSelection);

                            if (userSelection == 0) {
                                String savedFile = ((ViewJFrameImage)
                                                        ViewUserInterface.getReference().getImageFrameVector().firstElement())
                                                       .saveVOIAs();
                                filePath = savedFile;
                            }

                            if (userSelection == 1) {
                                JFileChooser chooser = new JFileChooser();
                                chooser.setDialogTitle("Set VOI file location");
                                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                                chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "xml", "voi" }));
                                
                                int returnValue = chooser.showOpenDialog(view.getFrame());

                                if (returnValue == JFileChooser.APPROVE_OPTION) {
                                    String file = chooser.getSelectedFile().getName();
                                    String directory = String.valueOf(chooser.getCurrentDirectory()) +
                                                       File.separatorChar;
                                    filePath = directory + file;
                                }
                            }
                        }
                    } else {
                        filePath += voiName;
                    }

                    xmlDoc.append("<VOI");
                    xmlDoc.append(" name=\"" + voiName + "\""); // vois
                    xmlDoc.append(" filePath= \"" + filePath + " \"" + " />");
                    // xmlDoc.append("</VOI>\n");
                }
                xmlDoc.append("</Image>\n");
        	}

        	// xmlDoc.append("</" + ((TreeNode) root.getChildAt(i).getChildAt(j)).toString().trim() + ">\n");
        	xmlDoc.append("</Image_Placeholder>\n");
        }
            
         
        xmlDoc.append("</" + "root" + ">\n");
        

        // xmlDoc.toString().replace("Script Executer", "Script_Executer").replace("VOI Needed","VOI_Needed");
        // return xmlDoc.toString().replace(" ", "_").replace("$", "__").replace("'", "___");
        // return xmlDoc.toString().replace("Script Executer", "Script_Executer");
        return xmlDoc.toString().replaceAll("Script Executer", "Script_Executer");
               /*
                * return xmlDoc.toString().replace ("Script Executer", "Script__Executer").replace ("VOI
                * Needed","VOI_Needed").replace ("$", "___").replace ("'", "____");
                */
    }

    /**
     * Populates image and voi lists by calling ViewUserInterface and getting all images currently open, then getting
     * all open VOIs associated with those images.
     */
    private void populateAvailableObjectsLists() {
        Enumeration images = ViewUserInterface.getReference().getRegisteredImages();
System.err.println("doing this");
        while (images.hasMoreElements()) {
            model.addToAvailableImageList((ModelImage) images.nextElement());
        }
    }

    /**
     * Creates the tree structure from the parser code.
     *
     * @param  scriptFile  The name of the script file to use
     */
    private void populateInitialScriptTree(String scriptFile) {

        try {

            // this is pretty inefficient, but it works..
            model.setScriptImageVars(Parser.getImageVarsUsedInScript(scriptFile));
            model.setScriptImageVarLabels(Parser.getImageLabelsUsedInScript(scriptFile));
            model.setScriptImageVarActions(Parser.getActionsForImagesUsedInScript(scriptFile));

            int[] numberOfVOIs = new int[model.getScriptImageVars().length];

            for (int i = 0; i < model.getScriptImageVars().length; i++) {
                numberOfVOIs[i] = Parser.getNumberOfVOIsRequiredForImageVar(scriptFile, model.getScriptImageVars()[i]);
            }

            model.setNumberOfRequiredVOIsForScriptImages(numberOfVOIs);
        } catch (ParserException pe) {
            MipavUtil.displayError("Unable to get the number of images and VOIs required for the script.\n\n" + pe);
        }
    }

    /**
     * Calls methods to populate model and direct view to draw itself.
     *
     * @param  scriptFile  The name of the script file to use
     */
    private void populateModel(String scriptFile) {
        populateAvailableObjectsLists();
        populateInitialScriptTree(scriptFile);
        // view.displayView(scriptFile);
    }

    /**
     * DOCUMENT ME!
     */
    private void runScript() {

        if (view.isTreeReadyForScriptExecution()) {
            view.getFrame().setVisible(false);

            String[] scriptVars = model.getScriptImageVars();

            Vector scriptExecutors = new Vector();
            Vector scriptExecutorsVOIs = new Vector();
            view.fillImagesVOIs(scriptExecutors, scriptExecutorsVOIs);
            

            for (int i = 0; i < scriptExecutors.size(); i++) {
                Vector scriptImages = (Vector) scriptExecutors.elementAt(i);
                Vector imagesOpenedByDialog = new Vector();
                Preferences.debug("run dialog:\tScript execution #" + i + " images to be used:\n",
                                  Preferences.DEBUG_SCRIPTING);

                for (int j = 0; j < scriptImages.size(); j++) {

                    // open any images which were selected from disk in this dialog, then replace their filepath with
                    // their new image name
                    ScriptImage si = model.getScriptImage((String) scriptImages.elementAt(j));
                   
                    if (si.isOpenedByScript()) {
                    	
                    	String imageName = openImageWithFrame(si.getFileLocation(), si.isMultiFile());
                        scriptImages.setElementAt(imageName, j);
                        imagesOpenedByDialog.addElement(ViewUserInterface.getReference().getRegisteredImageByName(imageName));
                    }
                    
                    Preferences.debug("run dialog:\tScript execution #" + i + "\t" + scriptVars[j] + " -> " +
                                      scriptImages.elementAt(j) + "\n", Preferences.DEBUG_SCRIPTING);
                }

                Vector scriptVOIs = (Vector) scriptExecutorsVOIs.elementAt(i);
                Preferences.debug("run dialog:\tScript execution #" + i + " VOIs to be used:\n",
                                  Preferences.DEBUG_SCRIPTING);

                for (int j = 0; j < scriptVOIs.size(); j++) {
                    Preferences.debug("run dialog:\tScript execution #" + i + "\t" + scriptVars[j] + " VOI " + j +
                                      " -> " + scriptVOIs.elementAt(j) + "\n", Preferences.DEBUG_SCRIPTING);
                }

                Preferences.debug("run dialog:\tStarting script execution #" + i + "\n", Preferences.DEBUG_SCRIPTING);

                if (ScriptRunner.getReference().runScript(model.getScriptFile(), scriptImages, scriptVOIs)) {
                    Preferences.debug("run dialog:\tFinished script execution #" + i + "\n",
                                      Preferences.DEBUG_SCRIPTING);
                } else {
                    Preferences.debug("run dialog:\tError during script execution #" + i + "\n",
                                      Preferences.DEBUG_SCRIPTING);
                }

                for (int j = 0; j < imagesOpenedByDialog.size(); j++) {
                    ((ModelImage) imagesOpenedByDialog.elementAt(j)).getParentFrame().close();
                }

                System.gc();
            }

            view.getFrame().dispose();
        }
    }
}
