package gov.nih.mipav.view.dialogs;

import java.io.File;
import java.io.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.xml.parsers.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogRunScriptModel.ScriptVOI;
import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;

/**
 * @author Nathan Pollack -- Contractor (SSAI)
 * @version 0.1 May 24, 2006
 * @see JDialogRunScriptView
 * @see JDialogRunScriptModel
 * <p></p>
 * 
 */
public class JDialogRunScriptController implements ActionListener {

    JDialogRunScriptView view;
    JDialogRunScriptModel model;
    

   /**
    * JDialogRunScriptController Constructor
    * @param    scriptFile  The name of the scriptFile to use , this is a script 
    * that has defined the structure of the script the user will populate
    * 
    */     
    public JDialogRunScriptController(String scriptFile) {       
        this.model = new JDialogRunScriptModel();       
        model.setScriptFile(scriptFile);
        populateModel(scriptFile);
        this.view = new JDialogRunScriptView(scriptFile, this, model);
        this.model.addObserver(view);
    }
    
    
    
    

    /*
     * Calls methods to populate model and direct view to draw itself
     */
    private void populateModel(String scriptFile) {
        populateLists();
        populateScriptTree(scriptFile);
       // view.displayView(scriptFile);
     }

    
    /*
     * Populates image and voi lists by calling ViewUserInterface and getting
     * all images currently open, then getting all open VOIs associated with those images
     */
    private void populateLists() {
        java.util.Enumeration images = ViewUserInterface.getReference().getRegisteredImages();
        while (images.hasMoreElements()) {
            model.updateVector((ModelImage)images.nextElement());
        }
    }
    
    /*
     * Creates the tree structure from the parser code
     */
    private void populateScriptTree(String scriptFile) {
        
         try{
            
            String[] images = gov.nih.mipav.model.scripting.Parser.getImageVarsUsedInScript(scriptFile);
            
        model.setImagePlaceHolders(gov.nih.mipav.model.scripting.Parser.getImageVarsUsedInScript(scriptFile));
        int[] numberOfVOIs = new int[model.getImagePlaceHolders().length];
        for (int i = 0; i < model.getImagePlaceHolders().length; i++) {
            numberOfVOIs[i] = gov.nih.mipav.model.scripting.Parser.getNumberOfVOIsRequiredForImageVar(scriptFile,model.getImagePlaceHolders()[i]);
        }//i
        model.setNumberOfVOIs(numberOfVOIs);
        } catch (Exception e){
            e.printStackTrace();
        }
    }

    
    
    
   

   


  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    /*
    *  Main event handler for MIPAV scripting tool
    * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
    */ 
    
    
    public void actionPerformed(ActionEvent e) {
        /*
         * ********************************************************
         * Add a new script executer
         * ********************************************************
         */
        if (e.getActionCommand().equalsIgnoreCase("Add new script executer")) {
            view.addExecuter(model.getImagePlaceHolders(), model.getNumberOfVOIs());
         }//add new executer

        /*
         * ********************************************************
         * Run script 
         * ********************************************************
         */
       if (e.getActionCommand().equalsIgnoreCase("Run script")) {
            if (!(view.parseTreeForPlaceHolders())) ParserHardcoded.runScript(model.getScriptFile());
        }//run script


       
       /*
        * ********************************************************
        * Add an image from file
        * ********************************************************
        */
        if (e.getActionCommand().equalsIgnoreCase("Add image from file")) {
            gov.nih.mipav.view.ViewUserInterface.getReference().openImageFrame();
            ModelImage lastModelImage = null;
            java.util.Enumeration imageNames = ViewUserInterface.getReference().getRegisteredImageNames();
            while (imageNames.hasMoreElements()) {
                lastModelImage = (ModelImage) (ViewUserInterface.getReference().getRegisteredImageByName(
                        (String) imageNames.nextElement()));
            }
            model.updateVector(lastModelImage);
            ((gov.nih.mipav.view.ViewJFrameImage)ViewUserInterface.getReference().getImageFrameVector().firstElement()).close();
       }//add image from file

        
        
        /*
         * ********************************************************
         * Add a VOI from file
         * ********************************************************
         */
        if (e.getActionCommand().equalsIgnoreCase("Add VOI from file")) {

            if ((((JList) ((JScrollPane) view
                    .getComponentByName("Images List: scroll")).getViewport()
                    .getView()).getSelectedValue()) == null) {
                JOptionPane.showMessageDialog(view.getFrame(),
                        "Please select an image to load VOIS from.",
                        "MIPAV Warning", JOptionPane.WARNING_MESSAGE);
                return;
            }

           // JDialogRunScriptView.ModelImageForScripting imageScript = ((JDialogRunScriptView.ModelImageForScripting) ((JList) ((JScrollPane) view
           //         .getComponentByName("Images List: scroll")).getViewport().getView()).getSelectedValue());

           // gov.nih.mipav.model.structures.VOI[] vois = gov.nih.mipav.view.ViewUserInterface
           //         .getReference().getActiveImageFrame().openVOI(imageScript.getModelImage(), false);

            
            JDialogRunScriptModel.ScriptModelImage imageScript = ((JDialogRunScriptModel.ScriptModelImage) ((JList) ((JScrollPane) view
                    .getComponentByName("Images List: scroll")).getViewport().getView()).getSelectedValue());
            
            
            int selectedIndex = ((JList) ((JScrollPane) view
                    .getComponentByName("Images List: scroll")).getViewport().getView()).getSelectedIndex();
            
            JDialogRunScriptModel.ScriptVOI[] vois = imageScript.getScriptVOIs();
            
            
            String fileName;
            String directory;
           
            
            //this code is reused, might want to make a helper method
            JFileChooser chooser = new JFileChooser();

            chooser.setDialogTitle("Open VOI");
            chooser.setCurrentDirectory(new File(gov.nih.mipav.view.ViewUserInterface.getReference().getDefaultDirectory()));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "xml", "voi" }));
            
            int returnValue = chooser.showOpenDialog(view.getFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                
            } else {
                return;
            }
            
            
            //adds the last voi to the voi vector in JDialogRunScriptModel
            
            //JDialogRunScriptModel.ScriptVOI scriptVOI = imageScript.new ScriptVOI(new VOI((short)3, "bigCircle", 32));
            //vois[vois.length-1] = new VOI((short)3, "bigCircle", 32);
            
            int[] x = new int[2];
            int[] y = new int[2];
            int[] z = new int[2];
            
          /*
            
            try{
            VOI[] voiArr = new FileVOI().readVOI(fileName, directory, 3, 32);
           // System.out.println("VOI: " + voiArr[0]);
            voiArr[0].getBounds(x,y,z);
            System.out.println("** " + voiArr[0].area());
           // System.out.println("x[0]: " + x[0] + ", y[0]: " + y[0] + ", z[0]: " + z[0]);
           // System.out.println("x[1]: " + x[1] + ", y[1]: " + y[1] + ", z[1]: " + z[1]);
            } catch (IOException ioe){
                ioe.printStackTrace();
            }
            
            */
            
          //  VOI voi = new VOI((short)1, fileName, imageScript.getZDim());
         //   
         //       voi.getBounds(x,y,z);
         //       System.out.println("x[1]: " + x[1] + ", y[1]: " + y[1] + ", z[1]: " + z[1]);
                

          //  System.out.println("VOI: " + voi);
            
            
            model.addVOI(fileName,directory,selectedIndex);
           // model.updateVector(vois[vois.length-1]);
            
            
            
        /*    
            ((gov.nih.mipav.view.ViewJFrameImage)ViewUserInterface.getReference().getImageFrameVector().firstElement())
            .actionPerformed(new ActionEvent(this,0,"Open all VOIs from..."));*/
            
        }//add voi from file

        /*
         * ********************************************************
         * Save the script
         * ********************************************************
         */
        if (e.getActionCommand().equalsIgnoreCase("Save...")) {            
            
           String xmlTree = parseTreeToXML(view.tree);
            
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Save script to XML");
            chooser.showSaveDialog(view.contentPane);

            File saveFile = new File(chooser.getCurrentDirectory(), chooser.getSelectedFile().getName());

            try {
                PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(saveFile)));
                out.write(xmlTree);
                out.flush();
                out.close();

            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        }//save

        /*
         * ********************************************************
         * Open a script
         * ********************************************************
         */
        if (e.getActionCommand().equalsIgnoreCase("Open Script...")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Open script");
            chooser.showOpenDialog(view.contentPane);

            try {
                File openFile = new File(chooser.getCurrentDirectory(), chooser.getSelectedFile().getName());
                
                org.w3c.dom.Document savedScript = DocumentBuilderFactory
                        .newInstance().newDocumentBuilder().parse(openFile);
                
                 

                view.createScriptTree(savedScript);

            } catch (IOException ioe) {
                ioe.printStackTrace();
            } catch (IllegalArgumentException iae) {
                System.out.println("file is null)");
                iae.printStackTrace();
            } catch (Exception ex) {
                ex.printStackTrace();
            
            }
        }//open

    }//actionPerformed

    String parseTreeToXML(javax.swing.JTree tree) {
        StringBuffer xmlDoc = new StringBuffer();
        TreeNode root = (TreeNode) tree.getModel().getRoot();
        xmlDoc.append("<" + "root" + ">\n");
        for (int i = 0; i < root.getChildCount(); i++) {
            xmlDoc.append("<" + ((TreeNode) root.getChildAt(i)).toString().trim() + ">\n"); //script executer
            for (int j = 0; j < ((TreeNode) root.getChildAt(i)).getChildCount(); j++) {

                JDialogRunScriptView.MipavScriptTreeNode node = (JDialogRunScriptView.MipavScriptTreeNode) root.getChildAt(i).getChildAt(j);
                
               // System.out.println("file--: " +  model.getScriptModelImage((String)node.getUserObject()).getFileLocation());
                String imageName = ((TreeNode) root.getChildAt(i).getChildAt(j)).toString().trim();
                
                xmlDoc.append("<Image");
                xmlDoc.append(" name=\"" + imageName + "\"");
                
                xmlDoc.append(" defaultName= \"" + node.getDefaultName() + "\"");
                
                xmlDoc.append(" filePath= \"" + model.getScriptModelImage((String)node.getUserObject()).getFileLocation() + " \" "+ ">\n"); //images
                //xmlDoc.append("<filePath>" + model.getScriptModelImage((String)node.getUserObject()).getFileLocation() + "</filePath>\n");
                
                //VOIs
                for (int k = 0; k < ((TreeNode) root.getChildAt(i).getChildAt(j)).getChildCount(); k++) {
                    String voiName = ((TreeNode) root.getChildAt(i).getChildAt(j).getChildAt(k)).toString().trim();
                    String filePath = model.getScriptModelImage(imageName).getScriptVOI(voiName).getVoiFileLocation();
                    
                    
                    if (filePath == null){
                        String imageFilePath = model.getScriptModelImage((String)node.getUserObject()).getFileLocation();
                        String voiFileLocation = imageFilePath.substring(0,imageFilePath.lastIndexOf(java.io.File.separator)+1) + "defaultVOIs_" + imageName + java.io.File.separator;
                        String fileName = voiFileLocation + voiName + ".xml";
                      
                        if (new java.io.File(fileName).exists()){
                            filePath = fileName;
                            }else{
                                Object[] options = { "Save", "Set File Location" };
                                int userSelection = JOptionPane.showOptionDialog(null, "Uable to locate VOI: " + voiName + ", in default MIPAV location either save it now, or set its location on the file system", "Question", JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE ,null, options, options[0]);
                                System.out.println("userSelection: " + userSelection);
                                
                                if (userSelection == 0){
                                model.makeVoiActive(voiName);
                                String savedFile = ((gov.nih.mipav.view.ViewJFrameImage)ViewUserInterface.getReference().getImageFrameVector().firstElement()).saveVOIAs();
                                filePath = savedFile;
                                }
                                
                                if (userSelection == 1){
                                    JFileChooser chooser = new JFileChooser();

                                    chooser.setDialogTitle("Set VOI file location");
                                    chooser.setCurrentDirectory(new File(gov.nih.mipav.view.ViewUserInterface.getReference().getDefaultDirectory()));
                                    chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "xml", "voi" }));                                   
                                    int returnValue = chooser.showOpenDialog(view.getFrame());
                                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                                        String file = chooser.getSelectedFile().getName();
                                        String directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar; 
                                        filePath = directory + file;
                                    }
                                }
                                
                             }
                    }else{
                        filePath += voiName;
                    }
                    
                    xmlDoc.append("<VOI");
                    xmlDoc.append(" name=\"" + voiName + "\"");//vois
                    xmlDoc.append(" filePath= \"" + filePath + " \""+ " />"); 
                    //xmlDoc.append("</VOI>\n");
                }
                
                
                
                //xmlDoc.append("</" + ((TreeNode) root.getChildAt(i).getChildAt(j)).toString().trim() + ">\n");
                xmlDoc.append("</Image>\n");
            }
            xmlDoc.append("</" + ((TreeNode) root.getChildAt(i)).toString().trim() + ">\n");
        }
        xmlDoc.append("</" + "root" + ">\n");
        
       // xmlDoc.toString().replace("Script Executer", "Script_Executer").replace("VOI Needed","VOI_Needed");
        //return xmlDoc.toString().replace(" ", "_").replace("$", "__").replace("'", "___");
        return xmlDoc.toString().replace("Script Executer", "Script_Executer");
        /*   return xmlDoc.toString().replace
        ("Script Executer", "Script__Executer").replace
        ("VOI Needed","VOI_Needed").replace
        ("$", "___").replace
        ("'", "____");*/
    }

    public static void main(String[] args) {
        new JDialogRunScriptController("myScript");
    }
}
