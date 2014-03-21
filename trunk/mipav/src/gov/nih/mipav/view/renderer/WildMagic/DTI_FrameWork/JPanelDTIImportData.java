package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

    import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMosaicToSlices;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoNIFTI;
import gov.nih.mipav.model.file.FileInfoPARREC;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewOpenFileUI;
import gov.nih.mipav.view.ViewUserInterface;

    import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Vector;

    import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;


/**
* <hr>
* The copyright below only pertains to methods within JDialogImageInfo that relate to the Gradient Table Creator for
* Philips PAR/REC files V3/V4 that is displayed in the DTI tab. Portions of code that relate to this copyright are
* denoted with comments giving credit to software and authors.
* 
* <pre>
* Copyright (c) 2011, Bennett Landman
* All rights reserved.
* Redistribution and use in source and binary forms, with or without 
* modification, are permitted provided that the following conditions are met:
* 
*      - Redistributions of source code must retain the above copyright 
*        notice, this list of conditions and the following disclaimer.
*        
*      - Redistributions in binary form must reproduce the above copyright 
*        notice, this list of conditions and the following disclaimer in the 
*        documentation and/or other materials provided with the distribution.
*        
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
* OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
* SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT 
* OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
* TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
* 
* </pre>
* */

    public class JPanelDTIImportData extends JPanel implements AlgorithmInterface, ActionListener{
        
        // ~ Instance fields
        // ------------------------------------------------------------------------------------------------    
        /** table to display the src image names. */
                        
        /** Diffusion Tensor image. */
        public ModelImage m_kDWIImage;
        
        /** T2 image. */
        public ModelImage m_kT2Image;
        
        public JScrollPane scrollPane;
        
        private DTIParameters dtiparams, newDTIparams, parDTIParams;

        private Font serif12;
               
        /** TextArea of main dialogfor text output.* */
        private JTextArea outputTextArea;
               
        private JTable srcBvalGradTable;
        
        private JTextField textDWIDataimage;
                 
        /** DOCUMENT ME! */
        private JComboBox invertedBox;
        
        /** Radio button to save gradBval text file in FSL format */
        JRadioButton fslButton;
        
        /** Radio button to save gradBval text file in DTIStudio format */
        JRadioButton dtiStudioButton; 
        
        /** Radio button to save gradBval text file in mipavStandard format */
        JRadioButton mipavStandardButton; 
        
        /** bValGrad test file name from user */
        String filebvalGradTxtName;
        
        /** number of Volumes in DWI Image */
        private int numVolumes;
        
        
        /**int to determine which format user selects for gradBval test file */
        private int gradBvalText;
            
        /**chooser for save gradBval text dialog */
        private JFileChooser saveGradchooser; 
        
        /** table model for the srcimages. */
        public DefaultTableModel srcTableModel;
            
        /** DOCUMENT ME! */
        private JCheckBox isDWICellEditBox;
        
        /** DOCUMENT ME! */
        private JCheckBox negXCheckBox;
        
       /** DOCUMENT ME! */
        private JCheckBox negYCheckBox;
        
        /** DOCUMENT ME! */
        private JCheckBox negZCheckBox;
        
        /** DOCUMENT ME! */
        private JCheckBox openedImageCheckBox;
        
        /** DOCUMENT ME! */
        private JButton openDWIButton; 
        
        /** DOCUMENT ME! */
        public JCheckBox useT2CheckBox;
        
        /** DOCUMENT ME! */
        private JLabel t2FileLabel;
        
        /** DOCUMENT ME! */
        private JLabel bvalGradFileLabel;
        
        /** DOCUMENT ME! */
        private JTextField textT2image; 
        
        /** DOCUMENT ME! */
        private JTextField textBvalGradFile;
        
        /** DOCUMENT ME! */
        private JButton openT2Button; 
        
        /** DOCUMENT ME! */
        private JButton bvalGradAppButton;
           
        /** DOCUMENT ME! */
        private JCheckBox isJonesBox; 
        
        /** DOCUMENT ME! */
        private JCheckBox isKirbyBox;
        
        /** DOCUMENT ME! */
        private JComboBox fatshiftBox;

        /** DOCUMENT ME! */
        private JComboBox gradResBox;

        /** DOCUMENT ME! */
        private JComboBox gradOPBox;
        
        /** DOCUMENT ME! */
        private JComboBox philRelBox;
        
        /** DOCUMENT ME! */
        private JComboBox patientPosBox;
        
        /** DOCUMENT ME! */
        private JComboBox patientOrientBox;
        
        /** DOCUMENT ME! */
        private JComboBox foldOverBox;
        
        /** DOCUMENT ME! */
        private JComboBox osBox;
             
        /** DOCUMENT ME! */
        private double[][] gradCreatetable;
        
        /** DOCUMENT ME! */
        private double[][] angCorrGT;
        
        /** DOCUMENT ME! */
        private double[][] rev_angCorrGT;
        
        /** DOCUMENT ME! */
        private String space;
        
        /** DOCUMENT ME! */
        private JLabel osLabel;
        
        /** DOCUMENT ME! */
        private JLabel invertedLabel;
        
        /** DOCUMENT ME! */
        private JLabel gradResLabel;
        
        /** DOCUMENT ME! */
        private JLabel gradOPLabel;
        
        /** DOCUMENT ME! */
        private JLabel fatShiftLabel;
        
        /** DOCUMENT ME! */
        private JLabel patientPosLabel;
        
        /** DOCUMENT ME! */
        private JLabel patientOrientLabel;
        
        /** DOCUMENT ME! */
        private JLabel foldOverLabel;
        
        /** DOCUMENT ME! */
        private JLabel philRelLabel;

        private DTIPipeline pipeline;
        
        private ViewUserInterface ui;
        
        /** current directory * */
        private String currDir = null;
        
        private boolean lastStackFlag = false;
        
        private JPanel srcPanel; 
        
        Vector<String> openImageNames;
        
        public ViewJFrameImage frame;
        
        public ViewJFrameImage t2frame;

        private JRadioButton browseDWIButton;

        private JRadioButton activeDWIButton;

        private JPanel DWIOpenPanel;

        private JPanel t2OpenPanel;

        private JPanel loadTable;

        private JPanel DWIButtonPanel;

        private ViewOpenFileUI openFile;

        private JButton loadBValGradFileButton;

        private JButton clearDWITableButton;

        private JButton saveBvalGradButton;

        private double sliceAng0;

        private double sliceAng1;

        private double sliceAng2;

        private String gradResWOP;

        private AlgorithmMosaicToSlices mosaicToSliceAlgo;

        private String parNversion;

        private String parNExamName;

        private String parNProtocolName;

        private String parNPatientPosition;

        private String parNfoldover;

        private double[] parNsliceAng;

        private double[] parNoffCentre;

        private int parNorient;
        
        private boolean isBmatFile = false;
        
        private FileInfoPARREC fileInfoPARREC = null;

        private boolean checkSiemens;

        


        
        // ~ Constructors
        // ---------------------------------------------------------------------------------------------------

        public JPanelDTIImportData(DTIPipeline pipeline) {
            super();
            this.pipeline = pipeline;
            ui = ViewUserInterface.getReference();
            //this.image = ModelImage image;
            init();
        }
        
        /**
         * When Apply button is pressed, applies changes to all three areas: image name, resolutions, and transformation
         * matrix. When OK button is pressed, applies changes and closes dialog box. When Cancel button is pressed, closes
         * dialog without making any additional changes.
         * 
         * @param event Event that triggers this function.
         */
        
        
        
        public void actionPerformed(final ActionEvent event) {
            final String command = event.getActionCommand();
            
    
            

            if (command.equals("applyTable")) {
                // When applyTable button is activated, the bvalues and gradients are saved to the DTI parameters object
                if (m_kDWIImage != null){ 

                  if (dtiparams != null){                   
                               // Populate Gradient column
                      if(isBmatFile==true){
                          if (srcTableModel.getRowCount() != 0){
                              if (!srcTableModel.getValueAt(0, 5).equals("")){
                                  double[][] flBmatrixArray = new double[numVolumes][6];
                                  for (int i = 0; i < numVolumes; i++) {  
                                      flBmatrixArray[i][0] = Double.valueOf((String)srcTableModel.getValueAt(i, 1));
                                      flBmatrixArray[i][1] = Double.valueOf((String)srcTableModel.getValueAt(i, 2));
                                      flBmatrixArray[i][2] = Double.valueOf((String)srcTableModel.getValueAt(i, 3));
                                      flBmatrixArray[i][3] = Double.valueOf((String)srcTableModel.getValueAt(i, 4));
                                      flBmatrixArray[i][4] = Double.valueOf((String)srcTableModel.getValueAt(i, 5));
                                      flBmatrixArray[i][5] = Double.valueOf((String)srcTableModel.getValueAt(i, 6));
                                  }
                                  dtiparams.setbMatrixVals(flBmatrixArray);
                              }
                          }
                          
                      }
                      else {
                          if (srcTableModel.getRowCount() != 0){
                              if (!srcTableModel.getValueAt(0, 1).equals("")){
                                  double [] flBvalueArr= new double[numVolumes]; 
                                  for (int i = 0; i < numVolumes; i++) { 
                                      flBvalueArr[i]= Double.valueOf((String)srcTableModel.getValueAt(i, 1));
                                      }
                                 dtiparams.setbValues(flBvalueArr);
                              }
                              
                              if (!srcTableModel.getValueAt(0, 3).equals("")){
                                  double[][] flGradArr = new double[numVolumes][3];
                                  for (int i = 0; i < numVolumes; i++) {
                                      if (!srcTableModel.getValueAt(i, 2).equals("")){
                                          flGradArr[i][0]= Double.valueOf((String)srcTableModel.getValueAt(i, 2));
                                          }
                                          else{
                                              flGradArr[i][0]= 0.0;
                                              }
                                      if (!srcTableModel.getValueAt(i, 3).equals("")){
                                          flGradArr[i][1]= Double.valueOf((String)srcTableModel.getValueAt(i, 3));
                                          }
                                          else{
                                              flGradArr[i][1]= 0.0;
                                              }
                                      if (!srcTableModel.getValueAt(i, 4).equals("")){
                                          flGradArr[i][2]= Double.valueOf((String)srcTableModel.getValueAt(i, 4));
                                          }
                                          else{
                                              flGradArr[i][2]= 0.0;
                                          }
                                      }
    
    
                                  dtiparams.setGradients(flGradArr);
                              }
                              dtiparams.setNumVolumes(numVolumes);
                              m_kDWIImage.setDTIParameters(dtiparams);
                          }
                          

                      }
                      if (useT2CheckBox.isSelected()==false){
                          if (m_kT2Image != null){
                              pipeline.nextButton.setEnabled(true);
                              pipeline.nextButton.setActionCommand("next1");
                          }
                          else {
                              MipavUtil.displayError("Please upload T2 Image");
                              useT2CheckBox.setSelected(false);
                          }
                      }
                      else {
                          pipeline.nextButton.setEnabled(true);
                          pipeline.nextButton.setActionCommand("next1");
                      }
                      
                      
                 }
                  
                  else if (dtiparams == null){
                      if (m_kDWIImage.getExtents()[3] == numVolumes ||srcTableModel.getValueAt(m_kDWIImage.getExtents()[3], 0).equals("")){
                          numVolumes = m_kDWIImage.getExtents()[3];
                          if(isBmatFile==true){
                              if (srcTableModel.getRowCount() != 0){
                                  if (!srcTableModel.getValueAt(0, 5).equals("")){
                                      double[][] flBmatrixArray = new double[numVolumes][6];
                                      for (int i = 0; i < numVolumes; i++) {  
                                          flBmatrixArray[i][0] = Double.valueOf((String)srcTableModel.getValueAt(i, 1));
                                          flBmatrixArray[i][1] = Double.valueOf((String)srcTableModel.getValueAt(i, 2));
                                          flBmatrixArray[i][2] = Double.valueOf((String)srcTableModel.getValueAt(i, 3));
                                          flBmatrixArray[i][3] = Double.valueOf((String)srcTableModel.getValueAt(i, 4));
                                          flBmatrixArray[i][4] = Double.valueOf((String)srcTableModel.getValueAt(i, 5));
                                          flBmatrixArray[i][5] = Double.valueOf((String)srcTableModel.getValueAt(i, 6));
                                      }
                                      newDTIparams.setbMatrixVals(flBmatrixArray);
                                  }
                              }
                              
                          }
                          else{
                          if (srcTableModel.getRowCount() != 0 ){
                              newDTIparams = new DTIParameters(m_kDWIImage.getExtents()[3]);
                              if (!srcTableModel.getValueAt(0, 1).equals("")){
                                  double [] flBvalueArr= new double[m_kDWIImage.getExtents()[3]]; 
                                  for (int i = 0; i < m_kDWIImage.getExtents()[3]; i++) {
                                      flBvalueArr[i]= Double.valueOf((String)srcTableModel.getValueAt(i, 1));
                                      }
                                 newDTIparams.setbValues(flBvalueArr);
                              }
                              
                              if (!srcTableModel.getValueAt(0, 3).equals("")){
                                  double[][] flGradArr = new double[m_kDWIImage.getExtents()[3]][3];
                                  for (int i = 0; i < numVolumes; i++) {
                                      if (!srcTableModel.getValueAt(i, 2).equals("")){
                                          flGradArr[i][0]= Double.valueOf((String)srcTableModel.getValueAt(i, 2));
                                          }
                                          else{
                                              flGradArr[i][0]= 0.0;
                                          }
                                      if (!srcTableModel.getValueAt(i, 3).equals("")){
                                          flGradArr[i][1]= Double.valueOf((String)srcTableModel.getValueAt(i, 3));
                                          }
                                          else{
                                              flGradArr[i][1]= 0.0;
                                          }
                                      if (!srcTableModel.getValueAt(i, 4).equals("")){
                                          flGradArr[i][2]= Double.valueOf((String)srcTableModel.getValueAt(i, 4));
                                          }
                                          else{
                                              flGradArr[i][1]= 0.0;
                                          }
                                      }
                                  
                              
                              newDTIparams.setGradients(flGradArr);
                              }

                              newDTIparams.setNumVolumes(m_kDWIImage.getExtents()[3]);
                              m_kDWIImage.setDTIParameters(newDTIparams);
                          }
                              pipeline.nextButton.setEnabled(true);
                              pipeline.nextButton.setActionCommand("next1");
                          }
                          
                  }
                      else{
                          MipavUtil.displayError("Please enter " +m_kDWIImage.getExtents()[3] +" rows of bvalues and gradients"); 
                      }
                  }
                }
                
                
               
                else{
                    MipavUtil.displayError("Please select a 4D DWI dataset");
                }

                

            }
            
            
            
       else if (command.equals("bvalGradBrowse")) {
            final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose File");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                try{
                    readBVGradBMatfile(currDir);
                    DWIButtonPanel.setBorder(highlightTitledBorder("Table Options"));
                    loadTable.setBorder(buildTitledBorder("Upload B-Value/Gradient File  or B-Matrix File"));
                    saveBvalGradButton.setEnabled(true);
                    isDWICellEditBox.setEnabled(true);
                    bvalGradAppButton.setEnabled(true);
                }
                catch (Exception e){
                    DWIButtonPanel.setBorder(buildTitledBorder("Table Options"));
                    bvalGradAppButton.setEnabled(false);
                    saveBvalGradButton.setEnabled(false);
                    isDWICellEditBox.setEnabled(false);
                    loadTable.setBorder(highlightTitledBorder("Upload B-Value/Gradient File  or B-Matrix File"));
                    
                }
            }
            
        }else if (command.equals("DWICellEditSwitch")){
            //Makes bval/grad table editable to the user. The user can manually correct values in each box of the table
                final boolean en = isDWICellEditBox.isSelected();
                    if (en == true){
                    srcBvalGradTable.setBackground(Color.white);    
                    srcBvalGradTable.setEnabled(true);
                    clearDWITableButton.setEnabled(true);
                    if(srcTableModel.getColumnCount()==5){ 
                        negXCheckBox.setEnabled(true);
                        negYCheckBox.setEnabled(true);
                        negZCheckBox.setEnabled(true); 
                    }
                    }   
                 
                    else {
                        //Color c = new Color(10,10,10,10);
                        srcBvalGradTable.setBackground(Color.lightGray);
                        srcBvalGradTable.setEnabled(false);
                        clearDWITableButton.setEnabled(false);
                        negXCheckBox.setEnabled(false);
                        negYCheckBox.setEnabled(false);
                        negZCheckBox.setEnabled(false);

                    }

         

            
            } else if (command.equals("DWITableDeleteButton")){
                //Entire table can be cleared with this command
                       srcBvalGradTable.setBackground(Color.white);
                       srcBvalGradTable.setEnabled(true);
                       if(srcTableModel.getColumnCount()==5){ 
                        for (int i = 0; i < numVolumes; i++) {
                            // Add empty rows based on number of volumes
                            srcTableModel.setValueAt("",i, 1);
                            srcTableModel.setValueAt("",i, 2);
                            srcTableModel.setValueAt("",i, 3);
                            srcTableModel.setValueAt("",i, 4);                
                            }
                       }
                       else if(srcTableModel.getColumnCount()==7){
                           for (int i = 0; i < numVolumes; i++) {
                               // Add empty rows based on number of volumes
                               srcTableModel.setValueAt("",i, 1);
                               srcTableModel.setValueAt("",i, 2);
                               srcTableModel.setValueAt("",i, 3);
                               srcTableModel.setValueAt("",i, 4);
                               srcTableModel.setValueAt("",i, 5); 
                               srcTableModel.setValueAt("",i, 6); 
                               }
                          } 
                       

                        

            } else if (command.equals("saveBvalGrad")) {
                //User can save bval/grad table to FSL, dtiStudio, or MIPAV Standard Format
                saveGradchooser = new JFileChooser(ViewUserInterface.getReference().getDefaultDirectory());
                saveGradchooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                saveGradchooser.setDialogTitle("Save B-Value/ Gradient Table to TXT File");
                saveGradchooser.setAccessory(buildSaveGradBvalPanel());
                if (isBmatFile ==true){
                    dtiStudioButton.setEnabled(false);
                }
                
                int returnValue = saveGradchooser.showSaveDialog(this);
                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    filebvalGradTxtName = saveGradchooser.getSelectedFile().getPath();
                   if (fslButton.isSelected()){
                      gradBvalText = 1;
                      createBVGradBMatFileTXT();
                   }
                   else if (dtiStudioButton.isSelected()){
                       gradBvalText = 2;
                       createBVGradBMatFileTXT();                  
                   }               
                   else if (mipavStandardButton.isSelected()){
                       gradBvalText = 3;
                       createBVGradBMatFileTXT();                  
                   }  
                    
                }
                
            }
            else if (command.equals("JonesSwitch")) {
                DWIJonesKirbyDialog();
               
            }
            
            else if (command.equals("gradTable")) {
                gradientTableCreator();
            }
            
            else if (command.equals("NegX")){
                //Switches signs in the Gradient X direction
                srcBvalGradTable.setBackground(Color.white);
                srcBvalGradTable.setEnabled(true);          
                 for (int i = 0; i < numVolumes; i++) {
                     if (srcTableModel.getValueAt(i, 2) != ""){
                     // Add empty rows based on number of volumes
                     float negX = (Float.valueOf((String)srcTableModel.getValueAt(i,2))*-1)+0;
                     srcTableModel.setValueAt(String.valueOf(negX),i, 2);
                 }
                }
                
            }
            else if (command.equals("NegY")){
                //Switches signs in the Gradient Y direction
                srcBvalGradTable.setBackground(Color.white);
                srcBvalGradTable.setEnabled(true);
                 for (int i = 0; i < numVolumes; i++) {
                     if (srcTableModel.getValueAt(i, 3) != ""){
                     // Add empty rows based on number of volumes
                     float negX = (Float.valueOf((String)srcTableModel.getValueAt(i,3))*-1)+0;
                     srcTableModel.setValueAt(String.valueOf(negX),i, 3);
                     }
                 }
                
            }
            else if (command.equals("NegZ")){
                //Switches signs in the Gradient Z direction
                srcBvalGradTable.setBackground(Color.white);
                srcBvalGradTable.setEnabled(true);
                 for (int i = 0; i < numVolumes; i++) {
                     if (srcTableModel.getValueAt(i, 4) != ""){
                     // Add empty rows based on number of volumes
                     float negX = (Float.valueOf((String)srcTableModel.getValueAt(i,4))*-1)+0;
                     srcTableModel.setValueAt(String.valueOf(negX),i, 4);
                     }
                 }
            
            } else if (command.equals("openedImage")) {
                //Sets the DWI image to the current image opened by the user
                if (activeDWIButton.isSelected()){
                openDWIButton.setEnabled(false);
                textDWIDataimage.setEnabled(false);
                
                    if (ui.getActiveImageFrame() != null){
                        frame = ui.getActiveImageFrame();
                        m_kDWIImage = frame.getImageA();                           
                            if (m_kDWIImage != null && m_kDWIImage.is4DImage()==true){
                                getImageDTIParams(); 
                                DWIOpenPanel.setBorder(buildTitledBorder("Upload DWI Image"));
                                browseDWIButton.setEnabled(false);
                                activeDWIButton.setEnabled(false);
                                textDWIDataimage.setText(m_kDWIImage.getImageDirectory()+m_kDWIImage.getImageFileName());
                                textDWIDataimage.setEnabled(false);
                                openDWIButton.setEnabled(false);
                                t2OpenPanel.setBorder(highlightTitledBorder("Use Structural Image as Reference Space (optional)"));
                                t2FileLabel.setEnabled(true);
                                textT2image.setEnabled(true);
                                openT2Button.setEnabled(true);
                                useT2CheckBox.setEnabled(true);
                                pipeline.repaint();
                        }
                            else if (m_kDWIImage != null && m_kDWIImage.isDicomImage() ==true &&
                                    m_kDWIImage.is3DImage()==true){
                                checkSiemens3d();
                                if (checkSiemens ==true){
                                    getImageDTIParams(); 
                                    DWIOpenPanel.setBorder(buildTitledBorder("Upload DWI Image"));
                                    browseDWIButton.setEnabled(false);
                                    activeDWIButton.setEnabled(false);
                                    textDWIDataimage.setText(m_kDWIImage.getImageDirectory()+m_kDWIImage.getImageFileName());
                                    textDWIDataimage.setEnabled(false);
                                    openDWIButton.setEnabled(false);
                                    t2OpenPanel.setBorder(highlightTitledBorder("Use Structural Image as Reference Space (optional)"));
                                    t2FileLabel.setEnabled(true);
                                    textT2image.setEnabled(true);
                                    openT2Button.setEnabled(true);
                                    useT2CheckBox.setEnabled(true);
                                    pipeline.repaint();
                                }
                                else{
                                    m_kDWIImage = null;   
                                    openDWIButton.setEnabled(true);
                                    textDWIDataimage.setEnabled(true);
                                }
                            }
                            else{
                                m_kDWIImage = null;
                                MipavUtil.displayError("Please select a 4D DWI Image"); 
                                openedImageCheckBox.setSelected(false);
                                //dwiFileLabel.setForeground(Color.BLACK);
                                textDWIDataimage.setEnabled(true);
                                textDWIDataimage.setBackground(Color.WHITE);
                                openDWIButton.setEnabled(true); 
                                DWIOpenPanel.setBorder(highlightTitledBorder("Upload DWI Image"));
                                t2OpenPanel.setBorder(buildTitledBorder("Use Structural Image as Reference Space (optional)"));
                                t2FileLabel.setEnabled(false);
                                textT2image.setEnabled(false);
                                openT2Button.setEnabled(false);
                                useT2CheckBox.setEnabled(false);
                        }
                    }
                    
                    else{
                        MipavUtil.displayError("No DWI active image is selected");
                        textDWIDataimage.setEnabled(true);
                        textDWIDataimage.setBackground(Color.WHITE);
                        openDWIButton.setEnabled(true); 
                     }

                }
                else{
                    textDWIDataimage.setEnabled(true);
                    textDWIDataimage.setBackground(Color.WHITE);
                    openDWIButton.setEnabled(true); 
                    //pipeline.nextButton.setEnabled(false);
                }
                
                
            } else if (command.equals("browseDWIFile")) {
                try{
                    loadDWIFile();
                    if (m_kDWIImage != null && m_kDWIImage.isDicomImage() ==true &&
                            m_kDWIImage.is3DImage()==true && checkSiemens ==false){
                        m_kDWIImage = null;
                        textDWIDataimage.setText("");               
                    }
                    else{
                    DWIOpenPanel.setBorder(buildTitledBorder("Upload DWI Image"));
                    browseDWIButton.setEnabled(false);
                    activeDWIButton.setEnabled(false);
                    textDWIDataimage.setEnabled(false);
                    openDWIButton.setEnabled(false);
                    t2OpenPanel.setBorder(highlightTitledBorder("Use Structural Image as Reference Space (optional)"));
                    t2FileLabel.setEnabled(true);
                    textT2image.setEnabled(true);
                    openT2Button.setEnabled(true);
                    useT2CheckBox.setEnabled(true);
                    pipeline.repaint();
                    }
                }
                catch (Exception e){
                    MipavUtil.displayError("Error loading DWI File");
                }
                
            }else if (command.equals("SkipT2")) {
                //User has no structual image to upload
                if (useT2CheckBox.isSelected()){
                    t2FileLabel.setEnabled(false);
                    textT2image.setEnabled(false);
                    openT2Button.setEnabled(false);
                    useT2CheckBox.setEnabled(false);
                    bvalGradFileLabel.setEnabled(true);
                    loadBValGradFileButton.setEnabled(true);
                    t2OpenPanel.setBorder(buildTitledBorder("Use Structural Image as Reference Space (optional)"));
                    if (dtiparams != null || parDTIParams != null){ 
                        DWIButtonPanel.setBorder(highlightTitledBorder("Table Options"));
                        bvalGradAppButton.setEnabled(true);
                        saveBvalGradButton.setEnabled(true);
                        isDWICellEditBox.setEnabled(true);
                    }
                    else{
                        loadTable.setBorder(highlightTitledBorder("Upload B-Value/Gradient File  or B-Matrix File"));  
                    }
                }
                else{
                    t2FileLabel.setEnabled(true);
                    textT2image.setEnabled(true);
                    openT2Button.setEnabled(true);
                    useT2CheckBox.setEnabled(true);
                    bvalGradFileLabel.setEnabled(false);
                    loadBValGradFileButton.setEnabled(false);
                    t2OpenPanel.setBorder(highlightTitledBorder("Use Structural Image as Reference Space (optional)"));
                    loadTable.setBorder(buildTitledBorder("Upload B-Value/Gradient File  or B-Matrix File"));
                }
                
            }else if (command.equals("browseT2File")) {
                try{
                    loadT2File();
                }
                    catch (Exception e){
                        MipavUtil.displayError("Error loading Structural Image");
                    }
                    if(m_kT2Image != null && m_kT2Image.is3DImage() 
                            && m_kT2Image.getImageOrientation()== m_kDWIImage.getImageOrientation()){
                        textT2image.setText(openFile.getImagePath());
                        t2FileLabel.setEnabled(false);
                        textT2image.setEnabled(false);
                        openT2Button.setEnabled(false);
                        useT2CheckBox.setEnabled(false);
                        bvalGradFileLabel.setEnabled(true);
                        textBvalGradFile.setEnabled(true);
                        loadBValGradFileButton.setEnabled(true);
                        t2OpenPanel.setBorder(buildTitledBorder("Use Structural Image as Reference Space (optional)"));
                        if (dtiparams != null || parDTIParams != null){ 
                            DWIButtonPanel.setBorder(highlightTitledBorder("Table Options"));
                            saveBvalGradButton.setEnabled(true);
                            isDWICellEditBox.setEnabled(true);
                            bvalGradAppButton.setEnabled(true);
                        }
                        else{
                            loadTable.setBorder(highlightTitledBorder("Upload B-Value/Gradient or B-Matrix File  or B-Matrix File"));  
                        }
                        
                        pipeline.repaint();  
                    }
                    else{
                        String DWIorientation = "";
                        if (m_kDWIImage.getImageOrientation()== 0){
                            DWIorientation = "Axial";
                        }
                        else if (m_kDWIImage.getImageOrientation()== 1){
                            DWIorientation = "Sagittal";
                        }
                        else if(m_kDWIImage.getImageOrientation()== 2){
                            DWIorientation = "Coronal";
                        }
                        
                        MipavUtil.displayError("Please select 3D Structural Image with " + DWIorientation + " orientation" ); 
                    }

                

            } else if (command.equals("UseBMat")){
            java.lang.Object[] newColIdentifiers = {"Volume","bxx","bxy", "bxz", "byy", "byz", "bzz"};
            srcTableModel.setColumnIdentifiers(newColIdentifiers);
            bvalGradFileLabel = new JLabel("B-matrix File: ");
            
            }
            
        }
              
                      
        
        /**
         * init
         */

        public void init() {
            GridBagConstraints gbc = new GridBagConstraints();
            GridBagConstraints gbc2 = new GridBagConstraints();
            


            gbc2 = new GridBagConstraints();

            //Create table that will display gradients and bvalues
            srcPanel = new JPanel(new GridBagLayout());
            srcTableModel = new DefaultTableModel(); 
            

            srcTableModel.addColumn("Volume");
            srcTableModel.addColumn("B-Value");
            srcTableModel.addColumn("X Gradient");
            srcTableModel.addColumn("Y Gradient");
            srcTableModel.addColumn("Z Gradient");

            srcBvalGradTable = new JTable(srcTableModel) {

                public String getToolTipText(final MouseEvent e) {
                    String tip = null;
                    final java.awt.Point p = e.getPoint();
                    final int rowIndex = rowAtPoint(p);
                    final int columnIndex = columnAtPoint(p);
                    if (columnIndex == 0) {
                        final String inputField = (String) srcTableModel.getValueAt(rowIndex, 0);
                        tip = inputField;
                        return tip;

                    } else {
                        return null;
                    }

                }
            };

            srcBvalGradTable.setPreferredScrollableViewportSize(new Dimension(595, 200));
            srcBvalGradTable.getColumn("Volume").setMaxWidth(60);
            srcBvalGradTable.setBackground(Color.lightGray);
            srcBvalGradTable.setEnabled(false);

            DWIButtonPanel = new JPanel();
            DWIButtonPanel.setBorder(buildTitledBorder("Table Options "));

           
            saveBvalGradButton = new JButton("Save Table As");
            saveBvalGradButton.setEnabled(false);
            saveBvalGradButton.addActionListener(this);
            saveBvalGradButton.setActionCommand("saveBvalGrad");
            
            isDWICellEditBox = new JCheckBox("Edit Table", false);
            isDWICellEditBox.setEnabled(false);
            isDWICellEditBox.addActionListener(this);
            isDWICellEditBox.setActionCommand("DWICellEditSwitch");
            
            clearDWITableButton = new JButton("Clear");
            clearDWITableButton.setEnabled(false);
            clearDWITableButton.addActionListener(this);
            clearDWITableButton.setActionCommand("DWITableDeleteButton");
            
            negXCheckBox = new JCheckBox("+/- x");
            negXCheckBox.setEnabled(false);
            negXCheckBox.addActionListener(this);
            negXCheckBox.setActionCommand("NegX");

            negYCheckBox = new JCheckBox("+/- y");
            negYCheckBox.setEnabled(false);
            negYCheckBox.addActionListener(this);
            negYCheckBox.setActionCommand("NegY");

            negZCheckBox = new JCheckBox("+/- z");
            negZCheckBox.setEnabled(false);
            negZCheckBox.addActionListener(this);
            negZCheckBox.setActionCommand("NegZ");


            DWIButtonPanel.add(isDWICellEditBox);          
            DWIButtonPanel.add(clearDWITableButton);
            DWIButtonPanel.add(negXCheckBox );
            DWIButtonPanel.add(negYCheckBox );
            DWIButtonPanel.add(negZCheckBox );
            DWIButtonPanel.add(saveBvalGradButton);
            


            gbc2.gridx = 0;
            gbc2.gridy = 3;
            gbc2.anchor = GridBagConstraints.NORTHWEST;
            gbc2.weightx = .75;
            gbc2.weighty = 1;
            gbc2.gridwidth = 1;
            gbc2.fill = GridBagConstraints.BOTH;
            final JScrollPane srcImagesScrollPane = new JScrollPane(srcBvalGradTable);
            srcPanel.add(srcImagesScrollPane, gbc2);
            gbc2.gridx = 0;
            gbc2.gridy = 4;
            gbc2.weightx = 1;
            gbc2.weighty = 0;
            gbc2.gridwidth = 2;
            gbc2.fill = GridBagConstraints.BOTH;

            srcPanel.add(DWIButtonPanel, gbc2);
            
            final JPanel BvalGradApply = new JPanel();
            //BvalGradApply.setBorder(buildTitledBorder("Application"));
            
            bvalGradAppButton = new JButton("Apply Table");
            //openDWIButton.setToolTipText("Browse dwi dataset image file");
            bvalGradAppButton.addActionListener(this);
            bvalGradAppButton.setActionCommand("applyTable");
            bvalGradAppButton.setEnabled(false);
            gbc.gridx = 0;
            gbc.gridy = 1;
            //gbc.weightx = 1;
            gbc.insets = new Insets(0, 0, 10, 0);
            gbc.fill = GridBagConstraints.NORTHWEST;
            BvalGradApply.add(bvalGradAppButton);
                        
            gbc2.gridx = 0;
            gbc2.gridy = 5;
            gbc2.weightx = 1;
            gbc2.weighty = 0;
            gbc2.gridwidth = 2;
            gbc2.fill = GridBagConstraints.BOTH;
            srcPanel.add(BvalGradApply, gbc2);
            
            
            DWIOpenPanel = new JPanel(new GridBagLayout());
            //DWIOpenPanel.setBorder(buildTitledBorder("Input Options " +"(load DWI dataset, optional: load reference structural image for EPI distortion correction)"));
            DWIOpenPanel.setBorder(highlightTitledBorder("Upload DWI Image"));
            browseDWIButton = new JRadioButton("DWI Image Browse");
            browseDWIButton.setSelected(true); 
            browseDWIButton.setFont(serif12);
            activeDWIButton = new JRadioButton("Use Active DWI image");
            activeDWIButton.setFont(serif12);
            activeDWIButton.addActionListener(this);
            activeDWIButton.setActionCommand("openedImage");
                      
            ButtonGroup group = new ButtonGroup();
            group.add(browseDWIButton);
            group.add(activeDWIButton);

            
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.weightx = 1;
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.insets = new Insets(0, 2, 0, 2);
            //DWIOpenPanel.add(openedImageCheckBox,gbc);
            DWIOpenPanel.add(browseDWIButton,gbc);
           
            gbc.gridx = 2;
            gbc.gridy = 1;
            gbc.weightx = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            //DWIOpenPanel.add(dwiFileLabel,gbc);
            DWIOpenPanel.add(activeDWIButton,gbc);
                                   
            textDWIDataimage = new JTextField();
            textDWIDataimage.setPreferredSize(new Dimension(75, 21));
            textDWIDataimage.setEditable(true);
            textDWIDataimage.setBackground(Color.white);
            textDWIDataimage.setFont(MipavUtil.font12);
            gbc.gridx = 0;
            gbc.gridy = 2;
            gbc.weightx = 0.25;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            DWIOpenPanel.add(textDWIDataimage,gbc);
                        
            openDWIButton = new JButton("Browse");
            openDWIButton.setPreferredSize(new Dimension(100, 21));
            openDWIButton.addActionListener(this);
            openDWIButton.setActionCommand("browseDWIFile");
            openDWIButton.setEnabled(true);
            gbc.gridx = 1;
            gbc.gridy = 2;
            gbc.weightx = 0.25;
            gbc.fill = GridBagConstraints.NONE;
            DWIOpenPanel.add(openDWIButton,gbc);
            
           
            gbc2.gridx = 0;
            gbc2.gridy = 0;
            gbc2.weightx = .5;
            gbc2.weighty = 0;  
            gbc2.gridwidth = 1;
            gbc2.fill = GridBagConstraints.BOTH;
            srcPanel.add(DWIOpenPanel, gbc2);



            
            t2OpenPanel = new JPanel(new GridBagLayout());
            t2OpenPanel.setBorder(buildTitledBorder("Use Structural Image as Reference Space (optional)"));            
            t2FileLabel = new JLabel("Structural Image(ex: T2 Image): ");
            t2FileLabel.setFont(serif12);
            t2FileLabel.setEnabled(false);
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.weightx = 1;
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.insets = new Insets(0, 2, 0, 2);
            t2OpenPanel.add(t2FileLabel,gbc);
            
            textT2image = new JTextField();
            textT2image.setPreferredSize(new Dimension(100, 21));
            textT2image.setEnabled(false);
            textT2image.setBackground(Color.white);
            textT2image.setFont(MipavUtil.font12);
            gbc.gridx = 1;
            gbc.gridy = 1;
            gbc.weightx = 0.15;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            t2OpenPanel.add(textT2image,gbc);
            
            
            openT2Button = new JButton("Browse");
            openT2Button.addActionListener(this);
            openT2Button.setActionCommand("browseT2File");
            openT2Button.setEnabled(false);
            gbc.gridx = 2;
            gbc.gridy = 1;
            gbc.weightx = 0.25;
            gbc.fill = GridBagConstraints.NONE;
            t2OpenPanel.add(openT2Button,gbc);
            
            useT2CheckBox = new JCheckBox("Skip");
            useT2CheckBox.setActionCommand("SkipT2");
            useT2CheckBox.setSelected(false);
            useT2CheckBox.setEnabled(false);
            useT2CheckBox.setFont(serif12);
            useT2CheckBox.addActionListener(this);
            gbc.gridx = 3;
            gbc.gridy = 1;
            gbc.weightx = 0.25;
            gbc.fill = GridBagConstraints.NONE;
            t2OpenPanel.add(useT2CheckBox,gbc);
            
            
            gbc2.gridx = 0;
            gbc2.gridy = 1;
            gbc2.weightx = .5;
            gbc2.weighty = 0;  
            gbc2.gridwidth = 1;
            gbc2.fill = GridBagConstraints.BOTH;
            srcPanel.add(t2OpenPanel, gbc2);
            
            loadTable = new JPanel(new GridBagLayout());
            loadTable.setBorder(buildTitledBorder("Upload B-Value/Gradient File  or B-Matrix File"));
            
            /*useBMatCheckBox = new JCheckBox("Use BMatrix File");
            useBMatCheckBox.setActionCommand("UseBMat");
            useBMatCheckBox.setSelected(false);
            useBMatCheckBox.setEnabled(true);
            useBMatCheckBox.setFont(serif12);
            useBMatCheckBox.addActionListener(this);
            gbc.gridx = 3;
            gbc.gridy = 1;
            gbc.weightx = 0.25;
            gbc.fill = GridBagConstraints.NONE;
            loadTable.add(useBMatCheckBox,gbc);*/
            
            bvalGradFileLabel = new JLabel("Bvalue/Gradient File or B-Matrix File: ");
            bvalGradFileLabel.setFont(serif12);
            bvalGradFileLabel.setEnabled(false);
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.weightx = 1;
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.insets = new Insets(0, 2, 0, 2);
            loadTable.add(bvalGradFileLabel,gbc);
            
            textBvalGradFile = new JTextField();
            textBvalGradFile.setPreferredSize(new Dimension(100, 21));
            textBvalGradFile.setEnabled(false);
            textBvalGradFile.setBackground(Color.white);
            textBvalGradFile.setFont(MipavUtil.font12);
            gbc.gridx = 1;
            gbc.gridy = 1;
            gbc.weightx = 0.25;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            loadTable.add(textBvalGradFile,gbc);
            
            loadBValGradFileButton = new JButton("Browse");
            loadBValGradFileButton.addActionListener(this);
            loadBValGradFileButton.setEnabled(false);
            loadBValGradFileButton.setActionCommand("bvalGradBrowse");
            gbc.gridx = 2;
            gbc.gridy = 1;
            gbc.weightx = 0.25;
            gbc.fill = GridBagConstraints.NONE;
            loadTable.add(loadBValGradFileButton,gbc);
            

            
            
            gbc2.gridx = 0;
            gbc2.gridy = 2;
            gbc2.weightx = 1;
            gbc2.weighty = 0;
            gbc2.gridwidth = 1;
            gbc2.fill = GridBagConstraints.BOTH;
            srcPanel.add(loadTable, gbc2);
            

            

            scrollPane = new JScrollPane(srcPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            
         
            //this.add(scrollPane, BorderLayout.WEST);
        } 
   
        private void getImageDTIParams() { 
            

           dtiparams = m_kDWIImage.getDTIParameters();
            
        
        //Set DTI Param object to get bvalues and gradients for display in srcTableModel
            
            boolean isPARREC = false;
            FileInfoBase fileInfo = m_kDWIImage.getFileInfo(0);
            try {
                fileInfoPARREC = (FileInfoPARREC) fileInfo;
                isPARREC = true;
            } catch (ClassCastException e) {
                isPARREC = false;
            }
            
            boolean isNIFTI = false;
            FileInfoNIFTI fileInfoNIFTI = null;
            try {
                fileInfoNIFTI = (FileInfoNIFTI) fileInfo;
                isNIFTI = true;
            } catch (ClassCastException e) {
                isNIFTI = false;
            }
            
            if (isNIFTI){
                String fileDir = m_kDWIImage.getImageDirectory();
                String fileName = m_kDWIImage.getImageFileName();
                File filePar = new File(fileDir + (fileName.substring(0, fileName.indexOf(".")) + "." + "par"));
                boolean exists = filePar.exists();
                
                if (exists){
                    try {
                        niftiParExtraction(filePar);
                        
                    } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }
            
            if (m_kDWIImage.isDicomImage()){
                philipsDicomGradExtract();                
            }
            
            if (dtiparams != null){
                //Checks DTI Parameters object to determine if opened image has bvals and grads stored in it
                m_kDWIImage.setDTIParameters(dtiparams);
                
                if (dtiparams.getNumVolumes() != 0){
                numVolumes = m_kDWIImage.getExtents()[3];
                
                    for (int i = 0; i < numVolumes; i++) {
                        // Add empty rows based on number of volumes
                        final Vector<Object> rowData = new Vector<Object>();
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        srcTableModel.addRow(rowData);
                    }
                
                    if (dtiparams.getbValues() != null){
                        for (int i = 0; i < numVolumes; i++) { 
                            // Populate Volume column
                            srcTableModel.setValueAt(String.valueOf(i),i,0);
                            // Populate Bvalue column
                            double[] flBvalArr = dtiparams.getbValues();
                            srcTableModel.setValueAt(String.valueOf(flBvalArr[i]),i,1);
                     }
                    }
                    if (dtiparams.getGradients() != null){ 
                        for (int i = 0; i < numVolumes; i++) {
                             // Populate Gradient column
                             double[][] flGradArr = dtiparams.getGradients();
                             srcTableModel.setValueAt(String.valueOf(flGradArr[i][0]), i, 2);
                             srcTableModel.setValueAt(String.valueOf(flGradArr[i][1]), i, 3);
                             srcTableModel.setValueAt(String.valueOf(flGradArr[i][2]), i, 4);
                            }
                         }
                    
                    if (dtiparams.getbMatrixVals() != null){ 
                        isBmatFile = true;
                        java.lang.Object[] newColIdentifiers = {"Volume","bxx","bxy", "bxz", "byy", "byz", "bzz"};
                        srcTableModel.setColumnIdentifiers(newColIdentifiers);
                        for (int i = 0; i < numVolumes; i++) {
                            // Populate Gradient column
                            double[][] flGradArr = dtiparams.getbMatrixVals();
                            srcTableModel.setValueAt(String.valueOf(flGradArr[i][0]), i, 1);
                            srcTableModel.setValueAt(String.valueOf(flGradArr[i][1]), i, 2);
                            srcTableModel.setValueAt(String.valueOf(flGradArr[i][2]), i, 3);
                            srcTableModel.setValueAt(String.valueOf(flGradArr[i][3]), i, 4);
                            srcTableModel.setValueAt(String.valueOf(flGradArr[i][4]), i, 5);
                            srcTableModel.setValueAt(String.valueOf(flGradArr[i][5]), i, 6);
                           }
                        }

                
                }
            }
            
            else if (parDTIParams !=null){
                if (parDTIParams.getNumVolumes() != 0){
                    numVolumes = m_kDWIImage.getExtents()[3];
                    
                        for (int i = 0; i < numVolumes; i++) {
                            // Add empty rows based on number of volumes
                            final Vector<Object> rowData = new Vector<Object>();
                            rowData.add("");
                            rowData.add("");
                            rowData.add("");
                            rowData.add("");
                            rowData.add("");
                            srcTableModel.addRow(rowData);
                        }
                    
                        if (parDTIParams.getbValues() != null){
                            for (int i = 0; i < numVolumes; i++) { 
                                // Populate Volume column
                                srcTableModel.setValueAt(String.valueOf(i),i,0);
                                // Populate Bvalue column
                                double[] flBvalArr = parDTIParams.getbValues();
                                srcTableModel.setValueAt(String.valueOf(flBvalArr[i]),i,1);
                         }
                        }
                        if (parDTIParams.getGradients() != null){ 
                            for (int i = 0; i < numVolumes; i++) {
                                 // Populate Gradient column
                                 double[][] flGradArr = parDTIParams.getGradients();
                                 srcTableModel.setValueAt(String.valueOf(flGradArr[i][0]), i, 2);
                                 srcTableModel.setValueAt(String.valueOf(flGradArr[i][1]), i, 3);
                                 srcTableModel.setValueAt(String.valueOf(flGradArr[i][2]), i, 4);
                                }
                             }
                
            }
            }
            
            

            if (isPARREC || isNIFTI || m_kDWIImage.isDicomImage()) {
                //Checks if image is Philips PAR/REC image and the version to determine which GTC parameters to include
                //Parameters based on: http://jist.projects.nitrc.org/docs/IACL/DTI/MedicAlgorithmMultiGradientTableCreator.html 
                if ( fileInfoPARREC != null && (fileInfoPARREC.getExamName().toUpperCase()).contains("DTI")
                        || fileInfoPARREC != null && (fileInfoPARREC.getProtocolName().toUpperCase()).contains("DTI") || parNversion != null && 
                        parNProtocolName.toUpperCase().contains("DTI")) {

    
                    if (fileInfoPARREC != null  ||  parNversion != null ) {
                        //Determine if Philips PAR/REC is version 3 or 4 to determine which gradient table dialog to be displayed
                        final JPanel GradCreatorPanel = new JPanel(new GridBagLayout());
                        final GridBagConstraints gbc = new GridBagConstraints();
                        final GridBagConstraints gbc2 = new GridBagConstraints();
                        gbc.insets = new Insets(5, 1, 1, 5);
                        gbc.fill = GridBagConstraints.BOTH;
                        
                        //Add all parameters not aquired in PAR file for user to input
                        GradCreatorPanel.setBorder(buildTitledBorder("Philips Gradient Creator Input Parameters"));
                        fatShiftLabel = new JLabel("Fatshift");
                        fatshiftBox = new JComboBox();
                        fatshiftBox.setBackground(Color.white);
                        fatshiftBox.addItem("RL"); //Right
                        fatshiftBox.addItem("LR"); //Left
                        fatshiftBox.addItem("AP"); //Anterior
                        fatshiftBox.addItem("PA"); //Posterior
                        fatshiftBox.addItem("HI"); //Head/Superior
                        fatshiftBox.addItem("FS"); //Feet/Inferior
                        gbc.gridy = 0;
                        gbc.gridx = 0;
                        GradCreatorPanel.add(fatShiftLabel,gbc);
                        gbc.gridy = 0;
                        gbc.gridx = 1;
                        GradCreatorPanel.add(fatshiftBox,gbc);
                        
                        isJonesBox = new JCheckBox("Jones30", false);
                        isJonesBox.setForeground(Color.BLACK);
                        isJonesBox.addActionListener(this);
                        isJonesBox.setActionCommand("JonesSwitch");                            
                        gbc.gridy = 1;
                        gbc.gridx = 0;
                        GradCreatorPanel.add(isJonesBox,gbc);
                        
                        isKirbyBox = new JCheckBox("Kirby", false);
                        isKirbyBox.setForeground(Color.lightGray);                         
                        gbc.gridy = 1;
                        gbc.gridx = 1;
                        GradCreatorPanel.add(isKirbyBox,gbc);
                        
    
                        gradResLabel = new JLabel("Gradient Resolution");
                        gradResBox = new JComboBox();
                        gradResBox.setBackground(Color.white);
                        gradResBox.addItem("Low");//Volumes = 8
                        gradResBox.addItem("Medium");//Volumes = 17
                        gradResBox.addItem("High"); //Volumes = 34
                        gbc.gridy = 2;
                        gbc.gridx = 0;
                        GradCreatorPanel.add(gradResLabel,gbc);
                        gbc.gridy = 2;
                        gbc.gridx = 1;
                        GradCreatorPanel.add(gradResBox,gbc);
    
                        gradOPLabel = new JLabel("Gradient Overplus");
                        gradOPBox = new JComboBox();
                        gradOPBox.setBackground(Color.white);
                        gradOPBox.addItem("No");
                        gradOPBox.addItem("Yes");
                        gbc.gridy = 3;
                        gbc.gridx = 0;
                        GradCreatorPanel.add(gradOPLabel,gbc);
                        gbc.gridy = 3;
                        gbc.gridx = 1;
                        GradCreatorPanel.add(gradOPBox,gbc);
                        
                        philRelLabel = new JLabel("Philips Release");
                        philRelBox = new JComboBox();
                        philRelBox.setBackground(Color.white);
                        philRelBox.addItem("Rel_1.5");
                        philRelBox.addItem("Rel_1.7");
                        philRelBox.addItem("Rel_2.0");
                        philRelBox.addItem("Rel_2.1");
                        philRelBox.addItem("Rel_2.5");
                        philRelBox.addItem("Rel_11.x");
                        gbc.gridy = 4;
                        gbc.gridx = 0;
                        GradCreatorPanel.add(philRelLabel,gbc);
                        gbc.gridy = 4;
                        gbc.gridx = 1;
                        GradCreatorPanel.add(philRelBox,gbc);
                        
                        if (fileInfoPARREC != null  && fileInfoPARREC.getVersion().equals("V4.1") ||
                                fileInfoPARREC != null && fileInfoPARREC.getVersion().equals("V4.2") 
                                || parNversion != null && parNversion.equals("V4.1") || parNversion != null && parNversion.equals("V4.2") ){
                          //Add all parameters not aquired in PAR file for user to input
                            osLabel = new JLabel("OS"); //Operating System
                            osLabel.setForeground(Color.lightGray);
                            osBox = new JComboBox();
                            osBox.setForeground(Color.lightGray);
                            osBox.addItem("Windows");
                            osBox.addItem("VMS");
                            gbc.gridy = 5;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(osLabel,gbc);
                            gbc.gridy = 5;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(osBox,gbc);
                            
                            invertedLabel = new JLabel("Inverted");
                            invertedLabel.setForeground(Color.lightGray);
                            invertedBox = new JComboBox();
                            invertedBox.setForeground(Color.lightGray);
                            invertedBox.addItem("No");
                            invertedBox.addItem("Yes");
                            gbc.gridy = 6;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(invertedLabel,gbc);
                            gbc.gridy = 6;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(invertedBox,gbc);
                            
                            final JButton applyGradOptions = new JButton("Compute Gradient Table");
                            applyGradOptions.addActionListener(this);
                            applyGradOptions.setActionCommand("gradTable");
                            gbc.gridy = 7;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(applyGradOptions,gbc);
                             
                            gbc2.gridy = 3;
                            gbc2.gridx = 1;
                            gbc2.gridwidth = 1;
                            gbc2.weightx = .25;
                            gbc2.weighty = 1;
                            srcPanel.add(GradCreatorPanel, gbc2);
                        }
                        
                        else if (fileInfoPARREC != null && fileInfoPARREC.getVersion().equals("V3")||
                                fileInfoPARREC != null && fileInfoPARREC.getVersion().equals("V4") || 
                               parNversion != null && parNversion.equals("V3") || parNversion != null && parNversion.equals("V4")){
                            //Add all parameters not aquired in PAR file for user to input
                            patientPosLabel = new JLabel("Patient Position");
                            patientPosBox = new JComboBox();
                            patientPosBox.setBackground(Color.white);
                            patientPosBox.addItem("Head First");
                            patientPosBox.addItem("Feet First");
                            gbc.gridy = 5;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(patientPosLabel,gbc);
                            gbc.gridy = 5;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(patientPosBox,gbc);
                            
                            patientOrientLabel = new JLabel("Patient Orientation");
                            patientOrientBox = new JComboBox();
                            patientOrientBox.setBackground(Color.white);
                            patientOrientBox.addItem("SP"); //Supine
                            patientOrientBox.addItem("PR"); //Prone
                            patientOrientBox.addItem("RD"); //Right Decubitus
                            patientOrientBox.addItem("LD"); //Left Decubitus
                            gbc.gridy = 6;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(patientOrientLabel,gbc);
                            gbc.gridy = 6;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(patientOrientBox,gbc);
                            
                            foldOverLabel = new JLabel("Fold Over");
                            foldOverBox = new JComboBox();
                            foldOverBox.setBackground(Color.white);
                            foldOverBox.addItem("AP"); //Anterior-Posterior
                            foldOverBox.addItem("RL"); //Right-Left
                            foldOverBox.addItem("FH"); //Head-Feet/Superior-Inferior
                            gbc.gridy = 7;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(foldOverLabel,gbc);
                            gbc.gridy = 7;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(foldOverBox,gbc);
                                                    
                            osLabel = new JLabel("OS"); //Operating System
                            osLabel.setForeground(Color.lightGray);
                            osBox = new JComboBox();
                            osBox.setForeground(Color.lightGray);
                            osBox.addItem("Windows");
                            osBox.addItem("VMS");
                            gbc.gridy = 8;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(osLabel,gbc);
                            gbc.gridy = 8;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(osBox,gbc);
                            
                            invertedLabel = new JLabel("Inverted");
                            invertedLabel.setForeground(Color.lightGray);
                            invertedBox = new JComboBox();
                            invertedBox.setForeground(Color.lightGray);
                            invertedBox.addItem("No");
                            invertedBox.addItem("Yes");
                            gbc.gridy = 9;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(invertedLabel,gbc);
                            gbc.gridy = 9;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(invertedBox,gbc);
                            
                            final JButton applyGradOptions = new JButton("Compute Gradient Table");
                            applyGradOptions.addActionListener(this);
                            applyGradOptions.setActionCommand("gradTable");
                            gbc.gridy = 10;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(applyGradOptions,gbc);
                             
                            gbc2.gridy = 3;
                            gbc2.gridx = 1;
                            gbc2.gridwidth = 1;
                            gbc2.weightx = .25;
                            gbc2.weighty = 1;
                            srcPanel.add(GradCreatorPanel, gbc2);
                            
                        }                           
                    }
                }
            }
            }
            
    
        
        private TitledBorder buildTitledBorder(String title) {
            return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                                    Color.black);
        }
        
        private TitledBorder highlightTitledBorder(String title){
            return new TitledBorder(new LineBorder( Color.black, 2), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                    Color.black);
        }

       
        private JPanel buildSaveGradBvalPanel() {
        
        //Determine which TXT format to save bvalues and gradients in    
        final JPanel saveBvalGradPanel = new JPanel();
        saveBvalGradPanel.setBorder(buildTitledBorder("Format Options"));

        fslButton = new JRadioButton("FSL");
        fslButton.setSelected(true);
           
        dtiStudioButton = new JRadioButton("DTI Studio");
        
        mipavStandardButton = new JRadioButton("Standard MIPAV Format");
        
       ButtonGroup group = new ButtonGroup();
       group.add(fslButton);
       group.add(dtiStudioButton);
       group.add(mipavStandardButton);
       
       saveBvalGradPanel.setLayout(new GridLayout(3, 1));
       saveBvalGradPanel.add(fslButton);
       saveBvalGradPanel.add(dtiStudioButton);
       saveBvalGradPanel.add(mipavStandardButton);
         
       return saveBvalGradPanel;
              

    }
        private void DWIJonesKirbyDialog(){
            //Create color changes in GradientTableCreator Dialog is Jones check box is selected or unselected
            
            FileInfoBase fileInfo = m_kDWIImage.getFileInfo(0);
            FileInfoPARREC fileInfoPARREC = (FileInfoPARREC) fileInfo;
            if (isJonesBox.isSelected()){
                isKirbyBox.setSelected(true);
                isKirbyBox.setForeground(Color.BLACK);
                osLabel.setForeground(Color.BLACK);
                osBox.setForeground(Color.BLACK);
                invertedLabel.setForeground(Color.BLACK);
                invertedBox.setForeground(Color.BLACK);

                if (fileInfoPARREC != null && fileInfoPARREC.getVersion().equals("V3") ||  parNversion.equals("V3")) {
                    gradResLabel.setForeground(Color.lightGray);
                    gradResBox.setForeground(Color.lightGray);
                    gradOPLabel.setForeground(Color.lightGray);
                    gradOPBox.setForeground(Color.lightGray);
                    philRelLabel.setForeground(Color.lightGray);
                    philRelBox.setForeground(Color.lightGray);
                    patientPosLabel.setForeground(Color.lightGray);
                    patientPosBox.setForeground(Color.lightGray);
                    patientOrientLabel.setForeground(Color.lightGray);
                    patientOrientBox.setForeground(Color.lightGray);                     
                }
                
                else if (fileInfoPARREC != null && fileInfoPARREC.getVersion().equals("V4") ||  parNversion.equals("V4")){
                    gradResLabel.setForeground(Color.lightGray);
                    gradResBox.setForeground(Color.lightGray);
                    gradOPLabel.setForeground(Color.lightGray);
                    gradOPBox.setForeground(Color.lightGray);
                    philRelLabel.setForeground(Color.lightGray);
                    philRelBox.setForeground(Color.lightGray);              
                }
            }
            
            else {
                isKirbyBox.setForeground(Color.lightGray);
                osLabel.setForeground(Color.lightGray);
                osBox.setForeground(Color.lightGray);
                invertedLabel.setForeground(Color.lightGray);
                invertedBox.setForeground(Color.lightGray);
                if (fileInfoPARREC != null && fileInfoPARREC.getVersion().equals("V3") ||  parNversion.equals("V3")) {
                    gradResLabel.setForeground(Color.BLACK);
                    gradResBox.setForeground(Color.BLACK);
                    gradOPLabel.setForeground(Color.BLACK);
                    gradOPBox.setForeground(Color.BLACK);
                    philRelLabel.setForeground(Color.BLACK);
                    philRelBox.setForeground(Color.BLACK);
                    patientPosLabel.setForeground(Color.BLACK);
                    patientPosBox.setForeground(Color.BLACK);
                    patientOrientLabel.setForeground(Color.BLACK);
                    patientOrientBox.setForeground(Color.BLACK);                
                }
                
                else if (fileInfoPARREC != null && fileInfoPARREC.getVersion().equals("V4") ||  parNversion.equals("V4")){
                    gradResLabel.setForeground(Color.BLACK);
                    gradResBox.setForeground(Color.BLACK);
                    gradOPLabel.setForeground(Color.BLACK);
                    gradOPBox.setForeground(Color.BLACK);
                    philRelLabel.setForeground(Color.BLACK);
                    philRelBox.setForeground(Color.BLACK);               
                }
            }
            
        }
        private void gradientTableCreator() {
            //Get info from PAR file and user inputs from Gradient Table Creator Dialog
            FileInfoBase fileInfo = m_kDWIImage.getFileInfo(0);
            
            //FileInfoPARREC fileInfoPARREC = (FileInfoPARREC) fileInfo;

            fatshiftBox.getSelectedItem();
            gradResBox.getSelectedItem();
            gradOPBox.getSelectedItem();
            String philRel = (String) philRelBox.getSelectedItem(); 
            String os = (String) osBox.getSelectedItem(); 
            String inverted = (String) invertedBox.getSelectedItem(); 
            
            gradResWOP = ((String) gradOPBox.getSelectedItem()) + ((String) gradResBox.getSelectedItem());
            
           
            /**
             * From JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.DTIGradientTableCreator.java
             * @author John Bogovic
             */
                   
            if (inverted.equals("Yes")){
                invertedLabel.setForeground(Color.red);
                MipavUtil.displayError("Inverted must be NO");
            }
            
            if(isJonesBox.isSelected() && inverted.equals("No")){
                if(isKirbyBox.isSelected()){
                    if(numVolumes==32 && os.equals("Windows")){
                        gradCreatetable = getJones30();
                        space = "MPS";
                        angulationCorrection(gradCreatetable);   
                    }else if(numVolumes==35 && os.equals("VMS")){
                        gradCreatetable = getJones30VMS();
                        space = "MPS";
                        angulationCorrection(gradCreatetable);   
                    }else if(numVolumes==31 && os.equals("Windows")){
                        gradCreatetable  = getJones30();
                        space = "LPH";
                        angulationCorrection(gradCreatetable);   
                    }
                    else{
                        osLabel.setForeground(Color.red);
                        MipavUtil.displayError("Philips Gradient Creator"+"Image dimensions " + numVolumes + " or Operating System "+ os + " are not consistent with gradient resolution choice - expected 32,35, or 31 dimensions");
                    }
                }
                
                else{
                    MipavUtil.displayError("Philips Gradient Creator "+"Jones30 is valid only for the KIRBY scanners");
                }
            }
            
            else {
            if(gradResWOP.equals("YesLow")){
                if(numVolumes==8){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_1.5") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getLowOP();
                        space = "LPH";
                        angulationCorrection(gradCreatetable);   
                     }
                    else{
                        gradCreatetable = getLowOP2();
                        space = "XYZ";
                        angulationCorrection(gradCreatetable);   
                    }       
                }           
                else{
                    MipavUtil.displayError("Philips Gradient Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient table choice - expected 8 dimensions");
                }            
            }
            
            else if(gradResWOP.equals("YesMedium")){
                if(numVolumes==17){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getMediumOP();
                        space = "LPH";
                        angulationCorrection(gradCreatetable);   
                     }
                    else{
                        gradCreatetable = getMediumOP2();
                        space = "XYZ";
                        angulationCorrection(gradCreatetable);   
                    }            
                }           
                else{
                    MipavUtil.displayError("Philips Gradient Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient resolution choice - expected 17 dimensions");
                } 
            } 
            
            else if(gradResWOP.equals("YesHigh")){
                if(numVolumes==34){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0")){
                        gradCreatetable = getHighOP_24prev();
                        space = "LPH";
                        angulationCorrection(gradCreatetable);   
                     }
                    else if(philRel.equals("Rel_2.5")){
                        gradCreatetable = getHighOP_rel25();
                        space = "LPH";
                        angulationCorrection(gradCreatetable);   
                    }                
                    else{
                        gradCreatetable = getHighOP_25post();
                        space = "XYZ";
                        angulationCorrection(gradCreatetable);   
                    }           
                }
                else{
                    MipavUtil.displayError("Philips Gradient Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient resolution  choice - expected 35 dimensions");
                } 
            }
            else if(gradResWOP.equals("NoLow")){
                if(numVolumes==8){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getLow();
                        space = "MPS";
                        angulationCorrection(gradCreatetable);   
                     }           
                }
                else{
                    MipavUtil.displayError("Philips Gradient Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient resolution choice - expected 8 dimensions");
                } 
            }
            
            else if(gradResWOP.equals("NoMedium")){
                if(numVolumes==17){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getMedium();
                        space = "MPS";
                        angulationCorrection(gradCreatetable);   
                     }           
            }
                else{
                    MipavUtil.displayError("Philips Gradient Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient resolution choice - expected 17 dimensions");
                }              
            }
            
            else if(gradResWOP.equals("NoHigh")){
                if(numVolumes==34){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getHigh();
                        space = "MPS";
                        angulationCorrection(gradCreatetable);   
                     }          
                }
                else{
                    MipavUtil.displayError("Philips Gradient Creator"+"Image dimensions " + numVolumes + " are not consistent with gradient resolution choice - expected 34 dimensions");
                }
            }
            
            else{
                MipavUtil.displayError("Philips Gradient Creator "+"Could not determine a table!");
            }              
        }
      
      }
        
        public static final double[][] getLowOP(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{-0.9428,-0.4714, 0.9428},
                     {0.9428,-0.9428, 0.4714},
                        {-1.0000,-1.0000, 0.0000},
                         {0.0000,-1.0000, 1.0000},
                        {1.0000, 0.0000, 1.0000}};        
        }
        
        public static final double[][] getMediumOP(){ 
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{-0.7071,-0.7071,-1.0000},
            {-0.7071,-0.7071, 1.0000},
             {1.0000,-1.0000, 0.0000},
            {-0.1561,-0.9999,-0.9879},
             {0.4091,-0.9894,-0.9240},
             {0.8874,-0.4674,-0.9970},
             {0.9297,-0.3866,-0.9930},
            {-0.9511,-0.7667,-0.7124},
             {0.9954,-0.6945, 0.7259},
            {-0.9800,-0.3580, 0.9547},
            {-0.9992,-1.0000, 0.0392},
            {-0.3989,-0.9999, 0.9171},
             {0.4082,-0.9923, 0.9213},
             {0.9982,-0.9989, 0.0759},
             {0.9919,-0.2899, 0.9655}};
        }
        
        public static final double[][] getLowOP2(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{0.9428,-0.4714,0.9428},
             {0.4714,-0.9428,-0.9428},
             {0.9428,0.9428,-0.4714},
             {1.0,-1.0,0.0},
             {1.0,0.0,-1.0},
             {0.0,1.0,-1.0}};
        }
        
        public static final double[][] getMediumOP2(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{0.7071,-0.7071,1.0000},
            {0.7071,-0.7071,-1.0000},
            {1.0000,1.0000,0.0000},
            {0.9999,-0.1561,0.9879},
            {0.9894,0.4091,0.9240},
            {0.4674,0.8874,0.9970},
            {0.3866,0.9297,0.9930},
            {0.7667,-0.9511,0.7124},
            {0.6945,0.9954,-0.7259},
            {0.3580,-0.9800,-0.9547},
            {1.0000,-0.9992,-0.0392},
            {0.9999,-0.3989,-0.9171},
            {0.9923,0.4082,-0.9213},
            {0.9989,0.9982,-0.0759},
            {0.2899,0.9919,-0.9655}};
            
        }
        
        public static final double[][] getHighOP_24prev(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{-0.70710,-0.70710,-1.00000},
            {-0.70710,-0.70710, 1.00000},
            { 1.00000,-1.00000, 0.00000},
            {-0.92390,-0.38270,-1.00000},
            {-0.29510,-0.95550,-1.00000},
            { 0.02780,-0.99960,-1.00000},
             { 0.59570,-0.80320,-1.00000},
             { 0.97570,-0.21910,-1.00000},
            {-0.92420,-0.38280,-0.99970},
            {-0.41420,-1.00000,-0.91020},
            { 0.41650,-0.99900,-0.91020},
             { 0.72830,-0.68740,-0.99850},
             { 1.00000,-0.41420,-0.91020},
            {-1.00000,-0.66820,-0.74400},
            {-0.66820,-1.00000,-0.74400},
            { 0.78560,-0.91070,-0.74400},
             { 1.00000,-0.66820,-0.74400},
            {-1.00000,-1.00000,-0.00030},
            {-1.00000,-0.66820, 0.74400},
            { 1.00000,-0.66820, 0.74400},
             { 0.66820,-1.00000, 0.74400},
             { 1.00000,-0.66820, 0.74400},
            {-0.90000,-0.60130, 0.91020},
            {-0.99850,-0.99850, 0.07740},
            {-0.41420,-1.00000, 0.91020},
            { 0.41420,-1.00000, 0.91020},
             { 1.00000,-1.00000, 0.01110},
             { 1.00000,-0.41420, 0.91020},
            {-0.99880,-0.99880, 0.06920},
            { 0.04910,-0.99880, 1.00000},
             { 0.99990,-0.99990, 0.01630},
             {1.00000, 0.00000, 1.00000}};
        }
        
        public static final double[][] getHighOP_rel25(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{ 
                     {-0.70710,-0.70710,-1.00000},
                        {-0.70710,-0.70710, 1.00000},
                         {1.00000,-1.00000, 0.00000},
                        {-0.92390,-0.38270,-1.00000},
                        {-0.29510,-0.95550,-1.00000},
                         {0.02780,-0.99960,-1.00000},
                         {0.59570,-0.80320,-1.00000},
                         {0.97570,-0.21910,-1.00000},
                        {-0.92420,-0.38280,-0.99970},
                        {-0.41420,-1.00000,-0.91020},
                         {0.41650,-0.99900,-0.91020},
                         {0.72830,-0.68740,-0.99850},
                         {1.00000,-0.41420,-0.91020},
                        {-1.00000,-0.66820,-0.74400},
                        {-0.66820,-1.00000,-0.74400},
                         {0.78560,-0.91070,-0.74400},
                         {1.00000,-0.66820,-0.74400},
                        {-1.00000,-1.00000,-0.00030},
                        {-1.00000,-0.66820, 0.74400},
                         {1.00000,-0.66820, 0.74400},
                         {0.66820,-1.00000, 0.74400},
                        {-1.00000,-1.00000, 0.01110},
                        {-0.90000,-0.60130, 0.91020},
                        {-0.99850,-0.99850, 0.07740},
                        {-0.41420,-1.00000, 0.91020},
                         {0.41420,-1.00000, 0.91020},
                         {1.00000,-1.00000, 0.01110},
                         {1.00000,-0.41420, 0.91020},
                        {-0.99880,-0.99880, 0.06920},
                         {0.04910,-0.99880, 1.00000},
                         {0.99990,-0.99990, 0.01630},
                         {1.00000, 0.00000, 1.00000}};
        }
        
        public static final double[][] getHighOP_25post(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{0.3827,-0.9239,1.0000},
              {0.9555,-0.2951,1.0000},
              {0.9996,0.0278,1.0000},
              {0.8032,0.5957,1.0000},
              {0.2191,0.9757,1.0000},
              {0.3828,-0.9242,0.9997},
              {0.7071,-0.7071,1.0000},
              {1.0000,-0.4142,0.9102},
              {0.9990,0.4165,0.9102},
              {0.6874,0.7283,0.9985},
              {0.4142,1.0000,0.9102},
              {0.6682,-1.0000,0.7440},
              {1.0000,-0.6682,0.7440},
              {0.9107,0.7856,0.7440},
              {0.6682,1.0000,0.7440},
              {1.0000,-1.0000,0.0003},
              {1.0000,1.0000,0.0000},
              {0.6682,-1.0000,-0.7440},
              { 0.6682,1.0000,-0.7440},
              { 1.0000,0.6682,-0.7440},
              { 0.6682,1.0000,-0.7440},
              { 0.6013,-0.9000,-0.9102},
              { 0.9985,-0.9985,-0.0774},
              {1.0000,-0.4142,-0.9102},
              {  1.0000,0.4142,-0.9102},
              { 1.0000,1.0000,-0.0111},
              { 0.4142,1.0000,-0.9102},
              { 0.5624,-0.8269,-1.0000},
              { 0.9988,-0.9988,-0.0692},
              { 0.9988,0.0491,-1.0000},
              { 0.9999,0.9999,-0.0163},
              {0.0000,1.0000,-1.0000}   
             };
        }
            
        public static final double[][] getLow(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{
                     {1.0,0.0,0.0},
                     { 0.0,1.0,0.0},
                     { 0.0,0.0,1.0},
                    {-0.7044,-0.0881,-0.7044},
                     {0.7044,0.7044,0.0881},
                     {0.0881,0.7044,0.7044}};
        }
        
        public static final double[][] getMedium(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{
                    {1.0,0.0,0.0},
                    { 0.0,1.0,0.0},
                    { 0.0,0.0,1.0},
                    { -0.1789,-0.1113,-0.9776},
                   { -0.0635,0.3767,-0.9242},
                   {  0.710,0.0516,-0.7015},
                    {  0.6191,-0.4385,-0.6515},  
                    {  0.2424,0.7843,-0.5710},
                    { -0.2589,-0.6180,-0.7423},
                   { -0.8169,0.1697,-0.5513},
                   { -0.8438,0.5261,-0.1060},
                   { -0.2626,0.9548,-0.1389},
                   { 0.0001,0.9689,0.2476},
                    { 0.7453,0.6663,0.0242},
                    {0.9726,0.2317,0.0209}};
        }
        
        public static final double[][] getHigh(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{
                    {1.0,0.0,0.0},
                    { 0.0,1.0,0.0},
                    { 0.0,0.0,1.0},
                    { -0.0424,-0.1146,-0.9925},
                   {  0.1749,-0.2005,-0.9639},
                    { 0.2323,-0.1626,-0.9590},
                    { 0.3675,0.0261,-0.9296},
                    { 0.1902,0.3744,-0.9076},
                    {  -0.1168,0.8334,-0.5402},
                   {  -0.2005,0.2527,-0.9466},
                   {  -0.4958,0.1345,-0.8580},
                   { -0.0141,-0.6281,-0.7780},
                   { -0.7445,-0.1477,-0.6511},
                   { -0.7609,0.3204,-0.5643},
                   { -0.1809,0.9247,-0.3351},
                   {-0.6796,-0.4224,-0.5997},
                   { 0.7771,0.4707,-0.4178},
                    { 0.9242,-0.1036,-0.3677},
                    { 0.4685,-0.7674,-0.4378},
                    {  0.8817,-0.1893,-0.4322},
                    {  0.6904,0.7062,-0.1569},
                    {  0.2391,0.7571,-0.6080},
                    { -0.0578,0.9837,0.1703},
                   { -0.5368,0.8361,-0.1135},
                   { -0.9918,-0.1207,-0.0423},
                   { -0.9968,0.0709,-0.0379},
                   { -0.8724,0.4781,-0.1014},
                   { -0.2487,0.9335,0.2581},
                   { 0.1183,0.9919,-0.0471},
                    { 0.3376,0.8415,0.4218},
                    { 0.5286,0.8409,0.1163},
                    { 0.9969,0.0550,-0.0571} };
        }
        
        public static final double[][] getJones30(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{1,0,0},
                    {0.166,0.986,0},             
                    {-0.110,0.664,0.740},   
                    {0.901,-0.419,-0.110},      
                    {-0.169,-0.601, 0.781},    
                    {-0.815, -0.386, 0.433},
                    {0.656, 0.366, 0.660},
                    {0.582, 0.800, 0.143},
                    {0.900, 0.259, 0.350},
                    {0.693, -0.698, 0.178},
                    {0.357, -0.924, -0.140},
                    {0.543, -0.488, -0.683},
                    {-0.525, -0.396, 0.753},
                    {-0.639, 0.689, 0.341},
                    {-0.330, -0.013, -0.944},
                    {-0.524, -0.783, 0.335},
                    {0.609, -0.065, -0.791},
                    {0.220, -0.233, -0.947},
                    {-0.004, -0.910, -0.415},
                    {-0.511, 0.627, -0.589},
                    {0.414, 0.737, 0.535},
                    {-0.679, 0.139, -0.721},
                    {0.884, -0.296, 0.362},
                    {0.262, 0.432, 0.863},
                    {0.088, 0.185, -0.979},
                    {0.294, -0.907, 0.302},
                    {0.887, -0.089, -0.453},
                    {0.257, -0.443, 0.859},
                    {0.086, 0.867, -0.491},
                    {0.863, 0.504, -0.025}};
        }
        
        public static final double[][] getJones30VMS(){
            /**
             * Hard coded gradient table from JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.GTCParams.java      
             * @author Bennett Landman
             */ 
            
            return new double[][]{{1,0,0},
                    {0.166,0.986,0},             
                    {-0.110,0.664,0.740},   
                    {0.901,-0.419,-0.110},      
                    {-0.169,-0.601, 0.781},    
                    {-0.815, -0.386, 0.433},
                    {0.656, 0.366, 0.660},
                    {0.582, 0.800, 0.143},
                    {0.900, 0.259, 0.350},
                    {0.693, -0.698, 0.178},
                    {0.357, -0.924, -0.140},
                    {0.543, -0.488, -0.683},
                    {-0.525, -0.396, 0.753},
                    {-0.639, 0.689, 0.341},
                    {-0.330, -0.013, -0.944},
                    {-0.524, -0.783, 0.335},
                    {-0.609, -0.065, -0.791},
                    {0.220, -0.233, -0.947},
                    {-0.004, -0.910, -0.415},
                    {-0.511, 0.627, -0.589},
                    {0.414, 0.737, 0.535},
                    {-0.679, 0.139, -0.721},
                    {0.884, -0.296, 0.362},
                    {0.262, 0.432, 0.863},
                    {0.088, 0.185, -0.979},
                    {0.294, -0.907, 0.302},
                    {0.887, -0.089, -0.453},
                    {0.257, -0.443, 0.859},
                    {0.086, 0.867, -0.491},
                    {0.863, 0.504, -0.025}};
        }
        
        private double cos(double a){
            return Math.cos(a);
        }
        private double sin(double a){
            return Math.sin(a);
        }
        

        public double[][] matrixMultiply(double[][] A, double[][] B){       
            /**
             * 3x3 only
             * (helper to angulationCorrection)
             * From JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.DTIGradientTableCreator.java
             * @author John Bogovic
             */
            
            
            double[][] C = new double[A.length][B[0].length];
            for(int i=0; i<C.length; i++){
                for(int j=0; j<C[0].length; j++){
                    C[i][j]=A[i][0]*B[0][j] + A[i][1]*B[1][j]+A[i][2]*B[2][j];
                }
            }
            return C;
        }
        
        public double[][] applyRotation(double[][] A, double[][] table){
            /**
            * Apply rotation to every row of the input table
            * (helper to angulationCorrection)
            * From JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.DTIGradientTableCreator.java
            * @author John Bogovic
            */
            
            double[][] rotTable = new double[table.length][table[0].length];
            for(int i=0; i<rotTable.length; i++){
                    double[][] row = {{table[i][0]},{table[i][1]},{table[i][2]}};
                    double[][] newrow = matrixMultiply(A,row);                         
                    rotTable[i][0]=newrow[0][0];
                    rotTable[i][1]=newrow[1][0];
                    rotTable[i][2]=newrow[2][0];
            }
            return rotTable;
        }
        
        private double[][] normalizeTable(double[][] table){       
            /**
            * (helper to angulationCorrection)
            * From JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.DTIGradientTableCreator.java
            * @author John Bogovic
            */
            
            double[][] normTable = new double[table.length][table[0].length];
            for(int i=0; i<normTable.length; i++){
                double length = table[i][0]*table[i][0] + table[i][1]*table[i][1] + table[i][2]*table[i][2]; 
                if(length!=0){
                    length=Math.sqrt(length);
                    normTable[i][0]=table[i][0]/length;
                    normTable[i][1]=table[i][1]/length;
                    normTable[i][2]=table[i][2]/length;
                }else{
                    normTable[i][0]=table[i][0];
                    normTable[i][1]=table[i][1];
                    normTable[i][2]=table[i][2];
                }
            }
            
            return normTable;
        }
        
        public void angulationCorrection(double[][] tablein){
            /**
             * From JHU CATNAP: edu.jhu.ece.iacl.algorithms.dti.DTIGradientTableCreator.java
             *  Take in hard coded gradient table assigned from user inputs and applies angulationCorrection to
             *  output a corrected gradient table for PAR/REC v3 and v4 DWI images
             * @param tablein
             * @author John Bogovic
             */
            
            FileInfoBase fileInfo = m_kDWIImage.getFileInfo(0);

            angCorrGT=new double[tablein.length][tablein[0].length];
                    
            if (fileInfoPARREC != null){
            sliceAng0=Math.toRadians(fileInfoPARREC.getSliceAngulation()[0]);
            sliceAng1=Math.toRadians(fileInfoPARREC.getSliceAngulation()[1]);
            sliceAng2=Math.toRadians(fileInfoPARREC.getSliceAngulation()[2]);
            }
            else if(parNsliceAng != null ){
                sliceAng0=Math.toRadians(parNsliceAng[0]);
                sliceAng1=Math.toRadians(parNsliceAng[1]);
                sliceAng2=Math.toRadians(parNsliceAng[2]);
            }

//          ==========================================================
//          TRANSFORMATION DEFINITIONS 
//          ==========================================================

            // Transformations and reverse transformatins that we will use
            // Definitions for these matrices were taken from Philips documentation
            double[][] Tpo;
            double[][] rev_Tpo;
            
            if ((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("SUPINE"))
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("SUPINE")
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("S")
                    ||patientOrientBox.getSelectedItem()=="SP" ){
                Tpo = new double[][]{{1, 0, 0},{0, 1, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{1,0,0},{0,1,0},{0,0,1}};
            }
            else if (( fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null &&  fileInfoPARREC.getPatientPosition().toUpperCase().contains("PRONE")) 
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("PRONE")
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("P")
                    || patientOrientBox.getSelectedItem()=="PR" ){
                Tpo = new double[][]{{-1, 0, 0},{0, -1, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{-1,0,0},{0,-1,0},{0,0,1}};
            }  
            else if ( (fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("RIGHT")) 
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("RIGHT")
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("R")
                    || patientOrientBox.getSelectedItem()=="RD"){
                Tpo = new double[][]{{0,-1, 0},{1, 0, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{0,1,0},{-1,0,0},{0,0,1}};
            }  
            else if ((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null &&  fileInfoPARREC.getPatientPosition().toUpperCase().contains("LEFT")) 
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("LEFT")
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("L")
                    || patientOrientBox.getSelectedItem()=="LD" ){
                Tpo = new double[][]{{0,1, 0},{-1, 0, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{0,-1,0},{1,0,0},{0,0,1}};
            }else{
                Tpo=null;
                rev_Tpo=null;
            }

            double[][] Tpp;
            double[][] rev_Tpp;
          
            if ((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("HEADFIRST")) 
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("HEADFIRST")
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("HF")
                    || patientPosBox.getSelectedItem()=="Head First"){
                Tpp = new double[][]{{0, -1, 0},{-1, 0, 0}, {0, 0, 1}};
                rev_Tpp = new double[][]{{0,-1,0},{-1,0,0},{0,0,-1}};
            }
            else if ((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("FEETFIRST")) 
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("FEETFIRST")
                    ||parNPatientPosition != null && parNPatientPosition.toUpperCase().contains("FF")
                    || patientPosBox.getSelectedItem()=="Feet First"){
                Tpp = new double[][]{{0, 1, 0},{-1, 0, 0}, {0, 0, -1}};
                rev_Tpp = new double[][]{{0,-1,0},{1,0,0},{0,0,-1}};
            }else{
                Tpp=null;
                rev_Tpp=null;
            }

            double ap = sliceAng0;
            double fh = sliceAng1;
            double rl = sliceAng2;
            
            double[][] Tpom = matrixMultiply(Tpo,Tpp);
            double[][] rev_Tpom = matrixMultiply(rev_Tpp,rev_Tpo);

            double[][] Trl = {{1,0,0}, {0, cos(rl), -sin(rl)}, {0,sin(rl),cos(rl)}};
            double[][] Tap = {{cos(ap),0,sin(ap)}, {0,1,0}, {-sin(ap),0,cos(ap)}};
            double[][] Tfh = {{cos(fh),-sin(fh),0}, {sin(fh),cos(fh),0}, {0,0,1}};
            double[][] Tang = matrixMultiply(matrixMultiply(Trl,Tap),Tfh);

            double[][] rev_Trl = {{1,0,0}, {0, cos(rl), sin(rl)}, {0,-sin(rl),cos(rl)}};
            double[][] rev_Tap = {{cos(ap),0,-sin(ap)}, {0,1,0}, {sin(ap),0,cos(ap)}};
            double[][] rev_Tfh = {{cos(fh),sin(fh),0}, {-sin(fh),cos(fh),0}, {0,0,1}};
            double[][] rev_Tang = matrixMultiply(matrixMultiply(rev_Tfh,rev_Tap),rev_Trl);

            double[][] Tsom = null;
            double[][] rev_Tsom = null;
            

            
//          % Definitions for Tsom
            if (fileInfoPARREC != null){
                if (fileInfoPARREC.getSliceOrient()== 1 ){
                    Tsom = new double[][]{{0,0,-1},{0,-1,0},{1,0,0}};
                    rev_Tsom = new double[][]{{0,0,1},{0,-1,0},{-1,0,0}};
                }
                else if (fileInfoPARREC.getSliceOrient()== 2){
                    Tsom = new double[][]{{0,-1,0},{0,0,1},{1,0,0}};
                    rev_Tsom = new double[][]{{0,0,1},{-1,0,0},{0,1,0}};
                }
                else if (fileInfoPARREC.getSliceOrient()== 3){
                    Tsom = new double[][]{{0,-1,0},{-1,0,0},{0,0,1}};
                    rev_Tsom = new double[][]{{0,-1,0},{-1,0,0},{0,0,1}};
                }else{
                    Tsom=null;
                    rev_Tsom=null;
                }
            }
            else if (parNversion != null){
                if (parNorient == 1 ){
                    Tsom = new double[][]{{0,0,-1},{0,-1,0},{1,0,0}};
                    rev_Tsom = new double[][]{{0,0,1},{0,-1,0},{-1,0,0}};
                }
                else if (parNorient == 2){
                    Tsom = new double[][]{{0,-1,0},{0,0,1},{1,0,0}};
                    rev_Tsom = new double[][]{{0,0,1},{-1,0,0},{0,1,0}};
                }
                else if (parNorient == 3){
                    Tsom = new double[][]{{0,-1,0},{-1,0,0},{0,0,1}};
                    rev_Tsom = new double[][]{{0,-1,0},{-1,0,0},{0,0,1}};
                }else{
                    Tsom=null;
                    rev_Tsom=null;
                }
                
            }
            
            //Definitions for Tprep_par Tprep_per & Tfsd_m, Tfsd_p, Tfsd_s

            double[][] Tprep_par = {{1,0,0},{0,1,0},{0,0,1}};
            double[][]rev_Tprep_par = {{1,0,0},{0,1,0},{0,0,1}};
            double[][]Tprep_per = {{0,-1,0},{1,0,0},{0,0,1}};
            double[][]rev_Tprep_per = {{0,1,0},{-1,0,0},{0,0,1}};

            double[][] Tfsd_m = {{-1,0,0},{0,1,0},{0,0,1}};
            double[][] rev_Tfsd_m = {{-1,0,0},{0,1,0},{0,0,1}};
            double[][] Tfsd_p = {{1,0,0},{0,-1,0},{0,0,1}};
            double[][] rev_Tfsd_p = {{1,0,0},{0,-1,0},{0,0,1}};
            double[][] Tfsd_s = {{1,0,0},{0,1,0},{0,0,-1}};
            double[][] rev_Tfsd_s = {{1,0,0},{0,1,0},{0,0,-1}};


            double[][] Tprep = null;
            double[][] rev_Tprep = null;
            double[][] Tfsd = null;
            double[][] rev_Tfsd = null;
            if(fileInfoPARREC != null && fileInfoPARREC.getSliceOrient()== 1 || parNorient == 1){
                
                if((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("ANTERIOR")) 
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("ANTERIOR") 
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("AP") 
                        || foldOverBox.getSelectedItem()=="AP"){
                    Tprep =Tprep_per;
                    rev_Tprep = rev_Tprep_per;
                    if(fatshiftBox.getSelectedItem()=="A"){
                        Tfsd = Tfsd_m;
                        rev_Tfsd = rev_Tfsd_m;
                    }else if(fatshiftBox.getSelectedItem()=="P"){
                        Tfsd = Tfsd_p;
                        rev_Tfsd = rev_Tfsd_p;
                    }else{
                        fatShiftLabel.setForeground(Color.red);
                        MipavUtil.displayError("Gradient Table Creator: " + fatshiftBox.getSelectedItem() + " is not consistent with --Anterior-Posterior-- foldover ");
                        Tfsd = null;
                        rev_Tfsd = null;
                    }

                }
          
                else if((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("RIGHT")) 
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("RIGHT")
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("RL") 
                        || foldOverBox.getSelectedItem()=="RL" ){
                    Tprep =Tprep_par;
                    rev_Tprep = rev_Tprep_par;
                    if(fatshiftBox.getSelectedItem()=="R"){
                        Tfsd = Tfsd_p;
                        rev_Tfsd = rev_Tfsd_p;
                    }else if(fatshiftBox.getSelectedItem()=="L"){
                        Tfsd = Tfsd_m;
                        rev_Tfsd = rev_Tfsd_m;
                    }else{
                        fatShiftLabel.setForeground(Color.red);
                        MipavUtil.displayError("Gradient Table Creator: " + fatshiftBox.getSelectedItem() + " is not consistent with --Right-Left-- foldover ");
                        Tfsd = null;
                        rev_Tfsd = null;
                    }

                }
                else {
                    
                    fatShiftLabel.setForeground(Color.red);
                    MipavUtil.displayError("Fat Shift Label Error");
                    /*Tprep=null;
                    rev_Tprep=null;
                    Tfsd = null;
                    rev_Tfsd = null;*/
                }
            }
            else if(fileInfoPARREC != null && fileInfoPARREC != null && fileInfoPARREC.getSliceOrient()== 3 || parNorient == 3){
                if((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("SUPERIOR")) 
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("SUPERIOR")
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("FH") 
                        || foldOverBox.getSelectedItem()=="FH"){
                    Tprep =Tprep_per;
                    rev_Tprep = rev_Tprep_per;
                    if(fatshiftBox.getSelectedItem()=="F"){
                        Tfsd = Tfsd_p;
                        rev_Tfsd = rev_Tfsd_p;
                    }else if(fatshiftBox.getSelectedItem()=="H"){
                        Tfsd = Tfsd_m;
                        rev_Tfsd = rev_Tfsd_m;
                    }else{
                        fatShiftLabel.setForeground(Color.red);
                        MipavUtil.displayError("Gradient Table Creator: " + fatshiftBox.getSelectedItem() + " is not consistent with --Superior-Inferior OR FH-- foldover ");
                        Tfsd = null;
                        rev_Tfsd = null;
                    }
                }
                else if((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("RIGHT")) 
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("RIGHT")
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("RL") 
                        || foldOverBox.getSelectedItem()=="RL"){
                    Tprep =Tprep_par;
                    rev_Tprep = rev_Tprep_par;
                    if(fatshiftBox.getSelectedItem()=="R"){
                        Tfsd = Tfsd_p;
                        rev_Tfsd = rev_Tfsd_p;
                    }else if(fatshiftBox.getSelectedItem()=="L"){
                        Tfsd = Tfsd_m;
                        rev_Tfsd = rev_Tfsd_m;
                    }else{
                        fatShiftLabel.setForeground(Color.red);
                        MipavUtil.displayError("Gradient Table Creator: " + fatshiftBox.getSelectedItem() + " is not consistent with --Right-Left-- foldover ");
                        Tfsd = null;
                        rev_Tfsd = null;
                    }

                }
                else{
                    Tprep=null;
                    rev_Tprep=null;
                    Tfsd = null;
                    rev_Tfsd = null;
                }
            }
            else if(fileInfoPARREC != null && fileInfoPARREC.getSliceOrient()== 2 || parNorient == 2){
                if((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("SUPERIOR")) 
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("SUPERIOR")
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("FH")
                        || foldOverBox.getSelectedItem()=="FH" ){
                    Tprep =Tprep_per;
                    rev_Tprep = rev_Tprep_per;
                    if(fatshiftBox.getSelectedItem()=="F"){
                        Tfsd = Tfsd_p;
                        rev_Tfsd = rev_Tfsd_p;
                    }else if(fatshiftBox.getSelectedItem()=="H"){
                        Tfsd = Tfsd_m;
                        rev_Tfsd = rev_Tfsd_m;
                    }else{
                        fatShiftLabel.setForeground(Color.red);
                        MipavUtil.displayError("Gradient Table Creator: " + fatshiftBox.getSelectedItem() + " is not consistent with --Superior-Inferior OR FH-- foldover ");
                        Tfsd = null;
                        rev_Tfsd = null;
                    }

                }
                else if((fileInfoPARREC != null && fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("ANTERIOR"))
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("ANTERIOR")
                        || parNPatientPosition != null && parNfoldover.toUpperCase().contains("AP")
                        ||foldOverBox.getSelectedItem()=="AP"){
                    Tprep =Tprep_par;
                    rev_Tprep = rev_Tprep_par;
                    if(fatshiftBox.getSelectedItem()=="A"){
                        Tfsd = Tfsd_p;
                        rev_Tfsd = rev_Tfsd_p;
                    }else if(fatshiftBox.getSelectedItem()=="P"){
                        Tfsd = Tfsd_m;
                        rev_Tfsd = rev_Tfsd_m;
                    }else{
                        fatShiftLabel.setForeground(Color.red);
                        MipavUtil.displayError("Gradient Table Creator: " + fatshiftBox.getSelectedItem() + " is not consistent with --Anterior-Posterior OR FH-- foldover ");
                        Tfsd = null;
                        rev_Tfsd = null;
                    }
                }
                else{
                    Tprep=null;
                    rev_Tprep=null;
                    Tfsd = null;
                    rev_Tfsd = null;
                }
            }else{
                Tprep=null;
                rev_Tprep=null;
                Tfsd=null;
                rev_Tfsd=null;
            }
            
//          % ==========================================
//          % END OF PHILIPS TRANSFORMATION DEFINITIONS
//          % ==========================================
            
            /*
             * APPLY TRANSFORMATIONS
             */
//          % ======================================
//          % APPLICATION OF THE TRANSFORMATIONS
//          % ======================================
            if(space=="LPH"){
                angCorrGT = applyRotation(matrixMultiply(rev_Tsom,rev_Tang),gradCreatetable);
                rev_angCorrGT = applyRotation(matrixMultiply(Tang,Tsom),angCorrGT);
            }
            else if(space=="XYZ"){
                angCorrGT = applyRotation(matrixMultiply(rev_Tsom,matrixMultiply(rev_Tang,Tpom)),gradCreatetable);
                rev_angCorrGT = applyRotation(matrixMultiply(rev_Tpom,matrixMultiply(Tang,Tsom)),angCorrGT);
            }
            else if(space=="MPS"){
                angCorrGT = applyRotation(matrixMultiply(Tprep,Tfsd),gradCreatetable);
                rev_angCorrGT = applyRotation(matrixMultiply(rev_Tfsd,rev_Tprep),angCorrGT);;
            }
            else{
                System.err.println("Gradient Table Creator"+"NO CORRECTION APPLIED!");
            }
            
            DecimalFormat twoDForm = new DecimalFormat("#.####");


//          % Normalize the non zero vectors
            angCorrGT = normalizeTable(angCorrGT);
            rev_angCorrGT = normalizeTable(rev_angCorrGT);
            

            //Testing
            /*for (int i = 0; i<tablein.length; i++){
            System.out.println("angCorrGT: " +(i+1) + "\t" +Double.valueOf(twoDForm.format(angCorrGT[i][0]))+ "\t" + Double.valueOf(twoDForm.format(angCorrGT[i][1]))+ "\t" + Double.valueOf(twoDForm.format(angCorrGT[i][2])));
            }
            
            for (int i = 0; i<tablein.length; i++){
                System.out.println("rev_angCorrGT: " +(i+1) + "\t" +Double.valueOf(twoDForm.format(rev_angCorrGT[i][0]))+ "\t" + Double.valueOf(twoDForm.format(rev_angCorrGT[i][1]))+ "\t" + Double.valueOf(twoDForm.format(rev_angCorrGT[i][2])));
                }*/
            

            if (numVolumes==35){
                for (int i = 0; i<tablein.length; i++){
                    if (gradResWOP.contains("Yes")){
                    srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i][2]*-1))), i, 2);
                    srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i][1]*-1))), i, 3);
                    srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i][0]))), i, 4);
                    }
                    else{
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i][1]*-1))), i, 2);
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i][0]*-1))), i, 3);
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i][2]))), i, 4); 
                    }
                }
            }
            else if (numVolumes==8 || numVolumes==17 || numVolumes==34 || numVolumes==32){

                double[] flBvalArr = null;
                if (dtiparams != null){
                    flBvalArr = dtiparams.getbValues();
                }
                else if( parDTIParams != null){
                    flBvalArr = parDTIParams.getbValues();
                }
                int bval0Count = 0;
                for (int i = 0; i<numVolumes; i++){
                    if (flBvalArr[i]== 0){
                        srcTableModel.setValueAt("0", i, 2);
                        srcTableModel.setValueAt("0", i, 3);
                        srcTableModel.setValueAt("0", i, 4); 
                        bval0Count = 1;
                    }
                    else if (i == numVolumes-1) {
                        srcTableModel.setValueAt("100", i, 2);
                        srcTableModel.setValueAt("100", i, 3);
                        srcTableModel.setValueAt("100", i, 4); 
                    }
                    else{
                        if (gradResWOP.contains("Yes")){
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][2]*-1))), i, 2);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][1]*-1))), i, 3);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][0]))), i, 4);
                        }
                        else{
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][1]*-1))), i, 2);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][0]*-1))), i, 3);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][2]))), i, 4);
                        }
                        }
                    
                }
            }
            else if(numVolumes==31){
                double[] flBvalArr = dtiparams.getbValues();
                int bval0Count = 0;
                for (int i = 0; i<numVolumes; i++){

                    if (flBvalArr[i]== 0){
                        srcTableModel.setValueAt(0, i, 2);
                        srcTableModel.setValueAt(0, i, 3);
                        srcTableModel.setValueAt(0, i, 4); 
                        bval0Count = 1;
                    }
                    else{
                        if (gradResWOP.contains("Yes")){
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][2]*-1))), i, 2);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][1]*-1))), i, 3);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][0]))), i, 4);
                        }
                        else{
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][1]*-1))), i, 2);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][0]*-1))), i, 3);
                            srcTableModel.setValueAt((String.valueOf(twoDForm.format(angCorrGT[i-bval0Count][2]))), i, 4); 
                        }
                        }
                }
                
            }

        }
        
        public void loadDWIFile() {
           
            ViewOpenFileUI openFile = new ViewOpenFileUI(true);          
            final boolean stackFlag = getLastStackFlag();
            ArrayList<Vector<String>> openImagesArrayList = openFile.open(stackFlag);
            final FileIO fileIO = new FileIO();
            //m_kDWIImage = fileIO.readImage(openFile.getImagePath());
            textDWIDataimage.setText(openFile.getImagePath());
            m_kDWIImage = openFile.getImage();
            if (m_kDWIImage != null && m_kDWIImage.is4DImage()){
                getImageDTIParams(); 
                Vector<Frame> imageFrameVector = ui.getImageFrameVector();
                for (int i = 0; i<imageFrameVector.size(); i++){
                    String imageFrameName = imageFrameVector.get(i).getName();
                    String openedFileName = openFile.getFileName();
                    if (openedFileName.equals(imageFrameName)){
                        frame = (ViewJFrameImage) imageFrameVector.get(i);
                        break;
                    }
                }
            }
            else if (m_kDWIImage != null && m_kDWIImage.isDicomImage() ==true &&
                    m_kDWIImage.is3DImage()==true){
                checkSiemens3d();
                getImageDTIParams(); 
                Vector<Frame> imageFrameVector = ui.getImageFrameVector();
                for (int i = 0; i<imageFrameVector.size(); i++){
                    String imageFrameName = imageFrameVector.get(i).getName();
                    String openedFileName = openFile.getFileName();
                    if (openedFileName.equals(imageFrameName)){
                        frame = (ViewJFrameImage) imageFrameVector.get(i);
                        break;
                    }
                }
            }

            else{
                MipavUtil.displayError("Please select a 4D DWI Image"); 
            }
            


            

            
        }
        
        public void niftiParExtraction(File parFileName) throws IOException{
            RandomAccessFile raFile = null;
            float vox_offset = 0.0f;
            HashMap<String,String> VolMap;
            HashMap<String,Integer> SliceMap;
            HashMap<String,String> VolParameters;
            Vector<String> SliceParameters;
            Vector<String> Slices;           
            parNversion = "";            
            parNExamName = "";
            parNProtocolName = "";            
            parNPatientPosition = "";         
            parNfoldover = "";
            int sliceOrientPos = 0;
            int bValuePos = 0;
            int gradPos = 0;           
            int sliceOrientIndex;           
            int bValueIndex;           
            int gradIndex;
            int counter = 0;
            try {
                raFile = new RandomAccessFile(parFileName, "r");
            } catch (FileNotFoundException e) {
                Preferences.debug("raFile = new RandomAccessFile(fileHeader, r) gave " + "FileNotFoundException " + e,
                        Preferences.DEBUG_FILEIO);
            }
            
            String nextLine = raFile.readLine();
            
            VolMap = buildParVolMap();
            SliceMap = buildParSliceMap();

            VolParameters = new HashMap<String,String>();
            SliceParameters = new Vector<String>();
            Slices = new Vector<String>();
            
            
            //String version;
            //String[] versionNumber;

            while(null!=nextLine) {
                nextLine = nextLine.trim();
                if(nextLine.length()<1) { // Blank line = comment
                    nextLine = raFile.readLine().trim();
                    continue;
                }
                switch(nextLine.charAt(0)) {
                    case '#' : //# = comment
                        if(nextLine.contains("Research image export tool")) {
                            //need to get version
                            parNversion = nextLine.substring(nextLine.lastIndexOf("V"), nextLine.length());
                        }
                                
                        String imageInfo  = "";                
                        int sliceOrientIndexCounter = -1;
                        int bValIndexCounter = -1;
                        int gradIndexCounter = -1;
                        
                        if(nextLine.compareToIgnoreCase("# === IMAGE INFORMATION DEFINITION =============================================")==0) {
                            String line = raFile.readLine().trim();
                            String ignore = "The rest of this file contains ONE line per image";

                            while(line.compareToIgnoreCase("# === IMAGE INFORMATION ==========================================================")!=0) {
                                if(line.length()>1) {
                                   
                                    if(!line.contains(ignore)) {
                                        
                                        SliceParameters.add(line.trim());
                                                                                       
                                        counter ++;
                                        
                                        imageInfo = new String(imageInfo + line.trim());
                                           
                                           if (imageInfo.contains("slice orientation ( TRA/SAG/COR ) ")){    
                                               sliceOrientIndexCounter++;
                                           }
                                            if (imageInfo.contains("diffusion_b_factor")){                                         
                                               bValIndexCounter++;
                                           }
                                           
                                           if (imageInfo.contains("diffusion (ap, fh, rl)")){                                      
                                               gradIndexCounter++;
                                           }                                       
                                      }                  
                                }
                         
                                line = raFile.readLine().trim();                           
                            }
                            sliceOrientPos = counter - sliceOrientIndexCounter;
                            bValuePos = counter - bValIndexCounter;
                            gradPos = counter - gradIndexCounter;                       
                        }
                 
                        break;
                    case '.' : // scan file variable

                            if(nextLine.contains("Examination name")){
                                String examNameLine = nextLine.trim();
                                int examNameInd = examNameLine.indexOf(":");
                                int examNameLineLength = examNameLine.length();
                                
                                for (int i = 0 ; i < examNameLineLength-(examNameInd+1); i++) {
                                    int examIndex = (examNameInd+1)+i;
                                    char examLetter= examNameLine.charAt(examIndex);
                                    parNExamName =parNExamName + examLetter;
                                    parNExamName = parNExamName.trim();
                                    
                                  
                                }
                            }
                       
                           if(nextLine.contains("Protocol name")){
                               String protocolNameLine = nextLine.trim();
                               int protocolNameInd = protocolNameLine.indexOf(":");
                               int protocolNameLineLength = protocolNameLine.length();
                               for (int i = 0 ; i < protocolNameLineLength-(protocolNameInd+1); i++) {
                                   int protocolIndex = (protocolNameInd+1)+i;
                                   char protocolLetter= protocolNameLine.charAt(protocolIndex);
                                   parNProtocolName =parNProtocolName + protocolLetter;
                                   parNProtocolName = parNProtocolName.trim();   
                              }
                               
                           }
                                                      
                            //Determine date of exam
                            if(nextLine.contains("Examination date/time")){
                            int ind = nextLine.indexOf(":");
                            String date = nextLine.substring(ind+1,nextLine.indexOf("/",ind));
                            date = date.trim();
                        }
                            if(nextLine.contains("Patient position")){
                                String patientPositionLine = nextLine.trim();
                                int patientPositionInd = patientPositionLine.indexOf(":");
                                int patientPositionLineLength = patientPositionLine.length();
                                for (int i = 0 ; i < patientPositionLineLength-(patientPositionInd+1); i++) {
                                    int positionIndex = (patientPositionInd+1)+i;
                                    char positionLetter= patientPositionLine.charAt(positionIndex);
                                    parNPatientPosition =parNPatientPosition + positionLetter;
                                    parNPatientPosition = parNPatientPosition.trim();   
                               }
                            }
                            
                            if(nextLine.contains("Preparation direction")){
                                String foldoverLine = nextLine.trim();
                                int foldoverInd = foldoverLine.indexOf(":");
                                int foldoverLineLength = foldoverLine.length();
                                for (int i = 0 ; i < foldoverLineLength-(foldoverInd+1); i++) {
                                    int foldoverIndex = (foldoverInd+1)+i;
                                    char foldoverLetter= foldoverLine.charAt(foldoverIndex);
                                    parNfoldover = parNfoldover + foldoverLetter;
                                    parNfoldover = parNfoldover.trim();  
                               }
                            }
                            
                            
                            //Checks to see if examination is DTI to extract angulation and off centre to determine gradients
                                if(nextLine.contains("Angulation midslice")){
                                String info = nextLine.substring(nextLine.indexOf(":")+1);
                                info=info.trim();
                                parNsliceAng = new double[3];
                                parNsliceAng[0] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                                info = info.substring(info.indexOf(' ')+1, info.length()).trim();
                                parNsliceAng[1] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                                parNsliceAng[2] = Double.parseDouble(info.substring(info.indexOf(' ')+1, info.length()));

                            }
                            
                            if(nextLine.contains("Off Centre midslice")){
                                String info = nextLine.substring(nextLine.indexOf(":")+1);
                                info=info.trim();
                                parNoffCentre = new double[3];
                                parNoffCentre[0] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                                info = info.substring(info.indexOf(' ')+1, info.length()).trim();                          
                                parNoffCentre[1] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                                parNoffCentre[2] = Double.parseDouble(info.substring(info.indexOf(' ')+1, info.length()));
                            }
                    

                        String []tags = nextLine.split(":");
                        String tag = tags[0].trim();
                        String key;
                        if(tags.length < 2) {
                            key = "";
                        }else {
                            key = tags[1].trim();
                        }
                        String stgTag = (String)VolMap.get(tag);
                        if(null!=stgTag) {
                            VolParameters.put(stgTag,key);
                        } else {
                            Preferences.debug("FilePARREC:readHeader. Unknown Volume Tag: " + tag + "=" + key + "\n",
                                    Preferences.DEBUG_FILEIO);
                        }
                        break;
                    default: // parse as image slice information
                      
                        Slices.add(nextLine);               
                        break;

                }
               
                nextLine = raFile.readLine();

            }

            //All done, close the header file//
            try {
                raFile.close();
            } catch (IOException e) {
                Preferences.debug("raFile.close() gave IOException " + e + "\n", Preferences.DEBUG_FILEIO);
                throw new IOException(" Error on raFile.close()");
            }


            String s;
            String[] ss;
            //Get the volume variables:
            s = (String)VolParameters.get("max_num_slices");
            int numSlices = Integer.valueOf(s);
            //get numVolumes
            numVolumes = Slices.size()/numSlices;


            // Let's parse the first slice:    
            String sl = (String)Slices.get(0);      
            String[] values = sl.split("\\s+");
            
     
            //Create bvalue String array from V3 par/rec file (for DTI par/rec files)
            double [] flBvalueArray = new double[numVolumes];
            double[][] flGradientArray = new double[numVolumes][3];
                
            parDTIParams = new DTIParameters(numVolumes);
 
            parDTIParams.setNumVolumes(numVolumes);
                
                
                //Determine arrangement of slices stored with data
                String firstSliceIndex = Slices.get(0);
                firstSliceIndex = firstSliceIndex.trim();
                final String[] firstSliceArr = firstSliceIndex.split("\\s+");
                int firstSliceValue= Integer.parseInt(firstSliceArr[0]);
                
                String secondSliceIndex = Slices.get(1);
                secondSliceIndex = secondSliceIndex.trim();
                final String[] secondSliceArray = secondSliceIndex.split("\\s+");
                int secondSliceValue = Integer.parseInt(secondSliceArray[0]);
                          
                // Find slice index of bvalues
                int counter2o = 0;
                int counter3o = 0;
                for (int i = 0; i < (sliceOrientPos-2); i++){
                    if (SliceParameters.get(i).contains("2")){
                        counter2o++;
                    }
                    if (SliceParameters.get(i).contains("3")){ 
                        counter3o++;
                    }
                }           
                sliceOrientIndex = ((counter2o*1)+(counter3o*2) + (sliceOrientPos-1));
                
                String firstSlice = Slices.get(0);
                firstSlice = firstSlice.trim();
                final String[] firstOrientSlice = firstSlice.split("\\s+");
                parNorient = Integer.parseInt(firstOrientSlice[sliceOrientIndex]);

                
                // Find slice index of bvalues
                int counter2 = 0;
                int counter3 = 0;
                for (int i = 0; i < (bValuePos-2); i++){
                    if (SliceParameters.get(i).contains("2")){
                        counter2++;
                    }
                    if (SliceParameters.get(i).contains("3")){ 
                        counter3++;
                    }
                }
                
                bValueIndex = ((counter2*1)+(counter3*2) + (bValuePos-1));

                
                if (parNversion.equals("V3")||parNversion.equals("V4")){                
                    if (firstSliceValue!=secondSliceValue){
                        for (int i = 0; i < numVolumes; i++){
                            String sliceIndex = Slices.get(i*numSlices);
                            sliceIndex = sliceIndex.trim();
                            final String[] sliceArr = sliceIndex.split("\\s+");                      
                            flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);


                            }
                        parDTIParams.setbValues(flBvalueArray);                   
                        }
                    
                    else{
                        for (int i = 0; i < numVolumes; i++){
                            String sliceIndex = Slices.get(i);
                            sliceIndex = sliceIndex.trim();
                            final String[] sliceArr = sliceIndex.split("\\s+");
                            flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);
                            

                        }
                        parDTIParams.setbValues(flBvalueArray);
                    }       
            }
                else if(parNversion.equals("V4.1")||parNversion.equals("V4.2") ){
                 // Find slice index automatically of gradient values
                    int counter2s = 0;
                    int counter3s = 0;
                        for (int i = 0; i < (gradPos-2); i++){
                            if (SliceParameters.get(i).contains("2")){
                                counter2s++;
                                }
                            if (SliceParameters.get(i).contains("3")){ 
                                counter3s++;
                                }
                            }
                        gradIndex = ((counter2*1)+(counter3*2) + (gradPos-1));
                      
                    if (firstSliceValue!=secondSliceValue){
                        for (int i = 0; i < numVolumes; i++){
                            String sliceIndex = Slices.get(i*numSlices);
                            sliceIndex = sliceIndex.trim();
                            final String[] sliceArr = sliceIndex.split("\\s+");
                            flGradientArray[i][0] = Double.valueOf(sliceArr[gradIndex]);
                            flGradientArray[i][1] = Double.valueOf(sliceArr[gradIndex+1]);
                            flGradientArray[i][2] = Double.valueOf(sliceArr[gradIndex+2]);
                            flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);
                            }
                        
                        parDTIParams.setbValues(flBvalueArray);
                        parDTIParams.setGradients(flGradientArray);

                        }
                    
                    else{
                        for (int i = 0; i < numVolumes; i++){
                            String sliceIndex = Slices.get(i);
                            sliceIndex = sliceIndex.trim();
                            final String[] sliceArr = sliceIndex.split("\\s+");
                            flGradientArray[i][0] = Double.valueOf(sliceArr[gradIndex]);
                            flGradientArray[i][1] = Double.valueOf(sliceArr[gradIndex+1]);
                            flGradientArray[i][2] = Double.valueOf(sliceArr[gradIndex+2]);
                            flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);
                            }
                        
                        parDTIParams.setbValues(flBvalueArray);
                        parDTIParams.setGradients(flGradientArray);
                    }

                }
                   

            
        }
        
        public void philipsDicomGradExtract(){
            FileInfoDicom dicomInfo = (FileInfoDicom) m_kDWIImage.getFileInfo(0);
            FileDicomSQ sq;
            FileDicomTagTable tagTable = dicomInfo.getTagTable();
            String seriesDescription = (String) tagTable.getValue("0008,103E");
            String scannerType = (String) tagTable.getValue("0008,0070");
            parNversion = "";            
            parNExamName = "";
            parNProtocolName = "";            
            parNPatientPosition = "";         
            parNfoldover = "";
            parNProtocolName = (String) tagTable.getValue("0008,103E");
            
            if ((seriesDescription != null && seriesDescription.toUpperCase().contains("DTI"))) {
                if (scannerType != null && scannerType.toUpperCase().contains("PHILIPS")) {  
                    //System.out.println("image.getExents" +image.getExtents()[3]);                       
                          if (m_kDWIImage.is4DImage()){
                              parNversion = "V4.2";
                              if(tagTable.getValue("0018,5100")!= null){
                                  parNPatientPosition =  (String) tagTable.getValue("0018,5100");
                              }
                              if(tagTable.getValue("2001,105F")!= null){
                              sq = (FileDicomSQ) tagTable.getValue("2001,105F", false);
                              FileDicomTagTable item = sq.getItem(0);
                                  if(item.getValue("2001,1035")!= null){
                                      String sOrient = (String) item.getValue("2001,1035");
                                      parNorient = Integer.valueOf(sOrient);
                                  }
                                  if(item.getValue("2005,1071")!= null){
                                      parNsliceAng = new double[3];
                                      String sAng1 = (String) item.getValue("2005,1071");;
                                      String sAng2 = (String) item.getValue("2005,1072");
                                      String sAng3 = (String) item.getValue("2005,1073");;
                                      parNsliceAng[0] = Double.valueOf(sAng1);
                                      parNsliceAng[1] = Double.valueOf(sAng2);
                                      parNsliceAng[2] = Double.valueOf(sAng3);
                                  }
                                  if(item.getValue("2005,107B")!= null){
                                      parNfoldover = (String) item.getValue("2005,107B");
                                  }
                                           
                              }
                              
                          }
                          }
            }
            
        }
        
        public void checkSiemens3d(){
            FileInfoDicom dicomInfo = (FileInfoDicom) m_kDWIImage.getFileInfo(0);
            FileDicomTagTable tagTable = dicomInfo.getTagTable();
            String studyDescription = (String) tagTable.getValue("0008,1030");
            String seriesDescription = (String) tagTable.getValue("0008,103E");
            String scannerType = (String) tagTable.getValue("0008,0070");

            if ((studyDescription != null && studyDescription.toUpperCase().contains("DTI")) || 
                    (seriesDescription != null && seriesDescription.toUpperCase().contains("DTI"))) {
               if (scannerType != null && scannerType.toUpperCase().contains("SIEMEN")) {
                   checkSiemens = true;
                   if (tagTable.getValue("0018,1310") != null) {
                       // Acquisition matrix
                       FileDicomTag tag = tagTable.get(new FileDicomKey("0018,1310"));
                       Object[] values = tag.getValueList();
                       int valNumber = values.length;  
                       int subXDim = 0;
                       int subYDim = 0;
                       int subZDim = 0;
                       int subTDim;
                       if ((valNumber == 4) && (values instanceof Short[])) {
                           int frequencyRows = ((Short) values[0]).intValue();
                           int frequencyColumns = ((Short) values[1]).intValue();
                           int phaseRows = ((Short) values[2]).intValue();
                           int phaseColumns = ((Short) values[3]).intValue();
                        if ((frequencyRows > 0) && (phaseRows == 0)) {
                               subYDim = frequencyRows;
                           }
                           else if ((frequencyRows == 0) && (phaseRows > 0)) {
                               subYDim = phaseRows;
                           }
                        if ((frequencyColumns > 0) && (phaseColumns == 0)) {
                               subXDim = frequencyColumns;
                           }
                           else if ((frequencyColumns == 0) && (phaseColumns > 0)) {
                               subXDim = phaseColumns;
                           }
                       }
                    // if (tagTable.getValue("0018,1310") != null)
                       if (tagTable.getValue("0019,100A") != null) {
                           tag = tagTable.get(new FileDicomKey("0019,100A"));
                           Object value = tag.getValue(false);
                           if (value instanceof Short) {
                               subZDim = ((Short) value).intValue();
                               Preferences.debug("subZDim = " + subZDim + "\n");
                           }   
                       } // if (tagTable.getValue("0019,100A") != null)
                       
                       subTDim = m_kDWIImage.getExtents()[2];
                       Preferences.debug("subTDim = " + subTDim + "\n");
                       
                       int destExtents[] = new int[4];
                       ModelImage destImage = null;
                       
                       destExtents[0] = subXDim;
                       destExtents[1] = subYDim;
                       destExtents[2] = subZDim;
                       destExtents[3] = subTDim;

                       destImage = new ModelImage(m_kDWIImage.getType(), destExtents, m_kDWIImage.getImageName()+ "_mosaic_to_slices");
                       mosaicToSliceAlgo = new AlgorithmMosaicToSlices(m_kDWIImage, destImage);

                       // This is very important. Adding this object as a listener allows the algorithm to
                       // notify this object when it has completed of failed. See algorithm performed event.
                       // This is made possible by implementing AlgorithmedPerformed interface
                       mosaicToSliceAlgo.addListener(this);
                       
                       mosaicToSliceAlgo.run();
                       
                       try {
                           if (mosaicToSliceAlgo.getResultImage() != null){
                               m_kDWIImage = mosaicToSliceAlgo.getResultImage();
                               new ViewJFrameImage(m_kDWIImage, null, new Dimension(610, 200));
                           }
                       } catch (OutOfMemoryError error) {
                           System.gc();
                           MipavUtil.displayError("Out of memory: unable to open new frame");
                       }


                   }
               }
               else{
                   MipavUtil.displayError("Please select a 4D DWI Image");  
               }
            }
            else{
                MipavUtil.displayError("Please select a 4D DWI Image");  
            }
            
        }
        
        public void loadT2File() {

           openFile = new ViewOpenFileUI(true);          
            final boolean stackFlag = getLastStackFlag();
            ArrayList<Vector<String>> openImagesArrayList = openFile.open(stackFlag);
            final FileIO fileIO = new FileIO();
            m_kT2Image = openFile.getImage();
            Vector<Frame> imageFrameVector = ui.getImageFrameVector();
                for (int i = 0; i<imageFrameVector.size(); i++){
                    String imageFrameName = imageFrameVector.get(i).getName();
                    String openedFileName = openFile.getFileName();
                    if (openedFileName.equals(imageFrameName)){
                        t2frame = (ViewJFrameImage) imageFrameVector.get(i);
                        break;
                    }
                }




            

            
        }
        public boolean getLastStackFlag() {
            return this.lastStackFlag;
        }
        
        
        /**
         * reads the bval/gradient file...dti studio format, BRUKER method, and fsl format are accepted
         * 
         * @param gradientFilePath
         * @return
         */
        public boolean readBVGradBMatfile(final String gradientFilePath) {
        	String lineString = null;
            String[] parseString;
            boolean okay;
            int numVars;
            int numFound;
            double bMat[][][] = null;
            boolean bMatOkay;
            int index0;
            int index1;
            int index2;
            boolean gradientsOkay = false;
            boolean bValuesOkay = false;
            int i;
            boolean foundEOF[] = new boolean[]{false};
            double bMatrixVals[][] = null;
            double gradients[][] = null;
            double bValues[] = null;
                   
            if (srcTableModel.getRowCount()>0){
                int rowCount = srcTableModel.getRowCount();
                for (i = 0; i < rowCount; i++) {
                    int delRow = (rowCount-i)-1;
                    srcTableModel.removeRow(delRow);                             
                }               
            }

            try {
                String str;
                final File file = new File(gradientFilePath);
                textBvalGradFile.setText(gradientFilePath);
                final RandomAccessFile raFile = new RandomAccessFile(file, "r");
                if (gradientFilePath.contains("method")) {
                	raFile.seek(0);
                	lineString = readLine(raFile, foundEOF);

                    while (lineString != null) {
                        parseString = parse(lineString);

                        if (parseString[0].equalsIgnoreCase("##$PVM_DwBMat")) {
                        	okay = true;
                        	if (parseString.length == 6) {
            	                if (parseString[1].equals("(")) {
            	                	Preferences.debug("For PVM_DwBMat parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
            	                }
            	                else
            	                {
            	                	Preferences.debug("For PVM_DwBMat parseString[1] unexpectedly == " + parseString[1] + "\n", 
            	                			Preferences.DEBUG_FILEIO);
            	                	okay = false;
            	                }
            	                if (okay) {
            		                if (parseString[2].endsWith(",")) {
            		                    numVolumes = Integer.valueOf(parseString[2].substring(0,parseString[2].length()-1));
            		                    Preferences.debug("For PVM_DwBMat numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
            		                }
            		                else {
            		                	Preferences.debug("For PVM_DwBMat parseString[2] unexpectedly == " + parseString[2] + "\n",
            		                			Preferences.DEBUG_FILEIO);
            		                	okay = false;
            		                }
            	                }
            	                if (okay) {
            		                if (parseString[3].equals("3,")) {
            		                    Preferences.debug("For PVM_DwBMat parseString[3] equals '3,', as expected\n", Preferences.DEBUG_FILEIO);			
            		                }
            		                else {
            		                	Preferences.debug("For PVM_DwBMat parseString[3] unexpectedly == " + parseString[3] + "\n",
            		                			          Preferences.DEBUG_FILEIO);
            		                	okay = false;
            		                }
            	                }
            	                if (okay) {
            	                	if (parseString[4].equals("3")) {
            		                    Preferences.debug("For PVM_DwBMat parseString[4] equals 3, as expected\n", Preferences.DEBUG_FILEIO);			
            		                }
            		                else {
            		                	Preferences.debug("For PVM_DwBMat parseString[4] unexpectedly == " + parseString[4] + "\n",
            		                			          Preferences.DEBUG_FILEIO);
            		                	okay = false;
            		                }	
            	                }
            	                if (okay) {
            	                    if (parseString[5].equals(")")) {
            	                        Preferences.debug("For PVM_DwMat parseString[5] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
            	                    }
            	                    else {
            	                    	Preferences.debug("For PVM_DwBMat parseString[5] unexpectedly == " + parseString[5] + "\n",
            	                    			Preferences.DEBUG_FILEIO);
            		                	okay = false;	
            	                    }
            	                }
                        	}
                        	else {
                        		Preferences.debug("For PVM_DwBMat parseString.length unexpectedly == " + parseString.length + "\n",
                        				          Preferences.DEBUG_FILEIO);
                        		okay = false;
                        	}
                        	if (okay) {
                                numVars = 9 * numVolumes;
                        		numFound = 0;
                        		bMat = new double[numVolumes][3][3];
                        		index0 = 0;
                        		index1 = 0;
                        		index2 = 0;
                        		bMatOkay = true;
                        		while ((numFound < numVars) && (lineString != null) && bMatOkay) {
                        			lineString = readLine(raFile, foundEOF);
                        			if (lineString != null) {
                        			    parseString = parse(lineString);
                        			    for (i = 0; i < parseString.length && bMatOkay; i++) {
                        			    	try {
                        			    	    bMat[index0][index1][index2] = Double.valueOf(parseString[i]);
                        			    	}
                        			    	catch(NumberFormatException nfe) {
                                                Preferences.debug("bMat[" + index0 + "][" + index1 + "][" + index2 + "] could not be read.",
                                                		Preferences.DEBUG_FILEIO);
                                                bMatOkay = false;
                                            }
                        			    	if (bMatOkay) {
                        			    		numFound++;
                        			    		if (index2 < 2) {
                        			    			index2++;
                        			    		}
                        			    		else if (index1 < 2) {
                        			    			index2 = 0;
                        			    			index1++;
                        			    		}
                        			    		else {
                        			    			index2 = 0;
                        			    			index1 = 0;
                        			    			index0++;
                        			    		}
                        			    	}
                        			    }
                        			} // if (lineString != null)
                        			else {
                        				Preferences.debug("For PVM_DwBMat lineString == null while numFound == " + numFound + 
                        						           " and numVars = " + numVars + "\n", Preferences.DEBUG_FILEIO);
                        				bMatOkay = false;
                        			}
                        		} // while ((numFound < numVars) && (lineString != null) && bMatOkay)
                        		if (numFound == numVars) {
                        			for (i = 0; i < numVolumes && bMatOkay; i++) {
                        			    if (bMat[i][0][1] != bMat[i][1][0]) {
                        			    	Preferences.debug("bMat[" + i + "][0][1] = " + bMat[i][0][1] + " but bMat["+i+"][1][0] = " +
                        			                           bMat[i][1][0] + "\n", Preferences.DEBUG_FILEIO);
                        			    	bMatOkay = false;
                        			    }
                        			    if (bMat[i][0][2] != bMat[i][2][0]) {
                        			    	Preferences.debug("bMat[" + i + "][0][2] = " + bMat[i][0][2] + " but bMat["+i+"][2][0] = " +
                        			                           bMat[i][2][0] + "\n", Preferences.DEBUG_FILEIO);
                        			    	bMatOkay = false;
                        			    }
                        			    if (bMat[i][1][2] != bMat[i][2][1]) {
                        			    	Preferences.debug("bMat[" + i + "][1][2] = " + bMat[i][1][2] + " but bMat["+i+"][2][1] = " +
                        			                           bMat[i][2][1] + "\n", Preferences.DEBUG_FILEIO);
                        			    	bMatOkay = false;
                        			    }
                        			} // for (i = 0; i < numVolumes && bMatOkay; i++)
                        			if (bMatOkay) {
                        				bMatrixVals = new double[numVolumes][6];
                        				for (i = 0; i < numVolumes; i++) {
                        					bMatrixVals[i][0] = bMat[i][0][0];
                        					bMatrixVals[i][1] = bMat[i][0][1];
                        					bMatrixVals[i][2] = bMat[i][0][2];
                        					bMatrixVals[i][3] = bMat[i][1][1];
                        					bMatrixVals[i][4] = bMat[i][1][2];
                        					bMatrixVals[i][5] = bMat[i][2][2];
                        			    }
                        			} // if (bMatOkay)
                        		} // if (numFound == numVars)
                        	} // if (okay)
                        } // else if (parseString[0].equalsIgnoreCase("##$PVM_DwBMat"))
                        else if (parseString[0].equalsIgnoreCase("##$PVM_DwGradVec")) {
                        	okay = true;
                        	if (parseString.length == 5) {
            	                if (parseString[1].equals("(")) {
            	                	Preferences.debug("For PVM_DwGradVec parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
            	                }
            	                else
            	                {
            	                	Preferences.debug("For PVM_DwGradVec parseString[1] unexpectedly == " + parseString[1] + "\n",
            	                			Preferences.DEBUG_FILEIO);
            	                	okay = false;
            	                }
            	                if (okay) {
            		                if (parseString[2].endsWith(",")) {
            		                    numVolumes = Integer.valueOf(parseString[2].substring(0,parseString[2].length()-1));
            		                    Preferences.debug("For PVM_DwGradVec numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
            		                }
            		                else {
            		                	Preferences.debug("For PVM_DwGradVec parseString[2] unexpectedly == " + parseString[2] + "\n",
            		                			Preferences.DEBUG_FILEIO);
            		                	okay = false;
            		                }
            	                }
            	                if (okay) {
            	                	if (parseString[3].equals("3")) {
            		                    Preferences.debug("For PVM_DwGradVec parseString[3] equals 3, as expected\n", Preferences.DEBUG_FILEIO);			
            		                }
            		                else {
            		                	Preferences.debug("For PVM_DwGradVec parseString[3] unexpectedly == " + parseString[3] + "\n",
            		                			          Preferences.DEBUG_FILEIO);
            		                	okay = false;
            		                }	
            	                }
            	                if (okay) {
            	                    if (parseString[4].equals(")")) {
            	                        Preferences.debug("For PVM_DwGradVec parseString[4] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
            	                    }
            	                    else {
            	                    	Preferences.debug("For PVM_DwGradVec parseString[4] unexpectedly == " + parseString[4] + "\n",
            	                    			Preferences.DEBUG_FILEIO);
            		                	okay = false;	
            	                    }
            	                }
                        	}
                        	else {
                        		Preferences.debug("For PVM_DwGradVec parseString.length unexpectedly == " + parseString.length + "\n",
                        				          Preferences.DEBUG_FILEIO);
                        		okay = false;
                        	}
                        	if (okay) {
                                numVars = 3 * numVolumes;
                        		numFound = 0;
                        		gradients = new double[numVolumes][3];
                        		index0 = 0;
                        		index1 = 0;
                        		gradientsOkay = true;
                        		while ((numFound < numVars) && (lineString != null) && gradientsOkay) {
                        			lineString = readLine(raFile, foundEOF);
                        			if (lineString != null) {
                        			    parseString = parse(lineString);
                        			    for (i = 0; i < parseString.length && gradientsOkay; i++) {
                        			    	try {
                        			    	    gradients[index0][index1] = Double.valueOf(parseString[i]);
                        			    	}
                        			    	catch(NumberFormatException nfe) {
                                                Preferences.debug("gradients[" + index0 + "][" + index1 + "] could not be read.",
                                                		Preferences.DEBUG_FILEIO);
                                                gradientsOkay = false;
                                            }
                        			    	if (gradientsOkay) {
                        			    		numFound++;
                        			    		if (index1 < 2) {
                        			    			index1++;
                        			    		}
                        			    		else {
                        			    			index1 = 0;
                        			    			index0++;
                        			    		}
                        			    	}
                        			    }
                        			} // if (lineString != null)
                        			else {
                        				Preferences.debug("For PVM_DwGradVec lineString == null while numFound == " + numFound + 
                        						           " and numVars = " + numVars + "\n", Preferences.DEBUG_FILEIO);
                        				gradientsOkay = false;
                        			}
                        		} // while ((numFound < numVars) && (lineString != null) && gradientsOkay)
                        		if (numFound < numVars) {
                        			gradientsOkay = false;
                        		}
                        	} // if (okay)
                        } // else if (parseString[0].equalsIgnoreCase("##$PVM_DwGradVec"))
                        else if (parseString[0].equalsIgnoreCase("##$PVM_DwEffBval")) {
                        	okay = true;
                        	if (parseString.length == 4) {
            	                if (parseString[1].equals("(")) {
            	                	Preferences.debug("For PVM_DwEffBval parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
            	                }
            	                else
            	                {
            	                	Preferences.debug("For PVM_DwEffBval parseString[1] unexpectedly == " + parseString[1] + "\n",
            	                			Preferences.DEBUG_FILEIO);
            	                	okay = false;
            	                }
            	                if (okay) {
            		                numVolumes = Integer.valueOf(parseString[2]);
            		                Preferences.debug("For PVM_DwEffBval numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
            	                }
            	                if (okay) {
            	                    if (parseString[3].equals(")")) {
            	                        Preferences.debug("For PVM_DwEffBval parseString[3] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
            	                    }
            	                    else {
            	                    	Preferences.debug("For PVM_DwEffBval parseString[3] unexpectedly == " + parseString[3] + "\n",
            	                    			Preferences.DEBUG_FILEIO);
            		                	okay = false;	
            	                    }
            	                }
                        	}
                        	else {
                        		Preferences.debug("For PVM_DwEffBval parseString.length unexpectedly == " + parseString.length + "\n",
                        				          Preferences.DEBUG_FILEIO);
                        		okay = false;
                        	}
                        	if (okay) {
                        		numFound = 0;
                        		bValues = new double[numVolumes];
                        		bValuesOkay = true;
                        		while ((numFound < numVolumes) && (lineString != null) && bValuesOkay) {
                        			lineString = readLine(raFile, foundEOF);
                        			if (lineString != null) {
                        			    parseString = parse(lineString);
                        			    for (i = 0; i < parseString.length && bValuesOkay; i++) {
                        			    	try {
                        			    	    bValues[numFound] = Double.valueOf(parseString[i]);
                        			    	}
                        			    	catch(NumberFormatException nfe) {
                                                Preferences.debug("bValues[" + numFound + "] could not be read.",
                                                		Preferences.DEBUG_FILEIO);
                                                bValuesOkay = false;
                                            }
                        			    	if (bValuesOkay) {
                        			    		numFound++;
                        			    	}
                        			    }
                        			} // if (lineString != null)
                        			else {
                        				Preferences.debug("For PVM_DwEffBval lineString == null while numFound == " + numFound + 
                        						           " and numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
                        				bValuesOkay = false;
                        			}
                        		} // while ((numFound < numVOlumes) && (lineString != null) && bValuesOkay)
                        		if (numFound < numVolumes) {
                        			bValuesOkay = false;
                        		} // if (numFound < numVolumes)
                        	} // if (okay)
                        } // else if (parseString[0].equalsIgnoreCase("##$PVM_DwEffBval"))

                        lineString = readLine(raFile, foundEOF);
                    } // while (lineString != null)
                    for (int j = 0; j < numVolumes; j++) {
                        final Vector<String> rowData = new Vector<String>();
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        srcTableModel.addRow(rowData);
                    }
                   
                    if (bMatrixVals != null) {
                    	//Change table indentifiers for bmatrix file
                        isBmatFile = true;
                        java.lang.Object[] newColIdentifiers = {"Volume","bxx","bxy", "bxz", "byy", "byz", "bzz"};
                        srcTableModel.setColumnIdentifiers(newColIdentifiers);  		
                    }
                   
                    for (i = 0; i < numVolumes; i++) {
                        if ( ((String) srcTableModel.getValueAt(i, 3)).trim().equals("")) {
                        	srcTableModel.setValueAt(String.valueOf(i),i,0);
                        	if (isBmatFile) {
	                        	//MIPAV Standard bmatrix text file format
	                            srcTableModel.setValueAt(String.valueOf(bMatrixVals[i][0]), i, 1);
	                            srcTableModel.setValueAt(String.valueOf(bMatrixVals[i][1]), i, 2);
	                            srcTableModel.setValueAt(String.valueOf(bMatrixVals[i][2]), i, 3);
	                            srcTableModel.setValueAt(String.valueOf(bMatrixVals[i][3]), i, 4);
	                            srcTableModel.setValueAt(String.valueOf(bMatrixVals[i][4]), i, 5);
	                            srcTableModel.setValueAt(String.valueOf(bMatrixVals[i][5]), i, 6); 
                        	} // if (isBMatFile)
                        	else {
                        		if (bValuesOkay) {
                        		    srcTableModel.setValueAt(String.valueOf(bValues[i]), i , 1);	
                        		}
                        		if (gradientsOkay) {
                        			srcTableModel.setValueAt(String.valueOf(gradients[i][0]), i, 2);
                        			srcTableModel.setValueAt(String.valueOf(gradients[i][1]), i, 3);
                        			srcTableModel.setValueAt(String.valueOf(gradients[i][2]), i, 4);
                        		}
                        	}
                        } // if ( ((String) srcTableModel.getValueAt(i, 3)).trim().equals("")
                    } // for (i = 0; i < numVolumes; i++)
                } // if (gradientFilePath.contains("method"))
                else { // gradientFilePath does not contain "method"
	                String firstLine = raFile.readLine();
	                if (firstLine.contains(":")) {
	                    String line;
	                    int lineCount = 0;
	                    // counts number of lines in file
	                    while ( (line = raFile.readLine()) != null) {
	                        lineCount++;
	                    }
	                    numVolumes = m_kDWIImage.getExtents()[3];
	
	                    raFile.seek(0);
	                    
	                    // this is DTI Studio and MIPAV standard file format                   
	                    for (int j = 0; j < numVolumes; j++) {
	                        final Vector<String> rowData = new Vector<String>();
	                        rowData.add("");
	                        rowData.add("");
	                        rowData.add("");
	                        rowData.add("");
	                        rowData.add("");
	                        srcTableModel.addRow(rowData);
	                    }
	                    str = raFile.readLine();
	                    final String[] arrCheck = str.split(":");
	                    if(arrCheck.length ==2){
	                        final String gradCheck = arrCheck[1].trim();
	                        final String[]arr2check = gradCheck.split("\\s+");
	                        if(arr2check.length==6){
	                            //Change table indentifiers for bmatrix file
	                            isBmatFile = true;
	                            java.lang.Object[] newColIdentifiers = {"Volume","bxx","bxy", "bxz", "byy", "byz", "bzz"};
	                            srcTableModel.setColumnIdentifiers(newColIdentifiers);  
	                        }
	                    }
	
	                    raFile.seek(0);
	                    for (i = 0; i < numVolumes; i++) {
	                        if ( ((String) srcTableModel.getValueAt(i, 3)).trim().equals("")) {
	                            
	                            str = raFile.readLine();
	                            if (!str.equals("")) {
	                                final String[] arr = str.split(":");
	
	                                if (arr.length == 2) {
	                                    // Populate Volume column
	                                    srcTableModel.setValueAt(String.valueOf(i),i,0);
	                                    final String grads = arr[1].trim();
	                                    final String []arr2 = grads.split("\\s+");
	
	                                    if (arr2.length == 4){
	                                        // MIPAV Standard bval/grad text file format
	                                        srcTableModel.setValueAt(arr2[0], i, 1);
	                                        srcTableModel.setValueAt(arr2[1], i, 2);
	                                        srcTableModel.setValueAt(arr2[2], i, 3);
	                                        srcTableModel.setValueAt(arr2[3], i, 4);
	                                    }
	                                    else if (arr2.length == 3) {
	                                     // DTI Studio grad text file format
	                                        srcTableModel.setValueAt(arr2[0], i, 2);
	                                        srcTableModel.setValueAt(arr2[1], i, 3);
	                                        srcTableModel.setValueAt(arr2[2], i, 4); 
	                                        
	                                        if (dtiparams != null && dtiparams.getbValues() != null ){ 
	                                            srcTableModel.setValueAt(String.valueOf(dtiparams.getbValues()[i]), i, 1);
	
	                                        }
	                                        else if (parDTIParams != null && parDTIParams.getbValues() != null ){
	                                             srcTableModel.setValueAt(String.valueOf(parDTIParams.getbValues()[i]), i, 1);
	                                        }                                   
	                                    }
	                                    else if(arr2.length == 6){
	                                        //MIPAV Standard bmatrix text file format
	                                        srcTableModel.setValueAt(arr2[0], i, 1);
	                                        srcTableModel.setValueAt(arr2[1], i, 2);
	                                        srcTableModel.setValueAt(arr2[2], i, 3);
	                                        srcTableModel.setValueAt(arr2[3], i, 4);
	                                        srcTableModel.setValueAt(arr2[4], i, 5);
	                                        srcTableModel.setValueAt(arr2[5], i, 6);                                        
	                                    }
	                                }                               
	                            }
	                        }
	                    }
	
	                    
	
	                } else {
	                 // this is FSL, dcm2nii, and Miscellaneous  text file format 
	
	                    // String line;
	                    try{
	                    int decimalCount = 0;
	                    StringBuffer buffFirstLine = new StringBuffer(firstLine);
	                    int length = buffFirstLine.length();
	                    // count number of decimal points in first line
	                    for (i = 0; i < length; i++) {
	                        char index = buffFirstLine.charAt(i);
	                        if (index == '.') {
	                            decimalCount++;
	                            
	                        }
	                    }
	                    int blineCount = 0;
	                    while (raFile.readLine() != null){
	                        blineCount++;
	                    }
	
	                    raFile.seek(0);
	                    //System.out.println("decimal count: " +decimalCount);
	
	                    if (decimalCount > 4 && decimalCount != 6 ) {
	                        raFile.seek(0);
	                        int lineCount = 0;
	                        while (raFile.readLine() != null){
	                            lineCount++;
	                        }
	                        raFile.seek(0);
	                        numVolumes = m_kDWIImage.getExtents()[3];                       
	
	                        
	                        for (int j = 0; j < numVolumes; j++) {
	                            final Vector<String> rowData = new Vector<String>();
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            srcTableModel.addRow(rowData);
	                            // Populate Volume column
	                            srcTableModel.setValueAt(String.valueOf(j),j,0);
	                        }
	
	                        final int numRows = srcTableModel.getRowCount();
	                        int start = 0;
	                        if(lineCount <5){
	                            for (i = 0; i < numRows; i++) {
	                                if ( ((String) srcTableModel.getValueAt(i, 3)).trim().equals("")) {
	                                    start = i;
	                                    break;
	                                }
	                            }
	    
	                            int k = start;
	                            String firstline = raFile.readLine();
	                            firstline = firstline.trim();
	                            String[] arr = firstline.split("\\s+");
	    
	                            for (final String element : arr) {
	                                if (k < numRows) {
	                                    srcTableModel.setValueAt(element, k, 2);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }
	    
	                            k = start;
	                            String secondLine = raFile.readLine();
	                            secondLine = secondLine.trim();
	                            arr = secondLine.split("\\s+");
	                            for (final String element : arr) {
	                                if (k < numRows) {
	                                    srcTableModel.setValueAt(element, k, 3);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }
	    
	                            k = start;
	                            String thirdlLine = raFile.readLine();
	                            thirdlLine = thirdlLine.trim();
	                            arr = thirdlLine.split("\\s+");
	                            for (final String element : arr) {
	                                if (k < numRows) {                                   
	                                    srcTableModel.setValueAt(element, k, 4);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }     
	                            if (lineCount == 4){
	                                // this is FSL (4 lines the length of numVolumes with gradients and bvalues)
	                                k = start;
	                                String fourthLine = raFile.readLine();
	                                fourthLine = fourthLine.trim();
	                                arr = fourthLine.split("\\s+");
	                                for (final String element : arr) {
	                                    if (k < numRows) {
	                                        srcTableModel.setValueAt(element, k, 1);
	                                        k = k + 1;
	                                    } else {
	                                        break;
	                                    }
	                                }
	                            }
	                            else if (lineCount == 3){
	                               //dcm2nii file text file format (3 lines the length of numVolumes with gradient values) 
	                                if (dtiparams != null && dtiparams.getbValues() != null ){
	                                    for (i = 0; i < numVolumes; i++){
	                                        srcTableModel.setValueAt(String.valueOf(dtiparams.getbValues()[i]), i, 1);
	                                    }
	    
	                                }
	                                else if (parDTIParams != null && parDTIParams.getbValues() != null ){
	                                    for (i = 0; i < numVolumes; i++){
	                                         srcTableModel.setValueAt(String.valueOf(parDTIParams.getbValues()[i]), i, 1);
	                                    }
	    
	                                }
	                                                      
	                            }
	                        }
	                        else if (lineCount == 6){
	                            isBmatFile = true;
	                            //6 lines the length of numVolumes with bmatrix values
	                            java.lang.Object[] newColIdentifiers = {"Volume","bxx","bxy", "bxz", "byy", "byz", "bzz"};
	                            srcTableModel.setColumnIdentifiers(newColIdentifiers);
	                            int k = start;
	                            String firstline = raFile.readLine();
	                            firstline = firstline.trim();
	                            String[] arr = firstline.split("\\s+");
	    
	                            for (final String element : arr) {
	                                if (k < numRows) {
	                                    srcTableModel.setValueAt(element, k, 1);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }    
	                            k = start;
	                            String secondLine = raFile.readLine();
	                            secondLine = secondLine.trim();
	                            arr = secondLine.split("\\s+");
	                            for (final String element : arr) {
	                                if (k < numRows) {
	                                    srcTableModel.setValueAt(element, k, 2);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }    
	                            k = start;
	                            String thirdlLine = raFile.readLine();
	                            thirdlLine = thirdlLine.trim();
	                            arr = thirdlLine.split("\\s+");
	                            for (final String element : arr) {
	                                if (k < numRows) {                                   
	                                    srcTableModel.setValueAt(element, k, 3);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }
	                            k = start;
	                            String fourthLine = raFile.readLine();
	                            fourthLine = fourthLine.trim();
	                            arr = fourthLine.split("\\s+");
	                            for (final String element : arr) {
	                                if (k < numRows) {                                   
	                                    srcTableModel.setValueAt(element, k, 4);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }
	                            k = start;
	                            String fifthLine = raFile.readLine();
	                            fifthLine = fifthLine.trim();
	                            arr = fifthLine.split("\\s+");
	                            for (final String element : arr) {
	                                if (k < numRows) {                                   
	                                    srcTableModel.setValueAt(element, k, 5);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }
	                            k = start;
	                            String sixthLine = raFile.readLine();
	                            sixthLine = sixthLine.trim();
	                            arr = sixthLine.split("\\s+");
	                            for (final String element : arr) {
	                                if (k < numRows) {                                   
	                                    srcTableModel.setValueAt(element, k, 6);
	                                    k = k + 1;
	                                } else {
	                                    break;
	                                }
	                            }                                                      
	                        }
	                    }
	                    else if(decimalCount == 3 || decimalCount == 6 ){
	                        //Miscellaneous text file format- 3 gradients or 6 bmatrix values corresponding 
	                        //to one volume per line (without volume number)
	                        raFile.seek(0);
	                        numVolumes = m_kDWIImage.getExtents()[3];
	                        String firstLineFile = raFile.readLine();
	                        String [] arrfirstLine = firstLineFile.split("\\s+");
	
	                        for (int j = 0; j < numVolumes; j++) {
	                            final Vector<String> rowData = new Vector<String>();
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            srcTableModel.addRow(rowData);
	                            // Populate Volume column
	                            srcTableModel.setValueAt(String.valueOf(j),j,0);
	                        }
	                        
	                        if (arrfirstLine.length == 6){
	                            isBmatFile = true;
	                            java.lang.Object[] newColIdentifiers = {"Volume","bxx","bxy", "bxz", "byy", "byz", "bzz"};
	                            srcTableModel.setColumnIdentifiers(newColIdentifiers);  
	                        }
	                        
	                        raFile.seek(0);
	                        for (i = 0; i < numVolumes; i++){
	                            String grads = raFile.readLine();
	                            grads = grads.trim();
	                            String [] arrGrads = grads.split("\\s+");
	                            if(arrGrads.length == 3){
	                                //gradients
	                                srcTableModel.setValueAt(arrGrads[0], i, 2);
	                                srcTableModel.setValueAt(arrGrads[1], i, 3);
	                                srcTableModel.setValueAt(arrGrads[2], i, 4);
	                                
	                                if (dtiparams != null && dtiparams.getbValues() != null ){ 
	                                    srcTableModel.setValueAt(String.valueOf(dtiparams.getbValues()[i]), i, 1);   
	                                }
	                                else if (parDTIParams != null && parDTIParams.getbValues() != null ){
	                                     srcTableModel.setValueAt(String.valueOf(parDTIParams.getbValues()[i]), i, 1);   
	                                }                           
	                            }
	                            else if(arrGrads.length == 6){
	                                //bmatrix values
	                                srcTableModel.setValueAt(arrGrads[0], i, 1);
	                                srcTableModel.setValueAt(arrGrads[1], i, 2);
	                                srcTableModel.setValueAt(arrGrads[2], i, 3);
	                                srcTableModel.setValueAt(arrGrads[3], i, 4);
	                                srcTableModel.setValueAt(arrGrads[4], i, 5);
	                                srcTableModel.setValueAt(arrGrads[5], i, 6);                                     
	                            }
	                        }
	                                                
	                    }
	                    else if(decimalCount == 0){
	                        raFile.seek(0);
	                        numVolumes = m_kDWIImage.getExtents()[3];
	                        String firstLineFile = raFile.readLine();
	                        String [] arrfirstLine = firstLineFile.split("\\s+");
	
	                        for (int j = 0; j < numVolumes; j++) {
	                            final Vector<String> rowData = new Vector<String>();
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            rowData.add("");
	                            srcTableModel.addRow(rowData);
	                            // Populate Volume column
	                            srcTableModel.setValueAt(String.valueOf(j),j,0);
	                        }
	                        if (arrfirstLine.length == 6){
	                            isBmatFile = true;
	                            java.lang.Object[] newColIdentifiers = {"Volume","bxx","bxy", "bxz", "byy", "byz", "bzz"};
	                            srcTableModel.setColumnIdentifiers(newColIdentifiers);  
	                        }
	                        
	                        raFile.seek(0);
	                        for (i = 0; i < numVolumes; i++){
	                            String grads = raFile.readLine();
	                            grads = grads.trim();
	                            String [] arrGrads = grads.split("\\s+");
	                                if(arrGrads.length == 6){
	                                 //FSL bmatrix values
	                                srcTableModel.setValueAt(arrGrads[0], i, 1);
	                                srcTableModel.setValueAt(arrGrads[1], i, 2);
	                                srcTableModel.setValueAt(arrGrads[2], i, 3);
	                                srcTableModel.setValueAt(arrGrads[0], i, 4);
	                                srcTableModel.setValueAt(arrGrads[1], i, 5);
	                                srcTableModel.setValueAt(arrGrads[2], i, 6);                                                       
	                                }                           
	                        }
	                        
	                    }
	                    }
	                    catch (Exception e){
	                        MipavUtil.displayError("Invalid Bval/Gradient Text File");
	                        
	                    }
	
	                }
                } // else gradientFilePath does not contain "method"
                raFile.close();

            } catch (final Exception e) {

                MipavUtil.displayError("Error reading B-Value/Grad File...DTI Studio, FSL, BRUKER method, and .txt formats are accepted");
                return false;
            }

            return true;
        }
        
        /**
         * DOCUMENT ME!
         *
         * @param   inString  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private String[] parse(String inString) {
            String[] tmpString = new String[50];
            String[] outString;
            int i;
            int sNum = 0;
            int firstEl = 0;

            for (i = 0; i < inString.length(); i++) {

                if ((inString.charAt(i) <= 0x20) || (inString.charAt(i) == '=')) {

                    if (firstEl != i) {
                        tmpString[sNum++] = inString.substring(firstEl, i);
                    }

                    firstEl = i + 1;
                }
            }

            if (firstEl != i) {
                tmpString[sNum++] = inString.substring(firstEl, i);
            }

            if (sNum == 0) {
                outString = new String[1];
                outString[0] = inString;
            } else {
                outString = new String[sNum];

                for (i = 0; i < (sNum); i++) {
                    outString[i] = tmpString[i];
                }
            }

            return outString;

        }
        
        /**
         * Reads lines of the file until a nonnull String results or the end of the file is reached.
         *
         * @return     the line read in
         *
         * @exception  IOException  if there is an error reading the file
         */
        private String readLine(RandomAccessFile raFile, boolean foundEOF[]) throws IOException {
            String tempString = null;

            while ((tempString == null) && (raFile.getFilePointer() < (raFile.length() - 1)) && (!foundEOF[0])) {

                try {
                    tempString = raFile.readLine();
                } catch (EOFException error) {
                    tempString = null;
                    foundEOF[0] = true;
                } catch (IOException error) {
                    throw (error);
                }


                if (tempString != null) {

                    if (tempString.length() == 0) {
                        tempString = null;
                    }
                }
            } // while

            return tempString;
        }
        /**
         * This method creates the B-Value/Gradient file for DTI Tab
         * 
         * @return
         */
        public boolean createBVGradBMatFileTXT() {
            try {
                StringBuffer sb;
                int padLength;
                File bvalGradFile = new File(filebvalGradTxtName);
                FileOutputStream outputStream = new FileOutputStream(bvalGradFile);
                PrintStream printStream = new PrintStream(outputStream);
                
                String firstGrad = "";
                String secondGrad = "";
                String thirdGrad = "";
                String bvalString = "";

                
               
               
                for (int i = 0; i < numVolumes; i++) {
                  if (gradBvalText == 1){
                      //FSL format
                     if(srcTableModel.getColumnCount() > 5){
                          printStream.print(srcTableModel.getValueAt(i,1) + "    " +srcTableModel.getValueAt(i,2) + "    " +srcTableModel.getValueAt(i,3) + "    "+srcTableModel.getValueAt(i,4)
                          + "    " +srcTableModel.getValueAt(i,5) + "    " +srcTableModel.getValueAt(i,6)); 
                          printStream.println(); 
                      }
                      else{
                          firstGrad = firstGrad + srcTableModel.getValueAt(i,2)+ "    ";
                          secondGrad = secondGrad + srcTableModel.getValueAt(i,3)+ "    ";
                          thirdGrad = thirdGrad + srcTableModel.getValueAt(i,4)+ "    ";
                          bvalString = bvalString + srcTableModel.getValueAt(i,1) + "    " ;

                      }
                        
                  }
                    else if (gradBvalText == 2){
                        // dtiStudio format grad file
                        printStream.print((i+1) +":" + "\t" +srcTableModel.getValueAt(i,2) + "    "+srcTableModel.getValueAt(i,3) + "    "+srcTableModel.getValueAt(i,4));              
                        printStream.println();
                    }
                    else if (gradBvalText == 3){
                        //Standard MIPAV format
                        if(srcTableModel.getColumnCount() > 5){
                            //Bmatrix file
                            printStream.print((i) +":" + "\t" +srcTableModel.getValueAt(i,1) + "    " +srcTableModel.getValueAt(i,2) + "    " +srcTableModel.getValueAt(i,3) + "    "+srcTableModel.getValueAt(i,4)
                                    + "    " +srcTableModel.getValueAt(i,5) + "    " +srcTableModel.getValueAt(i,6));                  
                            printStream.println();  
                        }
                        else{
                            //Bval/grad file
                            printStream.print((i) +":" + "\t" +srcTableModel.getValueAt(i,1) + "    " +srcTableModel.getValueAt(i,2) + "    " +srcTableModel.getValueAt(i,3) + "    "+srcTableModel.getValueAt(i,4));                  
                            printStream.println();
                        }
                        
                    }
                   
                }
                if(!firstGrad.equals("")){
                    printStream.println(firstGrad);
                    printStream.println(secondGrad);
                    printStream.println(thirdGrad);
                    printStream.println(bvalString);
                }

                outputStream.close();
        
            } catch(Exception e) {
                Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("! ERROR: Creation of bvalueGrad file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
                if (outputTextArea != null) {
                    outputTextArea.append("! ERROR: " + e.toString() + "\n");
                    outputTextArea.append("! ERROR: Creation of bvalueGrad file file failed....exiting algorithm \n");
                }
                return false;
            }
            
            
            Preferences.debug(" - bvalueGrad file created : " + "dtiStudio.txt", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append(" - bvalueGrad file created : " +"dtiStudio.txt" + " \n");
            }
            
            return true;
                
            }
        
 
        
        private HashMap<String,String> buildParVolMap() {
            HashMap<String,String> map = new HashMap<String,String>();
            map.put(".    Patient name","info_patient_name");
            map.put(".    Examination name","scn_exam_name");
            map.put(".    Protocol name","scn_protocol_name");
            map.put(".    Examination date/time","info_exam_datetime");
            map.put(".    Acquisition nr","scn_acquisitin_num");
            map.put(".    Reconstruction nr","scn_recon_num");
            map.put(".    Scan Duration [sec]","scn_scan_dur");
            map.put(".    Max. number of cardiac phases","max_card_phs");
            map.put(".    Max. number of echoes","max_num_echo");
            map.put(".    Max. number of slices/locations","max_num_slices");
            map.put(".    Max. number of dynamics","max_num_dynamics");
            map.put(".    Max. number of mixes","max_num_mixes");
            map.put(".    Image pixel size [8 or 16 bits]","scn_pix_bits");
            map.put(".    Technique","scn_technique");
            map.put(".    Scan mode","scn_scan_mode");
            map.put(".    Scan resolution  (x, y)","scn_scan_res");
            map.put(".    Scan percentage","scn_scan_pct");
            map.put(".    Recon resolution (x, y)","scn_recon_res");
            map.put(".    Number of averages","scn_NEX");
            map.put(".    Repetition time [msec]","scn_rep_time");
            map.put(".    FOV (ap,fh,rl) [mm]","scn_fov");
            map.put(".    Slice thickness [mm]","scn_slicethk");
            map.put(".    Slice gap [mm]","scn_slicegap");
            map.put(".    Water Fat shift [pixels]","scn_water_fat_shift");
            map.put(".    Angulation midslice(ap,fh,rl)[degr]","orient_ang_midslice");
            map.put(".    Off Centre midslice(ap,fh,rl) [mm]","orient_off_ctr_midslice");
            map.put(".    Flow compensation <0=no 1=yes> ?","special_flow_comp");
            map.put(".    Presaturation     <0=no 1=yes> ?","special_presatuaration");
            map.put(".    Cardiac frequency","cardiac_cardiac_freq");
            map.put(".    Min. RR interval","cardiac_min_rr_int");
            map.put(".    Max. RR interval","cardiac_max_rr_int");
            map.put(".    Phase encoding velocity [cm/sec]","cardiac_phase_enc_vel");
            map.put(".    MTC               <0=no 1=yes> ?","special_mtc");
            map.put(".    SPIR              <0=no 1=yes> ?","special_spir");
            map.put(".    EPI factor        <0,1=no EPI>","special_epi_factor");
            map.put(".    TURBO factor      <0=no turbo>","special_turbo_factor");
            map.put(".    Dynamic scan      <0=no 1=yes> ?","special_dynamic_scan");
            map.put(".    Diffusion         <0=no 1=yes> ?","diffusion_diffusion");
            map.put(".    Diffusion echo time [msec]","diffusion_diffusion_echo");
            map.put(".    Inversion delay [msec]","special_inversion_delay");
    //... % Variables for May 31, 2005
            map.put(".    Series Type","scn_series_type");
            map.put(".    Patient position","orient_patient_pos");
            map.put(".    Preparation direction","orient_prep_dir");
            map.put(".    Repetition time [ms]","scn_rep_time");
            map.put(".    Diffusion echo time [ms]","special_diffusion_echo_time");
    //... % Variables for December 29, 2006 (release 2.1)
            map.put(".    Max. number of diffusion values","special_max_num_diffusion_values");
            map.put(".    Max. number of gradient orients","special_max_num_gradient_orients");
    //...%Variables for Feb 12, 2008
            map.put(".    Number of label types   <0=no ASL>", "special_num_of_label_types");
            return map;
        }
        
        private HashMap<String,Integer> buildParSliceMap() {
            HashMap<String,Integer> map = new HashMap<String,Integer>();
            map.put("#  slice number                             (integer)",new Integer(1));
            map.put("#  echo number                              (integer)",new Integer(1));
            map.put("#  dynamic scan number                      (integer)",new Integer(1));
            map.put("#  cardiac phase number                     (integer)",new Integer(1));
            map.put("#  image_type_mr                            (integer)",new Integer(1));
            map.put("#  scanning sequence                        (integer)",new Integer(1));
            map.put("#  index in REC file (in images)            (integer)",new Integer(1));
            map.put("#  image pixel size (in bits)               (integer)",new Integer(1));
            map.put("#  scan percentage                          (integer)",new Integer(1));
            map.put("#  recon resolution (x y)                   (2*integer)",new Integer(2));
            map.put("#  rescale intercept                        (float)",new Integer(1));
            map.put("#  rescale slope                            (float)",new Integer(1));
            map.put("#  scale slope                              (float)",new Integer(1));
            map.put("#  window center                            (integer)",new Integer(1));
            map.put("#  window width                             (integer)",new Integer(1));
            map.put("#  image angulation (ap,fh,rl in degrees )  (3*float)",new Integer(3));
            map.put("#  image offcentre (ap,fh,rl in mm )        (3*float)",new Integer(3));
            map.put("#  slice thickness (in mm )                 (float)",new Integer(1));
            map.put("#  slice gap (in mm )                       (float)",new Integer(1));
            map.put("#  image_display_orientation                (integer)",new Integer(1));
            map.put("#  slice orientation ( TRA/SAG/COR )        (integer)",new Integer(1));
            map.put("#  fmri_status_indication                   (integer)",new Integer(1));
            map.put("#  image_type_ed_es  (end diast/end syst)   (integer)",new Integer(1));
            map.put("#  pixel spacing (x,y) (in mm)              (2*float)",new Integer(2));
            map.put("#  echo_time                                (float)",new Integer(1));
            map.put("#  dyn_scan_begin_time                      (float)",new Integer(1));
            map.put("#  trigger_time                             (float)",new Integer(1));
            map.put("#  diffusion_b_factor                       (float)",new Integer(1));
            map.put("#  number of averages                       (integer)",new Integer(1));
            map.put("#  image_flip_angle (in degrees)            (float)",new Integer(1));
            map.put("#  cardiac frequency   (bpm)                (integer)",new Integer(1));
            map.put("#  minimum RR-interval (in ms)              (integer)",new Integer(1));
            map.put("#  maximum RR-interval (in ms)              (integer)",new Integer(1));
            map.put("#  TURBO factor  <0=no turbo>               (integer)",new Integer(1));
            map.put("#  Inversion delay (in ms)                  (float)",new Integer(1));
    //... % new columns for December 29, 2006 (release 2.1)
            map.put("#  diffusion b value number    (imagekey!)  (integer)",new Integer(1));
            map.put("#  gradient orientation number (imagekey!)  (integer)",new Integer(1));
            map.put("#  contrast type                            (string)",new Integer(1));
            map.put("#  diffusion anisotropy type                (string)",new Integer(1));
            map.put("#  diffusion (ap, fh, rl)                   (3*float)",new Integer(3));
    //...%new columns for Feb 12, 2008
            map.put("#  label type (ASL)            (imagekey!)  (integer)", new Integer(1));
            return map;
        };


        @Override
        public void algorithmPerformed(AlgorithmBase algorithm) {
            // TODO Auto-generated method stub
            
        }

            
        }
        



