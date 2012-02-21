package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

    import gov.nih.mipav.model.algorithms.AlgorithmInterface;
    import gov.nih.mipav.model.file.DTIParameters;
    import gov.nih.mipav.model.file.FileIO;
    import gov.nih.mipav.model.file.FileInfoBase;
    import gov.nih.mipav.model.file.FileInfoPARREC;
import gov.nih.mipav.model.structures.CustomHashtable;
    import gov.nih.mipav.model.structures.ModelImage;

    import gov.nih.mipav.view.MipavUtil;
    import gov.nih.mipav.view.Preferences;
    import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewOpenFileUI;
    import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;

import java.awt.BorderLayout;
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
    import java.io.File;
    import java.io.FileOutputStream;
    import java.io.PrintStream;
    import java.io.RandomAccessFile;
    import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Enumeration;
    import java.util.Vector;

import javax.swing.BorderFactory;
    import javax.swing.ButtonGroup;
    import javax.swing.JButton;
    import javax.swing.JCheckBox;
    import javax.swing.JComboBox;
import javax.swing.JComponent;
    import javax.swing.JFileChooser;
    import javax.swing.JLabel;
    import javax.swing.JPanel;
    import javax.swing.JRadioButton;
    import javax.swing.JScrollPane;
    import javax.swing.JTabbedPane;
    import javax.swing.JTable;
    import javax.swing.JTextArea;
    import javax.swing.JTextField;
    import javax.swing.ScrollPaneConstants;
    import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
    import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;

import Jama.Matrix;

    public class JPanelDTIImportData extends JPanel implements ActionListener{
        
        // ~ Instance fields
        // ------------------------------------------------------------------------------------------------    
        /** table to display the src image names. */
        
        /** src image * */
        private ModelImage image;
        
        /** Eigenvector image * */
        //private ModelImage m_kEigenVectorImage;
        
        /** Diffusion Tensor image. */
        public ModelImage m_kDWIImage;
        
        /** T2 image. */
        public ModelImage m_kT2Image;
        
        public JScrollPane scrollPane;
        
        private DTIParameters dtiparams, newDTIparams;
        
        /** main panel * */
        private JPanel mainPanel;
        
        private Font serif12;
        
        private Font serif12b;
        
        /** TextArea of main dialogfor text output.* */
        private JTextArea outputTextArea;
        
        /** grid bag constraints * */
        private GridBagConstraints gbc, gbc2, gbc3;
        
        private JTable srcBvalGradTable;
        
        private JTextField textDWIDataimage;
        
        /** DOCUMENT ME! */
        private JTextField invertedTextField;
          
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
        
        /** number of Volumes in DWI Image */
        private int dwiBrowse;
        
        /**int to determine which format user selects for gradBval test file */
        private int gradBvalText;
            
        /**chooser for save gradBval text dialog */
        private JFileChooser saveGradchooser; 
        
        /** table model for the srcimages. */
        private DefaultTableModel srcTableModel;
        
        /** DOCUMENT ME! */
        private JTextField bValueTextField;

        /** DOCUMENT ME! */
        private JTextField gradientTextField;

        /** DOCUMENT ME! */
        private JTextField fatshiftTextField;

        /** DOCUMENT ME! */
        private JTextField gradResTextField;

        /** DOCUMENT ME! */
        private JTextField gradOPTextField;
        
        /** DOCUMENT ME! */
        private JTextField philRelTextField;
        
        /** DOCUMENT ME! */
        private JTextField patientPosTextField;
        
        /** DOCUMENT ME! */
        private JTextField patientOrientTextField;
        
        /** DOCUMENT ME! */
        private JTextField foldOverTextField;
        
        /** DOCUMENT ME! */
        private JTextField osTextField;
        
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
        private JLabel dwiFileLabel;
        
        /** DOCUMENT ME! */
        private JTextField textDWIimage; 
        
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
        private JCheckBox isDWITableDeleteBox;
        
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
        
        private String openImage = "";

        /** DOCUMENT ME! */
        private JPanel accessoryPanel = new JPanel();
        
        /** DOCUMENT ME! */
        private JCheckBox multiBox = null;
        
        /** DOCUMENT ME! */
        private boolean saveAs = false;
        
        private DTIPipeline pipeline;
        
        private ViewUserInterface ui;
        
        /** current directory * */
        private String currDir = null;
        
        private boolean lastStackFlag = false;
        
        private JPanel srcPanel; 
        
        Vector<String> openImageNames;
        
        public ViewJFrameImage frame;
        
        public ViewJFrameImage t2frame;
        
        private JCheckBox preProcessedBox;

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
                if (m_kDWIImage != null){ 

                  if (dtiparams != null){                   
                               // Populate Gradient column
                      if (srcTableModel.getRowCount() != 0){
                          if (!srcTableModel.getValueAt(0, 1).equals("")){
                              float [] flBvalueArr= new float[numVolumes]; 
                              for (int i = 0; i < numVolumes; i++) {      
                                  flBvalueArr[i]= Float.valueOf((String)srcTableModel.getValueAt(i, 1));
                                  }
                             dtiparams.setbValues(flBvalueArr);
                          }
                          
                          if (!srcTableModel.getValueAt(0, 3).equals("")){
                              float[][] flGradArr = new float[numVolumes][3];
                              for (int i = 0; i < numVolumes; i++) {
                                  if (!srcTableModel.getValueAt(i, 2).equals("")){
                                      flGradArr[i][0]= Float.valueOf((String)srcTableModel.getValueAt(i, 2));
                                      }
                                      else{
                                          flGradArr[i][0]= (float) 0.0;
                                          }
                                  if (!srcTableModel.getValueAt(i, 3).equals("")){
                                      flGradArr[i][1]= Float.valueOf((String)srcTableModel.getValueAt(i, 3));
                                      }
                                      else{
                                          flGradArr[i][1]= (float) 0.0;
                                          }
                                  if (!srcTableModel.getValueAt(i, 4).equals("")){
                                      flGradArr[i][2]= Float.valueOf((String)srcTableModel.getValueAt(i, 4));
                                      }
                                      else{
                                          flGradArr[i][2]= (float) 0.0;
                                      }
                                  }


                              dtiparams.setGradients(flGradArr);
                              System.out.println("bvalssize" +dtiparams.getbValues().length);
                              System.out.println("gradientssize" +dtiparams.getGradients().length);
                          }
                          dtiparams.setNumVolumes(numVolumes);
                          m_kDWIImage.setDTIParameters(dtiparams);
                          
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
                      
                      
                 }
                  
                  else if (dtiparams == null){
                      //System.out.println("numVolumes: " +numVolumes);
                      //System.out.println("srcTablemodelvalue: " +srcTableModel.getValueAt(m_kDWIImage.getExtents()[3]-1, 0));
                      if (m_kDWIImage.getExtents()[3] == numVolumes ||srcTableModel.getValueAt(m_kDWIImage.getExtents()[3], 0).equals("")){
                          numVolumes = m_kDWIImage.getExtents()[3];
                          if (srcTableModel.getRowCount() != 0 ){
                              newDTIparams = new DTIParameters(m_kDWIImage.getExtents()[3]);
                              if (!srcTableModel.getValueAt(0, 1).equals("")){
                                  float [] flBvalueArr= new float[m_kDWIImage.getExtents()[3]]; 
                                  for (int i = 0; i < m_kDWIImage.getExtents()[3]; i++) {
                                      flBvalueArr[i]= Float.valueOf((String)srcTableModel.getValueAt(i, 1));
                                      //System.out.println("flBvalueArr: " +flBvalueArr[i]);
                                      }
                                 newDTIparams.setbValues(flBvalueArr);
                              }
                              
                              if (!srcTableModel.getValueAt(0, 3).equals("")){
                                  float[][] flGradArr = new float[m_kDWIImage.getExtents()[3]][3];
                                  for (int i = 0; i < numVolumes; i++) {
                                      if (!srcTableModel.getValueAt(i, 2).equals("")){
                                          flGradArr[i][0]= Float.valueOf((String)srcTableModel.getValueAt(i, 2));
                                          }
                                          else{
                                              flGradArr[i][0]= (float) 0.0;
                                          }
                                      if (!srcTableModel.getValueAt(i, 3).equals("")){
                                          flGradArr[i][1]= Float.valueOf((String)srcTableModel.getValueAt(i, 3));
                                          }
                                          else{
                                              flGradArr[i][1]= (float) 0.0;
                                          }
                                      if (!srcTableModel.getValueAt(i, 4).equals("")){
                                          flGradArr[i][2]= Float.valueOf((String)srcTableModel.getValueAt(i, 4));
                                          }
                                          else{
                                              flGradArr[i][1]= (float) 0.0;
                                          }
                                      }
                                  
                              
                              newDTIparams.setGradients(flGradArr);
                              }
                              //System.out.println("@"+srcTableModel.getValueAt(1, 1)+"@");
                              if (!srcTableModel.getValueAt(0, 2).equals("")){
                                  //System.out.println("emptystring");
                              }
                              newDTIparams.setNumVolumes(m_kDWIImage.getExtents()[3]);
                              m_kDWIImage.setDTIParameters(newDTIparams);
                              pipeline.nextButton.setEnabled(true);
                              pipeline.nextButton.setActionCommand("next1");
                          }
                          
                  }
                      else{
                          MipavUtil.displayError("Please enter " +m_kDWIImage.getExtents()[3] +" rows of bvalues and gradients"); 
                      }
                  }
                  //System.out.println("flGradArr: " +newDTIparams.getGradients()[1][0]);
                }
                
                
               
                else{
                    MipavUtil.displayError("Please select a 4D DWI dataset");
                }

                

            }
            
            
            
       else if (command.equals("bvalGradBrowse")) {
            dwiBrowse = 1;
            final JFileChooser chooser = new JFileChooser();

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose File");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                try{
                    readBValGradientFile(currDir);
                    DWIButtonPanel.setBorder(highlightTitledBorder("Table Options"));
                    loadTable.setBorder(buildTitledBorder("Upload B-Value/Gradient File"));
                    saveBvalGradButton.setEnabled(true);
                    isDWICellEditBox.setEnabled(true);
                    bvalGradAppButton.setEnabled(true);
                }
                catch (Exception e){
                    DWIButtonPanel.setBorder(buildTitledBorder("Table Options"));
                    bvalGradAppButton.setEnabled(false);
                    saveBvalGradButton.setEnabled(false);
                    isDWICellEditBox.setEnabled(false);
                    loadTable.setBorder(highlightTitledBorder("Upload B-Value/Gradient File"));
                    
                }
            }
            
        }else if (command.equals("DWICellEditSwitch")){
                final boolean en = isDWICellEditBox.isSelected();
                    if (en == true){
                    srcBvalGradTable.setBackground(Color.white);    
                    srcBvalGradTable.setEnabled(true);
                    clearDWITableButton.setEnabled(true);
                    negXCheckBox.setEnabled(true);
                    negYCheckBox.setEnabled(true);
                    negZCheckBox.setEnabled(true);                
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
                       srcBvalGradTable.setBackground(Color.white);
                       srcBvalGradTable.setEnabled(true);
                        for (int i = 0; i < numVolumes; i++) {
                            // Add empty rows based on number of volumes
                            srcTableModel.setValueAt("",i, 0);
                            srcTableModel.setValueAt("",i, 1);
                            srcTableModel.setValueAt("",i, 2);
                            srcTableModel.setValueAt("",i, 3);
                            srcTableModel.setValueAt("",i, 4);                
                            }

                        

            } else if (command.equals("saveBvalGrad")) {
                saveGradchooser = new JFileChooser(ViewUserInterface.getReference().getDefaultDirectory());
                saveGradchooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                saveGradchooser.setDialogTitle("Save B-Value/ Gradient Table to TXT File");
                saveGradchooser.setAccessory(buildSaveGradBvalPanel());
                
                int returnValue = saveGradchooser.showSaveDialog(this);
                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    filebvalGradTxtName = saveGradchooser.getSelectedFile().getPath();
                   if (fslButton.isSelected()){
                      gradBvalText = 1;
                      createBValGradFileTXT();
                   }
                   else if (dtiStudioButton.isSelected()){
                       gradBvalText = 2;
                       createBValGradFileTXT();                  
                   }               
                   else if (mipavStandardButton.isSelected()){
                       gradBvalText = 3;
                       createBValGradFileTXT();                  
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
                //System.out.println("openedImage");
                if (activeDWIButton.isSelected()){
                //dwiFileLabel.setForeground(Color.lightGray);
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
                catch (Exception e){
                    MipavUtil.displayError("Error loading DWI File");
                }
                
            }else if (command.equals("SkipT2")) {
                if (useT2CheckBox.isSelected()){
                    t2FileLabel.setEnabled(false);
                    textT2image.setEnabled(false);
                    openT2Button.setEnabled(false);
                    useT2CheckBox.setEnabled(false);
                    bvalGradFileLabel.setEnabled(true);
                    loadBValGradFileButton.setEnabled(true);
                    t2OpenPanel.setBorder(buildTitledBorder("Use Structural Image as Reference Space (optional)"));
                    if (dtiparams != null){ 
                        DWIButtonPanel.setBorder(highlightTitledBorder("Table Options"));
                        bvalGradAppButton.setEnabled(true);
                        saveBvalGradButton.setEnabled(true);
                        isDWICellEditBox.setEnabled(true);
                    }
                    else{
                        loadTable.setBorder(highlightTitledBorder("Upload B-Value/Gradient File"));  
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
                    loadTable.setBorder(buildTitledBorder("Upload B-Value/Gradient File"));
                }
                
            }else if (command.equals("browseT2File")) {
                try{
                    loadT2File();
                    if (m_kT2Image.getExtents()[0] == m_kDWIImage.getExtents()[0] && m_kT2Image.getExtents()[1] == m_kDWIImage.getExtents()[1]){
                        textT2image.setText(openFile.getImagePath());
                        t2FileLabel.setEnabled(false);
                        textT2image.setEnabled(false);
                        openT2Button.setEnabled(false);
                        useT2CheckBox.setEnabled(false);
                        bvalGradFileLabel.setEnabled(true);
                        textBvalGradFile.setEnabled(true);
                        loadBValGradFileButton.setEnabled(true);
                        t2OpenPanel.setBorder(buildTitledBorder("Use Structural Image as Reference Space (optional)"));
                        if (dtiparams != null){ 
                            DWIButtonPanel.setBorder(highlightTitledBorder("Table Options"));
                            saveBvalGradButton.setEnabled(true);
                            isDWICellEditBox.setEnabled(true);
                            bvalGradAppButton.setEnabled(true);
                        }
                        else{
                            loadTable.setBorder(highlightTitledBorder("Upload B-Value/Gradient File"));  
                        }
                        
                        pipeline.repaint();
                    }
                    else{
                        MipavUtil.displayError("Structural Image and DWI Image X,Y, and Z extents must be the same");
                        t2OpenPanel.setBorder(highlightTitledBorder("Use Structural Image as Reference Space (optional)"));
                        t2FileLabel.setEnabled(true);
                        textT2image.setEnabled(true);
                        openT2Button.setEnabled(true);
                        useT2CheckBox.setEnabled(true);
                        textBvalGradFile.setEnabled(false);
                        bvalGradFileLabel.setEnabled(false);
                        loadBValGradFileButton.setEnabled(false);
                        DWIButtonPanel.setBorder(buildTitledBorder("Table Options"));
                        
                    }
                }
                catch (Exception e){
                    MipavUtil.displayError("Error loading Structural Image");
                }
        } else if (command.equals("preProcessed")){
            if (preProcessedBox.isSelected()){
                pipeline.tabbedPane.setSelectedIndex(4);
                }
        }
            
        }
              
                      
        
        /**
         * init
         */

        public void init() {
            gbc = new GridBagConstraints();
            gbc2 = new GridBagConstraints();
            gbc3 = new GridBagConstraints();
            


            gbc2 = new GridBagConstraints();
            mainPanel = new JPanel(new GridBagLayout());

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
            
            preProcessedBox = new JCheckBox("Skip Pre-processing");
            preProcessedBox.setActionCommand("preProcessed");
            preProcessedBox.setSelected(false);
            preProcessedBox.setEnabled(true);
            preProcessedBox.addActionListener(this);
            gbc.gridx = 1;
            gbc.gridy = 1;
            //gbc.weightx = 1;
            gbc.insets = new Insets(0, 0, 10, 0);
            gbc.fill = GridBagConstraints.NORTHWEST;
            //BvalGradApply.add(preProcessedBox);
            
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
            loadTable.setBorder(buildTitledBorder("Upload B-Value/Gradient File"));
            
            bvalGradFileLabel = new JLabel("Bvalue/Gradient File: ");
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
            FileInfoPARREC fileInfoPARREC = null;
            try {
                fileInfoPARREC = (FileInfoPARREC) fileInfo;
                isPARREC = true;
            } catch (ClassCastException e) {
                isPARREC = false;
            }
            
            if (dtiparams != null){
                m_kDWIImage.setDTIParameters(dtiparams);
                
                if (dtiparams.getNumVolumes() != 0){
                numVolumes = dtiparams.getNumVolumes();
                
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
                            float[] flBvalArr = dtiparams.getbValues();
                            srcTableModel.setValueAt(String.valueOf(flBvalArr[i]),i,1);
                     }
                    }
                    if (dtiparams.getGradients() != null){ 
                        for (int i = 0; i < numVolumes; i++) {
                             // Populate Gradient column
                             float[][] flGradArr = dtiparams.getGradients();
                             srcTableModel.setValueAt(String.valueOf(flGradArr[i][0]), i, 2);
                             srcTableModel.setValueAt(String.valueOf(flGradArr[i][1]), i, 3);
                             srcTableModel.setValueAt(String.valueOf(flGradArr[i][2]), i, 4);
                            }
                         }
                
                }
            }
   
            if (isPARREC) {
    
                if ( (fileInfoPARREC.getExamName().toUpperCase()).contains("DTI")
                        || (fileInfoPARREC.getProtocolName().toUpperCase()).contains("DTI")) {
    
    
                    if (fileInfoPARREC.getVersion().equals("V3") || fileInfoPARREC.getVersion().equals("V4")) {
                        //Determine if Philips PAR/REC is version 3 or 4 to determine which gradient table dialog to be displayed
                        final JPanel GradCreatorPanel = new JPanel(new GridBagLayout());
                        final GridBagConstraints gbc = new GridBagConstraints();
                        gbc.insets = new Insets(5, 1, 1, 5);
                        gbc.fill = GridBagConstraints.BOTH;
                        
                        //Add all parameters not aquired in PAR file for user to input
                        GradCreatorPanel.setBorder(buildTitledBorder("Gradient Creator Input Parameters"));
                        fatShiftLabel = new JLabel("Fatshift");
                        fatshiftTextField = new JTextField(5);
                        fatshiftBox = new JComboBox();
                        fatshiftBox.setBackground(Color.white);
                        fatshiftBox.addItem("R");
                        fatshiftBox.addItem("L");
                        fatshiftBox.addItem("A");
                        fatshiftBox.addItem("P");
                        fatshiftBox.addItem("H");
                        fatshiftBox.addItem("F");
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
                        gradResTextField = new JTextField(5);
                        gradResBox = new JComboBox();
                        gradResBox.setBackground(Color.white);
                        gradResBox.addItem("Low");
                        gradResBox.addItem("Medium");
                        gradResBox.addItem("High");
                        gbc.gridy = 2;
                        gbc.gridx = 0;
                        GradCreatorPanel.add(gradResLabel,gbc);
                        gbc.gridy = 2;
                        gbc.gridx = 1;
                        GradCreatorPanel.add(gradResBox,gbc);
    
                        gradOPLabel = new JLabel("Gradient Overplus");
                        gradOPTextField = new JTextField(5);
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
                        philRelTextField = new JTextField(5);
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
                        
                        if (fileInfoPARREC.getVersion().equals("V4")){
                          //Add all parameters not aquired in PAR file for user to input
                            osLabel = new JLabel("OS");
                            osLabel.setForeground(Color.lightGray);
                            osTextField = new JTextField(5);
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
                            invertedTextField = new JTextField(5);
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
                        
                        else if (fileInfoPARREC.getVersion().equals("V3")){
                            //Add all parameters not aquired in PAR file for user to input
                            patientPosLabel = new JLabel("Patient Position");
                            patientPosTextField = new JTextField(5);
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
                            patientOrientTextField = new JTextField(5);
                            patientOrientBox = new JComboBox();
                            patientOrientBox.setBackground(Color.white);
                            patientOrientBox.addItem("SP");
                            patientOrientBox.addItem("PR");
                            patientOrientBox.addItem("RD");
                            patientOrientBox.addItem("LD");
                            gbc.gridy = 6;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(patientOrientLabel,gbc);
                            gbc.gridy = 6;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(patientOrientBox,gbc);
                            
                            foldOverLabel = new JLabel("Fold Over");
                            foldOverTextField = new JTextField(5);
                            foldOverBox = new JComboBox();
                            foldOverBox.setBackground(Color.white);
                            foldOverBox.addItem("AP");
                            foldOverBox.addItem("RL");
                            foldOverBox.addItem("FH");
                            gbc.gridy = 7;
                            gbc.gridx = 0;
                            GradCreatorPanel.add(foldOverLabel,gbc);
                            gbc.gridy = 7;
                            gbc.gridx = 1;
                            GradCreatorPanel.add(foldOverBox,gbc);
                                                    
                            osLabel = new JLabel("OS");
                            osLabel.setForeground(Color.lightGray);
                            osTextField = new JTextField(5);
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
                            invertedTextField = new JTextField(5);
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

                if (fileInfoPARREC.getVersion().equals("V3")) {
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
                
                else if (fileInfoPARREC.getVersion().equals("V4")){
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
                if (fileInfoPARREC.getVersion().equals("V3")) {
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
                
                else if (fileInfoPARREC.getVersion().equals("V4")){
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
            FileInfoPARREC fileInfoPARREC = (FileInfoPARREC) fileInfo;

            fatshiftBox.getSelectedItem();
            gradResBox.getSelectedItem();
            gradOPBox.getSelectedItem();
            String philRel = (String) philRelBox.getSelectedItem(); 
            String os = (String) osBox.getSelectedItem(); 
            String inverted = (String) invertedBox.getSelectedItem(); 
            
            String gradResWOP = ((String) gradOPBox.getSelectedItem()) + ((String) gradResBox.getSelectedItem());
            
           
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
                    }else if(numVolumes==35 && os.equals("VMS")){
                        gradCreatetable = getJones30VMS();
                        space = "MPS";
                    }else if(numVolumes==31 && os.equals("Windows")){
                        gradCreatetable  = getJones30();
                        space = "LPH";
                    }
                    else{
                        osLabel.setForeground(Color.red);
                        MipavUtil.displayError("Gradient Table Creator "+"Image dimensions " + numVolumes + " or Operating System "+ os + " are not consistent with gradient table choice - expected 32,35, or 31 dimensions");
                    }
                }
                
                else{
                    MipavUtil.displayError("Gradient Table Creator "+"Jones30 is valid only for the KIRBY scanners");
                }
            }
            
            else {
            if(gradResWOP.equals("YesLow")){
                if(numVolumes==8){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_1.5") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getLowOP();
                        space = "LPH";
                     }
                    else{
                        gradCreatetable = getLowOP2();
                        space = "XYZ";
                    }       
                }           
                else{
                    MipavUtil.displayError("Gradient Table Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient table choice - expected 8 dimensions");
                }            
            }
            
            else if(gradResWOP.equals("YesMedium")){
                if(numVolumes==17){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getMediumOP();
                        space = "LPH";
                     }
                    else{
                        gradCreatetable = getMediumOP2();
                        space = "XYZ";
                    }            
                }           
                else{
                    MipavUtil.displayError("Gradient Table Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient table choice - expected 17 dimensions");
                } 
            } 
            
            else if(gradResWOP.equals("YesHigh")){
                if(numVolumes==34){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0")){
                        gradCreatetable = getHighOP_24prev();
                        space = "LPH";
                     }
                    else if(philRel.equals("Rel_2.5")){
                        gradCreatetable = getHighOP_rel25();
                        space = "LPH";                   
                    }                
                    else{
                        gradCreatetable = getHighOP_25post();
                        space = "XYZ";
                    }           
                }
                else{
                    MipavUtil.displayError("Gradient Table Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient table choice - expected 35 dimensions");
                } 
            }
            else if(gradResWOP.equals("NoLow")){
                if(numVolumes==8){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getLow();
                        space = "MPS";
                     }           
                }
                else{
                    MipavUtil.displayError("Gradient Table Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient table choice - expected 8 dimensions");
                } 
            }
            
            else if(gradResWOP.equals("NoMedium")){
                if(numVolumes==17){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getMedium();
                        space = "MPS";
                     }           
            }
                else{
                    MipavUtil.displayError("Gradient Table Creator "+"Image dimensions " + numVolumes + " are not consistent with gradient table choice - expected 17 dimensions");
                }              
            }
            
            else if(gradResWOP.equals("NoHigh")){
                if(numVolumes==34){
                    if(philRel.equals("Rel_1.5") || philRel.equals("Rel_1.7") || philRel.equals("Rel_2.0") || philRel.equals("Rel_2.1") || philRel.equals("Rel_2.5")){
                        gradCreatetable = getHigh();
                        space = "MPS";
                     }          
                }
                else{
                    MipavUtil.displayError("Gradient Table Creator"+"Image dimensions " + numVolumes + " are not consistent with gradient table choice - expected 34 dimensions");
                }
            }
            
            else{
                MipavUtil.displayError("Gradient Table Creator "+"Could not determine a table!");
            }              
        }
            angulationCorrection(gradCreatetable);   
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
            System.out.println("alengeth: " +A.length);
            System.out.println("b0lengeth: " +B[0].length);
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
            FileInfoPARREC fileInfoPARREC = (FileInfoPARREC) fileInfo;
            angCorrGT=new double[tablein.length][tablein[0].length];
                    
            fileInfoPARREC.getSliceAngulation()[0]=Math.toRadians(fileInfoPARREC.getSliceAngulation()[0]);
            fileInfoPARREC.getSliceAngulation()[1]=Math.toRadians(fileInfoPARREC.getSliceAngulation()[1]);
            fileInfoPARREC.getSliceAngulation()[2]=Math.toRadians(fileInfoPARREC.getSliceAngulation()[2]);
//          ==========================================================
//          TRANSFORMATION DEFINITIONS 
//          ==========================================================

            // Transformations and reverse transformatins that we will use
            // Definitions for these matrices were taken from Philips documentation
            double[][] Tpo;
            double[][] rev_Tpo;
           // System.out.println(fileInfoPARREC.getPatientPosition().toUpperCase());
           // if (fileInfoPARREC.getPatientPosition().toUpperCase().contains("SUPINE")){
           //     System.out.println("supineworking");
                
            //}
            //System.out.println(patientOrientBox.getSelectedItem());
            //patientOrientBox.getSelectedItem()!= null && patientOrientBox.getSelectedItem()=="SP" || ;
            
            if ((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("SUPINE"))|| patientOrientBox.getSelectedItem()=="SP" ){
                Tpo = new double[][]{{1, 0, 0},{0, 1, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{1,0,0},{0,1,0},{0,0,1}};
            }
            else if ((fileInfoPARREC.getPatientPosition()!= null &&  fileInfoPARREC.getPatientPosition().toUpperCase().contains("PRONE")) || patientOrientBox.getSelectedItem()=="PR" ){
                Tpo = new double[][]{{-1, 0, 0},{0, -1, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{-1,0,0},{0,-1,0},{0,0,1}};
            }  
            else if ( (fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("RIGHT")) || patientOrientBox.getSelectedItem()=="RD"){
                Tpo = new double[][]{{0,-1, 0},{1, 0, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{0,1,0},{-1,0,0},{0,0,1}};
            }  
            else if ((fileInfoPARREC.getPatientPosition()!= null &&  fileInfoPARREC.getPatientPosition().toUpperCase().contains("LEFT")) || patientOrientBox.getSelectedItem()=="LD" ){
                Tpo = new double[][]{{0,1, 0},{-1, 0, 0}, {0, 0, 1}};
                rev_Tpo = new double[][]{{0,-1,0},{1,0,0},{0,0,1}};
            }else{
                Tpo=null;
                rev_Tpo=null;
            }

            double[][] Tpp;
            double[][] rev_Tpp;
          
            if ((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("HEADFIRST")) || patientPosBox.getSelectedItem()=="Head First"){
                Tpp = new double[][]{{0, -1, 0},{-1, 0, 0}, {0, 0, 1}};
                rev_Tpp = new double[][]{{0,-1,0},{-1,0,0},{0,0,-1}};
            }
            else if ((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPatientPosition().toUpperCase().contains("FEETFIRST")) || patientPosBox.getSelectedItem()=="Feet First"){
                Tpp = new double[][]{{0, 1, 0},{-1, 0, 0}, {0, 0, -1}};
                rev_Tpp = new double[][]{{0,-1,0},{1,0,0},{0,0,-1}};
            }else{
                Tpp=null;
                rev_Tpp=null;
            }

            double ap = fileInfoPARREC.getSliceAngulation()[0];
            double fh = fileInfoPARREC.getSliceAngulation()[1];
            double rl =fileInfoPARREC.getSliceAngulation()[2];
            
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

            double[][] Tsom;
            double[][] rev_Tsom;
            

            
//          % Definitions for Tsom
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


            double[][] Tprep;
            double[][] rev_Tprep;
            double[][] Tfsd;
            double[][] rev_Tfsd;
            if(fileInfoPARREC.getSliceOrient()== 1){
                
                if((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("ANTERIOR")) || foldOverBox.getSelectedItem()=="AP"){
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
          
                else if( (fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("RIGHT")) || foldOverBox.getSelectedItem()=="RL" ){
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
                    Tprep=null;
                    rev_Tprep=null;
                    Tfsd = null;
                    rev_Tfsd = null;
                }
            }
            else if(fileInfoPARREC.getSliceOrient()== 3){
                if((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("SUPERIOR")) || foldOverBox.getSelectedItem()=="FH"){
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
                else if((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("RIGHT")) || foldOverBox.getSelectedItem()=="RL"){
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
            else if(fileInfoPARREC.getSliceOrient()== 2){
                if((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("SUPERIOR")) || foldOverBox.getSelectedItem()=="FH" ){
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
                else if((fileInfoPARREC.getPatientPosition()!= null && fileInfoPARREC.getPreparationDirection().toUpperCase().contains("ANTERIOR"))||foldOverBox.getSelectedItem()=="AP"){
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

//          % Normalize the non zero vectors
            angCorrGT = normalizeTable(angCorrGT);
            rev_angCorrGT = normalizeTable(rev_angCorrGT);
            
            DecimalFormat twoDForm = new DecimalFormat("#.####");
            /*System.out.println(+Double.valueOf(twoDForm.format(angCorrGT[0][0])));
            System.out.println(+Double.valueOf(twoDForm.format(angCorrGT[0][1])));
            System.out.println(+Double.valueOf(twoDForm.format(angCorrGT[0][2])));
            
            for (int i = 0; i<tablein.length; i++){
            System.out.println("angCorrGT: " +(i+1) + "\t" +Double.valueOf(twoDForm.format(angCorrGT[i][0]))+ "\t" + Double.valueOf(twoDForm.format(angCorrGT[i][1]))+ "\t" + Double.valueOf(twoDForm.format(angCorrGT[i][2])));
            }*/
            //System.out.println("gradCreateTable" +gradCreatetable);
            if (numVolumes==35){
                for (int i = 0; i<tablein.length; i++){
                    srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i][0]))), i, 2);
                    srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i][1]))), i, 3);
                    srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i][2]))), i, 4);
                }
            }
            else if (numVolumes==8 || numVolumes==17 || numVolumes==34 || numVolumes==32){

                float[] flBvalArr = dtiparams.getbValues();
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
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i-bval0Count][0]))), i, 2);
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i-bval0Count][1]))), i, 3);
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i-bval0Count][2]))), i, 4);
                        }
                    
                }
            }
            else if(numVolumes==31){
                float[] flBvalArr = dtiparams.getbValues();
                int bval0Count = 0;
                for (int i = 0; i<numVolumes; i++){
                    System.out.println("i" +i);
                    if (flBvalArr[i]== 0){
                        srcTableModel.setValueAt(0, i, 2);
                        srcTableModel.setValueAt(0, i, 3);
                        srcTableModel.setValueAt(0, i, 4); 
                        bval0Count = 1;
                    }
                    else{
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i-bval0Count][0]))), i, 2);
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i-bval0Count][1]))), i, 3);
                        srcTableModel.setValueAt((String.valueOf(twoDForm.format(rev_angCorrGT[i-bval0Count][2]))), i, 4);
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
            if (m_kDWIImage != null){
                getImageDTIParams();                       
            }
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
         * reads the bval/gradient file...both dti studio format and fsl format are accepted
         * 
         * @param gradientFilePath
         * @return
         */
        public boolean readBValGradientFile(final String gradientFilePath) {

            /*
             * if ((getExamName().toUpperCase()).contains("DTI")){ }
             */
                   
            if (srcTableModel.getRowCount()>0){
                int rowCount = srcTableModel.getRowCount();
                for (int i = 0; i < rowCount; i++) {
                    int delRow = (rowCount-i)-1;
                    srcTableModel.removeRow(delRow);                             
                }
                
            }

            try {
                String str;
                final File file = new File(gradientFilePath);
                textBvalGradFile.setText(gradientFilePath);
                final RandomAccessFile raFile = new RandomAccessFile(file, "r");

                String firstLine = raFile.readLine();
                if (firstLine.contains(":")) {
                    String line;
                    int lineCount = 0;
                    // counts number of lines in file
                    while ( (line = raFile.readLine()) != null) {
                        lineCount++;
                    }
                    numVolumes = lineCount + 1;



                    raFile.seek(0);
                    // this is DTI Studio
                    
                    for (int j = 0; j < numVolumes; j++) {
                        final Vector<String> rowData = new Vector<String>();
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        srcTableModel.addRow(rowData);
                    }
                    final int numRows = srcTableModel.getRowCount();;


                    for (int i = 0; i < numVolumes; i++) {
                        if ( ((String) srcTableModel.getValueAt(i, 3)).trim().equals("")) {
                            
                            str = raFile.readLine();
                            if (str != null) {
                                final String[] arr = str.split(":");

                                if (arr.length == 2) {
                                    // Populate Volume column
                                    srcTableModel.setValueAt(String.valueOf(i),i,0);
                                    final String grads = arr[1].trim();
                                    final String[] arr2 = grads.split("\\s+");
                                    srcTableModel.setValueAt(arr2[0], i, 2);
                                    srcTableModel.setValueAt(arr2[1], i, 3);
                                    srcTableModel.setValueAt(arr2[2], i, 4);
                                }
                            }

                        }

                    }

                } else {
                    // this is FSL

                    // String line;
                    try{
                    //System.out.println("fsl");

                    int decimalCount = 0;
                    StringBuffer buffFirstLine = new StringBuffer(firstLine);
                    int length = buffFirstLine.length();
                    // count number of decimal points in first line
                    for (int i = 0; i < length; i++) {
                        char index = buffFirstLine.charAt(i);
                        if (index == '.') {
                            decimalCount++;
                        }

                    }
                    //System.out.println("decimal count: " +decimalCount);

                    //if (decimalCount > 4) {
                        raFile.seek(0);
                        numVolumes = decimalCount;

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

                        for (int i = 0; i < numRows; i++) {
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
                    catch (Exception e){
                        MipavUtil.displayError("Invalid Bval/Gradient Text File");
                        
                    }

                    /*else {
                        System.out.println("line count = 0");
                        String line;
                        int lineCount = 0;
                        // counts number of lines in file
                        while ( (line = raFile.readLine()) != null) {
                            lineCount++;
                        }
                        numVolumes = lineCount + 1;
                        raFile.seek(0);
                        // this is DTI Studio
                        for (int j = 0; j < numVolumes; j++) {
                            final Vector<String> rowData = new Vector<String>();
                            rowData.add("");
                            rowData.add("");
                            rowData.add("");
                            rowData.add("");
                            rowData.add("");
                            srcTableModel.addRow(rowData);
                        }
                        final int numRows = srcTableModel.getRowCount();

                        for (int i = 0; i < numRows; i++) {
                            if ( ((String) srcTableModel.getValueAt(i, 3)).trim().equals("")) {
                                str = raFile.readLine();
                                if (str != null) {
                                    // Populate Volume column
                                    srcTableModel.setValueAt(String.valueOf(i),i,0);
                                    final String[] arr = str.split("\\s+");
                                    srcTableModel.setValueAt(arr[1], i, 2);
                                    srcTableModel.setValueAt(arr[2], i, 3);
                                    srcTableModel.setValueAt(arr[3], i, 4);

                                }

                            }

                        }

                    }*/

                }
                raFile.close();

            } catch (final Exception e) {

                MipavUtil.displayError("Error reading B-Value/Grad File...DTI Studio, FSL, and .txt formats are accepted");
                return false;
            }

            return true;
        }
        /**
         * This method creates the B-Value/Gradient file for DTI Tab
         * 
         * @return
         */
        public boolean createBValGradFileTXT() {
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
                        firstGrad = firstGrad + srcTableModel.getValueAt(i,2)+ "    ";
                        secondGrad = secondGrad + srcTableModel.getValueAt(i,3)+ "    ";
                        thirdGrad = thirdGrad + srcTableModel.getValueAt(i,4)+ "    ";
                        bvalString = bvalString + srcTableModel.getValueAt(i,1) + "    " ;
                        
                  }
                    else if (gradBvalText == 2){                                 
                        printStream.print((i+1) +":" + "\t" +srcTableModel.getValueAt(i,2) + "    "+srcTableModel.getValueAt(i,3) + "    "+srcTableModel.getValueAt(i,4));              
                        printStream.println();
                    }
                    else if (gradBvalText == 3){
                        printStream.print((i) +":" + "\t" +srcTableModel.getValueAt(i,1) + "    " +srcTableModel.getValueAt(i,2) + "    " +srcTableModel.getValueAt(i,3) + "    "+srcTableModel.getValueAt(i,4));                  
                        printStream.println();
                        
                    }

                   
                }
                printStream.println(firstGrad);
                printStream.println(secondGrad);
                printStream.println(thirdGrad);
                printStream.println(bvalString);
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

            
        }
        



