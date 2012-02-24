package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;

public class JPanelDTIEstimateTensor extends JPanel implements AlgorithmInterface, ActionListener, ItemListener {
    


    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------ 
    private DTIPipeline pipeline;
    
    private ViewUserInterface ui;
    
    /** Mask image for the tensor calculation */
    private ModelImage maskImage = null;
    
    JTextField textDTIimage = new JTextField();
    
    /** Diffusion tensor image: */
    public ModelImage tensorImage = null;
    
    private JComboBox comboBoxDTI_Algorithm;
    
    private Font serif12;

    public JPanel wholeTensorPanel;

    public JLabel maskLabel;

    public JPanel maskOpenPanel;

    public JTextField textMaskimage;

    public JButton openMaskImageButton;

    public JCheckBox skipMaskCheckBox;

    private JButton calcButton;

    private JLabel labelDTIEst;

    private JPanel tensorEstPanel;
    
    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    public JPanelDTIEstimateTensor(DTIPipeline pipeline) {
        super();
        this.pipeline = pipeline;
        ui = ViewUserInterface.getReference();
        //this.image = ModelImage image;
        init();
    }
    
    // ~ Methods
    // --------------------------------------------------------------------------------------------------------
    
    private final static int DEFAULT = 0;
    private final static int LLMSE = 1;
    public final static int LINEAR = 2;
    public final static int NON_LINEAR = 3;
    public final static int RESTORE = 4;
    public final static int WEIGHTED_LINEAR = 5;
    
    public void actionPerformed(ActionEvent event) {
        final String command = event.getActionCommand();
        if ( command.equals("browseMaskFile")){
           loadMaskImage();
                if (maskImage!=null){
                    comboBoxDTI_Algorithm.setEnabled(true);
                    labelDTIEst.setEnabled(true);
                    tensorEstPanel.setBorder(highlightTitledBorder("Choose Tensor Edtimation Algorithm"));
                    calcButton.setEnabled(true);
                    maskOpenPanel.setBorder(buildTitledBorder("Upload Mask Image"));
                    maskLabel.setEnabled(false);
                    openMaskImageButton.setEnabled(false);
                    textMaskimage.setEnabled(false);
                }
                else{                   
                    MipavUtil.displayError("Error loading mask image..."); 
                }                
                       
        }
        
        // pipeline.inputTensorImage is used in all cases if user runs pre-processing or skips it 
        //and runs epi distortion correction or skips it
        else if (command.equals("calcTensor"))
        {
            if (pipeline.inputTensorImage != null){
                switch ( comboBoxDTI_Algorithm.getSelectedIndex() ) {
                case DEFAULT: 
                    AlgorithmDWI2DTI calcDTI = new AlgorithmDWI2DTI( pipeline.inputTensorImage, maskImage );
                    calcDTI.addListener(this);
                    calcDTI.run();
                    break;
                case LLMSE:
                    tensorImage = EstimateTensorLLMSE.estimate( pipeline.inputTensorImage, maskImage, true );
                    finishTensorPanel();
                    break;
                 //Need to add these algorithms to MIPAV
                case LINEAR:
                case NON_LINEAR:
                case RESTORE:
                case WEIGHTED_LINEAR:
                    tensorImage = EstimateTensorLLMSE.estimateCamino( pipeline.inputTensorImage, maskImage, comboBoxDTI_Algorithm.getSelectedIndex() );
                    finishTensorPanel();
                    break;
                }
                pipeline.nextButton.setEnabled(true);
                pipeline.nextButton.setActionCommand("next4");
                
            }
            else{
                MipavUtil.displayError("Error loading input tensor image...");
                pipeline.nextButton.setEnabled(false);
            }
        }
        else if(command.equals("SkipMask")){
            //need to work on this command
        }
        
    }
    
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if ( algorithm instanceof AlgorithmDWI2DTI && algorithm.isCompleted() )
        {
            //tensorImage = EstimateTensorLLMSE.estimate( currentImage, true );
            tensorImage = ((AlgorithmDWI2DTI)algorithm).getDTI();
            finishTensorPanel();
            // delete intermediate images:
            ((AlgorithmDWI2DTI)algorithm).deleteImages();
        }
    }
    
    public void init() {
        
        final GridBagConstraints gbc = new GridBagConstraints();
        
        maskOpenPanel = new JPanel(new GridBagLayout());
        maskOpenPanel.setBorder(buildTitledBorder("Upload Mask Image"));            
        maskLabel = new JLabel("Mask Image: ");
        maskLabel.setFont(serif12);
        maskLabel.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 2, 0, 2);
        maskOpenPanel.add(maskLabel,gbc);
        
        textMaskimage = new JTextField();
        textMaskimage.setPreferredSize(new Dimension(100, 21));
        textMaskimage.setEnabled(false);
        textMaskimage.setBackground(Color.white);
        textMaskimage.setFont(MipavUtil.font12);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.15;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskOpenPanel.add(textMaskimage,gbc);
        
        
        openMaskImageButton = new JButton("Browse");
        openMaskImageButton.setToolTipText("Browse mask image file");
        openMaskImageButton.addActionListener(this);
        openMaskImageButton.setActionCommand("browseMaskFile");
        openMaskImageButton.setEnabled(false);
        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        maskOpenPanel.add(openMaskImageButton,gbc);
        
        //Need to work on this checkbox
        skipMaskCheckBox = new JCheckBox("Skip Perform BET");
        skipMaskCheckBox.setActionCommand("SkipMask");
        skipMaskCheckBox.setSelected(false);
        skipMaskCheckBox.setEnabled(false);
        skipMaskCheckBox.setFont(serif12);
        skipMaskCheckBox.addActionListener(this);
        gbc.gridx = 3;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        maskOpenPanel.add(skipMaskCheckBox,gbc);
        
        tensorEstPanel = new JPanel(new GridBagLayout());
        tensorEstPanel.setBorder(buildTitledBorder("Choose Tensor Estimation Algorithm"));       
        labelDTIEst = new JLabel("DTI Algorithm:");
        labelDTIEst.setEnabled(false);
        labelDTIEst.setForeground(Color.black);
        labelDTIEst.setFont(serif12);
        labelDTIEst.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDTI_Algorithm = new JComboBox();
        comboBoxDTI_Algorithm.setEnabled(false);
        comboBoxDTI_Algorithm.setFont(MipavUtil.font12);
        comboBoxDTI_Algorithm.setBackground(Color.white);
        comboBoxDTI_Algorithm.setToolTipText("Select DTI Algorithm");
        comboBoxDTI_Algorithm.addItem("Weighted, noise-reduction");
        comboBoxDTI_Algorithm.addItem("LLMSE");
        comboBoxDTI_Algorithm.addItem("CAMINO: Linear");
        comboBoxDTI_Algorithm.addItem("CAMINO: Non-Linear");
        comboBoxDTI_Algorithm.addItem("CAMINO: Restore");
        comboBoxDTI_Algorithm.addItem("CAMINO: Weighted Linear");
        comboBoxDTI_Algorithm.setSelectedIndex(0);
        comboBoxDTI_Algorithm.addItemListener(this);
        comboBoxDTI_Algorithm.setActionCommand("comboBoxDTI");
        comboBoxDTI_Algorithm.addActionListener(this);
        

        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 2, 0, 2);
        tensorEstPanel.add(labelDTIEst, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        tensorEstPanel.add(comboBoxDTI_Algorithm, gbc);
        
        final JPanel calcPanel= new JPanel();

        
        calcButton = new JButton("Calculate Tensor");
        calcButton.addActionListener(this);
        calcButton.setActionCommand("calcTensor");
        calcButton.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = new Insets(0, 0, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;
        calcPanel.add(calcButton);
        
        wholeTensorPanel = new JPanel();
        wholeTensorPanel.setLayout(new BoxLayout(wholeTensorPanel, BoxLayout.Y_AXIS));
        wholeTensorPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        maskOpenPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        //maskOpenPanel.setPreferredSize(new Dimension(50, 25));
        maskOpenPanel.setMaximumSize(new Dimension(1000, 75));
        wholeTensorPanel.add(maskOpenPanel);
        
        tensorEstPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        tensorEstPanel.setMaximumSize(new Dimension(1000, 75));
        wholeTensorPanel.add(tensorEstPanel);
        
        calcPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        calcPanel.setMaximumSize(new Dimension(1000, 75));
        wholeTensorPanel.add(calcPanel);
               
    }
    
    private TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }
    

    
    private TitledBorder highlightTitledBorder(String title){
        return new TitledBorder(new LineBorder( Color.black, 2), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }
    
    private void finishTensorPanel()
    {
        // Set up the fiber tracking panel inputs:
        /*tabbedPane.setSelectedIndex(FIBER_TRACKING);
        fiberTrack.setInputImage( tensorImage );
        nextButton.setEnabled(true);
        // save the tensor image
        ModelImage.saveImage( tensorImage, tensorImage.getImageName() + ".xml", tensorImage.getImageDirectory() );
        currentImage = tensorImage;*/
        if ( maskImage != null )
        {
            maskImage.disposeLocal();
            maskImage = null;
        }
    }
    

    public void itemStateChanged(ItemEvent event) 
    {
        if ( event.getSource() == comboBoxDTI_Algorithm )
        {
            //System.err.println( comboBoxDTI_Algorithm.getItemAt( comboBoxDTI_Algorithm.getSelectedIndex() ) );
        }
    }


    private void loadMaskImage() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Diffusion Tensor Color image file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            maskImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);

            textDTIimage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }



}
