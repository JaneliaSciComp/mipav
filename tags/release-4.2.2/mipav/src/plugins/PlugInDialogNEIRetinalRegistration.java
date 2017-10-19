import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.LineBorder;


/**
 * This is the dialog for the NEI Retinal Registration PlugIn.
 * 
 * @author morseaj
 *
 */
public class PlugInDialogNEIRetinalRegistration extends JDialogScriptableBase implements AlgorithmInterface {

    /** handle to the algorithm **/
    private PlugInAlgorithmNEIRetinalRegistration alg;
    
    
    /** handle to the second algorithm**/
    private PlugInAlgorithmNEIRetinalRegistration2 alg2;
    
    /** path to first image dir **/
    private JTextField ImageDirTextField1; 
    
    /** path to second image dir **/
    private JTextField ImageDirTextField2; 
    
    /** path to second image dir **/
    private JTextField RefTextField;
    
    /** min/man's **/
    private JTextField yminText;
    private JTextField ymaxText;   
    private JTextField bminText; 
    private JTextField bmaxText;
    
    /** user inputs **/
    private JTextField epsYText;
    private JTextField epsBText;
    private JTextField dPerPText;
    private JTextField VOIText;
    private JTextField mpMapText;
    
    /** output text area **/
    protected JTextArea outputTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane;
    
    /** output text area **/
    protected JTextArea outputTextArea2;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane2;
    
    /** current directory  **/
    private String currDir = null;
    
    /** yellow path **/
    private String imagePath1;
    
    /** blue path **/
    private String imagePath2;
    
    /** reference path **/
    private String refPath;
    
    
    /** browse buttons **/
    private JButton image1PathBrowseButton;
    private JButton image2PathBrowseButton;
    private JButton mapBrowseButton;
    private JButton VOIBrowseButton;
    private JButton refPathBrowseButton;

    /** tabed pane **/
    private JTabbedPane tabs;
    
    /** should image be concatnated **/
    private JCheckBox toConcat;
    
    /** is the image already registered **/
    private JCheckBox preReg;
    
    /** are the numbers percents **/
    private boolean autoMinMax;
    
    /** units of min/max values **/
    private JComboBox typeMinMax;
    
    
    public PlugInDialogNEIRetinalRegistration() {
        
    }
    
    public PlugInDialogNEIRetinalRegistration(boolean modal) {
        super(modal);
        init();
    }
    
    public void init() {
        setForeground(Color.black);
        setTitle("NEI Retinal Registration" + " v1.2");
        
        tabs = new JTabbedPane();
        

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 0;
        
        
        //First Tab for registration and MPmap creation
        JPanel mainPanel = new JPanel(new GridBagLayout());
        JPanel mainPanelA = new JPanel(new GridBagLayout());
        JPanel mainPanelB = new JPanel(new GridBagLayout());
        JPanel mainPanelC = new JPanel(new GridBagLayout());
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(5,5,5,0);
        JLabel ImageDirLabel1 = new JLabel(" yellow directory : ");
        mainPanelA.add(ImageDirLabel1, gbc);
        
        gbc.gridx = 1;
        ImageDirTextField1 = new JTextField(55);
        mainPanelA.add(ImageDirTextField1, gbc);
        
        gbc.gridx = 2;
        image1PathBrowseButton = new JButton("Browse");
        image1PathBrowseButton.addActionListener(this);
        image1PathBrowseButton.setActionCommand("imagePath1Browse");
        mainPanelA.add(image1PathBrowseButton, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel ImageDirLabel2 = new JLabel(" blue directory : ");
        mainPanelA.add(ImageDirLabel2, gbc);
        
        gbc.gridx = 1;
        ImageDirTextField2 = new JTextField(55);
        mainPanelA.add(ImageDirTextField2, gbc);
        
        gbc.gridx = 2;
        image2PathBrowseButton = new JButton("Browse");
        image2PathBrowseButton.addActionListener(this);
        image2PathBrowseButton.setActionCommand("imagePath2Browse");
        mainPanelA.add(image2PathBrowseButton, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        JLabel ImageRefLabel = new JLabel(" reference image : ");
        mainPanelA.add(ImageRefLabel, gbc);
        
        gbc.gridx = 1;
        RefTextField = new JTextField(55);
        mainPanelA.add(RefTextField, gbc);
        
        gbc.gridx = 2;
        refPathBrowseButton = new JButton("Browse");
        refPathBrowseButton.addActionListener(this);
        refPathBrowseButton.setActionCommand("imageRef2Browse");
        mainPanelA.add(refPathBrowseButton, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel epsYellowLabel = new JLabel(" epsYellow : ");
        mainPanelB.add(epsYellowLabel, gbc);
     
        gbc.gridx = 1;
        epsYText = new JTextField(5);
        mainPanelB.add(epsYText, gbc);

        gbc.gridx = 2;
        JLabel epsBlueLabel = new JLabel(" epsBlue : ");
        mainPanelB.add(epsBlueLabel, gbc);
        
        
        gbc.gridx = 3;
        epsBText = new JTextField(5);
        mainPanelB.add(epsBText, gbc);
        
        gbc.gridx = 4;
        preReg = new JCheckBox("don't register image:");
        preReg.addActionListener(this);
        preReg.setActionCommand("preReg");
        preReg.setHorizontalTextPosition(SwingConstants.LEFT);
        mainPanelB.add(preReg, gbc);
        
        gbc.gridx = 5;
        toConcat = new JCheckBox("make concatenated  image:");
        toConcat.setHorizontalTextPosition(SwingConstants.LEFT);
        mainPanelB.add(toConcat, gbc);
        

        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel yminLabel = new JLabel(" yMin : ");
        mainPanelB.add(yminLabel, gbc);
        
        gbc.gridx = 1;
        yminText = new JTextField(5);
        mainPanelB.add(yminText, gbc);

        gbc.gridx = 2;
        JLabel ymaxLabel = new JLabel(" yMax : ");
        mainPanelB.add(ymaxLabel, gbc);
        
        gbc.gridx = 3;
        ymaxText = new JTextField(5);
        mainPanelB.add(ymaxText, gbc);
        
        gbc.gridx = 4;
        gbc.anchor = GridBagConstraints.EAST;
        JLabel typeText = new JLabel(" Min/Max Value Type:");
        mainPanelB.add(typeText, gbc);
        
        gbc.gridx = 5;
        gbc.anchor = GridBagConstraints.WEST;
        typeMinMax = new JComboBox();
        typeMinMax.addItem("Percentile");
        typeMinMax.addItem("Intensity");
        mainPanelB.add(typeMinMax, gbc);
        
       
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        JLabel minLabel = new JLabel(" bMin : ");
        mainPanelB.add(minLabel, gbc);
        
        gbc.gridx = 1;
        bminText = new JTextField(5);
        mainPanelB.add(bminText, gbc);

        gbc.gridx = 2;
        JLabel bmaxLabel = new JLabel(" bMax : ");
        mainPanelB.add(bmaxLabel, gbc);
        
        gbc.gridx = 3;
        bmaxText = new JTextField(5);
        mainPanelB.add(bmaxText, gbc);
        
        
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 0;
        gbc.gridy = 0;
        outputTextArea = new JTextArea(10, 60);
        outputTextArea.setEditable(false);
        outputTextArea.setBackground(Color.lightGray);
        outputTextArea.setBorder(new LineBorder(Color.black));
        outputTextArea.setForeground(Color.black);
        scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainPanelC.add(scrollPane, gbc);
        
        mainPanel.add(mainPanelA, gbc);
        gbc.gridy = 1;
        mainPanel.add(mainPanelB, gbc);
        gbc.gridy = 2;
        mainPanel.add(mainPanelC, gbc);
        
        
        JPanel OKCancelHelpPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelHelpPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelHelpPanel.add(cancelButton, BorderLayout.CENTER);
        buildHelpButton();
        helpButton.setActionCommand("help");
        OKCancelHelpPanel.add(helpButton, BorderLayout.EAST);
 
        tabs.addTab("Create MPmaps", null, mainPanel);
        

        JPanel secondPanel = new JPanel(new GridBagLayout());
        JPanel secondPanelA = new JPanel(new GridBagLayout());
        JPanel secondPanelB = new JPanel(new GridBagLayout());
        JPanel secondPanelC = new JPanel(new GridBagLayout());

        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        
        
        // Create Second tab for data creation
        JLabel mapLabel = new JLabel(" mpMap File : ");
        secondPanelA.add(mapLabel, gbc);
        
        gbc.gridx = 1;
        mpMapText = new JTextField(55);
        secondPanelA.add(mpMapText, gbc);
        
        
        gbc.gridx = 2;
        mapBrowseButton = new JButton("Browse");
        mapBrowseButton.addActionListener(this);
        mapBrowseButton.setActionCommand("mapBrowse");
        secondPanelA.add(mapBrowseButton, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel VOILabel = new JLabel(" VOI File : ");
        secondPanelA.add(VOILabel, gbc);
        
        gbc.gridx = 1;
        VOIText = new JTextField(55);
        secondPanelA.add(VOIText, gbc);
        
        gbc.gridx = 2;
        VOIBrowseButton = new JButton("Browse");
        VOIBrowseButton.addActionListener(this);
        VOIBrowseButton.setActionCommand("VOIBrowse");
        secondPanelA.add(VOIBrowseButton, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel dPerPLabel = new JLabel(" Degrees/Pixel : ");
        secondPanelB.add(dPerPLabel, gbc);
        
        gbc.gridx = 1;
        dPerPText = new JTextField(5);
        secondPanelB.add(dPerPText, gbc);
        

        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 0;
        gbc.gridy = 0;
        outputTextArea2 = new JTextArea(10, 60);
        outputTextArea2.setEditable(false);
        outputTextArea2.setBackground(Color.lightGray);
        outputTextArea2.setBorder(new LineBorder(Color.black));
        outputTextArea2.setForeground(Color.black);
        scrollPane2 = new JScrollPane(outputTextArea2, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        secondPanelC.add(scrollPane2, gbc);
        
        secondPanel.add(secondPanelA, gbc);
        gbc.gridy = 1;
        secondPanel.add(secondPanelB, gbc);
        gbc.gridy = 2;
        secondPanel.add(secondPanelC, gbc);
        
        tabs.addTab("Analyze mpMap", null, secondPanel);
        
        getContentPane().add(tabs, BorderLayout.CENTER);
        getContentPane().add(OKCancelHelpPanel, BorderLayout.SOUTH);
        

        
        pack();
        //setSize(876,721);
        setResizable(false);
        setVisible(true);
       
    }
    
    
    
    @Override
    protected void callAlgorithm() {
        
        if (tabs.getSelectedIndex() == 0) //if on registration tab
        { 
            if (typeMinMax.getSelectedIndex()==0)
                autoMinMax = true;
            else
                autoMinMax = false;
            outputTextArea.append("*Beginning Retinal Registration* \n");
            float ymin = -1, ymax = -1,bmin = -1, bmax = -1;
            
            ymin = Float.parseFloat(yminText.getText().trim());
            ymax = Float.parseFloat(ymaxText.getText().trim());
            bmin = Float.parseFloat(bminText.getText().trim());
            bmax = Float.parseFloat(bmaxText.getText().trim());
            
            
            alg = new PlugInAlgorithmNEIRetinalRegistration(imagePath1, imagePath2, outputTextArea, refPath, toConcat.isSelected(), Float.parseFloat(epsYText.getText().trim()), 
                    Float.parseFloat(epsBText.getText().trim()),ymin,ymax, bmin, bmax, preReg.isSelected(), autoMinMax);
            alg.addListener(this);
            
            if (isRunInSeparateThread()) {
    
                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                alg.run();
            }
        }
        else{
            
            OKButton.setEnabled(false);
            VOIBrowseButton.setEnabled(false);
            mapBrowseButton.setEnabled(false);
            tabs.setEnabledAt(0, false);
            
            
            
            
            alg2 = new PlugInAlgorithmNEIRetinalRegistration2(mpMapText.getText().trim(), VOIText.getText().trim(),  
                    Float.parseFloat(dPerPText.getText().trim()), outputTextArea2);
            alg2.addListener(this);
            

            if (isRunInSeparateThread()) {
                
                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (alg2.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                alg2.run();
            }

            
        }
    }
    

    @Override
    protected void setGUIFromParams() {
    // TODO Auto-generated method stub

    }

    @Override
    protected void storeParamsFromGUI() throws ParserException {
    // TODO Auto-generated method stub

    }

    public void algorithmPerformed(AlgorithmBase algorithm) {
        if(alg != null){
            if(alg.isCompleted()) {
                alg = null;
                //if OKButton is not null, then the rest are not null
                //we don't want to do these if this algoritm was done via a script
                if(OKButton != null) {
                    OKButton.setEnabled(true);
                    image1PathBrowseButton.setEnabled(true);
                    image2PathBrowseButton.setEnabled(true);
                    refPathBrowseButton.setEnabled(true);
                    tabs.setEnabledAt(1, true);
                
                }
                
    
                outputTextArea.append("*End Retinal Registration* \n");
            }
        }
        if(alg2 != null){
            
            if(alg2.isCompleted()){
                alg2 = null;
                //if OKButton is not null, then the rest are not null
                //we don't want to do these if this algoritm was done via a script
                if(OKButton != null) {
                    OKButton.setEnabled(true);
                    VOIBrowseButton.setEnabled(true);
                    mapBrowseButton.setEnabled(true);
                    tabs.setEnabledAt(0, true);
                   
                    
                }
                
            }
        }
            
    // TODO Auto-generated method stub

    }

    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        if(command.equalsIgnoreCase("imagePath1Browse")) {
            JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Choose first image directory");
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                ImageDirTextField1.setText(chooser.getSelectedFile().getAbsolutePath());
                currDir = chooser.getSelectedFile().getParent();
            }
        }
        else if(command.equalsIgnoreCase("preReg")){
            refPathBrowseButton.setEnabled(!preReg.isSelected());
            RefTextField.setEnabled(!preReg.isSelected());
            
        }
        else if(command.equalsIgnoreCase("imagePath2Browse")) {
            JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Choose second image directory");
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                ImageDirTextField2.setText(chooser.getSelectedFile().getAbsolutePath());
                currDir = chooser.getSelectedFile().getParent();
            }
        }
        else if(command.equalsIgnoreCase("mapBrowse")) {
            JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setDialogTitle("Choose MPmap");
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                mpMapText.setText(chooser.getSelectedFile().getAbsolutePath());
                currDir = chooser.getSelectedFile().getAbsolutePath();
            }
        }
        else if(command.equalsIgnoreCase("VOIBrowse")) {
            JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setDialogTitle("Choose Fovea VOI.");
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                VOIText.setText(chooser.getSelectedFile().getAbsolutePath());
                currDir = chooser.getSelectedFile().getAbsolutePath();
            }
        }
        else if(command.equalsIgnoreCase("imageRef2Browse")) {
            JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setDialogTitle("Choose Ref Image");
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                RefTextField.setText(chooser.getSelectedFile().getAbsolutePath());
                currDir = chooser.getSelectedFile().getAbsolutePath();
            }
        }
        
        else if(command.equalsIgnoreCase("cancel")) {

            if(alg != null) {
                alg.setThreadStopped(true);
            }
            if(alg2 != null) {
                alg2.setThreadStopped(true);
            }
            dispose();
        }    
        else if(command.equalsIgnoreCase("help")) {
        	 if (tabs.getSelectedIndex() == 0) //if on registration tab
             {
        		MipavUtil.showHelp("CrMPmaps001"); 
             }else {
            	 MipavUtil.showHelp("AnMPmaps002"); 
             }
        }
        else if(command.equalsIgnoreCase("ok")) {
            if (tabs.getSelectedIndex() == 0) //if on registration tab
            {
                if(ImageDirTextField1.getText().trim().equals("") || ImageDirTextField2.getText().trim().equals("") || (RefTextField.getText().trim().equals("") && !preReg.isSelected())) {
                    MipavUtil.displayError("Both Image Directories & a Reference are Required!");
                    return;
                }
                if((typeMinMax.getSelectedIndex() == 0) && ((Float.parseFloat(yminText.getText().trim()) > 100 )||(Float.parseFloat(bminText.getText().trim()) > 100 )||(Float.parseFloat(ymaxText.getText().trim()) > 100 )||(Float.parseFloat(bmaxText.getText().trim()) > 100 ))) {
                    MipavUtil.displayError("Percentile must be below 100!");
                    return;
                }
                if((typeMinMax.getSelectedIndex() == 0) && ((Float.parseFloat(yminText.getText().trim()) < 0 )||(Float.parseFloat(bminText.getText().trim()) < 0)||(Float.parseFloat(ymaxText.getText().trim()) < 0 )||(Float.parseFloat(bmaxText.getText().trim()) < 0 ))) {
                    MipavUtil.displayError("Percentile must be above 0");
                    return;
                }
                if(RefTextField.getText().trim().equals("") && !preReg.isSelected()){
                    MipavUtil.displayError("Reference Image is Required!");
                    return;
                }
                if(epsYText.getText().trim().equals("")){
                    MipavUtil.displayError("epsYellow is Required!");
                    return;
                }
                if((Float.parseFloat(epsYText.getText().trim()) < 0) ||  (Float.parseFloat(epsYText.getText().trim()) > 1)) {
                	MipavUtil.displayError("epsYellow must be between 0 and 1!");
                    return;
                }
                if(epsBText.getText().trim().equals("")){
                    MipavUtil.displayError("epsBlue is Required!");
                    return;
                }
                if((Float.parseFloat(epsBText.getText().trim()) < 0) ||  (Float.parseFloat(epsBText.getText().trim()) > 1)) {
                	MipavUtil.displayError("epsBlue must be between 0 and 1!");
                    return;
                }
                
                boolean success = validateFilePaths();
                if(!success) {
                    MipavUtil.displayError("One or some of the paths provided is not accurate");
                    ImageDirTextField1.setText("");
                    ImageDirTextField2.setText("");
                    RefTextField.setText("");
                    return;
                }
                
                OKButton.setEnabled(false);
                image1PathBrowseButton.setEnabled(false);
                image2PathBrowseButton.setEnabled(false);
                refPathBrowseButton.setEnabled(false);
                tabs.setEnabledAt(1, false);
                setimagePaths(ImageDirTextField1.getText().trim(), ImageDirTextField2.getText().trim(), RefTextField.getText().trim());
            }
            
        
        else{
            if (VOIText.getText().trim().equals("")||mpMapText.getText().trim().equals("")||dPerPText.getText().trim().equals(""))
                {
                MipavUtil.displayError("All Field are Required!");
                return;
                }
            File file1 = new File(VOIText.getText().trim());
            File file2 = new File(mpMapText.getText().trim());
            if(!file1.exists()){
                VOIText.setText("");
                MipavUtil.displayError("VOI location not Valid!");
                return;
            }
            if(!file2.exists()){
                mpMapText.setText("");
                MipavUtil.displayError("MPmap location not Valid!");  
                return;
            }
            try{
            Float.parseFloat(dPerPText.getText().trim());
            }
            catch(NumberFormatException e1){
                MipavUtil.displayError("Degrees per Pixel must be an Integer!");
                dPerPText.setText("");
                return;
            }
            
        }
        
            callAlgorithm();
         
    }
        
    }
    
    
    /**
     * 
     * @return
     */
    public boolean validateFilePaths() {
        File image1File = new File(ImageDirTextField1.getText().trim());
        File image2File = new File(ImageDirTextField2.getText().trim());
        File ref = new File(RefTextField.getText().trim());
        if(!image1File.exists()) {
            return false;
        }
        if(!image2File.exists()) {
            return false;
        }
        if(!ref.exists() && !preReg.isSelected()) {
            return false;
        }
        return true;
        
    }
    

    
    public void setimagePaths(String imagePath1, String imagePath2, String refPath) {
        this.imagePath1 = imagePath1;
        this.imagePath2 = imagePath2;
        this.refPath = refPath;
    }
}
