import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.LineBorder;



public class PlugInDialogNEIRetinalRegistration extends JDialogScriptableBase implements AlgorithmInterface {

    /** handle to the algorithm **/
    private PlugInAlgorithmNEIRetinalRegistration alg;
    
    /** path to first image dir **/
    private JTextField ImageDirTextField1; 
    
    /** path to second image dir **/
    private JTextField ImageDirTextField2; 
    

    private JTextField RefTextField; 
    
    private JTextField epsYText;
    
    private JTextField epsBText;
    
    private JTextField dPerPText;
    
    /** output text area **/
    protected JTextArea outputTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane;
    
    /** current directory  **/
    private String currDir = null;
    
    /** study path **/
    private String imagePath1;
    
    /** study path **/
    private String imagePath2;
    
    /** study path **/
    private String refPath;
    
    
    /** browse button 1 **/
    JButton image1PathBrowseButton;
    
    /** browse button 1 **/
    JButton image2PathBrowseButton;
    
    JButton refPathBrowseButton;
    
    ModelImage im;
    
    private JCheckBox toConcat;
    
    
    public PlugInDialogNEIRetinalRegistration() {
        
    }
    
    public PlugInDialogNEIRetinalRegistration(boolean modal) {
        super(modal);
        init();
    }
    
    public void init() {
        setForeground(Color.black);
        setTitle("NEI Retinal Registration" + " v0.5");
        
        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
        
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 1;
        mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel ImageDirLabel1 = new JLabel(" yellow directory : ");
        mainPanel.add(ImageDirLabel1, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 1;
        mainPanelConstraints.gridwidth = 7;
        mainPanelConstraints.insets = new Insets(15,5,15,20);
        ImageDirTextField1 = new JTextField(55);
        mainPanel.add(ImageDirTextField1, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 8;
        mainPanelConstraints.gridy = 1;
        mainPanelConstraints.insets = new Insets(15,5,15,5);
        image1PathBrowseButton = new JButton("Browse");
        image1PathBrowseButton.addActionListener(this);
        image1PathBrowseButton.setActionCommand("imagePath1Browse");
        mainPanel.add(image1PathBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel ImageDirLabel2 = new JLabel(" blue directory : ");
        mainPanel.add(ImageDirLabel2, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.gridwidth = 7;
        mainPanelConstraints.insets = new Insets(15,5,15,0);
        ImageDirTextField2 = new JTextField(55);
        mainPanel.add(ImageDirTextField2, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 8;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.insets = new Insets(15,5,15,5);
        image2PathBrowseButton = new JButton("Browse");
        image2PathBrowseButton.addActionListener(this);
        image2PathBrowseButton.setActionCommand("imagePath2Browse");
        mainPanel.add(image2PathBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel ImageRefLabel = new JLabel(" reference image : ");
        mainPanel.add(ImageRefLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.gridwidth = 7;
        mainPanelConstraints.insets = new Insets(15,5,15,0);
        RefTextField = new JTextField(55);
        mainPanel.add(RefTextField, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 8;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.insets = new Insets(15,5,15,5);
        refPathBrowseButton = new JButton("Browse");
        refPathBrowseButton.addActionListener(this);
        refPathBrowseButton.setActionCommand("imageRef2Browse");
        mainPanel.add(refPathBrowseButton, mainPanelConstraints);
        
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(15,0,5,0);
        JLabel epsYellowLabel = new JLabel(" epsYellow : ");
        mainPanel.add(epsYellowLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(25,70,15,0);
        epsYText = new JTextField(5);
        mainPanel.add(epsYText, mainPanelConstraints);

        mainPanelConstraints.gridx = 3;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(15,150,5,0);
        JLabel epsBlueLabel = new JLabel(" epsBlue : ");
        mainPanel.add(epsBlueLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 4;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(25,210,15,0);
        epsBText = new JTextField(5);
        mainPanel.add(epsBText, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 5;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(15,290,5,0);
        JLabel dPerPLabel = new JLabel(" Degrees/Pixel : ");
        mainPanel.add(dPerPLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 6;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(25,380,15,0);
        dPerPText = new JTextField(5);
        mainPanel.add(dPerPText, mainPanelConstraints);
        
        
        mainPanelConstraints.gridx = 7;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(25,450,15,0);
        toConcat = new JCheckBox("make concated image:   ");
        toConcat.setHorizontalTextPosition(SwingConstants.LEFT);
        mainPanel.add(toConcat, mainPanelConstraints);
        
        
        mainPanelConstraints.anchor = GridBagConstraints.CENTER;
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 5;
        mainPanelConstraints.gridwidth = 9;
        mainPanelConstraints.insets = new Insets(15,85,15,0) ;
        outputTextArea = new JTextArea(15, 60);
        outputTextArea.setEditable(false);
        outputTextArea.setBackground(Color.lightGray);
        outputTextArea.setBorder(new LineBorder(Color.black));
        outputTextArea.setForeground(Color.black);
        scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel.add(scrollPane, mainPanelConstraints);
        
        JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        
        
        pack();
        setResizable(false);
        setVisible(true);
    }
    
    
    
    @Override
    protected void callAlgorithm() {
        outputTextArea.append("*Beginning Retinal Registration* \n");
        
        
        alg = new PlugInAlgorithmNEIRetinalRegistration(imagePath1, imagePath2, outputTextArea, refPath, toConcat.isSelected(), Float.parseFloat(epsYText.getText().trim()), 
                Float.parseFloat(epsBText.getText().trim()), Float.parseFloat(dPerPText.getText().trim()));
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

    @Override
    protected void setGUIFromParams() {
    // TODO Auto-generated method stub

    }

    @Override
    protected void storeParamsFromGUI() throws ParserException {
    // TODO Auto-generated method stub

    }

    public void algorithmPerformed(AlgorithmBase algorithm) {
        if(alg.isCompleted()) {
            alg = null;
            //if OKButton is not null, then the rest are not null
            //we don't want to do these if this algoritm was done via a script
            if(OKButton != null) {
                OKButton.setEnabled(true);
                image1PathBrowseButton.setEnabled(true);
                image2PathBrowseButton.setEnabled(true);
                refPathBrowseButton.setEnabled(true);
            }
            

            outputTextArea.append("*End Retinal Registration* \n");
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
                currDir = chooser.getSelectedFile().getAbsolutePath();
            }
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
            dispose();
        }        
        else if(command.equalsIgnoreCase("ok")) {
            if(ImageDirTextField1.getText().trim().equals("") || ImageDirTextField2.getText().trim().equals("") || RefTextField.getText().trim().equals("")) {
                MipavUtil.displayError("Both Image Directories & a Reference are Required!");
                return;
            }
            if(RefTextField.getText().trim().equals("")){
                MipavUtil.displayError("Reference Image is Required!");
                return;
            }
            if(epsYText.getText().trim().equals("")){
                MipavUtil.displayError("epsYellow is Required!");
                return;
            }
            if(epsBText.getText().trim().equals("")){
                MipavUtil.displayError("epsBlue is Required!");
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
            setimagePaths(ImageDirTextField1.getText().trim(), ImageDirTextField2.getText().trim(), RefTextField.getText().trim());
            callAlgorithm();
        }

         
    }
    
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
        if(!ref.exists()) {
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

