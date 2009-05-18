import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;

import javax.swing.*;


/**
 * @version  May 18, 2009
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 
 */
public class PlugInDialogLargeSynapse extends JDialogScriptableBase  {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    
    public static final String BROWSE = "BROWSE";
    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** Minimum number of red pixels in a transition */
    private int redMin = 1;
    
    private JLabel redMinLabel;
    
    private JTextField redMinText;
    
    private int redMax = 20;
    
    private JLabel redMaxLabel;
    
    private JTextField redMaxText;
    
    private int greenMin = 1;
    
    private JLabel greenMinLabel;
    
    private JTextField greenMinText;
    
    private int greenMax = 200;
    
    private JLabel greenMaxLabel;
    
    private JTextField greenMaxText;
    
    private int blueMinXY = 1;
    
    private JLabel blueMinXYLabel;
    
    private JTextField blueMinXYText;
    
    private int blueMaxXY = 20;
    
    private JLabel blueMaxXYLabel;
    
    private JTextField blueMaxXYText;
    
    private int blueMinZ = 1;
    
    private JLabel blueMinZLabel;
    
    private JTextField blueMinZText;
    
    private int blueMaxZ = 20;
    
    private JLabel blueMaxZLabel;
    
    private JTextField blueMaxZText;
    
    private int redIntensity = 5;
    
    private JLabel redIntensityLabel;
    
    private JTextField redIntensityText;
    
    private int redBrightIntensity = 55;
    
    private JLabel redBrightIntensityLabel;
    
    private JTextField redBrightIntensityText;
    
    private int greenIntensity = 5;
    
    private JLabel greenIntensityLabel;
    
    private JTextField greenIntensityText;
    
    private int greenBrightIntensity = 85;
    
    private JLabel greenBrightIntensityLabel;
    
    private JTextField greenBrightIntensityText;
    
    private int blueIntensity = 15;
    
    private JLabel blueIntensityLabel;
    
    private JTextField blueIntensityText;
    
    private int blueBrightIntensity = 80;
    
    private JLabel blueBrightIntensityLabel;
    
    private JTextField blueBrightIntensityText;
    
    private JCheckBox histoInfoCheckBox;
    
    /** If true, provide histograms of red, green, and blue values along detection lines */
    private boolean histoInfo = false;  
    
    /** Text field for the input TIFF file */
    private JTextField inText;
    
    /** Button to browse for the TIFF file */
    private JButton browseButton;
    
    private PlugInLargeSynapse largeSynapsePlugin;
    
    /** Whether the dialog exited successfully */
    private boolean successfulExit = false;
    
    private String inputFileName = null;
    
    private String directory = null;
    
    private JLabel xyProcessLabel;
    
    private JTextField xyProcessText;
    
    /** Length of a processed square within a slice */
    private int xyProcessLength = 500;
    
    private JLabel xyOverlapLabel;
    
    private JTextField xyOverlapText;
    
    /** Overlap length in a processed square in a slice */
    private int xyOverlapLength = 100;
    
    private JLabel zProcessLabel;
    
    private JTextField zProcessText;
    
    /** Height of a processed volume across slices */
    private int zProcessLength = 60;
    
    private JLabel zOverlapLabel;
    
    private JTextField zOverlapText;
    
    /** Overlap of processed volume heights across slices */
    private int zOverlapLength = 12;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogLargeSynapse() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogLargeSynapse(boolean modal, PlugInLargeSynapse largeSynapsePlugin) {
        super(modal);
        this.largeSynapsePlugin = largeSynapsePlugin;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals(BROWSE)) {

            try {
                
                JFileChooser chooser = new JFileChooser();
                chooser.setDialogTitle("Select TIFF file");
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setCurrentDirectory(new File(Preferences.getImageDirectory()));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TIFF));
                chooser.setMultiSelectionEnabled(false);
                
                int returnValue = chooser.showOpenDialog(this);

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    inText.setText(chooser.getSelectedFile().getAbsolutePath());
                    inText.setToolTipText(null);
                    inputFileName = chooser.getSelectedFile().getName();
                    directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                    Preferences.setImageDirectory(chooser.getCurrentDirectory());
                } else {
                    return;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory");

                return;
            }

        }
        else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } 
    }
    
    public String getInputFileName() {
        return inputFileName;
    }
    
    public String getDirectory() {
        return directory;
    }
    
    public boolean isSuccessfulExit() {
        return successfulExit;
    }
    
    public int getRedMin() {
        return redMin;
    }
    
    public int getRedMax() {
        return redMax;
    }
    
    public int getGreenMin() {
        return greenMin;
    }
    
    public int getGreenMax() {
        return greenMax;
    }
    
    public int getBlueMinXY() {
        return blueMinXY;
    }
    
    public int getBlueMaxXY() {
        return blueMaxXY;
    }
    
    public int getBlueMinZ() {
        return blueMinZ;
    }
    
    public int getBlueMaxZ() {
        return blueMaxZ;
    }
    
    public int getRedIntensity() {
        return redIntensity;
    }
    
    public int getRedBrightIntensity() {
        return redBrightIntensity;
    }
    
    public int getGreenIntensity() {
        return greenIntensity;
    }
    
    public int getGreenBrightIntensity() {
        return greenBrightIntensity;
    }
    
    public int getBlueIntensity() {
        return blueIntensity;
    }
    
    public int getBlueBrightIntensity() {
        return blueBrightIntensity;
    }
    
    public boolean getHistoInfo() {
        return histoInfo;
    }
    
    public int getXYProcessLength() {
        return xyProcessLength;
    }
    
    public int getXYOverlapLength() {
        return xyOverlapLength;
    }
    
    public int getZProcessLength() {
        return zProcessLength;
    }
    
    public int getZOverlapLength() {
        return zOverlapLength;
    }
    
    /**
     * Once all the necessary variables are set, call the large synapse detection plugin
     */
    protected void callAlgorithm() {

        try {
        
            setVisible(false); // Hide dialog
            successfulExit = true;
            largeSynapsePlugin.runPlugin();    //continues execution of plugin with successful exit
        
        } catch (OutOfMemoryError x) {
            
            MipavUtil.displayError("PlugInDialogLargeSynapse: unable to allocate enough memory");
            successfulExit = false;
        }

    } // end callAlgorithm()

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += xyProcessLength + delim;
        str += xyOverlapLength + delim;
        str += zProcessLength + delim;
        str += zOverlapLength + delim;
        str += redMin + delim;
        str += redMax + delim;
        str += greenMin + delim;
        str += greenMax + delim;
        str += blueMinXY + delim;
        str += blueMaxXY + delim;
        str += blueMinZ + delim;
        str += blueMaxZ + delim;
        str += redIntensity + delim;
        str += redBrightIntensity + delim;
        str += greenIntensity + delim;
        str += greenBrightIntensity + delim;
        str += blueIntensity + delim;
        str += blueBrightIntensity + delim;
        str += histoInfo;
        
        return str;
    }
    
    /**
     * Accessor that sets xyProcessLength, base side of a block processing volume
     * @param xyProcessLength
     */
    public void setXYProcessLength(int xyProcessLength) {
        this.xyProcessLength = xyProcessLength;
    }
    
    /**
     * Accessor that sets xyOverlapLength, overlap base length of neighboring block
     * @param xyOverlapLength
     */
    public void setXYOverlapLength(int xyOverlapLength) {
        this.xyOverlapLength = xyOverlapLength;
    }
    
    /**
     * Accessor that sets zProcessLength, height of a block processing volume
     * @param zProcessLength
     */
    public void setZProcessLength(int zProcessLength) {
        this.zProcessLength = zProcessLength;
    }
    
    /**
     * Accessor that sets zOverlapLength, overlap height of neighboring block
     * @param zOverlapLength
     */
    public void setZOverlapLength(int zOverlapLength) {
        this.zOverlapLength = zOverlapLength;
    }
    
    /**
     * Accessor that sets the redMin variable, for minimum red pixel width.
     *
     * @param  redMin  minimum number of red pixels
     */
    public void setRedMin(int redMin) {
        this.redMin = redMin;
    }
    
    /**
     * Accessor that sets the redMax variable, for maximum red pixel width.
     *
     * @param  redMax  maximum number of red pixels
     */
    public void setRedMax(int redMax) {
        this.redMax = redMax;
    }
    
    /**
     * Accessor that sets the greenMin variable, for minimum green pixel width.
     *
     * @param  greenMin  minimum number of green pixels
     */
    public void setGreenMin(int greenMin) {
        this.greenMin = greenMin;
    }
    
    /**
     * Accessor that sets the greenMax variable, for maximum green pixel width.
     *
     * @param  greenMax  maximum number of green pixels
     */
    public void setGreenMax(int greenMax) {
        this.greenMax = greenMax;
    }
    
    /**
     * Accessor that sets the blueMinXY variable, for minimum blue pixel width within a slice.
     *
     * @param  blueMinXY  minimum number of blue pixels within one slice
     */
    public void setBlueMinXY(int blueMinXY) {
        this.blueMinXY = blueMinXY;
    }
    
    /**
     * Accessor that sets the blueMaxXY variable, for maximum blue pixel width within a slice.
     *
     * @param  blueMaxXY  maximum number of blue pixels within one slice
     */
    public void setBlueMaxXY(int blueMaxXY) {
        this.blueMaxXY = blueMaxXY;
    }
    
    /**
     * Accessor that sets the blueMinZ variable, for minimum blue pixel width between slices.
     *
     * @param  blueMinZ  minimum number of blue pixels between slices
     */
    public void setBlueMinZ(int blueMinZ) {
        this.blueMinZ = blueMinZ;
    }
    
    /**
     * Accessor that sets the blueMaxZ variable, for maximum blue pixel width between slices.
     *
     * @param  blueMaxZ  maximum number of blue pixels between slices
     */
    public void setBlueMaxZ(int blueMaxZ) {
        this.blueMaxZ = blueMaxZ;
    }
    
    /**
     * Accessor that sets the redIntensity variable, for minimum red intensity over green, blue.
     *
     * @param  redIntensity  minimum red intensity over green, blue
     */
    public void setRedIntensity(int redIntensity) {
        this.redIntensity = redIntensity;
    }
    
    /**
     * Accessor that sets the redBrightIntensity variable, for minimum red BrightIntensity over green, blue.
     *
     * @param  redBrightIntensity  minimum red BrightIntensity over green, blue
     */
    public void setRedBrightIntensity(int redBrightIntensity) {
        this.redBrightIntensity = redBrightIntensity;
    }
    
    /**
     * Accessor that sets the greenIntensity variable, for minimum green intensity over red, blue.
     *
     * @param  green intensity  minimum green intensity over red, blue
     */
    public void setGreenIntensity(int greenIntensity) {
        this.greenIntensity = greenIntensity;
    }
    
    /**
     * Accessor that sets the greenBrightIntensity variable, for minimum green BrightIntensity over red, blue.
     *
     * @param  greenBrightIntensity  minimum green BrightIntensity over red, blue
     */
    public void setGreenBrightIntensity(int greenBrightIntensity) {
        this.greenBrightIntensity = greenBrightIntensity;
    }
    
    /**
     * Accessor that sets the blueIntensity variable, for minimum blue intensity over red, green.
     *
     * @param  blueIntensity  minimum blue intensity over red, green
     */
    public void setBlueIntensity(int blueIntensity) {
        this.blueIntensity = blueIntensity;
    }
    
    /**
     * Accessor that sets the blueBrightIntensity variable, for minimum blue BrightIntensity over red, green.
     *
     * @param  blueBrightIntensity  minimum blue BrightIntensity over red, green
     */
    public void setBlueBrightIntensity(int blueBrightIntensity) {
        this.blueBrightIntensity = blueBrightIntensity;
    }
    
    public void setHistoInfo(boolean histoInfo) {
        this.histoInfo = histoInfo;    
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() { }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        setXYProcessLength(scriptParameters.getParams().getInt("xy_process_length"));
        setXYOverlapLength(scriptParameters.getParams().getInt("xy_overlap_length"));
        setZProcessLength(scriptParameters.getParams().getInt("z_process_length"));
        setZOverlapLength(scriptParameters.getParams().getInt("z_overlap_length"));
        setRedMin(scriptParameters.getParams().getInt("red_min"));
        setRedMax(scriptParameters.getParams().getInt("red_max"));
        setGreenMin(scriptParameters.getParams().getInt("green_min"));
        setGreenMax(scriptParameters.getParams().getInt("green_max"));
        setBlueMinXY(scriptParameters.getParams().getInt("blue_minxy"));
        setBlueMaxXY(scriptParameters.getParams().getInt("blue_maxxy"));
        setBlueMinZ(scriptParameters.getParams().getInt("blue_minz"));
        setBlueMaxZ(scriptParameters.getParams().getInt("blue_maxz"));
        setRedIntensity(scriptParameters.getParams().getInt("red_intensity"));
        setRedBrightIntensity(scriptParameters.getParams().getInt("red_bright_intensity"));
        setGreenIntensity(scriptParameters.getParams().getInt("green_intensity"));
        setGreenBrightIntensity(scriptParameters.getParams().getInt("green_bright_intensity"));
        setBlueIntensity(scriptParameters.getParams().getInt("blue_intensity"));
        setBlueBrightIntensity(scriptParameters.getParams().getInt("blue_bright_intensity"));
        setHistoInfo(scriptParameters.getParams().getBoolean("histo_info"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.getParams().put(ParameterFactory.newParameter("xy_process_length", xyProcessLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("xy_overlap_length", xyOverlapLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z_process_length", zProcessLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z_overlap_length", zOverlapLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_min", redMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_max", redMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_min", greenMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_max", greenMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_minxy", blueMinXY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_maxxy", blueMaxXY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_minz", blueMinZ));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_maxz", blueMaxZ));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_intensity", redIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_bright_intensity", redBrightIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_intensity", greenIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_bright_intensity", greenBrightIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_intensity", blueIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_bright_intensity", blueBrightIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("histo_info", histoInfo));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Large Synapse Detection  05/18/09");
        
        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Select TIFF input file"));

        inText = new JTextField(30);
        inText.setText("Input TIFF file name");
        inText.setFont(serif12);
        inText.setEnabled(false);

        browseButton = new JButton("Browse");
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.setActionCommand(BROWSE);
        browseButton.addActionListener(this);
        
        
        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.insets = insets;
        gbc2.weightx = 0;
        gbc2.fill = GridBagConstraints.NONE;
        gbc2.anchor = GridBagConstraints.WEST;

        inputPanel.add(browseButton, gbc2);
        gbc2.gridx = 1;
        gbc2.gridy = 0;
        gbc2.weightx = 1;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(inText, gbc2);
     
        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));
        mainPanel.add(inputPanel, gbc);
        
        xyProcessLabel = new JLabel("Processed square length within slice");
        xyProcessLabel.setForeground(Color.black);
        xyProcessLabel.setFont(serif12);
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(xyProcessLabel, gbc);
        
        xyProcessText = new JTextField(10);
        xyProcessText.setText("500");
        xyProcessText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(xyProcessText, gbc);
        
        xyOverlapLabel = new JLabel("Overlap length within slice");
        xyOverlapLabel.setForeground(Color.black);
        xyOverlapLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(xyOverlapLabel, gbc);
        
        xyOverlapText = new JTextField(10);
        xyOverlapText.setText("80");
        xyOverlapText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(xyOverlapText, gbc);
        
        zProcessLabel = new JLabel("Processed height across slices");
        zProcessLabel.setForeground(Color.black);
        zProcessLabel.setFont(serif12);
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(zProcessLabel, gbc);
        
        zProcessText = new JTextField(10);
        zProcessText.setText("60");
        zProcessText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(zProcessText, gbc);
        
        zOverlapLabel = new JLabel("Overlap length between slices");
        zOverlapLabel.setForeground(Color.black);
        zOverlapLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(zOverlapLabel, gbc);
        
        zOverlapText = new JTextField(10);
        zOverlapText.setText("15");
        zOverlapText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(zOverlapText, gbc);
        
        redMinLabel = new JLabel("Minimum red pixel width");
        redMinLabel.setForeground(Color.black);
        redMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redMinLabel, gbc);

        redMinText = new JTextField(10);
        redMinText.setText("1");
        redMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redMinText, gbc);
        
        redMaxLabel = new JLabel("Maximum red pixel width");
        redMaxLabel.setForeground(Color.black);
        redMaxLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redMaxLabel, gbc);

        redMaxText = new JTextField(10);
        redMaxText.setText("20");
        redMaxText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redMaxText, gbc);
        
        greenMinLabel = new JLabel("Minimum green pixel width");
        greenMinLabel.setForeground(Color.black);
        greenMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMinLabel, gbc);

        greenMinText = new JTextField(10);
        greenMinText.setText("1");
        greenMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMinText, gbc);
        
        greenMaxLabel = new JLabel("Maximum green pixel width");
        greenMaxLabel.setForeground(Color.black);
        greenMaxLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMaxLabel, gbc);

        greenMaxText = new JTextField(10);
        greenMaxText.setText("200");
        greenMaxText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMaxText, gbc);
        
        blueMinXYLabel = new JLabel("Minimum blue pixel width within a slice");
        blueMinXYLabel.setForeground(Color.black);
        blueMinXYLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMinXYLabel, gbc);

        blueMinXYText = new JTextField(10);
        blueMinXYText.setText("1");
        blueMinXYText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMinXYText, gbc);
        
        blueMaxXYLabel = new JLabel("Maximum blue pixel width within a slice");
        blueMaxXYLabel.setForeground(Color.black);
        blueMaxXYLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMaxXYLabel, gbc);
        
        blueMaxXYText = new JTextField(10);
        blueMaxXYText.setText("20");
        blueMaxXYText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMaxXYText, gbc);
        
        blueMinZLabel = new JLabel("Minimum blue pixel width between slices");
        blueMinZLabel.setForeground(Color.black);
        blueMinZLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMinZLabel, gbc);

        blueMinZText = new JTextField(10);
        blueMinZText.setText("1");
        blueMinZText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMinZText, gbc);
        
        blueMaxZLabel = new JLabel("Maximum blue pixel width  between slices");
        blueMaxZLabel.setForeground(Color.black);
        blueMaxZLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMaxZLabel, gbc);
        
        blueMaxZText = new JTextField(10);
        blueMaxZText.setText("20");
        blueMaxZText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMaxZText, gbc);
        
        redIntensityLabel = new JLabel("Minimum red intensity");
        redIntensityLabel.setForeground(Color.black);
        redIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redIntensityLabel, gbc);

        redIntensityText = new JTextField(10);
        redIntensityText.setText("5");
        redIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redIntensityText, gbc);
        
        redBrightIntensityLabel = new JLabel("Minimum bright red intensity");
        redBrightIntensityLabel.setForeground(Color.black);
        redBrightIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redBrightIntensityLabel, gbc);

        redBrightIntensityText = new JTextField(10);
        redBrightIntensityText.setText("55");
        redBrightIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redBrightIntensityText, gbc);
        
        greenIntensityLabel = new JLabel("Minimum green intensity");
        greenIntensityLabel.setForeground(Color.black);
        greenIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenIntensityLabel, gbc);

        greenIntensityText = new JTextField(10);
        greenIntensityText.setText("5");
        greenIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenIntensityText, gbc);
        
        greenBrightIntensityLabel = new JLabel("Minimum bright green intensity");
        greenBrightIntensityLabel.setForeground(Color.black);
        greenBrightIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenBrightIntensityLabel, gbc);

        greenBrightIntensityText = new JTextField(10);
        greenBrightIntensityText.setText("85");
        greenBrightIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenBrightIntensityText, gbc);
        
        blueIntensityLabel = new JLabel("Minimum blue intensity");
        blueIntensityLabel.setForeground(Color.black);
        blueIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueIntensityLabel, gbc);

        blueIntensityText = new JTextField(10);
        blueIntensityText.setText("15");
        blueIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueIntensityText, gbc);
        
        blueBrightIntensityLabel = new JLabel("Minimum bright blue intensity");
        blueBrightIntensityLabel.setForeground(Color.black);
        blueBrightIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueBrightIntensityLabel, gbc);

        blueBrightIntensityText = new JTextField(10);
        blueBrightIntensityText.setText("80");
        blueBrightIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueBrightIntensityText, gbc);
        
        histoInfoCheckBox = new JCheckBox("Obtain histograms of detected line colors");
        histoInfoCheckBox.setFont(serif12);
        histoInfoCheckBox.setForeground(Color.black);
        histoInfoCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(histoInfoCheckBox, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        if (inputFileName == null) {
            return false;
        }
        
        if (directory == null) {
            return false;
        }
        
        tmpStr = redMinText.getText();
        redMin = Integer.parseInt(tmpStr);

        if (redMin < 1) {
            MipavUtil.displayError("red minimum must be at least 1");
            redMinText.requestFocus();
            redMinText.selectAll();

            return false;
        } else if (redMin > 200) {
            MipavUtil.displayError("red minimum must not exceed 200");
            redMinText.requestFocus();
            redMinText.selectAll();

            return false;
        }
        
        tmpStr = redMaxText.getText();
        redMax = Integer.parseInt(tmpStr);

        if (redMax < redMin) {
            MipavUtil.displayError("red maximum must be at least red minimum");
            redMaxText.requestFocus();
            redMaxText.selectAll();

            return false;
        } else if (redMax > 500) {
            MipavUtil.displayError("red maximum must not exceed 500");
            redMaxText.requestFocus();
            redMaxText.selectAll();

            return false;
        }
        
        tmpStr = greenMinText.getText();
        greenMin = Integer.parseInt(tmpStr);

        if (greenMin < 1) {
            MipavUtil.displayError("green minimum must be at least 1");
            greenMinText.requestFocus();
            greenMinText.selectAll();

            return false;
        } else if (greenMin > 200) {
            MipavUtil.displayError("green minimum must not exceed 200");
            greenMinText.requestFocus();
            greenMinText.selectAll();

            return false;
        }
        
        tmpStr = greenMaxText.getText();
        greenMax = Integer.parseInt(tmpStr);

        if (greenMax < greenMin) {
            MipavUtil.displayError("green maximum must be at least green minimum");
            greenMaxText.requestFocus();
            greenMaxText.selectAll();

            return false;
        } else if (greenMax > 500) {
            MipavUtil.displayError("green maximum must not exceed 500");
            greenMaxText.requestFocus();
            greenMaxText.selectAll();

            return false;
        }
        
        tmpStr = blueMinXYText.getText();
        blueMinXY = Integer.parseInt(tmpStr);

        if (blueMinXY < 1) {
            MipavUtil.displayError("blue minimum xy must be at least 1");
            blueMinXYText.requestFocus();
            blueMinXYText.selectAll();

            return false;
        } else if (blueMinXY > 200) {
            MipavUtil.displayError("blue minimum xy must not exceed 200");
            blueMinXYText.requestFocus();
            blueMinXYText.selectAll();

            return false;
        }
        
        tmpStr = blueMaxXYText.getText();
        blueMaxXY = Integer.parseInt(tmpStr);

        if (blueMaxXY < blueMinXY) {
            MipavUtil.displayError("blue maximum xy must be at least blue minimum");
            blueMaxXYText.requestFocus();
            blueMaxXYText.selectAll();

            return false;
        } else if (blueMaxXY > 500) {
            MipavUtil.displayError("blue maximum xy must not exceed 500");
            blueMaxXYText.requestFocus();
            blueMaxXYText.selectAll();

            return false;
        }
        
        tmpStr = blueMinZText.getText();
        blueMinZ = Integer.parseInt(tmpStr);

        
        if (blueMinZ < 1) {
            MipavUtil.displayError("blue minimum z must be at least 1");
            blueMinZText.requestFocus();
            blueMinZText.selectAll();

            return false;
        } else if (blueMinZ > 200) {
            MipavUtil.displayError("blue minimum z must not exceed 200");
            blueMinZText.requestFocus();
            blueMinZText.selectAll();

            return false;
        }
        
        tmpStr = blueMaxZText.getText();
        blueMaxZ = Integer.parseInt(tmpStr);

        if (blueMaxZ < blueMinZ) {
            MipavUtil.displayError("blue maximum z must be at least blue minimum");
            blueMaxZText.requestFocus();
            blueMaxZText.selectAll();

            return false;
        } else if (blueMaxZ > 500) {
            MipavUtil.displayError("blue maximum z must not exceed 500");
            blueMaxZText.requestFocus();
            blueMaxZText.selectAll();

            return false;
        }
        
        tmpStr = xyProcessText.getText();
        xyProcessLength = Integer.parseInt(tmpStr);
        
        if (xyProcessLength < 2 * (redMin + greenMin + blueMinXY)) {
            MipavUtil.displayError("Processed square length must be at least 2 * (redMin + greenMin + blueMinXY)");
            xyProcessText.requestFocus();
            xyProcessText.selectAll();
            
            return false;
        }
        
        tmpStr = xyOverlapText.getText();
        xyOverlapLength = Integer.parseInt(tmpStr);
        
        if (xyOverlapLength < 0) {
            MipavUtil.displayError("Overlap square length cannot be negative");
            xyOverlapText.requestFocus();
            xyOverlapText.selectAll();
        }
        else if (xyOverlapLength >= xyProcessLength) {
            MipavUtil.displayError("xyOverlapLength must be less than xyProcessLength");
            xyOverlapText.requestFocus();
            xyOverlapText.selectAll();
        }
        
        tmpStr = zProcessText.getText();
        zProcessLength = Integer.parseInt(tmpStr);
        
        if (zProcessLength < 2 * (redMin + greenMin + blueMinZ)) {
            MipavUtil.displayError("Processed height must be at least 2 * (redMin + greenMin + blueMinZ)");
            zProcessText.requestFocus();
            zProcessText.selectAll();
            
            return false;
        }
        
        tmpStr = zOverlapText.getText();
        zOverlapLength = Integer.parseInt(tmpStr);
        
        if (zOverlapLength < 0) {
            MipavUtil.displayError("Overlap height cannot be negative");
            zOverlapText.requestFocus();
            zOverlapText.selectAll();
        }
        else if (zOverlapLength >= zProcessLength) {
            MipavUtil.displayError("zOverlapLength must be less than zProcessLength");
            zOverlapText.requestFocus();
            zOverlapText.selectAll();
        }
        
        tmpStr = redIntensityText.getText();
        redIntensity = Integer.parseInt(tmpStr);

        if (redIntensity < 1) {
            MipavUtil.displayError("red intensity must be at least 1");
            redIntensityText.requestFocus();
            redIntensityText.selectAll();

            return false;
        } else if (redIntensity > 255) {
            MipavUtil.displayError("red intensity must not exceed 255");
            redIntensityText.requestFocus();
            redIntensityText.selectAll();

            return false;
        }
        
        tmpStr = redBrightIntensityText.getText();
        redBrightIntensity = Integer.parseInt(tmpStr);

        if (redBrightIntensity < redIntensity) {
            MipavUtil.displayError("red bright intensity must be at least red intensity");
            redBrightIntensityText.requestFocus();
            redBrightIntensityText.selectAll();

            return false;
        } else if (redBrightIntensity > 255) {
            MipavUtil.displayError("red bright intensity must not exceed 255");
            redBrightIntensityText.requestFocus();
            redBrightIntensityText.selectAll();

            return false;
        }
        
        tmpStr = greenIntensityText.getText();
        greenIntensity = Integer.parseInt(tmpStr);

        if (greenIntensity < 1) {
            MipavUtil.displayError("green intensity must be at least 1");
            greenIntensityText.requestFocus();
            greenIntensityText.selectAll();

            return false;
        } else if (greenIntensity > 255) {
            MipavUtil.displayError("green intensity must not exceed 255");
            greenIntensityText.requestFocus();
            greenIntensityText.selectAll();

            return false;
        }
        
        tmpStr = greenBrightIntensityText.getText();
        greenBrightIntensity = Integer.parseInt(tmpStr);

        if (greenBrightIntensity < greenIntensity) {
            MipavUtil.displayError("green bright intensity must be at least green intensity");
            greenBrightIntensityText.requestFocus();
            greenBrightIntensityText.selectAll();

            return false;
        } else if (greenBrightIntensity > 255) {
            MipavUtil.displayError("green bright intensity must not exceed 255");
            greenBrightIntensityText.requestFocus();
            greenBrightIntensityText.selectAll();

            return false;
        }
        
        tmpStr = blueIntensityText.getText();
        blueIntensity = Integer.parseInt(tmpStr);

        if (blueIntensity < 1) {
            MipavUtil.displayError("blue intensity must be at least 1");
            blueIntensityText.requestFocus();
            blueIntensityText.selectAll();

            return false;
        } else if (blueIntensity > 255) {
            MipavUtil.displayError("blue intensity must not exceed 255");
            blueIntensityText.requestFocus();
            blueIntensityText.selectAll();

            return false;
        }
        
        tmpStr = blueBrightIntensityText.getText();
        blueBrightIntensity = Integer.parseInt(tmpStr);

        if (blueBrightIntensity < blueIntensity) {
            MipavUtil.displayError("blue bright intensity must be at least blue intensity");
            blueBrightIntensityText.requestFocus();
            blueBrightIntensityText.selectAll();

            return false;
        } else if (blueBrightIntensity > 255) {
            MipavUtil.displayError("blue bright intensity must not exceed 255");
            blueBrightIntensityText.requestFocus();
            blueBrightIntensityText.selectAll();

            return false;
        }
        
        histoInfo = histoInfoCheckBox.isSelected();
        
        return true;
    } // end setVariables()

}
