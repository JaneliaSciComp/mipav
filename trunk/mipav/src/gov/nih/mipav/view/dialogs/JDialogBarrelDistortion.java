package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog for correcting barrel and/or pincushion distortion
 */
public class JDialogBarrelDistortion extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID =;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmBarrelDistortion bdAlgo;

    /** Source image. */
    private ModelImage image;

    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private JPanel paramsPanel;
    
    private float a = 0.0f;
    
    private float b = 0.0f;
    
    private float c = 0.0f;
    
    private float d = 1.0f;
    
    private JLabel mainLabel;
    
    private JLabel mainLabel2;
    
    private JLabel aLabel;
    
    private JLabel bLabel;
    
    private JLabel cLabel;
    
    private JLabel dLabel;
    
    private JTextField aText;
    
    private JTextField bText;
    
    private JTextField cText;
    
    private JTextField dText;
    
    private JCheckBox noScalingDCheckBox;
    
    private boolean noScalingD = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogBarrelDistortion() { }

    /**
     * Construct the barrel/pin cushion correction dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogBarrelDistortion(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        //loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("Barr010");
            MipavUtil.showWebHelp("Barrel_Distortion_Correction");
        }
        else if (source == noScalingDCheckBox) {
            if (noScalingDCheckBox.isSelected()) {
                dLabel.setEnabled(false);
                dText.setEnabled(false);
            }
            else {
                dLabel.setEnabled(true);
                dText.setEnabled(true);
            }
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmBarrelDistortion) {
            Preferences.debug("Barrel distortion: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((bdAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if (resultImage.isColorImage()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if (algorithm instanceof AlgorithmBarrelDistortion)

        

        if (bdAlgo != null) {
            bdAlgo.finalize();
            bdAlgo = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Accessor that sets whether or not d is read in from the text or
     * calculated from d = 1 - a - b - c.  Calculation is used if it
     * is desired that no image scaling occurs.
     *
     * @param  noScalingD  <code>true</code> indicates d = 1 - a - b - c for no image scaling.
     */
    public void setNoScalingD(boolean noScalingD) {
        this.noScalingD = noScalingD;
    }

    /**
     * Accessor that sets a.
     *
     * @param  a.
     */
    public void setA(float a) {
        this.a = a;
    }
    
    /**
     * Accessor that sets b.
     *
     * @param  b.
     */
    public void setB(float b) {
        this.b = b;
    }
    
    /**
     * Accessor that sets c.
     *
     * @param  c.
     */
    public void setC(float c) {
        this.c = c;
    }
    
    /**
     * Accessor that sets d.
     *
     * @param  d.
     */
    public void setD(float d) {
        this.d = d;
    }

    /**
     * Once all the necessary variables are set, call the Barrel/Pincushion Distortion
     * Correction algorithm based on what type of image this is
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_barrel");
        
        try {

            // Make result image
            if (image.getType() == ModelImage.ARGB) {
                resultImage = new ModelImage(ModelImage.ARGB, image.getExtents(), name);
            } else if (image.getType() == ModelImage.ARGB_USHORT) {
                resultImage = new ModelImage(ModelImage.ARGB_USHORT, image.getExtents(), name);
            } else if (image.getType() == ModelImage.ARGB_FLOAT) {
                resultImage = new ModelImage(ModelImage.ARGB_FLOAT, image.getExtents(), name);
            } else {

                // resultImage     = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                }
            }

            // Make algorithm
            bdAlgo = new AlgorithmBarrelDistortion(resultImage, image, a, b, c, d);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            bdAlgo.addListener(this);

            createProgressBar(image.getImageName(), bdAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (bdAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                bdAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Barrel distortion: unable to allocate enough memory");

            return;
        }
            
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        
        noScalingD = scriptParameters.getParams().getBoolean("no_scaling_D");
        a = scriptParameters.getParams().getFloat("a");
        b = scriptParameters.getParams().getFloat("b");
        c = scriptParameters.getParams().getFloat("c");
        d = scriptParameters.getParams().getFloat("d");
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);

        scriptParameters.getParams().put(ParameterFactory.newParameter("no_scaling_D", noScalingD));
        scriptParameters.getParams().put(ParameterFactory.newParameter("a", a));
        scriptParameters.getParams().put(ParameterFactory.newParameter("b", b));
        scriptParameters.getParams().put(ParameterFactory.newParameter("c", c));
        scriptParameters.getParams().put(ParameterFactory.newParameter("d", d));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Barrel / pincushion distortion correction");
        getContentPane().setLayout(new BorderLayout());

        paramsPanel = new JPanel(new GridBagLayout());
        paramsPanel.setBorder(buildTitledBorder("Correction parameters"));
        
        mainLabel = new JLabel("rsrc = (a*redst**3 + b*rdest**2 + c*rdest + d)*rdest");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        paramsPanel.add(mainLabel, gbc);
        
        mainLabel2 = new JLabel("Use negative (a,b,c) up to -1.0 to shift distant points away from the center");
        mainLabel2.setForeground(Color.black);
        mainLabel2.setFont(serif12);
        mainLabel2.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 1;
        paramsPanel.add(mainLabel2, gbc);
        
        mainLabel2 = new JLabel("Use positive (a,b,c) up to 1.0 to shift distant points towards the center");
        mainLabel2.setForeground(Color.black);
        mainLabel2.setFont(serif12);
        mainLabel2.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 2;
        paramsPanel.add(mainLabel2, gbc);
        
        aLabel = new JLabel("a  (Affects only the outermost pixels of the image)");
        aLabel.setForeground(Color.black);
        aLabel.setFont(serif12);
        aLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramsPanel.add(aLabel, gbc);
        
        aText = new JTextField(10);
        aText.setText("0.0");
        aText.setFont(serif12);
        aText.setForeground(Color.black);
        aText.setEnabled(true);
        
        gbc.gridx = 1;
        gbc.gridy = 3;
        paramsPanel.add(aText, gbc);
        
        bLabel = new JLabel("b  (Most cases only require b optimization)");
        bLabel.setForeground(Color.black);
        bLabel.setFont(serif12);
        bLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramsPanel.add(bLabel, gbc);
        
        bText = new JTextField(10);
        bText.setText("0.0");
        bText.setFont(serif12);
        bText.setForeground(Color.black);
        bText.setEnabled(true);
        
        gbc.gridx = 1;
        gbc.gridy = 4;
        paramsPanel.add(bText, gbc);
        
        cLabel = new JLabel("c  (Most uniform correction)");
        cLabel.setForeground(Color.black);
        cLabel.setFont(serif12);
        cLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramsPanel.add(cLabel, gbc);
        
        cText = new JTextField(10);
        cText.setText("0.0");
        cText.setFont(serif12);
        cText.setForeground(Color.black);
        cText.setEnabled(true);
        
        gbc.gridx = 1;
        gbc.gridy = 5;
        paramsPanel.add(cText, gbc);
        
        dLabel = new JLabel("d  (Describes the linear scaling of the image)");
        dLabel.setForeground(Color.black);
        dLabel.setFont(serif12);
        dLabel.setEnabled(false);
        
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramsPanel.add(dLabel, gbc);
        
        dText = new JTextField(10);
        dText.setText("1.0");
        dText.setFont(serif12);
        dText.setForeground(Color.black);
        dText.setEnabled(false);
        
        gbc.gridx = 1;
        gbc.gridy = 6;
        paramsPanel.add(dText, gbc);
        
        noScalingDCheckBox = new JCheckBox("Set d = 1-a-b-c for no image scaling");
        noScalingDCheckBox.setFont(serif12);
        noScalingDCheckBox.setForeground(Color.black);
        noScalingDCheckBox.setEnabled(true);
        noScalingDCheckBox.setSelected(true);
        noScalingDCheckBox.addActionListener(this);
        
        gbc.gridx = 0;
        gbc.gridy = 7;
        paramsPanel.add(noScalingDCheckBox, gbc);

        getContentPane().add(paramsPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = aText.getText();
        if (testParameter(tmpStr, -1.0, 1.0)) {
            a = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("a must be between -1.0 and 1.0");
            aText.requestFocus();
            aText.selectAll();

            return false;
        }
       
        tmpStr = bText.getText();
        if (testParameter(tmpStr, -1.0, 1.0)) {
            b = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("b must be between -1.0 and 1.0");
            bText.requestFocus();
            bText.selectAll();

            return false;
        }
        
        tmpStr = cText.getText();
        if (testParameter(tmpStr, -1.0, 1.0)) {
            c = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("c must be between -1.0 and 1.0");
            cText.requestFocus();
            cText.selectAll();

            return false;
        }
        
        noScalingD = noScalingDCheckBox.isSelected();
        if (noScalingD) {
            d = 1.0f - a - b - c;
        }
        else {
            tmpStr = dText.getText();
            if (testParameter(tmpStr, -10.0, 10.0)) {
                d = Float.valueOf(tmpStr).floatValue();
            } else {
                MipavUtil.displayError("d must be between -10.0 and 10.0");
                dText.requestFocus();
                dText.selectAll();

                return false;
            }
        }

        return true;
    }
}
