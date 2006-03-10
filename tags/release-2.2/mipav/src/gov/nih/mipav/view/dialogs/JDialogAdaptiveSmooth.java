package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
 *   Dialog to get user input, then call the algorithm. The user has the
 *   option to generate a new image or replace the source image.
 *   Algorithms are executed in their own thread.
 *
 *       @see        AlgorithmAdaptiveSmooth
 *
 */
public class JDialogAdaptiveSmooth
    extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    private AlgorithmAdaptiveSmooth adaptiveSmoothAlgo;
    private ModelImage image; // source image
    private ModelImage resultImage = null; // result image
    private ViewUserInterface userInterface;

    private int displayLoc; // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced

    private String[] titles;

    private JPanel paramPanel;
    private JTextField textRadiusY;
    private JLabel labelRadiusY;
    private JTextField textRadiusCr;
    private JLabel labelRadiusCr;
    private JTextField textRadiusCb;
    private JLabel labelRadiusCb;
    private JTextField textDistWeight;
    private JLabel labelDistWeight;
    private JCheckBox reduceCheckBox;

    private float radiusY;
    private float radiusCr;
    private float radiusCb;
    private float distWeight;
    private boolean reduce;

    private JPanel destinationPanel;
    private ButtonGroup destinationGroup;
    private JRadioButton replaceImage;
    private JRadioButton newImage;

    /**
     *  @param parent          Parent frame.
     *  @param im              Source image.
     */
    public JDialogAdaptiveSmooth(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param im	Source image.
     */
    public JDialogAdaptiveSmooth(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAdaptiveSmooth() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }

        if (srcImageKey.equals(destImageKey)) {
            this.setDisplayLocReplace();
        }
        else {
            this.setDisplayLocNew();
        }

        try {
            setRadiusY(parser.getNextFloat());
            setRadiusCr(parser.getNextFloat());
            setRadiusCb(parser.getNextFloat());
            setDistWeight(parser.getNextFloat());
            setReduce(parser.getNextBoolean());
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();
        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {
        if (algo.isCompleted()) {
            if (userInterface.isScriptRecording()) {
                //check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {
                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append(
                    "AdaptiveSmooth " + userInterface.getScriptDialog().getVar(image.getImageName()) + " ");
                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(
                        userInterface.getScriptDialog().getVar(resultImage.getImageName()) + " " + radiusY + " "
                        + radiusCr + " " + radiusCb + " " + distWeight + " " + reduce);
                }
                else {
                    userInterface.getScriptDialog().append(
                        userInterface.getScriptDialog().getVar(image.getImageName()) + " " + radiusY + " "
                        + radiusCr + " " + radiusCb + " " + distWeight + " " + reduce);
                }

                userInterface.getScriptDialog().append("\n");
            }
        }
    }

    /**
     *	Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Adaptive Smooth");

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        mainPanel.add(paramPanel, gbc);

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = gbc.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        labelRadiusY = createLabel("Y radius ");
        paramPanel.add(labelRadiusY, gbc2);

        gbc2.gridx = 1;
        textRadiusY = createTextField("2.0");
        paramPanel.add(textRadiusY, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 1;
        labelRadiusCr = createLabel("Cr radius ");
        if (!image.isColorImage()) {
            labelRadiusCr.setEnabled(false);
        }
        paramPanel.add(labelRadiusCr, gbc2);

        gbc2.gridx = 1;
        textRadiusCr = createTextField("4.0");
        if (!image.isColorImage()) {
            textRadiusCr.setEnabled(false);
        }
        paramPanel.add(textRadiusCr, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 2;
        labelRadiusCb = createLabel("Cb radius ");
        if (!image.isColorImage()) {
            labelRadiusCb.setEnabled(false);
        }
        paramPanel.add(labelRadiusCb, gbc2);

        gbc2.gridx = 1;
        textRadiusCb = createTextField("5.0");
        if (!image.isColorImage()) {
            textRadiusCb.setEnabled(false);
        }
        paramPanel.add(textRadiusCb, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 3;
        labelDistWeight = createLabel("Edge preservation strength ");
        paramPanel.add(labelDistWeight, gbc2);

        gbc2.gridx = 1;
        textDistWeight = new JTextField();
        if (image.isColorImage()) {
            textDistWeight.setText("3072.0");
        }
        else {
            distWeight = (float) ( (12.0f / 255.0f) * (image.getMax() - image.getMin()));
            textDistWeight.setText(String.valueOf(distWeight));
        }
        textDistWeight.setFont(serif12);
        paramPanel.add(textDistWeight, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 4;
        gbc2.gridwidth = 2;
        reduceCheckBox = new JCheckBox("Filter Cr and Cb at halved dimensions");
        reduceCheckBox.setFont(serif12);
        reduceCheckBox.setSelected(false);
        if (!image.isColorImage()) {
            reduceCheckBox.setEnabled(false);
        }
        paramPanel.add(reduceCheckBox, gbc2);

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        }
        else {
            replaceImage.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     *  Accessor that returns the image.
     *  @return          The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     *	Accessor that sets the display loc variable to replace, so the current image
     *	is replaced once the algorithm completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     *	Accessor that sets the display loc variable to new, so that a new image
     *	is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     *	Accessor that sets the Y radius.
     *	@param radius	Value to set Y radius to.
     */
    public void setRadiusY(float radius) {
        radiusY = radius;
    }

    /**
     *	Accessor that sets the Cr radius.
     *	@param radius	Value to set Cr radius to.
     */
    public void setRadiusCr(float radius) {
        radiusCr = radius;
    }

    /**
     *	Accessor that sets the Cb radius.
     *	@param radius	Value to set Cb radius to.
     */
    public void setRadiusCb(float radius) {
        radiusCb = radius;
    }

    /**
     *   Accessor that sets the edge presevation strength
     *   @param weigth Value to set distWeight to.
     */
    public void setDistWeight(float weight) {
        distWeight = weight;
    }

    /**
     *   Accessor that sets whether or not Cr and Cb are filtered at halved dimensions
     *   @ param reduc  Value to set reduce to.
     */
    public void setReduce(boolean reduc) {
        reduce = reduc;
    }

    /**
     *	Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
     *	@param event       Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        }
        else if (command.equals("Cancel")) {
            dispose();
        }
        else if (command.equals("Help")) {
            MipavUtil.showHelp(
                "filtersspatialAdaptiveSmooth_filters_(spatial)_AdaptiveSmooth_htm_toc_applying_the_AdaptiveSmooth");
        }

    }

    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();
        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        }
        else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        tmpStr = textRadiusY.getText();
        if (testParameter(tmpStr, 0.0, 1000.0)) {
            radiusY = Float.valueOf(tmpStr).floatValue();
        }
        else {
            textRadiusY.requestFocus();
            textRadiusY.selectAll();
            return false;
        }

        tmpStr = textRadiusCr.getText();
        if (testParameter(tmpStr, 0.0, 1000.0)) {
            radiusCr = Float.valueOf(tmpStr).floatValue();
        }
        else {
            textRadiusCr.requestFocus();
            textRadiusCr.selectAll();
            return false;
        }

        tmpStr = textRadiusCb.getText();
        if (testParameter(tmpStr, 0.0, 1000.0)) {
            radiusCb = Float.valueOf(tmpStr).floatValue();
        }
        else {
            textRadiusCb.requestFocus();
            textRadiusCb.selectAll();
            return false;
        }

        tmpStr = textDistWeight.getText();
        if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
            distWeight = Float.valueOf(tmpStr).floatValue();
        }
        else {
            textDistWeight.requestFocus();
            textDistWeight.selectAll();
            return false;
        }

        reduce = reduceCheckBox.isSelected();
        return true;
    }

    /**
     *	Once all the necessary variables are set, call the Gaussian Blur
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_adaptiveSmooth");
        int[] destExtents;

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
        }
        else {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
        }

        if (displayLoc == NEW) {
            try {
                // Make result image of float type
                if (image.isColorImage()) {
                    resultImage = new ModelImage(ModelImage.ARGB, destExtents, name, userInterface);
                }
                else {
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                }
                //resultImage = (ModelImage)image.clone();
                //resultImage.setImageName(name);
                // Make algorithm
                adaptiveSmoothAlgo = new AlgorithmAdaptiveSmooth(resultImage, image, radiusY, radiusCr, radiusCb,
                    distWeight, reduce);
                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                adaptiveSmoothAlgo.addListener(this);
                // Hide dialog
                setVisible(false);

                if (runInSeparateThread) {
                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (adaptiveSmoothAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                }
                else {
                    adaptiveSmoothAlgo.setActiveImage(isActiveImage);
                    if (!userInterface.isAppFrameVisible()) {
                        adaptiveSmoothAlgo.setProgressBarVisible(false);
                    }
                    adaptiveSmoothAlgo.run();
                }
            }
            catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AdaptiveSmooth: unable to allocate enough memory");
                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }
                return;
            }
        }
        else {
            try {
                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                adaptiveSmoothAlgo = new AlgorithmAdaptiveSmooth(null, image, radiusY, radiusCr, radiusCb, distWeight,
                    reduce);
                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                adaptiveSmoothAlgo.addListener(this);
                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];
                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ( (Frame) (imageFrames.elementAt(i))).getTitle();
                    ( (Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ( (Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame( (Frame) (imageFrames.elementAt(i)));
                }

                if (runInSeparateThread) {
                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (adaptiveSmoothAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                }
                else {
                    adaptiveSmoothAlgo.setActiveImage(isActiveImage);
                    if (!userInterface.isAppFrameVisible()) {
                        adaptiveSmoothAlgo.setProgressBarVisible(false);
                    }
                    adaptiveSmoothAlgo.run();
                }
            }
            catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AdaptiveSmooth: unable to allocate enough memory");
                return;
            }
        }
    }

    //************************************************************************
     //************************** Algorithm Events ****************************
      //************************************************************************

       /**
        *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
        *   algorithms when it has completed or failed to to complete, so that the dialog can be display
        *   the result image and/or clean up.
        *   @param algorithm   Algorithm that caused the event.
        */
       public void algorithmPerformed(AlgorithmBase algorithm) {
           ViewJFrameImage imageFrame = null;
           if (algorithm instanceof AlgorithmAdaptiveSmooth) {
               image.clearMask();
               if (adaptiveSmoothAlgo.isCompleted() == true && resultImage != null) {

                   updateFileInfo(image, resultImage);
                   resultImage.clearMask();
                   //The algorithm has completed and produced a new image to be displayed.
                   try {
                       imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                   }
                   catch (OutOfMemoryError error) {
                       MipavUtil.displayError("Out of memory: unable to open new frame");
                   }
               }
               else if (resultImage == null) {
                   // These next lines set the titles in all frames where the source image is displayed to
                   // image name so as to indicate that the image is now unlocked!
                   // The image frames are enabled and then registed to the userinterface.
                   Vector imageFrames = image.getImageFrameVector();
                   for (int i = 0; i < imageFrames.size(); i++) {
                       ( (Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                       ( (Frame) (imageFrames.elementAt(i))).setEnabled(true);
                       if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame) {
                           userInterface.registerFrame( (Frame) (imageFrames.elementAt(i)));
                       }
                   }
                   if (parentFrame != null) {
                       userInterface.registerFrame(parentFrame);
                   }
                   image.notifyImageDisplayListeners(null, true);
               }
               else if (resultImage != null) {
                   //algorithm failed but result image still has garbage
                   resultImage.disposeLocal(); // clean up memory
                   resultImage = null;
               }
           }
           insertScriptLine(adaptiveSmoothAlgo);
           dispose();
       }

}
