import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.event.*;
import java.awt.*;
import java.util.Vector;
import javax.swing.*;


public class PlugInDialogProcessICG extends JDialogBase implements
        AlgorithmInterface,
        ScriptableInterface {

    private PlugInAlgorithmProcessICG icgAlgo;
    private ModelImage image = null; // source image
    private ModelImage resultImage = null;

    private ViewUserInterface userInterface;

    private String[] titles = null;

    private JTextField sdField = null;
    private JTextField frameField = null;
    private JCheckBox distortBox = null;

    private double distortionThreshold = .8;
    private boolean doDistortion = true;
    private int registrationFrame = 1;


    /**
     *  Sets variables needed to call algorithm.
     *  @param theParentFrame    Parent frame
     *  @param im                Source image
     */
    public PlugInDialogProcessICG(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);

        image = imA;
        if (image.getNDims() != 3) {
            MipavUtil.displayError("Image must be 2.5D");
            return;
        }
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
        setVisible(true);
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param imA	Source image.
     */
    public PlugInDialogProcessICG(ViewUserInterface UI, ModelImage imA) {
        super();

        userInterface = UI;
        image = imA;
        init();
        setVisible(true);
    }

    public PlugInDialogProcessICG() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws
            IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        //setActiveImage(parser.isActiveImage());
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
                if (userInterface.getScriptDialog().getImgTableVar(image.
                        getImageName()) == null) {
                    if (userInterface.getScriptDialog().getActiveImgTableVar(
                            image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.
                                getImageName());
                    }
                }

                userInterface.getScriptDialog().append(
                        "PlugInDialogProcessICG " +
                        userInterface.getScriptDialog().
                        getVar(image.getImageName()) +
                        " ");

                userInterface.getScriptDialog().putVar(resultImage.getImageName());
                userInterface.getScriptDialog().append(userInterface.
                        getScriptDialog().getVar(resultImage.getImageName()) +
                        "\n");
            }
        }
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getSource() == distortBox) {
            if (distortBox.isSelected()) {
                sdField.setEnabled(true);
            } else {
                sdField.setEnabled(false);
            }
        }
    }

    private void init() {

        setTitle("Process ICG");
        PanelManager optionsPanel = new PanelManager("Options");
        sdField = WidgetFactory.buildTextField(Double.toString(
                distortionThreshold));
        MipavUtil.makeNumericsOnly(sdField, true);

        int currentFrame = ((ViewJFrameImage)parentFrame).getViewableSlice();
        frameField = WidgetFactory.buildTextField(Integer.toString(currentFrame + 1));
        MipavUtil.makeNumericsOnly(frameField, false);

        distortBox = WidgetFactory.buildCheckBox("Replace poorly aligned frames post registration", true, this);

        optionsPanel.add(distortBox);
        optionsPanel.addOnNextLine(WidgetFactory.buildLabel("Standard deviation: "));
        optionsPanel.add(sdField);

        optionsPanel.addOnNextLine(WidgetFactory.buildLabel("Reference frame: "));
        optionsPanel.add(frameField);
        getContentPane().add(optionsPanel.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(false);

    }

    private boolean setVariables() {

        if (distortBox.isSelected()) {
            try {
                distortionThreshold = Double.parseDouble(sdField.getText());
            } catch (Exception e) {
                sdField.requestFocus();
                return false;
            }
        }
        try {
            registrationFrame = Integer.parseInt(frameField.getText());
            if (registrationFrame < 1 ||
                registrationFrame > image.getExtents()[2]) {
                MipavUtil.displayWarning("Registration frame must be between 1 and " + image.getExtents()[2]);
            }
        } catch (Exception e) {
            frameField.requestFocus();
            return false;
        }

        doDistortion = distortBox.isSelected();

        return true;
    }

    /**
     *  Closes dialog box when the OK button is pressed and calls the algorithm.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }

    /**
     *    Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {

            //unregister the frame holding the source image

            Vector imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];
            for (int i = 0; i < imageFrames.size(); i++) {
                titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " +
                        titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
            }

            // Make algorithm
            icgAlgo = new PlugInAlgorithmProcessICG(image, registrationFrame,
                    distortionThreshold, doDistortion);
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            icgAlgo.addListener(this);
            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (icgAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError(
                            "A thread is already running on this object");
                }
            } else {
                //icgAlgo.setActiveImage(isActiveImage);
                icgAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError(
                    "Dialog RGB to Gray: unable to allocate enough memory");
            return;
        }
    }

    /**
     *  Accessor that returns the image.
     *  @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
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

           if (algorithm instanceof PlugInAlgorithmProcessICG) {
               System.err.println("ICG Process completed");

               // These next lines set the titles in all frames where the source image is displayed to
               // image name so as to indicate that the image is now unlocked!
               // The image frames are enabled and then registed to the userinterface.
               Vector imageFrames = image.getImageFrameVector();
               for (int i = 0; i < imageFrames.size(); i++) {
                   ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                   ((Frame) (imageFrames.elementAt(i))).setEnabled(true);
                   if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                       userInterface.registerFrame((Frame) (imageFrames.
                               elementAt(i)));
                   }

               }
               if (parentFrame != null) {
                   System.err.println("should update title");
                   userInterface.registerFrame(parentFrame);
                   ((ViewJFrameImage)parentFrame).initExtentsVariables(((ViewJFrameImage)parentFrame).getActiveImage());
                   ((ViewJFrameImage)parentFrame).setTitle();
                   ((ViewJFrameImage)parentFrame).updateImageExtents();
               }

           }
       }
}
