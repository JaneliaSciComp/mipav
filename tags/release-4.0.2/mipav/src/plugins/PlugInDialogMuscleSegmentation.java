import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogFaceAnonymizerBET;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import javax.swing.*;

/**
 * @author senseneyj
 * @version  June 4, 2007
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 */
public class PlugInDialogMuscleSegmentation extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private JRadioButton twoThighRadio;
    
    private JRadioButton abdomenRadio;
    
    private PlugInAlgorithmMuscleSegmentation.ImageType imageType;
    
    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    /** DOCUMENT ME! */
    private PlugInAlgorithmMuscleSegmentation muscleSegAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogMuscleSegmentation() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogMuscleSegmentation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        init();
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

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
        
        

        if (algorithm instanceof PlugInAlgorithmMuscleSegmentation) {
            Preferences.debug("Muscle segmentation, Elapsed time: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((muscleSegAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage);
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

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end AlgorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the NIA muscle segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            //FileInfoBase[] info = image.getFileInfo();
            //info[0].displayAboutInfo(this); //expecting a 2D image

            String name = makeImageName(image.getImageName(), "_muscle");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            muscleSegAlgo = new PlugInAlgorithmMuscleSegmentation(resultImage, image, imageType, parentFrame);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            muscleSegAlgo.addListener(this);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (muscleSegAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                muscleSegAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Muscle segmentation: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    protected void setGUIFromParams() {
    // TODO Auto-generated method stub, no params yet
    }

    protected void storeParamsFromGUI() throws ParserException {
    // TODO Auto-generated method stub, no params yet
    }
   
    private void init() {
        
        
        setForeground(Color.black);
        setTitle("NIA Muscle Segmentation");

        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("What kind of image?"));

        twoThighRadio = new JRadioButton("Two thighs");
        twoThighRadio.setFont(MipavUtil.font12);

        abdomenRadio = new JRadioButton("Abdomen");
        abdomenRadio.setFont(MipavUtil.font12);
        
        if (true) {
            twoThighRadio.setSelected(true);
        } else {
            abdomenRadio.setSelected(true);
        }
        
        ButtonGroup group = new ButtonGroup();
        group.add(twoThighRadio);
        group.add(abdomenRadio);
        
        mainPanel.add(twoThighRadio);
        mainPanel.add(abdomenRadio);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

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

        if (twoThighRadio.isSelected()) {
            imageType = PlugInAlgorithmMuscleSegmentation.ImageType.TWO_THIGHS;
        } else if (abdomenRadio.isSelected()) {
            imageType = PlugInAlgorithmMuscleSegmentation.ImageType.ABDOMEN;
        } else {
            MipavUtil.displayWarning("You have selected an unsupported image type.");
            return false;
        }
        return true;
    }
}
