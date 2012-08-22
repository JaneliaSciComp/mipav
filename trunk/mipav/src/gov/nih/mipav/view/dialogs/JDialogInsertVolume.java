package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmInsertVolume;

import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class JDialogInsertVolume extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7157371462824445245L;
    
    /** Average volume type - the inserted volume is set equal to the mean of the 2 surrounding volumes. */
    public static final int AVERAGE_VOLUME = 1;

    /** Blank volume type - the inserted volume is blank. */
    public static final int BLANK_VOLUME = 2;

    /** Original volume type - a 2D image is inserted. */
    public static final int INSERTED_IMAGE_VOLUME = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton insertedImgButton;

    /** DOCUMENT ME! */
    private JRadioButton average;

    /** DOCUMENT ME! */
    private JRadioButton blank;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage insertedImage = null; // inserted image for ORIGINAL_VOLUME

    /** DOCUMENT ME! */
    private int insertVolume;

    /** DOCUMENT ME! */
    private AlgorithmInsertVolume insertVolumeAlgo;

    /** DOCUMENT ME! */
    private int nVolumes; // number of volumes in image


    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int volumeType;

    /** DOCUMENT ME! */
    private JTextField textVolume;

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogInsertVolume() { }

    /**
     * Creates new dialog for inserting a volume.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogInsertVolume(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
            //MipavUtil.showHelp("U4055");
            MipavUtil.showWebHelp("Slice_tools#Insert_Missing_Slices");
        } else if ((source == insertedImgButton) || (source == average) || (source == blank) ) {

            
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmInsertVolume) {

            if ((insertVolumeAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // put the new image into a new frame
                    new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    Preferences.debug("\nHave inserted ");

                    if (volumeType == BLANK_VOLUME) {
                        Preferences.debug("blank volume ");
                    } else if (volumeType == INSERTED_IMAGE_VOLUME) {
                        Preferences.debug("original volume from " + insertedImage.getImageName() + "\n");
                    } 

                    Preferences.debug("as new volume number " + insertVolume + "\n");


                    if (image.getNDims() == 4)  { // image.getNDims() == 4
                        Preferences.debug("into " + image.getFileInfo()[0].getExtents()[2] + " volume " +
                                          image.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          image.getImageName() + "\n");
                        Preferences.debug("to create\n");
                        Preferences.debug(resultImage.getFileInfo()[0].getExtents()[2] + " volume " +
                                          resultImage.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          resultImage.getImageName() + "\n");
                    } // image.getNDims() == 4
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
                
                resultImage.getMatrixHolder().replaceMatrices(image.getMatrixHolder().getMatrices());
                resultImage.getFileInfo(0).setOrigin(image.getFileInfo(0).getOrigin());
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    Preferences.debug("\nHave inserted ");

                    if (volumeType == BLANK_VOLUME) {
                        Preferences.debug("blank volume ");
                    } else if (volumeType == INSERTED_IMAGE_VOLUME) {
                        Preferences.debug("original volume from " + insertedImage.getImageName() + "\n");
                    } 

                    Preferences.debug("as new volume number " + insertVolume + "\n");


                    if (image.getNDims() == 4) { // image.getNDims() == 4
                        Preferences.debug("into " + (image.getFileInfo()[0].getExtents()[2] - 1) + " volume " +
                                          image.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          image.getImageName() + "\n");
                        Preferences.debug("to create\n");
                        Preferences.debug(image.getFileInfo()[0].getExtents()[2] + " volume " +
                                          image.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          image.getImageName() + "\n");
                    } // image.getNDims() == 4
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))

            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }

        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        insertVolumeAlgo.finalize();
        insertVolumeAlgo = null;
        dispose();
    }
    

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor to specify the 2D image used as an inserted volume for ORGINAL_VOLUME.
     *
     * @param  insertedImage  DOCUMENT ME!
     */
    public void setInsertedImage(ModelImage insertedImage) {
        this.insertedImage = insertedImage;
    }

    /**
     * Accessor which lets you change where to insert the volume.
     *
     * @param  num  volume number before which the new volume is to be inserted
     */
    public void setInsertVolumeNumber(int num) {
        insertVolume = num;
    }

    /**
     * Accessor which lets you change the type of volume to be inserted.
     *
     * @param  type  the type of volume to be inserted (either AVERAGE_VOLUME or BLANK_Volume)
     */
    public void setVolumeType(int type) {
        volumeType = type;
    }

    /**
     * Once all the necessary variables are set, call the Insert Volume algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        nVolumes = image.getExtents()[3];

        int[] destExtents = null; // length along an axis of the destination image

        try {

            if (image.getNDims() == 4){
                destExtents = new int[4];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];
                destExtents[3] = image.getExtents()[3]+1;
            }

            // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
            resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());
            
            
            
            
            
            

            // Make algorithm:
            
            if (volumeType== INSERTED_IMAGE_VOLUME && insertedImage != null ){
                insertVolumeAlgo = new AlgorithmInsertVolume(image, resultImage, volumeType, insertVolume, insertedImage);              
            }

            else{
                insertedImage = null;
                insertVolumeAlgo = new AlgorithmInsertVolume(image, resultImage, volumeType, insertVolume, insertedImage);
            }
            

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            insertVolumeAlgo.addListener(this);

            createProgressBar(image.getImageName(), insertVolumeAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (insertVolumeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                insertVolumeAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up image memory
                resultImage = null;
            }

            MipavUtil.displayError("Insert Volume reports: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }


    /**
     * Builds a list of images to register to the template image.
     */
    private JComboBox buildComboBox() {
        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        comboBoxImage.addItemListener(this);
        comboBoxImage.setEnabled(false);
        
        final JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        final Enumeration<String> names = userInterface.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            final String name = names.nextElement();

            if ( !name.equals(image.getImageName())) {
                insertedImage = userInterface.getRegisteredImageByName(name);

                if ( (insertedImage.getNDims()== 3) && (image.getExtents()[0]==insertedImage.getExtents()[0]) 
                        && (image.getExtents()[1]==insertedImage.getExtents()[1]) &&(image.isColorImage() == insertedImage.isColorImage())
                        && (image.isComplexImage() == insertedImage.isComplexImage())
                        && (userInterface.getFrameContainingImage(insertedImage) != null)) {
                    comboBox.addItem(name);
                }
            }
        }
        comboBox.addItemListener(this);
        return comboBox;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        nVolumes = image.getExtents()[3];

        setTitle("Insert volume");
        setForeground(Color.black);

        JPanel volumePanel = new JPanel(new GridBagLayout());
        volumePanel.setBorder(buildTitledBorder("Insert volume"));

        JLabel volumeLabel = new JLabel("Insert before volume #(0-" + String.valueOf(nVolumes-1) + ") or enter " +
                                       String.valueOf(nVolumes) + " for new last volume");
        volumeLabel.setFont(serif12);
        volumeLabel.setForeground(Color.black);

        textVolume = new JTextField(5);
        textVolume.setText("");
        textVolume.setFont(serif12);
        textVolume.setEnabled(true);
        textVolume.addFocusListener(this);

        ButtonGroup volumeGroup = new ButtonGroup();
        blank = new JRadioButton("Blank", true);
        blank.setFont(serif12);
        blank.addActionListener(this);
        volumeGroup.add(blank);

        insertedImgButton = new JRadioButton("Inserted Image Volume", false);
        insertedImgButton.setFont(serif12);
        insertedImgButton.addActionListener(this);
        volumeGroup.add(insertedImgButton);
        
        
        /*final JLabel labelImage = new JLabel("Volume to be inserted:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);*/

        comboBoxImage = buildComboBox();


        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 0;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE;
        volumePanel.add(volumeLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        volumePanel.add(textVolume, gbc);


        gbc.gridx = 0;
        gbc.gridy = 1;
        volumePanel.add(blank, gbc);


        gbc.gridy = 2;
        gbc.gridwidth = 1;
        volumePanel.add(insertedImgButton, gbc);
        gbc.gridx = 1;
        volumePanel.add(comboBoxImage, gbc);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        mainPanel.add(volumePanel);

        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buttonPanel.add(OKButton); buildCancelButton(); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = textVolume.getText();

        if (testParameter(tmpStr, 0, (nVolumes))) {
            insertVolume = Integer.parseInt(tmpStr);
        } else {
            textVolume.requestFocus();
            textVolume.selectAll();

            return false;
        }

        if (blank.isSelected()) {
            volumeType = BLANK_VOLUME;
        } else if (insertedImgButton.isSelected()) {
            volumeType = INSERTED_IMAGE_VOLUME;
        
            String selectName = (String) (comboBoxImage.getSelectedItem());

            if (selectName != null) {
                insertedImage = userInterface.getRegisteredImageByName(selectName);
            } else {
                insertedImage = null;
            }
        }
        if (insertVolume < 0) {
            MipavUtil.displayError("Volume you insert before must be at least 0");
            textVolume.requestFocus();
            textVolume.selectAll();

            return false;
        } else if (insertVolume > (nVolumes)) {
            MipavUtil.displayError("Volume number cannot exceed " + String.valueOf(nVolumes) +
                                   " for addition to end");
            textVolume.requestFocus();
            textVolume.selectAll();

            return false;
        } else if ((volumeType == INSERTED_IMAGE_VOLUME) && (insertedImage == null)) {
            MipavUtil.displayError("No other image to insert.");

            return false;
        }

        return true;
    }

    @Override
    protected void setGUIFromParams() {
        // TODO Auto-generated method stub
        
    }

    @Override
    protected void storeParamsFromGUI() throws ParserException {
        // TODO Auto-generated method stub
        
    }
}


