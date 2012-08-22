package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to remove separate time slices in an image. Dialog asks which slices the user wishes to remove; it
 * provides buttons to mark all slices for removal and to de-select any slices from image removal; it gives options to
 * remove or to cancel. Allows only 4D images; 2D or 3D images would not make sense with this operation.**(as of 25 Oct,
 * does not yet rename removed slice image when saving)**(as of 1 November, does not yet process the more complicated
 * DICOM images completely.
 */
public class JDialogRemoveTSlices extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5187681503326982110L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox[] checkboxList;

    /** DOCUMENT ME! */
    private JPanel checkboxPanel;

    /** DOCUMENT ME! */
    private JButton checkButton; // dialog button to set all checks to TRUE (checked-TRUE means 'remove this slice')

    /** DOCUMENT ME! */
    private boolean[] checkListRemove; // copy of what user wanted to remove (TRUE is remove)

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int nChecked; // number of slices to remove

    /** DOCUMENT ME! */
    private int nSlices; // number of slices in image

    /** DOCUMENT ME! */
    private AlgorithmRemoveTSlices removeTSlicesAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private JButton unCheckButton; // dialog button to set all checks to FALSE

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRemoveTSlices() { }

    /**
     * Creates new dialog for removing time slices.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRemoveTSlices(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource(); // whatever the user clicked on
        int i; // counting variable


        if (source == OKButton) { // if user pressed "remove" ...
            nChecked = this.numberChecked();

            // copy the selection of whether or not to remove from the list of boxes:
            checkListRemove = new boolean[nSlices];

            for (i = 0; i < nSlices; i++) {

                if (checkboxList[i].isSelected()) {
                    checkListRemove[i] = true;
                } else {
                    checkListRemove[i] = false;
                }
            }

            callAlgorithm();
        } else if (source == checkButton) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (source == unCheckButton) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(false);
            }
        } else if (source == cancelButton) {
            dispose();
        } else if (source == helpButton) {
        	//MipavUtil.showHelp("U4005");
        	MipavUtil.showWebHelp("4_D_tools#Removing_time_volumes");
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

        if (algorithm instanceof AlgorithmRemoveTSlices) {

            if ((removeTSlicesAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // put the new image into a new frame
                    new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Remove Slices reports: out of memory; " + "unable to open a new frame");
                }

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    int currentNum = 0;
                    Preferences.debug("\nHave removed time volumes:\n");

                    for (int i = 0; i < checkListRemove.length; i++) {

                        if (checkListRemove[i]) {
                            Preferences.debug("\t" + (i));

                            if (((currentNum % 5) == 4) || (currentNum == (nChecked - 1))) {
                                Preferences.debug("\n");
                            }

                            currentNum++;
                        } // if (checkListRemove[i])
                    } // for (int i = 0; i < checkListRemove.length; i++)

                    Preferences.debug("from " + image.getFileInfo(0).getExtents()[2] + " slice " +
                                      image.getFileInfo(0).getExtents()[3] + " volume 4D " + image.getImageName() +
                                      "\n");
                    Preferences.debug("to create:\n");

                    if (resultImage.getNDims() == 3) {
                        Preferences.debug(resultImage.getFileInfo(0).getExtents()[2] + " slice 3D " +
                                          resultImage.getImageName() + "\n");
                    } else {
                        Preferences.debug(resultImage.getFileInfo(0).getExtents()[2] + " slice " +
                                          resultImage.getFileInfo(0).getExtents()[3] + " volume 4D " +
                                          resultImage.getImageName() + "\n");
                    }
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
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

        removeTSlicesAlgo.finalize();
        removeTSlicesAlgo = null;
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
     * Accessor that sets the which slices to remove according to the boolean array paramater.
     *
     * @param  cl  for every element that is true, the slice corresponding to that element index will be removed
     */
    public void setCheckListRemove(boolean[] cl) {
        checkListRemove = cl;
    }

    /**
     * Once all the necessary variables are set, call the Remove Slices algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int[] destExtents = null; // length along an axis of the destination image
        System.gc();

        if ((nChecked != image.getExtents()[3]) && (nChecked != 0)) {

            // if the number checked is not as large as the number of time slices available (if user checked them all)
            // or at least ONE is checked ...
            try {

                // destination image extents (length in a particular direction)
                // if user cuts all but 1 time slice, make dest a 3D image:
                if ((image.getExtents()[3] - nChecked) == 1) {
                    destExtents = new int[3];
                    destExtents[0] = image.getExtents()[0];
                    destExtents[1] = image.getExtents()[1];
                    destExtents[2] = image.getExtents()[2];
                } // else dest will have 4D, so make it a 4D image:
                else if ((image.getExtents()[3] - nChecked) > 1) {
                    destExtents = new int[4];
                    destExtents[0] = image.getExtents()[0];
                    destExtents[1] = image.getExtents()[1];
                    destExtents[2] = image.getExtents()[2];
                    destExtents[3] = image.getExtents()[3] - nChecked;
                }

                // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
                resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());


                // Make algorithm:
                removeTSlicesAlgo = new AlgorithmRemoveTSlices(image, resultImage, checkListRemove);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                removeTSlicesAlgo.addListener(this);

                createProgressBar(image.getImageName(), removeTSlicesAlgo);

                setVisible(false); // Hide dialog

                if(isRunInSeparateThread()) {
        	        // Start the thread as a low priority because we wish to still have user interface work fast
        	        if (removeTSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
        	            MipavUtil.displayError("A thread is already running on this object");
        	        }
                } else {
                	removeTSlicesAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                MipavUtil.displayError("Remove Slices reports: unable to allocate enough memory");

                return;
            }
        } else if (nChecked == 0) {
            MipavUtil.displayError("No slices were selected!  Select some slices.");
        } else {
            MipavUtil.displayError("All slices are selected!  Unselect some slices.");
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        nSlices = image.getExtents()[3];

        boolean[] checkListRemoved = new boolean[nSlices];

        for (int i = 0; i < nSlices; i++) {
            checkListRemoved[i] = false;
        }

        int[] selectedSlices = scriptParameters.getParams().getList("user_selected_slices").getAsIntArray();

        for (int i = 0; i < selectedSlices.length; i++) {
            checkListRemoved[selectedSlices[i]] = true;
        }

        setCheckListRemove(checkListRemoved);

        nChecked = 0;

        for (int i = 0; i < nSlices; i++) {

            if (checkListRemove[i]) {
                nChecked++;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());

        int numSelectedSlices = 0;

        for (int i = 0; i < nSlices; i++) {

            if (checkListRemove[i]) {
                numSelectedSlices++;
            }
        }

        int[] selectedSlices = new int[numSelectedSlices];

        for (int i = 0, j = 0; i < nSlices; i++) {

            if (checkListRemove[i]) {
                selectedSlices[j++] = i;
            }
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("user_selected_slices", selectedSlices));
    }


    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        nSlices = image.getExtents()[3];

        JPanel mainPanel = new JPanel(new BorderLayout()); // everything gets placed on this panel

        setTitle("Remove time volumes");
        setForeground(Color.black);

        checkboxPanel = new JPanel(); // place a check-box list in here
        checkboxPanel.setLayout(new GridLayout(nSlices, 1));
        checkboxPanel.setForeground(Color.white);
        checkboxPanel.setBackground(Color.white);
        checkboxList = new JCheckBox[nSlices]; // selector for the user to choose which slices to remove.  TRUE means
                                               // remove.

        for (int i = 0; i < nSlices; i++) // place nSlices of check options for user and give them a name
        {
            checkboxList[i] = new JCheckBox("Time volume " + (String.valueOf(i)));
            checkboxList[i].setBackground(Color.white);
            checkboxPanel.add(checkboxList[i]);
        }

        // make the list scroll if there are enough checkboxes
        JScrollPane scrollPane = new JScrollPane(checkboxPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainPanel.add(scrollPane);

        mainPanel.setBorder(buildTitledBorder("Check the time slice indices to remove"));
        mainPanel.setPreferredSize(new Dimension(210, 390));

        JPanel checkPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;

        // make check & uncheck buttons for the panel--place inside the above border
        checkButton = new JButton("Select all");
        checkButton.setPreferredSize(MipavUtil.defaultButtonSize);
        checkButton.setFont(serif12B);
        checkPanel.add(checkButton, gbc);
        checkButton.addActionListener(this);

        gbc.gridx = 1;
        unCheckButton = new JButton("Clear");
        unCheckButton.setPreferredSize(MipavUtil.defaultButtonSize);
        unCheckButton.setFont(serif12B);
        unCheckButton.addActionListener(this);
        checkPanel.add(unCheckButton, gbc);

        mainPanel.add(checkPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel();

        // Make & set the OK (remove) and Cancel buttons--place outside the border
        buildOKButton();
        OKButton.setText("Remove");
        buttonPanel.add(OKButton);

        buildCancelButton();
        buttonPanel.add(cancelButton);

        buildHelpButton();
        buttonPanel.add(helpButton);
        
        mainDialogPanel.setLayout(new BorderLayout());
        mainDialogPanel.add(mainPanel); // put the main panel into the center of the dialog
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainDialogPanel);
        pack();
        setVisible(true); // let someone see the dialog.
    }

    /**
     * This method finds the number of checked checkboxes in the list.
     *
     * @return  number checked
     */
    private int numberChecked() {
        int i;
        int numChecked = 0;

        for (i = 0; i < nSlices; i++) {

            if (checkboxList[i].isSelected()) {
                numChecked++;
            }
        }

        return (numChecked);
    }
}
