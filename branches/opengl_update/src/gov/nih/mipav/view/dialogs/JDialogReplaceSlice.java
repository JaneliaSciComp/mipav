package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogReplaceSlice extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4522782534141992295L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JComboBox sliceBox;

    /** DOCUMENT ME! */
    private JTextField sliceField;

    /** DOCUMENT ME! */
    private ModelImage sliceImage = null;

    /** DOCUMENT ME! */
    private int sliceNum;


    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogReplaceSlice object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  im              DOCUMENT ME!
     */
    public JDialogReplaceSlice(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        userInterface = ViewUserInterface.getReference();
        setTitle("Replace slice");

        if (im.getExtents().length != 3) {
            MipavUtil.displayError("Source image must be 3D");
            dispose();

            return;
        }

        init();

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Invoked when an action occurs.
     *
     * @param  event  ActionEvent
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        }

    }


    /**
     * DOCUMENT ME!
     *
     * @param  algo  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                Preferences.debug("\nHave replaced slice " + sliceNum + " on ");
                Preferences.debug(image.getImageName() + " with ");
                Preferences.debug(sliceImage.getImageName() + "\n");
            } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
            image.notifyImageExtentsListeners(); //update display of image to show changes
            
        }
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        sliceImage = userInterface.getRegisteredImageByName((String) sliceBox.getSelectedItem());

        AlgorithmReplaceSlice algo = new AlgorithmReplaceSlice(image, sliceImage, sliceNum);

        algo.addListener(this);

        createProgressBar(image.getImageName(), algo);
        
        setVisible(false);
        algo.run();
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {


        JPanel mainPanel = new JPanel();

        Enumeration<ModelImage> e = userInterface.getRegisteredImages();

        Vector<String> sliceItems = new Vector<String>();

        int[] imageExtents = image.getExtents();

        int[] currentExtents = null;

        ModelImage currentImage = null;


        while (e.hasMoreElements()) {
            currentImage = e.nextElement();

            currentExtents = currentImage.getExtents();

            if ((currentExtents.length == 2) && (currentExtents[0] == imageExtents[0]) &&
                    (currentExtents[1] == imageExtents[1])) {

                sliceItems.addElement(new String(currentImage.getImageName()));
            }

        }

        if (sliceItems.isEmpty()) {
            MipavUtil.displayError("Must have at least one 2D image open to use the Replace Slice Utility");
            dispose();

            return;
        }

        sliceBox = new JComboBox(sliceItems);

        sliceField = new JTextField(2);
        MipavUtil.makeNumericsOnly(sliceField, false);

        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;

        JLabel imageLabel = new JLabel("Slice image: ");
        imageLabel.setFont(MipavUtil.font12);

        JLabel sliceLabel = new JLabel("Slice index to replace: ");
        sliceLabel.setFont(MipavUtil.font12);
        

        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(5,5,5,5);
        mainPanel.add(imageLabel, gbc);

        gbc.gridwidth = 3;
        gbc.gridx = 1;
        mainPanel.add(sliceBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        mainPanel.add(sliceLabel, gbc);

        gbc.gridx = 1;
        gbc.gridwidth = 1;
        mainPanel.add(sliceField, gbc);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);

    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        try {
            sliceNum = Integer.parseInt(sliceField.getText());
        } catch (Exception ex) {
            MipavUtil.displayError("Enter a slice number to replace");

            return false;
        }

        if ((sliceNum > (image.getExtents()[2] - 1)) || (sliceNum < 0)) {
            MipavUtil.displayError("Slice number must be between 0 and " + ((image.getExtents()[2]) - 1));

            return false;
        }

        return true;
    }

}
