package gov.nih.mipav.view.dialogs;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;


/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
public class JDialogReplaceSlice
    extends JDialogBase
    implements AlgorithmInterface {


    private ViewUserInterface userInterface;

    private ModelImage image;
    private ModelImage sliceImage = null;

    private JComboBox sliceBox;
    private JTextField sliceField;

    private int sliceNum;

    public JDialogReplaceSlice( Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
    image = im; // set the image from the arguments to an image in this class
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    setTitle("Replace slice");

    if (im.getExtents().length != 3) {
        MipavUtil.displayError("Source image must be 3D");
        dispose();
        return;
    }

    init();

    }

    private void init() {


        JPanel mainPanel = new JPanel();

        Enumeration e = userInterface.getRegisteredImages();

        Vector sliceItems = new Vector();

        int [] imageExtents = image.getExtents();

        int [] currentExtents = null;

        ModelImage currentImage = null;


        while (e.hasMoreElements()) {
            currentImage = (ModelImage) e.nextElement();

            currentExtents = currentImage.getExtents();
            if (currentExtents.length == 2 &&
                currentExtents[0] == imageExtents[0] &&
                currentExtents[1] == imageExtents[1]) {

                sliceItems.addElement(new String(currentImage.getImageName()));
            }

        }

        if (sliceItems.isEmpty()) {
            MipavUtil.displayError("No valid images open for replace slice");
            dispose();
            return;
        }

        sliceBox = new JComboBox(sliceItems);

        sliceField = new JTextField(2);
        MipavUtil.makeNumericsOnly(sliceField, false);

        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = gbc.WEST;

        JLabel imageLabel = new JLabel("Slice image: ");
        imageLabel.setFont(MipavUtil.font12);

        JLabel sliceLabel = new JLabel("Slice to replace: ");
        sliceLabel.setFont(MipavUtil.font12);

        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
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

    private boolean setVariables() {

        try {
            sliceNum = Integer.parseInt(sliceField.getText());
        } catch (Exception ex) {
            MipavUtil.displayError("Enter a slice number to replace");
            return false;
        }

        if (sliceNum > image.getExtents()[2] ||
            sliceNum < 1) {
            MipavUtil.displayError("Slice number must be between 1 and " + (image.getExtents()[2]));
            return false;
        }

        return true;
    }

    private void callAlgorithm() {

        sliceImage = userInterface.getRegisteredImageByName((String)sliceBox.getSelectedItem());

        AlgorithmReplaceSlice algo = new AlgorithmReplaceSlice(image, sliceImage, sliceNum - 1);

        algo.addListener(this);

        setVisible(false);
        algo.run();
    }

    /**
     * Invoked when an action occurs.
     *
     * @param event ActionEvent
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        }
        else if (command.equals("Cancel")) {
            dispose();
        }

    }


    public void algorithmPerformed(AlgorithmBase algo) {
        if (algo.isCompleted()) {
            if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
             Preferences.debug("\nHave replaced slice " + sliceNum + " on ");
             Preferences.debug(image.getImageName() + " with ");
             Preferences.debug(sliceImage.getImageName() + "\n");
         } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))

        }
    }

}
