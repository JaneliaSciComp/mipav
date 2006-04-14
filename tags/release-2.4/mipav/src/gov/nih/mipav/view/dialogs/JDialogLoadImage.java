package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;


/** JDialogLoadImage allows the MIPAV user to import an image
*   (namely, ImageA) from another image frame.  The titles
*   of other images are listed in a drop-down combo-box, as they
*   are found by MIPAV when the dialog is opened.  The dialog is
*   modal and the okay button will not be available if there
*   is only one frame open (the only way to bring up the dialog).
*   The dialog will not discriminate between frames which
*   have the same names, taking the first frame it finds which
*   the selected name.
*   <p>
*   This class has the option to include a "Browse Files..."
*   button; however, this functionality has not been
*   completely implemented.
*   @author David Parsons
*   @version 1.00
*/

public class JDialogLoadImage extends JDialogBase
{
    private     JPanel              picListingPanel;
    private     JComboBox           imageChooser;
    private 	JCheckBox	     matchOrigins, matchOrients;
    private     ViewJFrameImage     masterImage;
    private     ViewUserInterface   userInterface;
    private     JButton             browseButton;

    private 	boolean             doOrigins, doOrients;

    // image taken from the frame to be imported:
    private     ModelImage          importImage;

    public JDialogLoadImage(ViewJFrameImage component) {
        super(component, true );
        setTitle("Load ImageB onto " + component.getImageA().getImageName());
        masterImage = component;
        userInterface = masterImage.getUserInterface();

        picListingPanel = new JPanel();
        picListingPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel loading = new JLabel("Set as ImageB: ");
        loading.setFont(MipavUtil.font12);
        loading.setForeground(Color.black);
        picListingPanel.add(loading);

        imageChooser = buildImageComboBox(component.getImageA());
        imageChooser.setToolTipText("On-screen images");
        picListingPanel.add(imageChooser);

        matchOrients = new JCheckBox(
            "Match orientations of two images.");
        matchOrients.setAlignmentX(Component.LEFT_ALIGNMENT);
        matchOrients.setEnabled(true);
        matchOrients.setSelected(true);
        doOrients = matchOrients.isSelected();

        matchOrigins = new JCheckBox(
            "Use image origin information to align images.");
        matchOrigins.setAlignmentX(Component.LEFT_ALIGNMENT);
        matchOrigins.setEnabled(true);
        matchOrigins.setSelected(true);
        doOrigins = matchOrigins.isSelected();

        Box mainBox = new Box(BoxLayout.Y_AXIS);
        mainBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainBox.add(picListingPanel);
        mainBox.add(matchOrients);
        mainBox.add(matchOrigins);
        this.getContentPane().add(mainBox, BorderLayout.NORTH);

        JPanel okCancelPanel = new JPanel();
        buildOKButton();
        okCancelPanel.add(OKButton);
        if (imageChooser.getItemCount() == 0) {
            OKButton.setEnabled(false);
        }

        buildCancelButton();
        okCancelPanel.add(cancelButton);
        this.getContentPane().add(okCancelPanel, BorderLayout.SOUTH);

        pack();
        setLocation(Toolkit.getDefaultToolkit().getScreenSize().width/2 - this.getSize().width,
                    Toolkit.getDefaultToolkit().getScreenSize().height/2 - this.getSize().height);

        setVisible(true);
    }

    /** adds the "Browse Files ...." button to the right of the panel. */
    public void addBrowseFilesButton() {
        GridBagLayout gbl = (GridBagLayout) picListingPanel.getLayout();
        GridBagConstraints gbc = gbl.getConstraints(picListingPanel);
        browseButton = new JButton("Browse...");
        browseButton.setActionCommand("browse");
        browseButton.addActionListener(this);
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        gbc.gridheight = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(browseButton, gbc);
        picListingPanel.add(browseButton);
    }

    /** gets the image that was imported from the frame or taken from
    *   files (ie., "browse files")
    *@return ModelImage image imported from frame or decoded from file.
    */
    public ModelImage getImage() {return importImage;}

    public boolean getMatchOrigins() {return doOrigins;}
    public boolean getMatchOrients() {return doOrients;}

    /** when a button is clicked */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();
        if (command.equalsIgnoreCase("ok")) {
            Vector uiV = userInterface.getImageFrameVector();
            int elem = -1;
            for (int i = uiV.size()-1; i >= 0; i--) {// get the first image-index which has this title
                try {
                    if (((ViewJFrameImage) uiV.elementAt(i)).getComponentImage().getActiveImage().getImageName().equals(imageChooser.getSelectedItem())) {
                        elem = i;
                        break;
                    }
                }
                catch (ClassCastException cce) {}
              }
              if (elem != -1) {
                ViewJFrameImage imageFrame = (ViewJFrameImage) uiV.elementAt(elem);
                importImage = (ModelImage) imageFrame.getImageA().clone();
                if (matchOrigins.isSelected()) {
                  doOrigins = true;
                }
                else {
                  doOrigins = false;
                }
                if (matchOrients.isSelected()) {
                  doOrients = true;
                }
                else {
                  doOrients = false;
                }
              }
              dispose();
        }
        else if (command.equalsIgnoreCase("browse")) {
            dispose();
            MipavUtil.displayError("Browse files...  Not Yet Supported.");
        }
        else if (command.equalsIgnoreCase("cancel")) {
            dispose();
        }
    }
}
