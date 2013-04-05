package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * JDialogLoadImage allows the MIPAV user to import an image (namely, ImageA) from another image frame. The titles of
 * other images are listed in a drop-down combo-box, as they are found by MIPAV when the dialog is opened. The dialog is
 * modal and the okay button will not be available if there is only one frame open (the only way to bring up the
 * dialog). The dialog will not discriminate between frames which have the same names, taking the first frame it finds
 * which the selected name.
 *
 * <p>This class has the option to include a "Browse Files..." button; however, this functionality has not been
 * completely implemented.</p>
 *
 * @author   David Parsons
 * @version  1.00
 */

public class JDialogLoadImageForRegistration extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3843075015237096037L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton browseButton;

    /** DOCUMENT ME! */
    private String directory;

    /** DOCUMENT ME! */
    private JRadioButton fileButton;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private JTextField fileTFld;

    /** DOCUMENT ME! */
    private JRadioButton frameButton;

    /** DOCUMENT ME! */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private JComboBox imageChooser;

    /** image taken from the frame to be imported:. */
    private ModelImage importImage;

    /** DOCUMENT ME! */
    private boolean isFrame = true;

    /** DOCUMENT ME! */
    private ModelLUT LUTb = null;

    /** DOCUMENT ME! */
    private ViewJFrameImage masterImage;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogLoadImageForRegistration object.
     *
     * @param  component  DOCUMENT ME!
     */
    public JDialogLoadImageForRegistration(ViewJFrameImage component) {
        super(component, true);
        imageA = component.getImageA();
        setTitle("Load " + imageA.getImageName() + " and Image(B) into Registration");
        masterImage = component;
        userInterface = masterImage.getUserInterface();

        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        JPanel framePanel = new JPanel();
        framePanel.setBorder(buildTitledBorder("Frame"));
        framePanel.setLayout(gbl);

        frameButton = new JRadioButton("", true);
        frameButton.addActionListener(this);
        frameButton.setActionCommand("frame");

        JLabel loading = new JLabel("Set as ImageB: ");
        loading.setFont(MipavUtil.font12);
        loading.setForeground(Color.black);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(10, 10, 10, 10);
        gbc.gridwidth = 1;
        gbl.setConstraints(loading, gbc);
        framePanel.add(loading);

        imageChooser = buildImageComboBox(component.getImageA());

        imageChooser.setToolTipText("On-screen images");
        gbc.gridheight = GridBagConstraints.REMAINDER;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTH;
        gbl.setConstraints(imageChooser, gbc);
        framePanel.add(imageChooser);

        // File Chooser
        JPanel filePanel = new JPanel();
        filePanel.setForeground(Color.black);
        filePanel.setBorder(buildTitledBorder("File"));

        // need to add label, texfield and button
        fileTFld = new JTextField("");
        fileTFld.setColumns(10);
        fileTFld.setFont(serif12);

        browseButton = new JButton(" Browse ");
        browseButton.addActionListener(this);
        browseButton.setActionCommand("browse");
        browseButton.setFont(serif12);

        filePanel.add(fileTFld, BorderLayout.WEST);
        filePanel.add(browseButton, BorderLayout.EAST);

        fileButton = new JRadioButton("", false);
        fileButton.addActionListener(this);
        fileButton.setActionCommand("file");

        ButtonGroup group = new ButtonGroup();
        group.add(frameButton);
        group.add(fileButton);

        GridBagConstraints gbc3 = new GridBagConstraints();
        GridBagLayout gl = new GridBagLayout();
        gbc3.anchor = GridBagConstraints.WEST;

        JPanel bigPanel = new JPanel(gl);
        bigPanel.setBorder(buildTitledBorder("Select from frame or file"));

        gbc3.gridx = 0;
        gbc3.gridy = 0;

        // gbc3.gridwidth = 1;
        // gbc3.gridheight = 1;
        bigPanel.add(frameButton, gbc3);

        gbc3.gridx = 1;
        gbc3.gridwidth = 3;

        // gbc3.gridheight = 1;
        bigPanel.add(framePanel, gbc3);

        gbc3.gridx = 0;
        gbc3.gridy = 1;
        gbc3.gridwidth = 1;

        // gbc3.gridheight = 1;
        bigPanel.add(fileButton, gbc3);

        gbc3.gridx = 1;
        gbc3.gridy = 1;

        // gbc.gridwidth = 3;
        // gbc.gridheight = 1;
        bigPanel.add(filePanel, gbc3);

        this.getContentPane().add(bigPanel, BorderLayout.CENTER);

        JPanel okCancelPanel = new JPanel();
        buildOKButton();
        okCancelPanel.add(OKButton);

        if (imageChooser.getItemCount() == 0) {
            OKButton.setEnabled(false);
            imageChooser.setEnabled(false);
            frameButton.setSelected(false);
            fileButton.setSelected(true);
            frameButton.setEnabled(false);
        }

        buildCancelButton();
        okCancelPanel.add(cancelButton);
        this.getContentPane().add(okCancelPanel, BorderLayout.SOUTH);

        pack();


        setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) - this.getSize().width,
                    (Toolkit.getDefaultToolkit().getScreenSize().height / 2) - this.getSize().height);

        // this.setResizable(false);
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * when a button is clicked.
     *
     * @param  ae  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();

        if (command.equalsIgnoreCase("frame")) {
            imageChooser.setEnabled(true);
            fileTFld.setEnabled(false);
            browseButton.setEnabled(false);
            OKButton.setEnabled(true);

        } else if (command.equalsIgnoreCase("file")) {
            imageChooser.setEnabled(false);
            fileTFld.setEnabled(true);
            browseButton.setEnabled(true);

            if ((fileName == null) && fileTFld.getText().equalsIgnoreCase("")) {
                OKButton.setEnabled(false);
            }
        } else if (command.equalsIgnoreCase("browse")) {

            try {
                JFileChooser chooser = new JFileChooser();
                chooser.setApproveButtonText("OK");
                chooser.setDialogTitle("Select file to load");

                if (userInterface.getDefaultDirectory() != null) {
                    chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                }

                int returnVal = chooser.showOpenDialog(this);


                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    fileName = chooser.getSelectedFile().getName();
                    directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    userInterface.setDefaultDirectory(directory);
                    fileTFld.setText(fileName);
                    OKButton.setEnabled(true);
                } else {
                    return;
                }
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                return;
            }

        } else if (command.equalsIgnoreCase("ok")) {

            if (frameButton.isSelected()) {
                Vector<Frame> uiV = userInterface.getImageFrameVector();
                int elem = -1;

                for (int i = uiV.size() - 1; i >= 0; i--) { // get the first image-index which has this title

                    try {

                        if (((ViewJFrameImage) uiV.elementAt(i)).getComponentImage().getActiveImage().getImageName().equals(imageChooser.getSelectedItem())) {
                            elem = i;

                            break;
                        }
                    } catch (ClassCastException cce) { }
                }

                if (elem != -1) {
                    ViewJFrameImage imageFrame = (ViewJFrameImage) uiV.elementAt(elem);
                    importImage = (ModelImage) imageFrame.getImageA();
                    LUTb = imageFrame.getLUTa();
                    this.isFrame = true;
                }

                dispose();
            } else if (fileButton.isSelected()) {

                if ((fileTFld.getText() != null) && !fileTFld.getText().equalsIgnoreCase("")) {

                    if (directory == null) {
                        directory = userInterface.getDefaultDirectory();
                    }

                    fileName = fileTFld.getText();

                    FileIO fileIO = new FileIO();
                    importImage = fileIO.readImage(fileName, directory);
                    LUTb = fileIO.getModelLUT();

                    if (importImage.getNDims() != 2) {

                        // error
                        MipavUtil.displayError("Chosen image is not 2-D");
                        importImage.disposeLocal();
                        importImage = null;

                        return;
                    }

                    /*
                     * int [] extentsB = new int[2]; int [] extentsA = new int[2];
                     *
                     * extentsB = importImage.getExtents(); extentsA = imageA.getExtents();
                     *
                     * System.out.println("First image: " + extentsA[0] + "," + extentsA[1]); System.out.println("Second
                     * image: " + extentsB[0] + "," + extentsB[1]);
                     *
                     * if (extentsA[0] != extentsB[0] || extentsA[1] != extentsB[1]) { //error
                     * MipavUtil.displayError("Chosen image does not have the same dimensions as " +
                     * imageA.getImageName()); importImage.disposeLocal(); importImage = null; return; } if
                     * (importImage.getType() != imageA.getType()) { MipavUtil.displayError("Images must have the same
                     * type"); importImage.disposeLocal(); importImage = null; return; }
                     */
                    this.isFrame = false;
                    dispose();
                }
            }
        } else if (command.equalsIgnoreCase("cancel")) {
            dispose();
        } else {
            super.actionPerformed(ae);
        }
    }

    /**
     * gets the image that was imported from the frame or taken from files (ie., "browse files").
     *
     * @return  ModelImage image imported from frame or decoded from file.
     */
    public ModelImage getImage() {
        return importImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelLUT getModelLUT() {
        return LUTb;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isFromFrame() {
        return this.isFrame;
    }
}
