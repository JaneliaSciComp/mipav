package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to use difference, intersection, exclusive or, or intersection on 2 VOIs
 */
public class JDialogVOIBoolean extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup booleanGroup;

    /** DOCUMENT ME! */
    private JRadioButton intersectionButton;

    /** DOCUMENT ME! */
    private JRadioButton differenceButton;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private ModelImage image;

    
    /** DOCUMENT ME! */
    private JRadioButton exclusiveOrButton;

    /** DOCUMENT ME! */
    private JRadioButton subjectVOIButton;

    /** DOCUMENT ME! */
    private int subjectVOIIndex = -1;

    /** DOCUMENT ME! */
    private JRadioButton unionButton;

    /** DOCUMENT ME! */
    private ButtonGroup VOIGroup;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;
    
    private int nVOIs;

    /** DOCUMENT ME! */
    private JRadioButton clipVOIButton;

    /** DOCUMENT ME! */
    private int clipVOIIndex = -1;
    
    private JCheckBox removeSubjectVOICheckBox;
    
    private boolean removeSubjectVOI = false;
    
    private JCheckBox removeClipVOICheckBox;
    
    private boolean removeClipVOI = false;
    
    private GenericPolygonClipper.gpc_op op;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogVOIBoolean object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogVOIBoolean(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogVOIBoolean(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
        init();
    }
    
    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogVOIBoolean(Frame theParentFrame, VOIManagerInterface voiManager, ModelImage im) {
        this(theParentFrame,im);
        this.voiManager = voiManager;
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
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            componentImage.getVOIHandler().setPresetHue(-1.0f);
            dispose();
        } else if ((source == subjectVOIButton) || (source == clipVOIButton)) {

            if (subjectVOIButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(0.0f); // red
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(0.0f); // red
            } else if (clipVOIButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(1.0f / 3.0f); // green
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
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

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************


    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
        componentImage.getVOIHandler().setPresetHue(-1.0f);
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        int i;

        componentImage.getVOIHandler().setPresetHue(-1.0f);
        
        VOI resultVOI = new VOI((short)nVOIs, "resultVOI", VOI.CONTOUR, 2.0f/3.0f);
        
        new GenericPolygonClipper(op, VOIs.VOIAt(subjectVOIIndex), VOIs.VOIAt(clipVOIIndex), resultVOI);
        
        for (i = nVOIs - 1; i >= 0; i--) {
            if (removeSubjectVOI && i == subjectVOIIndex) {
                VOIs.remove(i);
            }
            else if (removeClipVOI && i == clipVOIIndex) {
                VOIs.remove(i);
            }
        }

        VOIs.add(resultVOI);
        image.getParentFrame().updateImages();
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JPanel VOIPanel;

        setForeground(Color.black);
        setTitle("VOI Boolean Operations");

        VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select VOIs"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;

        VOIGroup = new ButtonGroup();

        subjectVOIButton = new JRadioButton("Add required subject VOI", true);
        subjectVOIButton.setForeground(Color.red);
        subjectVOIButton.setFont(serif12);
        subjectVOIButton.addActionListener(this);
        VOIGroup.add(subjectVOIButton);
        VOIPanel.add(subjectVOIButton, gbc4);
        componentImage.getVOIHandler().newVOI(0.0f); // red
        //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage.getVOIHandler().setPresetHue(0.0f); // red

        clipVOIButton = new JRadioButton("Add required clip VOI", false);
        clipVOIButton.setForeground(Color.green.darker());
        clipVOIButton.setFont(serif12);
        clipVOIButton.addActionListener(this);
        VOIGroup.add(clipVOIButton);
        gbc4.gridy = 1;
        VOIPanel.add(clipVOIButton, gbc4);

        removeSubjectVOICheckBox = new JCheckBox("Remove Subject VOI");
        removeSubjectVOICheckBox.setFont(serif12);
        removeSubjectVOICheckBox.setForeground(Color.black);
        removeSubjectVOICheckBox.setSelected(false);
        gbc4.gridy = 2;
        VOIPanel.add(removeSubjectVOICheckBox, gbc4);
        
        removeClipVOICheckBox = new JCheckBox("Remove Clip VOI");
        removeClipVOICheckBox.setFont(serif12);
        removeClipVOICheckBox.setForeground(Color.black);
        removeClipVOICheckBox.setSelected(false);
        gbc4.gridy = 3;
        VOIPanel.add(removeClipVOICheckBox, gbc4);

        JPanel booleanPanel = new JPanel(new GridBagLayout());
        booleanPanel.setForeground(Color.black);
        booleanPanel.setBorder(buildTitledBorder("Select Boolean Operation"));

        GridBagConstraints gbc5 = new GridBagConstraints();

        gbc5.gridwidth = 1;
        gbc5.gridheight = 1;
        gbc5.anchor = GridBagConstraints.WEST;
        gbc5.weightx = 1;
        gbc5.insets = new Insets(3, 3, 3, 3);
        gbc5.fill = GridBagConstraints.HORIZONTAL;
        gbc5.gridx = 0;
        gbc5.gridy = 0;

        booleanGroup = new ButtonGroup();

        differenceButton = new JRadioButton("Difference", false);
        differenceButton.setForeground(Color.black);
        differenceButton.setFont(serif12);
        differenceButton.addActionListener(this);
        booleanGroup.add(differenceButton);
        booleanPanel.add(differenceButton, gbc5);

        intersectionButton = new JRadioButton("Intersection", true);
        intersectionButton.setForeground(Color.black);
        intersectionButton.setFont(serif12);
        intersectionButton.addActionListener(this);
        booleanGroup.add(intersectionButton);
        gbc5.gridy = 1;
        booleanPanel.add(intersectionButton, gbc5);

        exclusiveOrButton = new JRadioButton("Exclusive Or", false);
        exclusiveOrButton.setForeground(Color.black);
        exclusiveOrButton.setFont(serif12);
        exclusiveOrButton.addActionListener(this);
        booleanGroup.add(exclusiveOrButton);
        gbc5.gridy = 2;
        booleanPanel.add(exclusiveOrButton, gbc5);

        unionButton = new JRadioButton("Union", false);
        unionButton.setForeground(Color.black);
        unionButton.setFont(serif12);
        unionButton.addActionListener(this);
        booleanGroup.add(unionButton);
        gbc5.gridy = 3;
        booleanPanel.add(unionButton, gbc5);

        getContentPane().add(VOIPanel, BorderLayout.NORTH);
        getContentPane().add(booleanPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;
        float[] hsb;
        float hue;
        VOIs = image.getVOIs();
        nVOIs = VOIs.size();
        subjectVOIIndex = -1;
        clipVOIIndex = -1;

        if (intersectionButton.isSelected()) {
            op = GenericPolygonClipper.gpc_op.GPC_INT;
        } else if (differenceButton.isSelected()) {
            op = GenericPolygonClipper.gpc_op.GPC_DIFF;
        } else if (exclusiveOrButton.isSelected()) {
            op = GenericPolygonClipper.gpc_op.GPC_XOR;
        } else if (unionButton.isSelected()) {
            op = GenericPolygonClipper.gpc_op.GPC_UNION;
        }

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (subjectVOIIndex == -1) {
                        subjectVOIIndex = i;
                        VOIs.VOIAt(i).setName("subjectVOI");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 subjectVOI VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {

                    if (clipVOIIndex == -1) {
                        clipVOIIndex = i;
                        VOIs.VOIAt(i).setName("clipVOI");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 whole organ VOI");

                        return false;
                    }
                } else {
                    MipavUtil.displayError("VOI hue = " + hue + " Must be 0 for red or " +
                                           "1/3 for green");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        if (subjectVOIIndex == -1) {
            MipavUtil.displayError("Must specify a subject VOI");

            return false;
        }

        if (clipVOIIndex == -1) {
            MipavUtil.displayError("Must specify a clip VOI");

            return false;
        }
        
        removeSubjectVOI = removeSubjectVOICheckBox.isSelected();
        
        removeClipVOI = removeClipVOICheckBox.isSelected();

        return true;
    }

}
