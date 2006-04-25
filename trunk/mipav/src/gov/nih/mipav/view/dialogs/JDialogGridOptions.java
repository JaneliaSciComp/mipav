package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Sets options for overlaying a grid on the image.
 *
 * @author   ben
 * @version  1.0
 */
public class JDialogGridOptions extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1869680780124245552L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ViewJColorChooser chooser;

    /** DOCUMENT ME! */
    private Color color;

    /** DOCUMENT ME! */
    private JButton colorButton;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage comp;

    /** DOCUMENT ME! */
    private float height;

    /** DOCUMENT ME! */
    private JTextField heightField;

    /** DOCUMENT ME! */
    private String unitsStr;

    /** DOCUMENT ME! */
    private float width;

    /** DOCUMENT ME! */
    private JTextField widthField;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering parameters for entropy minimization.
     *
     * @param  theParentFrame  Parent frame
     * @param  comp            Source image
     */
    public JDialogGridOptions(Frame theParentFrame, ViewJComponentEditImage comp) {
        super(theParentFrame, false);
        this.comp = comp;

        unitsStr = FileInfoBase.getUnitsOfMeasureAbbrevStr(comp.getActiveImage().getFileInfo()[0].getUnitsOfMeasure(0));
        this.width = comp.getGridSpacingX();
        this.height = comp.getGridSpacingY();
        this.color = comp.getGridColor();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("Apply")) {

            if (setVariables()) {
                comp.setGridSpacingX(width);
                comp.setGridSpacingY(height);
                comp.setGridColor(color);

                if (comp.getGridOverlay()) {
                    comp.paintComponent(comp.getGraphics());
                }
            }
        } else if (command.equals("Color")) {
            chooser = new ViewJColorChooser(new Frame(), "Pick VOI color", new OkColorListener(), new CancelListener());

        } else if (command.equals("Close")) {
            setVisible(false);
            dispose();
        }
    }


    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Grid Overlay Options");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.gridwidth = 1;

        int yPos = 0;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());

        JLabel widthLabel = new JLabel("width (" + unitsStr + "): ");
        JLabel heightLabel = new JLabel("height (" + unitsStr + "): ");
        JLabel colorLabel = new JLabel("color: ");

        widthField = new JTextField(Float.toString(width), 4);
        heightField = new JTextField(Float.toString(height), 4);

        MipavUtil.makeNumericsOnly(widthField, true);
        MipavUtil.makeNumericsOnly(heightField, true);

        colorButton = new JButton(MipavUtil.getIcon("transparent.gif"));
        colorButton.setForeground(color);
        colorButton.setBackground(color);
        colorButton.addActionListener(this);
        colorButton.setActionCommand("Color");


        gbc.insets = new Insets(0, 5, 0, 5);

        paramPanel.add(widthLabel, gbc);

        gbc.gridx = 1;
        paramPanel.add(widthField, gbc);

        gbc.gridx = 2;
        paramPanel.add(heightLabel, gbc);

        gbc.gridx = 3;
        paramPanel.add(heightField, gbc);

        gbc.gridx = 4;
        paramPanel.add(colorLabel, gbc);

        gbc.gridx = 5;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        paramPanel.add(colorButton, gbc);

        JPanel mainPanel = new JPanel();
        mainPanel.add(paramPanel);
        mainPanel.setBorder(buildTitledBorder(""));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Apply");
        buttonPanel.add(OKButton);
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setResizable(false);
        setVisible(true);
    }

    /**
     * Check width and height for validity.
     *
     * @return  boolean is okay
     */
    private boolean setVariables() {

        try {
            width = Float.parseFloat(widthField.getText());
            height = Float.parseFloat(heightField.getText());

            if ((width <= 0) || (height <= 0)) {
                MipavUtil.displayError("Values must be greater than 0");

                return false;
            }
        } catch (Exception ex) {
            return false;
        }


        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Does nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Does nothing.
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Pick up the selected color and call method to change the VOI color.
     */
    class OkColorListener implements ActionListener {

        /**
         * Get color from chooser and set button and VOI color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color newColor = chooser.getColor();
            colorButton.setBackground(newColor);
            color = newColor;
        }
    }


}
