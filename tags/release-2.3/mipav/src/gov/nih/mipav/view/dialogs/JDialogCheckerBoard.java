package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 *
 *   Dialog to get the row and column numbers of
 *   checkerboard squares
 *
 *       @see        ViewJComponentEditImage
 */
public class JDialogCheckerBoard extends JDialogBase
    implements ChangeListener {

    private ViewJComponentEditImage compImage;
    private ViewJComponentRegistration regImage;
    private boolean doReg = false;

    private int maxRow;
    private int maxColumn;

    private JCheckBox doCheckbox;
    private JLabel labelColumnNumber, labelRowNumber;
    private JTextField textRowNumber, textColumnNumber;
    private JSlider slider, slider2;
    private Hashtable labelTable, labelTable2;
    private JButton closeButton;

    /**
     *  Creates new dialog and sets up GUI components.
     *  @param parent          Parent frame.
     *  @param im              Source image.
     */
    public JDialogCheckerBoard(Frame theParentFrame, ViewJComponentEditImage compImg) {
        super(theParentFrame, false);
        compImage = compImg;
        maxRow = Math.min(compImage.getImageA().getExtents()[1] / 4, 50);
        maxColumn = Math.min(compImage.getImageA().getExtents()[0] / 4, 50);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setup();
    }

    /**
     *  Creates new dialog and sets up GUI components.
     *  @param parent          Parent frame.
     *  @param im              Source image.
     */
    public JDialogCheckerBoard(Frame theParentFrame, ViewJComponentRegistration regImg) {
        super(theParentFrame, false);
        regImage = regImg;
        maxRow = Math.min(regImage.getImageA().getExtents()[1] / 4, 50);
        maxColumn = Math.min(regImage.getImageA().getExtents()[0] / 4, 50);
        doReg = true;
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setup();
    }

    /**
     *  Sets up the GUI components of the dialog.
     */
    private void setup() {
        setForeground(Color.black);

        setTitle("Checkerboard pattern");

        JPanel paramPanel = new JPanel();

        paramPanel.setLayout(new BoxLayout(paramPanel, BoxLayout.Y_AXIS));
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        doCheckbox = new JCheckBox("Use checkerboarding");
        doCheckbox.setFont(serif12);
        doCheckbox.setSelected(true);
        doCheckbox.setEnabled(true);
        doCheckbox.addActionListener(this);
        doCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        paramPanel.add(doCheckbox);

        JPanel rowPanel = new JPanel();

        labelRowNumber = new JLabel("Rows");
        labelRowNumber.setForeground(Color.black);
        labelRowNumber.setFont(serif12);
        labelRowNumber.setEnabled(true);
        rowPanel.add(labelRowNumber);

        rowPanel.add(Box.createHorizontalStrut(10));

        slider = new JSlider(2, maxRow, 2);
        slider.setFont(serif12);
        slider.setEnabled(true);
        slider.setMinorTickSpacing(5);
        slider.setPaintTicks(true);
        slider.addChangeListener(this);
        slider.setVisible(true);
        labelTable = new Hashtable();
        labelTable.put(new Integer(2), createLabel("2"));
        labelTable.put(new Integer(maxRow), createLabel(String.valueOf(maxRow)));
        slider.setLabelTable(labelTable);
        slider.setPaintLabels(true);
        rowPanel.add(slider);

        textRowNumber = new JTextField(String.valueOf(2), 4);
        textRowNumber.setFont(serif12);
        textRowNumber.setEnabled(false);
        textRowNumber.addFocusListener(this);
        rowPanel.add(textRowNumber);

        rowPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        paramPanel.add(rowPanel);

        JPanel columnPanel = new JPanel();

        labelColumnNumber = new JLabel("Columns");
        labelColumnNumber.setForeground(Color.black);
        labelColumnNumber.setFont(serif12);
        labelColumnNumber.setEnabled(true);
        columnPanel.add(labelColumnNumber);

        slider2 = new JSlider(2, maxColumn, 2);
        slider2.setFont(serif12);
        slider2.setEnabled(true);
        slider2.setMinorTickSpacing(5);
        slider2.setPaintTicks(true);
        slider2.addChangeListener(this);
        slider2.setVisible(true);
        labelTable2 = new Hashtable();
        labelTable2.put(new Integer(2), createLabel("2"));
        labelTable2.put(new Integer(maxColumn), createLabel(String.valueOf(maxColumn)));
        slider2.setLabelTable(labelTable2);
        slider2.setPaintLabels(true);
        columnPanel.add(slider2);

        textColumnNumber = new JTextField(String.valueOf(2), 4);
        textColumnNumber.setFont(serif12);
        textColumnNumber.setEnabled(false);
        textColumnNumber.addFocusListener(this);
        columnPanel.add(textColumnNumber);

        columnPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        paramPanel.add(columnPanel);

        getContentPane().add(paramPanel);

        JPanel buttonPanel = new JPanel();

        buildOKButton();
        OKButton.setText("Apply");
        OKButton.setActionCommand("OK");
        buttonPanel.add(OKButton);

        closeButton = new JButton("Close");
        closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        closeButton.setFont(serif12B);
        buttonPanel.add(closeButton);
        closeButton.addActionListener(this);
        closeButton.setActionCommand("Close");

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     *    Sets values based on knob along slider.
     *    @param ChangeEvent  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        int rowNumber, columnNumber;
        Object source = e.getSource();

        if (source == slider) {
            rowNumber = slider.getValue();
            textRowNumber.setText(String.valueOf(rowNumber));
        }
        if (source == slider2) {
            columnNumber = slider2.getValue();
            textColumnNumber.setText(String.valueOf(columnNumber));
        }
    }

    /**
     *  Sets parameters in ViewJComponentEditImage when Apply is pressed.
     *  Closes dialog box in response to both Apply and Cancel buttons.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        int rowNumber, columnNumber;

        String command = event.getActionCommand();
        Object source = event.getSource();

        if ((command.equals("OK")) || (command.equals("Close"))) {
            if (doCheckbox.isSelected()) {
                rowNumber = slider.getValue();
                columnNumber = slider2.getValue();
            } else { // no checkerboarding
                rowNumber = -1;
                columnNumber = -1;
            }
            if (doReg) {
                regImage.setRowColumnCheckers(rowNumber, columnNumber);
            } else {
                compImage.setCheckerboard(rowNumber, columnNumber);
                compImage.repaint();
            }
            if (command.equals("Close")) {
                if (doReg) {
                    regImage.checkerRegDialog = null;
                } else {
                    compImage.checkerDialog = null;
                }
                dispose();
            }
        } else if (command.equals("Cancel")) {
            if (doReg == true) {
                regImage.checkerRegDialog = null;
            } else {
                compImage.checkerDialog = null;
            }
            dispose();
        } else if (source == doCheckbox) {
            if (doCheckbox.isSelected()) {
                slider.setEnabled(true);
                slider2.setEnabled(true);
                labelRowNumber.setEnabled(true);
                labelColumnNumber.setEnabled(true);
                for (Enumeration en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                    ((JLabel) en.nextElement()).setEnabled(true);
                }
                for (Enumeration en = slider2.getLabelTable().elements(); en.hasMoreElements();) {
                    ((JLabel) en.nextElement()).setEnabled(true);
                }
            } else {
                slider.setEnabled(false);
                slider2.setEnabled(false);
                labelRowNumber.setEnabled(false);
                labelColumnNumber.setEnabled(false);
                for (Enumeration en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                    ((JLabel) en.nextElement()).setEnabled(false);
                }
                for (Enumeration en = slider2.getLabelTable().elements(); en.hasMoreElements();) {
                    ((JLabel) en.nextElement()).setEnabled(false);
                }
            }
        }
    }

}
