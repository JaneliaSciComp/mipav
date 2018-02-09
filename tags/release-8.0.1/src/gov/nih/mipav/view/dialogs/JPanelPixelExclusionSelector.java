package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.JPanelStatisticsList;
import gov.nih.mipav.view.MipavUtil;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;


/**
 * A dialog to permits choosing a range (positive or negative, and floating-point) of values with the intention of the
 * selection indicating a range of values. Includes &quot;Between&quot; cut-off value ranges. It is coded to display the
 * ranges as values for exclusion, but minor rewriting this class to allow operation specific titles.
 */
public class JPanelPixelExclusionSelector extends JPanel implements ActionListener {

    public enum RangeType {
        /** No pixels will be excluded from a calculation. */
        NO_RANGE,
        /** Pixels between boundA and boundB (inclusive) will be excluded from a calculation. */
        BETWEEN,
        /** Pixels outside the range of boundA to boundB (inclusive) will be excluded from a calculation. */
        OUTSIDE;
    }

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1471564039185960351L;

    /** The lower bound of the exclusion */
    private JTextField boundA = null;

    /** The upper bound of the exclusion */
    private JTextField boundB = null;

    /** The lower red bound of the exclusion */
    private JTextField boundLR = null;

    /** The upper red bound of the exclusion */
    private JTextField boundUR = null;

    /** The lower green bound of the exclusion */
    private JTextField boundLG = null;

    /** The upper green bound of the exclusion */
    private JTextField boundUG = null;

    /** The lower blue bound of the exclusion */
    private JTextField boundLB = null;

    /** The upper blue bound of the exclusion */
    private JTextField boundUB = null;

    /** Lists available exclusion types */
    private final JComboBox excludeSelection;

    /** The lower limit of the exclusion */
    private Float lowerLimit;

    private Float lowerLimitR;

    private Float lowerLimitG;

    private Float lowerLimitB;

    /** Whether pixels will be excluded from a calculation based on intensity values */
    private final JCheckBox permitExclusion;

    /** held for switching between states of the exclusion. */
    private Float upperLimit;

    private Float upperLimitR;

    private Float upperLimitG;

    private Float upperLimitB;

    /** The range type that this pixel exclusion selector covers. */
    private RangeType rangeFlag = RangeType.NO_RANGE;

    /** A reference to the JDialogVOIStatistic or JDialogVOIStats check box panel. */
    private final JPanelStatisticsList checkBoxPanel;

    private final boolean doColor;

    /**
     * Creates an exclusion panel which has a checkbox to make the range controls available, a selector to choose the
     * range controls (&quot;Between&quot;, &quot;Above&quot; and &quot;Below&quot;), and the range inputs for these
     * controls.
     */
    public JPanelPixelExclusionSelector(final JPanelStatisticsList checkBoxPanel, final boolean doColor) {
        super(new GridBagLayout());
        this.doColor = doColor;
        this.setBorder(new TitledBorder(new EtchedBorder(), "Pixel Exclusion", TitledBorder.DEFAULT_JUSTIFICATION,
                TitledBorder.DEFAULT_POSITION, MipavUtil.font12B));

        this.checkBoxPanel = checkBoxPanel; // internal reference to external checkboxpanel

        final ExcluderOptionsActionListener optionsListener = new ExcluderOptionsActionListener();

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        if ( !doColor) {
            gbc.gridwidth = 3;
        } else {
            gbc.gridwidth = 4;
        }
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.WEST;

        // add a checkbox to enable the enter-panel
        permitExclusion = new JCheckBox("Exclude pixels from calculation");
        permitExclusion.setFont(MipavUtil.font12);
        add(permitExclusion, gbc);
        permitExclusion.addActionListener(this);
        permitExclusion.addActionListener(optionsListener);

        final JLabel excludeLabel = new JLabel("Exclude pixels");
        excludeLabel.setFont(MipavUtil.font12);
        gbc.gridy++;
        gbc.insets = new Insets(2, 2, 1, 2);
        add(excludeLabel, gbc);

        final String[] selectors = {"Between", "Above", "Below", "Outside"};
        excludeSelection = new JComboBox(selectors);
        excludeSelection.setActionCommand("Exclusion Range");
        excludeSelection.setEditable(false);
        excludeSelection.setEnabled(false);
        excludeSelection.addActionListener(this);

        gbc.gridy++;
        gbc.insets = new Insets(1, 2, 2, 2);
        gbc.fill = GridBagConstraints.BOTH;
        add(excludeSelection, gbc);
        if ( !doColor) {
            boundA = new JTextField(3);
            MipavUtil.makeNumericsOnly(boundA, true, true);
            boundA.setEnabled(false);
            gbc.gridy++;
            gbc.insets = new Insets(7, 2, 7, 0);
            gbc.gridwidth = 1;
            gbc.weightx = .45;
            add(boundA, gbc);
            gbc.gridx++;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .1;
            gbc.insets = new Insets(7, 8, 7, 0);
            add(new JLabel(" - "), gbc);

            boundB = new JTextField(3);
            MipavUtil.makeNumericsOnly(boundB, true, true);
            boundB.setEnabled(false);
            gbc.gridx++;
            gbc.insets = new Insets(7, 0, 7, 2);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = .45;
            add(boundB, gbc);
        } // if (!doColor)
        else { // doColor
            gbc.gridx = 0;
            gbc.gridy++;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .1;
            gbc.insets = new Insets(7, 0, 7, 0);
            add(new JLabel("R"), gbc);
            boundLR = new JTextField(5);
            MipavUtil.makeNumericsOnly(boundLR, true, true);
            boundLR.setEnabled(false);
            gbc.gridx++;
            gbc.insets = new Insets(7, 2, 7, 0);
            gbc.gridwidth = 1;
            gbc.weightx = .45;
            add(boundLR, gbc);
            gbc.gridx++;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .1;
            gbc.insets = new Insets(7, 2, 7, 2);
            add(new JLabel(" - "), gbc);

            boundUR = new JTextField(5);
            MipavUtil.makeNumericsOnly(boundUR, true, true);
            boundUR.setEnabled(false);
            gbc.gridx++;
            gbc.insets = new Insets(7, 0, 7, 2);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = .45;
            add(boundUR, gbc);

            gbc.gridx = 0;
            gbc.gridy++;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .1;
            gbc.insets = new Insets(7, 0, 7, 0);
            add(new JLabel("G"), gbc);
            boundLG = new JTextField(5);
            MipavUtil.makeNumericsOnly(boundLG, true, true);
            boundLG.setEnabled(false);
            gbc.gridx++;
            gbc.insets = new Insets(7, 2, 7, 0);
            gbc.gridwidth = 1;
            gbc.weightx = .45;
            add(boundLG, gbc);
            gbc.gridx++;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .1;
            gbc.insets = new Insets(7, 2, 7, 2);
            add(new JLabel(" - "), gbc);

            boundUG = new JTextField(5);
            MipavUtil.makeNumericsOnly(boundUG, true, true);
            boundUG.setEnabled(false);
            gbc.gridx++;
            gbc.insets = new Insets(7, 0, 7, 2);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = .45;
            add(boundUG, gbc);

            gbc.gridx = 0;
            gbc.gridy++;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .1;
            gbc.insets = new Insets(7, 0, 7, 0);
            add(new JLabel("B"), gbc);
            boundLB = new JTextField(5);
            MipavUtil.makeNumericsOnly(boundLB, true, true);
            boundLB.setEnabled(false);
            gbc.gridx++;
            gbc.insets = new Insets(7, 2, 7, 0);
            gbc.gridwidth = 1;
            gbc.weightx = .45;
            add(boundLB, gbc);
            gbc.gridx++;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .1;
            gbc.insets = new Insets(7, 2, 7, 2);
            add(new JLabel(" - "), gbc);

            boundUB = new JTextField(5);
            MipavUtil.makeNumericsOnly(boundUB, true, true);
            boundUB.setEnabled(false);
            gbc.gridx++;
            gbc.insets = new Insets(7, 0, 7, 2);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = .45;
            add(boundUB, gbc);
        } // else doColor
    }

    /**
     * When state changes in some elements of the panel, the panel must make display changes; these changes are
     * registered here. When state the state of the checkbox changes (from checked to un- or vice-versa), the excluded
     * selection's state is changed and the Exclusion range property is reset.
     * 
     * <p>
     * Checks state of:
     * </p>
     * 
     * <ul>
     * <li>Enables or disables the exclusion drop-down and the text boxes based on the state of the checkbox</li>
     * <li>Changes the visibility of the text-boxes based on the state of the exclusion dropdown; &quot;Between&quot;
     * displays both text boxes, &quot;Above&quot; only displays the lower cutoff box, and &quot;Below&quot; displays
     * only the upper cutoff box.</li>
     * </ul>
     * 
     * @param e the ChangeEvent to watch.
     */
    @Override
    public void actionPerformed(final ActionEvent e) {

        if (e.getSource().equals(permitExclusion)) {

            if (permitExclusion.isSelected()) {
                excludeSelection.setEnabled(true);
                selectRangeInput();
            } else {
                excludeSelection.setEnabled(false);
                if (doColor) {
                    boundLR.setEnabled(false);
                    boundUR.setEnabled(false);
                    boundLG.setEnabled(false);
                    boundUG.setEnabled(false);
                    boundLB.setEnabled(false);
                    boundUB.setEnabled(false);
                    rangeFlag = RangeType.NO_RANGE;

                    // storeLimitValues(); // store before blanking the values
                    boundLR.setText("");
                    boundUR.setText("");
                    boundLG.setText("");
                    boundUG.setText("");
                    boundLB.setText("");
                    boundUB.setText("");
                } else {
                    boundA.setEnabled(false);
                    boundB.setEnabled(false);
                    rangeFlag = RangeType.NO_RANGE;

                    // storeLimitValues(); // store before blanking the values
                    boundA.setText("");
                    boundB.setText("");
                }
            }
        }

        if (e.getActionCommand().equals("Exclusion Range")) {
            selectRangeInput();
        }
    }

    /**
     * Returns the lower bound text as a number. May be too negative for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the lower bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return lower bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getLowerBound() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundA.getText()) < Float.parseFloat(boundB.getText())) {
                return new Float(boundA.getText());
            } else {
                return new Float(boundB.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * Returns the lower bound text as a number. May be too negative for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the lower bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return lower bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getLowerBoundR() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundLR.getText()) < Float.parseFloat(boundUR.getText())) {
                return new Float(boundLR.getText());
            } else {
                return new Float(boundUR.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * Returns the lower bound text as a number. May be too negative for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the lower bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return lower bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getLowerBoundG() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundLG.getText()) < Float.parseFloat(boundUG.getText())) {
                return new Float(boundLG.getText());
            } else {
                return new Float(boundUG.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * Returns the lower bound text as a number. May be too negative for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the lower bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return lower bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getLowerBoundB() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundLB.getText()) < Float.parseFloat(boundUB.getText())) {
                return new Float(boundLB.getText());
            } else {
                return new Float(boundUB.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * @return the range type
     */
    public RangeType getRangeFlag() {
        return rangeFlag;
    }

    /**
     * Returns the upper bound text as a number. May be too positive for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the upper bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return upper bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getUpperBound() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundA.getText()) > Float.parseFloat(boundB.getText())) {
                return new Float(boundA.getText());
            } else {
                return new Float(boundB.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * Returns the upper bound text as a number. May be too positive for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the upper bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return upper bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getUpperBoundR() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundLR.getText()) > Float.parseFloat(boundUR.getText())) {
                return new Float(boundLR.getText());
            } else {
                return new Float(boundUR.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * Returns the upper bound text as a number. May be too positive for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the upper bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return upper bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getUpperBoundG() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundLG.getText()) > Float.parseFloat(boundUG.getText())) {
                return new Float(boundLG.getText());
            } else {
                return new Float(boundUG.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * Returns the upper bound text as a number. May be too positive for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the upper bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being editable
     * as well.
     * </p>
     * 
     * @return upper bound text as a Float; null is returned if the panel is not set to be used or one of the text
     *         entries is empty or not a number.
     */
    public Float getUpperBoundB() {

        if ( !permitExclusion.isSelected()) {
            return null;
        }

        try {

            if (Float.parseFloat(boundLB.getText()) > Float.parseFloat(boundUB.getText())) {
                return new Float(boundLB.getText());
            } else {
                return new Float(boundUB.getText());
            }
        } catch (final NumberFormatException notANumber) {
            return null;
        } catch (final NullPointerException noNumber) {
            return null;
        }
    }

    /**
     * Sets the fields' value and accessability based on the state of the drop-down. &quot;Above&quot; will display an
     * uneditable &quot;max&quot; value and the lesser of the two values, &quot;Below&quot; will display an uneditable
     * &quot;min&quot; value and the larger of the two values and &quot;Between&quot; will display the largest possible
     * values, if the fields have not been set, or will reset the fields to the stored values.
     */
    public void selectRangeInput() {
        rangeFlag = RangeType.NO_RANGE;
        if (doColor) {
            if (excludeSelection.getSelectedItem().equals("Above")) {
                rangeFlag = RangeType.BETWEEN;
                storeLimitValues();
                boundUR.setEnabled(false);
                boundUR.setText(Float.toString(Float.MAX_VALUE));
                boundLR.setEnabled(true);
                boundUG.setEnabled(false);
                boundUG.setText(Float.toString(Float.MAX_VALUE));
                boundLG.setEnabled(true);
                boundUB.setEnabled(false);
                boundUB.setText(Float.toString(Float.MAX_VALUE));
                boundLB.setEnabled(true);

                try {
                    boundLR.setText(lowerLimitR.toString());
                    boundLG.setText(lowerLimitG.toString());
                    boundLB.setText(lowerLimitB.toString());
                } catch (final NullPointerException noLower) {
                    boundLR.setText(Float.toString( -Float.MAX_VALUE));
                    boundLG.setText(Float.toString( -Float.MAX_VALUE));
                    boundLB.setText(Float.toString( -Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Below")) {
                rangeFlag = RangeType.BETWEEN;
                storeLimitValues();
                boundLR.setEnabled(false);
                boundLR.setText(Float.toString( -Float.MAX_VALUE));
                boundUR.setEnabled(true);
                boundLG.setEnabled(false);
                boundLG.setText(Float.toString( -Float.MAX_VALUE));
                boundUG.setEnabled(true);
                boundLB.setEnabled(false);
                boundLB.setText(Float.toString( -Float.MAX_VALUE));
                boundUB.setEnabled(true);

                try {
                    boundUR.setText(upperLimitR.toString());
                    boundUG.setText(upperLimitG.toString());
                    boundUB.setText(upperLimitB.toString());
                } catch (final NullPointerException noUpper) {
                    boundUR.setText(Float.toString(Float.MAX_VALUE));
                    boundUG.setText(Float.toString(Float.MAX_VALUE));
                    boundUB.setText(Float.toString(Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Between")) {

                // set both text-inputs as needed, then make them editable
                rangeFlag = RangeType.BETWEEN;
                storeLimitValues();

                if (lowerLimit != null) {
                    boundLR.setText(lowerLimitR.toString());
                    boundLG.setText(lowerLimitG.toString());
                    boundLB.setText(lowerLimitB.toString());
                } else {
                    boundLR.setText(Float.toString( -Float.MAX_VALUE));
                    boundLG.setText(Float.toString( -Float.MAX_VALUE));
                    boundLB.setText(Float.toString( -Float.MAX_VALUE));
                }

                if (upperLimit != null) {
                    boundUR.setText(upperLimitR.toString());
                    boundUG.setText(upperLimitG.toString());
                    boundUB.setText(upperLimitB.toString());
                } else {
                    boundUR.setText(Float.toString(Float.MAX_VALUE));
                    boundUG.setText(Float.toString(Float.MAX_VALUE));
                    boundUB.setText(Float.toString(Float.MAX_VALUE));
                }

                boundLR.setEnabled(true);
                boundUR.setEnabled(true);
                boundLG.setEnabled(true);
                boundUG.setEnabled(true);
                boundLB.setEnabled(true);
                boundUB.setEnabled(true);
            } else if (excludeSelection.getSelectedItem().equals("Outside")) {
                rangeFlag = RangeType.OUTSIDE;
                storeLimitValues();

                // set both text-inputs as needed, then make them editable
                if (lowerLimit != null) {
                    boundLR.setText(lowerLimitR.toString());
                    boundLG.setText(lowerLimitG.toString());
                    boundLB.setText(lowerLimitB.toString());
                } else {
                    boundLR.setText(Float.toString( -Float.MAX_VALUE));
                    boundLG.setText(Float.toString( -Float.MAX_VALUE));
                    boundLB.setText(Float.toString( -Float.MAX_VALUE));
                }

                if (upperLimit != null) {
                    boundUR.setText(upperLimitR.toString());
                    boundUG.setText(upperLimitG.toString());
                    boundUB.setText(upperLimitB.toString());
                } else {
                    boundUR.setText(Float.toString(Float.MAX_VALUE));
                    boundUG.setText(Float.toString(Float.MAX_VALUE));
                    boundUB.setText(Float.toString(Float.MAX_VALUE));
                }

                boundLR.setEnabled(true);
                boundUR.setEnabled(true);
                boundLG.setEnabled(true);
                boundUG.setEnabled(true);
                boundLB.setEnabled(true);
                boundUB.setEnabled(true);
            }
        } // if (doColor)
        else { // black and white
            if (excludeSelection.getSelectedItem().equals("Above")) {
                rangeFlag = RangeType.BETWEEN;
                storeLimitValues();
                boundB.setEnabled(false);
                boundB.setText(Float.toString(Float.MAX_VALUE));
                boundA.setEnabled(true);

                try {
                    boundA.setText(lowerLimit.toString());
                } catch (final NullPointerException noLower) {
                    boundA.setText(Float.toString( -Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Below")) {
                rangeFlag = RangeType.BETWEEN;
                storeLimitValues();
                boundA.setEnabled(false);
                boundA.setText(Float.toString( -Float.MAX_VALUE));
                boundB.setEnabled(true);

                try {
                    boundB.setText(upperLimit.toString());
                } catch (final NullPointerException noUpper) {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Between")) {

                // set both text-inputs as needed, then make them editable
                rangeFlag = RangeType.BETWEEN;
                storeLimitValues();

                if (lowerLimit != null) {
                    boundA.setText(lowerLimit.toString());
                } else {
                    boundA.setText(Float.toString( -Float.MAX_VALUE));
                }

                if (upperLimit != null) {
                    boundB.setText(upperLimit.toString());
                } else {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }

                boundA.setEnabled(true);
                boundB.setEnabled(true);
            } else if (excludeSelection.getSelectedItem().equals("Outside")) {
                rangeFlag = RangeType.OUTSIDE;
                storeLimitValues();

                // set both text-inputs as needed, then make them editable
                if (lowerLimit != null) {
                    boundA.setText(lowerLimit.toString());
                } else {
                    boundA.setText(Float.toString( -Float.MAX_VALUE));
                }

                if (upperLimit != null) {
                    boundB.setText(upperLimit.toString());
                } else {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }

                boundA.setEnabled(true);
                boundB.setEnabled(true);
            }
        } // else black and white
    }

    /**
     * Set the lower bound from the script dialog.
     * 
     * @param floatValue lower bound string
     */
    public void setLowerBound(final String floatValue) {
        boundA.setText(floatValue);
    }

    /**
     * Set the lower red bound from the script dialog.
     * 
     * @param floatValue lower red bound string
     */
    public void setLowerBoundR(final String floatValue) {
        boundLR.setText(floatValue);
    }

    /**
     * Set the lower green bound from the script dialog.
     * 
     * @param floatValue lower green bound string
     */
    public void setLowerBoundG(final String floatValue) {
        boundLG.setText(floatValue);
    }

    /**
     * Set the lower blue bound from the script dialog.
     * 
     * @param floatValue lower blue bound string
     */
    public void setLowerBoundB(final String floatValue) {
        boundLB.setText(floatValue);
    }

    /**
     * Set the upper bound from the script dialog.
     * 
     * @param floatValue Maximum value string
     */
    public void setUpperBound(final String floatValue) {
        boundB.setText(floatValue);
    }

    /**
     * Set the upper red bound from the script dialog.
     * 
     * @param floatValue Maximum red value string
     */
    public void setUpperBoundR(final String floatValue) {
        boundUR.setText(floatValue);
    }

    /**
     * Set the upper green bound from the script dialog.
     * 
     * @param floatValue Maximum green value string
     */
    public void setUpperBoundG(final String floatValue) {
        boundUG.setText(floatValue);
    }

    /**
     * Set the upper blue bound from the script dialog.
     * 
     * @param floatValue Maximum blue value string
     */
    public void setUpperBoundB(final String floatValue) {
        boundUB.setText(floatValue);
    }

    /**
     * Tries to store the values held in the text areas to temporary storage. It only does so if there are valid (that
     * is, numbers and that they are neither infinite nor at the maximum or minimum value.
     * 
     * @see Float#MAX_VALUE
     * @see Float#MIN_VALUE
     */
    protected void storeLimitValues() {

        if (doColor) {
            /*
             * try to store the upper and lower bounds; only do so if they are valid values to store
             */
            try {

                if ( !getUpperBoundR().isInfinite() && !getUpperBoundR().isNaN()
                        && (getUpperBoundR().floatValue() != Float.MAX_VALUE)) {
                    upperLimitR = getUpperBoundR();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing t do */
            }

            try {

                if ( !getLowerBoundR().isInfinite() && !getLowerBoundR().isNaN()
                        && (getLowerBoundR().floatValue() != -Float.MAX_VALUE)) {
                    lowerLimitR = getLowerBoundR();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing to do */
            }

            /*
             * try to store the upper and lower bounds; only do so if they are valid values to store
             */
            try {

                if ( !getUpperBoundG().isInfinite() && !getUpperBoundG().isNaN()
                        && (getUpperBoundG().floatValue() != Float.MAX_VALUE)) {
                    upperLimitG = getUpperBoundG();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing t do */
            }

            try {

                if ( !getLowerBoundG().isInfinite() && !getLowerBoundG().isNaN()
                        && (getLowerBoundG().floatValue() != -Float.MAX_VALUE)) {
                    lowerLimitG = getLowerBoundG();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing to do */
            }

            /*
             * try to store the upper and lower bounds; only do so if they are valid values to store
             */
            try {

                if ( !getUpperBoundB().isInfinite() && !getUpperBoundB().isNaN()
                        && (getUpperBoundB().floatValue() != Float.MAX_VALUE)) {
                    upperLimitB = getUpperBoundB();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing t do */
            }

            try {

                if ( !getLowerBoundB().isInfinite() && !getLowerBoundB().isNaN()
                        && (getLowerBoundB().floatValue() != -Float.MAX_VALUE)) {
                    lowerLimitB = getLowerBoundB();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing to do */
            }
        } // if (doColor)
        else { // black and white
            /*
             * try to store the upper and lower bounds; only do so if they are valid values to store
             */
            try {

                if ( !getUpperBound().isInfinite() && !getUpperBound().isNaN()
                        && (getUpperBound().floatValue() != Float.MAX_VALUE)) {
                    upperLimit = getUpperBound();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing t do */
            }

            try {

                if ( !getLowerBound().isInfinite() && !getLowerBound().isNaN()
                        && (getLowerBound().floatValue() != -Float.MAX_VALUE)) {
                    lowerLimit = getLowerBound();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing to do */
            }
        } // else black and white
    }

    /**
     * This class listens to excluder options, changing the available statistics calculation options depending on
     * whether all contours are still closed.
     * 
     * @author senseneyj
     */
    private class ExcluderOptionsActionListener implements ActionListener {

        @Override
        public void actionPerformed(final ActionEvent e) {
            // if(permitExclusion.isSelected()) {
            // checkBoxPanel.isOpenContour(true); //open contours are possible when pixels are being excluded from
            // calculation
            // } else {
            // checkBoxPanel.isOpenContour(false); //TODO: if statistics generator is able to handle non-closed VOIs,
            // this should be changed
            // }
        }
    }
}
