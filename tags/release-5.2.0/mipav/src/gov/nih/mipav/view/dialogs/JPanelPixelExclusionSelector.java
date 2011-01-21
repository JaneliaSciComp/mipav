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
 * A dialog to permits choosing a range (positive or negative, and floating-point) of values with the intention of
 * the selection indicating a range of values. Includes &quot;Between&quot; cut-off value ranges. It is coded to
 * display the ranges as values for exclusion, but minor rewriting this class to allow operation specific titles.
 */
public class JPanelPixelExclusionSelector extends JPanel implements ActionListener {

    public enum ExclusionRangeType {
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
    private final JTextField boundA;

    /** The upper bound of the exclusion */
    private final JTextField boundB;

    /** Lists available exclusion types */
    private final JComboBox excludeSelection;

    /** The lower limit of the exclusion */
    private Float lowerLimit;

    /** Whether pixels will be excluded from a calculation based on intensity values */
    private final JCheckBox permitExclusion;

    /** held for switching between states of the exclusion. */
    private Float upperLimit;

    /** The range type that this pixel exclusion selector covers. */
    private ExclusionRangeType rangeFlag;
    
    /** A reference to the JDialogVOIStatistic or JDialogVOIStats check box panel. */
    private JPanelStatisticsList checkBoxPanel;

    /**
     * Creates an exclusion panel which has a checkbox to make the range controls available, a selector to choose
     * the range controls (&quot;Between&quot;, &quot;Above&quot; and &quot;Below&quot;), and the range inputs for
     * these controls.
     */
    public JPanelPixelExclusionSelector(JPanelStatisticsList checkBoxPanel) {
        super(new GridBagLayout());
        this.setBorder(new TitledBorder(new EtchedBorder(), "Pixel Exclusion", TitledBorder.DEFAULT_JUSTIFICATION,
                TitledBorder.DEFAULT_POSITION, MipavUtil.font12B));

        this.checkBoxPanel = checkBoxPanel; //internal reference to external checkboxpanel
        
        ExcluderOptionsActionListener optionsListener = new ExcluderOptionsActionListener();
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
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
    }

    /**
     * When state changes in some elements of the panel, the panel must make display changes; these changes are
     * registered here. When state the state of the checkbox changes (from checked to un- or vice-versa), the
     * excluded selection's state is changed and the Exlusion range property is reset.
     * 
     * <p>
     * Checks state of:
     * </p>
     * 
     * <ul>
     * <li>Enables or disables the exclusion drop-down and the text boxes based on the state of the checkbox</li>
     * <li>Changes the visibility of the text-boxes based on the state of the exclusion dropdown;
     * &quot;Between&quot; displays both text boxes, &quot;Above&quot; only displays the lower cutoff box, and
     * &quot;Below&quot; displays only the upper cutoff box.</li>
     * </ul>
     * 
     * @param e the ChangeEvent to watch.
     */
    public void actionPerformed(final ActionEvent e) {

        if (e.getSource().equals(permitExclusion)) {

            if (permitExclusion.isSelected()) {
                excludeSelection.setEnabled(true);
                selectRangeInput();
            } else {
                excludeSelection.setEnabled(false);
                boundA.setEnabled(false);
                boundB.setEnabled(false);
                rangeFlag = ExclusionRangeType.NO_RANGE;

                // storeLimitValues(); // store before blanking the values
                boundA.setText("");
                boundB.setText("");
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
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being
     * editable as well.
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
     * @return the range type
     */
    public ExclusionRangeType getRangeFlag() {
        return rangeFlag;
    }

    /**
     * Returns the upper bound text as a number. May be too positive for some applications.
     * 
     * <p>
     * There is a side-effect in that when the permitExclusion checkbox is unchecked, the upper bound returned is
     * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being
     * editable as well.
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
     * Sets the fields' value and accessability based on the state of the drop-down. &quot;Above&quot; will display
     * an uneditable &quot;max&quot; value and the lesser of the two values, &quot;Below&quot; will display an
     * uneditable &quot;min&quot; value and the larger of the two values and &quot;Between&quot; will display the
     * largest possible values, if the fields have not been set, or will reset the fields to the stored values.
     */
    public void selectRangeInput() {
        rangeFlag = ExclusionRangeType.NO_RANGE;

        if (excludeSelection.getSelectedItem().equals("Above")) {
            rangeFlag = ExclusionRangeType.BETWEEN;
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
            rangeFlag = ExclusionRangeType.BETWEEN;
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
            rangeFlag = ExclusionRangeType.BETWEEN;
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
            rangeFlag = ExclusionRangeType.OUTSIDE;
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
     * Set the upper bound from the script dialog.
     * 
     * @param floatValue Maximum value string
     */
    public void setUpperBound(final String floatValue) {
        boundB.setText(floatValue);
    }

    /**
     * Tries to store the values held in the text areas to temporary storage. It only does so if there are valid
     * (that is, numbers and that they are neither infinite nor at the maximum or minimum value.
     * 
     * @see Float#MAX_VALUE
     * @see Float#MIN_VALUE
     */
    protected void storeLimitValues() {

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
    }
    
    /**
     * This class listens to excluder options, changing the available statistics calculation options
     * depending on whether all contours are still closed. 
     * 
     * @author senseneyj
     */
    private class ExcluderOptionsActionListener implements ActionListener {
        
        public void actionPerformed(ActionEvent e) {
            if(permitExclusion.isSelected()) {
                checkBoxPanel.isOpenContour(true); //open contours are possible when pixels are being excluded from calculation
            } else {
                checkBoxPanel.isOpenContour(false); //TODO: if statistics generator is able to handle non-closed VOIs, this should be changed
            }
        }        
    }
}