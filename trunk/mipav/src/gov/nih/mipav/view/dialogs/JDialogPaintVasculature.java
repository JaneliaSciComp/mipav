package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog to facilitate the painting of a region of an image (such as the vascualture in a liver mip).
 *
 * @author   Evan McCreedy
 * @version  1.0
 */
public class JDialogPaintVasculature extends JDialogBase
        implements RegionGrowDialog, ChangeListener, ActionListener, KeyListener, MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7746172110383193434L;

    /** The mode where paint grow seed points are automatically determined and chosen by the user. */
    public static final int AUTO_POINT = 0;

    /** The mode where the user picks an arbitrary point to start a paint grow from. */
    public static final int CLICK_POINT = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Boolean which indicates that the seed point intensity bounds were changed from one of the text fields and not
     * from one of the sliders.
     */
    private boolean deltaSetFromField;

    /** Button to indicate that the painting is finished and the dialog and attached image frame should be closed. */
    private JButton doneButton;

    /** Button which indicates that a region grow has been initiated / is being adjusted. */
    private JButton growButton;

    /** Text field containing the seed value intensity. */
    private JTextField intensityTF;

    /** The value of seed points to use. */
    private int intensityValue = 151;

    /** Indicates that we are done with the current grow, and can close the MIP when the done button is next clicked. */
    private boolean isCloseOkay = true;

    /** Slider which adjusts the lower bound of the region grow. */
    private JSlider lowerDeltaSlider;

    /** Text field containing the lower bound of the region grow. */
    private JTextField lowerDeltaTF;

    /** The most we can get away from the initial seed point intensity value (151). */
    private int maxDelta = 60;

    /** The intensity of the current mouse position within the mip. */
    private JLabel mouseIntensityLabel;

    /** The x position of the current mouse position within the mip. */
    private JLabel mouseXLabel;

    /** The y position of the current mouse position within the mip. */
    private JLabel mouseYLabel;

    /**
     * The z position of the current mouse position within the mip (the z position of the volume voxel put into the
     * mip).
     */
    private JLabel mouseZLabel;

    /**
     * The intensity value to use to recalculate the possible seed points when switching from CLICK_POINT mode back to
     * AUTO_MODE.
     */
    private int oldSeedIntensity = intensityValue;

    /** List of frames that want to know about paint updates. */
    private Vector<ViewJComponentEditImage> paintGrowListeners;

    /** The point selection mode. */
    private int paintGrowMode = AUTO_POINT;

    /** The main RFAST frame. */
    private ViewJFrameImage rfastFrame;

    /** List of points where we have grown a region from. */
    private Vector<Vector3f> seedPoints;

    /** The dialog's toolbar panel. */
    private JPanel toolbarPanel;

    /**
     * Button to undo the last region grow change (either a click of the grow button, or an adjustment of the threshold
     * sliders).
     */
    private JButton undoButton;

    /** Slider which adjusts the upper bound of the region grow. */
    private JSlider upperDeltaSlider;

    /** Text field containing the upper bound of the region grow. */
    private JTextField upperDeltaTF;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create the paint vasculature dialog.
     *
     * @param  parent  the parent frame which this dialog is attached to
     */
    public JDialogPaintVasculature(ViewJFrameBase parent) {
        super(parent, false);
        parentFrame = parent;

        rfastFrame = (ViewJFrameImage) ((ViewJFramePaintVasculature) parentFrame).getParentFrame();

        paintGrowListeners = new Vector<ViewJComponentEditImage>();
        paintGrowListeners.add(((ViewJFramePaintVasculature) parentFrame).getComponentImage());
        paintGrowListeners.add(rfastFrame.getComponentImage());

        seedPoints = new Vector<Vector3f>();

        ((ViewJFramePaintVasculature) parentFrame).getComponentImage().addMouseListener(this);
        ((ViewJFramePaintVasculature) parentFrame).getComponentImage().addMouseMotionListener(this);

        initGUI();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Handle events from the dialog's buttons.
     *
     * @param  event  gui action events
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Grow")) {
            rfastFrame.getComponentImage().setLess(intensityValue - Float.parseFloat(lowerDeltaTF.getText()));
            rfastFrame.getComponentImage().setMore(Float.parseFloat(upperDeltaTF.getText()) - intensityValue);

            commitGrow(false);

            // region grow from all currently selected points in the MIP
            VOI voi = ((ViewJFramePaintVasculature) parentFrame).getSeedPoints();
            Vector<VOIBase> points = voi.getCurves();

            for (int i = 0; i < points.size(); i++) {

                if (((VOIPoint) points.get(i)).isActive()) {
                    Vector3f pt = ((VOIPoint) points.get(i)).exportPoint();
                    seedPoints.add(pt);
                    rfastFrame.getComponentImage().regionGrow((short) pt.X, (short) pt.Y, (short) pt.Z, intensityValue,
                                                              null, true);
                }
            }
        } else if (command.equals("Undo")) {

            if (!isCloseOkay) {
                rfastFrame.getComponentImage().undoLastPaint();
            }
        } else if (command.equals("Done")) {

            // could be just commiting the paint, not closing it
            if (!isCloseOkay) {
                commitGrow(true);
            } else {
                dispose();
                ((ViewJFramePaintVasculature) parentFrame).close();
            }
        }
    }

    /**
     * Tells the dialog to commit the current set of region grows and prepare for another set region grows, or the
     * closing of the dialog.
     *
     * @param  flag  whether to commit the grow, or remember to commit later before closing
     */
    public void commitGrow(boolean flag) {

        if (flag) {

            // reset the seed points
            seedPoints.removeAllElements();

            intensityTF.setEnabled(true);

            isCloseOkay = true;
            doneButton.setText("Done");
        } else {
            intensityTF.setEnabled(false);
            doneButton.setText("Commit");
            isCloseOkay = false;
        }
    }

    /**
     * Placeholder.
     *
     * @return  false.
     */
    public boolean getDisplayFuzzy() {
        return false;
    }

    /**
     * Placeholder.
     *
     * @return  -1.
     */
    public float getFuzzyThreshold() {
        return -1;
    }

    /**
     * {@inheritDoc}
     */
    public float getLowerBound() {
        return intensityValue - Float.parseFloat(lowerDeltaTF.getText());
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getLowerBoundB() {
        return 0.0f;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getLowerBoundG() {
        return 0.0f;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getLowerBoundR() {
        return 0.0f;
    }

    /**
     * Placeholder.
     *
     * @return  -1.
     */
    public int getMaxDistance() {
        return -1;
    }

    /**
     * Placeholder.
     *
     * @return  -1.
     */
    public int getMaxSize() {
        return -1;
    }

    /**
     * Get the current seed intensity.
     *
     * @return  the region grow seed intensity to use
     */
    public int getSeedIntensity() {
        return intensityValue;
    }

    /**
     * {@inheritDoc}
     */
    public float getUpperBound() {
        return Float.parseFloat(upperDeltaTF.getText()) - intensityValue;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getUpperBoundB() {
        return 0.0f;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getUpperBoundG() {
        return 0.0f;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getUpperBoundR() {
        return 0.0f;
    }

    /**
     * Placeholder.
     *
     * @return  false.
     */
    public boolean getUseVOI() {
        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getVariableThresholds() {
        return false;
    }

    /**
     * Allow the user to use left and right keys to adjust the intensity bounds sliders, and update the region grow when
     * the user hits enter from within the bounds text fields.
     *
     * @param  event  key event generated by either the sliders or text fields
     */
    public void keyPressed(KeyEvent event) {
        int keyCode = event.getKeyCode();

        // int modifiers = event.getModifiers();
        Object source = event.getSource();

        if (source == upperDeltaSlider) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (upperDeltaSlider.getMaximum() >= 1000) {
                    upperDeltaSlider.setValue(upperDeltaSlider.getValue() + 100);
                } else {
                    upperDeltaSlider.setValue(upperDeltaSlider.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (upperDeltaSlider.getMaximum() >= 1000) {
                    upperDeltaSlider.setValue(upperDeltaSlider.getValue() - 100);
                } else {
                    upperDeltaSlider.setValue(upperDeltaSlider.getValue() - 1);
                }
            }
        } else if (source == lowerDeltaSlider) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (lowerDeltaSlider.getMaximum() >= 1000) {
                    lowerDeltaSlider.setValue(lowerDeltaSlider.getValue() + 100);
                } else {
                    lowerDeltaSlider.setValue(lowerDeltaSlider.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (lowerDeltaSlider.getMaximum() >= 1000) {
                    lowerDeltaSlider.setValue(lowerDeltaSlider.getValue() - 100);
                } else {
                    lowerDeltaSlider.setValue(lowerDeltaSlider.getValue() - 1);
                }
            }
        } else if (source == upperDeltaTF) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(upperDeltaTF.getText()) * 100;

                    if ((newVal > upperDeltaSlider.getMaximum()) || (newVal < upperDeltaSlider.getMinimum())) {
                        upperDeltaTF.setText(String.valueOf(upperDeltaSlider.getValue() / 100.0f));
                    } else {
                        deltaSetFromField = true;
                        upperDeltaSlider.setValue(newVal);
                    }
                } catch (Exception ex) {
                    upperDeltaTF.setText(String.valueOf(upperDeltaSlider.getValue() / 100.0f));
                }
            }
        } else if (source == lowerDeltaTF) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(lowerDeltaTF.getText()) * 100;

                    if ((newVal > lowerDeltaSlider.getMaximum()) || (newVal < lowerDeltaSlider.getMinimum())) {
                        lowerDeltaTF.setText(String.valueOf(lowerDeltaSlider.getValue() / 100.0f));
                    } else {
                        deltaSetFromField = true;
                        lowerDeltaSlider.setValue(newVal);
                    }
                } catch (Exception ex) {
                    lowerDeltaTF.setText(String.valueOf(lowerDeltaSlider.getValue() / 100.0f));
                }
            }
        } else if (source == intensityTF) {

            if (keyCode == KeyEvent.VK_ENTER) {
                int newVal = Integer.parseInt(intensityTF.getText());
                intensityValue = newVal;
                updateSliderValues();
                ((ViewJFramePaintVasculature) parentFrame).findSeedPoints(newVal);
            }
        }
    }

    /**
     * Placeholder required by KeyListener. Does nothing.
     *
     * @param  e  KeyEvent
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * Placeholder required by KeyListener. Does nothing.
     *
     * @param  e  KeyEvent
     */
    public void keyTyped(KeyEvent e) { }

    /**
     * {@inheritDoc}
     */
    public void mouseClicked(MouseEvent event) {

        if (paintGrowMode == CLICK_POINT) {
            lowerDeltaSlider.removeChangeListener(this);
            upperDeltaSlider.removeChangeListener(this);

            float zoom = ((ViewJFramePaintVasculature) parentFrame).getComponentImage().getZoomX();
            int mipX = (int) (event.getX() / zoom);
            int mipY = (int) (event.getY() / zoom);
            short z = (short) ((ViewJFramePaintVasculature) parentFrame).getMIPZValue(mipX +
                                                                                      (mipY *
                                                                                           rfastFrame.getActiveImage().getExtents()[0]));
            intensityValue = ((ViewJFramePaintVasculature) parentFrame).getImageA().getInt(mipX, mipY);

            commitGrow(false);

            lowerDeltaSlider.setEnabled(true);
            upperDeltaSlider.setEnabled(true);
            updateSliderValues();

            intensityTF.setText("" + intensityValue);

            // remember the point, so we can change the thresholds
            seedPoints.removeAllElements();
            seedPoints.add(new Vector3f(mipX, mipY, z));

            rfastFrame.getComponentImage().regionGrow((short) mipX, (short) mipY, z, intensityValue, null, true);

            lowerDeltaSlider.addChangeListener(this);
            upperDeltaSlider.addChangeListener(this);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void mouseDragged(MouseEvent event) {
        float zoom = ((ViewJFramePaintVasculature) parentFrame).getComponentImage().getZoomX();
        int mipX = (int) (event.getX() / zoom);
        int mipY = (int) (event.getY() / zoom);
        short z = (short) ((ViewJFramePaintVasculature) parentFrame).getMIPZValue(mipX +
                                                                                  (mipY *
                                                                                       rfastFrame.getActiveImage().getExtents()[0]));
        float val = ((ViewJFramePaintVasculature) parentFrame).getImageA().getFloat(mipX, mipY);

        mouseXLabel.setText("" + mipX);
        mouseYLabel.setText("" + mipY);
        mouseZLabel.setText("" + z);
        mouseIntensityLabel.setText("" + val);

        if (intensityTF.getText().equals("")) {
            lowerDeltaTF.setText("" + (val - (maxDelta / 2)));
            upperDeltaTF.setText("" + (val + (maxDelta / 2)));
        }
    }

    /**
     * Do nothing. Required by MouseListener interface.
     *
     * @param  event  the mouse event
     */
    public void mouseEntered(MouseEvent event) {
        // Do nothing
    }

    /**
     * {@inheritDoc}
     */
    public void mouseExited(MouseEvent event) {
        mouseXLabel.setText("");
        mouseYLabel.setText("");
        mouseZLabel.setText("");
        mouseIntensityLabel.setText("");

        if (intensityTF.getText().equals("")) {
            lowerDeltaTF.setText("");
            upperDeltaTF.setText("");
        }
    }

    /**
     * {@inheritDoc}
     */
    public void mouseMoved(MouseEvent event) {
        float zoom = ((ViewJFramePaintVasculature) parentFrame).getComponentImage().getZoomX();
        int mipX = (int) (event.getX() / zoom);
        int mipY = (int) (event.getY() / zoom);
        short z = (short) ((ViewJFramePaintVasculature) parentFrame).getMIPZValue(mipX +
                                                                                  (mipY *
                                                                                       rfastFrame.getActiveImage().getExtents()[0]));
        float val = ((ViewJFramePaintVasculature) parentFrame).getImageA().getFloat(mipX, mipY);

        mouseXLabel.setText("" + mipX);
        mouseYLabel.setText("" + mipY);
        mouseZLabel.setText("" + z);
        mouseIntensityLabel.setText("" + val);

        if (intensityTF.getText().equals("")) {
            lowerDeltaTF.setText("" + (val - (maxDelta / 2)));
            upperDeltaTF.setText("" + (val + (maxDelta / 2)));
        }
    }

    /**
     * Do nothing. Required by MouseListener interface.
     *
     * @param  event  the mouse event
     */
    public void mousePressed(MouseEvent event) {
        // Do nothing
    }

    /**
     * Do nothing. Required by MouseListener interface.
     *
     * @param  event  the mouse event
     */
    public void mouseReleased(MouseEvent event) {
        // Do nothing
    }

    /**
     * {@inheritDoc}
     */
    public void notifyPaintListeners(boolean isRegionGrow, boolean backup, BitSet paintMask) {
        BitSet paintRegion = null;
        paintRegion = paintMask;

        PaintGrowListener curListener = null, grower = null;
        grower = (PaintGrowListener) rfastFrame.getComponentImage();

        for (int i = 0; i < paintGrowListeners.size(); i++) {
            curListener = (PaintGrowListener) paintGrowListeners.elementAt(i);

            if ((curListener == grower) && isRegionGrow) {
                curListener.updatePaint(paintRegion, backup, true);
            } else {
                curListener.updatePaint(paintRegion, backup, false);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void resetDialogs() {

        for (int i = 0; i < paintGrowListeners.size(); i++) {
            ((PaintGrowListener) paintGrowListeners.elementAt(i)).setGrowDialog(null);
        }
    }

    /**
     * Change the method for determining the points to paint grow from.
     *
     * @param  mode  the new paint grow mode
     */
    public void setPaintGrowMode(int mode) {
        paintGrowMode = mode;

        if (mode == AUTO_POINT) {
            seedPoints.removeAllElements();

            intensityValue = oldSeedIntensity;
            intensityTF.setText("" + intensityValue);

            lowerDeltaSlider.setEnabled(true);
            upperDeltaSlider.setEnabled(true);

            updateSliderValues();
            ((ViewJFramePaintVasculature) parentFrame).findSeedPoints(intensityValue);

            intensityTF.setEnabled(true);
            growButton.setEnabled(true);

            commitGrow(true);
        } else if (mode == CLICK_POINT) {
            oldSeedIntensity = Integer.parseInt(intensityTF.getText());
            intensityTF.setEnabled(false);
            intensityTF.setText("");
            lowerDeltaSlider.setEnabled(false);
            lowerDeltaTF.setText("");
            upperDeltaSlider.setEnabled(false);
            upperDeltaTF.setText("");
            growButton.setEnabled(false);
        }
    }

    /**
     * Placeholder.
     *
     * @param  posString  doesn't matter.
     */
    public void setPositionText(String posString) { }

    /**
     * Placeholder.
     *
     * @param  regionGrowAlgo  doesn't matter.
     */
    public void setRegionGrowAlgo(AlgorithmRegionGrow regionGrowAlgo) { }

    /**
     * Handle events from the intensity bounds sliders.
     *
     * @param  event  event fired by slider change
     */
    public void stateChanged(ChangeEvent event) {
        Object source = event.getSource();

        if (source == upperDeltaSlider) {

            if (!deltaSetFromField) {
                upperDeltaTF.setText(String.valueOf(upperDeltaSlider.getValue() / (float) 100));
            } else {
                deltaSetFromField = false;
            }
        } else if (source == lowerDeltaSlider) {

            if (!deltaSetFromField) {
                lowerDeltaTF.setText(String.valueOf(lowerDeltaSlider.getValue() / (float) 100));
            } else {
                deltaSetFromField = false;
            }
        }

        // don't adjust the grow if we have committed the last region grow, or if the sliders are still adjusting
        if (!isCloseOkay && ((source == upperDeltaSlider) || (source == lowerDeltaSlider)) &&
                (!upperDeltaSlider.getValueIsAdjusting() && !lowerDeltaSlider.getValueIsAdjusting())) {
            rfastFrame.getComponentImage().setLess(intensityValue - Float.parseFloat(lowerDeltaTF.getText()));
            rfastFrame.getComponentImage().setMore(Float.parseFloat(upperDeltaTF.getText()) - intensityValue);

            // region grow from all the seed points
            for (int i = 0; i < seedPoints.size(); i++) {
                Vector3f pt = (Vector3f) seedPoints.get(i);
                rfastFrame.getComponentImage().setRegionGrowVars((short) pt.X, (short) pt.Y, (short) pt.Z,
                                                                 intensityValue);
                rfastFrame.getComponentImage().regionGrow(null);
            }
        }
    }

    /**
     * Update the lower threshold slider labels and values.
     */
    public void updateLowerSliderValues() {
        int lowerMin = (intensityValue - maxDelta) * 100;
        int lowerMax = intensityValue * 100;
        int lowerInit = (intensityValue - (maxDelta / 2)) * 100;

        lowerDeltaSlider.setMinimum(lowerMin);
        lowerDeltaSlider.setMaximum(lowerMax);
        lowerDeltaSlider.setValue(lowerInit);
        lowerDeltaSlider.setMajorTickSpacing((int) (lowerMax - lowerMin) / 10);

        Hashtable<Integer,JLabel> sliderDictionary = new Hashtable<Integer,JLabel>();

        JLabel minLabel = new JLabel(Float.toString(lowerMin / 100f));
        minLabel.setForeground(Color.black);
        minLabel.setFont(MipavUtil.font12);
        sliderDictionary.put(new Integer(lowerMin), minLabel);

        JLabel midLabel = new JLabel(Float.toString(Math.round(lowerInit / 100f)));
        midLabel.setForeground(Color.black);
        midLabel.setFont(MipavUtil.font12);
        sliderDictionary.put(new Integer(lowerInit), midLabel);

        JLabel maxLabel = new JLabel(Float.toString(lowerMax / 100f));
        maxLabel.setForeground(Color.black);
        maxLabel.setFont(MipavUtil.font12);
        sliderDictionary.put(new Integer(lowerMax), maxLabel);

        lowerDeltaSlider.setLabelTable(sliderDictionary);
    }

    /**
     * Update the slider labels around the current <code>intensityValue</code>.
     */
    public void updateSliderValues() {
        updateLowerSliderValues();
        updateUpperSliderValues();
    }

    /**
     * Update the upper threshold slider labels and values.
     */
    public void updateUpperSliderValues() {
        int upperMin = intensityValue * 100;
        int upperMax = (intensityValue + maxDelta) * 100;
        int upperInit = (intensityValue + (maxDelta / 2)) * 100;

        upperDeltaSlider.setMinimum(upperMin);
        upperDeltaSlider.setMaximum(upperMax);
        upperDeltaSlider.setValue(upperInit);
        upperDeltaSlider.setMajorTickSpacing((int) (upperMax - upperMin) / 10);

        Hashtable<Integer,JLabel> sliderDictionary = new Hashtable<Integer,JLabel>();

        JLabel minLabel = new JLabel(Float.toString(upperMin / 100f));
        minLabel.setForeground(Color.black);
        minLabel.setFont(MipavUtil.font12);
        sliderDictionary.put(new Integer(upperMin), minLabel);

        JLabel midLabel = new JLabel(Float.toString(Math.round(upperInit / 100f)));
        midLabel.setForeground(Color.black);
        midLabel.setFont(MipavUtil.font12);
        sliderDictionary.put(new Integer(upperInit), midLabel);

        JLabel maxLabel = new JLabel(Float.toString(upperMax / 100f));
        maxLabel.setForeground(Color.black);
        maxLabel.setFont(MipavUtil.font12);
        sliderDictionary.put(new Integer(upperMax), maxLabel);

        upperDeltaSlider.setLabelTable(sliderDictionary);
    }

    /**
     * Construct the dialog interface.
     */
    private void initGUI() {
        setTitle("Paint Vasculature");

        initToolbar();

        growButton = new JButton("Grow");
        growButton.setActionCommand("Grow");
        growButton.setFont(MipavUtil.font12B);
        growButton.addActionListener(this);

        undoButton = new JButton("Undo");
        undoButton.setActionCommand("Undo");
        undoButton.setFont(MipavUtil.font12B);
        undoButton.addActionListener(this);

        doneButton = new JButton("Done");
        doneButton.setActionCommand("Done");
        doneButton.setFont(MipavUtil.font12B);
        doneButton.addActionListener(this);

        JPanel buttonPanel = new JPanel(new GridLayout(1, 3));
        buttonPanel.add(growButton);
        buttonPanel.add(undoButton);
        buttonPanel.add(doneButton);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 1;

        JPanel mousePositionPanel = new JPanel(new GridLayout());
        mousePositionPanel.setBorder(MipavUtil.buildTitledBorder("Mouse position and intensity"));

        JLabel tempLabel = new JLabel("X:");
        tempLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(tempLabel);

        gbc.gridx++;
        mouseXLabel = new JLabel();
        mouseXLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(mouseXLabel);

        gbc.gridx++;
        tempLabel = new JLabel("Y:");
        tempLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(tempLabel);

        gbc.gridx++;
        mouseYLabel = new JLabel();
        mouseYLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(mouseYLabel);

        gbc.gridx++;
        tempLabel = new JLabel("Z:");
        tempLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(tempLabel);

        gbc.gridx++;
        mouseZLabel = new JLabel();
        mouseZLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(mouseZLabel);

        gbc.gridx++;
        tempLabel = new JLabel("Intensity: ");
        tempLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(tempLabel);

        gbc.gridx++;
        mouseIntensityLabel = new JLabel();
        mouseIntensityLabel.setFont(MipavUtil.font12);
        mousePositionPanel.add(mouseIntensityLabel);

        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel intensityPanel = new JPanel(new GridBagLayout());
        intensityPanel.setBorder(buildTitledBorder("Region grow start point"));

        JLabel intensityLabel = new JLabel("Intensity value:");
        intensityLabel.setFont(MipavUtil.font12);
        intensityPanel.add(intensityLabel, gbc);
        intensityTF = new JTextField(4);
        intensityTF.setText("" + intensityValue);
        intensityTF.addKeyListener(this);
        gbc.gridx++;
        intensityPanel.add(intensityTF, gbc);

        JPanel lowerDeltaPanel = initLowerSlider();
        JPanel upperDeltaPanel = initUpperSlider();

        mainDialogPanel.setLayout(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainDialogPanel.add(toolbarPanel, gbc);
        gbc.gridy++;
        mainDialogPanel.add(mousePositionPanel, gbc);
        gbc.gridy++;
        mainDialogPanel.add(intensityPanel, gbc);
        gbc.gridy++;
        mainDialogPanel.add(lowerDeltaPanel, gbc);
        gbc.gridy++;
        mainDialogPanel.add(upperDeltaPanel, gbc);
        gbc.gridy++;
        mainDialogPanel.add(buttonPanel, gbc);

        getContentPane().add(mainDialogPanel);
        pack();

        setVisible(true);
    }

    /**
     * Construct the lower intensity bound slider panel.
     *
     * @return  panel which allows adjustment of the lower intensity bound
     */
    private JPanel initLowerSlider() {
        lowerDeltaSlider = new JSlider(JSlider.HORIZONTAL);
        lowerDeltaSlider.setPaintTicks(true);
        lowerDeltaSlider.setPaintLabels(true);
        lowerDeltaSlider.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
        lowerDeltaSlider.addKeyListener(this);

        updateLowerSliderValues();

        // start listening after we set up the labels
        lowerDeltaSlider.addChangeListener(this);

        lowerDeltaTF = new JTextField(4);
        lowerDeltaTF.setText(String.valueOf((float) (lowerDeltaSlider.getValue() / 100.0f)));
        lowerDeltaTF.addKeyListener(this);

        JPanel lowerDeltaPanel = new JPanel(new GridBagLayout());
        lowerDeltaPanel.setBorder(buildTitledBorder("Lower region grow threshold"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        lowerDeltaPanel.add(lowerDeltaSlider, gbc);

        gbc.gridwidth = 1;
        gbc.gridx = 3;
        lowerDeltaPanel.add(lowerDeltaTF, gbc);

        return lowerDeltaPanel;
    }

    /**
     * Construct the dialog toolbar (including zoom, lut, and paint clearing tools).
     */
    private void initToolbar() {
        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(parentFrame);

        ButtonGroup modeGroup = new ButtonGroup();

        JToolBar modeBar = ViewToolBarBuilder.initToolBar();
        JToggleButton pointButton = toolbarBuilder.buildToggleButton("PointRegionGrow",
                                                                     "Region grow from auto-generated points which are likely to be vasculature.",
                                                                     "pointROI", modeGroup);
        pointButton.setSelected(true);
        modeBar.add(pointButton);
        modeBar.add(toolbarBuilder.buildToggleButton("ArbitraryRegionGrow",
                                                     "Select an arbitrary point to region grow from.", "paintcan",
                                                     modeGroup));

        JToolBar zoomBar = ViewToolBarBuilder.initToolBar();
        zoomBar.add(toolbarBuilder.buildButton("MagImage", "Magnify image 2.0x", "zoomin"));
        zoomBar.add(toolbarBuilder.buildButton("UnMagImage", "Magnify image 0.5x", "zoomout"));

        JToolBar paintBar = ViewToolBarBuilder.initToolBar();
        paintBar.add(toolbarBuilder.buildButton("EraseAll", "Erase all paint", "clear"));

        toolbarPanel = new JPanel();
        toolbarPanel.add(modeBar);
        toolbarPanel.add(zoomBar);
        toolbarPanel.add(toolbarBuilder.buildBasicLUTToolBar());
        toolbarPanel.add(paintBar);
    }

    /**
     * Construct the upper intensity bound slider panel.
     *
     * @return  panel which allows adjustment of the upper intensity bound
     */
    private JPanel initUpperSlider() {
        upperDeltaSlider = new JSlider(JSlider.HORIZONTAL);
        upperDeltaSlider.setPaintTicks(true);
        upperDeltaSlider.setPaintLabels(true);
        upperDeltaSlider.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
        upperDeltaSlider.addKeyListener(this);

        updateUpperSliderValues();

        // start listening after we set up the labels
        upperDeltaSlider.addChangeListener(this);

        upperDeltaTF = new JTextField(4);
        upperDeltaTF.setText(String.valueOf((float) (upperDeltaSlider.getValue() / 100.0f)));
        upperDeltaTF.addKeyListener(this);

        JPanel deltaPanel = new JPanel(new GridBagLayout());
        deltaPanel.setBorder(buildTitledBorder("Upper region grow threshold"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        deltaPanel.add(upperDeltaSlider, gbc);

        gbc.gridwidth = 1;
        gbc.gridx = 3;
        deltaPanel.add(upperDeltaTF, gbc);

        return deltaPanel;
    }
}
