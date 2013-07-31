package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.Preferences.DefaultDisplay;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog creates 2 sliders which adjust the level and window of an image. The level is found at the x coordinate of the
 * mid point of the sloping transfer segment. The window is the x width of the sloping transfer segment. Note y
 * inversion in transfer segment because graphical origin is in upper left corner.
 * 
 * <p>
 * ________ / 255 ^ / | / | / | / <------- Transfer function | / L | / U | / T | / | / | / |______/ 0
 * |________________________________> | | | minImage | level | maxImage | | st win end win
 * </p>
 * 
 * <p>
 * Image intensity
 * </p>
 * 
 * @author senseneyj
 */
public class JDialogWinLevel extends JDialogBase implements ChangeListener, KeyListener, MouseListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3270517526341902110L;

    /** DOCUMENT ME! */
    public static final int IMAGE_A = 0;

    /** DOCUMENT ME! */
    public static final int IMAGE_B = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton closeButton, saveButton, loadButton;

    /**
     * Reference to the image data of the slice presently displayed. Needed to calculate the max/min of the slice used
     * to adjust the transfer function.
     */
    private final float[] dataSlice;

    /** Reference to the image that will be affected by the adjust of the window and level. */
    private final ModelImage image;

    /** Average of the min and max extents of the transfer window that describes the window size. */
    private float level;

    private float min, max;

    /** DOCUMENT ME! */
    public JSlider levelSlider, minSlider;

    /** Stores the maximum slider value. */
    private int levelSliderMax;

    /** Reference to the LUT used to display the image. */
    private final ModelLUT LUT;

    /** Image's maximum intensity. */
    private float maxImage;

    /** Image's minimum intensity. */
    private float minImage;

    /** DOCUMENT ME! */
    private final JPanel windowLevelPanel, minMaxPanel;

    /** The size of the window. */
    private float window;

    /** DOCUMENT ME! */
    public JSlider windowSlider, maxSlider;

    /** Stores the minimum slider value. */
    private int windowSliderMax;

    /** DOCUMENT ME! */

    /** textfield inputs for window and level * */
    private JTextField winValTextField, levelValTextField, minValTextField, maxValTextField;

    /** the maxes and mins for window and level * */
    private float winMaxFloat, winMinFloat, levelMaxFloat, levelMinFloat;

    /** Three arrays to save the coordinates of the LUT's transfer fucntion. z[] not used. */
    private final float[] x = new float[4];

    /** DOCUMENT ME! */
    private final float[] y = new float[4];

    /** DOCUMENT ME! */
    private final float[] z = new float[4];

    /** tabbed pane for w/L and min/max * */
    public JTabbedPane tabbedPane = new JTabbedPane();

    /** slope for mapping of min/max sliders * */
    public float minMaxSlope;

    /** b intercept for mapping of min/max sliders * */
    public float minMaxBInt;

    public boolean keyTyped = false;

	private int allowChangesWin;

	private int allowChangesMinMax;

	private JLabel sliderMinMax, sliderMinMin, sliderMaxMax, sliderMaxMin;
	
	private JLabel sliderWinMin, sliderWinMax, sliderLevMin, sliderLevMax;

	private boolean boundsChanged = false;

	private int levelSliderMin;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     * 
     * @param theParentFrame parent frame
     * @param image source image
     * @param LUT DOCUMENT ME!
     */
    public JDialogWinLevel(final Frame theParentFrame, final ModelImage image, final ModelLUT LUT) {
        this(theParentFrame, image, LUT, ((ViewJFrameImage) theParentFrame).getComponentImage().getActiveImageBuffer());
    }

    /**
     * Constructor.
     * 
     * @param theParentFrame parent frame
     * @param image source image
     * @param LUT DOCUMENT ME!
     */
    public JDialogWinLevel(final Frame theParentFrame, final ModelImage image, final ModelLUT LUT, float[] dataS) {
        super(theParentFrame, false);

        float min, max;
        int i;

        this.image = image;
        this.LUT = LUT;

        setTitle("Level and Window");
        setForeground(Color.black);
        try {
            setIconImage(MipavUtil.getIconImage("winlevel.gif"));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any runtime error on those systems
        }
        getContentPane().setLayout(new BorderLayout());
        calcMinMax();
        
        // TODO replace with shared method call to set up tf

        dataSlice = dataS;
        min = Float.MAX_VALUE;
        max = -Float.MAX_VALUE;

        for (i = 0; i < dataSlice.length; i++) {

            if (dataSlice[i] > max) {
                max = dataSlice[i];
            }

            if (dataSlice[i] < min) {
                min = dataSlice[i];
            }
        }
        
        //calcWinLevTransferFuntion(image, win, lev, x, y);

        // Set LUT min max values of the image slice !!
        x[0] = minImage;
        y[0] = 255;
        z[0] = 0;
        x[1] = min;
        y[1] = 255;
        z[1] = 0;
        x[2] = max;
        y[2] = 0;
        z[2] = 0;
        x[3] = maxImage;
        y[3] = 0;
        z[3] = 0;
        
        LUT.getTransferFunction().importArrays(x, y, 4);
        image.notifyImageDisplayListeners(LUT, false);

        final GridBagConstraints gbc = new GridBagConstraints();

        // build a monochrome window/level slider panel and populate it
        windowLevelPanel = buildWindowLevelPanel();
        minMaxPanel = buildMinMaxPanel();

        buildLevelSlider(windowLevelPanel, gbc);
        buildWindowSlider(windowLevelPanel, gbc);
        buildMinSlider(minMaxPanel, gbc);
        buildMaxSlider(minMaxPanel, gbc);
        tabbedPane.add("Level & Window", windowLevelPanel);
        tabbedPane.add("Min & Max", minMaxPanel);
        tabbedPane.addChangeListener(this);
        if (image.getHistogramFrame() != null) {
            updateHistoLUTFrame();
        }
        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        buildButtons(gbc);

        setResizable(true);
        setMinimumSize(new Dimension(250, 400));
        pack();
        locateDialog();

        setVisibleStandard(true);
        image.notifyImageDisplayListeners(LUT, false);

        if (image.getHistogramFrame() != null) {
            updateHistoLUTFrame();
        }

        System.gc();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
     */
    public void actionPerformed(final ActionEvent event) {
        final Object source = event.getSource();

        if (source == closeButton) {
            dispose();
        } else if (source == saveButton) {
            if (tabbedPane.getSelectedIndex() == 0) {
                Preferences.setProperty(Preferences.PREF_LEVEL, levelValTextField.getText());
                Preferences.setProperty(Preferences.PREF_WINDOW, winValTextField.getText());
                Preferences.setDefaultDisplay(DefaultDisplay.WindowLevel);
            } else if (tabbedPane.getSelectedIndex() == 1) {
                Preferences.setProperty(Preferences.PREF_MIN, minValTextField.getText());
                Preferences.setProperty(Preferences.PREF_MAX, maxValTextField.getText());
                Preferences.setDefaultDisplay(DefaultDisplay.MinMax);
            }
        } else if (source == loadButton) {
        	// TODO switch to use common function
            if (tabbedPane.getSelectedIndex() == 0) {
                final String lev = Preferences.getProperty(Preferences.PREF_LEVEL);
                final String win = Preferences.getProperty(Preferences.PREF_WINDOW);
                if (lev != null && win != null) {
                    final float num1 = validateCurrentNum(lev, levelMinFloat, levelMaxFloat);
                    final float num2 = validateCurrentNum(win, winMinFloat, winMaxFloat);
                    
                    if (num1 != -1 && num2 != -1) {
                    	final float val = ( (num1 - minImage) * levelSliderMax) / (maxImage - minImage);
                        final float val2 = (num2 * windowSliderMax) / (2 * (maxImage - minImage));
                        levelSlider.setValue((int) val);
                        windowSlider.setValue((int) val2);
                    } else {
//                    	if(num1 == -1){
//                    		MipavUtil.displayError("Level preference values are not valid with this dataset");
//                    	} else {
	                    	if (allowChangesWin == 1){
	                    		changeBounds(Float.parseFloat(lev), Float.parseFloat(win), true);
	                    	} else if (allowChangesWin == 0){
		                        String message = "The preference values you are attempting to load are outside your image bounds." +
		                        		"\nWould you like to change the bounds? \n\nWARNING: Selecting yes will permanently change your image";
		                        JCheckBox checkbox = new JCheckBox("Do not show this message again", false);
		                        Object[] content = {message, checkbox};
		                        int response = JOptionPane.showConfirmDialog(null, content, "", JOptionPane.YES_NO_OPTION,
		                                JOptionPane.WARNING_MESSAGE);
		                        
		                        if (response == JOptionPane.YES_OPTION) {
		                            if (checkbox.isSelected()) {
		                                allowChangesWin = 1;
		                            }
		                            changeBounds(Float.parseFloat(lev), Float.parseFloat(win), true);
		                        } else {
		                            if (checkbox.isSelected()) {
		                                allowChangesWin = 2;
		                            }
		                        } 
	                    	}
	                    }
//                    }
                } else {
                    MipavUtil.displayError("There are no window and level preference values saved");
                }
            } else if (tabbedPane.getSelectedIndex() == 1) {
                final String minString = Preferences.getProperty(Preferences.PREF_MIN);
                final String maxString = Preferences.getProperty(Preferences.PREF_MAX);
                if (minString != null && maxString != null) {
                    final boolean success = validateMinMaxNums(image, minString, maxString);

                    if (success == true) {
                        float val1 = Float.parseFloat(minString);
                        minValTextField.setText(Float.toString(val1));
                        val1 = (val1 - minMaxBInt) / minMaxSlope;
                        keyTyped = true;

                        minSlider.setValue((int) val1);

                        float val2 = Float.parseFloat(maxString);
                        maxValTextField.setText(Float.toString(val2));
                        val2 = (val2 - minMaxBInt) / minMaxSlope;
                        keyTyped = true;

                        maxSlider.setValue((int) val2);
                    } else {
                    	float val1 = Float.parseFloat(minString);
                    	float val2 = Float.parseFloat(maxString);
//                        MipavUtil.displayError("Min and max preference values are not valid with this dataset");
                    	if (allowChangesMinMax == 1) {
                    		changeBounds(val1, val2, false);
                    	} else if (allowChangesMinMax == 0){
	                    	String message = "The preference values you are attempting to load are outside your image bounds." +
	                        		"\nWould you like to change the bounds? \nWARNING: Selecting yes will permanently change your image";
	                        JCheckBox checkbox = new JCheckBox("Do not show this message again", false);
	                        Object[] content = {message, checkbox};
	                        int response = JOptionPane.showConfirmDialog(null, content, "", JOptionPane.YES_NO_OPTION,
	                                JOptionPane.WARNING_MESSAGE);
	                        
							if (response == JOptionPane.YES_OPTION) {
	                            if (checkbox.isSelected()) {
	                                allowChangesMinMax = 1;
	                            }
	                            changeBounds(val1, val2, false);
	                        } else {
	                            if (checkbox.isSelected()) {
	                                allowChangesMinMax = 2;
	                            }
	                        }
                    	}
                    }
                } else {
                    MipavUtil.displayError("There are no min and max preference values saved");
                }
            }
        } else {
            super.actionPerformed(event);
        }
    }

    // this code commented out. idea is to make the sliders assume a value
    // based on a limitd number of data points from the histogram...
    // ie., say the histogram has been changed. the histogram then tells the w/l
    // that it should update the sliders accordingly. The sliders only permit a
    // certain change in the histogram, so they need to make some assumptions to
    // get a "decent" value (even if they cannot represent fully the histgram
    // transfer function).
    // /** upafe
    // */
    // public void updateSliders() {
    //
    // // check old values...see if the corners x&y [1] is along the max/min
    // // and then adjust sliders to match....
    // // else, adjust the histgram to match the sliders... maybe later
    // // keep the current histopoints inside the window/level adjustment.
    // float X[], Y[], Z[];
    // LUT.getTransferFunction().exportArrays(X, Y, Z, 4);
    // levelSlider.setValue(_______________)
    // level = (levelSlider.getValue() * (maxImage - minImage)/levelSliderMax) + minImage;
    // window = (windowSlider.getValue() * 2 * (maxImage - minImage)/windowSliderMax);
    //
    // winValLabel.setText(Float.toString(Math.round(window)));
    // levelValLabel.setText(Float.toString(Math.round(level)));
    // }

    private void changeBounds(float left, float right, boolean levWin) {   
        boundsChanged = true;
    	
    	if (levWin) {
    		String levString = Float.toString(left);
    		String winString = Float.toString(right);

	        levelValTextField.setText(levString);
    		winValTextField.setText(winString);	  

    		
            float minTemp = (left - minMaxBInt) / minMaxSlope;
            
	        if (levelMinFloat > left) {
	        	sliderLevMin.setText(levString);
	        	sliderMinMin.setText(levString);
            	sliderMaxMin.setText(levString);
            	minSlider.setMinimum((int) minTemp);
            	maxSlider.setMinimum((int) minTemp);
            	image.setMin(left);
	        } else if(levelMaxFloat < right) {
	        	sliderLevMax.setText(levString);
	        	sliderMinMax.setText(levString);
            	sliderMaxMax.setText(levString);
            	maxSlider.setMaximum((int) minTemp);
            	minSlider.setMaximum((int) minTemp);
            	image.setMax(left);	
	        }
	        calcMinMaxSlope(image);
	        calcMinMax();
    		float levTemp = ( (left - minImage) * levelSliderMax) / (maxImage - minImage);
    		float winTemp = (right * windowSliderMax) / (2 * (maxImage - minImage));

    		if (levelMinFloat > left) {
    			levelSlider.setMinimum((int)levTemp);
    			levelMinFloat = left;
    		} else if (levelMaxFloat < left) {
    			levelSlider.setMaximum((int)levTemp);
            	levelMaxFloat = left;
    		}

	        if(winMaxFloat < right) {
	        	sliderWinMax.setText(winString);
	        	windowSlider.setMaximum((int) winTemp);
	        	winMaxFloat = right;
	        }
	        
	        levelSlider.setValue((int) levTemp);
	        windowSlider.setValue((int) winTemp);
    	} else {
    		String minString = Float.toString(left);
        	String maxString = Float.toString(right);
        	
            minValTextField.setText(minString);
            float minTemp = (left - minMaxBInt) / minMaxSlope;
            maxValTextField.setText(maxString);
            float maxTemp = (right - minMaxBInt) / minMaxSlope;
            
            boolean minChanged = false;
            boolean maxChanged = false;
            if (minSlider.getMinimum() > minTemp) {
            	sliderMinMin.setText(minString);
            	sliderMaxMin.setText(minString);
            	minSlider.setMinimum((int) minTemp);
            	maxSlider.setMinimum((int) minTemp);
            	image.setMin(left);
            	sliderLevMin.setText(minString);
            	minChanged = true;
            }
            minSlider.setValue((int) minTemp);
            
            if (maxSlider.getMaximum() < maxTemp) {
            	sliderMinMax.setText(maxString);
            	sliderMaxMax.setText(maxString);
            	maxSlider.setMaximum((int) maxTemp);
            	minSlider.setMaximum((int) maxTemp);
            	image.setMax(right);
            	sliderLevMax.setText(maxString);
            	maxChanged = true;
            }
            maxSlider.setValue((int) maxTemp);

            calcMinMax();
            
            float levMinTemp = ( (left - minImage) * levelSliderMax) / (maxImage - minImage);
        	float levMaxTemp = ( (right - minImage) * levelSliderMax) / (maxImage - minImage);

    		if (minChanged || maxChanged) {
    			if (minChanged){
    				levelSlider.setMinimum((int)levMinTemp);
    				levelMinFloat = left;
    			}
    			if (maxChanged){
    				levelMaxFloat = right;
    				levelSlider.setMaximum((int)levMaxTemp);
    			}
    		}
    		
    		float winMax = (windowSlider.getMaximum() * 2 * (maxImage - minImage) / windowSliderMax);
    		sliderWinMax.setText(Float.toString(winMax));
    	}
    }


	/**
     * Setting location of window-level adjustment panel based on the amount of space available near the image window.
     */
    public void locateDialog() {

        if ( (parentFrame.getLocation().x - getSize().width) > 0) {
            setLocation(parentFrame.getLocation().x - getSize().width, parentFrame.getLocation().y);
        } else {
            final int tmp = (parentFrame.getLocation().x + parentFrame.getSize().width);

            setLocation(tmp, parentFrame.getLocation().y + 30);
        }
    }

    /**
     * Overides the super.setVisible(boolean b) (which also locates a panel in the center of the screen), to use the
     * super.setVisibleStandard(boolean b) which merely displays the Swing object onscreen. (as if it
     * super.super.setVisible(boolean b) could be called) <i>when</i> the property in MipavProps "BoundWindowLevel" is
     * <code>false</code> <i>or</i> when there is no property. The window/level dialog is "free." When there is
     * "BoundWindowLevel" and when it is <code>true</code>, the window/level dialog will get relocated to next to the
     * image window and then redrawn.
     * 
     * @see gov.nih.mipav.view.JDialogBase#setVisible(boolean)
     * @see gov.nih.mipav.view.JDialogBase#setVisibleStandard(boolean)
     * @see JDialogWinLevel#locateDialog()
     */
    public void setVisible(final boolean vis) {
        final String bound = Preferences.getProperty("BoundWindowLevel");

        if (bound != null) { // default is to float the panel

            if (bound.equalsIgnoreCase("true")) {
                locateDialog();
            }
        }

        super.setVisibleStandard(vis);
    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param e event that triggered this function
     */
    public void stateChanged(final ChangeEvent e) {
        // System.out.println("dialog chang(ing/ed)");

        // check old values...see if the corners x&y [1] is along the max/min
        // and then adjust sliders to match....
        // else, adjust the histgram to match the sliders... maybe later
        // keep the current histopoints inside the window/level adjustment.

        final Object source = e.getSource();
        if (source == levelSlider || source == windowSlider) {
            calcMinMax();
            

                level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
                window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);


            if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
                winValTextField.setText(Float.toString(window));
                levelValTextField.setText(Float.toString(level));
            } else {
                winValTextField.setText(Float.toString(Math.round(window)));
                levelValTextField.setText(Float.toString(Math.round(level)));
            }

            calcWinLevTransferFunction(image, window, level, x, y);

            // update the transfer function so the on-screen image
            // (modelImage/viewJFrameImage) updates for the user
            LUT.getTransferFunction().importArrays(x, y, 4);
            image.notifyImageDisplayListeners(LUT, false);

            // if ((levelSlider.getValueIsAdjusting()) || (windowSlider.getValueIsAdjusting())) {
            // return;
            // }

            // if the slider is finally done, update the transfer function
            // in the histogram.
            if (image.getHistogramFrame() != null) {
                updateHistoLUTFrame();
            }
        } else if (source == minSlider || source == maxSlider) {
        	// TODO use shared method
        	
            min = minSlider.getValue();
            max = maxSlider.getValue();

            min = (minMaxSlope * min) + minMaxBInt;
            max = (minMaxSlope * max) + minMaxBInt;

            if (keyTyped) {
                min = Float.parseFloat(minValTextField.getText());
                max = Float.parseFloat(maxValTextField.getText());
                keyTyped = false;
            } else {
                if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
                    minValTextField.setText(Float.toString(min));
                    maxValTextField.setText(Float.toString(max));
                } else {
                    min = Math.round(min);
                    max = Math.round(max);
                    minValTextField.setText(Float.toString(min));
                    maxValTextField.setText(Float.toString(max));
                }
            }

            y[1] = 255.0f;
            x[1] = min;

            y[2] = 0;
            x[2] = max;

            LUT.getTransferFunction().importArrays(x, y, 4);
            image.notifyImageDisplayListeners(LUT, false);
            if (image.getHistogramFrame() != null) {
                updateHistoLUTFrame();
            }

        } else if (source == tabbedPane) {
            if (tabbedPane.getSelectedIndex() == 0) {
                calcMinMax();
                
                // TODO use shared method

	                level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
	                window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);

                if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
                    winValTextField.setText(Float.toString(window));
                    levelValTextField.setText(Float.toString(level));
                } else {
                    winValTextField.setText(Float.toString(Math.round(window)));
                    levelValTextField.setText(Float.toString(Math.round(level)));
                }
                calcWinLevTransferFunction(image, window, level, x, y);

                // update the transfer function so the on-screen image
                // (modelImage/viewJFrameImage) updates for the user
                LUT.getTransferFunction().importArrays(x, y, 4);
                image.notifyImageDisplayListeners(LUT, false);

                // if ((levelSlider.getValueIsAdjusting()) || (windowSlider.getValueIsAdjusting())) {
                // return;
                // }

                // if the slider is finally done, update the transfer function
                // in the histogram.
                if (image.getHistogramFrame() != null) {
                    updateHistoLUTFrame();
                }
            } else if (tabbedPane.getSelectedIndex() == 1) {
            	// TODO use shared method
            	
                /*
                 * min = minSlider.getValue(); max = maxSlider.getValue();
                 * 
                 * min = (minMaxSlope * min) + minMaxBInt; max = (minMaxSlope * max) + minMaxBInt;
                 * 
                 * if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {
                 * minValTextField.setText(Float.toString(min)); maxValTextField.setText(Float.toString(max)); }else {
                 * min = Math.round(min); max = Math.round(max); minValTextField.setText(Float.toString(min));
                 * maxValTextField.setText(Float.toString(max)); }
                 */

                min = Float.parseFloat(minValTextField.getText());
                max = Float.parseFloat(maxValTextField.getText());

                y[1] = 255.0f;
                x[1] = min;

                y[2] = 0;
                x[2] = max;

                LUT.getTransferFunction().importArrays(x, y, 4);
                image.notifyImageDisplayListeners(LUT, false);
                if (image.getHistogramFrame() != null) {
                    updateHistoLUTFrame();
                }
            }
        }
    }

    /**
     * key typed
     */
    public void keyTyped(final KeyEvent event) {
        final Object source = event.getSource();

        if (event.getKeyChar() == KeyEvent.VK_ENTER) {
            if (source == levelValTextField) {
                final String numString = levelValTextField.getText();
                final float num = validateCurrentNum(numString, levelMinFloat, levelMaxFloat);
                if (num != -1) {
                    final float val = ( (num - minImage) * levelSliderMax) / (maxImage - minImage);
                    levelSlider.setValue((int) val);
                } else {
                    level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
                    levelValTextField.setText(Float.toString(Math.round(level)));
                }
            } else if (source == winValTextField) {
                final String numString = winValTextField.getText();
                final float num = validateCurrentNum(numString, winMinFloat, winMaxFloat);
                if (num != -1) {
                    final float val = (num * windowSliderMax) / (2 * (maxImage - minImage));
                    windowSlider.setValue((int) val);
                } else {
                    window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);
                    winValTextField.setText(Float.toString(Math.round(window)));
                }
            } else if (source == minValTextField) {
                final String numStringMin = minValTextField.getText();
                final String numStringMax = maxValTextField.getText();
                final boolean success = validateMinMaxNums(image, numStringMin, numStringMax);
                if (success == true) {
                    float val = Float.parseFloat(numStringMin);
                    val = (val - minMaxBInt) / minMaxSlope;
                    keyTyped = true;
                    minSlider.setValue((int) val);
                } else {
                    min = minSlider.getValue();

                    min = (minMaxSlope * min) + minMaxBInt;

                    if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        minValTextField.setText(Float.toString(min));
                    } else {
                        min = Math.round(min);
                        minValTextField.setText(Float.toString(min));
                    }
                }

            } else if (source == maxValTextField) {
                final String numStringMin = minValTextField.getText();
                final String numStringMax = maxValTextField.getText();
                final boolean success = validateMinMaxNums(image, numStringMin, numStringMax);
                if (success == true) {
                    float val = Float.parseFloat(numStringMax);
                    val = (val - minMaxBInt) / minMaxSlope;
                    keyTyped = true;
                    maxSlider.setValue((int) val);
                } else {

                    max = maxSlider.getValue();

                    max = (minMaxSlope * max) + minMaxBInt;

                    if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {

                        maxValTextField.setText(Float.toString(max));
                    } else {

                        max = Math.round(max);

                        maxValTextField.setText(Float.toString(max));
                    }
                }

            }
        }

    }

    /**
     * Update the window, level sliders from CTPreset dialog.
     * 
     * @param min min value
     * @param max max value
     */
    public void updateSliders(final int min, final int max) {
        int windowValue, levelValue;
        float winVal, levelVal;

        windowValue = (max - min);
        levelValue = (max + min) / 2;

        winVal = windowValue * windowSliderMax / (2 * (maxImage - minImage));
        levelVal = (levelValue - minImage) * levelSliderMax / (maxImage - minImage);

        levelSlider.setValue((int) levelVal);
        windowSlider.setValue((int) winVal);
        level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
        window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);

        winValTextField.setText(Float.toString(Math.round(window)));
        levelValTextField.setText(Float.toString(Math.round(level)));

    }

    /**
     * Builds the close button.
     * 
     * @param gbc DOCUMENT ME!
     */
    private void buildButtons(final GridBagConstraints gbc) {

        final JPanel buttonPanel = new JPanel();

        saveButton = new JButton("Save");
        saveButton.addActionListener(this);
        // closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        // closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        saveButton.setFont(serif12B);
        saveButton.setSize(MipavUtil.defaultButtonSize);
        saveButton.setToolTipText("Save values to Preferences file");
        buttonPanel.add(saveButton);

        loadButton = new JButton("Load");
        loadButton.addActionListener(this);
        // closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        // closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        loadButton.setFont(serif12B);
        loadButton.setSize(MipavUtil.defaultButtonSize);
        loadButton.setToolTipText("Load values from Preferences file");
        buttonPanel.add(loadButton);

        closeButton = new JButton("Close");
        closeButton.addActionListener(this);
        // closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        // closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        closeButton.setFont(serif12B);
        closeButton.setSize(MipavUtil.defaultButtonSize);
        buttonPanel.add(closeButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    /**
     * Builds the level slider and places it in the slider panel.
     * 
     * @param spanel DOCUMENT ME!
     * @param gbc DOCUMENT ME!
     */
    private void buildLevelSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        sliderLevMax = new JLabel(Float.toString(maxImage));
        levelMaxFloat = maxImage;
        sliderLevMax.setForeground(Color.black);
        sliderLevMax.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);

        final JLabel levelLabel = new JLabel("Level");
        spanel.add(levelLabel, gbc);

        gbc.gridy = 1;

        spanel.add(sliderLevMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        level = (x[1] + x[2]) / 2.0f;
        levelSlider = new JSlider(0, levelSliderMax,
                (int) ( (level - minImage) * levelSliderMax / (maxImage - minImage)));
        
//        if (boundsChanged){
//        	levelSlider = new JSlider((int)levelMinFloat, (int)levelMaxFloat, (int)level);
//        } else {
//        	level = (x[1] + x[2]) / 2.0f;
//            levelSlider = new JSlider(0, levelSliderMax,
//                    (int) ( (level - minImage) * levelSliderMax / (maxImage - minImage)));
//            
//        }

        // set slider attributes
        levelSlider.setFont(serif12);
        levelSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            levelSlider.setMajorTickSpacing((int) (levelSliderMax * 0.25f));
        } else {
            levelSlider.setMajorTickSpacing((int) (levelSliderMax * 0.25f));
        }

        levelSlider.setPaintTicks(true);
        levelSlider.addChangeListener(this);
        levelSlider.setVisible(true);
        levelSlider.setOrientation(SwingConstants.VERTICAL);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(levelSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderLevMin = new JLabel(Float.toString(minImage));
        levelMinFloat = minImage;
        sliderLevMin.setForeground(Color.black);
        sliderLevMin.setFont(serif12);
        spanel.add(sliderLevMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // levelValLabel = new JLabel(Float.toString(level));
        // levelValLabel.setForeground(Color.black);
        // levelValLabel.setFont(serif12B);
        levelValTextField = new JTextField(6);
        levelValTextField.setText(Float.toString(level));
        levelValTextField.addKeyListener(this);
        levelValTextField.addFocusListener(this);
        spanel.add(levelValTextField, gbc);
    }

    /**
     * Builds the min slider and places it in the slider panel.
     * 
     * @param spanel DOCUMENT ME!
     * @param gbc DOCUMENT ME!
     */
    private void buildMinSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        sliderMinMax = new JLabel(Float.toString(maxImage));
        // levelMaxFloat = maxImage;
        sliderMinMax.setForeground(Color.black);
        sliderMinMax.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);

        final JLabel minLabel = new JLabel("Min");
        spanel.add(minLabel, gbc);

        gbc.gridy = 1;
        spanel.add(sliderMinMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        min = .25f * (maxImage - minImage); //TODO: Change this to a useful value based on image statistics
        minSlider = new JSlider(0, 11999, 3000);

        // set slider attributes
        minSlider.setFont(serif12);
        minSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            minSlider.setMajorTickSpacing(3000);
        } else {
            minSlider.setMajorTickSpacing(3000);
        }

        minSlider.setPaintTicks(true);
        minSlider.addChangeListener(this);
        minSlider.setVisible(true);
        minSlider.setOrientation(SwingConstants.VERTICAL);
        minSlider.addMouseListener(this);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(minSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderMinMin = new JLabel(Float.toString(minImage));
        // levelMinFloat = minImage;
        sliderMinMin.setForeground(Color.black);
        sliderMinMin.setFont(serif12);
        spanel.add(sliderMinMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // levelValLabel = new JLabel(Float.toString(level));
        // levelValLabel.setForeground(Color.black);
        // levelValLabel.setFont(serif12B);
        minValTextField = new JTextField(6);
        minValTextField.setText(Float.toString(min));
        minValTextField.addKeyListener(this);
        minValTextField.addFocusListener(this);
        spanel.add(minValTextField, gbc);

        minMaxBInt = minImage;
        minMaxSlope = calcMinMaxSlope(image);

    }

    private void buildMaxSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        sliderMaxMax = new JLabel(Float.toString(maxImage));
        // levelMaxFloat = maxImage;
        sliderMaxMax.setForeground(Color.black);
        sliderMaxMax.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);

        final JLabel maxLabel = new JLabel("Max");
        spanel.add(maxLabel, gbc);

        gbc.gridy = 1;

        spanel.add(sliderMaxMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        max = .75f * (maxImage - minImage); //TODO: Change this to a useful value based on image statistics
        maxSlider = new JSlider(0, 11999, 9000);

        // set slider attributes
        maxSlider.setFont(serif12);
        maxSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            maxSlider.setMajorTickSpacing(3000);
        } else {
            maxSlider.setMajorTickSpacing(3000);
        }

        maxSlider.setPaintTicks(true);
        maxSlider.addChangeListener(this);
        maxSlider.setVisible(true);
        maxSlider.setOrientation(SwingConstants.VERTICAL);
        maxSlider.addMouseListener(this);

        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(maxSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderMaxMin = new JLabel(Float.toString(minImage));
        // levelMinFloat = minImage;
        sliderMaxMin.setForeground(Color.black);
        sliderMaxMin.setFont(serif12);
        spanel.add(sliderMaxMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // levelValLabel = new JLabel(Float.toString(level));
        // levelValLabel.setForeground(Color.black);
        // levelValLabel.setFont(serif12B);
        maxValTextField = new JTextField(6);
        maxValTextField.setText(Float.toString(max));
        maxValTextField.addKeyListener(this);
        maxValTextField.addFocusListener(this);
        spanel.add(maxValTextField, gbc);
    }

    /**
     * Builds the slider Panel.
     * 
     * @param borderTitle the title of the border.
     * 
     * @return DOCUMENT ME!
     */
    private JPanel buildWindowLevelPanel() {
        final JPanel spanel = new JPanel();

        // getContentPane().add(spanel, BorderLayout.CENTER);
        spanel.setLayout(new GridBagLayout());

        spanel.setForeground(Color.black);
        // spanel.setBorder(buildTitledBorder(borderTitle));

        return spanel;
    }

    private JPanel buildMinMaxPanel() {
        final JPanel spanel = new JPanel();

        // getContentPane().add(spanel, BorderLayout.CENTER);
        // spanel.setVisible(false);
        spanel.setLayout(new GridBagLayout());

        spanel.setForeground(Color.black);
        // spanel.setBorder(buildTitledBorder(borderTitle));

        return spanel;
    }

    /**
     * Builds the window slider and places it in the slider panel.
     * 
     * @param spanel DOCUMENT ME!
     * @param gbc DOCUMENT ME!
     */
    private void buildWindowSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderWinMax = new JLabel(Float.toString(2.0f * (maxImage - minImage)));
        winMaxFloat = 2.0f * (maxImage - minImage);
        sliderWinMax.setForeground(Color.black);
        sliderWinMax.setFont(serif12);

        final JLabel windowLabel = new JLabel("Window");
        spanel.add(windowLabel, gbc);

        gbc.gridy = 1;

        spanel.add(sliderWinMax, gbc);

        // current setting of the slider
        window = x[2] - x[1]; // the width of the window x[2] (max) - x[1] (min)
        windowSlider = new JSlider(0, windowSliderMax,
                (int) (window * windowSliderMax / (2.0f * (maxImage - minImage))));

        // set slider attributes
        windowSlider.setFont(serif12);
        windowSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            windowSlider.setMajorTickSpacing((int) (windowSliderMax * 0.25f));
        } else {
            windowSlider.setMajorTickSpacing((int) (windowSliderMax * 0.25f));
        }

        windowSlider.setPaintTicks(true);
        windowSlider.addChangeListener(this);
        windowSlider.setVisible(true);
        windowSlider.setOrientation(SwingConstants.VERTICAL);

        // slider placement
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(windowSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderWinMin = new JLabel("0.0");
        winMinFloat = 0.0f;
        sliderWinMin.setForeground(Color.black);
        sliderWinMin.setFont(serif12);
        spanel.add(sliderWinMin, gbc);

        // current value of window
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // winValLabel = new JLabel(Float.toString(window));
        // winValLabel.setForeground(Color.black);
        // winValLabel.setFont(serif12B);
        winValTextField = new JTextField(6);
        winValTextField.setText(Float.toString(window));
        winValTextField.addKeyListener(this);
        winValTextField.addFocusListener(this);
        spanel.add(winValTextField, gbc);
    }

    /**
     * validate current number
     * 
     * @param numString
     * @param min
     * @param max
     * @return
     */
    public static final float validateCurrentNum(final String numString, final float min, final float max) {
        float num;

        try {
            num = Float.parseFloat(numString);
        } catch (final NumberFormatException e) {
            return -1;
        }
        if (num >= min && num <= max) {
            return num;
        } else {
            return -1;
        }
    }

    public static final boolean validateMinMaxNums(final ModelImage img, final String minString, final String maxString) {
        float numMin;
        float numMax;
        
        float[] bounds = calcMinMax(img);

        try {
            numMin = Float.parseFloat(minString);
            numMax = Float.parseFloat(maxString);
        } catch (final NumberFormatException e) {
            return false;
        }

        if (numMin < bounds[0] || numMin > bounds[1] || numMax < bounds[0] || numMax > bounds[1] || numMin > numMax) {
            return false;
        } else {
            return true;
        }

    }

    /**
     * Displays histoLUT frame for a gray scale image.
     */
    private void updateHistoLUTFrame() {
        image.notifyImageDisplayListeners(LUT, false);
        image.getHistogramFrame().redrawFrames();
    }

    public void keyPressed(final KeyEvent arg0) {}

    public void keyReleased(final KeyEvent arg0) {}

    public void mouseClicked(final MouseEvent e) {}

    public void mouseEntered(final MouseEvent e) {}

    public void mouseExited(final MouseEvent e) {}

    public void mousePressed(final MouseEvent e) {}

    public void mouseReleased(final MouseEvent e) {
        final Object source = e.getSource();

        if (source == maxSlider) {
            if (maxSlider.getValue() <= minSlider.getValue()) {
                maxSlider.setValue(minSlider.getValue() + 1);
            }
        } else if (source == minSlider) {
            if (minSlider.getValue() >= maxSlider.getValue()) {
                minSlider.setValue(maxSlider.getValue() - 1);
            }
        }

    }

    public void notifyImageDisplayListeners() {
        image.notifyImageDisplayListeners(LUT, false);
    }
    
    /**
     * Calculate the x and y components of the transfer function, given the active image and the desired window and level.
     * @param img The active image.
     * @param win The desired window.
     * @param lev The desired level.
     * @param tfx The array in which the x components of the transfer function will be placed.  Should already be allocated and of size 4.
     * @param tfy The array in which the y components of the transfer function will be placed.  Should already be allocated and of size 4.
     */
    public static final void calcWinLevTransferFunction(ModelImage img, float win, float lev, float[] tfx, float tfy []) {
    	if (tfx == null || tfx.length != 4) {
    		throw new IllegalArgumentException("Transfer function x component not set up correctly.");
    	}
    	if (tfy == null || tfy.length != 4) {
    		throw new IllegalArgumentException("Transfer function y component not set up correctly.");
    	}
    	
    	float[] bounds = calcMinMax(img);
    	
    	// first point always at lower left
        tfx[0] = bounds[0];
        tfy[0] = 255;
        
        if (win == 0) {
    		win = 1;
    	}
    	
    	tfx[2] = lev + (win / 2);

        if (tfx[2] > bounds[1]) {
            tfy[2] = 255.0f * (tfx[2] - bounds[1]) / win;
            tfx[2] = bounds[1];
        } else {
            tfy[2] = 0.0f;
        }

        tfx[1] = lev - (win / 2);

        if (tfx[1] < bounds[0]) {
            tfy[1] = 255.0f - (255.0f * (bounds[0] - tfx[1]) / win);
            tfx[1] = bounds[0];
        } else {
            tfy[1] = 255.0f;
        }
        
        // last point always at upper right of histogram
        tfx[3] = bounds[1];
        tfy[3] = 0;
    }
    
    /**
     * Calculate the image range bounds for transfer function determination.
     * @param img The active image.
     * @return An array containing the minimum value of the range in the 0th index and the maximum in the 1st index.
     */
    public static final float[] calcMinMax(ModelImage img) {
    	float[] bounds = new float[2];
    	
    	if (img.getType() == ModelStorageBase.UBYTE) {
    		bounds[0] = 0;
    		bounds[1] = 255;
        } else if (img.getType() == ModelStorageBase.BYTE) {
        	bounds[0] = -128;
        	bounds[1] = 127;
        } else {
        	bounds[0] = (float) img.getMin();
        	bounds[1] = (float) img.getMax();
        }
    	
    	return bounds;
    }
    
    private static final int calcLevelSliderMax(ModelImage img) {
    	if (img.getType() == ModelStorageBase.UBYTE) {
            return 255;
        } else if (img.getType() == ModelStorageBase.BYTE) {
            return 255;
        } else {
            return 1999;
        }
    }
    
    private static final int calcWindowSliderMax(ModelImage img) {
    	if (img.getType() == ModelStorageBase.UBYTE) {
            return 511;
        } else if (img.getType() == ModelStorageBase.BYTE) {
            return 511;
        } else {
            return 3999;
        }
    }
    
    private static final float calcMinMaxSlope(ModelImage img) {
    	float[] bounds = calcMinMax(img);
    	return (bounds[1] - bounds[0]) / 11999;
    }
    
    /**
     * Calculate the maximum and minimum valuse to setup the window and level sliders.
     */
    private void calcMinMax() {
    	float[] bounds = calcMinMax(image);
    	minImage = bounds[0];
    	maxImage = bounds[1];
    	levelSliderMax = calcLevelSliderMax(image);
    	windowSliderMax = calcWindowSliderMax(image);
    }
} // end class JDialogWinLevel
