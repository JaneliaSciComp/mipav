package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog creates 2 sliders which adjust the level and window of an image. The level is found at the x coordinate of the
 * mid point of the sloping transfer segment. The window is the x width of the sloping transfer segment. Note y
 * inversion in transfer segment because graphical origin is in upper left corner.
 *
 * <p>________ / 255 ^ / | / | / | / <------- Transfer function | / L | / U | / T | / | / | / |______/ 0
 * |________________________________> | | | minImage | level | maxImage | | st win end win</p>
 *
 * <p>Image intensity</p>
 */
public class JDialogWinLevel extends JDialogBase implements ChangeListener, KeyListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3270517526341902110L;

    /** DOCUMENT ME! */
    public static final int IMAGE_A = 0;

    /** DOCUMENT ME! */
    public static final int IMAGE_B = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton closeButton, saveButton, loadButton;

    /**
     * Reference to the image data of the slice presently displayed. Needed to calculate the max/min of the slice used
     * to adjust the transfer function.
     */
    private float[] dataSlice;

    /** Reference to the image that will be affected by the adjust of the window and level. */
    private ModelImage image;

    /** Average of the min and max extents of the transfer window that desribes the window size. */
    private float level;
    
    private float min, max;

    /** DOCUMENT ME! */
    private JLabel levelLabel;

    /** DOCUMENT ME! */
    public JSlider levelSlider, minSlider;

    /** Stores the maximum slider value. */
    private int levelSliderMax;

    /** Reference to the LUT used to display the image. */
    private ModelLUT LUT;

    /** Image's maximum intensity. */
    private float maxImage;

    /** Image's minimum intensity. */
    private float minImage;

    /** DOCUMENT ME! */
    private JPanel windowLevelPanel, minMaxPanel;

    /** The size of the window. */
    private float window;

    /** DOCUMENT ME! */
    private JLabel windowLabel;

    /** DOCUMENT ME! */
    public JSlider windowSlider, maxSlider;

    /** Stores the minimum slider value. */
    private int windowSliderMax;

    /** DOCUMENT ME! */

    /** textfield inputs for window and level **/
    private JTextField winValTextField,levelValTextField, minValTextField, maxValTextField;
    
    /** the maxes and mins for window and level **/
    private float winMaxFloat, winMinFloat, levelMaxFloat, levelMinFloat;

    /** Three arrays to save the coordinates of the LUT's transfer fucntion. z[] not used. */
    private float[] x = new float[4];

    /** DOCUMENT ME! */
    private float[] y = new float[4];

    /** DOCUMENT ME! */
    private float[] z = new float[4];
    
    /** tabbed pane for w/L and min/max **/
    public JTabbedPane tabbedPane = new JTabbedPane();

    /** slope for mapping of min/max sliders **/
    public float minMaxSlope;
    
    /** b interecept for mapping of min/max sliders **/
    public float minMaxBInt;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  image           source image
     * @param  LUT             DOCUMENT ME!
     */
    public JDialogWinLevel(Frame theParentFrame, ModelImage image, ModelLUT LUT) {
        super(theParentFrame, false);

        float min, max;
        int i;

        this.image = image;
        this.LUT = LUT;

        setTitle("Level and Window");
        setForeground(Color.black);
        try{
        	setIconImage(MipavUtil.getIconImage("winlevel.gif"));
        }catch(Exception e) {
        	
        }
        getContentPane().setLayout(new BorderLayout());
        calcMinMax();

        dataSlice = ((ViewJFrameImage) parentFrame).getComponentImage().getActiveImageBuffer();
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
        
        GridBagConstraints gbc = new GridBagConstraints();

        // build a monochrome window/level slider panel and populate it
        windowLevelPanel = buildWindowLevelPanel();
        minMaxPanel = buildMinMaxPanel();
        
        buildLevelSlider(windowLevelPanel, gbc);
        buildWindowSlider(windowLevelPanel, gbc);
        buildMinSlider(minMaxPanel,gbc);
        buildMaxSlider(minMaxPanel,gbc);
        tabbedPane.add("Level & Window", windowLevelPanel);
        tabbedPane.add("Min & Max", minMaxPanel);
        tabbedPane.addChangeListener(this);
        if (image.getHistoLUTFrame() != null) {
            updateHistoLUTFrame();
        }
        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        buildButtons(gbc);

        setResizable(true);
        setMinimumSize(new Dimension(250,400));
        pack();
        locateDialog();

        setVisibleStandard(true);
        image.notifyImageDisplayListeners(LUT, false);

        if (image.getHistoLUTFrame() != null) {
            updateHistoLUTFrame();
        }

        System.gc();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == closeButton) {
            dispose();
        }else if(source == saveButton) {
        	if(tabbedPane.getSelectedIndex() == 0) {
        		Preferences.setProperty(Preferences.PREF_LEVEL, levelValTextField.getText());
        		Preferences.setProperty(Preferences.PREF_WINDOW, winValTextField.getText());
        	}else if(tabbedPane.getSelectedIndex() == 1) {
        		Preferences.setProperty(Preferences.PREF_MIN, minValTextField.getText());
        		Preferences.setProperty(Preferences.PREF_MAX, maxValTextField.getText());
        	}
        }else if(source == loadButton) {
        	if(tabbedPane.getSelectedIndex() == 0) {
        		String lev = Preferences.getProperty(Preferences.PREF_LEVEL);
        		String win = Preferences.getProperty(Preferences.PREF_WINDOW);
        		if(lev != null && win != null) {
        			float num1 = validateCurrentNum(lev, levelMinFloat, levelMaxFloat);
        			float num2 = validateCurrentNum(win, winMinFloat, winMaxFloat);
        			
        			if(num1 != -1 && num2 != -1) {
    					float val = ((num1 - minImage) * levelSliderMax) / (maxImage - minImage);
    					levelSlider.setValue((int)val);
    					float val2 = (num2 * windowSliderMax)/(2 * (maxImage - minImage));
    					windowSlider.setValue((int)val2);
    				}else {
    					MipavUtil.displayError("Window and level preference values are not valid with this dataset");
    				}
        		}else {
        			MipavUtil.displayError("There are no window and level preference values saved");
        		}
        	}else if(tabbedPane.getSelectedIndex() == 1) {
        		String minString = Preferences.getProperty(Preferences.PREF_MIN);
        		String maxString = Preferences.getProperty(Preferences.PREF_MAX);
        		if(minString != null && maxString != null) {
        			boolean success = validateMinMaxNums(minString, maxString);
        			
    				if(success == true) {
    					float val1 = Float.parseFloat(minString);
    					val1 = (val1 - minMaxBInt)/minMaxSlope;
    					minSlider.setValue((int)val1);
    					float val2 = Float.parseFloat(maxString);
    					val2 = (val2 - minMaxBInt)/minMaxSlope;
    					maxSlider.setValue((int)val2);
    				}else {
    					MipavUtil.displayError("Min and max preference values are not valid with this dataset");
    				}
        		}else {
        			MipavUtil.displayError("There are no min and max preference values saved");
        		}
        	}
        }

    }

    // this code commented out.  idea is to make the sliders assume a value
    // based on a limitd number of data points from the histogram...
    // ie., say the histogram has been changed.  the histogram then tells the w/l
    // that it should update the sliders accordingly.  The sliders only permit a
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
    // float   X[], Y[], Z[];
    // LUT.getTransferFunction().exportArrays(X, Y, Z, 4);
    // levelSlider.setValue(_______________)
    // level  = (levelSlider.getValue()      * (maxImage - minImage)/levelSliderMax) + minImage;
    // window = (windowSlider.getValue() * 2 * (maxImage - minImage)/windowSliderMax);
    //
    // winValLabel.setText(Float.toString(Math.round(window)));
    // levelValLabel.setText(Float.toString(Math.round(level)));
    // }


    /**
     * Setting location of window-level adjustment panel based on the amount of space available near the image window.
     */
    public void locateDialog() {

        if ((parentFrame.getLocation().x - getSize().width) > 0) {
            setLocation(parentFrame.getLocation().x - getSize().width, parentFrame.getLocation().y);
        } else {
            int tmp = (parentFrame.getLocation().x + parentFrame.getSize().width);

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
     * @see  gov.nih.mipav.view.JDialogBase#setVisible(boolean)
     * @see  gov.nih.mipav.view.JDialogBase#setVisibleStandard(boolean)
     * @see  JDialogWinLevel#locateDialog()
     */
    public void setVisible(boolean vis) {
        String bound = Preferences.getProperty("BoundWindowLevel");

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
     * @param  e  event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        // System.out.println("dialog chang(ing/ed)");

        // check old values...see if the corners x&y [1] is along the max/min
        // and then adjust sliders to match....
        // else, adjust the histgram to match the sliders... maybe later
        // keep the current histopoints inside the window/level adjustment.
    	
    	Object source = e.getSource();
    	if(source == levelSlider || source == windowSlider) {
	        calcMinMax();
	        
	        level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
	        window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);
	
	        if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {
	        	winValTextField.setText(Float.toString(window));
		        levelValTextField.setText(Float.toString(level));
    		}else {
    			winValTextField.setText(Float.toString(Math.round(window)));
    	        levelValTextField.setText(Float.toString(Math.round(level)));
    		}
	        
	
	        if (window == 0) {
	            window = 1;
	        }
	
	        x[2] = level + (window / 2);
	
	        if (x[2] > maxImage) {
	            y[2] = 255.0f * (x[2] - maxImage) / window;
	            x[2] = maxImage;
	        } else {
	            y[2] = 0.0f;
	        }
	
	        x[1] = level - (window / 2);
	
	        if (x[1] < minImage) {
	            y[1] = 255.0f - (255.0f * (minImage - x[1]) / window);
	            x[1] = minImage;
	        } else {
	            y[1] = 255.0f;
	        }
	
	        // update the transfer function so the on-screen image
	        // (modelImage/viewJFrameImage) updates for the user
	        LUT.getTransferFunction().importArrays(x, y, 4);
	        image.notifyImageDisplayListeners(LUT, false);
	
	        // if ((levelSlider.getValueIsAdjusting()) || (windowSlider.getValueIsAdjusting())) {
	        // return;
	        // }
	
	        // if the slider is finally done, update the transfer function
	        // in the histogram.
	        if (image.getHistoLUTFrame() != null) {
	            updateHistoLUTFrame();
	        }
    	}else if(source == minSlider || source == maxSlider) {
    		min = minSlider.getValue();
    		max = maxSlider.getValue();
    		
    		min = (minMaxSlope * min) + minMaxBInt;
    		max = (minMaxSlope * max) + minMaxBInt;
    		
    		if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {
    			minValTextField.setText(Float.toString(min));
    			maxValTextField.setText(Float.toString(max));
    		}else {
    			min = Math.round(min);
    			max = Math.round(max);
    			minValTextField.setText(Float.toString(min));
    			maxValTextField.setText(Float.toString(max));
    		}
	        
	        y[1] = 255.0f;
            x[1] = min;
            
	        y[2] = 0;
            x[2] = max;
            
            LUT.getTransferFunction().importArrays(x, y, 4);
	        image.notifyImageDisplayListeners(LUT, false);
	        if (image.getHistoLUTFrame() != null) {
	            updateHistoLUTFrame();
	        }
            
    	} else if(source == tabbedPane) {
    		if(tabbedPane.getSelectedIndex() == 0) {
    			calcMinMax();
    	        level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
    	        window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);
    	
    	        if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {
    	        	winValTextField.setText(Float.toString(window));
    		        levelValTextField.setText(Float.toString(level));
        		}else {
        			winValTextField.setText(Float.toString(Math.round(window)));
        	        levelValTextField.setText(Float.toString(Math.round(level)));
        		}
    	        

    	
    	        if (window == 0) {
    	            window = 1;
    	        }
    	
    	        x[2] = level + (window / 2);
    	
    	        if (x[2] > maxImage) {
    	            y[2] = 255.0f * (x[2] - maxImage) / window;
    	            x[2] = maxImage;
    	        } else {
    	            y[2] = 0.0f;
    	        }
    	
    	        x[1] = level - (window / 2);
    	
    	        if (x[1] < minImage) {
    	            y[1] = 255.0f - (255.0f * (minImage - x[1]) / window);
    	            x[1] = minImage;
    	        } else {
    	            y[1] = 255.0f;
    	        }
    	
    	        // update the transfer function so the on-screen image
    	        // (modelImage/viewJFrameImage) updates for the user
    	        LUT.getTransferFunction().importArrays(x, y, 4);
    	        image.notifyImageDisplayListeners(LUT, false);
    	
    	        // if ((levelSlider.getValueIsAdjusting()) || (windowSlider.getValueIsAdjusting())) {
    	        // return;
    	        // }
    	
    	        // if the slider is finally done, update the transfer function
    	        // in the histogram.
    	        if (image.getHistoLUTFrame() != null) {
    	            updateHistoLUTFrame();
    	        }
    		}else if(tabbedPane.getSelectedIndex() == 1) {
    			min = minSlider.getValue();
        		max = maxSlider.getValue();
        		
    			min = (minMaxSlope * min) + minMaxBInt;
        		max = (minMaxSlope * max) + minMaxBInt;
        		
        		if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {
        			minValTextField.setText(Float.toString(min));
        			maxValTextField.setText(Float.toString(max));
        		}else {
        			min = Math.round(min);
        			max = Math.round(max);
        			minValTextField.setText(Float.toString(min));
        			maxValTextField.setText(Float.toString(max));
        		}

    	        y[1] = 255.0f;
                x[1] = min;
                
    	        y[2] = 0;
                x[2] = max;
                
                LUT.getTransferFunction().importArrays(x, y, 4);
    	        image.notifyImageDisplayListeners(LUT, false);
    	        if (image.getHistoLUTFrame() != null) {
    	            updateHistoLUTFrame();
    	        }
    		}
    	}
    }
    
    
    
    

    
    /**
     * key typed
     */
	public void keyTyped(KeyEvent event) {
		Object source = event.getSource();
		
		if(event.getKeyChar() == KeyEvent.VK_ENTER  ) {
			if(source == levelValTextField) {
				String numString = levelValTextField.getText();
				float num = validateCurrentNum(numString, levelMinFloat, levelMaxFloat);
				if(num != -1) {
					float val = ((num - minImage) * levelSliderMax) / (maxImage - minImage);
					levelSlider.setValue((int)val);
				}else {
					level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
					levelValTextField.setText(Float.toString(Math.round(level)));
				}
			}else if(source == winValTextField){
				String numString = winValTextField.getText();
				float num = validateCurrentNum(numString, winMinFloat, winMaxFloat);
				if(num != -1) {
					float val = (num * windowSliderMax)/(2 * (maxImage - minImage));
					windowSlider.setValue((int)val);
				}else {
					window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);
					winValTextField.setText(Float.toString(Math.round(window)));
				}
			}else if(source == minValTextField) {
				String numStringMin = minValTextField.getText();
				String numStringMax = maxValTextField.getText();
				boolean success = validateMinMaxNums(numStringMin, numStringMax);
				if(success == true) {
					float val = Float.parseFloat(numStringMin);
					val = (val - minMaxBInt)/minMaxSlope;
					minSlider.setValue((int)val);
				}else {
					min = minSlider.getValue();
	        		
	    			min = (minMaxSlope * min) + minMaxBInt;

	        		
	        		if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {
	        			minValTextField.setText(Float.toString(min));
	        		}else {
	        			min = Math.round(min);
	        			minValTextField.setText(Float.toString(min));
	        		}
				}
				
			}else if(source == maxValTextField) {
				String numStringMin = minValTextField.getText();
				String numStringMax = maxValTextField.getText();
				boolean success = validateMinMaxNums(numStringMin, numStringMax);
				if(success == true) {
					float val = Float.parseFloat(numStringMax);
					val = (val - minMaxBInt)/minMaxSlope;
					maxSlider.setValue((int)val);
				}else {

	        		max = maxSlider.getValue();
	        		

	        		max = (minMaxSlope * max) + minMaxBInt;
	        		
	        		if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {

	        			maxValTextField.setText(Float.toString(max));
	        		}else {

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
     * @param  min  min value
     * @param  max  max value
     */
    public void updateSliders(int min, int max) {
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
     * @param  gbc  DOCUMENT ME!
     */
    private void buildButtons(GridBagConstraints gbc) {
    	
    	JPanel buttonPanel = new JPanel();
    	
        
        
        saveButton = new JButton("Save");
        saveButton.addActionListener(this);
        //closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        //closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        saveButton.setFont(serif12B);
        saveButton.setSize(MipavUtil.defaultButtonSize);
        saveButton.setToolTipText("Save values to Preferences file");
        buttonPanel.add(saveButton);
        
        loadButton = new JButton("Load");
        loadButton.addActionListener(this);
        //closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        //closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        loadButton.setFont(serif12B);
        loadButton.setSize(MipavUtil.defaultButtonSize);
        loadButton.setToolTipText("Load values from Preferences file");
        buttonPanel.add(loadButton);
        
        closeButton = new JButton("Close");
        closeButton.addActionListener(this);
        //closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        //closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        closeButton.setFont(serif12B);
        closeButton.setSize(MipavUtil.defaultButtonSize);
        buttonPanel.add(closeButton);
        
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    /**
     * Builds the level slider and places it in the slider panel.
     *
     * @param  spanel  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     */
    private void buildLevelSlider(JPanel spanel, GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        JLabel levelMax = new JLabel(Float.toString(maxImage));
        levelMaxFloat = maxImage;
        levelMax.setForeground(Color.black);
        levelMax.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);
        
        JLabel levelLabel = new JLabel("Level");
        spanel.add(levelLabel, gbc);
        
        gbc.gridy = 1;
        
        spanel.add(levelMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        level = (x[1] + x[2]) / 2.0f;
        levelSlider = new JSlider(0, levelSliderMax,
                                  (int) ((level - minImage) * levelSliderMax / (maxImage - minImage)));

        // set slider attributes
        levelSlider.setFont(serif12);
        levelSlider.setEnabled(true);

        if ((image.getType() == ModelImage.BYTE) || (image.getType() == ModelImage.UBYTE)) {
            levelSlider.setMajorTickSpacing((int) (levelSliderMax * 0.25f));
        } else {
            levelSlider.setMajorTickSpacing((int) (levelSliderMax * 0.25f));
        }

        levelSlider.setPaintTicks(true);
        levelSlider.addChangeListener(this);
        levelSlider.setVisible(true);
        levelSlider.setOrientation(javax.swing.JSlider.VERTICAL);

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

        JLabel levelMin = new JLabel(Float.toString(minImage));
        levelMinFloat = minImage;
        levelMin.setForeground(Color.black);
        levelMin.setFont(serif12);
        spanel.add(levelMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        //levelValLabel = new JLabel(Float.toString(level));
        //levelValLabel.setForeground(Color.black);
        //levelValLabel.setFont(serif12B);
        levelValTextField = new JTextField(6);
        levelValTextField.setText(Float.toString(level));
        levelValTextField.addKeyListener(this);
        levelValTextField.addFocusListener(this);
        spanel.add(levelValTextField, gbc);
    }
    
    
    
    
    /**
     * Builds the min slider and places it in the slider panel.
     *
     * @param  spanel  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     */
    private void buildMinSlider(JPanel spanel, GridBagConstraints gbc) {

    	
    	
    	
        // discovers the slider max and applies it to a
        // label at the top of the slider
        JLabel sliderMax = new JLabel(Float.toString(maxImage));
        //levelMaxFloat = maxImage;
        sliderMax.setForeground(Color.black);
        sliderMax.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);
        
        JLabel minLabel = new JLabel("Min");
        spanel.add(minLabel, gbc);
        
        gbc.gridy = 1;
        spanel.add(sliderMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        min = .25f * (maxImage - minImage);
        minSlider = new JSlider(0, 1999,500);

        // set slider attributes
        minSlider.setFont(serif12);
        minSlider.setEnabled(true);

        if ((image.getType() == ModelImage.BYTE) || (image.getType() == ModelImage.UBYTE)) {
        	minSlider.setMajorTickSpacing(500);
        } else {
        	minSlider.setMajorTickSpacing(500);
        }

        minSlider.setPaintTicks(true);
        minSlider.addChangeListener(this);
        minSlider.setVisible(true);
        minSlider.setOrientation(javax.swing.JSlider.VERTICAL);
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

        JLabel sliderMin = new JLabel(Float.toString(minImage));
        //levelMinFloat = minImage;
        sliderMin.setForeground(Color.black);
        sliderMin.setFont(serif12);
        spanel.add(sliderMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        //levelValLabel = new JLabel(Float.toString(level));
        //levelValLabel.setForeground(Color.black);
        //levelValLabel.setFont(serif12B);
        minValTextField = new JTextField(6);
        minValTextField.setText(Float.toString(min));
        minValTextField.addKeyListener(this);
        minValTextField.addFocusListener(this);
        spanel.add(minValTextField, gbc);
        
        minMaxBInt = minImage;
        minMaxSlope = (maxImage - minImage)/1999;
        
    }
    
    
    
    
    
    
    
    private void buildMaxSlider(JPanel spanel, GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        JLabel sliderMax = new JLabel(Float.toString(maxImage));
        //levelMaxFloat = maxImage;
        sliderMax.setForeground(Color.black);
        sliderMax.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);
        
        JLabel maxLabel = new JLabel("Max");
        spanel.add(maxLabel, gbc);
        
        gbc.gridy = 1;
        
        spanel.add(sliderMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        max = .75f * (maxImage - minImage);
        maxSlider = new JSlider(0, 1999,1500);

        // set slider attributes
        maxSlider.setFont(serif12);
        maxSlider.setEnabled(true);

        if ((image.getType() == ModelImage.BYTE) || (image.getType() == ModelImage.UBYTE)) {
        	maxSlider.setMajorTickSpacing(500);
        } else {
        	maxSlider.setMajorTickSpacing(500);
        }

        maxSlider.setPaintTicks(true);
        maxSlider.addChangeListener(this);
        maxSlider.setVisible(true);
        maxSlider.setOrientation(javax.swing.JSlider.VERTICAL);
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

        JLabel sliderMin = new JLabel(Float.toString(minImage));
        //levelMinFloat = minImage;
        sliderMin.setForeground(Color.black);
        sliderMin.setFont(serif12);
        spanel.add(sliderMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        //levelValLabel = new JLabel(Float.toString(level));
        //levelValLabel.setForeground(Color.black);
        //levelValLabel.setFont(serif12B);
        maxValTextField = new JTextField(6);
        maxValTextField.setText(Float.toString(max));
        maxValTextField.addKeyListener(this);
        maxValTextField.addFocusListener(this);
        spanel.add(maxValTextField, gbc);
    }
    
    
    
    

    /**
     * Builds the slider Panel.
     *
     * @param   borderTitle  the title of the border.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildWindowLevelPanel() {
        JPanel spanel = new JPanel();

        //getContentPane().add(spanel, BorderLayout.CENTER);
        spanel.setLayout(new GridBagLayout());

        spanel.setForeground(Color.black);
        //spanel.setBorder(buildTitledBorder(borderTitle));

        return spanel;
    }
    
    private JPanel buildMinMaxPanel() {
        JPanel spanel = new JPanel();

        //getContentPane().add(spanel, BorderLayout.CENTER);
        //spanel.setVisible(false);
        spanel.setLayout(new GridBagLayout());

        spanel.setForeground(Color.black);
        //spanel.setBorder(buildTitledBorder(borderTitle));

        return spanel;
    }
    
    

    /**
     * Builds the window slider and places it in the slider panel.
     *
     * @param  spanel  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     */
    private void buildWindowSlider(JPanel spanel, GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JLabel winMax = new JLabel(Float.toString(2.0f * (maxImage - minImage)));
        winMaxFloat = 2.0f * (maxImage - minImage);
        winMax.setForeground(Color.black);
        winMax.setFont(serif12);
        
        JLabel windowLabel = new JLabel("Window");
        spanel.add(windowLabel, gbc);
        
        gbc.gridy = 1;
        
        spanel.add(winMax, gbc);

        // current setting of the slider
        window = x[2] - x[1]; // the width of the window x[2] (max)  - x[1] (min)
        windowSlider = new JSlider(0, windowSliderMax,
                                   (int) (window * windowSliderMax / (2.0f * (maxImage - minImage))));

        // set slider attributes
        windowSlider.setFont(serif12);
        windowSlider.setEnabled(true);

        if ((image.getType() == ModelImage.BYTE) || (image.getType() == ModelImage.UBYTE)) {
            windowSlider.setMajorTickSpacing((int) (windowSliderMax * 0.25f));
        } else {
            windowSlider.setMajorTickSpacing((int) (windowSliderMax * 0.25f));
        }

        windowSlider.setPaintTicks(true);
        windowSlider.addChangeListener(this);
        windowSlider.setVisible(true);
        windowSlider.setOrientation(javax.swing.JSlider.VERTICAL);

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

        JLabel winMin = new JLabel("0.0");
        winMinFloat = 0.0f;
        winMin.setForeground(Color.black);
        winMin.setFont(serif12);
        spanel.add(winMin, gbc);

        // current value of window
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        //winValLabel = new JLabel(Float.toString(window));
        //winValLabel.setForeground(Color.black);
        //winValLabel.setFont(serif12B);
        winValTextField = new JTextField(6);
        winValTextField.setText(Float.toString(window));
        winValTextField.addKeyListener(this);
        winValTextField.addFocusListener(this);
        spanel.add(winValTextField, gbc);
    }
    
    
    
    /**
	 * validate current number
	 * @param numString
	 * @param min
	 * @param max
	 * @return
	 */
	public float validateCurrentNum(String numString, float min, float max) {
		float num;

		try {
			num = Float.parseFloat(numString);
		}catch(NumberFormatException e){
			return -1;
		}
		if(num >= min && num <= max) {
			return num;
		}else {
			return -1;
		}
	}
	
	
	
	public boolean validateMinMaxNums(String minString, String maxString) {
		float numMin;
		float numMax;
		
		try {
			numMin = Float.parseFloat(minString);
			numMax = Float.parseFloat(maxString);
		}catch(NumberFormatException e){
			return false;
		}
		
		if(numMin < minImage || numMin > maxImage || numMax < minImage || numMax > maxImage || numMin > numMax) {
			return false;
		}else {
			return true;
		}
		
		
	}

    /**
     * Calculate the maximum and minimum valuse to setup the window and level sliders.
     */
    private void calcMinMax() {

        if (image.getType() == ModelStorageBase.UBYTE) {
            minImage = 0;
            maxImage = 255;
            levelSliderMax = 255;
            windowSliderMax = 511;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            minImage = -128;
            maxImage = 127;
            levelSliderMax = 255;
            windowSliderMax = 511;
        } else {
            minImage = (float) image.getMin();
            maxImage = (float) image.getMax();
            levelSliderMax = 1999;
            windowSliderMax = 3999;
        }
    }

    /**
     * Displays histoLUT frame for a gray scale image.
     */
    private void updateHistoLUTFrame() {

        image.notifyImageDisplayListeners(LUT, false);
        image.getHistoLUTFrame().update();

    }
    
    public void keyPressed(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	public void keyReleased(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		Object source = e.getSource();

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
	
	
	

} // end class JDialogWinLevel
