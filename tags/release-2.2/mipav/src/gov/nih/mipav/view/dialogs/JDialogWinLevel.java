package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;


/**
 *  Dialog creates 2 sliders which adjust the level and window of an image.
 *  The level is found at the x coordinate of the mid point of the sloping
 *  transfer segment.  The window is the x width of the sloping transfer
 *  segment.  Note y inversion in transfer segment because graphical origin
 *  is in upper left corner.
 *
 *                                      ________
 *                                     /
 *           255     ^                /
 *                   |               /
 *                   |              /
 *                   |             /  <------- Transfer function
 *                   |            /
 *            L      |           /
 *            U      |          /
 *            T      |         /
 *                   |        /
 *                   |       /
 *                   |______/
 *            0      |________________________________>
 *                          |      |      |
 *               minImage   |    level    |          maxImage
 *                          |             |
 *                         st win       end win
 *
 *                            Image intensity
 *
 *
 */
public class JDialogWinLevel extends JDialogBase
    implements ChangeListener {

    public static final int IMAGE_A = 0;
    public static final int IMAGE_B = 1;

    private JLabel levelLabel;
    private JSlider levelSlider;
    private JLabel windowLabel;
    private JSlider windowSlider;
    private JPanel sliderPanel;
    private JButton closeButton;
    private JLabel winValLabel, levelValLabel;

    /**
     *  Reference to the image that will be affected by the adjust of the window and level.
     */
    private ModelImage image;

    /**
     *  Reference to the LUT used to display the image
     */
    private ModelLUT LUT;

    /**
     *   Image's minimum intensity.
     */
    private float minImage;

    /**
     *   Image's maximum intensity.
     */
    private float maxImage;

    /**
     *  Three arrays to save the coordinates of the LUT's transfer fucntion.
     *  z[] not used.
     */
    private float[] x = new float[4];
    private float[] y = new float[4];
    private float[] z = new float[4];

    /**
     *  Reference to the image data of the slice presently displayed. Needed
     *  to calculate the max/min of the slice used to adjust the transfer function.
     */
    private float[] dataSlice;

    /**
     *   Stores the maximum slider value
     */
    private int levelSliderMax;

    /**
     *   Stores the minimum slider value
     */
    private int windowSliderMax;

    /**
     *   Average of the min and max extents of the transfer
     *   window that desribes the window size.
     */
    private float level;

    /**
     *   The size of the window.
     */
    private float window;

    /**
     *  Constructor
     *  @param parent          parent frame
     *  @param im              source image
     */
    public JDialogWinLevel( Frame theParentFrame, ModelImage image, ModelLUT LUT ) {
        super( theParentFrame, false );

        float min, max;
        int i;

        this.image = image;
        this.LUT = LUT;

        setTitle( "Level and Window" );
        setForeground( Color.black );

        getContentPane().setLayout( new BorderLayout() );
        calcMinMax();

        dataSlice = ( (ViewJFrameImage) parentFrame ).getComponentImage().getActiveImageBuffer();
        min = Float.MAX_VALUE;
        max = -Float.MAX_VALUE;
        for ( i = 0; i < dataSlice.length; i++ ) {
            if ( dataSlice[i] > max ) {
                max = dataSlice[i];
            }
            if ( dataSlice[i] < min ) {
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
        LUT.getTransferFunction().importArrays( x, y, 4 );

        GridBagConstraints gbc = new GridBagConstraints();

        // build a monochrome window/level slider panel and populate it
        sliderPanel = buildSliderPanel( "Level & Window" );
        buildLevelSlider( sliderPanel, gbc );
        buildWindowSlider( sliderPanel, gbc );
        buildCloseButton( gbc );

        setResizable( true );
        pack();
        locateDialog();

        setVisibleStandard( true );
        image.notifyImageDisplayListeners( LUT, false );
        if ( image.getHistoLUTFrame() != null ) {
            updateHistoLUTFrame();
        }

        System.gc();
    }

    /**
     *  Closes dialog box when the OK button is pressed and
     *  calls the algorithm
     *  @param event       event that triggers function
     */
    public void actionPerformed( ActionEvent event ) {
        Object source = event.getSource();

        if ( source == closeButton ) {
            dispose();
        }

    }

    /**
     *   Calculate the maximum and minimum valuse to setup the
     *   window and level sliders.
     */
    private void calcMinMax() {
        if ( image.getType() == ModelStorageBase.UBYTE ) {
            minImage = 0;
            maxImage = 255;
            levelSliderMax = 255;
            windowSliderMax = 511;
        } else if ( image.getType() == ModelStorageBase.BYTE ) {
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
     * Update the window, level sliders from CTPreset dialog.
     * @param min  min value
     * @param max  max value
     */
    public void updateSliders( int min, int max ) {
        int windowValue, levelValue;
        float winVal, levelVal;

        windowValue = ( max - min );
        levelValue = ( max + min ) / 2;

        winVal = windowValue * windowSliderMax / ( 2 * ( maxImage - minImage ) );
        levelVal = ( levelValue - minImage ) * levelSliderMax / ( maxImage - minImage );

        levelSlider.setValue( (int) levelVal );
        windowSlider.setValue( (int) winVal );
        level = ( levelSlider.getValue() * ( maxImage - minImage ) / levelSliderMax ) + minImage;
        window = ( windowSlider.getValue() * 2 * ( maxImage - minImage ) / windowSliderMax );

        winValLabel.setText( Float.toString( Math.round( window ) ) );
        levelValLabel.setText( Float.toString( Math.round( level ) ) );

    }

    /**
     *    Sets values based on knob along slider
     *    @param ChangeEvent  event that triggered this function
     */
    public void stateChanged( ChangeEvent e ) {
        // System.out.println("dialog chang(ing/ed)");

        // check old values...see if the corners x&y [1] is along the max/min
        // and then adjust sliders to match....
        // else, adjust the histgram to match the sliders... maybe later
        // keep the current histopoints inside the window/level adjustment.

        calcMinMax();
        level = ( levelSlider.getValue() * ( maxImage - minImage ) / levelSliderMax ) + minImage;
        window = ( windowSlider.getValue() * 2 * ( maxImage - minImage ) / windowSliderMax );

        winValLabel.setText( Float.toString( Math.round( window ) ) );
        levelValLabel.setText( Float.toString( Math.round( level ) ) );

        if ( window == 0 ) {
            window = 1;
        }

        x[2] = level + window / 2;
        if ( x[2] > maxImage ) {
            y[2] = 255.0f * ( x[2] - maxImage ) / window;
            x[2] = maxImage;
        } else {
            y[2] = 0.0f;
        }

        x[1] = level - window / 2;
        if ( x[1] < minImage ) {
            y[1] = 255.0f - 255.0f * ( minImage - x[1] ) / window;
            x[1] = minImage;
        } else {
            y[1] = 255.0f;
        }

        // update the transfer function so the on-screen image
        // (modelImage/viewJFrameImage) updates for the user
        LUT.getTransferFunction().importArrays( x, y, 4 );
        image.notifyImageDisplayListeners( LUT, false );

        // if ((levelSlider.getValueIsAdjusting()) || (windowSlider.getValueIsAdjusting())) {
        // return;
        // }

        // if the slider is finally done, update the transfer function
        // in the histogram.
        if ( image.getHistoLUTFrame() != null ) {
            updateHistoLUTFrame();
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
     *  Setting location of window-level adjustment panel
     *  based on the amount of space available near the image window
     *
     */
    public void locateDialog() {
        if ( parentFrame.getLocation().x - getSize().width > 0 ) {
            setLocation( parentFrame.getLocation().x - getSize().width, parentFrame.getLocation().y );
        } else {
            int tmp = ( parentFrame.getLocation().x + parentFrame.getSize().width );

            setLocation( tmp, parentFrame.getLocation().y + 30 );
        }
    }

    /**
     *   Overides the super.setVisible(boolean b) (which also
     *   locates a panel in the center of the screen), to use
     *   the super.setVisibleStandard(boolean b) which merely
     *   displays the Swing object onscreen.
     *   (as if it super.super.setVisible(boolean b) could be called)
     *   <i>when</i> the property in MipavProps "BoundWindowLevel"
     *   is <code>false</code> <i>or</i> when there is no property.
     *   The window/level dialog is "free."
     *   When there is "BoundWindowLevel" and when it is <code>true</code>,
     *   the window/level dialog will get relocated to next to the image
     *   window and then redrawn.
     *
     *   @see gov.nih.mipav.view.JDialogBase#setVisible(boolean)
     *   @see gov.nih.mipav.view.JDialogBase#setVisibleStandard(boolean)
     *   @see JDialogWinLevel#locateDialog()
     */
    public void setVisible( boolean vis ) {
        String bound = Preferences.getProperty( "BoundWindowLevel" );

        if ( bound != null ) { // default is to float the panel
            if ( bound.equalsIgnoreCase( "true" ) ) {
                locateDialog();
            }
        }
        super.setVisibleStandard( vis );
    }

    /**
     *  Displays histoLUT frame for a gray scale image
     *  @param imageAorB
     *
     */
    private void updateHistoLUTFrame() {

        image.notifyImageDisplayListeners( LUT, false );
        image.getHistoLUTFrame().update();

    }

    /**
     *   Builds the slider Panel
     *   @param borderTitle  the title of the border.
     */
    private JPanel buildSliderPanel( String borderTitle ) {
        JPanel spanel = new JPanel();

        getContentPane().add( spanel, BorderLayout.CENTER );
        spanel.setLayout( new GridBagLayout() );

        spanel.setForeground( Color.black );
        spanel.setBorder( buildTitledBorder( borderTitle ) );
        return spanel;
    }

    /**
     *   Builds the level slider and places it in the slider panel.
     */
    private void buildLevelSlider( JPanel spanel, GridBagConstraints gbc ) {
        // discovers the slider max and applies it to a
        // label at the top of the slider
        JLabel levelMax = new JLabel( Float.toString( maxImage ) );

        levelMax.setForeground( Color.black );
        levelMax.setFont( serif12 );
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = gbc.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets( 3, 3, 3, 3 );
        spanel.add( levelMax, gbc );

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        level = ( x[1] + x[2] ) / 2.0f;
        levelSlider = new JSlider( 0, levelSliderMax,
                (int) ( ( level - minImage ) * levelSliderMax / ( maxImage - minImage ) ) );
        // set slider attributes
        levelSlider.setFont( serif12 );
        levelSlider.setEnabled( true );
        if ( ( image.getType() == ModelImage.BYTE ) || ( image.getType() == ModelImage.UBYTE ) ) {
            levelSlider.setMajorTickSpacing( (int) ( levelSliderMax * 0.25f ) );
        } else {
            levelSlider.setMajorTickSpacing( (int) ( levelSliderMax * 0.25f ) );
        }
        levelSlider.setPaintTicks( true );
        levelSlider.addChangeListener( this );
        levelSlider.setVisible( true );
        levelSlider.setOrientation( javax.swing.JSlider.VERTICAL );

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add( levelSlider, gbc );

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 9;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        JLabel levelMin = new JLabel( Float.toString( minImage ) );

        levelMin.setForeground( Color.black );
        levelMin.setFont( serif12 );
        spanel.add( levelMin, gbc );

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        levelValLabel = new JLabel( Float.toString( level ) );
        levelValLabel.setForeground( Color.black );
        levelValLabel.setFont( serif12B );
        spanel.add( levelValLabel, gbc );
    }

    /**
     *   Builds the window slider and places it in the slider panel.
     */
    private void buildWindowSlider( JPanel spanel, GridBagConstraints gbc ) {
        // discovers the slider max and applies it to a
        // label at the top of the slider
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        JLabel winMax = new JLabel( Float.toString( 2.0f * ( maxImage - minImage ) ) );

        winMax.setForeground( Color.black );
        winMax.setFont( serif12 );
        spanel.add( winMax, gbc );

        // current setting of the slider
        window = x[2] - x[1]; // the width of the window x[2] (max)  - x[1] (min)
        windowSlider = new JSlider( 0, windowSliderMax,
                (int) ( window * windowSliderMax / ( 2.0f * ( maxImage - minImage ) ) ) );
        // set slider attributes
        windowSlider.setFont( serif12 );
        windowSlider.setEnabled( true );
        if ( ( image.getType() == ModelImage.BYTE ) || ( image.getType() == ModelImage.UBYTE ) ) {
            windowSlider.setMajorTickSpacing( (int) ( windowSliderMax * 0.25f ) );
        } else {
            windowSlider.setMajorTickSpacing( (int) ( windowSliderMax * 0.25f ) );
        }
        windowSlider.setPaintTicks( true );
        windowSlider.addChangeListener( this );
        windowSlider.setVisible( true );
        windowSlider.setOrientation( javax.swing.JSlider.VERTICAL );
        // slider placement
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add( windowSlider, gbc );

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.gridy = 9;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        JLabel winMin = new JLabel( "0.0" );

        winMin.setForeground( Color.black );
        winMin.setFont( serif12 );
        spanel.add( winMin, gbc );

        // current value of window
        gbc.gridx = 1;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        winValLabel = new JLabel( Float.toString( window ) );
        winValLabel.setForeground( Color.black );
        winValLabel.setFont( serif12B );
        spanel.add( winValLabel, gbc );
    }

    /**
     *   Builds the close button.
     */
    private void buildCloseButton( GridBagConstraints gbc ) {
        closeButton = new JButton( "Close" );
        closeButton.addActionListener( this );
        closeButton.setMinimumSize( MipavUtil.defaultButtonSize );
        closeButton.setPreferredSize( MipavUtil.defaultButtonSize );
        closeButton.setFont( serif12B );
        closeButton.setSize( MipavUtil.defaultButtonSize );
        getContentPane().add( closeButton, BorderLayout.SOUTH );
    }

}  // end class JDialogWinLevel
