package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;


/**
 * Simple dialog to change Opacity Settings (1 = opaque, 0 = transparent).
 *
 * @version    1.0 Sept 17, 1999
 * @author     Matthew J. McAuliffe, Ph.D.
 */
public class JDialogOpacityControls extends JDialogBase
    implements ChangeListener {

    /** Opacity slider. */
    private JSlider opacitySlider;

    /** Label that gives current value of slider. */
    private JLabel current;

    /** Opacity number. */
    private float opacity;

    /** Controls affected by opacity changes. */
    private ViewControlsImage controls;

    /**
     *   Creates new dialog with a slider and close button.
     *   @param  theParentFrame  The parent frame
     *   @param  cntrls          The controls that the opacity will apply to.
     */
    public JDialogOpacityControls( Frame theParentFrame, ViewControlsImage cntrls ) {
        super( theParentFrame, false );
        controls = cntrls;
        init( controls.getTools().getOpacity() );
    }

    /**
     *   Creates new dialog with a slider and close button.
     *   @param  theParentFrame  The parent frame
     *   @param  initVal         The initial value of the opacity.
     */
    public JDialogOpacityControls( Frame theParentFrame, float initVal ) {
        super( theParentFrame, false );
        init( initVal );
    }

    /**
     *   Creates new dialog with a slider and close button.
     *   @param  theParentFrame  The parent frame
     *   @param  cntrls          The controls that the opacity will apply to.
     *   @param  isVisible       Dialog visible or not
     */
    public JDialogOpacityControls( Frame theParentFrame, ViewControlsImage cntrls, boolean isVisible ) {
        super( theParentFrame, false );
        controls = cntrls;
        // init(controls.getTools().getOpacity());
    }

    /**
     *   Creates new dialog with a slider and close button.
     *   @param  theParentFrame  The parent frame
     *   @param  initVal         The initial value of the opacity.
     *   @param  isVisible       Dialog visible or not
     */
    public JDialogOpacityControls( Frame theParentFrame, float initVal, boolean isVisible ) {
        super( theParentFrame, false );
        // init(initVal);
    }

    /**
     *   Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     *   @param initValue    Initial value of slider.
     */
    private void init( float initValue ) {
        setTitle( "Paint Opacity" );
        opacitySlider = new JSlider( JSlider.HORIZONTAL, 0, 100, (int) ( initValue * 100 ) );

        opacitySlider.setMajorTickSpacing( 20 );
        opacitySlider.setPaintTicks( true );
        opacitySlider.setEnabled( true );
        opacitySlider.addChangeListener( this );

        JLabel maximum = new JLabel( String.valueOf( 1 ) );
        maximum.setForeground( Color.black );
        maximum.setFont( serif12 );

        current = new JLabel( String.valueOf( opacitySlider.getValue() / 100.0f ) );
        current.setForeground( Color.black );
        current.setFont( serif12B );

        JLabel minimum = new JLabel( String.valueOf( 0 ) );
        minimum.setForeground( Color.black );
        minimum.setFont( serif12 );

        JPanel sliderPanel = new JPanel( new GridBagLayout() );
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = gbc.HORIZONTAL;

        sliderPanel.add( opacitySlider, gbc );

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.NONE;

        sliderPanel.add( minimum, gbc );

        gbc.gridx = 1;
        gbc.anchor = gbc.CENTER;
        gbc.weightx = .5;

        sliderPanel.add( current, gbc );

        gbc.gridx = 2;
        gbc.anchor = gbc.EAST;
        gbc.weightx = 0;

        sliderPanel.add( maximum, gbc );
        sliderPanel.setBorder( buildTitledBorder( "Opacity" ) );

        JPanel buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText( "Close" );
        buttonPanel.add( cancelButton );

        JPanel mainPanel = new JPanel( new BorderLayout() );
        mainPanel.add( sliderPanel );
        mainPanel.add( buttonPanel, BorderLayout.SOUTH );
        mainPanel.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

        getContentPane().add( mainPanel );

        pack();
        setVisible( true );
    }

    /**
     *   Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     *   @param initValue    Initial value of slider.
     */
    public JPanel getMainPanel( float initValue ) {
        setTitle( "Paint Opacity" );
        opacitySlider = new JSlider( JSlider.HORIZONTAL, 0, 100, (int) ( initValue * 100 ) );

        opacitySlider.setMajorTickSpacing( 20 );
        opacitySlider.setPaintTicks( true );
        opacitySlider.setEnabled( true );
        opacitySlider.addChangeListener( this );

        JLabel maximum = new JLabel( String.valueOf( 1 ) );
        maximum.setForeground( Color.black );
        maximum.setFont( serif12 );

        current = new JLabel( String.valueOf( opacitySlider.getValue() / 100.0f ) );
        current.setForeground( Color.black );
        current.setFont( serif12B );

        JLabel minimum = new JLabel( String.valueOf( 0 ) );
        minimum.setForeground( Color.black );
        minimum.setFont( serif12 );

        JPanel sliderPanel = new JPanel( new GridBagLayout() );
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = gbc.HORIZONTAL;

        sliderPanel.add( opacitySlider, gbc );

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.NONE;

        sliderPanel.add( minimum, gbc );

        gbc.gridx = 1;
        gbc.anchor = gbc.CENTER;
        gbc.weightx = .5;

        sliderPanel.add( current, gbc );

        gbc.gridx = 2;
        gbc.anchor = gbc.EAST;
        gbc.weightx = 0;

        sliderPanel.add( maximum, gbc );
        sliderPanel.setBorder( buildTitledBorder( "Opacity" ) );

        JPanel buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText( "Apply" );
        buttonPanel.add( cancelButton );

        JPanel mainPanel = new JPanel();
        mainPanel.add( sliderPanel );
        mainPanel.add( buttonPanel );
        mainPanel.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

        return mainPanel;
    }

    /**
     *   Sets opacity once close button is pressed.
     *   @param event Event that triggered function
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();

        if ( command.equals( "Close" ) || command.equals( "Apply" ) ) {
            opacity = opacitySlider.getValue() / 100f;
            if ( controls != null ) {
                controls.getTools().setOpacity( opacity );
            }
            dispose();
        }
    }

    /**
     *    Sets values based on knob along slider.
     *    @param ChangeEvent  Event that triggered this function
     */
    public void stateChanged( ChangeEvent e ) {
        Object source = e.getSource();

        if ( source == opacitySlider ) {
            opacity = opacitySlider.getValue() / 100f;

            current.setText( String.valueOf( opacity ) );

            if ( !opacitySlider.getValueIsAdjusting() ) {
                controls.getTools().setOpacity( opacity );
                ( (ViewJFrameBase) parentFrame ).updateImages( true );
            }
        }
    }

    /**
     *    Accessor that returns the new opacity.
     *    @return Opacity.
     */
    public float getOpacity() {
        return opacity;
    }

}
