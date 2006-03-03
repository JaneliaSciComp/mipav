package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import javax.swing.*;

import java.awt.event.*;
import java.awt.*;


/**
 * Dialog for creating new/editing existing on-screen
 * annotations.  This allows writing text and choosing the font style, size and color.
 * @author Ben Link
 * @version 1.0
 */
public class JDialogAnnotation extends JDialogBase
    implements ActionListener {

    /**
     * The image that contains the VOI text
     */
    private ModelImage activeImage;

    /**
     * TextField that will contain the string to be displayed
     */
    private JTextField textField;

    /**
     * Panel to contain the okay/cancel/help buttons
     */
    private JPanel buttonPanel;

    /**
     * panel to hold the jtextfield
     */
    private JPanel scrollPanel;

    /**
     * pane within scrollpanel
     */
    private JScrollPane scrollPane;

    /**
     * the user interface
     */
    private ViewUserInterface userInterface;

    /**
     * combobox to hold the names of all available fonts
     */
    private JComboBox fontTypeBox;

    /**
     * textfield for font size (int only)
     */
    private JTextField fontSizeField;

    /**
     * checkbox for bold style
     */
    private JCheckBox boldBox;

    /**
     * checkbox for italic style
     */
    private JCheckBox italicBox;

    /**
     * button to bring up color chooser
     */
    private JButton colorButton;

    /**
     * color chooser to select text color
     */
    private ViewJColorChooser colorChooser;

    /**
     * the VOI that contains the VOIText
     */
    private VOI textVOI;

    /**
     * name/style of the font
     */
    private String fontName;

    /**
     * descriptors for font (BOLD, PLAIN, ITALIC)
     */
    private int fontDescriptors;

    /**
     * size of font (int)
     */
    private int fontSize;

    /**
     * slice of image where the VOI resides
     */
    private int slice;

    /**
     * whether this is an existing or new VOIText (isRegistered = existing)
     */
    private boolean isRegistered;

    /**
     *  Constructs a new annotation dialog, displays it, and turns recording on.
     *
     *  @param title  Title of dialog frame
     *  @param ui     user interface (dialog uses main frame from UI as parent)
     */
    public JDialogAnnotation( ModelImage image, VOI textVOI, int slice, boolean isRegistered ) {
        super( image.getParentFrame(), false );
        this.activeImage = image;
        this.textVOI = textVOI;
        if ( textVOI == null ) {
            return;
        }
        this.slice = slice;
        this.isRegistered = isRegistered;
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();
        init( "Annotation" );



        setVisible( true );
        setResizable( false );
    }

    /**
     * Catches action events: Okay, Cancel, Help and ChooseColor
     * @param event ActionEvent
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();

        if ( command.equals( "OK" ) ) {
            if ( textVOI != null ) {
                if ( setVariables() ) {
                    if ( activeImage != null ) {
                        if ( !isRegistered ) {
                            activeImage.registerVOI( textVOI );
                        }
                        textVOI.setActive( true );
                        ( (VOIText) ( textVOI.getCurves()[slice].elementAt( 0 ) ) ).setActive( true );
                        activeImage.notifyImageDisplayListeners();
                    }
                    windowClosing( null );
                }
            }
        } else if ( command.equals( "ChooseColor" ) ) {
            //open up a color chooser dialog
            colorChooser = new ViewJColorChooser( this.parentFrame, "Pick VOI color", new OkColorListener(),
                    new CancelListener() );

        } else if ( command.equals( "Cancel" ) ) {
            windowClosing( null );
        } else if ( command.equals( "Help" ) ) {
            MipavUtil.showHelp( "102572" );
        }
    }

    /**
     * Watches the font descriptor checkboxes (bold/italic)
     * and the font name (style) combo box, updates displayed font
     * with each change
     * @param event ItemEvent the item change event that occured
     */
    public void itemStateChanged( ItemEvent event ) {
        if ( event.getSource() == boldBox || event.getSource() == italicBox ) {
            fontDescriptors = Font.PLAIN;
            if ( boldBox.isSelected() ) {
                fontDescriptors += Font.BOLD;
            }
            if ( italicBox.isSelected() ) {
                fontDescriptors += Font.ITALIC;
            }

            textField.setFont(
                    new Font( (String) fontTypeBox.getSelectedItem(), fontDescriptors,
                    Integer.parseInt( fontSizeField.getText() ) ) );

        } else if ( event.getSource() == fontTypeBox ) {
            textField.setFont(
                    new Font( (String) fontTypeBox.getSelectedItem(), fontDescriptors,
                    Integer.parseInt( fontSizeField.getText() ) ) );
        }
        pack();
    }

    /**
     * Initializes the dialog box and adds the components.
     * @param title   Title of the dialog box.
     */
    private void init( String title ) {
        setTitle( title );

        BorderLayout layout = new BorderLayout( 25, 25 );

        scrollPanel = new JPanel();
        buttonPanel = this.buildButtons();

        textField = new JTextField( 30 );
        textField.setBackground( Color.DARK_GRAY );

        scrollPane = new JScrollPane( textField, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );

        scrollPanel.setBorder( buildTitledBorder( "Text" ) );

        scrollPanel.add( scrollPane );

        VOIText vt = (VOIText) textVOI.getCurves()[slice].elementAt( 0 );
        textField.setFont( new Font( vt.getFontName(), vt.getFontDescriptors(), vt.getFontSize() ) );

        if ( !vt.getText().equals( "" ) ) {
            textField.setText( vt.getText() );
        } else {
            textField.setText( "Enter text here" );
        }

        mainDialogPanel.setLayout( layout );

        JPanel centerPanel = new JPanel( new BorderLayout() );

        centerPanel.add( buildFontPanel(), BorderLayout.NORTH );
        centerPanel.add( scrollPanel, BorderLayout.CENTER );

        mainDialogPanel.add( centerPanel, BorderLayout.CENTER );
        mainDialogPanel.add( buttonPanel, BorderLayout.SOUTH );

        getContentPane().add( mainDialogPanel );

        pack();
        textField.requestFocus();
        textField.selectAll();
    }

    /**
     * builds the panel that allows customization of font style/size/color
     * @return JPanel the font panel
     */
    private JPanel buildFontPanel() {
        JPanel fontPanel = new JPanel();
        fontPanel.setLayout( new GridBagLayout() );
        GridBagConstraints gbc = new GridBagConstraints();

        JLabel pointLabel = new JLabel( "pt" );

        fontTypeBox = new JComboBox();
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        String[] names = ge.getAvailableFontFamilyNames();
        for ( int i = 0; i < names.length; i++ ) {
            fontTypeBox.addItem( names[i] );
        }

        fontTypeBox.addItemListener( this );

        boldBox = new JCheckBox( "bold" );
        boldBox.addItemListener( this );

        italicBox = new JCheckBox( "italic" );
        italicBox.addItemListener( this );

        fontSizeField = new JTextField( 2 );
        fontSizeField.setText( "12" );
        MipavUtil.makeNumericsOnly( fontSizeField, false );

        // update the displayed text when the font size is changed and the user presses enter
        // for whatever reason, the keylistener wasn't getting the enter key event after the
        // input/actionmap sets were added to jdialogbase, but this seems to work
        fontSizeField.getInputMap( JComponent.WHEN_FOCUSED ).put( KeyStroke.getKeyStroke( "ENTER" ), "updateText" );
        fontSizeField.getActionMap().put( "updateText", new UpdateTextAction() );

        VOIText vt = (VOIText) textVOI.getCurves()[slice].elementAt( 0 );
        fontSize = vt.getFontSize();
        fontSizeField.setText( Integer.toString( fontSize ) );
        fontDescriptors = vt.getFontDescriptors();
        if ( fontDescriptors == Font.BOLD ) {
            boldBox.setSelected( true );
        } else if ( fontDescriptors == Font.ITALIC ) {
            italicBox.setSelected( true );
        } else if ( fontDescriptors == ( Font.BOLD + Font.ITALIC ) ) {
            boldBox.setSelected( true );
            italicBox.setSelected( true );
        }
        fontName = vt.getFontName();
        for ( int i = 0; i < fontTypeBox.getItemCount(); i++ ) {
            if ( fontName.equalsIgnoreCase( (String) fontTypeBox.getItemAt( i ) ) ) {
                fontTypeBox.setSelectedIndex( i );
                break;
            }
        }

        colorButton = new JButton( MipavUtil.getIcon( "transparent.gif" ) );

        colorButton.setBackground( textVOI.getColor() );
        colorButton.setForeground( textVOI.getColor() );
        colorButton.setToolTipText( "Click to change text color" );
        textField.setForeground( textVOI.getColor() );

        colorButton.addActionListener( this );
        colorButton.setActionCommand( "ChooseColor" );
        colorButton.setSize( 24, 24 );
        colorButton.setMaximumSize( new Dimension( 24, 24 ) );

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.NONE;
        gbc.gridwidth = 2;
        gbc.insets = new Insets( 0, 5, 0, 5 );
        gbc.gridwidth = 2;

        fontPanel.add( fontTypeBox );

        gbc.gridx = 2;
        gbc.gridwidth = 1;

        fontPanel.add( boldBox );

        gbc.gridx = 3;

        fontPanel.add( italicBox );

        gbc.gridx = 4;

        gbc.insets = new Insets( 0, 5, 0, 0 );
        fontPanel.add( fontSizeField, gbc );

        gbc.insets = new Insets( 0, 0, 0, 5 );

        gbc.gridx = 5;

        fontPanel.add( pointLabel, gbc );

        gbc.gridx = 6;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        fontPanel.add( colorButton, gbc );

        fontPanel.setBorder( buildTitledBorder( "Font options" ) );

        return fontPanel;
    }

    /**
     * makes sure the dialog is ready after (OKAY) is pressed
     * @return boolean are the dialog's variables kosher
     */
    private boolean setVariables() {

        try {
            VOIText vt = (VOIText) textVOI.getCurves()[slice].elementAt( 0 );
            vt.setFontSize( Integer.parseInt( fontSizeField.getText() ) );
            vt.setText( textField.getText() );

            vt.setFontDescriptors( fontDescriptors );
            vt.setFontName( (String) fontTypeBox.getSelectedItem() );
            vt.setColor( colorButton.getForeground() );
            textVOI.setColor( colorButton.getForeground() );
        } catch ( Exception ex ) {
            return false;
        }

        return true;
    }

    /**
     *    Pick up the selected color and call method to change the VOI color
     *
     */
    class OkColorListener
        implements ActionListener {

        /**
         *   Get color from chooser and set button
         *   and VOI color.
         *   @param e    Event that triggered function.
         */
        public void actionPerformed( ActionEvent e ) {
            Color color = colorChooser.getColor();
            colorButton.setBackground( color );
            colorButton.setForeground( color );
            textField.setForeground( color );
        }
    }


    /**
     *    Does nothing.
     */
    class CancelListener
        implements ActionListener {

        /**
         *   Does nothing.
         */
        public void actionPerformed( ActionEvent e ) {}
    }

    /**
     * Update the displayed text accourding to the current settings of the window.
     */
    protected class UpdateTextAction extends AbstractAction {
        public void actionPerformed( ActionEvent event ) {
            if ( !fontSizeField.getText().equals( "" ) ) {
                textField.setFont( new Font( (String) fontTypeBox.getSelectedItem(), fontDescriptors, Integer.parseInt( fontSizeField.getText() ) ) );
                pack();
            }
        }
    }
}
