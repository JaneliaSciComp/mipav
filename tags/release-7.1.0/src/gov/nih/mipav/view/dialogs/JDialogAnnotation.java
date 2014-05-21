package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.plaf.basic.BasicBorders;


/**
 * Dialog for creating new/editing existing on-screen annotations. This allows writing text and choosing the font style,
 * size and color.
 *
 * @author   Ben Link
 * @version  1.0
 */
public class JDialogAnnotation extends JDialogBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2171038314100102783L;
    
    /**Default notes section text*/
    public static final String DEFAULT_NOTES = "Enter notes for the annotation here.  This field is optional.";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The image that contains the VOI text. */
    private ModelImage activeImage;

    /** button to bring up the background color chooser. */
    private JButton backgroundColorButton;

    /** checkbox for bold style. */
    private JCheckBox boldBox;

    /** Panel to contain the okay/cancel/help buttons. */
    private JPanel buttonPanel;

    /** button to bring up color chooser. */
    private JButton colorButton;

    /** color chooser to select text color. */
    private ViewJColorChooser colorChooser;

    /** descriptors for font (BOLD, PLAIN, ITALIC). */
    private int fontDescriptors;

    /** name/style of the font. */
    private String fontName;

    /** size of font (int). */
    private int fontSize;

    /** textfield for font size (int only). */
    private JTextField fontSizeField;

    /** combobox to hold the names of all available fonts. */
    private JComboBox fontTypeBox;

    /** toggle between background and text color changing. */
    private boolean isBackground = false;

    /** whether this is an existing or new VOIText (isRegistered = existing). */
    private boolean isRegistered;

    /** checkbox for italic style. */
    private JCheckBox italicBox;

    /** NameField that will contain the string to be displayed. */
    private JTextField nameField;

    /** NoteField for providing a note about this marker. Note only displayed when clicked. */
    private JTextArea noteField;
    
    /** the VOI that contains the VOIText. */
    private VOI textVOI;
    
    /** the VOIText element in the VOI to modify */
    private int element = 0;

    /** DOCUMENT ME! */
    private JCheckBox useMarkerBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new annotation dialog, displays it, and turns recording on.
     *
     * @param  image         Title of dialog frame
     * @param  textVOI       user interface (dialog uses main frame from UI as parent)
     * @param  slice         DOCUMENT ME!
     * @param  isRegistered  DOCUMENT ME!
     */
    public JDialogAnnotation(ModelImage image, VOI textVOI, int element, boolean isRegistered, boolean modal) {
        super(image.getParentFrame(), modal);
        this.activeImage = image;
        this.textVOI = textVOI;
        this.element = element;

        if (textVOI == null) {
            return;
        }
        this.isRegistered = isRegistered;
        init("Annotation");
        setVisible(true);
        setResizable(false);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Catches action events: Okay, Cancel, Help and ChooseColor.
     *
     * @param  event  ActionEvent
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (textVOI != null) {

                if (setVariables()) {

                    if (activeImage != null) {

                        if (!isRegistered) {
                            activeImage.registerVOI(textVOI);
                        }

                        textVOI.setActive(true);
                        //textVOI.setName(nameField.getText());
                        if(!noteField.getText().equals(DEFAULT_NOTES) && noteField.getText().length() > 0)
                        	((VOIText) (textVOI.getCurves().elementAt(element))).setNote(noteField.getText());
                        ((VOIText) (textVOI.getCurves().elementAt(element))).setActive(true);
                        activeImage.notifyImageDisplayListeners();
                    }

                    windowClosing(null);
                }
            }
        } else if (command.equals("ChooseColor")) {
            isBackground = false;

            // open up a color chooser dialog
            colorChooser = new ViewJColorChooser(this, "Pick text color", new OkColorListener(),
                                                 new CancelListener());

        } else if (command.equals("ChooseBackgroundColor")) {
            isBackground = true;

            // open up a color chooser dialog
            colorChooser = new ViewJColorChooser(this, "Pick background color", new OkColorListener(),
                                                 new CancelListener());

        } else if (command.equals("Cancel")) {
            windowClosing(null);
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("102572");
            MipavUtil.showWebHelp(" Creating_new_images#Annotating_images_with_text");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Watches the font descriptor checkboxes (bold/italic) and the font name (style) combo box, updates displayed font
     * with each change.
     *
     * @param  event  ItemEvent the item change event that occured
     */
    public void itemStateChanged(ItemEvent event) {

        if ((event.getSource() == boldBox) || (event.getSource() == italicBox)) {
            fontDescriptors = Font.PLAIN;
            if (boldBox.isSelected()) 
                fontDescriptors += Font.BOLD;
            if (italicBox.isSelected()) 
                fontDescriptors += Font.ITALIC;
                
        	nameField.setFont(new Font((String) fontTypeBox.getSelectedItem(), Font.PLAIN, 12));
            noteField.setFont(new Font((String) fontTypeBox.getSelectedItem(), Font.PLAIN, 12));

        } else if (event.getSource() == fontTypeBox) {
            nameField.setFont(new Font((String) fontTypeBox.getSelectedItem(), Font.PLAIN, 12));
            noteField.setFont(new Font((String) fontTypeBox.getSelectedItem(), Font.PLAIN, 12));
        }

        pack();
    }

    /**
     * builds the panel that allows customization of font style/size/color.
     *
     * @return  JPanel the font panel
     */
    private JPanel buildFontPanel() {
        JPanel fontPanel = new JPanel();
        fontPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        JLabel pointLabel = new JLabel("pt");

        fontTypeBox = new JComboBox();

        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        String[] names = ge.getAvailableFontFamilyNames();

        for (int i = 0; i < names.length; i++) {
            fontTypeBox.addItem(names[i]);
        }

        fontTypeBox.addItemListener(this);

        boldBox = new JCheckBox("Bold");
        boldBox.addItemListener(this);

        italicBox = new JCheckBox("Italic");
        italicBox.addItemListener(this);

        fontSizeField = new JTextField(2);
        fontSizeField.setText("12");
        MipavUtil.makeNumericsOnly(fontSizeField, false);

        // update the displayed text when the font size is changed and the user presses enter
        // for whatever reason, the keylistener wasn't getting the enter key event after the
        // input/actionmap sets were added to jdialogbase, but this seems to work
        fontSizeField.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke("ENTER"), "updateText");
        fontSizeField.getActionMap().put("updateText", new UpdateTextAction());

        VOIText vt = (VOIText) textVOI.getCurves().elementAt(element);
        fontSize = vt.getFontSize();
        fontSizeField.setText(Integer.toString(fontSize));
        fontDescriptors = vt.getFontDescriptors();

        if (fontDescriptors == Font.BOLD) {
            boldBox.setSelected(true);
        } else if (fontDescriptors == Font.ITALIC) {
            italicBox.setSelected(true);
        } else if (fontDescriptors == (Font.BOLD + Font.ITALIC)) {
            boldBox.setSelected(true);
            italicBox.setSelected(true);
        }

        fontName = vt.getFontName();

        for (int i = 0; i < fontTypeBox.getItemCount(); i++) {

            if (fontName.equalsIgnoreCase((String) fontTypeBox.getItemAt(i))) {
                fontTypeBox.setSelectedIndex(i);

                break;
            }
        }

        colorButton = new JButton(MipavUtil.getIcon("transparent.gif"));

        colorButton.setBackground(vt.getColor());
        colorButton.setForeground(vt.getColor());
        colorButton.setToolTipText("Click to change text color");

        colorButton.addActionListener(this);
        colorButton.setActionCommand("ChooseColor");
        colorButton.setSize(24, 24);
        colorButton.setMaximumSize(new Dimension(24, 24));

        backgroundColorButton = new JButton(MipavUtil.getIcon("transparent.gif"));

        backgroundColorButton.setBackground(vt.getBackgroundColor());
        backgroundColorButton.setForeground(vt.getBackgroundColor());
        backgroundColorButton.setToolTipText("Click to change background color");

        backgroundColorButton.addActionListener(this);
        backgroundColorButton.setActionCommand("ChooseBackgroundColor");
        backgroundColorButton.setSize(24, 24);
        backgroundColorButton.setMaximumSize(new Dimension(24, 24));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 2;
        gbc.insets = new Insets(0, 5, 0, 5);
        gbc.gridwidth = 2;

        fontPanel.add(fontTypeBox);

        gbc.gridx = 2;
        gbc.gridwidth = 1;

        fontPanel.add(boldBox);

        gbc.gridx = 3;

        fontPanel.add(italicBox);

        gbc.gridx = 4;

        gbc.insets = new Insets(0, 5, 0, 0);
        fontPanel.add(fontSizeField, gbc);

        gbc.insets = new Insets(0, 0, 0, 5);

        gbc.gridx = 5;

        fontPanel.add(pointLabel, gbc);

        gbc.gridx = 6;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        fontPanel.add(colorButton, gbc);

        gbc.gridx = 7;
        fontPanel.add(backgroundColorButton, gbc);

        fontPanel.setBorder(buildTitledBorder("Font options"));

        return fontPanel;
    }

    /**
     * Initializes the dialog box and adds the components.
     *
     * @param  title  Title of the dialog box.
     */
    private void init(String title) {
        setTitle(title);

        BorderLayout layout = new BorderLayout(25, 25);

        JPanel namePanel = new JPanel();
        buttonPanel = this.buildButtons();

        nameField = new JTextField(25);
        
        namePanel.setBorder(buildTitledBorder("Name"));

        namePanel.add(nameField);

        VOIText vt = (VOIText) textVOI.getCurves().elementAt(element);
        nameField.setFont(new Font(vt.getFontName(), vt.getFontDescriptors(), vt.getFontSize()));
        nameField.setBorder(BasicBorders.getTextFieldBorder());

        if (!vt.getText().equals("")) {
            nameField.setText(vt.getText());
        } else {
            nameField.setText("Enter name here");
        }
        nameField.setMinimumSize(new Dimension(229, 26));
        nameField.setPreferredSize(new Dimension(229, 26));
        nameField.setMaximumSize(new Dimension(229, 26));

        JPanel markerPanel = new JPanel();
        markerPanel.setBorder(buildTitledBorder("Marker options"));

        useMarkerBox = WidgetFactory.buildCheckBox("Use arrow marker", false, this);

        markerPanel.add(useMarkerBox);

        boolean useMarker = vt.useMarker();
        useMarkerBox.setSelected(useMarker);
        
        JPanel notePanel = new JPanel();
        notePanel.setBorder(buildTitledBorder("Notes Section"));
       
        noteField = new JTextArea(DEFAULT_NOTES);
        noteField.setRows(4);
        noteField.setFont(new Font(vt.getFontName(), vt.getFontDescriptors(), vt.getFontSize()));
        noteField.setEditable(true);
        noteField.setLineWrap(true);
        noteField.setWrapStyleWord(true);
        if (!vt.getNote().equals("") && !vt.getNote().equals(DEFAULT_NOTES)) {
            noteField.setText(vt.getNote());
        } else {
            noteField.setText(DEFAULT_NOTES);
        }
        
        JScrollPane containerPane = new JScrollPane(noteField);
        containerPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        containerPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        containerPane.setMinimumSize(new Dimension(505, 93));
        containerPane.setMaximumSize(new Dimension(505, 93));
        containerPane.setPreferredSize(new Dimension(505, 93));
        containerPane.setBorder(BasicBorders.getTextFieldBorder());

        notePanel.add(containerPane);

        mainDialogPanel.setLayout(layout);

        JPanel centerPanel = new JPanel(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 2;
        centerPanel.add(buildFontPanel(), c);
        c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 1;
		c.gridwidth = 1;
        centerPanel.add(namePanel, c);
        c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 1;
		c.gridy = 1;
		c.gridwidth = 1;
        centerPanel.add(markerPanel, c);
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 2;
        c.gridwidth = 2;
        centerPanel.add(notePanel, c);

        mainDialogPanel.add(centerPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        nameField.requestFocus();
        nameField.selectAll();
    }

    /**
     * makes sure the dialog is ready after (OKAY) is pressed.
     *
     * @return  boolean are the dialog's variables kosher
     */
    private boolean setVariables() {

        try {
            VOIText vt = (VOIText) textVOI.getCurves().elementAt(element);
            vt.setFontSize(Integer.parseInt(fontSizeField.getText()));
            vt.setText(nameField.getText());

            vt.setFontDescriptors(fontDescriptors);
            vt.setFontName((String) fontTypeBox.getSelectedItem());
            vt.setColor(colorButton.getForeground());
            //textVOI.setColor(colorButton.getForeground());
            vt.setBackgroundColor(backgroundColorButton.getForeground());

            Preferences.setProperty(Preferences.PREF_VOI_TEXT_COLOR, MipavUtil.makeColorString(colorButton.getForeground()));
            Preferences.setProperty(Preferences.PREF_VOI_TEXT_BACKGROUND_COLOR, MipavUtil.makeColorString(backgroundColorButton.getForeground()));
            
            vt.setUseMarker(useMarkerBox.isSelected());

        } catch (Exception ex) {
            return false;
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Update the displayed text accourding to the current settings of the window.
     */
    protected class UpdateTextAction extends AbstractAction {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -7210907002611412247L;

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent event) {

            if (!fontSizeField.getText().equals("")) {
                nameField.setFont(new Font((String) fontTypeBox.getSelectedItem(), fontDescriptors,
                                           Integer.parseInt(fontSizeField.getText())));
                noteField.setFont(new Font((String) fontTypeBox.getSelectedItem(), fontDescriptors,
                        Integer.parseInt(fontSizeField.getText())));
            }
            pack();
        }
    }


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
            Color color = colorChooser.getColor();

            if (isBackground) {
                backgroundColorButton.setForeground(color);
                backgroundColorButton.setBackground(color);
            } else {
                colorButton.setForeground(color);
                colorButton.setBackground(color);
            }
        }
    }
}
