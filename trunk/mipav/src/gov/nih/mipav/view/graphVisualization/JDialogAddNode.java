package gov.nih.mipav.view.graphVisualization;

import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.plaf.basic.BasicBorders;

public class JDialogAddNode extends JDialogBase implements ActionListener {

	private static final long serialVersionUID = 3937866689552181956L;

	/**Default notes section text*/
    public static final String DEFAULT_NOTES = "Enter notes for the annotation here.  This field is optional.";

    /** Panel to contain the okay/cancel/help buttons. */
    private JPanel buttonPanel;
	
    /** NameField that will contain the string to be displayed. */
    private JTextField nameField;

    /** NoteField for providing a note about this marker. Note only displayed when clicked. */
    private JTextArea noteField;
    
    private MipavGraphPanel parent;
    private String notes;
    private boolean addNode;
    
    public JDialogAddNode(MipavGraphPanel kParent, String kNotes, boolean bAdd) {
        super(true);
        parent = kParent;
        notes = kNotes;
        addNode = bAdd;
        init("Annotation");
        setVisible(true);
        setResizable(false);
    }

    /**
     * Catches action events: Okay, Cancel, Help and ChooseColor.
     *
     * @param  event  ActionEvent
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
        	if ( parent != null )
        	{
        		String notes = null;
                if(!noteField.getText().equals(DEFAULT_NOTES) && noteField.getText().length() > 0)
                {
                	notes = noteField.getText();
                	System.err.println( notes );
                }
                if ( addNode )
                {
                	parent.addNode(nameField.getText(), notes);
                }
                else
                {
                	parent.editNotes(notes);
                }

        	}
        	windowClosing(null);
        } else if (command.equals("Cancel")) {
            windowClosing(null);
        }
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

        if ( addNode )
        {
        	nameField = new JTextField(25);        
        	namePanel.setBorder(buildTitledBorder("Name"));
        	namePanel.add(nameField);        
        	nameField.setBorder(BasicBorders.getTextFieldBorder());
        	nameField.setText("Enter name here");
        	nameField.setMinimumSize(new Dimension(229, 26));
        	nameField.setPreferredSize(new Dimension(229, 26));
        	nameField.setMaximumSize(new Dimension(229, 26));
        }
        
        JPanel notePanel = new JPanel();
        notePanel.setBorder(buildTitledBorder("Notes Section"));
       
        noteField = new JTextArea(DEFAULT_NOTES);
        noteField.setRows(4);
        noteField.setEditable(true);
        noteField.setLineWrap(true);
        noteField.setWrapStyleWord(true);
        if ( notes != null )
        {
        	noteField.setText(notes);
        }
        else {
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
		c.gridy = 1;
		c.gridwidth = 1;
		if ( addNode )
		{
			centerPanel.add(namePanel, c);
		}
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 2;
        c.gridwidth = 2;
        centerPanel.add(notePanel, c);

        mainDialogPanel.add(centerPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        if ( addNode )
        {
        	nameField.requestFocus();
        	nameField.selectAll();
        }
        else
        {
        	noteField.requestFocus();
        	noteField.selectAll();
        }
    }

}
