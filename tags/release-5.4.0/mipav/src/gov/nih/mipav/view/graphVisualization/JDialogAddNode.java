package gov.nih.mipav.view.graphVisualization;

import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.graphVisualization.MipavGraphPanel.PropertiesDialog;

import hypergraph.graphApi.Node;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.plaf.basic.BasicBorders;

/** Dialog used by the MipavGraphPanel to add a node to the graph, or to modify the Notes 
 * attribute of a node.
 * */
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
    
    /** Parent graph panel to update on 'OK' */
    private MipavGraphPanel parent;
    /** Current notes for the node. */
    private String notes;
    /** When true the dialog creates a new node, otherwise it is used to edit the Notes. */
    private boolean addNode;
    
  
    
    
    private Node pickedNode;
    
    public JDialogAddNode(MipavGraphPanel kParent, String kNotes, boolean bAdd) {
        super(true);
        parent = kParent;
        notes = kNotes;
        addNode = bAdd;
        init("Annotation");
        setVisible(true);
        setResizable(false);
    }
    
    
    
    public JDialogAddNode(MipavGraphPanel kParent, Node pickedNode, String kNotes, boolean bAdd, boolean setVisible) {
        super(true);
        parent = kParent;
        notes = kNotes;
        addNode = bAdd;
        this.pickedNode = pickedNode;
        init("Annotation");
        if(setVisible) {
        	setVisible(true);
        	setResizable(false);
        }
    }

    /**
     * Catches action events: Okay, Cancel.     *
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
                }
                if ( addNode )
                {
                	// add a new node to the graph:
                	parent.addNode(nameField.getText().trim(), notes);
                }
                else{
                	// pass in the edited notes field:
                	if ( notes != null ) {
                		parent.editNotes(notes);
                	}
                	if(pickedNode != null) {
                		pickedNode.setLabel(nameField.getText().trim());
                	}
                }

        	}
        	parent.repaint();
        	windowClosing(null);
        } else if (command.equals("Cancel")) {
        	if(parent.getPropertiesDialog() != null) {
        		parent.getPropertiesDialog() .dispose();
        	
			
			}
         
        }
    }
	
	
	

    public synchronized JTextField getNameField() {
		return nameField;
	}



	public synchronized JTextArea getNoteField() {
		return noteField;
	}



	public synchronized String getNotes() {
		return notes;
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
        OKButton.addActionListener(parent);
        OKButton.setActionCommand("notesNode");

        //if ( addNode )
        //{
        	// create a text field for the new name:
        	nameField = new JTextField(45);        
        	namePanel.setBorder(buildTitledBorder("Name"));
        	namePanel.add(nameField);        
        	nameField.setBorder(BasicBorders.getTextFieldBorder());
        	if(!addNode && pickedNode != null) {
        		nameField.setText(pickedNode.getLabel());
        	}
        	nameField.setMinimumSize(new Dimension(229, 26));
        	nameField.setPreferredSize(new Dimension(229, 26));
        	nameField.setMaximumSize(new Dimension(229, 26));
        //}
        
        JPanel notePanel = new JPanel();
        notePanel.setBorder(buildTitledBorder("Notes Section"));
       
        noteField = new JTextArea();
        noteField.setRows(4);
        noteField.setEditable(true);
        noteField.setLineWrap(true);
        noteField.setWrapStyleWord(true);
        if ( notes != null )
        {
        	// set the notes field whatever the existing Notes were for the node:
        	noteField.setText(notes);
        }
        else {
        	//noteField.setText(DEFAULT_NOTES);
        }
        JScrollPane containerPane = new JScrollPane(noteField);
        containerPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        containerPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        containerPane.setMinimumSize(new Dimension(505, 193));
        containerPane.setMaximumSize(new Dimension(505, 193));
        containerPane.setPreferredSize(new Dimension(505, 193));
        containerPane.setBorder(BasicBorders.getTextFieldBorder());

        notePanel.add(containerPane);        

        mainDialogPanel.setLayout(layout);

        JPanel centerPanel = new JPanel(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 1;
		c.gridwidth = 1;
		//if ( addNode )
		//{
			centerPanel.add(namePanel, c);
		//}
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
