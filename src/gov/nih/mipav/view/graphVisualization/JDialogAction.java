package gov.nih.mipav.view.graphVisualization;

import gov.nih.mipav.view.dialogs.JDialogBase;
import hypergraph.graphApi.Node;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.plaf.basic.BasicBorders;

public class JDialogAction extends JDialogBase implements ActionListener {
	
	
    /** Panel to contain the okay/cancel/help buttons. */
    private JPanel buttonPanel;
	
	
    /** Parent graph panel to update on 'OK' */
    private MipavGraphPanel parent;
    /** Current notes for the node. */
    private String action;
	
    private Node pickedNode;
    
    /** NoteField for providing a note about this marker. Note only displayed when clicked. */
    private JTextField actionField;
    
   // private JLabel actionLabel;
	
	
	  public JDialogAction(MipavGraphPanel kParent, Node pickedNode, String Action, boolean setVisible) {
	        super(true);
	        parent = kParent;
	        action = Action;
	        this.pickedNode = pickedNode;
	        init("Annotation");
	        if(setVisible) {
	        	setVisible(true);
	        	setResizable(false);
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
	        OKButton.addActionListener(parent);
	        OKButton.setActionCommand("actionNode");


	        JPanel actionPanel = new JPanel(new GridBagLayout());
	        GridBagConstraints gbc = new GridBagConstraints();
	        //actionPanel.setBorder(buildTitledBorder("Action"));
	       
	        //actionField = new JTextField(32);
	        //actionField.setRows(4);
	        //actionField.setEditable(true);
	        //actionField.setLineWrap(true);
	        //actionField.setWrapStyleWord(true);
	        
	        
	        
	        actionField = new JTextField(45);        
        	namePanel.setBorder(buildTitledBorder("Action"));
        	namePanel.add(actionField);        
        	actionField.setBorder(BasicBorders.getTextFieldBorder());
        	
        	
	        
	        //actionLabel = new JLabel("Action ");
	        if ( action != null )
	        {
	        	// set the notes field whatever the existing Notes were for the node:
	        	actionField.setText(action);
	        }
	        else {
	        	//noteField.setText(DEFAULT_NOTES);
	        }
	        /*JScrollPane containerPane = new JScrollPane(actionField);
	        containerPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	        containerPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
	        containerPane.setMinimumSize(new Dimension(505, 193));
	        containerPane.setMaximumSize(new Dimension(505, 193));
	        containerPane.setPreferredSize(new Dimension(505, 193));
	        containerPane.setBorder(BasicBorders.getTextFieldBorder());*/

	      /* gbc.anchor = GridBagConstraints.WEST;
	       gbc.gridx = 0;
	       gbc.gridy = 0;
	       actionPanel.add(actionLabel,gbc);    
	       gbc.gridx = 1;
	       actionPanel.add(actionField,gbc);        */

	        mainDialogPanel.setLayout(layout);

	        JPanel centerPanel = new JPanel(new GridBagLayout());

	       // GridBagConstraints c = new GridBagConstraints();
	       /* c.fill = GridBagConstraints.HORIZONTAL;
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
	        centerPanel.add(actionPanel, c);*/
	        gbc.fill = GridBagConstraints.BOTH;
	        gbc.anchor = GridBagConstraints.NORTHWEST;
		       gbc.gridx = 0;
		       gbc.gridy = 0;
		       centerPanel.add(namePanel,gbc);        

	        mainDialogPanel.add(centerPanel, BorderLayout.CENTER);
	        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

	        getContentPane().add(mainDialogPanel);

	        pack();

	        actionField.requestFocus();
	        actionField.selectAll();
	        
	    }
	
	
	
	
	
	
	
	
	
	
	
	

	public synchronized JTextField getActionField() {
			return actionField;
		}






	@Override
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if (command.equals("Cancel")) {
			if(parent.getPropertiesDialog() != null) {
	    		parent.getPropertiesDialog() .dispose();
			}
		} else {
		    super.actionPerformed(e);
		}
	}

}
