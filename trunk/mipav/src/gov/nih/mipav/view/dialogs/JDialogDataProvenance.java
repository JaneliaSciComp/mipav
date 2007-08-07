package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.provenance.ProvenanceHolder;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;


/**
 * Simple dialog used to show the image or system data provenance (by passing in a provenanceholder
 * Displays data in table format, and the currently selected item will show up in the JTextArea (not editable, but selectable)
 *
 */
public class JDialogDataProvenance extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3861014709972568409L;

    /** Column names for data provenance*/
    public static final String [] dpColumnNames = new String[] {"Time","Action","JVM","Mipav","User"};

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Describes the initial size of the textual display area when the dialog is created. The value is given in pixel
     * size rather than the number of characters since the display area has no characters to display.
     */
    protected final Dimension DEFAULT_SIZE = new Dimension(650, 400);


    /** DOCUMENT ME! */
    private JPanel buttonPanel;

    /** DOCUMENT ME! */
    private JScrollPane scrollPane; // here so we can set scroll correct

    /** DOCUMENT ME! */
    private JTextArea textArea;

    private ProvenanceHolder pHolder;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor for displaying data provenance (image or system)
     */
    public JDialogDataProvenance(Frame parent, String title, boolean followScroll, ProvenanceHolder ph) {
        super(parent, false);
        this.pHolder = ph;
        if (followScroll) {
            setResizable(true);
            init(title);
            scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
        } else {
            setResizable(true);
            init(title);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the "Close" button is pressed.
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == cancelButton) {
            dispose();
        }
    }
    
    /**
     * Accessor to the <code>JPanel</code> which holds the buttons at the bottom of the dialog.
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel getButtonPanel() {
        return buttonPanel;
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     *
     * @param  title  Title of the dialog box.
     */
    protected void init(String title) {
        JPanel scrollPanel;

        Box scrollingBox = new Box(BoxLayout.Y_AXIS);
        
        setTitle(title);

        ViewTableModel dpModel = new ViewTableModel();
        JTable dpTable = new JTable(dpModel);

        for (int i = 0; i < dpColumnNames.length; i++) {
            dpModel.addColumn(dpColumnNames[i]);
        }
        dpTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        dpTable.getColumn("Time").setMinWidth(60);
        dpTable.getColumn("Time").setMaxWidth(200);
    //    dpTable.getColumn("Action").setMinWidth(100);
        dpTable.getColumn("JVM").setMinWidth(60);
        dpTable.getColumn("JVM").setMaxWidth(60);
        dpTable.getColumn("Mipav").setMinWidth(40);
        dpTable.getColumn("Mipav").setMaxWidth(40);
        dpTable.getColumn("User").setMinWidth(80);
        dpTable.getColumn("User").setMaxWidth(100);

        dpTable.getTableHeader().setReorderingAllowed(false);
        
               
        int size = pHolder.size();
        String rose [] = null;
        for (int i = 0; i < size; i++) {
        	rose = new String[dpColumnNames.length];

        	rose[0] = Long.toString(pHolder.elementAt(i).getTimeStamp());
        	rose[1] = pHolder.elementAt(i).getAction();
        	rose[2] = pHolder.elementAt(i).getJavaVersion();
        	rose[3] = pHolder.elementAt(i).getMipavVersion();
        	rose[4] = pHolder.elementAt(i).getUser();        	
            dpModel.addRow(rose);
        }
        
        textArea = new JTextArea();
        textArea.setBackground(Color.white);
        textArea.setEditable(false);
        textArea.setBorder(this.buildTitledBorder("Currently selected value"));
        
        scrollingBox.add(dpTable.getTableHeader());
        scrollingBox.add(dpTable);
        scrollingBox.add(textArea);
        
        SelectionListener listener = new SelectionListener(dpTable, textArea);
        dpTable.getSelectionModel().addListSelectionListener(listener);
        dpTable.getColumnModel().getSelectionModel().addListSelectionListener(listener);
        
        scrollPane = new JScrollPane(scrollingBox, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPanel = new JPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(scrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(10, 15, 10, 15));

        buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(cancelButton);
        getContentPane().add(scrollPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setSize(DEFAULT_SIZE);
        setVisible(true);
    }

    public static class SelectionListener implements ListSelectionListener {
        JTable table;
        JTextArea textArea;
        
        SelectionListener(JTable table, JTextArea textArea) {
            this.table = table;
            this.textArea = textArea;
        }
        public void valueChanged(ListSelectionEvent e) {	
        	 if (e.getValueIsAdjusting()) {
                 // The mouse button has not yet been released
        		 return;
             }
        	 textArea.setText((String)table.getModel().getValueAt(table.getSelectedRow(), table.getSelectedColumn()));
        }
    }
}
