import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.view.*;

import gov.nih.ndar.ws.accession.VToolSimpleAccessionClient;
import gov.nih.ndar.ws.client.Startup;
import info.clearthought.layout.*;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.*;
import java.io.FileNotFoundException;
import java.util.*;

import javax.swing.*;
import javax.swing.table.*;
import javax.xml.namespace.QName;

import org.apache.axiom.om.OMElement;
import org.apache.axis2.AxisFault;


public class PlugInDialogDataEntryTool extends JDialogStandalonePlugin implements ActionListener {

    private static final String CA_TYPE = "Clinical Assessments";
    
	private static final String GEN_TYPE = "Genomics";
	
	public static final String ICON_NAME= "ndar_puzzle_icon.gif";
	
	private static final String ENV = Startup.PROD_NAME;//TODO change for each env
//	private static final String ENV = Startup.DEMO_NAME;

	private JTable table;
	
	private MyTableModel tableModel;
	
	private final List<PlugInGenomicsEntry> allList = new ArrayList<PlugInGenomicsEntry>();
	
	private boolean fromAddTabAction;
	protected static VToolSimpleAccessionClient client;
	
	public PlugInDialogDataEntryTool(){
		final int response = JOptionPane.showConfirmDialog(this, PlugInDialogNDAR.NDAR_PRIVACY_NOTICE,
                "NDAR Genomics and Clinical Assessments Data Entry Tool", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
		this.fromAddTabAction = false;
		if (response == JOptionPane.YES_OPTION){
			init();
		 } else {
        if (JDialogStandalonePlugin.isExitRequired()) {
            System.exit(0);
            // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else {
            return;
        }
    }
	}
	
	public PlugInDialogDataEntryTool(final boolean fromAddTabAction){
		this.fromAddTabAction = fromAddTabAction;
		init();
	}
	
	public void init(){
		if(!fromAddTabAction || client == null){
			PlugInDialogDataEntryTool.testConnection();
			PlugInDialogDataEntryTool.setClient(Startup.getClient(PlugInDialogDataEntryTool.ENV));
		}
		
        setTitle("Please select which Data Structures you would like to add data");
        tableModel = new MyTableModel();
        tableModel.addColumn("");
        tableModel.addColumn("Short Name");
        tableModel.addColumn("Description");
        tableModel.addColumn("Published");
        table = new JTable(tableModel);
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setRowHeight(22);
        table.getColumnModel().getColumn(0).setPreferredWidth(25);
        table.getColumn("Short Name").setMinWidth(150);
        table.getColumn("Description").setMinWidth(350);
        table.getTableHeader().setReorderingAllowed(false);
        final TableRowSorter<MyTableModel> sorter = new TableRowSorter<MyTableModel>(tableModel);
        sorter.toggleSortOrder(1);
        table.setRowSorter(sorter);

        getDataStructures("All");

        final JPanel titlePanel = new JPanel();

		final JLabel sort = new JLabel("Filter By:");
        final JRadioButton caButton = new JRadioButton("Clinical Assessments Data");
        caButton.setActionCommand(PlugInDialogDataEntryTool.CA_TYPE);

        final JRadioButton genButton = new JRadioButton("Genomics Data");
        genButton.setActionCommand(PlugInDialogDataEntryTool.GEN_TYPE);

        final JRadioButton allButton = new JRadioButton("All");
        allButton.setActionCommand("All");
        allButton.setSelected(true);

        final ButtonGroup group = new ButtonGroup();
        group.add(caButton);
        group.add(genButton);
        group.add(allButton);

        caButton.addActionListener(this);
        genButton.addActionListener(this);
        allButton.addActionListener(this);

        titlePanel.add(sort, "1,1");
        titlePanel.add(caButton, "3,1");
        titlePanel.add(genButton, "5,1");
        titlePanel.add(allButton, "7,1");

        final JPanel buttonPanel = new JPanel();
        final double border = 3;
        final double[][] buttonPanelSize = { {border, 100, border, 100}, {border, TableLayout.FILL, border}};
        buttonPanel.setLayout(new TableLayout(buttonPanelSize));

        final JButton openButton = new JButton("Open");
        openButton.addActionListener(new ActionListener() {
            public void actionPerformed(final ActionEvent e) {
                actionOpen();
            }
        });
        final JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(final ActionEvent e) {
                actionCancel();
            }
        });
        buttonPanel.add(openButton, "1,1");
        buttonPanel.add(cancelButton, "3,1");

        final double border1 = 3;
        final double[][] panelSize = { {TableLayout.FILL},
                {border1, 30, border1, TableLayout.FILL, border1, 50}};
        setLayout(new TableLayout(panelSize));
        final JScrollPane scrollPane = new JScrollPane(table, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        add(titlePanel, "0,1,L,C");
        add(scrollPane, "0,3");
        add(buttonPanel, "0,5,c,c");

        try {
            setIconImage(MipavUtil.getIconImage(ICON_NAME));
        } catch (final Exception e) {

        }

        setSize(new Dimension(900, 500));
        setWindowSettings();
        setVisible(true);
    }

    public void getDataStructures(final String type) {
        OMElement e = null;
        try {
        	PlugInDialogDataEntryTool.testConnection();
			e = PlugInDialogDataEntryTool.getClient().getPublishedStructures();
		} catch (final AxisFault e1) {
            e1.printStackTrace();
        }
        final Iterator<OMElement> iter = e.getChildElements();
        final QName qName = new QName(e.getNamespace().getNamespaceURI(), "short_name");
        final QName qDesc = new QName(e.getNamespace().getNamespaceURI(), "desc");
        final QName qPublished = new QName(e.getNamespace().getNamespaceURI(), "status");
        final QName qType = new QName(e.getNamespace().getNamespaceURI(), "type");

        while (iter.hasNext()) {
            final OMElement e2 = iter.next();
            if (e2.getLocalName().equalsIgnoreCase("data_structure")) {
                final String shortName = e2.getAttributeValue(qName);
                final String desc = e2.getAttributeValue(qDesc);
                final String pub = e2.getAttributeValue(qPublished);
                final String dsType = e2.getAttributeValue(qType);
                final Object[] o = {shortName, desc, pub};
                // System.out.println("Row: "+o[0]+" "+o[1]+" "+o[2]+" "+dsType);
                if (dsType.equals(type) && !type.equals("All")) {
                    final PlugInGenomicsEntry entry = new PlugInGenomicsEntry(o);
                    tableModel.addRow(entry.rowData);
                    allList.add(entry);
                } else if (type.equals("All")) {
                    final PlugInGenomicsEntry entry = new PlugInGenomicsEntry(o);
                    tableModel.addRow(entry.rowData);
                    allList.add(entry);
                }
            }
        }
    }

    public void actionPerformed(final ActionEvent e) {
        if (e.getActionCommand().equals(PlugInDialogDataEntryTool.CA_TYPE)) {
            System.out.println("Creating Clinical Assessments list");
            clearTable();
            getDataStructures(PlugInDialogDataEntryTool.CA_TYPE);
        } else if (e.getActionCommand().equals(PlugInDialogDataEntryTool.GEN_TYPE)) {
            System.out.println("Creating Genomics list");
            clearTable();
            getDataStructures(PlugInDialogDataEntryTool.GEN_TYPE);

        } else {
            System.out.println("Getting All");
            clearTable();
            getDataStructures("All");
        }

    }

    public void actionOpen() {
        final List<String> selectedList = new ArrayList<String>();
        for (final PlugInGenomicsEntry entry : allList) {
            if (entry.isRowSelected()) {
                selectedList.add((String) entry.rowData[1]);
            }
        }
        System.out.println(selectedList);
        if (selectedList.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Please select a Data Structure", "Error", JOptionPane.ERROR_MESSAGE);
        } else{
			if(fromAddTabAction)
				PlugInGenericDataEntryContainer.addPane(selectedList);
			else
				new PlugInGenericDataEntryContainer(selectedList);
			this.dispose();
        }

    }

    public void actionCancel() {
    	if(fromAddTabAction)
    		this.dispose();
    	else if (JDialogStandalonePlugin.isExitRequired()) {
            windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        }
    }

    public void clearTable() {
        for (int i = tableModel.getRowCount(); i > 0; --i) {
            tableModel.removeRow(i - 1);
        }
        allList.clear();
    }

    class MyTableModel extends DefaultTableModel {
        public Class<?> getColumnClass(final int columnIndex) {
            if (columnIndex == 0) {
                return Boolean.class;
            }
            return String.class;
        }

        public boolean isCellEditable(final int row, final int col) {
            if (col == 0) {
                return true;
            }
            return false;
        }

        @Override
        public void setValueAt(final Object value, final int row, final int col) {
            super.setValueAt(value, row, col);
            allList.get(row).setValueAt(value, col);
        }
	}
	
	public static void testConnection(){
		try {
			PlugInDialogDataEntryTool.getClient().testConnection();
		}catch(NullPointerException e){
			System.err.println("Connection NULL, Creating new client");
			PlugInDialogDataEntryTool.setClient(Startup.getClient(PlugInDialogDataEntryTool.ENV));
		} catch (AxisFault e) {
			System.err.println("Connection Timeout, Creating new client");
			PlugInDialogDataEntryTool.setClient(Startup.getClient(PlugInDialogDataEntryTool.ENV));
		}
	}
	
	public static VToolSimpleAccessionClient getClient() {
        return PlugInDialogDataEntryTool.client;
    }

    public static void setClient(final VToolSimpleAccessionClient client) {
        PlugInDialogDataEntryTool.client = client;
    }
}
