import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.ndar.ws.accession.VToolSimpleAccessionClient;
import gov.nih.ndar.ws.client.Startup;
import info.clearthought.layout.TableLayout;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableRowSorter;
import javax.xml.namespace.QName;

import org.apache.axiom.om.OMElement;
import org.apache.axis2.AxisFault;

public class PlugInDataEntryTool extends JFrame implements PlugInGeneric,ActionListener  {

	private static final String CA_TYPE = "Clinical Assessments";
	private static final String GEN_TYPE = "Genomics";
	private JTable table;
	private MyTableModel tableModel;
	private List<PlugInGenomicsEntry> allList = new ArrayList<PlugInGenomicsEntry>();
	protected static VToolSimpleAccessionClient client;
	
	public PlugInDataEntryTool(){
		final int response = JOptionPane.showConfirmDialog(this, PlugInDialogNDAR.NDAR_PRIVACY_NOTICE,
                "NDAR Genomics and Clinical Assessments Data Entry Tool", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
		if (response == JOptionPane.YES_OPTION)
			init();
	}
	
	public void init(){
		setClient(Startup.getClient(Startup.PROD_NAME));
		
        setTitle("Please select which Data Structures you would like to add data");
        tableModel = new MyTableModel();
        tableModel.addColumn("");
        tableModel.addColumn("Short Name");
        tableModel.addColumn("Description");
        tableModel.addColumn("Published");
        table = new JTable(tableModel);
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setRowHeight(22);
        table.getColumnModel().getColumn(0).setPreferredWidth(20);
        table.getColumn("Short Name").setMinWidth(150);
        table.getColumn("Description").setMinWidth(300);
        table.getTableHeader().setReorderingAllowed(false);
        TableRowSorter<MyTableModel> sorter = new TableRowSorter<MyTableModel>(tableModel);
        sorter.toggleSortOrder(1);
        table.setRowSorter(sorter);
        
		getDataStructures("All");
        
		JPanel titlePanel = new JPanel();

        JLabel sort = new JLabel("Sort By:");
        JRadioButton caButton = new JRadioButton("Clinical Assessments Data");
        caButton.setActionCommand(CA_TYPE);
        
        JRadioButton genButton = new JRadioButton("Genomics Data");
        genButton.setActionCommand(GEN_TYPE);
        
        JRadioButton allButton = new JRadioButton("All");
        allButton.setActionCommand("All");
        allButton.setSelected(true);
        
        ButtonGroup group = new ButtonGroup();
        group.add(caButton);
        group.add(genButton);
        group.add(allButton);

        caButton.addActionListener(this);
        genButton.addActionListener(this);
        allButton.addActionListener(this);
        
        titlePanel.add(sort,"1,1");
        titlePanel.add(caButton,"3,1");
        titlePanel.add(genButton,"5,1");
        titlePanel.add(allButton,"7,1");
        
        JPanel buttonPanel = new JPanel();
		double border = 3;
        double[][] buttonPanelSize = {{border, 100, border, 100},
        		{border, TableLayout.FILL, border}};
        buttonPanel.setLayout(new TableLayout(buttonPanelSize));
        
        JButton openButton = new JButton("Open");
        openButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				actionOpen();
            }
        });
        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				actionCancel();
            }
        });
        buttonPanel.add(openButton, "1,1");
        buttonPanel.add(cancelButton, "3,1");
        
        double border1 = 3;
        double[][] panelSize = {{TableLayout.FILL},
        		{border1,30,border1,TableLayout.FILL,border1,50}};
        setLayout(new TableLayout(panelSize));
        JScrollPane scrollPane = new JScrollPane(table,ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        add(titlePanel,"0,1,L,C");
        add(scrollPane,"0,3");
        add(buttonPanel,"0,5,c,c");
        
        Icon icon = null;
        try {
            icon = new ImageIcon(MipavUtil.getIconImage(Preferences.getIconName()));
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {

        }

		setSize(new Dimension(900,500));
		setVisible(true);
	}
	
	public void getDataStructures(String type){
		OMElement e = null;
        try {
			e = getClient().getPublishedStructures();
		} catch (AxisFault e1) {
			e1.printStackTrace();
		}
		Iterator<OMElement> iter = e.getChildElements();
        QName qName = new QName(e.getNamespace().getNamespaceURI(), "short_name");
        QName qDesc = new QName(e.getNamespace().getNamespaceURI(), "desc");
        QName qPublished = new QName(e.getNamespace().getNamespaceURI(), "status");
        QName qType = new QName(e.getNamespace().getNamespaceURI(), "type");
        
        while(iter.hasNext()){
        	OMElement e2 = iter.next();        	
        	if (e2.getLocalName().equalsIgnoreCase("data_structure")){
				String shortName = e2.getAttributeValue(qName);
				String desc = e2.getAttributeValue(qDesc);
				String pub = e2.getAttributeValue(qPublished);
				String dsType = e2.getAttributeValue(qType);
				Object[] o = {shortName, desc, pub};
//				System.out.println("Row: "+o[0]+" "+o[1]+" "+o[2]+" "+dsType);
				if(dsType.equals(type) && !type.equals("All")){
					PlugInGenomicsEntry entry = new PlugInGenomicsEntry(o);
					tableModel.addRow(entry.rowData);
					allList.add(entry);			
				} else if (type.equals("All")){
					PlugInGenomicsEntry entry = new PlugInGenomicsEntry(o);
					tableModel.addRow(entry.rowData);
					allList.add(entry);
				}
        	}
        }
	}
	
	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals(CA_TYPE)){
			System.out.println("Creating Clinical Assessments list");
			clearTable();
			getDataStructures(CA_TYPE);
		} else if (e.getActionCommand().equals(GEN_TYPE)){
			System.out.println("Creating Genomics list");
			clearTable();
			getDataStructures(GEN_TYPE);
			
		}else{
			System.out.println("Getting All");
			clearTable();
			getDataStructures("All");
		}
		
	}
	
	public void actionOpen(){
		List<String> selectedList = new ArrayList<String>();
		for(PlugInGenomicsEntry entry : allList){
			if (entry.isRowSelected()) {
				selectedList.add((String)entry.rowData[1]);
			}
		}
		System.out.println(selectedList);
		if(selectedList.isEmpty()){
			JOptionPane.showMessageDialog(this, "Please select a Data Structure", "Error",
					JOptionPane.ERROR_MESSAGE);
		}
		else{
			new PlugInGenericDataEntryContainer(selectedList);
			this.dispose();
		}
		
	}
	
	public void actionCancel(){
		int response = JOptionPane.showConfirmDialog(this, "Are you sure you want to exit?",
                "Exit", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
		if(response==0)
			this.dispose();
	}
	
	public void clearTable(){
		for(int i=tableModel.getRowCount();i>0;--i)
			tableModel.removeRow(i-1);
		allList.clear();
    }
	
	
	class MyTableModel extends DefaultTableModel {
		public Class<?> getColumnClass(int columnIndex) {
			if(columnIndex==0)
				return Boolean.class;
			return String.class;
		}
		
		public boolean isCellEditable(int row, int col) {
			if(col==0)
				return true;
        	return false;
        }
		@Override
		public void setValueAt(Object value, int row, int col) {
			super.setValueAt(value, row, col);
			allList.get(row).setValueAt(value, col);
		}
	}

	public static VToolSimpleAccessionClient getClient() {
		return client;
	}

	public static void setClient(VToolSimpleAccessionClient client) {
		PlugInDataEntryTool.client = client;
	}

	@Override
	public void run() {		
	}
}
