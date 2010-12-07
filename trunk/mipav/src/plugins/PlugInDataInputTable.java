

import info.clearthought.layout.TableLayout;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractCellEditor;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableColumn;
import javax.xml.namespace.QName;

import org.apache.axiom.om.OMElement;
import org.apache.axis2.AxisFault;

/**
 * A tab representing one data structure to be held by the DataEntryContainer
 * @author jhunter
 * 
 */
public class PlugInDataInputTable extends JPanel{

	private static final long serialVersionUID = -8613880570320438353L;
	private JTable table;
	private SubjectsTableModel tableModel;
	private List<PlugInGenomicsEntry> allList = new ArrayList<PlugInGenomicsEntry>();
	private JLabel dataStructureTitle;
	private JLabel dataStructureDescTitle;
	private String dataStructure;
	private String dataStructureType;
	private String dataStructureVersion;
	private String dataStructureDesc;
	private String lastDir;
	private Map<String,Boolean> colRequiredMap = new HashMap<String,Boolean>();
	private Map<String,String> colTypeMap = new HashMap<String,String>();
	private Map<String,String> colRangeMap = new HashMap<String,String>();
	private List<PlugInGenomicsEntry> fileRowValues;
	private List<String> fileColNames;
	private List<String> dataElementList;
	private List<String> fileDataStructureInfo;
	
	public PlugInDataInputTable(){
		init("", false);
	}
	/**
	 * 
	 * @param Data Structure name
	 */
	public PlugInDataInputTable(String ds){
		init(ds, false);
	}
	
	/**
	 * 
	 * @param name of data structure
	 * @param cols or data element list
	 * @param entries - data element values 
	 */
	public PlugInDataInputTable(List<String> struct, List<String> cols, List<PlugInGenomicsEntry> entries) {
		this.fileDataStructureInfo=struct;
		this.fileColNames=cols;
		this.fileRowValues=entries;
		init(struct.get(0), true);
	}

	public void init(String targetDataStruct, boolean fromFile){

		dataStructure = targetDataStruct;
		
		if(fromFile&&fileDataStructureInfo.size()>=2){
			System.out.println("Processing From File: "+fileDataStructureInfo);
			dataStructure = fileDataStructureInfo.get(0);
			dataStructureVersion = fileDataStructureInfo.get(1);
			dataElementList = getDataElements(fileDataStructureInfo.get(0)
					+fileDataStructureInfo.get(1));
			boolean complete=true;
			List<String> missingColumns = new ArrayList<String>();
			int i=0;
			for(String col : dataElementList){
				if(!fileColNames.contains(col)){
					missingColumns.add(col);
					complete=false;
					i++;
				}
			}
			if(!complete){
				JOptionPane.showMessageDialog(this, "Warning: Some Data Element columns imported from this" +
						" file are missing or incorrectly named, including:\n"+parseMissedCols(missingColumns), "Warning",
						JOptionPane.WARNING_MESSAGE);
			}

			dataElementList = fileColNames;
		}
		else
			dataElementList = getDataElements(targetDataStruct.toLowerCase().trim());
		
		if(dataStructure.endsWith(dataStructureVersion))
			dataStructure = dataStructure.substring(0, dataStructure.lastIndexOf(dataStructureVersion));
		tableModel = new SubjectsTableModel(dataElementList);
		dataStructureTitle = new JLabel("<html><font style=\"font-weight:bold;\">Data Structure: </font>"+ dataStructure.toUpperCase()
				+"     <font style=\"font-weight:bold;\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Version:</font> "+dataStructureVersion
				+"     <font style=\"font-weight:bold;\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Type: </font>"+dataStructureType+"</html>");
		dataStructureTitle.setFont(new Font("Arial",Font.PLAIN,14));
		dataStructureDescTitle = new JLabel("<html><NOBR><font style=\"font-weight:bold;\">Description: </font>"+dataStructureDesc+"</NOBR></html>");
		dataStructureDescTitle.setFont(new Font("Arial",Font.PLAIN,14));
		JLabel requiredNote = new JLabel("<html>Columns in <font color=\"red\">red</font> are required fields</html>");
		table = new JTable(tableModel);
		
        customizeColumns();
		table.setPreferredScrollableViewportSize(new Dimension(1030, 300));
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setRowHeight(22);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        table.getTableHeader().setReorderingAllowed(false);
        table.addMouseMotionListener(new MouseMotionAdapter(){ 
        	public void mouseMoved(MouseEvent e){
        		if(table.getColumnCount()>0){
	        		Point p = e.getPoint(); 
	        		int column = table.columnAtPoint(p);        			
	        		String col = table.getColumnName(column);
//	        		TableColumn col = table.getColumnModel().getColumn(column);
	        		String type = colTypeMap.get(col);
	        		String range = colRangeMap.get(col);
	        		if(range!=null && !range.equals(""))
	        			table.setToolTipText("Type: "+type+" Value Range: "+range); 
	        		else if(column==0);
	        		else
	        			table.setToolTipText("Type: "+type);
        		}
        	}
        }); 
        
        
        JPanel buttonPanel = new JPanel();
        double border = .07;
        double[][] buttonPanelSize = {{border, 100, border, 130, border, 170, 
        	border,170,border,200,border,200},{border, TableLayout.FILL, border}};
        buttonPanel.setLayout(new TableLayout(buttonPanelSize));
        
        JButton addButton = new JButton("Add Row");
        addButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				actionAdd();
            }
        });
        
        JButton removeButton = new JButton("Remove Row");
        removeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				actionRemove();
            }
        });
        
        JButton uploadButton = new JButton("Load From File");
        uploadButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				actionUpload();
            }
        });
        
//        JButton exportButton = new JButton("Export To XML");
//        exportButton.addActionListener(new ActionListener() {
//            public void actionPerformed(ActionEvent e) {
//				actionExportXml();
//            }
//        });
        
        JButton exportTxtButton = new JButton("Save To TXT/CSV");
        exportTxtButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	actionExportTxt();
            }
        });
        
        JButton newTabButton = new JButton("Add New Data Structure Tab");
        newTabButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	actionAddTab();
            }
        });
        
        JButton removeTabButton = new JButton("Remove Data Structure Tab");
        removeTabButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	actionRemoveTab();
            }
        });
        
        buttonPanel.add(addButton, "1,1");
        buttonPanel.add(removeButton, "3,1");
        buttonPanel.add(uploadButton, "5,1");
//        buttonPanel.add(exportButton, "7,1");
        buttonPanel.add(exportTxtButton, "7,1");
        buttonPanel.add(newTabButton, "9,1");
        buttonPanel.add(removeTabButton, "11,1");

        JScrollPane scrollPane = new JScrollPane(table);
       
        double border1 = 3;
        double[][] panelSize = {{TableLayout.FILL},
        		{border1,20,border1,20,border1,15,border1,TableLayout.FILL,border1,30}};
        setLayout(new TableLayout(panelSize));

//        add(title, "0,1");
        add(dataStructureTitle, "0,1");
//        add(dataStructureVersionTitle, "0,3");
        add(dataStructureDescTitle, "0,3");
	    add(requiredNote, "0,5");
        add(scrollPane, "0,7");
        add(buttonPanel, "0,9,c,c");
        
        //default 1
        if(fromFile)
        	actionAdd(fileRowValues);
        else
        	actionAdd();
	}

	public List<String> getDataElements(String targetDataStruct){
		System.out.println("In getElements");
		OMElement e = null;
        try {
        	System.out.println("Processing: "+targetDataStruct);
        	PlugInDialogDataEntryTool.testConnection();
			e = PlugInDialogDataEntryTool.getClient().getDataDictionary(targetDataStruct);
		} catch (AxisFault e1) {
			e1.printStackTrace();
		}
//		System.out.println(e.toString());
		List<String> dataElementList = new ArrayList<String>();
		Iterator<OMElement> iter = e.getChildElements();
        QName qDataType = new QName(e.getNamespace().getNamespaceURI(), "type");
        QName qDataStructType = new QName(e.getNamespace().getNamespaceURI(), "type");
        QName qDataStructVersion = new QName(e.getNamespace().getNamespaceURI(), "version");
        QName qDataStructDesc = new QName(e.getNamespace().getNamespaceURI(), "desc");
        QName qName = new QName(e.getNamespace().getNamespaceURI(), "name");
        QName qRequired  = new QName(e.getNamespace().getNamespaceURI(), "required");
        QName qValueRange = new QName(e.getNamespace().getNamespaceURI(), "value_range");
        QName qSize = new QName(e.getNamespace().getNamespaceURI(), "size");

        while(iter.hasNext()){
        	OMElement e2 = iter.next();   
//			System.out.println(e2.toString());
			dataStructureType = e2.getAttributeValue(qDataStructType);
			dataStructureVersion = e2.getAttributeValue(qDataStructVersion);
			dataStructureDesc = e2.getAttributeValue(qDataStructDesc);
        	Iterator<OMElement> iter2 = e2.getChildren();
        	while(iter2.hasNext()){
        		OMElement de = iter2.next();
//        		System.out.println(e2.toString());
        		if (de.getLocalName().equalsIgnoreCase("data_element")){
            		String dataType = de.getAttributeValue(qDataType);
    				String name = de.getAttributeValue(qName);
    				String required = de.getAttributeValue(qRequired);
    				String range = de.getAttributeValue(qValueRange);
    				String size = de.getAttributeValue(qSize);
//    				System.out.println(de.toString());
//    				System.out.println("name: "+ name+" dataType: "+dataType+" required: "+required+" range: "+range);
    				colRequiredMap.put(name,(required.trim().equals("Required"))?true:false);
    				if(dataType.trim().contains("String"))
    					colTypeMap.put(name, dataType+" ("+size+")");
    				else
    					colTypeMap.put(name,dataType);
    				colRangeMap.put(name,range);
    				dataElementList.add(de.getAttributeValue(qName));
            	}
        	}
        }
        return dataElementList;
	}
	
	public void customizeColumns(){
        TableColumn column = null;
		JTableHeader header =  table.getTableHeader();
		for(int i=0;i<table.getColumnCount();i++){
        	if(i==0)
        		table.getColumnModel().getColumn(0).setPreferredWidth(20);
        	else{
        		column = table.getColumnModel().getColumn(i);
        		String value = column.getHeaderValue().toString();
        		FontMetrics fm = header.getFontMetrics(header.getFont());
        		column.setPreferredWidth(fm.stringWidth(value)+15);
            	Boolean req = colRequiredMap.get(table.getColumnName(i));
        		if(req!=null && req){
        			column.setHeaderValue("<html><font color=\"red\">"+column.getHeaderValue()+"</font></html>");
	        	}
        	}
        }
	}
	
	public String parseMissedCols(List<String> missing){
		StringBuffer ret = new StringBuffer();
		for(String col : missing){
			ret.append("<html>&#149; "+col+"</html>\n");
		}
		return ret.toString();
	}
	
	public void actionAdd(){
		PlugInGenomicsEntry entry = new PlugInGenomicsEntry(new Object[tableModel.getColumnCount()]);
		tableModel.addRow(entry.rowData);
		allList.add(entry);
	}

	private void actionAdd(List<PlugInGenomicsEntry> entries) {
		for(PlugInGenomicsEntry entry : entries){
			tableModel.addRow(entry.rowData);
			allList.add(entry);
		}
	}
	
	public void actionRemove(){
		List<Integer> selectedRows = new ArrayList<Integer>();
		int indx = 0;
		for(PlugInGenomicsEntry entry : allList) {
			if (entry.isRowSelected()) {
				selectedRows.add(indx);
			}
			indx++;
		}
		System.out.println(selectedRows+" #rows: "+tableModel.getRowCount()+" "+allList.size());
		for(int i=selectedRows.size()-1; i>=0; i--){
			int r = selectedRows.get(i);
			tableModel.removeRow(r);
			allList.remove(r);
		}
	}
	
	public void actionUpload(){
		JFileChooser chooser = new JFileChooser((lastDir!=null)?lastDir:System.getProperty("user.home"));
    	chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    	chooser.setMultiSelectionEnabled(false);
    	chooser.removeChoosableFileFilter(chooser.getAcceptAllFileFilter());
    	int returnVal = chooser.showOpenDialog(getParent());
	    if(returnVal == JFileChooser.APPROVE_OPTION) {
	    	File file = chooser.getSelectedFile();
	    	lastDir = file.getPath();
	    	if(file.getName().endsWith(".txt") 
	    			|| file.getName().endsWith(".csv")){
	    		PlugInFileParser txt = new PlugInFileParser();
	    		List<PlugInGenomicsEntry> entries = txt.parseValues(file);
	    		List<String> dataStructInfo = txt.parseDataStructInfo(file);
	    		List<String> colList = txt.parseColumns(file);
	    		System.out.println(dataStructInfo);
	    		//check for empty file
	    		if(dataStructInfo !=null && dataStructInfo.size()>1 && colList != null){
	    			if(dataStructInfo.get(0).equalsIgnoreCase(dataStructure)){
		    			clearTable();
		    			List<String> colCheck = getDataElements((dataStructure+dataStructureVersion).toLowerCase().trim());
		    			dataStructure = dataStructInfo.get(0).toUpperCase();
		    			dataStructureVersion = dataStructInfo.get(1).toUpperCase();
		    			dataStructureTitle = new JLabel("<html><font style=\"font-weight:bold;\">Data Structure: </font>"+ dataStructure.toUpperCase()
		    					+"     <font style=\"font-weight:bold;\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Version:</font> "+dataStructureVersion
		    					+"     <font style=\"font-weight:bold;\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Type: </font>"+dataStructureType+"</html>");
		    			dataStructureTitle.setFont(new Font("Arial",Font.PLAIN,14));
		    			dataStructureDescTitle = new JLabel("<html><NOBR><font style=\"font-weight:bold;\">Description: </font>"+dataStructureDesc+"</NOBR></html>");
		    			dataStructureDescTitle.setFont(new Font("Arial",Font.PLAIN,14));
			    		tableModel.addColumn("");
//			    		System.out.println(colCheck.toString());
			    		boolean complete=true;
			    		List<String> missingColumns = new ArrayList<String>();
			    		for(String col : colList){
			    			if(!colCheck.contains(col)){
			    				complete=false;
			    				missingColumns.add(col);
			    			}
			    			tableModel.addColumn(col);
			    		}
			    		
		    			for(int i=0;i<entries.size();i++){
		//	    			System.out.println("Adding Entry: "+i+" "+entries.get(i).rowData);
			    			PlugInGenomicsEntry entry = new PlugInGenomicsEntry(new Object[entries.get(i).rowData.length]);
			    			entry.rowData = entries.get(i).rowData;
			    			tableModel.addRow(entry.rowData);
			    			allList.add(entry);
		    			}
		    			customizeColumns();
		    			if(!complete){
		    				JOptionPane.showMessageDialog(this, "Warning: Some Data Element columns imported from this" +
		    						" file are missing or incorrectly named, including:\n"+parseMissedCols(missingColumns), "Warning",
		    						JOptionPane.WARNING_MESSAGE);
		    			}
	    			}else{
	    				int response = JOptionPane.showConfirmDialog(this, "The Data Structure in this file does not match " +
	    						"the currently opened tab. Load this data in a new tab?",
	    		                "Open File", JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
	    				if(response==JOptionPane.YES_OPTION){
	    					PlugInGenericDataEntryContainer.addPane(dataStructInfo, colList, entries);
	    				}
	    			}
	    		}else
	    			JOptionPane.showMessageDialog(this, "NDAR was unable to process your file, check data and try again", "Error",
							JOptionPane.ERROR_MESSAGE);
	    	}else
	    		JOptionPane.showMessageDialog(this, "NDAR was unable to process your file. Please upload a tab delimited TXT file", "Error",
						JOptionPane.ERROR_MESSAGE);
	    }
	}
	
	public void actionExportXml(){
		JFileChooser chooser = new JFileChooser((lastDir!=null)?lastDir:System.getProperty("user.home"));
    	chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    	chooser.setMultiSelectionEnabled(false);
    	chooser.setApproveButtonText("Save");
    	chooser.removeChoosableFileFilter(chooser.getAcceptAllFileFilter());
    	chooser.setSelectedFile(new File(dataStructure+".xml"));
    	if(chooser.showOpenDialog(getParent()) == JFileChooser.APPROVE_OPTION){
			PlugInFileParser xml = new PlugInFileParser();
	    	File file = chooser.getSelectedFile();
			xml.exportXml(dataStructure,dataStructureType,dataStructureVersion,
					file, dataElementList, allList);
    	}
	}
	
	public void actionExportTxt(){
		JFileChooser chooser = new JFileChooser((lastDir!=null)?lastDir:System.getProperty("user.home"));
    	chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    	chooser.setMultiSelectionEnabled(false);
    	chooser.setApproveButtonText("Save");
    	chooser.removeChoosableFileFilter(chooser.getAcceptAllFileFilter());
    	chooser.setSelectedFile(new File(dataStructure+dataStructureVersion+".txt"));
    	if(chooser.showOpenDialog(getParent()) == JFileChooser.APPROVE_OPTION){
			try{
	    		PlugInFileParser xml = new PlugInFileParser();
		    	File file = chooser.getSelectedFile();
		    	if(file.exists()){
		    		int response= JOptionPane.showConfirmDialog(this, "A file with the name you specified already exists. Overwrite file?",
		                    "NDAR Image Submission Package Creation Tool", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
		    		if(response==JOptionPane.YES_OPTION)
		    			xml.exportTxt(dataStructure,dataStructureType,dataStructureVersion,
		    					file, dataElementList, allList);
		    		else
		    			actionExportTxt();
		    	}
		    	else
		    		xml.exportTxt(dataStructure,dataStructureType,dataStructureVersion,
							file, dataElementList, allList);
		    	System.out.println("File wrote to successfully");
			}catch(Exception e){
				e.printStackTrace();
				JOptionPane.showMessageDialog(this, "NDAR was unable to export your file, check data and try again", "Error",
						JOptionPane.ERROR_MESSAGE);
			}
    	}
	}
	
	public void actionAddTab(){
		new PlugInDialogDataEntryTool(true);
	}
	
	public void actionRemoveTab(){
		Object[] options = new Object[]{"Yes","No","Save To TXT/CSV"};
		int response= JOptionPane.showOptionDialog(this, "Are you sure you want to remove this tab? All data entered into table will be lost.",
                "NDAR Image Submission Package Creation Tool", JOptionPane.YES_NO_OPTION,
                JOptionPane.QUESTION_MESSAGE, null,options , null);
		if(response==JOptionPane.YES_OPTION)
			PlugInGenericDataEntryContainer.removePane(this);
		else if(response==2){
			actionExportTxt();
			PlugInGenericDataEntryContainer.removePane(this);
		}
			
	}
	
	public void clearTable(){
		for(int i=tableModel.getRowCount();i>0;--i)
			tableModel.removeRow(i-1);
    	tableModel.setColumnCount(0);
    	allList.clear();
    	colRequiredMap.clear();
    	colTypeMap.clear();
    }

	class SubjectsTableModel extends DefaultTableModel {
		
		public SubjectsTableModel(List<String> tableList){
//			System.out.println(tableList.toString());
			this.addColumn("");
			for(int i=0;i<tableList.size();i++){
				this.addColumn(tableList.get(i));
			}
		}
		
		@Override
		public void setValueAt(Object value, int row, int col) {
			super.setValueAt(value, row, col);
			allList.get(row).setValueAt(value, col);
		}
		
        public boolean isCellEditable(int row, int col) {
        	return true;
        }
        
        public Class<?> getColumnClass(int columnIndex) {
        	if(columnIndex==0)
        		return Boolean.class;
        	String range = colRangeMap.get(table.getColumnName(columnIndex)); 
    		String type = colTypeMap.get(table.getColumnName(columnIndex));
        	TableColumn col = table.getColumnModel().getColumn(columnIndex);
        	if(range!=null && type!=null){
	        	if(!range.equals("")){
	        		String[] split = range.split(";");
	        		JComboBox comboBox = new JComboBox(split);
	        		if(!type.contains("Date")&&split.length>1){
	            		col.setCellEditor(new DefaultCellEditor(comboBox));
	        			return comboBox.getClass();
	        		}
	        	}
	    		if(type.contains("Integer"))
	    			return Integer.class;
	    		else if(type.contains("File") || type.contains("Thumbnail")){
	    			col.setCellEditor(new MyFileChooserCellRenderer());
	    			return JFileChooser.class;
	    		}
	    		else 
	    			return String.class;
        	}
        	return String.class;
    	}
	}
	
	class MyFileChooserCellRenderer extends AbstractCellEditor implements TableCellEditor,ActionListener {

		String filePath;
		JFileChooser chooser;
		JButton button;
		protected static final String EDIT = "edit";
		
		public MyFileChooserCellRenderer(){
			button = new JButton("Browse...");
            button.setActionCommand(EDIT);
            button.addActionListener(this);
            button.setBorderPainted(false);
			chooser = new JFileChooser((lastDir!=null)?lastDir:System.getProperty("user.home"));
		}
		
		 public void actionPerformed(ActionEvent e) {
			 int returnVal = chooser.showOpenDialog(PlugInDataInputTable.this);
             if(returnVal == JFileChooser.APPROVE_OPTION)
                 filePath = chooser.getSelectedFile().getName();
             fireEditingStopped();
		 }
		
		@Override
		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {
			filePath = (String)value;
			return button;
		}

		@Override
		public Object getCellEditorValue() {
			return filePath.trim();
		}
	}
}
