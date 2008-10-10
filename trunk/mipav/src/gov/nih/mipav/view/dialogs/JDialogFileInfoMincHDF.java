package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.HObject;
//import gov.nih.mipav.model.file.HDF.*;


public class JDialogFileInfoMincHDF extends JDialogBase implements ActionListener {
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private FileInfoMincHDF mincInfo;

    /** DOCUMENT ME! */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private JScrollPane scrollPaneDicom;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new dialog with given title and parent, non modal.
     *
     * @param  parent  Parent of the dialog.
     * @param  title   Title of the dialog.
     */

    public JDialogFileInfoMincHDF(Frame parent, String title) {
	super(parent, false);
	setTitle(title);
    }

    /**
     * Default Constructor
     *
     */
    public JDialogFileInfoMincHDF() {}

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Recursively parse and display (to JDialogText) the nodes
     * @throws Exception
     */
    private static void displayNodes(DefaultMutableTreeNode rNode, ViewTableModel model) throws Exception {
	long [] dataDims;
	int children = rNode.getChildCount();
	DefaultMutableTreeNode node;
	Object [] rowData = { "" , "", "" };

	String val;
	for (int i = 0; i < children; i++) {
	    node = (DefaultMutableTreeNode)rNode.getChildAt(i);
	    if (node.isLeaf()) {
		HObject userObject = (HObject)node.getUserObject();

		String nodeName = userObject.toString();
		if (nodeName.startsWith(FileMincHDF.DICOM_GROUP_PREFIX)) {
		    //do nothing, dicom tags displayed elsewhere
		} else {
		    rowData[0] = nodeName;

		    //	dialog.append(userObject + "\n");
		    List<Attribute> metaData = userObject.getMetadata();
		    Iterator<Attribute> it = metaData.iterator();
		    while(it.hasNext()) {
			Attribute currentAttribute = it.next();
			dataDims = currentAttribute.getDataDims();
			String name = currentAttribute.getName();
			if (!name.equals("varid") && !name.equals("vartype") && !name.equals("version")) {
			    rowData[1] = name;

			    val = "";
			    Object value = currentAttribute.getValue();
			    for (int j = 0; j < dataDims[0]; j++) {
				//System.err.println("dataDims:\t" + i + "\t" + dataDims[i]);

				if (j != 0) {
				    val += ", ";
				}
				
				if (value instanceof String[]) {
				    val += ((String[])value)[j];
				    //	dialog.append("\t" + ((String[])value)[i]);
				} else if (value instanceof float[]) {
				    val += ((float[])value)[j];
				} else if (value instanceof double[]) {
				    val += ((double[])value)[j];
				} else if (value instanceof int[]) {
				    val += ((int[])value)[j];
				} else if (value instanceof short[]) {
				    val += ((short[])value)[j];
				}
			    }
			    
			    rowData[2] = val;
			    model.addRow(rowData);
			    rowData[0] = "";
			}
		    }
		}
	    } else {
		//do nothing, should be leaf
	    }
	}
    }

    /**
     * Sort the tag column or name column of the table model. If reverse is true, sorts in reverse order.
     *
     * @param  model         the table model to sort on
     * @param  col           column to sort on
     * @param  reverse       whether or not to sort in reverse order.
     * @param  isInfoDialog  DOCUMENT ME!
     */
    public static void sort(ViewTableModel model, int col, boolean reverse, boolean isInfoDialog) {
	int begin = 1;



	for (int p = begin; p < model.getRowCount(); p++) {

	    for (int j = begin - 1; j < p; j++) {

		if (model.getValueAt(p, col) != null && model.getValueAt(j, col) != null
			&& ((String)model.getValueAt(p, col)).startsWith("(") && 
				((String)model.getValueAt(j, col)).startsWith("(")) {


		    if (model.getValueAt(p, col) != null) {
			if (((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) < 0) {
			    model.moveRow(p, p, j);

			    break;

			}
		    }
		}
	    }
	}
    }

    /**
     * Closes the dialog when the user clicks close and toggles private tags on and off when the user hits the "Show
     * Private" button.
     *
     * <p>Brings up a 'Sanitise dialog'--to remove potentially damaging information, like the patient's name, from the
     * image--when user clicks the "Sanitise Image" button.</p>
     *
     * <p>Creates editor dialogs to allow changing the value-field of a tag when user clicks "Edit Tag" button. This
     * implmentation supports virtually any number of tag editors, bringing forward any previously opened editor. Most
     * processing occurs when this class hears an editor window close; at that point it checks for "all slices" option
     * in the editor and will alert any open window (frame) to set title as that information may have changed.</p>
     *
     * @param  e  event that triggered this action
     */
    public void actionPerformed(ActionEvent e) {

	if (e.getActionCommand().equals("Close")) { // close
	    dispose(); // remove self
	} else {
	    Preferences.debug("eventsource was: " + e.getSource().toString());
	}
    }

    /**
     * This method displays all the valid variables, that is, the ones that are no longer equal to their default values.
     * It parses special types as needed and translates other strings. However, this method does not yet translate every
     * single DICOM tag, only those most used. The others it outputs as strings.
     *
     * @param  _image  The image being displayed.
     * @param  _info   The fileInfo to be displayed, of type FileInfoDicom.
     */
    public void displayAboutInfo(ModelImage _image, FileInfoMincHDF _info, int sIndex) {
	mincInfo = _info; // set the input var
	imageA = _image; // set the input var
	
	Box mainBox;

	try {
	    mainBox = new Box(BoxLayout.Y_AXIS);
	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Out of memory!");

	    return;
	} catch (IllegalArgumentException ex) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Editing table too small!");

	    return;
	}
	
	// essential image info display
	mainBox.add(new JLabel("Essential image information"));
	JTable essentialsTable = makeEssentialImageInfoTable();
	//mainBox.add(essentialsTable.getTableHeader());
	mainBox.add(essentialsTable);

	// dimension node display
	mainBox.add(new JLabel("Dimension node"));
	JTable dimensionTable = makeDimensionNodeTable();
	mainBox.add(dimensionTable.getTableHeader());
	mainBox.add(dimensionTable);
	
	// image node display
	mainBox.add(new JLabel("Image node"));
	JTable imageTable = makeImageNodeTable();
	mainBox.add(imageTable.getTableHeader());
	mainBox.add(imageTable);

	// info node display
	mainBox.add(new JLabel("Information node"));
	JTable informationTable = makeInformationNodeTable();
	mainBox.add(informationTable.getTableHeader());
	mainBox.add(informationTable);

	// dicom tag display
	mainBox.add(new JLabel("Dicom tags stored in Information node"));
	JTable dicomTable = makeDicomNodeTable();
	mainBox.add(dicomTable.getTableHeader());
	mainBox.add(dicomTable);

	try {
	    scrollPaneDicom = new JScrollPane(mainBox, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
		    JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	    scrollPaneDicom.setPreferredSize(new Dimension(200, 200));
	    scrollPaneDicom.setMinimumSize(new Dimension(150, 100));
	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Out of memory!");
	    return;
	}

	scrollPaneDicom.setBackground(Color.black);

	getContentPane().add(scrollPaneDicom, BorderLayout.CENTER);
	getContentPane().setSize(new Dimension(700, 650));
	setSize(700, 650);
    }
    
    private JTable makeEssentialImageInfoTable() {
	ViewTableModel tableModel;
	JTable nodeTable;
	
	try {
	    tableModel = new ViewTableModel();
	    nodeTable = new JTable(tableModel);
	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Out of memory!");
	    error.printStackTrace();
	    return null;
	} catch (IllegalArgumentException ex) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Editing table too small!");
	    ex.printStackTrace();
	    return null;
	}
	
	tableModel.addColumn("Tag");
	tableModel.addColumn("Name");
	tableModel.addColumn("Value");
	
	nodeTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
	nodeTable.setPreferredScrollableViewportSize(new Dimension(200, 200));
	nodeTable.setMinimumSize(new Dimension(300, 300));
	
	nodeTable.getColumn("Tag").setMinWidth(90);
	nodeTable.getColumn("Tag").setMaxWidth(90);
	nodeTable.getColumn("Name").setMinWidth(160);
	nodeTable.getColumn("Name").setMaxWidth(500);
	nodeTable.getColumn("Value").setMinWidth(50);
	nodeTable.getColumn("Value").setMaxWidth(1000);
	
	// info node display
	int[] extents = mincInfo.getExtents();

	for (int i = 0; i < extents.length; i++) {
	    tableModel.addRow(new Object[]{"", "Dimension " + i, new Integer(extents[i])});
	}

	int dataType = mincInfo.getDataType();

	tableModel.addRow(new Object[]{"", "Type", ModelStorageBase.getBufferTypeStr(dataType)});

	tableModel.addRow(new Object[]{"", "Min", new Double(mincInfo.getMin())});
	tableModel.addRow(new Object[]{"", "Max", new Double(mincInfo.getMax())});
	
	tableModel.addRow(new Object[]{"", "Orientation", FileInfoBase.getImageOrientationStr(mincInfo.getImageOrientation())});

	float[] resolutions = mincInfo.getResolutions();
	for (int i = 0; i < extents.length; i++) {
	    tableModel.addRow(new Object[]{"", "Pixel resolution " + i, new Float(resolutions[i])});
	}

	int[] measure = mincInfo.getUnitsOfMeasure();
	for (int i = 0; i < extents.length; i++) {
	    tableModel.addRow(new Object[]{"", "Unit of measure " + i, FileInfoBase.getUnitsOfMeasureStr(measure[i])});
	}
	
	tableModel.addRow(new Object[]{"", "Transformation matrix", });
	
	String matrixString = imageA.getMatrix().matrixToString(8, 4);
	int nextIndex = 0, index = 0;
	String subStr = new String();

	for (int i = 0; i < imageA.getMatrix().getDim(); i++) {
	    nextIndex = matrixString.indexOf("\n", index);

	    if (nextIndex != -1) {
		subStr = matrixString.substring(index, nextIndex);
		index = nextIndex + 1;
		tableModel.addRow(new Object[]{"", "", subStr});
	    } else {
		subStr = matrixString.substring(index, matrixString.length());
		tableModel.addRow(new Object[]{"", "", subStr});
	    }
	}
	
	return nodeTable;
    }
        
    private JTable makeDimensionNodeTable() {
	ViewTableModel tableModel;
	JTable nodeTable;
	
	try {
	    tableModel = new ViewTableModel();
	    nodeTable = new JTable(tableModel);
	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Out of memory!");
	    error.printStackTrace();
	    return null;
	} catch (IllegalArgumentException ex) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Editing table too small!");
	    ex.printStackTrace();
	    return null;
	}
	
	tableModel.addColumn("Variable");
	tableModel.addColumn("Attribute");
	tableModel.addColumn("Value");
	
	nodeTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
	nodeTable.setPreferredScrollableViewportSize(new Dimension(200, 200));
	nodeTable.setMinimumSize(new Dimension(300, 300));
	
	nodeTable.getColumn("Variable").setMinWidth(90);
	nodeTable.getColumn("Variable").setMaxWidth(90);
	nodeTable.getColumn("Attribute").setMinWidth(160);
	nodeTable.getColumn("Attribute").setMaxWidth(500);
	nodeTable.getColumn("Value").setMinWidth(50);
	nodeTable.getColumn("Value").setMaxWidth(1000);
	
	// dimension node display
	if (mincInfo.getDimensionNode() != null) {
	    try {
		displayNodes(mincInfo.getDimensionNode(), tableModel);
	    } catch (Exception e) {
		e.printStackTrace();
		return null;
	    }
	}
	
	return nodeTable;
    }
    
    private JTable makeImageNodeTable() {
	ViewTableModel tableModel;
	JTable nodeTable;
	
	try {
	    tableModel = new ViewTableModel();
	    nodeTable = new JTable(tableModel);
	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Out of memory!");
	    error.printStackTrace();
	    return null;
	} catch (IllegalArgumentException ex) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Editing table too small!");
	    ex.printStackTrace();
	    return null;
	}
	
	tableModel.addColumn("Variable");
	tableModel.addColumn("Attribute");
	tableModel.addColumn("Value");
	
	nodeTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
	nodeTable.setPreferredScrollableViewportSize(new Dimension(200, 200));
	nodeTable.setMinimumSize(new Dimension(300, 300));
	
	nodeTable.getColumn("Variable").setMinWidth(90);
	nodeTable.getColumn("Variable").setMaxWidth(90);
	nodeTable.getColumn("Attribute").setMinWidth(160);
	nodeTable.getColumn("Attribute").setMaxWidth(500);
	nodeTable.getColumn("Value").setMinWidth(50);
	nodeTable.getColumn("Value").setMaxWidth(1000);
	
	// image node display
	if (mincInfo.getInfoNode() != null) {
	    try {
		displayNodes(mincInfo.getImageNode(), tableModel);
	    } catch (Exception e) {
		e.printStackTrace();
		return null;
	    }
	}
	
	return nodeTable;
    }
    
    private JTable makeInformationNodeTable() {
	ViewTableModel tableModel;
	JTable nodeTable;
	
	try {
	    tableModel = new ViewTableModel();
	    nodeTable = new JTable(tableModel);
	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Out of memory!");
	    error.printStackTrace();
	    return null;
	} catch (IllegalArgumentException ex) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Editing table too small!");
	    ex.printStackTrace();
	    return null;
	}
	
	tableModel.addColumn("Variable");
	tableModel.addColumn("Attribute");
	tableModel.addColumn("Value");
	
	nodeTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
	nodeTable.setPreferredScrollableViewportSize(new Dimension(200, 200));
	nodeTable.setMinimumSize(new Dimension(300, 300));
	
	nodeTable.getColumn("Variable").setMinWidth(90);
	nodeTable.getColumn("Variable").setMaxWidth(90);
	nodeTable.getColumn("Attribute").setMinWidth(160);
	nodeTable.getColumn("Attribute").setMaxWidth(500);
	nodeTable.getColumn("Value").setMinWidth(50);
	nodeTable.getColumn("Value").setMaxWidth(1000);
	
	// info node display
	if (mincInfo.getInfoNode() != null) {
	    try {
		displayNodes(mincInfo.getInfoNode(), tableModel);
	    } catch (Exception e) {
		e.printStackTrace();
		return null;
	    }
	}
	
	return nodeTable;
    }
    
    private JTable makeDicomNodeTable() {
	ViewTableModel tableModel;
	JTable nodeTable;
	
	try {
	    tableModel = new ViewTableModel();
	    nodeTable = new JTable(tableModel);
	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Out of memory!");
	    error.printStackTrace();
	    return null;
	} catch (IllegalArgumentException ex) {
	    MipavUtil.displayError("JDialogFileInfoMincHDF reports: Editing table too small!");
	    ex.printStackTrace();
	    return null;
	}
	
	tableModel.addColumn("Tag");
	tableModel.addColumn("Name");
	tableModel.addColumn("Value");
	
	nodeTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
	nodeTable.setPreferredScrollableViewportSize(new Dimension(200, 200));
	nodeTable.setMinimumSize(new Dimension(300, 300));
	
	nodeTable.getColumn("Tag").setMinWidth(90);
	nodeTable.getColumn("Tag").setMaxWidth(90);
	nodeTable.getColumn("Name").setMinWidth(160);
	nodeTable.getColumn("Name").setMaxWidth(500);
	nodeTable.getColumn("Value").setMinWidth(50);
	nodeTable.getColumn("Value").setMaxWidth(1000);
	
	// dicom tag display (technically part of info node)
	FileDicomKey key;
	Hashtable<FileDicomKey,FileDicomTag> dicomTagList = mincInfo.getDicomTable();
	Enumeration<FileDicomKey> keyList = dicomTagList.keys();
	
	if (dicomTagList.size() > 0) {
	    try {
		while (keyList.hasMoreElements()) {
		    key = keyList.nextElement();
		    
		    tableModel.addRow(new Object[]{"(" + key + ")", dicomTagList.get(key).getName(), dicomTagList.get(key).getValue(true)});
		}
		
		sort(tableModel, 0, false, true);
	    } catch (Exception e) {
		e.printStackTrace();
		return null;
	    }
	}
	
	return nodeTable;
    }
}
