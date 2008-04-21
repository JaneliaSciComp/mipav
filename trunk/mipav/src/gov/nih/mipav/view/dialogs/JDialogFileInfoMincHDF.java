package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
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


public class JDialogFileInfoMincHDF extends JDialogScriptableBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private FileInfoMincHDF mincInfo;

    /** DOCUMENT ME! */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private JScrollPane scrollPaneDicom;

    /** DOCUMENT ME! */
    private ViewTableModel tagsModel;

    /** DOCUMENT ME! */
    private JTable tagsTable;

    /** DOCUMENT ME! */
    private boolean isAppend = false;

    /** fileName of where dicom tags are save to **/
    private String fileName;

    /** directory of where dicom tags are save to **/
    private String directory;

    /** slice index for which fileInfo is saved **/
    private int sliceIndex;

    private boolean launchFileChooser = true;

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
     * Shows the "Other Image Information", with or without private tags.
     *
     * @param  tagsModel  DOCUMENT ME!
     * @param  DicomInfo  DOCUMENT ME!
     * @param  show       boolean that indicates whether or not to show private tags
     */
    public void showDicomSection(ViewTableModel tagsModel, FileInfoMincHDF mincInfo) {
	Enumeration<FileDicomKey> e;
	String name;
	FileDicomKey key;
	Object[] rowData = { "", "", "" };
	Hashtable<FileDicomKey,FileDicomTag> tagsList = mincInfo.getDicomTable();
	
	if (tagsList.size() > 0) {
	    tagsModel.addRow(new String[]{"", "DICOM information", ""});
	}

	// go through the hashlist, and for each element you find, copy it
	// into the table, showing full info if it was coded
	int ii;

	for (ii = 0, e = tagsList.keys(); e.hasMoreElements(); ii++) {
	    key = (FileDicomKey) e.nextElement();
	    name = key.getKey();                       

	    String tagName = "(" + name + ")";

	    rowData[0] = tagName;
	    rowData[1] =  DicomDictionary.getName(key);
	    rowData[2] =  ((FileDicomTag) tagsList.get(key)).getValue(true);

	    tagsModel.addRow(rowData);
	}
	sort(tagsModel, 0, false, true);
    }

    /**
     * Recursively parse and display (to JDialogText) the nodes
     *  @throws Exception
     */
    private static int displayNodes(DefaultMutableTreeNode rNode, ViewTableModel model, int index) throws Exception {
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
		if (nodeName.startsWith("dicom_")) {
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
			if (!name.equals("varid") && !name.equals("vartype") &&
				!name.equals("version")) {
			    rowData[1] = name;

			    val = "";
			    Object value = currentAttribute.getValue();
			    for (int j = 0; j < dataDims.length; j++) {
				//System.err.print(", " + dataDims[i]);

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
			    index++;
			}
		    }
		}
	    } else {
		//do nothing, should be leaf
	    }
	} 

	return index;
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
	sliceIndex = sIndex;

	Object[] rowData = {  "", "", "" };
	String[] columnNames = {"Tag", "Name", "Value" };

	try {
	    tagsModel = new ViewTableModel();
	    tagsTable = new JTable(tagsModel);

	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoDICOM reports: Out of memory!");

	    return;
	} catch (IllegalArgumentException ex) {
	    MipavUtil.displayError("JDialogFileInfoDICOM reports: Editing table too small!");

	    return;
	}

	int[] extents;
	int i;

	for (i = 0; i < 3; i++) {
	    tagsModel.addColumn(columnNames[i]);
	}

	tagsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
	//tagsTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

	tagsTable.getColumn("Tag").setMinWidth(90);
	tagsTable.getColumn("Tag").setMaxWidth(90);
	tagsTable.getColumn("Name").setMinWidth(160);
	tagsTable.getColumn("Name").setMaxWidth(500);
	tagsTable.getColumn("Value").setMinWidth(50);
	tagsTable.getColumn("Value").setMaxWidth(1000);

	tagsModel.addRow(rowData);
	tagsModel.addRow(rowData);
	tagsModel.setValueAt("Essential Image Information", 0, 1);
	tagsModel.setValueAt(null, 0, 0);
	tagsModel.setValueAt(null, 1, 0);
	i = 2;
	extents = mincInfo.getExtents();

	for (int j = 0; j < extents.length; j++) {
	    tagsModel.addRow(rowData);
	    tagsModel.setValueAt("Dimension", i, 1);
	    tagsModel.setValueAt(new Integer(extents[j]), i, 2);
	    i++;
	}

	int dataType = mincInfo.getDataType();

	tagsModel.addRow(rowData);
	tagsModel.setValueAt("Type", i, 2);
	tagsModel.setValueAt(ModelStorageBase.getBufferTypeStr(dataType), i, 2);

	i++;
	tagsModel.addRow(rowData);
	tagsModel.setValueAt("Min", i, 2);
	tagsModel.setValueAt(new Double(mincInfo.getMin()), i, 2);
	tagsModel.addRow(rowData);
	tagsModel.setValueAt("Max", ++i, 2);
	tagsModel.setValueAt(new Double(mincInfo.getMax()), i, 2);
	tagsModel.addRow(rowData);
	tagsModel.setValueAt("Orientation", ++i, 1);

	switch (mincInfo.getImageOrientation()) {

	case FileInfoBase.AXIAL:
	    tagsModel.setValueAt("Axial", i, 2);
	    break;

	case FileInfoBase.CORONAL:
	    tagsModel.setValueAt("Coronal", i, 2);
	    break;

	case FileInfoBase.SAGITTAL:
	    tagsModel.setValueAt("Sagittal", i, 2);
	    break;

	default:
	    tagsModel.setValueAt("Unknown", i, 2);
	}

	float[] resolutions;

	resolutions = mincInfo.getResolutions();
	i++;

	for (int j = 0; j < extents.length; j++) {
	    tagsModel.addRow(rowData);
	    tagsModel.setValueAt("Pixel resolution " + j, i, 1);
	    tagsModel.setValueAt(new Float(resolutions[j]), i, 2);
	    i++;
	}

	int measure = mincInfo.getUnitsOfMeasure(0);

	tagsModel.addRow(rowData);
	tagsModel.setValueAt("Unit of measure", i, 1);
	tagsModel.setValueAt(null, i, 0);

	if (measure == FileInfoBase.INCHES) {
	    tagsModel.setValueAt("Inches per pixel", i, 2);
	} else if (measure == FileInfoBase.MILLIMETERS) {
	    tagsModel.setValueAt("Millimeters per pixel", i, 2);
	} else if (measure == FileInfoBase.CENTIMETERS) {
	    tagsModel.setValueAt("Centimeters per pixel", i, 2);
	} else if (measure == FileInfoBase.METERS) {
	    tagsModel.setValueAt("Meters per pixel", i, 2);
	} else if (measure == FileInfoBase.KILOMETERS) {
	    tagsModel.setValueAt("Kilometers per pixel", i, 2);
	} else if (measure == FileInfoBase.MILES) {
	    tagsModel.setValueAt("Miles per pixel", i, 2);
	} else {
	    tagsModel.setValueAt("Unknown", i, 2);
	}

	i++;
	tagsModel.addRow(rowData);
	tagsModel.setValueAt("Transformation Matrix", i, 1);

	String matrixString = imageA.getMatrix().matrixToString(8, 4);
	int nextIndex = 0, index = 0;
	String subStr = new String();

	for (int ii = 0; ii < imageA.getMatrix().getNRows(); ii++) {
	    i++;
	    nextIndex = matrixString.indexOf("\n", index);

	    if (nextIndex != -1) {
		subStr = matrixString.substring(index, nextIndex);
		index = nextIndex + 1;
		tagsModel.addRow(rowData);
		tagsModel.setValueAt(subStr, i, 2);
	    } else {
		subStr = matrixString.substring(index, matrixString.length());
		tagsModel.addRow(rowData);
		tagsModel.setValueAt(subStr, i, 2);
	    }
	}


	tagsModel.addRow(rowData);
	tagsModel.addRow(rowData);
	i += 2;

	//dimension node display
	if (mincInfo.getDimensionNode() != null) {
	    tagsModel.setValueAt("Dimension information", i, 1); 
	    i++;
	    tagsModel.addRow(rowData);
	    try {
		i = displayNodes(mincInfo.getDimensionNode(), tagsModel, i);
		tagsModel.addRow(rowData);
		tagsModel.addRow(rowData);
		i+=2;
	    } catch (Exception e) {

	    }
	}

	// info node display
	if (mincInfo.getInfoNode() != null) {
	    tagsModel.addRow(rowData);
	    tagsModel.setValueAt("Info node", i, 1);
	    i++;
	    try {
		i = displayNodes(mincInfo.getInfoNode(), tagsModel, i);
		tagsModel.addRow(rowData);
		tagsModel.addRow(rowData);
		i+=2;
	    } catch (Exception e) {

	    }
	}

	showDicomSection(tagsModel, mincInfo);

	try {
	    tagsTable.setPreferredScrollableViewportSize(new Dimension(200, 200));
	    tagsTable.setMinimumSize(new Dimension(300, 300));
	    scrollPaneDicom = new JScrollPane(tagsTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
		    JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	    scrollPaneDicom.setPreferredSize(new Dimension(200, 200));
	    scrollPaneDicom.setMinimumSize(new Dimension(150, 100));

	} catch (OutOfMemoryError error) {
	    MipavUtil.displayError("JDialogFileInfoDICOM reports: Out of memory!");

	    return;
	}

	scrollPaneDicom.setBackground(Color.black);

	getContentPane().add(scrollPaneDicom, BorderLayout.CENTER);
	getContentPane().setSize(new Dimension(700, 650));
	setSize(700, 650);

    }




    /**
     * call algorithm
     *
     */
    protected void callAlgorithm() {
    }



    /**
     * set gui from parameters
     *
     */
    protected void setGUIFromParams() {
	imageA = scriptParameters.retrieveInputImage();
	isAppend = scriptParameters.getParams().getBoolean("isAppend");
	directory = scriptParameters.getParams().getString("directory");
	fileName = scriptParameters.getParams().getString("fileName");
	sliceIndex = scriptParameters.getParams().getInt("sliceIndex");
	launchFileChooser = false;
	FileInfoMincHDF fileInfo;
	if(imageA.getFileInfo().length > sliceIndex) {
	    fileInfo = (FileInfoMincHDF)imageA.getFileInfo()[sliceIndex];
	}else {
	    fileInfo = (FileInfoMincHDF)imageA.getFileInfo()[0];
	}

	displayAboutInfo(imageA,fileInfo,sliceIndex);

    }

    /**
     * store parameters from gui
     * @throws ParserException
     */
    protected void storeParamsFromGUI() throws ParserException {
	scriptParameters.storeInputImage(imageA);
	scriptParameters.getParams().put(ParameterFactory.newParameter("isAppend", isAppend));
	scriptParameters.getParams().put(ParameterFactory.newParameter("directory", directory));
	scriptParameters.getParams().put(ParameterFactory.newParameter("fileName", fileName));
	scriptParameters.getParams().put(ParameterFactory.newParameter("sliceIndex", sliceIndex));

    } 
}


