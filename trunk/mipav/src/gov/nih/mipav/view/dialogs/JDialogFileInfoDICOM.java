package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;


/**
 * This class shows the dialog which contains the DICOM header information as contained in the FileInfoDicom class. It
 * merely brings up the dialog and does whatever preparation to make the file-info readable in the table.
 *
 * @author   parsonsd; (mostly) cut and pasted from Neva Cherniavsky's FileInfoDicom
 * @version  0.2
 * @see      FileInfoDicom
 */
public class JDialogFileInfoDICOM extends JDialogScriptableBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7337962386655472087L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private FileInfoDicom DicomInfo;

    /** DOCUMENT ME! */
    private JDialogDICOMTagEditor editorDialogDicom;

    /** DOCUMENT ME! */
    private Vector editorDialogDicomList;

    /** DOCUMENT ME! */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private ListSelectionModel listSelectorDicom;

    /** DOCUMENT ME! */
    private JScrollPane scrollPaneDicom;

    /** DOCUMENT ME! */
    private int selectedRowDicom;

    /** DOCUMENT ME! */
    private boolean showPrivate = Preferences.is(Preferences.PREF_SHOW_PRIVATE_TAGS); // if this class shows private tags (priv)

    /** DOCUMENT ME! */
    private ViewTableModel tagsModel;

    /** DOCUMENT ME! */
    private JTable tagsTable;
    
    /** DOCUMENT ME! */
    private boolean isAppend = false;
    
    /** DOCUMENT ME! */
    private JPanel toolbarPanel;
    
    /** DOCUMENT ME ! */
    private ViewToolBarBuilder toolbarBuilder;
    
    /** buttons for toolbar **/
    private JButton saveCheckedButton, saveCheckedAppendButton, checkAllButton, uncheckAllButton, editTagButton, overlayButton, anonymizeButton;

    /** tool bar **/
    private JToolBar toolBar;
    
    private JToggleButton showPrivateButton, bogusShowPrivateButton;
    
    /** fileName of where dicom tags are save to **/
    private String fileName;
    
    /** directory of where dicom tags are save to **/
    private String directory;
    
    /** slice index for which fileInfo is saved **/
    private int sliceIndex;
    
    private boolean launchFileChooser = true;
    
    /**Maps the group word to a displayed color, private tags are not specified. **/
    private TreeMap<String,Color> groupColorMap;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new dialog with given title and parent, non modal.
     *
     * @param  parent  Parent of the dialog.
     * @param  title   Title of the dialog.
     */

    public JDialogFileInfoDICOM(Frame parent, String title) {
        super(parent, false);
        setTitle(title);
    }
    
    /**
     * Default Constructor
     *
     */
    public JDialogFileInfoDICOM() {
    	
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Shows the "Other Image Information", with or without private tags.
     *
     * @param  tagsModel  DOCUMENT ME!
     * @param  DicomInfo  DOCUMENT ME!
     * @param  show       boolean that indicates whether or not to show private tags
     */
    public static void showTags(ViewTableModel tagsModel, FileInfoDicom DicomInfo, boolean show) {
        Enumeration e;
        String name;
        FileDicomKey key;
        String[] tags = null;
        Object[] rowData = { new Boolean(false), "", "", "" };
        Hashtable tagsList = DicomInfo.getTagTable().getTagList();
        
        //check preferences to see if any dicom tags were selected for saving
        String prefTagsString = Preferences.getProperty(Preferences.SAVE_DICOM_TAGS);
    	if(prefTagsString != null && (!prefTagsString.trim().equals(""))) {
    		tags = prefTagsString.split(";");
    	}
        
        // go through the hashlist, and for each element you find, copy it
        // into the table, showing full info if it was coded
        int ii;
        
        for (ii = 0, e = tagsList.keys(); e.hasMoreElements(); ii++) {
            key = (FileDicomKey) e.nextElement();
            name = key.getKey();

            if (((FileDicomTag) tagsList.get(key)).getValue(true) != null) {
            	String tagName = "(" + name + ")";
            	if(tags != null) {
            		for(int k=0;k<tags.length;k++) {
            			if(tagName.equals(tags[k])) {
            				rowData[0] = new Boolean(true);
            				break;
            			}else {
            				rowData[0] = new Boolean(false);
            			}
            		}
            	}
                rowData[1] = tagName;
                rowData[2] = ((FileDicomTag) tagsList.get(key)).getName();
                
                String vr = ((FileDicomTag) tagsList.get(key)).getValueRepresentation();
                int vm = ((FileDicomTag) tagsList.get(key)).getValueMultiplicity();


                if (rowData[2].equals("Private Tag") || vr.equals("OB")) {

                    // System.out.println("OB/Priv: "+name + ".." +((FileDicomTag)tagsList.get(key)).getValue(true));
                    // if (rowData[1].equals("Private Tag") || vr.equals("OB") || vm > 1) {
                    if (((FileDicomTag) tagsList.get(key)).getValue(false) instanceof Byte[]) {
                        // if (key.equals("0008,0040")) {   System.err.println("IN JdialogFileInfo looking at
                        // 0008,0040");   System.err.println("value: " + ((FileDicomTag)
                        // tagsList.get(key)).getValue(false).toString()); }

                        Byte[] bytesV = (Byte[]) ((FileDicomTag) tagsList.get(key)).getValue(false);
                        byte[] bytesValue = new byte[bytesV.length];

                        if ((bytesValue != null) && (bytesV != null)) {

                            // System.out.println(" length = " + bytesV.length);
                            for (int k = 0; k < bytesV.length; k++) {
                                bytesValue[k] = bytesV[k].byteValue();
                            }

                            if (bytesV.length == 0) {
                                rowData[3] = "";
                            } else if ((bytesValue[0] > 32) && (bytesValue[0] < 127)) {
                                rowData[3] = new String(bytesValue);
                            } else {
                                rowData[3] = convertType(bytesValue, DicomInfo.getEndianess(), vm);
                            }
                        }
                    } else if(vr.equals("SQ")) {
                        FileDicomSQ sq = (FileDicomSQ) ((FileDicomTag) tagsList.get(key)).getValue(true);
                        Vector display = sq.getSequenceDisplay();

                        rowData[3] = "";

                        for (Enumeration f = display.elements(); f.hasMoreElements();) {
                        	if(addRow(rowData, show)) {
	                        	tagsModel.addRow(rowData);
	
	                            StringTokenizer st = new StringTokenizer((String) f.nextElement(), ";;;");
	
	                            rowData[2] = st.nextToken();
	
	                            if (st.hasMoreTokens()) {
	                                rowData[3] = st.nextToken();
	                            }
                        	} else {
                        		f.nextElement();
                        	}
                        }
                	} else {

                        FileDicomTag t;
                        Object[] tagVals;

                        t = (FileDicomTag) tagsList.get(key);
                        tagVals = t.getValueList();

                        String dispString = "";
                        int num = t.getNumberOfValues();

                        if (num == 0) {
                            Preferences.debug("No Multiplicity: " + name + "  " + t.getValue(true) +
                                              ", check that this is not an error.\n");
                        }

                        for (int q = 0; q < num; q++) {

                            if (tagVals[q] != null) {
                            	try {
                            		dispString += tagVals[q].toString();
                            	} catch(NullPointerException e1) {
                            		dispString += "";
                            	}
                            		
                                if ((q + 1) < num) {
                                    dispString += ", ";
                                }
                            }
                        }

                        rowData[3] = dispString;
                    }
                    // // vm = 2 for patient orientation
                    // if (!name.equals("0020,0020")) {
                    // if (vm > 1 && vr == "SS") { // hack Neva fix this!!!
                    // System.out.println("Inside NEVA hack.  Neva, fix this!!!");
                    // System.out.println("Wait.  Check it out.  The code asks if "+
                    // " the string var vr \"==\" SS.  Wonder when the \n" +
                    // " String's hashcode will -ever- be equal to \"SS\"."+
                    // "  Or Maybe Never.  This should be fixed.");
                    // short[] values = (short[])((FileDicomTag)tagsList.get(key)).getValue(false);
                    // for (int k = 0; k < vm; k++) {
                    // rowData[2] = values[k] + " ";
                    // }
                    // }
                    // else {
                    // rowData[2] = ((FileDicomTag)tagsList.get(key)).getValue(true).toString();
                    // }
                    // }
                    // }
                } // special cases which contain coded information:
                else if (vr.equals("PN")) {
                    String s = (String) ((FileDicomTag) tagsList.get(key)).getValue(true);

                    rowData[3] = s.replace('^', ',');
                } else if (name.equals("0008,0060")) {

                    switch (DicomInfo.getModality()) {

                        case 1:
                            rowData[3] = "BIOMAGENETIC_IMAGING";
                            break;

                        case 2:
                            rowData[3] = "COLOR_FLOW_DOPPLER";
                            break;

                        case 3:
                            rowData[3] = "COMPUTED_RADIOGRAPHY";
                            break;

                        case 4:
                            rowData[3] = "COMPUTED_TOMOGRAPHY";
                            break;

                        case 5:
                            rowData[3] = "DUPLEX_DOPPLER";
                            break;

                        case 6:
                            rowData[3] = "DIAPHANOGRAPHY";
                            break;

                        case 7:
                            rowData[3] = "DIGITAL_RADIOGRAPHY";
                            break;

                        case 8:
                            rowData[3] = "ENDOSCOPY";
                            break;

                        case 9:
                            rowData[3] = "GENERAL_MICROSCOPY";
                            break;

                        case 10:
                            rowData[3] = "HARDCODY";
                            break;

                        case 11:
                            rowData[3] = "INTRAORAL_RADIOGRAPHY";
                            break;

                        case 12:
                            rowData[3] = "LASER_SURFACE_SCAN";
                            break;

                        case 13:
                            rowData[3] = "MAGNETIC_RESONANCE_ANGIOGRAPHY";
                            break;

                        case 14:
                            rowData[3] = "MAMMOGRAPHY";
                            break;

                        case 15:
                            rowData[3] = "MAGNETIC_RESONANCE";
                            break;

                        case 16:
                            rowData[3] = "MAGNETIC_RESONANCE_SPECTROSCOPY";
                            break;

                        case 17:
                            rowData[3] = "NUCLEAR_MEDICINE";
                            break;

                        case 18:
                            rowData[3] = "OTHER";
                            break;

                        case 19:
                            rowData[3] = "POSITRON_EMISSION_TOMOGRAPHY";
                            break;

                        case 20:
                            rowData[3] = "PANORAMIC_XRAY";
                            break;

                        case 21:
                            rowData[3] = "RADIO_FLUOROSCOPY";
                            break;

                        case 22:
                            rowData[3] = "RADIOGRAPHIC_IMAGING";
                            break;

                        case 23:
                            rowData[3] = "RADIOTHERAPY_DOSE";
                            break;

                        case 24:
                            rowData[3] = "RADIOTHERAPY_IMAGE";
                            break;

                        case 25:
                            rowData[3] = "RADIOTHERAPY_PLAN";
                            break;

                        case 26:
                            rowData[3] = "RADIOTHERAPY_RECORD";
                            break;

                        case 27:
                            rowData[3] = "RADIOTHERAPY_STRUCTURE_SET";
                            break;

                        case 28:
                            rowData[3] = "SLIDE_MICROSCOPY";
                            break;

                        case 29:
                            rowData[3] = "SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY";
                            break;

                        case 30:
                            rowData[3] = "THERMOGRAPHY";
                            break;

                        case 31:
                            rowData[3] = "ULTRASOUND";
                            break;

                        case 32:
                            rowData[3] = "XRAY_ANGIOGRAPHY";
                            break;

                        case 33:
                            rowData[3] = "EXTERNAL_CAMERA_PHOTOGRAPHY";
                            break;

                        case 34:
                            rowData[3] = "UNKNOWN";
                            break;

                        default:
                            rowData[3] = "UNKNOWN";
                            break;
                    }
                } else if (name.equals("0008,0064")) {
                    String s = ((String) ((FileDicomTag) tagsList.get(key)).getValue(true)).trim();

                    if (s.equals("DV")) {
                        rowData[3] = "Digitized Video";
                    } else if (s.equals("DI")) {
                        rowData[3] = "Digital Interface";
                    } else if (s.equals("DF")) {
                        rowData[3] = "Digitized Film";
                    } else if (s.equals("WSD")) {
                        rowData[3] = "Workstation";
                    }
                } else if (name.equals("0018,5100")) {
                    String s = ((String) ((FileDicomTag) tagsList.get(key)).getValue(true)).trim();

                    if (s.equals("HFP")) {
                        rowData[3] = "Head First-Prone";
                    } else if (s.equals("HFS")) {
                        rowData[3] = "Head First-Supine";
                    } else if (s.equals("HFDR")) {
                        rowData[3] = "Head First-Decubitus Right";
                    } else if (s.equals("HFDL")) {
                        rowData[3] = "Head First-Decubitus Left";
                    } else if (s.equals("FFP")) {
                        rowData[3] = "Feet First-Prone";
                    } else if (s.equals("FFS")) {
                        rowData[3] = "Feet First-Supine";
                    } else if (s.equals("FFDR")) {
                        rowData[3] = "Feet First-Decubitus Right";
                    } else if (s.equals("FFDL")) {
                        rowData[3] = "Feet First-Decubitus Left";
                    } else {
                        rowData[3] = s;
                    }
                } else if (vr.equals("SQ")) {

                    // System.err.println("Key  = " + key);
                    FileDicomSQ sq = (FileDicomSQ) ((FileDicomTag) tagsList.get(key)).getValue(true);
                    Vector display = sq.getSequenceDisplay();

                    rowData[3] = "";

                    for (Enumeration f = display.elements(); f.hasMoreElements();) {
                    	if(addRow(rowData, show)) {
                        	tagsModel.addRow(rowData);

	                        StringTokenizer st = new StringTokenizer((String) f.nextElement(), ";;;");
	
	                        rowData[2] = st.nextToken();
	
	                        if (st.hasMoreTokens()) {
	                            rowData[3] = st.nextToken();
	                        }
                    	} else {
                    		f.nextElement();
                    	}
                    }
                } // standard tag.  add tag.get(key).getValue(true) as-is to the table
                else { // if ( ((FileDicomTag) tagsList.get(key)).getMultiplicity() > 1) {

                    FileDicomTag t;
                    Object[] tagVals;

                    t = (FileDicomTag) tagsList.get(key);
                    tagVals = t.getValueList();

                    String dispString = "";
                    int num = t.getNumberOfValues();

                    if (num == 0) {
                        Preferences.debug("No Multiplicity: " + name + "  " + t.getValue(true) +
                                          ", check that this is not an error.\n");
                    }

                    for (int q = 0; q < num; q++) {
                    	try {
                    		dispString += tagVals[q].toString();
                    	} catch(NullPointerException e1) {
                    		dispString += "";
                    	}
                    		
                        if ((q + 1) < num) {
                            dispString += ", ";
                        }
                    }

                    rowData[3] = dispString;
                }

                //instances where rowData will be a private tag are checked here for showTags
                if(addRow(rowData, show)) {
                	tagsModel.addRow(rowData);
                }
            }
        }

       sort(tagsModel, 1, false, true);
    }
    
    /**
     * Shows the "Other Image Information", with or without private tags.
     *
     * @param  tagsModel  DOCUMENT ME!
     * @param  DicomInfo  DOCUMENT ME!
     * @param  show       boolean that indicates whether or not to show private tags
     */
    public static void showTagsNoCheckbox(ViewTableModel tagsModel, FileInfoDicom DicomInfo, boolean show) {
        Enumeration e;
        String name;
        FileDicomKey key;
        String[] tags = null;
        Object[] rowData = {"", "", "" };
        Hashtable tagsList = DicomInfo.getTagTable().getTagList();
        
        //check preferences to see if any dicom tags were selected for saving
        String prefTagsString = Preferences.getProperty(Preferences.SAVE_DICOM_TAGS);
    	if(prefTagsString != null && (!prefTagsString.trim().equals(""))) {
    		tags = prefTagsString.split(";");
    	}
        
        // go through the hashlist, and for each element you find, copy it
        // into the table, showing full info if it was coded
        int ii;
        
        for (ii = 0, e = tagsList.keys(); e.hasMoreElements(); ii++) {
            key = (FileDicomKey) e.nextElement();
            name = key.getKey();

            if (((FileDicomTag) tagsList.get(key)).getValue(true) != null) {
            	String tagName = "(" + name + ")";
                rowData[0] = tagName;
                rowData[1] = ((FileDicomTag) tagsList.get(key)).getName();
                
                String vr = ((FileDicomTag) tagsList.get(key)).getValueRepresentation();
                int vm = ((FileDicomTag) tagsList.get(key)).getValueMultiplicity();


                if (rowData[1].equals("Private Tag") || vr.equals("OB")) {

                    // System.out.println("OB/Priv: "+name + ".." +((FileDicomTag)tagsList.get(key)).getValue(true));
                    // if (rowData[1].equals("Private Tag") || vr.equals("OB") || vm > 1) {
                    if (((FileDicomTag) tagsList.get(key)).getValue(false) instanceof Byte[]) {
                        // if (key.equals("0008,0040")) {   System.err.println("IN JdialogFileInfo looking at
                        // 0008,0040");   System.err.println("value: " + ((FileDicomTag)
                        // tagsList.get(key)).getValue(false).toString()); }

                        Byte[] bytesV = (Byte[]) ((FileDicomTag) tagsList.get(key)).getValue(false);
                        byte[] bytesValue = new byte[bytesV.length];

                        if ((bytesValue != null) && (bytesV != null)) {

                            // System.out.println(" length = " + bytesV.length);
                            for (int k = 0; k < bytesV.length; k++) {
                                bytesValue[k] = bytesV[k].byteValue();
                            }

                            if (bytesV.length == 0) {
                                rowData[2] = "";
                            } else if ((bytesValue[0] > 32) && (bytesValue[0] < 127)) {
                                rowData[2] = new String(bytesValue);
                            } else {
                                rowData[2] = convertType(bytesValue, DicomInfo.getEndianess(), vm);
                            }
                        }
                    } else if(vr.equals("SQ")) {
                        FileDicomSQ sq = (FileDicomSQ) ((FileDicomTag) tagsList.get(key)).getValue(true);
                        Vector display = sq.getSequenceDisplay();

                        rowData[2] = "";

                        for (Enumeration f = display.elements(); f.hasMoreElements();) {
                        	if(addRow(rowData, show)) {
	                        	tagsModel.addRow(rowData);
	
	                            StringTokenizer st = new StringTokenizer((String) f.nextElement(), ";;;");
	
	                            rowData[1] = st.nextToken();
	
	                            if (st.hasMoreTokens()) {
	                                rowData[2] = st.nextToken();
	                            }
                        	} else {
                        		f.nextElement();
                        	}
                        }
                	} else {

                        FileDicomTag t;
                        Object[] tagVals;

                        t = (FileDicomTag) tagsList.get(key);
                        tagVals = t.getValueList();

                        String dispString = "";
                        int num = t.getNumberOfValues();

                        if (num == 0) {
                            Preferences.debug("No Multiplicity: " + name + "  " + t.getValue(true) +
                                              ", check that this is not an error.\n");
                        }

                        for (int q = 0; q < num; q++) {

                            if (tagVals[q] != null) {
                            	try {
                            		dispString += tagVals[q].toString();
                            	} catch(NullPointerException e1) {
                            		dispString += "";
                            	}
                            		
                                if ((q + 1) < num) {
                                    dispString += ", ";
                                }
                            }
                        }

                        rowData[2] = dispString;
                    }
                    // // vm = 2 for patient orientation
                    // if (!name.equals("0020,0020")) {
                    // if (vm > 1 && vr == "SS") { // hack Neva fix this!!!
                    // System.out.println("Inside NEVA hack.  Neva, fix this!!!");
                    // System.out.println("Wait.  Check it out.  The code asks if "+
                    // " the string var vr \"==\" SS.  Wonder when the \n" +
                    // " String's hashcode will -ever- be equal to \"SS\"."+
                    // "  Or Maybe Never.  This should be fixed.");
                    // short[] values = (short[])((FileDicomTag)tagsList.get(key)).getValue(false);
                    // for (int k = 0; k < vm; k++) {
                    // rowData[2] = values[k] + " ";
                    // }
                    // }
                    // else {
                    // rowData[2] = ((FileDicomTag)tagsList.get(key)).getValue(true).toString();
                    // }
                    // }
                    // }
                } // special cases which contain coded information:
                else if (vr.equals("PN")) {
                    String s = (String) ((FileDicomTag) tagsList.get(key)).getValue(true);

                    rowData[2] = s.replace('^', ',');
                } else if (name.equals("0008,0060")) {

                    switch (DicomInfo.getModality()) {

                        case 1:
                            rowData[2] = "BIOMAGENETIC_IMAGING";
                            break;

                        case 2:
                            rowData[2] = "COLOR_FLOW_DOPPLER";
                            break;

                        case 3:
                            rowData[2] = "COMPUTED_RADIOGRAPHY";
                            break;

                        case 4:
                            rowData[2] = "COMPUTED_TOMOGRAPHY";
                            break;

                        case 5:
                            rowData[2] = "DUPLEX_DOPPLER";
                            break;

                        case 6:
                            rowData[2] = "DIAPHANOGRAPHY";
                            break;

                        case 7:
                            rowData[2] = "DIGITAL_RADIOGRAPHY";
                            break;

                        case 8:
                            rowData[2] = "ENDOSCOPY";
                            break;

                        case 9:
                            rowData[2] = "GENERAL_MICROSCOPY";
                            break;

                        case 10:
                            rowData[2] = "HARDCODY";
                            break;

                        case 11:
                            rowData[2] = "INTRAORAL_RADIOGRAPHY";
                            break;

                        case 12:
                            rowData[2] = "LASER_SURFACE_SCAN";
                            break;

                        case 13:
                            rowData[2] = "MAGNETIC_RESONANCE_ANGIOGRAPHY";
                            break;

                        case 14:
                            rowData[2] = "MAMMOGRAPHY";
                            break;

                        case 15:
                            rowData[2] = "MAGNETIC_RESONANCE";
                            break;

                        case 16:
                            rowData[2] = "MAGNETIC_RESONANCE_SPECTROSCOPY";
                            break;

                        case 17:
                            rowData[2] = "NUCLEAR_MEDICINE";
                            break;

                        case 18:
                            rowData[2] = "OTHER";
                            break;

                        case 19:
                            rowData[2] = "POSITRON_EMISSION_TOMOGRAPHY";
                            break;

                        case 20:
                            rowData[2] = "PANORAMIC_XRAY";
                            break;

                        case 21:
                            rowData[2] = "RADIO_FLUOROSCOPY";
                            break;

                        case 22:
                            rowData[2] = "RADIOGRAPHIC_IMAGING";
                            break;

                        case 23:
                            rowData[2] = "RADIOTHERAPY_DOSE";
                            break;

                        case 24:
                            rowData[2] = "RADIOTHERAPY_IMAGE";
                            break;

                        case 25:
                            rowData[2] = "RADIOTHERAPY_PLAN";
                            break;

                        case 26:
                            rowData[2] = "RADIOTHERAPY_RECORD";
                            break;

                        case 27:
                            rowData[2] = "RADIOTHERAPY_STRUCTURE_SET";
                            break;

                        case 28:
                            rowData[2] = "SLIDE_MICROSCOPY";
                            break;

                        case 29:
                            rowData[2] = "SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY";
                            break;

                        case 30:
                            rowData[2] = "THERMOGRAPHY";
                            break;

                        case 31:
                            rowData[2] = "ULTRASOUND";
                            break;

                        case 32:
                            rowData[2] = "XRAY_ANGIOGRAPHY";
                            break;

                        case 33:
                            rowData[2] = "EXTERNAL_CAMERA_PHOTOGRAPHY";
                            break;

                        case 34:
                            rowData[2] = "UNKNOWN";
                            break;

                        default:
                            rowData[2] = "UNKNOWN";
                            break;
                    }
                } else if (name.equals("0008,0064")) {
                    String s = ((String) ((FileDicomTag) tagsList.get(key)).getValue(true)).trim();

                    if (s.equals("DV")) {
                        rowData[2] = "Digitized Video";
                    } else if (s.equals("DI")) {
                        rowData[2] = "Digital Interface";
                    } else if (s.equals("DF")) {
                        rowData[2] = "Digitized Film";
                    } else if (s.equals("WSD")) {
                        rowData[2] = "Workstation";
                    }
                } else if (name.equals("0018,5100")) {
                    String s = ((String) ((FileDicomTag) tagsList.get(key)).getValue(true)).trim();

                    if (s.equals("HFP")) {
                        rowData[2] = "Head First-Prone";
                    } else if (s.equals("HFS")) {
                        rowData[2] = "Head First-Supine";
                    } else if (s.equals("HFDR")) {
                        rowData[2] = "Head First-Decubitus Right";
                    } else if (s.equals("HFDL")) {
                        rowData[2] = "Head First-Decubitus Left";
                    } else if (s.equals("FFP")) {
                        rowData[2] = "Feet First-Prone";
                    } else if (s.equals("FFS")) {
                        rowData[2] = "Feet First-Supine";
                    } else if (s.equals("FFDR")) {
                        rowData[2] = "Feet First-Decubitus Right";
                    } else if (s.equals("FFDL")) {
                        rowData[2] = "Feet First-Decubitus Left";
                    } else {
                        rowData[2] = s;
                    }
                } else if (vr.equals("SQ")) {

                    // System.err.println("Key  = " + key);
                    FileDicomSQ sq = (FileDicomSQ) ((FileDicomTag) tagsList.get(key)).getValue(true);
                    Vector display = sq.getSequenceDisplay();

                    rowData[2] = "";

                    for (Enumeration f = display.elements(); f.hasMoreElements();) {
                    	if(addRow(rowData, show)) {
                        	tagsModel.addRow(rowData);

	                        StringTokenizer st = new StringTokenizer((String) f.nextElement(), ";;;");
	
	                        rowData[1] = st.nextToken();
	
	                        if (st.hasMoreTokens()) {
	                            rowData[1] = st.nextToken();
	                        }
                    	} else {
                    		f.nextElement();
                    	}
                    }
                } // standard tag.  add tag.get(key).getValue(true) as-is to the table
                else { // if ( ((FileDicomTag) tagsList.get(key)).getMultiplicity() > 1) {

                    FileDicomTag t;
                    Object[] tagVals;

                    t = (FileDicomTag) tagsList.get(key);
                    tagVals = t.getValueList();

                    String dispString = "";
                    int num = t.getNumberOfValues();

                    if (num == 0) {
                        Preferences.debug("No Multiplicity: " + name + "  " + t.getValue(true) +
                                          ", check that this is not an error.\n");
                    }

                    for (int q = 0; q < num; q++) {
                    	try {
                    		dispString += tagVals[q].toString();
                    	} catch(NullPointerException e1) {
                    		dispString += "";
                    	}
                    		
                        if ((q + 1) < num) {
                            dispString += ", ";
                        }
                    }

                    rowData[2] = dispString;
                }

                //instances where rowData will be a private tag are checked here for showTags
                if(addRow(rowData, show)) {
                	tagsModel.addRow(rowData);
                }
            }
        }

       sort(tagsModel, 0, false, true);
    }
    
    /**
     * Shows the "Other Image Information", with or without private tags.
     *
     * @param  tagsModel  DOCUMENT ME!
     * @param  DicomInfo  DOCUMENT ME!
     * @param  show       boolean that indicates whether or not to show private tags
     */
    public static void showTags(ViewTableModel tagsModel, FileDicomItem DicomInfo, boolean show) {
        Iterator<String> e;
        String name;
        String key;
        String[] tags = null;
        Object[] rowData = {"", "", "" };
        TreeMap<String, FileDicomTag> tagsList = DicomInfo.getDataSet();
        
        //check preferences to see if any dicom tags were selected for saving
        String prefTagsString = Preferences.getProperty(Preferences.SAVE_DICOM_TAGS);
    	if(prefTagsString != null && (!prefTagsString.trim().equals(""))) {
    		tags = prefTagsString.split(";");
    	}
        Set<String> set = tagsList.keySet();
        // go through the hashlist, and for each element you find, copy it
        // into the table, showing full info if it was coded
        int ii;
        
        
        for (ii = 0, e = set.iterator(); e.hasNext(); ii++) {
            key =  e.next();
            name = key;

            if (((FileDicomTag) tagsList.get(key)).getValue(true) != null) {
            	String tagName = "(" + name + ")";
                rowData[0] = tagName;
                rowData[1] = ((FileDicomTag) tagsList.get(key)).getName();
                
                String vr = ((FileDicomTag) tagsList.get(key)).getValueRepresentation();
                int vm = ((FileDicomTag) tagsList.get(key)).getValueMultiplicity();


                if (rowData[1].equals("Private Tag") || vr.equals("OB")) {

                    // System.out.println("OB/Priv: "+name + ".." +((FileDicomTag)tagsList.get(key)).getValue(true));
                    // if (rowData[1].equals("Private Tag") || vr.equals("OB") || vm > 1) {
                    if (((FileDicomTag) tagsList.get(key)).getValue(false) instanceof Byte[]) {
                        // if (key.equals("0008,0040")) {   System.err.println("IN JdialogFileInfo looking at
                        // 0008,0040");   System.err.println("value: " + ((FileDicomTag)
                        // tagsList.get(key)).getValue(false).toString()); }

                        Byte[] bytesV = (Byte[]) ((FileDicomTag) tagsList.get(key)).getValue(false);
                        byte[] bytesValue = new byte[bytesV.length];

                        if ((bytesValue != null) && (bytesV != null)) {

                            // System.out.println(" length = " + bytesV.length);
                            for (int k = 0; k < bytesV.length; k++) {
                                bytesValue[k] = bytesV[k].byteValue();
                            }

                            if (bytesV.length == 0) {
                                rowData[2] = "";
                            } else if ((bytesValue[0] > 32) && (bytesValue[0] < 127)) {
                                rowData[2] = new String(bytesValue);
                            } else {
                                rowData[2] = convertType(bytesValue, true, vm);
                            }
                        }
                    } else if(vr.equals("SQ")) {
                        FileDicomSQ sq = (FileDicomSQ) ((FileDicomTag) tagsList.get(key)).getValue(true);
                        Vector display = sq.getSequenceDisplay();

                        rowData[2] = "";

                        for (Enumeration f = display.elements(); f.hasMoreElements();) {
                        	if(addRow(rowData, show)) {
	                        	tagsModel.addRow(rowData);
	
	                            StringTokenizer st = new StringTokenizer((String) f.nextElement(), ";;;");
	
	                            rowData[1] = st.nextToken();
	
	                            if (st.hasMoreTokens()) {
	                                rowData[2] = st.nextToken();
	                            }
                        	} else {
                        		f.nextElement();
                        	}
                        }
                	} else {

                        FileDicomTag t;
                        Object[] tagVals;

                        t = (FileDicomTag) tagsList.get(key);
                        tagVals = t.getValueList();

                        String dispString = "";
                        int num = t.getNumberOfValues();

                        if (num == 0) {
                            Preferences.debug("No Multiplicity: " + name + "  " + t.getValue(true) +
                                              ", check that this is not an error.\n");
                        }

                        for (int q = 0; q < num; q++) {

                            if (tagVals[q] != null) {
                            	try {
                            		dispString += tagVals[q].toString();
                            	} catch(NullPointerException e1) {
                            		dispString += "";
                            	}
                            		
                                if ((q + 1) < num) {
                                    dispString += ", ";
                                }
                            }
                        }

                        rowData[2] = dispString;
                    }
                    // // vm = 2 for patient orientation
                    // if (!name.equals("0020,0020")) {
                    // if (vm > 1 && vr == "SS") { // hack Neva fix this!!!
                    // System.out.println("Inside NEVA hack.  Neva, fix this!!!");
                    // System.out.println("Wait.  Check it out.  The code asks if "+
                    // " the string var vr \"==\" SS.  Wonder when the \n" +
                    // " String's hashcode will -ever- be equal to \"SS\"."+
                    // "  Or Maybe Never.  This should be fixed.");
                    // short[] values = (short[])((FileDicomTag)tagsList.get(key)).getValue(false);
                    // for (int k = 0; k < vm; k++) {
                    // rowData[2] = values[k] + " ";
                    // }
                    // }
                    // else {
                    // rowData[2] = ((FileDicomTag)tagsList.get(key)).getValue(true).toString();
                    // }
                    // }
                    // }
                } // special cases which contain coded information:
                else if (vr.equals("PN")) {
                    String s = (String) ((FileDicomTag) tagsList.get(key)).getValue(true);

                    rowData[2] = s.replace('^', ',');
                } else if (name.equals("0008,0060")) {
                            rowData[2] = DicomInfo.getTag("0008,0060");
                } else if (name.equals("0008,0064")) {
                    String s = ((String) ((FileDicomTag) tagsList.get(key)).getValue(true)).trim();

                    if (s.equals("DV")) {
                        rowData[2] = "Digitized Video";
                    } else if (s.equals("DI")) {
                        rowData[2] = "Digital Interface";
                    } else if (s.equals("DF")) {
                        rowData[2] = "Digitized Film";
                    } else if (s.equals("WSD")) {
                        rowData[2] = "Workstation";
                    }
                } else if (name.equals("0018,5100")) {
                    String s = ((String) ((FileDicomTag) tagsList.get(key)).getValue(true)).trim();

                    if (s.equals("HFP")) {
                        rowData[2] = "Head First-Prone";
                    } else if (s.equals("HFS")) {
                        rowData[2] = "Head First-Supine";
                    } else if (s.equals("HFDR")) {
                        rowData[2] = "Head First-Decubitus Right";
                    } else if (s.equals("HFDL")) {
                        rowData[2] = "Head First-Decubitus Left";
                    } else if (s.equals("FFP")) {
                        rowData[2] = "Feet First-Prone";
                    } else if (s.equals("FFS")) {
                        rowData[2] = "Feet First-Supine";
                    } else if (s.equals("FFDR")) {
                        rowData[2] = "Feet First-Decubitus Right";
                    } else if (s.equals("FFDL")) {
                        rowData[2] = "Feet First-Decubitus Left";
                    } else {
                        rowData[2] = s;
                    }
                } else if (vr.equals("SQ")) {

                    // System.err.println("Key  = " + key);
                    FileDicomSQ sq = (FileDicomSQ) ((FileDicomTag) tagsList.get(key)).getValue(true);
                    Vector display = sq.getSequenceDisplay();

                    rowData[2] = "";

                    for (Enumeration f = display.elements(); f.hasMoreElements();) {
                    	if(addRow(rowData, show)) {
                        	tagsModel.addRow(rowData);

	                        StringTokenizer st = new StringTokenizer((String) f.nextElement(), ";;;");
	
	                        rowData[1] = st.nextToken();
	
	                        if (st.hasMoreTokens()) {
	                            rowData[2] = st.nextToken();
	                        }
                    	} else {
                    		f.nextElement();
                    	}
                    }
                } // standard tag.  add tag.get(key).getValue(true) as-is to the table
                else { // if ( ((FileDicomTag) tagsList.get(key)).getMultiplicity() > 1) {

                    FileDicomTag t;
                    Object[] tagVals;

                    t = (FileDicomTag) tagsList.get(key);
                    tagVals = t.getValueList();

                    String dispString = "";
                    int num = t.getNumberOfValues();

                    if (num == 0) {
                        Preferences.debug("No Multiplicity: " + name + "  " + t.getValue(true) +
                                          ", check that this is not an error.\n");
                    }

                    for (int q = 0; q < num; q++) {
                    	try {
                    		dispString += tagVals[q].toString();
                    	} catch(NullPointerException e1) {
                    		dispString += "";
                    	}
                    		
                        if ((q + 1) < num) {
                            dispString += ", ";
                        }
                    }

                    rowData[2] = dispString;
                }

                //instances where rowData will be a private tag are checked here for showTags
                if(addRow(rowData, show)) {
                	tagsModel.addRow(rowData);
                }
            }
        }

        sort(tagsModel, 0, false, true);
    }
    
    /**
	 * Determines whether the given row should be shown. Show indicates whether
	 * private tags are being displayed.
     */
    private static boolean addRow(Object[] rowData, boolean show) {
    	if (rowData[2].toString().contains("Private Tag") || rowData[2].toString().contains("private tag") 
        		|| rowData[2].toString().contains("Private tag")) {
            if (show) {
                return true;
            }
        } else {
            return true;
        }
    	
    	return false;
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

        if ((col == 2) && isInfoDialog) {

            while (!model.getValueAt(begin, 2).equals("Other Image Information")) {
                begin++;
            }

            begin += 3;
        }

        for (int p = begin; p < model.getRowCount(); p++) {

            for (int j = begin - 1; j < p; j++) {

                if (model.getValueAt(p, col) != null) {

                    if (reverse) {

                        if (((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) > 0) {
                            model.moveRow(p, p, j);

                            break;
                        }
                    } else {

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
        JDialogDICOMTagEditor tagEditor;

        if (e.getActionCommand().equals("Close")) { // close

            // clear out the editor dialog boxes
            for (int i = editorDialogDicomList.size() - 1; i >= 0; i--) {
                tagEditor = (JDialogDICOMTagEditor) editorDialogDicomList.elementAt(i);

                if (tagEditor != null) {
                    editorDialogDicomList.removeElementAt(i); // kill all open tag editing dialogs
                    tagEditor.dispose(); // remove the tag editor
                }
            }

            dispose(); // remove self
        } else if (e.getActionCommand().equals("Show")) { // show

            /*if (showPrivate) { // if currently showing private
                showPrivate = false; // but, toggle current status OFF
            } else {
                showPrivate = true; // toggle current status to ON
            }*/
            
            
            if(Preferences.is(Preferences.PREF_SHOW_PRIVATE_TAGS)) {
            	bogusShowPrivateButton.setSelected(true);
            	showPrivate = false;
            }else {
            	showPrivateButton.setSelected(true);
            	showPrivate = true;
            }
            
            // swap the border painting
            Preferences.setProperty(Preferences.PREF_SHOW_PRIVATE_TAGS, String.valueOf("" + !Preferences.is(Preferences.PREF_SHOW_PRIVATE_TAGS)));
            
            

            int i;

            for (i = 0; i < tagsModel.getRowCount(); i++) {

                if (tagsModel.getValueAt(i, 2).equals("Other Image Information")) {
                    break;
                }
            }

            i += 2;

            for (; i < tagsModel.getRowCount();) {
                tagsModel.removeRow(i);
            }

            showTags(tagsModel, DicomInfo, showPrivate); // display whatever the current showPrivate status

        } else if (e.getActionCommand().equals("AnonymizeImage")) { // anonymize image
            new JDialogAnonymizeImage(this, imageA); // changes the image internally,

            // so we don't need to remember the dialog.
            // now that dialog has finished,
            // tell any other objects that care that there are new data (ie, a new name) & update
            Vector imageFrames = imageA.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle();
            }

            setTitle(imageA.getImageName());

        } else if (e.getActionCommand().equals("OverlayTag")) {
            String tagKey;

            tagKey = new String((String) tagsTable.getValueAt(selectedRowDicom, 1)); // find the key to the selected
                                                                                     // DICOM tag

            if (tagKey.equals("")) { // workaround prevent the portion of the image information from
                return; // causing an exception if user tries to click edit tag
            } // caused 'cos i don't understand listSelectionModel well enough

            tagKey = tagKey.substring(1, tagKey.length() - 1); // remove the parens that are part of the display (the
                                                               // hashtable does not use parens)
            new JDialogOverlay(parentFrame, true, tagKey);

        } else if (e.getActionCommand().equals("EditTag")) { // edit the high-lighted tag

            String tagKey;

            // get the hash-code
            tagKey = new String((String) tagsTable.getValueAt(selectedRowDicom, 1)); // find the key to the selected
                                                                                     // DICOM tag

            if (tagKey.equals("")) { // workaround prevent the portion of the image information from
                return; // causing an exception if user tries to click edit tag
            } // caused 'cos i don't understand listSelectionModel well enough

            tagKey = tagKey.substring(1, tagKey.length() - 1); // remove the parens that are part of the display (the
                                                               // hashtable does not use parens)

            // make a dialog that edits the tag at that hash-code
            boolean broughtToFront = bringToFront(tagKey);

            if (!broughtToFront) { // make a new dialog to edit the key

                // if the tag is a "Private Tag", it won't be able to be changed (without
                // returning, starting an editor dialog crashes the program)
                if (DicomInfo.getTagTable().get(tagKey).getName().equals("Private Tag")) {
                    MipavUtil.displayError("Sorry, can't change private tags!");

                    return;
                }

                editorDialogDicom = new JDialogDICOMTagEditor(this, tagKey, DicomInfo.getTagTable(), false); // make a dialog that edits that particular
                editorDialogDicom.addButtonListener(this);
                editorDialogDicom.addWindowListener(new WindowAdapter() { // listen for when the dialog comes alive
                        public void windowActivated(WindowEvent e) {

                            // bringToFront(e);
                            JDialogDICOMTagEditor tagDialog; // temporary tag editor dialog

                            tagDialog = (JDialogDICOMTagEditor) e.getSource();

                            // bring to front of list and front of screen.
                            // find element first,
                            int index = editorDialogDicomList.indexOf(tagDialog);

                            // then move it from index i to 0;
                            tagDialog = (JDialogDICOMTagEditor) editorDialogDicomList.elementAt(index);
                            editorDialogDicomList.removeElementAt(index); // remove tag from list
                            editorDialogDicomList.insertElementAt(tagDialog, 0); // and Put it back at the top.
                            tagDialog.toFront();

                        }

                        // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                        // were no problems with user inputted values); if it closed by okbutton, then
                        // modify all slices if user so desired.
                        // In addition, when the change was applied to all slices, notify the image frames
                        // in the image frame vector to reset the title (title changes only for name,
                        // @see ModelImage.setTitle
                        //
                        public void windowClosed(WindowEvent e) {
                            JDialogDICOMTagEditor tagDialog; // temporary tag editor dialog

                            tagDialog = (JDialogDICOMTagEditor) e.getSource();

                            if (tagDialog.wasDialogOkay()) {

                                if (tagDialog.applyToAllSlices()) { // apply change to all slices

                                    int i;

                                    if (imageA.getNDims() == 2) {
                                        DicomInfo.getTagTable().setValue(tagDialog.getTagKey(),
                                                                         tagDialog.returnTag().getValue(false));
                                    } else {
                                        FileInfoDicom tempInfo;

                                        for (i = 0; i < imageA.getExtents()[2]; i++) {
                                            tempInfo = (FileInfoDicom) imageA.getFileInfo(i);
                                            tempInfo.getTagTable().setValue(tagDialog.getTagKey(),
                                                                            tagDialog.returnTag().getValue(false));
                                            imageA.setFileInfo(tempInfo, i);
                                        }
                                    }

                                    // tell any other objects that care that there are new data
                                    // (ie, a new name) & update
                                    Vector imageFrames = imageA.getImageFrameVector();

                                    for (i = 0; i < imageFrames.size(); i++) {
                                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle();
                                    }

                                    setTitle(imageA.getImageName());
                                } else { // do not apply this change to all image-info.  Apply this change to only this
                                         // slice.

                                    // place the tag back into the DicomInfo
                                    DicomInfo.getTagTable().setValue(tagDialog.getTagKey(),
                                                                     tagDialog.returnTag().getValue(false));
                                }

                                if (editorDialogDicomList.size() > 0) {
                                    editorDialogDicomList.removeElementAt(0);
                                } // clean up

                                int i;

                                for (i = 0; i < tagsModel.getRowCount(); i++) {

                                    if (tagsModel.getValueAt(i, 2).equals("Other Image Information")) {
                                        break;
                                    }
                                }

                                i += 2;

                                for (; i < tagsModel.getRowCount();) {
                                    tagsModel.removeRow(i);
                                }

                                showTags(tagsModel, DicomInfo, showPrivate); // update the displayed table
                            } else { }
                        }

                    });
                editorDialogDicom.setVisible(true);
                editorDialogDicomList.insertElementAt(editorDialogDicom, 0); // make topmost dialog as the first
                                                                             // element in the list
            }

        } else if (e.getActionCommand().equals("SaveTags")) {
        	isAppend = false;
            saveTags();
        } else if (e.getActionCommand().equals("SaveTagsAppend")) {
        	isAppend = true;
            saveTags();
        } else if (e.getActionCommand().equals("TagEditorApplyToAllSlicesCheckBox")) { }
        else if (e.getActionCommand().equals("TagEditorOK")) { // not included anymore because the handling is done by
                                                               // a closedWindow handler.

            // which more effectively deals with the order-of-processing.  This would be called before the
            // dialog boxes OKButton event handler which would check to make sure the input fields in the box
            // had legit values.  See the closedWindow handler for "EditTags" above.
            // try {
            // tagEditor = (JDialogDICOMTagEditor)editorDialogDicomList.firstElement();
            // if (tagEditor.wasDialogOkay()) {
            // String tagID = tagEditor.getTagKey();
            //
            // DicomInfo.putTag(tagID, tagEditor.returnTag()); // place the tag back into the DicomInfo
            // editorDialogDicomList.removeElementAt(0);    // clean up
            // tagEditor.dispose();
            // showTags(showPrivate);          // update the displayed table
            // }
            // else {
            // return;
            // }
            // }
            // catch (NoSuchElementException noElement) {
            // MipavUtil.displayError("JDialogFileInfoDICOM: No first tag in the tag editor list.");
            // }
        } else if (e.getActionCommand().equals("TagEditorCancel")) {

            try {
                editorDialogDicomList.removeElementAt(0);
            } catch (ArrayIndexOutOfBoundsException badArr) {
                MipavUtil.displayError("JDialogFileInfoDICOM: Out of bounds array at specified tag in the tag editor list.  How???");
            }
        } else if(e.getActionCommand().equals("CheckAll")) {
        	for (int i = 0; i < tagsModel.getRowCount(); i++) {
        		if(tagsModel.getValueAt(i, 0) != null) {
        			tagsModel.setValueAt(new Boolean(true), i, 0);
        		}
            }
        }else if(e.getActionCommand().equals("UncheckAll")) {
        	for (int i = 0; i < tagsModel.getRowCount(); i++) {
        		if(tagsModel.getValueAt(i, 0) != null) {
        			tagsModel.setValueAt(new Boolean(false), i, 0);
        		}
            }
        }else {
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
    public void displayAboutInfo(ModelImage _image, FileInfoDicom _info, int sIndex) {
        DicomInfo = _info; // set the input var
        imageA = _image; // set the input var
        sliceIndex = sIndex;

        Object[] rowData = { new Boolean(false), "", "", "" };
        String[] columnNames = { " ", "Tag", "Name", "Value" };

        try {
            tagsModel = new ViewTableModel() {
                public boolean isCellEditable(int row, int column) {
                    if (column == 0) {
                      return true;
                    }
                    return false;
                  }
                };
            tagsTable = new JTable(tagsModel);
            

            editorDialogDicomList = new Vector(); // Vector to hold editing dialogs
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("JDialogFileInfoDICOM reports: Out of memory!");

            return;
        } catch (IllegalArgumentException ex) {
            MipavUtil.displayError("JDialogFileInfoDICOM reports: Editing table too small!");

            return;
        }

        int[] extents;
        int i;

        for (i = 0; i < 4; i++) {
            tagsModel.addColumn(columnNames[i]);
        }

        tagsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        tagsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        
        //tagsTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        tagsTable.getColumn(" ").setMinWidth(50);
        tagsTable.getColumn(" ").setMaxWidth(50);
        tagsTable.getColumn(" ").setCellRenderer(new CheckBoxRenderer());
        tagsTable.getColumn(" ").setCellEditor(new CheckBoxEditor());
        
        tagsTable.getColumn("Tag").setMinWidth(90);
        tagsTable.getColumn("Tag").setMaxWidth(90);
        tagsTable.getColumn("Tag").setCellRenderer(new TagCodeRenderer());
        tagsTable.getColumn("Name").setMinWidth(160);
        tagsTable.getColumn("Name").setMaxWidth(500);
        tagsTable.getColumn("Name").setCellRenderer(new TagReferenceRenderer());
        tagsTable.getColumn("Value").setMinWidth(50);
        tagsTable.getColumn("Value").setMaxWidth(1000);
        tagsTable.getColumn("Value").setCellRenderer(new TagReferenceRenderer());

        tagsTable.getTableHeader().addMouseListener(new HeaderListener());
        tagsTable.setSelectionBackground(new Color(184, 230, 255));
        
        // make the table high-light only 1 row at a time; (for edit tag)
        // taken from http://java.sun.com/docs/books/tutorial/uiswing/components/table.html#selection
        listSelectorDicom = tagsTable.getSelectionModel();
        
        listSelectorDicom.addListSelectionListener(new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    ListSelectionModel lsm = (ListSelectionModel) e.getSource();

                    if (lsm.isSelectionEmpty()) {
                    	editTagButton.setEnabled(false); // ...//no rows are selected
                    	overlayButton.setEnabled(false);
                    } else {
                        int oldSelectedRow = selectedRowDicom;

                        selectedRowDicom = lsm.getMinSelectionIndex();

                        if ((oldSelectedRow != selectedRowDicom) && ((selectedRowDicom - lsm.getMaxSelectionIndex()) == 0)) {
                        	String tagKey = new String((String) tagsTable.getValueAt(selectedRowDicom, 1));
                        	if (!tagKey.equals("")) {
                        		editTagButton.setEnabled(true); 
                        		overlayButton.setEnabled(true);
                        	}else {
                        		editTagButton.setEnabled(false); 
                        		overlayButton.setEnabled(false);
                        	}
                        } // ...//selectedRow is selected
                        // else {
                        // lsm.clearSelection();
                        // }
                    }
                }
            });

        tagsModel.addRow(rowData);
        tagsModel.addRow(rowData);
        tagsModel.setValueAt("Essential Image Information", 0, 2);
        tagsModel.setValueAt(null, 0, 0);
        tagsModel.setValueAt(null, 1, 0);
        i = 2;
        extents = DicomInfo.getExtents();

        for (int j = 0; j < extents.length; j++) {
            tagsModel.addRow(rowData);
            tagsModel.setValueAt("Dimension", i, 2);
            tagsModel.setValueAt(new Integer(extents[j]), i, 3);
            tagsModel.setValueAt(null, i, 0);
            i++;
        }

        int dataType = DicomInfo.getDataType();

        tagsModel.addRow(rowData);
        tagsModel.setValueAt(null, i, 0);
        tagsModel.setValueAt("Type", i, 2);
        tagsModel.setValueAt(ModelStorageBase.getBufferTypeStr(dataType), i, 3);

        i++;
        tagsModel.addRow(rowData);
        tagsModel.setValueAt(null, i, 0);
        tagsModel.setValueAt("Min", i, 2);
        tagsModel.setValueAt(new Double(DicomInfo.getMin()), i, 3);
        tagsModel.addRow(rowData);
        tagsModel.setValueAt("Max", ++i, 2);
        tagsModel.setValueAt(null, i, 0);
        tagsModel.setValueAt(new Double(DicomInfo.getMax()), i, 3);
        tagsModel.addRow(rowData);
        tagsModel.setValueAt("Orientation", ++i, 2);
        tagsModel.setValueAt(null, i, 0);

        switch (DicomInfo.getImageOrientation()) {

            case FileInfoBase.AXIAL:
                tagsModel.setValueAt("Axial", i, 3);
                break;

            case FileInfoBase.CORONAL:
                tagsModel.setValueAt("Coronal", i, 3);
                break;

            case FileInfoBase.SAGITTAL:
                tagsModel.setValueAt("Sagittal", i, 3);
                break;

            default:
                tagsModel.setValueAt("Unknown", i, 3);
        }

        float[] resolutions;

        resolutions = DicomInfo.getResolutions();
        i++;

        for (int j = 0; j < extents.length; j++) {
            tagsModel.addRow(rowData);
            tagsModel.setValueAt("Pixel resolution " + j, i, 2);
            tagsModel.setValueAt(new Float(resolutions[j]), i, 3);
            tagsModel.setValueAt(null, i, 0);
            i++;
        }

        int measure = DicomInfo.getUnitsOfMeasure(0);

        tagsModel.addRow(rowData);
        tagsModel.setValueAt("Unit of measure", i, 2);
        tagsModel.setValueAt(null, i, 0);

        if (measure == FileInfoBase.INCHES) {
            tagsModel.setValueAt("Inches per pixel", i, 3);
        } else if (measure == FileInfoBase.MILLIMETERS) {
            tagsModel.setValueAt("Millimeters per pixel", i, 3);
        } else if (measure == FileInfoBase.CENTIMETERS) {
            tagsModel.setValueAt("Centimeters per pixel", i, 3);
        } else if (measure == FileInfoBase.METERS) {
            tagsModel.setValueAt("Meters per pixel", i, 3);
        } else if (measure == FileInfoBase.KILOMETERS) {
            tagsModel.setValueAt("Kilometers per pixel", i, 3);
        } else if (measure == FileInfoBase.MILES) {
            tagsModel.setValueAt("Miles per pixel", i, 3);
        } else {
            tagsModel.setValueAt("Unknown", i, 3);
        }

        i++;
        tagsModel.addRow(rowData);
        tagsModel.setValueAt("Transformation Matrix", i, 2);
        tagsModel.setValueAt(null, i, 0);

        String matrixString = imageA.getMatrix().matrixToString(8, 4);
        int nextIndex = 0, index = 0;
        String subStr = new String();

        for (int ii = 0; ii < imageA.getMatrix().getDim(); ii++) {
            i++;
            nextIndex = matrixString.indexOf("\n", index);

            if (nextIndex != -1) {
                subStr = matrixString.substring(index, nextIndex);
                index = nextIndex + 1;
                tagsModel.addRow(rowData);
                tagsModel.setValueAt(subStr, i, 3);
                tagsModel.setValueAt(null, i, 0);
            } else {
                subStr = matrixString.substring(index, matrixString.length());
                tagsModel.addRow(rowData);
                tagsModel.setValueAt(subStr, i, 3);
                tagsModel.setValueAt(null, i, 0);
            }
        }

        
        tagsModel.addRow(rowData);
        tagsModel.setValueAt(null, i+1, 0);
        tagsModel.addRow(rowData);
        tagsModel.setValueAt(null, i+2, 0);
        i += 2;
        tagsModel.setValueAt("Other Image Information", i, 2);
        i++;
        tagsModel.addRow(rowData);
        tagsModel.setValueAt(null, i, 0);
        showTags(tagsModel, DicomInfo, showPrivate);

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

        //toolbar
        toolbarPanel = new JPanel();
        toolbarPanel.setLayout(new BorderLayout());
        
        toolbarBuilder = new ViewToolBarBuilder(this);
        ButtonGroup showPrivateGroup = new ButtonGroup();
        showPrivateButton = toolbarBuilder.buildToggleButton("Show", "Show/Hide Private Tags", "showprivate", showPrivateGroup);
        bogusShowPrivateButton = new JToggleButton();
        showPrivateGroup.add(bogusShowPrivateButton);
        if (Preferences.is(Preferences.PREF_SHOW_PRIVATE_TAGS)) {
        	showPrivateButton.setSelected(true);
        }else {
        	bogusShowPrivateButton.setSelected(true);
        }
        saveCheckedButton = toolbarBuilder.buildButton("SaveTags", "Save Checked Tags to new file", "savechecked");
        saveCheckedAppendButton = toolbarBuilder.buildButton("SaveTagsAppend", "Save and Append Checked Tags to file", "savecheckedappend");
        checkAllButton = toolbarBuilder.buildButton("CheckAll", "Check All Tags", "checkall");
        uncheckAllButton = toolbarBuilder.buildButton("UncheckAll", "Uncheck All Tags", "uncheckall");
        editTagButton = toolbarBuilder.buildButton("EditTag", "Edit Tag", "edittag");
        editTagButton.setEnabled(false);
        overlayButton = toolbarBuilder.buildButton("OverlayTag", "Overlay", "overlay");
        overlayButton.setEnabled(false);
        anonymizeButton = toolbarBuilder.buildButton("AnonymizeImage", "Anonymize", "anon");

        toolBar = ViewToolBarBuilder.initToolBar();
        toolBar.add(showPrivateButton);
        toolBar.add(ViewToolBarBuilder.makeSeparator());
        toolBar.add(checkAllButton);
        toolBar.add(uncheckAllButton);
        toolBar.add(ViewToolBarBuilder.makeSeparator());
        toolBar.add(saveCheckedButton);
        toolBar.add(saveCheckedAppendButton);
        toolBar.add(ViewToolBarBuilder.makeSeparator());
        toolBar.add(editTagButton);
        toolBar.add(ViewToolBarBuilder.makeSeparator());
        toolBar.add(overlayButton);
        toolBar.add(ViewToolBarBuilder.makeSeparator());
        toolBar.add(anonymizeButton);
        
        toolbarPanel.add(toolBar);


        getContentPane().add(toolbarPanel, BorderLayout.NORTH);
        getContentPane().add(scrollPaneDicom, BorderLayout.CENTER);
        getContentPane().setSize(new Dimension(700, 650));
        setSize(700, 650);
        
        buildColorMap();

    }

    /**
     * Converts type.
     *
     * @param   bytesValue  Array of bytes to convert
     * @param   endianess   DOCUMENT ME!
     * @param   vm          DOCUMENT ME!
     *
     * @return  String with new value
     */
    private static String convertType(byte[] bytesValue, boolean endianess, int vm) {

        if (vm == 0) {
            vm = 1;
        }

        int length = bytesValue.length / vm;
        String retValue = "";

        switch (length) {

            case 2:

                short value;

                for (int i = 0; i < vm; i++) {

                    if (endianess) {
                        value = (short) ((bytesValue[0] << 8) | bytesValue[1]);
                    } else {
                        value = (short) ((bytesValue[1] << 8) | bytesValue[0]);
                    }

                    retValue += String.valueOf((int) value);

                    if ((i + 1) < vm) {
                        retValue += "\\";
                    }
                }

                break;

            case 4:

                int value2;

                for (int i = 0; i < vm; i++) {

                    if (endianess) {
                        value2 = (int) ((bytesValue[0] << 24) | (bytesValue[1] << 16) | (bytesValue[2] << 8) |
                                            bytesValue[3]);
                    } else {
                        value2 = (int) ((bytesValue[3] << 24) | (bytesValue[2] << 16) | (bytesValue[1] << 8) |
                                            bytesValue[0]);
                    }

                    retValue += String.valueOf(value2) + " ";

                    if ((i + 1) < vm) {
                        retValue += "\\";
                    }
                }

                break;

            case 8:

                double value3;

                for (int i = 0; i < vm; i++) {

                    if (endianess) {
                        value3 = (double) ((bytesValue[0] << 56) | (bytesValue[1] << 48) | (bytesValue[2] << 40) |
                                               (bytesValue[3] << 32) | (bytesValue[4] << 24) | (bytesValue[5] << 16) |
                                               (bytesValue[6] << 8) | bytesValue[7]);
                    } else {
                        value3 = (double) ((bytesValue[7] << 56) | (bytesValue[6] << 48) | (bytesValue[5] << 40) |
                                               (bytesValue[4] << 32) | (bytesValue[3] << 24) | (bytesValue[2] << 16) |
                                               (bytesValue[1] << 8) | bytesValue[0]);
                    }

                    retValue += String.valueOf(value3) + " ";

                    if ((i + 1) < vm) {
                        retValue += "\\";
                    }
                }

                break;

            default:
                retValue = new String(bytesValue);
        }

        return retValue;
    }


    /**
     * Checks whether or not the dialog exists; if it does, it brings the dialog to front.
     *
     * @param   tagKey  The tag's Key. Used to dtermine if this tag already has an editor associated with it.
     *
     * @return  <code>true</code> if both a tag with the tagkey existed in the list and the associated dialog was
     *          brought to front.
     */
    private boolean bringToFront(String tagKey) {
        JDialogDICOMTagEditor tagEditor; // temporary tag editor dialog

        // list is empty
        if (editorDialogDicomList.isEmpty()) {
            return false;
        }

        // check all tag editors in the list to see if the given tag key is has a tag in the list.
        // drop out once one has been found and brought to front of list and of screen.
        for (int i = 0; i < editorDialogDicomList.size(); i++) {

            if (((JDialogDICOMTagEditor) editorDialogDicomList.elementAt(i)).getTagKey().equals(tagKey)) {

                // dialog has already been made
                tagEditor = (JDialogDICOMTagEditor) editorDialogDicomList.elementAt(i); // get ith tag from list
                editorDialogDicomList.removeElementAt(i); // remove this tag from the list
                editorDialogDicomList.insertElementAt(tagEditor, 0); // and Put it back in at the top.
                tagEditor.toFront();

                return true;
            }
        }

        return false;
    }
    
    /**
     * Builds which cells to display as light blue and green.
     */
    private void buildColorMap() {
    	Color green = new Color(0xbbffbb); // light green
    	Color blue = new Color(0xccffff); // light blue
    	boolean doBlue = false;
    	Vector<FileDicomKey> v;
    	Collections.sort(v = new Vector<FileDicomKey>(DicomInfo.getTagTable().getTagList().keySet()), new CompareGroup());
    	Iterator<FileDicomKey> e = v.iterator();
    	FileDicomKey next;
    	String groupNumber = new String();
    	groupColorMap = new TreeMap<String,Color>();
    	Hashtable<FileDicomKey, FileDicomTagInfo> tempTable = DicomDictionary.getDicomTagTable();
    	while(e.hasNext()) {
    		if(tempTable.get(next = e.next()) != null && 
    				groupColorMap.get(groupNumber = next.getGroup()) == null) {
    			groupColorMap.put(groupNumber, doBlue ? blue : green);
	    		doBlue = !doBlue;
    		}
    	}
    }
    
    private class CompareGroup implements Comparator<FileDicomKey> {

		public int compare(FileDicomKey o1Key, FileDicomKey o2Key) {
			String o1 = o1Key.getGroup();
			String o2 = o2Key.getGroup();
			
			if(o1.equals("50xx")) {
				o1 = "5000";
			} else if(o1.equals("60xx")) {
				o1 = "6000";
			}
			
			if(o2.equals("50xx")) {
				o2 = "5000";
			} else if(o2.equals("60xx")) {
				o2 = "6000";
			}
			
			return Integer.valueOf(o1, 16) - Integer.valueOf(o2, 16);
		}	
    }

    /**
     * Save tags to a file in ASCII format.
     */
    private void saveTags() {

        JFileChooser chooser = null;
        ViewUserInterface UI = ViewUserInterface.getReference();
        RandomAccessFile raFile;
        File file;
        int i;
        boolean isEmpty = true;
        StringBuffer saveTagsSB = new StringBuffer();

        for (int k=0; k < tagsModel.getRowCount(); k++) {
        	if (tagsModel.getValueAt(k, 0) != null && ((Boolean)tagsModel.getValueAt(k, 0)).booleanValue() == true) {
        		isEmpty = false;
        		break;
        	}
        	
        	
        }

        if (isEmpty) {

            // should show error message
            MipavUtil.displayError("Please check tags to be saved.");

            return;
        }
        if(launchFileChooser) {
	        try {
	
	            chooser = new JFileChooser();
	
	            if (UI.getDefaultDirectory() != null) {
	                file = new File(UI.getDefaultDirectory());
	
	                if (file != null) {
	                    chooser.setCurrentDirectory(file);
	                } else {
	                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	                }
	            } else {
	                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	            }
	
	            chooser.setDialogTitle("Save Tags");
	
	            int returnValue = chooser.showSaveDialog(UI.getMainFrame());
	
	            if (returnValue == JFileChooser.APPROVE_OPTION) {
	                fileName = chooser.getSelectedFile().getName();
	                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
	                UI.setDefaultDirectory(directory);
	            } else {
	                return;
	            }
	        } catch (OutOfMemoryError e) {
	            MipavUtil.displayError("Out of memory!");
	
	            return;
	        }
        }

        try {
            file = new File(directory + fileName);
            raFile = new RandomAccessFile(file, "rw");

            for (i = 0; i < tagsModel.getRowCount(); i++) {

                if (tagsModel.getValueAt(i, 2).equals("Other Image Information")) {
                    break;
                }
            }

            i += 2;

            if(isAppend) {
            	raFile.skipBytes( (int)raFile.length() );
            }else {
            	raFile.setLength(0);
            	raFile.seek(0);
            }

            for (; i < tagsModel.getRowCount(); i++) {

                if (tagsModel.getValueAt(i, 0) != null && ((Boolean)tagsModel.getValueAt(i, 0)).booleanValue() == true) {
                	
                    raFile.writeBytes(tagsModel.getValueAt(i, 1) + "\t" + tagsModel.getValueAt(i, 2) + "\t" +
                                      tagsModel.getValueAt(i, 3) + "\n");
                    saveTagsSB.append(tagsModel.getValueAt(i, 1));
                    saveTagsSB.append(";");
                }
            }

            Preferences.setProperty(Preferences.SAVE_DICOM_TAGS, saveTagsSB.toString().substring(0, saveTagsSB.toString().lastIndexOf(";")));
            raFile.close();
            insertScriptLine();
        } catch (IOException error) {
            MipavUtil.displayError("");

            // should show error message
            return;
        }

    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Simple listener for the table header.
     */
    private class HeaderListener implements MouseListener {

        /**
         * When the user clicks on a header, sorts the column.
         *
         * @param  e  event that triggered this method
         */

        public void mouseClicked(MouseEvent e) {
            Object source = e.getSource();
            Point p = e.getPoint();
            int col;

            if (source.equals(tagsTable.getTableHeader())) {
                col = tagsTable.columnAtPoint(p);

                if (col == 3) {
                    return;
                }
                if(col == 0) {
                	return;
                }

                if (e.isShiftDown()) {
                    sort(tagsModel, col, true, true);
                } else {
                    sort(tagsModel, col, false, true);
                }
            }
        }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseDragged(MouseEvent e) { }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseEntered(MouseEvent e) { }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseExited(MouseEvent e) { }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mousePressed(MouseEvent e) { }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseReleased(MouseEvent e) { }
    }
    
    
    
    
    /**
     * call algorithm
     *
     */
	protected void callAlgorithm() {
        saveTags();
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
		FileInfoDicom fileInfo;
		if(imageA.getFileInfo().length > sliceIndex) {
			fileInfo = (FileInfoDicom)imageA.getFileInfo()[sliceIndex];
		}else {
			fileInfo = (FileInfoDicom)imageA.getFileInfo()[0];
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
    
    
    
    
    
//  --------------------INNER CLASSES -------------------------------------------------------------------------------  
	private class CheckBoxRenderer extends DefaultTableCellRenderer {
  	  JCheckBox checkBox = new JCheckBox();
  	  public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
  		  	if (value instanceof Boolean) { // Boolean
  		  		checkBox.setSelected(((Boolean) value).booleanValue());
  		  		checkBox.setHorizontalAlignment(JLabel.CENTER);
  		  		return checkBox;
  		  	} 

  		  	return null;
  	  }
    }
	
	private class TagReferenceRenderer extends DefaultTableCellRenderer {
		
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			
			Component cell = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
			
			TableCellRenderer refRenderer = table.getCellRenderer(row, 1);
			if(refRenderer instanceof TagCodeRenderer && ((TagCodeRenderer) refRenderer).hasValidTag() && row > 16+(imageA.getNDims()-1)*2) {
				cell.setBackground(((TagCodeRenderer) refRenderer).getBackground());
			} else {
				int[] rows = table.getSelectedRows();
		  		boolean rowSelected = false;
		  		for(int i=0; i<rows.length; i++) {
		  			if(rows[i] == row) {
		  				rowSelected = true;
		  				break;
		  			}
		  		}
		  		if(!rowSelected) {
		  			cell.setBackground(Color.white);
		  		}
			}
			return cell;
		}
		
	}
	
	private class TagCodeRenderer extends DefaultTableCellRenderer {
		
		private boolean hasValidTag = false;
		
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			Component cell = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
			
			int[] rows = table.getSelectedRows();
	  		boolean rowSelected = false;
	  		for(int i=0; i<rows.length; i++) {
	  			if(rows[i] == row) {
	  				rowSelected = true;
	  				break;
	  			}
	  		}
	  		if(!rowSelected) {
				if(column == 1 && value instanceof String && ((String)value).length() == 11 && row > 16+(imageA.getNDims()-1)*2) {
	  		  		hasValidTag = true;
	  		  		String name = ((String)value).substring(1, ((String)value).length() - 1);
	  		  		String group = name.substring(0, 4);
	  		  		Color f = groupColorMap.get(group);
	  		  		if(f == null) {
	  		  			f = new Color(0xffcccc); // light red
	  		  		}

	  		  		cell.setBackground(f);
	  		  		return cell;
	  		  	}
	  		  	
				return null;
	  		}
	  		return cell;
		}

		public boolean hasValidTag() {
			return hasValidTag;
		}
	}

    
    private class CheckBoxEditor implements TableCellEditor {
  	  private final static int BOOLEAN = 1;
  	  DefaultCellEditor cellEditor;
  	  int flg;
  	  
  	  public CheckBoxEditor() {
  	    JCheckBox checkBox = new JCheckBox();
  	    cellEditor = new DefaultCellEditor(checkBox);
  	    checkBox.setHorizontalAlignment(JLabel.CENTER);
  	  }

  	  public Component getTableCellEditorComponent(JTable table, Object value,
  	      boolean isSelected, int row, int column) {
  	    if (value instanceof Boolean) { // Boolean
  	      flg = BOOLEAN;
  	      return cellEditor.getTableCellEditorComponent(table,
  	          value, isSelected, row, column);
  	    }
  	    return null;
  	  }

  	  public Object getCellEditorValue() {
  	    switch (flg) {
  	    case BOOLEAN:
  	      return cellEditor.getCellEditorValue();
  	    default:
  	      return null;
  	    }
  	  }

  	  public Component getComponent() {
  	    return cellEditor.getComponent();
  	  }

  	  public boolean stopCellEditing() {
  	    return cellEditor.stopCellEditing();
  	  }

  	  public void cancelCellEditing() {
  	    cellEditor.cancelCellEditing();
  	  }

  	  public boolean isCellEditable(EventObject anEvent) {
  	    return cellEditor.isCellEditable(anEvent);
  	  }

  	  public boolean shouldSelectCell(EventObject anEvent) {
  	    return cellEditor.shouldSelectCell(anEvent);
  	  }

  	  public void addCellEditorListener(CellEditorListener l) {
  	    cellEditor.addCellEditorListener(l);
  	  }

  	  public void removeCellEditorListener(CellEditorListener l) {
  	    cellEditor.removeCellEditorListener(l);
  	  }

  	  public void setClickCountToStart(int n) {
  	    cellEditor.setClickCountToStart(n);
  	  }

  	  public int getClickCountToStart() {
  	    return cellEditor.getClickCountToStart();
  	  }
  	}

}


