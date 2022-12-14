package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlicesVolumes;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.tree.*;

import java.util.Date;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.io.File;
import java.io.FileNotFoundException;
import java.text.SimpleDateFormat;


/**
 * DOCUMENT ME!
 *
 * @version  1.1 June 15, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/view/dialogs/JDialogVOIStats.java $ $Revision: 56 $ $Date: 2/17/06
 *           6:20p $</p>
 */
public class JDialogVOIStats extends JDialogScriptableBase
        implements ItemListener, ChangeListener, FocusListener, 
        UpdateVOISelectionListener, TreeSelectionListener, AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1618733051752245417L;

    /** DOCUMENT ME! */
    private static Icon ICON_POLYGON = MipavUtil.getIcon("polygon.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_POLYLINE = MipavUtil.getIcon("polyline.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_POINT = MipavUtil.getIcon("pointROI.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_LINE = MipavUtil.getIcon("linear.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_MEDICAL_FRAME = MipavUtil.getIcon("med_frame.gif");
    /** DOCUMENT ME! */
    private static Icon ICON_X_AXIS = MipavUtil.getIcon("xalign.gif");
    /** DOCUMENT ME! */
    private static Icon ICON_Y_AXIS = MipavUtil.getIcon("yalign.gif");
    /** DOCUMENT ME! */
    private static Icon ICON_Z_AXIS = MipavUtil.getIcon("zalign.gif");
    

    /** DOCUMENT ME! */
    private static Icon ICON_PROTRACTOR = MipavUtil.getIcon("protractor.gif");

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Algorithm for computing VOI statistics */
    private AlgorithmVOIProps algoVOI;

    /** Applies bounding box/shading/name display and other UI changes to ModelImage */
    protected JButton applyButton;

    /** Performs statistics calculations */
    protected JButton calcButton;
    
    /** Displays Stats dialog help */
    protected JButton helpButton;

    /** Whether to display VOI with shading */
    protected JCheckBox checkboxOpacity;

    /** Whether to display bounding box around VOI */
    protected JCheckBox checkboxBoundingBox;
    
    /** Whether to save statistics to a file */
    protected JCheckBox checkboxSaveStats;

    /** Whether to include the selected VOI for statistics processing */
    protected JCheckBox checkboxIncludeForProcessing;

    /** Whether to display VOI name */
    protected JCheckBox checkboxVOIName;

    /** Allows VOI color to be selected */
    protected JButton colorButton;

    /** Internal dialog box for selecting VOI color */
    private ViewJColorChooser colorChooser;

    /** The current color of the selected VOI */
    private Color colorVOI;

    /** Displays points and name of selected VOI or contour */
    private JTextArea contourTextArea;

    /** Displays current opacity level in slider */
    protected JLabel currentOpacity;

    /** DOCUMENT ME! */
    protected JCheckBox followVOISelectionBox = null;

    /** DOCUMENT ME! */
    protected Border frameBorder = null;

    /** DOCUMENT ME! */
    private boolean frameFollowsSelection = true;

    /** Internal reference to the currently selected ModelImage */
    protected ModelImage image;

    /** The list of statistics to calculate */
    protected JPanelStatisticsList listPanel;

    /** The opacity slider for the selected VOI */
    protected JSlider opacitySlider;

    /** DOCUMENT ME! */
    private VOITreePopup popup = null;

    /** A reference in the voiTree to the currently selected image */
    private DefaultMutableTreeNode root;

    /** The seed value last entered by the user */
    private short seedValue;

    /** Watershed seed value */
    protected JTextField seedValueTF;

    /** DOCUMENT ME! */
    private boolean treeSelectionChange = false;
    private boolean updateTree = false;

    /** The selected VOI when the VOIStats dialogue was created */
    private VOI voi;

    /** DOCUMENT ME! */
    protected JScrollPane voiContourPane;

    /** The tree of VOIs, composed of an image with children VOIs */
    private DefaultTreeModel voiModel;

    /** Name of the currently selected voi */
    protected JTextField VOIName;

    /** Thickness of the currently selected voi */
    protected JTextField VOIThicknessField;
    
    /** uid of the selected voi */
    protected JTextField UIDfield;

    /** The graphical representation of voiModel */
    private JTree voiTree;

    /** Displays the orthoganal list of VOIs in the selected ModelImage */
    protected JScrollPane voiTreePane;

    /** List of VOI sets that will have statistics calculated */
    private ViewVOIVector[] processList;

    /** Current set of VOIs that are being processed */
    private int processListIndex = 0;
    
    protected VOIHandlerInterface voiHandler;
    
    private JRadioButton activeVolumeButton = null;
    
    private JRadioButton allVolumesButton = null;
    
    private int activeVolume;
    
    private boolean doAllVolumes;
    
    private int tDim;
    
    private AlgorithmSubset subsetAlgo;
    
    private ModelImage subsetImage = null;
    
    /** VOI used for whole image processing */
    private VOI wholeImage;
    
    /** DOCUMENT ME! */
    private float rangeMaximum = 0f;

    /** DOCUMENT ME! */
    private float rangeMinimum = 0f;
    
    private float rangeMaximumR = 0f;
    
    private float rangeMinimumR = 0f;
    
    private float rangeMaximumG = 0f;
    
    private float rangeMinimumG  = 0f;
    
    private float rangeMaximumB = 0f;
    
    private float rangeMinimumB = 0f;
    
    private JPanelPixelExclusionSelector excluder;
    
    /** When running as a script, holds the pixel exclusion range. */
    private RangeType scriptRange;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogVOIStats() {

    	
    	listPanel = new JPanelStatisticsList();
    	excluder = new JPanelPixelExclusionSelector(listPanel, false);
    }
    
    /**
     * Constructor for the JDialogVOIStats.
     *
     * <p>this class ought to listen for VOI updates, but we are having it implemented elsewhere.</p>
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  img             DOCUMENT ME!
     * @param  _voi            DOCUMENT ME!
     */
    public JDialogVOIStats(VOIHandlerInterface theVoiHandler, ModelImage img, VOI _voi) {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }
        voi = _voi;
        image = img;
        voiHandler = theVoiHandler;
        
        processList = new ViewVOIVector[2];
        processList[0] = new ViewVOIVector(); //used to store whole VOIs
        processList[1] = new ViewVOIVector(); //used to store individual contours
        //adds any VOIs that have a component selected into the list of VOIs to be calculated

        init();
        updateVOIPanel(voi,image);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Applies changes to VOI when "Apply" is pressed; closes when "Cancel" is pressed; and calculates statistics and
     * outputs them to the message frame when "Calculate" is pressed. Also brings up a color chooser when the color
     * button is pressed.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;
        

        if (source == colorButton) {
        	if (voi == null)
        		MipavUtil.displayError("You must select a VOI");
        	else
        		showColorChooser();
        } else if (source == VOIName) {
        	voi.setName(VOIName.getText());
        	voi.update();
        	updateVOIPanel(voi, image);
        } else if (source == VOIThicknessField) {
        	boolean changedThickness = false;
            int thickChange = 1;

            try {
                int thickness = voi.getThickness();
                thickChange = Integer.parseInt(VOIThicknessField.getText());

                if (((thickChange < 0) || (thickChange > 20))) {
                    MipavUtil.displayWarning("VOI thickness must be greater than 0 and less than 20");
                } else if (thickness != thickChange) {
                    changedThickness = true;
                }
            } catch (Exception e) {
                VOIThicknessField.setText("1");
            }

            if (changedThickness) {
                voi.setThickness(thickChange);
                Preferences.setProperty(Preferences.PREF_VOI_THICKNESS, Integer.toString(thickChange));
                voi.update();
            	updateVOIPanel(voi, image);
            }
        } else if (source == UIDfield) {
        	try { 
                int uid = Integer.valueOf(UIDfield.getText()).intValue();
                voi.setUID(uid);
            } catch(NumberFormatException e) {
                MipavUtil.displayError("UID must be an integer");
            }
        } else if (source == followVOISelectionBox) {
            frameFollowsSelection = followVOISelectionBox.isSelected();
        } else if (source == helpButton) {
        	//MipavUtil.showHelp("10231");
        	MipavUtil.showWebHelp("Calculating_statistics_on_VOI_groups");
        } else if (source == applyButton) {
            ViewVOIVector vectorVOI = image.getVOIs();
            if (vectorVOI.size() == 0) {
            	return;
            }
            ViewVOIVector newVOIVector;
            int[] temp;
            int j = 0, location = -1;
            String name = "";

//            tmpStr = seedValueTF.getText();
//
//            if (testParameter(tmpStr, 0, 32000)) {
//                seedValue = Short.valueOf(tmpStr).shortValue();
//                voi.setWatershedID(seedValue);
//            }

            try {
                newVOIVector = new ViewVOIVector();
                temp = new int[100];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: JDialogVOIStats.actionPerformed");

                return;
            }

            for (int i = 0; i < vectorVOI.size(); i++) {

                if ((vectorVOI.elementAt(i)).getName().equals(VOIName.getText()) &&
                        !(vectorVOI.elementAt(i)).equals(voi)) {
                    newVOIVector.addElement(vectorVOI.elementAt(i));
                    temp[j++] = i;
                    name = VOIName.getText();
                }

                if ((vectorVOI.elementAt(i)).equals(voi)) {
                    location = i;
                }
            }

//            voi.setName(VOIName.getText());
            
//            try { 
//                int uid = Integer.valueOf(UIDfield.getText()).intValue();
//                voi.setUID(uid);
//            } catch(NumberFormatException e) {
//                MipavUtil.displayError("UID must be an integer");
//            }

//            boolean changedThickness = false;
//            int thickChange = 1;
//
//            try {
//                int thickness = voi.getThickness();
//                thickChange = Integer.parseInt(VOIThicknessField.getText());
//
//                if (((thickChange < 0) || (thickChange > 20))) {
//                    MipavUtil.displayWarning("VOI thickness must be greater than 0 and less than 20");
//                } else if (thickness != thickChange) {
//                    changedThickness = true;
//                }
//            } catch (Exception e) {
//                VOIThicknessField.setText("1");
//            }
//
//            if (changedThickness) {
//                voi.setThickness(thickChange);
//                Preferences.setProperty(Preferences.PREF_VOI_THICKNESS, Integer.toString(thickChange));
//            }

          
            voi.setPolarity(VOI.ADDITIVE);

//            voi.setColor(colorVOI);

                //voiHandler.setVOIColor(colorVOI);

            if ((j != 0) && (location != -1)) {

                newVOIVector.addElement(voi);
                temp[j++] = location;

                int[] where = new int[j];

                for (int i = 0; i < j; i++) {
                    where[i] = temp[i];
                }

                int size = image.getVOIs().size();
                for(int i=0; i<size; i++) {
                    image.getVOIs().get(i).setAllActive(true);
                }
                
                image.groupVOIs();

                Vector<VOI> VOIs = image.getVOIs();
                updateVOIPanel((VOIs.elementAt(VOIs.size() - 1)), image);
            } else {
                updateVOIPanel(voi, image);
            }

            updateTree();
            image.notifyImageDisplayListeners(null, true);
        } else if (source == calcButton) {
        	
        	boolean anyStats = false;
        	boolean[] statList = listPanel.getSelectedList();
        	for(int i=0; i<statList.length; i++) {
        	    if(statList[i]) {
        	        anyStats = true;
        	        break;
        	    }
        	}
        	
        	if(!anyStats) {
        	    MipavUtil.displayInfo("No statistics have been selected for calculation.");
        	    return;
        	}

            //Get the VOIs to use for calculations
            TreePath[] tPaths = voiTree.getSelectionPaths();
            TreePath currentPath;

            processList = new ViewVOIVector[2];
            processList[0] = new ViewVOIVector(); //used to store whole VOIs
            processList[1] = new ViewVOIVector(); //used to store individual contours
            //adds any VOIs that have a component selected into the list of VOIs to be calculated
           
            for (int i = 0; i < tPaths.length; i++) {
                currentPath = tPaths[i];

                if(currentPath.getLastPathComponent() instanceof VOIGroupNode) {
                    processList[0].add(((VOIGroupNode)currentPath.getLastPathComponent()).getVOIgroup());
                } else if(currentPath.getLastPathComponent() instanceof VOIOrientationNode) {
                    Vector<VOIBase>[] sortedCurves = ((VOIOrientationNode)currentPath.getLastPathComponent()).getVOI();
                    
                    VOI parent = null;
                    
                    for(Object obj : currentPath.getPath()) {
                        if(obj instanceof VOIGroupNode) {
                            parent = ((VOIGroupNode)obj).getVOIgroup();
                        }
                    }
                    
                    if(parent == null) {
                        MipavUtil.displayError("No matching VOI found fo selected contours");
                        return;
                    }
                    
                    VOI v = getVOIforProcessing(parent);

                    for(int k=0; k<sortedCurves.length; k++) {
                        for(int m=0; m<sortedCurves[k].size(); m++) {
                            v.getCurves().get(sortedCurves[k].get(m).getContourID()).setProcess(true); //only set the selected contours for processing
                        } 
                    }
                } else if(currentPath.getLastPathComponent() instanceof VOIContourNode) {
                    VOIBase b = (VOIBase) ((VOIContourNode)currentPath.getLastPathComponent()).getUserObject(); 
                    VOI v = getVOIforProcessing(b.getGroup());
                    
                    v.getCurves().get(b.getContourID()).setProcess(true); //only set the selected contour for processing
                }
            }
            
            if(processList[0].size() == 0) { //do whole image processing
                VOIVector wholeImageVec = new VOIVector();
                
                int slice = image.getNDims() > 2 ? image.getExtents()[2] : 1;
                wholeImage = new VOI((short) 0, "wholeImage", VOI.CONTOUR, 0.0f);
                int xDim = image.getExtents()[0]-2; //TODO: Change to xDim-1 when bug 539 is fixed
                int yDim = image.getExtents()[1]-2; //TODO: Change to xDim-1 when bug 539 is fixed
                
                for(int i=0; i<slice; i++) {    
                    Vector3f orig = new Vector3f(0, 0, i);
                    Vector3f cornerX = new Vector3f(xDim-2, 0, i);
                    Vector3f cornerXY = new Vector3f(xDim-2, yDim-2, i);
                    Vector3f cornerY = new Vector3f(0, yDim-2, i);
                    Vector<Vector3f> points = new Vector<Vector3f>();
                    points.add(orig);
                    points.add(cornerX);
                    points.add(cornerXY);
                    points.add(cornerY);
                    VOIContour wholeSlice = new VOIContour(true, true, points);
                    wholeImage.importCurve(wholeSlice);
                    wholeSlice.setGroup(wholeImage);
                    wholeSlice.setProcess(true);
                }

                wholeImageVec.add(wholeImage);
                image.registerVOI(wholeImage);
                
                processList[0].add(wholeImage);
            }
            
            callVOIAlgo(processList[0], AlgorithmVOIProps.PROCESS_PER_VOI, isRunInSeparateThread());
        } else if (source == closeButton) {
            cancelFlag = true;
            setVisible(false);
        } else {
            super.actionPerformed(event);
        }
    }
    
    private VOI getVOIforProcessing(VOI parent) {
        VOI v = null;
        for(int j=0; j<processList[1].size(); j++) {
            if(processList[1].get(j).getName().equals(parent.getName())) {
                v = processList[1].get(j);
                break;
            }
        }
        
        if(v == null) {
            v = new VOI(parent);
            v.setProcess(false);
            processList[1].add(v);
        }
        
        return v;
    }

    public JPanelStatisticsList getListPanel() {
        return listPanel;
    }

    public void callVOIAlgo(ViewVOIVector voiProcessingSet, int processingMode, boolean inSepThread) {
      //set min/max ranges for all VOIs that are in the process list
        for(int i=0; i<voiProcessingSet.size(); i++) {
            if (image.isColorImage()) {
                try {
                    voiProcessingSet.get(i).setMinimumIgnoreR(excluder.getLowerBoundR());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMinimumIgnoreR(-Float.MAX_VALUE);
                }
                
                try {
                    voiProcessingSet.get(i).setMaximumIgnoreR(excluder.getUpperBoundR());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMaximumIgnoreR(Float.MAX_VALUE);
                } 
                
                try {
                    voiProcessingSet.get(i).setMinimumIgnoreG(excluder.getLowerBoundG());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMinimumIgnoreG(-Float.MAX_VALUE);
                }
                
                try {
                    voiProcessingSet.get(i).setMaximumIgnoreG(excluder.getUpperBoundG());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMaximumIgnoreG(Float.MAX_VALUE);
                }
                
                try {
                    voiProcessingSet.get(i).setMinimumIgnoreB(excluder.getLowerBoundB());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMinimumIgnoreB(-Float.MAX_VALUE);
                }
                
                try {
                    voiProcessingSet.get(i).setMaximumIgnoreB(excluder.getUpperBoundB());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMaximumIgnoreB(Float.MAX_VALUE);
                }
            } // if (image.isColorImage())
            else { // black and white image
                try {
                    voiProcessingSet.get(i).setMinimumIgnore(excluder.getLowerBound());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMinimumIgnore(-Float.MAX_VALUE);
                }
                
                try {
                    voiProcessingSet.get(i).setMaximumIgnore(excluder.getUpperBound());
                } catch(final NullPointerException npe) {
                    voiProcessingSet.get(i).setMaximumIgnore(Float.MAX_VALUE);
                }
            } // else black and white image
        }
        
        doAllVolumes = false;
        tDim = 1;
        int destExtents[] = null;
        activeVolume = 0;
        if (image.getNDims() >= 4) {
            tDim = image.getExtents()[3];
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
         // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
            subsetImage = new ModelImage(image.getType(), destExtents, image.getImageName());
            if (allVolumesButton.isSelected()) {
                doAllVolumes = true;
            }
            else {
                activeVolume = image.getParentFrame().getComponentImage().getTimeSlice();
            }
        }
        
        // only loading the image works because we have been changing
        // the thing held BY the image.
        
        if (tDim == 1) {
            subsetImage = image;
        }
        
            
        if (tDim > 1) {
            subsetAlgo = new AlgorithmSubset(image, subsetImage, AlgorithmSubset.REMOVE_T, activeVolume); 
            subsetAlgo.run();
        }
        algoVOI = new AlgorithmVOIProps(subsetImage, processingMode,
                      excluder.getRangeFlag(), voiProcessingSet); 
        
        algoVOI.addListener(this);
        // only calculate these if appropriate box is checked for speed.
        // also, only use one slice if 2D image
        if(processingMode != AlgorithmVOIProps.PROCESS_PER_VOI || image.getNDims() == 2) {
            listPanel.setSliceCount(1);
        } else {
            listPanel.setSliceCount(image.getExtents()[2]);
        }
        algoVOI.setSelectedStatistics(listPanel.getSelectedList());
        createProgressBar(subsetImage.getImageName(), algoVOI);
        
        
        if (inSepThread) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algoVOI.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {

            algoVOI.run();
        }   
    }
    
   
   


    // *******************************************************************
    // ************************* Focus Events ****************************
    // *******************************************************************

    public void algorithmPerformed(AlgorithmBase algorithm) {
        if(algorithm instanceof AlgorithmVOIProps && algoVOI.isCompleted()) {
            
            if(wholeImage != null) {
                image.unregisterVOI(wholeImage);
                wholeImage = null;
            }
            
            VOIStatisticalProperties properties;
            String[] statLabels = listPanel.getNameList();
            int processType = ((AlgorithmVOIProps)algorithm).getProcessType();
            
            for(int i=0; i<((AlgorithmVOIProps)algorithm).getVOIList().size(); i++) { //all VOIs that are selected for processing
                VOI tempVOI = ((AlgorithmVOIProps)algorithm).getVOIList().get(i);
                properties = algoVOI.getVOIProperties(tempVOI);
                String pSetDesc = tempVOI.getName();
                String name, valueType, specificName, specificRedName, specificGreenName, specificBlueName;
                Object value;
                SimpleDateFormat dFormat = new SimpleDateFormat("yyyy-MM-dd");
                SimpleDateFormat tFormat = new SimpleDateFormat("HH:mm:ss");
                Date date = new Date();
                String dateStr = dFormat.format(date);
                String timeStr = tFormat.format(date);

                int numCurves = processType == AlgorithmVOIProps.PROCESS_PER_VOI ? 1 : tempVOI.getCurves().size();
                for(int curve=0; curve<numCurves; curve++) {
                    if(processType != AlgorithmVOIProps.PROCESS_PER_VOI && !tempVOI.getCurves().get(curve).getProcess()) {
                        continue;
                    }
                    
                    // Save statistics in the header only if image format is XML.
                    if (((image.getFileInfo(0).getFileFormat() == FileUtility.XML) ||
                            (image.getFileInfo(0).getFileFormat() == FileUtility.XML_MULTIFILE)) && checkboxSaveStats != null && checkboxSaveStats.isSelected()) {
                        for (int j = 0; j < image.getFileInfo().length; j++) { //all imageInfos for image
                            
                            ((FileInfoImageXML)image.getFileInfo(j)).createPSet(pSetDesc);
                            for(int k=0; k < statLabels.length; k++) { //all selected statistics
                                name = statLabels[k];
                                specificName = extendName(name, tempVOI, curve, processType);
                                value = properties.getVOIStatistic(specificName);
                                ((FileInfoImageXML)image.getFileInfo(j)).getPSet(pSetDesc).addParameter(specificName);
                                ((FileInfoImageXML)image.getFileInfo(j)).getPSet(pSetDesc).getParameter(specificName).setValue(value.toString());
                                if(value instanceof Integer) {
                                    valueType = "int";
                                } else if(value instanceof Float) {
                                    valueType = "float";
                                } else {
                                    valueType = "string";
                                }
                                ((FileInfoImageXML)image.getFileInfo(j)).getPSet(pSetDesc).getParameter(specificName).setValueType(valueType);
                                ((FileInfoImageXML)image.getFileInfo(j)).getPSet(pSetDesc).getParameter(specificName).setDate(dateStr);
                                ((FileInfoImageXML)image.getFileInfo(j)).getPSet(pSetDesc).getParameter(specificName).setTime(timeStr);
                            }
                        }
                    }
                    
                    ViewUserInterface UI = ViewUserInterface.getReference();
                    Preferences.data("\n -----------------------------------------------------------------------------\n");
                    Preferences.data("Image:     " + image.getImageName() + "\n");
                    if (image.getNDims() >= 4) {
                    	Preferences.data("Active volume = " + activeVolume + "\n");
                    }
                    Preferences.data("VOI  :     " + tempVOI.getName() + "\n");
                    if(processType != AlgorithmVOIProps.PROCESS_PER_VOI) {
                    	Preferences.data("Contour :     "+tempVOI.getCurves().get(curve).getContourID()+"     Slice: "+tempVOI.getCurves().get(curve).slice()+"\n");
                    }
                    
                    
                    for(int k=0; k < statLabels.length; k++) { //all selected statistics
                        name = statLabels[k];
                        specificName = extendName(name, tempVOI, curve, processType);
                        
                        if (image.isColorImage()) {
                            if ((name.equals("Min Intensity")) || (name.equals("Max Intensity")) ||
                                (name.equals("Avg Voxel Intensity")) || (name.equals("Std Dev of Intensity")) ||
                                (name.equals("Sum Intensities")) || (name.equals("Center of Mass")) ||
                                (name.equals("Coefficient of skewness")) || (name.equals("Coefficient of kurtosis")) ||
                                (name.equals("Median Intensity")) || (name.equals("Mode Intensity")) ||
                                (name.equals("Mode Count"))) {
                            	specificRedName = extendName(name+"Red", tempVOI, curve, processType);
                            	specificGreenName = extendName(name+"Green", tempVOI, curve, processType);
                            	specificBlueName = extendName(name+"Blue", tempVOI, curve, processType);
                            	Preferences.data("  "+name+"\tRed\t\t= "+properties.getVOIStatistic(specificRedName).toString()+"\n"); 
                            	Preferences.data("  "+name+"\tGreen\t\t= "+properties.getVOIStatistic(specificGreenName).toString()+"\n");
                            	Preferences.data("  "+name+"\tBlue\t\t= "+properties.getVOIStatistic(specificBlueName).toString()+"\n");  
                            }
                            else {
                            	Preferences.data("  "+name+"\t\t= "+properties.getVOIStatistic(specificName).toString()+"\n");    
                            }
                        }
                        else { 
                        	Preferences.data("  "+name+"\t\t= "+properties.getVOIStatistic(specificName).toString()+"\n");
                        }
                    }
                }
            }
            if (doAllVolumes && (activeVolume < tDim - 1)) {
               anotherCall();    
            } else if (image.getNDims() >= 4) {
                subsetImage.disposeLocal();
                subsetImage = null;
            }
            
            processListIndex++;
            if(processList != null && processListIndex < processList.length && processList[processListIndex].size() > 0) {
                callVOIAlgo(processList[processListIndex], AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR, false);
            } else {
                processListIndex = 0;
            }
            
            if(algorithm.isCompleted()) {
            	insertScriptLine();
            }
        }
        
    }
    
    /**
     * Changes voi name used for statistics retrieval based on processing mode
     */
    private String extendName(String name, VOI tempVOI, int contourNum, int processType) {
        StringBuffer buffer = new StringBuffer(name);
        switch (processType) {
        case AlgorithmVOIProps.PROCESS_PER_SLICE:
        case AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR:
            buffer.append(tempVOI.getCurves().get(contourNum).slice());
            break;
        }
           
        switch (processType) {
        case AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR:
        case AlgorithmVOIProps.PROCESS_PER_CONTOUR:
            buffer.append(VOIStatisticalProperties.DELIM).append(contourNum);
        }
        
        return buffer.toString();
    }
    
    private void anotherCall() {
        activeVolume++;
        subsetAlgo = new AlgorithmSubset(image, subsetImage, AlgorithmSubset.REMOVE_T, activeVolume); 
        subsetAlgo.run();
        int processType = AlgorithmVOIProps.PROCESS_PER_VOI;
        if(processListIndex != 0) {
            processType = AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR;
        }
        algoVOI = new AlgorithmVOIProps(subsetImage, processType,
                      excluder.getRangeFlag(), processList[processListIndex]); //TODO: Allow user to select processing method based on curves selected in processList
        
        algoVOI.addListener(this);
        //only calculate these if appropriate box is checked for speed.
        algoVOI.setSelectedStatistics(listPanel.getSelectedList());
        createProgressBar(subsetImage.getImageName(), algoVOI);
        
        
        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algoVOI.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {

            algoVOI.run();
        }       
    }
    
    

    /**
     * Test the seed value and if appropriate, sets it.
     *
     * @param  event  Event that triggered function.
     */
    public void focusLost(FocusEvent event) {
    	
    	if (voi != null) {
	    	Object source = event.getSource();
	    	
	    	if (source == seedValueTF) {
		        String tmpStr = seedValueTF.getText();
		
		        if (testParameter(tmpStr, 0, 32000)) {
		            seedValue = Short.valueOf(tmpStr).shortValue();
		            voi.setWatershedID(seedValue);
		        }
	        } else if (source == VOIName) {
	        	if (VOIName.getText() != null){
		        	voi.setName(VOIName.getText());
		        	voi.update();
		        	updateVOIPanel(voi, image);
	        	}
	        } else if (source == VOIThicknessField) {
	        	boolean changedThickness = false;
	            int thickChange = 1;
	
	            try {
	                int thickness = voi.getThickness();
	                thickChange = Integer.parseInt(VOIThicknessField.getText());
	
	                if (((thickChange < 0) || (thickChange > 20))) {
	                    MipavUtil.displayWarning("VOI thickness must be greater than 0 and less than 20");
	                } else if (thickness != thickChange) {
	                    changedThickness = true;
	                }
	            } catch (Exception e) {
	                VOIThicknessField.setText("1");
	            }
	
	            if (changedThickness) {
	                voi.setThickness(thickChange);
	                Preferences.setProperty(Preferences.PREF_VOI_THICKNESS, Integer.toString(thickChange));
	                voi.update();
	            	updateVOIPanel(voi, image);
	            }
	        } else if (source == UIDfield) {
	        	try { 
	                int uid = Integer.valueOf(UIDfield.getText()).intValue();
	                voi.setUID(uid);
	            } catch(NumberFormatException e) {
	                MipavUtil.displayError("UID must be an integer");
	            }
	        }
    	}
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Updates the VOI when the checkboxes for title, boundary, processing, and opacity are changed
     *
     * @param  event  Event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source.equals(checkboxOpacity)) {

            if (checkboxOpacity.isSelected()) {
                opacitySlider.setEnabled(true);
                voi.setDisplayMode(VOI.SOLID);
                voi.setOpacity(opacitySlider.getValue() / (float) 100);
            } else {
                opacitySlider.setEnabled(false);
                voi.setDisplayMode(VOI.BOUNDARY);
            }
        } else if (source.equals(checkboxBoundingBox)) {
	        if (checkboxBoundingBox.isSelected() == true) {
	            voi.setBoundingBoxFlag(true);
	        } else {
	            voi.setBoundingBoxFlag(false);
	        }
        } else if (source.equals(checkboxIncludeForProcessing)) {
	        if (checkboxIncludeForProcessing.isSelected() == true) {
	            voi.setProcess(true);
	        } else {
	            voi.setProcess(false);
	        }
        }
        
        ViewUserInterface.getReference().setUseVOIName(checkboxVOIName.isSelected());
    }

    /**
     * responds to the volume of interest (<code>VOI</code>) change events.
     *
     * <p>This method calls <code>updateVOI</code> using the <code>UpdateVOIEvent</code> changed <code>VOI</code>, and
     * retrieves the runningInSeparateThread out of the current image's frame.</p>
     *
     * @see  UpdateVOIEvent
     * @see  #updateVOI
     * @see  ViewJFrameBase#getActiveImage
     */
    public void selectionChanged(UpdateVOIEvent newVOIselection) {

        if (newVOIselection == null) {
            System.err.println("JDialogVOIStats.selectionChanged: new selection null");

            return;
        } else if (image == null) {
            System.err.println("JDialogVOIStats.selectionChanged: image is null");

            return;
        } 
        if ( voi != newVOIselection.getChangedVolumeOfInterest() )
        {
            updateVOIPanel(newVOIselection.getChangedVolumeOfInterest(), voiHandler.getActiveImage());
        }
        updateTree();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  visible  DOCUMENT ME!
     */
    public void setVisible(boolean visible) {

        if ((popup == null) && visible) {
            popup = new VOITreePopup();
            voiTree.addMouseListener(popup);
        }

        super.setVisible(visible);

        if (visible) {
            updateTree();
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void showColorChooser() {
        colorChooser = new ViewJColorChooser(null, "Pick VOI color", new OkColorListener(),
                                             new CancelListener());
    }

    // *******************************************************************
    // ************************* Change Events ****************************
    // *******************************************************************

    /**
     * Sets values based on knob along slider. Changes the opacity of the VOI
     *
     * @param  e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == opacitySlider) {
            currentOpacity.setText(String.valueOf(opacitySlider.getValue() / (float) 100));

            if (voi != null) {
                voi.setOpacity(opacitySlider.getValue() / (float) 100);
            }
        }
        
        updateVOIPanel(voi, image);
    }

    /**
     * Updates the dialog based on the VOI passed in.
     *
     * @param  _voi  VOI whose properties we want to calculate.
     * @param  img   Image where voi is to be updated
     */
    public void updateVOIPanel(VOI _voi, ModelImage img) {

        voi = _voi;
        image = img;

        if (voi != null) {

            if (voi.getBoundingBoxFlag() == true) {
                checkboxBoundingBox.setSelected(true);
            } else {
                checkboxBoundingBox.setSelected(false);
            }

            ViewUserInterface.getReference().setUseVOIName(checkboxVOIName.isSelected());

            /*if (voi.getPolarity() == voi.ADDITIVE) {
                checkboxAdditiveOrSubtractive.setSelected(true);
            } else {
                checkboxAdditiveOrSubtractive.setSelected(false);
            }*/

            checkboxIncludeForProcessing.setSelected(voi.getProcess());

            if (voi.getDisplayMode() == VOI.BOUNDARY) {
                checkboxOpacity.setSelected(false);
                opacitySlider.setEnabled(false);
            } else {
                checkboxOpacity.setSelected(true);
                opacitySlider.setEnabled(true);
            }

            // VOIName.setBackground(voi.getColor());
            colorButton.setBackground(voi.getColor());
            colorVOI = voi.getColor();

            seedValueTF.setText(String.valueOf(voi.getWatershedID()));

            VOIName.setText(voi.getName());
            UIDfield.setText(Integer.valueOf(voi.getUID()).toString());
            setTitle("VOI Properties/Statistics - " + voi.getUID());

            VOIThicknessField.setText(new Integer(voi.getThickness()).toString());
            
            // Enable "Save statistics in header" checkbox if image is of XML format, disable for any other format.
            if (image.getFileInfo(0).getFileFormat() == FileUtility.XML) {
            	checkboxSaveStats.setEnabled(true);
            } else {
            	checkboxSaveStats.setEnabled(false);
            }
            
            // turn things on/off depending on if a PolyLine is selected
            if ((voi.getCurveType() == VOI.POLYLINE) || (voi.getCurveType() == VOI.POLYLINE_SLICE) ||
                    (voi.getCurveType() == VOI.POINT) || (voi.getCurveType() == VOI.LINE) ||
                    (voi.getCurveType() == VOI.PROTRACTOR)) {

                listPanel.setCheckBoxesDisabled();
                calcButton.setEnabled(false);
                
                excluder.setEnabled(false);
                seedValueTF.setEnabled(false);
            } else {
                excluder.setEnabled(true);

                seedValueTF.setEnabled(true);
                calcButton.setEnabled(true);
                
                listPanel.setSliceCount(voi.getVOISlices());
                listPanel.setCheckBoxesEnabled();
            }
        }

        validate();
    }

    
    private void printTree( TreeModel model, Object parent )
    {
        if ( model.isLeaf(parent) && parent instanceof VOIContourNode )
        {
            VOIBase contour = ((VOIContourNode)parent).getVOI();
            if ( contour != null )
            {
                contour.setActive(false);
            }
            return;
        }
        int childCount = model.getChildCount(parent);
        for ( int i = 0; i < childCount; i++ )
        {
            printTree( model, model.getChild(parent,i) );
        }
    }

    /**
     * Updates the ViewJFrameImage when a VOI/contour is selected.
     *
     * @param  e  TreeSelectionEvent
     */
    public void valueChanged(TreeSelectionEvent e) {
        if ( updateTree )
        {
            return;
        }
        TreeModel model = voiTree.getModel();
        if ( !updateTree )
        {
            printTree( model, model.getRoot() );
        }
        
        TreePath leadPath = e.getNewLeadSelectionPath();

        if (leadPath != null) {
            Object[] leadObjects = leadPath.getPath();
            //int curveIndex = 0;

            if (leadObjects[leadObjects.length - 1] instanceof VOIContourNode) {
                VOIBase leadBase = ((VOIContourNode) leadObjects[leadObjects.length - 1]).getVOI();
                //VOI leadVOI = ((VOIGroupNode)((VOIFrameNode) ((VOINode) leadObjects[leadObjects.length - 1]).getParent()).getParent()).getVOIgroup();

                if (frameFollowsSelection && (image.getNDims() > 2)) {
                    voiHandler.setCenter(leadBase.getGeometricCenter(), true );
                    //System.err.println( "frameFollowsSelection " + leadBase );
                    leadBase.setActive(true);
                }

                updateContourPane(leadBase);
                setVOIActive(leadBase.getGroup());

            } else if (leadObjects[leadObjects.length - 1] instanceof VOIFrameNode) {
                //curveIndex = ((VOIFrameNode) leadObjects[leadObjects.length - 1]).getFrameNumber();

                if (frameFollowsSelection && (image.getNDims() > 2)) {
                    //voiHandler.setSlice(curveIndex);
                }
            } else if (leadObjects[leadObjects.length - 1] instanceof VOIGroupNode) {
                VOI leadGroup = ((VOIGroupNode) leadObjects[leadObjects.length - 1]).getVOIgroup();
                leadGroup.setActive(true);
                
                setVOIActive(leadGroup);
            }

        }
        treeSelectionChange = true;
    }
    
    /**
     * Internal method for setting all relevant parts of the GUI and the MIPAV interface to reflect the active VOI.
     */
    private void setVOIActive(VOI v) {
        updateVOIPanel(v, image);
        if(voiHandler instanceof VOIManagerInterface) {
            ((VOIManagerInterface) voiHandler).setSelectedVOI(v, false, false);
        }
    }

    /**
     * DOCUMENT ME!
     */
    protected void buildVOIContourPane() {
        contourTextArea = new JTextArea();
        contourTextArea.setFont(MipavUtil.font10);
        contourTextArea.setEditable(false);

        voiContourPane = new JScrollPane(contourTextArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

    }

    /**
     * DOCUMENT ME!
     */
    protected void buildVOITree() {

        ViewVOIVector VOIs = image.getVOIs();

        root = new DefaultMutableTreeNode(image.getImageName());
        voiModel = new DefaultTreeModel(root);

        Enumeration<VOI> e = VOIs.elements();

        VOI currentVOI = null;

        int index = 0;

        while (e.hasMoreElements()) {
            currentVOI = e.nextElement();
            voiModel.insertNodeInto(new VOIGroupNode(currentVOI,image.getExtents()), root, index);
            //voiModel.insertNodeInto(new VOIGroupNode(currentVOI), root, index);
            index++;
        }

        voiTree = new JTree(voiModel);
        voiTree.setCellRenderer(new VOITreeRenderer());

        voiTree.setFont(MipavUtil.font12);
        voiTree.addTreeSelectionListener(this);


        voiTreePane = new JScrollPane(voiTree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                      JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        voiTreePane.setPreferredSize(new Dimension(100,300));
    }

    /**
     * Sets up GUI components - buttons, checkboxes, sliders, etc.
     */
    protected void init() {

        // setTitle("VOI Statistics");
    	
        frameBorder = BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(),
                                                         BorderFactory.createLoweredBevelBorder());

        JLabel labelName = new JLabel("VOI name:");
        labelName.setFont(serif12);
        labelName.setForeground(Color.black);

        JLabel labelColor = new JLabel("VOI color:");
        labelColor.setFont(serif12);
        labelColor.setForeground(Color.black);

        JLabel labelThickness = new JLabel("VOI thickness:");
        labelThickness.setFont(serif12);
        labelThickness.setForeground(Color.black);
        
        JLabel labelUID = new JLabel("VOI UID:");
        labelUID.setFont(serif12);
        labelUID.setForeground(Color.black);

        colorButton = new JButton();
        colorButton.setPreferredSize(new Dimension(25, 25));
        colorButton.setToolTipText("Change VOI color");
        colorButton.addFocusListener(this);
        colorButton.addActionListener(this);

        VOIName = new JTextField(15);
        VOIName.setFont(serif12);
        VOIName.addFocusListener(this);
        VOIName.addActionListener(this);

        VOIThicknessField = new JTextField(3);
        VOIThicknessField.setFont(serif12);
        MipavUtil.makeNumericsOnly(VOIThicknessField, false);
        VOIThicknessField.addFocusListener(this);
        VOIThicknessField.addActionListener(this);
        
        UIDfield = new JTextField(3);
        UIDfield.setFont(serif12);
        MipavUtil.makeNumericsOnly(UIDfield, false);
        UIDfield.addActionListener(this);

        JPanel namePanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        namePanel.add(labelName, gbc);

        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        namePanel.add(VOIName, gbc);

        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        namePanel.add(labelUID, gbc);

        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        namePanel.add(UIDfield, gbc);
        
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.fill = GridBagConstraints.NONE;
        namePanel.add(labelThickness, gbc);

        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        namePanel.add(VOIThicknessField, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        namePanel.add(labelColor, gbc);

        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        namePanel.add(colorButton, gbc);

        gbc.insets = new Insets(0, 0, 0, 0);

        checkboxBoundingBox = new JCheckBox("Show contour bounding box");
        checkboxBoundingBox.setFont(serif12);
        checkboxBoundingBox.addItemListener(this);

        //checkboxAdditiveOrSubtractive = new JCheckBox("Use additive polarity for VOI");
        //checkboxAdditiveOrSubtractive.setFont(serif12);

        checkboxIncludeForProcessing = new JCheckBox("Include for processing");
        checkboxIncludeForProcessing.setFont(serif12);
        checkboxIncludeForProcessing.addItemListener(this);

        checkboxOpacity = new JCheckBox("Display VOI shading");
        checkboxOpacity.setFont(serif12);
        checkboxOpacity.addItemListener(this);

        checkboxVOIName = new JCheckBox("Show VOI name");
        checkboxVOIName.setFont(serif12);
        checkboxVOIName.addItemListener(this);
        checkboxVOIName.setSelected(Preferences.is(Preferences.PREF_SHOW_VOI_NAME));

        JPanel checkboxPanel = new JPanel();
        checkboxPanel.setLayout(new BoxLayout(checkboxPanel, BoxLayout.Y_AXIS));
        checkboxPanel.add(checkboxBoundingBox);
        //checkboxPanel.add(checkboxAdditiveOrSubtractive);
        checkboxPanel.add(checkboxIncludeForProcessing);
        checkboxPanel.add(checkboxVOIName);
        checkboxPanel.add(checkboxOpacity);

        opacitySlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 30);
        opacitySlider.addChangeListener(this);

        opacitySlider.setMajorTickSpacing(20);
        opacitySlider.setValue(30);
        opacitySlider.setPaintTicks(true);
        opacitySlider.setEnabled(false);
        opacitySlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(1));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        currentOpacity = new JLabel(String.valueOf(opacitySlider.getValue() / 100.0f));
        currentOpacity.setForeground(Color.black);
        currentOpacity.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf(0));
        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        JPanel sliderPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(opacitySlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(currentOpacity, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Opacity"));

        JPanel panelVOIProps = new JPanel(new GridBagLayout());
        panelVOIProps.setBorder(buildTitledBorder("VOI properties"));
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panelVOIProps.add(namePanel, gbc);
        gbc.gridy = 1;
        panelVOIProps.add(checkboxPanel, gbc);
        gbc.gridy = 2;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.NORTH; // gbc.fill = GridBagConstraints.BOTH;
        panelVOIProps.add(sliderPanel, gbc);

        listPanel = new JPanelStatisticsList();

        try {
            listPanel.setSliceCount(image.getExtents()[2]);
        } catch (ArrayIndexOutOfBoundsException aioobe) {

            // otherwise, this must be a 2d image.
            listPanel.setSliceCount(1);
        } finally {
            listPanel.setCheckBoxesEnabled();
        }
        
        JPanel volumePanel = null;
        if (image.getNDims() >= 4) {
            ButtonGroup volumeGroup = new ButtonGroup();
            activeVolumeButton = new JRadioButton("Active volume only");
            activeVolumeButton.setFont(MipavUtil.font12);
            volumeGroup.add(activeVolumeButton);
            
            allVolumesButton = new JRadioButton("Across all volumes");
            allVolumesButton.setFont(MipavUtil.font12);
            volumeGroup.add(allVolumesButton);
            
            activeVolumeButton.setSelected(true);
            allVolumesButton.setSelected(false);
            
            volumePanel = new JPanel();
            volumePanel.setLayout(new GridBagLayout());
            volumePanel.setBorder(new TitledBorder(new EtchedBorder(), "Volume Selection", TitledBorder.DEFAULT_JUSTIFICATION,
                    TitledBorder.DEFAULT_POSITION, MipavUtil.font12B));
            GridBagConstraints gbc3 = new GridBagConstraints();
            gbc3.gridx = 0;
            gbc3.gridy = 0;
            volumePanel.add(activeVolumeButton, gbc3);
            gbc3.gridy = 1;
            volumePanel.add(allVolumesButton, gbc3);
        }
        
        excluder = new JPanelPixelExclusionSelector(listPanel, image.isColorImage());
        
        checkboxSaveStats = new JCheckBox("Save statistics in header");
        checkboxSaveStats.setFont(serif12);
        //checkboxSaveStats.addActionListener(this);

        JPanel checkPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        checkPanel.add(checkboxSaveStats, gbc2);

        JPanel statsPanel = new JPanel();
        statsPanel.setLayout(new BoxLayout(statsPanel, BoxLayout.Y_AXIS));
        statsPanel.add(listPanel);
        if (image.getNDims() >= 4) {
            statsPanel.add(volumePanel);
        }
        statsPanel.add(excluder);

        JLabel labelSeed = new JLabel("Seed value (0-32K)");
        labelSeed.setFont(serif12);
        labelSeed.setForeground(Color.black);

        seedValueTF = new JTextField(5);
        seedValueTF.setFont(serif12);
        seedValueTF.addFocusListener(this);

        JPanel seedValuePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        seedValuePanel.setBorder(buildTitledBorder("Watershed seed value"));
        seedValuePanel.add(labelSeed);
        seedValuePanel.add(seedValueTF);
        
        statsPanel.add(seedValuePanel);
        statsPanel.add(checkPanel);

        applyButton = new JButton("Apply");
        applyButton.setPreferredSize(MipavUtil.defaultButtonSize);
        applyButton.setFont(serif12B);
        applyButton.addActionListener(this);

        closeButton = buildCloseButton();
        closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        
        helpButton = buildHelpButton();
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);

        // build the VOI tree
        buildVOITree();
        buildVOIContourPane();

        GridBagConstraints gb = new GridBagConstraints();

        JPanel mainTreePanel = new JPanel(new GridBagLayout());
        mainTreePanel.setBorder(buildTitledBorder("VOI Browser"));
        mainTreePanel.setPreferredSize(new Dimension(250,300));

        gb.anchor = GridBagConstraints.CENTER;
        gb.gridx = 0;
        gbc.gridy = 0;
        gb.weightx = 1.0;
        gb.weighty = 1.0;
        gb.fill = GridBagConstraints.BOTH;

        mainTreePanel.add(voiTreePane, gb);

        JPanel treeOptionPanel = new JPanel(new BorderLayout());
        treeOptionPanel.setBorder(buildTitledBorder("Tree options"));
        followVOISelectionBox = new JCheckBox("Frame follows VOI selection", true);
        followVOISelectionBox.setFont(MipavUtil.font12);
        followVOISelectionBox.addActionListener(this);
        followVOISelectionBox.setEnabled(image.getNDims() > 2);
        treeOptionPanel.add(followVOISelectionBox, BorderLayout.CENTER);


        gb.gridy = 1;
        gb.weightx = 1;
        gb.weighty = 0;
        gb.fill = GridBagConstraints.HORIZONTAL;
        mainTreePanel.add(treeOptionPanel, gb);

        gb.gridy = 2;
        gb.weightx = .5;
        gb.weighty = .5;
        gb.fill = GridBagConstraints.BOTH;
        mainTreePanel.add(voiContourPane, gb);

        JPanel leftButton = new JPanel();
        leftButton.add(applyButton);
        leftButton.add(closeButton);
        leftButton.add(helpButton);

        JPanel leftWholePanel = new JPanel(new BorderLayout());
        leftWholePanel.add(panelVOIProps, BorderLayout.NORTH);
        leftWholePanel.add(mainTreePanel, BorderLayout.CENTER);

        JPanel leftPanel = new JPanel(new BorderLayout());
        leftPanel.add(leftWholePanel);
        leftPanel.add(leftButton, BorderLayout.SOUTH);

        calcButton = new JButton("Calculate");
        calcButton.setPreferredSize(new Dimension(100, 30));
        calcButton.setFont(serif12B);
        calcButton.addActionListener(this);

        JPanel rightButton = new JPanel();
        rightButton.add(calcButton);
        //rightButton.add(helpButton);

        JPanel rightPanel = new JPanel(new BorderLayout());
        rightPanel.add(statsPanel);
        rightPanel.add(rightButton, BorderLayout.SOUTH);

        mainDialogPanel.setLayout(new GridBagLayout());
        gb.gridx = 0;
        gb.gridy = 0;
        gb.weightx = 1;
        gb.weighty = 1;
        gb.fill = GridBagConstraints.BOTH;
        mainDialogPanel.add(leftPanel, gb);

        gb.gridx = 1;
        mainDialogPanel.add(rightPanel, gb);


        // mainDialogPanel.setLayout(new BorderLayout());
        // mainDialogPanel.add(leftPanel, BorderLayout.WEST);
        // mainDialogPanel.add(rightPanel);
        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainDialogPanel);
        pack();

    }

    /**
     * DOCUMENT ME!
     *
     * @param  leadBase  DOCUMENT ME!
     */
    private void updateContourPane(VOIBase leadBase) {
        int i = 0;
        int size = leadBase.size();
        contourTextArea.setText("VOI name: " + leadBase.getName() + "\n");
        contourTextArea.append("contour name: " + leadBase.getLabel() + "\n");
        contourTextArea.append("number of points: " + leadBase.size() + "\n");

        String[] positions = null;
        Vector3f currentPt = null;
        int currentX, currentY, currentZ;

        if ((image.getFileInfo(0).getOrigin()[0] != 0) || (image.getFileInfo(0).getOrigin()[1] != 0) ||
                (image.getFileInfo(0).getOrigin()[2] != 0)) {

            for (i = 0; i < size; i++) {
                currentPt = (Vector3f) leadBase.elementAt(i);
                currentX = (int) currentPt.X;
                currentY = (int) currentPt.Y;
                currentZ = (int) currentPt.Z;

                positions = ViewJComponentEditImage.getScannerPositionLabels(image, currentPt);

                if (positions != null) {
	
                	contourTextArea.append(" X: " + String.valueOf(currentX) + " Y: " +
                			String.valueOf(currentY) + " Z: " + String.valueOf(currentZ) +
                			"  Position: " + positions[0] + " " + positions[1] + " " + positions[2] +
                	"\n");
	                	
	
                } else {

                	contourTextArea.append(" X: " + String.valueOf(currentX) + " Y: " +
                			String.valueOf(currentY) + " Z: " + String.valueOf(currentZ) +
                	"\n");
                    
                }

            }
        } else {

            for (i = 0; i < size; i++) {
                currentPt = (Vector3f) leadBase.elementAt(i);
                currentX = (int) currentPt.X;
                currentY = (int) currentPt.Y;
                currentZ = (int) currentPt.Z;

                contourTextArea.append(" X: " + String.valueOf(currentX) + " Y: " + String.valueOf(currentY) +
                                       " Z: " + String.valueOf(currentZ) + "\n");
            }

        }


        // for (i = 0; i < leadBase.size(); i++) {
        // contourTextArea.append("\t");
        // }

        contourTextArea.setCaretPosition(0);

        // System.err.println(contourTextArea.getText());
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void updateTree() {
        if (treeSelectionChange) {
            treeSelectionChange = false;
            return;
        }
        updateTree = true;
        if (this.isVisible()) {
            root.removeAllChildren();

            ViewVOIVector VOIs = image.getVOIs();
            Enumeration<VOI> e = VOIs.elements();

            VOI tempVOI = null;

            VOIGroupNode currentNode = null;
            Vector<VOIBase> curves = null;

            Enumeration<VOIBase> voiEnum = null;
            VOIBase voiBase = null;

            Enumeration<VOIFrameNode> voiNodeEnum = null;
            VOIContourNode currentVOINode = null;
            Enumeration<TreeNode> voiFrameEnum = null;

            Vector<TreePath> treePaths = new Vector<TreePath>();

            TreeNode tempNode = null;

            // iterate through all VOIs
            while (e.hasMoreElements()) {
                tempVOI = e.nextElement();

                // create VOI group node (for VOI)
                currentNode = new VOIGroupNode(tempVOI,image.getExtents());

                // check to see if the current VOI is the VOI shown in this dialog
                // or if the VOI isActive (can have multiple selections on tree)
                if (tempVOI.isActive())
                {

                    // add a new tree path so the VOI node is selected
                    treePaths.addElement(new TreePath(new Object[] { root, currentNode }));

                    curves = tempVOI.getCurves();

                    // look through curves to find which of the VOI's VOIBases (contours etc)
                    // are active
                    voiEnum = curves.elements();
                    while (voiEnum.hasMoreElements()) {
                        voiBase = voiEnum.nextElement();

                        // check to see if the VOIBase is active
                        if (voiBase.isActive())
                        {

                        	voiFrameEnum = currentNode.children();

                            while (voiFrameEnum.hasMoreElements()) {

                                tempNode = voiFrameEnum.nextElement();

                                if (tempNode instanceof VOIOrientationNode) {

                                    voiNodeEnum = (Enumeration<VOIFrameNode>) tempNode.children();

                                    // find the child that matches this selected contour
                                    while (voiNodeEnum.hasMoreElements()) {
                                        
                                        VOIFrameNode currentFrameNode = voiNodeEnum.nextElement();
                                        Enumeration<TreeNode> voiFrameEnum2 = currentFrameNode.children();
                                        
                                        // find the child that matches this selected contour
                                        while (voiFrameEnum2.hasMoreElements()) {
                                            currentVOINode = (VOIContourNode) voiFrameEnum2.nextElement();

                                            if (currentVOINode.getVOI().equals(voiBase)) {
                                                treePaths.addElement(new TreePath(new Object[] {
                                                        root, currentNode, tempNode, currentFrameNode,
                                                        currentVOINode
                                                }));
                                            }
                                        }
                                    }

                                }
                            }

                        }
                    }
                }

                root.add(currentNode);
            }
            voiModel.reload();

            if (treePaths.size() > 0) {
                TreePath[] tPaths = new TreePath[treePaths.size()];

                for (int i = 0; i < tPaths.length; i++) {
                    tPaths[i] = treePaths.elementAt(i);
                }
                voiTree.setSelectionPaths(tPaths);

                for (int i = 0; i < tPaths.length; i++) {
                    voiTree.expandPath( tPaths[i] );
                }
            } else {
                TreePath path = new TreePath(root);
                voiTree.setSelectionPath(path);
                voiTree.expandPath(path);
            }
            voiTreePane.validate();
        }

        updateTree = false;
    }
    
    
    public void setCheckboxBoundingBox(boolean flag) {
    	checkboxBoundingBox.setSelected(flag);
    }
    

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Does nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Does nothing.
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Pick up the selected color and call method to change the VOI color.
     */
    class OkColorListener implements ActionListener {

        /**
         * Get color from chooser and set button and VOI color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();
            colorButton.setBackground(color);
            colorVOI = color;
            voi.setColor(colorVOI);
            voi.update();
            updateVOIPanel(voi, image);
            updateTree();
            image.notifyImageDisplayListeners(null, true);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class VOITreePopup extends JPanel implements ActionListener, PopupMenuListener, MouseListener {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 4967124790752630122L;

        /** DOCUMENT ME! */
        private JMenu contourOrderSubMenu;

        /** DOCUMENT ME! */
        private JMenu editSubMenu;

        /** DOCUMENT ME! */
        private JMenu graphSubMenu;

        /** DOCUMENT ME! */
        private JMenu groupSubMenu;

        /** DOCUMENT ME! */
        private JMenuItem itemClose;

        /** DOCUMENT ME! */
        private JCheckBoxMenuItem itemShowVOIName;

        /** DOCUMENT ME! */
        private JMenu orderSubMenu;

        /** DOCUMENT ME! */
        private JMenu propSubMenu;

        /** DOCUMENT ME! */
        private JPopupMenu voiPopup;

        /**
         * Creates a new VOITreePopup object.
         */
        public VOITreePopup() {

            try {
                voiPopup = new JPopupMenu();

                orderSubMenu = ViewMenuBuilder.buildMenu("VOI Order", 0, false);
                contourOrderSubMenu = ViewMenuBuilder.buildMenu("Contour Order", 0, false);
                editSubMenu = ViewMenuBuilder.buildMenu("Edit", 0, false);
                propSubMenu = ViewMenuBuilder.buildMenu("Propagate", 0, false);
                ViewMenuBuilder.buildMenuItem("Show VOI Graph", "ShowGraph", 0,
                        voiHandler, null, false);
                ViewMenuBuilder.buildMenuItem("Point area average intensities", "ShowPAIIDialog",
                                                                   0, voiHandler, null, false);
                itemShowVOIName = ViewMenuBuilder.buildCheckBoxMenuItem("Show VOI name", "ShowName",
                        voiHandler,
                                                                        Preferences.is(Preferences.PREF_SHOW_VOI_NAME));

                graphSubMenu = ViewMenuBuilder.buildMenu("Graph", 0, false);
                itemClose = ViewMenuBuilder.buildMenuItem("Close VOI (polyline->polygon)", "closeVOI", 0,
                        voiHandler, null, false);

                groupSubMenu = ViewMenuBuilder.buildMenu("Group", 0, false);

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: VOITreePopup Constructor");

                return;
            }

            // Graph Submenu
            graphSubMenu.add(ViewMenuBuilder.buildMenuItem("Boundary intensity", "boundaryIntensity", 0,
                    voiHandler, null, false));

            if ((image.getNDims() == 3) || (image.getNDims() == 4)) {
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Total Intensity", "totalIntensity", 0, this, null,
                                                               false));
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Average Intensity", "avgIntensity", 0, this, null,
                                                               false));
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Total Intensity with Threshold",
                                                               "totalIntensityThreshold", 0, this, null, false));
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Average Intensity with Threshold",
                                                               "avgIntensityThreshold", 0, this, null, false));
            }

            // Grouping menu
            groupSubMenu.add(ViewMenuBuilder.buildMenuItem("Group VOIs", "GroupVOIs", 0, voiHandler, null, false));
            groupSubMenu.add(ViewMenuBuilder.buildMenuItem("Ungroup VOIs", "UngroupVOIs", 0,
                    voiHandler, null, false));


            // Order submenu
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Bring VOI to Front", "BringToFront", 0,
                    voiHandler, "front.gif", true));
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Send VOI to Back", "SendToBack", 0,
                    voiHandler, "back.gif", true));
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Bring VOI Forward", "BringForward", 0,
                    voiHandler, "forward.gif", true));
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Send VOI Backward", "SendBackward", 0,
                    voiHandler, "backward.gif", true));

            // Contour order submenu
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem("Bring Contour to Front", "BringContourToFront", 0,
                    voiHandler, "front.gif", true));
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem("Send Contour to Back", "SendContourToBack", 0,
                    voiHandler, "back.gif", true));
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem("Bring Contour Forward", "BringContourForward", 0,
                    voiHandler, "forward.gif", true));
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem("Send Contour Backward", "SendContourBackward", 0,
                    voiHandler, "backward.gif", true));

            // Edit submenu
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Delete", "deleteVOI", 0, voiHandler,
                                                          "delete.gif", true));
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Cut", "cutVOI", 0, voiHandler,
                                                          "cutpaint.gif", true));
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Copy", "copyVOI", 0, voiHandler,
                                                          "copypaint.gif", true));
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Paste", "pasteVOI", 0, voiHandler,
                                                          "pastepaint.gif", true));

            // propagate submenu
            propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Next Slice", "PropVOIUp", 0,
                    voiHandler, "voipropu.gif", true));
            propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Previous Slice", "PropVOIDown", 0,
                    voiHandler, "voipropd.gif", true));
            propSubMenu.add(ViewMenuBuilder.buildMenuItem("To All Slices", "PropVOIAll", 0,
                    voiHandler, null, true));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) { }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void mouseClicked(MouseEvent event) {
            checkPopup(event);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void mouseEntered(MouseEvent event) { }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void mouseExited(MouseEvent event) { }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void mousePressed(MouseEvent event) {
            checkPopup(event);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void mouseReleased(MouseEvent event) {
            checkPopup(event);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void popupMenuCanceled(PopupMenuEvent event) {
            // Preferences.debug("Popup menu will be visible!");
        }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {
            // Preferences.debug("Popup menu will be invisible!");
        }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
            // Preferences.debug("Popup menu will be visible!");
        }

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        private void checkPopup(MouseEvent event) {

            if (event.isPopupTrigger()) {

                int voiGrouping = getSelectedGrouping();

                if (voiGrouping != -1) {
                    // itemShowVOIName.setSelected(Preferences.is(Preferences.
                    // PREF_SHOW_VOI_NAME));

                    initToVOIType(voiGrouping);

                    voiPopup.show(voiTree, event.getX(), event.getY());
                } else {
                    //System.err.println("VOIs are of different types, select only like-contours...");
                }
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private int getSelectedGrouping() {

            int numOpenContours = 0;
            int numClosedContours = 0;
            int numLines = 0;
            int numPoints = 0;

            TreePath[] tPaths = voiTree.getSelectionPaths();
            TreePath currentPath = null;
            Object[] currentObjects = null;
            VOIBase currentVOIBase = null;

            int type = 0;

            if ((tPaths != null) && (tPaths.length > 0)) {

                for (int idx = 0; idx < tPaths.length; idx++) {

                    currentPath = tPaths[idx];
                    currentObjects = currentPath.getPath();

                    if (currentObjects != null) {

                        // look at the final object in path of current TreePath
                        // to see if it is a VOINode
                        if (currentObjects[currentObjects.length - 1] instanceof VOIContourNode) {

                            currentVOIBase = ((VOIContourNode) currentObjects[currentObjects.length - 1]).getVOI();

                            if (currentVOIBase instanceof VOIContour) {

                                if (((VOIContour) currentVOIBase).isClosed()) {
                                    numClosedContours++;
                                    type = VOI.CONTOUR;
                                } else {
                                    numOpenContours++;
                                    type = VOI.POLYLINE;
                                }

                            } else if (currentVOIBase instanceof VOIPoint) {
                                numPoints++;
                                type = VOI.POINT;
                            } else if (currentVOIBase instanceof VOILine) {
                                numLines++;
                                type = VOI.LINE;
                            }

                        } // otherwise check to see if a VOIGroupNode is selected
                        else if (currentObjects[currentObjects.length - 1] instanceof VOIGroupNode) {
                            // do something
                        }
                    }
                }
            }
            // System.err.println("Num open contours: " + numOpenContours);
            // System.err.println("Num closed contours: " + numClosedContours);
            // System.err.println("Num pts: " + numPoints);
            // System.err.println("Num lines: " + numLines);


            int numDiff = 0;

            if (numOpenContours > 0) {
                numDiff++;
            }

            if (numClosedContours > 0) {
                numDiff++;
            }

            if (numPoints > 0) {
                numDiff++;
            }

            if (numLines > 0) {
                numDiff++;
            }

            if (numDiff != 1) {
                return -1;
            }

            return type;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  type  DOCUMENT ME!
         */
        private void initToVOIType(int type) {

            switch (type) {

                case VOI.CONTOUR:
                    voiPopup.removeAll();
                    voiPopup.add(groupSubMenu);
                    voiPopup.addSeparator();
                    voiPopup.add(orderSubMenu);
                    voiPopup.add(contourOrderSubMenu);
                    voiPopup.add(editSubMenu);
                    voiPopup.add(propSubMenu);
                    voiPopup.add(itemShowVOIName);
                    break;

                case VOI.POLYLINE:
                    voiPopup.removeAll();
                    voiPopup.add(groupSubMenu);
                    voiPopup.addSeparator();
                    voiPopup.add(orderSubMenu);
                    voiPopup.add(contourOrderSubMenu);
                    voiPopup.add(editSubMenu);
                    voiPopup.add(propSubMenu);
                    voiPopup.add(itemShowVOIName);
                    voiPopup.add(itemClose);
                    break;

                case VOI.POINT:
                    voiPopup.removeAll();
                    voiPopup.add(groupSubMenu);
                    voiPopup.addSeparator();
                    voiPopup.add(itemShowVOIName);


            }
        }
    }


    /**
     * DOCUMENT ME!
     */
    private class VOITreeRenderer extends DefaultTreeCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -4107179145193872844L;

        /**
         * DOCUMENT ME!
         *
         * @param   tree      DOCUMENT ME!
         * @param   value     DOCUMENT ME!
         * @param   sel       DOCUMENT ME!
         * @param   expanded  DOCUMENT ME!
         * @param   leaf      DOCUMENT ME!
         * @param   row       DOCUMENT ME!
         * @param   hasFocus  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
                                                      boolean leaf, int row, boolean hasFocus) {

            super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

            if (value instanceof VOIGroupNode) {

                setIcon(null);

                int type = ((VOIGroupNode) value).getVOIgroup().getCurveType();

                Icon typeIcon = null;

                switch (type) {

                    case VOI.POLYLINE:
                        typeIcon = ICON_POLYLINE;
                        break;

                    case VOI.CONTOUR:
                        typeIcon = ICON_POLYGON;
                        break;

                    case VOI.LINE:
                        typeIcon = ICON_LINE;
                        break;

                    case VOI.POINT:
                        typeIcon = ICON_POINT;
                        break;

                    case VOI.PROTRACTOR:
                        typeIcon = ICON_PROTRACTOR;
                        break;

                    default:
                        setIcon(null);
                }

                int rgb = ((VOIGroupNode) value).getVOIgroup().getColor().getRGB();
                int black = Color.black.getRGB();

                if (typeIcon != null) {
                    ImageIcon ico = (ImageIcon) typeIcon;
                    int imageWidth = ico.getIconWidth();
                    int imageHeight = ico.getIconHeight();

                    int[] pixels = new int[imageWidth * imageHeight];
                    PixelGrabber pg = new PixelGrabber(ico.getImage(), 0, 0, imageWidth, imageHeight, pixels, 0,
                                                       imageWidth);

                    try {
                        pg.grabPixels();
                    } catch (InterruptedException e) {
                        Preferences.debug("JIMI: Interrupted waiting for pixels!" + "\n");

                        return null;
                    }

                    BufferedImage image2 = new BufferedImage(ico.getIconWidth() + 15, ico.getIconHeight(),
                                                             BufferedImage.TYPE_INT_ARGB);

                    for (int y = 0; y < imageHeight; y++) {

                        for (int x = 0; x < imageWidth; x++) {
                            image2.setRGB(x, y, pixels[(y * imageWidth) + x]);
                        }
                    }


                    // draw black border around color box
                    for (int i = ico.getIconWidth() + 3; i < (image2.getWidth() - 3); i++) {

                        for (int j = 0; j < 2; j++) {
                            image2.setRGB(i, j, black);
                        }

                        for (int j = image2.getHeight() - 2; j < image2.getHeight(); j++) {
                            image2.setRGB(i, j, black);
                        }
                    }

                    for (int j = 0; j < image2.getHeight(); j++) {

                        for (int i = ico.getIconWidth() + 3; i < (ico.getIconWidth() + 5); i++) {
                            image2.setRGB(i, j, black);
                        }

                        for (int i = image2.getWidth() - 5; i < (image2.getWidth() - 3); i++) {
                            image2.setRGB(i, j, black);
                        }

                    }

                    // draw color
                    for (int i = ico.getIconWidth() + 5; i < (image2.getWidth() - 5); i++) {

                        for (int j = 2; j < (image2.getHeight() - 2); j++) {
                            image2.setRGB(i, j, rgb);
                        }
                    }

                    setIcon(new ImageIcon(image2));

                } else {
                    BufferedImage image = new BufferedImage(9, 26, BufferedImage.TYPE_INT_ARGB);

                    for (int i = 2; i < 7; i++) {

                        for (int j = 4; j < 24; j++) {
                            image.setRGB(i, j, rgb);
                        }
                    }

                    // draw black border
                    for (int i = 0; i < 9; i++) {

                        for (int j = 2; j < 4; j++) {
                            image.setRGB(i, j, black);
                        }

                        for (int j = 24; j < 26; j++) {
                            image.setRGB(i, j, black);
                        }
                    }

                    for (int j = 2; j < 26; j++) {

                        for (int i = 0; i < 2; i++) {
                            image.setRGB(i, j, black);
                        }

                        for (int i = 7; i < 9; i++) {
                            image.setRGB(i, j, black);
                        }
                    }

                    setIcon(new ImageIcon(image));
                }

                // ImageIcon ico = new ImageIcon(image);
                // setIcon(ico);


                setBorder(null);
                setFont(MipavUtil.font12);
            } else if (value instanceof VOIFrameNode) {
                setBorder(frameBorder);
                setFont(MipavUtil.font10);
                setIcon(null);

            } else if (value instanceof VOIContourNode) {
                setIcon(null);
                setBorder(null);
                setFont(MipavUtil.font12);
            } else if (value instanceof VOIOrientationNode) {
            	if(((VOIOrientationNode)value).getName().equals("X Plane")) {
            		setIcon(ICON_X_AXIS);
            	}else if(((VOIOrientationNode)value).getName().equals("Y Plane")) {
            		setIcon(ICON_Y_AXIS);
            	}else if(((VOIOrientationNode)value).getName().equals("Z Plane")) {
            		setIcon(ICON_Z_AXIS);
            	}
            	
                setFont(MipavUtil.font12);
                setBorder(null);
            } else {

                // setForeground(Color.white);
                setIcon(ICON_MEDICAL_FRAME);
                setFont(MipavUtil.font12);
                setBorder(null);
                // setIcon(null);
            }

            return this;
        }
    }


	@Override
	protected void callAlgorithm() {
		if(processList != null && processList.length > 0 && processList[0].size() > 0) {
			callVOIAlgo(processList[0], AlgorithmVOIProps.PROCESS_PER_VOI, isRunInSeparateThread());
		} else if(processList != null && processList.length > 1 && processList[1].size() > 0) {
			callVOIAlgo(processList[1], AlgorithmVOIProps.PROCESS_PER_VOI, isRunInSeparateThread());
		} else {
			VOIVector wholeImageVec = new VOIVector();
	        VOI wholeImage = new VOI((short) 0, "wholeImage", VOI.CONTOUR, 0.0f);
	        int slice = image.getParentFrame().getComponentImage().getSlice();
	        int xDim = image.getExtents()[0]-2; //TODO: Change to xDim-1 when bug 539 is fixed
	        int yDim = image.getExtents()[1]-2; //TODO: Change to xDim-1 when bug 539 is fixed
	        
            Vector3f orig = new Vector3f(0, 0, slice);
            Vector3f cornerX = new Vector3f(xDim-2, 0, slice);
            Vector3f cornerXY = new Vector3f(xDim-2, yDim-2, slice);
            Vector3f cornerY = new Vector3f(0, yDim-2, slice);
            Vector<Vector3f> points = new Vector<Vector3f>();
            points.add(orig);
            points.add(cornerX);
            points.add(cornerXY);
            points.add(cornerY);
            VOIContour wholeSlice = new VOIContour(true, true, points);
            wholeImage.importCurve(wholeSlice);
            wholeSlice.setGroup(wholeImage);
            wholeSlice.setProcess(true);

	        wholeImageVec.add(wholeImage);
	        
	        callVOIAlgo(wholeImageVec, AlgorithmVOIProps.PROCESS_PER_VOI, isRunInSeparateThread());
		}
	}

	/**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        setScriptRunning(true);
        image = scriptParameters.retrieveInputImage();

        ViewUserInterface userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

//        final VOIVector voiVec = image.getVOIs();
//
//        if (voiVec.size() < 1) {
//            this.dispose();
//
//            return;
//        }
//
//        for (int i = 0; i < voiVec.size(); i++) {
//            voiVec.VOIAt(i).setAllActive(true);
//        }

        JList selectedList = new JList();
        selectedList.setListData(image.getVOIs());

        // createDialog(userInterface, true);

        String rangeFlag = scriptParameters.getParams().getString("do_use_exclusion_range");

        scriptRange = RangeType.valueOf(rangeFlag);
        
        if (scriptRange != RangeType.NO_RANGE) {
            if (image.isColorImage()) {
                rangeMinimumR = scriptParameters.getParams().getFloat("exclusion_range_minr");
                rangeMaximumR = scriptParameters.getParams().getFloat("exclusion_range_maxr");
                rangeMinimumG = scriptParameters.getParams().getFloat("exclusion_range_ming");
                rangeMaximumG = scriptParameters.getParams().getFloat("exclusion_range_maxg");
                rangeMinimumB = scriptParameters.getParams().getFloat("exclusion_range_minb");
                rangeMaximumB = scriptParameters.getParams().getFloat("exclusion_range_maxb");
    
                for (int i = 0; i < selectedList.getModel().getSize(); i++) {
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreR(rangeMaximumR);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreR(Float.MAX_VALUE);
                    }
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreR(rangeMinimumR);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreR( -Float.MAX_VALUE);
                    }
                    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreG(rangeMaximumG);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreG(Float.MAX_VALUE);
                    }
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreG(rangeMinimumG);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreG( -Float.MAX_VALUE);
                    }
                    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreB(rangeMaximumB);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreB(Float.MAX_VALUE);
                    }
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreB(rangeMinimumB);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreB( -Float.MAX_VALUE);
                    }
                }    
            } // if (image.isColorImage())
            else { // black and white image
                rangeMinimum = scriptParameters.getParams().getFloat("exclusion_range_min");
                rangeMaximum = scriptParameters.getParams().getFloat("exclusion_range_max");
    
                for (int i = 0; i < selectedList.getModel().getSize(); i++) {
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(rangeMaximum);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(Float.MAX_VALUE);
                    }
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore(rangeMinimum);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore( -Float.MAX_VALUE);
                    }
                }
            } // else black and white image
        }

        boolean[] toCompute = scriptParameters.getParams().getList("stat_checklist").getAsBooleanArray();
        for(int i=0; i<toCompute.length; i++) {
        	toCompute[i] = true;
        }
        
        listPanel.setSelectedList(toCompute);

        doAllVolumes = scriptParameters.getParams().getBoolean("do_all_volumes");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("stat_checklist", listPanel.getSelectedList()));

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_all_volumes", doAllVolumes));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_exclusion_range", excluder.getRangeFlag().name()));

        if (excluder.getRangeFlag() != RangeType.NO_RANGE) {
            if (image.isColorImage()) {
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_minr", excluder.getLowerBoundR()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_maxr", excluder.getUpperBoundR()
                                .floatValue()));    
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_ming", excluder.getLowerBoundG()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_maxg", excluder.getUpperBoundG()
                                .floatValue()));   
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_minb", excluder.getLowerBoundB()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_maxb", excluder.getUpperBoundB()
                                .floatValue()));    
            }
            else {
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_min", excluder.getLowerBound()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_max", excluder.getUpperBound()
                                .floatValue()));
            }
        }
    }

}
