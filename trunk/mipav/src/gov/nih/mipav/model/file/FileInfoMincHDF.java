package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;

import javax.swing.tree.*;

import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.HObject;

public class FileInfoMincHDF extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
   
	
	private transient DefaultMutableTreeNode dimensionNode;
	private transient DefaultMutableTreeNode informationNode;
	
	protected int[] axisOrientation = { ORI_UNKNOWN_TYPE, ORI_UNKNOWN_TYPE, ORI_UNKNOWN_TYPE };
	
	private double [] validRange = null;
	
	private transient Hashtable dicomTable = null;
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * MINC file information constructor.
     *
     * @param  name       file name
     * @param  directory  file directory
     * @param  format     format (in this case, MINC)
     */
    public FileInfoMincHDF(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns all of the dicom-converted tags in the minc header as a tag-value hashtable.
     *
     * @return  a tag-value hashtable
     */
    public Hashtable convertTagsToTable() {
    	Hashtable table = new Hashtable();
    	
    	
    	
    	return table;
    }
    
    /**
     * Displays important information about the image.
     *
     * @param  dlog    where to display the info
     * @param  matrix  the transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        
        if (informationNode != null) {
        	try {
        		parseNodes(informationNode, dialog);
        	} catch (Exception e) {
        		e.printStackTrace();
        	}
        }
        
    }

    /**
     * Recursively parse and display (to JDialogText) the nodes
     * @param node HDF tree node
     * @param dialog dialog to display the information
     * @throws Exception
     */
    private void parseNodes(DefaultMutableTreeNode node, JDialogText dialog) throws Exception {
    	long [] dataDims;
    	boolean isDicom = false;
    	if (node.isLeaf()) {
    		dialog.append("\n");
    		HObject userObject = (HObject)node.getUserObject();
    		
    		String nodeName = userObject.toString();
    		String dicomGroup = null;
    	//	dicom_0x0008
    		if (nodeName.startsWith("dicom_")) {
    			isDicom = true;
    			dicomGroup = nodeName.substring(8);
    		} else {
    			dialog.append(userObject + "\n");
    		}
    		List metaData = userObject.getMetadata();
    		Iterator it = metaData.iterator();
    		while(it.hasNext()) {
    			Attribute currentAttribute = (Attribute)it.next();
    			dataDims = currentAttribute.getDataDims();
    			String name = currentAttribute.getName();
    			if (!name.equals("varid") && !name.equals("vartype") &&
    					!name.equals("version")) {
    				
    				if (!isDicom) {
    					dialog.append("\t" + currentAttribute.getName());
    				} else {
    					dialog.append("(" + dicomGroup + "," + name.substring(5) + ") - ");
    				}
    				Object value = currentAttribute.getValue();
    				for (int i = 0; i < dataDims.length; i++) {
    					//System.err.print(", " + dataDims[i]);
    					if (value instanceof String[]) {
    						if (isDicom) {
    							FileDicomKey tagKey = new FileDicomKey(dicomGroup + "," + name.substring(5));

                                FileDicomTagInfo info = DicomDictionary.getInfo(tagKey);
                                dialog.append(info.getName() + " :\t");
    						}
    						
    						dialog.append("\t" + ((String[])value)[i]);
    					} else if (value instanceof float[]) {
    						dialog.append("\t" + ((float[])value)[i]);
    					} else if (value instanceof double[]) {
    						dialog.append("\t" + ((double[])value)[i]);
    					} else if (value instanceof int[]) {
    						dialog.append("\t" + ((int[])value)[i]);
    					} else if (value instanceof short[]) {
    						dialog.append("\t" + ((short[])value)[i]);
    					}
    				}
    				dialog.append("\n");
    			//System.err.println("");
    			//System.err.println(currentAttribute.getValue());
    			}
    		}
    		
    	} else {
    	
    		//recursively calls parseHDFHeader to reach every non-leaf
    		for (int i = 0; i < node.getChildCount(); i++) {
    			parseNodes((DefaultMutableTreeNode)node.getChildAt(i), dialog);
    		}
    	}
    }
  
    /**
     * Sets the dimension node (xspace, yspace and optional zspace nodes)
     * @param dimNode the dimension node
     */
    public void setDimensionNode(DefaultMutableTreeNode dimNode) {
    	this.dimensionNode = dimNode;
    }
    
    /**
     * Returns the dimension node (only 1 exists at fileinfo[0] position
     * @return the dimension node
     */
    public DefaultMutableTreeNode getDimensionNode() {
    	return this.dimensionNode;
    }
    
    /**
     * Sets the information node (includes dicom tags and patient info
     * @param infoNode the information node
     */
    public void setInfoNode(DefaultMutableTreeNode infoNode) {
    	this.informationNode = infoNode;
    }

    /**
     * Returns the information node with dicom/patient info
     * @return the information node
     */
    public DefaultMutableTreeNode getInfoNode() {
    	return this.informationNode;
    }
    
    /**
     * Sets the image modality based on the.
     */
    public void setModality(String modality) {

    	  if (modality.equals("PET__")) {
              setModality(FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY);
          } else if (modality.equals("MRI__")) {
              setModality(FileInfoBase.MAGNETIC_RESONANCE);
          } else if (modality.equals("SPECT")) {
              setModality(FileInfoBase.SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY);
          } else if (modality.equals("GAMMA")) {
              // setModality(FileInfoBase.);
          } else if (modality.equals("MRS__")) {
              setModality(FileInfoBase.MAGNETIC_RESONANCE_SPECTROSCOPY);
          } else if (modality.equals("MRA__")) {
              setModality(FileInfoBase.MAGNETIC_RESONANCE_ANGIOGRAPHY);
          } else if (modality.equals("CT___")) {
              setModality(FileInfoBase.COMPUTED_TOMOGRAPHY);
          } else if (modality.equals("DSA__")) {
              // setModality(FileInfoBase.);
          } else if (modality.equals("DR___")) {
              setModality(FileInfoBase.DIGITAL_RADIOGRAPHY);
          }
    }
    
    public void setValidRange(double[] valid_range) {
    	this.validRange = valid_range;
    }
    
    public double [] getValidRange() {
    	return this.validRange;
    }
    
    public void createDicomTable() {
    	this.dicomTable = new Hashtable();
    }
    
    public void setDicomTable(Hashtable dTable) {
    	this.dicomTable = dTable;
    }
    
    public Hashtable getDicomTable() {
    	return this.dicomTable;
    }
    
    public final double[] getConvertStartLocationsToDICOM(double[] step, double[][]cosines, 
    		boolean[]isCentered, int slice) {
        double x = 0;
        double y = 0;
        double z = 0;;

        // System.out.println("convert: begin res:\t" + xRes + " " + yRes + " " + zRes);

        double[] startLocs = new double[getExtents().length];

        if (startLocs.length == 2) {
            startLocs[0] = x;
            startLocs[1] = y;
        } else {
            startLocs[0] = x;
            startLocs[1] = y;
            startLocs[2] = z + (step[2] * slice);
        }

        // System.out.println("convert: locs:\t" + startLocs[0] + " " + startLocs[1] + " " + startLocs[2]);

        TransMatrix matrix = new TransMatrix(getExtents().length + 1);
        matrix.identity();

        for (int i = 0; i < 3; i++) {
        	for (int j = 0; j < cosines[i].length; j++) {
        		matrix.set(i, j, cosines[i][j]);
        	}
        	
        }
       
        // System.out.println("convert: matrix:\t" + matrix.matrixToString(24, 16));

        matrix.invert();

        // System.out.println("convert: invmat:\t" + matrix.matrixToString(24, 16));

        double[] transformedPt = new double[getExtents().length];

        if (isCentered[0]) {
            startLocs[0] -= (step[0] / 2);
        }

        if (isCentered[1]) {
            startLocs[1] -= (step[1] / 2);
        }

        // mni seems not to adjust the zstart by the zstep even when xspace has the attrib alignment=centre
        /*if (isZCentered) {
         *  startLocs[2] -= (step[2] / 2);}*/

        if (getExtents().length == 2) {
            matrix.transform(startLocs[0], startLocs[1], transformedPt);
        } else if (getExtents().length == 3) {
            matrix.transform(startLocs[0], startLocs[1], startLocs[2], transformedPt);
        }

        // System.out.println("convert: trans:\t" + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2]);

        if (startLocs.length == 3) {

            if (getImageOrientation() == FileInfoBase.SAGITTAL) {
                transformedPt[0] = -transformedPt[0];
                transformedPt[2] = -transformedPt[2];
            } else if (getImageOrientation() == FileInfoBase.AXIAL) {
                transformedPt[0] = -transformedPt[0];
                transformedPt[1] = -transformedPt[1];
            } else if (getImageOrientation() == FileInfoBase.CORONAL) {
                transformedPt[0] = -transformedPt[0];
                transformedPt[2] = -transformedPt[2];
            }
        }

      // System.err.println("convert: result[" + slice + "]:\t" + transformedPt[0] + " " + transformedPt[1] + " " +
     //   transformedPt[2]);

        
        
        return transformedPt;
    }
    
    /**
     * Sets start locations of each axis.
     *
     * @param  origin  the image origin
     */
    public final void setStartLocations(double[] origin) {

        if (origin.length != 3) {
            Preferences.debug("Start locations array must be of length 3.\n");

            return;
        }

        float[] fOrigin = new float[origin.length];

        for (int i = 0; i < origin.length; i++) {
            fOrigin[i] = (float) origin[i];
        }

        super.setOrigin(fOrigin);
    }
    
    /**
     * In MINC images, "real" values for pixels are calculated by taking the given image min and image max and rescaling
     * the data accordingly. Image min and image max are given per slice.
     * @param rescaleIntercept
     * @param rescaleSlope
     * @param imageMax
     * @param imageMin
     * @param validMax
     * @param validMin
     */
    public static void calculateRescaleIntercept(double[] rescaleIntercept, double[] rescaleSlope, 
    		double[] imageMax, double [] imageMin,
    		double [] valid_range) {

        try {

            for (int i = 0; i < rescaleSlope.length; i++) {
                rescaleSlope[i] = FileInfoMinc.calculateSlope(imageMax[i], imageMin[i], valid_range[1], valid_range[0]);
                rescaleIntercept[i] = FileInfoMinc.calculateIntercept(imageMin[i], rescaleSlope[i], valid_range[0]);
            }
        } catch (ArrayIndexOutOfBoundsException error) {

            for (int i = 0; i < rescaleSlope.length; i++) {
                rescaleSlope[i] = 1.0;
                rescaleIntercept[i] = 0.0;
            }
        }
    }
}
