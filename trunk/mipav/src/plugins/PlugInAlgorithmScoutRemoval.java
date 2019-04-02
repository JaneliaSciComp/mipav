//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileNIFTI;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import java.io.File;
import java.io.IOException;





public class PlugInAlgorithmScoutRemoval extends AlgorithmBase {
    
    private String fileDirectory;
    /**
     * Constructor.
     *
     */
    public PlugInAlgorithmScoutRemoval(String fileDirectory) {
    	this.fileDirectory = fileDirectory;
    }

    
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	int extents3D[] = new int[3];
    	String patientID;
    	int numberValidDirectories;
    	boolean useLast3Numbers;
    	String last3Numbers;
    	String directoryName;
    	int len;
    	String oneFileList[] = new String[1];
    	boolean performSort = true;
    	ModelImage image2D;
    	FileInfoDicom dicomInfo;
    	FileDicomTagTable tagTable;
    	String orientation;
    	int index1;
    	int index2;
    	double coord[] = new double[3];
    	int selectedFileNumber;
    	String fileList[];
    	int index;
    	String selectedFileName;
    	ModelImage image3D;
    	int i;
    	
    	
    	FileIO fileIO = new FileIO(); 
    	fileIO.setQuiet(false);
    	System.out.println("fileDirectory = " + fileDirectory);
    	File folder = new File(fileDirectory);
    	for (File fileEntry : folder.listFiles()) {
    		if (fileEntry.isDirectory()) {
    			patientID = fileEntry.getName();
    			for (File fileEntry2: fileEntry.listFiles()) {
    				if (fileEntry2.isDirectory() && fileEntry2.getName().equals("anon")) {
    					numberValidDirectories = 0;
    					for (File fileEntry3: fileEntry2.listFiles()) {
    						if (fileEntry3.isDirectory() && fileEntry3.listFiles().length > 1) {
    						    numberValidDirectories++;	
    						}
    					}
    					useLast3Numbers = numberValidDirectories > 1;
    					for (File fileEntry3: fileEntry2.listFiles()) {
    						if (fileEntry3.isDirectory() && fileEntry3.listFiles().length > 1) {
    						    directoryName = fileEntry3.getName();
    						    fileIO.setFileDir(fileDirectory + File.separator + patientID + File.separator + "anon" 
    						    		+ File.separator + directoryName + File.separator);
    						    len = directoryName.length();
    						    last3Numbers = directoryName.substring(len-3, len);
    						    for (File fileEntry4: fileEntry3.listFiles()) {
    						    	if (!fileEntry4.isDirectory()) {
    						    		selectedFileName = fileEntry4.getName();
    						    	    oneFileList[0] = selectedFileName;	
    						    	    image2D = fileIO.readDicom(selectedFileName, oneFileList, performSort);
    						    	    dicomInfo = (FileInfoDicom) image2D.getFileInfo(0);
    						        	tagTable = dicomInfo.getTagTable();
    						        	if (tagTable.getValue("0020,0032") != null) {
    						            	// Image position patient
    						        		orientation = (String) tagTable.getValue("0020,0032");
    						                index1 = -1;
    						                index2 = -1;

    						                for (i = 0; i < orientation.length(); i++) {

    						                    if (orientation.charAt(i) == '\\') {

    						                        if (index1 == -1) {
    						                            index1 = i;
    						                        } else {
    						                            index2 = i;
    						                        }
    						                    }
    						                }

    						                coord[0] = Double.valueOf(orientation.substring(0, index1)).doubleValue();
    						                coord[1] = Double.valueOf(orientation.substring(index1 + 1, index2)).doubleValue();
    						                coord[2] = Double.valueOf(orientation.substring(index2 + 1)).doubleValue();
    						                if (((coord[0] == 0) || (coord[1] == 0) || (coord[2] == 0)) &&
    						                    ((coord[0] == 256) || (coord[1] == 256) || (coord[2] == 256)) &&
    						                    ((coord[0] == -256) || (coord[1] == -256) || (coord[2] == -256))) {
    						                	try {
    						                		fileEntry4.delete();
    						                	}
    						                	catch (SecurityException ex) {
    						                	     MipavUtil.displayError("Security exception trying to delete " + fileEntry.getName());
    						                	     setCompleted(false);
    						                	     return;
    						                	}
    						                }
    						                image2D.disposeLocal();
        						    	    image2D = null;
    						            }
    						            else {
    						            	MipavUtil.displayError("Tag (0020,0032) for Image Position Patient is null");
    						            	setCompleted(false);
    						            	return;
    						            }
    						    	} // if (!fileEntry4.isDirectory())
    						    } // for (File fileEntry4: fileEntry3.listFiles())
    						    selectedFileNumber = 0;
    						    for (File fileEntry4: fileEntry3.listFiles()) {
    						        if (!fileEntry4.isDirectory()) {
    						        	selectedFileNumber++;
    						        }
    						    }
    						    fileList = new String[selectedFileNumber];
    					    	index = 0;
    					    	for (File fileEntry4: fileEntry3.listFiles()) {
    						        if (!fileEntry4.isDirectory()) {
    						        	fileList[index++] = fileEntry4.getName();
    						        }
    						    }
    					    	selectedFileName = fileList[0];
    					    	image3D = fileIO.readDicom(selectedFileName, fileList, performSort);
    					    	image3D.calcMinMax();
    		                    extents3D = image3D.getExtents();
    					    	FileWriteOptions options = new FileWriteOptions(true);
    					        options.setFileType(FileUtility.NIFTI);
    					        File file = new File(fileDirectory + File.separator + "NIFTI" + File.separator);
    					        if (!file.exists()) {
    					        	try {
    					                file.mkdir();
    					        	}
    					        	catch (SecurityException ex) {
				                	     MipavUtil.displayError("Security exception trying to create " + fileDirectory + File.separator + "NIFTI" + File.separator);
				                	     setCompleted(false);
				                	     return;
				                	}
    					        }
    					        options.setFileDirectory(fileDirectory + File.separator + "NIFTI" + File.separator);
    					        if (useLast3Numbers) {
    					            options.setFileName(patientID + "_" + last3Numbers + ".nii");
    					        }
    					        else {
    					        	options.setFileName(patientID + ".nii");
    					        }
    					        options.setBeginSlice(0);
    					        options.setEndSlice(extents3D[2]-1);
    					        options.setOptionsSet(false);
    					        options.setSaveAs(true);
    					        FileNIFTI niftiFile;

    					        try { // Construct a new file object
    					            niftiFile = new FileNIFTI(options.getFileName(), options.getFileDirectory());
    					            niftiFile.writeImage(image3D, options);
    					            niftiFile.finalize();
    					            niftiFile = null;
    					        } catch (final IOException error) {

    					            MipavUtil.displayError("IOException on writing " + options.getFileName());

    					            error.printStackTrace();
    					            setCompleted(false);
    					            return;
    					        } catch (final OutOfMemoryError error) {

    					            MipavUtil.displayError("Out of memory error on writing " + options.getFileName());

    					            error.printStackTrace();
    					            setCompleted(false);
    					            return;
    					        }
    					    	image3D.disposeLocal();
    					    	image3D = null;
    					    }
    					}
    				}
    			}
    		}
    	}
    	
    	setCompleted(true); 
    } // end runAlgorithm()
    
    

//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

}
