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
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.file.FileAnalyze;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;
import java.util.Collection;
import java.util.Vector;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;




public class PlugInAlgorithmTSPAnalysis extends AlgorithmBase implements MouseListener {
    
    private int xDim;

    private int yDim;

    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;
    
    private Collection<ModelImage> resultImageList;
    
    private String pwiImageFileDirectory;
    
    // Thresholding to mask out image pixels not corresponding to brain tissue
    private int masking_threshold = 800;
    
    private double TSP_threshold = 0.8;
    
    private int TSP_iter = 4;
    
    private double Psvd = 0.1;
    
    private ModelImage pickImage;
    
    private final Lock accessLock = new ReentrantLock();
    private final Condition canProcessMouseClick = accessLock.newCondition();
	private int xS;
	private int yS;
    /**
     * Constructor.
     *
     */
    public PlugInAlgorithmTSPAnalysis(String pwiImageFileDirectory, int masking_threshold,
    		double TSP_threshold, int TSP_iter, double Psvd) {
        //super(resultImage, srcImg);
    	this.pwiImageFileDirectory = pwiImageFileDirectory;
    	this.masking_threshold = masking_threshold;
    	this.TSP_threshold = TSP_threshold;
    	this.TSP_iter = TSP_iter;
    	this.Psvd = Psvd;
    }

    
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int length;
    	int volume;
    	int dataSize;
    	int extents[] = new int[4];
    	float resolutions[] = new float[4];
    	int units[] = new int[4];
    	int extents3D[] = new int[3];
    	float resolutions3D[] = new float[3];
    	int units3D[] = new int[3];
    	String tDimString = null;
    	String t0String = null;
    	float t0;
    	String t1String = null;
    	float t1;
    	float delT;
    	int buffer[];
    	int data[][][][];
    	byte mask[][][][];
    	int brain_mask[][][][];
    	double temp_mean[];
    	int brain_mask_norm[][][][];
    	double dbuffer[];
    	int x, y, z, t;
    	int i,ii;
    	long sum;
    	int count;
    	// Remove hyphen from corr_map so MIPAV does not read corr_map and corr_map2 together as 1 file.
    	double corrmap[][][];
    	double corr_map2[][][];
    	double delay_map[][][];
    	double peaks_map[][][];
    	double temp[];
    	double maxTemp;
    	int maxIndex;
    	double cc;
    	int it;
    	int brain_mask2[][][][];
    	int brain_mask_norm2[][][][];
    	int A[];
    	int B[];
    	double maxPeak;
    	String delZString;
    	float delZ;
    	ModelImage corr_map2Image;
    	ModelImage corrmapImage;
    	ModelImage peaks_mapImage;
    	ModelImage delay_mapImage;
    	String TEString;
    	double TE;
    	int data_norm[][][][];
    	int peaks[][][];
    	int ttp[][][];
    	int minpeaks;
    	int minttp;
    	byte mask3D[][][];
    	double peaks_mean;
    	double diff;
    	double diff_squared_sum;
    	double peaks_std;
    	double peaks_threshold;
    	double autoaif[];
    	double minautoaif;
    	double S[];
    	double Ca[];
    	int sliceBuffer[];
    	int extents2D[];
    	
    	File folder = new File(pwiImageFileDirectory);
    	int selectedFileNumber = 0;
    	for (File fileEntry : folder.listFiles()) {
    		if (!fileEntry.isDirectory()) {
    			if (fileEntry.getName().length() > 2) {
    			    String startName = fileEntry.getName().substring(0,2);
    			    if (startName.equalsIgnoreCase("IM")) {
    			    	selectedFileNumber++;
    			    }
    			}
    		}
    	}
    	String fileList[] = new String[selectedFileNumber];
    	int index = 0;
    	for (File fileEntry : folder.listFiles()) {
    		if (!fileEntry.isDirectory()) {
    			if (fileEntry.getName().length() > 2) {
    			    String startName = fileEntry.getName().substring(0,2);
    			    if (startName.equalsIgnoreCase("IM")) {
    			    	fileList[index++] = fileEntry.getName();
    			    }
    			}
    		}
    	}
    	String selectedFileName = fileList[0];
    	FileIO fileIO = new FileIO(); 
    	fileIO.setFileDir(pwiImageFileDirectory + File.separator);
    	boolean performSort = true;
    	fileIO.setQuiet(false);
    	ModelImage image3D = fileIO.readDicom(selectedFileName, fileList, performSort);
    	image3D.calcMinMax();
    	//int imageIndex = 0;
    	//new ViewJFrameImage(vols2, null, new Dimension(610, 200 + (imageIndex++ * 20)));
    	int extents3Dorg[] = image3D.getExtents();
    	length = extents3Dorg[0] * extents3Dorg[1];
    	xDim = extents3Dorg[0];
    	yDim = extents3Dorg[1];
    	extents[0] = xDim;
    	extents3D[0] = xDim;
    	extents[1] = yDim;
    	extents3D[1] = yDim;
    	FileInfoDicom dicomInfo = (FileInfoDicom) image3D.getFileInfo(0);
    	FileDicomTagTable tagTable = dicomInfo.getTagTable();
    	if (tagTable.getValue("0018,0081") != null) {
        	// Echo time in milliseconds
            FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0081"));
            TEString = (String)tag.getValue(false);     
        }
        else {
        	MipavUtil.displayError("Tag (0018,0081) for Number of Echo Time TE is null");
        	setCompleted(false);
        	return;
        }
    	TE = Double.valueOf(TEString.trim()).doubleValue();
    	//System.out.println("TE = " + TE);
        if (tagTable.getValue("0020,0105") != null) {
        	// Number of temporal positions
            FileDicomTag tag = tagTable.get(new FileDicomKey("0020,0105"));
            tDimString = (String)tag.getValue(false);     
        }
        else {
        	MipavUtil.displayError("Tag (0020,0105) for Number of Temporal Positions is null");
        	setCompleted(false);
        	return;
        }
        tDim = Integer.valueOf(tDimString.trim()).intValue();
        extents[3] = tDim;
        zDim = extents3Dorg[2]/tDim;
        extents[2] = zDim;
        extents3D[2] = zDim;
        for (i = 0; i < 2; i++) {
        	resolutions[i] = image3D.getResolutions(0)[i];
        	resolutions3D[i] = resolutions[i];
        	//System.out.println("resolutions["+i+"] = " + resolutions[i]);
        }
        if (tagTable.getValue("0018,0088") != null) {
            // Spacing between slices
        	FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0088"));
        	delZString = (String)tag.getValue(false);
        }
        else {
        	MipavUtil.displayError("Tag (0018,0088) for Spacing between slices is null");
        	setCompleted(false);
        	return;
        }
        delZ = Float.valueOf(delZString.trim()).floatValue();
        //System.out.println("delZ = " + delZ);
        resolutions[2] = delZ;
        resolutions3D[2] = delZ;
        //System.out.println("zDim = " + zDim + " tDim = " + tDim);
        if (tagTable.getValue("0018,1060") != null) {
        	// Trigger time
        	FileDicomTag tag = tagTable.get(new FileDicomKey("0018,1060"));
        	t0String = (String)tag.getValue(false);
        }
        else {
        	MipavUtil.displayError("Tag (0018,1060) for Trigger Time is null");
        	setCompleted(false);
        	return;
        }
        t0 = Float.valueOf(t0String.trim()).floatValue();
        dicomInfo = (FileInfoDicom) image3D.getFileInfo(1);
        tagTable = dicomInfo.getTagTable();
        if (tagTable.getValue("0018,1060") != null) {
        	// Trigger time
        	FileDicomTag tag = tagTable.get(new FileDicomKey("0018,1060"));
        	t1String = (String)tag.getValue(false);
        }
        else {
        	MipavUtil.displayError("Tag (0018,1060) for Trigger Time is null");
        	setCompleted(false);
        	return;
        }
        t1 = Float.valueOf(t1String.trim()).floatValue();
        // Time in milliseconds
        delT = t1 - t0;
        //System.out.println("delT = " + delT);
        resolutions[3] = delT;
        for (i = 0; i < 3; i++) {
        	units[i] = Unit.MILLIMETERS.getLegacyNum();
        	units3D[i] = units[i];
        }
        units[3] = Unit.MILLISEC.getLegacyNum();
        volume = zDim * length;
        dataSize = volume * tDim;
        buffer = new int[dataSize];
    	try {
    		image3D.exportData(0,  dataSize, buffer);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on image3D.exportData");
    		setCompleted(false);
    		return;
    	}
    	image3D.disposeLocal();
    	image3D = null;
    	data = new int[xDim][yDim][zDim][tDim];
    	for (t = 0; t < tDim; t++) {
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					data[x][y][z][t] = buffer[x + y*xDim + t*length +z*tDim*length];
    				}
    			}
    		}
    	}
    	
    	// Start TSP processing
    	brain_mask = new int[xDim][yDim][zDim][tDim];
    	for (t = 0; t < tDim; t++) {
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					if (data[x][y][z][t] < masking_threshold) {
    						brain_mask[x][y][z][t] = 0;
    					}
    					else {
    						brain_mask[x][y][z][t] = data[x][y][z][t];
    					}
    				}
    			}
    		}
    	}
    	temp_mean = new double[tDim];
    	brain_mask_norm = new int[xDim][yDim][zDim][tDim];
    	// Normalize PWI by subtracting off pre_contrast (first) image
    	for (t = 0; t < tDim; t++) {
    	    for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
					brain_mask_norm[x][y][z][t] = brain_mask[x][y][z][t] - brain_mask[x][y][z][0];	
					}
				}
	    	}
    	}
    	// Loop over time dimension to calculate whole brain average perfusion time signal
    	for (t = 1; t < tDim; t++) {
    	    sum = 0;
    	    count = 0;
    	    for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
					    if (brain_mask_norm[x][y][z][t] != 0) {
					    	sum += brain_mask_norm[x][y][z][t];
					    	count++;
					    }
					}
				}
    	    }
    	    temp_mean[t] = (double)sum/(double)count;
    	} // for (t = 1; t < tDim; t++)
    	temp_mean[0] = 0;
    	
    	// Zero/Initialize output maps
        // TSP Correlation map with out AIF delay compensation
    	corrmap = new double[xDim][yDim][zDim];
    	// TSP Correlation map with AIF delay compensation
    	corr_map2 = new double[xDim][yDim][zDim];
    	// TSP Delay map considering temporal similarity with whole brain average
    	delay_map = new double[xDim][yDim][zDim];
    	// TSP Peaks map is the absolute value of the SI corresponding to the largest deviation from baseline
    	peaks_map = new double[xDim][yDim][zDim];
    	
    	// First TSP iteration, Loop over all voxels.  First find delay from cross-correlation.
    	// Then find Correlation Coefficient after shifting by delay
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				sum = 0;
    				for (t = 1; t < tDim; t++) {
    					sum += brain_mask_norm[x][y][z][t];
    				}
    				if (sum != 0) {
    				    temp = xcorrbias(brain_mask_norm[x][y][z], temp_mean);
    				    maxTemp = -Double.MAX_VALUE;
    				    maxIndex = -1;
    				    for (i = 0; i < temp.length; i++) {
    				    	if (temp[i] > maxTemp) {
    				    		maxTemp = temp[i];
    				    		maxIndex = i;
    				    	}
    				    }
    				    delay_map[x][y][z] = maxIndex;
    				    cc = corrcoef(circshift(brain_mask_norm[x][y][z], -maxIndex + tDim), temp_mean);
    				    corr_map2[x][y][z] = cc;
    				} // if (sum != 0)
    			}
    		}
    	} // for (x = 0; x < xDim; x++)
    	
    	// Following TSP iterations, Recalc who brain average (healthy)
    	// considering only tissue with correlations > TSP threshold
    	brain_mask2 = new int[xDim][yDim][zDim][tDim];
    	brain_mask_norm2 = new int[xDim][yDim][zDim][tDim];
    	for (it = 1; it <= TSP_iter; it++) {
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					if (corr_map2[x][y][z] < TSP_threshold) {
    						for (t = 0; t < tDim; t++) {
    						    brain_mask2[x][y][z][t] = 0;
    						}
    					}
    					else {
    						for (t = 0; t < tDim; t++) {
    						    brain_mask2[x][y][z][t] = data[x][y][z][t];
    						}
    					}
    				}
    			}
    		} // for (z = 0; z < zDim; z++)
    		for (i = 0; i < tDim; i++) {
    			temp_mean[i] = 0;
    		}
    		for (t = 0; t < tDim; t++) {
        	    for (z = 0; z < zDim; z++) {
    				for (y = 0; y < yDim; y++) {
    					for (x = 0; x < xDim; x++) {
    					brain_mask_norm2[x][y][z][t] = brain_mask2[x][y][z][t] - brain_mask2[x][y][z][0];	
    					}
    				}
    	    	}
        	}
    		for (t = 1; t < tDim; t++) {
        	    sum = 0;
        	    count = 0;
        	    for (z = 0; z < zDim; z++) {
    				for (y = 0; y < yDim; y++) {
    					for (x = 0; x < xDim; x++) {
    					    if (brain_mask_norm[x][y][z][t] != 0) {
    					    	sum += brain_mask_norm2[x][y][z][t];
    					    	count++;
    					    }
    					}
    				}
        	    }
        	    temp_mean[t] = (double)sum/(double)count;
        	} // for (t = 1; t < tDim; t++)
        	temp_mean[0] = 0;
        	
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			for (z = 0; z < zDim; z++) {
        				sum = 0;
        				for (t = 1; t < tDim; t++) {
        					sum += brain_mask_norm[x][y][z][t];
        				}
        				if (sum != 0) {
        				    temp = xcorrbias(brain_mask_norm[x][y][z], temp_mean);
        				    maxTemp = -Double.MAX_VALUE;
        				    maxIndex = -1;
        				    for (i = 0; i < temp.length; i++) {
        				    	if (temp[i] > maxTemp) {
        				    		maxTemp = temp[i];
        				    		maxIndex = i;
        				    	}
        				    }
        				    delay_map[x][y][z] = maxIndex;
        				    maxPeak = -Double.MAX_VALUE;
        				    for (t = 0; t < tDim; t++) {
        				    	if (Math.abs(brain_mask_norm[x][y][z][t]) > maxPeak) {
        				    		maxPeak = Math.abs(brain_mask_norm[x][y][z][t]);
        				    	}
        				    }
        				    peaks_map[x][y][z] = maxPeak;
        				    cc = corrcoef(brain_mask_norm[x][y][z], temp_mean);
        				    corrmap[x][y][z] = cc;
        				    cc = corrcoef(circshift(brain_mask_norm[x][y][z], -maxIndex + tDim), temp_mean);
        				    corr_map2[x][y][z] = cc;
        				} // if (sum != 0)
        			}
        		}
        	} // for (x = 0; x < xDim; x++)
    	} // for (it = 1; it <= TSP_iter; it++)
    	
    	// Clean up outliers (AIF delay must be within +/- 40 frames of whole brain average)
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    			    if (delay_map[x][y][z] > tDim + 40)	{
    			    	delay_map[x][y][z] = tDim = 40;
    			    }
    			    else if (delay_map[x][y][z] < tDim - 40) {
    			    	delay_map[x][y][z] = tDim - 40;
    			    }
    			    delay_map[x][y][z] = (delay_map[x][y][z] - tDim) * delT;
    			    // Corr map > 1 or < 0 is not realistic
    			    if (corrmap[x][y][z] > 1) {
    			    	corrmap[x][y][z] = 1;
    			    }
    			    else if (corrmap[x][y][z] < 0) {
    			    	corrmap[x][y][z] = 0;
    			    }
    			}
    		}
    	} // for (x = 0; x < xDim; x++)
    	
    	// Write images and clean up variable
    	A = new int[]{xDim, yDim, zDim};
    	B = new int[]{xDim/2, yDim/2, zDim/2};
    	dbuffer = new double[volume];
    	corr_map2Image = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "corr_map2");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = corr_map2[x][y][z];
    			}
    		}
    	}
    	try {
    		corr_map2Image.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on corr_map2Image");
    		setCompleted(false);
    		return;
    	}
    	FileInfoBase fileInfo[] = corr_map2Image.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
    	FileWriteOptions options = new FileWriteOptions(true);
        options.setFileType(FileUtility.ANALYZE);
        options.setFileDirectory(pwiImageFileDirectory + File.separator);
        options.setFileName("corr_map2.img");
        options.setBeginSlice(0);
        options.setEndSlice(extents3D[2]-1);
        options.setOptionsSet(false);
        options.setSaveAs(true);
        FileAnalyze analyzeFile;

        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(corr_map2Image, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing corr_map2.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing corr_map2.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	corr_map2Image.disposeLocal();
    	corr_map2Image = null;
    	
    	corrmapImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "corrmap");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = corrmap[x][y][z];
    			}
    		}
    	}
    	try {
    		corrmapImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on corrmapImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = corrmapImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("corrmap.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(corrmapImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing corrmap.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing corrmap.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	corrmapImage.disposeLocal();
    	corrmapImage = null;
    	
    	peaks_mapImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "peaks_map");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = peaks_map[x][y][z];
    			}
    		}
    	}
    	try {
    		peaks_mapImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on peaks_mapImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = peaks_mapImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("peaks_map.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(peaks_mapImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing peaks_map.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing peaks_map.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	peaks_mapImage.disposeLocal();
    	peaks_mapImage = null;
    	
    	delay_mapImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "delay_map");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = delay_map[x][y][z];
    			}
    		}
    	}
    	try {
    		delay_mapImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on delay_mapImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = delay_mapImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("delay_map.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(delay_mapImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing delay_map.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing delay_map.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	delay_mapImage.disposeLocal();
    	delay_mapImage = null;
    	
    	// Deconvolution analysis
    	// Auto AIF Calculation
    	// AIF is average signal of pixels with the largest SI deviations
    	// (4 std) from baseline (likely to be large vessels)
    	data_norm = new int[xDim][yDim][zDim][tDim];
    	for (t = 0; t < tDim; t++) {
    	    for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
					    data_norm[x][y][z][t] = data[x][y][z][t] - data[x][y][z][0];	
					}
				}
	    	}
    	} // for (t = 0; t < tDim; t++)
    	peaks = new int[xDim][yDim][zDim];
    	ttp = new int[xDim][yDim][zDim];
    	for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					minpeaks = Integer.MAX_VALUE;
					minttp = Integer.MAX_VALUE;
					for (t = 0; t < tDim; t++) {
						if (data_norm[x][y][z][t] < minpeaks) {
							minpeaks = data_norm[x][y][z][t];
							minttp = t;
						}
					}
					peaks[x][y][z] = minpeaks;
					ttp[x][y][z] = minttp;
				}
			}	
		} // for (z = 0; z < zDim; z++)
    	mask3D = new byte[xDim][yDim][zDim];
    	sum = 0;
	    count = 0;
	    for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
				    if (peaks[x][y][z] != 0) {
				    	sum += peaks[x][y][z];
				    	count++;
				    }
				}
			}
	    }
	    peaks_mean = (double)sum/(double)count;
	    diff_squared_sum = 0.0;
	    for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
				    if (peaks[x][y][z] != 0) {
				    	diff = peaks[x][y][z] - peaks_mean;
				    	diff_squared_sum += diff * diff;
				    }
				}
			}
	    }
	    peaks_std = Math.sqrt(diff_squared_sum/(count-1));
	    peaks_threshold = peaks_mean - 4.0*peaks_std;
	    for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
				    if (peaks[x][y][z] < peaks_threshold) {
				    	mask3D[x][y][z] = 1;
				    }
				}
			}
	    }
	    autoaif = new double[tDim];
	    minautoaif = Double.MAX_VALUE;
	    for (t = 0; t < tDim; t++) {
	        sum = 0;
	        count = 0;
	        for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						if (mask3D[x][y][z] == 1) {
						    sum += data_norm[x][y][z][t];
						    count++;
						}
					}
				}
	        }
	        autoaif[t] = (double)sum/(double)count;
	        if (autoaif[t] < minautoaif) {
	        	minautoaif = autoaif[t];
	        }
	    } // for (t = 0; t < tDim; t++)
	    // time signal from mri
	    S = new double[tDim];
	    for (t = 0; t < tDim; t++) {
	    	S[t] = autoaif[t] - minautoaif + 1;
	    }
	    // Calculate AIF as amount of contrast agent as estimated from R2
	    Ca = new double[tDim];
	    for (t = 1; t < tDim; t++) {
	    	Ca[t] = -TE*Math.log(S[t]/S[0]);
	    }
	    Ca[0] = 0;
	    
	    // Optional pick image pixel corresponding to AIF (uncomment)
	    sliceBuffer = new int[length];
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		sliceBuffer[x + y * xDim] = data[x][y][9][0];
	    	}
	    }
	    extents2D = new int[]{xDim,yDim};
	    pickImage = new ModelImage(ModelStorageBase.INTEGER,extents2D,"pickImage");
	    try {
	    	pickImage.importData(0, sliceBuffer, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException on pickImage.importData");
	    	setCompleted(false);
	    	return;
	    }
	    ViewJFrameImage pickFrame = new ViewJFrameImage(pickImage);
	    accessLock.lock();
	    try {
		    canProcessMouseClick.await();
		}
		catch (InterruptedException e) {
			e.printStackTrace();
		}
	    accessLock.unlock();
	    try {
	        pickFrame.finalize();
	    }
	    catch (Throwable e) {
	    	
	    }
    	setCompleted(true);
    } // end runAlgorithm()
    
    public void mouseClicked(MouseEvent mouseEvent) {
	     ViewJComponentBase vBase= (ViewJComponentBase)pickImage.getParentFrame().getComponentImage();
		try {

		   xS = Math.round((mouseEvent.getX() / (vBase.getZoomX() * vBase.getResolutionX())) - 0.5f);
           yS = Math.round((mouseEvent.getY() / (vBase.getZoomY() * vBase.getResolutionY())) - 0.5f);

           if ((xS < 0) || (xS >= pickImage.getExtents()[0]) || (yS < 0) || (yS >= pickImage.getExtents()[1])) {
               return;
           }

          
       } catch (OutOfMemoryError error) {
           System.gc();
           MipavUtil.displayError("Out of memory: PlugInAlgorithmTSPAnalysis.mouseClicked+"
           		+ "");

           return;
       }
		
	    canProcessMouseClick.signalAll();
	}
    
    public void mousePressed(MouseEvent event) {
		
	}
	
	public void mouseReleased(MouseEvent event) {
		
	}
	
	public void mouseEntered(MouseEvent event) {
		
	}
	
	public void mouseExited(MouseEvent event) {
		
	}
    
    private double[] xcorrbias(int x[], double y[]) {
    	int i;
    	int m;
    	int n;
        int N = Math.max(x.length, y.length);
        double xArr[] = new double[N];
        double yArr[] = new double[N];
        for (i = 0; i < x.length; i++) {
        	xArr[i] = x[i];
        }
        for (i = 0; i < y.length; i++) {
        	yArr[i] = y[i];
        }
        double cout[] = new double[2*N-1];
        for (m = N-1; m >= 1; m--) {
            for (n = 0; n <= N-m-1; n++) {
                cout[N-1-m] += yArr[n+m]*xArr[n];	
            }
        }
        for (m = 0; m <= N-1; m++) {
        	for (n = 0; n <= N-m-1; n++) {
        		cout[N-1+m] += xArr[n+m]*yArr[n];
        	}
        }
        for (i = 0; i < 2*N-1; i++) {
        	cout[i] = cout[i]/N;
        }
        return cout;
    }
    
    private int[] circshift(int x[], int n) {
    	int i;
    	int y[] = new int[x.length];
    	if (n > 0) {
    	    for (i = 0; i < n; i++) {
    	    	y[i] = x[x.length - (n - i)];
    	    }
    	    for (i = n; i < x.length; i++) {
    	    	y[i] = x[i-n];
    	    }
    	}
    	else if (n == 0) {
    		for (i = 0; i < x.length; i++) {
    			y[i] = x[i];
    		}
    	}
    	else {
    		n = -n;
    		for (i = 0; i < n; i++) {
    			y[x.length - (n - i)] = x[i];
    		}
    		for (i = n; i < x.length; i++) {
    			y[i-n] = x[i];
    		}
    	}
    	return y;
    }
    
    private double corrcoef(int x[], double y[]) {
    	int N = x.length;
    	int i;
    	double sumX = 0;
    	double sumY = 0;
    	double meanX;
    	double meanY;
    	double diffX;
    	double diffY;
    	double diffXSquaredSum = 0;
    	double diffYSquaredSum = 0;
    	double stdX;
    	double stdY;
    	double cf = 0;
    	for (i = 0; i < N; i++) {
    	    sumX += x[i];
    	    sumY += y[i];
    	}
    	meanX = sumX/N;
    	meanY = sumY/N;
    	for (i = 0; i < N; i++) {
    	    diffX = x[i] - meanX;
    	    diffXSquaredSum += diffX*diffX;
    	    diffY = y[i] - meanY;
    	    diffYSquaredSum += diffY*diffY;
    	    cf += diffX * diffY;
    	}
    	stdX = Math.sqrt(diffXSquaredSum/(N-1));
    	stdY = Math.sqrt(diffYSquaredSum/(N-1));
    	cf = cf/(stdX * stdY * (N-1));
    	return cf;
    }
    
    

//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }
    
    
    
    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }

}
