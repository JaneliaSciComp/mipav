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
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Color;
import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;
import java.util.Collection;
import java.util.Vector;



public class PlugInAlgorithmTSPAnalysis extends AlgorithmBase {
    
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
    /**
     * Constructor.
     *
     */
    public PlugInAlgorithmTSPAnalysis(String pwiImageFileDirectory, int masking_threshold,
    		double TSP_threshold, int TSP_iter) {
        //super(resultImage, srcImg);
    	this.pwiImageFileDirectory = pwiImageFileDirectory;
    	this.masking_threshold = masking_threshold;
    	this.TSP_threshold = TSP_threshold;
    	this.TSP_iter = TSP_iter;
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
    	int x, y, z, t;
    	int i,ii;
    	long sum;
    	int count;
    	double corr_map[][][];
    	double corr_map2[][][];
    	int delay_map[][][];
    	double peaks_map[][][];
    	double temp[];
    	double maxTemp;
    	int maxIndex;
    	double cc;
    	int it;
    	int brain_mask2[][][][];
    	int brain_mask_norm2[][][][];
    	double maxPeak;
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
    	int extents3D[] = image3D.getExtents();
    	length = extents[0] * extents[1];
    	xDim = extents[0];
    	yDim = extents[1];
    	FileInfoDicom dicomInfo = (FileInfoDicom) image3D.getFileInfo(0);
    	FileDicomTagTable tagTable = dicomInfo.getTagTable();
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
        zDim = extents3D[2]/tDim;
        extents[2] = zDim;
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
        delT = t1 - t0;
        //System.out.println("delT = " + delT);
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
    	for (ii = 1; ii < tDim; ii++) {
    	    sum = 0;
    	    count = 0;
    	    for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
					    if (brain_mask_norm[x][y][z][t] < 0) {
					    	sum += brain_mask_norm[x][y][z][t];
					    	count++;
					    }
					}
				}
    	    }
    	    temp_mean[ii] = (double)sum/(double)count;
    	} // for (ii = 1; ii < tDim; ii++)
    	temp_mean[0] = 0;
    	
    	// Zero/Initialize output maps
        // TSP Correlation map with out AIF delay compensation
    	corr_map = new double[xDim][yDim][zDim];
    	// TSP Correlation map with AIF delay compensation
    	corr_map2 = new double[xDim][yDim][zDim];
    	// TSP Delay map considering temporal similarity with whole brain average
    	delay_map = new int[xDim][yDim][zDim];
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
    				if (sum < 0) {
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
    				} // if (sum < 0)
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
    		for (ii = 1; ii < tDim; ii++) {
        	    sum = 0;
        	    count = 0;
        	    for (z = 0; z < zDim; z++) {
    				for (y = 0; y < yDim; y++) {
    					for (x = 0; x < xDim; x++) {
    					    if (brain_mask_norm2[x][y][z][t] < 0) {
    					    	sum += brain_mask_norm2[x][y][z][t];
    					    	count++;
    					    }
    					}
    				}
        	    }
        	    temp_mean[ii] = (double)sum/(double)count;
        	} // for (ii = 1; ii < tDim; ii++)
        	temp_mean[0] = 0;
        	
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			for (z = 0; z < zDim; z++) {
        				sum = 0;
        				for (t = 1; t < tDim; t++) {
        					sum += brain_mask_norm2[x][y][z][t];
        				}
        				if (sum < 0) {
        				    temp = xcorrbias(brain_mask_norm2[x][y][z], temp_mean);
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
        				    	if (Math.abs(brain_mask_norm2[x][y][z][t]) > maxPeak) {
        				    		maxPeak = Math.abs(brain_mask_norm2[x][y][z][t]);
        				    	}
        				    }
        				    peaks_map[x][y][z] = maxPeak;
        				    cc = corrcoef(brain_mask_norm2[x][y][z], temp_mean);
        				    corr_map[x][y][z] = cc;
        				    cc = corrcoef(circshift(brain_mask_norm2[x][y][z], -maxIndex + tDim), temp_mean);
        				    corr_map2[x][y][z] = cc;
        				} // if (sum < 0)
        			}
        		}
        	} // for (x = 0; x < xDim; x++)
    	} // for (it = 1; it <= TSP_iter; it++)
    	
    	setCompleted(true);
    } // end runAlgorithm()
    
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
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    
    
    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }

}
