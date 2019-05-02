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
import gov.nih.mipav.model.algorithms.NLConstrainedEngine;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.file.FileAnalyze;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileNIFTI;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.util.DoubleDouble;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import Jama.Matrix;
import Jama.SingularValueDecomposition;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;




public class PlugInAlgorithmTSPAnalysis extends AlgorithmBase implements MouseListener {
    
    private String pwiImageFileDirectory;
    
    private boolean calculateMaskingThreshold = true;
    
    // Thresholding to mask out image pixels not corresponding to brain tissue
    private int masking_threshold = 600;
    
    private double TSP_threshold = 0.8;
    
    private int TSP_iter = 4;
    
    private double Psvd = 0.1;
    
    private boolean autoAIFCalculation = true;
    
    private boolean multiThreading = true;
    
    private ModelImage pickImage;
    
    private final Lock accessLock = new ReentrantLock();
    private final Condition canProcessMouseClick = accessLock.newCondition();
	private int xS;
	private int yS;
	private double D_inv[][];
    /**
     * Constructor.
     *
     */
    public PlugInAlgorithmTSPAnalysis(String pwiImageFileDirectory, boolean calculateMaskingThreshold, int masking_threshold,
    		double TSP_threshold, int TSP_iter, double Psvd, boolean autoAIFCalculation,
    		boolean multiThreading) {
        //super(resultImage, srcImg);
    	this.pwiImageFileDirectory = pwiImageFileDirectory;
    	this.calculateMaskingThreshold = calculateMaskingThreshold;
    	this.masking_threshold = masking_threshold;
    	this.TSP_threshold = TSP_threshold;
    	this.TSP_iter = TSP_iter;
    	this.Psvd = Psvd;
    	this.autoAIFCalculation = autoAIFCalculation;
    	this.multiThreading = multiThreading;
    }   
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int tDim;
    	float delT;
        int data[][][][];
    	double TE;
    	int Tmax[][][];
    	double CBV[][][];
    	double CBF[][][];
    	double MTT[][][];
    	double chiSquared[][][];
    	int zDim;
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
    	String TRString = null;
    	float TR;
    	int buffer[];
    	int brain_mask[][][][];
    	double temp_mean[];
    	int brain_mask_norm[][][][];
    	double dbuffer[];
    	int x, y, z, t;
    	int i,j,ii,jj;
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
    	double maxPeak;
    	String delZString;
    	float delZ;
    	ModelImage corr_map2Image;
    	ModelImage corrmapImage;
    	ModelImage peaks_mapImage;
    	ModelImage delay_mapImage;
    	String TEString;
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
    	double CaPad[];
    	double a[][];
    	double D[][];
    	Matrix dMat;
    	SingularValueDecomposition svd;
    	double singularValues[];
    	Matrix uMat;
    	Matrix vMat;
    	double singularThreshold;
    	double W[][];
    	Matrix wMat;
    	Matrix D_invMat;
    	//double relCBF[][][];
    	double TTP[][][];
    	double x0[];
    	double xdata[];
    	double C[];
    	boolean alltMeetThreshold;
    	double b[];
    	double sumb;
    	double rcbf;
    	expfun minsearch;
    	int exitStatus;
    	double p[];
    	ModelImage CBFImage;
    	ModelImage MTTImage;
    	ModelImage CBVImage;
    	ModelImage TmaxImage;
    	ModelImage TTPImage;
    	ModelImage chiSquaredImage;
    	long sumT[];
    	int countT[];
    	boolean test = false;
    	boolean Philips = true;
    	
    	if (test) {
    		testxcorr();
    		testcircshift();
    		testcorrcoef();
    		testexpfun();
    		setCompleted(true);
    		return;
    	}
    	
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
    	fileIO.setSuppressProgressBar(true);
    	ModelImage image3D = fileIO.readDicom(selectedFileName, fileList, performSort);
    	image3D.calcMinMax();
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
    	FileInfoDicom dicomInfoLast = (FileInfoDicom) image3D.getFileInfo(extents3Dorg[2]-1);
    	FileDicomTagTable tagTableLast = dicomInfoLast.getTagTable();
    	if (tagTable.getValue("0018,0081") != null) {
        	// Echo time in milliseconds
            FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0081"));
            TEString = (String)tag.getValue(false);     
        }
        else {
        	MipavUtil.displayError("Tag (0018,0081) for Echo Time TE is null");
        	setCompleted(false);
        	return;
        }
    	// Uae echo time in milliseconds
    	TE = Double.valueOf(TEString.trim()).doubleValue();
    	//System.out.println("TE = " + TE);
        if (tagTable.getValue("0020,0105") != null) {
        	// Number of temporal positions
            FileDicomTag tag = tagTable.get(new FileDicomKey("0020,0105"));
            tDimString = (String)tag.getValue(false);    
            Philips = true;
        }
        else if (tagTableLast.getValue("0020,0012") != null) {
        	FileDicomTag tag = tagTableLast.get(new FileDicomKey("0020,0012"));
            tDimString = (String)tag.getValue(false); 
            Philips = false;
        }
        else {
        	MipavUtil.displayError("Tags (0020,0012) and (0020,0105) are both null");
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
            // Spacing between slices in millimeters
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
        if (tagTable.getValue("0018,0080") != null) {
        	// Repetition time in milliseconds
        	FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0080"));
        	TRString = (String)tag.getValue(false);
        }
        else {
        	MipavUtil.displayError("Tag (0018,0080) for Repetition Time is null");
        	setCompleted(false);
        	return;
        }
        TR = Float.valueOf(TRString.trim()).floatValue();
        // Change delT to seconds
        delT = (float)(TR * 1.0E-3);
        //System.out.println("delT = " + delT);
        resolutions[3] = delT;
        for (i = 0; i < 3; i++) {
        	units[i] = Unit.MILLIMETERS.getLegacyNum();
        	units3D[i] = units[i];
        }
        units[3] = Unit.SECONDS.getLegacyNum();
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
    	data = new int[zDim][yDim][xDim][tDim];
    	// Start TSP processing
    	brain_mask = new int[zDim][yDim][xDim][tDim];
    	temp_mean = new double[tDim];
    	brain_mask_norm = new int[zDim][yDim][xDim][tDim];
    	// Normalize PWI by subtracting off pre_contrast (first) image
    	// Loop over time dimension to calculate whole brain average perfusion time signal
    	
		
    	if (Philips) {
	    	for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						for (t = 0; t < tDim/2; t++) {
						    data[z][y][x][2*t] = buffer[x + y*xDim + t*length +z*tDim*length];
						    data[z][y][x][2*t+1] = buffer[x + y*xDim + (t+tDim/2)*length +z*tDim*length];
						}
					}
				}
			}
    	}
    	else {
    		for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						for (t = 0; t < tDim; t++) {
						    data[z][y][x][t] = buffer[x + y*xDim + z*length + t*volume];
						}
					}
				}
			}	
    	}
    	
    	if (calculateMaskingThreshold) {
    		long dataSum = 0;
    		for (i = 0; i < dataSize; i++) {
    			dataSum += buffer[i];
    		}
    		double mean = (double)dataSum/(double)dataSize;
    		double squareSum = 0.0;
    		for (i = 0; i < dataSize; i++) {
    			double difference = buffer[i] - mean;
    			squareSum += (difference * difference);
    		}
    		double stdDev = Math.sqrt(squareSum/(double)(dataSize-1));
    		masking_threshold = (int)Math.round(mean + 0.5 * stdDev);
    	} // if (calculateMaskingThreshold)
    	
		for (t = 0; t < tDim; t++) {
    		sum = 0;
    		count = 0;
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
						if (data[z][y][x][t] < masking_threshold) {
							brain_mask[z][y][x][t] = 0;
						}
						else {
							brain_mask[z][y][x][t] = data[z][y][x][t];
						}
						brain_mask_norm[z][y][x][t] = brain_mask[z][y][x][t] - brain_mask[z][y][x][0];
						if (brain_mask_norm[z][y][x][t] != 0) {
					    	sum += brain_mask_norm[z][y][x][t];
					    	count++;
					    }
			        }
		        }
	        }
		    if (t != 0) {
			    temp_mean[t] = (double)sum/(double)count;
		    }
	    } // for (t = 0; t < tDim; t++)
	    temp_mean[0] = 0;
	    //for (i = 0; i < temp_mean.length; i++) {
	    	//System.out.println("temp_mean["+i+"] = " + temp_mean[i]);
	    	//Preferences.debug("temp_mean["+i+"] = " + temp_mean[i] + "\n", Preferences.DEBUG_ALGORITHM);
	    //}
    	
    	
    	// Zero/Initialize output maps
        // TSP Correlation map with out AIF delay compensation
    	corrmap = new double[zDim][yDim][xDim];
    	// TSP Correlation map with AIF delay compensation
    	corr_map2 = new double[zDim][yDim][xDim];
    	// TSP Delay map considering temporal similarity with whole brain average
    	delay_map = new double[zDim][yDim][xDim];
    	// TSP Peaks map is the absolute value of the SI corresponding to the largest deviation from baseline
    	peaks_map = new double[zDim][yDim][xDim];
    	
    	// First TSP iteration, Loop over all voxels.  First find delay from cross-correlation.
    	// Then find Correlation Coefficient after shifting by delay
    	if (multiThreading) {
    		ExecutorService executorService = Executors.newCachedThreadPool();	
            for (z = 0; z < zDim; z++) {
            	executorService.execute(new corr1Calc(xDim,yDim,tDim,brain_mask_norm[z],delay_map[z],corr_map2[z],
            			temp_mean.clone()));	
            }
            
            executorService.shutdown();
            try {
            	boolean tasksEnded = executorService.awaitTermination(30, TimeUnit.MINUTES);
            	if (!tasksEnded) {
            		MipavUtil.displayError("Time out while waiting for corr1Calc tasks to finish");
            		setCompleted(false);
            		return;
            	}
            }
            catch (InterruptedException ex) {
            	ex.printStackTrace();
            	MipavUtil.displayError("Interrupted exception during corr1Calc tasks");
            	setCompleted(false);
            	return;
            }	
    	}
    	else { // single thread
	    	for (z = 0; z < zDim; z++) {
	    		for (y = 0; y < yDim; y++) {
	    			for (x = 0; x < xDim; x++) {
	    				sum = 0;
	    				for (t = 1; t < tDim; t++) {
	    					sum += brain_mask_norm[z][y][x][t];
	    				}
	    				if (sum != 0) {
	    				    temp = xcorrbias(brain_mask_norm[z][y][x], temp_mean);
	    				    maxTemp = -Double.MAX_VALUE;
	    				    maxIndex = -1;
	    				    for (i = 0; i < temp.length; i++) {
	    				    	if (temp[i] > maxTemp) {
	    				    		maxTemp = temp[i];
	    				    		maxIndex = i+1;
	    				    	}
	    				    }
	    				    delay_map[z][y][x] = maxIndex;
	    				    cc = corrcoef(circshift(brain_mask_norm[z][y][x], -maxIndex + tDim), temp_mean);
	    				    corr_map2[z][y][x] = cc;
	    				} // if (sum != 0)
	    			}
	    		}
	    	} // for (z = 0; z < zDim; z++)
    	} // else single thread
    	
    	// Following TSP iterations, Recalc who brain average (healthy)
    	// considering only tissue with correlations > TSP threshold
    	brain_mask2 = new int[zDim][yDim][xDim][tDim];
    	brain_mask_norm2 = new int[zDim][yDim][xDim][tDim];
    	sumT = new long[tDim];
    	countT = new int[tDim];
    	for (it = 1; it <= TSP_iter; it++) {
    		for (t = 1; t < tDim; t++) {
    			sumT[t] = 0;
    			countT[t] = 0;
    		}
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					if (corr_map2[z][y][x] < TSP_threshold) {
    						for (t = 0; t < tDim; t++) {
    						    brain_mask2[z][y][x][t] = 0;
    						    brain_mask_norm2[z][y][x][t] = 0;
    						}
    					}
    					else {
    						for (t = 0; t < tDim; t++) {
    						    brain_mask2[z][y][x][t] = data[z][y][x][t];
    						    brain_mask_norm2[z][y][x][t] = brain_mask2[z][y][x][t] - brain_mask2[z][y][x][0];
    						    if (brain_mask_norm2[z][y][x][t] != 0) {
        					    	sumT[t] += brain_mask_norm2[z][y][x][t];
        					    	countT[t]++;
        					    }
    						}
    					}
    				}
    			}
    		} // for (z = 0; z < zDim; z++)
    		
    		for (t = 1; t < tDim; t++) {
        	    temp_mean[t] = (double)sumT[t]/(double)countT[t];
        	} // for (t = 1; t < tDim; t++)
        	temp_mean[0] = 0;
        	
        	if (multiThreading) {
        		ExecutorService executorService = Executors.newCachedThreadPool();	
                for (z = 0; z < zDim; z++) {
                	executorService.execute(new corr2Calc(xDim,yDim,tDim,brain_mask_norm[z],delay_map[z],peaks_map[z],corrmap[z],
                			corr_map2[z], temp_mean.clone()));	
                }
                
                executorService.shutdown();
                try {
                	boolean tasksEnded = executorService.awaitTermination(30, TimeUnit.MINUTES);
                	if (!tasksEnded) {
                		MipavUtil.displayError("Time out while waiting for corr2Calc tasks to finish");
                		setCompleted(false);
                		return;
                	}
                }
                catch (InterruptedException ex) {
                	ex.printStackTrace();
                	MipavUtil.displayError("Interrupted exception during corr2Calc tasks");
                	setCompleted(false);
                	return;
                }	
        	}
        	else { // single thread
	        	for (z = 0; z < zDim; z++) {
	        		for (y = 0; y < yDim; y++) {
	        			for (x = 0; x < xDim; x++) {
	        				sum = 0;
	        				for (t = 1; t < tDim; t++) {
	        					sum += brain_mask_norm[z][y][x][t];
	        				}
	        				if (sum != 0) {
	        				    temp = xcorrbias(brain_mask_norm[z][y][x], temp_mean);
	        				    maxTemp = -Double.MAX_VALUE;
	        				    maxIndex = -1;
	        				    for (i = 0; i < temp.length; i++) {
	        				    	if (temp[i] > maxTemp) {
	        				    		maxTemp = temp[i];
	        				    		maxIndex = i+1;
	        				    	}
	        				    }
	        				    delay_map[z][y][x] = maxIndex;
	        				    maxPeak = -Double.MAX_VALUE;
	        				    for (t = 0; t < tDim; t++) {
	        				    	if (Math.abs(brain_mask_norm[z][y][x][t]) > maxPeak) {
	        				    		maxPeak = Math.abs(brain_mask_norm[z][y][x][t]);
	        				    	}
	        				    }
	        				    peaks_map[z][y][x] = maxPeak;
	        				    cc = corrcoef(brain_mask_norm[z][y][x], temp_mean);
	        				    corrmap[z][y][x] = cc;
	        				    cc = corrcoef(circshift(brain_mask_norm[z][y][x], -maxIndex + tDim), temp_mean);
	        				    corr_map2[z][y][x] = cc;
	        				} // if (sum != 0)
	        			}
	        		}
	        	} // for (z = 0; z < zDim; z++)
        	} // else single thread
    	} // for (it = 1; it <= TSP_iter; it++)
    	
    	// Clean up outliers (AIF delay must be within +/- 40 frames of whole brain average)
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    			    if (delay_map[z][y][x] > tDim + 40)	{
    			    	delay_map[z][y][x] = tDim + 40;
    			    }
    			    else if (delay_map[z][y][x] < tDim - 40) {
    			    	delay_map[z][y][x] = tDim - 40;
    			    }
    			    delay_map[z][y][x] = (delay_map[z][y][x] - tDim) * delT;
    			    // Corr map > 1 or < 0 is not realistic
    			    if (corrmap[z][y][x] > 1) {
    			    	corrmap[z][y][x] = 1;
    			    }
    			    else if (corrmap[z][y][x] < 0) {
    			    	corrmap[z][y][x] = 0;
    			    }

    			}
    		}
    	} // for (x = 0; x < xDim; x++)
    	
    	// Write images and clean up variable
    	dbuffer = new double[volume];
    	corr_map2Image = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "corr_map2");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = corr_map2[z][y][x];
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
    				dbuffer[x + y*xDim + z*length] = corrmap[z][y][x];
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
    				dbuffer[x + y*xDim + z*length] = peaks_map[z][y][x];
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
    				dbuffer[x + y*xDim + z*length] = delay_map[z][y][x];
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
    	S = new double[tDim];
    	data_norm = new int[zDim][yDim][xDim][tDim];
    	for (t = 0; t < tDim; t++) {
    	    for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
					    data_norm[z][y][x][t] = data[z][y][x][t] - data[z][y][x][0];	
					}
				}
	    	}
    	} // for (t = 0; t < tDim; t++)
    	peaks = new int[zDim][yDim][xDim];
    	ttp = new int[zDim][yDim][xDim];
    	for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					minpeaks = Integer.MAX_VALUE;
					minttp = Integer.MAX_VALUE;
					for (t = 0; t < tDim; t++) {
						if (data_norm[z][y][x][t] < minpeaks) {
							minpeaks = data_norm[z][y][x][t];
							minttp = t+1;
						}
					}
					peaks[z][y][x] = minpeaks;
					ttp[z][y][x] = minttp;
				}
			}	
		} // for (z = 0; z < zDim; z++)
    	if (autoAIFCalculation) {
	    	// Auto AIF Calculation
	    	// AIF is average signal of pixels with the largest SI deviations
	    	// (4 std) from baseline (likely to be large vessels)
	    	mask3D = new byte[zDim][yDim][xDim];
	    	sum = 0;
		    count = 0;
		    for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
					    if (peaks[z][y][x] != 0) {
					    	sum += peaks[z][y][x];
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
					    if (peaks[z][y][x] != 0) {
					    	diff = peaks[z][y][x] - peaks_mean;
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
					    if (peaks[z][y][x] < peaks_threshold) {
					    	mask3D[z][y][x] = 1;
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
							if (mask3D[z][y][x] == 1) {
							    sum += data_norm[z][y][x][t];
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
		    for (t = 0; t < tDim; t++) {
		    	S[t] = autoaif[t] - minautoaif + 1;
		    }
    	} // if (autoAIFCalculation)
    	else {
		    // Pick image pixel corresponding to AIF
		    sliceBuffer = new int[length];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		sliceBuffer[x + y * xDim] = data[9][y][x][0];
		    	}
		    }
		    extents2D = new int[]{xDim,yDim};
		    accessLock.lock();
		    pickImage = new ModelImage(ModelStorageBase.INTEGER,extents2D,"pickImage");
		    try {
		    	pickImage.importData(0, sliceBuffer, true);
		    }
		    catch (IOException e) {
		    	MipavUtil.displayError("IOException on pickImage.importData");
		    	setCompleted(false);
		    	return;
		    }
		    new ViewJFrameImage(pickImage);
		    pickImage.getParentFrame().getComponentImage().addMouseListener(this);
		    try {
			    canProcessMouseClick.await();
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}
		    accessLock.unlock();
		    pickImage.getParentFrame().getComponentImage().removeMouseListener(this);
		    pickImage.disposeLocal();
		    pickImage = null;
		    System.out.println("xS = " + xS + " yS = " + yS);
		    for (t = 0; t < tDim; t++) {
		    	S[t] = data[9][yS][xS][t];
		    }
    	} // else pick image pixel corresponding to AIF
	    
	    // Calculate AIF as amount of contrast agent as estimated from R2
	    Ca = new double[tDim];
	    for (t = 1; t < tDim; t++) {
	    	Ca[t] = -TE*Math.log(S[t]/S[0]);
	    }
	    Ca[0] = 0;
	    
	    // Assemble prefiltered 'a' matrix from Ca
	    // zero pad Ca
	    CaPad = new double[2*tDim];
	    for (t = 0; t < tDim; t++) {
	    	CaPad[t] = Ca[t];
	    }
	    a = new double[2*tDim][2*tDim];
        for (ii = 0; ii < 2*tDim; ii++) { 
        	for (jj = 0; jj < 2*tDim; jj++) {
        	    if (jj <= ii) {
        	    	if (jj == ii) {
        	    		a[ii][jj] = delT * (4*CaPad[ii-jj] + CaPad[ii-jj+1])/5;
        	    	}
        	    	else if ((ii - jj) > CaPad.length - 2) {
        	    		a[ii][jj] = delT*(CaPad[ii-jj-1] + 4*CaPad[ii-jj])/5;
        	    	}
        	    	else {
        	    		a[ii][jj] = delT*(CaPad[ii-jj-1] + 4*CaPad[ii-jj] + CaPad[ii-jj+1])/6;
        	    	}
        	    }
        	}
        } // for (ii = 0; ii < 2*tDim; ii++)
        
        // Assemble block-circulant 'D' matrix
	    D = new double[2*tDim][2*tDim];
	    for (ii = 0; ii < 2*tDim; ii++) {
	    	for (jj = 0; jj < 2*tDim; jj++) {
	    		if (jj <= ii) {
	    			D[ii][jj] = a[ii][jj];
	    		}
	    		else {
	    			D[ii][jj] = a[2*tDim+ii-jj][0];
	    		}
	    	}
	    }
	    
	    
	    // Compute SVD of 'D' and calculate inverse
        dMat = new Matrix(D);
        svd = new SingularValueDecomposition(dMat);
        uMat = svd.getU();
        singularValues = svd.getSingularValues();
        vMat = svd.getV();
        // threshold singularValues
        singularThreshold = Psvd * singularValues[0];
        for (i = 0; i < 2*tDim; i++) {
        	if (singularValues[i] < singularThreshold) {
        		singularValues[i] = 0;
        	}
        }
        W = new double[2*tDim][2*tDim];
        for (i = 0; i < 2*tDim; i++) {
        	if (singularValues[i] == 0) {
        		W[i][i] = 0;
        	}
        	else {
        		W[i][i] = 1.0/singularValues[i];
        	}
        }
        wMat = new Matrix(W);
        D_invMat = (vMat.times(wMat)).times(uMat.transpose());
        D_inv = D_invMat.getArray();
        
        
        
        // Iterate over brain volume to find rCBF
        CBV = new double[zDim][yDim][xDim];
        CBF = new double[zDim][yDim][xDim];
        MTT = new double[zDim][yDim][xDim];
        Tmax = new int[zDim][yDim][xDim];
        //relCBF = new double[xDim][yDim][zDim];
        TTP = new double[zDim][yDim][xDim];
        chiSquared = new double[zDim][yDim][xDim];
        // Apply same mask as in TSP for speed of iteration
        // Calculate Peaks and Time to peak mask
        for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					if (data[z][y][x][0] < masking_threshold) {
						peaks[z][y][x] = 0;
					}
					else {
						TTP[z][y][x] = ttp[z][y][x] * delT;
					}
				}
			}
		} // for (z = 0; z < zDim; z++)
        if (multiThreading) {
        	ExecutorService executorService = Executors.newCachedThreadPool();	
            for (z = 0; z < zDim; z++) {
            	executorService.execute(new endCalc(xDim,yDim,tDim,delT,TE,masking_threshold,
            			data[z],CBV[z],CBF[z],MTT[z],Tmax[z],chiSquared[z]));	
            }
            
            executorService.shutdown();
            try {
            	boolean tasksEnded = executorService.awaitTermination(30, TimeUnit.MINUTES);
            	if (!tasksEnded) {
            		MipavUtil.displayError("Time out while waiting for endCalc tasks to finish");
            		setCompleted(false);
            		return;
            	}
            }
            catch (InterruptedException ex) {
            	ex.printStackTrace();
            	MipavUtil.displayError("Interrupted exception during final tasks");
            	setCompleted(false);
            	return;
            }
        } // if (multThreading)
        else { // single processor
		    // Define some variables for fminsearch - initial guess
		    x0 = new double[]{0.1,4};
		    xdata = new double[2*tDim];
		    for (i = 0; i < 2*tDim; i++) {
		    	xdata[i] = i * delT;
		    }
		    C = new double[2*tDim];
		    b = new double[2*tDim];
		    // Iterate
		    for (z = 0; z < zDim; z++) {
		    	//fireProgressStateChanged((int)(100*z/zDim), null,
						//"Working on slice " + (z+1) + " of " + zDim);
		    	for (y = 0; y < yDim; y++) {
		    		for (x = 0; x < xDim; x++) {
		    		    alltMeetThreshold = true;
		    		    for (t = 0; (t < tDim) && alltMeetThreshold; t++) {
		    		    	if (data[z][y][x][t] < masking_threshold) {
		    		    		alltMeetThreshold = false;
		    		    	}
		    		    } // for (t = 0; (t < tDim) && alltMeetThreshold; t++)
		    		    if (alltMeetThreshold) {
		    		        // time signal from mri
		    		    	for (t = 0; t < tDim; t++) {
		    		    		S[t] = data[z][y][x][t];
		    		    	}
		    		    	// Calculate amount of contrast agent as estimated from R2
		    		    	for (t = 0; t < tDim; t++) {
		    		    		C[t] = -TE*Math.log(S[t]/S[0]);
		    		    	}
		    		    	// Solve for residual function
		    		    	for (i = 0; i < 2*tDim; i++) {
		    		    		b[i] = 0;
		    		    		// Second half of C is zeros
		    		    		for (j = 0; j < tDim; j++) {
		    		    		    b[i] += D_inv[i][j] * C[j];	
		    		    		}
		    		    	} // for (i = 0; i < 2*tDim; i++)
		    		    	sumb = 0;
		    		    	for (i = 0; i < 2*tDim; i++) {
		    		    		sumb += b[i];
		    		    	}
		    		    	if ((!Double.isNaN(sumb)) && (!Double.isInfinite(sumb))) {
		    		    	    rcbf = -Double.MAX_VALUE;
		    		    	    Tmax[z][y][x] = -1;
		    		    	    for (i = 0; i < b.length/4; i++) {
		    		    	    	if (b[i] > rcbf) {
		    		    	    		rcbf = b[i];
		    		    	    		Tmax[z][y][x] = i+1;
		    		    	    	}
		    		    	    }
		    		    	    // Shift b to have a peak at origin for fitting
		    		    	    b = circshift(b,-Tmax[z][y][x]);
		    		    	    minsearch = new expfun(x0, b, xdata);
		    		    	    minsearch.driver();
		    		    	    exitStatus = minsearch.getExitStatus();
		    		    	    p = minsearch.getParameters();
		    		    	    if ((exitStatus >= 0) && (p[1] > 0) && (p[1] < 75)) {
		    		    	    	// Normal termination
		    		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
		    					    CBF[z][y][x] = p[0];
		    					    // relCBF is max value of residual function.  Should be similar to CBF,
		    					    // but may be different.
		    					    //relCBF[x][y][z] = rcbf;
		    					    MTT[z][y][x] = p[1];
		    					    CBV[z][y][x] = rcbf * p[1];
		    					    chiSquared[z][y][z] = minsearch.getChiSquared();
		    		    	    }
		    		    	} // if ((!Double.isNaN(sumb)) && (!Double.isInfinite(sumb)))
		    		    } // if (alltMeetThreshold)
		    		} // for (x = 0; x < xDim; x++)
		    	} // for (y = 0; y < yDim; y++)
		    } // for (z = 0; z < zDim; z++)
        } // else single processor
        
        // Write maps to images
        CBFImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "CBF");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = CBF[z][y][x];
    			}
    		}
    	}
    	try {
    		CBFImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on CBFImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = CBFImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("CBF.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(CBFImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing CBF.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing CBF.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	CBFImage.disposeLocal();
    	CBFImage = null;
    	
    	MTTImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "MTT");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = MTT[z][y][x];
    			}
    		}
    	}
    	try {
    		MTTImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on MTTImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = MTTImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("MTT.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(MTTImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing MTT.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing MTT.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	MTTImage.disposeLocal();
    	MTTImage = null;
    	
    	CBVImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "CBV");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = CBV[z][y][x];
    			}
    		}
    	}
    	try {
    		CBVImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on CBVImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = CBVImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("CBV.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(CBVImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing CBV.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing CBV.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	CBVImage.disposeLocal();
    	CBVImage = null;
    	
    	TmaxImage = new ModelImage(ModelStorageBase.INTEGER, extents3D, "Tmax");
    	buffer = new int[volume];
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				buffer[x + y*xDim + z*length] = Tmax[z][y][x];
    			}
    		}
    	}
    	try {
    		TmaxImage.importData(0, buffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on TmaxImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = TmaxImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.INTEGER);
    	}
        options.setFileName("Tmax.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(TmaxImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing Tmax.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing Tmax.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	TmaxImage.disposeLocal();
    	TmaxImage = null;
    	
    	TTPImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "TTP");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = TTP[z][y][x];
    			}
    		}
    	}
    	try {
    		TTPImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on TTPImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = TTPImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("TTP.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(TTPImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing TTP.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing TTP.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
    	TTPImage.disposeLocal();
    	TTPImage = null;
    	
    	chiSquaredImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "chiSquared");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = chiSquared[z][y][x];
    			}
    		}
    	}
    	try {
    		chiSquaredImage.importData(0, dbuffer, true);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on chiSquaredImage");
    		setCompleted(false);
    		return;
    	}
    	fileInfo = chiSquaredImage.getFileInfo();
    	for (i = 0; i < zDim; i++) {
    		fileInfo[i].setResolutions(resolutions3D);
    		fileInfo[i].setUnitsOfMeasure(units3D);
    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
    	}
        options.setFileName("chiSquared.img");
       
        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            boolean zerofunused = false;
            analyzeFile.setZerofunused(zerofunused);
            //createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(chiSquaredImage, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            MipavUtil.displayError("IOException on writing chiSquared.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        } catch (final OutOfMemoryError error) {

            MipavUtil.displayError("Out of memory error on writing chiSquared.img");

            error.printStackTrace();
            setCompleted(false);
            return;
        }
        chiSquaredImage.disposeLocal();
    	chiSquaredImage = null;
    	
    	for (t = 0; t < 2*tDim; t++) {
    		D_inv[t] = null;
    	}
    	D_inv = null;
    	
    	System.out.println("Finished directory " + pwiImageFileDirectory);
    	
    	setCompleted(true); 
    } // end runAlgorithm()
    
    public class corr1Calc implements Runnable {
    	int xDim;
        int yDim;
        int tDim;
        int brain_mask_norm[][][];
        double delay_map[][];
        double corr_map2[][];
        double temp_mean[];
        
        public corr1Calc(int xDim, int yDim, int tDim, int brain_mask_norm[][][], double delay_map[][], 
        		double corr_map2[][], double temp_mean[]) {
        	this.xDim = xDim;
        	this.yDim = yDim;
        	this.tDim = tDim;
        	this.brain_mask_norm = brain_mask_norm;
        	this.delay_map = delay_map;
        	this.corr_map2 = corr_map2;
        	this.temp_mean = temp_mean;
        }
        
        public void run() {
        	int i, x, y, t;
        	long sum;
        	double temp[];
        	double maxTemp;
        	int maxIndex;
        	double cc;
        	for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				sum = 0;
    				for (t = 1; t < tDim; t++) {
    					sum += brain_mask_norm[y][x][t];
    				}
    				if (sum != 0) {
    				    temp = xcorrbias(brain_mask_norm[y][x], temp_mean);
    				    maxTemp = -Double.MAX_VALUE;
    				    maxIndex = -1;
    				    for (i = 0; i < temp.length; i++) {
    				    	if (temp[i] > maxTemp) {
    				    		maxTemp = temp[i];
    				    		maxIndex = i+1;
    				    	}
    				    }
    				    delay_map[y][x] = maxIndex;
    				    cc = corrcoef(circshift(brain_mask_norm[y][x], -maxIndex + tDim), temp_mean);
    				    corr_map2[y][x] = cc;
    				} // if (sum != 0)
    			}
    		}	
        }
    }
    
    public class corr2Calc implements Runnable {
    	int xDim;
        int yDim;
        int tDim;
        int brain_mask_norm[][][];
        double delay_map[][];
        double peaks_map[][];
        double corrmap[][];
        double corr_map2[][];
        double temp_mean[];
        
        public corr2Calc(int xDim, int yDim, int tDim, int brain_mask_norm[][][], double delay_map[][], 
        		double peaks_map[][], double corrmap[][], double corr_map2[][], double temp_mean[]) {
        	this.xDim = xDim;
        	this.yDim = yDim;
        	this.tDim = tDim;
        	this.brain_mask_norm = brain_mask_norm;
        	this.delay_map = delay_map;
        	this.peaks_map = peaks_map;
        	this.corrmap = corrmap;
        	this.corr_map2 = corr_map2;
        	this.temp_mean = temp_mean;
        }
        
        public void run() {
        	int i, x, y, t;
        	long sum;
        	double temp[];
        	double maxTemp;
        	int maxIndex;
        	double cc;
        	double maxPeak;
        	
        	for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				sum = 0;
    				for (t = 1; t < tDim; t++) {
    					sum += brain_mask_norm[y][x][t];
    				}
    				if (sum != 0) {
    				    temp = xcorrbias(brain_mask_norm[y][x], temp_mean);
    				    maxTemp = -Double.MAX_VALUE;
    				    maxIndex = -1;
    				    for (i = 0; i < temp.length; i++) {
    				    	if (temp[i] > maxTemp) {
    				    		maxTemp = temp[i];
    				    		maxIndex = i+1;
    				    	}
    				    }
    				    delay_map[y][x] = maxIndex;
    				    maxPeak = -Double.MAX_VALUE;
    				    for (t = 0; t < tDim; t++) {
    				    	if (Math.abs(brain_mask_norm[y][x][t]) > maxPeak) {
    				    		maxPeak = Math.abs(brain_mask_norm[y][x][t]);
    				    	}
    				    }
    				    peaks_map[y][x] = maxPeak;
    				    cc = corrcoef(brain_mask_norm[y][x], temp_mean);
    				    corrmap[y][x] = cc;
    				    cc = corrcoef(circshift(brain_mask_norm[y][x], -maxIndex + tDim), temp_mean);
    				    corr_map2[y][x] = cc;
    				} // if (sum != 0)
    			}
    		}
        	
        }
    }
    
    public class endCalc implements Runnable {
    	// The global D_inv is shared by all these Runnable routines.
    	// All other variables are unique.
        int xDim;
        int yDim;
        int tDim;
        float delT;
        double TE;
        double masking_threshold;
        int data[][][];
        double CBV[][];
        double CBF[][];
        double MTT[][];
        int Tmax[][];
        double chiSquared[][];
    	
    	public endCalc(int xDim, int yDim, int tDim, float delT, double TE, double masking_threshold,
    			int data[][][], double CBV[][], double CBF[][], double MTT[][], int Tmax[][],
    			double chiSquared[][]) {
        	this.xDim = xDim;
        	this.yDim = yDim;
        	this.tDim = tDim;
        	this.delT = delT;
        	this.TE = TE;
        	this.masking_threshold = masking_threshold;
        	this.data = data;
        	this.CBV = CBV;
        	this.CBF = CBF;
        	this.MTT = MTT;
        	this.Tmax = Tmax;
        	this.chiSquared = chiSquared;
        }
    	
    	public void run() {
    		double x0[];
            double xdata[];
            int i;
            double C[];
            double b[];
    		x0 = new double[]{0.1,4};
            xdata = new double[2*tDim];
            for (i = 0; i < 2*tDim; i++) {
            	xdata[i] = i * delT;
            }
            C = new double[2*tDim];
            b = new double[2*tDim];
            int y;
            int x;
            boolean alltMeetThreshold;
            int t;
            double S[] = new double[tDim];
            int j;
            double sumb;
            double rcbf;
            expfun minsearch;
        	int exitStatus;
        	double p[];
            // Iterate
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        		    alltMeetThreshold = true;
        		    for (t = 0; (t < tDim) && alltMeetThreshold; t++) {
        		    	if (data[y][x][t] < masking_threshold) {
        		    		alltMeetThreshold = false;
        		    	}
        		    } // for (t = 0; (t < tDim) && alltMeetThreshold; t++)
        		    if (alltMeetThreshold) {
        		        // time signal from mri
        		    	for (t = 0; t < tDim; t++) {
        		    		S[t] = data[y][x][t];
        		    	}
        		    	// Calculate amount of contrast agent as estimated from R2
        		    	for (t = 0; t < tDim; t++) {
        		    		C[t] = -TE*Math.log(S[t]/S[0]);
        		    	}
        		    	// Solve for residual function
        		    	for (i = 0; i < 2*tDim; i++) {
        		    		b[i] = 0;
        		    		// Second half of C is zeros
        		    		for (j = 0; j < tDim; j++) {
        		    		    b[i] += D_inv[i][j] * C[j];	
        		    		}
        		    	} // for (i = 0; i < 2*tDim; i++)
        		    	sumb = 0;
        		    	for (i = 0; i < 2*tDim; i++) {
        		    		sumb += b[i];
        		    	}
        		    	if ((!Double.isNaN(sumb)) && (!Double.isInfinite(sumb))) {
        		    	    rcbf = -Double.MAX_VALUE;
        		    	    Tmax[y][x] = -1;
        		    	    for (i = 0; i < b.length/4; i++) {
        		    	    	if (b[i] > rcbf) {
        		    	    		rcbf = b[i];
        		    	    		Tmax[y][x] = i+1;
        		    	    	}
        		    	    }
        		    	    // Shift b to have a peak at origin for fitting
        		    	    b = circshift(b,-Tmax[y][x]);
        		    	    minsearch = new expfun(x0, b, xdata);
        		    	    minsearch.driver();
        		    	    exitStatus = minsearch.getExitStatus();
        		    	    p = minsearch.getParameters();
        		    	    if ((exitStatus >= 0) && (p[1] >= 0) && (p[1] < 75)) {
        		    	    	// Normal termination
        		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
        					    CBF[y][x] = p[0];
        					    // relCBF is max value of residual function.  Should be similar to CBF,
        					    // but may be different.
        					    //relCBF[x][y][z] = rcbf;
        					    MTT[y][x] = p[1];
        					    CBV[y][x] = rcbf * p[1];
        					    chiSquared[y][x] = minsearch.getChiSquared();
        		    	    }
        		    	} // if ((!Double.isNaN(sumb)) && (!Double.isInfinite(sumb)))
        		    } // if (alltMeetThreshold)
        		} // for (x = 0; x < xDim; x++)
        	} // for (y = 0; y < yDim; y++)
    	}
    }
    
    public void testexpfun() {
    	expfun minsearch;
    	int exitStatus;
    	double p[];
    	double x0[] = new double[]{0.1,4};
    	double b[] = new double[]{0.07, 0.06};
    	double xdata[] = new double[]{5.0,6.0};
    	 minsearch = new expfun(x0, b, xdata);
 	    minsearch.driver();
 	    exitStatus = minsearch.getExitStatus();	
 	    System.out.println("exitStatus = " + exitStatus);
 	    p = minsearch.getParameters();
 	    System.out.println("p[0] = " + p[0] + " MATLAB answer = 0.151297958944948");
 	    System.out.println("p[1] = " + p[1] + " MATLAB answer = 6.48713976636383");
    }
    
    class expfun extends NLConstrainedEngine {
    	double b[];
    	double xdata[];
        public expfun(double x0[], double b[], double xdata[]) {
        	// nPoints, params
        	super(b.length, x0.length);
        	this.b = b;
        	this.xdata = xdata;
        	
        	bounds = 0; // bounds = 0 means unconstrained

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters
        	
        	

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < x0.length; i++) {
				gues[i] = x0[i];
			}
        }
        
        /**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}

		/**
		 * Display results of displaying exponential fitting parameters.
		 */
		public void dumpResults() {
			Preferences
					.debug(" ******* Fit Elsunc Whole Diffusion-Reaction Model ********* \n\n",
							Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters)
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared())
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("a0 " + String.valueOf(a[0]) + "\n",
					Preferences.DEBUG_ALGORITHM);
			Preferences.debug("a1 " + String.valueOf(a[1]) + "\n",
					Preferences.DEBUG_ALGORITHM);
		}
		
		/**
		 * Fit to function.
		 * 
		 * @param a
		 *            The x value of the data point.
		 * @param residuals
		 *            The best guess parameter values.
		 * @param covarMat
		 *            The derivative values of y with respect to fitting
		 *            parameters.
		 */
		public void fitToFunction(double[] a, double[] residuals,
				double[][] covarMat) {
			int ctrl;
			int i;
			
			
			try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Monoexponential decay
					for (i = 0; i < nPts; i++) {
					    residuals[i] = (a[0]*Math.exp(-1/a[1]*xdata[i])) - b[i];
					}
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian analytically
				else if (ctrl == 2) {
					for (i = 0; i < nPts; i++) {
					    covarMat[i][0] = Math.exp(-1.0/a[1]*xdata[i]);
					    covarMat[i][1] = xdata[i]/(a[1]*a[1])*a[0]*Math.exp(-1.0/a[1]*xdata[i]);
					}
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
		}
    }
    
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
           MipavUtil.displayError("Out of memory: PlugInAlgorithmTSPAnalysis.mouseClicked");

           return;
       }
	        accessLock.lock();
		    canProcessMouseClick.signalAll();
		    accessLock.unlock();
	}
    
    public void mousePressed(MouseEvent event) {
		
	}
	
	public void mouseReleased(MouseEvent event) {
		
	}
	
	public void mouseEntered(MouseEvent event) {
		
	}
	
	public void mouseExited(MouseEvent event) {
		
	}
	
	public void testxcorr() {
        int i;
		int x[] = new int[]{1,2,3,4};
		double y[] = new double[]{1,2,1,-1};
		// Answer from MATLAB
		double answer[] = new double[]{-0.25,-0.25,0.25,1.0,3.0,2.75,1.0};
		double result[] = xcorrbias(x, y);
		for (i = 0; i < 7; i++) {
			System.out.println("result["+i+"] = " + result[i] + " answer["+i+"] = " + answer[i]);
		}
	    x = new int[]{-34, 56, 98, -11, 45, 9};
		y = new double[]{3.4, -2.7, 9.0, -6.1, 3.8, 4.8};
		answer = new double[]{-27.2000,   23.2667,  148.4333,  -54.6667,   28.7000,  149.4167,  -68.9167,  118.8333,  -12.9833,   21.4500,    5.1000};
		result = xcorrbias(x,y);
		for (i = 0; i < 11; i++) {
			System.out.println("result["+i+"] = " + result[i] + " answer["+i+"] = " + answer[i]);
		}
	}
    
    /*private double[] xcorrbias(int x[], double y[]) {
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
    }*/
    
    private double[] xcorrbias(int x[], double y[]) {
    	int i;
    	int m;
    	int n;
        int N = Math.max(x.length, y.length);
        DoubleDouble xArr[] = new DoubleDouble[N];
        DoubleDouble yArr[] = new DoubleDouble[N];
        for (i = 0; i < x.length; i++) {
        	xArr[i] = DoubleDouble.valueOf((double)x[i]);
        }
        for (i = 0; i < y.length; i++) {
        	yArr[i] = DoubleDouble.valueOf(y[i]);
        }
        DoubleDouble coutDD[] = new DoubleDouble[2*N-1];
        double cout[] = new double[2*N-1];
        for (i = 0; i < 2*N-1; i++) {
        	coutDD[i] = DoubleDouble.valueOf(0.0);
        }
        for (m = N-1; m >= 1; m--) {
            for (n = 0; n <= N-m-1; n++) {
                coutDD[N-1-m] = coutDD[N-1-m].add(yArr[n+m].multiply(xArr[n]));	
            }
        }
        for (m = 0; m <= N-1; m++) {
        	for (n = 0; n <= N-m-1; n++) {
        		coutDD[N-1+m] = coutDD[N-1+m].add(xArr[n+m].multiply(yArr[n]));
        	}
        }
        DoubleDouble NDD = DoubleDouble.valueOf((double)N);
        for (i = 0; i < 2*N-1; i++) {
        	cout[i] = (coutDD[i].divide(NDD)).doubleValue();
        }
        return cout;
    }
    
    /*private double[] xcorrbiasfft(int x[], double y[]) {
		int i;
    	FFTUtility fft;
        int convLength = x.length + y.length - 1;
        double xArr[] = new double[convLength];
        double xImagArr[] = new double[convLength];
        double yArr[] = new double[convLength];
        double yImagArr[] = new double[convLength];
        double cout[] = new double[convLength];
        double cImagout[] = new double[convLength];
        int N = x.length;
        for (i = 0; i < x.length; i++) {
        	xArr[i] = x[i];
        }
        for (i = 0; i < y.length; i++) {
        	yArr[i] = y[i];
        }
        // Instantiate the 1d FFT routine
 		// -1 for forward transform
 		fft = new FFTUtility(xArr, xImagArr, 1,convLength, 1, -1,
 				FFTUtility.FFT);
 		fft.setShowProgress(false);
 		fft.run();
 		fft.finalize();
 		fft = null;
 		fft = new FFTUtility(yArr, yImagArr, 1,convLength, 1, -1,
 				FFTUtility.FFT);
 		fft.setShowProgress(false);
 		fft.run();
 		fft.finalize();
 		fft = null;
 		for (i = 0; i < convLength; i++) {
 			cout[i] = xArr[i]*yArr[i] + xImagArr[i]*yImagArr[i];
 			cImagout[i] = -xArr[i]*yImagArr[i] + xImagArr[i]*yArr[i];
 		}
 	    // +1 for backward transform
		fft = new FFTUtility(cout, cImagout, 1, convLength, 1, 1,
				FFTUtility.FFT);
		fft.setShowProgress(false);		
		fft.run();
		fft.finalize();
		fft = null;
		for (i = 0; i < convLength; i++) {
			cout[i] = cout[i]/N;
		}
		cout = circshift(cout,N-1);
		return cout;
	}*/
    
    public void testcircshift() {
    	int i;
    	int x[] = new int[]{0,1,2,3,4,5};
    	int result[] = circshift(x, 3);
    	int answer[] = new int[]{3,4,5,0,1,2};
    	for (i = 0; i < 6; i++) {
    		System.out.println("result["+i+"] = " + result[i] + " answer["+i+"] = " + answer[i]);	
    	}
    	result = circshift(x, 0);
    	answer = new int[]{0, 1, 2, 3, 4, 5};
    	for (i = 0; i < 6; i++) {
    		System.out.println("result["+i+"] = " + result[i] + " answer["+i+"] = " + answer[i]);	
    	}
    	result = circshift(x,-3);
    	answer = new int[]{3, 4, 5, 0, 1, 2};
    	for (i = 0; i < 6; i++) {
    		System.out.println("result["+i+"] = " + result[i] + " answer["+i+"] = " + answer[i]);	
    	}
    }
    
    private int[] circshift(int x[], int n) {
    	int i;
    	n = n % x.length;
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
    
    private double[] circshift(double x[], int n) {
    	int i;
    	n = n % x.length;
    	double y[] = new double[x.length];
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
    
    public void testcorrcoef() {
    	int x[] = new int[]{1, 5, 6, 9, -8, 11};
    	double y[] = new double[]{9.7, 3.1, 6.2, -1.2, 0.0, 3.5};
    	double answer = 0.035919004668078;
    	double result = corrcoef(x, y);
    	System.out.println("result = " + result + " answer = " + answer);
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

}
