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

import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmSeparableConvolver;
import gov.nih.mipav.model.algorithms.NLConstrainedEngine;
import gov.nih.mipav.model.algorithms.NMSimplex;
import gov.nih.mipav.model.algorithms.NelderMead;
import gov.nih.mipav.model.algorithms.Statistics;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlurSep;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileTypeTable;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.util.DoubleDouble;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.imageio.ImageIO;

import Jama.Matrix;
import Jama.SingularValueDecomposition;

import java.awt.Color;
import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;




public class PlugInAlgorithmTSPAnalysis extends AlgorithmBase implements MouseListener {
    
    private String pwiImageFileDirectory;
    private ModelImage pwiImage = null;
    private boolean spatialSmoothing = true;
    private float sigmax = 5.0f;
    private float sigmay = 5.0f;
    
    private boolean calculateMaskingThreshold = true;
    
    // Thresholding to mask out image pixels not corresponding to brain tissue
    private int masking_threshold = 600;
    
    private double TSP_threshold = 0.8;
    
    private int TSP_iter = 4;
    
    private double Psvd = 0.1;
    
    private boolean autoAIFCalculation = true;
    
    private boolean plotAIF = true;
    
    private boolean multiThreading = true;
    
    public static final int ELSUNC_2D_SEARCH = 1;
	
    public static final int ELSUNC_1D_SEARCH = 2;
	
    public static final int NMSIMPLEX_2D_SEARCH = 3;
	
    public static final int NELDERMEAD_2D_SEARCH = 4;
	
	private int search = ELSUNC_2D_SEARCH;
	
	private boolean calculateCorrelation = true;
	
	private boolean calculateCBFCBVMTT = true;
	
	private boolean calculateBounds = false;
	
	private String fileNameBase = "IM";
    
    private ModelImage pickImage;
    
    private final Lock accessLock = new ReentrantLock();
    private final Condition canProcessMouseClick = accessLock.newCondition();
	private int xS;
	private int yS;
	private double D_inv[][];
	
//    private ModelImage MTTImage;
    private ModelImage TmaxImage;
//    private ModelImage delay_mapImage;
    
    private File aifFile = null;
    private File sliceAifFile = null;
    
    private String outputFilePath = null;
    private String outputPrefix = "";
    
    private FileIO fileIO = null;
    
    private int saveFileFormat = FileUtility.NIFTI;
    
    // used by CoreTool to only save AIF/sliceAIF pngs, if chosen by the user
    private boolean doSaveAllOutputs = true;
	
    /**
     * Constructor.
     *
     */
    public PlugInAlgorithmTSPAnalysis(String pwiImageFileDirectory, boolean spatialSmoothing, float sigmax, float sigmay, 
    		boolean calculateMaskingThreshold, int masking_threshold,
    		double TSP_threshold, int TSP_iter, double Psvd, boolean autoAIFCalculation, boolean plotAIF,
    		boolean multiThreading, int search, boolean calculateCorrelation, 
    		boolean calculateCBFCBVMTT, boolean calculateBounds, String fileNameBase) {
        //super(resultImage, srcImg);
    	this.pwiImageFileDirectory = pwiImageFileDirectory;
    	this.spatialSmoothing = spatialSmoothing;
    	this.sigmax = sigmax;
    	this.sigmay = sigmay;
    	outputFilePath = pwiImageFileDirectory;
    	this.calculateMaskingThreshold = calculateMaskingThreshold;
    	this.masking_threshold = masking_threshold;
    	this.TSP_threshold = TSP_threshold;
    	this.TSP_iter = TSP_iter;
    	this.Psvd = Psvd;
    	this.autoAIFCalculation = autoAIFCalculation;
    	this.plotAIF = plotAIF;
    	this.multiThreading = multiThreading;
    	this.search = search;
    	this.calculateCorrelation = calculateCorrelation;
    	this.calculateCBFCBVMTT = calculateCBFCBVMTT;
    	this.calculateBounds = calculateBounds;
    	this.fileNameBase = fileNameBase;
    }
    
    public PlugInAlgorithmTSPAnalysis(ModelImage pwiImage, boolean calculateMaskingThreshold, int masking_threshold,
            double TSP_threshold, int TSP_iter, double Psvd, boolean autoAIFCalculation,
            boolean multiThreading, int search, boolean calculateCorrelation,
            boolean calculateCBFCBVMTT, boolean calculateBounds) {
        //super(resultImage, srcImg);
        this.pwiImageFileDirectory = pwiImage.getImageDirectory();
        this.pwiImage = pwiImage;
        outputFilePath = pwiImageFileDirectory;
        this.calculateMaskingThreshold = calculateMaskingThreshold;
        this.masking_threshold = masking_threshold;
        this.TSP_threshold = TSP_threshold;
        this.TSP_iter = TSP_iter;
        this.Psvd = Psvd;
        this.autoAIFCalculation = autoAIFCalculation;
        this.multiThreading = multiThreading;
        this.search = search;
        this.calculateCorrelation = calculateCorrelation;
        this.calculateCBFCBVMTT = calculateCBFCBVMTT;
        this.calculateBounds = calculateBounds;
        //this.fileNameBase = fileNameBase;
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int tDim;
    	float delT;
        short data[][][][];
    	double TE;
    	short Tmax[][][];
    	double CBV[][][] = null;
    	double CBF[][][] = null;
    	double MTT[][][] = null;
    	//double chiSquared[][][];
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
    	short buffer[];
    	short brain_mask[];
    	double temp_mean[];
    	short brain_mask_norm[][][][];
    	double dbuffer[];
    	int x, y, z, t;
    	int i,j,ii,jj;
    	long sum[];
    	long sumt;
    	int count[];
    	int countt;
    	// Remove hyphen from corr_map so MIPAV does not read corr_map and corr_map2 together as 1 file.
    	double corrmap[][][] = null;
    	double corr_map2[][][] = null;
    	short delay_map[][][];
    	short peaks_map[][][];
    	double temp[];
    	double maxTemp;
    	int maxIndex;
    	double cc;
    	int it;
    	short brain_mask2[];
    	short brain_mask_norm2;
    	short maxPeak;
    	String delZString;
    	float delZ;
    	ModelImage corr_map2Image;
    	ModelImage corrmapImage;
    	ModelImage peaks_mapImage;
    	ModelImage delay_mapImage;
    	String TEString;
    	short data_norm;
    	short peaks[][][];
    	short ttp[][][];
    	short minpeaks;
    	short minttp;
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
    	int dim = 2;
        double eps = 1.0e-8;
        double scale = 1.0;
        boolean display = false;
    	double xdata[];
    	double C[];
    	double b[];
    	double sumb;
    	double rcbf;
    	expfun1D minsearch1D;
    	expfun2D minsearch2D;
    	expfunNM minsearchNM;
    	expfunNM2 minsearchNM2;
    	int n = 2;
    	double tolx = 1.0E-8;
    	double tolf = 1.0E-8;
    	int max_iter = 5000;
    	int max_eval = 5000;
    	boolean verbose = false;
    	int exitStatus;
    	double p[] = new double[2];
    	ModelImage CBFImage;
    	ModelImage CBVImage;
    	ModelImage TTPImage;
    	//ModelImage chiSquaredImage;
    	long sumT[];
    	int countT[];
    	double num;
    	double denom;
    	double num1;
    	double denom1;
    	int nPts;
    	double covarMat[][] = null;
    	double errorSumOfSquares;
    	double s2;
    	double arr2D[][] = new double[2][2];
    	double det;
    	double invDiag2D[] = new double[2];
    	double se0;
    	double se1;
    	// Distance from value to lower and upper 95% confidence bounds
    	double p0MaxDistFromValue[][][] = null;
    	double p1MaxDistFromValue[][][] = null;
    	ModelImage p0MaxDistImage;
    	ModelImage p1MaxDistImage;
    	double expval;
    	double t975[] = new double[1];
    	boolean test = false;
    	boolean Philips = true;
    	String subfolder = null;
    	String subsubfolder = null;
    	
    	if (test) {
    		testxcorr();
    		testcircshift();
    		testcorrcoef();
    		testexpfun();
    		setCompleted(true);
    		return;
    	}

        ModelImage image3D;
        if (pwiImage == null) {
        	fireProgressStateChanged("Opening selected images...");
        	File folder = new File(pwiImageFileDirectory);
        	int selectedFileNumber = 0;
        	for (File fileEntry : folder.listFiles()) {
        		if (!fileEntry.isDirectory()) {
        			if (fileEntry.getName().length() > fileNameBase.length()) {
        			    String startName = fileEntry.getName().substring(0,fileNameBase.length());
        			    if (startName.equalsIgnoreCase(fileNameBase)) {
        			    	selectedFileNumber++;
        			    }
        			}
        		}
        		else {
        			for (File fileEntry2 : fileEntry.listFiles()) {
        				if (!fileEntry2.isDirectory()) {
        					if (fileEntry2.getName().length() > fileNameBase.length()) {
        	    			    String startName = fileEntry2.getName().substring(0,fileNameBase.length());
        	    			    if (startName.equalsIgnoreCase(fileNameBase)) {
        	    			        if (subfolder == null) {
                                        subfolder = fileEntry.getName();
                                        pwiImageFileDirectory = pwiImageFileDirectory + File.separator + subfolder;
                                    }
        	    			    	selectedFileNumber++;
        	    			    }
        	    			}	
        				}
        				else {
        					for (File fileEntry3 : fileEntry2.listFiles()) {
        	    				if (!fileEntry3.isDirectory()) {
        	    					if (fileEntry3.getName().length() > fileNameBase.length()) {
        	    	    			    String startName = fileEntry3.getName().substring(0,fileNameBase.length());
        	    	    			    if (startName.equalsIgnoreCase(fileNameBase)) {
        	    	    			        if (subsubfolder == null) {
                                                subsubfolder = fileEntry2.getName();
                                                pwiImageFileDirectory = pwiImageFileDirectory + File.separator + fileEntry.getName() + File.separator + subsubfolder;
                                            }
        	    	    			    	selectedFileNumber++;
        	    	    			    }
        	    	    			}	
        	    				}
        					}
        				}
        			}
        		}
        	}

        	if (selectedFileNumber == 0) {
        		MipavUtil.displayError("No selected image files were found");
        		setCompleted(false);
        		return;
        	}
        	
        	folder = new File(pwiImageFileDirectory);
        	
        	String fileList[] = new String[selectedFileNumber];
        	int index = 0;
        	for (File fileEntry : folder.listFiles()) {
        		if (!fileEntry.isDirectory()) {
        			if (fileEntry.getName().length() > fileNameBase.length()) {
        			    String startName = fileEntry.getName().substring(0,fileNameBase.length());
        			    if (startName.equalsIgnoreCase(fileNameBase)) {
        			    	fileList[index++] = fileEntry.getName();
        			    }
        			}
        		}
        	}
        	String selectedFileName = fileList[0];
        	FileIO fileIO = new FileIO(); 
        	fileIO.setFileDir(pwiImageFileDirectory + File.separator);
        	boolean performSort = true;
        	fileIO.setQuiet(true);
        	fileIO.setSuppressProgressBar(true);
        	image3D = fileIO.readDicom(selectedFileName, fileList, performSort);
        	image3D.calcMinMax();
        } else {
            image3D = pwiImage;
        }

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
    	// Use echo time in milliseconds
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
        if ((tDim <= 2) || (tDim > 500)) {
        	MipavUtil.displayError("Exiting with an impossible time dimension value of " + tDim);
        	setCompleted(false);
        	return;
        }
        extents[3] = tDim;
        zDim = extents3Dorg[2]/tDim;
        if ((zDim <= 2) || (zDim > 500)) {
        	MipavUtil.displayError("Exiting with an impossible z dimension value of " + zDim);
        	setCompleted(false);
        	return;
        }
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
        fireProgressStateChanged("Exporting data  ...");
        fireProgressStateChanged(10);
        buffer = new short[dataSize];
    	try {
    		image3D.exportData(0,  dataSize, buffer);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on image3D.exportData");
    		setCompleted(false);
    		return;
    	}
    	// if the image was opened outside of the algorithm, let the caller clean it up
    	if (pwiImage == null) {
    	    image3D.disposeLocal();
    	    image3D = null;
    	}
    	
    	data = new short[zDim][yDim][xDim][tDim];
    	// Start TSP processing
    	brain_mask = new short[tDim];
    	temp_mean = new double[tDim];
    	brain_mask_norm = new short[zDim][yDim][xDim][tDim];
    	// Normalize PWI by subtracting off pre_contrast (first) image
    	// Loop over time dimension to calculate whole brain average perfusion time signal
    	
		
    	if (Philips) {
	    	for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						for (t = 0; t < tDim; t++) {
							if (t == 0) {
							    data[z][y][x][t] =(short)Math.round( (3.0*buffer[x + y*xDim + t*length + z*tDim*length] + 
							    		buffer[x + y*xDim + (t+1)*length + z*tDim*length])/4.0);
							}
							else if (t == tDim-1) {
								data[z][y][x][t] =(short)Math.round( (buffer[x + y*xDim + (t-1)*length + z*tDim*length] + 
							    		3.0*buffer[x + y*xDim + t*length + z*tDim*length])/4.0);	
							}
							else {
								data[z][y][x][t] =(short)Math.round( (buffer[x + y*xDim + (t-1)*length + z*tDim*length] + 
							    		3.0*buffer[x + y*xDim + t*length + z*tDim*length] + 
							    		buffer[x + y*xDim + (t+1)*length + z*tDim*length])/5.0);		
							}
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
							if (t == 0) {
						        data[z][y][x][t] = (short)Math.round( (3.0*buffer[x + y*xDim + z*length + t*volume] +
						    		buffer[x + y*xDim + z*length + (t+1)*volume])/4.0);
							}
							else if (t == tDim-1) {
								data[z][y][x][t] = (short)Math.round( (buffer[x + y*xDim + z*length + (t-1)*volume] +
							    		3.0*buffer[x + y*xDim + z*length + t*volume])/4.0);	
							}
							else {
								data[z][y][x][t] = (short)Math.round( (buffer[x + y*xDim + z*length + (t-1)*volume] +
							    		3.0*buffer[x + y*xDim + z*length + t*volume] + 
							    		buffer[x + y*xDim + z*length + (t+1)*volume])/5.0);		
							}
						}
					}
				}
			}	
    	}
    	
    	extents2D = new int[] {xDim,yDim};
    	if (spatialSmoothing) {
	    	fireProgressStateChanged("2D Gaussian blurring  ...");
	        fireProgressStateChanged(15);
	    	float outputBuffer[];
	    	float inputBuffer[] = new float[length];
	    	float[] sigmas = new float[] {sigmax, sigmay};
	    	GaussianKernelFactory kernelFactory = GaussianKernelFactory.getInstance(sigmas);
	    	kernelFactory.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
	    	Kernel gaussianKernel = kernelFactory.createKernel();
	    	boolean color = false;
	    	for (z = 0; z < zDim; z++) {
	    		for (t = 0; t < tDim; t++) {
	    			for (y = 0; y < yDim; y++) {
	    				for (x = 0; x < xDim; x++) {
	    					inputBuffer[x + y * xDim] = data[z][y][x][t];
	    				}
	    			}
	    			
	    			AlgorithmSeparableConvolver convolver = new AlgorithmSeparableConvolver(
	    					inputBuffer, extents2D, gaussianKernel.getData(), color);
	                convolver.run();
	    	        if (threadStopped) {
	    	            setCompleted(false);
	    	            finalize();
	
	    	            return;
	    	        }
	    	        
	    	        outputBuffer = convolver.getOutputBuffer();
	    	        if (threadStopped) {
	    	            setCompleted(false);
	    	            finalize();
	
	    	            return;
	    	        }
	    	        
	    	        for (y = 0; y < yDim; y++) {
	    				for (x = 0; x < xDim; x++) {
	    					data[z][y][x][t] = (short)Math.round(outputBuffer[x + y*xDim]);
	    				}
	    			}
	    	        convolver.finalize();
	    		}
	    	}
    	} // if (spatialSmoothing)
    	
    	if (calculateMaskingThreshold) {
    		long dataSum = 0;
    		for (z = 0; z < zDim; z++) {
        		for (t = 0; t < tDim; t++) {
        			for (y = 0; y < yDim; y++) {
        				for (x = 0; x < xDim; x++) {
    			            dataSum = dataSum + data[z][y][x][t];
        				}
        			}
        		}
    		}
    		double mean = (double)dataSum/(double)dataSize;
    		double squareSum = 0.0;
    		for (z = 0; z < zDim; z++) {
        		for (t = 0; t < tDim; t++) {
        			for (y = 0; y < yDim; y++) {
        				for (x = 0; x < xDim; x++) {
    			            double difference = data[z][y][x][t] - mean;
    			            squareSum += (difference * difference);
        				}
        			}
        		}
    		}
    		double stdDev = Math.sqrt(squareSum/(double)(dataSize-1));
    		masking_threshold = (int)Math.round(mean + 0.5 * stdDev);
    	} // if (calculateMaskingThreshold)
    	
		
		sum = new long[tDim];
		count = new int[tDim];
    	for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					for (t = 0; t < tDim; t++) {
						if (data[z][y][x][t] < masking_threshold) {
							brain_mask[t] = 0;
						}
						else {
							brain_mask[t] = data[z][y][x][t];
						}
						brain_mask_norm[z][y][x][t] = (short)(brain_mask[t] - brain_mask[0]);
						if (brain_mask_norm[z][y][x][t] != 0) {
					    	sum[t] += brain_mask_norm[z][y][x][t];
					    	count[t]++;
					    }
					}
		        }
	        }
        }
		for (t = 1; t < tDim; t++) {
			temp_mean[t] = (double)sum[t]/(double)count[t];
		}
	    temp_mean[0] = 0;
	    //for (i = 0; i < temp_mean.length; i++) {
	    	//System.out.println("temp_mean["+i+"] = " + temp_mean[i]);
	    	//Preferences.debug("temp_mean["+i+"] = " + temp_mean[i] + "\n", Preferences.DEBUG_ALGORITHM);
	    //}
    	
    	
    	// Zero/Initialize output maps
	    if (calculateCorrelation) {
	        // TSP Correlation map with out AIF delay compensation
	    	corrmap = new double[zDim][yDim][xDim];
	    	// TSP Correlation map with AIF delay compensation
	    	corr_map2 = new double[zDim][yDim][xDim];
	    }
    	// TSP Delay map considering temporal similarity with whole brain average
    	delay_map = new short[zDim][yDim][xDim];
    	// TSP Peaks map is the absolute value of the SI corresponding to the largest deviation from baseline
    	peaks_map = new short[zDim][yDim][xDim];
    	
    	// First TSP iteration, Loop over all voxels.  First find delay from cross-correlation.
    	// Then find Correlation Coefficient after shifting by delay
    	fireProgressStateChanged("First TSP iteration  ...");
        fireProgressStateChanged(20);
    	if (multiThreading) {
    		ExecutorService executorService = Executors.newCachedThreadPool();	
            for (z = 0; z < zDim; z++) {
            	fireProgressStateChanged("First TSP iteration launching multithread " + (z+1) + " of " + zDim);
                fireProgressStateChanged(20 + (9 * z)/(zDim-1));
                if (calculateCorrelation) {
	            	executorService.execute(new corr1Calc(xDim,yDim,tDim,brain_mask_norm[z],delay_map[z],corr_map2[z],
	            			temp_mean.clone()));	
                }
                else {
                	executorService.execute(new corr1Calc(xDim,yDim,tDim,brain_mask_norm[z],delay_map[z],null,
	            			temp_mean.clone()));		
                }
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
	    		fireProgressStateChanged("First TSP iteration doing slice " + (z+1) + " of " + zDim);
                fireProgressStateChanged(20 + (9 * z)/(zDim-1));
	    		for (y = 0; y < yDim; y++) {
	    			for (x = 0; x < xDim; x++) {
	    				sumt = 0;
	    				for (t = 1; t < tDim; t++) {
	    					sumt += brain_mask_norm[z][y][x][t];
	    				}
	    				if (sumt != 0) {
	    				    temp = xcorrbias(brain_mask_norm[z][y][x], temp_mean);
	    				    maxTemp = -Double.MAX_VALUE;
	    				    maxIndex = -1;
	    				    for (i = 0; i < temp.length; i++) {
	    				    	if (temp[i] > maxTemp) {
	    				    		maxTemp = temp[i];
	    				    		maxIndex = i+1;
	    				    	}
	    				    }
	    				    delay_map[z][y][x] = (short)maxIndex;
	    				    if (calculateCorrelation) {
		    				    cc = corrcoef(circshift(brain_mask_norm[z][y][x], -maxIndex + tDim), temp_mean);
		    				    corr_map2[z][y][x] = cc;
	    				    }
	    				} // if (sumt != 0)
	    			}
	    		}
	    	} // for (z = 0; z < zDim; z++)
    	} // else single thread
    	
    	// Following TSP iterations, Recalc who brain average (healthy)
    	// considering only tissue with correlations > TSP threshold
    	brain_mask2 = new short[tDim];
    	sumT = new long[tDim];
    	countT = new int[tDim];
    	for (it = 1; it <= TSP_iter; it++) {
    		fireProgressStateChanged("Later TSP iteration number " + it + " out of " + TSP_iter);
            fireProgressStateChanged(30 + 40 * (it-1)/TSP_iter);
    		for (t = 1; t < tDim; t++) {
    			sumT[t] = 0;
    			countT[t] = 0;
    		}
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					if ((corr_map2 != null) && (corr_map2[z][y][x] < TSP_threshold)) {
    						for (t = 0; t < tDim; t++) {
    						    brain_mask2[t] = 0;
    						    brain_mask_norm2 = 0;
    						}
    					}
    					else {
    						for (t = 0; t < tDim; t++) {
    						    brain_mask2[t] = data[z][y][x][t];
    						    brain_mask_norm2 = (short)(brain_mask2[t] - brain_mask2[0]);
    						    if (brain_mask_norm2 != 0) {
        					    	sumT[t] += brain_mask_norm2;
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
                	if (calculateCorrelation) {
                	    executorService.execute(new corr2Calc(xDim,yDim,tDim,brain_mask_norm[z],delay_map[z],peaks_map[z],corrmap[z],
                			corr_map2[z], temp_mean.clone()));
                	}
                	else {
                		executorService.execute(new corr2Calc(xDim,yDim,tDim,brain_mask_norm[z],delay_map[z],peaks_map[z],null,
                    			null,temp_mean.clone()));	
                	}
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
	        				sumt = 0;
	        				for (t = 1; t < tDim; t++) {
	        					sumt += brain_mask_norm[z][y][x][t];
	        				}
	        				if (sumt != 0) {
	        				    temp = xcorrbias(brain_mask_norm[z][y][x], temp_mean);
	        				    maxTemp = -Double.MAX_VALUE;
	        				    maxIndex = -1;
	        				    for (i = 0; i < temp.length; i++) {
	        				    	if (temp[i] > maxTemp) {
	        				    		maxTemp = temp[i];
	        				    		maxIndex = i+1;
	        				    	}
	        				    }
	        				    delay_map[z][y][x] = (short)maxIndex;
	        				    maxPeak = Short.MIN_VALUE;
	        				    for (t = 0; t < tDim; t++) {
	        				    	if (Math.abs(brain_mask_norm[z][y][x][t]) > maxPeak) {
	        				    		maxPeak = (short)Math.abs(brain_mask_norm[z][y][x][t]);
	        				    	}
	        				    }
	        				    peaks_map[z][y][x] = maxPeak;
	        				    if (calculateCorrelation) {
		        				    cc = corrcoef(brain_mask_norm[z][y][x], temp_mean);
		        				    corrmap[z][y][x] = cc;
		        				    cc = corrcoef(circshift(brain_mask_norm[z][y][x], -maxIndex + tDim), temp_mean);
		        				    corr_map2[z][y][x] = cc;
	        				    }
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
    			    	delay_map[z][y][x] = (short)(tDim + 40);
    			    }
    			    else if (delay_map[z][y][x] < tDim - 40) {
    			    	delay_map[z][y][x] = (short)(tDim - 40);
    			    }
    			    // Dealy delT mutliplication to point of image readin so we can use a short array
    			    //delay_map[z][y][x] = (delay_map[z][y][x] - tDim) * delT;
    			    delay_map[z][y][x] = (short)(delay_map[z][y][x] - tDim);
    			    if (calculateCorrelation) {
	    			    // Corr map > 1 or < 0 is not realistic
	    			    if (corrmap[z][y][x] > 1) {
	    			    	corrmap[z][y][x] = 1;
	    			    }
	    			    else if (corrmap[z][y][x] < 0) {
	    			    	corrmap[z][y][x] = 0;
	    			    }
    			    }

    			}
    		}
    	} // for (x = 0; x < xDim; x++)
    	
    	// Write images and clean up variable
    	
    	fireProgressStateChanged("Writing first set of images");
        fireProgressStateChanged(70);
    	dbuffer = new double[volume];
    	FileInfoBase fileInfo[];
    	
    	if (calculateCorrelation) {
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
	    	fileInfo = corr_map2Image.getFileInfo();
	    	for (i = 0; i < zDim; i++) {
	    		fileInfo[i].setResolutions(resolutions3D);
	    		fileInfo[i].setUnitsOfMeasure(units3D);
	    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(corr_map2Image, outputFilePath, outputPrefix + "corr_map2", saveFileFormat);
	        
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
	    	
	    	saveImageFile(corrmapImage, outputFilePath, outputPrefix + "corrmap", saveFileFormat);
	    	
	    	corrmapImage.disposeLocal();
	    	corrmapImage = null;
    	} // if (calculateCorrelation)
    	
    	peaks_mapImage = new ModelImage(ModelStorageBase.SHORT, extents3D, "peaks_map");
    	buffer = new short[volume];
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				buffer[x + y*xDim + z*length] = peaks_map[z][y][x];
    			}
    		}
    	}
    	try {
    		peaks_mapImage.importData(0, buffer, true);
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
    		fileInfo[i].setDataType(ModelStorageBase.SHORT);
    	}
    	
    	saveImageFile(peaks_mapImage, outputFilePath, outputPrefix + "peaks_map", saveFileFormat);

    	peaks_mapImage.disposeLocal();
    	peaks_mapImage = null;
    	
    	delay_mapImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "delay_map");
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			for (z = 0; z < zDim; z++) {
    				dbuffer[x + y*xDim + z*length] = delay_map[z][y][x] * delT;
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
    	
    	saveImageFile(delay_mapImage, outputFilePath, outputPrefix + "delay_map", saveFileFormat);
    	
    	delay_mapImage.disposeLocal();
    	delay_mapImage = null;
    	
    	// Deconvolution analysis
    	fireProgressStateChanged("Deconvolution analysis");
        fireProgressStateChanged(75);
    	S = new double[tDim];
    	peaks = new short[zDim][yDim][xDim];
    	ttp = new short[zDim][yDim][xDim];
    	for (z = 0; z < zDim; z++) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					minpeaks = Short.MAX_VALUE;
					minttp = Short.MAX_VALUE;
					for (t = 0; t < tDim; t++) {
						data_norm = (short)(data[z][y][x][t] - data[z][y][x][0]);	
						if (data_norm < minpeaks) {
							minpeaks = data_norm;
							minttp = (short)(t+1);
						}
					}
					peaks[z][y][x] = minpeaks;
					ttp[z][y][x] = minttp;
				}
			}	
		} // for (z = 0; z < zDim; z++)
    	double xsum = 0.0;
		double ysum = 0.0;
		double zsum = 0.0;
		double xsumsquared = 0.0;
		double ysumsquared = 0.0;
		double zsumsquared = 0.0;
		int sumcount = 0;
		double xmean = 0.0;
		double ymean = 0.0;
		double zmean = 0.0;
		double xstd = 0.0;
		double ystd = 0.0;
		double zstd = 0.0;
    	if (autoAIFCalculation) {
	    	// Auto AIF Calculation
	    	// AIF is average signal of pixels with the largest SI deviations
	    	// (4 std) from baseline (likely to be large vessels)
	    	sumt = 0;
		    countt = 0;
		    for (z = 0; z < zDim; z++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
					    if (peaks[z][y][x] != 0) {
					    	sumt += peaks[z][y][x];
					    	countt++;
					    }
					}
				}
		    }
		    peaks_mean = (double)sumt/(double)countt;
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
		    peaks_std = Math.sqrt(diff_squared_sum/(countt-1));
		    peaks_threshold = peaks_mean - 4.0*peaks_std;
		    autoaif = new double[tDim];
		    minautoaif = Double.MAX_VALUE;
		    int numcountt = 0;
		    for (t = 0; t < tDim; t++) {
		        sumt = 0;
		        countt = 0;
		        for (z = 0; z < zDim; z++) {
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							if (peaks[z][y][x] < peaks_threshold) {
								if (t == 0) {
									sumcount++;
									xsum += x;
									ysum += y;
									zsum += z;
									xsumsquared += (x*x);
									ysumsquared += (y*y);
									zsumsquared += (z*z);
								}
								data_norm = (short)(data[z][y][x][t] - data[z][y][x][0]);
							    sumt += data_norm;
							    countt++;
							}
						}
					}
		        }
		        if (t == 0) {
		        	xmean = xsum/sumcount;
		        	ymean = ysum/sumcount;
		        	zmean = zsum/sumcount;
		        	if (sumcount > 1) {
		        	    xstd = Math.sqrt((xsumsquared - xsum*xsum/sumcount)/(sumcount - 1.0));
		        	    ystd = Math.sqrt((ysumsquared - ysum*ysum/sumcount)/(sumcount - 1.0));
		        	    zstd = Math.sqrt((zsumsquared - zsum*zsum/sumcount)/(sumcount - 1.0));
		        	    System.out.println("AIF selection point standard deviations in voxels:");
		        	    System.out.println("x standard deviation = " + xstd);
		        	    System.out.println("y standard deviation = " + ystd);
		        	    System.out.println("z standard deviation = " + zstd);
		        	} // if (sumcount > 1)
		        } // if (t == 0)
		        if (countt == 0) {
		        	System.err.println("No AIF selection pixels found for t = " + t);
		        	numcountt++;
		        }
		        autoaif[t] = (double)sumt/(double)countt;
		        if (autoaif[t] < minautoaif) {
		        	minautoaif = autoaif[t];
		        }
		    } // for (t = 0; t < tDim; t++)
		    // time signal from mri
		    if (numcountt > 0) {
		    	 MipavUtil.displayError("No AIF selection pixels found at " + numcountt + " out of " + tDim + " times");
		    	 setCompleted(false);
		    	 return;
		    }
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
		    zmean = 9;
		    ymean = yS;
		    xmean = xS;
		    sumcount = 1;
    	} // else pick image pixel corresponding to AIF
	    
	    // Calculate AIF as amount of contrast agent as estimated from R2
	    Ca = new double[tDim];
	    for (t = 1; t < tDim; t++) {
	    	Ca[t] = -TE*Math.log(S[t]/S[0]);
	    }
	    Ca[0] = 0;
	    if (plotAIF) {
	    	float xInit[] = new float[tDim];
	    	float yInit[] = new float[tDim];
	    	for (t = 0; t < tDim; t++) {
	    		xInit[t] = delT * t;
	    		yInit[t] = (float)Ca[t];
	    	}
	    	String title = "AIF";
	    	String labelX = "Scan time in seconds";
	    	String labelY = "Change in MR Contrast";
	    	boolean visible = true;
	    	ViewJFrameGraph vGraph = new ViewJFrameGraph(xInit, yInit, title, labelX, labelY, visible);
	    	try {
	    	  vGraph.save(outputFilePath + outputPrefix + "AIF.plt");
	    	  Component component = vGraph.getComponent(0);
	    	  Rectangle rect = component.getBounds();
	    	  String format = "png";
  	          BufferedImage captureImage =
  	                new BufferedImage(rect.width, rect.height,
  	                                    BufferedImage.TYPE_INT_ARGB);
  	          component.paint(captureImage.getGraphics());
  	 
  	          aifFile = new File(outputFilePath + outputPrefix + "AIF.png");
  	          ImageIO.write(captureImage, format, aifFile);
  	          vGraph.dispose();
  	          
		      short shortBuffer[] = new short[length];
  	          for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		shortBuffer[x + y * xDim] = data[(int)Math.round(zmean)][y][x][0];
		    	}
		       }
		       extents2D = new int[]{xDim,yDim};
		       ModelImage AIFImage = new ModelImage(ModelStorageBase.SHORT,extents2D,"AIFImage");
			    try {
			    	AIFImage.importData(0, shortBuffer, true);
			    }
			    catch (IOException e) {
			    	MipavUtil.displayError("IOException on shortImage.importData");
			    	setCompleted(false);
			    	return;
			    }
			    VOI newPtVOI = new VOI((short) 0, "pointAIF.voi", VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.RED);
                float xArr[] = new float[] {(float)xmean};
                float yArr[] = new float[] {(float)ymean};
                float zArr[] = new float[] {0.0f};
                newPtVOI.importCurve(xArr, yArr, zArr);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel("AIF Point");
                AIFImage.registerVOI(newPtVOI);
			    ViewJFrameImage vFrame = new ViewJFrameImage(AIFImage);
			    component = vFrame.getComponent(0);
			    rect = component.getBounds();
		    	format = "png";
	  	        captureImage =
	  	                new BufferedImage(rect.width, rect.height,
	  	                                    BufferedImage.TYPE_INT_ARGB);
	  	        component.paint(captureImage.getGraphics());
	  	 
	  	        sliceAifFile = new File(outputFilePath + outputPrefix + "sliceAIF.png");
	  	        ImageIO.write(captureImage, format, sliceAifFile);
	  	        vFrame.dispose();
	    	}
	    	catch (IOException e) {
                MipavUtil.displayError("Error: " + e + "\n");
                setCompleted(false);
                return;
            }
	    	
	    } // if (plotAIF)
	    
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
        fireProgressStateChanged("Iterate over brain volume to find Tmax");
        fireProgressStateChanged(80);
        if (calculateCBFCBVMTT) {
            CBV = new double[zDim][yDim][xDim];
            CBF = new double[zDim][yDim][xDim];
            MTT = new double[zDim][yDim][xDim];
        }
        Tmax = new short[zDim][yDim][xDim];
        //relCBF = new double[xDim][yDim][zDim];
        TTP = new double[zDim][yDim][xDim];
        //chiSquared = new double[zDim][yDim][xDim];
        if (calculateBounds) {
        	p0MaxDistFromValue = new double[zDim][yDim][xDim];
        	p1MaxDistFromValue = new double[zDim][yDim][xDim];
        	Statistics stat = new Statistics(Statistics.STUDENTS_T_INVERSE_CUMULATIVE_DISTRIBUTION_FUNCTION,.975,2*tDim-2,t975);
        	stat.run();
        }
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
            	fireProgressStateChanged("Iterating to find Tmax launching thread " + (z+1) + " of " + zDim);
                fireProgressStateChanged(80 + (14 * z)/(zDim-1));
            	if (calculateBounds) {
            		if (calculateCBFCBVMTT) {
            	        executorService.execute(new endCalc(search, xDim,yDim,tDim,delT,TE,masking_threshold,
            			data[z],CBV[z],CBF[z],MTT[z],Tmax[z],p0MaxDistFromValue[z], p1MaxDistFromValue[z],t975[0]/*,chiSquared[z]*/));	
            		}
            		else {
            			executorService.execute(new endCalc(search, xDim,yDim,tDim,delT,TE,masking_threshold,
                    			data[z],null,null,null,Tmax[z],p0MaxDistFromValue[z], p1MaxDistFromValue[z],t975[0]/*,chiSquared[z]*/));		
            		}
            	}
            	else {
            		if (calculateCBFCBVMTT) {
            		    executorService.execute(new endCalc(search, xDim,yDim,tDim,delT,TE,masking_threshold,
                			data[z],CBV[z],CBF[z],MTT[z],Tmax[z],null, null,t975[0]/*,chiSquared[z]*/));
            		}
            		else {
            			executorService.execute(new endCalc(search, xDim,yDim,tDim,delT,TE,masking_threshold,
                    			data[z],null,null,null,Tmax[z],null, null,t975[0]/*,chiSquared[z]*/));	
            		}
            	}
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
        	
		    xdata = new double[2*tDim];
		    for (i = 0; i < 2*tDim; i++) {
		    	xdata[i] = i * delT;
		    }
		    C = new double[2*tDim];
		    b = new double[2*tDim];
		    if (calculateBounds) {
		    	covarMat = new double[2*tDim][2];
		    }
		    // Iterate
		    for (z = 0; z < zDim; z++) {
		    	fireProgressStateChanged("Iterating to find Tmax doing slice " + (z+1) + " of " + zDim);
                fireProgressStateChanged(80 + (14 * z)/(zDim-1));
		    	for (y = 0; y < yDim; y++) {
		    		for (x = 0; x < xDim; x++) {
		    			if ((search == ELSUNC_2D_SEARCH) || (search == NMSIMPLEX_2D_SEARCH) || (search == NELDERMEAD_2D_SEARCH)) {
		    		    	x0 = new double[]{0.1,4};
		    		    }
		    		    else {
		            	    x0 = new double[]{4};
		    		    }
		    		    if (data[z][y][x][0] >= masking_threshold) {
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
		    		    	    		Tmax[z][y][x] = (short)(i+1);
		    		    	    	}
		    		    	    }
		    		    	    if (calculateCBFCBVMTT || calculateBounds) {
			    		    	    // Shift b to have a peak at origin for fitting
			    		    	    b = circshift(b,-Tmax[z][y][x]);
			    		    	    if (search == ELSUNC_2D_SEARCH) {
			    		    	    	minsearch2D = new expfun2D(x0, b, xdata);
				    		    	    minsearch2D.driver();
				    		    	    exitStatus = minsearch2D.getExitStatus();
				    		    	    p = minsearch2D.getParameters();
				    		    	    if ((exitStatus >= 0)  && (p[1] > 0) && (p[1] < 75)) {
				    		    	    	// Normal termination
				    		    	    	if (calculateCBFCBVMTT) {
					    		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
					    					    CBF[z][y][x] = p[0];
					    					    // relCBF is max value of residual function.  Should be similar to CBF,
					    					    // but may be different.
					    					    //relCBF[x][y][z] = rcbf;
					    					    MTT[z][y][x] = p[1];
					    					    CBV[z][y][x] = rcbf * p[1];
				    		    	    	}
				    					    //chiSquared[z][y][z] = minsearch.getChiSquared();
				    					    if (calculateBounds) {
					    					    nPts = xdata.length;
					    					    errorSumOfSquares = 0.0;
					    					    for (i = 0; i < nPts; i++) {
					    					    	expval = Math.exp(-1.0/p[1]*xdata[i]); 
					    						    covarMat[i][0] = expval;
					    						    covarMat[i][1] = xdata[i]/(p[1]*p[1])*p[0]*expval;
					    						    diff = (p[0]*expval) - b[i];
					    						    errorSumOfSquares += (diff * diff);
					    						}
					    					    s2 = errorSumOfSquares/(nPts - 2);
					    					    arr2D[0][0] = 0.0;
					    					    arr2D[0][1] = 0.0;
					    					    arr2D[1][0] = 0.0;
					    					    arr2D[1][1] = 0.0;
					    					    for (i = 0; i < nPts; i++) {
					    					    	arr2D[0][0] += covarMat[i][0]*covarMat[i][0];
					    					    	arr2D[0][1] += covarMat[i][0]*covarMat[i][1];
					    					    	arr2D[1][1] += covarMat[i][1]*covarMat[i][1];
					    					    }
					    					    arr2D[1][0] = arr2D[0][1];
					    					    det = arr2D[0][0]*arr2D[1][1] - arr2D[0][1]*arr2D[1][0];
					    					    if (det != 0.0) {
						    					    invDiag2D[0] = arr2D[1][1]/det;
						    					    invDiag2D[1] = arr2D[0][0]/det;
						    					    se0 = Math.sqrt(invDiag2D[0]*s2);
						    					    se1 = Math.sqrt(invDiag2D[1]*s2);
						    					    if ((!Double.isInfinite(se0))  && (!Double.isNaN(se0))) {
						    					        p0MaxDistFromValue[z][y][x] = t975[0] * se0;
						    					    }
						    					    if ((!Double.isInfinite(se1))  && (!Double.isNaN(se1))) {
						    					        p1MaxDistFromValue[z][y][x] = t975[0] * se1;
						    					    }
					    					    } // if (det != 0.0)
				    					    } // if (calculateBounds)
				    		    	    }	
			    		    	    } // if (search == ELSUNC_2D_SEARCH)
			    		    	    else if (search == NMSIMPLEX_2D_SEARCH) {
			    		    	    	minsearchNM = new expfunNM(x0, dim, eps, scale, display, b, xdata);
				    		    	    minsearchNM.driver();
				    		    	    if ((x0[1] > 0) && (x0[1] < 75)) {
				    		    	    	// Normal termination
				    		    	    	if (calculateCBFCBVMTT) {
					    		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
					    					    CBF[z][y][x] = x0[0];
					    					    // relCBF is max value of residual function.  Should be similar to CBF,
					    					    // but may be different.
					    					    //relCBF[x][y][z] = rcbf;
					    					    MTT[z][y][x] = x0[1];
					    					    CBV[z][y][x] = rcbf * x0[1];
				    		    	    	}
				    					    if (calculateBounds) {
					    					    nPts = xdata.length;
					    					    errorSumOfSquares = 0.0;
					    					    for (i = 0; i < nPts; i++) {
					    					    	expval = Math.exp(-1.0/x0[1]*xdata[i]);
					    						    covarMat[i][0] = expval;
					    						    covarMat[i][1] = xdata[i]/(x0[1]*x0[1])*x0[0]*expval;
					    						    diff = (x0[0]*expval) - b[i];
					    						    errorSumOfSquares += (diff * diff);
					    						}
					    					    s2 = errorSumOfSquares/(nPts - 2);
					    					    arr2D[0][0] = 0.0;
					    					    arr2D[0][1] = 0.0;
					    					    arr2D[1][0] = 0.0;
					    					    arr2D[1][1] = 0.0;
					    					    for (i = 0; i < nPts; i++) {
					    					    	arr2D[0][0] += covarMat[i][0]*covarMat[i][0];
					    					    	arr2D[0][1] += covarMat[i][0]*covarMat[i][1];
					    					    	arr2D[1][1] += covarMat[i][1]*covarMat[i][1];
					    					    }
					    					    arr2D[1][0] = arr2D[0][1];
					    					    det = arr2D[0][0]*arr2D[1][1] - arr2D[0][1]*arr2D[1][0];
					    					    if (det != 0.0) {
						    					    invDiag2D[0] = arr2D[1][1]/det;
						    					    invDiag2D[1] = arr2D[0][0]/det;
						    					    se0 = Math.sqrt(invDiag2D[0]*s2);
						    					    se1 = Math.sqrt(invDiag2D[1]*s2);
						    					    if ((!Double.isInfinite(se0))  && (!Double.isNaN(se0))) {
						    					        p0MaxDistFromValue[z][y][x] = t975[0] * se0;
						    					    }
						    					    if ((!Double.isInfinite(se1))  && (!Double.isNaN(se1))) {
						    					        p1MaxDistFromValue[z][y][x] = t975[0] * se1;
						    					    }
					    					    } // if (det != 0.0)
				    					    } // if (calculateBounds)
				    		    	    }
			    		    	    } // else if (search == NMSIMPLEX_2D_SEARCH)
			    		    	    else if (search == NELDERMEAD_2D_SEARCH) {
			    		    	    	minsearchNM2 = new expfunNM2(x0, n, tolx, tolf, max_iter, max_eval, verbose, b, xdata);
			    		    	    	minsearchNM2.driver();
			    		    	    	p = minsearchNM2.getSolX();
			    		    	    	if ((p[1] > 0) && (p[1] < 75)) {
				    		    	    	// Normal termination
			    		    	    		if (calculateCBFCBVMTT) {
					    		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
					    					    CBF[z][y][x] = p[0];
					    					    // relCBF is max value of residual function.  Should be similar to CBF,
					    					    // but may be different.
					    					    //relCBF[x][y][z] = rcbf;
					    					    MTT[z][y][x] = p[1];
					    					    CBV[z][y][x] = rcbf * p[1];
			    		    	    		}
				    					    //chiSquared[z][y][z] = minsearch.getChiSquared();
				    					    if (calculateBounds) {
					    					    nPts = xdata.length;
					    					    errorSumOfSquares = 0.0;
					    					    for (i = 0; i < nPts; i++) {
					    					    	expval = Math.exp(-1.0/p[1]*xdata[i]);
					    						    covarMat[i][0] = expval;
					    						    covarMat[i][1] = xdata[i]/(p[1]*p[1])*p[0]*expval;
					    						    diff = (p[0]*expval) - b[i];
					    						    errorSumOfSquares += (diff * diff);
					    						}
					    					    s2 = errorSumOfSquares/(nPts - 2);
					    					    arr2D[0][0] = 0.0;
					    					    arr2D[0][1] = 0.0;
					    					    arr2D[1][0] = 0.0;
					    					    arr2D[1][1] = 0.0;
					    					    for (i = 0; i < nPts; i++) {
					    					    	arr2D[0][0] += covarMat[i][0]*covarMat[i][0];
					    					    	arr2D[0][1] += covarMat[i][0]*covarMat[i][1];
					    					    	arr2D[1][1] += covarMat[i][1]*covarMat[i][1];
					    					    }
					    					    arr2D[1][0] = arr2D[0][1];
					    					    det = arr2D[0][0]*arr2D[1][1] - arr2D[0][1]*arr2D[1][0];
					    					    if (det != 0.0) {
						    					    invDiag2D[0] = arr2D[1][1]/det;
						    					    invDiag2D[1] = arr2D[0][0]/det;
						    					    se0 = Math.sqrt(invDiag2D[0]*s2);
						    					    se1 = Math.sqrt(invDiag2D[1]*s2);
						    					    if ((!Double.isInfinite(se0))  && (!Double.isNaN(se0))) {
						    					        p0MaxDistFromValue[z][y][x] = t975[0] * se0;
						    					    }
						    					    if ((!Double.isInfinite(se1))  && (!Double.isNaN(se1))) {
						    					        p1MaxDistFromValue[z][y][x] = t975[0] * se1;
						    					    }
					    					    } // if (det != 0.0)
				    					    } // if (calculateBounds)
				    		    	    }	
			    		    	    } // else if (search == NELDERMEAD_2D_SEARCH)
			    		    	    else if (calculateCBFCBVMTT) { // 1D search
				    		    	    minsearch1D = new expfun1D(x0, b, xdata);
				    		    	    minsearch1D.driver();
				    		    	    exitStatus = minsearch1D.getExitStatus();
				    		    	    p[1] = minsearch1D.getParameters()[0];
				    		    	    if ((exitStatus >= 0)  && (p[1] > 0) && (p[1] < 75)) {
				    		    	    	// Normal termination
				    		    	    	num1 = 0.0;
			        		    	    	denom1 = 0.0;
			        		    	    	for (i = 0; i < 2*tDim; i++) {
			                		    	    num = Math.exp(-xdata[i]/p[1]);
			            						denom = num * num;
			                                    num1 += b[i]*num;
			                                    denom1 += denom;
			        		    	    	}
			        		    	    	p[0] = num1/denom1;
				    		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
				    					    CBF[z][y][x] = p[0];
				    					    // relCBF is max value of residual function.  Should be similar to CBF,
				    					    // but may be different.
				    					    //relCBF[x][y][z] = rcbf;
				    					    MTT[z][y][x] = p[1];
				    					    CBV[z][y][x] = rcbf * p[1];
				    					    //chiSquared[z][y][z] = minsearch.getChiSquared();
				    		    	    }
			    		    	    } // else 1D search
		    		    	    } // if (calculateCBFCBVMTT || calculateBounds)
		    		    	} // if ((!Double.isNaN(sumb)) && (!Double.isInfinite(sumb)))
		    		    } // if ((data[z][y][x][0] >= masking_threshold)
		    		} // for (x = 0; x < xDim; x++)
		    	} // for (y = 0; y < yDim; y++)
		    } // for (z = 0; z < zDim; z++)
        } // else single processor
        
        // Write maps to images
        fireProgressStateChanged("Writing final set of images");
        fireProgressStateChanged(95);
        if (calculateCBFCBVMTT) {
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
	    	
	    	saveImageFile(CBFImage, outputFilePath, outputPrefix + "CBF", saveFileFormat);
	        
	    	CBFImage.disposeLocal();
	    	CBFImage = null;
	    	
	    	ModelImage MTTImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "MTT");
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
	    	
	    	saveImageFile(MTTImage, outputFilePath, outputPrefix + "MTT", saveFileFormat);
	        
	    	MTTImage.disposeLocal();
	    	MTTImage = null;
	    	
	    	CBVImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "CBV");
	    	for (x = 0; x < xDim; x++) {
	    		for (y = 0; y < yDim; y++) {
	    			for (z = 0; z < zDim; z++) {
	    				dbuffer[x + y*xDim + z*length] = MTT[z][y][x];
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
	        
	    	
	    	saveImageFile(CBVImage, outputFilePath, outputPrefix + "CBV", saveFileFormat);
	    	
	    	CBVImage.disposeLocal();
	    	CBVImage = null;
        } // if (calculateCBFCBVMTT)
    	
    	TmaxImage = new ModelImage(ModelStorageBase.SHORT, extents3D, "Tmax");
    	//buffer = new short[volume]; done for peaks_mapImage
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
    		fileInfo[i].setDataType(ModelStorageBase.SHORT);
    	}
    	
    	saveImageFile(TmaxImage, outputFilePath, outputPrefix + "Tmax", saveFileFormat);
    	
//    	TmaxImage.disposeLocal();
//    	TmaxImage = null;
    	
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
    	
    	saveImageFile(TTPImage, outputFilePath, outputPrefix + "TTP", saveFileFormat);
    	
    	TTPImage.disposeLocal();
    	TTPImage = null;
    	
    	if (calculateBounds) {
    		p0MaxDistImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "p0MaxDistance");
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			for (z = 0; z < zDim; z++) {
        				dbuffer[x + y*xDim + z*length] = p0MaxDistFromValue[z][y][x];
        			}
        		}
        	}
        	try {
        	    p0MaxDistImage.importData(0, dbuffer, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException on p0MaxDistImage");
        		setCompleted(false);
        		return;
        	}
        	fileInfo = p0MaxDistImage.getFileInfo();
        	for (i = 0; i < zDim; i++) {
        		fileInfo[i].setResolutions(resolutions3D);
        		fileInfo[i].setUnitsOfMeasure(units3D);
        		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
        	}
        	
        	saveImageFile(p0MaxDistImage, outputFilePath, outputPrefix + "p0MaxDist", saveFileFormat);

        	p0MaxDistImage.disposeLocal();
        	p0MaxDistImage = null;
        	
        	p1MaxDistImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "p1MaxDistance");
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			for (z = 0; z < zDim; z++) {
        				dbuffer[x + y*xDim + z*length] = p1MaxDistFromValue[z][y][x];
        			}
        		}
        	}
        	
        	try {
        	    p1MaxDistImage.importData(0, dbuffer, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException on p1MaxDistImage");
        		setCompleted(false);
        		return;
        	}
        	fileInfo = p1MaxDistImage.getFileInfo();
        	for (i = 0; i < zDim; i++) {
        		fileInfo[i].setResolutions(resolutions3D);
        		fileInfo[i].setUnitsOfMeasure(units3D);
        		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
        	}
        	
        	saveImageFile(p1MaxDistImage, outputFilePath, outputPrefix + "p1MaxDist", saveFileFormat);

        	p1MaxDistImage.disposeLocal();
        	p1MaxDistImage = null;
    	} // if (calculateBounds)
    	
    	/*chiSquaredImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "chiSquared");
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
        options.setFileName(outputPrefix + "chiSquared.img");
       
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
    	chiSquaredImage = null;*/
    	
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
        short brain_mask_norm[][][];
        short delay_map[][];
        double corr_map2[][] = null;
        double temp_mean[];
        
        public corr1Calc(int xDim, int yDim, int tDim, short brain_mask_norm[][][], short delay_map[][], 
        		double corr_map2[][], double temp_mean[]) {
        	this.xDim = xDim;
        	this.yDim = yDim;
        	this.tDim = tDim;
        	this.brain_mask_norm = brain_mask_norm;
        	this.delay_map = delay_map;
        	if (calculateCorrelation) {
        	    this.corr_map2 = corr_map2;
        	}
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
    				    delay_map[y][x] = (short)maxIndex;
    				    if (calculateCorrelation) {
	    				    cc = corrcoef(circshift(brain_mask_norm[y][x], -maxIndex + tDim), temp_mean);
	    				    corr_map2[y][x] = cc;
    				    }
    				} // if (sum != 0)
    			}
    		}	
        }
    }
    
    public class corr2Calc implements Runnable {
    	int xDim;
        int yDim;
        int tDim;
        short brain_mask_norm[][][];
        short delay_map[][];
        short peaks_map[][];
        double corrmap[][];
        double corr_map2[][];
        double temp_mean[];
        
        public corr2Calc(int xDim, int yDim, int tDim, short brain_mask_norm[][][], short delay_map[][], 
        		short peaks_map[][], double corrmap[][], double corr_map2[][], double temp_mean[]) {
        	this.xDim = xDim;
        	this.yDim = yDim;
        	this.tDim = tDim;
        	this.brain_mask_norm = brain_mask_norm;
        	this.delay_map = delay_map;
        	this.peaks_map = peaks_map;
        	if (calculateCorrelation) {
	        	this.corrmap = corrmap;
	        	this.corr_map2 = corr_map2;
        	}
        	this.temp_mean = temp_mean;
        }
        
        public void run() {
        	int i, x, y, t;
        	long sum;
        	double temp[];
        	double maxTemp;
        	short maxIndex;
        	double cc;
        	short maxPeak;
        	
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
    				    		maxIndex = (short)(i+1);
    				    	}
    				    }
    				    delay_map[y][x] = maxIndex;
    				    maxPeak = Short.MIN_VALUE;
    				    for (t = 0; t < tDim; t++) {
    				    	if (Math.abs(brain_mask_norm[y][x][t]) > maxPeak) {
    				    		maxPeak = (short)Math.abs(brain_mask_norm[y][x][t]);
    				    	}
    				    }
    				    peaks_map[y][x] = (short)maxPeak;
    				    if (calculateCorrelation) {
	    				    cc = corrcoef(brain_mask_norm[y][x], temp_mean);
	    				    corrmap[y][x] = cc;
	    				    cc = corrcoef(circshift(brain_mask_norm[y][x], -maxIndex + tDim), temp_mean);
	    				    corr_map2[y][x] = cc;
    				    }
    				} // if (sum != 0)
    			}
    		}
        	
        }
    }
    
    public class endCalc implements Runnable {
    	// The global D_inv is shared by all these Runnable routines.
    	// All other variables are unique.
    	int search;
        int xDim;
        int yDim;
        int tDim;
        float delT;
        double TE;
        double masking_threshold;
        short data[][][];
        double CBV[][];
        double CBF[][];
        double MTT[][];
        short Tmax[][];
        double p0MaxDistFromValue[][];
        double p1MaxDistFromValue[][];
        double t975;
        //double chiSquared[][];
    	
    	public endCalc(int search, int xDim, int yDim, int tDim, float delT, double TE, double masking_threshold,
    			short data[][][], double CBV[][], double CBF[][], double MTT[][], short Tmax[][],
    			double p0MaxDistFromValue[][], double p1MaxDistFromValue[][], double t975/*,
    			double chiSquared[][]*/) {
    		this.search = search;
        	this.xDim = xDim;
        	this.yDim = yDim;
        	this.tDim = tDim;
        	this.delT = delT;
        	this.TE = TE;
        	this.masking_threshold = masking_threshold;
        	this.data = data;
        	this.Tmax = Tmax;
        	if (calculateCBFCBVMTT) {
	        	this.CBV = CBV;
	        	this.CBF = CBF;
	        	this.MTT = MTT;
        	}
        	if (calculateBounds) {
        	    this.p0MaxDistFromValue = p0MaxDistFromValue;
        	    this.p1MaxDistFromValue = p1MaxDistFromValue;
        	    this.t975 = t975;
        	}
        	//this.chiSquared = chiSquared;
        }
    	
    	public void run() {
    		double x0[];
            double xdata[];
            int i;
            double C[];
            double b[];
            int dim = 2;
            double eps = 1.0e-8;
            double scale = 1.0;
            boolean display = false;
            xdata = new double[2*tDim];
            for (i = 0; i < 2*tDim; i++) {
            	xdata[i] = i * delT;
            }
            C = new double[2*tDim];
            b = new double[2*tDim];
            int y;
            int x;
            int t;
            double S[] = new double[tDim];
            int j;
            double sumb;
            double rcbf;
            expfun1D minsearch1D;
            expfun2D minsearch2D;
            expfunNM minsearchNM;
            expfunNM2 minsearchNM2;
        	int n = 2;
        	double tolx = 1.0E-8;
        	double tolf = 1.0E-8;
        	int max_iter = 5000;
        	int max_eval = 5000;
        	boolean verbose = false;
        	int exitStatus;
        	double p[] = new double[2];
        	double num;
        	double denom;
        	double num1;
        	double denom1;
        	int nPts;
        	double covarMat[][] = null;
        	double errorSumOfSquares;
        	double s2;
        	double arr2D[][] = new double[2][2];
        	double det;
        	double invDiag2D[] = new double[2];
        	double se0;
        	double se1;
        	double diff;
        	double expval;
        	if (calculateBounds) {
		    	covarMat = new double[2*tDim][2];
		    }
            // Iterate
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			if ((search == ELSUNC_2D_SEARCH) || (search == NMSIMPLEX_2D_SEARCH) || (search == NELDERMEAD_2D_SEARCH)) {
                    	x0 = new double[]{0.1,4};
                    }
                    else {
            		    x0 = new double[]{4};
                    }
        		    if (data[y][x][0] >= masking_threshold) {
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
        		    	    		Tmax[y][x] = (short)(i+1);
        		    	    	}
        		    	    }
        		    	    if (calculateCBFCBVMTT || calculateBounds) {
        		    	    // Shift b to have a peak at origin for fitting
	        		    	    b = circshift(b,-Tmax[y][x]);
	        		    	    if (search == ELSUNC_2D_SEARCH) {
	        		    	    	minsearch2D = new expfun2D(x0, b, xdata);
		        		    	    minsearch2D.driver();
		        		    	    exitStatus = minsearch2D.getExitStatus();
		        		    	    p = minsearch2D.getParameters();
		        		    	    if ((exitStatus >= 0) && (p[1] > 0) && (p[1] < 75)) {
		        		    	    	// Normal termination
		        		    	    	if (calculateCBFCBVMTT) {
			        		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
			        					    CBF[y][x] = p[0];
			        					    // relCBF is max value of residual function.  Should be similar to CBF,
			        					    // but may be different.
			        					    //relCBF[x][y][z] = rcbf;
			        					    MTT[y][x] = p[1];
			        					    CBV[y][x] = rcbf * p[1];
		        		    	    	}
		        					    //chiSquared[y][x] = minsearch.getChiSquared();
		        					    if (calculateBounds) {
				    					    nPts = xdata.length;
				    					    errorSumOfSquares = 0.0;
				    					    for (i = 0; i < nPts; i++) {
				    					    	expval =  Math.exp(-1.0/p[1]*xdata[i]);
				    						    covarMat[i][0] = Math.exp(-1.0/p[1]*xdata[i]);
				    						    covarMat[i][1] = xdata[i]/(p[1]*p[1])*p[0]*expval;
				    						    diff = (p[0]*expval) - b[i];
				    						    errorSumOfSquares += (diff * diff);
				    						}
				    					    s2 = errorSumOfSquares/(nPts - 2);
				    					    arr2D[0][0] = 0.0;
				    					    arr2D[0][1] = 0.0;
				    					    arr2D[1][0] = 0.0;
				    					    arr2D[1][1] = 0.0;
				    					    for (i = 0; i < nPts; i++) {
				    					    	arr2D[0][0] += covarMat[i][0]*covarMat[i][0];
				    					    	arr2D[0][1] += covarMat[i][0]*covarMat[i][1];
				    					    	arr2D[1][1] += covarMat[i][1]*covarMat[i][1];
				    					    }
				    					    arr2D[1][0] = arr2D[0][1];
				    					    det = arr2D[0][0]*arr2D[1][1] - arr2D[0][1]*arr2D[1][0];
				    					    if (det != 0.0) {
					    					    invDiag2D[0] = arr2D[1][1]/det;
					    					    invDiag2D[1] = arr2D[0][0]/det;
					    					    se0 = Math.sqrt(invDiag2D[0]*s2);
					    					    se1 = Math.sqrt(invDiag2D[1]*s2);
					    					    if ((!Double.isInfinite(se0))  && (!Double.isNaN(se0))) {
					    					        p0MaxDistFromValue[y][x] = t975 * se0;
					    					    }
					    					    if ((!Double.isInfinite(se1))  && (!Double.isNaN(se1))) {   
					    					        p1MaxDistFromValue[y][x] = t975 * se1;
					    					    }
				    					    } // if (det != 0.0)
			    					    } // if (calculateBounds)
		        		    	    }	
	        		    	    } // if (search == ELSUNC_2D_SEARCH)
	        		    	    else if (search == NMSIMPLEX_2D_SEARCH) {
	        		    	    	minsearchNM = new expfunNM(x0, dim, eps, scale, display, b, xdata);
	            		    	    minsearchNM.driver();
	            		    	    if ((x0[1] > 0) && (x0[1] < 75)) {
	            		    	    	// Normal termination
	            		    	    	if (calculateCBFCBVMTT) {
		            		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
		            					    CBF[y][x] = x0[0];
		            					    // relCBF is max value of residual function.  Should be similar to CBF,
		            					    // but may be different.
		            					    //relCBF[x][y][z] = rcbf;
		            					    MTT[y][x] = x0[1];
		            					    CBV[y][x] = rcbf * x0[1];
	            		    	    	}
	            					    if (calculateBounds) {
				    					    nPts = xdata.length;
				    					    errorSumOfSquares = 0.0;
				    					    for (i = 0; i < nPts; i++) {
				    					    	expval = Math.exp(-1.0/x0[1]*xdata[i]);
				    						    covarMat[i][0] = expval;
				    						    covarMat[i][1] = xdata[i]/(x0[1]*x0[1])*x0[0]*expval;
				    						    diff = (x0[0]*expval) - b[i];
				    						    errorSumOfSquares += (diff * diff);
				    						}
				    					    s2 = errorSumOfSquares/(nPts - 2);
				    					    arr2D[0][0] = 0.0;
				    					    arr2D[0][1] = 0.0;
				    					    arr2D[1][0] = 0.0;
				    					    arr2D[1][1] = 0.0;
				    					    for (i = 0; i < nPts; i++) {
				    					    	arr2D[0][0] += covarMat[i][0]*covarMat[i][0];
				    					    	arr2D[0][1] += covarMat[i][0]*covarMat[i][1];
				    					    	arr2D[1][1] += covarMat[i][1]*covarMat[i][1];
				    					    }
				    					    arr2D[1][0] = arr2D[0][1];
				    					    det = arr2D[0][0]*arr2D[1][1] - arr2D[0][1]*arr2D[1][0];
				    					    if (det != 0.0) {
					    					    invDiag2D[0] = arr2D[1][1]/det;
					    					    invDiag2D[1] = arr2D[0][0]/det;
					    					    se0 = Math.sqrt(invDiag2D[0]*s2);
					    					    se1 = Math.sqrt(invDiag2D[1]*s2);
					    					    if ((!Double.isInfinite(se0))  && (!Double.isNaN(se0))) {
					    					        p0MaxDistFromValue[y][x] = t975 * se0;
					    					    }
					    					    if ((!Double.isInfinite(se1))  && (!Double.isNaN(se1))) {
					    					        p1MaxDistFromValue[y][x] = t975 * se1;
					    					    }
				    					    } // if (det != 0.0)
			    					    } // if (calculateBounds)
	            		    	    }
	        		    	    } //  else if (search == NMSIMPLEX_2D_SEARCH)
	        		    	    else if (search == NELDERMEAD_2D_SEARCH) {
		    		    	    	minsearchNM2 = new expfunNM2(x0, n, tolx, tolf, max_iter, max_eval, verbose, b, xdata);
		    		    	    	minsearchNM2.driver();
		    		    	    	p = minsearchNM2.getSolX();
		    		    	    	if ((p[1] > 0) && (p[1] < 75)) {
			    		    	    	// Normal termination
		    		    	    		if (calculateCBFCBVMTT) {
				    		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
				    					    CBF[y][x] = p[0];
				    					    // relCBF is max value of residual function.  Should be similar to CBF,
				    					    // but may be different.
				    					    //relCBF[x][y][z] = rcbf;
				    					    MTT[y][x] = p[1];
				    					    CBV[y][x] = rcbf * p[1];
		    		    	    		}
			    					    //chiSquared[y][x] = minsearch.getChiSquared();
			    					    if (calculateBounds) {
				    					    nPts = xdata.length;
				    					    errorSumOfSquares = 0.0;
				    					    for (i = 0; i < nPts; i++) {
				    					    	expval = Math.exp(-1.0/p[1]*xdata[i]);
				    						    covarMat[i][0] = expval;
				    						    covarMat[i][1] = xdata[i]/(p[1]*p[1])*p[0]*expval;
				    						    diff = (p[0]*expval) - b[i];
				    						    errorSumOfSquares += (diff * diff);
				    						}
				    					    s2 = errorSumOfSquares/(nPts - 2);
				    					    arr2D[0][0] = 0.0;
				    					    arr2D[0][1] = 0.0;
				    					    arr2D[1][0] = 0.0;
				    					    arr2D[1][1] = 0.0;
				    					    for (i = 0; i < nPts; i++) {
				    					    	arr2D[0][0] += covarMat[i][0]*covarMat[i][0];
				    					    	arr2D[0][1] += covarMat[i][0]*covarMat[i][1];
				    					    	arr2D[1][1] += covarMat[i][1]*covarMat[i][1];
				    					    }
				    					    arr2D[1][0] = arr2D[0][1];
				    					    det = arr2D[0][0]*arr2D[1][1] - arr2D[0][1]*arr2D[1][0];
				    					    if (det != 0.0) {
					    					    invDiag2D[0] = arr2D[1][1]/det;
					    					    invDiag2D[1] = arr2D[0][0]/det;
					    					    se0 = Math.sqrt(invDiag2D[0]*s2);
					    					    se1 = Math.sqrt(invDiag2D[1]*s2);
					    					    if ((!Double.isInfinite(se0))  && (!Double.isNaN(se0))) {
					    					        p0MaxDistFromValue[y][x] = t975 * se0;
					    					    }
					    					    if ((!Double.isInfinite(se1))  && (!Double.isNaN(se1))) {
					    					        p1MaxDistFromValue[y][x] = t975 * se1;
					    					    }
				    					    } // if (det != 0.0)
			    					    } // if (calculateBounds)
			    		    	    }	
		    		    	    } // else if (search == NELDERMEAD_2D_SEARCH)
	        		    	    else if (calculateCBFCBVMTT) { // 1D search
		        		    	    minsearch1D = new expfun1D(x0, b, xdata);
		        		    	    minsearch1D.driver();
		        		    	    exitStatus = minsearch1D.getExitStatus();
		        		    	    p[1] = minsearch1D.getParameters()[0];
		        		    	    if ((exitStatus >= 0) && (p[1] > 0) && (p[1] < 75)) {
		        		    	    	// Normal termination
		        		    	    	num1 = 0.0;
		        		    	    	denom1 = 0.0;
		        		    	    	for (i = 0; i < 2*tDim; i++) {
		                		    	    num = Math.exp(-xdata[i]/p[1]);
		            						denom = num * num;
		                                    num1 += b[i]*num;
		                                    denom1 += denom;
		        		    	    	}
		        		    	    	p[0] = num1/denom1;
		        		    	    	// p[0] corresponds to CBF, p[1] corresponds to MTT
		        					    CBF[y][x] = p[0];
		        					    // relCBF is max value of residual function.  Should be similar to CBF,
		        					    // but may be different.
		        					    //relCBF[x][y][z] = rcbf;
		        					    MTT[y][x] = p[1];
		        					    CBV[y][x] = rcbf * p[1];
		        					    //chiSquared[y][x] = minsearch.getChiSquared();
		        		    	    }
	        		    	    } // 1D search
        		    	    } // if (calculateCBFCBVMTT || calculateBounds)
        		    	} // if ((!Double.isNaN(sumb)) && (!Double.isInfinite(sumb)))
        		    } // if ((data[y][x][0] >= masking_threshold)
        		} // for (x = 0; x < xDim; x++)
        	} // for (y = 0; y < yDim; y++)
    	}
    }
    
    
    public void testexpfun() {
    	expfun1D minsearch1D;
    	expfun2D minsearch2D;
    	expfunNM minsearchNM;
    	expfunNM2 minsearchNM2;
    	int exitStatus;
    	double p[] = new double[2];
    	double num;
    	double denom;
    	double num1;
    	double denom1;
    	int i;
    	double x0[] = new double[]{4};
    	double b[] = new double[]{0.07, 0.06};
    	double xdata[] = new double[]{5.0,6.0};
    	minsearch1D = new expfun1D(x0, b, xdata);
 	    minsearch1D.driver();
 	    exitStatus = minsearch1D.getExitStatus();	
 	    System.out.println("1D exitStatus = " + exitStatus);
 	    p[1] = minsearch1D.getParameters()[0];
 	   num1 = 0.0;
   	   denom1 = 0.0;
   	   for (i = 0; i < 2; i++) {
   	       num = Math.exp(-xdata[i]/p[1]);
		   denom = num * num;
           num1 += b[i]*num;
           denom1 += denom;
	   	}
	   	p[0] = num1/denom1;
 	    System.out.println("1D p[0] = " + p[0] + " MATLAB answer = 0.151297958944948");
 	    System.out.println("1D p[1] = " + p[1] + " MATLAB answer = 6.48713976636383");
 	    
 	    b = new double[10];
 	    xdata = new double[10];
 	    for (i = 0; i < 10; i++) {
 	    	xdata[i] = i;
 	    	b[i] = 0.133*Math.exp(-i/5.67);
 	    }
 	   minsearch1D = new expfun1D(x0, b, xdata);
	    minsearch1D.driver();
	    exitStatus = minsearch1D.getExitStatus();	
	    System.out.println("1D exitStatus = " + exitStatus);
	    p[1] = minsearch1D.getParameters()[0];
	   num1 = 0.0;
  	   denom1 = 0.0;
  	   for (i = 0; i < 10; i++) {
  	       num = Math.exp(-xdata[i]/p[1]);
		   denom = num * num;
          num1 += b[i]*num;
          denom1 += denom;
	   	}
	   	p[0] = num1/denom1;
	    System.out.println("1D p[0] = " + p[0] + " answer = 0.133");
	    System.out.println("1D p[1] = " + p[1] + " answer = 5.67");
	    
	    x0 = new double[]{0.1,4};
	    b = new double[]{0.07, 0.06};
        xdata = new double[]{5.0,6.0};
    	minsearch2D = new expfun2D(x0, b, xdata);
 	    minsearch2D.driver();
 	    exitStatus = minsearch2D.getExitStatus();	
 	    System.out.println("2D exitStatus = " + exitStatus);
 	    p = minsearch2D.getParameters();
 	    System.out.println("2D p[0] = " + p[0] + " MATLAB answer = 0.151297958944948");
 	    System.out.println("2D p[1] = " + p[1] + " MATLAB answer = 6.48713976636383");
 	    
 	    b = new double[10];
 	    xdata = new double[10];
 	    for (i = 0; i < 10; i++) {
 	    	xdata[i] = i;
 	    	b[i] = 0.133*Math.exp(-i/5.67);
 	    }
 	    minsearch2D = new expfun2D(x0, b, xdata);
	    minsearch2D.driver();
	    exitStatus = minsearch2D.getExitStatus();	
	    p = minsearch2D.getParameters();
	    System.out.println("2D exitStatus = " + exitStatus);
	    System.out.println("2D p[0] = " + p[0] + " answer = 0.133");
	    System.out.println("2D p[1] = " + p[1] + " answer = 5.67");
	    
	    int dim = 2;
        double eps = 1.0e-8;
        double scale = 1.0;
        boolean display = false;
	    x0 = new double[]{0.1,4};
	    b = new double[]{0.07, 0.06};
        xdata = new double[]{5.0,6.0};
	    minsearchNM = new expfunNM(x0, dim, eps, scale, display, b, xdata);
	    minsearchNM.driver();
	    System.out.println("2D NMSimplex x0[0] = " + x0[0] + " MATLAB answer = 0.151297958944948");
 	    System.out.println("2D NMSimplex x0[1] = " + x0[1] + " MATLAB answer = 6.48713976636383");
	    
	    x0 = new double[]{0.1,4};
	    b = new double[10];
 	    xdata = new double[10];
 	    for (i = 0; i < 10; i++) {
 	    	xdata[i] = i;
 	    	b[i] = 0.133*Math.exp(-i/5.67);
 	    }
 	    minsearchNM = new expfunNM(x0, dim, eps, scale, display, b, xdata);
	    minsearchNM.driver();
	    System.out.println("2D NMSimplex x0[0] = " +x0[0] + " answer = 0.133");
	    System.out.println("2D NMSimplex x0[1] = " +x0[1] + " answer = 5.67");
	    
	    int n = 2;
	    double tolx = 1.0E-8;
	    double tolf = 1.0E-8;
	    int max_iter = 5000;
	    int max_eval = 5000;
	    boolean verbose = false;
	    x0 = new double[]{0.1,4};
	    b = new double[]{0.07, 0.06};
        xdata = new double[]{5.0,6.0};
        minsearchNM2 = new expfunNM2(x0, n, tolx, tolf, max_iter, max_eval, verbose, b, xdata);
        minsearchNM2.driver();
        p = minsearchNM2.getSolX();
        System.out.println("NelderMead p[0] = " + p[0] +  " MATLAB answer = 0.151297958944948");
	    System.out.println("NelderMead p[1] = " + p[1] + " MATLAB answer = 6.48713976636383");
	    
	    x0 = new double[]{0.1,4};
	    b = new double[10];
 	    xdata = new double[10];
 	    for (i = 0; i < 10; i++) {
 	    	xdata[i] = i;
 	    	b[i] = 0.133*Math.exp(-i/5.67);
 	    }
 	   minsearchNM2 = new expfunNM2(x0, n, tolx, tolf, max_iter, max_eval, verbose, b, xdata);
       minsearchNM2.driver();
       p = minsearchNM2.getSolX();
       System.out.println("NelderMead p[0] = " + p[0] + " answer = 0.133");
	   System.out.println("NelderMead p[1] = " + p[1] + " answer = 5.67");
	}
    
    class expfun1D extends NLConstrainedEngine {
    	double b[];
    	double xdata[];
        public expfun1D(double x0[], double b[], double xdata[]) {
        	// nPoints, params
        	super(1, 1);
        	this.b = b;
        	this.xdata = xdata;
        	
        	bounds = 0; // bounds = 0 means unconstrained
        	//bl[0] = 1.0E-10;
        	//bu[0] = 74.999999;

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
			double num1;
			double num2;
			double denom1;
			double denom2;
			double num;
			double denom;
			double e1;
			double e2;
			double sum1;
			double sum2;
			double sum3;
			double sum4;
			double sum5;
			double sum6;
			double sum7;
			double sum8;
			double ratio1;
			double ratio2;
			
			try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Monoexponential decay 
					num1 = 0.0;
					denom1 = 0.0;
					num2 = 0.0;
					denom2 = 0.0;
					for (i = 0; i < xdata.length; i++) {
					    //residuals[i] = (a[0]*Math.exp(-1/a[1]*xdata[i])) - b[i];
						num = Math.exp(-xdata[i]/a[0]);
						denom = num * num;
                        num1 += b[i]*num;
                        denom1 += denom;
                        num2 += b[i]*xdata[i]*num;
                        denom2 += xdata[i]*denom;
					}
					residuals[0] = num1/denom1 - num2/denom2;
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian analytically
				else if (ctrl == 2) {
					sum1 = 0.0;
					sum2 = 0.0;
					sum3 = 0.0;
					sum4 = 0.0;
					sum5 = 0.0;
					sum6 = 0.0;
					sum7 = 0.0;
					sum8 = 0.0;
					for (i = 0; i < xdata.length; i++) {
					    e1 = Math.exp(-xdata[i]/a[0]);
					    e2 = e1 * e1;
					    sum1 += e2;
					    sum2 += (xdata[i]/(a[0]*a[0]))*b[i]*e1;
					    sum3 += b[i]*e1;
					    sum4 += 2.0*(xdata[i]/(a[0]*a[0]))*e2;
					    sum5 += e2*xdata[i];
					    sum6 += ((xdata[i]*xdata[i])/(a[0]*a[0]))*b[i]*e1;
					    sum7 += b[i]*e1*xdata[i];
					    sum8 += 2.0*((xdata[i]*xdata[i])/(a[0]*a[0]))*e2;
					}
					ratio1 = (sum1 * sum2 - sum3 * sum4)/(sum1 * sum1);
					ratio2 = (sum5 * sum6 - sum7 * sum8)/(sum5 * sum5);
					covarMat[0][0] = ratio1 - ratio2;
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
		}
    }

    
    /*public void testexpfun() {
    	int i;
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
 	    
 	    b = new double[10];
 	    xdata = new double[10];
 	    for (i = 0; i < 10; i++) {
 	    	xdata[i] = i;
 	    	b[i] = 0.133*Math.exp(-i/5.67);
 	    }
 	   minsearch = new expfun(x0, b, xdata);
	    minsearch.driver();
	    exitStatus = minsearch.getExitStatus();	
	    System.out.println("exitStatus = " + exitStatus);
	    p = minsearch.getParameters();
	    System.out.println("p[0] = " + p[0] + " answer = 0.133");
	    System.out.println("p[1] = " + p[1] + " answer = 5.67");
    }*/
    
    class expfun2D extends NLConstrainedEngine {
    	double b[];
    	double xdata[];
        public expfun2D(double x0[], double b[], double xdata[]) {
        	// nPoints, params
        	super(b.length, x0.length);
        	this.b = b;
        	this.xdata = xdata;
        	
        	bounds = 0; // bounds = 0 means unconstrained
        	//bl[0] = -Double.MAX_VALUE;
        	//bu[0] = Double.MAX_VALUE;
        	//bl[1] = 1.0E-10;
        	//bu[1] = 74.999999;

        	

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
		/*public void driver() {
			super.driver();
		}*/

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
    
    class expfunNM extends NMSimplex {
    	double b[];
    	double xdata[];
    	//int dim = 2;
		//double eps = 1.0e-4;
		//double scale = 1.0;
		//boolean display = false;
        public expfunNM(double x0[], int dim, double eps, double scale, boolean display, double b[], double xdata[]) {
        	super(x0,dim,eps,scale,display);
        	this.b = b;
        	this.xdata = xdata;	
        }
        
        public double evalObjfun(double x[]){
        	int i;
        	int nPts = b.length;
        	double diff;
        	double sum = 0.0;
        	// Monoexponential decay
			for (i = 0; i < nPts; i++) {
			    diff = (x[0]*Math.exp(-1/x[1]*xdata[i])) - b[i];
			    sum += diff*diff;
			}
			return sum;
        }
        
        
   		public void getConstrainedValues(double x[], int n) {
   	        /*if (x[1] < 1.0E-10) {
   	        	x[1] = 1.0E-10;
   	        }
   	        else if (x[1] > 74.999999) {
   	        	x[1] = 74.999999;
   	        }*/
   		}
   		
   		/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
    }
    
    
    
    class expfunNM2 extends NelderMead  {
    	double b[];
    	double xdata[];
    	public expfunNM2(double x0[], int n, double tolx, double tolf, int max_iter, int max_eval, boolean verbose, double b[], double xdata[]) {
    		super(n, x0, tolx, tolf, max_iter, max_eval, verbose);
    		this.b = b;
    		this.xdata = xdata;
    	}
    	
    	/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}

		@Override
		public void cost_function(int n, NelderMead.point_t point) {
			int i;
        	int nPts = b.length;
        	double diff;
        	double sum = 0.0;
        	// Monoexponential decay
			for (i = 0; i < nPts; i++) {
			    diff = (point.x[0]*Math.exp(-1/point.x[1]*xdata[i])) - b[i];
			    sum += diff*diff;
			}
			point.fx = sum;
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
	
	private double[] xcorrbias(short x[], double y[]) {
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
    
    private int[] circshift(short x[], int n) {
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
    
    private double corrcoef(short x[], double y[]) {
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

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
        
//        if (delay_mapImage != null) {
//            delay_mapImage.disposeLocal();
//            delay_mapImage = null;
//        }
//        if (MTTImage != null) {
//            MTTImage.disposeLocal();
//            MTTImage = null;
//        }
        if (TmaxImage != null) {
            TmaxImage.disposeLocal();
            TmaxImage = null;
        }
    }

//    public ModelImage getMTTImage() {
//        return MTTImage;
//    }
    
    public ModelImage getTmaxImage() {
        return TmaxImage;
    }
    
    public File getAifFile() {
        return aifFile;
    }
    
    public File getSliceAifFile() {
        return sliceAifFile;
    }
    
//    public ModelImage getDelayMapImage() {
//        return delay_mapImage;
//    }
    
    public void setOutputFilePath(String path) {
        outputFilePath = path;
    }
    
    public void setOutputPrefix(String prefix) {
        outputPrefix = prefix;
    }
    
    public void setSaveAllOutputs(boolean saveAll) {
        doSaveAllOutputs = saveAll;
    }
    
    private File saveImageFile(final ModelImage img, final String dir, final String fileBasename, int fileType) {
        return saveImageFile(img, dir, fileBasename, fileType, false);
    }
    
    private File saveImageFile(final ModelImage img, final String dir, final String fileBasename, int fileType, boolean alwaysSave) {
        if (fileIO == null) {
            fileIO = new FileIO();
            fileIO.setQuiet(true);
        }
        
        // if no directory specified, skip writing out images
        // or if option is set and this is a file that is optionally written out
        if (dir == null || (!alwaysSave && !doSaveAllOutputs)) {
        	return null;
        }
        
        FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileDirectory(dir);

        if (img.getNDims() == 3) {
            opts.setBeginSlice(0);
            opts.setEndSlice(img.getExtents()[2] - 1);
        } else if (img.getNDims() == 4) {
            opts.setBeginSlice(0);
            opts.setEndSlice(img.getExtents()[2] - 1);
            opts.setBeginTime(0);
            opts.setEndTime(img.getExtents()[3] - 1);
        }

        opts.setFileType(fileType);
        final String ext = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension();
        opts.setFileName(fileBasename + ext);

        opts.setOptionsSet(true);
        opts.setMultiFile(false);
        
        fileIO.writeImage(img, opts, false, false);
        
        return new File(dir + File.separator + fileBasename + ext);
    }
    
    }
