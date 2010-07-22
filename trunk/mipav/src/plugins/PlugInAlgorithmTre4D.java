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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Random;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * This class recalculates mo and dceFullTre to provide better estimates using the inverse Ernst equation
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmTre4D extends AlgorithmBase {
    
	  	/** X dimension of the image */
    private int xDim;

	    /** Y dimension of the image */
    private int yDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

	    /** Whether to perform a gaussian blur */
		private boolean doErnst;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmTre4D(ModelImage resultImage, ModelImage srcImg) {
        super(resultImage, srcImg);
            init();
        }
        
//  ~ Methods --------------------------------------------------------------------------------------------------------

		public void doErnst(boolean doErnst) {
			this.doErnst = doErnst;
		}

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    /**
     * Starts the algorithm.  At the conclusion of this method, AlgorithmBase reports to any
     * algorithm listeners that this algorithm has completed.  This method is not usually called explicitly by
     * a controlling dialog.  Instead, see AlgorithmBase.run() or start().
     */
	    public void runAlgorithm() {
	    	if(srcImage.getNDims() < 3) {
	    		calc2D();
                } else {
	    		calc3D();
                }
            
	    	setCompleted(true); //indicating to listeners that the algorithm completed successfully
    
	    } // end runAlgorithm()
    
	//  ~ Methods --------------------------------------------------------------------------------------------------------
    
	    private void calc2D() {
	    	fireProgressStateChanged("Message 2D: "+srcImage.getImageName());
         
	    	if(doErnst) {
	    		final float[] sigmas = {1.0f, 1.0f};
        
		    	AlgorithmGaussianBlur gaussianBlurAlgo = new AlgorithmGaussianBlur(destImage, srcImage, sigmas, true, false);
		    	gaussianBlurAlgo.setRunningInSeparateThread(false);
		    	linkProgressToAlgorithm(gaussianBlurAlgo);
		    	gaussianBlurAlgo.runAlgorithm();
        }
        
	    	for(int i=1; i<100; i++) {
	    		fireProgressStateChanged(i);
	    	}
        }
        
	    private void calc3D() {
	    	if(doErnst) {
		    	System.out.println("4d processing here");
		    	fireProgressStateChanged("Message 3D: "+srcImage.getImageName());
	        
		    	ModelImage dceHigh = ViewUserInterface.getReference().getRegisteredImageByName("DCEhighSlab");
		    	ModelImage r1 = ViewUserInterface.getReference().getRegisteredImageByName("r1_results");
		    	ModelImage newM0 = ViewUserInterface.getReference().getRegisteredImageByName("mo_results");
		    	ModelImage b1 = ViewUserInterface.getReference().getRegisteredImageByName("b1_results");
		    	ModelImage reichM0 = ViewUserInterface.getReference().getRegisteredImageByName("M0");
		    	
		    	System.out.println(dceHigh != null ? "DCEhighSlab Found" : "Null");
		    	System.out.println(r1 != null ? "r1 Found" : "Null");
		    	System.out.println(newM0 != null ? "m0 Found" : "Null");
		    	System.out.println(b1 != null ? "b1 Found" : "Null");
		    	System.out.println(reichM0 != null ? "m0Mod Found" : "Null");
		    	
		    	long time = System.currentTimeMillis();
		    	
		    	//get new estimate of r1
		    	int[] extents = new int[r1.getNDims()];
		    	for(int i=0; i<extents.length; i++) {
		    		extents[i] = r1.getExtents()[i];
		    	}
		    	
		    	ModelImage preGad = (ModelImage)r1.clone();
		    	//average pre-gad volumes
		    	double avg = 0.0;
		    	for(int i=0; i<extents[0]; i++) {
		    		for(int j=0; j<extents[1]; j++) {
		    			for(int k=0; k<extents[2]; k++) {
				    		avg = (dceHigh.getDouble(i, j, k, 0) + dceHigh.getDouble(i, j, k, 1) + dceHigh.getDouble(i, j, k, 2)) / 3.0;
		    				
		    				preGad.set(i, j, k, avg);
				    	}
			    	}
		    	}
		    	System.out.println("Avg computed");
		    	
		    	double fa = 15;
		    	int timeSeries = dceHigh.getExtents()[3];
		    	double[][][] r1Copy = new double[extents[0]][extents[1]][extents[2]];
		    	double[][][] m0Copy = new double[extents[0]][extents[1]][extents[2]];
		    	double[][][] m0Corrected =  new double[extents[0]][extents[1]][extents[2]];
		    	double[][][] b1Copy = new double[extents[0]][extents[1]][extents[2]];
		    	double[][][] b1Corrected = new double[extents[0]][extents[1]][extents[2]];
		    	double[][][][] dceFullCopy = new double[extents[0]][extents[1]][extents[2]][timeSeries];
		    	
		    	for(int i=0; i<extents[0]; i++) {
		    		for(int j=0; j<extents[1]; j++) {
		    			for(int k=0; k<extents[2]; k++) {
				    		r1Copy[i][j][k] = r1.getDouble(i, j, k);
				    		m0Copy[i][j][k] = newM0.getDouble(i, j, k);
				    		b1Copy[i][j][k] = b1.getDouble(i, j, k);
				    	}
			    	}
		    	}
		    	for(int t=0; t<timeSeries; t++) {
			    	for(int i=0; i<extents[0]; i++) {
			    		for(int j=0; j<extents[1]; j++) {
			    			for(int k=0; k<extents[2]; k++) {
					    		dceFullCopy[i][j][k][t] = dceHigh.getDouble(i, j, k, t);
					    	}
				    	}
			    	}
		    	}
		    	
		    	fireProgressStateChanged(25);
		    	
		    	System.err.println("Fully copied in "+(System.currentTimeMillis() - time));
		    	double trTime = 5.6;
		    	//get TR values
		    	File f = new File("E:\\my\\projects\\DESPOT\\tre4d\\DCECenterTimes.txt");
		    	
		    	ArrayList<Double> centerTimesArr = new ArrayList<Double>();
		    	String t = new String();
		    	try {
		    		BufferedReader r = new BufferedReader(new FileReader(f));
			    
		    		while((t = r.readLine()) != null) {
			    		centerTimesArr.add(Double.valueOf(t));
			    	}
			    	
			    	r.close();
			    } catch(NumberFormatException e) {
			    	System.out.println("Check t for number: "+t);
			    	e.printStackTrace();
			    } catch(Exception e1) {
			    	e1.printStackTrace();
			    }
			    
			    double[] dceCenter = new double[centerTimesArr.size()];
			    Double[] dceCenterInt = centerTimesArr.toArray(new Double[centerTimesArr.size()]);
			    for(int i=0; i<dceCenterInt.length; i++) {
			    	dceCenter[i] = dceCenterInt[i].doubleValue();
			    }
			    fireProgressStateChanged(50);
			    
			    for(int i=0; i<dceCenter.length; i++) {
			    	System.out.println("Center time "+i+": "+dceCenter[i]);
			    }
			    
			    double orig = 0.0, mod = 0.0;
			    double num = 0.0, den = 0.0;
			    for(int i=0; i<extents[0]; i++) {
		    		for(int j=0; j<extents[1]; j++) {
		    			for(int k=0; k<extents[2]; k++) {
		    				orig = newM0.getDouble(i, j, k);
		    				mod = reichM0.getDouble(i, j, k);
		    				if(orig != 0.0) {
		    					int z = 0; //here
		    				}
		    				num = 1 - Math.cos(b1Copy[i][j][k]*Math.toRadians(fa))*Math.exp(-trTime*r1Copy[i][j][k]);
				    		den = Math.sin(b1Copy[i][j][k]*Math.toRadians(fa))*(1-Math.exp(-trTime*r1Copy[i][j][k]));

				    		if(den != 0) {
				    			m0Copy[i][j][k] = preGad.getDouble(i,j,k)*(num/den);
				    		} else {
				    			m0Copy[i][j][k] = 0.0;
				    		}
				    		
				    		
				    		int z = 0;
				    	}
			    	}
		    	}
			    
			    fireProgressStateChanged(75);
			    Random r = new Random();
			    int rR = 0;
			    boolean print = false;
			    ModelImage m0FullCopy = (ModelImage)newM0.clone();
			    for(int k=0; k<extents[2]; k++) {
			    	for(int i=0; i<extents[0]; i++) {
			    		for(int j=0; j<extents[1]; j++) {
			    		
		    				if(newM0.get(i, j, k).intValue() != 0 && (rR = r.nextInt(20)) == 10) {
		    					print = true;
		    					System.out.print("Value at ("+i+", "+j+", "+k+") is :"+newM0.getDouble(i, j, k));
		    				}
		    				newM0.set(i, j, k, m0Copy[i][j][k]);
		    				if(print) {
		    					System.out.println(" and is now "+newM0.getDouble(i, j, k)+" but should be "+reichM0.getDouble(i,j,k));
		    					print = false;
		    				}
		    			}
		    		}
			    }
			    ViewJFrameImage display = new ViewJFrameImage(m0FullCopy);
			    display.setVisible(true);
	    	}
	    	
	    	
	        
	    	for(int i=1; i<100; i++) {
	    		fireProgressStateChanged(i);
            }
        }
            
		private void init() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        }
        }
