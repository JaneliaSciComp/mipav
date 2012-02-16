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

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * This class recalculates mo and dceFullTre to provide better estimates using the inverse Ernst equation
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmTre4D extends AlgorithmBase {

    private ModelImage dceHigh;

    private ModelImage r1;

    private ModelImage newM0;

    private ModelImage b1;

    private int fa;

    private double trTime;

    private File centerTimesFile;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmTre4D(ModelImage resultImage, ModelImage srcImg, 
            ModelImage dceHigh, ModelImage r1, ModelImage newM0, ModelImage b1, 
            int fa, double trTime, String centerTimesFile) {
        super(resultImage, srcImg);
        
        this.dceHigh = dceHigh;
        this.r1 = r1;
        this.newM0 = newM0;
        this.b1 = b1;
        this.fa = fa;
        this.trTime = trTime;
        this.centerTimesFile = new File(centerTimesFile);
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
    
    /**
     * Starts the algorithm.  At the conclusion of this method, AlgorithmBase reports to any
     * algorithm listeners that this algorithm has completed.  This method is not usually called explicitly by
     * a controlling dialog.  Instead, see AlgorithmBase.run() or start().
     */
    public void runAlgorithm() {
    	calc3D();
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()
    
	//  ~ Methods --------------------------------------------------------------------------------------------------------
        
    private void calc3D() {
    	System.out.println("4d processing here");
    	fireProgressStateChanged("4D: "+srcImage.getImageName());
    	
    	//ModelImage dceHigh = null, r1 = null, newM0 = null, b1 = null;
    	try {
	    	dceHigh = ViewUserInterface.getReference().getRegisteredImageByName("DCEhighSlab");
	    	r1 = ViewUserInterface.getReference().getRegisteredImageByName("r1_results");
	    	newM0 = ViewUserInterface.getReference().getRegisteredImageByName("m0_results");
	    	b1 = ViewUserInterface.getReference().getRegisteredImageByName("b1_results");
    	} catch(Exception e) {
    		MipavUtil.displayError("Please load the following image: "+e.getMessage());
    	}
    	
    	long time = System.currentTimeMillis();
    	
    	//get new estimate of r1
    	int[] extents = new int[dceHigh.getNDims()];
    	for(int i=0; i<extents.length; i++) {
    		extents[i] = dceHigh.getExtents()[i];
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
    	
    	int timeSeries = dceHigh.getExtents()[3];
    	double[][][] r1Copy = new double[extents[0]][extents[1]][extents[2]];
    	double[][][] m0Copy = new double[extents[0]][extents[1]][extents[2]];
    	double[][][] b1Copy = new double[extents[0]][extents[1]][extents[2]];
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
    	
    	ArrayList<Double> centerTimesArr = new ArrayList<Double>();
    	String tString = new String();
    	try {
    		BufferedReader r = new BufferedReader(new FileReader(centerTimesFile));
	    
    		while((tString = r.readLine()) != null) {
	    		centerTimesArr.add(Double.valueOf(tString));
	    	}
	    	
	    	r.close();
	    } catch(NumberFormatException e) {
	    	System.out.println("Check t for number: "+tString);
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
	    double faRad = Math.toRadians(fa);
	    double num = 0.0, den = 0.0;
	    for(int i=0; i<extents[0]; i++) {
    		for(int j=0; j<extents[1]; j++) {
    			for(int k=0; k<extents[2]; k++) {
    				num = 1 - Math.cos(b1Copy[i][j][k]*faRad)*Math.exp(-trTime*r1Copy[i][j][k]);
		    		den = Math.sin(b1Copy[i][j][k]*faRad)*(1-Math.exp(-trTime*r1Copy[i][j][k]));

		    		if(den != 0) {
		    			m0Copy[i][j][k] = preGad.getDouble(i,j,k)*(num/den);
		    		} else {
		    			m0Copy[i][j][k] = 0.0;
		    		}
		    	}
	    	}
    	}
	    
	    fireProgressStateChanged(75);
	    for(int k=0; k<extents[2]; k++) {
	    	for(int i=0; i<extents[0]; i++) {
	    		for(int j=0; j<extents[1]; j++) {
    				newM0.set(i, j, k, m0Copy[i][j][k]);
    			}
    		}
	    }
	    
	    newM0.setImageName("New M0");
	    ViewJFrameImage display = new ViewJFrameImage(newM0);
	    display.setVisible(true);
	    
	    System.gc();
	    System.out.println("Working on dceHigh");
	    
	    num = 0.0;
	    den = 0.0;
	    
	    for(int t=0; t<extents[3]; t++) {
		    System.out.println("Time slice "+t);
	    	for(int i=0; i<extents[0]; i++) {
	    		for(int j=0; j<extents[1]; j++) {
	    			for(int k=0; k<extents[2]; k++) {
	    				
	    				num = dceHigh.getDouble(i, j, k, t) - newM0.getDouble(i, j, k)*Math.sin(b1.getDouble(i, j, k)*faRad);
	    				den = dceHigh.getDouble(i, j, k, t)*Math.cos(b1.getDouble(i, j, k)*faRad) - newM0.getDouble(i, j, k)*Math.sin(b1.getDouble(i, j, k)*faRad);
	    			
	    				if(den != 0) {
			    			dceFullCopy[i][j][k][t] = (-1.0/trTime)*Math.log(num/den);
			    		} else {
			    			dceFullCopy[i][j][k][t] = 0.0;
			    		}
	    			}
	    		}		
		    }
	    }
	    
	    ModelImage r1New = (ModelImage)dceHigh.clone();
	    for(int t=0; t<extents[3]; t++) {
	    	System.out.println("Time slice2 "+t);
		    for(int i=0; i<extents[0]; i++) {
	    		for(int j=0; j<extents[1]; j++) {
	    			for(int k=0; k<extents[2]; k++) {
	    				r1New.set(i, j, k, t, dceFullCopy[i][j][k][t]*1000.0);
	    			}
	    		}
		    }
	    }
	    
	    r1New.setImageName("r1New");
	    ViewJFrameImage display2 = new ViewJFrameImage(r1New);
	    display2.setVisible(true);
    	
    	
        
    	for(int i=1; i<100; i++) {
    		fireProgressStateChanged(i);
        }
    }
        
}
