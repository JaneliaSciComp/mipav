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
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.structures.ModelImage;

/**
 * This class implements a basic algorithm that performs operations on 2D and 3D images. 
 * By extending AlgorithmBase, it has no more functionality than any other algorithm in MIPAV.
 * No functionality specifically makes it a plug-in.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmNewGeneric542 extends AlgorithmBase {

	    /** Whether to perform a gaussian blur */
		private boolean doGaussian;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmNewGeneric542(ModelImage resultImage, ModelImage srcImg) {
        super(resultImage, srcImg);
        }
        
//  ~ Methods --------------------------------------------------------------------------------------------------------

		public void doGaussian(boolean doGaussian) {
			this.doGaussian = doGaussian;
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
         
	    	if(doGaussian) {
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
	    	fireProgressStateChanged("Message 3D: "+srcImage.getImageName());
        
	    	if(doGaussian) {
	    		final float[] sigmas = {1.0f, 1.0f, 1.0f};

		    	AlgorithmGaussianBlur gaussianBlurAlgo = new AlgorithmGaussianBlur(destImage, srcImage, sigmas, true, false);
		    	gaussianBlurAlgo.setRunningInSeparateThread(false);
		    	linkProgressToAlgorithm(gaussianBlurAlgo);
		    	gaussianBlurAlgo.runAlgorithm();
        }
        
	    	for(int i=1; i<100; i++) {
	    		fireProgressStateChanged(i);
            }
                    }
            
		
        }
