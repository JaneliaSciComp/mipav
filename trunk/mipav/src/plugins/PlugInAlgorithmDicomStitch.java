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

import WildMagic.LibFoundation.Mathematics.Matrix2f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.Preferences;

/**
 * Stitches two dicom images together when necessary tags are populated.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmDicomStitch extends AlgorithmBase {

    /** Whether to perform a gaussian blur */
    private ModelImage stitchImage;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Original image model.
     * @param  stitchImage  Image to stitch to original.
     */
    public PlugInAlgorithmDicomStitch(ModelImage resultImage, ModelImage origImage, ModelImage stitchImage) {
        super(resultImage, origImage);
        this.stitchImage = stitchImage;
    }
        
    //  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        stitchImage = null;
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
    	fireProgressStateChanged("Message 3D: "+srcImage.getImageName());
    	int xDim = stitchImage.getExtents()[0];
    	int yDim = stitchImage.getExtents()[1];
    	int zDim = srcImage.getNDims() > 2 ? srcImage.getExtents()[2] : 1;
    	int tDim = srcImage.getNDims() > 3 ? srcImage.getExtents()[3] : 1;
    	for(int z=0; z<zDim; z++) {
    	    FileInfoDicom infoDicomStitch = (FileInfoDicom) stitchImage.getFileInfo()[z];
    	    FileInfoDicom infoDicomOrig = (FileInfoDicom) srcImage.getFileInfo()[z];
    	    
    	    //get necessary dicom elements for matrix multiplication
    	    Matrix4f toPos = createSpacingMatrix(infoDicomStitch);
    	    
    	    Matrix4f toImage = createSpacingMatrix(infoDicomOrig);
    	    toImage.Inverse();
    	    
    	    toImage.Mult(toPos);
    	    
    	    for(int i=0; i<xDim; i++) {
    	        for(int j=0; j<yDim; j++) {
    	            Vector4f posMove = new Vector4f();
    	            posMove.Set(i, j, 0, 1);
    	            
    	            Vector4f posResult = new Vector4f();
    	            toImage.MultRight(posMove, posResult);
    	            System.out.println(posMove+" changed to "+posResult.toString());
    	        }
    	    }   	    
    	}
    	
    	Matrix2f mat = new Matrix2f();
    	mat.Inverse();
    	
    
    	for(int i=1; i<100; i++) {
    		fireProgressStateChanged(i);
        }
    }

    private Matrix4f createSpacingMatrix(FileInfoDicom infoDicom) {
        Double[] xyCos = (Double[]) infoDicom.getTagTable().get("0020,0037").getValue(false);
        Double[] spacing = (Double[]) infoDicom.getTagTable().get("0028,0030").getValue(false);
        Double[] position = (Double[]) infoDicom.getTagTable().get("0020,0032").getValue(false);
        
        Matrix4f mat = new Matrix4f();
        mat.Set(0, 0, (float)(xyCos[0].doubleValue()*spacing[0].doubleValue()));
        mat.Set(0, 1, (float)(xyCos[1].doubleValue()*spacing[0].doubleValue()));
        mat.Set(0, 2, (float)(xyCos[2].doubleValue()*spacing[0].doubleValue()));
        mat.Set(0, 3, 0.0f);
        
        mat.Set(1, 0, (float)(xyCos[3].doubleValue()*spacing[1].doubleValue()));
        mat.Set(1, 1, (float)(xyCos[4].doubleValue()*spacing[1].doubleValue()));
        mat.Set(1, 2, (float)(xyCos[5].doubleValue()*spacing[1].doubleValue()));
        mat.Set(1, 3, 0.0f);
        
        mat.Set(2, 0, 0.0f);
        mat.Set(2, 1, 0.0f);
        mat.Set(2, 2, 0.0f);
        mat.Set(2, 3, 0.0f);
       
        mat.Set(3, 0, position[0].floatValue());
        mat.Set(3, 1, position[1].floatValue());
        mat.Set(3, 2, position[2].floatValue());
        mat.Set(3, 3, 1.0f);
        
        System.out.println("Created dicom position matrix:\n"+mat.toString());
        
        return mat;
    }
        
	
}
