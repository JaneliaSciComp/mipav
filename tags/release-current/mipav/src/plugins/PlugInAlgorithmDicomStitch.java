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

import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.Matrix2f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * Stitches two dicom images together when necessary tags are populated.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmDicomStitch extends AlgorithmBase {

    /** Whether to perform a gaussian blur */
    private ModelImage stitchImage;
    
    /** Whether to overlay result images in single frame. */
    private boolean doImageB;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Original image model.
     * @param  stitchImage  Image to stitch to original.
     * @param doImageB 
     */
    public PlugInAlgorithmDicomStitch(ModelImage resultImage, ModelImage origImage, ModelImage stitchImage, boolean doImageB) {
        super(resultImage, origImage);
        this.stitchImage = stitchImage;
        this.doImageB = doImageB;
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
    	double xMin = 0, xMax = 0, yMin = 0, yMax = 0, zMin = 0, zMax = 0;
    	//int xOffset = 0, yOffset = 0, zOffset = 0;
    	
    	long time = System.currentTimeMillis();
    	for(int z=0; z<zDim; z++) {
    	    FileInfoDicom infoDicomStitch = (FileInfoDicom) stitchImage.getFileInfo()[z];
    	    FileInfoDicom infoDicomOrig = (FileInfoDicom) srcImage.getFileInfo()[z];
    	    
    	    //get necessary dicom elements for matrix multiplication
    	    Matrix3f toPosStitch = createSpacingMatrix(infoDicomStitch);
    	    Matrix3f toImageOrig = createSpacingMatrix(infoDicomOrig);
    	    
    	    toImageOrig.Inverse();
    	    
    	    toImageOrig.Mult(toPosStitch);
    	    
    	    for(int i=0; i<xDim; i++) {
    	        for(int j=0; j<yDim; j++) {
    	            //if(stitchImage.getShort(i, j, z) != 0) {
    	                Vector3f posMove = new Vector3f();
                        posMove.Set(i, j, 1);
                        
                        Vector3f posResult = new Vector3f();
                        toImageOrig.MultRight(posMove, posResult);
                        
                        if(posResult.X < xMin) {
                            xMin = posResult.X;
                        } else if(posResult.X > xMax) {
                            xMax = posResult.X;
                        }
                        
                        if(posResult.Y < yMin) {
                            yMin = posResult.Y;
                        } else if(posResult.Y > yMax) {
                            yMax = posResult.Y;
                        }
                        
                        if(posResult.Z < zMin) {
                            zMin = posResult.Z;
                        } else if(posResult.Z > zMax) {
                            zMax = posResult.Z;
                        }
    	            //}
    	        }
    	    }   	    
    	    System.out.println("Slice "+z+" xRange: "+xMin+", "+xMax+" yRange: "+yMin+", "+yMax+" zRange: "+zMin+", "+zMax);
    	}
        
    	//xOffset = 0 - xMin;
    	//yOffset = 0 - yMin;
    	//zOffset = 0 - zMin;
    	
    	//System.out.println("Offsets: "+xOffset+", "+yOffset+", "+zOffset);
    	
    	ModelImage finalImage = new ModelImage(stitchImage.getDataType(), new int[]{(int) (xMax-xMin+1), (int) (yMax-yMin+1), (int) (zMax-zMin)+zDim},"Transformed");
    	for(int z=0; z<zDim; z++) {
    	    time = System.currentTimeMillis();
            FileInfoDicom infoDicomStitch = (FileInfoDicom) stitchImage.getFileInfo()[z];
            FileInfoDicom infoDicomOrig = (FileInfoDicom) srcImage.getFileInfo()[z];
            
            //get necessary dicom elements for matrix multiplication
            //System.out.println("stitchImage");
            Matrix3f toPos = createSpacingMatrix(infoDicomStitch);
            
            //System.out.println("origImage");
            Matrix3f toImage = createSpacingMatrix(infoDicomOrig);
            toImage.Inverse();
            
            toImage.Mult(toPos);
            
            //doTransformationWithAlgo(toImage, finalImage);

            //System.out.println("Offsets: "+xOffset+", "+yOffset+", "+zOffset);
            doTransformManual(toImage, finalImage, xDim, yDim, z);
        }
    	ModelImage srcImageRealloc = null;
    	
    	if(doImageB) {
    	    srcImageRealloc = new ModelImage(srcImage.getDataType(), finalImage.getExtents(), "srcImageExpanded");
            for(int z=0; z<zDim; z++) {
                for(int i=0; i<srcImage.getExtents()[0]; i++) {
                    for(int j=0; j<srcImage.getExtents()[1]; j++) {
                        srcImageRealloc.set(i, j, z, srcImage.getShort(i, j, z));
                    }
                }
            }
            
            srcImageRealloc.setFileInfo(srcImage.getFileInfo());
            srcImageRealloc.calcMinMax();
    	} else {
    	    for(int z=0; z<zDim; z++) {
                for(int i=0; i<srcImage.getExtents()[0]; i++) {
                    for(int j=0; j<srcImage.getExtents()[1]; j++) {
                        finalImage.set(i, j, z, srcImage.getShort(i, j, z));
                    }
                }
            }
    	}

    	finalImage.setFileInfo(srcImage.getFileInfo());
        finalImage.calcMinMax();
        
        ViewJFrameImage newFrame = new ViewJFrameImage(finalImage);
        if(doImageB) {
            newFrame.setVisible(false);
            newFrame.setImageB(srcImageRealloc);
            newFrame.enableImageB(true);
        }
        newFrame.setVisible(true);
    }
    
    private void doTransformManual(Matrix3f toImage, ModelImage finalImage, int xDim, int yDim, int z) {
        FileInfoDicom infoDicomStitch = (FileInfoDicom) stitchImage.getFileInfo()[z];
        FileInfoDicom infoDicomOrig = (FileInfoDicom) srcImage.getFileInfo()[z];
        double xMin = 0, xMax = 0, yMin = 0, yMax = 0, zMin = 0, zMax = 0, xOffset = 0, yOffset = 0, zOffset = 0;
        
        //get necessary dicom elements for matrix multiplication
        Matrix3f toPosStitch = createSpacingMatrix(infoDicomStitch);
        Matrix3f toImageOrig = createSpacingMatrix(infoDicomOrig);
        
        toImageOrig.Inverse();
        
        toImageOrig.Mult(toPosStitch);
        
        for(int i=0; i<xDim; i++) {
            for(int j=0; j<yDim; j++) {
                //if(stitchImage.getShort(i, j, z) != 0) {
                    Vector3f posMove = new Vector3f();
                    posMove.Set(i, j, 1);
                    
                    Vector3f posResult = new Vector3f();
                    toImageOrig.MultRight(posMove, posResult);
                    
                    if(posResult.X < xMin) {
                        xMin = posResult.X;
                    } else if(posResult.X > xMax) {
                        xMax = posResult.X;
                    }
                    
                    if(posResult.Y < yMin) {
                        yMin = posResult.Y;
                    } else if(posResult.Y > yMax) {
                        yMax = posResult.Y;
                    }
                    
                    if(posResult.Z < zMin) {
                        zMin = posResult.Z;
                    } else if(posResult.Z > zMax) {
                        zMax = posResult.Z;
                    }
                //}
            }
        }
        xOffset = 0;// - xMin;
        yOffset = 0;// - yMin;
        zOffset = 0;// - zMin;
        System.out.println("Slice "+z+" xRange: "+xMin+", "+xMax+" yRange: "+yMin+", "+yMax+" zRange: "+zMin+", "+zMax);
        
        int imageVal = 0;
        for(int i=0; i<xDim; i++) {
            for(int j=0; j<yDim; j++) {
                if((imageVal = stitchImage.getShort(i, j, z)) != 0) {
                    Vector3f posMove = new Vector3f();
                    posMove.Set(i, j, 1);
                    
                    Vector3f posResult = new Vector3f();
                    toImage.MultRight(posMove, posResult);
                    
                    if(i == 160 && j == 61) {
                    	System.out.println(i+", "+j+" became "+posResult);
                    }
                    
                    finalImage.set((int)(posResult.X+xOffset), (int)(posResult.Y+yOffset), (int)(posResult.Z+zOffset)+z, imageVal);
                }
            }
        }        
    }

    private void doTransformationWithAlgo(Matrix3f toImage, ModelImage finalImage) {
        for(int z=0; z<stitchImage.getExtents()[2]; z++) {
            for(int i=0; i<stitchImage.getExtents()[0]; i++) {
                for(int j=0; j<stitchImage.getExtents()[1]; j++) {
                    finalImage.set(i, j, z, stitchImage.getShort(i, j, z));
                }
            }
        }
        finalImage.setFileInfo(stitchImage.getFileInfo().clone());
        finalImage.calcMinMax();
        
        toImage.Transpose(); //transpose for use as a TransMatrix only
        TransMatrix transform = new TransMatrix(4, 0);
        for(int i=0; i<3; i++) {
            for(int j=0; j<2; j++) {
                transform.set(i, j, toImage.Get(i, j));
            }
        }
        
        for(int i=0; i<3; i++) {
            transform.set(i, 3, toImage.Get(i, 2));
        }
        
        transform.set(2, 2, 0);
        transform.set(3, 3, 1);
        
        //ViewJFrameImage finalTemp = new ViewJFrameImage(finalImage);
        //finalTemp.setVisible(true);
        
        AlgorithmTransform.transformBilinear(stitchImage, finalImage, transform, null);
    }

    private Matrix3f createSpacingMatrix(FileInfoDicom infoDicom) {
        Double[] xyCos = fromObjectToDouble(infoDicom.getTagTable().get("0020,0037").getValueList());
        Double[] spacing = fromObjectToDouble(infoDicom.getTagTable().get("0028,0030").getValueList());
        Double[] position = fromObjectToDouble(infoDicom.getTagTable().get("0020,0032").getValueList());
        
        Matrix3f mat = new Matrix3f();
        mat.Set(0, 0, (float)(xyCos[0].doubleValue()*spacing[0].doubleValue()));
        mat.Set(1, 0, (float)(xyCos[1].doubleValue()*spacing[0].doubleValue()));
        mat.Set(2, 0, (float)(xyCos[2].doubleValue()*spacing[0].doubleValue()));
        
        mat.Set(0, 1, (float)(xyCos[3].doubleValue()*spacing[1].doubleValue()));
        mat.Set(1, 1, (float)(xyCos[4].doubleValue()*spacing[1].doubleValue()));
        mat.Set(2, 1, (float)(xyCos[5].doubleValue()*spacing[1].doubleValue()));
       
        mat.Set(0, 2, position[1].floatValue()); //should be 1
        mat.Set(1, 2, position[0].floatValue());
        mat.Set(2, 2, position[2].floatValue());
        
        return mat;
    }
    
    private Double[] fromObjectToDouble(Object[] ar) {
        Double[] arDouble = new Double[ar.length];
        try {
            for(int i=0; i<ar.length; i++) {
                arDouble[i] = Double.valueOf(ar[i].toString());
            }
        } catch(NumberFormatException nfe) {
            return null;
        }
        
        return arDouble;
    }
        
	
}
