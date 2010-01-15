package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.IOException;
import java.util.Arrays;


/**
 * The class creates a RGB image of a 3d image in a Lightbox type format. 
 * @version  0.1 May 12, 2009
 * @author morseaj
 */

public class LightboxGenerator  extends AlgorithmBase{

    //~ Instance fields ------------------------------------------------------------------------------------------------



    private int startSlice;
    
    private int endSlice;
    
    /** Height of new image in pixels */
    private int newHeight;
    
    /** Width of new image in pixels */
    private int newWidth;
    
    private ModelImage original;
    
    private int rows;
    
    private int columns;
    
    private byte borderR;

    private byte borderG;
    
    private byte borderB;
    
    private boolean display;
    
    private int thickness;
    
    private ModelImage finalImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for Lightbox Image Files that will be used to create a lightBox image.
     *
     * @param image 3D image to be created in lightbox format
     * @param startSlice first slice to be used
     * @param endSlice last slice to be used
     * @param percentSize shrink images by percent
     * @param rows number of rows in lightbox
     * @param columns number of columns in lightbox
     * @param borderR R value for border
     * @param borderG G value for border
     * @param borderB B value for border
     * @param display should lightbox image be shown in frame once created?
     * @param borderThickness border thickness in pixels
     *           
     * 
     */

    public LightboxGenerator(ModelImage image, int startSlice,int endSlice, double percentSize,
    		int rows, int columns, int borderR, int borderG, int borderB, boolean display, int borderThickness) {
    	
    	//Creating a blank TransMatrix for resampling
		TransMatrix percentSizer = new TransMatrix(4);
		percentSizer.Set((float)1, (float)0, (float)0, (float)0, (float)0,
				(float)1, (float)0, (float)0, (float)0, (float)0, (float)1, (float)0, 
				(float)0, (float)0, (float)0, (float)1);
    	
		//Resample image size based on percent inputted
    	AlgorithmTransform transformer = new AlgorithmTransform(image, percentSizer, 1, (float)(image.getResolutions(0)[0]/(percentSize*.01)),
    			(float)(image.getResolutions(0)[1]/(percentSize*.01)), (int)(image.getExtents()[0] * percentSize*.01),
    			(int)(image.getExtents()[1]*percentSize*.01), image.getUnitsOfMeasure(), false, true, false, true, image.getImageCentermm(false) );
    	transformer.runAlgorithm();
    	image = transformer.getTransformedImage();

        this.original = image;
        this.startSlice = startSlice;
        this.endSlice = endSlice;
        this.rows = rows;
        this.columns = columns;
        this.newWidth = (int) (original.getExtents()[0]*columns) + (borderThickness*(columns+1));
        this.newHeight = (int) (original.getExtents()[1]*rows) + (borderThickness*(rows+1));
        this.borderR = (byte) borderR;
        this.borderG = (byte) borderG;
        this.borderB = (byte) borderB;
        this.display = display;
        this.thickness = borderThickness;

    }

    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() 
    {
    	if (original != null)
    	{
    		original.disposeLocal();
    		original = null;
    	}
    	


    	
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * returns the newly created image
     */
    public ModelImage getImage() 
    {
    	return finalImage;
    }

    /**
     * Accessor that returns the number of image slices saved.
     *
     * @return  The number of images.
     */


	public void runAlgorithm() {
    	try {
	    	int dim[] = new int[2];
	    	dim[0] = newWidth;
	    	dim[1] = newHeight;
	    	
	    	fireProgressStateChanged(10, original.getImageName(), "Creating Lightbox Image...");
	    	
	    	ModelImage newImage = (ModelImage) original.clone();
	    	newImage.setImageName(original.getImageName()+"_LightBox");
	    	float blank[] = new float[original.getSize()];
	    	Arrays.fill(blank, 0);
				newImage.importData(0, blank, false);
	    	newImage.setExtents(dim);
	    	newImage.recomputeDataSize();
	    	
	    	int currentSlice = startSlice;
	    	int numPerRow = original.getExtents()[0];
	    	int numPerColumn = original.getExtents()[1];
	    	int numPerSlice = numPerRow*numPerColumn;
	    	int mult = 1;
	    	
	    	//if image is in color
	    	if (original.isColorImage())
	    	{
	    		//numPerRow = numPerRow*4;
	    		//numPerSlice = numPerSlice*4;
	    		mult = 4;
	    	}
	    	
	    	float currentImageData[] = new float[numPerSlice*mult];
	    	float currentRowData[] = new float[numPerRow*mult];

	    	//export slice
	    	original.exportData(startSlice*numPerSlice*mult, numPerSlice*mult, currentImageData);

	
	    	//create lightbox image
	    	for(int i = 0; i < rows; i++)
	    	{        	
	    		for(int j = 0; j < columns; j++)
	    		{
	    			
	    			fireProgressStateChanged(10+ (((i*j)+j)/(columns*rows))*70);
					for(int jj = 0; jj < numPerColumn; jj++)
					{
						System.arraycopy(currentImageData, jj*numPerRow*mult, currentRowData, 0, numPerRow*mult);
						newImage.importData((((numPerColumn + thickness) * newWidth * i) + ((numPerRow+thickness) * j)+ (newWidth*jj) + (thickness*newWidth) +thickness)*mult , currentRowData, false);
						Arrays.fill(currentRowData, 0);
					}	

					currentSlice++;
					if (currentSlice <= endSlice)	
					{
						Arrays.fill(currentImageData, 0);
						original.exportData(currentSlice*numPerSlice*mult, numPerSlice*mult, currentImageData);
					}
					else
					{
						i = rows;
						j = columns;
					}
	    		}
	
	    	}
	    	newImage.calcMinMax();
	    	
	    	
	    	
	    	//convert to RGB
	    	ModelImage newRGB;
	    	if(original.isColorImage()) {
	    		newRGB = (ModelImage)newImage.clone();
	    		newImage.disposeLocal();
	    	}else {
	    		newRGB = new ModelImage(ModelImage.ARGB, newImage.getExtents(), newImage.getImageName());
		    	AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(newImage, newImage, newImage, newRGB, true, true,
	                                                                 255.0f, true);
		    	mathAlgo.run();
		    	fireProgressStateChanged(90);
		    	newImage.disposeLocal();
	    	}
	    	
	    	
	    	//add borders
	    	
	    	byte[] borderRow = new byte[newWidth*thickness];
	    	byte[] borderColumn = new byte[thickness];
	                       
	    	for(int i = 0; i <= rows; i++)
	    	{
	
	    		
	    		Arrays.fill(borderRow, borderR);
	    		newRGB.importRGBData(1, (i*newWidth*4*(numPerColumn+thickness)), borderRow, false);
	    		Arrays.fill(borderRow, borderG);
	    		newRGB.importRGBData(2, (i*newWidth*4*(numPerColumn+thickness)), borderRow, false);
	    		Arrays.fill(borderRow, borderB);
	    		newRGB.importRGBData(3, (i*newWidth*4*(numPerColumn+thickness)), borderRow, false);
	    	}
	    	fireProgressStateChanged(95);
	    	for(int i = 0; i < newHeight; i++)
	    	{
	    		for(int j = 0; j <= newWidth; j=j+(numPerRow+thickness))
	    		{
		    		Arrays.fill(borderColumn, borderR);
		    		newRGB.importRGBData(1, j*4 + (i*newWidth*4), borderColumn, false);
		    		Arrays.fill(borderColumn, borderG);
		    		newRGB.importRGBData(2, j*4 + (i*newWidth*4), borderColumn, false);
		    		Arrays.fill(borderColumn, borderB);
		    		newRGB.importRGBData(3, j*4 + (i*newWidth*4), borderColumn, false);
	    		}
	    	}
	    	
	    	
	    	newRGB.calcMinMax();
	    	
	    	if (display)
	    	new ViewJFrameImage(newRGB);
	    	fireProgressStateChanged(100);
	    	finalImage = newRGB;
	    	
		} catch (IOException e) {
			e.printStackTrace();
		}
    	

	}
}