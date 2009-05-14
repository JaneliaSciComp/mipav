package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.lang.Object;
import java.util.Arrays;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * The class creates a RGB image of a 3d image in a Lightbox type format. 
 * @version  0.1 May 12, 2009
 * @author morseaj
 */

public class LightboxGenerator  {

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
     * @throws IOException 
     */
    public void run() throws IOException
    {
    	
    	int dim[] = new int[2];
    	dim[0] = newWidth;
    	dim[1] = newHeight;
    	
    	ModelImage newImage = (ModelImage) original.clone();
    	newImage.setImageName(original.getImageName()+"_LightBox");
    	float blank[] = new float[original.getSize()];
    	Arrays.fill(blank, 0);
    	newImage.importData(0, blank, false);
    	newImage.setExtents(dim);
    	
    	int currentSlice = startSlice;
    	int numPerRow = original.getExtents()[0];
    	int numPerColumn = original.getExtents()[1];
    	int numPerSlice = numPerRow*numPerColumn;
    	int mult = 1;
    	
    	//if image is in color
    	if (original.isColorImage())
    	{
    		numPerRow = numPerRow*4;
    		numPerSlice = numPerSlice*4;
    		mult = 4;
    	}
    	
    	float currentImageData[] = new float[numPerSlice];
    	float currentRowData[] = new float[numPerRow];
    	

    	//export slice
    	original.exportData(startSlice*numPerSlice, numPerSlice, currentImageData);


    	//create lightbox image
    	for(int i = 0; i < rows; i++)
    	{        	
    		for(int j = 0; j < columns; j++)
    		{
				for(int jj = 0; jj < numPerColumn; jj++)
				{
					System.arraycopy(currentImageData, jj*numPerRow, currentRowData, 0, numPerRow);
					newImage.importData(((numPerColumn + thickness) * newWidth * i * mult) + ((numPerRow+thickness) * j)+ (newWidth*jj*mult) + (newWidth*mult) + (thickness*newWidth) +thickness , currentRowData, false);
					Arrays.fill(currentRowData, 0);
				}	

		    	
				currentSlice++;
				if (currentSlice <= endSlice)	
				{
					Arrays.fill(currentImageData, 0);
					original.exportData(currentSlice*numPerSlice, numPerSlice, currentImageData);
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
    	
    	ModelImage newRGB = new ModelImage(ModelImage.ARGB, newImage.getExtents(), newImage.getImageName());
    	AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(newImage, newImage, newImage, newRGB, true, true);
    	mathAlgo.run();
    	
    	newImage.disposeLocal();
    	
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
    	
    	finalImage = newRGB;
    	

    }
}