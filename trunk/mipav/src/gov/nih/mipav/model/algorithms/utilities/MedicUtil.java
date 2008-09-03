package gov.nih.mipav.model.algorithms.utilities;

import java.awt.*;
import java.awt.event.*;
import java.net.URL;

import javax.swing.*;
import javax.help.*;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.jar.*;
import java.util.zip.*;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;


/** 
 *  A collection of utilities for I/O, GUI, interface with MIPAV, etc.
 *
 *	@author Pierre-Louis Bazin
 *	@author Blake Lucas
 */
public class MedicUtil {
	protected static ViewUserInterface userInterface=null;
	protected static boolean quiet=false;

	public static void setQuiet(boolean q){
		quiet=q;
	}
	public static boolean isQuiet(){
		return quiet;
	}
	/**
	 * Singleton method to get User Interface
	 * @return userInterface
	 */
	public static ViewUserInterface getUI(){
		if(userInterface==null)userInterface=ViewUserInterface.getReference();
		return userInterface;
	}
	public static void displayMessage(String message){
		// GUI output
		if (!isQuiet()) getUI().setGlobalDataText(message);
		// console output
		System.out.print(message);
		System.out.flush();
	}
	public static void displayError(String message){
		// GUI output
		if (!isQuiet()) MipavUtil.displayError(message);
		// console output
		System.err.print(message);
		System.err.flush();
	}
    
	
   

	
	
	/**
     * Copy important file information between ModelImage structures,
	 *	assuming all slices have same properties (uses only the first slice from
	 *	the source).
     *
     * @param  image        Source image.
     * @param  resultImage  Resultant image.
     */
    public static final void updateFileInfo(ModelImage image, ModelImage resultImage) {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();
            
			fileInfo[0].setModality(image.getFileInfo()[0].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
			fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(image.getFileInfo()[0].getResolutions());
            fileInfo[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
            fileInfo[0].setOrigin(image.getFileInfo()[0].getOrigin());
            fileInfo[0].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[0].setPhotometric(image.getFileInfo()[0].getPhotometric());
			
			fileInfo[0].setImageOrientation(image.getImageOrientation());
            
			fileInfo[0].setExtents(resultImage.getExtents());
            fileInfo[0].setMax(resultImage.getMax());
            fileInfo[0].setMin(resultImage.getMin());
            
        } else if (resultImage.getNDims() == 3) {
			//System.out.print("3:");
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                fileInfo[i].setModality(image.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
				fileInfo[i].setEndianess(image.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[0].getResolutions());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[0].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[0].getPhotometric());
               
		        fileInfo[i].setImageOrientation(image.getImageOrientation());
				
				fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
            }
        } else if (resultImage.getNDims() == 4) {
            //System.out.print("4:");
            fileInfo = resultImage.getFileInfo();

			int[] units = new int[4];
			float[] res = new float[4];
			for (int n=0;n<4;n++) {
				if (n<image.getNDims()) {
					units[n] = image.getFileInfo()[0].getUnitsOfMeasure()[n];
					res[n] = image.getFileInfo()[0].getResolutions()[n];
				} else {
					units[n] = image.getFileInfo()[0].getUnitsOfMeasure()[image.getNDims()-1];
					res[n] = image.getFileInfo()[0].getResolutions()[image.getNDims()-1];
				}					
			}
				
            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                fileInfo[i].setModality(image.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[0].getEndianess());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[0].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[0].getPhotometric());
				
				fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setResolutions(res);
                
				fileInfo[i].setImageOrientation(image.getImageOrientation());
                
				fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
            }
        }
    }
    
	/**
     * Copy important file information between ModelImage structures,
	 *	assuming all slices have same properties (uses only the first slice from
	 *	the source).
     *
     * @param  image        Source image.
     * @param  resultImage  Resultant image.
     */
    public static final void updateFileInfo(FileInfoBase info, ModelImage resultImage) {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();
            
			fileInfo[0].setModality(info.getModality());
            fileInfo[0].setFileDirectory(info.getFileDirectory());
			fileInfo[0].setEndianess(info.getEndianess());
            fileInfo[0].setUnitsOfMeasure(info.getUnitsOfMeasure());
            fileInfo[0].setResolutions(info.getResolutions());
            fileInfo[0].setAxisOrientation(info.getAxisOrientation());
            fileInfo[0].setOrigin(info.getOrigin());
            fileInfo[0].setPixelPadValue(info.getPixelPadValue());
            fileInfo[0].setPhotometric(info.getPhotometric());
			
			fileInfo[0].setImageOrientation(info.getImageOrientation());
            
			fileInfo[0].setExtents(resultImage.getExtents());
            fileInfo[0].setMax(resultImage.getMax());
            fileInfo[0].setMin(resultImage.getMin());
            
        } else if (resultImage.getNDims() == 3) {
			//System.out.print("3:");
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                fileInfo[i].setModality(info.getModality());
                fileInfo[i].setFileDirectory(info.getFileDirectory());
				fileInfo[i].setEndianess(info.getEndianess());
                fileInfo[i].setUnitsOfMeasure(info.getUnitsOfMeasure());
                fileInfo[i].setResolutions(info.getResolutions());
                fileInfo[i].setAxisOrientation(info.getAxisOrientation());
                fileInfo[i].setOrigin(info.getOrigin());
                fileInfo[i].setPixelPadValue(info.getPixelPadValue());
                fileInfo[i].setPhotometric(info.getPhotometric());
               
		        fileInfo[i].setImageOrientation(info.getImageOrientation());
				
				fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
            }
        } else if (resultImage.getNDims() == 4) {
            //System.out.print("4:");
            fileInfo = resultImage.getFileInfo();

			int[] units = new int[4];
			float[] res = new float[4];
			for (int n=0;n<4;n++) {
				units[n] = info.getUnitsOfMeasure()[n];
				res[n] = info.getResolutions()[n];
			}
				
            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                fileInfo[i].setModality(info.getModality());
                fileInfo[i].setFileDirectory(info.getFileDirectory());
                fileInfo[i].setEndianess(info.getEndianess());
                fileInfo[i].setAxisOrientation(info.getAxisOrientation());
                fileInfo[i].setOrigin(info.getOrigin());
                fileInfo[i].setPixelPadValue(info.getPixelPadValue());
                fileInfo[i].setPhotometric(info.getPhotometric());
				
				fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setResolutions(res);
                
				fileInfo[i].setImageOrientation(info.getImageOrientation());
                
				fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
            }
        }
    }
    

}
