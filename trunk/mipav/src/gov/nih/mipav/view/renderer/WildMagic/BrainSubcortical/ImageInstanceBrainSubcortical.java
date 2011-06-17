package gov.nih.mipav.view.renderer.WildMagic.BrainSubcortical;

import gov.nih.mipav.model.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.structures.*;

import java.awt.Color;
import java.awt.Dimension;
import java.io.File;
import java.io.*;
import java.util.*;

import javax.swing.JFileChooser;
import javax.swing.JTextField;

import java.io.FileOutputStream;

import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfWriter;
import com.itextpdf.text.Document;
import com.itextpdf.text.Paragraph;



public class ImageInstanceBrainSubcortical {
	
	private String fileName;
	private String directory;
	private JFileChooser chooser = new JFileChooser();
	private ViewUserInterface UI;
	private ModelImage myImage;
	private JTextField textField;
	private String file_suffix;
	
    public 	Vector<StatisticsData> myData= new Vector<StatisticsData>();

    /** Same data-type, binary, or unsigned byte mask. */
   private int outputType = 0;

   // threshold constants value of color labeling. 
   private static float LeftHippocampus = 17.0f;
   private static float RightHippocampus = 53.0f;

   private static float LeftAmygdala = 18.0f;
   private static float RightAmygdala = 54.0f;

   private static float LeftCaudate = 11.0f;
   private static float RightCaudate = 50.0f;

   private static float LeftPutamen = 12.0f;
   private static float RightPutamen = 51.0f;

   private static float LeftGlobusPallidus = 13.0f;
   private static float RightGlobusPallidus = 52.0f;

   private static float LeftThalamus = 10.0f;
   private static float RightThalamus = 49.0f;

   // threshold algorithm	
   private AlgorithmThresholdDual algorThreshold_LeftHippocampus;
   private 	AlgorithmThresholdDual algorThreshold_RightHippocampus;
	 
   private 	AlgorithmThresholdDual algorThreshold_LeftAmygdala;
   private 	AlgorithmThresholdDual algorThreshold_RightAmygdala;

   private AlgorithmThresholdDual algorThreshold_LeftCaudate;
   private AlgorithmThresholdDual algorThreshold_RightCaudate;

   private AlgorithmThresholdDual algorThreshold_LeftPutamen;
   private AlgorithmThresholdDual algorThreshold_RightPutamen;

   private AlgorithmThresholdDual algorThreshold_LeftGlobusPallidus;
   private AlgorithmThresholdDual algorThreshold_RightGlobusPallidus;

   private AlgorithmThresholdDual algorThreshold_LeftThalamus;
   private AlgorithmThresholdDual algorThreshold_RightThalamus;
	
   // registration algorithm
   private AlgorithmRegOAR3D regLeftHippocampus;
   private AlgorithmRegOAR3D regRightHippocampus;
   
   private AlgorithmRegOAR3D regLeftAmygdala;
   private AlgorithmRegOAR3D regRightAmygdala;

   private AlgorithmRegOAR3D regLeftCaudate;
   private AlgorithmRegOAR3D regRightCaudate;

   private AlgorithmRegOAR3D regLeftPutamen;
   private AlgorithmRegOAR3D regRightPutamen;

   private AlgorithmRegOAR3D regLeftGlobusPallidus;
   private AlgorithmRegOAR3D regRightGlobusPallidus;

   private AlgorithmRegOAR3D regLeftThalamus;
   private AlgorithmRegOAR3D regRightThalamus;
   
   
   // model images
   public ModelImage image_LeftHippocampus;
   public ModelImage image_RightHippocampus;
	 
   public ModelImage image_LeftAmygdala;
   public ModelImage image_RightAmygdala;

   public ModelImage image_LeftCaudate;
   public ModelImage image_RightCaudate;

   public ModelImage image_LeftPutamen;
   public ModelImage image_RightPutamen;

   public ModelImage image_LeftGlobusPallidus;
   public ModelImage image_RightGlobusPallidus;

   public ModelImage image_LeftThalamus;
   public ModelImage image_RightThalamus;
	

   // registered images
   public ModelImage regImage_LeftHippocampus;
   public ModelImage regImage_RightHippocampus;
	 
   public ModelImage regImage_LeftAmygdala;
   public ModelImage regImage_RightAmygdala;

   public ModelImage regImage_LeftCaudate;
   public ModelImage regImage_RightCaudate;

   public ModelImage regImage_LeftPutamen;
   public ModelImage regImage_RightPutamen;

   public ModelImage regImage_LeftGlobusPallidus;
   public ModelImage regImage_RightGlobusPallidus;

   public ModelImage regImage_LeftThalamus;
   public ModelImage regImage_RightThalamus;

   
   public JDialogBrainSubcortical parentDialog;
   
   
   public ImageInstanceBrainSubcortical instanceB;

   
   // images comparison 
   public ModelImage image_LeftHippocampus_binary;
   public ModelImage image_RightHippocampus_binary;
   
   public ModelImage image_LeftAmygdala_binary;
   public ModelImage image_RightAmygdala_binary;

   public ModelImage image_LeftCaudate_binary;
   public ModelImage image_RightCaudate_binary;

   public ModelImage image_LeftPutamen_binary;
   public ModelImage image_RightPutamen_binary;

   public ModelImage image_LeftGlobusPallidus_binary;
   public ModelImage image_RightGlobusPallidus_binary;

   public ModelImage image_LeftThalamus_binary;
   public ModelImage image_RightThalamus_binary;   
   
   private AlgorithmThresholdDual algorThreshold_LeftHippocampus_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightHippocampus_binary;
   
   private 	AlgorithmThresholdDual algorThreshold_LeftAmygdala_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightAmygdala_binary;

   private AlgorithmThresholdDual algorThreshold_LeftCaudate_binary;
   private AlgorithmThresholdDual algorThreshold_RightCaudate_binary;

   private AlgorithmThresholdDual algorThreshold_LeftPutamen_binary;
   private AlgorithmThresholdDual algorThreshold_RightPutamen_binary;

   private AlgorithmThresholdDual algorThreshold_LeftGlobusPallidus_binary;
   private AlgorithmThresholdDual algorThreshold_RightGlobusPallidus_binary;

   private AlgorithmThresholdDual algorThreshold_LeftThalamus_binary;
   private AlgorithmThresholdDual algorThreshold_RightThalamus_binary;

   
   public ModelImage regImage_LeftHippocampus_binary;
   public ModelImage regImage_RightHippocampus_binary;
   
   public ModelImage regImage_LeftAmygdala_binary;
   public ModelImage regImage_RightAmygdala_binary;

   public ModelImage regImage_LeftCaudate_binary;
   public ModelImage regImage_RightCaudate_binary;

   public ModelImage regImage_LeftPutamen_binary;
   public ModelImage regImage_RightPutamen_binary;

   public ModelImage regImage_LeftGlobusPallidus_binary;
   public ModelImage regImage_RightGlobusPallidus_binary;

   public ModelImage regImage_LeftThalamus_binary;
   public ModelImage regImage_RightThalamus_binary;   
   
   
   
   private AlgorithmThresholdDual algorThreshold_LeftHippocampus_reg_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightHippocampus_reg_binary;
   
   private 	AlgorithmThresholdDual algorThreshold_LeftAmygdala_reg_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightAmygdala_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftCaudate_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightCaudate_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftPutamen_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightPutamen_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftGlobusPallidus_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightGlobusPallidus_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftThalamus_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightThalamus_reg_binary;
   
   
	 public ImageInstanceBrainSubcortical(String _suffix, JTextField _textField, JDialogBrainSubcortical parent) {
	        file_suffix = _suffix;
	        textField = _textField;
	        UI = ViewUserInterface.getReference();
	        parentDialog = parent;
	 }

	public void disposeLocal() {

		// image original
		image_LeftHippocampus = null;
		image_RightHippocampus = null;

		image_LeftAmygdala = null;
		image_RightAmygdala = null;

		image_LeftCaudate = null;
		image_RightCaudate = null;

		image_LeftPutamen = null;
		image_RightPutamen = null;

		image_LeftGlobusPallidus = null;
		image_RightGlobusPallidus = null;

		image_LeftThalamus = null;
		image_RightThalamus = null;

		// registered image
		regImage_LeftHippocampus = null;
		regImage_RightHippocampus = null;

		regImage_LeftAmygdala = null;
		regImage_RightAmygdala = null;

		regImage_LeftCaudate = null;
		regImage_RightCaudate = null;

		regImage_LeftPutamen = null;
		regImage_RightPutamen = null;

		regImage_LeftGlobusPallidus = null;
		regImage_RightGlobusPallidus = null;

		regImage_LeftThalamus = null;
		regImage_RightThalamus = null;

		// image in binary
		image_LeftHippocampus_binary = null;
		image_RightHippocampus_binary = null;

		image_LeftAmygdala_binary = null;
		image_RightAmygdala_binary = null;

		image_LeftCaudate_binary = null;
		image_RightCaudate_binary = null;

		image_LeftPutamen_binary = null;
		image_RightPutamen_binary = null;

		image_LeftGlobusPallidus_binary = null;
		image_RightGlobusPallidus_binary = null;

		image_LeftThalamus_binary = null;
		image_RightThalamus_binary = null;

		// Registered image in binary
		regImage_LeftHippocampus_binary = null;
		regImage_RightHippocampus_binary = null;

		regImage_LeftAmygdala_binary = null;
		regImage_RightAmygdala_binary = null;

		regImage_LeftCaudate_binary = null;
		regImage_RightCaudate_binary = null;

		regImage_LeftPutamen_binary = null;
		regImage_RightPutamen_binary = null;

		regImage_LeftGlobusPallidus_binary = null;
		regImage_RightGlobusPallidus_binary = null;

		regImage_LeftThalamus_binary = null;
		regImage_RightThalamus_binary = null;

		System.gc();

	}
	 public void threshold() {
	
		 float[] thresholds = new float[2];

	       

	        float fillValue = 0;
	        boolean isInverse = false;
	        boolean regionFlag = true;


	        try {
	        	
	        	float thres1 = LeftHippocampus;  // lowerThres;
	 	        float thres2 = LeftHippocampus;  // upperThres;

	 	        thresholds[0] = thres1;
	 	        thresholds[1] = thres2;

	        	image_LeftHippocampus = (ModelImage)myImage.clone();
	        	image_LeftHippocampus.setType(myImage.getType());
	        	image_LeftHippocampus.setImageName(myImage.getImageName() + "_leftHippocampus");
	            algorThreshold_LeftHippocampus = new AlgorithmThresholdDual(image_LeftHippocampus, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_LeftHippocampus.addListener(parentDialog);
	            algorThreshold_LeftHippocampus.run();
	            
	            //   Right Hippocampus ****
	            thres1 = RightHippocampus;  // lowerThres;
		        thres2 = RightHippocampus;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_RightHippocampus = (ModelImage)myImage.clone();
	        	image_RightHippocampus.setType(myImage.getType());
	        	image_RightHippocampus.setImageName(myImage.getImageName() + "_rightHippocampus");
	        	algorThreshold_RightHippocampus = new AlgorithmThresholdDual(image_RightHippocampus, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_RightHippocampus.addListener(parentDialog);
	            algorThreshold_RightHippocampus.run();
	            
	            // Left Amygdala  ***
	            thres1 = LeftAmygdala;  // lowerThres;
		        thres2 = LeftAmygdala;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_LeftAmygdala = (ModelImage)myImage.clone();
	        	image_LeftAmygdala.setType(myImage.getType());
	        	image_LeftAmygdala.setImageName(myImage.getImageName() + "_LeftAmygdala");
	            algorThreshold_LeftAmygdala = new AlgorithmThresholdDual(image_LeftAmygdala, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_LeftAmygdala.addListener(parentDialog);
	            algorThreshold_LeftAmygdala.run();
	            
	            
	            // Right Amygdala  ***
	            thres1 = RightAmygdala;  // lowerThres;
		        thres2 = RightAmygdala;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_RightAmygdala = (ModelImage)myImage.clone();
	        	image_RightAmygdala.setType(myImage.getType());
	        	image_RightAmygdala.setImageName(myImage.getImageName() + "_RightAmygdala");
	            algorThreshold_RightAmygdala = new AlgorithmThresholdDual(image_RightAmygdala, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_RightAmygdala.addListener(parentDialog);
	            algorThreshold_RightAmygdala.run();
	            
	            // Left Caudate  ***
	            thres1 = LeftCaudate;  // lowerThres;
		        thres2 = LeftCaudate;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_LeftCaudate = (ModelImage)myImage.clone();
	        	image_LeftCaudate.setType(myImage.getType());
	        	image_LeftCaudate.setImageName(myImage.getImageName() + "_LeftCaudate");
	            algorThreshold_LeftCaudate = new AlgorithmThresholdDual(image_LeftCaudate, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_LeftCaudate.addListener(parentDialog);
	            algorThreshold_LeftCaudate.run();
	            
	            // RightCaudate ***
	            thres1 = RightCaudate;  // lowerThres;
		        thres2 = RightCaudate;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_RightCaudate = (ModelImage)myImage.clone();
	        	image_RightCaudate.setType(myImage.getType());
	        	image_RightCaudate.setImageName(myImage.getImageName() + "_RightCaudate");
	            algorThreshold_RightCaudate = new AlgorithmThresholdDual(image_RightCaudate, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_RightCaudate.addListener(parentDialog);
	            algorThreshold_RightCaudate.run();

	            //  Left Putamen ***
	            thres1 = LeftPutamen;  // lowerThres;
		        thres2 = LeftPutamen;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_LeftPutamen = (ModelImage)myImage.clone();
	        	image_LeftPutamen.setType(myImage.getType());
	        	image_LeftPutamen.setImageName(myImage.getImageName() + "_LeftPutamen");
	            algorThreshold_LeftPutamen = new AlgorithmThresholdDual(image_LeftPutamen, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_LeftPutamen.addListener(parentDialog);
	            algorThreshold_LeftPutamen.run();
	            
	            
	            //  Right Putamen  ***
	            thres1 = RightPutamen;  // lowerThres;
		        thres2 = RightPutamen;  // upperThres;
		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_RightPutamen = (ModelImage)myImage.clone();
	        	image_RightPutamen.setType(myImage.getType());
	        	image_RightPutamen.setImageName(myImage.getImageName() + "_RightPutamen");   
	            algorThreshold_RightPutamen = new AlgorithmThresholdDual(image_RightPutamen, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_RightPutamen.addListener(parentDialog);
	            algorThreshold_RightPutamen.run();

	            
	            //  Left GlobusPallidus  ***
	            thres1 = LeftGlobusPallidus;  // lowerThres;
		        thres2 = LeftGlobusPallidus;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_LeftGlobusPallidus = (ModelImage)myImage.clone();
	        	image_LeftGlobusPallidus.setType(myImage.getType());
	        	image_LeftGlobusPallidus.setImageName(myImage.getImageName() + "_LeftGlobusPallidus");
	        	algorThreshold_LeftGlobusPallidus = new AlgorithmThresholdDual(image_LeftGlobusPallidus, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_LeftGlobusPallidus.addListener(parentDialog);
	            algorThreshold_LeftGlobusPallidus.run();

	            
	            // Right GlobusPallidus  ***
	            thres1 = RightGlobusPallidus;  // lowerThres;
		        thres2 = RightGlobusPallidus;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_RightGlobusPallidus = (ModelImage)myImage.clone();
	        	image_RightGlobusPallidus.setType(myImage.getType());
	        	image_RightGlobusPallidus.setImageName(myImage.getImageName() + "_RightGlobusPallidus");
	        	algorThreshold_RightGlobusPallidus = new AlgorithmThresholdDual(image_RightGlobusPallidus, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_RightGlobusPallidus.addListener(parentDialog);
	            algorThreshold_RightGlobusPallidus.run();
	            
	            //   Left Thalamus  ***
	            thres1 = LeftThalamus;  // lowerThres;
		        thres2 = LeftThalamus;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_LeftThalamus = (ModelImage)myImage.clone();
	        	image_LeftThalamus.setType(myImage.getType());
	        	image_LeftThalamus.setImageName(myImage.getImageName() + "_LeftThalamus");	
	            algorThreshold_LeftThalamus = new AlgorithmThresholdDual(image_LeftThalamus, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_LeftThalamus.addListener(parentDialog);
	            algorThreshold_LeftThalamus.run();
	            
	            
	            //   Right  Thalamus  ***
	            thres1 = RightThalamus;  // lowerThres;
		        thres2 = RightThalamus;  // upperThres;

		        thresholds[0] = thres1;
		        thresholds[1] = thres2;
	            image_RightThalamus = (ModelImage)myImage.clone();
	        	image_RightThalamus.setType(myImage.getType());
	        	image_RightThalamus.setImageName(myImage.getImageName() + "_RightThalamus");
	        	algorThreshold_RightThalamus = new AlgorithmThresholdDual(image_RightThalamus, myImage, thresholds, fillValue, outputType, regionFlag, isInverse);
	            algorThreshold_RightThalamus.addListener(parentDialog);
	            algorThreshold_RightThalamus.run();
	            
	            
	            
	            
	        } catch (OutOfMemoryError x) {
	            MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

	            return;
	        }
	 }
	 
	 public void registration(ImageInstanceBrainSubcortical _instanceB ) {
		 int cost = 1; 
		 int DOF = 6;
         int interp = 0; 
         float rotateBeginX = 10.0f; 
         float rotateEndX = -10.0f; 
         float coarseRateX = 10.0f;
         float fineRateX = 3.0f; 
         float rotateBeginY = 10.0f;
         float rotateEndY = -10.0f;
         float coarseRateY = 10.0f;
         float fineRateY = 3.0f;
         float rotateBeginZ = 10.0f;
         float rotateEndZ = -10.0f;
         float coarseRateZ = 10.0f;
         float fineRateZ = 3.0f;
         boolean maxOfMinResol = true;
         boolean doSubsample = true;
         boolean fastMode = true;
         int bracketBound = 10;
         int maxIterations = 2;
         int numMinima = 3;
         boolean doJTEM = false;
         
         instanceB = _instanceB;
         
         regLeftHippocampus = new AlgorithmRegOAR3D(image_LeftHippocampus, instanceB.image_LeftHippocampus, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regLeftHippocampus.setJTEM(doJTEM);
         regLeftHippocampus.addListener(parentDialog);
         regLeftHippocampus.run();
         
         
         regRightHippocampus = new AlgorithmRegOAR3D(image_RightHippocampus, instanceB.image_RightHippocampus, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regRightHippocampus.setJTEM(doJTEM);
         regRightHippocampus.addListener(parentDialog);
         regRightHippocampus.run();
         
         
         regLeftAmygdala = new AlgorithmRegOAR3D(image_LeftAmygdala, instanceB.image_LeftAmygdala, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regLeftAmygdala.setJTEM(doJTEM);
         regLeftAmygdala.addListener(parentDialog);
         regLeftAmygdala.run();
         
         regRightAmygdala = new AlgorithmRegOAR3D(image_RightAmygdala, instanceB.image_RightAmygdala, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regRightAmygdala.setJTEM(doJTEM);
         regRightAmygdala.addListener(parentDialog);
         regRightAmygdala.run();
         
         
         regLeftCaudate = new AlgorithmRegOAR3D(image_LeftCaudate, instanceB.image_LeftCaudate, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regLeftCaudate.setJTEM(doJTEM);
         regLeftCaudate.addListener(parentDialog);
         regLeftCaudate.run();
         
         regRightCaudate = new AlgorithmRegOAR3D(image_RightCaudate, instanceB.image_RightCaudate, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regRightCaudate.setJTEM(doJTEM);
         regRightCaudate.addListener(parentDialog);
         regRightCaudate.run();
         
         
         regLeftPutamen = new AlgorithmRegOAR3D(image_LeftPutamen, instanceB.image_LeftPutamen, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regLeftPutamen.setJTEM(doJTEM);
         regLeftPutamen.addListener(parentDialog);
         regLeftPutamen.run();
         
         regRightPutamen = new AlgorithmRegOAR3D(image_RightPutamen, instanceB.image_RightPutamen, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regRightPutamen.setJTEM(doJTEM);
         regRightPutamen.addListener(parentDialog);
         regRightPutamen.run();
         
         
         regLeftGlobusPallidus = new AlgorithmRegOAR3D(image_LeftGlobusPallidus, instanceB.image_LeftGlobusPallidus, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regLeftGlobusPallidus.setJTEM(doJTEM);
         regLeftGlobusPallidus.addListener(parentDialog);
         regLeftGlobusPallidus.run();
         
         regRightGlobusPallidus = new AlgorithmRegOAR3D(image_RightGlobusPallidus, instanceB.image_RightGlobusPallidus, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regRightGlobusPallidus.setJTEM(doJTEM);
         regRightGlobusPallidus.addListener(parentDialog);
         regRightGlobusPallidus.run();
         
         
         regLeftThalamus = new AlgorithmRegOAR3D(image_LeftThalamus, instanceB.image_LeftThalamus, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regLeftThalamus.setJTEM(doJTEM);
         regLeftThalamus.addListener(parentDialog);
         regLeftThalamus.run();
         
         regRightThalamus = new AlgorithmRegOAR3D(image_RightThalamus, instanceB.image_RightThalamus, cost, DOF, interp, rotateBeginX, rotateEndX,
                 coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                 rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, fastMode, bracketBound,
                 maxIterations, numMinima);
         regRightThalamus.setJTEM(doJTEM);
         regRightThalamus.addListener(parentDialog);
         regRightThalamus.run();
       
          
         
	 }
	 
	 public void selectFile() {
	        chooser.setDialogTitle("Open Image");

	        if (UI.getDefaultDirectory() != null) {
	            final File file = new File(UI.getDefaultDirectory());

	            if (file != null) {
	                chooser.setCurrentDirectory(file);
	            } else {
	                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	            }
	        } else {
	            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	        }

	        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {file_suffix}));

	        final int returnValue = chooser.showOpenDialog(UI.getMainFrame());

	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	            fileName = chooser.getSelectedFile().getName();
	            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
	            // UI.setDefaultDirectory(directory);
	            textField.setText(fileName);
	        } else {
	            return;
	        }

	    }
	
	    public void readImage() {
	    	FileIO fileIO = null;
	    	// boolean multiFile = false;
	    	// FileInfoBase fileInfo = null;
	    	// System.err.println("fileName = " + fileName);
	    	  try {
	              fileIO = new FileIO();
	              // fileIO.setRawImageInfo(rawInfo);
	              myImage = fileIO.readImage(fileName, directory);
	              // new ViewJFrameImage(myImage);
	          } catch (OutOfMemoryError e) {
	              MipavUtil.displayError("Out of memory!");
	          }

	    	
	    	
	    }
	    
	    public void saveRegisteredImages() {

	    	   String regdir = directory + File.separator + "Registered" + File.separator;
	    	   File fileDirectory = new File(regdir);
				if (!fileDirectory.isDirectory()) {
					fileDirectory.mkdir();
				}
	    	   
	    	   String fileName = regImage_LeftHippocampus.getImageFileName() + ".mgz";
	    	   int fileType = regImage_LeftHippocampus.getType();
	    	   regImage_LeftHippocampus.saveImage(regdir, fileName, fileType, true);
	    	   
	    	  
	    	   fileName = regImage_RightHippocampus.getImageFileName() + ".mgz";
	    	   fileType = regImage_RightHippocampus.getType();
	    	   regImage_RightHippocampus.saveImage(regdir, fileName, fileType, true);
	    		
	    	   fileName = regImage_LeftAmygdala.getImageFileName() + ".mgz";
	    	   fileType = regImage_LeftAmygdala.getType();
	    	   regImage_LeftAmygdala.saveImage(regdir, fileName, fileType, true);
	    	   
	    	   fileName = regImage_RightAmygdala.getImageFileName() + ".mgz";
	    	   fileType = regImage_RightAmygdala.getType();
	    	   regImage_RightAmygdala.saveImage(regdir, fileName, fileType, true);

	    	   fileName = regImage_LeftCaudate.getImageFileName() + ".mgz";
	    	   fileType = regImage_LeftCaudate.getType();
	    	   regImage_LeftCaudate.saveImage(regdir, fileName, fileType, true);
	    	   
	    	   fileName = regImage_RightCaudate.getImageFileName() + ".mgz";
	    	   fileType = regImage_RightCaudate.getType();
	    	   regImage_RightCaudate.saveImage(regdir, fileName, fileType, true);

	    	   fileName = regImage_LeftPutamen.getImageFileName() + ".mgz";
	    	   fileType = regImage_LeftPutamen.getType();
	    	   regImage_LeftPutamen.saveImage(regdir, fileName, fileType, true);
	    	   
	    	   fileName = regImage_RightPutamen.getImageFileName() + ".mgz";
	    	   fileType = regImage_RightPutamen.getType();
	    	   regImage_RightPutamen.saveImage(regdir, fileName, fileType, true);

	    	   fileName = regImage_LeftGlobusPallidus.getImageFileName() + ".mgz";
	    	   fileType = regImage_LeftGlobusPallidus.getType();
	    	   regImage_LeftGlobusPallidus.saveImage(regdir, fileName, fileType, true);
	    	   
	    	   fileName = regImage_RightGlobusPallidus.getImageFileName() + ".mgz";
	    	   fileType = regImage_RightGlobusPallidus.getType();
	    	   regImage_RightGlobusPallidus.saveImage(regdir, fileName, fileType, true);

	    	   fileName = regImage_LeftThalamus.getImageFileName() + ".mgz";
	    	   fileType = regImage_LeftThalamus.getType();
	    	   regImage_LeftThalamus.saveImage(regdir, fileName, fileType, true);
	    	   
	    	   fileName = regImage_RightThalamus.getImageFileName() + ".mgz";
	    	   fileType = regImage_RightThalamus.getType();
	    	   regImage_RightThalamus.saveImage(regdir, fileName, fileType, true);
	    	  
	    }
	    
	    
	    public void saveImages() {
	    	 
	    	String fileDir = directory + File.separator + "Subcortical" + File.separator;
	    	
	    	File fileDirectory = new File(fileDir);
			if (!fileDirectory.isDirectory()) {
				fileDirectory.mkdir();
			}
			
	    	String fileName = image_LeftHippocampus.getImageFileName() + ".mgz";
	    	int fileType = image_LeftHippocampus.getType();
	        image_LeftHippocampus.saveImage(fileDir, fileName, fileType, true);
	    	
	        fileName = image_RightHippocampus.getImageFileName() + ".mgz";
	    	fileType = image_RightHippocampus.getType();
	        image_RightHippocampus.saveImage(fileDir, fileName, fileType, true);
	    	
	        fileName = image_LeftAmygdala.getImageFileName() + ".mgz";
	    	fileType = image_LeftAmygdala.getType();
	    	image_LeftAmygdala.saveImage(fileDir, fileName, fileType, true);
	    	
	    	fileName = image_RightAmygdala.getImageFileName() + ".mgz";
	    	fileType = image_RightAmygdala.getType();
	    	image_RightAmygdala.saveImage(fileDir, fileName, fileType, true);

	    	fileName = image_LeftCaudate.getImageFileName() + ".mgz";
	    	fileType = image_LeftCaudate.getType();
	    	image_LeftCaudate.saveImage(fileDir, fileName, fileType, true);
	    	
	    	fileName = image_RightCaudate.getImageFileName() + ".mgz";
	    	fileType = image_RightCaudate.getType();
	    	image_RightCaudate.saveImage(fileDir, fileName, fileType, true);

	    	fileName = image_LeftPutamen.getImageFileName() + ".mgz";
	    	fileType = image_LeftPutamen.getType();
	    	image_LeftPutamen.saveImage(fileDir, fileName, fileType, true);
	    	
	    	fileName = image_RightPutamen.getImageFileName() + ".mgz";
	    	fileType = image_RightPutamen.getType();
	    	image_RightPutamen.saveImage(fileDir, fileName, fileType, true);

	    	fileName = image_LeftGlobusPallidus.getImageFileName() + ".mgz";
	    	fileType = image_LeftGlobusPallidus.getType();
	    	image_LeftGlobusPallidus.saveImage(fileDir, fileName, fileType, true);
	    	
	    	fileName = image_RightGlobusPallidus.getImageFileName() + ".mgz";
	    	fileType = image_RightGlobusPallidus.getType();
	    	image_RightGlobusPallidus.saveImage(fileDir, fileName, fileType, true);

	    	fileName = image_LeftThalamus.getImageFileName() + ".mgz";
	    	fileType = image_LeftThalamus.getType();
	    	image_LeftThalamus.saveImage(fileDir, fileName, fileType, true);
	    	
	    	fileName = image_RightThalamus.getImageFileName() + ".mgz";
	    	fileType = image_RightThalamus.getType();
	    	image_RightThalamus.saveImage(fileDir, fileName, fileType, true);
	    	
	   }
	
	    
	    public void algorithmPerformed(AlgorithmBase algorithm) {
	    	
	    	int interp2 = 0;
	    	float fillValue = 0.0f;
	    	
	    	if ( algorithm instanceof AlgorithmRegOAR3D ) {
	   
	    		if ( regLeftHippocampus != null && regLeftHippocampus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftHippocampus.getTransform();	
	    			
	    			final int xdimA = image_LeftHippocampus.getExtents()[0];
                    final int ydimA = image_LeftHippocampus.getExtents()[1];
                    final int zdimA = image_LeftHippocampus.getExtents()[2];
                    final float xresA = image_LeftHippocampus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftHippocampus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftHippocampus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftHippocampus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftHippocampus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftHippocampus = transform.getTransformedImage();
                    transform.finalize();

                    regImage_LeftHippocampus.calcMinMax();
                    regImage_LeftHippocampus.setImageName(name);
                    regImage_LeftHippocampus.setType(myImage.getType());

                    /*
                    if (regImage_LeftHippocampus != null) {
                        
                        try {
                            new ViewJFrameImage(regImage_LeftHippocampus, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regLeftHippocampus != null) {
                    	regLeftHippocampus.disposeLocal();
                    	regLeftHippocampus = null;
                    }
                    System.gc();
	    			
	    		}
	    		else if ( regRightHippocampus != null && regRightHippocampus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightHippocampus.getTransform();	
	    			
	    			final int xdimA = image_RightHippocampus.getExtents()[0];
                    final int ydimA = image_RightHippocampus.getExtents()[1];
                    final int zdimA = image_RightHippocampus.getExtents()[2];
                    final float xresA = image_RightHippocampus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightHippocampus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightHippocampus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightHippocampus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightHippocampus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightHippocampus = transform.getTransformedImage();
                    transform.finalize();

                    regImage_RightHippocampus.calcMinMax();
                    regImage_RightHippocampus.setImageName(name);
                    regImage_RightHippocampus.setType(myImage.getType());

                    /*
                    if (regImage_RightHippocampus != null) {

                        try {
                            new ViewJFrameImage(regImage_RightHippocampus, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regRightHippocampus != null) {
                    	regRightHippocampus.disposeLocal();
                    	regRightHippocampus = null;
                    }
                    System.gc();
	    		}
	    		else if ( regLeftAmygdala != null && regLeftAmygdala.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftAmygdala.getTransform();	
	    			
	    			final int xdimA = image_LeftAmygdala.getExtents()[0];
                    final int ydimA = image_LeftAmygdala.getExtents()[1];
                    final int zdimA = image_LeftAmygdala.getExtents()[2];
                    final float xresA = image_LeftAmygdala.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftAmygdala.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftAmygdala.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftAmygdala.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftAmygdala, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftAmygdala = transform.getTransformedImage();
                    transform.finalize();

                    regImage_LeftAmygdala.calcMinMax();
                    regImage_LeftAmygdala.setImageName(name);
                    regImage_LeftAmygdala.setType(myImage.getType());
                    
                    /*
                    if (regImage_LeftAmygdala != null) {

                        try {
                            new ViewJFrameImage(regImage_LeftAmygdala, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regLeftAmygdala != null) {
                    	regLeftAmygdala.disposeLocal();
                    	regLeftAmygdala = null;
                    }
                    System.gc();
	    		}
	    		else if ( regRightAmygdala != null && regRightAmygdala.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightAmygdala.getTransform();	
	    			
	    			final int xdimA = image_RightAmygdala.getExtents()[0];
                    final int ydimA = image_RightAmygdala.getExtents()[1];
                    final int zdimA = image_RightAmygdala.getExtents()[2];
                    final float xresA = image_RightAmygdala.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightAmygdala.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightAmygdala.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightAmygdala.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightAmygdala, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightAmygdala = transform.getTransformedImage();
                    transform.finalize();

                    regImage_RightAmygdala.calcMinMax();
                    regImage_RightAmygdala.setImageName(name);
                    regImage_RightAmygdala.setType(myImage.getType());
                    
                    /*
                    if (regImage_RightAmygdala != null) {

                        try {
                            new ViewJFrameImage(regImage_RightAmygdala, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regRightAmygdala != null) {
                    	regRightAmygdala.disposeLocal();
                    	regRightAmygdala = null;
                    }
                    System.gc();
	    		}
	    		else if ( regLeftCaudate != null && regLeftCaudate.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftCaudate.getTransform();	
	    			
	    			final int xdimA = image_LeftCaudate.getExtents()[0];
                    final int ydimA = image_LeftCaudate.getExtents()[1];
                    final int zdimA = image_LeftCaudate.getExtents()[2];
                    final float xresA = image_LeftCaudate.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftCaudate.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftCaudate.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftCaudate.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftCaudate, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftCaudate = transform.getTransformedImage();
                    transform.finalize();

                    regImage_LeftCaudate.calcMinMax();
                    regImage_LeftCaudate.setImageName(name);
                    regImage_LeftCaudate.setType(myImage.getType());

                    /*
                    if (regImage_LeftCaudate != null) {

                        try {
                            new ViewJFrameImage(regImage_LeftCaudate, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regLeftCaudate != null) {
                    	regLeftCaudate.disposeLocal();
                    	regLeftCaudate = null;
                    }
                    System.gc();
	    		}
	    		else if ( regRightCaudate != null && regRightCaudate.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightCaudate.getTransform();	
	    			
	    			final int xdimA = image_RightCaudate.getExtents()[0];
                    final int ydimA = image_RightCaudate.getExtents()[1];
                    final int zdimA = image_RightCaudate.getExtents()[2];
                    final float xresA = image_RightCaudate.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightCaudate.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightCaudate.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightCaudate.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightCaudate, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightCaudate = transform.getTransformedImage();
                                        
                    transform.finalize();

                    regImage_RightCaudate.calcMinMax();
                    regImage_RightCaudate.setImageName(name);
                    regImage_RightCaudate.setType(myImage.getType());

                    /*
                    if (regImage_RightCaudate != null) {

                        try {
                            new ViewJFrameImage(regImage_RightCaudate, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regRightCaudate != null) {
                    	regRightCaudate.disposeLocal();
                    	regRightCaudate = null;
                    }
                    System.gc();
	    		}
	    		else if ( regLeftPutamen != null && regLeftPutamen.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftPutamen.getTransform();	
	    			
	    			final int xdimA = image_LeftPutamen.getExtents()[0];
                    final int ydimA = image_LeftPutamen.getExtents()[1];
                    final int zdimA = image_LeftPutamen.getExtents()[2];
                    final float xresA = image_LeftPutamen.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftPutamen.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftPutamen.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftPutamen.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftPutamen, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftPutamen = transform.getTransformedImage();
                    transform.finalize();

                    regImage_LeftPutamen.calcMinMax();
                    regImage_LeftPutamen.setImageName(name);
                    regImage_LeftPutamen.setType(myImage.getType());

                    /*
                    if (regImage_LeftPutamen != null) {

                        try {
                            new ViewJFrameImage(regImage_LeftPutamen, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regLeftPutamen != null) {
                    	regLeftPutamen.disposeLocal();
                    	regLeftPutamen = null;
                    }
                    System.gc();
	    		}
	    		else if ( regRightPutamen != null && regRightPutamen.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightPutamen.getTransform();	
	    			
	    			final int xdimA = image_RightPutamen.getExtents()[0];
                    final int ydimA = image_RightPutamen.getExtents()[1];
                    final int zdimA = image_RightPutamen.getExtents()[2];
                    final float xresA = image_RightPutamen.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightPutamen.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightPutamen.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightPutamen.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightPutamen, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightPutamen = transform.getTransformedImage();
                    transform.finalize();

                    regImage_RightPutamen.calcMinMax();
                    regImage_RightPutamen.setImageName(name);
                    regImage_RightPutamen.setType(myImage.getType());
                    
                    /*
                    if (regImage_RightPutamen != null) {

                        try {
                            new ViewJFrameImage(regImage_RightPutamen, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regRightPutamen != null) {
                    	regRightPutamen.disposeLocal();
                    	regRightPutamen = null;
                    }
                    System.gc();
	    		}
	    		else if ( regLeftGlobusPallidus != null && regLeftGlobusPallidus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftGlobusPallidus.getTransform();	
	    			
	    			final int xdimA = image_LeftGlobusPallidus.getExtents()[0];
                    final int ydimA = image_LeftGlobusPallidus.getExtents()[1];
                    final int zdimA = image_LeftGlobusPallidus.getExtents()[2];
                    final float xresA = image_LeftGlobusPallidus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftGlobusPallidus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftGlobusPallidus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftGlobusPallidus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftGlobusPallidus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftGlobusPallidus = transform.getTransformedImage();
                    transform.finalize();

                    regImage_LeftGlobusPallidus.calcMinMax();
                    regImage_LeftGlobusPallidus.setImageName(name);
                    regImage_LeftGlobusPallidus.setType(myImage.getType());
                    
                    /*
                    if (regImage_LeftGlobusPallidus != null) {

                        try {
                            new ViewJFrameImage(regImage_LeftGlobusPallidus, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regLeftGlobusPallidus != null) {
                    	regLeftGlobusPallidus.disposeLocal();
                    	regLeftGlobusPallidus = null;
                    }
                    System.gc();
	    			
	    		}
	    		else if ( regRightGlobusPallidus != null && regRightGlobusPallidus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightGlobusPallidus.getTransform();	
	    			
	    			final int xdimA = image_RightGlobusPallidus.getExtents()[0];
                    final int ydimA = image_RightGlobusPallidus.getExtents()[1];
                    final int zdimA = image_RightGlobusPallidus.getExtents()[2];
                    final float xresA = image_RightGlobusPallidus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightGlobusPallidus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightGlobusPallidus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightGlobusPallidus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightGlobusPallidus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightGlobusPallidus = transform.getTransformedImage();
                    transform.finalize();

                    regImage_RightGlobusPallidus.calcMinMax();
                    regImage_RightGlobusPallidus.setImageName(name);
                    regImage_RightGlobusPallidus.setType(myImage.getType());
                    
                    /*
                    if (regImage_RightGlobusPallidus != null) {

                        try {
                            new ViewJFrameImage(regImage_RightGlobusPallidus, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regRightGlobusPallidus != null) {
                    	regRightGlobusPallidus.disposeLocal();
                    	regRightGlobusPallidus = null;
                    }
                    System.gc();
	    			
	    		}
	    		else if ( regLeftThalamus != null && regLeftThalamus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftThalamus.getTransform();	
	    			
	    			final int xdimA = image_LeftThalamus.getExtents()[0];
                    final int ydimA = image_LeftThalamus.getExtents()[1];
                    final int zdimA = image_LeftThalamus.getExtents()[2];
                    final float xresA = image_LeftThalamus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftThalamus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftThalamus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftThalamus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftThalamus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftThalamus = transform.getTransformedImage();
                    transform.finalize();

                    regImage_LeftThalamus.calcMinMax();
                    regImage_LeftThalamus.setImageName(name);
                    regImage_LeftThalamus.setType(myImage.getType());

                    /*
                    if (regImage_LeftThalamus != null) {

                        try {
                            new ViewJFrameImage(regImage_LeftThalamus, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regLeftThalamus != null) {
                    	regLeftThalamus.disposeLocal();
                    	regLeftThalamus = null;
                    }
                    System.gc();
	    			
	    		}
	    		else if ( regRightThalamus != null && regRightThalamus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightThalamus.getTransform();	
	    			
	    			final int xdimA = image_RightThalamus.getExtents()[0];
                    final int ydimA = image_RightThalamus.getExtents()[1];
                    final int zdimA = image_RightThalamus.getExtents()[2];
                    final float xresA = image_RightThalamus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightThalamus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightThalamus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightThalamus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightThalamus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightThalamus = transform.getTransformedImage();
                    transform.finalize();

                    regImage_RightThalamus.calcMinMax();
                    regImage_RightThalamus.setImageName(name);
                    regImage_RightThalamus.setType(myImage.getType());
                    
                    /*
                    if (regImage_RightThalamus != null) {

                        try {
                            new ViewJFrameImage(regImage_RightThalamus, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                    */
                    
                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                    
                    if (regRightThalamus != null) {
                    	regRightThalamus.disposeLocal();
                    	regRightThalamus = null;
                    }
                    System.gc();
	    		}
	    		
	    		
	    	}
	    	else if (algorithm instanceof AlgorithmThresholdDual) {
	        	
	    		// image threshold of color label
	            if (algorThreshold_LeftHippocampus != null && (algorThreshold_LeftHippocampus.isCompleted() == true)) {
	            	image_LeftHippocampus.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftHippocampus, image_LeftHippocampus);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftHippocampus, image_LeftHippocampus);
	                }

	                try {
	                	image_LeftHippocampus.calcMinMax();
	                	image_LeftHippocampus.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftHippocampus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftHippocampus.finalize();
		            algorThreshold_LeftHippocampus = null;
		            System.gc();
	            }
	            else if (algorThreshold_RightHippocampus != null && (algorThreshold_RightHippocampus.isCompleted() == true)) {
	            	image_RightHippocampus.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightHippocampus, image_RightHippocampus);
	                } else {
	                	parentDialog.updateFileInfo(image_RightHippocampus, image_RightHippocampus);
	                }

	                try {
	                	image_RightHippocampus.calcMinMax();
	                	image_RightHippocampus.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightHippocampus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightHippocampus.finalize();
		            algorThreshold_RightHippocampus = null;
		            System.gc();
	            }
	            else if (algorThreshold_LeftAmygdala != null && (algorThreshold_LeftAmygdala.isCompleted() == true)) {
	            	image_LeftAmygdala.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftAmygdala, image_LeftAmygdala);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftAmygdala, image_LeftAmygdala);
	                }

	                try {
	                	image_LeftAmygdala.calcMinMax();
	                	image_LeftAmygdala.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftAmygdala);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftAmygdala.finalize();
		            algorThreshold_LeftAmygdala = null;
		            System.gc();
	            }
	            else if (algorThreshold_RightAmygdala != null && (algorThreshold_RightAmygdala.isCompleted() == true)) {
	            	image_RightAmygdala.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightAmygdala, image_RightAmygdala);
	                } else {
	                	parentDialog.updateFileInfo(image_RightAmygdala, image_RightAmygdala);
	                }

	                try {
	                	image_RightAmygdala.calcMinMax();
	                	image_RightAmygdala.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightAmygdala);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightAmygdala.finalize();
		            algorThreshold_RightAmygdala = null;
		            System.gc();
	            }
	            else if (algorThreshold_LeftCaudate != null && (algorThreshold_LeftCaudate.isCompleted() == true)) {
	            	image_LeftCaudate.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftCaudate, image_LeftCaudate);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftCaudate, image_LeftCaudate);
	                }

	                try {
	                	image_LeftCaudate.calcMinMax();
	                	image_LeftCaudate.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftCaudate);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftCaudate.finalize();
		            algorThreshold_LeftCaudate = null;
		            System.gc();
	            }
	            else if (algorThreshold_RightCaudate != null && (algorThreshold_RightCaudate.isCompleted() == true)) {
	            	image_RightCaudate.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightCaudate, image_RightCaudate);
	                } else {
	                	parentDialog.updateFileInfo(image_RightCaudate, image_RightCaudate);
	                }

	                try {
	                	image_RightCaudate.calcMinMax();
	                	image_RightCaudate.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightCaudate);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightCaudate.finalize();
		            algorThreshold_RightCaudate = null;
		            System.gc();
	            }
	            else if (algorThreshold_LeftPutamen != null && (algorThreshold_LeftPutamen.isCompleted() == true)) {
	            	image_LeftPutamen.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftPutamen, image_LeftPutamen);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftPutamen, image_LeftPutamen);
	                }

	                try {
	                	image_LeftPutamen.calcMinMax();
	                	image_LeftPutamen.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftPutamen);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftPutamen.finalize();
		            algorThreshold_LeftPutamen = null;
		            System.gc();
	            }
	            else if (algorThreshold_RightPutamen != null && (algorThreshold_RightPutamen.isCompleted() == true)) {
	            	image_RightPutamen.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightPutamen, image_RightPutamen);
	                } else {
	                	parentDialog.updateFileInfo(image_RightPutamen, image_RightPutamen);
	                }

	                try {
	                	image_RightPutamen.calcMinMax();
	                	image_RightPutamen.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightPutamen);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightPutamen.finalize();
		            algorThreshold_RightPutamen = null;
		            System.gc();
	            }
	            else if (algorThreshold_LeftGlobusPallidus != null && (algorThreshold_LeftGlobusPallidus.isCompleted() == true)) {
	            	image_LeftGlobusPallidus.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftGlobusPallidus, image_LeftGlobusPallidus);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftGlobusPallidus, image_LeftGlobusPallidus);
	                }

	                try {
	                	image_LeftGlobusPallidus.calcMinMax();
	                	image_LeftGlobusPallidus.notifyImageDisplayListeners(null, true);
	                	//   new ViewJFrameImage(image_LeftGlobusPallidus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftGlobusPallidus.finalize();
		            algorThreshold_LeftGlobusPallidus = null;
		            System.gc();
	            }
	            else if (algorThreshold_RightGlobusPallidus != null && (algorThreshold_RightGlobusPallidus.isCompleted() == true)) {
	            	image_RightGlobusPallidus.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightGlobusPallidus, image_RightGlobusPallidus);
	                } else {
	                	parentDialog.updateFileInfo(image_RightGlobusPallidus, image_RightGlobusPallidus);
	                }

	                try {
	                	image_RightGlobusPallidus.calcMinMax();
	                	image_RightGlobusPallidus.notifyImageDisplayListeners(null, true);
	                	//   new ViewJFrameImage(image_RightGlobusPallidus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightGlobusPallidus.finalize();
		            algorThreshold_RightGlobusPallidus = null;
		            System.gc();
	            }
	            else if (algorThreshold_LeftThalamus != null && (algorThreshold_LeftThalamus.isCompleted() == true)) {
	            	image_LeftThalamus.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftThalamus, image_LeftThalamus);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftThalamus, image_LeftThalamus);
	                }

	                try {
	                	image_LeftThalamus.calcMinMax();
	                	image_LeftThalamus.notifyImageDisplayListeners(null, true);
	                	//   new ViewJFrameImage(image_LeftThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftThalamus.finalize();
		            algorThreshold_LeftThalamus = null;
		            System.gc();
	            }
	            else if (algorThreshold_RightThalamus != null && (algorThreshold_RightThalamus.isCompleted() == true)) {
	            	image_RightThalamus.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightThalamus, image_RightThalamus);
	                } else {
	                	parentDialog.updateFileInfo(image_RightThalamus, image_RightThalamus);
	                }

	                try {
	                	image_RightThalamus.calcMinMax();
	                	image_RightThalamus.notifyImageDisplayListeners(null, true);
	                	//    new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightThalamus.finalize();
		            algorThreshold_RightThalamus = null;
		            System.gc();
	            }
	            
	            // binary threshold on origin and registered images. 
	            if (algorThreshold_LeftHippocampus_binary != null && (algorThreshold_LeftHippocampus_binary.isCompleted() == true)) {
	            	image_LeftHippocampus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftHippocampus_binary, image_LeftHippocampus_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftHippocampus_binary, image_LeftHippocampus_binary);
	                }

	                try {
	                	image_LeftHippocampus_binary.calcMinMax();
	                	image_LeftHippocampus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftHippocampus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftHippocampus_binary.finalize();
		            algorThreshold_LeftHippocampus_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightHippocampus_binary != null && (algorThreshold_RightHippocampus_binary.isCompleted() == true)) {
	            	image_RightHippocampus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightHippocampus_binary, image_RightHippocampus_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_RightHippocampus_binary, image_RightHippocampus_binary);
	                }

	                try {
	                	image_RightHippocampus_binary.calcMinMax();
	                	image_RightHippocampus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightHippocampus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightHippocampus_binary.finalize();
		            algorThreshold_RightHippocampus_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftAmygdala_binary != null && (algorThreshold_LeftAmygdala_binary.isCompleted() == true)) {
	            	image_LeftAmygdala_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftAmygdala_binary, image_LeftAmygdala_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftAmygdala_binary, image_LeftAmygdala_binary);
	                }

	                try {
	                	image_LeftAmygdala_binary.calcMinMax();
	                	image_LeftAmygdala_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftAmygdala);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftAmygdala_binary.finalize();
		            algorThreshold_LeftAmygdala_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightAmygdala_binary != null && (algorThreshold_RightAmygdala_binary.isCompleted() == true)) {
	            	image_RightAmygdala_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightAmygdala_binary, image_RightAmygdala_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_RightAmygdala_binary, image_RightAmygdala_binary);
	                }

	                try {
	                	image_RightAmygdala_binary.calcMinMax();
	                	image_RightAmygdala_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightAmygdala);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightAmygdala_binary.finalize();
		            algorThreshold_RightAmygdala_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftCaudate_binary != null && (algorThreshold_LeftCaudate_binary.isCompleted() == true)) {
	            	image_LeftCaudate_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftCaudate_binary, image_LeftCaudate_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftCaudate_binary, image_LeftCaudate_binary);
	                }

	                try {
	                	image_LeftCaudate_binary.calcMinMax();
	                	image_LeftCaudate_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftCaudate);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftCaudate_binary.finalize();
		            algorThreshold_LeftCaudate_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightCaudate_binary != null && (algorThreshold_RightCaudate_binary.isCompleted() == true)) {
	            	image_RightCaudate_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightCaudate_binary, image_RightCaudate_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_RightCaudate_binary, image_RightCaudate_binary);
	                }

	                try {
	                	image_RightCaudate_binary.calcMinMax();
	                	image_RightCaudate_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightCaudate);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightCaudate_binary.finalize();
		            algorThreshold_RightCaudate_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftPutamen_binary != null && (algorThreshold_LeftPutamen_binary.isCompleted() == true)) {
	            	image_LeftPutamen_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftPutamen_binary, image_LeftPutamen_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftPutamen_binary, image_LeftPutamen_binary);
	                }

	                try {
	                	image_LeftPutamen_binary.calcMinMax();
	                	image_LeftPutamen_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftPutamen);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftPutamen_binary.finalize();
		            algorThreshold_LeftPutamen_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightPutamen_binary != null && (algorThreshold_RightPutamen_binary.isCompleted() == true)) {
	            	image_RightPutamen_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightPutamen_binary, image_RightPutamen_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_RightPutamen_binary, image_RightPutamen_binary);
	                }

	                try {
	                	image_RightPutamen_binary.calcMinMax();
	                	image_RightPutamen_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightPutamen);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightPutamen_binary.finalize();
		            algorThreshold_RightPutamen_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftGlobusPallidus_binary != null && (algorThreshold_LeftGlobusPallidus_binary.isCompleted() == true)) {
	            	image_LeftGlobusPallidus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftGlobusPallidus_binary, image_LeftGlobusPallidus_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftGlobusPallidus_binary, image_LeftGlobusPallidus_binary);
	                }

	                try {
	                	image_LeftGlobusPallidus_binary.calcMinMax();
	                	image_LeftGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftGlobusPallidus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftGlobusPallidus_binary.finalize();
		            algorThreshold_LeftGlobusPallidus_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightGlobusPallidus_binary != null && (algorThreshold_RightGlobusPallidus_binary.isCompleted() == true)) {
	            	image_RightGlobusPallidus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightGlobusPallidus_binary, image_RightGlobusPallidus_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_RightGlobusPallidus_binary, image_RightGlobusPallidus_binary);
	                }

	                try {
	                	image_RightGlobusPallidus_binary.calcMinMax();
	                	image_RightGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightGlobusPallidus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightGlobusPallidus_binary.finalize();
		            algorThreshold_RightGlobusPallidus_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftThalamus_binary != null && (algorThreshold_LeftThalamus_binary.isCompleted() == true)) {
	            	image_LeftThalamus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_LeftThalamus_binary, image_LeftThalamus_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_LeftThalamus_binary, image_LeftThalamus_binary);
	                }

	                try {
	                	image_LeftThalamus_binary.calcMinMax();
	                	image_LeftThalamus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_LeftThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftThalamus_binary.finalize();
		            algorThreshold_LeftThalamus_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightThalamus_binary != null && (algorThreshold_RightThalamus_binary.isCompleted() == true)) {
	            	image_RightThalamus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(image_RightThalamus_binary, image_RightThalamus_binary);
	                } else {
	                	parentDialog.updateFileInfo(image_RightThalamus_binary, image_RightThalamus_binary);
	                }

	                try {
	                	image_RightThalamus_binary.calcMinMax();
	                	image_RightThalamus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightThalamus_binary.finalize();
		            algorThreshold_RightThalamus_binary = null;
		            System.gc();
	            } 
	            
	            // registered image binary threshold
	            if (algorThreshold_LeftHippocampus_reg_binary != null && (algorThreshold_LeftHippocampus_reg_binary.isCompleted() == true)) {
	            	regImage_LeftHippocampus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_LeftHippocampus_binary, regImage_LeftHippocampus_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_LeftHippocampus_binary, regImage_LeftHippocampus_binary);
	                }

	                try {
	                	regImage_LeftHippocampus_binary.calcMinMax();
	                	regImage_LeftHippocampus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftHippocampus_reg_binary.finalize();
	                algorThreshold_LeftHippocampus_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightHippocampus_reg_binary != null && (algorThreshold_RightHippocampus_reg_binary.isCompleted() == true)) {
	            	regImage_RightHippocampus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_RightHippocampus_binary, regImage_RightHippocampus_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_RightHippocampus_binary, regImage_RightHippocampus_binary);
	                }

	                try {
	                	regImage_RightHippocampus_binary.calcMinMax();
	                	regImage_RightHippocampus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightHippocampus_reg_binary.finalize();
	                algorThreshold_RightHippocampus_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftAmygdala_reg_binary != null && (algorThreshold_LeftAmygdala_reg_binary.isCompleted() == true)) {
	            	regImage_LeftAmygdala_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_LeftAmygdala_binary, regImage_LeftAmygdala_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_LeftAmygdala_binary, regImage_LeftAmygdala_binary);
	                }

	                try {
	                	regImage_LeftAmygdala_binary.calcMinMax();
	                	regImage_LeftAmygdala_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftAmygdala_reg_binary.finalize();
	                algorThreshold_LeftAmygdala_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightAmygdala_reg_binary != null && (algorThreshold_RightAmygdala_reg_binary.isCompleted() == true)) {
	            	regImage_RightAmygdala_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_RightAmygdala_binary, regImage_RightAmygdala_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_RightAmygdala_binary, regImage_RightAmygdala_binary);
	                }

	                try {
	                	regImage_RightAmygdala_binary.calcMinMax();
	                	regImage_RightAmygdala_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightAmygdala_reg_binary.finalize();
	                algorThreshold_RightAmygdala_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftCaudate_reg_binary != null && (algorThreshold_LeftCaudate_reg_binary.isCompleted() == true)) {
	            	regImage_LeftCaudate_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_LeftCaudate_binary, regImage_LeftCaudate_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_LeftCaudate_binary, regImage_LeftCaudate_binary);
	                }

	                try {
	                	regImage_LeftCaudate_binary.calcMinMax();
	                	regImage_LeftCaudate_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftCaudate_reg_binary.finalize();
	                algorThreshold_LeftCaudate_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightCaudate_reg_binary != null && (algorThreshold_RightCaudate_reg_binary.isCompleted() == true)) {
	            	regImage_RightCaudate_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_RightCaudate_binary, regImage_RightCaudate_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_RightCaudate_binary, regImage_RightCaudate_binary);
	                }

	                try {
	                	regImage_RightCaudate_binary.calcMinMax();
	                	regImage_RightCaudate_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightCaudate_reg_binary.finalize();
	                algorThreshold_RightCaudate_reg_binary = null;
		            System.gc();
	            }  else if (algorThreshold_LeftPutamen_reg_binary != null && (algorThreshold_LeftPutamen_reg_binary.isCompleted() == true)) {
	            	regImage_LeftPutamen_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_LeftPutamen_binary, regImage_LeftPutamen_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_LeftPutamen_binary, regImage_LeftPutamen_binary);
	                }

	                try {
	                	regImage_LeftPutamen_binary.calcMinMax();
	                	regImage_LeftPutamen_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftPutamen_reg_binary.finalize();
	                algorThreshold_LeftPutamen_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightPutamen_reg_binary != null && (algorThreshold_RightPutamen_reg_binary.isCompleted() == true)) {
	            	regImage_RightPutamen_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_RightPutamen_binary, regImage_RightPutamen_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_RightPutamen_binary, regImage_RightPutamen_binary);
	                }

	                try {
	                	regImage_RightPutamen_binary.calcMinMax();
	                	regImage_RightPutamen_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightPutamen_reg_binary.finalize();
	                algorThreshold_RightPutamen_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftGlobusPallidus_reg_binary != null && (algorThreshold_LeftGlobusPallidus_reg_binary.isCompleted() == true)) {
	            	regImage_LeftGlobusPallidus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_LeftGlobusPallidus_binary, regImage_LeftGlobusPallidus_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_LeftGlobusPallidus_binary, regImage_LeftGlobusPallidus_binary);
	                }

	                try {
	                	regImage_LeftGlobusPallidus_binary.calcMinMax();
	                	regImage_LeftGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftGlobusPallidus_reg_binary.finalize();
	                algorThreshold_LeftGlobusPallidus_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_RightGlobusPallidus_reg_binary != null && (algorThreshold_RightGlobusPallidus_reg_binary.isCompleted() == true)) {
	            	regImage_RightGlobusPallidus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_RightGlobusPallidus_binary, regImage_RightGlobusPallidus_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_RightGlobusPallidus_binary, regImage_RightGlobusPallidus_binary);
	                }

	                try {
	                	regImage_RightGlobusPallidus_binary.calcMinMax();
	                	regImage_RightGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightGlobusPallidus_reg_binary.finalize();
	                algorThreshold_RightGlobusPallidus_reg_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftThalamus_reg_binary != null && (algorThreshold_LeftThalamus_reg_binary.isCompleted() == true)) {
	            	regImage_LeftThalamus_binary.clearMask();
	                // The algorithm has completed and produced a new image to be
	                // displayed.
	                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
	                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
	                    parentDialog.updateFileInfoOtherModality(regImage_LeftThalamus_binary, regImage_LeftThalamus_binary);
	                } else {
	                	parentDialog.updateFileInfo(regImage_LeftThalamus_binary, regImage_LeftThalamus_binary);
	                }

	                try {
	                	regImage_LeftThalamus_binary.calcMinMax();
	                	regImage_LeftThalamus_binary.notifyImageDisplayListeners(null, true);
	                	// new ViewJFrameImage(image_RightThalamus);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftThalamus_reg_binary.finalize();
	                algorThreshold_LeftThalamus_reg_binary = null;
		            System.gc();
	            } 
	            
	            
	        }

	    }

	/**
	 * Threshold the image origin and registered image to binary image for comparison. 
	 */
	public void originImageThresholdBinary() {

		float[] thresholds = new float[2];
		float fillValue = 0;
		boolean isInverse = false;
		boolean regionFlag = true;
		int outputType = 1;

		try {

			float thres1 = 1.0f; // lowerThres;
			float thres2 = LeftHippocampus; // upperThres;

			thresholds[0] = thres1;
			thresholds[1] = thres2;

			// Left Hippocampus ***
			image_LeftHippocampus_binary = (ModelImage) image_LeftHippocampus.clone();
			image_LeftHippocampus_binary.setType(ModelStorageBase.BOOLEAN);
			image_LeftHippocampus_binary.setImageName(myImage.getImageName()
					+ "_leftHippocampus_binary");
			algorThreshold_LeftHippocampus_binary = new AlgorithmThresholdDual(
					image_LeftHippocampus_binary, image_LeftHippocampus,
					thresholds, fillValue, outputType, regionFlag, isInverse);
			algorThreshold_LeftHippocampus_binary.addListener(parentDialog);
			algorThreshold_LeftHippocampus_binary.run();

		
			//   Right Hippocampus ****
            thres1 = 1.0f;  			// lowerThres;
	        thres2 = RightHippocampus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_RightHippocampus_binary = (ModelImage)image_RightHippocampus.clone();
        	image_RightHippocampus_binary.setType(ModelStorageBase.BOOLEAN);
        	image_RightHippocampus_binary.setImageName(myImage.getImageName() + "_rightHippocampus_binary");
        	algorThreshold_RightHippocampus_binary = new AlgorithmThresholdDual(image_RightHippocampus_binary, image_RightHippocampus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightHippocampus_binary.addListener(parentDialog);
            algorThreshold_RightHippocampus_binary.run();
            
            // Left Amygdala  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftAmygdala;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_LeftAmygdala_binary = (ModelImage)image_LeftAmygdala.clone();
        	image_LeftAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
        	image_LeftAmygdala_binary.setImageName(myImage.getImageName() + "_LeftAmygdala_binary");
            algorThreshold_LeftAmygdala_binary = new AlgorithmThresholdDual(image_LeftAmygdala_binary, image_LeftAmygdala, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftAmygdala_binary.addListener(parentDialog);
            algorThreshold_LeftAmygdala_binary.run();
            
            
            // Right Amygdala  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightAmygdala;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_RightAmygdala_binary = (ModelImage)image_RightAmygdala.clone();
        	image_RightAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
        	image_RightAmygdala_binary.setImageName(myImage.getImageName() + "_RightAmygdala_binary");
            algorThreshold_RightAmygdala_binary = new AlgorithmThresholdDual(image_RightAmygdala_binary, image_RightAmygdala, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightAmygdala_binary.addListener(parentDialog);
            algorThreshold_RightAmygdala_binary.run();
            
            // Left Caudate  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftCaudate;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_LeftCaudate_binary = (ModelImage)image_LeftCaudate.clone();
        	image_LeftCaudate_binary.setType(ModelStorageBase.BOOLEAN);
        	image_LeftCaudate_binary.setImageName(myImage.getImageName() + "_LeftCaudate_binary");
            algorThreshold_LeftCaudate_binary = new AlgorithmThresholdDual(image_LeftCaudate_binary, image_LeftCaudate, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftCaudate_binary.addListener(parentDialog);
            algorThreshold_LeftCaudate_binary.run();
            
            // RightCaudate ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightCaudate;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_RightCaudate_binary = (ModelImage)image_RightCaudate.clone();
        	image_RightCaudate_binary.setType(ModelStorageBase.BOOLEAN);
        	image_RightCaudate_binary.setImageName(myImage.getImageName() + "_RightCaudate_binary");
            algorThreshold_RightCaudate_binary = new AlgorithmThresholdDual(image_RightCaudate_binary, image_RightCaudate, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightCaudate_binary.addListener(parentDialog);
            algorThreshold_RightCaudate_binary.run();

            //  Left Putamen ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftPutamen;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_LeftPutamen_binary = (ModelImage)image_LeftPutamen.clone();
        	image_LeftPutamen_binary.setType(ModelStorageBase.BOOLEAN);
        	image_LeftPutamen_binary.setImageName(myImage.getImageName() + "_LeftPutamen_binary");
            algorThreshold_LeftPutamen_binary = new AlgorithmThresholdDual(image_LeftPutamen_binary, image_LeftPutamen, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftPutamen_binary.addListener(parentDialog);
            algorThreshold_LeftPutamen_binary.run();
            
            
            //  Right Putamen  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightPutamen;  // upperThres;
	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_RightPutamen_binary = (ModelImage)image_RightPutamen.clone();
        	image_RightPutamen_binary.setType(ModelStorageBase.BOOLEAN);
        	image_RightPutamen_binary.setImageName(myImage.getImageName() + "_RightPutamen_binary");   
            algorThreshold_RightPutamen_binary = new AlgorithmThresholdDual(image_RightPutamen_binary, image_RightPutamen, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightPutamen_binary.addListener(parentDialog);
            algorThreshold_RightPutamen_binary.run();

            
            //  Left GlobusPallidus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftGlobusPallidus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_LeftGlobusPallidus_binary = (ModelImage)image_LeftGlobusPallidus.clone();
        	image_LeftGlobusPallidus_binary.setType(ModelStorageBase.BOOLEAN);
        	image_LeftGlobusPallidus_binary.setImageName(myImage.getImageName() + "_LeftGlobusPallidus_binary");
        	algorThreshold_LeftGlobusPallidus_binary = new AlgorithmThresholdDual(image_LeftGlobusPallidus_binary, image_LeftGlobusPallidus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftGlobusPallidus_binary.addListener(parentDialog);
            algorThreshold_LeftGlobusPallidus_binary.run();

            
            // Right GlobusPallidus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightGlobusPallidus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_RightGlobusPallidus_binary = (ModelImage)image_RightGlobusPallidus.clone();
        	image_RightGlobusPallidus_binary.setType(ModelStorageBase.BOOLEAN);
        	image_RightGlobusPallidus_binary.setImageName(myImage.getImageName() + "_RightGlobusPallidus_binary");
        	algorThreshold_RightGlobusPallidus_binary = new AlgorithmThresholdDual(image_RightGlobusPallidus_binary, image_RightGlobusPallidus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightGlobusPallidus_binary.addListener(parentDialog);
            algorThreshold_RightGlobusPallidus_binary.run();
            
            //   Left Thalamus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftThalamus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_LeftThalamus_binary = (ModelImage)image_LeftThalamus.clone();
        	image_LeftThalamus_binary.setType(ModelStorageBase.BOOLEAN);
        	image_LeftThalamus_binary.setImageName(myImage.getImageName() + "_LeftThalamus_binary");	
            algorThreshold_LeftThalamus_binary = new AlgorithmThresholdDual(image_LeftThalamus_binary, image_LeftThalamus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftThalamus_binary.addListener(parentDialog);
            algorThreshold_LeftThalamus_binary.run();
            
            
            //   Right  Thalamus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightThalamus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            image_RightThalamus_binary = (ModelImage)image_RightThalamus.clone();
        	image_RightThalamus_binary.setType(ModelStorageBase.BOOLEAN);
        	image_RightThalamus_binary.setImageName(myImage.getImageName() + "_RightThalamus_binar");
        	algorThreshold_RightThalamus_binary = new AlgorithmThresholdDual(image_RightThalamus_binary, image_RightThalamus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightThalamus_binary.addListener(parentDialog);
            algorThreshold_RightThalamus_binary.run();
			
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog threshold: unable to allocate enough memory");

			return;
		}

	}
	
		    
	public void registeredImageThresholdBinary() {

		float[] thresholds = new float[2];
		float fillValue = 0;
		boolean isInverse = false;
		boolean regionFlag = true;
		int outputType = 1;

		try {

			float thres1 = 1.0f; // lowerThres;
			float thres2 = LeftHippocampus; // upperThres;

			thresholds[0] = thres1;
			thresholds[1] = thres2;
			
			/// Left Hippocampus ***
			regImage_LeftHippocampus_binary = (ModelImage) regImage_LeftHippocampus.clone();
			regImage_LeftHippocampus_binary.setType(ModelStorageBase.BOOLEAN);
			regImage_LeftHippocampus_binary.setImageName(regImage_LeftHippocampus.getImageName()
					+ "_binary");
			algorThreshold_LeftHippocampus_reg_binary = new AlgorithmThresholdDual(
					regImage_LeftHippocampus_binary, regImage_LeftHippocampus,
					thresholds, fillValue, outputType, regionFlag, isInverse);
			algorThreshold_LeftHippocampus_reg_binary.addListener(parentDialog);
			algorThreshold_LeftHippocampus_reg_binary.run();
			
			
			///   Right Hippocampus ****
            thres1 = 1.0f;  			// lowerThres;
	        thres2 = RightHippocampus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_RightHippocampus_binary = (ModelImage)regImage_RightHippocampus.clone();
            regImage_RightHippocampus_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_RightHippocampus_binary.setImageName(regImage_RightHippocampus.getImageName() + "_binary");
        	algorThreshold_RightHippocampus_reg_binary = new AlgorithmThresholdDual(regImage_RightHippocampus_binary, regImage_RightHippocampus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightHippocampus_reg_binary.addListener(parentDialog);
            algorThreshold_RightHippocampus_reg_binary.run();
            
            /// Left Amygdala  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftAmygdala;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_LeftAmygdala_binary = (ModelImage)regImage_LeftAmygdala.clone();
            regImage_LeftAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_LeftAmygdala_binary.setImageName(regImage_LeftAmygdala.getImageName() + "_binary");
            algorThreshold_LeftAmygdala_reg_binary = new AlgorithmThresholdDual(regImage_LeftAmygdala_binary, regImage_LeftAmygdala, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftAmygdala_reg_binary.addListener(parentDialog);
            algorThreshold_LeftAmygdala_reg_binary.run();
            
            
            /// Right Amygdala  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightAmygdala;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_RightAmygdala_binary = (ModelImage)regImage_RightAmygdala.clone();
            regImage_RightAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_RightAmygdala_binary.setImageName(regImage_RightAmygdala.getImageName() + "_binary");
            algorThreshold_RightAmygdala_reg_binary = new AlgorithmThresholdDual(regImage_RightAmygdala_binary, regImage_RightAmygdala, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightAmygdala_reg_binary.addListener(parentDialog);
            algorThreshold_RightAmygdala_reg_binary.run();
            
            /// Left Caudate  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftCaudate;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_LeftCaudate_binary = (ModelImage)regImage_LeftCaudate.clone();
            regImage_LeftCaudate_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_LeftCaudate_binary.setImageName(regImage_LeftCaudate.getImageName() + "_binary");
            algorThreshold_LeftCaudate_reg_binary = new AlgorithmThresholdDual(regImage_LeftCaudate_binary, regImage_LeftCaudate, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftCaudate_reg_binary.addListener(parentDialog);
            algorThreshold_LeftCaudate_reg_binary.run();
            
            /// RightCaudate ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightCaudate;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_RightCaudate_binary = (ModelImage)regImage_RightCaudate.clone();
            regImage_RightCaudate_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_RightCaudate_binary.setImageName(regImage_RightCaudate.getImageName() + "_binary");
            algorThreshold_RightCaudate_reg_binary = new AlgorithmThresholdDual(regImage_RightCaudate_binary, regImage_RightCaudate, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightCaudate_reg_binary.addListener(parentDialog);
            algorThreshold_RightCaudate_reg_binary.run();

            ///  Left Putamen ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftPutamen;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_LeftPutamen_binary = (ModelImage)regImage_LeftPutamen.clone();
        	regImage_LeftPutamen_binary.setType(ModelStorageBase.BOOLEAN);
        	regImage_LeftPutamen_binary.setImageName(regImage_LeftPutamen.getImageName() + "_binary");
            algorThreshold_LeftPutamen_reg_binary = new AlgorithmThresholdDual(regImage_LeftPutamen_binary, regImage_LeftPutamen, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftPutamen_reg_binary.addListener(parentDialog);
            algorThreshold_LeftPutamen_reg_binary.run();
            
            
            ///  Right Putamen  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightPutamen;  // upperThres;
	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_RightPutamen_binary = (ModelImage)regImage_RightPutamen.clone();
            regImage_RightPutamen_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_RightPutamen_binary.setImageName(regImage_RightPutamen.getImageName() + "_binary");   
            algorThreshold_RightPutamen_reg_binary = new AlgorithmThresholdDual(regImage_RightPutamen_binary, regImage_RightPutamen, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightPutamen_reg_binary.addListener(parentDialog);
            algorThreshold_RightPutamen_reg_binary.run();

            
            ///  Left GlobusPallidus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftGlobusPallidus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_LeftGlobusPallidus_binary = (ModelImage)regImage_LeftGlobusPallidus.clone();
            regImage_LeftGlobusPallidus_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_LeftGlobusPallidus_binary.setImageName(regImage_LeftGlobusPallidus.getImageName() + "_binary");
        	algorThreshold_LeftGlobusPallidus_reg_binary = new AlgorithmThresholdDual(regImage_LeftGlobusPallidus_binary, regImage_LeftGlobusPallidus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftGlobusPallidus_reg_binary.addListener(parentDialog);
            algorThreshold_LeftGlobusPallidus_reg_binary.run();

            
            /// Right GlobusPallidus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightGlobusPallidus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_RightGlobusPallidus_binary = (ModelImage)regImage_RightGlobusPallidus.clone();
        	regImage_RightGlobusPallidus_binary.setType(ModelStorageBase.BOOLEAN);
        	regImage_RightGlobusPallidus_binary.setImageName(regImage_RightGlobusPallidus.getImageName() + "_binary");
        	algorThreshold_RightGlobusPallidus_reg_binary = new AlgorithmThresholdDual(regImage_RightGlobusPallidus_binary, regImage_RightGlobusPallidus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightGlobusPallidus_reg_binary.addListener(parentDialog);
            algorThreshold_RightGlobusPallidus_reg_binary.run();
            
            ///   Left Thalamus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = LeftThalamus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_LeftThalamus_binary = (ModelImage)regImage_LeftThalamus.clone();
            regImage_LeftThalamus_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_LeftThalamus_binary.setImageName(regImage_LeftThalamus.getImageName() + "_binary");	
            algorThreshold_LeftThalamus_reg_binary = new AlgorithmThresholdDual(regImage_LeftThalamus_binary, regImage_LeftThalamus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_LeftThalamus_reg_binary.addListener(parentDialog);
            algorThreshold_LeftThalamus_reg_binary.run();
            
            
            //   Right  Thalamus  ***
            thres1 = 1.0f;  // lowerThres;
	        thres2 = RightThalamus;  // upperThres;

	        thresholds[0] = thres1;
	        thresholds[1] = thres2;
            regImage_RightThalamus_binary = (ModelImage)regImage_RightThalamus.clone();
            regImage_RightThalamus_binary.setType(ModelStorageBase.BOOLEAN);
            regImage_RightThalamus_binary.setImageName(myImage.getImageName() + "_RightThalamus_binar");
        	algorThreshold_RightThalamus_reg_binary = new AlgorithmThresholdDual(regImage_RightThalamus_binary, regImage_RightThalamus, thresholds, fillValue, outputType, regionFlag, isInverse);
            algorThreshold_RightThalamus_reg_binary.addListener(parentDialog);
            algorThreshold_RightThalamus_reg_binary.run();
			
			
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog threshold: unable to allocate enough memory");

			return;

		}
	}
	
	public void statisticsDataGeneration() {

		compare(image_LeftHippocampus_binary, regImage_LeftHippocampus_binary, "LeftHippocampus");
		
		compare(image_RightHippocampus_binary, regImage_RightHippocampus_binary, "RightHippocampus");

		compare(image_LeftAmygdala_binary, regImage_LeftAmygdala_binary, "LeftAmygdala");
		compare(image_RightAmygdala_binary, regImage_RightAmygdala_binary, "RightAmygdala");

		compare(image_LeftCaudate_binary, regImage_LeftCaudate_binary, "LeftCaudate");
		compare(image_RightCaudate_binary, regImage_RightCaudate_binary, "RightCaudate");

		compare(image_LeftPutamen_binary, regImage_LeftPutamen_binary, "LeftPutamen");
		compare(image_RightPutamen_binary, regImage_RightPutamen_binary, "RightPutamen");

		compare(image_LeftGlobusPallidus_binary, regImage_LeftGlobusPallidus_binary, "LeftGlobusPallidus");
		compare(image_RightGlobusPallidus_binary, regImage_RightGlobusPallidus_binary, "RightGlobusPallidus");

		compare(image_LeftThalamus_binary, regImage_LeftThalamus_binary, "LeftThalamus");
		compare(image_RightThalamus_binary, regImage_RightThalamus_binary, "RightThalamus");
        
	}
	
	public void compare(ModelImage srcImage, ModelImage targetImage, String sectionName) {
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int zDim = srcImage.getExtents()[2];
		
		int sliceSize = xDim * yDim;
		int volSize = xDim * yDim * zDim;
		
		int[] sourceBuffer = new int[volSize];
		int[] targetBuffer = new int[volSize];
		
	    float[] buffer;
	    
	    int length = 4 * volSize;
		int L1 = 0, L2 = 0, L1andL2 = 0;
	    
	    ModelImage destImage = new ModelImage(ModelStorageBase.ARGB, srcImage.getExtents(), sectionName+"_comparedRGB");
		
		try {
			
			srcImage.exportData(0, volSize, sourceBuffer);
			targetImage.exportData(0, volSize, targetBuffer);
		
			buffer = new float[length]; 
			
			for ( int i = 0, j = 0; i < volSize; i++, j+=4 ) {
				if ( sourceBuffer[i] == 0 && targetBuffer[i] == 0 ) {
					buffer[j] = 0; buffer[j+1] = 0;buffer[j+2] = 0;buffer[j+3] = 0;
				} else if ( sourceBuffer[i] == 1 && targetBuffer[i] == 1 ) {
					// overlappaed region
					L1andL2++;
					L1++;
					L2++;
					buffer[j] = 0; buffer[j+1] = 255;buffer[j+2] = 0;buffer[j+3] = 0;
				} else if ( sourceBuffer[i] == 1 && targetBuffer[i] != 1 ) {
					// belong to srcImage,  Green color
					L1++;
					buffer[j] = 0; buffer[j+1] = 0;buffer[j+2] = 255;buffer[j+3] = 0;
				} else if ( sourceBuffer[i] != 1 && targetBuffer[i] == 1 ) {
					// belong to target Image,  Blue color
					L2++;
					buffer[j] = 0; buffer[j+1] = 0;buffer[j+2] = 0;buffer[j+3] = 255;			
				}
			}
			
			destImage.importData(0, buffer, false);
			saveComparedImage(destImage);
			
			// Overlap:            		   V(L1 & L2 ) 
			//           O(L1, L2) = -----------------------  * 100% 
			//                           (V(L1) + V(L2))/2
			
			double overlapInPercent =   L1andL2 / (( L1 + L2) / 2d) * 100;
			
			// Difference:        		| V(L1) - V(L2) |
			//           D(L1, L2) = ------------------------  * 100%
			//                         (V(L1) + V(L2)) / 2
			
			double differenceInPercent = Math.abs(L1 - L2) /  ( (L1 + L2) / 2d ) * 100;
			
			myData.add(new StatisticsData(sectionName, overlapInPercent, differenceInPercent));
			
		} catch ( IOException e ) {
			MipavUtil.displayError("IOException on srcImage.exportData(0, volSize, sourceBuffer).");
		}
	}

	public void printReport() {

		String fileDir = directory + File.separator + "StatisticReport"
				+ File.separator;

		File fileDirectory = new File(fileDir);
		if (!fileDirectory.isDirectory()) {
			fileDirectory.mkdir();
		}

		try {
			Document document = new Document();
			PdfWriter.getInstance(document, new FileOutputStream(fileDir
					+ "reportPDFTable.pdf"));
			document.open();

			// Amygdala(Am), Caudate(Ca), Hippocampus(Hp), Pallidum(Pa),
			// Putaman(Pu), Thalamus(Th)

			PdfPTable table = new PdfPTable(3);

			// Add the title lines
			table.addCell(" ");
			table.addCell("Overlap");
			table.addCell("Difference");

			for (int i = 0; i < myData.size(); i++) {

				StatisticsData data = myData.get(i);

				table.addCell(data.sectionName);
				table.addCell(String.valueOf(data.overlap) + "%");
				table.addCell(String.valueOf(data.difference) + "%");

			}

			document.add(table);
			document.close();
		} catch (Exception e) {
			e.printStackTrace();
		}

	}
	
	  public void saveComparedImage(ModelImage comparedImage) {
	    	 
	    	String fileDir = directory + File.separator + "StatisticComparison" + File.separator;
	    	
	    	File fileDirectory = new File(fileDir);
			if (!fileDirectory.isDirectory()) {
				fileDirectory.mkdir();
			}
			
	    	String fileName = comparedImage.getImageFileName();
	    	int fileType = comparedImage.getType();
	    	
	    	parentDialog.updateFileInfo(myImage, comparedImage);
	    	
	    	comparedImage.saveImage(fileDir, fileName, fileType, true);
	  }

	  class StatisticsData {
		  
		  String sectionName;
		  double overlap;
		  double difference;
		  
		  public StatisticsData(String _sectionName, double _overlap, double _difference) {
			  sectionName = _sectionName;
			  overlap = _overlap;
			  difference = _difference;
		  }
		  
	  }
}