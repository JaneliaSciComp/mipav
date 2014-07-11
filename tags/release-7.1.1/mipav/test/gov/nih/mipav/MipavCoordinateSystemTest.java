package gov.nih.mipav;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;
import gov.nih.mipav.util.TestingFileUtil;

import java.io.File;
import junit.framework.Assert;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Testing the MipavCoordinateSystem transformation.  All the testings are based on
 * the regression testing with genormcor2.xml image file. 
 * @author ruida cheng
 *
 */
public class MipavCoordinateSystemTest {
	
	//  Source Image
	private static ModelImage srcImage;
	
	// Image buffer
	private static short[] imageBuffer;
	
	// Test image file name
	private static String testImageFileName;
	
	
	public static void main(String[] args) {
	    File f = new File(System.getProperties().getProperty("user.dir"));
	    
	    // genormcor2.raw: 256 x 256 x 34,  Axial. 
	    testImageFileName = f.getPath() + File.separatorChar + "genormcor2.raw";
		
		srcImage = new ModelImage(DataType.SHORT, new int[]{256, 256, 34}, "");
        imageBuffer = TestingFileUtil.readRawFileShort(testImageFileName, false);
        try {
          srcImage.importData(0, imageBuffer, true);
        } catch ( Exception e ) {
        	e.printStackTrace();
        }
        testFileToModel();
        testFileToPatient();
        testFileToScanner();
        testModelToFile();
        testPatientToFile();
        testScannerToFile();
        testFileToModel(0);
	}
	
	/*
	 * Test the file to model coordinate transformation. 
	 */
	public static void testFileToModel() {
		Vector3f target = new Vector3f();
		int[] fileExtents = srcImage.getExtents( );
		int[] result = new int[3];
		result[0] = fileExtents[2];
		result[1] = fileExtents[1];
		result[2] = fileExtents[0];
		// System.err.println("fileExtents[0] = " + fileExtents[0] + " fileExtents[1] = " + fileExtents[1] + " fileExtents[2] = " + fileExtents[2]);
	    MipavCoordinateSystems.fileToModel( new Vector3f( fileExtents[0], fileExtents[1], fileExtents[2] ), target,
	                                                srcImage );
	    // System.err.println("center.X = " + center.X + " center.Y = " + center.Y + " center.Z = " + center.Z);
        Assert.assertEquals(result[0], (int)target.X);
        Assert.assertEquals(result[1], (int)target.Y);
        Assert.assertEquals(result[2], (int)target.Z);
        System.err.println("Pass fileToModel testing.");
	}
	
	/*
	 * Test the file to patient coordinate transformation. 
	 */
	public static void testFileToPatient() {
		// select position sample point.
		WildMagic.LibFoundation.Mathematics.Vector3f position = new WildMagic.LibFoundation.Mathematics.Vector3f();
		position.X = 127.0f;  position.Y = 132.0f; position.Z = 15.0f;
		
		// regression approach for the testing. 
		WildMagic.LibFoundation.Mathematics.Vector3f axial = new WildMagic.LibFoundation.Mathematics.Vector3f();
        MipavCoordinateSystems.fileToPatient(position, axial, srcImage, FileInfoBase.AXIAL);
        Assert.assertEquals(axial.Z, 15.0f);
        
        WildMagic.LibFoundation.Mathematics.Vector3f coronal = new WildMagic.LibFoundation.Mathematics.Vector3f();
        MipavCoordinateSystems.fileToPatient(position, coronal, srcImage, FileInfoBase.CORONAL);
        Assert.assertEquals(coronal.Z, 132.0f); 
        
        WildMagic.LibFoundation.Mathematics.Vector3f sagittal = new WildMagic.LibFoundation.Mathematics.Vector3f();
        MipavCoordinateSystems.fileToPatient(position, sagittal, srcImage, FileInfoBase.SAGITTAL);
        Assert.assertEquals(sagittal.Z, 127.0f);
       
        System.err.println("Pass fileToPatient testing.");
	}
	
	/**
	 * Test the file to scanner position transformation.
	 */
	public static void testFileToScanner() {
		  Vector3f result = new Vector3f();
		  Vector3f position = new Vector3f();
		  position.X = 127.0f;  position.Y = 132.0f; position.Z = 15.0f;
		  
	      MipavCoordinateSystems.fileToScanner(position, result, srcImage);
	      
	      // Based on regression testing.
	      Assert.assertEquals(result.X, 127.0f);
	      Assert.assertEquals(result.Y, 132.0f);
	      Assert.assertEquals(result.Z, 15.0f);
	      
	      System.err.println("Pass fileToScanner testing.");
	}
	
	/*
	 * Test model to file transformation.
	 */
	public static void testModelToFile() {
		 Vector3f in = new Vector3f();
		 Vector3f out = new Vector3f();
		 in.X = 127.0f;  in.Y = 132.0f; in.Z = 15.0f;
		 
		 MipavCoordinateSystems.modelToFile(in, out, srcImage);
		 // System.err.println("out.X = " + out.X + " out.Y = " + out.Y + " out.Z = " + out.Z);
		 Assert.assertEquals(out.X, 15.0f);
	     Assert.assertEquals(out.Y, 132.0f);
	     Assert.assertEquals(out.Z, 127.0f);
	     
	     System.err.println("Pass modelToFile testing.");
	}
	
	/*
	 * Test patient to file transformation
	 */
	public static void testPatientToFile() {
		Vector3f in = new Vector3f();
		in.X = 127.0f;
		in.Y = 132.0f;
		in.Z = 15.0f;

		WildMagic.LibFoundation.Mathematics.Vector3f axial = new WildMagic.LibFoundation.Mathematics.Vector3f();
		MipavCoordinateSystems.patientToFile(in, axial, srcImage, FileInfoBase.AXIAL);
		Assert.assertEquals(axial.X, 127.0f);
		Assert.assertEquals(axial.Y, 132.0f);
		Assert.assertEquals(axial.Z, 15.0f);

		WildMagic.LibFoundation.Mathematics.Vector3f coronal = new WildMagic.LibFoundation.Mathematics.Vector3f();
		MipavCoordinateSystems.patientToFile(in, coronal, srcImage, FileInfoBase.CORONAL);
		Assert.assertEquals(coronal.X, 127.0f);
		Assert.assertEquals(coronal.Y, 15.0f);
		Assert.assertEquals(coronal.Z, 132.0f);

		WildMagic.LibFoundation.Mathematics.Vector3f sagittal = new WildMagic.LibFoundation.Mathematics.Vector3f();
		MipavCoordinateSystems.patientToFile(in, sagittal, srcImage, FileInfoBase.SAGITTAL);
		Assert.assertEquals(sagittal.X, 15.0f);
		Assert.assertEquals(sagittal.Y, 132.0f);
		Assert.assertEquals(sagittal.Z, 127.0f);

		System.err.println("Pass patientToFile testing.");
	}

	/*
	 * Test scanner to file transformation. 
	 */
	public static void testScannerToFile() {
		Vector3f in = new Vector3f();
		Vector3f out = new Vector3f();
		in.X = 127.0f;
		in.Y = 132.0f;
		in.Z = 15.0f;
		MipavCoordinateSystems.scannerToFile(in, out, srcImage);
		Assert.assertEquals(out.X, 127.0f);
	    Assert.assertEquals(out.Y, 132.0f);
	    Assert.assertEquals(out.Z, 15.0f);
	    
	    System.err.println("Pass scannerToFile testing.");
	}
	
	/**
	 * Test file to model transformation. 
	 * @param index  do nothing for no dupicate method call.
	 */
	public static void testFileToModel(int index) {
		int iIndex;
		iIndex = MipavCoordinateSystems.fileToModel(0, srcImage );
		Assert.assertEquals(iIndex, 2);
		iIndex = MipavCoordinateSystems.fileToModel(1, srcImage );
		Assert.assertEquals(iIndex, 1);
		iIndex = MipavCoordinateSystems.fileToModel(2, srcImage );
		Assert.assertEquals(iIndex, 0);
		
		System.err.println("Pass fileToModel(index) testing.");
		
	}
	
}