package gov.nih.mipav.model.file;

import gov.nih.mipav.model.file.FileInfoBase.Unit;



import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Vector;

import java.lang.*;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
/**
 * The class reads and writes PAR/REC files.
 *
 * A right handed coordinate system is used with the positive axis pointing (Anterior/Posterior)A->P, (Foot/Head)F->H,
 * and (Right/Left)R->L.
 * 
 * * Updated September 16, 2011 by Beth Tyrie
 * 
 *      // This explains the derivation of the dicom transformation matrix and origin from the PARREC parameters.
        // The axial PAR file is KKZ_130213_4_1.PAR
        // The first slice of the axial dicom file is 20131023203338769.MR.dcm
        // The sagittal PAR file is KKZ_130213_3_1.PAR
        // The first slice of the sagittal dicom file is 20131107105801812.MR.dcm.
        // The coronal PAR file is KKZ_130213_10_1.PAR
        // The first slice of the coronal dicom file is 20131107105805315.MR.dcm.
        // The angles are the same for the header volume parameters and for every slice
        int ori;
        final int axial = 1;
        final int sagittal = 2;
        final int coronal = 3;
        ori = coronal;
        
        double angulation_rl = 0.0;
        double angulation_ap = 0.0;
        double angulation_fh = 0.0;
        
        switch(ori) {
            case axial: 
            angulation_rl = -8.700;
            angulation_ap = -4.865;
            angulation_fh = 3.364;
            break;
            case sagittal:
            angulation_rl = 0.305;
            angulation_ap = -4.280;
            angulation_fh = 4.083;
            break;
            case coronal:
            angulation_rl = 3.931;
            angulation_ap = -4.031;
            angulation_fh = 4.346;
        }
        
        double angX = 0.0;
        double angY = 0.0;
        double angZ = 0.0;
        
        switch(ori) {
            case axial:
            angX = angulation_rl;
            angY = angulation_ap;
            angZ = angulation_fh;
            break;
            case sagittal:
            angX = angulation_ap;
            angY = -angulation_fh;
            angZ = -angulation_rl;
            break;
            case coronal:
            angX = angulation_rl;
            angY = -angulation_fh;
            angZ = angulation_ap;
        }

        double Sx    = Math.sin(angX * Math.PI/180.0);
        double Sy    = Math.sin(angY * Math.PI/180.0);
        double Sz    = Math.sin(angZ * Math.PI/180.0);
        double Cx    = Math.cos(angX * Math.PI/180.0);
        double Cy    = Math.cos(angY * Math.PI/180.0);
        double Cz    = Math.cos(angZ * Math.PI/180.0);    
        
        // EulerOrder = ORDER_XYZ;
        // This is the Transformation matrix shown in the dicom header
        // The dicom transformation matrix or image.getMatrix() is the transpose of the matrix given by getPatientOrientation().
        // image.getMatrix() and getPatientOrientation() contain just the rotation component.
        double m00=Cy*Cz;
        double m01=-Cy*Sz;
        double m02=Sy;
        double m10=Cz*Sx*Sy+Cx*Sz;
        double m11=Cx*Cz-Sx*Sy*Sz;
        double m12=-Cy*Sx;
        double m20=-Cx*Cz*Sy+Sx*Sz;
        double m21=Cz*Sx+Cx*Sy*Sz;
        double m22=Cx*Cy;
        
        TransMatrix tr = new TransMatrix(4);
        switch (ori) {
            case axial:
            tr.M00 = (float)m00;
            tr.M01 = (float)m01;
            tr.M02 = (float)m02;
            tr.M03 = 0;
            tr.M10 = (float)m10;
            tr.M11 = (float)m11;
            tr.M12 = (float)m12;
            tr.M13 = 0;
            tr.M20 = (float)m20;
            tr.M21 = (float)m21;
            tr.M22 = (float)m22;
            tr.M23 = 0;
            tr.M30 = 0;
            tr.M31 = 0;
            tr.M32 = 0;
            tr.M33 = 1;
            break;
            case sagittal:
            tr.M00 = -(float)m20;
            tr.M01 = -(float)m21;
            tr.M02 = -(float)m22;
            tr.M03 = 0;
            tr.M10 = (float)m00;
            tr.M11 = (float)m01;
            tr.M12 = (float)m02;
            tr.M13 = 0;
            tr.M20 = -(float)m10;
            tr.M21 = -(float)m11;
            tr.M22 = -(float)m12;
            tr.M23 = 0;
            tr.M30 = 0;
            tr.M31 = 0;
            tr.M32 = 0;
            tr.M33 = 1;
            break;
            case coronal:
            tr.M00 = (float)m00;
            tr.M01 = (float)m01;
            tr.M02 = (float)m02;
            tr.M03 = 0;
            tr.M10 = (float)m20;
            tr.M11 = (float)m21;
            tr.M12 = (float)m22;
            tr.M13 = 0;
            tr.M20 = -(float)m10;
            tr.M21 = -(float)m11;
            tr.M22 = -(float)m12;
            tr.M23 = 0;
            tr.M30 = 0;
            tr.M31 = 0;
            tr.M32 = 0;
            tr.M33 = 1;
        }
        
        System.out.println("image transMatrix = " + tr);
        
        double resX = 0.0;
        double resY = 0.0;
        double resZ = 0.0;
        
        switch(ori) {
            case axial:
            resX = 0.859375;
            resY = 0.859375;
            resZ = 1.0;
            break;
            case sagittal:
            resX = 0.42857143;
            resY = 0.42857143;
            resZ = 5.0;
            break;
            case coronal:
            resX = 0.3515625;
            resY = 0.3515625;
            resZ = 3.3;
        }
        
        // The header volume offsets and the individual slice offsets are all different
        // The header volume offsets correspond to (xDim - 1)/2, (yDim - 1)/2, (zDim - 1)/2
        // The slice offsets correspond to (xDim - 1)/2, (yDim - 1)/2, zSlice.
        
        double rl_offset_center = 0.0;
        double ap_offset_center = 0.0;
        double fh_offset_center = 0.0;
        
        switch(ori) {
            case axial:
            rl_offset_center = 6.655;
            ap_offset_center = 3.537;
            fh_offset_center = 22.171;
            break;
            case sagittal:
            rl_offset_center = 8.844;
            ap_offset_center = -3.225;
            fh_offset_center = -0.535;
            break;
            case coronal:
            rl_offset_center = 6.147;
            ap_offset_center = 16.846;
            fh_offset_center = 16.330;
        }
        
        double offsetX = 0.0;
        double offsetY = 0.0;
        double offsetZ = 0.0;
        switch(ori) {
            case axial:
            offsetX = rl_offset_center;
            offsetY = ap_offset_center;
            offsetZ = fh_offset_center;
            break;
            case sagittal:
            offsetX = ap_offset_center;
            offsetY = -fh_offset_center;
            offsetZ = -rl_offset_center;
            break;
            case coronal:
            offsetX = rl_offset_center;
            offsetY = -fh_offset_center;
            offsetZ = ap_offset_center;
        }
        
        double dimX = 0.0;
        double dimY = 0.0;
        double dimZ = 0.0;
        
        switch(ori) {
            case axial:   
            dimX = 256;
            dimY = 256;
            dimZ = 140;
            break;
            case sagittal:
            dimX = 560;
            dimY = 560;
            dimZ = 31;
            case coronal:
            dimX = 512;
            dimY = 512;
            dimZ = 47;
        }
        
        double originX = offsetX - m00 * resX * (dimX-1)/2 - m01 * resY * (dimY-1)/2- m02 * resZ * (dimZ-1)/2;
        double originY = offsetY - m10 * resX * (dimX-1)/2 - m11 * resY * (dimY-1)/2 - m12 * resZ * (dimZ-1)/2;
        double originZ = offsetZ - m20 * resX * (dimX-1)/2 - m21 * resY * (dimY-1)/2 - m22 * resZ * (dimZ-1)/2;
        if (ori == sagittal) {
            originY = -originY;
            originZ = -originZ;
        }
        else if (ori == coronal) {
            originY = -originY;
        }
        // These are the origins shown for dicom slice 0
        System.out.println("originX = " + originX);
        System.out.println("originY = " + originY);
        System.out.println("originZ = " + originZ);

 */

public class FilePARREC extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The extensions of ANALYZE file. */
    public static final String[] hdrEXTENSIONS = { ".par", ".PAR", ".parv2", ".PARv2" };
    public static final String[] imgEXTENSIONS = { ".rec", ".REC", ".frec", ".fREC" };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** File directory of the image. */
    private String fileDir;

    /** Reference to the file info. for an Analyze header */
    private FileInfoPARREC fileInfo = null;

    /** File name of the image. */
    private String fileName;

    /** The image read in from the file. */
    private ModelImage image;
    
    /** Object to save DWI paramters to */
    private DTIParameters dtiparams;

    /** Voxel offset tag used to read in the image. */
    private float vox_offset = 0.0f;
    
    private FileInfoPARREC[] fileInfoArray;

    /** file info **/
    private FileInfoPARREC outInfo;
    
    /** vol map **/
    private HashMap<String,String> VolMap;
    
    /** slice map **/
    private HashMap<String,Integer> SliceMap;
    
    /** vol parameters **/
    private HashMap<String,String> VolParameters;
    
    /** slice parameters **/
    private Vector<String> SliceParameters;
    
    /** slices **/
    private Vector<String> Slices;
    
    /**version**/
    private String version;
    
    private String patientName = "";
    
    /**exam name**/
    private String examName = "";
    
    /**protocol name**/
    private String protocolName = "";
    
    /**patient position **/
    private String patientPosition = "";
    
    private String foldover = "";
    
    private int sliceOrientPos;
    
    /**bFactorIndex**/
    private int bValuePos;
    
    /**bFactorIndex**/
    private int gradPos;
    
    private int sliceOrientIndex;
    
    private int bValueIndex;
    
    private int gradIndex;

    /**counter**/   
    private int counter = 0;
    
    /** floating point value = raw value/scaleSlope + rescaleIntercept/(rescaleSlope*scaleSlope) **/
    private float rescaleIntercept[];
    private float rescaleSlope[];
    private float scaleSlope[];
    /** True if the 3 values are the same for all slices **/
    private boolean sameSliceScalings = true;
    private int originalDataType;
    
    /** normal par/rec are sorted by volumes...sometime, they are not sorted like this...instead they are based on slices
     * if that is true, then we need to change around the image dataarray to make it sorted by volumes
     */
    private boolean isSortedByVolumes = true;
    
    /** num slices per volume **/
    private int numSlices;
    
    /** num vols in 4d dataset **/
    private int numVolumes;
    
    boolean changeToUnsignedInts = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileAnalyze object.
     *
     * @param  fileNames  DOCUMENT ME!
     */
    public FilePARREC(String[] fileNames) {       
        super(fileNames);
    }

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FilePARREC(String fName, String fDir) {
        fileName = fName;
        fileDir = fDir;

        fileNames = getCompleteFileNameList(fileDir+fileName);
    }
    
    /**
     * Constructor used by FileIO to write an image
     * @param fileName
     * @param fileDirectory
     * @param fileInfoPARREC[]
     * @param changeToUnsignedInts
     */
     public FilePARREC(String fileName, String fileDirectory, FileInfoPARREC[] fileInfoArray, boolean changeToUnsignedInts) {
         this.fileInfoArray = fileInfoArray;
         outInfo = fileInfoArray[0];
         this.fileName = fileName;
         this.fileDir = fileDirectory;
         fileNames =getCompleteFileNameListDefault(fileDirectory+fileName);
         this.changeToUnsignedInts = changeToUnsignedInts;
     }

   /**
    * Constructor used by FileIO to write an image
    * @param fileName
    * @param fileDirectory
    * @param fileInfo
    */
    public FilePARREC(String fileName, String fileDirectory, FileInfoBase fileInfo) {
        outInfo = (FileInfoPARREC)fileInfo;
        this.fileName = fileName;
        this.fileDir = fileDirectory;
        fileNames =getCompleteFileNameListDefault(fileDirectory+fileName);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        if (VolMap != null) {
            VolMap.clear();
            VolMap = null;
        }
        if (SliceMap != null) {
            SliceMap.clear();
            SliceMap = null;
        }
        if (VolParameters != null) {
            VolParameters.clear();
            VolParameters = null;
        }
        if (SliceParameters != null) {
            SliceParameters.removeAllElements();
            SliceParameters = null;
        }
        if (Slices != null) {
            Slices.removeAllElements();
            Slices = null;
        }
        fileName = null;
        fileDir = null;
        fileInfo = null;
        if (fileInfoArray != null) {
            for (int i = 0; i < fileInfoArray.length; i++) {
                fileInfoArray[i] = null;
            }
            fileInfoArray = null;
        }
        image = null;
        outInfo = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * 
     * @param image
     */
    public void setImage(ModelImage image) {
        this.image = image;
    }
    
    
    /**
     * Returns the complete list of file names according to given file name.
     *
     * @param   absolutePath  one file name of PARREC.
     *
     * @return  the complete list of file names.
     */
    public static String[] getCompleteFileNameList(String absolutePath) {
        String[] completeFileNameList = new String[2];
        if (FilePARREC.isHeaderFile(absolutePath)) {
            completeFileNameList[0] = absolutePath;

            // Try all extensions until one has a file that exists
            // completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(".")) + EXTENSIONS[1];
            for (int k = 0; k < imgEXTENSIONS.length; k++) {

            	if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
            		completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) +
            			FilePARREC.imgEXTENSIONS[k];
            	} else {
            		completeFileNameList[1] = absolutePath;
            	}
                
                File fp = new File(completeFileNameList[1]);

                if (fp.exists()) {
                    break;
                }
            }
        } else if (FilePARREC.isImageFile(absolutePath)) {
            completeFileNameList[1] = absolutePath;

            // Try all extensions until one has a file that exists
            // completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(".")) + EXTENSIONS[0];
            for(int k=0;k<hdrEXTENSIONS.length;k++) {
            	
            	if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
            		completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) +
            			FilePARREC.hdrEXTENSIONS[k];
            	} else {
            		completeFileNameList[0] = absolutePath;
            	}
            }        
            for (int k = 0; k < hdrEXTENSIONS.length; k++) {
              
                if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
            		completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) +
            			FilePARREC.hdrEXTENSIONS[k];
            	} else {
            		completeFileNameList[1] = absolutePath;
            	}
                
                File fp = new File(completeFileNameList[0]);

                if (fp.exists()) {
                    break;
                }
            }
        } else {
            completeFileNameList = null;
        }

        
        return completeFileNameList;
    }


    /**
     * ======= Returns the header file.
     *
     * @param   fileNames  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    public static String getHeaderFile(String[] fileNames) {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        for (int i = 0; i < fileNames.length; i++) {
            for(int k=0;k<hdrEXTENSIONS.length;k++) {
                if (FileUtility.getExtension(fileNames[i]).equals(FilePARREC.hdrEXTENSIONS[k])) {
                    return fileNames[i];
                }
            }
        }

        return null;
    }

    /**
     * Returns the image file list.
     *
     * @param   fileNames  DOCUMENT ME!
     *
     * @return  the image file list.
     */

    public static String[] getImageFiles(String[] fileNames) {

        if (fileNames == null) {
            return null;
        }

        String[] result = new String[1];

        for (int i = 0; i < fileNames.length; i++) {

            for(int k=0;k<imgEXTENSIONS.length;k++) {
                if (FileUtility.getExtension(fileNames[i]).equals(FilePARREC.imgEXTENSIONS[k])) {
                    result[0] = fileNames[i];

                    return result;
                }
            }
        }

        return null;
    }

    /**
     * Return true if the file specified by absolutePath is header file of PARREC.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is header file.
     */

    public static boolean isHeaderFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);
        for(int k=0;k<hdrEXTENSIONS.length;k++) {
            if (extension.equalsIgnoreCase(FilePARREC.hdrEXTENSIONS[k])) {
                return true;
            }
        }

        return false;
    }

    /**
     * Return true if the file specified by absolutePath is image file of PARREC.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is image file.
     */

    public static boolean isImageFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);
        for(int k=0;k<imgEXTENSIONS.length;k++) {
            if (extension.equalsIgnoreCase(FilePARREC.imgEXTENSIONS[k]))
                return true;

        }
        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  java.io.IOException       DOCUMENT ME!
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public ModelImage createImage() throws IOException, OutOfMemoryError {
        fileInfo = new FileInfoPARREC(fileName, fileDir, FileUtility.PARREC);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException("PAR/REC header file error"));
        }

        try {
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        // if vox units defines the units of measure, then use that instead
        // clones the file info
        updateUnitsOfMeasure(fileInfo, image);
        updateTransformMatrix(fileInfo, image);
        //updateStartLocations(image.getFileInfo());


        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

            long offset = (long) Math.abs(vox_offset); //?? used to read one slice?

            rawFile.readImage(image, offset);



        } catch (IOException error) {
            throw new IOException("FilePARREC: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        /** Initializes the ModelImage object */
        if (image != null) {
            image.calcMinMax();
        }

        return image;
    }


    private void updateTransformMatrix(FileInfoPARREC fileInfo, ModelImage image) {
        int i;
        
        double[] sliceAng = fileInfo.getSliceAngulation();
        
        double rot[] = new double[3];
        for (i = 0; i < 3; i++) {
            rot[i] = Math.toRadians(sliceAng[i]);
        }
        
        double Sx    = Math.sin(rot[0]);
        double Sy    = Math.sin(rot[1]);
        double Sz    = Math.sin(rot[2]);
        double Cx    = Math.cos(rot[0]);
        double Cy    = Math.cos(rot[1]);
        double Cz    = Math.cos(rot[2]); 
        
        // EulerOrder = ORDER_XYZ;
        // This is the Transformation matrix shown in the dicom header
        // The dicom transformation matrix or image.getMatrix() is the transpose of the matrix given by getPatientOrientation().
        // image.getMatrix() and getPatientOrientation() contain just the rotation component.
        double m00=Cy*Cz;
        double m01=-Cy*Sz;
        double m02=Sy;
        double m10=Cz*Sx*Sy+Cx*Sz;
        double m11=Cx*Cz-Sx*Sy*Sz;
        double m12=-Cy*Sx;
        double m20=-Cx*Cz*Sy+Sx*Sz;
        double m21=Cz*Sx+Cx*Sy*Sz;
        double m22=Cx*Cy;
        
        TransMatrix tr = new TransMatrix(4);
        int ori = fileInfo.getImageOrientation();
        switch (ori) {
            case FileInfoBase.AXIAL:
            tr.M00 = (float)m00;
            tr.M01 = (float)m01;
            tr.M02 = (float)m02;
            tr.M03 = 0;
            tr.M10 = (float)m10;
            tr.M11 = (float)m11;
            tr.M12 = (float)m12;
            tr.M13 = 0;
            tr.M20 = (float)m20;
            tr.M21 = (float)m21;
            tr.M22 = (float)m22;
            tr.M23 = 0;
            tr.M30 = 0;
            tr.M31 = 0;
            tr.M32 = 0;
            tr.M33 = 1;
            break;
            case FileInfoBase.SAGITTAL:
            tr.M00 = -(float)m20;
            tr.M01 = -(float)m21;
            tr.M02 = -(float)m22;
            tr.M03 = 0;
            tr.M10 = (float)m00;
            tr.M11 = (float)m01;
            tr.M12 = (float)m02;
            tr.M13 = 0;
            tr.M20 = -(float)m10;
            tr.M21 = -(float)m11;
            tr.M22 = -(float)m12;
            tr.M23 = 0;
            tr.M30 = 0;
            tr.M31 = 0;
            tr.M32 = 0;
            tr.M33 = 1;
            break;
            case FileInfoBase.CORONAL:
            tr.M00 = (float)m00;
            tr.M01 = (float)m01;
            tr.M02 = (float)m02;
            tr.M03 = 0;
            tr.M10 = (float)m20;
            tr.M11 = (float)m21;
            tr.M12 = (float)m22;
            tr.M13 = 0;
            tr.M20 = -(float)m10;
            tr.M21 = -(float)m11;
            tr.M22 = -(float)m12;
            tr.M23 = 0;
            tr.M30 = 0;
            tr.M31 = 0;
            tr.M32 = 0;
            tr.M33 = 1;
        }
        double resX = fileInfo.getResolutions()[0];
        double resY = fileInfo.getResolutions()[1];
        double resZ = fileInfo.getResolutions()[2];
        
        double[] offCentre = fileInfo.getOffCentre();
        // The header volume offsets and the individual slice offsets are all different
        // The header volume offsets correspond to (xDim - 1)/2, (yDim - 1)/2, (zDim - 1)/2
        // The slice offsets correspond to (xDim - 1)/2, (yDim - 1)/2, zSlice.
        double offsetX = offCentre[0];
        double offsetY = offCentre[1];
        double offsetZ = offCentre[2];
        
        double dimX = image.getExtents()[0];
        double dimY = image.getExtents()[1];
        double dimZ = image.getExtents()[2];
        
        double originX = offsetX - m00 * resX * (dimX-1)/2 - m01 * resY * (dimY-1)/2- m02 * resZ * (dimZ-1)/2;
        double originY = offsetY - m10 * resX * (dimX-1)/2 - m11 * resY * (dimY-1)/2 - m12 * resZ * (dimZ-1)/2;
        double originZ = offsetZ - m20 * resX * (dimX-1)/2 - m21 * resY * (dimY-1)/2 - m22 * resZ * (dimZ-1)/2;
        if (ori == FileInfoBase.SAGITTAL) {
            originY = -originY;
            originZ = -originZ;
        }
        else if (ori == FileInfoBase.CORONAL) {
            originY = -originY;
        }
        
        switch(ori) {
            case FileInfoBase.AXIAL:
                // R-L, A-P, I-S
                tr.M03 = (float)(originX);
                tr.M13 = (float)(originY);
                tr.M23 = (float)(originZ);
                break;
            case FileInfoBase.SAGITTAL:
                // A-P, S-I, L-R
                tr.M03 = (float)(originZ);
                tr.M13 = (float)(originX);
                tr.M23 = (float)(originY);
                break;
            case FileInfoBase.CORONAL:
                // R-L, S-I, A-P
                tr.M03 = (float)(originX);
                tr.M13 = (float)(originZ);
                tr.M23 = (float)(originY);
        }
        image.setMatrix(tr);
    }
    
    /**
     * Converts translation matrix to mipav specific format
     */
    public static TransMatrix ConvertToMIPAVConvention(TransMatrix mat){
        //float[] vals = {0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1}; 
        TransMatrix mipavMat = new TransMatrix(4);
        mipavMat.set(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
        //mat.Transpose();
        mipavMat.mult(mat);
        mipavMat.mult(mipavMat);
        return mipavMat;
    }
    
    /**
     * Makes rotation matrix based on image off-centres that were stored in header and converted to orientation-specific values.
     */
    public static TransMatrix makeTranslationMatrix(double[] translations){
        TransMatrix finalTrans = new TransMatrix(4);
        for(int i = 0; i < 3; i++){
            finalTrans.set(i,i,1);
            finalTrans.set(3,i,translations[i]);
        }
        finalTrans.set(3,3,1);      
        
        return finalTrans;
    }
    
    /**
     * Makes rotation matrix based on image angulations that were stored in header and converted to orientation-specific values.
     */
    public static TransMatrix makeRotationMatrix(int[] size, double[] rotations){
        float[] center = new float[3];
        for( int i=0; i<3; i++){
            center[i] = (size[i]+1.0f)/2;
        }
        TransMatrix transCenter = new TransMatrix(4);
        for(int i = 0; i < 3; i++){
            transCenter.set(i,i,1);
            transCenter.set(3,i,-1*center[i]);
        }
        transCenter.set(3,3,1);
        
        TransMatrix transHome = new TransMatrix(4,4);
        for(int i = 0; i < 3; i++){
            transHome.set(i,i,1);
            transHome.set(3,i,center[i]);
        }
        transHome.set(3,3,1);
        
        double[] thetas = new double[3];
        for(int i = 0; i < 3; i++){ 
            thetas[i] = Math.toRadians(rotations[i]);
        }
        
        TransMatrix rotX = new TransMatrix(4), rotY = new TransMatrix(4), rotZ = new TransMatrix(4);
        rotX.set(0,0,1);    rotX.set(0,1,0);                    rotX.set(0,2,0);                        rotX.set(0,3,0);
        rotX.set(1,0,0);    rotX.set(1,1,Math.cos(thetas[0]));  rotX.set(1,2,-1*Math.sin(thetas[0]));   rotX.set(1,3,0);
        rotX.set(2,0,0);    rotX.set(2,1,Math.sin(thetas[0]));  rotX.set(2,2,Math.cos(thetas[0]));      rotX.set(2,3,0);
        rotX.set(3,0,0);    rotX.set(3,1,0);                    rotX.set(3,2,0);                        rotX.set(3,3,1);

        rotY.set(0,0,Math.cos(thetas[1]));  rotY.set(0,1,0);    rotY.set(0,2,-1*(Math.sin(thetas[1]))); rotY.set(0,3,0);
        rotY.set(1,0,0);                    rotY.set(1,1,1);    rotY.set(1,2,0);                        rotY.set(1,3,0);
        rotY.set(2,0,Math.sin(thetas[1]));  rotY.set(2,1,0);    rotY.set(2,2,Math.cos(thetas[1]));      rotY.set(2,3,0);
        rotY.set(3,0,0);                    rotY.set(3,1,0);    rotY.set(3,2,0);                        rotY.set(3,3,1);
        
        rotZ.set(0,0,Math.cos(thetas[2]));      rotZ.set(0,1,Math.sin(thetas[2]));  rotZ.set(0,2,0);    rotZ.set(0,3,0);
        rotZ.set(1,0,-1*Math.sin(thetas[2]));   rotZ.set(1,1,Math.cos(thetas[2]));  rotZ.set(1,2,0);    rotZ.set(1,3,0);
        rotZ.set(2,0,0);                        rotZ.set(2,1,0);                    rotZ.set(2,2,1);    rotZ.set(2,3,0);
        rotZ.set(3,0,0);                        rotZ.set(3,1,0);                    rotZ.set(3,2,0);    rotZ.set(3,3,1);

        transCenter.mult(rotX);
        transCenter.mult(rotY);
        transCenter.mult(rotZ);
        transCenter.mult(transHome);
        
        return transCenter;
    }

    /**
     * Returns the FileInfoAnalyze read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoPARREC getFileInfo() {
        return fileInfo;
    }

    /**
     * <<<<<<< .working Returns the header file.
     *
     * @return  DOCUMENT ME!
     */
    public String getHeaderFile() {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        for (int i = 0; i < fileNames.length; i++) {
            for(int k=0;k<hdrEXTENSIONS.length;k++)
            {
                if (FileUtility.getExtension(fileNames[i]).equals(FilePARREC.hdrEXTENSIONS[k])) {
                    return fileNames[i];
                }
            }
        }

        return null;
    }

    /**
     * Returns the image file list.
     *
     * @return  the image file list.
     */
    public String[] getImageFiles() {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        String[] result = new String[1];

        for (int i = 0; i < fileNames.length; i++) {
            for(int k=0;k<imgEXTENSIONS.length;k++) {
                if (FileUtility.getExtension(fileNames[i]).equals(FilePARREC.imgEXTENSIONS[k])) {
                    result[0] = fileNames[i];

                    return result;
                }
            }
        }

        return null;
    }

    /**
     * Reads the analyze header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  java.io.IOException  if there is an error reading the header
     *
     * @see        gov.nih.mipav.model.file.FileInfoAnalyze
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        //Setup Basic Variables//
        String fileHeaderName;
        fileHeaderName=getHeaderFile();
        Preferences.debug(" fileHeaderName = " + fileHeaderName  + "\n", Preferences.DEBUG_FILEIO);
        File fileHeader = new File(fileHeaderName);
        if (fileHeader.exists() == false) {
            Preferences.debug(fileDir + fileHeaderName + " cannot be found.\n", Preferences.DEBUG_FILEIO);
            return false;
        }

        // Required to read in multi-file analyze images. i.e. a set of 3D images to make a 4D dataset
        // The files should all have the same prefix. fooR_001.img, fooR_002.img etc.
        if (fileInfo == null) { // if the file info does not yet exist: make it
            Preferences.debug("fileInfo is null\n", Preferences.DEBUG_FILEIO);
            fileInfo = new FileInfoPARREC(imageFileName, fileDir, FileUtility.PARREC);
            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) { // Why 3/20/2001
                throw (new IOException(" Analyze header file error"));
            }
        }

        //Open the Header File//
        try {
            raFile = new RandomAccessFile(fileHeader, "r");
        } catch (FileNotFoundException e) {
            Preferences.debug("raFile = new RandomAccessFile(fileHeader, r) gave " + "FileNotFoundException " + e,
            		Preferences.DEBUG_FILEIO);
            throw new IOException("Error on raFile = new RandomAccessFile(fileHeader,r)");
        }

        // Begin the processing loop
        
       /* BufferedReader rdr = new BufferedReader(new FileReader(new File(fileName)));
        String thisline = "";
        thisline = rdr.readLine();
        int num = 0;
        
        while(!thisline.contains("END OF DATA DESCRIPTION FILE")){
            thisline = rdr.readLine();
            
            //Extract Date
            if(thisline.contains("Examination date/time")){
                int ind = thisline.indexOf(":");
                System.out.println("workingdate");
                String date = thisline.substring(ind+1,thisline.indexOf("/",ind));
                date=date.trim();
                date=date.substring(0, 4)+date.substring(5, 7)+date.substring(8, 10);
            }
            
        }*/

        VolMap = buildParVolMap();
        SliceMap = buildParSliceMap();

        VolParameters = new HashMap<String,String>();
        SliceParameters = new Vector<String>();
        Slices = new Vector<String>();

        String nextLine = raFile.readLine();
        
        //String version;
        //String[] versionNumber;

        while(null!=nextLine) {
            nextLine = nextLine.trim();
            if(nextLine.length()<1) { // Blank line = comment
                nextLine = raFile.readLine().trim();
                continue;
            }
            switch(nextLine.charAt(0)) {
                case '#' : //# = comment
                	if(nextLine.contains("Research image export tool")) {
                		//need to get version
                		version = nextLine.substring(nextLine.lastIndexOf("V"), nextLine.length());
                		fileInfo.setVersion(version);            	               		
                	}
                           	
                	String imageInfo  = "";                
                	int sliceOrientIndexCounter = -1;
                	int bValIndexCounter = -1;
                    int gradIndexCounter = -1;
                	
                    if(nextLine.compareToIgnoreCase("# === IMAGE INFORMATION DEFINITION =============================================")==0) {
                        String line = raFile.readLine().trim();
                        String ignore = "The rest of this file contains ONE line per image";

                        while(line.compareToIgnoreCase("# === IMAGE INFORMATION ==========================================================")!=0) {
                            if(line.length()>1) {
                               
                                if(!line.contains(ignore)) {
                                    
                                    SliceParameters.add(line.trim());
                                 
                                	fileInfo.setImageInfoList(line.trim());                                                        
                                	counter ++;
                                	
                                	imageInfo = new String(imageInfo + line.trim());
                                	   
                                	   if (imageInfo.contains("slice orientation ( TRA/SAG/COR ) ")){                                         
                                	       sliceOrientIndexCounter++;
                                       }
                                	    if (imageInfo.contains("diffusion_b_factor")){                              	       
                                	       bValIndexCounter++;
                                	   }
                                	   
                                       if (imageInfo.contains("diffusion (ap, fh, rl)")){                                      
                                           gradIndexCounter++;
                                       }                                	   
                                  }                  
                            }
                     
                            line = raFile.readLine().trim();                           
                        }
                        sliceOrientPos = counter - sliceOrientIndexCounter;
                        bValuePos = counter - bValIndexCounter;
                        gradPos = counter - gradIndexCounter;                       
                    }
             
                    break;
                case '.' : // scan file variable
                    if(nextLine.contains("Patient name")) {
                        String patientNameLine = nextLine.trim();
                        int patientNameInd = patientNameLine.indexOf(":");
                        int patientNameLineLength = patientNameLine.length();
                        
                        for (int i = 0 ; i < patientNameLineLength-(patientNameInd+1); i++) {
                            int patientIndex = (patientNameInd+1)+i;
                            char patientLetter= patientNameLine.charAt(patientIndex);
                            patientName =patientName + patientLetter;
                            patientName = patientName.trim();
                          
                        }
                        fileInfo.setPatientName(patientName);
                    } 
                    if(nextLine.contains("Examination name")) {
                        String examNameLine = nextLine.trim();
                        int examNameInd = examNameLine.indexOf(":");
                        int examNameLineLength = examNameLine.length();
                        
                        for (int i = 0 ; i < examNameLineLength-(examNameInd+1); i++) {
                            int examIndex = (examNameInd+1)+i;
                            char examLetter= examNameLine.charAt(examIndex);
                            examName =examName + examLetter;
                            examName = examName.trim();
                          
                        }
                        fileInfo.setExamName(examName);
                    } else if(nextLine.contains("Protocol name")) {
                       String protocolNameLine = nextLine.trim();
                       int protocolNameInd = protocolNameLine.indexOf(":");
                       int protocolNameLineLength = protocolNameLine.length();
                       for (int i = 0 ; i < protocolNameLineLength-(protocolNameInd+1); i++) {
                           int protocolIndex = (protocolNameInd+1)+i;
                           char protocolLetter= protocolNameLine.charAt(protocolIndex);
                           protocolName =protocolName + protocolLetter;
                           protocolName = protocolName.trim();                            
                       }
                       fileInfo.setProtocolName(protocolName);
                       
                   } else if(nextLine.contains("Examination date/time")){ //Determine date of exam
                        int ind = nextLine.indexOf(":");
                        String date = nextLine.substring(ind+1,nextLine.indexOf("/",ind));
                        date = date.trim();
                        fileInfo.setDate(date);
                   } else if(nextLine.contains("Patient position")){
                        String patientPositionLine = nextLine.trim();
                        int patientPositionInd = patientPositionLine.indexOf(":");
                        int patientPositionLineLength = patientPositionLine.length();
                        for (int i = 0 ; i < patientPositionLineLength-(patientPositionInd+1); i++) {
                            int positionIndex = (patientPositionInd+1)+i;
                            char positionLetter= patientPositionLine.charAt(positionIndex);
                            patientPosition =patientPosition + positionLetter;
                            patientPosition = patientPosition.trim();                            
                       }
                        fileInfo.setPatientPosition(patientPosition);
                    } else if(nextLine.contains("Preparation direction")){
                        String foldoverLine = nextLine.trim();
                        int foldoverInd = foldoverLine.indexOf(":");
                        int foldoverLineLength = foldoverLine.length();
                        for (int i = 0 ; i < foldoverLineLength-(foldoverInd+1); i++) {
                            int foldoverIndex = (foldoverInd+1)+i;
                            char foldoverLetter= foldoverLine.charAt(foldoverIndex);
                            foldover = foldover + foldoverLetter;
                            foldover = foldover.trim();                            
                       }
                        fileInfo.setPreparationDirection(foldover);
                    } else if(nextLine.contains("Angulation midslice")){
                        String info = nextLine.substring(nextLine.indexOf(":")+1);
                        info=info.trim();
                        double [] sliceAng = new double[3];
                        sliceAng[0] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                        info = info.substring(info.indexOf(' ')+1, info.length()).trim();
                        sliceAng[1] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                        sliceAng[2] = Double.parseDouble(info.substring(info.indexOf(' ')+1, info.length()));
                        fileInfo.setSliceAngulation(sliceAng);
    
                    } else if(nextLine.contains("Off Centre midslice")){
                        String info = nextLine.substring(nextLine.indexOf(":")+1);
                        info=info.trim();
                        double [] offCentre = new double[3];
                        offCentre[0] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                        info = info.substring(info.indexOf(' ')+1, info.length()).trim();                          
                        offCentre[1] = Double.parseDouble(info.substring(0, info.indexOf(' ')));
                        offCentre[2] = Double.parseDouble(info.substring(info.indexOf(' ')+1, info.length()));
                        fileInfo.setOffCentre(offCentre);   
                    } else if(nextLine.contains("Repetition time")) {
                    	String info = nextLine.substring(nextLine.indexOf(":")+1);
                        String repetitionTime = info.trim();
                        fileInfo.setRepetitionTime(repetitionTime);
                    }
                    

                	fileInfo.setGeneralInfoList(nextLine);
                    String []tags = nextLine.split(":");
                    String tag = tags[0].trim();
                    String key;
                    if(tags.length < 2) {
                    	key = "";
                    } else {
                    	key = tags[1].trim();
                    }
                    String stgTag = (String)VolMap.get(tag);
                    if(null!=stgTag) {
                        VolParameters.put(stgTag,key);
                    } else {
                        Preferences.debug("FilePARREC:readHeader. Unknown Volume Tag: " + tag + "=" + key + "\n",
                        		Preferences.DEBUG_FILEIO);
                    }
                    break;
                default: // parse as image slice information
                  
                    Slices.add(nextLine);               
                    break;

            }
            
            nextLine = raFile.readLine();

        }

        //All done, close the header file//
        try {
            raFile.close();
        } catch (IOException e) {
            Preferences.debug("raFile.close() gave IOException " + e + "\n", Preferences.DEBUG_FILEIO);
            throw new IOException(" Error on raFile.close()");
        }

        //Now parse the header file:

        //Setup Default File Information//
        fileInfo.setEndianess(LITTLE_ENDIAN);
        fileInfo.setModality(FileInfoBase.MAGNETIC_RESONANCE);
        for(int j=0;j<3;j++)
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(),j);
        fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(),3);


        String s;
        String[] ss;
        //Get the volume variables:
        s = (String)VolParameters.get("max_num_slices");
        if(s==null) {
            Preferences.debug("FilePARREC:readHeader. Number of slices not found."+ "\n", Preferences.DEBUG_FILEIO);
            return false;
        }
        numSlices = Integer.valueOf(s);
        fileInfo.setNumSlices(numSlices);
        //get numVolumes
        numVolumes = Slices.size()/numSlices;


        // Let's parse the first slice:    
        String sl = (String)Slices.get(0);      
        String[] values = sl.split("\\s+");
        
 
        //Create bvalue String array from V3 par/rec file (for DTI par/rec files)
        double [] flBvalueArray = new double[numVolumes];
        double[][] flGradientArray = new double[numVolumes][3];
        
        if ((examName.toUpperCase()).contains("DTI")|| (protocolName.toUpperCase()).contains("DTI")){ 
            dtiparams = new DTIParameters(numVolumes);
            dtiparams.setNumVolumes(numVolumes);
            
            
            //Determine arrangement of slices stored with data
            String firstSliceIndex = Slices.get(0);
            firstSliceIndex = firstSliceIndex.trim();
            final String[] firstSliceArr = firstSliceIndex.split("\\s+");
            int firstSliceValue= Integer.parseInt(firstSliceArr[0]);
            
            String secondSliceIndex = Slices.get(1);
            secondSliceIndex = secondSliceIndex.trim();
            final String[] secondSliceArray = secondSliceIndex.split("\\s+");
            int secondSliceValue = Integer.parseInt(secondSliceArray[0]);
                      
            // Find slice index of bvalues
            int counter2o = 0;
            int counter3o = 0;
            for (int i = 0; i < (sliceOrientPos-2); i++){
                if (SliceParameters.get(i).contains("2")){
                    counter2o++;
                }
                if (SliceParameters.get(i).contains("3")){ 
                    counter3o++;
                }
            }           
            sliceOrientIndex = ((counter2o*1)+(counter3o*2) + (sliceOrientPos-1));
            
            String firstSlice = Slices.get(0);
            firstSlice = firstSlice.trim();
            final String[] firstOrientSlice = firstSlice.split("\\s+");
            int firstOrientValue = Integer.parseInt(firstOrientSlice[sliceOrientIndex]);
            fileInfo.setSliceOrient(firstOrientValue);
            
            // Find slice index of bvalues
            int counter2 = 0;
            int counter3 = 0;
            for (int i = 0; i < (bValuePos-2); i++){
                if (SliceParameters.get(i).contains("2")){
                    counter2++;
                }
                if (SliceParameters.get(i).contains("3")){ 
                    counter3++;
                }
            }
            
            bValueIndex = ((counter2*1)+(counter3*2) + (bValuePos-1));

            
            if (version.equals("V3")||version.equals("V4")){                
                if (firstSliceValue!=secondSliceValue){
                    for (int i = 0; i < numVolumes; i++){
                        String sliceIndex = Slices.get(i*numSlices);
                        sliceIndex = sliceIndex.trim();
                        final String[] sliceArr = sliceIndex.split("\\s+");                      
                        flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);

                        }
                    dtiparams.setbValues(flBvalueArray);                   
                    }
                
                else{
                    for (int i = 0; i < numVolumes; i++){
                        String sliceIndex = Slices.get(i);
                        sliceIndex = sliceIndex.trim();
                        final String[] sliceArr = sliceIndex.split("\\s+");
                        flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);

                    }
                    dtiparams.setbValues(flBvalueArray);
                }       
        }
            else if(version.equals("V4.1")||version.equals("V4.2") ){
             // Find slice index automatically of gradient values
                int counter2s = 0;
                int counter3s = 0;
                    for (int i = 0; i < (gradPos-2); i++){
                        if (SliceParameters.get(i).contains("2")){
                            counter2s++;
                            }
                        if (SliceParameters.get(i).contains("3")){ 
                            counter3s++;
                            }
                        }
                    gradIndex = ((counter2*1)+(counter3*2) + (gradPos-1));
                  
                if (firstSliceValue!=secondSliceValue){
                    for (int i = 0; i < numVolumes; i++){
                        String sliceIndex = Slices.get(i*numSlices);
                        sliceIndex = sliceIndex.trim();
                        final String[] sliceArr = sliceIndex.split("\\s+");
                        flGradientArray[i][0] = Double.valueOf(sliceArr[gradIndex]);
                        flGradientArray[i][1] = Double.valueOf(sliceArr[gradIndex+1]);
                        flGradientArray[i][2] = Double.valueOf(sliceArr[gradIndex+2]);
                        flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);
                        }
                    
                    dtiparams.setbValues(flBvalueArray);
                    dtiparams.setGradients(flGradientArray);

                    }
                
                else{
                    for (int i = 0; i < numVolumes; i++){
                        String sliceIndex = Slices.get(i);
                        sliceIndex = sliceIndex.trim();
                        final String[] sliceArr = sliceIndex.split("\\s+");
                        flGradientArray[i][0] = Double.valueOf(sliceArr[gradIndex]);
                        flGradientArray[i][1] = Double.valueOf(sliceArr[gradIndex+1]);
                        flGradientArray[i][2] = Double.valueOf(sliceArr[gradIndex+2]);
                        flBvalueArray[i] = Double.parseDouble(sliceArr[bValueIndex]);
                        }
                    
                    dtiparams.setbValues(flBvalueArray);
                    dtiparams.setGradients(flGradientArray);
                }

            }
        }       


        float slicethk=0, slicegap=0;
        int ori=0;
        int dim1=0, dim2=0;

        s = (String)VolParameters.get("scn_recon_res");
        if(s!=null) {
            ss = s.trim().split("\\s+");
            if(ss.length==2) {
                dim1 = Integer.valueOf(ss[0]);
                dim2 = Integer.valueOf(ss[1]);
            }
        }

        float fovRL=0, fovIS=0, fovAP=0;
        s = (String)VolParameters.get("scn_fov");
        if(s!=null) {
            ss = s.trim().split("\\s+");
            if(ss.length==3) {
                fovRL = Float.valueOf(ss[2]);
                fovIS = Float.valueOf(ss[1]);
                fovAP = Float.valueOf(ss[0]);
            } else {
                Preferences.debug("FilePARREC:readHeader. FOV doesn't make sense: "+s+ "\n", Preferences.DEBUG_FILEIO);
                return false;
            }
        } else {
            Preferences.debug("FilePARREC:readHeader. FOV not found."+ "\n", Preferences.DEBUG_FILEIO);
            return false;
        }

        int bpp = 0;
        s= (String)VolParameters.get("scn_pix_bits");
        if(s!=null)
			bpp=Integer.valueOf(s).intValue();
        
        float repetitionTime = -1.0f;
        s = (String)VolParameters.get("scn_rep_time");
        if (s != null) {
            repetitionTime = Float.valueOf(s).floatValue();
        }

        int idx =0;
        rescaleIntercept = new float[Slices.size()];
        rescaleSlope = new float[Slices.size()];
        scaleSlope = new float[Slices.size()];
        float resInit = 0.0f;
        float[] res1 = new float[Slices.size()];
        float[] res2 = new float[Slices.size()];
        for (int j = 0; j < Slices.size(); j++) {
            rescaleIntercept[j] = 0.0f;
            rescaleSlope[j] = 1.0f;
            scaleSlope[j] = 1.0f;
            res1[j] = resInit;
            res2[j] = resInit;
        }
        for(int j=0;j<SliceParameters.size();j++) {
            String tag = (String)SliceParameters.get(j);
            if(tag.compareToIgnoreCase("#  slice thickness (in mm )                 (float)")==0) {
                slicethk = Float.valueOf(values[idx]);
            } else if(tag.compareToIgnoreCase("#  slice gap (in mm )                       (float)")==0) {
                slicegap = Float.valueOf(values[idx]);
            } else if(tag.compareToIgnoreCase("#  slice orientation ( TRA/SAG/COR )        (integer)")==0) {
                ori =Integer.valueOf(values[idx]);
            } else if(tag.compareToIgnoreCase("#  recon resolution (x y)                   (2*integer)")==0) {
                dim1 =Integer.valueOf(values[idx]);
                dim2 =Integer.valueOf(values[idx+1]);
            } else if(tag.compareToIgnoreCase("#  image pixel size (in bits)               (integer)")==0) {
                bpp = Integer.valueOf(values[idx]);
            } else if(tag.compareToIgnoreCase("#  rescale intercept                        (float)")== 0) {
                rescaleIntercept[0] = Float.valueOf(values[idx]);
            } else if(tag.compareToIgnoreCase("#  rescale slope                            (float)")==0) {
                rescaleSlope[0] = Float.valueOf(values[idx]);
            } else if(tag.compareToIgnoreCase("#  scale slope                              (float)")==0) {
                scaleSlope[0] = Float.valueOf(values[idx]);
            } else if(tag.compareToIgnoreCase("#  pixel spacing (x,y) (in mm)              (2*float)")==0) {
                res1[0] = Float.valueOf(values[idx]);
                res2[0] = Float.valueOf(values[idx+1]);
            }
       
            Integer I = (Integer)SliceMap.get(tag);
            if(I==null) {
                Preferences.debug("FilePARREC:readHeader. Bad slice tag;"+tag + "\n", Preferences.DEBUG_FILEIO);
                return false;
            }
            idx += I.intValue();
        }
        
        //need to figure out if this par/rec is sorted normally by volumes or by slices.  If its by slices then we will see all the
        //slice 1s then all the slice 2s.  etc.
        sl = (String)Slices.get(0);
        values = sl.split("\\s+");
        String sliceNumString1 = values[0]; //slice number is the 1st one 
        sl = (String)Slices.get(1);
        values = sl.split("\\s+");
        String sliceNumString2 = values[0];
        if(sliceNumString1.equals(sliceNumString2)) {
        	isSortedByVolumes = false;
        }
        
        
        // Let's parse the other slices for rescaleIntercept, rescaleSlope, scaleSlope, and resolutions:
        for (int i = 1; i < Slices.size(); i++) {
            sl = (String)Slices.get(i);
            values = sl.split("\\s+");
            idx =0;
            for(int j=0;j<SliceParameters.size();j++) {
                String tag = (String)SliceParameters.get(j);
                if(tag.compareToIgnoreCase("#  rescale intercept                        (float)")== 0) {
                    rescaleIntercept[i] = Float.valueOf(values[idx]);
                } else if(tag.compareToIgnoreCase("#  rescale slope                            (float)")==0) {
                    rescaleSlope[i] = Float.valueOf(values[idx]);
                } else if(tag.compareToIgnoreCase("#  scale slope                              (float)")==0) {
                    scaleSlope[i] = Float.valueOf(values[idx]);
                } else if(tag.compareToIgnoreCase("#  pixel spacing (x,y) (in mm)              (2*float)")==0) {
                    res1[i] = Float.valueOf(values[idx]);
                    res2[i] = Float.valueOf(values[idx+1]);
                }
                
                Integer I = (Integer)SliceMap.get(tag);
                if(I==null) {
                    Preferences.debug("FilePARREC:readHeader. Bad slice tag;"+tag + "\n", Preferences.DEBUG_FILEIO);
                    return false;
                }
                idx += I.intValue();
            }
        }
        sameSliceScalings = true;
        for (int j = 1; j < Slices.size(); j++) {
            if ((rescaleIntercept[j] != rescaleIntercept[0]) || (rescaleSlope[j] != rescaleSlope[0]) ||
                (scaleSlope[j] != scaleSlope[0])) {
                sameSliceScalings = false;
            }
        }
        if (sameSliceScalings) {
            Preferences.debug("FilePARREC:readHeader rescaleIntercept = " + rescaleIntercept[0] + "\n",
            		Preferences.DEBUG_FILEIO);
            Preferences.debug("FilePARREC:readHeader rescaleSlope = " + rescaleSlope[0] + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("FilePARREC:readHeader scaleSlope = " + scaleSlope[0] + "\n", Preferences.DEBUG_FILEIO);
        }
        else {
            for (int j = 0; j < Slices.size(); j++) {
                Preferences.debug("FilePARREC:readHeader rescaleIntercept[" + j + "] = " + rescaleIntercept[j] + "\n",
                		Preferences.DEBUG_FILEIO);
                Preferences.debug("FilePARREC:readHeader rescaleSlope[" + j + "] = " + rescaleSlope[j] + "\n",
                		Preferences.DEBUG_FILEIO);
                Preferences.debug("FilePARREC:readHeader scaleSlope[" + j + "] = " + scaleSlope[j] + "\n",
                		Preferences.DEBUG_FILEIO);    
            }
        }


        if(bpp==0) {
            Preferences.debug("FilePARREC:readHeader: BPP not specified" + "\n", Preferences.DEBUG_FILEIO);
            return false;
        }
        //Format of the "rec" file
        if(FileUtility.getExtension(getImageFiles()[0]).startsWith(".f")) {
            originalDataType = ModelStorageBase.FLOAT;
            fileInfo.setOriginalDataType(ModelStorageBase.FLOAT);
            fileInfo.setDataType(ModelStorageBase.FLOAT);
            Preferences.debug("FilePARREC:readHeader. Floating Point" + "\n", Preferences.DEBUG_FILEIO);
        } else {
            if(Integer.valueOf(bpp)==16) {
                originalDataType = ModelStorageBase.USHORT;
                fileInfo.setOriginalDataType(ModelStorageBase.USHORT);
                if ((scaleSlope[0] == 1.0f) && (rescaleIntercept[0] == 0.0f) && sameSliceScalings) {
                    fileInfo.setDataType(ModelStorageBase.USHORT);
                    Preferences.debug("FilePARREC:readHeader. Unsigned Short" + "\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    fileInfo.setDataType(ModelStorageBase.FLOAT);
                    Preferences.debug("FilePARREC: readHeader. Raw data USHORT will scale to Float\n", Preferences.DEBUG_FILEIO);
                }
            } else if(Integer.valueOf(bpp)==8) {
                originalDataType = ModelStorageBase.UBYTE;
                fileInfo.setOriginalDataType(ModelStorageBase.UBYTE);
                if ((scaleSlope[0] == 1.0f) && (rescaleIntercept[0] == 0.0f) && sameSliceScalings) {
                    fileInfo.setDataType(ModelStorageBase.UBYTE);
                    Preferences.debug("FilePARREC:readHeader. Unsigned BYTE" + "\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    fileInfo.setDataType(ModelStorageBase.FLOAT);
                    Preferences.debug("FilePARREC: readHeader. Raw data UBYTE will scale to Float\n", Preferences.DEBUG_FILEIO);
                }
                
            } else {
                Preferences.debug("FilePARREC:readHeader. Unknown bpp" + bpp + "\n", Preferences.DEBUG_FILEIO);;
                return false;
            }
        }


        if(dim1==0 || dim2==0) {
            Preferences.debug("FilePARREC:readHeader. Invalid slice dimension"+ "\n", Preferences.DEBUG_FILEIO);
            return false;
        }

        int[] Extents;
        if(numVolumes>1) {
            Extents = new int[] { dim1, dim2, numSlices, numVolumes };
        } else {
            Extents = new int[] { dim1, dim2, numSlices};

        }

        fileInfo.setExtents(Extents);
        fileInfo.setVolParameters(VolParameters);
        
        boolean res1Cons = res1[0] != resInit, res2Cons = res2[0] != resInit;
        float res1Val = res1[0];
        for(int i=0; i<res1.length; i++) {
            if(res1[i] != res1Val) {
                res1Cons = false;
                break;
            }
        }
        
        float res2Val = res2[0];
        for(int i=0; i<res2.length; i++) {
            if(res2[i] != res2Val) {
                res2Cons = false;
                break;
            }
        }

        switch(ori) {
            case 1: //TRA
                fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 1);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_I2S_TYPE, 2);
                if(res1Cons && res2Cons) {
                    fileInfo.setResolutions(res1Val, 0);
                    fileInfo.setResolutions(res2Val, 1);
                } else {
                    fileInfo.setResolutions(fovRL/dim1,0);
                    fileInfo.setResolutions(fovAP/dim2,1);
                }
                fileInfo.setResolutions(fovIS/numSlices,2);                
                if(numVolumes>1) {
                    if (repetitionTime > 0.0f) {
                        fileInfo.setResolutions(repetitionTime, 3);
                    }
                    else {
                        fileInfo.setResolutions(1,3);
                    }
                }     
//                fileInfo.setSliceThickness(fov2/numSlices);
                break;
            case 2: //SAG
                fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 0);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 1);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_L2R_TYPE, 2);
                if(res1Cons && res2Cons) {
                    fileInfo.setResolutions(res1Val, 0);
                    fileInfo.setResolutions(res2Val, 1);
                } else {
                    fileInfo.setResolutions(fovAP/dim1,0);
                    fileInfo.setResolutions(fovIS/dim2,1);
                }
                fileInfo.setResolutions(fovRL/numSlices,2);
                if(numVolumes>1) {
                    if (repetitionTime > 0.0f) {
                        fileInfo.setResolutions(repetitionTime, 3);
                    }
                    else {
                        fileInfo.setResolutions(1,3);
                    }
                }     
                //fileInfo.setSliceThickness(fov3/numSlices);
                break;
            case 3: //COR
                fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 1);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 2);
                if(res1Cons && res2Cons) {
                    fileInfo.setResolutions(res1Val, 0);
                    fileInfo.setResolutions(res2Val, 1);
                } else {
                    fileInfo.setResolutions(fovRL/dim1,0);
                    fileInfo.setResolutions(fovIS/dim2,1);
                }
                fileInfo.setResolutions(fovAP/numSlices,2);
                if(numVolumes>1) {
                    if (repetitionTime > 0.0f) {
                        fileInfo.setResolutions(repetitionTime, 3);
                    }
                    else {
                        fileInfo.setResolutions(1,3);
                    }
                }     
//                fileInfo.setSliceThickness(fov1/numSlices);
                break;

            default:
                Preferences.debug("FilePARREC:readHeader. Unknown Orientation;"+ori+ "\n", Preferences.DEBUG_FILEIO);
                return false;

        }
        
        double[] sliceAngle = fileInfo.getSliceAngulation();
        double[] offCentre = fileInfo.getOffCentre();
        
        if(sliceAngle != null && offCentre != null) {
            switch(fileInfo.getImageOrientation()) {
            
            case FileInfoBase.AXIAL:
                sliceAngle = new double[]{sliceAngle[2], sliceAngle[0], sliceAngle[1]};
                offCentre = new double[]{offCentre[2], offCentre[0], offCentre[1]};
                break;
                
            case FileInfoBase.SAGITTAL:
                sliceAngle = new double[]{sliceAngle[0], -sliceAngle[1], -sliceAngle[2]};
                offCentre = new double[]{offCentre[0], -offCentre[1], -offCentre[2]};
                break;
                
            case FileInfoBase.CORONAL:
                sliceAngle = new double[]{sliceAngle[2], -sliceAngle[1], sliceAngle[0]};
                offCentre = new double[]{offCentre[2], -offCentre[1], offCentre[0]};
                break;       
            }
            
            fileInfo.setSliceAngulation(sliceAngle);
            fileInfo.setOffCentre(offCentre);
        }
        
        float []o;
        if(numVolumes>1) {
            o = new float[4];
            for(int j=0;j<4;j++) o[j]=0;
        } else {
            o = new float[3];
            for(int j=0;j<3;j++) o[j]=0;
        }
        
        fileInfo.setOrigin(o);
        
        

        
        return true; // If it got this far, it has successfully read in the header
    }

    /**
     * Reads an PAR/REC image file by reading the header then making a FileRaw to read the image for all filenames in
     * the file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     *
     * @exception  java.io.IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        gov.nih.mipav.model.file.FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        boolean haveHeader = false;
        int k;
        int index;
        float scaleFactor[];
        float offsetAdd[];
        for(k=0;k<hdrEXTENSIONS.length;k++)
        {
            if (FileUtility.getExtension(fileName).equals(FilePARREC.hdrEXTENSIONS[k])) {
                haveHeader = true;
            }
        }
        if (haveHeader && (fileNames[1] != null)) {
            index = fileName.lastIndexOf('.');
            fileName = fileName.substring(0,index+1);
            index = fileNames[1].lastIndexOf('.');
            fileName = fileName.concat(fileNames[1].substring(index+1));
        } // if (haveHeader && (fileNames[1] != null))
        fileInfo = new FileInfoPARREC(fileName, fileDir, FileUtility.PARREC);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" PAR/REC header file error"));
        }

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                        fileInfo.getFileName());
            } else {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        // if vox units defines the units of measure, then use that instead
        // clones the file info
        updateUnitsOfMeasure(fileInfo, image);
        updateTransformMatrix(fileInfo, image);
//        updateStartLocations(image.getFileInfo());	


        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            linkProgress(rawFile);

            long offset = (long) Math.abs(vox_offset);

            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = offset + getOffset(fileInfo);
                }
            }

            if ((scaleSlope[0] == 1.0f) && (rescaleIntercept[0] == 0.0f) && sameSliceScalings) {
                rawFile.readImage(image, offset);
            }
            else {
                scaleFactor = new float[Slices.size()];  
                offsetAdd = new float[Slices.size()];
                for (int i = 0; i < Slices.size(); i++) {
                    scaleFactor[i] = 1.0f/scaleSlope[i];
                    offsetAdd[i] = rescaleIntercept[i]/(scaleSlope[i] * rescaleSlope[i]);
                }
                rawFile.readFloatImage(image, originalDataType, scaleFactor, offsetAdd, offset);
            }

            if (one) {
                fileInfo.setExtents(extents);
            }
        } catch (IOException error) {
            throw new IOException("FilePARREC: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }
        
        //we need to rearrange the buffer because the par/rec image is not sorted properly
        if(!isSortedByVolumes) {
        	int type;
        	type = image.getType();
        	int[] exts = image.getExtents();
        	int sliceLength = exts[0] * exts[1];
        	if(image.isColorImage()) {
        		sliceLength = sliceLength * 4;
        	}
        	int dataSizeLength = image.getDataSize();
        	byte[] sliceByteData = null;
        	byte[] newByteData = null;
        	short[] sliceShortData = null;
        	short[] newShortData = null;
        	float[] sliceFloatData = null;
        	float[] newFloatData = null;
        	int[] sliceIntData = null;
        	int[] newIntData = null;
        	long[] sliceLongData = null;
        	long[] newLongData = null;
        	int st = 0;
        	int newDataStart = 0;
        	float scaleSlope2[] = new float[Slices.size()];
        	float rescaleIntercept2[] = new float[Slices.size()];
        	float rescaleSlope2[] = new float[Slices.size()];
        	int newIndex = 0;
        	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE || type == ModelStorageBase.ARGB) {
        		sliceByteData = new byte[sliceLength];
        		newByteData = new byte[dataSizeLength];
        	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT || type == ModelStorageBase.ARGB_USHORT ) {
        		sliceShortData = new short[sliceLength];
        		newShortData = new short[dataSizeLength];
        	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
        		sliceIntData = new int[sliceLength];
        		newIntData = new int[dataSizeLength];
        	}else if(type == ModelStorageBase.LONG) {
        		sliceLongData = new long[sliceLength];
        		newLongData = new long[dataSizeLength];
        	}else if(type == ModelStorageBase.FLOAT || type == ModelStorageBase.ARGB_FLOAT) {
        		sliceFloatData = new float[sliceLength];
        		newFloatData = new float[dataSizeLength];	
        	}
        	

        	for(int start=0,counter=0;start<dataSizeLength;start=start+sliceLength,counter++) {
        		if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE) {
        			try {
          	        	 image.exportData(start, sliceLength, sliceByteData);
          	         	} catch (IOException error) {
          	             System.out.println("IO exception");
          	             return null;
          	         	}
   	       	         if(counter%numVolumes == 0 && start != 0) {
   	       	        	 st = st + sliceLength;
   	       	        	newDataStart = st;
   	       	        	newIndex = newDataStart/sliceLength;
   	       	         }
   	       	         scaleSlope2[newIndex] = scaleSlope[counter];
   	       	         rescaleIntercept2[newIndex] = rescaleIntercept[counter];
   	       	         rescaleSlope2[newIndex] = rescaleSlope[counter];
   	       	         for(int i=newDataStart,m=0;i<newDataStart+sliceLength;i++,m++) {
   	       	        	 newByteData[i] = sliceByteData[m];
   	       	         }
   	       	         newDataStart = newDataStart + (numSlices*sliceLength); 
        			
        		}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
        			try {
          	        	 image.exportData(start, sliceLength, sliceShortData);
          	         	} catch (IOException error) {
          	             System.out.println("IO exception");
          	             return null;
          	         	}
   	       	         if(counter%numVolumes == 0 && start != 0) {
   	       	        	 st = st + sliceLength;
   	       	        	 newDataStart = st;
   	       	             newIndex = newDataStart/sliceLength;
   	       	         }
   	       	         scaleSlope2[newIndex] = scaleSlope[counter];
                     rescaleIntercept2[newIndex] = rescaleIntercept[counter];
                     rescaleSlope2[newIndex] = rescaleSlope[counter];
   	       	         for(int i=newDataStart,m=0;i<newDataStart+sliceLength;i++,m++) {
   	       	        	 newShortData[i] = sliceShortData[m];
   	       	         }
   	       	         newDataStart = newDataStart + (numSlices*sliceLength);
        		}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
        			try {
          	        	 image.exportData(start, sliceLength, sliceIntData);
          	         	} catch (IOException error) {
          	             System.out.println("IO exception");
          	             return null;
          	         	}
   	       	         if(counter%numVolumes == 0 && start != 0) {
   	       	        	 st = st + sliceLength;
   	       	        	 newDataStart = st;
   	       	             newIndex = newDataStart/sliceLength;
   	       	         }
   	       	         scaleSlope2[newIndex] = scaleSlope[counter];
                     rescaleIntercept2[newIndex] = rescaleIntercept[counter];
                     rescaleSlope2[newIndex] = rescaleSlope[counter];
   	       	         for(int i=newDataStart,m=0;i<newDataStart+sliceLength;i++,m++) {
   	       	        	 newIntData[i] = sliceIntData[m];
   	       	         }
   	       	         newDataStart = newDataStart + (numSlices*sliceLength); 
        		}else if(type == ModelStorageBase.LONG) {
        			try {
          	        	 image.exportData(start, sliceLength, sliceLongData);
          	         	} catch (IOException error) {
          	             System.out.println("IO exception");
          	             return null;
          	         	}
   	       	         if(counter%numVolumes == 0 && start != 0) {
   	       	        	 st = st + sliceLength;
   	       	        	 newDataStart = st;
   	       	             newIndex = newDataStart/sliceLength;
   	       	         }
   	       	         scaleSlope2[newIndex] = scaleSlope[counter];
                     rescaleIntercept2[newIndex] = rescaleIntercept[counter];
                     rescaleSlope2[newIndex] = rescaleSlope[counter];
   	       	         for(int i=newDataStart,m=0;i<newDataStart+sliceLength;i++,m++) {
   	       	        	 newLongData[i] = sliceLongData[m];
   	       	         }
   	       	         newDataStart = newDataStart + (numSlices*sliceLength); 
        		}else if(type == ModelStorageBase.FLOAT) {
            		try {
       	        	 image.exportData(start, sliceLength, sliceFloatData);
       	         	} catch (IOException error) {
       	             System.out.println("IO exception");
       	             return null;
       	         	}
	       	         if(counter%numVolumes == 0 && start != 0) {
	       	        	 st = st + sliceLength;
	       	        	 newDataStart = st;
	       	        	 newIndex = newDataStart/sliceLength;
	       	         }
	       	         scaleSlope2[newIndex] = scaleSlope[counter];
                     rescaleIntercept2[newIndex] = rescaleIntercept[counter];
                     rescaleSlope2[newIndex] = rescaleSlope[counter];
	       	         for(int i=newDataStart,m=0;i<newDataStart+sliceLength;i++,m++) {
	       	        	 newFloatData[i] = sliceFloatData[m];
	       	         }
	       	         newDataStart = newDataStart + (numSlices*sliceLength); 
            	}
        	}
        	
        	for (int i = 0; i < Slices.size(); i++) {
        	    scaleSlope[i] = scaleSlope2[i];
        	    rescaleIntercept[i] = rescaleIntercept2[i];
        	    rescaleSlope[i] = rescaleSlope2[i];
        	}
        	scaleSlope2 = null;
        	rescaleIntercept2 = null;
        	rescaleSlope2 = null;
        	
        	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE) {
        		try {
     		    	image.importData(0, newByteData, true);
        		} catch (IOException error) {
     	            System.out.println("IO exception");
     	            error.printStackTrace();
     	            return null;
        		}
        	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
        		try {
     		    	image.importData(0, newShortData, true);
        		} catch (IOException error) {
     	            System.out.println("IO exception");
     	            error.printStackTrace();
     	            return null;
        		}
        	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
        		try {
     		    	image.importData(0, newIntData, true);
        		} catch (IOException error) {
     	            System.out.println("IO exception");
     	            error.printStackTrace();
     	            return null;
        		}
        	}else if(type == ModelStorageBase.LONG) {
        		try {
     		    	image.importData(0, newLongData, true);
        		} catch (IOException error) {
     	            System.out.println("IO exception");
     	            error.printStackTrace();
     	            return null;
        		}
        	}else if(type == ModelStorageBase.FLOAT) {
        		try {
     		    	image.importData(0, newFloatData, true);
        		} catch (IOException error) {
     	            System.out.println("IO exception");
     	            error.printStackTrace();
     	            return null;
        		}
        	}
        } //end if(!isSortedByVolumes)
        
        
        
        
        if (image != null) {
            image.calcMinMax();
            if (one) {
                fileInfo.setRescaleIntercept((double)rescaleIntercept[0]);
                fileInfo.setRescaleSlope((double)rescaleSlope[0]);
                fileInfo.setScaleSlope(scaleSlope[0]);
            }
            else {
                if (image.getFileInfo()[0] instanceof FileInfoPARREC) {
                    FileInfoPARREC[] fileInfoP = new FileInfoPARREC[Slices.size()];
                    for (int i = 0; i < Slices.size(); i++) {
                        fileInfoP[i] = (FileInfoPARREC)image.getFileInfo()[i];
                        fileInfoP[i].setRescaleIntercept((double)rescaleIntercept[i]);
                        fileInfoP[i].setRescaleSlope((double)rescaleSlope[i]);
                        fileInfoP[i].setScaleSlope(scaleSlope[i]);
                    }
                }
            }
        }
        
        
        if (dtiparams != null){
          //Saves DWI parameters from PAR file to DTIParameters object
            image.setDTIParameters(dtiparams);
        }
        
        
        

        return image;
    }
    
    
    
    

    /**
     * Reads in a PAR/REC image (first the header file, then the raw file)
     *
     * @param      buffer  Image buffer to store image data into.
     *
     * @exception  java.io.IOException  if there is an error reading the file
     *
     * @see        gov.nih.mipav.model.file.FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {
        int i;

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoPARREC(fileName, fileDir, FileUtility.ANALYZE);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
                throw (new IOException("Cannot read image because of PAR/REC header file error"));
            }
        }

        // if vox units defines the units of measure, then use that instead
        // updateUnitsOfMeasure(fileInfo);
        //only mm supported
        int units = Unit.MILLIMETERS.getLegacyNum(); //FileInfoBase.getUnitsOfMeasureFromStr(fileInfo.getVoxUnits());

        if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
        } else {
            fileInfo.setUnitsOfMeasure(units, 0);
            fileInfo.setUnitsOfMeasure(units, 1);
        }
        
        if (image != null) {
            updateTransformMatrix(fileInfo, image);
        }
        
        if (image != null) {
            FileInfoPARREC[] fileInfoP = (FileInfoPARREC[])image.getFileInfo();
            for (i = 0; i < Slices.size(); i++) {
                fileInfoP[i].setRescaleIntercept((double)rescaleIntercept[i]);
                fileInfoP[i].setRescaleSlope((double)rescaleSlope[i]);
                fileInfoP[i].setScaleSlope(scaleSlope[i]);
            }
        }


        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

            long offset = (long) Math.abs(vox_offset);
            rawFile.readImage(buffer, offset, fileInfo.getDataType());
            rawFile.raFile.close();

            if (vox_offset < 0.0f) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] = Math.abs(buffer[i]);
                }
            }

            //flipTopBottom(buffer, fileInfo);
        } catch (IOException error) {
            error.printStackTrace();
            throw new IOException("FilePARREC: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }

    /**
     * Writes a PAR/REC format image and header.
     *
     * @param      image  Image model of data to write.
     *
     * @exception  java.io.IOException  if there is an error writing the file
     *
     * @see        gov.nih.mipav.model.file.FileInfoAnalyze
     * @see        gov.nih.mipav.model.file.FileRaw
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        int i, k, seq;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();
        int beginTime = options.getBeginTime();
        int endTime = options.getEndTime();
        String headerFileName = fileNames[0];
        int headerIndex = fileNames[0].lastIndexOf(".");
        String baseHeaderName = fileNames[0].substring(0, headerIndex);
        String headerSuffix = fileNames[0].substring(headerIndex);
        int dataIndex = fileNames[1].lastIndexOf(".");
        String dataFileName = fileNames[1];
        String baseDataName = fileNames[1].substring(0, dataIndex);
        String dataSuffix = fileNames[1].substring(dataIndex);
        int optionsFileNameIndex;
        String optionsFileName;
        ModelImage image2;
        boolean rescale;
        
        if (changeToUnsignedInts) {
            int numSlices = 1;
            if (image.getNDims() > 2) {
                numSlices = numSlices * image.getExtents()[2];
                if (image.getNDims() > 3) {
                    numSlices = numSlices * image.getExtents()[3];
                }
            }
            int sliceSize = image.getExtents()[0] * image.getExtents()[1];
            float rescaleIntercept[] = new float[numSlices];
            float rescaleSlope[] = new float[numSlices];
            float scaleSlope[] = new float[numSlices];
            for (i = 0; i < numSlices; i++) {
                rescaleIntercept[i] = (float)fileInfoArray[i].getRescaleIntercept();
                rescaleSlope[i] = (float)fileInfoArray[i].getRescaleSlope();
                scaleSlope[i] = fileInfoArray[i].getScaleSlope();
            }
            float scaleFactor[] = new float[numSlices];  
            float offsetAdd[] = new float[numSlices];
            for (i = 0; i < numSlices; i++) {
                scaleFactor[i] = 1.0f/scaleSlope[i];
                offsetAdd[i] = rescaleIntercept[i]/(scaleSlope[i] * rescaleSlope[i]);
            }
            float floatBuffer[] = new float[sliceSize];
            int intBuffer[] = new int[sliceSize];
            image2 = new ModelImage(fileInfoArray[0].getOriginalDataType(), image.getExtents(), image.getImageName() + "_uint");
            for (i = 0; i < numSlices; i++) {
                fileInfoArray[i].setDataType(fileInfoArray[0].getOriginalDataType());
                image2.setFileInfo((FileInfoPARREC)fileInfoArray[i].clone(), i);
            }
            for (i = 0; i < numSlices; i++) {
                image.exportData(i * sliceSize, sliceSize, floatBuffer);
                for (k = 0; k < sliceSize; k++) {
                    intBuffer[k] = Math.round((floatBuffer[k] - offsetAdd[i])/scaleFactor[i]);
                }
                image2.importData(i * sliceSize, intBuffer, false);
            } // for (i = 0; i < numSlices; i++)
            image2.calcMinMax();
            rescale = true;
        } // if (changeToUnsignedInts)
        else if ((fileInfoArray[0].getOriginalDataType() != ModelStorageBase.FLOAT) && 
                (fileInfoArray[0].getDataType() == ModelStorageBase.FLOAT)) {
            int numSlices = 1;
            if (image.getNDims() > 2) {
                numSlices = numSlices * image.getExtents()[2];
                if (image.getNDims() > 3) {
                    numSlices = numSlices * image.getExtents()[3];
                }
            }
            image2 = (ModelImage)image.clone();
            for (i = 0; i < numSlices; i++) {
                fileInfoArray[i].setRescaleIntercept(0.0);
                fileInfoArray[i].setRescaleSlope(1.0);
                fileInfoArray[i].setScaleSlope(1.0f);
                image2.setFileInfo((FileInfoPARREC)fileInfoArray[i].clone(), i);
            } 
            rescale = false;
        }
        else {
            rescale = true;
            image2 = image;
        }
        
        if (options.isMultiFile() && image.getNDims() == 4) {
            for (k = beginTime, seq = options.getStartNumber(); k <= endTime; k++, seq++) {

                if (options.getDigitNumber() == 1) {
                    headerFileName = baseHeaderName + Integer.toString(seq) + headerSuffix;
                    dataFileName = baseDataName + Integer.toString(seq) + dataSuffix;
                } else if (options.getDigitNumber() == 2) {

                    if (seq < 10) {
                       headerFileName = baseHeaderName + "0" + Integer.toString(seq) + headerSuffix;
                       dataFileName = baseDataName + "0" + Integer.toString(seq) + dataSuffix;
                    } else {
                        headerFileName = baseHeaderName + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + Integer.toString(seq) + dataSuffix;
                    }
                } else if (options.getDigitNumber() == 3) {

                    if (seq < 10) {
                        headerFileName = baseHeaderName + "00" + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + "00" + Integer.toString(seq) + dataSuffix;
                    } else if (seq < 100) {
                        headerFileName = baseHeaderName + "0" + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + "0" + Integer.toString(seq) + dataSuffix;
                    } else {
                        headerFileName = baseHeaderName + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + Integer.toString(seq) + dataSuffix;
                    }
                } else if (options.getDigitNumber() == 4) {

                    if (seq < 10) {
                        headerFileName = baseHeaderName + "000" + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + "000" + Integer.toString(seq) + dataSuffix;
                    } else if (seq < 100) {
                        headerFileName = baseHeaderName + "00" + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + "00" + Integer.toString(seq) + dataSuffix;
                    } else if (seq < 1000) {
                        headerFileName = baseHeaderName + "0" + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + "0" + Integer.toString(seq) + dataSuffix;
                    } else {
                        headerFileName = baseHeaderName + Integer.toString(seq) + headerSuffix;
                        dataFileName = baseDataName + Integer.toString(seq) + dataSuffix;
                    }
                }
                // write header with image, # of images per, and 1 time slice

                writeHeader(image2, headerFileName, beginSlice, endSlice, k, k, rescale);
                optionsFileNameIndex = dataFileName.lastIndexOf("\\");
                optionsFileName = dataFileName.substring(optionsFileNameIndex+1);
                FileInfoXML tempInfo = new FileInfoImageXML(optionsFileName, options.getFileDirectory(), FileUtility.RAW);
                tempInfo.setEndianess(FileBase.LITTLE_ENDIAN); //FORCE LITTLE ENDIAN for rec/frec files!!!
                FileRaw rawFile = new FileRaw(dataFileName, "", tempInfo, FileBase.READ_WRITE);
                linkProgress(rawFile);
                rawFile.writeImage(image2, beginSlice, endSlice, k, k);

            } // end for loop

            
        }
        else {
    	    writeHeader(image2, fileNames[0], beginSlice, endSlice, beginTime, endTime, rescale);
    	    FileInfoXML tempInfo = new FileInfoImageXML(options.getFileName(), options.getFileDirectory(), FileUtility.RAW);
            tempInfo.setEndianess(FileBase.LITTLE_ENDIAN); //FORCE LITTLE ENDIAN for rec/frec files!!!
            FileRaw rawFile = new FileRaw(fileNames[1], "", tempInfo, FileBase.READ_WRITE);
            linkProgress(rawFile);
            rawFile.writeImage(image2, options);
        }
    	
        if (changeToUnsignedInts) {
            image2.disposeLocal();
            image2 = null;
        }
    	
    }


    /**
     * Helper method to calculate the offset for getting only the middle analyze image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset(FileInfoPARREC fileInfo) {
        int offset = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * (fileInfo.getExtents()[2] / 2);

        switch (originalDataType) {

            case ModelStorageBase.BOOLEAN:
            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
                offset *= 2;
                break;

            case ModelStorageBase.FLOAT:
            case ModelStorageBase.INTEGER:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
            case ModelStorageBase.COMPLEX:
                offset *= 8;
                break;

            case ModelStorageBase.ARGB:
                offset *= 3;
                break;

            case ModelStorageBase.ARGB_USHORT:
                offset *= 6;
                break;
        }

        return offset;
    }

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void updateStartLocations(FileInfoBase[] fileInfo) {
        int axisOrient;

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin);
                axisOrient = fileInfo[i].getAxisOrientation(2);

                if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                        (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                    origin[2] += resolutions[2];
                } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
                }
            }
        } else if (image.getNDims() == 4) {
            float tmp = origin[2];

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin);
                    axisOrient = fileInfo[i].getAxisOrientation(2);

                    if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                            (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                        origin[2] += resolutions[2];
                    } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                    }
                }

                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
    }

    /**
     * Updates the units of Measure in the file info based on the voxUnits from an Analyze Header.
     *
     * @param  fileInfo  -- an Analyze file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    private void updateUnitsOfMeasure(FileInfoPARREC fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

// if vox units defines the units of measure, then use that instead
//Only mm supported in PAR/REC
        int units = Unit.MILLIMETERS.getLegacyNum(); //FileInfoBase.getUnitsOfMeasureFromStr(fileInfo.getVoxUnits());

        if (image.getNDims() == 2) {

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
            }

            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
                fileInfo.setUnitsOfMeasure(units, 2);
            }

            for (int i = 0; i < extents[2]; i++) {
                FileInfoPARREC newFileInfo = (FileInfoPARREC) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                newFileInfo.setSliceInfo((String)Slices.get(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters and msec.
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
                fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 3);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
                fileInfo.setUnitsOfMeasure(units, 2);
                fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 3);
            }

            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoPARREC newFileInfo = (FileInfoPARREC) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                newFileInfo.setSliceInfo((String)Slices.get(i));
                
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }
//        System.out.println("NDIMS:"+image.getNDims());
        //image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo

    } // end updateUnitsOfMeasure()

    


    /**
     * Gets the header and image file names given a header or image file name
     * @param absolutePath header or image filename
     * @return array [0] headerfilename [1] imagefilename
     */
    public String[] getCompleteFileNameListDefault(String absolutePath) {
        String[] completeFileNameList = new String[2];

        if (FilePARREC.isHeaderFile(absolutePath)) {
            completeFileNameList[0] = absolutePath;

            String ext = FileUtility.getExtension((absolutePath));
            int k = 0;
            
            for (int i = 0; i < 4; i++) {
                if (ext.equals(hdrEXTENSIONS[i])) {
                    k = i;
                }
            }

            if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
            	completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) + FilePARREC.imgEXTENSIONS[k];
                completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) + FilePARREC.hdrEXTENSIONS[k];
            }
            
            
        } else if (FilePARREC.isImageFile(absolutePath)) {
            completeFileNameList[1] = absolutePath;

            //public static final String[] hdrEXTENSIONS = { ".par", ".PAR", ".parv2", ".PARv2" };
            //public static final String[] imgEXTENSIONS = { ".rec", ".REC", ".frec", ".fREC" };

            String ext = FileUtility.getExtension((absolutePath));
            
            int k = 0;
            
            for (int i = 0; i < 4; i++) {
                if (ext.equals(imgEXTENSIONS[i])) {
                    k = i;
                }
            }
            
            if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
            	completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) + FilePARREC.hdrEXTENSIONS[k];
            	completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) + FilePARREC.imgEXTENSIONS[k];
            }
        } else {
            completeFileNameList = null;
        }

        
        return completeFileNameList;
    }


    //todo: monitor options for cropped data
    /**
     * 
     * @param writeImage
     * @param headerFileName
     * @param beginSlice
     * @param endSlice
     * @param beginTime
     * @param endTime
     * @param rescale
     * @throws IOException
     */
    public void writeHeader(ModelImage writeImage, String headerFileName, int beginSlice, int endSlice,
                            int beginTime, int endTime, boolean rescale) throws IOException {
        int loc;
    	@SuppressWarnings("unused")
        int bpp=0;
    	@SuppressWarnings("unused")
        int ori=0;
        switch(outInfo.getDataType()) {
            case ModelStorageBase.FLOAT:
                bpp=32;
                break;
            case ModelStorageBase.USHORT :
                bpp=16;
                break;
            case ModelStorageBase.UBYTE:
                bpp=8;
                break;
            default: bpp=16;
        }
        switch(outInfo.getImageOrientation()) {
            case FileInfoBase.AXIAL:
                ori=1;
                break;
            case FileInfoBase.SAGITTAL:
                ori=2;
                break;
            case FileInfoBase.CORONAL:
                ori=3;
                break;
            default: ori=1;

        }
        int []extents = outInfo.getExtents();
        String version = outInfo.getVersion();
        //Set the number of slices
        
        PrintStream fp = new PrintStream(new File(headerFileName));
        fp.println("# === DATA DESCRIPTION FILE ======================================================");
        fp.println("#");
        fp.println("# CAUTION - Investigational device.");
        fp.println("# Limited by Federal Law to investigational use.");
        fp.println("#");
        fp.println("# Dataset name: Generated by MIPAV (MINIMAL FILE)");
        fp.println("#");
        fp.println("# CLINICAL TRYOUT             Research image export tool     " + version);
        fp.println("#");
        fp.println("# === GENERAL INFORMATION ========================================================");
        fp.println("# ");
        
        /*
        fp.println(".    Max. number of slices/locations    : "+extents[2]);
        switch(outInfo.getImageOrientation()) {
            case FileInfoBase.AXIAL:
                fp.println(".    FOV (ap,fh,rl) [mm]                : "+extents[0]*outInfo.getResolution(0)+
                        " "+extents[2]*outInfo.getResolution(2)+" "+extents[1]*outInfo.getResolution(1));
                break;
            case FileInfoBase.SAGITTAL:
                fp.println(".    FOV (ap,fh,rl) [mm]                : "+extents[1]*outInfo.getResolution(1)+
                        " "+extents[0]*outInfo.getResolution(0)+" "+extents[2]*outInfo.getResolution(2));
                break;
            case FileInfoBase.CORONAL:
                fp.println(".    FOV (ap,fh,rl) [mm]                : "+extents[1]*outInfo.getResolution(1)+
                        " "+extents[2]*outInfo.getResolution(2)+" "+extents[0]*outInfo.getResolution(0));
                break;
            default: ori=1;

        }
       */
        ArrayList<String> generalInfoList = outInfo.getGeneralInfoList();
        for(int i=0;i<generalInfoList.size();i++) {
        	String info = generalInfoList.get(i);
        	if (info.indexOf("Max. number of slices/locations") >= 0) {
        		info = ".    Max. number of slices/locations    :   " + String.valueOf(endSlice - beginSlice + 1);
        	}
        	fp.println(info);
        }
        
        /*HashMap VolParameters = outInfo.getVolParameters();
        fp.print(".    Patient name                       : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("info_patient_name");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Examination name                   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_exam_name");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Protocol name                      : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_protocol_name");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Examination date/time              : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("info_exam_datetime");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Series Type                        : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_series_type");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Acquisition nr                     : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_acquisitin_num");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Reconstruction nr                  : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_recon_num");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Scan Duration [sec]                : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_scan_dur");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Max. number of cardiac phases      : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("max_card_phs");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Max. number of echoes              : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("max_num_echo");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.println(".    Max. number of slices/locations    : "+extents[2]);
        
        fp.print(".    Max. number of dynamics            : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("max_num_dynamics");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Max. number of mixes               : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("max_num_mixes");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Patient position                   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("orient_patient_pos");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Preparation direction              : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("orient_prep_dir");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Technique                          : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_technique");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Scan resolution  (x, y)            : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_scan_res");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Scan mode                          : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_scan_mode");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Repetition time [ms]               : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_rep_time");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        switch(outInfo.getImageOrientation()) {
        case FileInfoBase.AXIAL:
            fp.println(".    FOV (ap,fh,rl) [mm]                : "+extents[0]*outInfo.getResolution(0)+
                    " "+extents[2]*outInfo.getResolution(2)+" "+extents[1]*outInfo.getResolution(1));
            break;
        case FileInfoBase.SAGITTAL:
            fp.println(".    FOV (ap,fh,rl) [mm]                : "+extents[1]*outInfo.getResolution(1)+
                    " "+extents[0]*outInfo.getResolution(0)+" "+extents[2]*outInfo.getResolution(2));
            break;
        case FileInfoBase.CORONAL:
            fp.println(".    FOV (ap,fh,rl) [mm]                : "+extents[1]*outInfo.getResolution(1)+
                    " "+extents[2]*outInfo.getResolution(2)+" "+extents[0]*outInfo.getResolution(0));
            break;
        default: ori=1;

        }
        
        fp.print(".    Water Fat shift [pixels]           : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("scn_water_fat_shift");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Angulation midslice(ap,fh,rl)[degr]: ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("orient_ang_midslice");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Off Centre midslice(ap,fh,rl) [mm] : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("orient_off_ctr_midslice");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Flow compensation <0=no 1=yes> ?   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_flow_comp");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Presaturation     <0=no 1=yes> ?   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_presatuaration");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Phase encoding velocity [cm/sec]   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("cardiac_phase_enc_vel");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    MTC               <0=no 1=yes> ?   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_mtc");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    SPIR              <0=no 1=yes> ?   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_spir");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    EPI factor        <0,1=no EPI>     : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_epi_factor");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Dynamic scan      <0=no 1=yes> ?   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_dynamic_scan");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Diffusion         <0=no 1=yes> ?   : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("diffusion_diffusion");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Diffusion echo time [ms]           : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_diffusion_echo_time");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Max. number of diffusion values    : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_max_num_diffusion_values");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Max. number of gradient orients    : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_max_num_gradient_orients");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }
        
        fp.print(".    Number of label types   <0=no ASL> : ");
        if(VolParameters != null) {
        	String s;
        	s = (String)VolParameters.get("special_num_of_label_types");
            if(s!=null) {
            	fp.println(s);
            }else {
            	fp.println("NULL");
            }
        }else {
        	fp.println("NULL");
        }*/
        
        
       
        
        
        
        
        
        
        
        
        

        fp.println("#");
        fp.println("# === PIXEL VALUES =============================================================");
        fp.println("#  PV = pixel value in REC file, FP = floating point value, DV = displayed value on console");
        fp.println("#  RS = rescale slope,           RI = rescale intercept,    SS = scale slope ");
        fp.println("#  DV = PV * RS + RI             FP = DV / (RS * SS) ");
        fp.println("# ");
        fp.println("# === IMAGE INFORMATION DEFINITION =============================================");
        fp.println("#  The rest of this file contains ONE line per image, this line contains the following information:");
        fp.println("#");
        
        SliceMap = buildParSliceMap();
        ArrayList<String> imageInfoList = outInfo.getImageInfoList();
        int idx = 0;
        int xyIndex = -1;
        int orIndex = -1;
        int riIndex = -1;
        int rsIndex = -1;
        int ssIndex = -1;
        for(int i=0;i<imageInfoList.size();i++) {
        	String info = imageInfoList.get(i);
        	if(info.compareToIgnoreCase("#  recon resolution (x y)                   (2*integer)")==0) {
        		xyIndex = idx;
        	}
        	else if(info.indexOf("#  slice orientation ( TRA/SAG/COR )        (integer)")>=0) {
        		orIndex = idx;
        	}
        	else if (info.compareToIgnoreCase("#  rescale intercept                        (float)") == 0) {
        	    riIndex = idx;
        	}
        	else if (info.compareToIgnoreCase("#  rescale slope                            (float)") == 0) {
        	    rsIndex = idx;
        	}
        	else if (info.compareToIgnoreCase("#  scale slope                              (float)") == 0) {
        	    ssIndex = idx;
        	}
        	Integer I = (Integer)SliceMap.get(info);
            if(I==null) {
                Preferences.debug("FilePARREC:writeHeader. Bad slice info;"+info + "\n", Preferences.DEBUG_FILEIO);
                return;
            }
            idx += I.intValue();
        	fp.println(info);
        }
        
        /*fp.println("#  slice number                             (integer)");
        fp.println("#  echo number                              (integer)");
        fp.println("#  dynamic scan number                      (integer)");
        fp.println("#  cardiac phase number                     (integer)");
        fp.println("#  image_type_mr                            (integer)");
        fp.println("#  scanning sequence                        (integer)");
        fp.println("#  index in REC file (in images)            (integer)");
        fp.println("#  image pixel size (in bits)               (integer)");
        fp.println("#  scan percentage                          (integer)");
        fp.println("#  recon resolution (x y)                   (2*integer)");
        fp.println("#  rescale intercept                        (float)");
        fp.println("#  rescale slope                            (float)");
        fp.println("#  scale slope                              (float)");
        fp.println("#  window center                            (integer)");
        fp.println("#  window width                             (integer)");
        fp.println("#  image angulation (ap,fh,rl in degrees )  (3*float)");
        fp.println("#  image offcentre (ap,fh,rl in mm )        (3*float)");
        fp.println("#  slice thickness (in mm )                 (float)");
        fp.println("#  slice gap (in mm )                       (float)");
        fp.println("#  image_display_orientation                (integer)");
        fp.println("#  slice orientation ( TRA/SAG/COR )        (integer)");
        fp.println("#  fmri_status_indication                   (integer)");
        fp.println("#  image_type_ed_es  (end diast/end syst)   (integer)");
        fp.println("#  pixel spacing (x,y) (in mm)              (2*float)");
        fp.println("#  echo_time                                (float)");
        fp.println("#  dyn_scan_begin_time                      (float)");
        fp.println("#  trigger_time                             (float)");
        fp.println("#  diffusion_b_factor                       (float)");
        fp.println("#  number of averages                       (integer)");
        fp.println("#  image_flip_angle (in degrees)            (float)");
        fp.println("#  cardiac frequency   (bpm)                (integer)");
        fp.println("#  minimum RR-interval (in ms)              (integer)");
        fp.println("#  maximum RR-interval (in ms)              (integer)");
        fp.println("#  TURBO factor  <0=no turbo>               (integer)");
        fp.println("#  Inversion delay (in ms)                  (float)");
        fp.println("#  diffusion b value number    (imagekey!)  (integer)");
        fp.println("#  gradient orientation number (imagekey!)  (integer)");
        fp.println("#  contrast type                            (string)");
        fp.println("#  diffusion anisotropy type                (string)");
        fp.println("#  diffusion (ap, fh, rl)                   (3*float)");
        fp.println("#  label type (ASL)            (imagekey!)  (integer)");*/
        fp.println("#");
        fp.println("# === IMAGE INFORMATION ==========================================================");
        fp.println("#  sl ec  dyn ph ty    idx pix scan% rec size                (re)scale              window        angulation              offcentre        thick   gap   info      spacing     echo     dtime   ttime    diff  avg  flip    freq   RR-int  turbo delay b grad cont anis         diffusion       L.ty");
        fp.println("");

        
        /**
        if(extents.length>3) {
            for(int k=options.getBeginTime();k<=options.getEndTime();k++) {
                for(int j=1;j<=extents[2];j++) {

                    fp.println(j+" "+k+" "+bpp+" "+extents[0]+" "+extents[1]+
                            " "+outInfo.getResolution(2)+" "+0+" "+ori);
                }
            }
        } else {
            for(int j=1;j<=extents[2];j++) {

                fp.println(j+" "+1+" "+bpp+" "+extents[0]+" "+extents[1]+
                        " "+outInfo.getResolution(2)+" "+0+" "+ori);
            }
        }**/
        //Vector Slices = outInfo.getSlices();
        //Vector SliceParameters = outInfo.getSliceParameters();
        //following info is slice specific....so can not get it only from outInfo
        
        
        for (int i = beginTime; i <= endTime; i++) {
            for (int j = beginSlice; j <= endSlice; j++) {
                loc = j + i*extents[2];
                FileInfoPARREC fileInfoPR = (FileInfoPARREC)writeImage.getFileInfo(loc);
                String tag = fileInfoPR.getSliceInfo();
                if ((xyIndex >= 0) || (orIndex >= 0) || ((!rescale) && ((riIndex >= 0) || (rsIndex >= 0) || (ssIndex >= 0)))) {
                	String[] values = tag.split("\\s+");
                	if (xyIndex >= 0) {
                	    values[xyIndex] = String.valueOf(extents[0]);
                	    values[xyIndex+1] = String.valueOf(extents[1]);
                	}
                	if (orIndex >= 0) {
                		if (writeImage.getImageOrientation() == FileInfoBase.AXIAL) {
                			values[orIndex] = String.valueOf(1);
                		}
                		else if (writeImage.getImageOrientation() == FileInfoBase.SAGITTAL) {
                			values[orIndex] = String.valueOf(2);
                		}
                		else if (writeImage.getImageOrientation() == FileInfoBase.CORONAL) {
                			values[orIndex] = String.valueOf(3);
                		}
                	}
                	if (!rescale) {
                	    if (riIndex >= 0) {
                	        values[riIndex] = String.valueOf(0.0);
                	    }
                	    if (rsIndex >= 0) {
                	        values[rsIndex] = String.valueOf(1.0);
                	    }
                	    if (ssIndex >= 0) {
                	        values[ssIndex] = String.valueOf(1.0);
                	    }
                	} // if (!rescale)
                	tag = values[0] + "  ";
                	for (int k = 1; k < values.length-1; k++) {
                	    tag += values[k] + "  ";	
                	}
                	tag = tag + values[values.length-1];
                }   
                fp.println(tag);
            }
        }
        
        
        
	       /* if(Slices != null) {
	        	for(int j=0;j<Slices.size();j++) {
	        		String tag = (String)Slices.get(j);
	                //String tag = (String)SliceParameters.get(j);
	                fp.println(tag);
	        	}
	        }*/



        fp.println("# === END OF DATA DESCRIPTION FILE ===============================================");

        fp.close();
    }

    
    private HashMap<String,String> buildParVolMap() {
        HashMap<String,String> map = new HashMap<String,String>();
        map.put(".    Patient name","info_patient_name");
        map.put(".    Examination name","scn_exam_name");
        map.put(".    Protocol name","scn_protocol_name");
        map.put(".    Examination date/time","info_exam_datetime");
        map.put(".    Acquisition nr","scn_acquisitin_num");
        map.put(".    Reconstruction nr","scn_recon_num");
        map.put(".    Scan Duration [sec]","scn_scan_dur");
        map.put(".    Max. number of cardiac phases","max_card_phs");
        map.put(".    Max. number of echoes","max_num_echo");
        map.put(".    Max. number of slices/locations","max_num_slices");
        map.put(".    Max. number of dynamics","max_num_dynamics");
        map.put(".    Max. number of mixes","max_num_mixes");
        map.put(".    Image pixel size [8 or 16 bits]","scn_pix_bits");
        map.put(".    Technique","scn_technique");
        map.put(".    Scan mode","scn_scan_mode");
        map.put(".    Scan resolution  (x, y)","scn_scan_res");
        map.put(".    Scan percentage","scn_scan_pct");
        map.put(".    Recon resolution (x, y)","scn_recon_res");
        map.put(".    Number of averages","scn_NEX");
        map.put(".    Repetition time [msec]","scn_rep_time");
        map.put(".    FOV (ap,fh,rl) [mm]","scn_fov");
        map.put(".    Slice thickness [mm]","scn_slicethk");
        map.put(".    Slice gap [mm]","scn_slicegap");
        map.put(".    Water Fat shift [pixels]","scn_water_fat_shift");
        map.put(".    Angulation midslice(ap,fh,rl)[degr]","orient_ang_midslice");
        map.put(".    Off Centre midslice(ap,fh,rl) [mm]","orient_off_ctr_midslice");
        map.put(".    Flow compensation <0=no 1=yes> ?","special_flow_comp");
        map.put(".    Presaturation     <0=no 1=yes> ?","special_presatuaration");
        map.put(".    Cardiac frequency","cardiac_cardiac_freq");
        map.put(".    Min. RR interval","cardiac_min_rr_int");
        map.put(".    Max. RR interval","cardiac_max_rr_int");
        map.put(".    Phase encoding velocity [cm/sec]","cardiac_phase_enc_vel");
        map.put(".    MTC               <0=no 1=yes> ?","special_mtc");
        map.put(".    SPIR              <0=no 1=yes> ?","special_spir");
        map.put(".    EPI factor        <0,1=no EPI>","special_epi_factor");
        map.put(".    TURBO factor      <0=no turbo>","special_turbo_factor");
        map.put(".    Dynamic scan      <0=no 1=yes> ?","special_dynamic_scan");
        map.put(".    Diffusion         <0=no 1=yes> ?","diffusion_diffusion");
        map.put(".    Diffusion echo time [msec]","diffusion_diffusion_echo");
        map.put(".    Inversion delay [msec]","special_inversion_delay");
//... % Variables for May 31, 2005
        map.put(".    Series Type","scn_series_type");
        map.put(".    Patient position","orient_patient_pos");
        map.put(".    Preparation direction","orient_prep_dir");
        map.put(".    Repetition time [ms]","scn_rep_time");
        map.put(".    Diffusion echo time [ms]","special_diffusion_echo_time");
//... % Variables for December 29, 2006 (release 2.1)
        map.put(".    Max. number of diffusion values","special_max_num_diffusion_values");
        map.put(".    Max. number of gradient orients","special_max_num_gradient_orients");
//...%Variables for Feb 12, 2008
        map.put(".    Number of label types   <0=no ASL>", "special_num_of_label_types");
        return map;
    }

    private HashMap<String,Integer> buildParSliceMap() {
        HashMap<String,Integer> map = new HashMap<String,Integer>();
        map.put("#  slice number                             (integer)",new Integer(1));
        map.put("#  echo number                              (integer)",new Integer(1));
        map.put("#  dynamic scan number                      (integer)",new Integer(1));
        map.put("#  cardiac phase number                     (integer)",new Integer(1));
        map.put("#  image_type_mr                            (integer)",new Integer(1));
        map.put("#  scanning sequence                        (integer)",new Integer(1));
        map.put("#  index in REC file (in images)            (integer)",new Integer(1));
        map.put("#  image pixel size (in bits)               (integer)",new Integer(1));
        map.put("#  scan percentage                          (integer)",new Integer(1));
        map.put("#  recon resolution (x y)                   (2*integer)",new Integer(2));
        map.put("#  rescale intercept                        (float)",new Integer(1));
        map.put("#  rescale slope                            (float)",new Integer(1));
        map.put("#  scale slope                              (float)",new Integer(1));
        map.put("#  window center                            (integer)",new Integer(1));
        map.put("#  window width                             (integer)",new Integer(1));
        map.put("#  image angulation (ap,fh,rl in degrees )  (3*float)",new Integer(3));
        map.put("#  image offcentre (ap,fh,rl in mm )        (3*float)",new Integer(3));
        map.put("#  slice thickness (in mm )                 (float)",new Integer(1));
        map.put("#  slice gap (in mm )                       (float)",new Integer(1));
        map.put("#  image_display_orientation                (integer)",new Integer(1));
        map.put("#  slice orientation ( TRA/SAG/COR )        (integer)",new Integer(1));
        map.put("#  fmri_status_indication                   (integer)",new Integer(1));
        map.put("#  image_type_ed_es  (end diast/end syst)   (integer)",new Integer(1));
        map.put("#  pixel spacing (x,y) (in mm)              (2*float)",new Integer(2));
        map.put("#  echo_time                                (float)",new Integer(1));
        map.put("#  dyn_scan_begin_time                      (float)",new Integer(1));
        map.put("#  trigger_time                             (float)",new Integer(1));
        map.put("#  diffusion_b_factor                       (float)",new Integer(1));
        map.put("#  number of averages                       (integer)",new Integer(1));
        map.put("#  image_flip_angle (in degrees)            (float)",new Integer(1));
        map.put("#  cardiac frequency   (bpm)                (integer)",new Integer(1));
        map.put("#  minimum RR-interval (in ms)              (integer)",new Integer(1));
        map.put("#  maximum RR-interval (in ms)              (integer)",new Integer(1));
        map.put("#  TURBO factor  <0=no turbo>               (integer)",new Integer(1));
        map.put("#  Inversion delay (in ms)                  (float)",new Integer(1));
//... % new columns for December 29, 2006 (release 2.1)
        map.put("#  diffusion b value number    (imagekey!)  (integer)",new Integer(1));
        map.put("#  gradient orientation number (imagekey!)  (integer)",new Integer(1));
        map.put("#  contrast type                            (string)",new Integer(1));
        map.put("#  diffusion anisotropy type                (string)",new Integer(1));
        map.put("#  diffusion (ap, fh, rl)                   (3*float)",new Integer(3));
//...%new columns for Feb 12, 2008
        map.put("#  label type (ASL)            (imagekey!)  (integer)", new Integer(1));
        return map;
    };
	
	
	
    /*String [][] bvalues = new String[Slices.size()/numSlices][numSlices];
    int numCount = 0;     
    for (int i = 0; i < Slices.size()/numSlices; i++){          
        for (int j = 0; j < numSlices; j++){
            String sliceIndex = Slices.get((numCount*numSlices)+j);
            sliceIndex = sliceIndex.trim();
            final String[] sliceArr = sliceIndex.split("\\s+");
            bvalues[i][j] = sliceArr[27];
            //System.out.println("bvalues4.1:" +bvalues[1][j]);


}
numCount++;
}*/

/*//Convert bvalue String array to bvalue int array from V3 par/rec file
double flBvalueArray[][] = new double[Slices.size()/numSlices][numSlices];
for (int i = 0; i < Slices.size()/numSlices; i++){          
for (int j = 0; j <numSlices ; j++){
    flBvalueArray[i][j] = Double.parseDouble(bvalues[i][j]);
   
   
}
}

fileInfo.setBvalues(flBvalueArray);
//Test intBvalueArray
for (int i = 0; i < Slices.size()/numSlices; i++){
System.out.println("intBvalueArray index: " +flBvalueArray[i][1]);
}*/
    
    
}
