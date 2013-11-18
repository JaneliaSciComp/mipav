package gov.nih.mipav.model.file;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Arrays;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogScriptableTransform;

/**
 * Reads a spar/sdat file.  MIPAV only uses the spar information since the sdat is spectroscopy data.
 * 
 * @author senseneyj
 *
 */
public class FileSpar extends FileBase {

    /** The extensions of ANALYZE file. */
    public static final String[] hdrEXTENSIONS = { ".spar", ".SPAR"};
    public static final String[] imgEXTENSIONS = { ".sdat", ".SDAT"};
    
    /** File directory of the image. */
    private String fileDir;
    
    /** File name of the image. */
    private String fileName;

    /** The image read in from the file. */
    private ModelImage image;
    
    /** file info */
    private FileInfoSPAR outInfo;
    
    /** imageA file info */
    private FileInfoBase imageAInfo;
    
    /** Reference fileinfo */
    private FileInfoSPAR fileInfo;
    
    public FileSpar(String fileName, String fileDir) {
        this(null, fileName, fileDir);
    }
    
    public FileSpar(FileInfoBase imageAInfo, String fileName, String fileDir) {
        
        if(!isHeaderFile(fileDir + fileName)) {
            String[] list = getCompleteFileNameList(fileDir + fileName);
            String match = fileName.substring(0, fileName.lastIndexOf("."));
            for(String str : list) {
                if(str.toLowerCase().contains(".spar") && str.contains(match)) {
                    fileName = match + ".spar";
                    break;
                }
            }
        }
        
        this.imageAInfo = imageAInfo;
        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    private static double parseDouble(String nextLine) {
        String info = nextLine.substring(nextLine.indexOf(":")+1);
        info=info.trim();
        return Double.parseDouble(info.substring(0));
    }
    
    private static int parseInt(String nextLine) {
        String info = nextLine.substring(nextLine.indexOf(":")+1);
        info=info.trim();
        return Integer.parseInt(info.substring(0));
    }
    
    /**
     * Reads the spar header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image (is also the spar file).
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  java.io.IOException  if there is an error reading the header
     *
     * @see        gov.nih.mipav.model.file.FileInfoAnalyze
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {

        File fileHeader = new File(fileDir + imageFileName);
        
        //Open the Header File//
        try {
            raFile = new RandomAccessFile(fileHeader, "r");
        } catch (FileNotFoundException e) {
            Preferences.debug("raFile = new RandomAccessFile(fileHeader, r) gave " + "FileNotFoundException " + e,
                    Preferences.DEBUG_FILEIO);
            throw new IOException("Error on raFile = new RandomAccessFile(fileHeader,r)");
        }
        
        fileInfo = new FileInfoSPAR(imageFileName, fileDir, FileUtility.SPAR);

        String nextLine = raFile.readLine();
        
        //until image is created, is defined as [ap, lr, cc] (these will change depending on orientation
        double[] offCentre = new double[3];
        double[] angulation = new double[3];
        int[] extents = new int[3];
        int ori  = FileInfoBase.SAGITTAL;
        //String version;
        //String[] versionNumber;

        while(nextLine != null) {
            nextLine = nextLine.trim();
            if(nextLine.length()<1) { // Blank line = comment
                nextLine = raFile.readLine();
                continue;
            }
            
            if(nextLine.contains("ap_off_center")){
                offCentre[0] = parseDouble(nextLine);
            } else if(nextLine.contains("lr_off_center")) {
                offCentre[1] = parseDouble(nextLine);
            } else if(nextLine.contains("cc_off_center")) {
                offCentre[2] = parseDouble(nextLine);
            } else if(nextLine.contains("ap_angulation")) {
                angulation[0] = parseDouble(nextLine);
            } else if(nextLine.contains("lr_angulation")) {
                angulation[1] = parseDouble(nextLine);
            } else if(nextLine.contains("cc_angulation")) {
                angulation[2] = parseDouble(nextLine);
            } else if(nextLine.contains("ap_size")) {
                extents[0] = parseInt(nextLine);
            } else if(nextLine.contains("lr_size")) {
                extents[1] = parseInt(nextLine);
            } else if(nextLine.contains("cc_size")) {
                extents[2] = parseInt(nextLine);
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
        
        ori = FileInfoBase.AXIAL;
        
        if(imageAInfo != null) {
            ori = imageAInfo.getImageOrientation();
            fileInfo.setResolutions(Arrays.copyOf(imageAInfo.getResolutions(), imageAInfo.getResolutions().length));
        }

        fileInfo.setImageOrientation(ori);

        double[] imageOffCentre = new double[3];
        double[] imageAngulation = new double[3];
        int[] maskExtents = new int[3];
        
        switch(ori) {
            case FileInfoBase.AXIAL: //TRA
                fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 1);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_I2S_TYPE, 2);
               
                maskExtents[0] = extents[1];
                maskExtents[1] = extents[0];
                maskExtents[2] = extents[2];
                
                imageOffCentre[0] = offCentre[1];
                imageOffCentre[1] = offCentre[0];
                imageOffCentre[2] = offCentre[2];
                
                imageAngulation[0] = angulation[1];
                imageAngulation[1] = angulation[0];
                imageAngulation[2] = angulation[2];
                
                break;
            case FileInfoBase.SAGITTAL: //SAG
                fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 0);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 1);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 2);
                
                maskExtents[0] = extents[0];
                maskExtents[1] = extents[2];
                maskExtents[2] = extents[1];
                
                imageOffCentre[0] = offCentre[0];
                imageOffCentre[1] = -offCentre[2];
                imageOffCentre[2] = offCentre[1];
                
                imageAngulation[0] = angulation[0];
                imageAngulation[1] = -angulation[2];
                imageAngulation[2] = angulation[1];
                break;
            case FileInfoBase.CORONAL: //COR
                fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 1);
                fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 2);
                
                maskExtents[0] = extents[1];
                maskExtents[1] = extents[2];
                maskExtents[2] = extents[0];
                
                imageOffCentre[0] = offCentre[1];
                imageOffCentre[1] = -offCentre[2];
                imageOffCentre[2] = offCentre[0];
                
                imageAngulation[0] = angulation[1];
                imageAngulation[1] = -angulation[2];
                imageAngulation[2] = angulation[0];
                
                break;

            default:
                Preferences.debug("FileSPAR:readHeader. Unknown Orientation;"+ori+ "\n", Preferences.DEBUG_FILEIO);
                return false;

        }
        
        fileInfo.setOffCentre(Arrays.copyOf(imageOffCentre, imageOffCentre.length));
        fileInfo.setAngulation(Arrays.copyOf(imageAngulation, imageAngulation.length));
        fileInfo.setMaskExtents(Arrays.copyOf(maskExtents, maskExtents.length));
        fileInfo.setDataType(DataType.BOOLEAN.getLegacyNum());
        
        if(imageAInfo != null) {
            fileInfo.setExtents(Arrays.copyOf(imageAInfo.getExtents(), imageAInfo.getExtents().length));
            fileInfo.setOrigin(Arrays.copyOf(imageAInfo.getOrigin(), imageAInfo.getOrigin().length));
        } else {
            fileInfo.setExtents(Arrays.copyOf(maskExtents, maskExtents.length));
        }

        return true; // If it got this far, it has successfully read in the header
    }
    
    private int setLowerExtents(int index) {
        int bound = (int) (fileInfo.getExtents()[index]/2.0-(fileInfo.getMaskExtents()[index]/2.0));
        if(bound < 0) {
            bound = 0;
        }
        return bound;
    }
    
    private int setUpperExtents(int index) {
        int bound = (int) (fileInfo.getExtents()[index]/2.0+(fileInfo.getMaskExtents()[index]/2.0));
        if(bound >= fileInfo.getExtents()[index]) {
            bound = fileInfo.getExtents()[index];
        }
        return bound;
    }
    
    /**
     * Constructs a SPAR image mask based on the parameters defined in FileInfoSPAR
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
        
        if(fileInfo == null) {
            if(!readHeader(fileName, fileDir)) {
                throw (new IOException(" SPAR header file error"));
            }
        }
        
        try {
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
        } catch (OutOfMemoryError error) {
            throw (error);
        }
        
        for(int i=0; i<image.getFileInfo().length; i++) {
            image.setFileInfo(fileInfo, i);
        }
        
        int lowerX = setLowerExtents(0);
        int lowerY = setLowerExtents(1);
        int lowerZ = setLowerExtents(2);
        int upperX = setUpperExtents(0); //exclusive bound
        int upperY = setUpperExtents(1);
        int upperZ = setUpperExtents(2);
        
        for(int i=lowerX; i<upperX; i++) {
            for(int j=lowerY; j<upperY; j++) {
                for(int k=lowerZ; k<upperZ; k++) {
                    image.set(i, j, k, true);
                }
            }
        }
        
        image = updateTransformMatrix(image);

        if (image != null) {
            image.calcMinMax();
        }

        return image;
    }
    
    public FileInfoSPAR getFileInfo() {
		return outInfo;
	}


    private ModelImage updateTransformMatrix(ModelImage image) {
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
        
        
        if (imageAInfo == null) {
            image.setMatrix(tr);
            return image;
        }
        
        ModelImage imageA = null;
        try {
            imageA = ViewUserInterface.getReference().getRegisteredImageByName(imageAInfo.getFileName().substring(0, imageAInfo.getFileName().lastIndexOf(".")));
        }
        catch (Exception e) {
            String fileDirectory = imageAInfo.getFileDirectory();
            if (fileDirectory == null) {
                return null;
            }
            // If the string ends with a File.separator, strip out the final FileSeparator
            if ((fileDirectory.lastIndexOf(File.separator)) == fileDirectory.length() - 1) {
                fileDirectory = fileDirectory.substring(0, fileDirectory.length() - 1);
            }
            int lastSeparatorIndex = fileDirectory.lastIndexOf(File.separator);
            fileDirectory = fileDirectory.substring(lastSeparatorIndex + 1);
            int periodIndex = fileDirectory.indexOf(".");
            if (periodIndex > 0) {
                fileDirectory = fileDirectory.substring(0, periodIndex);
            }
            fileDirectory = "_" + fileDirectory;
            imageA = ViewUserInterface.getReference().getRegisteredImageByName(fileDirectory);
        }
        
        if(image != imageA) {
            TransMatrix aTrans = new TransMatrix(imageA.getMatrix());
            if (imageA.getFileInfo()[0] instanceof FileInfoDicom) {
                originX = imageA.getFileInfo()[0].getOrigin(0);
                originY = imageA.getFileInfo()[0].getOrigin(1);
                originZ = imageA.getFileInfo()[0].getOrigin(2);
                switch(ori) {
                    case FileInfoBase.AXIAL:
                        // R-L, A-P, I-S
                        aTrans.M03 = (float)(originX);
                        aTrans.M13 = (float)(originY);
                        aTrans.M23 = (float)(originZ);
                        break;
                    case FileInfoBase.SAGITTAL:
                        // A-P, S-I, L-R
                        aTrans.M03 = (float)(originZ);
                        aTrans.M13 = (float)(originX);
                        aTrans.M23 = (float)(originY);
                        break;
                    case FileInfoBase.CORONAL:
                        // R-L, S-I, A-P
                        aTrans.M03 = (float)(originX);
                        aTrans.M13 = (float)(originZ);
                        aTrans.M23 = (float)(originY);
                }
            }
            aTrans.Inverse();
            aTrans.mult(tr);
            
            JDialogScriptableTransform transform = new JDialogScriptableTransform(null, image);
            transform.setPadFlag(false);
            transform.setMatrix(aTrans);
            transform.setImage25D(false);
            transform.setSeparateThread(false);
            transform.setClipFlag(true);
            transform.setDimAndResXYZ();
            transform.setUnits(imageA.getUnitsOfMeasure());
            transform.setQuietRunning(true);
            transform.setOutDimensions(imageA.getExtents());
            transform.setOutResolutions(imageA.getResolutions(0));
            transform.setUpdateOrigin(true);
            transform.setInterp(AlgorithmTransform.NEAREST_NEIGHBOR);
            transform.actionPerformed(new ActionEvent(this, 0, "Script"));
            
            //ViewJFrameImage view = new ViewJFrameImage(transform.getResultImage());
            //view.setVisible(true);
            
            ModelImage resultImage = transform.getResultImage();
            resultImage.getMatrixHolder().replaceMatrices(imageA.getMatrixHolder().getMatrices());
            for (i = 0; i < resultImage.getExtents()[2]; i++) {
                resultImage.getFileInfo(i).setOrigin(imageA.getFileInfo()[i].getOrigin());
            }
            
           
            return resultImage;
        }
        
        return image; //no transformation is possible
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
        if (isHeaderFile(absolutePath)) {
            completeFileNameList[0] = absolutePath;

            // Try all extensions until one has a file that exists
            // completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(".")) + EXTENSIONS[1];
            for (int k = 0; k < imgEXTENSIONS.length; k++) {

                if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
                    completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) +
                        imgEXTENSIONS[k];
                } else {
                    completeFileNameList[1] = absolutePath;
                }
                
                File fp = new File(completeFileNameList[1]);

                if (fp.exists()) {
                    break;
                }
            }
        } else if (isImageFile(absolutePath)) {
            completeFileNameList[1] = absolutePath;

            // Try all extensions until one has a file that exists
            // completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(".")) + EXTENSIONS[0];
            for(int k=0;k<hdrEXTENSIONS.length;k++) {
                
                if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
                    completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) +
                        hdrEXTENSIONS[k];
                } else {
                    completeFileNameList[0] = absolutePath;
                }
            }        
            for (int k = 0; k < hdrEXTENSIONS.length; k++) {
              
                if (absolutePath.endsWith(FileUtility.getExtension((absolutePath)))) {
                    completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(FileUtility.getExtension((absolutePath)))) +
                        hdrEXTENSIONS[k];
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
                if (FileUtility.getExtension(fileNames[i]).equals(hdrEXTENSIONS[k])) {
                    return fileNames[i];
                }
            }
        }

        return null;
    }
    
    /**
     * Return true if the file specified by absolutePath is header file of ANALYZE.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is header file.
     */

    public static boolean isHeaderFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);
        for(int k=0;k<hdrEXTENSIONS.length;k++) {
            if (extension.equalsIgnoreCase(hdrEXTENSIONS[k])) {
                return true;
            }
        }

        return false;
    }

    /**
     * Return true if the file specified by absolutePath is image file of ANALYZE.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is image file.
     */

    public static boolean isImageFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);
        for(int k=0;k<imgEXTENSIONS.length;k++) {
            if (extension.equalsIgnoreCase(imgEXTENSIONS[k]))
                return true;

        }
        return false;
    }

}
