package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how.
*
*/

public class FileInfoMicroCat extends FileInfoBase {

    private String CBRHostName;                   // cbr03
    private String RPCPortNum;                    // 8000
    private String CATFileName;                   // F:\Dental\NewScans\KO Jan22\B6N_KO_1.cat
    private String imageDestinationDirectory;     // F:\Dental\NewScans\KO Jan22
    private String baseNameforReconstructedSlices;// B6N_KO_1
    private int    projectionUSize;               // 512
    private int    projectionVSize;               // 512
    private int    numberOfProjections;           // 181
    private String CBRProjectionDirectory;        // d:/data/
    private String CBRImageDirectory;             // d:/images/
    private int    volumeSizeX = -1;              // 256
    private int    volumeSizeY = -1;              // 256
    private int    volumeSizeZ = -1;              // 512
    private float  voxelSizeX;                    // 0.200(mm)
    private float  voxelSizeY;                    // 0.200(mm)
    private float  voxelSizeZ;                    // 0.200(mm)
    private String filterType;                    // Shepp-Logan
    private int    UCenterOffset;                 // 4 (pixels)
    private int    VCenterOffset;                 // 0 (pixels)
    private float  volumeOriginX;                 // 0
    private float  volumeOriginY;                 // 0
    private float  volumeOriginZ;                 // 0
    private float  logScale;                      // 1000.0
    private float  startAngle;                    // 65536
    private int    offset;                        // 400
    private int    cylinderReconstruction;        // 1
    private int    enableStepSize;                // 0
    private String stepSizeFile;                  // stepsizefilename
    private int    rotationDirection;                     // 1 (CW=-1, CCW=1)
    private int    timeoutBetweenProjectionTransfers;     // 30000 (ms)
    private String autoStartCBREngine;                    // No
    private String projectionDataAlreadyOnCBR;            // No

    /**
    *  File info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoMicroCat(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
    *  Displays the file information
    *  @param dlog    Dialog box that is written to
    *  @param matrix  Transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        dialog.append("CBR host name:                         " + CBRHostName                       + "\n");
        dialog.append("RPC port number:                       " + RPCPortNum                        + "\n");
        dialog.append("CAT file name:                         " + CATFileName                       + "\n");
        dialog.append("Image destination directory:           " + imageDestinationDirectory         + "\n");
        dialog.append("Base name for reconstructed slices:    " + baseNameforReconstructedSlices    + "\n");
        dialog.append("Projection U size:                     " + projectionUSize                   + "\n");
        dialog.append("Projection V size:                     " + projectionVSize                   + "\n");
        dialog.append("Number of projections:                 " + numberOfProjections               + "\n");
        dialog.append("CBR projection directory:              " + CBRProjectionDirectory            + "\n");
        dialog.append("CBR image directory:                   " + CBRImageDirectory                 + "\n");
        dialog.append("Volume size X:                         " + volumeSizeX                       + "\n");
        dialog.append("Volume size Y:                         " + volumeSizeY                       + "\n");
        dialog.append("Volume size Z:                         " + volumeSizeZ                       + "\n");
        dialog.append("Voxel size X:                          " + voxelSizeX                        + "\n");
        dialog.append("Voxel size Y:                          " + voxelSizeY                        + "\n");
        dialog.append("Voxel size Z:                          " + voxelSizeZ                        + "\n");
        dialog.append("Filter type:                           " + filterType                        + "\n");
        dialog.append("U center offset:                       " + UCenterOffset                     + "\n");
        dialog.append("V center offset:                       " + VCenterOffset                     + "\n");
        dialog.append("Volume origin X:                       " + volumeOriginX                     + "\n");
        dialog.append("Volume origin Y:                       " + volumeOriginY                     + "\n");
        dialog.append("Volume origin Z:                       " + volumeOriginZ                     + "\n");
        dialog.append("Log scale:                             " + logScale                          + "\n");
        dialog.append("Start angle:                           " + startAngle                        + "\n");
        dialog.append("Offset:                                " + offset                            + "\n");
        dialog.append("Cylinder reconstruction:               " + cylinderReconstruction            + "\n");
        dialog.append("Enable step size:                      " + enableStepSize                    + "\n");
        dialog.append("Step size file:                        " + stepSizeFile                      + "\n");
        dialog.append("Rotation direction:                    " + rotationDirection                 + "\n");
        dialog.append("Timeout between projection transfers:  " + timeoutBetweenProjectionTransfers + "\n");
        dialog.append("Autostart CBR engine:                  " + autoStartCBREngine                + "\n");
        dialog.append("Projection data already on CBR:        " + projectionDataAlreadyOnCBR        + "\n");

    }

    /**
    *   Accessor to set CBRHostName.
    *   @param s    String to set it to.
    */
    public void setCBRHostName(String s) {CBRHostName = s;}

    /**
    *   Accessor to set RPCPortNum.
    *   @param s    String to set it to.
    */
    public void setRPCPortNum(String s) {RPCPortNum = s;}

    /**
    *   Accessor to set CATFileName.
    *   @param s    String to set it to.
    */
    public void setCATFileName(String s) {CATFileName = s;}

    /**
    *   Accessor to set imageDestinationDirectory.
    *   @param s    String to set it to.
    */
    public void setImageDestinationDirectory(String s) {imageDestinationDirectory = s;}

    /**
    *   Accessor to set baseNameforReconstructedSlices.
    *   @param s    String to set it to.
    */
    public void setBaseNameforReconstructedSlices(String s) {baseNameforReconstructedSlices = s;}

    /**
    *   Accessor to set projectionUSize.
    *   @param x    Int to set it to.
    */
    public void setProjectionUSize(int x) {projectionUSize = x;}

    /**
    *   Accessor to set projectionVSize.
    *   @param x    Int to set it to.
    */
    public void setProjectionVSize(int x) {projectionVSize = x;}

    /**
    *   Accessor to set numberOfProjections.
    *   @param x    Int to set it to.
    */
    public void setNumberOfProjections(int x) {numberOfProjections = x;}

    /**
    *   Accessor to set CBRProjectionDirectory.
    *   @param s    String to set it to.
    */
    public void setCBRProjectionDirectory(String s) {CBRProjectionDirectory = s;}

    /**
    *   Accessor to set CBRImageDirectory.
    *   @param s    String to set it to.
    */
    public void setCBRImageDirectory(String s) {CBRImageDirectory = s;}

    /**
    *   Accessor to set volumeSizeX.
    *   @param x    Int to set it to.
    */
    public void setVolumeSizeX(int x) {volumeSizeX = x;}

    /**
    *   Accessor to set volumeSizeY.
    *   @param x    Int to set it to.
    */
    public void setVolumeSizeY(int x) {volumeSizeY = x;}

    /**
    *   Accessor to set volumeSizeZ.
    *   @param x    Int to set it to.
    */
    public void setVolumeSizeZ(int x) {volumeSizeZ = x;}

    /**
    *   Accessor to set voxelSizeX.
    *   @param f    Float to set it to.
    */
    public void setVoxelSizeX(float f) {voxelSizeX = f;}

    /**
    *   Accessor to set voxelSizeY.
    *   @param f    Float to set it to.
    */
    public void setVoxelSizeY(float f) {voxelSizeY = f;}

    /**
    *   Accessor to set voxelSizeZ.
    *   @param f    Float to set it to.
    */
    public void setVoxelSizeZ(float f) {voxelSizeZ = f;}

    /**
    *   Accessor to set filterType.
    *   @param s    String to set it to.
    */
    public void setFilterType(String s) {filterType = s;}

    /**
    *   Accessor to set UCenterOffset.
    *   @param x    Int to set it to.
    */
    public void setUCenterOffset(int x) {UCenterOffset = x;}

    /**
    *   Accessor to set VCenterOffset.
    *   @param x    Int to set it to.
    */
    public void setVCenterOffset(int x) {VCenterOffset = x;}

    /**
    *   Accessor to set volumeOriginX.
    *   @param f    Float to set it to.
    */
    public void setVolumeOriginX(float f) {volumeOriginX = f;}

    /**
    *   Accessor to set volumeOriginY.
    *   @param f    Float to set it to.
    */
    public void setVolumeOriginY(float f) {volumeOriginY = f;}

    /**
    *   Accessor to set volumeOriginZ.
    *   @param f    Float to set it to.
    */
    public void setVolumeOriginZ(float f) {volumeOriginZ = f;}

    /**
    *   Accessor to set logScale.
    *   @param f    Float to set it to.
    */
    public void setLogScale(float f) {logScale = f;}

    /**
    *   Accessor to set startAngle.
    *   @param f    Float to set it to.
    */
    public void setStartAngle(float f) {startAngle = f;}

    /**
    *   Accessor to set offset.
    *   @param x    Int to set it to.
    */
    public void setOffset(int x) {offset = x;}

    /**
    *   Accessor to set cylinderReconstruction.
    *   @param x    Int to set it to.
    */
    public void setCylinderReconstruction(int x) {cylinderReconstruction = x;}

    /**
    *   Accessor to set enableStepSize.
    *   @param x    Int to set it to.
    */
    public void setEnableStepSize(int x) {enableStepSize = x;}

    /**
    *   Accessor to set stepSizeFile.
    *   @param s    String to set it to.
    */
    public void setStepSizeFile(String s) {stepSizeFile = s;}

    /**
    *   Accessor to set rotationDirection.
    *   @param x    Int to set it to.
    */
    public void setRotationDirection(int x) {rotationDirection = x;}

    /**
    *   Accessor to set timeoutBetweenProjectionTransfers.
    *   @param x    Int to set it to.
    */
    public void setTimeoutBetweenProjectionTransfers(int x) {timeoutBetweenProjectionTransfers = x;}

    /**
    *   Accessor to set autoStartCBREngine.
    *   @param s    String to set it to.
    */
    public void setAutoStartCBREngine(String s) {autoStartCBREngine = s;}

    /**
    *   Accessor to set projectionDataAlreadyOnCBR.
    *   @param s    String to set it to.
    */
    public void setProjectionDataAlreadyOnCBR(String s) {projectionDataAlreadyOnCBR = s;}

    /**
    *   Accessor to get volumeSizeX.
    *   @return volumeSizeX
    */
    public int getVolumeSizeX() {return volumeSizeX;}

    /**
    *   Accessor to get volumeSizeY.
    *   @return volumeSizeY
    */
    public int getVolumeSizeY() {return volumeSizeY;}

    /**
    *   Accessor to get volumeSizeZ.
    *   @return volumeSizeZ
    */
    public int getVolumeSizeZ() {return volumeSizeZ;}

    /**
    *   Accessor to get voxelSizeX.
    *   @return voxelSizeX
    */
    public float getVoxelSizeX() {return voxelSizeX;}

    /**
    *   Accessor to get voxelSizeY.
    *   @return voxelSizeY
    */
    public float getVoxelSizeY() {return voxelSizeY;}

    /**
    *   Accessor to get voxelSizeZ.
    *   @return voxelSizeZ
    */
    public float getVoxelSizeZ() {return voxelSizeZ;}

    /**
    *   Accessor to get baseNameforReconstructedSlices.
    *   @return baseNameforReconstructedSlices
    */
    public String getBaseNameforReconstructedSlices() {return baseNameforReconstructedSlices;}

}
