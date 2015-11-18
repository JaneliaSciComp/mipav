import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.filters.AlgorithmAnisotropicDiffusion;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR25D2;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmCrop;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;

import javax.swing.JTextArea;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmCellFiring extends AlgorithmBase {
    
    
    private int xDim;
    
    private int yDim;
    
    private int zDim;
    
    private boolean alreadyDisplayed;
    
    private boolean displayInputImage;
    
    private float downSampleXY;
    
    private float downSampleZ;
    
    private boolean displayDownSampleImage;
    
    private boolean saveDownSampleImage;
    
    private boolean cropImage;
    
    private boolean registerImage;
    
    private int earliestSlices;
    
    private boolean anisotropicDiffusion;

    private final JTextArea outputTextArea;
    

    public PlugInAlgorithmCellFiring(ModelImage image, boolean alreadyDisplayed, boolean displayInputImage, 
    		float downSampleXY, float downSampleZ, boolean displayDownSampleImage, 
    		boolean saveDownSampleImage,  boolean cropImage, boolean registerImage, 
    		int earliestSlices, boolean anisotropicDiffusion, final JTextArea outputTextArea) {
    	super(null, image);
    	this.alreadyDisplayed = alreadyDisplayed;
    	this.displayInputImage = displayInputImage;
    	this.downSampleXY = downSampleXY;
    	this.downSampleZ = downSampleZ;
    	this.displayDownSampleImage = displayDownSampleImage;
    	this.saveDownSampleImage = saveDownSampleImage;
    	this.cropImage = cropImage;
    	this.registerImage = registerImage;
    	this.earliestSlices = earliestSlices;
        this.anisotropicDiffusion = anisotropicDiffusion;
        this.outputTextArea = outputTextArea;
    }

    @SuppressWarnings("unused")
	@Override
    public void runAlgorithm() {
    	
        outputTextArea.append("Running Algorithm v1.0" + "\n");

        final long begTime = System.currentTimeMillis();
        
        if (displayInputImage && (!alreadyDisplayed)) {
        	new ViewJFrameImage(srcImage);
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        ModelImage presentImage = srcImage;
        
        if ((downSampleXY < 1.0f) || (downSampleZ < 1.0f)) {
        	int interp = AlgorithmTransform.TRILINEAR;
        	TransMatrix xfrm = new TransMatrix(4);
        	xfrm.identity();
            int oXdim = Math.round(downSampleXY * xDim);
            int oYdim = Math.round(downSampleXY * yDim);
            int oZdim = Math.round(downSampleZ * zDim);
            float oXres = srcImage.getFileInfo(0).getResolutions()[0] * xDim / oXdim;
            float oYres = srcImage.getFileInfo(0).getResolutions()[1] * yDim / oYdim;
            float oZres = srcImage.getFileInfo(0).getResolutions()[2] * zDim / oZdim;
            int oUnits[] = srcImage.getUnitsOfMeasure();
            final boolean clip = true;
            final boolean pad = false;
            final boolean transformVOI = cropImage;
            
            final boolean doCenter = false;
            final Vector3f center = new Vector3f();
            final float fillValue = 0.0f;
            final boolean updateOrigin = false;
            final boolean isSATransform = false;
            TransMatrix transMatrix = xfrm.clone();

            TransMatrix xfrmC;
            int DIM = srcImage.getNDims();
            float[] imgOrigin = new float[4];
            imgOrigin = srcImage.getFileInfo(0).getOrigin().clone();
            int[] axisOrient = new int[3];
            axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

            for (int i = 0; i < axisOrient.length; i++) {
                axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
            }

            int imgOrient = srcImage.getFileInfo(0).getImageOrientation();
            float iXres = srcImage.getFileInfo(0).getResolutions()[0];
            float iYres = srcImage.getFileInfo(0).getResolutions()[1];
            float iZres = srcImage.getFileInfo(0).getResolutions()[2];
            int iXdim = srcImage.getExtents()[0];
            int iYdim = srcImage.getExtents()[1];
            int iZdim = srcImage.getExtents()[2];

            /* Read the direction vector from the MipavCoordinateSystems class: */
            int[] direct = new int[3];
            direct = MipavCoordinateSystems.getModelDirections(srcImage);
            boolean haveCentered = false;
            int[] margins = new int[6];

            // System.out.println("Directions are " +direct[0] +", " +direct[1] +" and " +direct[2]);
            if (pad) {

                if (doCenter) {
                    xfrmC = new TransMatrix(4);
                    // xfrmC.identity();
                    xfrmC.setTranslate(center.X, center.Y, center.Z);
                    xfrm.multLeft(xfrmC);
                    xfrm.setTranslate( -center.X, -center.Y, -center.Z);
                    haveCentered = true;
                }

                margins = AlgorithmTransform.getImageMargins(srcImage, xfrm, oXres, oYres, oZres);
                Preferences.debug("Padding is " + margins[0] + ", " + margins[1]
                        + " and " + margins[2] + ".\n", Preferences.DEBUG_ALGORITHM);
                float tx, ty, tz;

                tx = direct[0] * margins[0] * oXres;
                ty = direct[1] * margins[1] * oYres;
                tz = direct[2] * margins[2] * oZres;

                // System.out.println("Image origin before padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
                imgOrigin[0] -= tx;
                imgOrigin[1] -= ty;
                imgOrigin[2] -= tz;

                // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
                oXdim = oXdim + margins[0] + margins[3];
                oYdim = oYdim + margins[1] + margins[4];
                oZdim = oZdim + margins[2] + margins[5];
            } else {

                for (int m = 0; m < 6; m++) {
                    margins[m] = 0;
                }
            }

            float startPos = imgOrigin[2];

            int[] extents;
            float[] destResolutions;
            boolean do25D = false;
            float startTime = 0.0f;

            if (DIM == 3) {
                destResolutions = new float[3];
                extents = new int[3];

                if (pad) {
                    do25D = false;
                }

                if (do25D) {
                    oZres = iZres;
                }
            } else { // DIM ==4
                startTime = imgOrigin[3];
                do25D = false;
                destResolutions = new float[4];
                extents = new int[4];
            }

            destResolutions[0] = oXres;
            destResolutions[1] = oYres;
            destResolutions[2] = oZres;

            extents[0] = oXdim;
            extents[1] = oYdim;
            extents[2] = oZdim;
            int iTdim;
            int oTdim;

            if (DIM == 4) {
                iTdim = srcImage.getExtents()[3];
                oTdim = iTdim;
                extents[3] = oTdim;
                destResolutions[3] = srcImage.getFileInfo(0).getResolutions()[3];
            }

            final String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");
            int type = srcImage.getType();

            ModelImage downSampleImage = new ModelImage(type, extents, name);

            TransMatrix newMatrix;
            // TransMatrix newTMatrix;
            int imgLength;

            if (updateOrigin) {
                // AlgorithmTransform.updateOrigin(this.transMatrix);
 
                // Remmember the the interpolation routines going from the output to the input image
                // use the inverse matrix, but that here we wish to take the input origin to the
                // output origin, so we do not take the inverse.
                TransMatrix trans;

                if ( (DIM >= 3) && ( !do25D)) {
                    imgLength = iXdim * iYdim * iZdim;
                } else {
                    imgLength = iXdim * iYdim;
                }

                try {

                    if ( (do25D) || (DIM == 2)) {
                        trans = new TransMatrix(3);
                    } else { // (DIM >= 3) && (!do25D)
                        trans = new TransMatrix(4);
                    }

                    trans.Copy(transMatrix);

                    if ( (doCenter) && ( !haveCentered)) {

                        if ( (do25D) || (DIM == 2)) {
                            xfrmC = new TransMatrix(3);
                        } else { // (DIM >= 3) && (!do25D)
                            xfrmC = new TransMatrix(4);
                        }

                        // by default: xfrmC.identity();

                        if ( (DIM >= 3) && ( !do25D)) {
                            xfrmC.setTranslate(center.X, center.Y, center.Z);
                        } else { // (DIM == 2) || do25D
                            xfrmC.setTranslate(center.X, center.Y);
                        }

                        trans.Copy(xfrmC);
                        trans.mult(transMatrix);

                        if ( (DIM >= 3) && ( !do25D)) {
                            trans.setTranslate( -center.X, -center.Y, -center.Z);
                        } else { // (DIM == 2) || do25D
                            trans.setTranslate( -center.X, -center.Y);
                        }
                    } // if ((doCenter) && (!haveCentered))

                    if (trans.getDim() == 3) {
                        trans.transform(imgOrigin[0], imgOrigin[1], imgOrigin);
                    } else {
                        // System.err.println("Before: AlgorithmTransform.imgOrigin[0] = " + AlgorithmTransform.imgOrigin[0] +
                        // "AlgorithmTransform.imgOrigin[1] = " + AlgorithmTransform.imgOrigin[1] +
                        // "AlgorithmTransform.imgOrigin[2] = " + AlgorithmTransform.imgOrigin[2]);
                        trans.transform(imgOrigin[0], imgOrigin[1], imgOrigin[2], imgOrigin);

                        // System.err.println("After: AlgorithmTransform.imgOrigin[0] = " + AlgorithmTransform.imgOrigin[0] +
                        // "AlgorithmTransform.imgOrigin[1] = " + AlgorithmTransform.imgOrigin[1] +
                        // "AlgorithmTransform.imgOrigin[2] = " + AlgorithmTransform.imgOrigin[2]);
                    }
                } catch (final OutOfMemoryError e) {
                	srcImage = null;
                    downSampleImage = null;
                    //maskImage = null;
                    //imgBuf = null;
                    //imgBuf2 = null;
                    System.gc();
                    System.gc();
                    displayError("Out of memory on srcImage.exportData");
                    setCompleted(false);

                    return;
                }
            }
            
            final FileInfoBase[] fileInfo = downSampleImage.getFileInfo();
            float firstPos[] = null;
            float delPos[] = null;
            float sliceLocation0 = Float.NaN;
            float delLoc = Float.NaN;

            if (downSampleImage.getNDims() == 2) {
                fileInfo[0] = (FileInfoBase) srcImage.getFileInfo(0).clone();
                fileInfo[0].setDataType(downSampleImage.getType());
                fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
                fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                fileInfo[0].setEndianess(srcImage.getFileInfo()[0].getEndianess());
                fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[0].setResolutions(destResolutions);
                fileInfo[0].setExtents(downSampleImage.getExtents());
                fileInfo[0].setMax(downSampleImage.getMax());
                fileInfo[0].setMin(downSampleImage.getMin());
                fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
                fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
                fileInfo[0].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
                fileInfo[0].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
                fileInfo[0].setUnitsOfMeasure(oUnits);

            } else if (downSampleImage.getNDims() == 3) {
                final float[] coord = new float[3];
                final float[] tempPos = new float[3];
                String orientation;

                // if the transform was scanner anatomical, set to AXIAL
                if (isSATransform) {
                    imgOrient = FileInfoBase.AXIAL;
                    axisOrient[0] = FileInfoBase.ORI_R2L_TYPE;
                    axisOrient[1] = FileInfoBase.ORI_A2P_TYPE;
                    axisOrient[2] = FileInfoBase.ORI_I2S_TYPE;
                }

                if (srcImage.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                    final FileInfoDicom oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(0);
                    final FileDicomTagTable[] childTagTables = new FileDicomTagTable[downSampleImage.getExtents()[2] - 1];

                    // first create all of the new file infos (reference and children) and fill them with tags from the old
                    // file info. some of these tag values will be overridden in the next loop
                    for (int i = 0; i < downSampleImage.getExtents()[2]; i++) {

                        if (i == 0) {

                            // create a new reference file info
                            fileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                    oldDicomInfo.getFileFormat());
                            ((FileInfoDicom) fileInfo[0]).setVr_type(oldDicomInfo.getVr_type());
                        } else {

                            // all other slices are children of the first file info..
                            fileInfo[i] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                    oldDicomInfo.getFileFormat(), (FileInfoDicom) fileInfo[0]);
                            ((FileInfoDicom) fileInfo[i]).setVr_type(oldDicomInfo.getVr_type());
                            childTagTables[i - 1] = ((FileInfoDicom) fileInfo[i]).getTagTable();
                        }

                        if (srcImage.getExtents()[2] > i) {

                            // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                            ((FileInfoDicom) fileInfo[i]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(i));
                        } else {

                            // not possible for other rotations because the z-dimension is different
                            ((FileInfoDicom) fileInfo[i]).getTagTable().importTags(oldDicomInfo);
                        }
                        ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0028,0010",
                                new Short((short) downSampleImage.getExtents()[1]), 2);
                        ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0028,0011",
                                new Short((short) downSampleImage.getExtents()[0]), 2);
                    }

                    ((FileInfoDicom) fileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
                } else {

                    for (int i = 0; i < downSampleImage.getExtents()[2]; i++) {

                        if (srcImage.getExtents()[2] > i) {
                            fileInfo[i] = (FileInfoBase) srcImage.getFileInfo(i).clone();
                        } else {
                            fileInfo[i] = (FileInfoBase) srcImage.getFileInfo(0).clone();
                        }
                    }
                }

                if ( (srcImage.getFileInfo(0).getFileFormat() == FileUtility.DICOM)
                        && (srcImage.getExtents()[2] != downSampleImage.getExtents()[2])) {

                    float lastPos[] = null;
                    orientation = (String) ((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0020,0032");
                    if (orientation != null) {

                        int index1 = -1, index2 = -1;

                        for (int k = 0; k < orientation.length(); k++) {

                            if (orientation.charAt(k) == '\\') {

                                if (index1 == -1) {
                                    index1 = k;
                                } else {
                                    index2 = k;
                                }
                            }
                        }

                        coord[0] = Float.valueOf(orientation.substring(0, index1)).floatValue();
                        coord[1] = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                        coord[2] = Float.valueOf(orientation.substring(index2 + 1)).floatValue();
                        firstPos = new float[3];

                        transMatrix.transform(coord[0], coord[1], coord[2], firstPos);
                    } // if (orientation != null)
                    orientation = (String) ((FileInfoDicom) fileInfo[downSampleImage.getExtents()[2] - 1]).getTagTable()
                            .getValue("0020,0032");

                    if (orientation != null) {

                        int index1 = -1, index2 = -1;

                        for (int k = 0; k < orientation.length(); k++) {

                            if (orientation.charAt(k) == '\\') {

                                if (index1 == -1) {
                                    index1 = k;
                                } else {
                                    index2 = k;
                                }
                            }
                        }

                        coord[0] = Float.valueOf(orientation.substring(0, index1)).floatValue();
                        coord[1] = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                        coord[2] = Float.valueOf(orientation.substring(index2 + 1)).floatValue();

                        lastPos = new float[3];

                        transMatrix.transform(coord[0], coord[1], coord[2], lastPos);
                    } // if (orientation != null)
                    if ( (firstPos != null) && (lastPos != null)) {
                        delPos = new float[3];
                        for (int i = 0; i <= 2; i++) {
                            delPos[i] = (lastPos[i] - firstPos[i]) / (downSampleImage.getExtents()[2] - 1);
                        }
                    } // if ((firstPos != null) && (lastPos != null)
                    if ( ( ((FileInfoDicom) fileInfo[0]).getTagTable().containsTag("0020,1041"))
                            && ( ((FileInfoDicom) fileInfo[1]).getTagTable().containsTag("0020,1041"))) {
                        if ( ((String) ((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0020,1041") != null)
                                && ((String) ((FileInfoDicom) fileInfo[1]).getTagTable().getValue("0020,1041") != null)) {
                            try {
                                sliceLocation0 = Float.parseFloat((String) ((FileInfoDicom) fileInfo[0]).getTagTable()
                                        .getValue("0020,1041"));
                                final float sliceLocation1 = Float.parseFloat((String) ((FileInfoDicom) fileInfo[1])
                                        .getTagTable().getValue("0020,1041"));
                                delLoc = (sliceLocation1 - sliceLocation0) * destResolutions[2]
                                        / srcImage.getFileInfo()[0].getResolutions()[2];
                            } catch (final NumberFormatException nfe) {

                            }
                        }

                    }
                } // if ((image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) &&

                for (int i = 0; i < downSampleImage.getExtents()[2]; i++) {

                    fileInfo[i].setDataType(downSampleImage.getType());
                    fileInfo[i].setResolutions(destResolutions);
                    fileInfo[i].setSliceThickness(destResolutions[2]);
                    fileInfo[i].setExtents(downSampleImage.getExtents());
                    fileInfo[i].setMax(downSampleImage.getMax());
                    fileInfo[i].setMin(downSampleImage.getMin());
                    fileInfo[i].setImageOrientation(imgOrient);
                    fileInfo[i].setAxisOrientation(axisOrient);
                    fileInfo[i].setUnitsOfMeasure(oUnits);

                    fileInfo[i].setOrigin(imgOrigin);
                    imgOrigin[2] =startPos
                            + (direct[2] * i * destResolutions[2]);

                    if (fileInfo[i].getFileFormat() == FileUtility.DICOM) {
                        if (srcImage.getExtents()[2] == downSampleImage.getExtents()[2]) {
                            // don't interpolate here in case spacing between slices is uneven
                            orientation = (String) ((FileInfoDicom) fileInfo[i]).getTagTable().getValue("0020,0032");

                            if (orientation != null) {

                                int index1 = -1, index2 = -1;

                                for (int k = 0; k < orientation.length(); k++) {

                                    if (orientation.charAt(k) == '\\') {

                                        if (index1 == -1) {
                                            index1 = k;
                                        } else {
                                            index2 = k;
                                        }
                                    }
                                }

                                coord[0] = Float.valueOf(orientation.substring(0, index1)).floatValue();
                                coord[1] = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                                coord[2] = Float.valueOf(orientation.substring(index2 + 1)).floatValue();

                                transMatrix.transform(coord[0], coord[1], coord[2], tempPos);

                                // System.err.println("transformed " + orientation + " to: " +tempPos[0] + " " + tempPos[1]
                                // + "
                                // " + tempPos[2]);
                                orientation = tempPos[0] + "\\" + tempPos[1] + "\\" + tempPos[2];
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0032", orientation);
                            } // if (orientation != null)

                        } // if (image.getExtents()[2] == resultImage.getExtents()[2])
                        else { // image.getExtents()[2] != resultImage.getExtents()[2]
                            if (delPos != null) {
                                orientation = (firstPos[0] + i * delPos[0]) + "\\" + (firstPos[1] + i * delPos[1]) + "\\"
                                        + (firstPos[2] + i * delPos[2]);
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0032", orientation);
                            } // if (delPos != null)
                            if ( !Float.isNaN(delLoc)) {
                                final String sliceLoc = Float.toString(sliceLocation0 + i * delLoc);
                                // slice location
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,1041", sliceLoc,
                                        sliceLoc.length());
                            }
                            final String instanceString = Integer.toString(i + 1);
                            ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0013", instanceString,
                                    instanceString.length());
                            final String imagesInAcquisition = Integer.toString(downSampleImage.getExtents()[2]);
                            ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,1002", imagesInAcquisition,
                                    imagesInAcquisition.length());
                            final String res2 = String.valueOf(destResolutions[2]);
                            if ( ((FileInfoDicom) fileInfo[i]).getTagTable().containsTag("0018,0050")) {
                                // Slice thickness
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0018,0050", res2, res2.length());
                            }
                            if ( ((FileInfoDicom) fileInfo[i]).getTagTable().containsTag("0018,0088")) {
                                // Spacing between slices
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0018,0088", res2, res2.length());
                            }
                        } // else image.getExtents()[2] != resultImage.getExtents()[2]
                    } // if (fileInfo[i].getFileFormat() == FileUtility.DICOM)
                } // for (int i = 0; i < resultImage.getExtents()[2]; i++)
            } else if (downSampleImage.getNDims() == 4) {

                for (int i = 0; i < (downSampleImage.getExtents()[2] * downSampleImage.getExtents()[3]); i++) {
                    fileInfo[i].setModality(srcImage.getFileInfo()[0].getModality());
                    fileInfo[i].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                    fileInfo[i].setDataType(downSampleImage.getType());
                    fileInfo[i].setEndianess(srcImage.getFileInfo()[0].getEndianess());
                    fileInfo[i].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                    fileInfo[i].setResolutions(destResolutions);
                    fileInfo[i].setSliceThickness(destResolutions[2]);
                    fileInfo[i].setExtents(downSampleImage.getExtents());
                    fileInfo[i].setMax(downSampleImage.getMax());
                    fileInfo[i].setMin(downSampleImage.getMin());
                    fileInfo[i].setImageOrientation(imgOrient);
                    fileInfo[i].setAxisOrientation(axisOrient);
                    imgOrigin[2] =startPos
                            + (direct[2] * (i % downSampleImage.getExtents()[2]) * destResolutions[2]);
                    imgOrigin[3] = startTime + (i / downSampleImage.getExtents()[2])
                            * destResolutions[3];
                    fileInfo[i].setOrigin(imgOrigin);
                    fileInfo[i].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
                    fileInfo[i].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
                }
            }

            downSampleImage.setFileInfo(fileInfo);

            // uses inverse transform to transform backwards
            TransMatrix xfrmt = null;
            TransMatrix trans;
            byte byteBuf[] = null;
            byte byteBuf2[] = null;
            int bufferFactor;
            short[] imgBuf = null;
            float[] imgBuf2;
            int imgLength2;

            if ( (DIM >= 3) && ( !do25D)) {
                imgLength = iXdim * iYdim * iZdim;
            } else {
                imgLength = iXdim * iYdim;
            }

            try {

                if ( (do25D) || (DIM == 2)) {
                    trans = new TransMatrix(3);
                } else { // (DIM >= 3) && (!do25D)
                    trans = new TransMatrix(4);
                }

                trans.Copy(transMatrix);

                if ( (doCenter) && ( !haveCentered)) {

                    if ( (do25D) || (DIM == 2)) {
                        xfrmC = new TransMatrix(3);
                    } else { // (DIM >= 3) && (!do25D)
                        xfrmC = new TransMatrix(4);
                    }

                    // by default: xfrmC.identity();

                    if ( (DIM >= 3) && ( !do25D)) {
                        xfrmC.setTranslate(center.X, center.Y, center.Z);
                    } else { // (DIM == 2) || do25D
                        xfrmC.setTranslate(center.X, center.Y);
                    }

                    trans.Copy(xfrmC);
                    trans.mult(transMatrix);

                    if ( (DIM >= 3) && ( !do25D)) {
                        trans.setTranslate( -center.X, -center.Y, -center.Z);
                    } else { // (DIM == 2) || do25D
                        trans.setTranslate( -center.X, -center.Y);
                    }
                } // if ((doCenter) && (!haveCentered))

                xfrmt = AlgorithmTransform.matrixtoInverseArray(trans);

                bufferFactor = 1;

                if (srcImage.isColorImage()) {
                    bufferFactor = 4;
                    imgLength = imgLength * 4;
                }

                if ( (srcImage.getType() == ModelStorageBase.ARGB) && (interp == AlgorithmTransform.TRILINEAR)) {
                    // Reduce needed memory by a factor of 4
                    byteBuf = new byte[imgLength];
                    srcImage.exportData(0, imgLength, byteBuf);
                } else {
                    imgBuf = new short[imgLength];
                    srcImage.exportData(0, imgLength, imgBuf);
                }

                if (bufferFactor == 4) {

                    if ( (DIM >= 3) && ( !do25D)) {
                        if (srcImage.getType() == ModelStorageBase.ARGB) {
                            // Reduce needed memory by a factor of 4
                            byteBuf2 = new byte[4 * oXdim * oYdim * oZdim];
                        } else {
                            imgBuf2 = new float[4 * oXdim * oYdim * oZdim];
                        }
                    } else {
                        imgBuf2 = new float[4 * oXdim * oYdim];
                    }
                }

            } catch (final IOException error) {
                displayError("IOException on srcImage.exportData");
                setCompleted(false);

                srcImage = null;
                downSampleImage = null;
                //maskImage = null;
                imgBuf = null;
                imgBuf2 = null;
                System.gc();

                return;
            } catch (final OutOfMemoryError e) {
            	srcImage = null;
                downSampleImage = null;
                //maskImage = null;
                imgBuf = null;
                imgBuf2 = null;
                System.gc();
                displayError("Algorithm Transform: ZZZ. Out of memory on srcImage.exportData");
                setCompleted(false);

                return;
            }

            if (bufferFactor == 1) { // black and white

                if (DIM == 3) {

                    if (interp == AlgorithmTransform.TRILINEAR) {
                    	outputTextArea.append("Downsampling image" + "\n");
                        int i, j, k;
                        int iAdj, jAdj, kAdj;
                        short[] tempBuf;
                        double X, Y, Z;
                        int x0, y0, z0;
                        double value;
                        double imm, jmm, kmm;
                        double k1, k2, k3, j1, j2, j3;
                        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
                        int deltaX, deltaY, deltaZ;

                        int sliceSize;
                        sliceSize = iXdim * iYdim;

                        imgLength2 = oXdim * oYdim * oZdim;
                        tempBuf = new short[imgLength2];

                        T00 = xfrmt.M00;
                        T01 = xfrmt.M01;
                        T02 = xfrmt.M02;
                        T03 = xfrmt.M03;
                        T10 = xfrmt.M10;
                        T11 = xfrmt.M11;
                        T12 = xfrmt.M12;
                        T13 = xfrmt.M13;
                        T20 = xfrmt.M20;
                        T21 = xfrmt.M21;
                        T22 = xfrmt.M22;
                        T23 = xfrmt.M23;
                        // T30 = (float)xfrm[3][0]; T31 = (float)xfrm[3][1]; T32 = (float)xfrm[3][2]; T33 = (float)xfrm[3][3];

                        int position1, position2;
                        double b1, b2;
                        double dx, dy, dz, dx1, dy1;

                        double invXRes = 1.0 / iXres;
                        double invYRes = 1.0 / iYres;
                        double invZRes = 1.0 / iZres;

                        int index = 0;

                        for (k = 0; (k < oZdim) && !threadStopped; k++) {

                            if (pad) {
                                kAdj = k - margins[2];
                            } else {
                                kAdj = k;
                            }

                            kmm = kAdj * oZres;
                            k1 = (kmm * T02) + T03;
                            k2 = (kmm * T12) + T13;
                            k3 = (kmm * T22) + T23;

                            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                                if (pad) {
                                    jAdj = j - margins[1];
                                } else {
                                    jAdj = j;
                                }

                                jmm = jAdj * oYres;
                                j1 = (jmm * T01) + k1;
                                j2 = (jmm * T11) + k2;
                                j3 = (jmm * T21) + k3;

                                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                                    // transform i,j,k
                                    value = fillValue; // if voxel transforms out of bounds
                                    if (pad) {
                                        iAdj = i - margins[0];
                                    } else {
                                        iAdj = i;
                                    }

                                    imm = iAdj * oXres;
                                    X = (j1 + (imm * T00)) * invXRes;

                                    if ( (X > -0.5) && (X < iXdim)) {
                                        Y = (j2 + (imm * T10)) * invYRes;
                                        if ( (Y > -0.5) && (Y < iYdim)) {
                                            Z = (j3 + (imm * T20)) * invZRes;
                                            if ( (Z > -0.5) && (Z < iZdim)) {

                                                if (X <= 0) {
                                                    x0 = 0;
                                                    dx = 0;
                                                    deltaX = 0;
                                                } else if (X >= (iXdim - 1)) {
                                                    x0 = iXdim - 1;
                                                    dx = 0;
                                                    deltaX = 0;
                                                } else {
                                                    x0 = (int) X;
                                                    dx = X - x0;
                                                    deltaX = 1;
                                                }

                                                if (Y <= 0) {
                                                    y0 = 0;
                                                    dy = 0;
                                                    deltaY = 0;
                                                } else if (Y >= (iYdim - 1)) {
                                                    y0 = iYdim - 1;
                                                    dy = 0;
                                                    deltaY = 0;
                                                } else {
                                                    y0 = (int) Y;
                                                    dy = Y - y0;
                                                    deltaY = iXdim;
                                                }

                                                if (Z <= 0) {
                                                    z0 = 0;
                                                    dz = 0;
                                                    deltaZ = 0;
                                                } else if (Z >= (iZdim - 1)) {
                                                    z0 = iZdim - 1;
                                                    dz = 0;
                                                    deltaZ = 0;
                                                } else {
                                                    z0 = (int) Z;
                                                    dz = Z - z0;
                                                    deltaZ = sliceSize;
                                                }

                                                dx1 = 1 - dx;
                                                dy1 = 1 - dy;

                                                position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                                position2 = position1 + deltaZ;

                                                b1 = (dy1 * ( (dx1 * imgBuf[position1]) + (dx * imgBuf[position1 + deltaX])))
                                                        + (dy * ( (dx1 * imgBuf[position1 + deltaY]) + (dx * imgBuf[position1
                                                                + deltaY + deltaX])));

                                                b2 = (dy1 * ( (dx1 * imgBuf[position2]) + (dx * imgBuf[position2 + deltaX])))
                                                        + (dy * ( (dx1 * imgBuf[position2 + deltaY]) + (dx * imgBuf[position2
                                                                + deltaY + deltaX])));

                                                value = ( (1 - dz) * b1) + (dz * b2);

                                            } // end if Z in bounds
                                        } // end if Y in bounds
                                    } // end if X in bounds

                                    tempBuf[index++] = (short)Math.round(value);
                                } // end for i
                            } // end for j
                        } // end for k

                        try {
                            downSampleImage.importData(0, tempBuf, false);
                            tempBuf = null;
                        } catch (final IOException error) {
                            displayError("AlgorithmTransform: IOException on downSampleImage.importdata(0,tempBuf, false).");

                            setCompleted(false);

                            return;
                        }
                        
                        ModelImage maskImage;
                        imgBuf = null;
                        imgBuf = new short[iXdim * iYdim];
                        if ((downSampleXY < 1.0f) && (transformVOI) && (srcImage.getVOIs().size() != 0)) {
                        	outputTextArea.append("Downsampling VOI" + "\n");
                            int X0pos, Y0pos;
                            double temp1, temp2;
                            int roundX, roundY;
                            int index2;
                            int indexC;
                            final int length = iXdim * iYdim;
                            int index2Size;
                            int inputExtents[] = new int[2];
                            inputExtents[0] = iXdim;
                            inputExtents[1] = iYdim;
                            int outputExtents[] = new int[2];
                            outputExtents[0] = oXdim;
                            outputExtents[1] = oYdim;

                            ModelImage tmpMask;
                            VOIVector voiVector;

                            T00 = xfrmt.M00;
                            T01 = xfrmt.M01;
                            T02 = xfrmt.M03;
                            T10 = xfrmt.M10;
                            T11 = xfrmt.M11;
                            T12 = xfrmt.M13;

                            // Move VOI to slice 0
                            voiVector = srcImage.getVOIs();
                            VOI rectVOI = voiVector.get(0);
                            VOIBaseVector curves = rectVOI.getCurves();
    	            	    VOIBase vBase = curves.get(0);
    	            	    int nPoints = vBase.size();
    	            	    float xArr[] = new float[nPoints];
    	            	    float yArr[] = new float[nPoints];
    	            	    float zArr[] = new float[nPoints];
    	            	    for (i = 0; i < nPoints; i++) {
    	            	    	xArr[i] = vBase.get(i).X;
    	            	    	yArr[i] = vBase.get(i).Y;
    	            	    }
	            	        for (i = 0; i < nPoints; i++) {
	            	            zArr[i] = 0;	
	            	        }
	            	        rectVOI.importCurve(xArr, yArr, zArr);

                            indexC = -1;
                            try {
                                maskImage = new ModelImage(ModelStorageBase.SHORT, inputExtents, "Short Image");
                                tmpMask = new ModelImage(ModelStorageBase.SHORT, outputExtents, null);
                            } catch (final OutOfMemoryError error) {
                                throw error;
                            }
                            for (index = 0; index < voiVector.size(); index++) {
                                final VOI presentVOI = voiVector.elementAt(index);
                                if (presentVOI.getCurveType() == VOI.CONTOUR) {
                                    curves = presentVOI.getCurves();
                                    index2Size = curves.size();
                                } else {
                                    index2Size = 1;
                                }
                                for (index2 = 0; index2 < index2Size; index2++) {
                                    indexC++;

                                    maskImage.clearMask();

                                    (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false,
                                            false, index2);

                                    final BitSet mask = maskImage.getMask();

                                    for (i = 0; i < length; i++) {

                                        if (mask.get(i)) {
                                            maskImage.set(i, indexC + 1);
                                        } else {
                                            maskImage.set(i, 0);
                                        }
                                    }

                                    try {
                                        maskImage.exportData(0, length, imgBuf); // locks and releases lock

                                    } catch (final IOException error) {
                                        displayError("Algorithm VOI transform: Image(s) locked");
                                        setCompleted(false);

                                        return;
                                    }

                                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                                        if (pad) {
                                            iAdj = i - margins[0];
                                        } else {
                                            iAdj = i;
                                        }

                                        imm = iAdj * oXres;
                                        temp1 = (imm * T00) + T02;
                                        temp2 = (imm * T10) + T12;

                                        for (j = 0; (j < oYdim) && !threadStopped; j++) {

                                            // transform i,j
                                            if (pad) {
                                                jAdj = j - margins[1];
                                            } else {
                                                jAdj = j;
                                            }

                                            jmm = jAdj * oYres;
                                            value = fillValue; // if transformed out of bounds.
                                            X = (temp1 + (jmm * T01)) / iXres;
                                            roundX = (int) (X + 0.5);

                                            if ( (X >= -0.5) && (X < iXdim)) {
                                                Y = (temp2 + (jmm * T11)) / iYres;
                                                roundY = (int) (Y + 0.5);

                                                if ( (Y >= -0.5) && (Y < iYdim)) {
                                                    X0pos = Math.min(roundX, iXdim - 1);
                                                    Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                                    value = imgBuf[Y0pos + X0pos];
                                                } // end if Y in bounds
                                            } // end if X in bounds

                                            tmpMask.set(i, j, value);
                                        } // end for j
                                    } // end for i

                                    if (threadStopped) {
                                        return;
                                    }

                                    // ******* Make algorithm for VOI extraction.
                                    tmpMask.calcMinMax();

                                    AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);

                                    VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
                                    VOIExtAlgo.run();
                                    VOIExtAlgo.finalize();
                                    VOIExtAlgo = null;
                                    
                                    downSampleImage.addVOIs(tmpMask.getVOIs());
                                    tmpMask.resetVOIs();
                                    for (j = 0; j < oYdim; j++) {
                                        for (i = 0; i < oXdim; i++) {
                                            tmpMask.set(i, j, fillValue);
                                        }
                                    }
                                } // for (index2 = 0; index2 < curves.size(); index2++)
                            } // for (index = 0; index < voiVector.size(); index++)
                            tmpMask.disposeLocal();
                            tmpMask = null;
                            maskImage.disposeLocal();
                            maskImage = null;
                            imgBuf = null;
                        }
                        
                       
                    } 
                } // if (DIM == 3)
            } // end of if (bufferFactor == 1)
            

            if (threadStopped) {
                downSampleImage.disposeLocal();
                finalize();

                return;
            }

            // copy the src image's matrices into the destination image
            downSampleImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
           
            // add the new transform matrix to the destination image
            transMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
            downSampleImage.getMatrixHolder().addMatrix(transMatrix);

            if (transMatrix.isIdentity()) {
                // BEN: change
                // destImage.setMatrix(transMatrix);
                // destImage.getFileInfo(0).setTransformID(srcImage.getFileInfo(0).getTransformID());
            } else {

                // srcImage Matrix * transMatrix invert * [x y z]transpose
                // since (transMatrix invert * [x y z]transpose) takes the
                // destination image to the source image and srcImage Matrix
                // takes the source image to the axial image.
                // The translation to the center and away from the center are
                // not present since these translations are multiplied into
                // the transformation matrix in AlgorithmTransform.transform()
                // when rotation around the center is specified in JDialogTransform.
                transMatrix.Inverse();

                if (srcImage.getNDims() > 2) {

                    if (transMatrix.getDim() == 4) {
                        newMatrix = new TransMatrix(srcImage.getMatrix());
                        newMatrix.mult(transMatrix);
                    } else { // 2.5D processing
                        newMatrix = new TransMatrix(srcImage.getMatrix());

                        final TransMatrix mat3D = new TransMatrix(4);
                        mat3D.set(0, 0, transMatrix.M00);
                        mat3D.set(0, 1, transMatrix.M01);
                        mat3D.set(0, 2, 0.0);
                        mat3D.set(0, 3, transMatrix.M02);
                        mat3D.set(1, 0, transMatrix.M10);
                        mat3D.set(1, 1, transMatrix.M11);
                        mat3D.set(1, 2, 0.0);
                        mat3D.set(1, 3, transMatrix.M12);
                        mat3D.set(2, 0, transMatrix.M20);
                        mat3D.set(2, 1, transMatrix.M21);
                        mat3D.set(2, 2, transMatrix.M22);
                        mat3D.set(3, 3, 1.0);
                        newMatrix.mult(mat3D);
                    }

                } else { // srcImage.getNDims() == 2
                    newMatrix = new TransMatrix(3);

                    // There is the posibility the for 2D DICOM that the matrix might be 4x4
                    // If 3 x3 OK to load else the newMatrix is identity
                    if (srcImage.getMatrix().getDim() == 3) {
                        newMatrix.Copy(srcImage.getMatrix());
                    }

                    newMatrix.mult(transMatrix);
                }

                // System.err.println("NEW MATRIX: " + newTMatrix);

                // replace the destination image's default (composite) matrix
                // newTMatrix.setTransformID(TransMatrix.TRANSFORM_COMPOSITE);
                // destImage.setMatrix(newTMatrix);

            }
            
            downSampleImage.calcMinMax();
            presentImage = downSampleImage;
            
            if (displayDownSampleImage) {
            	new ViewJFrameImage(downSampleImage);
            }
            
            if (saveDownSampleImage) {
            	outputTextArea.append("Saving downsampled image" + "\n");
            	final FileIO io = new FileIO();
	            io.setQuiet(true);
	            io.setSuppressProgressBar(true);
	            final FileWriteOptions options = new FileWriteOptions(null, null, true);
	            options.setFileType(FileUtility.TIFF);
	
	            options.setIsScript(true);
	            options.setOptionsSet(true);
	            
	            options.setFileDirectory(srcImage.getImageDirectory() + File.separator);
	            int index = srcImage.getImageFileName().indexOf(".");
	            String baseName;
	            if (index > 0) {
	                baseName = srcImage.getImageFileName().substring(0, index);	
	            }
	            else {
	            	baseName = srcImage.getImageFileName();
	            }
	            options.setBeginSlice(0);
                options.setEndSlice(downSampleImage.getExtents()[2] - 1);
                options.setFileName(baseName + "_XY" + String.valueOf(downSampleXY) + "_Z" +
	                                 String.valueOf(downSampleZ) + ".tif");
                boolean allowScriptRecording = false;
                io.writeImage(downSampleImage, options, false, allowScriptRecording);
            } // if (saveDownSampleImage)
        } // if ((downSampleXY < 1.0f) || (downSampleZ < 1.0f))
        
        if (cropImage) {
        	VOIVector VOIs = presentImage.getVOIs();
            if (VOIs != null) {
            	VOI rectVOI = VOIs.get(0);
            	if (rectVOI != null) {
            		outputTextArea.append("Cropping image" + "\n");
            		int xBounds[] = new int[2];
            		int yBounds[] = new int[2];
            		int zBounds[] = new int[2];
            	    rectVOI.getBounds(xBounds, yBounds, zBounds);
            	    // Do not use zBounds
            	    zBounds[0] = 0;
            	    zBounds[1] = presentImage.getExtents()[2] - 1;
            	    int cropExtents[] = new int[3];
            	    cropExtents[0] = xBounds[1] - xBounds[0] + 1;
            	    cropExtents[1] = yBounds[1] - yBounds[0] + 1;
            	    cropExtents[2] = presentImage.getExtents()[2];
            	    ModelImage croppedImage = new ModelImage(srcImage.getDataType(), cropExtents, srcImage.getImageName() + "_crop");
            	    // Extra space around VOI bounds in x and y dimensions
            	    int cushion = 0;
            	    AlgorithmCrop cropAlgo = new AlgorithmCrop(croppedImage, presentImage, cushion, xBounds, yBounds, zBounds);
            	    
            	    cropAlgo.run();
            	    try {
    					new ViewJFrameImage(croppedImage, null, new Dimension(610,
    							200));
    				} catch (OutOfMemoryError error) {
    					MipavUtil
    							.displayError("Out of memory: unable to open new crop image frame");
    					setCompleted(false);
    					return;
    				}
            	    if (((downSampleXY < 1.0f) || (downSampleZ < 1.0f)) && (!displayDownSampleImage)) {
            	    	presentImage.disposeLocal();
            	    	presentImage = null;
            	    }
            	    presentImage = croppedImage;
            	    cropAlgo.finalize();
            	    cropAlgo = null;
            	    xBounds = null;
            	    yBounds = null;
            	    zBounds = null;
            	    cropExtents = null;
            	}
            	else {
            		MipavUtil.displayError("No VOI is present");
            		setCompleted(false);
            		return;
            	}
            } // if (VOIs != null)
            else {
            	MipavUtil.displayError("No VOI vector is present");
            	setCompleted(false);
            	return;
            }
        } // if (cropImage)
        
        if (registerImage) {
        	// Use first slice as reference
        	outputTextArea.append("Registering image" + "\n");
        	boolean doAdjacent = false;
        	int refImageNum = 0;
        	int  cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
        	int DOF = 3;
        	int interp = AlgorithmTransform.BILINEAR;
        	int interp2 = AlgorithmTransform.BILINEAR;
        	float rotateBegin = -3.0f;
        	float rotateEnd = 3.0f;
        	float coarseRate = 3.0f;
        	float fineRate = 2.0f;
        	boolean doGraph = false;
        	boolean doSubsample = true;
        	boolean transformVOIs = false;
        	int maxIterations = 2;
        	int numMinima = 6;
        	AlgorithmRegOAR25D2 reg25 = new AlgorithmRegOAR25D2(presentImage, cost, DOF, interp, interp2, doAdjacent, refImageNum,
                    rotateBegin, rotateEnd, coarseRate, fineRate, doGraph, doSubsample,
                    transformVOIs, maxIterations, numMinima);
        	reg25.run();
        	if (reg25 != null) {
                reg25.disposeLocal();
                reg25.finalize();
            }
        	reg25 = null;
        } // if (registerImage)
        
        outputTextArea.append("Subtract average of earliest slices" + "\n");
        int presentXDim = presentImage.getExtents()[0];
        int presentYDim = presentImage.getExtents()[1];
        int presentArea = presentXDim * presentYDim;
        int presentZDim = presentImage.getExtents()[2];
        int presentVolume = presentArea * presentZDim;
        float buffer[];
        try {
            buffer = new float[presentVolume];
        }
        catch (OutOfMemoryError e) {
        	MipavUtil.displayError("Out of memory: unable to create float buffer = new float[presentVolume]");
	        setCompleted(false);
	        return;	
        }
        try {
        	presentImage.exportData(0, presentVolume, buffer);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on presentImage.exportData(0, presentVolume, buffer");
        	setCompleted(false);
        	return;
        }
        
        
        float areaBuffer[];
        try {
        	areaBuffer = new float[presentArea];
        }
        catch (OutOfMemoryError e) {
        	MipavUtil.displayError("Out of memory: unable to create float areaBuffer = new float[presentArea]");
	        setCompleted(false);
	        return;	
        }
        
        for (int z = 0; z < earliestSlices; z++) {
            for (int i = 0; i < presentArea; i++) {
            	areaBuffer[i] += buffer[i + z * presentArea];
            }
        }
        
        for (int i = 0; i < presentArea; i++) {
        	areaBuffer[i] = areaBuffer[i]/earliestSlices;
        }
        
        for (int z = 0; z < presentZDim; z++) {
        	for (int i = 0; i < presentArea; i++) {
        	    buffer[i + z * presentArea] -= areaBuffer[i];	
        	}
        }
        
        try {
        	presentImage.importData(0, buffer, true);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on presentImage.import(0, buffer, true");
        	setCompleted(false);
        	return;
        }
        buffer = null;
        areaBuffer = null;
        
        if (anisotropicDiffusion) {
        	outputTextArea.append("Anisotropic diffusion" + "\n");
            float sigmaX = 1.0f;
            float sigmaY = 1.0f;
            float sigmaZ = 1.0f;
            boolean useCorrectionFactor = true;
            if (useCorrectionFactor) {
                sigmaZ = sigmaZ * (presentImage.getFileInfo()[0].getResolution(0)/presentImage.getFileInfo()[0].getResolution(2));	
            } // if (useCorrectionFactor)
            float sigmaArray[] = new float[3];
            sigmaArray[0] = sigmaX;
            sigmaArray[1] = sigmaY;
            sigmaArray[2] = sigmaZ;
            int iterations = 10;
            float kValue = 15.0f;
            boolean entireImage = true;
            boolean image25D = false;
            AlgorithmAnisotropicDiffusion anisotropicAlgo = new AlgorithmAnisotropicDiffusion(presentImage, sigmaArray, iterations,
            		                kValue, entireImage, image25D);
            anisotropicAlgo.run();
            anisotropicAlgo.finalize();
            anisotropicAlgo = null;
        } // if (anisotropicDiffusion)
        

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

    }
    


}
