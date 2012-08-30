package gov.nih.mipav.model.structures;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.model.provenance.*;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionChangeName;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.io.*;
import java.text.DecimalFormat;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This class extends the generic buffer class ModelStorageArray and is used to store n-dimensional images and buffer
 * class that supports boolean, byte, short, int, long, float, double, etc. data types. After the buffer is created the
 * minimum and maximum parameters are calculated. ModelImage is a specific buffer to addressing issues relating to
 * images.
 * 
 * @author Matthew J. McAuliffe Ph.D.
 * @version 1.0
 */
public class ModelImage extends ModelStorageBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1234298038008494667L;

    /**
     * Used to indicate that this image object is Image A when two images are displayed in the same frame. See also
     * imageOrder in this class.
     */
    public static final int IMAGE_A = 0;

    /**
     * Used to indicate that this image object is Image B when two images are displayed in the same frame. See also
     * imageOrder in this class.
     */
    public static final int IMAGE_B = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /**
     * List of frames where this image is displayed. This is an important list, for example when the LUT of table is
     * changed the list is used to notify all the frames displaying this image that they should update their view of the
     * image. The transient keyword is used to indicate that the frame list should NOT be cloned when the image is
     * cloned. The new cloned image should build up its own list of frame(s) where it is displayed.
     */
    private transient Vector<ViewImageUpdateInterface> frameList = null;

    /**
     * ImageName is patient's name when using DICOM images. It is filename for any other image something other than
     * filename must be used for DICOM because one DICOM image is made of many seperate files. ** Not necessarily the
     * file name.
     */
    private String imageName;

    /** If imageNameArray is not null, then a different image name will appear on every slice */
    private String imageNameArray[] = null;

    /** Indicates the image order when two images are displayed in the same frame. */
    private int imageOrder = ModelImage.IMAGE_A;

    /**
     * Mask is a binary object that is true interior to a VOI and false otherwise used in algorithms to process only on
     * VOIs if indicated by the user.
     */
    private BitSet mask;

    /** Backup of mask for undoing. */
    private BitSet maskBU;

    private boolean useMask = false;

    /** Holds all of the images associated matrices. */
    private MatrixHolder matrixHolder;

    /**
     * Holds the data provenance (image history) The transient keyword is used to indicate that the provenanceHolder
     * should not be cloned when the image is cloned. The new cloned image creates its own provenanceHolder
     */
    private transient ProvenanceHolder provenanceHolder;

    /** Reference to talairach transform information. */
    private TalairachTransformInfo talairach;
    
    /** Reference to DWI bvalue and gradient information. */
    private DTIParameters dtiParameters;

    /**
     * The user interface has a vector of all image models loaded into MIPAV. I put the reference to it here so that
     * when an image is created it can added tothe vector in the user interface.
     */
    private transient ViewUserInterface UI = null;

    /** List of VOIs that are displayed with this image. */
    private VOIVector voiVector = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new ModelImage object, registering it to the default user interface.
     * 
     * @param type indicates type of buffer as DataType enum (ie. boolean, byte ...)
     * @param dimExtents array indicating image extent in each dimension.
     * @param name name of the image.
     */
    public ModelImage(final DataType type, final int[] dimExtents, final String name) {
        this(type.getLegacyNum(), dimExtents, name);
    }
    
    /**
     * Creates a new ModelImage object, registering it to the default user interface.
     * 
     * @param type indicates type of buffer(ie. boolean, byte ...)
     * @param dimExtents array indicating image extent in each dimension.
     * @param name name of the image.
     */
    public ModelImage(final int type, final int[] dimExtents, final String name) {
        this(type, dimExtents, name, ViewUserInterface.getReference());
    }

    /**
     * Creates a new ModelImage object that is registered to the provided user interface.  Should not be used when
     * an instance of the interface exists.
     * 
     * @param type indicates type of buffer(ie. boolean, byte ...)
     * @param dimExtents array indicating image extent in each dimension.
     * @param name name of the image.
     * @param _UI should be ViewUserInterface.getReference()
     * 
     * @deprecated Only one ViewUserInterface should be instantiated for an instance of MIPAV running, so _UI should be ViewUserInterface.getReference()
     */
    public ModelImage(final int type, final int[] dimExtents, final String name, final ViewUserInterface _UI) {
    	super(type, dimExtents);

        int i;

        // The user interface has a vector of all image models loaded into
        // MIPAV. I keep a reference to the userinterface here so that when an image
        // is created it can added to the hashtable in the user interface.
        this.UI = _UI;

        imageName = ModelImage.makeImageName(name, ""); // removes suffix if one is there.

        if (UI == null) {
            Preferences.debug("New ModelImage = " + imageName + ", but UI is null.");
        }

        if (UI != null) {
            UI.registerImage(this);
        }

        final float[] resolutions = new float[5];

        for (i = 0; i < 5; i++) {
            resolutions[i] = (float) 1.0;
        }

        final int[] units = new int[5];

        for (i = 0; i < 5; i++) {
            units[i] = Unit.MILLIMETERS.getLegacyNum();
        }

        int length = 1;

        if (dimExtents.length <= 3) {

            for (i = 0; i < dimExtents.length; i++) {
                length *= dimExtents[i];
            }
        } else {

            for (i = 0; i < 3; i++) {
                length *= dimExtents[i];
            }
        }

        mask = new BitSet(length);
        maskBU = new BitSet(length);

        // create Matrix Holder to store matrices
        this.matrixHolder = new MatrixHolder(dimExtents.length);

        // create the data provenance holder to store the image history
        this.provenanceHolder = new ProvenanceHolder();

        if (dimExtents.length == 2) {
            fileInfo = new FileInfoBase[1];

            // save the entire filename with the suffix -- helps later when saving file
            fileInfo[0] = new FileInfoImageXML(ModelImage.makeImageName(name, ".xml"), null, FileUtility.XML);
            fileInfo[0].setExtents(dimExtents);
            fileInfo[0].setResolutions(resolutions);
            fileInfo[0].setUnitsOfMeasure(units);
            fileInfo[0].setDataType(type);
        } else if (dimExtents.length == 3) {
            fileInfo = new FileInfoBase[dimExtents[2]];

            for (i = 0; i < dimExtents[2]; i++) {

                // save the entire filename with the suffix -- helps later when saving file
                fileInfo[i] = new FileInfoImageXML(ModelImage.makeImageName(name, ".xml"), null, FileUtility.XML);
                fileInfo[i].setExtents(dimExtents);
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(type);
            }
        } else if (dimExtents.length == 4) {
            fileInfo = new FileInfoBase[dimExtents[2] * dimExtents[3]];

            for (i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {

                // save the entire filename with the suffix -- helps later when saving file
                fileInfo[i] = new FileInfoImageXML(ModelImage.makeImageName(name, ".xml"), null, FileUtility.XML);
                fileInfo[i].setExtents(dimExtents);
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(type);
            }
        }

        frameList = new Vector<ViewImageUpdateInterface>();
        voiVector = new VOIVector();
    }

    /**
     * Creates a new ModelImage object based on an existing ModelSimpleImage, only exists as a FLOAT datatype.
     * 
     * @param simpleImage legacy ModelSimpleImage 
     * @param name name of the image.
     * 
     */
    public ModelImage(final ModelSimpleImage simpleImage, final String name) {
        this(ModelStorageBase.FLOAT, simpleImage.extents, name);
        try {
            this.importData(0, simpleImage.data, true);

        } catch (final IOException ioe) {
            System.out.println("error importing values");

        }

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns whether or not the given data type is a color data type.
     * 
     * @param dataType The data type from a ModelImage to determine if it is of one of the three types of color images
     *            supported.
     * 
     * @return <code>true</code> if color, <code>false</code> if not color.
     */
    public static boolean isColorImage(final int dataType) {

        if ( (dataType == ModelStorageBase.ARGB) || (dataType == ModelStorageBase.ARGB_USHORT)
                || (dataType == ModelStorageBase.ARGB_FLOAT)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Add a listener to this class so that notifyListener can be used to notify all listeners to update the display of
     * the image.
     * 
     * @param obj "object' to be added to the list
     */
    public void addImageDisplayListener(final ViewImageUpdateInterface obj) {
        if (frameList == null) {
            frameList = new Vector<ViewImageUpdateInterface>();
        }

        for (int i = 0; i < frameList.size(); i++) {

            // look through list. if duplicate object found, do not add the object
            if (frameList.get(i).equals(obj)) {
                return;
            }
        }
        frameList.addElement(obj);
    }

    /**
     * adds VOI vector for with new VOIs.
     * 
     * @param VOIs VOIs to add to image
     */
    public void addVOIs(final VOIVector VOIs) {
        final int nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {
            voiVector.add((VOI) VOIs.VOIAt(i).clone());
        }

        System.gc();
    }

    /**
     * Anonymize the image by altering the sensitive data of each slice to something generic.
     * 
     * @see FileInfoDicom#anonymize
     */
    public void anonymize(final boolean[] list, final boolean doRename) {
        int i;

        if (this.isDicomImage()) { // if image is DICOM,

            if (getNDims() == 2) { // and if image is a single slice
                ((FileInfoDicom) fileInfo[0]).anonymize(list); // tell the fileInfo to anonymize itself
                this.setFileInfo(fileInfo[0], 0); // and then make sure (by resetting) the fileInfo in this image is
                // the same as the sanitised version
            } else { // and image has more than one slice

                for (i = 0; i < getExtents()[2]; i++) { // then for all slices in this image,
                    ((FileInfoDicom) fileInfo[i]).anonymize(list); // tell the fileInfo of slice i to anonymize itself
                    this.setFileInfo(fileInfo[i], i); // and then make sure (by resetting) the ith fileInfo in this
                    // image is the same as the sanitised version
                }
            }
        } else if (this.isMincImage()) {

            if (getNDims() == 2) {
                ((FileInfoMinc) fileInfo[0]).anonymize(list);
                this.setFileInfo(fileInfo[0], 0);
            } else {

                for (i = 0; i < getExtents()[2]; i++) {
                    ((FileInfoMinc) fileInfo[i]).anonymize(list);
                    this.setFileInfo(fileInfo[i], i);
                }
            }
        } else if (this.isGE_Signa4XImage()) {
        	if (getNDims() == 2) {
        		((FileInfoGESigna4X)fileInfo[0]).anonymize();
        	}
        	else {
        		for (i = 0; i < getExtents()[2]; i++) {
        			((FileInfoGESigna4X)fileInfo[i]).anonymize();
        			this.setFileInfo(fileInfo[i],i);
        		}
        	}
        } else if (this.isGE_GenesisImage()) {
        	if (getNDims() == 2) {
        		((FileInfoGESigna5X)fileInfo[0]).anonymize();
        	}
        	else {
        		for (i = 0; i < getExtents()[2]; i++) {
        			((FileInfoGESigna5X)fileInfo[i]).anonymize();
        			this.setFileInfo(fileInfo[i],i);
        		}
        	}
        }

        if (doRename) {
            this.setImageName("Anonymous");
        }
    }

    /**
     * Calculates the min and max values for the image array.
     */
    public void calcMinMax() {

        super.calcMinMax(this.getLogMagDisplay());

        /*
         * 
         * if (fileInfo[0].getModality() == FileInfoBase.COMPUTED_TOMOGRAPHY) {
         * 
         * if (getMin() < -1024) { // Do nothing } else { setMin(-1024); }
         * 
         * if (getMax() > 3071) { // Do nothing } else { setMax(3071); } }
         */

        for (final FileInfoBase element : fileInfo) {

            if ( !isColorImage()) {
                element.setMin(getMin());
                element.setMax(getMax());
            } else {
                element.setMinR(getMinR());
                element.setMaxR(getMaxR());
                element.setMinG(getMinG());
                element.setMaxG(getMaxG());
                element.setMinB(getMinB());
                element.setMaxB(getMaxB());
            }
        }

        // compare min and max to the last min and max
        // if they've changed, then reset the transfer function
        // in any frames
        if ( (getMin() != lastMin) || (getMax() != lastMax)) {
            final ViewJFrameImage frame = this.getParentFrame();

            if (frame == null) {
                return;
            }

            ModelLUT lut = null;

            if (this == frame.getImageA()) {
                lut = frame.getLUTa();
            } else if (this == frame.getImageB()) {
                lut = frame.getLUTb();
            }

            if (lut == null) {
                return;
            }

            float min, max;

            if (this.getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
            } else if (this.getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
            } else {
                min = (float) this.getMin();
                max = (float) this.getMax();
            }

            final float imgMin = (float) this.getMin();
            final float imgMax = (float) this.getMax();

            lut.resetTransferLine(min, imgMin, max, imgMax);
        }
    }

    /**
     * @param dims new dimensions for image
     */
    public void setExtents(final int[] dims) {
        super.setExtents(dims);
        if (this.getParentFrame() != null) {
            this.getParentFrame().changeMenuEnables();
        }
    }

    /**
     * Changes the image dimensionality or extents.
     * 
     * @param dimExtents new dimensions for mask, maskBU, and fileInfo
     */
    public void changeExtents(final int[] dimExtents) {
        int i;
        int length = 1;

        if (dimExtents.length <= 3) {

            for (i = 0; i < dimExtents.length; i++) {
                length *= dimExtents[i];
            }
        } else {

            for (i = 0; i < 3; i++) {
                length *= dimExtents[i];
            }
        }

        mask = new BitSet(length);
        maskBU = new BitSet(length);
        setExtents(dimExtents);

        final float[] resolutions = fileInfo[0].getResolutions();
        final int[] units = fileInfo[0].getUnitsOfMeasure();
        final int type = fileInfo[0].getDataType();

        if (dimExtents.length == 2) {
            fileInfo = new FileInfoBase[1];
            fileInfo[0] = new FileInfoImageXML(null, null, FileUtility.XML);
            fileInfo[0].setExtents(dimExtents);
            fileInfo[0].setResolutions(resolutions);
            fileInfo[0].setUnitsOfMeasure(units);
            fileInfo[0].setDataType(type);
        } else if (dimExtents.length == 3) {
            fileInfo = new FileInfoBase[dimExtents[2]];

            for (i = 0; i < dimExtents[2]; i++) {
                fileInfo[i] = new FileInfoImageXML(null, null, FileUtility.XML);
                fileInfo[i].setExtents(dimExtents);
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(type);
            }
        } else if (dimExtents.length == 4) {
            fileInfo = new FileInfoBase[dimExtents[2] * dimExtents[3]];

            for (i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
                fileInfo[i] = new FileInfoImageXML(null, null, FileUtility.XML);
                fileInfo[i].setExtents(dimExtents);
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(type);
            }
        }
    }

    /**
     * Sets the entire mask object to false.
     */
    public void clearMask() {

        if (mask != null) {
            final int size = mask.size();

            for (int i = 0; i < size; i++) {
                mask.clear(i);
            }
        }
    }

    /**
     * Copies the image and all data associated with the image (i.e. VOIs). Invokes the clone(String newName) method
     * with newName set to null;
     * 
     * @return the new copy of the image
     */
    public Object clone() {
        final ModelImage image = (ModelImage) this.clone(null);

        image.frameList = new Vector<ViewImageUpdateInterface>();
        image.provenanceHolder = new ProvenanceHolder();

        return (image);
    }

    /**
     * Copies the image and all data associated with the image (i.e. VOIs). Sets the name of the new image to newName.
     * 
     * @param newName String containing the name for the cloned image. If null then 'this' image name is appended with
     *            "_clone".
     * 
     * @return the new copy of the image
     */
    public Object clone(final String newName) {
        final ModelImage image = (ModelImage) super.clone();

        if (image == null) {
            return null;
        }

        image.setUserInterface(UI);

        image.releaseLock(); // this.image was cloned when it was locked, therefore

        // unlocked the cloned copy.
        // image.makeInfoPanes();
        // set the name of the cloned image
        if (newName == null) {
            image.setClonedImageName(new String(image.getImageName() + "_clone"));
        } else {
            image.setClonedImageName(newName);
        }

        // register the cloned image
        if (UI != null) {
            UI.registerImage(image);
        }

        return (image);
    } // end clone(String)

    /**
     * Deep copies the file type info from the fromImage to the current image object.
     * 
     * @param fromImage image from which to copy file type info
     */
    public void copyFileTypeInfo(final ModelImage fromImage) {
        int numInfos;
        int numFromInfos;

        if (getNDims() == 2) {
            numInfos = 1;
            numFromInfos = 1;
        } else if (getNDims() == 3) {
            numInfos = getExtents()[2];
            numFromInfos = fromImage.getExtents()[2];
        } else {
            numInfos = getExtents()[2] * getExtents()[3];
            numFromInfos = fromImage.getExtents()[2] * fromImage.getExtents()[3];
        }

        final FileInfoBase[] fileInfo = new FileInfoBase[numInfos];

        if (fromImage.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
            final FileInfoDicom oldDicomInfo = (FileInfoDicom) fromImage.getFileInfo(0);
            final FileDicomTagTable[] childTagTables = new FileDicomTagTable[numInfos - 1];

            // first create all of the new file infos (reference and children) and fill them with tags from the old
            // file info. some of these tag values will be overridden in the next loop
            for (int i = 0; i < numInfos; i++) {

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

                if (numInfos > i) {
                    if (i < numFromInfos) {
                        // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                        ((FileInfoDicom) fileInfo[i]).getTagTable()
                                .importTags((FileInfoDicom) fromImage.getFileInfo(i));
                    } else {
                        ((FileInfoDicom) fileInfo[i]).getTagTable().importTags(
                                (FileInfoDicom) fromImage.getFileInfo(numFromInfos - 1));
                    }
                } else {

                    // not possible for other rotations because the z-dimension is different
                    ((FileInfoDicom) fileInfo[i]).getTagTable().importTags(oldDicomInfo);
                }
            }

            ((FileInfoDicom) fileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
        } else {

            for (int i = 0; i < numInfos; i++) {
                if (getNDims() == 2) {
                    fileInfo[i] = (FileInfoBase) getFileInfo(0).clone();
                } else if (getNDims() == 3) {
                    if (getExtents()[2] > i) {
                        fileInfo[i] = (FileInfoBase) getFileInfo(i).clone();
                    } else {
                        fileInfo[i] = (FileInfoBase) getFileInfo(0).clone();
                    }
                } else {
                    if (getExtents()[2] * getExtents()[3] > i) {
                        fileInfo[i] = (FileInfoBase) getFileInfo(i).clone();
                    } else {
                        fileInfo[i] = (FileInfoBase) getFileInfo(0).clone();
                    }
                }
            }

        }

        FileInfoBase.copyCoreInfo(fromImage.getFileInfo(), fileInfo);

        setFileInfo(fileInfo);

        return;
    }

    /**
     * Creates mask and maskBU of new length.
     * 
     * @param length int
     */
    public void createMask(final int length) {
        mask = new BitSet(length);
        maskBU = new BitSet(length);
    }

    /**
     * Displays all information about an image.
     * 
     * @param dialog dialog object where image information is to be displayed
     * @param z index of file information -- each image slice can have separate file information (i.e. like DICOM).
     * @param t t slice of the fileinfo to display.
     * @param dicom boolean indicating if this is a DICOM file
     * @param xml boolean indicating if this is a XML file
     */
    public void displayAboutInfo(final JDialogBase dialog, final int z, final int t, final boolean dicom,
            final boolean xml) {
        int index;

        if (getNDims() == 2) {
            index = 0;
        } else if (getNDims() == 3) {
            index = z;
        } else if (getNDims() == 4) {
            index = (t * getExtents()[2]) + z;
        } else {
            index = (t * getExtents()[2]) + z;
        } 

        if (dicom) {
            dialog.setTitle(dialog.getTitle() + ": " + (index));
            ((JDialogFileInfoDICOM) dialog).displayAboutInfo(this, (FileInfoDicom) fileInfo[index], index);
        } else if (xml) {
            dialog.setTitle(dialog.getTitle() + ": " + (index));
            fileInfo[index].displayAboutInfo(dialog, getMatrix());
        } else if (fileInfo[0] instanceof FileInfoMincHDF) {
            dialog.setTitle(dialog.getTitle() + ": " + (index));
            ((JDialogFileInfoMincHDF) dialog).displayAboutInfo(this, (FileInfoMincHDF) fileInfo[0], index);
        } else {
            fileInfo[index].displayAboutInfo(dialog, getMatrix());
        }
    }

    /**
     * Unregisters image and disposes of image memory and associated objects.
     */
    public void disposeLocal() {
        disposeLocal(true);
    }

    /**
     * Unregisters image and disposes of image memory and associated objects.
     * 
     * @param garbageCollect boolean - A flag indicating whether or not garbage collection is invoked.
     */
    public void disposeLocal(final boolean garbageCollect) {
        unRegisterImage();
        disposeThisImage();

        if (garbageCollect) {
            System.gc();
        }
    }

    public boolean matched( final int[] axisOrderOut, final boolean[] axisFlipOut )
    {
        final int orientationIn = getImageOrientation();
        final int[] axisOrderIn = MipavCoordinateSystems.getAxisOrder(this, orientationIn);

        boolean bMatched = true;
        for (int i = 0; i < axisOrderIn.length; i++) {
            if (axisOrderIn[i] != axisOrderOut[i]) {
                bMatched = false;
                break;
            }
            if (axisFlipOut[i] == true) {
                bMatched = false;
                break;
            }
        }
        return bMatched;
    }
    
    /**
     * Exports data based on the mapping the current ModelImage to a new ModelImage oriented based on the axisOrder and
     * axisFlip arrays.
     * 
     * @param axisOrderOut The mapping of current ModelImage to the new ModelImage axes.
     * @param axisFlip Invert flags for the new axes.
     * 
     * @return A new ModelImage. Extents, resolutions, units, origins and orientations are all updated.
     */
    public final ModelImage export(final int[] axisOrderOut, final boolean[] axisFlipOut, boolean bClone,
    	         ViewJProgressBar progressBar, int startValue, int finalValue) {

        boolean bMatched = matched( axisOrderOut, axisFlipOut );
        if (bMatched) {
            if ( bClone )
            {
                return (ModelImage) this.clone();
            }
            return this;
        }
        
        final int iDims = getNDims();
        final int[] extentsOut = new int[iDims];
        final float[] resolutionsOut = new float[iDims];
        final int[] unitsOfMeasureOut = new int[iDims];
        final float[] startLocationsOut = new float[iDims];
        for (int i = 0; i < iDims; i++) {
            extentsOut[i] = getExtents()[axisOrderOut[i]];
            resolutionsOut[i] = getResolutions(0)[axisOrderOut[i]];
            unitsOfMeasureOut[i] = getUnitsOfMeasure()[axisOrderOut[i]];
            startLocationsOut[i] = getOrigin()[axisOrderOut[i]];
        }

        final ModelImage kReturn = new ModelImage(getType(), extentsOut, "");
        if (kReturn.fileInfo != null) {
            for (int i = 0; i < kReturn.getFileInfo().length; i++) {
            	if (progressBar != null) {
            	    progressBar.updateValueImmed(( startValue + 
            	    		((finalValue - startValue) * i) / (10 * kReturn.getFileInfo().length)));
            	}
            	kReturn.fileInfo[i] = (FileInfoBase)this.getFileInfo(0).clone();
            	kReturn.fileInfo[i].setExtents(extentsOut);
                kReturn.fileInfo[i].setResolutions(resolutionsOut);
                kReturn.fileInfo[i].setUnitsOfMeasure(unitsOfMeasureOut);
                kReturn.fileInfo[i].setOrigin(startLocationsOut);
            }
        }
        try {
            setLock(ModelStorageBase.W_LOCKED);

            /* Get the loop bounds, based on the coordinate-systems: transformation: */
            final int iBound = (iDims > 0) ? getExtents()[axisOrderOut[0]] : 1;
            final int jBound = (iDims > 1) ? getExtents()[axisOrderOut[1]] : 1;
            final int kBound = (iDims > 2) ? getExtents()[axisOrderOut[2]] : 1;
            final int tBound = (iDims > 3) ? getExtents()[axisOrderOut[3]] : 1;
            final int ktProd = kBound * tBound;

            /*
             * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
             * coordinate-systems: transformation:
             */
            final int[] aiFactors = new int[3];
            aiFactors[0] = 1;
            aiFactors[1] = (iDims > 1) ? getExtents()[0] : 1;
            aiFactors[2] = (iDims > 2) ? (getExtents()[0] * getExtents()[1]) : 1;

            final int iFactor = aiFactors[axisOrderOut[0]];
            final int jFactor = aiFactors[axisOrderOut[1]];
            final int kFactor = aiFactors[axisOrderOut[2]];

            final int tFactor = (iDims > 2) ? (getExtents()[0] * getExtents()[1] * getExtents()[2])
                    : ( (iDims > 1) ? (getExtents()[0] * getExtents()[1]) : ( (iDims > 0) ? getExtents()[0] : 1));

            final boolean exportComplex = ( (getType() == ModelStorageBase.COMPLEX) || (getType() == ModelStorageBase.DCOMPLEX)) ? true
                    : false;
            double real, imaginary, mag;

            for (int t = 0; t < tBound; t++) {

                for (int k = 0; k < kBound; k++) {
                	if (progressBar != null) {
                	    progressBar.updateValueImmed( startValue + (finalValue - startValue)/10 +
                	    		(9*(finalValue - startValue) * (k + t*kBound)) / (10* ktProd));
                	}
                    for (int j = 0; j < jBound; j++) {

                        for (int i = 0; i < iBound; i++) {

                            /* calculate the ModelImage space index: */
                            int iIndex = i;
                            int jIndex = j;
                            int kIndex = k;

                            if (axisFlipOut[0]) {
                                iIndex = (iBound - 1) - i;
                            }
                            if (axisFlipOut[1]) {
                                jIndex = (jBound - 1) - j;
                            }
                            if (axisFlipOut[2]) {
                                kIndex = (kBound - 1) - k;
                            }

                            final int srcIndex = (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor)
                                    + (t * tFactor);
                            final int dstIndex = t * (kBound * jBound * iBound) + k * (jBound * iBound) + j * (iBound)
                                    + i;

                            /* if color: */
                            if ( (getType() == ModelStorageBase.ARGB) || (getType() == ModelStorageBase.ARGB_USHORT)
                                    || (getType() == ModelStorageBase.ARGB_FLOAT)) {
                                kReturn.set( (dstIndex * 4) + 0, getFloat( (srcIndex * 4) + 0));
                                kReturn.set( (dstIndex * 4) + 1, getFloat( (srcIndex * 4) + 1));
                                kReturn.set( (dstIndex * 4) + 2, getFloat( (srcIndex * 4) + 2));
                                kReturn.set( (dstIndex * 4) + 3, getFloat( (srcIndex * 4) + 3));
                            }
                            /* if complex: */
                            else if ( (getType() == ModelStorageBase.COMPLEX)
                                    || (getType() == ModelStorageBase.DCOMPLEX)) {

                                if (exportComplex) {
                                    kReturn.set( (dstIndex * 2) + 0, getFloat(srcIndex * 2));
                                    kReturn.set( (dstIndex * 2) + 1, getFloat( (srcIndex * 2) + 1));
                                } else {
                                    real = getFloat(srcIndex * 2);
                                    imaginary = getFloat( (srcIndex * 2) + 1);

                                    if (getLogMagDisplay() == true) {
                                        mag = Math.sqrt( (real * real) + (imaginary * imaginary));
                                        kReturn.set(dstIndex, (float) (0.4342944819 * Math.log( (1.0 + mag))));
                                    } else {
                                        kReturn.set(dstIndex, (float) Math.sqrt( (real * real)
                                                + (imaginary * imaginary)));
                                    }
                                }
                            }
                            /* not color: */
                            else {
                                kReturn.set(dstIndex, getFloat(srcIndex));
                            }
                            //update mask:
                            if ( t == 0 )
                            {
                            	kReturn.mask.set( dstIndex, mask.get(srcIndex ) );
                            }
                        }
                    }
                }
            }
            
            if ( voiVector != null )
            {
                VOIVector kReturnVOIVector = getVOIsCopy();
                for ( int i = 0; i < kReturnVOIVector.size(); i++ )
                {
                    Vector<VOIBase> curves = kReturnVOIVector.elementAt(i).getCurves();
                    for ( int j = 0; j < curves.size(); j++ )
                    {
                        VOIBase contour = curves.elementAt(j);
                        for ( int k = 0; k < contour.size(); k++ )
                        {
                            Vector3f kIn = contour.elementAt(k);
                            float[] in = new float[]{kIn.X, kIn.Y, kIn.Z};
                            kIn.X = in[axisOrderOut[0]];
                            kIn.Y = in[axisOrderOut[1]];
                            kIn.Z = in[axisOrderOut[2]];
                            if (axisFlipOut[0]) {
                                kIn.X = (iBound - 1) - kIn.X;
                            }
                            if (axisFlipOut[1]) {
                                kIn.Y = (jBound - 1) - kIn.Y;
                            }
                            if (axisFlipOut[2]) {
                                kIn.Z = (kBound - 1) - kIn.Z;
                            }
                        }
                    }
                }
                kReturn.setVOIs( kReturnVOIVector );
            }

        } catch (final IOException error) {

        } finally {
            releaseLock();
        }
        
        calcStartLocations(startLocationsOut, axisOrderOut, axisFlipOut);
        final int[] axisOrientationOut = new int[3];
        int orientationOut = calcAxisOrientation(axisOrientationOut, axisOrderOut, axisFlipOut);
        kReturn.setImageOrientation(orientationOut);
        if (kReturn.fileInfo != null) {
            for (int i = 0; i < kReturn.getFileInfo().length; i++) {
                kReturn.fileInfo[i].setAxisOrientation(axisOrientationOut);
                kReturn.fileInfo[i].setOrigin(startLocationsOut);
            }
        }

        return kReturn;
    }

    /**
     * Calculates the new start locations based on image orientation.
     * 
     * @param newLoc float[] buffer to store the new start locations
     */
    private void calcStartLocations(final float[] newLoc, final int[] axisOrder, final boolean axisFlip[]) {

        final int[] oldDims = getExtents();
        int i;
        float xOr = 0.0f;
        float yOr = 0.0f;;
        float zOr = 0.0f;
        float originalOr[] = new float[3];
        float flippedOr[] = new float[3];
        Vector3f position;
        Vector3f out;
        float origin[] = new float[Math.max(3,newLoc.length)];

        final int[] axisOrient = fileInfo[0].getAxisOrientation();
        for (i = 0; i < Math.min(3, getNDims()); i++) {
            if (i == 0) {
            	originalOr[0] = 0.0f;
            	flippedOr[0] = oldDims[0] - 1;
            }
            else if (i == 1) {
            	originalOr[1] = 0.0f;
            	flippedOr[1] = oldDims[1] - 1;
            }
            else {
            	originalOr[2] = 0.0f;
            	flippedOr[2] = oldDims[2] - 1;
            }
        }
        
        for (i = 0; i < 3; i++) {
        	if (axisFlip[i]) {
        		if (axisOrder[i] == 0) {
        		    xOr = flippedOr[0];	
        		}
        		else if (axisOrder[i] == 1) {
        			yOr = flippedOr[1];
        		}
        		else {
        			zOr = flippedOr[2];
        		}
        	}
        	else {
        		if (axisOrder[i] == 0) {
        			xOr = originalOr[0];
        		}
        		else if (axisOrder[i] == 1) {
        			yOr = originalOr[1];
        		}
        		else {
        			zOr = originalOr[2];
        		}
        	}
        }
        
        position = new Vector3f(xOr, yOr, zOr);
        out = new Vector3f(position);
        MipavCoordinateSystems.fileToScanner(position, out, this);
        for (i = 0; i < Math.min(3, getNDims()); i++) {
	        if ((axisOrient[i] == FileInfoBase.ORI_R2L_TYPE) || (axisOrient[i] == FileInfoBase.ORI_L2R_TYPE)) {
	            origin[i] = out.X;
	        }
	        else if ((axisOrient[i] == FileInfoBase.ORI_A2P_TYPE) || (axisOrient[i] == FileInfoBase.ORI_P2A_TYPE)) {
	            origin[i] = out.Y;
	        }
	        else {
	            origin[i] = out.Z;
	        }
        }

        for (i = 0; i < newLoc.length; i++) {
            newLoc[i] = origin[axisOrder[i]];
            if (Math.abs(newLoc[i]) < .000001f) {
                newLoc[i] = 0f;
            }
        }
    }

    /**
     * Calculate the new image AxisOrientation, based on re-ordering the axes using axisOrder and axisFlip.
     * 
     * @param newOrient new image AxisOrientation (R2L or L2R, P2A or A2P, etc...)
     * @param axisOrder re-ordering of axes
     * @param axisFlip inverting new axes.
     */
    private int calcAxisOrientation(final int[] newOrient, final int[] axisOrder, final boolean axisFlip[]) {

        final int[] oldOrient = getAxisOrientation();
        for (int i = 0; i < newOrient.length; i++) {
            newOrient[i] = oldOrient[axisOrder[i]];
            if (axisFlip[i]) {
                if (newOrient[i] == FileInfoBase.ORI_R2L_TYPE) {
                    newOrient[i] = FileInfoBase.ORI_L2R_TYPE;
                } else if (newOrient[i] == FileInfoBase.ORI_L2R_TYPE) {
                    newOrient[i] = FileInfoBase.ORI_R2L_TYPE;
                } else if (newOrient[i] == FileInfoBase.ORI_P2A_TYPE) {
                    newOrient[i] = FileInfoBase.ORI_A2P_TYPE;
                } else if (newOrient[i] == FileInfoBase.ORI_A2P_TYPE) {
                    newOrient[i] = FileInfoBase.ORI_P2A_TYPE;
                } else if (newOrient[i] == FileInfoBase.ORI_I2S_TYPE) {
                    newOrient[i] = FileInfoBase.ORI_S2I_TYPE;
                } else if (newOrient[i] == FileInfoBase.ORI_S2I_TYPE) {
                    newOrient[i] = FileInfoBase.ORI_I2S_TYPE;
                }
            }
        }
        int imageOrientation = FileInfoBase.UNKNOWN_ORIENT;
        if ( newOrient[2] == FileInfoBase.ORI_S2I_TYPE || newOrient[2] == FileInfoBase.ORI_I2S_TYPE)
        {
            imageOrientation = FileInfoBase.AXIAL;
        }
        if ( newOrient[2] == FileInfoBase.ORI_R2L_TYPE || newOrient[2] == FileInfoBase.ORI_L2R_TYPE)
        {
            imageOrientation = FileInfoBase.SAGITTAL;
        }
        if ( newOrient[2] == FileInfoBase.ORI_P2A_TYPE || newOrient[2] == FileInfoBase.ORI_A2P_TYPE)
        {
            imageOrientation = FileInfoBase.CORONAL;
        }
        return imageOrientation;
    }

    /**
     * Forms a solid (without holes) binary image from all VOIs in the image.
     * 
     * @return image image of boolean type with VOI objects = 1 and background = 0
     */
    public ModelImage generateBinaryImage() {
        return this.generateBinaryImage(false, false);
    }

    /**
     * Forms a binary image from VOIs.
     * 
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param onlyActive Only mask regions that are active (i.e. selected VOIs)
     * 
     * @return image image of boolean type with VOI objects = 1 and background = 0
     */
    public ModelImage generateBinaryImage(final boolean XOR, final boolean onlyActive) {
        ModelImage maskImage = null;

        if (voiVector.size() == 0) {
            return null;
        }

        try {
            maskImage = new ModelImage(ModelStorageBase.BOOLEAN, this.getExtents(), "Binary Image");

            JDialogBase.updateFileInfoOtherModality(this, maskImage);

            fixFileTypeInfo(maskImage);
        } catch (final OutOfMemoryError error) {
            throw error;
        }

        for (int i = 0; i < voiVector.size(); i++) {
            (voiVector.elementAt(i)).createBinaryImage(maskImage, XOR, onlyActive);
        }

        maskImage.clearMask();
        maskImage.calcMinMax();

        return maskImage;
    }

    /**
     * Generates a solid (no holes) short image of regions defined by VOIs.
     * 
     * @param offset offset value added to ID - normally 1 used to label the masked regions
     * 
     * @return ModelImage mask image of type short
     */
    public ModelImage generateShortImage(final int offset) {
        return this.generateShortImage(offset, false, false);
    }

    /**
     * Exports a short mask of the VOI[index]. VOI[0] = 1 ... VOI[n] = n
     * 
     * @param offset offset value added to ID - normally 1. ID is used to label the masked regions
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param onlyActive Only mask regions that are active (i.e. selected VOIs)
     * 
     * @return ModelImage mask image of type short
     */
    public ModelImage generateShortImage(final int offset, final boolean XOR, final boolean onlyActive) {
        ModelImage maskImage = null;

        if (voiVector.size() == 0) {
            return null;
        }

        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, this.getExtents(), "Short Image");

            JDialogBase.updateFileInfoOtherModality(this, maskImage);

            fixFileTypeInfo(maskImage);
        } catch (final OutOfMemoryError error) {
            throw error;
        }

        for (int i = 0; i < voiVector.size(); i++) {
            maskImage.clearMask();
           (voiVector.elementAt(i)).createShortImage(maskImage, offset, XOR, onlyActive);
        }

        maskImage.clearMask();
        maskImage.calcMinMax();

        return maskImage;
    }

    /**
     * Generates a solid (no holes) unsigned byte image of regions defined by VOIs.
     * 
     * @param offset offset value added to ID - normally 1. ID is used to label the masked regions
     * 
     * @return ModelImage mask image of type unsigned byte
     */

    public ModelImage generateUnsignedByteImage(final int offset) {
        return this.generateUnsignedByteImage(offset, false, false);
    }

    /**
     * Exports an unsigned byte mask of the VOI[index]. VOI[0] = 1 ... VOI[n] = n
     * 
     * @param offset offset value added to ID - normally 1. ID is used to label the masked regions
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param onlyActive Only mask regions that are active (i.e. selected VOIs)
     * 
     * @return ModelImage mask image of type unsigned byte
     */
    public ModelImage generateUnsignedByteImage(final int offset, final boolean XOR, final boolean onlyActive) {
        ModelImage maskImage = null;

        if (voiVector.size() == 0) {
            return null;
        }

        try {
            maskImage = new ModelImage(ModelStorageBase.UBYTE, this.getExtents(), "UBYTE Image");

            JDialogBase.updateFileInfoOtherModality(this, maskImage);

            fixFileTypeInfo(maskImage);
        } catch (final OutOfMemoryError error) {
            throw error;
        }

        for (int i = 0; i < voiVector.size(); i++) {
            maskImage.clearMask();
            (voiVector.elementAt(i)).createUByteImage(maskImage, offset, XOR, onlyActive);
        }

        maskImage.clearMask();
        maskImage.calcMinMax();

        return maskImage;
    }

    /**
     * Generates a BitSet mask of all the VOIs and sets ImageModel mask.
     * 
     * @return binary mask of all VOIs returned as a BitSet object
     */
    public BitSet generateVOIMask() {
        return this.generateVOIMask(false);
    }

    /**
     * Generates a BitSet mask of all the VOIs and sets ImageModel mask.
     * 
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * 
     * @return binary mask of all VOIs returned as a BitSet object
     */
    public BitSet generateVOIMask(final boolean XOR) {
        int i;
        final int[] extents = getExtents();

        if (voiVector.size() != 0) {

            for (i = 0; i < voiVector.size(); i++) {
                voiVector.VOIAt(i).createBinaryMask3D(mask, extents[0], extents[1], XOR, false);
            }
        } 
        return mask;
    }

    /**
     * Generates a mask of the type short - without XORing VOI contours.
     * 
     * @param mask mask of VOI of type short
     * @param index indicates a specific VOI used to create the mask
     * 
     * @return mask short mask of the VOI
     */
    public short[] generateVOIMask(final short[] mask, final int index) {
        return this.generateVOIMask(mask, index, false);
    }

    /**
     * Generates a BitSet mask of all the VOIs and sets ImageModel mask.
     * 
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param onlyActive Only mask regions that are active (i.e. selected )
     * 
     * @return binary mask of all VOIs returned as a BitSet object
     */
    public BitSet generateVOIMask(final boolean XOR, final boolean onlyActive) {
        int i;
        final int[] extents = getExtents();

        if (voiVector.size() != 0) {

            for (i = 0; i < voiVector.size(); i++) {
                voiVector.VOIAt(i).createBinaryMask3D( mask, extents[0], extents[1], XOR, onlyActive);
            }
        }

        return mask;
    }

    /**
     * Exports a short mask of the VOI[index].
     * 
     * @param mask mask of VOI of type short
     * @param index indicates a specific VOI used to create the mask
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * 
     * @return mask short mask of the VOI
     */
    public short[] generateVOIMask(final short[] mask, final int index, final boolean XOR) {

        if (voiVector.size() != 0) {
            int zDim = getExtents().length > 2 ?  getExtents()[2] : 1;
            return ( (voiVector.elementAt(index)).createShortMask(mask, getExtents()[0], getExtents()[1], zDim, XOR));
        }
        return null;
    }

    /**
     * Method that returns the animate frame if it exists else returns null.
     * 
     * @return animate frame
     */
    public ViewJFrameAnimate getAnimateFrame() {

        for (int i = 0; i < frameList.size(); i++) {

            if (frameList.elementAt(i) instanceof ViewJFrameAnimate) {
                return (ViewJFrameAnimate) frameList.elementAt(i);
            }
        }

        return null;
    }

    /**
     * Method that returns the HistoLUT frame if it exists else returns null.
     * 
     * @return histoLUTFrame
     */
    public ViewJFrameHistoLUT getHistoLUTFrame() {

        if (frameList != null) {

            for (int i = 0; i < frameList.size(); i++) {

                if (frameList.elementAt(i) instanceof ViewJFrameHistoLUT) {
                    return (ViewJFrameHistoLUT) frameList.elementAt(i);
                }
            }
        }

        return null;
    }

    /**
     * Method that returns the HistoRGB frame if it exists else returns null.
     * 
     * @return histoRGBFrame
     */
    public ViewJFrameHistoRGB getHistoRGBFrame() {

        if (frameList != null) {

            for (int i = 0; i < frameList.size(); i++) {

                if (frameList.elementAt(i) instanceof ViewJFrameHistoRGB) {
                    return (ViewJFrameHistoRGB) frameList.elementAt(i);
                }

            }
        }

        return null;
    }

    /**
     * Calculates translation offset for transforming image about the center of the image.
     * 
     * @return Center of image in pixels.
     */
    public Vector3f getImageCenter() {
        Vector3f center;

        try {
            center = new Vector3f();
            center.X = (getExtents()[0] - 1) / 2f;
            center.Y = (getExtents()[1] - 1) / 2f;

            if (getExtents().length > 2) {
                center.Z = (getExtents()[2] - 1) / 2f;
            } else {
                center.Z = 0f;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("getImageCenter: Out of memory error");
            center = null;
            System.gc();
        }

        return center;
    }

    /**
     * Calculates translation offset for transforming image about the center of the image in the resolution space.
     * 
     * @param useScanner DOCUMENT ME!
     * 
     * @return Center of the image in millimeters (or other physical dimension).
     */
    public Vector3f getImageCentermm(final boolean useScanner) {
        Vector3f center;
        center = new Vector3f();

        if (useScanner && (getExtents().length > 2)) {
            MipavCoordinateSystems.scannerToFile(new Vector3f(0f, 0f, 0f), center, this);

            if ( (center.X >= 0) && (center.X <= getExtents()[0]) && (center.Y >= 0) && (center.Y <= getExtents()[1])
                    && (center.Z >= 0) && (center.Z <= getExtents()[2])) {

                center.X *= fileInfo[0].getResolutions()[0];
                center.Y *= fileInfo[0].getResolutions()[1];
                center.Z *= fileInfo[0].getResolutions()[2];

                return center;
            }
        }

        try {

            center.X = (getExtents()[0] - 1) * fileInfo[0].getResolutions()[0] / 2f;
            center.Y = (getExtents()[1] - 1) * fileInfo[0].getResolutions()[1] / 2f;

            if (getExtents().length > 2) {
                center.Z = (getExtents()[2] - 1) * fileInfo[0].getResolutions()[2] / 2f;

            } else {
                center.Z = 0f;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("GetImageCentermm: Out of memory error");
            center = null;
            System.gc();
        }

        return center;
    }

    /**
     * Returns the directory where the image file is located.
     * 
     * @return The directory where the image file resides.
     */
    public String getImageDirectory() {

        if (fileInfo != null) {
            // System.out.println(fileInfo[0].getFileDirectory());
            return fileInfo[0].getFileDirectory();
        }
		return null;
    }

    /**
     * Returns the file name of the image.
     * 
     * @return the String that represents the filename (as stored in the fileinfo)
     */
    public String getImageFileName() {

        if (fileInfo != null) {
            // System.out.println(fileInfo[0].getFileName());
            return fileInfo[0].getFileName();
        }
		return null;
    }

    /**
     * Accessor that returns.
     * 
     * @return image frame vector
     */
    public Vector<ViewImageUpdateInterface> getImageFrameVector() {

        if (Preferences.debugLevel(Preferences.DEBUG_MINOR)) {
            Preferences.debug("Model Image Registered frames to image list:" + this.getImageName() + "\n");

            if (frameList != null) {

                for (int i = 0; i < frameList.size(); i++) {
                    Preferences.debug( ((JFrame) (frameList.elementAt(i))).getTitle() + "\n");
                }
            }

            Preferences.debug("\n");
        }

        return frameList;
    }

    /**
     * Returns the type of image.
     * 
     * @return type of image (MRI, CT, ...)
     */
    public int getImageModality() {

        if (fileInfo != null) {
            return fileInfo[0].getModality();
        } else {
            return FileInfoBase.UNKNOWN_MODALITY;
        }
    }

    /**
     * Accessor that returns the name of the image.
     * 
     * @return the String representing the filename if DICOM image then ImageName is the patients' name else, imageName
     *         is the file name (see the contructor for more)
     */
    public String getImageName() {

        if (imageName == null) {
            return ("Unknown");
        } else {
            return imageName;
        }
    }

    /**
     * For multiple image viewers this indicates order of the image.
     * 
     * @return integer indicating image order
     */
    public int getImageOrder() {
        return imageOrder;
    }

    /**
     * Method that returns the lightbox frame if it exists else returns null.
     * 
     * @return lightbox frame
     */
    public ViewJFrameLightBox getLightBoxFrame() {

        if (frameList != null) {

            for (int i = 0; i < frameList.size(); i++) {

                if (frameList.elementAt(i) instanceof ViewJFrameLightBox) {
                    return (ViewJFrameLightBox) frameList.elementAt(i);
                }
            }
        }

        return null;
    }

    /**
     * Accessor that returns.
     * 
     * @return mask that indicates which pixels/voxels will be processed.
     */
    public BitSet getMask() {
        return mask;
    }

    /**
     * Accessor that returns.
     * 
     * @return a bakeup of mask that indicates which pixels/voxels will be processed.
     */
    public BitSet getMaskBU() {
        return maskBU;
    }

    public void useMask(final boolean bOn) {
        useMask = bOn;
    }

    public boolean useMask() {
        return useMask;
    }

    /**
     * Accessor that returns transformation matrix.
     * 
     * @return transformation matrix
     */
    public TransMatrix getMatrix() {
        return matrixHolder.getCompositeMatrix(true);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public MatrixHolder getMatrixHolder() {
        return matrixHolder;
    }

    public ProvenanceHolder getProvenanceHolder() {
        return this.provenanceHolder;
    }

    /**
     * If no LUT or RGB color table is defined, this returns the packed int value for the color at iIndexs for the input
     * ModelImage kImage:
     * 
     * @param iIndex pixel index
     * 
     * @return RGB color value.
     */
    public final int getPackedColor(final int iIndex) {
        int iRed = 0;
        int iGreen = 0;
        int iBlue = 0;
        int iNewColor = 0;

        if (this.getType() == ModelStorageBase.ARGB) {
            iRed = this.getC(iIndex, 1).byteValue();
            iGreen = this.getC(iIndex, 2).byteValue();
            iBlue = this.getC(iIndex, 3).byteValue();
        } else if (this.getType() == ModelStorageBase.ARGB_USHORT) {
            final Number kRed = new Double(255 * (this.getC(iIndex, 1).shortValue() - this.getMin())
                    / (this.getMax() - this.getMin()));
            iRed = kRed.byteValue();
            final Number kGreen = new Double(255 * (this.getC(iIndex, 2).shortValue() - this.getMin())
                    / (this.getMax() - this.getMin()));
            iGreen = kGreen.byteValue();
            final Number kBlue = new Double(255 * (this.getC(iIndex, 3).shortValue() - this.getMin())
                    / (this.getMax() - this.getMin()));
            iBlue = kBlue.byteValue();
        } else if (this.getType() == ModelStorageBase.ARGB_FLOAT) {
            final Number kRed = new Double(255 * (this.getC(iIndex, 1).floatValue() - this.getMin())
                    / (this.getMax() - this.getMin()));
            iRed = kRed.byteValue();
            final Number kGreen = new Double(255 * (this.getC(iIndex, 2).floatValue() - this.getMin())
                    / (this.getMax() - this.getMin()));
            iGreen = kGreen.byteValue();
            final Number kBlue = new Double(255 * (this.getC(iIndex, 3).floatValue() - this.getMin())
                    / (this.getMax() - this.getMin()));
            iBlue = kBlue.byteValue();
        } else {
            final Number kGray = new Double(255 * (this.getFloat(iIndex) - this.getMin())
                    / (this.getMax() - this.getMin()));
            iRed = kGray.byteValue();
            iGreen = iRed;
            iBlue = iRed;
        }

        iRed = iRed << 16;
        iGreen = iGreen << 8;
        iNewColor = (iRed & 0x00ff0000) | (iGreen & 0x0000ff00) | (iBlue & 0x000000ff);
        iNewColor = (iNewColor | 0xff000000);

        return iNewColor;
    }

    /**
     * Returns the parent frame of this image. Should always exist.
     * 
     * @return The parent frame of this image.
     */
    public ViewJFrameImage getParentFrame() {

        if (frameList != null) {

            for (int i = 0; i < frameList.size(); i++) {

                if (frameList.elementAt(i) instanceof ViewJFrameImage) {
                    return (ViewJFrameImage) frameList.elementAt(i);
                }
            }

            return null; // this should never happen!
        } else {
            return null; // this does happen
        }
    }

    /**
     * Extract an arbitrary slice plane from the image using tri-linear interpolation. Performs bounds checking and
     * returns a full plane of the image (up to the slicesize * 2).
     * 
     * @param topLeft the top left point of the plane; must be in the image coord system.
     * @param topRight the top right point of the plane; must be in the image coord system.
     * @param botLeft the bottom left point of the plane; must be in the image coord system.
     * @param botRight the bottom right point of the plane; must be in the image coord system. not really used..
     * 
     * @return a float buffer containing the extracted plane (size == image.getSliceSize() 2).
     * @deprecated
     */
    public final float[] getPlane(final Vector3f topLeft, final Vector3f topRight, final Vector3f botLeft,
            final Vector3f botRight) {
        double x, y, z;
        int i, j, index;

        // start at top left, move along line defined by (topLeft, topRight) to fill extents[0] pixels,
        // move down a distance determined by the line (topLeft, botLeft) divided by extents[1]
        // stop when slicesize pixels have been filled (and we should reach botRight..)

        final float xRes = getFileInfo(0).getResolutions()[0];
        final float yRes = getFileInfo(0).getResolutions()[1];
        final float zRes = getFileInfo(0).getResolutions()[2];
        topLeft.X *= xRes;
        topLeft.Y *= yRes;
        topLeft.Z *= zRes;
        topRight.X *= xRes;
        topRight.Y *= yRes;
        topRight.Z *= zRes;
        botLeft.X *= xRes;
        botLeft.Y *= yRes;
        botLeft.Z *= zRes;
        botRight.X *= xRes;
        botRight.Y *= yRes;
        botRight.Z *= zRes;

        final int planeLength = MipavMath.round(Math.sqrt( ( (botLeft.X - topLeft.X) * (botLeft.X - topLeft.X))
                + ( (botLeft.Y - topLeft.Y) * (botLeft.Y - topLeft.Y))
                + ( (botLeft.Z - topLeft.Z) * (botLeft.Z - topLeft.Z))));
        final int planeWidth = MipavMath.round(Math.sqrt( ( (topRight.X - topLeft.X) * (topRight.X - topLeft.X))
                + ( (topRight.Y - topLeft.Y) * (topRight.Y - topLeft.Y))
                + ( (topRight.Z - topLeft.Z) * (topRight.Z - topLeft.Z))));
        final int planeSize = planeLength * planeWidth;
        final float[] plane = new float[planeSize];

        final double colStep = 1.0 / planeWidth;
        final double rowStep = 1.0 / planeLength;
        double colFactor = 0;
        double rowFactor = 0;
        double rowOffsetX = 0;
        double rowOffsetY = 0;
        double rowOffsetZ = 0;
        final float colDeltaX = topRight.X - topLeft.X;
        final float colDeltaY = topRight.Y - topLeft.Y;
        final float colDeltaZ = topRight.Z - topLeft.Z;
        final float rowDeltaX = botLeft.X - topLeft.X;
        final float rowDeltaY = botLeft.Y - topLeft.Y;
        final float rowDeltaZ = botLeft.Z - topLeft.Z;

        final float invXRes = 1.0f / xRes;
        final float invYRes = 1.0f / yRes;
        final float invZRes = 1.0f / zRes;

        for (rowFactor = 0, i = 0, index = 0; rowFactor < 1; i++, rowFactor = rowStep * i) {

            for (colFactor = 0, j = 0; colFactor < 1; j++, colFactor = colStep * j, index++) {

                // get point along horiz. line
                x = topLeft.X + (colFactor * colDeltaX) + rowOffsetX;
                y = topLeft.Y + (colFactor * colDeltaY) + rowOffsetY;
                z = topLeft.Z + (colFactor * colDeltaZ) + rowOffsetZ;

                x *= invXRes;
                y *= invYRes;
                z *= invZRes;

                plane[index] = getFloatTriLinearBounds((float) x, (float) y, (float) z);
            }

            // move down one row using vert. line
            rowOffsetX = rowFactor * rowDeltaX;
            rowOffsetY = rowFactor * rowDeltaY;
            rowOffsetZ = rowFactor * rowDeltaZ;
        }

        return plane;
    }

    /**
     * Method that returns the registration frame if it exists else returns null.
     * 
     * @return registration frame
     */
    public ViewJFrameRegistration getRegistrationFrame() {

        for (int i = 0; i < frameList.size(); i++) {

            if (frameList.elementAt(i) instanceof ViewJFrameRegistration) {
                return (ViewJFrameRegistration) frameList.elementAt(i);
            }
        }

        return null;
    }

    /**
     * Takes input x,y,z coordinate and returns that point transformed into the scanner's (DICOM) coordinate system. L =
     * Left - first axis is positive to the left P = Posterior - second axis is positive to the posterior S = Superior -
     * third axis is positive to the superior
     * 
     * @param x Absolute x value in slice.
     * @param y Absolute y value in slice.
     * @param z Absolute z value in slice.
     * @param scannerCoord the point transformed into the scanner's (DICOM) coordinate system.
     */
    public void getScannerCoordLPS(final int x, final int y, final int z, final float[] scannerCoord) {

        if ( ( !matrixHolder.containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                && (getFileInfo()[0].getFileFormat() != FileUtility.XML)
                && (getFileInfo()[0].getFileFormat() != FileUtility.MINC)
                && (getFileInfo()[0].getFileFormat() != FileUtility.NIFTI)
                && (getFileInfo()[0].getFileFormat() != FileUtility.AFNI)) {
            return;
        }

        int nDims = getNDims();

        if (nDims > 3) {
            nDims = 3;
        }

        final float[] coord = new float[3];
        final float[] tCoord = new float[3];
        final float[] origin = new float[3];
        final float[] res = new float[3];

        // Get the voxel coordinate in from mouse events in image space
        coord[0] = x;
        coord[1] = y;
        coord[2] = z;

        // Get image origin
        origin[0] = getFileInfo()[0].getOrigin()[0];
        origin[1] = getFileInfo()[0].getOrigin()[1];
        origin[2] = getFileInfo()[0].getOrigin()[2];
        // System.out.println("Origin " + origin[0] + ", " + origin[1] + ", " + origin[2] );

        for (int j = 0; j < 3; j++) {

            if ( (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_L2R_TYPE)
                    || (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_R2L_TYPE)) {
                origin[0] = getFileInfo()[0].getOrigin()[j];

            } else if ( (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_P2A_TYPE)
                    || (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_A2P_TYPE)) {
                origin[1] = getFileInfo()[0].getOrigin()[j];

            } else if ( (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_S2I_TYPE)
                    || (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_I2S_TYPE)) {
                origin[2] = getFileInfo()[0].getOrigin()[j];

            }
        }
        // origin in LPS order

        // Get voxel resolutions
        res[0] = getFileInfo(0).getResolutions()[0];
        res[1] = getFileInfo(0).getResolutions()[1];
        res[2] = getFileInfo(0).getResolutions()[2];
        // System.out.println("res " + res[0] + ", " + res[1] + ", " + res[2]);

        // Change voxel coordinate into millimeter space
        coord[0] = coord[0] * res[0];
        coord[1] = coord[1] * res[1];
        coord[2] = coord[2] * res[2];

        // System.out.println("dicomMatrix = " + dicomMatrix.toString());
        if ( (matrixHolder.containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                || (getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {

            // System.out.println("dicomMatrix = " + dicomMatrix.toString());
            final TransMatrix dicomMatrix = (getMatrix().clone());

            // Finally convert the point to axial millimeter DICOM space.
            dicomMatrix.transform(coord, tCoord);

            // Add in the
            scannerCoord[0] = origin[0] + tCoord[0];
            scannerCoord[1] = origin[1] + tCoord[1];
            scannerCoord[2] = origin[2] + tCoord[2];
        } else {

            for (int j = 0; j < 3; j++) {

                if (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_L2R_TYPE) {
                    tCoord[0] = -coord[j];
                } else if (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_R2L_TYPE) {
                    tCoord[0] = coord[j];
                } else if (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_P2A_TYPE) {
                    tCoord[1] = -coord[j];
                } else if (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_A2P_TYPE) {
                    tCoord[1] = coord[j];
                } else if (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_S2I_TYPE) {
                    tCoord[2] = -coord[j];
                } else if (getFileInfo(0).getAxisOrientation()[j] == FileInfoBase.ORI_I2S_TYPE) {
                    tCoord[2] = coord[j];
                }
            }

            scannerCoord[0] = origin[0] + tCoord[0];
            scannerCoord[1] = origin[1] + tCoord[1];
            scannerCoord[2] = origin[2] + tCoord[2];

        }
    }

    /**
     * Takes input x,y,z coordinate and returns that point transformed into the RAS coordinate system. R = Right - first
     * axis is positive to the right A = Anterior - second axis is positive to the anterior S = Superior - third axis is
     * positive to the superior
     * 
     * @param x Absolute x value in slice.
     * @param y Absolute y value in slice.
     * @param z Absolute z value in slice.
     * @param scannerCoord the point transformed into the scanner's (DICOM) coordinate system.
     */

    public void getScannerCoordRAS(final int x, final int y, final int z, final float[] scannerCoord) {

        if ( !matrixHolder.containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
            return;
        }

        int nDims = getNDims();

        if (nDims > 3) {
            nDims = 3;
        }

        getScannerCoordLPS(x, y, z, scannerCoord);

        scannerCoord[0] = -scannerCoord[0];
        scannerCoord[1] = -scannerCoord[1];
    }

    /**
     * Accessor that returns the talairach transform information.
     * 
     * @return TalairachTransformInfo talairach info
     */
    public TalairachTransformInfo getTalairachTransformInfo() {
        return talairach;
    }

    /**
     * Method that returns the tri image frame if it exists else returns null.
     * 
     * @return tri image frame
     */
    public ViewJFrameTriImage getTriImageFrame() {

        for (int i = 0; i < frameList.size(); i++) {

            if (frameList.elementAt(i) instanceof ViewJFrameTriImage) {
                return (ViewJFrameTriImage) frameList.elementAt(i);
            }
        }

        return null;
    }

    /**
     * Returns the reference to the user interface.
     * 
     * @deprecated DOCUMENT ME!
     * 
     * @return the reference to the user interface.
     */
    public ViewUserInterface getUserInterface() {
        return UI;
    }

    /**
     * Accessor that returns.
     * 
     * @return VOI vector
     */
    public VOIVector getVOIs() {
        return voiVector;
    }

    
    public VOIVector getVOIsCopy()
    {
        return new VOIVector(voiVector);
    }

    /**
     * Forms a single VOI structure from all the active VOIs presently loaded in the imageModel.
     */
    public void groupVOIs() {
        int i, k;
        int nVOIs;
        int nContours;
        Vector<VOIBase> contours;
        VOI newVOI = null;
        VOI newPtVOI = null;
        VOI newPLineVOI = null;
        VOI newLineVOI = null;
        VOI newProtractorVOI = null;

        String nameExt = null;

        if (getNDims() > 2) {
            nameExt = new String("3D");
        } else {
            nameExt = new String("2D");
        }

        VOIVector tempVOIs = new VOIVector(voiVector);

        VOIBase tempBase = null;
        nVOIs = tempVOIs.size();

        for (i = nVOIs - 1; i >= 0; i--) {

            if ( (tempVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) && tempVOIs.VOIAt(i).isActive()) {

                if (newVOI == null) {
                    newVOI = new VOI((short) 0, "joinedContour", VOI.CONTOUR, -1.0f);
                    newVOI.setAllActive(true);
                }
                contours = tempVOIs.VOIAt(i).getCurves();
                nContours = contours.size();
                for (k = nContours - 1; k >= 0; k--) {
                    if (((VOIBase) contours.elementAt(k)).isActive()) {
                        tempBase = (VOIContour) contours.elementAt(k);
                        contours.removeElementAt(k);
                        newVOI.getCurves().addElement(tempBase);
                    }
                }

                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }

            } else if ( (tempVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) && tempVOIs.VOIAt(i).isActive()) {

                if (newPLineVOI == null) {
                    newPLineVOI = new VOI((short) 0, "polyLine" + nameExt, VOI.POLYLINE, -1.0f);
                    newPLineVOI.setAllActive(true);
                }

                contours = tempVOIs.VOIAt(i).getCurves();
                nContours = contours.size();

                for (k = nContours - 1; k >= 0; k--) {

                    if (((VOIBase) contours.elementAt(k)).isActive()) {
                        tempBase = (VOIContour) contours.elementAt(k);
                        contours.removeElementAt(k);
                        newPLineVOI.getCurves().addElement(tempBase);
                    }
                }
                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }

            } else if ( (tempVOIs.VOIAt(i).getCurveType() == VOI.LINE) && tempVOIs.VOIAt(i).isActive()) {

                if (newLineVOI == null) {
                    newLineVOI = new VOI((short) 0, "line" + nameExt, VOI.LINE, -1.0f);
                    newLineVOI.setAllActive(true);
                }
                contours = tempVOIs.VOIAt(i).getCurves();
                nContours = contours.size();

                for (k = nContours - 1; k >= 0; k--) {

                    if (((VOIBase) contours.elementAt(k)).isActive()) {
                        tempBase = (VOILine) contours.elementAt(k);
                        contours.removeElementAt(k);
                        newLineVOI.getCurves().addElement(tempBase);
                    }
                }

                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }

            } else if ( (tempVOIs.VOIAt(i).getCurveType() == VOI.POINT) && tempVOIs.VOIAt(i).isActive()) {

                if (newPtVOI == null) {
                    newPtVOI = new VOI((short) 0, "point" + nameExt, VOI.POINT, -1.0f);
                    newPtVOI.setAllActive(true);
                }
                contours = tempVOIs.VOIAt(i).getCurves();
                nContours = contours.size();
                for (k = nContours - 1; k >= 0; k--) {

                    if (tempVOIs.VOIAt(i).getCurves().elementAt(k).isActive()) {
                        tempBase = (VOIPoint) contours.elementAt(k);
                        contours.removeElementAt(k);
                        newProtractorVOI.getCurves().addElement(tempBase);
                    }
                }
                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }
            } else if ( (tempVOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) && tempVOIs.VOIAt(i).isActive()) {

                if (newProtractorVOI == null) {
                    newProtractorVOI = new VOI((short) 0, "protractor" + nameExt, VOI.PROTRACTOR, -1.0f);
                    newProtractorVOI.setAllActive(true);
                }
                contours = tempVOIs.VOIAt(i).getCurves();
                nContours = contours.size();

                for (k = nContours - 1; k >= 0; k--) {

                    if (tempVOIs.VOIAt(i).getCurves().elementAt(k).isActive()) {
                        tempBase = (VOIProtractor) contours.elementAt(k);
                        contours.removeElementAt(k);
                        newProtractorVOI.getCurves().addElement(tempBase);
                    }
                }
                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }
            }
        }
        if (newVOI != null) {
            tempVOIs.addElement(newVOI);
        }

        if (newPtVOI != null) {
            tempVOIs.addElement(newPtVOI);
        }

        if (newPLineVOI != null) {
            tempVOIs.addElement(newPLineVOI);
        }

        if (newLineVOI != null) {
            tempVOIs.addElement(newLineVOI);
        }

        if (newProtractorVOI != null) {
            tempVOIs.addElement(newProtractorVOI);
        }

        voiVector.removeAllElements();
        setVOIs(tempVOIs);
        tempVOIs = null;

        /**
         * System.err.println("\n\nGrouping DEBUG INFO:"); VOI tempVOI = null; Vector [] tempCurves = null; //temp stuff
         * for debugging purposes for (i = 0; i < voiVector.size(); i++) { tempVOI = voiVector.VOIAt(i);
         * System.err.println( i + ": VOI name: " + tempVOI.getName()); tempCurves = tempVOI.getCurves(); for (j = 0; j <
         * tempCurves.length; j++) { System.err.println("\tSize: " + tempCurves[j].size()); System.err.println("\t" +
         * tempCurves[j].toString()); } }
         */
        for (i = 0; i < voiVector.size(); i++) {
            (voiVector.elementAt(i)).setID((short) i);
        }

        notifyImageDisplayListeners();

    }

    /**
     * Forms a single VOI structure from all of the VOIs presently loaded in the imageModel.
     * 
     * @param newVOIVector a new ViewVOIVector to hold the grouped VOIs
     * @param where int array telling where to sort
     * @param name the name of the VOI
     */
    public void groupVOIs(final ViewVOIVector newVOIVector, final int[] where, final String name) {
        // System.err.println("calling group VOIs, passing in new vector");

        int i, j, k;
        int nVOIs;
        int nContours;
        Vector<VOIBase> contours;
        VOI newVOI = null;
        VOI newPtVOI = null;
        VOI newPLineVOI = null;
        VOI newLineVOI = null;
        VOI newProtractorVOI = null;

        String nameExt = null;

        if (getNDims() > 2) {
            nameExt = new String("3D");
        } else {
            nameExt = new String("2D");
        }

        nVOIs = newVOIVector.size();

        for (i = 0; i < nVOIs; i++) {

            if (newVOIVector.VOIAt(i).getCurveType() == VOI.CONTOUR) {

                if (newVOI == null) {
                    newVOI = new VOI((short) 0, "polygon", VOI.CONTOUR, -1.0f);
                }
                contours = newVOIVector.VOIAt(i).getCurves();
                nContours = contours.size();
                for (k = 0; k < nContours; k++) {
                    newVOI.getCurves().addElement(contours.elementAt(k));
                }
            } else if (newVOIVector.VOIAt(i).getCurveType() == VOI.POLYLINE) {

                if (newPLineVOI == null) {
                    newPLineVOI = new VOI((short) 0, "polyline" + nameExt, VOI.POLYLINE, -1.0f);
                }
                contours = voiVector.VOIAt(i).getCurves();
                nContours = contours.size();
                for (k = 0; k < nContours; k++) {
                    newPLineVOI.getCurves().addElement(contours.elementAt(k));
                }
            } else if (newVOIVector.VOIAt(i).getCurveType() == VOI.LINE) {

                if (newLineVOI == null) {
                    newLineVOI = new VOI((short) 0, "line" + nameExt, VOI.LINE, -1.0f);
                }
                contours = voiVector.VOIAt(i).getCurves();
                nContours = contours.size();

                for (k = 0; k < nContours; k++) {
                    newLineVOI.getCurves().addElement(contours.elementAt(k));
                }
            } else if (newVOIVector.VOIAt(i).getCurveType() == VOI.POINT) {

                if (newPtVOI == null) {
                    newPtVOI = new VOI((short) 0, "point" + nameExt, VOI.POINT, -1.0f);
                }
                contours = newVOIVector.VOIAt(i).getCurves();
                nContours = contours.size();
                for (k = 0; k < nContours; k++) {
                    newVOI.getCurves().addElement(contours.elementAt(k));
                }
            } else if (voiVector.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {

                if (newProtractorVOI == null) {
                    newProtractorVOI = new VOI((short) 0, "protractor" + nameExt, VOI.PROTRACTOR, -1.0f);
                }
                contours = voiVector.VOIAt(i).getCurves();
                nContours = contours.size();

                for (k = 0; k < nContours; k++) {
                    newProtractorVOI.getCurves().addElement(contours.elementAt(k));
                }
            }
        }

        // Sort where so that the length of the vector won't get screwed up
        for (i = 1; i < where.length; i++) {
            final int tmp = where[i];

            for (j = i; (j > 0) && (tmp < where[j - 1]); j--) {
                where[j] = where[j - 1];
            }

            where[j] = tmp;
        }

        for (i = where.length - 1; i >= 0; i--) {
            voiVector.removeElementAt(where[i]);
        }

        if (newVOI != null) {
            voiVector.addElement(newVOI);
        }

        if (newPtVOI != null) {
            voiVector.addElement(newPtVOI);
        }

        if (newPLineVOI != null) {
            voiVector.addElement(newPLineVOI);
        }

        if (newLineVOI != null) {
            voiVector.addElement(newLineVOI);
        }

        if (newProtractorVOI != null) {
            voiVector.addElement(newProtractorVOI);
        }

        for (i = 0; i < voiVector.size(); i++) {
            voiVector.elementAt(i).setID((short) i);
        }
        notifyImageDisplayListeners();
    }

    /**
     * Accessor that returns whether or not the image is a color image.
     * 
     * @return <code>true</code> if color, <code>false</code> if not color.
     */
    public boolean isColorImage() {

        if ( (getType() == ModelStorageBase.ARGB) || (getType() == ModelStorageBase.ARGB_USHORT)
                || (getType() == ModelStorageBase.ARGB_FLOAT)) {
            return true;
        } else {
            return false;
        }
    }

    public boolean is2DImage() {
        return (getExtents().length == 2);
    }

    public boolean is3DImage() {
        return (getExtents().length == 3);
    }

    public boolean is4DImage() {
        return (getExtents().length == 4);
    }

    /**
     * Accessor that returns whether or not the image is a COMPLEX or DCOMPLEX image.
     * 
     * @return <code>true</code> if complex, <code>false</code> if not complex.
     */
    public boolean isComplexImage() {

        if ( (getDataType() == DataType.COMPLEX) || (getDataType() == DataType.DCOMPLEX)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Accessor that returns whether or not the image is a DICOM image.
     * 
     * @return <code>true</code> if DICOM, <code>false</code> if not DICOM.
     */
    public boolean isDicomImage() {

        if (fileInfo[0].getFileFormat() == FileUtility.DICOM) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns true if the ModelImage 3D dimensions combine to be a power of two.
     * @return true if the ModelImage 3D dimensions combine to be a power of two.
     */
    public boolean isPowerOfTwo()
    {
    	int size = 1;
    	for ( int i = 0; i < Math.min( 3, getExtents().length); i++ )
    	{
    		size *= getExtents()[i];
    	}
    	return MipavMath.isPowerOfTwo(size);
    }

    /**
     * Returns true if the ModelImage 2D dimensions (slice size) combine to be a power of two.
     * @return true if the ModelImage 2D dimensions (slice size) combine to be a power of two.
     */
    public boolean isSlicePowerOfTwo()
    {
    	int size = 1;
    	for ( int i = 0; i < Math.min( 2, getExtents().length); i++ )
    	{
    		size *= getExtents()[i];
    	}
    	return MipavMath.isPowerOfTwo(size);
    }

    /**
     * Accessor that returns whether or not the image is a MINC image.
     * 
     * @return <code>true</code> if MINC, <code>false</code> if not MINC.
     */
    public boolean isMincImage() {

        if (fileInfo[0].getFileFormat() == FileUtility.MINC) {
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Accessor that returns whether or not the image is a GE_SIGNA4X image.
     * 
     * @return <code>true</code> if GE_SIGNA4X, <code>false</code> if not GE_SIGNA4X.
     */
    public boolean isGE_Signa4XImage() {
    	if (fileInfo[0].getFileFormat() == FileUtility.GE_SIGNA4X){
    		return true;
    	}
    	else {
    		return false;
    	}
    }
    
    /**
     * Accessor that returns whether or not the image is a GE_GENESIS image.
     * 
     * @return <code>true</code> if GE_GENESIS, <code>false</code> if not GE_GENESIS.
     */
    public boolean isGE_GenesisImage() {
    	if (fileInfo[0].getFileFormat() == FileUtility.GE_GENESIS){
    		return true;
    	}
    	else {
    		return false;
    	}
    }

    /**
     * Used to notify all frames that display this image model need to be updated.
     */
    public synchronized void notifyImageDisplayListeners() {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ( (frameList.elementAt(i) instanceof ViewJFrameBase)) {
                frameList.elementAt(i).updateImages();
            }
        }
    }

    /**
     * Used to notify all frames except the triImage frames that display this image model need to be updated.
     */
    public synchronized void notifyImageDisplayListeners_notTriFrame() {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ( (frameList.elementAt(i) instanceof ViewJFrameBase)) {
                if ( ! (frameList.elementAt(i) instanceof gov.nih.mipav.view.ViewJFrameTriImage)) {
                    frameList.elementAt(i).updateImages();
                }
            }
        }
    }

    /**
     * Used to notify all frames that display this image model need to be updated.
     * 
     * @param LUT new LUT used to display image (can be null);
     * @param forceShow force the display method(s) to reload image data and display image slower but needed if image
     *            model changes.
     */
    public void notifyImageDisplayListeners(final ModelLUT LUT, final boolean forceShow) {

        if (frameList != null) {
            for (int i = 0; i < frameList.size(); i++) {

                if ( (frameList.elementAt(i) instanceof ViewJFrameBase)) {
                    final ModelImage imgA = ((ViewJFrameBase) frameList.elementAt(i)).getImageA();
                    final ModelImage imgB = ((ViewJFrameBase) frameList.elementAt(i)).getImageB();

                    if (this == imgA) {
                        frameList.elementAt(i).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgB) {
                        frameList.elementAt(i).updateImages(null, LUT, forceShow, -1);
                    }
                } /* LUT update of a non-ModelImage data strucuture: */
                else if ( (frameList.elementAt(i) instanceof VolumeTriPlanarInterface)) {
                    final ModelImage imgA = ((VolumeTriPlanarInterface) frameList.elementAt(i)).getImageA();
                    final ModelImage imgB = ((VolumeTriPlanarInterface) frameList.elementAt(i)).getImageB();

                    if (this == imgA) {
                        frameList.elementAt(i).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgB) {
                        frameList.elementAt(i).updateImages(null, LUT, forceShow, -1);
                    }
                } /* LUT update of a non-ModelImage data strucuture: */
                else if ( (frameList.elementAt(i) instanceof gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview.JPanelBrainSurfaceFlattener)) {
                    final ModelImage imgA = ((gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview.JPanelBrainSurfaceFlattener) frameList
                            .elementAt(i)).getImageA();
                    final ModelImage imgB = ((gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview.JPanelBrainSurfaceFlattener) frameList
                            .elementAt(i)).getImageB();

                    if (this == imgA) {
                        frameList.elementAt(i).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgB) {
                        frameList.elementAt(i).updateImages(null, LUT, forceShow, -1);
                    }
                } else if ( (frameList.elementAt(i) instanceof gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.JPanelBrainSurfaceFlattener_WM)) {
                    final ModelImage imgA = ((gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.JPanelBrainSurfaceFlattener_WM) frameList
                            .elementAt(i)).getImageA();
                    final ModelImage imgB = ((gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.JPanelBrainSurfaceFlattener_WM) frameList
                            .elementAt(i)).getImageB();

                    if (this == imgA) {
                        frameList.elementAt(i).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgB) {
                        frameList.elementAt(i).updateImages(null, LUT, forceShow, -1);
                    }
                } else if ( (frameList.elementAt(i) instanceof gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture)) {
                    final ModelImage imgS = ((gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture) frameList
                            .elementAt(i)).getImageSeparate();
                    final ModelImage imgL = ((gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture) frameList
                            .elementAt(i)).getImageLink();

                    if (this == imgS) {
                        frameList.elementAt(i).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgL) {
                        frameList.elementAt(i).updateImages(null, LUT, forceShow, -1);
                    }
                } else if ( (frameList.elementAt(i) instanceof gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM)) {
                    final ModelImage imgS = ((gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM) frameList
                            .elementAt(i)).getImageSeparate();
                    final ModelImage imgL = ((gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM) frameList
                            .elementAt(i)).getImageLink();

                    if (this == imgS) {
                        frameList.elementAt(i).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgL) {
                    	frameList.elementAt(i).updateImages(null, LUT, forceShow, -1);
                    }
                } else if ( (frameList.elementAt(i) instanceof gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI)) { 
                    frameList.elementAt(i).updateImages();
                } else { 
                    frameList.elementAt(i).updateImages();
                }

            }
        }

        if ( (getHistoLUTFrame() != null) && (forceShow == true)) {

            if (getHistoLUTFrame().getImageA() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_A);
            } else if (getHistoLUTFrame().getImageB() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_B);
            }
        }

        if ( (getHistoRGBFrame() != null) && (forceShow == true)) {

            if (getHistoRGBFrame().getImageA() == this) {
                getHistoRGBFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_A);
            } else if (getHistoRGBFrame().getImageB() == this) {
                getHistoRGBFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_B);
            }
        }
    }

    /**
     * Used to notify all frames that display this image model need to be updated for RGB (color) images.
     * 
     * @param forceShow force the display method(s) to reload image data and display image slower but needed if image
     *            model changes.
     * @param alphaBlend the amount to blend between two images displayed in the same frame.
     * @param RGBT ModelRGB
     */
    public void notifyImageDisplayListeners(final boolean forceShow, final int alphaBlend, final ModelRGB RGBT) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ( (frameList.elementAt(i) instanceof ViewJFrameBase)) {
                final ModelImage imgA = ((ViewJFrameBase) frameList.elementAt(i)).getImageA();
                final ModelImage imgB = ((ViewJFrameBase) frameList.elementAt(i)).getImageB();

                if (this == imgA) {
                    ((ViewJFrameBase) frameList.elementAt(i)).setRGBTA(RGBT);
                } else if (this == imgB) {
                    ((ViewJFrameBase) frameList.elementAt(i)).setRGBTB(RGBT);
                }

                frameList.elementAt(i).updateImages(null, null, forceShow, -1);
            } else if ( (frameList.elementAt(i) instanceof VolumeTriPlanarInterface)) {
                final ModelImage imgA = ((VolumeTriPlanarInterface) frameList.elementAt(i)).getImageA();
                final ModelImage imgB = ((VolumeTriPlanarInterface) frameList.elementAt(i)).getImageB();

                if (this == imgA) {
                    ((VolumeTriPlanarInterface) frameList.elementAt(i)).setRGBTA(RGBT);
                } else if (this == imgB) {
                    ((VolumeTriPlanarInterface) frameList.elementAt(i)).setRGBTB(RGBT);
                }

                frameList.elementAt(i).updateImages(null, null, forceShow, -1);
            } else if ( (frameList.elementAt(i) instanceof gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture)) {
                final ModelImage imgS = ((gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture) frameList
                        .elementAt(i)).getImageSeparate();
                final ModelImage imgL = ((gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture) frameList
                        .elementAt(i)).getImageLink();

                if (this == imgS) {
                    ((gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture) frameList.elementAt(i))
                            .setRGBTA(RGBT);
                } else if (this == imgL) {
                    ((gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurfaceTexture) frameList.elementAt(i))
                            .setRGBTB(RGBT);
                }
            } else if ( (frameList.elementAt(i) instanceof gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM)) {
                final ModelImage imgS = ((gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM) frameList
                        .elementAt(i)).getImageSeparate();

                if (this == imgS) {
                    ((gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM) frameList.elementAt(i))
                            .setRGBTA(RGBT);
                }
                
            }

        }

        if ( (getHistoLUTFrame() != null) && (forceShow == true)) {

            if (getHistoLUTFrame().getImageA() == this) {
                getHistoLUTFrame().notifyOfUpdate(null, ViewJFrameBase.IMAGE_A);
            } else if (getHistoLUTFrame().getImageB() == this) {
                getHistoLUTFrame().notifyOfUpdate(null, ViewJFrameBase.IMAGE_B);
            }
        }

        if ( (getHistoRGBFrame() != null) && (forceShow == true)) {

            if (getHistoRGBFrame().getImageA() == this) {
                getHistoRGBFrame().notifyOfUpdate(null, ViewJFrameBase.IMAGE_A);
            } else if (getHistoRGBFrame().getImageB() == this) {
                getHistoRGBFrame().notifyOfUpdate(null, ViewJFrameBase.IMAGE_B);
            }
        }
    }

    /**
     * Used to notify all listeners that the image is to be redisplayed.
     * 
     * @param LUT new LUT used to display image (can be null);
     * @param forceShow force the display method(s) to reload image data and display image slower but needed if image
     *            model changes.
     * @param alphaBlend indicates the amount of blending between two images (image 1's blending value) 1.0 - all of
     *            image 1; 0.5 - half image 1 and half image 2
     * @param interpMode image interpolation method (Nearest or Smooth)
     */
    public void notifyImageDisplayListeners(final ModelLUT LUT, final boolean forceShow, final int alphaBlend,
            final int interpMode) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ( (frameList.elementAt(i) instanceof ViewJFrameBase)) {
                final ModelImage imgA = ((ViewJFrameBase) frameList.elementAt(i)).getImageA();
                final ModelImage imgB = ((ViewJFrameBase) frameList.elementAt(i)).getImageB();

                ((ViewJFrameBase) frameList.elementAt(i)).setAlphaBlend(alphaBlend);

                if (this == imgA) {
                    frameList.elementAt(i).updateImages(LUT, null, forceShow, interpMode);
                } else if (this == imgB) {
                    frameList.elementAt(i).updateImages(null, LUT, forceShow, interpMode);
                }
            }
        }

        if ( (getHistoLUTFrame() != null) && (forceShow == true)) {

            if (getHistoLUTFrame().getImageA() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_A);
            } else if (getHistoLUTFrame().getImageB() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_B);
            }
        }

        if ( (getHistoRGBFrame() != null) && (forceShow == true)) {

            if (getHistoRGBFrame().getImageA() == this) {
                getHistoRGBFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_A);
            } else if (getHistoRGBFrame().getImageB() == this) {
                getHistoRGBFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_B);
            }
        }
    }

    /**
     * Used to notify all frames that display this image model that this image's extents have changed. The display of
     * this image may also need to be updated.
     */
    public synchronized void notifyImageExtentsListeners() {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ( (frameList.elementAt(i) instanceof ViewJFrameBase)) {
                frameList.elementAt(i).updateImageExtents();
            }
        }
    }

    /**
     * Prints basic image parameters to the System.out.
     */
    public void print() {
        Preferences.debug(toString());
    }

    /**
     * Read matrix from a file.
     * 
     * @param composite if true make a composite matrix of the by multipling this matrix with the one to be read from
     *            the file. If false replace this object matrix with a new matrix read from the file.
     * 
     * @return DOCUMENT ME!
     */
    public TransMatrix readTransformMatrix(final boolean composite) {
        final TransMatrix newMatrix = new TransMatrix(getNDims() + 1);
        String fileName, directory;
        JFileChooser chooser;

        // bring up file dialog
        try {
            chooser = new JFileChooser();

            if (UI.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MATRIX));

            final int returnVal = chooser.showOpenDialog(UI.getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                UI.setDefaultDirectory(directory);
            } else {
                return null;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ModelImage.readTransformMatrix");

            return null;
        }

        try {
            final File file = new File(UI.getDefaultDirectory() + fileName);
            final RandomAccessFile raFile = new RandomAccessFile(file, "r");

            newMatrix.readMatrix(raFile, composite);
            raFile.close();
        } catch (final IOException error) {
            MipavUtil.displayError("Matrix read error");

            return null;
        }

        return newMatrix;
    }

    /**
     * Reallocates ModelImage with new type and all image data lost.
     * 
     * @param type new type of image that is to be allocated
     */
    public void reallocate(final int type) {
        final int[] dimExtents = getExtents();
        int length = dimExtents[0]*dimExtents[1];
        int originalType = this.getFileInfo()[0].getDataType();
        boolean endianess = this.getFileInfo()[0].getEndianess();
        boolean originalEndianess = this.getFileInfo()[0].getOriginalEndianess();
        boolean changedEndianess = false;
        if (endianess != originalEndianess) {
        	changedEndianess = true;
        }
        int originalLength;
        if (dimExtents.length == 2) {
            fileInfo[0].setDataType(type);
        } else if (dimExtents.length == 3) {
            length *= dimExtents[2];
            for (int i = 0; i < dimExtents[2]; i++) {
                fileInfo[i].setDataType(type);
            }
        } else if (dimExtents.length == 4) {
            length *= dimExtents[2]*dimExtents[3];
            for (int i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
                fileInfo[i].setDataType(type);
            }
        }
        originalLength = length;

        try {
            super.reallocate(type);
        } catch (final IOException ioError) {
            MipavUtil.displayError("" + ioError);
        }
        
        // Afni only uses setDataType in FileInfoBase
        
        if (fileInfo[0] instanceof FileInfoNIFTI) {
        	short niftiType;
        	short niftiBits;
        	switch (type) {

            case ModelStorageBase.BOOLEAN:
                niftiType = FileInfoNIFTI.DT_BINARY;
                niftiBits = (short)1;
                break;

            case ModelStorageBase.BYTE:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_INT8;
                niftiBits = (short)8;
                break;

            case ModelStorageBase.UBYTE:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_UINT8;
                niftiBits = (short)8;
                break;

            case ModelStorageBase.SHORT:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_INT16;
                niftiBits = (short)16;
                break;

            case ModelStorageBase.USHORT:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_UINT16;
                niftiBits = (short)16;
                break;

            case ModelStorageBase.INTEGER:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_INT32;
                niftiBits = (short)32;
                break;

            case ModelStorageBase.UINTEGER:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_UINT32;
                niftiBits = (short)32;
                break;

            case ModelStorageBase.LONG:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_INT64;
                niftiBits = (short)64;
                break;

            case ModelStorageBase.FLOAT:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_FLOAT32;
                niftiBits = (short)32;
                break;

            case ModelStorageBase.DOUBLE:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_FLOAT64;
                niftiBits = (short)64;
                break;

            case ModelStorageBase.ARGB: 
                niftiType = FileInfoNIFTI.NIFTI_TYPE_RGB24;
                niftiBits = (short)24;
                break;

            case ModelStorageBase.COMPLEX:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_COMPLEX64;
                niftiBits = (short)64;
                break;

            case ModelStorageBase.DCOMPLEX:
                niftiType = FileInfoNIFTI.NIFTI_TYPE_COMPLEX128;
                niftiBits = (short)128;
                break;
            default:
            	MipavUtil.displayError("Illegal NIFTI data type");
            	return;
            
    }

    	if (dimExtents.length == 2) {
    		((FileInfoNIFTI)(fileInfo[0])).setSourceType(niftiType);
    		((FileInfoNIFTI)(fileInfo[0])).setBitPix(niftiBits);
        } else if (dimExtents.length == 3) {

            for (int i = 0; i < dimExtents[2]; i++) {
            	((FileInfoNIFTI)(fileInfo[i])).setSourceType(niftiType);
            	((FileInfoNIFTI)(fileInfo[i])).setBitPix(niftiBits);
            }
        } else if (dimExtents.length == 4) {

            for (int i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
            	((FileInfoNIFTI)(fileInfo[i])).setSourceType(niftiType);
            	((FileInfoNIFTI)(fileInfo[i])).setBitPix(niftiBits);
            }
        }

        	
        } // if (fileInfo[0] instanceof FileInfoNIFTI)
        else if (fileInfo[0] instanceof FileInfoAnalyze) {
        	short analyzeType;
        	short analyzeBits;
        	switch (type) {

            case ModelStorageBase.BOOLEAN:
                analyzeType = FileInfoAnalyze.DT_BINARY;
                analyzeBits = (short)1;
                break;

            case ModelStorageBase.BYTE:
                analyzeType = FileInfoAnalyze.DT_UNSIGNED_CHAR;
                analyzeBits = (short)8;
                break;

            case ModelStorageBase.UBYTE:
                analyzeType = FileInfoAnalyze.DT_UNSIGNED_CHAR;
                analyzeBits = (short)8;
                break;

            case ModelStorageBase.SHORT:
                analyzeType = FileInfoAnalyze.DT_SIGNED_SHORT;
                analyzeBits = (short)16;
                break;

            case ModelStorageBase.USHORT:
                analyzeType = FileInfoAnalyze.DT_UNSIGNED_SHORT;
                analyzeBits = (short)16;
                break;

            case ModelStorageBase.INTEGER:
                analyzeType = FileInfoAnalyze.DT_SIGNED_INT;
                analyzeBits = (short)32;
                break;

            case ModelStorageBase.UINTEGER:
                analyzeType = FileInfoAnalyze.DT_SIGNED_INT;
                analyzeBits = (short)32;
                break;

            case ModelStorageBase.LONG:
                MipavUtil.displayError("LONG data type illegal in Analyze");
                return;

            case ModelStorageBase.FLOAT:
                analyzeType = FileInfoAnalyze.DT_FLOAT;
                analyzeBits = (short)32;
                break;

            case ModelStorageBase.DOUBLE:
                analyzeType = FileInfoAnalyze.DT_DOUBLE;
                analyzeBits = (short)64;
                break;

            case ModelStorageBase.ARGB:
                analyzeType = FileInfoAnalyze.DT_RGB;
                analyzeBits = (short)24;
                break;

            case ModelStorageBase.COMPLEX:
                analyzeType = FileInfoAnalyze.DT_COMPLEX;
                analyzeBits = (short)64;
                break;
                
            case ModelStorageBase.DCOMPLEX:
            	MipavUtil.displayError("DCOMPLEX illegal analyze data type");
            	return;

            default:
            	MipavUtil.displayError("Illegal Analyze data type");
            	return;
            
        } 
        	
    	if (dimExtents.length == 2) {
    		((FileInfoAnalyze)(fileInfo[0])).setDataType(analyzeType);
    		((FileInfoAnalyze)(fileInfo[0])).setBitPix(analyzeBits);
        } else if (dimExtents.length == 3) {

            for (int i = 0; i < dimExtents[2]; i++) {
            	((FileInfoAnalyze)(fileInfo[i])).setDataType(analyzeType);
            	((FileInfoAnalyze)(fileInfo[i])).setBitPix(analyzeBits);
            }
        } else if (dimExtents.length == 4) {

            for (int i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
            	((FileInfoAnalyze)(fileInfo[i])).setDataType(analyzeType);
            	((FileInfoAnalyze)(fileInfo[i])).setBitPix(analyzeBits);
            }
        }
        	
        } // else if (fileInfo[0] instanceof FileInfoAnalyze)
        else if (fileInfo[0] instanceof FileInfoSPM) {
        	int spmType;
        	short spmBits;
        	switch (type) {

            case ModelStorageBase.BOOLEAN:
                spmType = FileInfoSPM.DT_BINARY;
                spmBits = (short)1;
                break;

            case ModelStorageBase.BYTE:
                spmType = FileInfoSPM.DT_BYTE;
                spmBits = (short)8;
                break;

            case ModelStorageBase.UBYTE:
                spmType = FileInfoSPM.DT_UNSIGNED_CHAR;
                spmBits = (short)8;
                break;

            case ModelStorageBase.SHORT:
                spmType = FileInfoSPM.DT_SIGNED_SHORT;
                spmBits = (short)16;
                break;

            case ModelStorageBase.USHORT:
                spmType = FileInfoSPM.DT_UNSIGNED_SHORT;
                spmBits = (short)16;
                break;

            case ModelStorageBase.INTEGER:
                spmType = FileInfoSPM.DT_SIGNED_INT;
                spmBits = (short)32;
                break;

            case ModelStorageBase.UINTEGER:
                spmType = FileInfoSPM.DT_UNSIGNED_INT;
                spmBits = (short)32;
                break;

            case ModelStorageBase.LONG:
                MipavUtil.displayError("LONG data type illegal in SPM");
                return;

            case ModelStorageBase.FLOAT:
                spmType = FileInfoSPM.DT_FLOAT;
                spmBits = (short)32;
                break;

            case ModelStorageBase.DOUBLE:
                spmType = FileInfoSPM.DT_DOUBLE;
                spmBits = (short)64;
                break;

            case ModelStorageBase.ARGB:
                spmType = FileInfoSPM.DT_RGB;
                spmBits = (short)24;
                break;

            case ModelStorageBase.COMPLEX:
                spmType = FileInfoSPM.DT_COMPLEX;
                spmBits = (short)64;
                break;
                
            case ModelStorageBase.DCOMPLEX:
            	MipavUtil.displayError("DCOMPLEX illegal SPM data type");
            	return;

            default:
            	MipavUtil.displayError("Illegal SPM data type");
            	return;
            
        } 
        	
    	if (dimExtents.length == 2) {
    		((FileInfoSPM)(fileInfo[0])).setDataType(spmType);
    		((FileInfoSPM)(fileInfo[0])).setBitPix(spmBits);
        } else if (dimExtents.length == 3) {

            for (int i = 0; i < dimExtents[2]; i++) {
            	((FileInfoSPM)(fileInfo[i])).setDataType(spmType);
            	((FileInfoSPM)(fileInfo[i])).setBitPix(spmBits);
            }
        } else if (dimExtents.length == 4) {

            for (int i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
            	((FileInfoSPM)(fileInfo[i])).setDataType(spmType);
            	((FileInfoSPM)(fileInfo[i])).setBitPix(spmBits);
            }
        }
        	
        } // else if (fileInfo[0] instanceof FileInfoSPM)
        else if (fileInfo[0] instanceof FileInfoMinc) {
        	boolean haveValidRange;
        	boolean haveValidMin;
        	boolean haveValidMax;
        	FileMincVarElem varArray[] = null;
        	int increaseBegin;
            int numSlices = 1;
            int imageIndex = 0;
            if (dimExtents.length >= 3) {
            	numSlices *= dimExtents[2];
            }
            if (dimExtents.length >= 4) {
            	numSlices *= dimExtents[3];
            }
            double defaultMin = -Double.MIN_VALUE;
            double defaultMax = Double.MAX_VALUE;
            double vmin;
            double vmax;
            switch (type) {

            case ModelStorageBase.BOOLEAN:
                MipavUtil.displayError("BOOLEAN illegal Minc data type");
                return;

            case ModelStorageBase.BYTE:
                defaultMin = -128.0;
                defaultMax = 127.0;
                break;

            case ModelStorageBase.UBYTE:
                defaultMin = 0.0;
                defaultMax = 255.0;
                break;

            case ModelStorageBase.SHORT:
                defaultMin = -32768.0;
                defaultMax = 32767.0;
                length *= 2;
                break;

            case ModelStorageBase.USHORT:
                defaultMin = 0.0;
                defaultMax = 65535.0;
                length *= 2;
                break;

            case ModelStorageBase.INTEGER:
                defaultMin = -2147483648.0;
                defaultMax = 2147483647.0;
                length *= 4;
                break;

            case ModelStorageBase.UINTEGER:
                defaultMin = 0.0;
                defaultMax = 4294967295.0;
                length *= 4;
                break;
               
            case ModelStorageBase.LONG:
                MipavUtil.displayError("LONG illegal Minc data type");
                return;

            case ModelStorageBase.FLOAT:
            	defaultMin = -Float.MAX_VALUE;
            	defaultMax = Float.MAX_VALUE;
            	length *= 4;
                break;

            case ModelStorageBase.DOUBLE:
            	defaultMin = -Double.MAX_VALUE;
            	defaultMax = Double.MAX_VALUE;
            	length *= 8;
                break;

            case ModelStorageBase.ARGB: 
                MipavUtil.displayError("ARGB illegal Minc data type");
                return;

            case ModelStorageBase.COMPLEX:
                MipavUtil.displayError("COMPLEX illegal Minc data type");
                return;

            case ModelStorageBase.DCOMPLEX:
                MipavUtil.displayError("DCOMPLEX illegal Minc data type");
                return;
            default:
            	MipavUtil.displayError("Illegal Minc data type");
            	return;
            }
            while ((length % 4) != 0) {
            	length++;
            }
            switch (originalType) {
            case SHORT:
            case USHORT:
            	originalLength *= 2;
            	break;
            case INTEGER:
            case UINTEGER:
            case FLOAT:
            	originalLength *= 4;
            	break;
            case DOUBLE:
            	originalLength *= 8;
            }
            while ((originalLength % 4) != 0) {
                originalLength++;	
            }
            // Note that the new buffer of values is not imported until
            // after the reallocation so valid_min, valid_max, and
            // valid_range values cannot be obtained from the image
            vmin = defaultMin;
            vmax = defaultMax;
            for (int i = 0; i  < numSlices; i++) {
                ((FileInfoMinc)fileInfo[i]).vmin = vmin;	
                ((FileInfoMinc)fileInfo[i]).vmax = vmax;
                varArray = ((FileInfoMinc)fileInfo[i]).getVarArray();
                if (varArray != null) {
                	increaseBegin = 0;
                	for (int j = 0; j < varArray.length; j++) {	
                        if (varArray[j].name.equals("image")) {
                        	imageIndex = j;
                        	varArray[j].vsize = length;
                            switch (type) {
                            case ModelStorageBase.BYTE:
                                varArray[j].signtype = new String("signed__");
                                varArray[j].nc_type = FileInfoMinc.NC_BYTE;
                                break;

                            case ModelStorageBase.UBYTE:
                            	varArray[j].signtype = new String("unsigned");
                                varArray[j].nc_type = FileInfoMinc.NC_BYTE;
                                break;

                            case ModelStorageBase.SHORT:
                            	varArray[j].signtype = new String("signed__");
                                varArray[j].nc_type = FileInfoMinc.NC_SHORT;
                                break;

                            case ModelStorageBase.USHORT:
                            	varArray[j].signtype = new String("unsigned");
                                varArray[j].nc_type = FileInfoMinc.NC_SHORT;
                                break;

                            case ModelStorageBase.INTEGER:
                            	varArray[j].signtype = new String("signed__");
                                varArray[j].nc_type = FileInfoMinc.NC_INT;
                                break;

                            case ModelStorageBase.UINTEGER:
                            	varArray[j].signtype = new String("unsigned");
                                varArray[j].nc_type = FileInfoMinc.NC_INT;
                                break;

                            case ModelStorageBase.FLOAT:
                            	varArray[j].nc_type = FileInfoMinc.NC_FLOAT;
                                break;

                            case ModelStorageBase.DOUBLE:
                            	varArray[j].nc_type = FileInfoMinc.NC_DOUBLE;
                                break;
                            
                        } 
                        haveValidRange = false;
                        haveValidMin = false;
                        haveValidMax = false;
                        for (int k = 0; k < varArray[j].vattArray.length; k++) {
                        	if (varArray[j].vattArray[k].name.equals("signtype")) {
                        		Character c[] = new Character[8];
                        		if ((type == ModelStorageBase.UBYTE) || (type == ModelStorageBase.USHORT) ||
                        		    (type == ModelStorageBase.UINTEGER)) {
                        			c[0] = Character.valueOf('u');
                        			c[1] = Character.valueOf('n');
                        			c[2] = Character.valueOf('s');
                        			c[3] = Character.valueOf('i');
                        			c[4] = Character.valueOf('g');
                        			c[5] = Character.valueOf('n');
                        			c[6] = Character.valueOf('e');
                        			c[7] = Character.valueOf('d');
                        		}
                        		else {
                        			c[0] = Character.valueOf('s');
                        			c[1] = Character.valueOf('i');
                        			c[2] = Character.valueOf('g');
                        			c[3] = Character.valueOf('n');
                        			c[4] = Character.valueOf('e');
                        			c[5] = Character.valueOf('d');
                        			c[6] = Character.valueOf('_');
                        			c[7] = Character.valueOf('_');
                        		}
                        		for (int m = 0; m < 8; m++) {
                        			varArray[j].vattArray[k].setValue(c[m], m);
                        		}
                        	}
                        	 if (varArray[j].vattArray[k].name.equals("valid_range")) {
                                 haveValidRange = true;	
                                 varArray[j].vattArray[k].setValue(Double.valueOf(vmin), 0);
                                 varArray[j].vattArray[k].setValue(Double.valueOf(vmax), 1);
                             }
                             if (varArray[j].vattArray[k].name.equals("valid_min")) {
                                 haveValidMin = true;
                                 varArray[j].vattArray[k].setValue(Double.valueOf(vmin), 0);
                             }
                             if (varArray[j].vattArray[k].name.equals("valid_max")) {
                             	haveValidMax = true;
                             	varArray[j].vattArray[k].setValue(Double.valueOf(vmax), 0);
                             }
                        } // for (int k = 0; k < varArray[j].vattArray.length; k++)
                        if ((!haveValidRange) && (!haveValidMin) && (!haveValidMax)) {
                        	int k = varArray[j].vattArray.length;
                        	FileMincAttElem vattArray2[] = new FileMincAttElem[k+1];

                            for (int m = 0; m < k; m++) {
                                vattArray2[m] = (FileMincAttElem) varArray[j].vattArray[m].clone();
                            }
                            varArray[j].createVattArray(k+1);
                            for (int m = 0; m < k; m++) {
                                varArray[j].vattArray[m] = (FileMincAttElem)vattArray2[m].clone();
                            }
                     	    varArray[j].addVattElem("valid_range", FileInfoMinc.NC_DOUBLE, 2, k);	
                     	    varArray[j].vattArray[k].setValue(Double.valueOf(vmin), 0);
                            varArray[j].vattArray[k].setValue(Double.valueOf(vmax), 1);
                            increaseBegin += 4; // length of string name
                            increaseBegin += 11; // bytes in string name
                            increaseBegin += 1; // pad to 4 byte boundary
                            increaseBegin += 4; // 4 bytes for nc_type
                            increaseBegin += 4; // 4 bytes for length of vattArray[k]
                            increaseBegin += 16; // 16 bytes in 2 doubles
                     	}
                        } // if (varArray[j].name.equals("image"))
                        if (varArray[j].name.equals("image-max")) {
                            for (int k = 0; k < varArray[j].values.size(); k++) {
                                varArray[j].values.setElementAt(Double.valueOf(defaultMax), k);
                            }
                        } // if (varArray[j].name.equals("image-max"))
                        if (varArray[j].name.equals("image-min")) {
                        	for (int k = 0; k < varArray[j].values.size(); k++) {
                                varArray[j].values.setElementAt(Double.valueOf(defaultMin), k);
                            }	
                        } // if (varArray[j].name.equals("image-min"))
                       
                	} // for (int j = 0; j < varArray.length; j++)
                	for (int j = 0; j < varArray.length; j++) {
                		if (j <= imageIndex) {
                			varArray[j].begin += increaseBegin;
                		}
                		else {
                			varArray[j].begin += increaseBegin + length - originalLength;
                		}
                	}
                } // if (varArray != null)
            } // for (int i = 0; i < numSlices; i++)
        } // else if (fileInfo[0] instanceof FileInfoMinc)
        
        else if (fileInfo[0] instanceof FileInfoMincHDF) {
        	
        	
        	
        	double defaultMin = -Double.MIN_VALUE;
            double defaultMax = Double.MAX_VALUE;

            
            int numSlices = 1;

            if (dimExtents.length >= 3) {
            	numSlices *= dimExtents[2];
            }
            if (dimExtents.length >= 4) {
            	numSlices *= dimExtents[3];
            }
            
            
            switch (type) {

            case ModelStorageBase.BOOLEAN:
                MipavUtil.displayError("BOOLEAN illegal Minc data type");
                return;

            case ModelStorageBase.BYTE:
                defaultMin = -128.0;
                defaultMax = 127.0;
                break;

            case ModelStorageBase.UBYTE:
                defaultMin = 0.0;
                defaultMax = 255.0;
                break;

            case ModelStorageBase.SHORT:
                defaultMin = -32768.0;
                defaultMax = 32767.0;
                length *= 2;
                break;

            case ModelStorageBase.USHORT:
                defaultMin = 0.0;
                defaultMax = 65535.0;
                length *= 2;
                break;

            case ModelStorageBase.INTEGER:
                defaultMin = -2147483648.0;
                defaultMax = 2147483647.0;
        
                break;

            case ModelStorageBase.UINTEGER:
                defaultMin = 0.0;
                defaultMax = 4294967295.0;
                length *= 4;
                break;
               
            case ModelStorageBase.LONG:
                MipavUtil.displayError("LONG illegal Minc data type");
                return;

            case ModelStorageBase.FLOAT:
            	defaultMin = -Float.MAX_VALUE;
            	defaultMax = Float.MAX_VALUE;
            	length *= 4;
                break;

            case ModelStorageBase.DOUBLE:
            	defaultMin = -Double.MAX_VALUE;
            	defaultMax = Double.MAX_VALUE;
            	length *= 8;
                break;

            case ModelStorageBase.ARGB: 
                MipavUtil.displayError("ARGB illegal Minc data type");
                return;

            case ModelStorageBase.COMPLEX:
                MipavUtil.displayError("COMPLEX illegal Minc data type");
                return;

            case ModelStorageBase.DCOMPLEX:
                MipavUtil.displayError("DCOMPLEX illegal Minc data type");
                return;
            default:
            	MipavUtil.displayError("Illegal Minc data type");
            	return;
            }
            
            double[] validRange = new double[2];
            
            
            validRange[0] = defaultMin;
            validRange[1] = defaultMax;
            
            
            for (int i = 0; i  < numSlices; i++) {
                //((FileInfoMincHDF)fileInfo[i]).setValidRange(validRange);
                ((FileInfoMincHDF)fileInfo[i]).setDataType(type);
        
            }

            
        }
        
        else if (fileInfo[0] instanceof FileInfoDicom) {
            // Under a rigid adherence to the DICOM standard most reallocations would be illegal
        	// because most image formats permit only 1 or 2 values in the (0028,0100) bits allocated
        	// field.  C.8.2.1.1.4 Bits Allocated says:
        	// "For CT Images, Bits Allocated (0028,0100) shall have the Enumerated Value of 16.
        	// C.8.3.1.1.4 Bits Allocated says:
        	// "For MR Images, Bits Allocated (0028,0100) shall have the Enumerated Value of 16.
        	// These and other similar requirements are ignored so that a type reallocation can be performed.
        	if (changedEndianess) {
        		MipavUtil.displayError("Cannot change dicom endianess");
        	    return;
        	} // if (changedEndianess)
        	short UNSIGNED_INTEGER = (short)0;
        	short SIGNED_INTEGER = (short)1;
        	short bitsAllocated = 8;
        	short bitsStored = 8;
        	short highBit = 7;
        	short pixelRepresentation = -1;
        	short bytesPerPixel;
        	switch (type) {

            case ModelStorageBase.BOOLEAN:
                bitsAllocated = (short)1;
                bitsStored  = (short)1;
                highBit = (short)0;
                bytesPerPixel = 1;
                break;

            case ModelStorageBase.BYTE:
                bitsAllocated = (short)8;
                bitsStored = (short)8;
                highBit = (short)7;
                pixelRepresentation = SIGNED_INTEGER;
                bytesPerPixel = 1;
                break;

            case ModelStorageBase.UBYTE:
            	bitsAllocated = (short)8;
                bitsStored = (short)8;
                highBit = (short)7;
                pixelRepresentation = UNSIGNED_INTEGER;
                bytesPerPixel = 1;
                break;

            case ModelStorageBase.SHORT:
            	bitsAllocated = (short)16;
                bitsStored = (short)16;
                highBit = (short)15;
                pixelRepresentation = SIGNED_INTEGER;
                bytesPerPixel = 2;
                break;

            case ModelStorageBase.USHORT:
            	bitsAllocated = (short)16;
                bitsStored = (short)16;
                highBit = (short)15;
                pixelRepresentation = UNSIGNED_INTEGER;
                bytesPerPixel = 2;
                break;

            case ModelStorageBase.UINTEGER:
            	bitsAllocated = (short)32;
                bitsStored = (short)32;
                highBit = (short)31;
                pixelRepresentation = UNSIGNED_INTEGER;
                bytesPerPixel = 4;
                break;

            case ModelStorageBase.ARGB:
            	bitsAllocated = (short)24;
                bitsStored = (short)24;
                highBit = (short)23;
                bytesPerPixel = 3;
                break;

            default:
            	MipavUtil.displayError("Illegal Dicom data type");
            	return;
    }

        	if (dimExtents.length == 2) {
        		((FileInfoDicom)fileInfo[0]).bytesPerPixel = bytesPerPixel;
        		((FileInfoDicom)fileInfo[0]).getTagTable().setValue("0028,0100", new Short(bitsAllocated), 2);
        		((FileInfoDicom)fileInfo[0]).getTagTable().setValue("0028,0101", new Short(bitsStored), 2);
        		((FileInfoDicom)fileInfo[0]).getTagTable().setValue("0028,0102", new Short(highBit), 2);
        		if (pixelRepresentation != -1) {
        		  ((FileInfoDicom)fileInfo[0]).getTagTable().setValue("0028,0103", new Short(pixelRepresentation), 2);
        		}
            } else if (dimExtents.length == 3) {

                for (int i = 0; i < dimExtents[2]; i++) {
                	((FileInfoDicom)fileInfo[i]).bytesPerPixel = bytesPerPixel;
                	((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0100", new Short(bitsAllocated), 2);
            		((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0101", new Short(bitsStored), 2);
            		((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0102", new Short(highBit), 2);
            		if (pixelRepresentation != -1) {
            		  ((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0103", new Short(pixelRepresentation), 2);
            		}
                }
            } else if (dimExtents.length == 4) {

                for (int i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
                	((FileInfoDicom)fileInfo[i]).bytesPerPixel = bytesPerPixel;
                	((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0100", new Short(bitsAllocated), 2);
            		((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0101", new Short(bitsStored), 2);
            		((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0102", new Short(highBit), 2);
            		if (pixelRepresentation != -1) {
            		  ((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0028,0103", new Short(pixelRepresentation), 2);
            		}	
                }
            }
        } // else if (fileInfo[0] instanceof FileInfoDicom)
    }

    /**
     * Method that register an VOI to this image.
     * 
     * @param voi Region of interest (VOI) to be registered with the image model
     */
    public void registerVOI(final VOI voi) {
        voiVector.addVOI(voi);
        // need to add voi to list object!!!
    }
    

    /**
     * Sets VOI vector for with new VOIs.
     * 
     * @param VOIs VOIs to image VOIs
     */
    public void restoreVOIs(VOIVector VOIs) {
        voiVector = VOIs;
    }

    public int isRegistered(final VOI voi) {
        return voiVector.indexOf(voi);
    }

    /**
     * Remove a listener from the class.
     * 
     * @param obj "object' to be added to the list
     */
    public void removeImageDisplayListener(final ViewImageUpdateInterface obj) {

        if (frameList != null) {
            frameList.removeElement(obj);
        }
    }

    /**
     * Resets VOI vector for new VOIs.
     */
    public void resetVOIs() {
        voiVector = new VOIVector();
    }

    /**
     * Save the image to a file. The file type the image is to be save in is passed into this method.
     * 
     * @param directory location where the image is to stored.
     * @param fileName the name of the file (without the extension).
     * @param fileType The format of the image file (i.e. Analyze, XML, DICOM etc.)
     * @param isActive Whether saving is being done in a separate thread
     * 
     * @return true if succeeded in saving.
     */
    public boolean saveImage(final String directory, final String fileName, final int fileType, final boolean isActive) {
        return saveImage(directory, fileName, fileType, isActive, true);
    }

    /**
     * Save the image to a file. The file type the image is to be save in is passed into this method.
     * 
     * @param directory location where the image is to stored.
     * @param fileName the name of the file (without the extension).
     * @param fileType The format of the image file (i.e. Analyze, XML, DICOM etc.)
     * @param isActive Whether saving is being done in a separate thread
     * @param bDisplayProgress when true display the progress bar for writing.
     * 
     * @return true if succeeded in saving.
     */
    public boolean saveImage(final String directory, final String fileName, final int fileType, final boolean isActive,
            final boolean bDisplayProgress) {
        final FileWriteOptions options = new FileWriteOptions(false);

        if (this.getNDims() == 3) {
            options.setBeginSlice(0);
            options.setEndSlice(this.getExtents()[2] - 1);
        } else if (this.getNDims() == 4) {
            options.setBeginSlice(0);
            options.setEndSlice(this.getExtents()[2] - 1);
            options.setBeginTime(0);
            options.setEndTime(this.getExtents()[3] - 1);
        }

        options.setRunningInSeparateThread(isActive);
        options.setFileType(fileType);

        if (fileName != null) {
            options.setFileName(fileName);
        } else if (getImageName() != null) {
            options.setFileName(getImageName());
        } else {
            Preferences.debug("ModelImage.saveImage - file name = null  " + "\n");

            return false;
        }

        if (directory != null) {
            options.setFileDirectory(directory);
        } else if (getImageName() != null) {
            options.setFileDirectory(getFileInfo(0).getFileDirectory());
        } else {
            Preferences.debug("ModelImage.saveImage - directory is null  " + "\n");

            return false;
        }

        options.setOptionsSet(true); // Options have been set - therefore don't bring up any dialogs

        // like the number of images.
        final FileIO fileIO = new FileIO();

        fileIO.writeImage(this, options, bDisplayProgress);

        return true;
    }

    // / Note to Matt should the matrix R/W be moved to the Matrix class ??
    /**
     * Saves the transformation matrix to file.
     * 
     * @param matrix DOCUMENT ME!
     */
    public void saveTransformMatrix(final TransMatrix matrix) {

        String fileName, directory;
        JFileChooser chooser;

        // bring up file dialog
        try {
            chooser = new JFileChooser();

            if (UI.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MATRIX));

            final int returnVal = chooser.showSaveDialog(UI.getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                UI.setDefaultDirectory(directory);
            } else {
                return;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ModelImage.saveTransformMatrix");

            return;
        }

        try {
            final File file = new File(UI.getDefaultDirectory() + fileName);
            final RandomAccessFile raFile = new RandomAccessFile(file, "rw");
            if (fileName.endsWith("xfm")) {
                // we need to convert matrix to left hand system when for saving as .xfm
                final TransMatrix mat = new TransMatrix(4);
                final TransMatrix rh_lhMatrix = new TransMatrix(4);

                // right handed to left handed or left handed to right handed coordinate systems
                rh_lhMatrix.Set(2, 2, -1);

                // p.223 Foley, Van Dam ...
                // Flipping only z axis
                // 1 0 0 0
                // 0 1 0 0
                // 0 0 -1 0
                // 0 0 0 1

                mat.Mult(rh_lhMatrix);
                mat.Mult(matrix);
                mat.Mult(rh_lhMatrix);

                mat.saveXFMMatrix(raFile);
            } else {
                matrix.saveMatrix(raFile);
            }
            raFile.close();
        } catch (final IOException error) {
            MipavUtil.displayError("Matrix save error");

            return;
        }
    }

    /**
     * Save the gradient magnitude image into the MIPAV default dir.
     * 
     * @param gmImage ModelImage gradient magnitude image to save
     */
    public static void saveImage(final ModelImage kImage, String fName, String dName ) {
        final FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);        
        final FileWriteOptions options = new FileWriteOptions(false);
        options.setFileDirectory(dName);
        options.setFileName(fName);
        options.setSaveAs(true);
        options.setBeginSlice(0);
        options.setOptionsSet(true);
        int end = kImage.getExtents().length > 2 ? kImage.getExtents()[2] - 1 : 0;
        options.setEndSlice(end);
        if ( kImage.getExtents().length > 3 )
        {
        	options.setBeginTime(0);
        	end = kImage.getExtents()[3] - 1;
        	options.setEndTime(end);
        }
        options.doPutInQuicklist(false);
        fileIO.writeImage(kImage, options);
    }
    
    
/*
    * Save the gradient magnitude image into the MIPAV default dir.
    * 
    * @param gmImage ModelImage gradient magnitude image to save
    */
   public static void saveImage(final ModelImage kImage) {
       final String fName = kImage.getImageName();
       final String dName = ViewUserInterface.getReference().getDefaultDirectory();
       final FileIO fileIO = new FileIO();

       fileIO.setQuiet(true);

       final FileWriteOptions options = new FileWriteOptions(false);

       options.setFileDirectory(dName);
       options.setFileName(fName);
       options.setBeginSlice(0);
       int end = kImage.getExtents().length > 2 ? kImage.getExtents()[2] - 1 : 0;
       options.setEndSlice(end);
       fileIO.writeImage(kImage, options);
   }
    

    /**
     * Save the images transformation matrix in the working directory with the supplied fileName.
     * 
     * @param fileName - fileName of transformation matrix
     * @param matrix DOCUMENT ME!
     */
    public void saveTransformMatrix(final String fileName, final TransMatrix matrix) {

        if (fileName == null) {
            return;
        }

        try {
            final File file = new File(UI.getDefaultDirectory() + fileName);
            final RandomAccessFile raFile = new RandomAccessFile(file, "rw");

            matrix.saveMatrix(raFile);
            raFile.close();
        } catch (final IOException error) {
            MipavUtil.displayError("Matrix save error");

            return;
        }
    }

    /**
     * Sets the image directory.
     * 
     * @param dir string representing the directory
     */
    public void setImageDirectory(final String dir) {

        if (fileInfo != null) {

            for (int i = 0; i < getFileInfo().length; i++) {
                getFileInfo(i).setFileDirectory(dir);
            }
        }
        }

    /**
     * Sets the image type (MRI, CT, ...).
     * 
     * @param type integer representing the type
     */
    public void setImageModality(final int type) {

        if (fileInfo != null) {
            fileInfo[0].setModality(type);
        }
    }

    /**
     * Accesor to set imageNameArray
     * 
     * @param imageNameArray
     */
    public void setImageNameArray(final String imageNameArray[]) {
        this.imageNameArray = imageNameArray.clone();
    }

    public String[] getImageNameArray() {
        return imageNameArray;
    }

    /**
     * Accessor that sets the name of the image. This method also updates the file name in the fileInfos to match the
     * new image name.
     * 
     * @param name the String representing the filename
     */
    public void setImageName(final String name) {
        setImageName(name, true);
    }

    /**
     * Accessor that sets the name of the image.
     * 
     * @param name the String representing the filename
     * @param updateFileName whether to update the file name stored in the image's fileInfos to match the new image name
     */
    public void setImageName(String name, final boolean updateFileName) {

        // update the fileInfo names
        final FileInfoBase[] fInfos = this.getFileInfo();
        final String tempName = fInfos[0].getFileName();
        String suffix = "";

        if ( (name != null) && (name.lastIndexOf(".") != -1)) {
            suffix = name.substring(name.lastIndexOf("."), name.length());
            name = name.substring(0, name.lastIndexOf("."));
        }

        if ( (suffix == null) && (tempName != null) && (tempName.lastIndexOf(".") != -1)) {
            suffix = tempName.substring(tempName.lastIndexOf("."), tempName.length());
        }

        if (suffix == null) {
            suffix = "";
        }

        // System.err.println("Full new name: " + name + suffix);
        if (updateFileName) {

            for (int i = 0; i < fInfos.length; i++) {

                if (fInfos[i] instanceof FileInfoDicom) {
                    fInfos[i].setFileName(name + (i + 1) + suffix);
                } else {
                    fInfos[i].setFileName(name + suffix);
                }
            }
        }

        // first check to see if the image name already equals name
        if ( (name != null) && name.equals(this.getImageName())) {

            // make sure the image is registered
            if (UI != null) {

                if ( !UI.isImageRegistered(name)) {
                    UI.registerImage(this);
                }
            }

            return;
        }

        // since all active images are registered by
        // their name, the key for the registered image
        // also needs to be changed.
        if (UI != null) {

            if (UI.isImageRegistered(imageName)) {

                // unregister this image
                UI.unRegisterImage(this);
            }
        }

        imageName = name;

        if (UI != null) {
            UI.registerImage(imageName, this);
        }
    }
    
    
    public DTIParameters getDTIParameters() {
        return dtiParameters;
    }
    
    /**
     * Sets the DTIParamters object
     * 
     * @param dti params object that stores dwi parameters (i.e. gradients and bvalues)
     */
    public void setDTIParameters(DTIParameters dtiParameters) {
        this.dtiParameters = dtiParameters;
    }

    /**
     * Accessor that sets the name of the image. NOT TO BE USED BY ANYONE EXCEPT ViewUserInterface.registerImage. Use
     * setImageName instead!!!
     * 
     * @param name the String representing the filename
     */
    public void setImageNamePrivate(final String name) {
        imageName = name;
    }

    /**
     * For multiple image viewers this indicates order of the image.
     * 
     * @param order integer indicating image order
     */
    public void setImageOrder(final int order) {
        imageOrder = order;
    }

    /**
     * Sets the image orientation (sagittal, axial, ...).
     * 
     * @param orient integer representing the orientation
     */
    public void setImageOrientation(final int orient) {

        if (fileInfo != null) {

            for (int i = 0; i < getFileInfo().length; i++) {
                getFileInfo(i).setImageOrientation(orient);
            }
        }
    }

    /**
     * Sets the mask which indicate which pixels/voxels to process.
     * 
     * @param _mask mask in the form of a BitSet, 1 indicates pixel should be processed 0 indicates pixel should not be
     *            processed
     */
    public void setMask(final BitSet _mask) {
        mask = _mask;
    }

    /**
     * Sets the mask which indicate which pixels/voxels to process.
     * 
     * @param mask mask in the form of a BitSet, 1 indicates pixel should be processed 0 indicates pixel should not be
     *            processed
     */
    public void setMaskBU(final BitSet mask) {
        maskBU = mask;
    }

    /**
     * Accessor that adds a matrix to the matrix holder.
     * 
     * @param matrix transformation matrix structure.
     */
    public void setMatrix(final TransMatrix matrix) {
        matrixHolder.addMatrix(matrix);
    }

    /**
     * Sets the slice in all frames displaying this image.
     * 
     * @param slice Indicates the z dim. slice that should be displayed.
     */
    public void setSlice(final int slice) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {
            frameList.elementAt(i).setSlice(slice);
        }
    }

    /**
     * Accessor that sets the talairach transform information.
     * 
     * @param tal TalairachTransformInfo talairach info
     */
    public void setTalairachTransformInfo(final TalairachTransformInfo tal) {
        this.talairach = tal;
    }

    /**
     * Sets the time slice in all frames displaying this image.
     * 
     * @param tSlice Indicates the t (time) dim. slice that should be displayed.
     */
    public void setTimeSlice(final int tSlice) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {
            frameList.elementAt(i).setTimeSlice(tSlice);
        }
    }

    /**
     * Sets user interface.
     * 
     * @param _UI reference to user interface
     */
    public void setUserInterface(final ViewUserInterface _UI) {
        UI = _UI;
    }

    /**
     * Sets VOI vector for with new VOIs.
     * 
     * @param VOIs VOIs to image VOIs
     */
    public void setVOIs(final VOIVector VOIs) {
        voiVector = new VOIVector(VOIs);
    }

    /**
     * Gives a readable representation of the ModelImage, including file name and extents.
     * 
     * @return the string representation
     */
    public String toString() {
        String s = "";

        s += "\n File name = \t" + getImageFileName() + "\n Extents:\t";

        for (int i = 0; i < getNDims(); i++) {
            s += "\t [" + i + "] = " + getExtents()[i];
        }

        s += "\n Resolutions:\t";

        for (int i = 0; i < getNDims(); i++) {
            s += "\t [" + i + "] = " + getFileInfo()[0].getResolutions()[i];
        }

        s += "\n Type: \t\t" + ModelStorageBase.getBufferTypeStr(getType());

        if (isColorImage()) {
            s += "\n Image minR:\t" + getMinR();
            s += "\n Image maxR:\t" + getMaxR();
            s += "\n Image minG:\t" + getMinG();
            s += "\n Image maxG:\t" + getMaxG();
            s += "\n Image minB:\t" + getMinB();
            s += "\n Image maxB:\t" + getMaxB();
        } else {
            s += "\n Image min:\t" + getMin();
            s += "\n Image max:\t" + getMax();
        }

        s += "\n For  first extent  -------------";
        s += "\n\t Modality:   \t" + FileInfoBase.getModalityStr(getFileInfo()[0].getModality());
        s += "\n\t File Dir:   \t" + getFileInfo()[0].getFileDirectory();
        s += "\n\t Endianess:  \t" + getFileInfo()[0].getEndianess();

        for (int i = 0; i < getNDims(); i++) {
            s += "\n\t Units:      \t"
                    + (Unit.getUnitFromLegacyNum(getFileInfo()[0].getUnitsOfMeasure()[0])).getAbbrev(); // possibly
                                                                                                        // expand
            // to see all
            // measurements
        }

        s += "\n\t Orientation:\t" + FileInfoBase.getImageOrientationStr(getImageOrientation());

        for (int i = 0; i < getNDims(); i++) {
            s += "\n\t Axis: " + i + "      \t" + FileInfoBase.getAxisOrientationStr(fileInfo[0].getAxisOrientation(i));
        }

        // s += "\n\t Orientation:\t" + FileInfoBase.getImageOrientationStr(getImageOrientation());
        for (int i = 0; i < getNDims(); i++) {
            s += "\n\t Origin: " + i + " \t" + getFileInfo()[0].getOrigin(i); // possibly expand to see all
                                                                                // measurements
        }

        s += "\n\t Pixel Pad:  \t" + getFileInfo()[0].getPixelPadValue();
        s += "\n\t Photometric:\t" + getFileInfo()[0].getPhotometric();
        s += "\n\t Ref:  \t" + super.toString();

        return s;
    }

    public void trimVOIs()
    {
        for ( int i = 0; i < voiVector.size(); i++ )
        {
            voiVector.get(i).trim();
        }
        notifyImageDisplayListeners();
    }

    public void updateVOIs()
    {
        for ( int i = 0; i < voiVector.size(); i++ )
        {
            voiVector.get(i).update();
        }
    }
    
    /**
     * Forms separate VOIs from all the VOI structures presently loaded in the imageModel.
     */
    public void ungroupVOIs() {

        int numContours = 0;
        int numPoints = 0;
        int numPLines = 0;
        int numLines = 0;
        int numProtractors = 0;
        int n = 0;
        int nPt = 0;
        int nPLine = 0;
        int nLine = 0;
        int nProtractor = 0;
        int nVOIs;
        int i, k;
        VOI[] newVOI = null;
        VOI[] newPtVOI = null;
        VOI[] newPLineVOI = null;
        VOI[] newLineVOI = null;
        VOI[] newProtractorVOI = null;

        Vector<VOIBase> contours;
        int nContours;
        short id = 0;

        // make a copy of the VOIs stored until the final step
        VOIVector tempVOIs = new VOIVector(voiVector);

        nVOIs = tempVOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if (tempVOIs.VOIAt(i).isActive() == true) {

                if (tempVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    numContours += tempVOIs.VOIAt(i).getCurves().size();
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                    numPoints += tempVOIs.VOIAt(i).getCurves().size();
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) {
                    numPLines += tempVOIs.VOIAt(i).getCurves().size();
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                    numLines += tempVOIs.VOIAt(i).getCurves().size();
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {
                    numProtractors += tempVOIs.VOIAt(i).getCurves().size();
                }
            } // if (tempVOIs.VOIAt(i).isActive() == true)
        } // for (i = 0; i < nVOIs; i++)

        if ( (numContours == 0) && (numPoints == 0) && (numPLines == 0) && (numLines == 0) && (numProtractors == 0)) {
            MipavUtil.displayWarning("Must select a VOI to ungroup");
            tempVOIs.removeAllElements();

            return;
        }

        if (numContours > 0) {
            newVOI = new VOI[numContours];
        }

        if (numPoints > 0) {
            newPtVOI = new VOI[numPoints];
        }

        if (numPLines > 0) {
            newPLineVOI = new VOI[numPLines];
        }

        if (numLines > 0) {
            newLineVOI = new VOI[numLines];
        }

        if (numProtractors > 0) {
            newProtractorVOI = new VOI[numProtractors];
        }

        String nameExt = null;

        if (getNDims() > 2) {
            nameExt = new String("3D");
        } else {
            nameExt = new String("2D");
        }

        for (i = 0; i < numContours; i++, id++) {
            newVOI[i] = new VOI(id, "contour" + nameExt + i, VOI.CONTOUR, -1.0f);
        }

        for (i = 0; i < numPoints; i++, id++) {
            newPtVOI[i] = new VOI(id, "point" + nameExt + i, VOI.POINT, -1.0f);
        }

        for (i = 0; i < numPLines; i++, id++) {
            newPLineVOI[i] = new VOI(id, "polyline" + nameExt + i, VOI.POLYLINE, -1.0f);
        }

        for (i = 0; i < numLines; i++, id++) {
            newLineVOI[i] = new VOI(id, "line" + nameExt + i, VOI.LINE, -1.0f);
        }

        for (i = 0; i < numProtractors; i++, id++) {
            newProtractorVOI[i] = new VOI(id, "protractor" + nameExt + i, VOI.PROTRACTOR, -1.0f);
        }

        VOIBase tempBase = null;

        for (i = nVOIs - 1; i >= 0; i--) {

            if (tempVOIs.VOIAt(i).isActive() == true) {

                if (tempVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contours = tempVOIs.VOIAt(i).getCurves();
                    nContours = contours.size();
                    for (k = 0; k < nContours; k++) {
                        tempBase = contours.elementAt(k);
                        newVOI[n].getCurves().addElement(tempBase);
                        n++;
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) {
                    contours = tempVOIs.VOIAt(i).getCurves();
                    nContours = contours.size();
                    for (k = 0; k < nContours; k++) {
                        tempBase = contours.elementAt(k);
                        newPLineVOI[nPLine].getCurves().addElement(tempBase);
                        nPLine++;
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                    contours = tempVOIs.VOIAt(i).getCurves();
                    nContours = contours.size();
                    for (k = 0; k < nContours; k++) {
                        tempBase = contours.elementAt(k);
                        newLineVOI[nLine].getCurves().addElement(tempBase);
                        nLine++;
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                    contours = tempVOIs.VOIAt(i).getCurves();
                    nContours = contours.size();
                    for (k = 0; k < nContours; k++) {
                        tempBase = contours.elementAt(k);
                        newLineVOI[nLine].getCurves().addElement(tempBase);
                        nPt++;
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {
                    contours = tempVOIs.VOIAt(i).getCurves();
                    nContours = contours.size();

                    for (k = 0; k < nContours; k++) {
                        tempBase = contours.elementAt(k);
                        newProtractorVOI[nProtractor].getCurves().addElement(contours.elementAt(k));
                        nProtractor++;
                    }

                    tempVOIs.removeElementAt(i);
                }
            } // if (tempVOIs.VOIAt(i).isActive() == true)
        } // for (i = 0; i < nVOIs; i++)

        for (i = 0; i < numContours; i++) {
            tempVOIs.addElement(newVOI[i]);
        }

        for (i = 0; i < numPoints; i++) {
            tempVOIs.addElement(newPtVOI[i]);
        }

        for (i = 0; i < numPLines; i++) {
            tempVOIs.addElement(newPLineVOI[i]);
        }

        for (i = 0; i < numLines; i++) {
            tempVOIs.addElement(newLineVOI[i]);
        }

        for (i = 0; i < numProtractors; i++) {
            tempVOIs.addElement(newProtractorVOI[i]);
        }

        // empty the current VOIVector
        voiVector.removeAllElements();
        setVOIs(tempVOIs);
        tempVOIs = null;

        for (i = 0; i < voiVector.size(); i++) {
            (voiVector.elementAt(i)).setID((short) i);
        }

        /**
         * System.err.println("\n\nUngrouping DEBUG INFO:"); VOI tempVOI = null; Vector [] tempCurves = null; //temp
         * stuff for debugging purposes for (i = 0; i < voiVector.size(); i++) { tempVOI = voiVector.VOIAt(i);
         * System.err.println( i + ": VOI name: " + tempVOI.getName()); tempCurves = tempVOI.getCurves(); for (j = 0; j <
         * tempCurves.length; j++) { System.err.println("\tSize: " + tempCurves[j].size()); System.err.println("\t" +
         * tempCurves[j].toString()); } }
         */

        notifyImageDisplayListeners();
    }

    /**
     * Unregisters all VOIs from this image model.
     */
    public void unregisterAllVOIs() {
        if ( voiVector != null )
        {
            voiVector.removeAllElements();
        }
    }

    /**
     * Unregisters the image from the user interface.
     */
    public void unRegisterImage() {

        if (UI != null) {
            UI.unRegisterImage(this);
        }
    }

    /**
     * Method that unregisters an VOI.
     * 
     * @param voi Volume of interest (VOI) to be removed from the image model
     */
    public void unregisterVOI(final VOI voi) {
        voiVector.removeElement(voi);
    }
    
    public static boolean updateFileInfo( ModelImage destImage, ModelImage srcImage, final int[] axisOrder, 
    		final boolean[] axisFlip, ViewJProgressBar progressBar, int startValue, int finalValue )
    {

        int orientation = destImage.getImageOrientation();
        int[] newDimExtents = null;
        FileInfoDicom[] newDicomInfo = null;
        int[] newAxisOrients = null;

        // Set the file info for the new rotated image identical to the original image,
        // and then adjusts the appropriate info.
        // For all image formats other than DICOM
        if (srcImage.getFileInfo(0).getFileFormat() != FileUtility.DICOM) {
            if (srcImage.getNDims() >= 3) {
                destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
            }
            newDimExtents = new int[destImage.getNDims()];
            newAxisOrients = new int[destImage.getNDims()];
            for (int i = 0; i < destImage.getNDims(); i++) {
            	newDimExtents[i] = destImage.getExtents()[i];
            	if (i < 3) {
                    newAxisOrients[i] = destImage.getAxisOrientation()[i];
                }
            }
        } else { // If file is DICOM...

            
            
            float[] newResolutions;
            float[] newStartLocations;
            int[] newUnitsOfMeasure;

            try {
                newDimExtents = new int[destImage.getNDims()];
                newAxisOrients = new int[destImage.getNDims()];
                newResolutions = new float[destImage.getNDims()];
                newUnitsOfMeasure = new int[destImage.getNDims()];
                newStartLocations = new float[destImage.getNDims()];
            } catch (OutOfMemoryError e) {
                return false;
            }

            for (int i = 0; i < destImage.getNDims(); i++) {
                newDimExtents[i] = destImage.getExtents()[i];
                newResolutions[i] = destImage.getResolutions(0)[i];
                newUnitsOfMeasure[i] = destImage.getUnitsOfMeasure()[i];
                newStartLocations[i] = destImage.getOrigin()[i];

                if (i < 3) {
                    newAxisOrients[i] = destImage.getAxisOrientation()[i];
                }
            }
            newDicomInfo = new FileInfoDicom[newDimExtents[2]];
            StringBuffer newTagPixelSpc = null;
            StringBuffer newTagSliceSpc = null;
            String imageOrient = null;
            TransMatrix matrix = null;
            float fSliceLocation = 0.f;

            FileInfoDicom oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(0);
            //String[] tmp = oldDicomInfo.parseTagValue("0028,0030");

            // pixel spacing, slice thickness, and spacing between slices changes for
            // X- or Y-axis rotation, but not for Z-axis rotation.  Also should not be set if
            // it wasn't there in the first place.
            //if ((rotateAxis != Z_AXIS_180) && (rotateAxis != Z_AXIS_PLUS) && (rotateAxis != Z_AXIS_PLUS) &&
            //        (tmp != null)) {
                newTagPixelSpc = new StringBuffer(Float.toString(newResolutions[1]) + "\\" +
                                                  Float.toString(newResolutions[0]));
                newTagSliceSpc = new StringBuffer(Float.toString(newResolutions[2]));
            //}

            matrix = oldDicomInfo.getPatientOrientation();
            if (matrix != null) {
                imageOrient = matrixToDICOMString(matrix);
            }

            FileDicomTagTable[] childTagTables = new FileDicomTagTable[newDimExtents[2] - 1];

            // first create all of the new file infos (reference and children) and fill them with tags from the old
            // file info.  some of these tag values will be overridden in the next loop
            for (int i = 0; i < newDimExtents[2]; i++) {
            	if (progressBar != null) {
            	    progressBar.updateValueImmed(( startValue + 
            	    		((finalValue - startValue) * i) / (2 * newDimExtents[2])));
            	}

                if (i == 0) {

                    // create a new reference file info
                    newDicomInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                        oldDicomInfo.getFileFormat());
                    newDicomInfo[0].setVr_type(oldDicomInfo.getVr_type());
                    newDicomInfo[0].setDataType(oldDicomInfo.getDataType());
                } else {

                    // all other slices are children of the first file info..
                    newDicomInfo[i] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                        oldDicomInfo.getFileFormat(), newDicomInfo[0]);
                    
                    newDicomInfo[i].setVr_type(oldDicomInfo.getVr_type());
                    newDicomInfo[i].setDataType(oldDicomInfo.getDataType());

                    childTagTables[i - 1] = newDicomInfo[i].getTagTable();
                }

                if (axisOrder[2] == 2) { // z-axis doesn't change

                    // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                    newDicomInfo[i].getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(i));
                } else {

                    // not possible for other rotations because the z-dimension is different
                    newDicomInfo[i].getTagTable().importTags(oldDicomInfo);
                }
            }

            newDicomInfo[0].getTagTable().attachChildTagTables(childTagTables);

            
            for (int i = 0; i < newDimExtents[2]; i++) {
            	if (progressBar != null) {
            	    progressBar.updateValueImmed(( startValue + (finalValue - startValue)/2 +
            	    		((finalValue - startValue) * i) / (2 * newDimExtents[2])));
            	}

                newDicomInfo[i] = (FileInfoDicom) srcImage.getFileInfo(0).clone();
                newDicomInfo[i].setExtents(newDimExtents);
                newDicomInfo[i].setImageOrientation(orientation);
                newDicomInfo[i].setAxisOrientation(newAxisOrients);
                newDicomInfo[i].setResolutions(newResolutions);
                for (int j = 0; j < 3; j++) {
                	// update changes with slice position later
                	newDicomInfo[i].setOrigin(newStartLocations[j], j);
            	}
                newDicomInfo[i].getTagTable().setValue("0028,0011", new Short((short) newDimExtents[0]), 2); // columns
                newDicomInfo[i].getTagTable().setValue("0028,0010", new Short((short) newDimExtents[1]), 2); // rows
                newDicomInfo[i].getTagTable().setValue("0020,0013", Short.toString((short) (i + 1)),
                                                       Short.toString((short) (i + 1)).length()); // instance number

                // if wasn't previously set, don't set it; if Z-axis rotation, don't set it
                if (newTagPixelSpc != null) {
                    newDicomInfo[i].getTagTable().setValue("0028,0030", newTagPixelSpc.toString(),
                                                           newTagPixelSpc.length()); // pixel spacing
                }

                if (newTagSliceSpc != null) {
                    newDicomInfo[i].getTagTable().setValue("0018,0050", newTagSliceSpc.toString(),
                                                           newTagSliceSpc.length()); // slice thickness
                    newDicomInfo[i].getTagTable().setValue("0018,0088", newTagSliceSpc.toString(),
                                                           newTagSliceSpc.length()); // spacing between slices
                }

                if ((imageOrient != null) && (newDicomInfo[i].getTagTable().getValue("0020,0037") != null)) {
                    newDicomInfo[i].getTagTable().setValue("0020,0037", imageOrient, imageOrient.length());
                }

                String position = null;
                DecimalFormat nf = new DecimalFormat("##0.000000");
                String sliceLocation = null;

                if (orientation == FileInfoBase.SAGITTAL) {
                    fSliceLocation = newStartLocations[0] + (i * newResolutions[0]);
                    position = positionToDICOMString(fSliceLocation, newStartLocations[1], newStartLocations[2]);
                    sliceLocation = nf.format(fSliceLocation);
                } else if (orientation == FileInfoBase.CORONAL) {
                    fSliceLocation = newStartLocations[1] + (i * newResolutions[1]);
                    position = positionToDICOMString(newStartLocations[0], fSliceLocation, newStartLocations[2]);
                    sliceLocation = nf.format(fSliceLocation);
                } else if (orientation == FileInfoBase.AXIAL) {
                    fSliceLocation = newStartLocations[2] + (i * newResolutions[2]);
                    position = positionToDICOMString(newStartLocations[0], newStartLocations[1], fSliceLocation);
                    sliceLocation = nf.format(fSliceLocation);
                }

                if (newDicomInfo[i].getTagTable().getValue("0020,1041") != null) {
                    newDicomInfo[i].getTagTable().setValue("0020,1041", sliceLocation, sliceLocation.length()); // image location
                }

                if (newDicomInfo[i].getTagTable().getValue("0020,0032") != null) {
                    newDicomInfo[i].getTagTable().setValue("0020,0032", position, position.length());
                }
            }
            
            destImage.setFileInfo(newDicomInfo);
            if (srcImage.getNDims() >= 3) {
                destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
            }
        }
        if (destImage.getNDims() >= 3) {
        	// Update any destImage NIFTI matrices
            MatrixHolder matHolder = null;
            int i;
            int j;
            matHolder = destImage.getMatrixHolder();
            float loc;
            int orient;

            if (matHolder != null) {
                LinkedHashMap<String, TransMatrix> matrixMap = matHolder.getMatrixMap();
                Iterator<String> iter = matrixMap.keySet().iterator();
                String nextKey = null;
                
                TransMatrix tempMatrix = null;
                
                while (iter.hasNext()) {
                    nextKey = iter.next();
                    tempMatrix = matrixMap.get(nextKey);
                    if (tempMatrix.isNIFTI()) {
                    	TransMatrix newMatrix = new TransMatrix(4);
                    	for (i = 0; i < 3; i++) {
                            for (j = 0; j < 3; j++) {
                            	if (axisFlip[i]) {
                            		newMatrix.set(j, i, -tempMatrix.get(j, axisOrder[i]));
                            	}
                            	else {
                                    newMatrix.set(j, i, tempMatrix.get(j, axisOrder[i]));
                            	}
                            }
                            loc = tempMatrix.get(axisOrder[i], 3);
                            if (axisFlip[i]) {
                            	orient = srcImage.getFileInfo(0).getAxisOrientation(axisOrder[i]);
                            	if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
                                        (orient == FileInfoBase.ORI_A2P_TYPE) || 
                                        (orient == FileInfoBase.ORI_I2S_TYPE)) {
                                	loc = loc + ((srcImage.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * srcImage.getFileInfo(0).getResolutions()[axisOrder[i]]);
                                }
                                else {
                                	loc = loc - ((srcImage.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * srcImage.getFileInfo(0).getResolutions()[axisOrder[i]]);	
                                }
                            }
                            newMatrix.set(i, 3, loc);
                    	} // for (i = 0; i < 3; i++)
                    	tempMatrix.Copy(newMatrix);
                    	if (destImage.getFileInfo(0) instanceof FileInfoNIFTI) {
	                        if (tempMatrix.isQform()) {
	                            if (destImage.getNDims() == 3) {
	                                for (i = 0; i < destImage.getExtents()[2]; i++) {
	                                    ((FileInfoNIFTI)destImage.getFileInfo(i)).setMatrixQ(newMatrix);
	                                }
	                            }
	                            else if (destImage.getNDims() == 4) {
	                                for (i = 0; i < destImage.getExtents()[2]*destImage.getExtents()[3]; i++) {
	                                    ((FileInfoNIFTI)destImage.getFileInfo(i)).setMatrixQ(newMatrix);    
	                                }
	                            }
	                        } // if (tempMatrix.isQform())
	                        else { // tempMatrix is sform
	                            if (destImage.getNDims() == 3) {
	                                for (i = 0; i < destImage.getExtents()[2]; i++) {
	                                    ((FileInfoNIFTI)destImage.getFileInfo(i)).setMatrixS(newMatrix);
	                                }
	                            }
	                            else if (destImage.getNDims() == 4) {
	                                for (i = 0; i < destImage.getExtents()[2]*destImage.getExtents()[3]; i++) {
	                                    ((FileInfoNIFTI)destImage.getFileInfo(i)).setMatrixS(newMatrix);    
	                                }
	                            }    
	                        } // else tempMatrix is sform
                    	} // if (destImage.getFileInfo(0) instanceof FileInfoNIFTI)
                    }
                }
            } // if (matHolder != null) 
            
            if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                    || (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
            	TransMatrix dicomMatrix = null;
            	dicomMatrix = srcImage.getMatrix();
            	TransMatrix newMatrix = new TransMatrix(4);
            	for (i = 0; i < 3; i++) {
                    for (j = 0; j < 3; j++) {
                    	if (axisFlip[i]) {
                    		newMatrix.set(j, i, -dicomMatrix.get(j, axisOrder[i]));
                    	}
                    	else {
                            newMatrix.set(j, i, dicomMatrix.get(j, axisOrder[i]));
                    	}
                    }
            	} // for (i = 0; i < 3; i++)
            	newMatrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);
            	destImage.getMatrixHolder().clearMatrices();
            	destImage.getMatrixHolder().addMatrix(newMatrix);
            	
            	for (i = 0; i < newDimExtents[2]; i++) {
    	            Vector3f pos = new Vector3f(0, 0, i);
    	        	Vector3f out = new Vector3f(pos);
    	            MipavCoordinateSystems.fileToScanner(pos, out, destImage);
    	            float origin[] = new float[3];
    	            origin[0] = out.X;
    	            origin[1] = out.Y;
    	            origin[2] = out.Z;
    	            for (j = 0; j < 3; j++) {
                		if ((newAxisOrients[j] == FileInfoBase.ORI_R2L_TYPE) ||
                			(newAxisOrients[j] == FileInfoBase.ORI_L2R_TYPE)) {
                			destImage.getFileInfo()[i].setOrigin(origin[0],j);
                		}
                		else if ((newAxisOrients[j] == FileInfoBase.ORI_A2P_TYPE) ||
                		        (newAxisOrients[j] == FileInfoBase.ORI_P2A_TYPE)) {
                			destImage.getFileInfo()[i].setOrigin(origin[1],j);
                		}
                		else { 
                			destImage.getFileInfo()[i].setOrigin(origin[2],j);
                	    }
                	}
                }
            } // if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
            
            
        } // if (destImage.getNDims() >= 3)
        return true;
    }
    

    /**
     * Convert the matrix to the String format (decimal string) to be stored in the DICOM tag (0020,0037) image
     * (patient) orientation.
     *
     * @param   matrix  Transformation matrix to be converted.
     *
     * @return  The string version of the transformation matrix (i.e. the directional cosines for the first two rows of
     *          the matrix delimited by "\")
     */
    private static String matrixToDICOMString(TransMatrix matrix) {
        String strMatrix = new String();

        DecimalFormat nf = new DecimalFormat("##0.0000000");

        strMatrix = nf.format(matrix.Get(0, 0)) + "\\" + nf.format(matrix.Get(0, 1)) + "\\" + nf.format(matrix.Get(0, 2)) +
                    "\\" + nf.format(matrix.Get(1, 0)) + "\\" + nf.format(matrix.Get(1, 1)) + "\\" + nf.format(matrix.Get(1, 2));

        return strMatrix;
    }

    /**
     * Convert the image position to the String format (decimal string) to be stored in the DICOM tag (0020,0032) image
     * (patient) orientation.
     *
     * @param   pt0  X position of patient.
     * @param   pt1  Y position of patient.
     * @param   pt2  Z position of patient.
     *
     * @return  The string version of the patient position delimited by "\".
     */
    private static String positionToDICOMString(double pt0, double pt1, double pt2) {
        String strPosition = new String();

        DecimalFormat nf = new DecimalFormat("##0.0#####");

        strPosition = nf.format(pt0) + "\\" + nf.format(pt1) + "\\" + nf.format(pt2);

        return strPosition;
    }
    
    
    
    /**
     * Give the image a new image name, updates frame (if not null), and file infos.
     * 
     * @param newImageName The new name for the image
     */
    public void updateFileName(String newImageName) {
        final String oldImageName = getImageName();
        setImageName(newImageName);

        ScriptRecorder.getReference().addLine(new ActionChangeName(this, oldImageName, newImageName));
        ProvenanceRecorder.getReference().addLine(new ActionChangeName(this, oldImageName, newImageName));

        try {
            UI.getFrameContainingImage(this).setTitle();
        } catch (final Exception e) { // was not in frame..
        }

        int index;
        String tmpString = "";

        if (getNDims() == 2) {

            if (fileInfo[0].getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (fileInfo[0])).getTagTable().setValue("0010,0010", newImageName.trim(),
                        newImageName.trim().length());
            } else {
                index = fileInfo[0].getFileName().trim().lastIndexOf(".");

                if (index > 0) {
                    tmpString = fileInfo[0].getFileName().trim().substring(index);
                }

                newImageName = newImageName + tmpString;
                fileInfo[0].setFileName(newImageName);
            }
        } else if (getNDims() == 3) {

            if (fileInfo[0].getFileFormat() == FileUtility.DICOM) {

                for (int i = 0; i < getExtents()[2]; i++) {
                    ((FileInfoDicom) (fileInfo[i])).getTagTable().setValue("0010,0010", newImageName.trim(),
                            newImageName.trim().length());
                }
            } else {

                if (fileInfo[0].getFileName() != null) {
                    index = fileInfo[0].getFileName().trim().lastIndexOf(".");

                    if (index > 0) {
                        tmpString = fileInfo[0].getFileName().trim().substring(index);
                    }

                    newImageName = newImageName + tmpString;
                }

                for (int i = 0; i < getExtents()[2]; i++) {
                    fileInfo[i].setFileName(newImageName);
                }
            }
        } else if (getNDims() == 4) {

            if (fileInfo[0].getFileFormat() == FileUtility.DICOM) {

                for (int i = 0; i < (getExtents()[2] * getExtents()[3]); i++) {
                    ((FileInfoDicom) (fileInfo[i])).getTagTable().setValue("0010,0010", newImageName.trim(),
                            newImageName.trim().length());
                }
            } else {

                if (fileInfo[0].getFileName() != null) {
                    index = fileInfo[0].getFileName().trim().lastIndexOf(".");

                    if (index > 0) {
                        tmpString = fileInfo[0].getFileName().trim().substring(index);
                    }

                    newImageName = newImageName + tmpString;
                }

                for (int i = 0; i < (getExtents()[2] * getExtents()[3]); i++) {
                    fileInfo[i].setFileName(newImageName);
                }
            }
        } else if (getNDims() == 5) {

            if (fileInfo[0].getFileFormat() == FileUtility.DICOM) {

                for (int i = 0; i < (getExtents()[2] * getExtents()[3] * getExtents()[4]); i++) {
                    ((FileInfoDicom) (fileInfo[i])).getTagTable().setValue("0010,0010", newImageName.trim(),
                            newImageName.trim().length());
                }
            } else {

                if (fileInfo[0].getFileName() != null) {
                    index = fileInfo[0].getFileName().trim().lastIndexOf(".");

                    if (index > 0) {
                        tmpString = fileInfo[0].getFileName().trim().substring(index);
                    }

                    newImageName = newImageName + tmpString;
                }

                for (int i = 0; i < (getExtents()[2] * getExtents()[3] * getExtents()[4]); i++) {
                    fileInfo[i].setFileName(newImageName);
                }
            }
        }
    }

    /**
     * Updates the images origin.
     * 
     * @param xfrm the transformation maxtrix used to transform the origin
     */
    public void updateImageOrigin(final TransMatrix xfrm) {

        final FileInfoBase[] fileInfo = getFileInfo();
        final float[] imgOrigin = fileInfo[0].getOrigin();

        if (getNDims() == 2) {
            final float[] tempOrigin = new float[2];

            xfrm.transform(imgOrigin[0], imgOrigin[1], tempOrigin);
            imgOrigin[0] = tempOrigin[0];
            imgOrigin[1] = tempOrigin[1];
        } else {
            final float[] tempOrigin = new float[3];

            xfrm.transform(imgOrigin[0], imgOrigin[1], imgOrigin[2], tempOrigin);
            imgOrigin[0] = tempOrigin[0];
            imgOrigin[1] = tempOrigin[1];
            imgOrigin[2] = tempOrigin[2];
        }

        int direction = 1;
        final float startPos = imgOrigin[2];
        int[] axisOrient = null;

        axisOrient = fileInfo[0].getAxisOrientation();

        if (getNDims() >= 3) {

            if ( (axisOrient[2] == FileInfoBase.ORI_L2R_TYPE) || (axisOrient[2] == FileInfoBase.ORI_A2P_TYPE)
                    || (axisOrient[2] == FileInfoBase.ORI_S2I_TYPE)) {
                direction = -1;
            } else {
                direction = 1;
            }
        }

        if (getNDims() == 2) {
            fileInfo[0].setOrigin(imgOrigin);
        } else if (getNDims() == 3) {

            for (int i = 0; i < getExtents()[2]; i++) {
                imgOrigin[2] = startPos + (direction * i * fileInfo[0].getResolutions()[2]);
                fileInfo[i].setOrigin(imgOrigin);

            }
        } else if (getNDims() == 4) {

            for (int j = 0; j < getExtents()[3]; j++) {

                for (int i = 0; i < getExtents()[2]; i++) {
                    imgOrigin[2] = startPos + (direction * i * fileInfo[0].getResolutions()[2]);
                    fileInfo[ (j * getExtents()[2]) + i].setOrigin(imgOrigin);
                }
            }
        }
    }

    /**
     * Make all spatial units the same, all time units the same, and all frequency units the same
     * 
     */
    public void makeUnitsOfMeasureIdentical() {
        int i;
        final int nDims = getNDims();
        UnitType unitType[];
        boolean set1To0 = false;
        boolean set2To0 = false;
        boolean set2To1 = false;

        final int[] units = fileInfo[0].getUnitsOfMeasure();
        if ( (nDims == 2) && (units[0] == units[1])) {
            return;
        } else if ( (nDims == 3) && (units[0] == units[1]) && (units[1] == units[2])) {
            return;
        }
        unitType = new UnitType[nDims];
        for (i = 0; i < nDims; i++) {
            unitType[i] = Unit.getUnitFromLegacyNum(units[i]).getType();
        } // for (i = 0; i < nDims; i++)

        if (nDims == 2) {
            if ( (unitType[0] == UnitType.NONE) || (unitType[1] == UnitType.NONE) || (unitType[0] != unitType[1])) {
                return;
            } else {
                set1To0 = true;
            }
        } // if (nDims == 2)
        else if ( (nDims == 3) || (nDims == 4)) {
            if ( (unitType[0] == UnitType.NONE) || (unitType[1] == UnitType.NONE) || (unitType[0] != unitType[1])) {
                ;
            } else {
                set1To0 = true;
            }

            if ( (unitType[0] == UnitType.NONE) || (unitType[2] == UnitType.NONE) || (unitType[0] != unitType[2])) {
                ;
            } else {
                set2To0 = true;
            }

            if ( ( !set1To0) && ( !set2To0)) {
                if ( (unitType[1] == UnitType.NONE) || (unitType[1] == UnitType.NONE) || (unitType[1] != unitType[2])) {
                    ;
                } else {
                    set2To1 = true;
                }
            } // if ((!set1To0) && (!set2To0))

            if ( ( !set1To0) && ( !set2To0) && ( !set2To1)) {
                return;
            }
        } // else if ((nDims == 3) || (nDims == 4))
        if (set1To0 && set2To0) {
            make2UnitsOfMeasureIdentical(0, 1);
            make2UnitsOfMeasureIdentical(0, 2);
        } else if (set1To0) {
            make2UnitsOfMeasureIdentical(0, 1);
        } else if (set2To0) {
            make2UnitsOfMeasureIdentical(0, 2);
        } else if (set2To1) {
            make2UnitsOfMeasureIdentical(1, 2);
        }
        return;
    } // public void makeUnitsOfMeasureIdentical()

    /**
     * 
     * @param newUnitDim
     * @param oldUnitDim dimension which will have unit of measure converted to unit of measure of dimension newUnitDim
     *            and will have the resolution correspondingly converted.
     */
    public void make2UnitsOfMeasureIdentical(final int newUnitDim, final int oldUnitDim) {
        int i;
        final int newUnit = fileInfo[0].getUnitsOfMeasure()[newUnitDim];
        final int oldUnit = fileInfo[0].getUnitsOfMeasure()[oldUnitDim];
        float res = fileInfo[0].getResolutions()[oldUnitDim];
        final double conv = ModelImage.getConversionFactor(newUnit, oldUnit);
        res *= conv;

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setUnitsOfMeasure(newUnit, oldUnitDim);
            fileInfo[i].setResolutions(res, oldUnitDim);
            this.setFileInfo(fileInfo[i], i);
        }
    } // public void make2UnitsOfMeasureIdentical(int newUnit, int originalUnit)

    

    public static double getConversionFactor(int newUnit, int oldUnit) {
        return Unit.getUnitFromLegacyNum(oldUnit).getConversionFactor(Unit.getUnitFromLegacyNum(newUnit));
    }

    /**
     * Calls disposeLocal of this class to ensure this class nulls the references to global class variables so that
     * memory will be recovered.
     * 
     * @throws Throwable Throws an error if there is a problem with the finalization of this object.
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    /**
     * Helper method for making the result image's name. Strips the current extension from the original name, adds the
     * given extension, and returns the new name.
     * 
     * @param imageName Source image name that will be modified to have a new extension.
     * @param ext Extension to add which gives information about what algorithm was performed on the image.
     * 
     * @return The new image name.
     */
    public static String makeImageName(final String imageName, final String ext) {
        String name;
        int index;

        if (imageName != null) {
            index = imageName.lastIndexOf(".");
        } else {
            return null;
        }

        if (index == -1) {
            name = imageName;
        } else {
            name = imageName.substring(0, index);
        } // Used for setting image name

        name += ext;

        return name;
    }

    /**
     * Disposes of image memory and LUT Frame.
     */
    private void disposeThisImage() {
        int i, j;

        if (frameList != null) {
            frameList.removeAllElements();
        }

        frameList = null;

        if (voiVector != null) {

            // voiVector.removeAllElements();
            i = voiVector.size();

            for (j = i - 1; j >= 0; j--) {
                VOI voi = (VOI) ( ((Vector<VOI>) voiVector).remove(j));

                try {
                    voi.finalize();
                } catch (final Throwable e) {}

                voi = null;
            } // for (j = i-1; j >= 0; j--)

            voiVector = null;
        } // if ( voiVector != null )

        matrixHolder = null;
        mask = null;
        maskBU = null;
        imageName = null;
        UI = null;

        super.disposeLocal();
    }

    /**
     * Fixes file information to resultant image structure. When one image's file information is copied to anothers this
     * method sets the modality to OTHER and file directory to an empty string.
     * 
     * @param image source image
     */
    private void fixFileTypeInfo(final ModelImage image) {
        FileInfoBase[] fileInfo;

        if (image.getNDims() == 2) {
            fileInfo = image.getFileInfo();
            fileInfo[0].setModality(FileInfoBase.OTHER);
            fileInfo[0].setFileName("mask." + image.getFileInfo()[0].getFileSuffix());
        } else if (image.getNDims() == 3) {
            fileInfo = image.getFileInfo();

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setModality(FileInfoBase.OTHER);
                fileInfo[i].setFileName("mask." + image.getFileInfo()[0].getFileSuffix());
            }
        } else if (image.getNDims() == 4) {
            fileInfo = image.getFileInfo();

            for (int i = 0; i < (image.getExtents()[2] * image.getExtents()[3]); i++) {
                fileInfo[i].setModality(FileInfoBase.OTHER);
                fileInfo[i].setFileName("mask." + image.getFileInfo()[0].getFileSuffix());
            }
        }
    }

    /**
     * Accessor that sets the name of the image without registering the image.
     * 
     * @param name the String representing the filename
     */
    private void setClonedImageName(final String name) {

        // The clone method will register the image after the name has been set.
        imageName = name;
    }

    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportDataUseMask(final int start, final int length, boolean rescale, final byte[] values)
            throws IOException {
        int i, j;

        double imMax = getMax();
        double imMin = getMin();
        double imDiff = imMax - imMin;
        if ( (start >= 0) && ( (start + length) <= getSize()) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);
                if ( (imDiff <= 255) && !rescale )
                {
                	for (i = start, j = 0; j < length; i++, j++) {
                		if ( (mask != null) && useMask) {
                			if (mask.get(j)) {                        	                        	
                				values[j] = (byte)(((getDouble(i) - imMin)/imDiff) * imDiff);
                			} else {
                				values[j] = 0;
                			}
                		} else {
                			values[j] = (byte)(((getDouble(i) - imMin)/imDiff) * imDiff);
                		}
                	}                	
                }
                else
                {
                	for (i = start, j = 0; j < length; i++, j++) {
                		if ( (mask != null) && useMask) {
                			if (mask.get(j)) {                        	                        	
                				values[j] = (byte)(((getDouble(i) - imMin)/imDiff) * 255);
                			} else {
                				values[j] = 0;
                			}
                		} else {
                			values[j] = (byte)(((getDouble(i) - imMin)/imDiff) * 255);
                		}
                	}
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    
    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportDataUseMask(final int start, final int length, final float[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= getSize()) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);
                for (i = start, j = 0; j < length; i++, j++) {
                	if ( mask != null ) {
                		if (mask.get(j)) {                        	                        	
                			values[j] = getFloat(i);
                		} else {
                			values[j] = 0;
                		}
                	} else {
                		values[j] =  getFloat(i);
                	}
                }                	

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

}
