package gov.nih.mipav.model.structures;


import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * This class extends the generic buffer class ModelStorageArray and is used to store n-dimensional images and buffer
 * class that supports boolean, byte, short, int, long, float, double, etc. data types. After the buffer is created the
 * minimum and maximum parameters are calculated. ModelImage is a specific buffer to addressing issues relating to
 * images.
 *
 * @author   Matthew J. McAuliffe Ph.D.
 * @version  1.0
 */
public class ModelImage extends ModelStorageBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1234298038008494667L;

    /** DOCUMENT ME! */
    public static final int IMAGE_A = 0;

    /** DOCUMENT ME! */
    public static final int IMAGE_B = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * List of frames where this image is displayed. This is an important list, for example when the LUT of table is
     * changed the list is used to notify all the frames displaying this image that they should update their view of the
     * image. The transient keyword is used to indicate that the frame list should NOT be cloned when the image is
     * cloned. The new cloned image should build up its own list of frame(s) where it is displayed.
     */
    private transient Vector frameList = null;

    /** JTextArea for history. */
    private transient JTextArea historyArea = null;

    /** Reference to the history scroll pane. */
    private transient JScrollPane historySPane = null;

    /**
     * ImageName is patient's name when using DICOM images. It is filename for any other image something other than
     * filename must be used for DICOM because one DICOM image is made of many seperate files. ** Not necessarily the
     * file name.
     */
    private String imageName;

    /** Indicates the image order when two images are displayed in the same frame. */
    private int imageOrder = IMAGE_A;

    /**
     * Mask is a binary object that is true interior to a VOI and false otherwise used in algorithms to process only on
     * VOIs if indicated by the user.
     */
    private BitSet mask;

    /** Backup of mask for undoing. */
    private BitSet maskBU;

    /**
     * Reference to a transformation matix that can be applied to the image and tranform (warp) the image in some
     * manner.
     */
    private TransMatrix matrix;

    /** Reference to talairach transform information. */
    private TalairachTransformInfo talairach;

    /**
     * The user interface has a vector of all image models loaded into MIPAV. I put the reference to it here so that
     * when an image is created it can added tothe vector in the user interface.
     */
    private transient ViewUserInterface UI = null;

    /** List of VOIs that are displayed with this image. */
    private VOIVector voiVector = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ModelImage object.
     *
     * @param  type        indicates type of buffer(ie. boolean, byte ...)
     * @param  dimExtents  array indicating image extent in each dimension.
     * @param  name        name of the image.
     *
     *                     <p>The only difference with this constructor is that it does not require the
     *                     ViewUserInterface param.</p>
     */
    public ModelImage(int type, int[] dimExtents, String name) {
        this(type, dimExtents, name, ViewUserInterface.getReference());
    }

    /**
     * Creates a new ModelImage object.
     *
     * @param  type        indicates type of buffer(ie. boolean, byte ...)
     * @param  dimExtents  array indicating image extent in each dimension.
     * @param  name        name of the image.
     * @param  _UI         user interface reference so that image can be registered
     */
    public ModelImage(int type, int[] dimExtents, String name, ViewUserInterface _UI) {
        super(type, dimExtents);

        int i;

        // The user interface has a vector of all image models loaded into
        // MIPAV. I keep a reference to the userinterface here so that when an image
        // is created it can added to the hashtable in the user interface.
        UI = _UI;
        imageName = makeImageName(name, ""); // removes suffix if one is there.

        if (UI == null) {
            Preferences.debug("New ModelImage = " + imageName + ", but UI is null.");
        }

        if (UI != null) {
            UI.registerImage(this);
        }

        float[] resolutions = new float[5];

        for (i = 0; i < 5; i++) {
            resolutions[i] = (float) 1.0;
        }

        int[] units = new int[5];

        for (i = 0; i < 5; i++) {
            units[i] = FileInfoBase.MILLIMETERS;
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

        if (dimExtents.length == 2) {
            fileInfo = new FileInfoBase[1];

            // save the entire filename with the suffix -- helps later when saving file
            fileInfo[0] = new FileInfoImageXML(name, null, FileBase.XML);
            fileInfo[0].setExtents(dimExtents);
            fileInfo[0].setResolutions(resolutions);
            fileInfo[0].setUnitsOfMeasure(units);
            fileInfo[0].setDataType(type);
            matrix = new TransMatrix(3);
        } else if (dimExtents.length == 3) {
            matrix = new TransMatrix(4);
            fileInfo = new FileInfoBase[dimExtents[2]];

            for (i = 0; i < dimExtents[2]; i++) {

                // save the entire filename with the suffix -- helps later when saving file
                fileInfo[i] = new FileInfoImageXML(name, null, FileBase.XML);
                fileInfo[i].setExtents(dimExtents);
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(type);
            }
        } else if (dimExtents.length == 4) {
            matrix = new TransMatrix(4);
            fileInfo = new FileInfoBase[dimExtents[2] * dimExtents[3]];

            for (i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {

                // save the entire filename with the suffix -- helps later when saving file
                fileInfo[i] = new FileInfoImageXML(name, null, FileBase.XML);
                fileInfo[i].setExtents(dimExtents);
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(type);
            }
        }

        frameList = new Vector();
        voiVector = new VOIVector();
        makeInfoPanes();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns whether or not the given data type is a color data type.
     *
     * @param   dataType  DOCUMENT ME!
     *
     * @return  <code>true</code> if color, <code>false</code> if not color.
     */
    public static boolean isColorImage(int dataType) {

        if ((dataType == ARGB) || (dataType == ARGB_USHORT) || (dataType == ARGB_FLOAT)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Add a listener to this class so that notifyListener can be used to notify all listeners to update the display of
     * the image.
     *
     * @param  obj  "object' to be added to the list
     */
    public void addImageDisplayListener(ViewImageUpdateInterface obj) {

        if (frameList == null) {
            frameList = new Vector();
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
     * @param  VOIs  VOIs to add to image
     */
    public void addVOIs(VOIVector VOIs) {
        int nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {
            voiVector.add(VOIs.VOIAt(i).clone());
        }

        System.gc();
    }

    /**
     * Accessor that sets the fileInfo class for the image Different from super(...) in that this method also sets the
     * imageName.
     *
     * <p>DICOM images set their image name based on the patient name held in the first slice</p>
     *
     * @param  list      fileInfo structure.
     * @param  doRename  DOCUMENT ME!
     */
    // public void setFileInfo(FileInfoBase fInfo, int i)  {    super.setFileInfo(fInfo, i);        // make sure the
    // ModelStorageBase sets file info first

    // doing this when reading in, not sure it's necessary here.

    // DICOM files must be given an imagename different from its filename because DICOM images are made of many
    // different files. if (this.isDicomImage()) {  // if image is DICOM,
    // setImageName((String)(((FileInfoDicom)fileInfo[0]).getValue("0010,0010")));  // set imageName to patient name } }

    /**
     * Anonymize the image by altering the sensitive data of each slice to something generic.
     *
     * @see  FileInfoDicom#anonymize
     */
    public void anonymize(boolean[] list, boolean doRename) {
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
        }

        if (doRename) {
            this.setImageName("Anonymous");
        }
    }

    /**
     * Calculates the min and max values for the image array.
     */
    public void calcMinMax() {

        super.calcMinMax();

        if (fileInfo[0].getModality() == FileInfoBase.COMPUTED_TOMOGRAPHY) {

            if (getMin() < -1024) { // Do nothing
            } else {
                setMin(-1024);
            }

            if (getMax() > 3071) { // Do nothing
            } else {
                setMax(3071);
            }
        }

        for (int i = 0; i < fileInfo.length; i++) {

            if (!isColorImage()) {
                fileInfo[i].setMin(getMin());
                fileInfo[i].setMax(getMax());
            } else {
                fileInfo[i].setMinR(getMinR());
                fileInfo[i].setMaxR(getMaxR());
                fileInfo[i].setMinG(getMinG());
                fileInfo[i].setMaxG(getMaxG());
                fileInfo[i].setMinB(getMinB());
                fileInfo[i].setMaxB(getMaxB());
            }
        }

        // compare min and max to the last min and max
        // if they've changed, then reset the transfer function
        // in any frames
        if ((getMin() != lastMin) || (getMax() != lastMax)) {
            ViewJFrameImage frame = this.getParentFrame();

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

            float imgMin = (float) this.getMin();
            float imgMax = (float) this.getMax();

            lut.resetTransferLine(min, imgMin, max, imgMax);
        }
    }

    /**
     * Changes the image dimensionality or extents ( used in FFT exclusively ).
     *
     * @param  dimExtents  new dimensions for mask, maskBU, and fileInfo
     */
    public void changeExtents(int[] dimExtents) {
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

        float[] resolutions = fileInfo[0].getResolutions();
        int[] units = fileInfo[0].getUnitsOfMeasure();
        int type = fileInfo[0].getDataType();

        if (dimExtents.length == 2) {
            fileInfo = new FileInfoBase[1];
            fileInfo[0] = new FileInfoImageXML(null, null, FileBase.XML);
            fileInfo[0].setExtents(dimExtents);
            fileInfo[0].setResolutions(resolutions);
            fileInfo[0].setUnitsOfMeasure(units);
            fileInfo[0].setDataType(type);
        } else if (dimExtents.length == 3) {
            fileInfo = new FileInfoBase[dimExtents[2]];

            for (i = 0; i < dimExtents[2]; i++) {
                fileInfo[i] = new FileInfoImageXML(null, null, FileBase.XML);
                fileInfo[i].setExtents(dimExtents);
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(type);
            }
        } else if (dimExtents.length == 4) {
            fileInfo = new FileInfoBase[dimExtents[2] * dimExtents[3]];

            for (i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
                fileInfo[i] = new FileInfoImageXML(null, null, FileBase.XML);
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
            int size = mask.size();

            for (int i = 0; i < size; i++) {
                mask.clear(i);
            }
        }
    }

    /**
     * Copies the image and all data associated with the image (i.e. VOIs). Invokes the clone(String newName) method
     * with newName set to null;
     *
     * @return  the new copy of the image
     */
    public Object clone() {
        ModelImage image = (ModelImage) this.clone(null);

        image.frameList = new Vector();

        return ((Object) image);
    }

    /**
     * Copies the image and all data associated with the image (i.e. VOIs). Sets the name of the new image to newName.
     *
     * @param   newName  String containing the name for the cloned image. If null then 'this' image name is appended
     *                   with "_clone".
     *
     * @return  the new copy of the image
     */
    public Object clone(String newName) {
        ModelImage image = (ModelImage) super.clone();

        if (image == null) {
            return null;
        }

        image.setUserInterface(UI);
        image.makeInfoPanes();

        if (this.historyArea != null) {
            image.getHistoryArea().setText(this.getHistoryArea().getText());
        }

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

        return ((Object) image);
    } // end clone(String)

    /**
     * Deep copies the file type info from the fromImage to the current image object.
     *
     * @param  fromImage  image from which to copy file type info
     */
    public void copyFileTypeInfo(ModelImage fromImage) {
        FileInfoBase fileInfo;

        for (int i = 0; i < fromImage.getFileInfo().length; i++) {
            fileInfo = (FileInfoBase) (fromImage.getFileInfo(i).cloneItself());
            setFileInfo(fileInfo, i);
        }

        return;

    } // end copyFileTypeInfo()

    /**
     * Deep copies the file type info from the fromImage to the current image object. But does not copy over image
     * buffer type.
     *
     * @param  fromImage  image from which to copy file type info
     */
    public void copyFileTypeInfoSansBufferType(ModelImage fromImage) {
        FileInfoBase fileInfo;

        for (int i = 0; i < fromImage.getFileInfo().length; i++) {
            fileInfo = (FileInfoBase) (fromImage.getFileInfo(i).cloneItself());
            fileInfo.setDataType(getType());
            fileInfo.setFileName("Mask");
            setFileInfo(fileInfo, i);
        }

        return;

    }

    /**
     * Creates mask and maskBU of new length.
     *
     * @param  length  int
     */
    public void createMask(int length) {
        mask = new BitSet(length);
        maskBU = new BitSet(length);
    }

    /**
     * Displays all information about an image.
     *
     * @param  dialog  dialog object where image information is to be displayed
     * @param  z       index of file information -- each image slice can have separate file information (i.e. like
     *                 DICOM).
     * @param  t       t slice of the fileinfo to display.
     * @param  dicom   boolean indicating if this is a DICOM file
     * @param  xml     boolean indicating if this is a XML file
     */
    public void displayAboutInfo(JDialogBase dialog, int z, int t, boolean dicom, boolean xml) {
        int index;

        if (getNDims() == 2) {
            index = 0;
        } else if (getNDims() == 3) {
            index = z;
        } else if (getNDims() == 4) {
            index = (t * getExtents()[2]) + z;
        } else {
            index = (t * getExtents()[2]) + z;
        } // Hmmmmmmm

        if (dicom) {
            dialog.setTitle(dialog.getTitle() + ": " + (index + 1));
            ((JDialogFileInfoDICOM) dialog).displayAboutInfo(this, (FileInfoDicom) fileInfo[index]);
        } else if (xml) {
            dialog.setTitle(dialog.getTitle() + ": " + (index + 1));
            fileInfo[index].displayAboutInfo((JDialogFileInfoXML) dialog, matrix);
        } else {

            try {

                // System.out.println(" dialog = " + dialog);
                // System.out.println(" +++++++++++++++++++++++++++++  z  = " + i);
                // System.out.println(" fileInfo = " + fileInfo[i]);
                dialog.setTitle(dialog.getTitle() + ": " + (index + 1));
                fileInfo[index].displayAboutInfo((JDialogFileInfo) dialog, matrix);
            } catch (ClassCastException cce) {
                fileInfo[index].displayAboutInfo((JDialogText) dialog, matrix);
            }
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
     * @param  garbageCollect  boolean - A flag indicating whether or not garbage collection is invoked.
     */
    public void disposeLocal(boolean garbageCollect) {
        unRegisterImage();
        disposeThisImage();

        if (garbageCollect) {
            System.gc();
        }
    }

    /**
     * Forms a solid (without holes) binary image from all VOIs in the image.
     *
     * @return  image image of boolean type with VOI objects = 1 and background = 0
     */
    public ModelImage generateBinaryImage() {
        return this.generateBinaryImage(false, false);
    }

    /**
     * Forms a binary image from VOIs.
     *
     * @param   XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param   onlyActive  Only mask regions that are active (i.e. selected VOIs)
     *
     * @return  image image of boolean type with VOI objects = 1 and background = 0
     */
    public ModelImage generateBinaryImage(boolean XOR, boolean onlyActive) {

        int i;
        ModelImage maskImage = null;

        if (voiVector.size() == 0) {
            return null;
        }

        try {
            maskImage = new ModelImage(ModelImage.BOOLEAN, this.getExtents(), "Binary Image", UI);
            updateFileTypeInfo(this, maskImage);
            fixFileTypeInfo(maskImage);

        } catch (OutOfMemoryError error) {
            throw error;
        }

        for (i = 0; i < voiVector.size(); i++) {
            ((VOI) voiVector.elementAt(i)).createBinaryImage(maskImage, XOR, onlyActive);
        }

        maskImage.clearMask();
        maskImage.calcMinMax();

        return maskImage;
    }

    /**
     * Generates a solid (no holes) short image of regions defined by VOIs.
     *
     * @param   offset  offset value added to ID - normally 1 used to label the masked regions
     *
     * @return  ModelImage mask image of type short
     */
    public ModelImage generateShortImage(int offset) {
        return this.generateShortImage(offset, false, false);
    }

    /**
     * Exports a short mask of the VOI[index]. VOI[0] = 1 ... VOI[n] = n
     *
     * @param   offset      offset value added to ID - normally 1. ID is used to label the masked regions
     * @param   XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param   onlyActive  Only mask regions that are active (i.e. selected VOIs)
     *
     * @return  ModelImage mask image of type short
     */
    public ModelImage generateShortImage(int offset, boolean XOR, boolean onlyActive) {

        int i;
        ModelImage maskImage = null;

        if (voiVector.size() == 0) {
            return null;
        }

        try {
            maskImage = new ModelImage(ModelImage.SHORT, this.getExtents(), "Short Image", UI);
            updateFileTypeInfo(this, maskImage);
            fixFileTypeInfo(maskImage);
        } catch (OutOfMemoryError error) {
            throw error;
        }

        for (i = 0; i < voiVector.size(); i++) {
            maskImage.clearMask();
            ((VOI) voiVector.elementAt(i)).createShortImage(maskImage, offset, XOR, onlyActive);
        }

        maskImage.clearMask();
        maskImage.calcMinMax();

        return maskImage;
    }

    /**
     * Generates a solid (no holes) unsigned byte image of regions defined by VOIs.
     *
     * @param   offset  offset value added to ID - normally 1. ID is used to label the masked regions
     *
     * @return  ModelImage mask image of type unsigned byte
     */

    public ModelImage generateUnsignedByteImage(int offset) {
        return this.generateUnsignedByteImage(offset, false, false);
    }

    /**
     * Exports an unsigned byte mask of the VOI[index]. VOI[0] = 1 ... VOI[n] = n
     *
     * @param   offset      offset value added to ID - normally 1. ID is used to label the masked regions
     * @param   XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param   onlyActive  Only mask regions that are active (i.e. selected VOIs)
     *
     * @return  ModelImage mask image of type unsigned byte
     */
    public ModelImage generateUnsignedByteImage(int offset, boolean XOR, boolean onlyActive) {
        int i;
        ModelImage maskImage = null;

        if (voiVector.size() == 0) {
            return null;
        }

        try {
            maskImage = new ModelImage(ModelImage.UBYTE, this.getExtents(), "UBYTE Image", UI);
            updateFileTypeInfo(this, maskImage);
            fixFileTypeInfo(maskImage);
        } catch (OutOfMemoryError error) {
            throw error;
        }

        for (i = 0; i < voiVector.size(); i++) {
            maskImage.clearMask();
            ((VOI) voiVector.elementAt(i)).createUByteImage(maskImage, offset, XOR, onlyActive);
        }

        maskImage.clearMask();
        maskImage.calcMinMax();

        return maskImage;
    }

    /**
     * Generates a BitSet mask of all the VOIs and sets ImageModel mask.
     *
     * @return  binary mask of all VOIs returned as a BitSet object
     */
    public BitSet generateVOIMask() {
        return this.generateVOIMask(Preferences.is(Preferences.PREF_USE_VOI_XOR));
    }

    /**
     * Generates a BitSet mask of all the VOIs and sets ImageModel mask.
     *
     * @param   XOR  indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     *
     * @return  binary mask of all VOIs returned as a BitSet object
     */
    public BitSet generateVOIMask(boolean XOR) {
        int i;
        int[] extents = getExtents();

        if (voiVector.size() != 0) {

            for (i = 0; i < voiVector.size(); i++) {

                // System.out.println( "ModelImage: generateVOIMask(boolean XOR)  voi = " + i );
                voiVector.VOIAt(i).createBinaryMask(mask, extents[0], extents[1], XOR, false);
            }
        }

        return mask;
    }

    /**
     * Generates a mask of the type short - without XORing VOI contours.
     *
     * @param   mask   mask of VOI of type short
     * @param   index  indicates a specific VOI used to create the mask
     *
     * @return  mask short mask of the VOI
     */
    public short[] generateVOIMask(short[] mask, int index) {
        return this.generateVOIMask(mask, index, false);
    }

    /**
     * Generates a BitSet mask of all the VOIs and sets ImageModel mask.
     *
     * @param   XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param   onlyActive  Only mask regions that are active (i.e. selected )
     *
     * @return  binary mask of all VOIs returned as a BitSet object
     */
    public BitSet generateVOIMask(boolean XOR, boolean onlyActive) {
        int i;
        int[] extents = getExtents();

        if (voiVector.size() != 0) {

            for (i = 0; i < voiVector.size(); i++) {
                voiVector.VOIAt(i).createBinaryMask(mask, extents[0], extents[1], XOR, onlyActive);
            }
        }

        return mask;
    }

    /**
     * Exports a short mask of the VOI[index].
     *
     * @param   mask   mask of VOI of type short
     * @param   index  indicates a specific VOI used to create the mask
     * @param   XOR    indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     *
     * @return  mask short mask of the VOI
     */
    public short[] generateVOIMask(short[] mask, int index, boolean XOR) {

        if (voiVector.size() != 0) {
            return (((VOI) voiVector.elementAt(index)).createShortMask(getExtents()[0], getExtents()[1], mask, XOR));
        } else {
            return null;
        }
    }

    /**
     * Method that returns the animate frame if it exists else returns null.
     *
     * @return  animate frame
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
     * @return  histoLUTFrame
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
     * @return  histoRGBFrame
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
     * Accessor to get the history pane.
     *
     * @return  history pane
     */
    public JTextArea getHistoryArea() {
        return this.historyArea;
    }

    /**
     * Accessor to get the history scroll pane.
     *
     * @return  history pane
     */
    public JScrollPane getHistoryPane() {
        return this.historySPane;
    }

    /**
     * Calculates translation offset for transforming image about the center of the image.
     *
     * @return  Center of image in pixels.
     */
    public Point3Df getImageCenter() {
        Point3Df center;

        try {
            center = new Point3Df();
            center.x = (getExtents()[0] - 1) / 2f;
            center.y = (getExtents()[1] - 1) / 2f;

            if (getExtents().length > 2) {
                center.z = (getExtents()[2] - 1) / 2f;
            } else {
                center.z = 0f;
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("getImageCenter: Out of memory error");
            center = null;
            System.gc();
        }

        return center;
    }

    /**
     * Calculates translation offset for transforming image about the center of the image in the resolution space.
     *
     * @return  Center of the image in millimeters (or other physical dimension).
     */
    public Point3Df getImageCentermm() {
        Point3Df center;

        try {
            center = new Point3Df();
            center.x = (getExtents()[0] - 1) * fileInfo[0].getResolutions()[0] / 2f;
            center.y = (getExtents()[1] - 1) * fileInfo[0].getResolutions()[1] / 2f;

            if (getExtents().length > 2) {
                center.z = (getExtents()[2] - 1) * fileInfo[0].getResolutions()[2] / 2f;

            } else {
                center.z = 0f;
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("GetImageCentermm: Out of memory error");
            center = null;
            System.gc();
        }

        return center;
    }

    /**
     * Returns the file name of the image.
     *
     * @return  the String that represents the filename (as stored in the fileinfo)
     */
    public String getImageFileName() {

        if (fileInfo != null) {
            return fileInfo[0].getFileName();
        } else {
            return null;
        }
    }


    /**
     * Accessor that returns.
     *
     * @return  image frame vector
     */
    public Vector getImageFrameVector() {

        if (Preferences.debugLevel(Preferences.DEBUG_MINOR)) {
            Preferences.debug("Model Image Registered frames to image list:" + this.getImageName() + "\n");

            if (frameList != null) {

                for (int i = 0; i < frameList.size(); i++) {
                    Preferences.debug(((JFrame) (frameList.elementAt(i))).getTitle() + "\n");
                }
            }

            Preferences.debug("\n");
        }

        return frameList;
    }

    /**
     * Returns the type of image.
     *
     * @return  type of image (MRI, CT, ...)
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
     * @return  the String representing the filename if DICOM image then ImageName is the patients' name else, imageName
     *          is the file name (see the contructor for more)
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
     * @return  integer indicating image order
     */
    public int getImageOrder() {
        return imageOrder;
    }

    /**
     * Gets the image orientation (sagittal, axial, ...).
     *
     * @return  integer representing the orientation
     */
    public int getImageOrientation() {

        if (fileInfo != null) {
            return fileInfo[0].getImageOrientation();
        } else {
            return FileInfoBase.UNKNOWN_ORIENT;
        }
    }

    /**
     * Method that returns the lightbox frame if it exists else returns null.
     *
     * @return  lightbox frame
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
     * @return  mask that indicates which pixels/voxels will be processed.
     */
    public BitSet getMask() {
        return mask;
    }

    /**
     * Accessor that returns.
     *
     * @return  a bakeup of mask that indicates which pixels/voxels will be processed.
     */
    public BitSet getMaskBU() {
        return maskBU;
    }

    /**
     * Accessor that returns transformation matrix.
     *
     * @return  transformation matrix
     */
    public TransMatrix getMatrix() {
        return matrix;
    }

    /**
     * If no LUT or RGB color table is defined, this returns the packed int value for the color at iIndexs for the input
     * ModelImage kImage:
     *
     * @param   iIndex  pixel index
     *
     * @return  RGB color value.
     */
    public final int getPackedColor(int iIndex) {
        int iRed = 0;
        int iGreen = 0;
        int iBlue = 0;
        int iNewColor = 0;

        if (this.isColorImage()) {
            iRed = this.getC(iIndex, 1).byteValue();
            iGreen = this.getC(iIndex, 2).byteValue();
            iBlue = this.getC(iIndex, 3).byteValue();
        } else {
            Number kGray = new Double(255 * (this.getFloat(iIndex) - this.getMin()) / (this.getMax() - this.getMin()));
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
     * @return  The parent frame of this image.
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
     * @param   topLeft   the top left point of the plane; must be in the image coord system.
     * @param   topRight  the top right point of the plane; must be in the image coord system.
     * @param   botLeft   the bottom left point of the plane; must be in the image coord system.
     * @param   botRight  the bottom right point of the plane; must be in the image coord system. not really used..
     *
     * @return  a float buffer containing the extracted plane (size == image.getSliceSize() 2).
     */
    public final float[] getPlane(Point3Df topLeft, Point3Df topRight, Point3Df botLeft, Point3Df botRight) {
        double x, y, z;
        int i, j, index;

        // start at top left, move along line defined by (topLeft, topRight) to fill extents[0] pixels,
        // move down a distance determined by the line (topLeft, botLeft) divided by extents[1]
        // stop when slicesize pixels have been filled (and we should reach botRight..)

        float xRes = getFileInfo(0).getResolutions()[0];
        float yRes = getFileInfo(0).getResolutions()[1];
        float zRes = getFileInfo(0).getResolutions()[2];
        topLeft.x *= xRes;
        topLeft.y *= yRes;
        topLeft.z *= zRes;
        topRight.x *= xRes;
        topRight.y *= yRes;
        topRight.z *= zRes;
        botLeft.x *= xRes;
        botLeft.y *= yRes;
        botLeft.z *= zRes;
        botRight.x *= xRes;
        botRight.y *= yRes;
        botRight.z *= zRes;

        int planeLength = (int) MipavMath.round(Math.sqrt(((botLeft.x - topLeft.x) * (botLeft.x - topLeft.x)) +
                                                          ((botLeft.y - topLeft.y) * (botLeft.y - topLeft.y)) +
                                                          ((botLeft.z - topLeft.z) * (botLeft.z - topLeft.z))));
        int planeWidth = (int) MipavMath.round(Math.sqrt(((topRight.x - topLeft.x) * (topRight.x - topLeft.x)) +
                                                         ((topRight.y - topLeft.y) * (topRight.y - topLeft.y)) +
                                                         ((topRight.z - topLeft.z) * (topRight.z - topLeft.z))));
        int planeSize = planeLength * planeWidth;
        float[] plane = new float[planeSize];

        double colStep = 1.0 / planeWidth;
        double rowStep = 1.0 / planeLength;
        double colFactor = 0;
        double rowFactor = 0;
        double rowOffsetX = 0;
        double rowOffsetY = 0;
        double rowOffsetZ = 0;
        float colDeltaX = topRight.x - topLeft.x;
        float colDeltaY = topRight.y - topLeft.y;
        float colDeltaZ = topRight.z - topLeft.z;
        float rowDeltaX = botLeft.x - topLeft.x;
        float rowDeltaY = botLeft.y - topLeft.y;
        float rowDeltaZ = botLeft.z - topLeft.z;

        float invXRes = 1.0f / xRes;
        float invYRes = 1.0f / yRes;
        float invZRes = 1.0f / zRes;

        for (rowFactor = 0, i = 0, index = 0; rowFactor < 1; i++, rowFactor = rowStep * i) {

            for (colFactor = 0, j = 0; colFactor < 1; j++, colFactor = colStep * j, index++) {

                // get point along horiz. line
                x = topLeft.x + (colFactor * colDeltaX) + rowOffsetX;
                y = topLeft.y + (colFactor * colDeltaY) + rowOffsetY;
                z = topLeft.z + (colFactor * colDeltaZ) + rowOffsetZ;

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
     * @return  registration frame
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
     * @param  x             Absolute x value in slice.
     * @param  y             Absolute y value in slice.
     * @param  z             Absolute z value in slice.
     * @param  scannerCoord  the point transformed into the scanner's (DICOM) coordinate system.
     */
    public void getScannerCoordLPS(int x, int y, int z, float[] scannerCoord) {

        if ((getFileInfo()[0].getTransformID() != FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL) &&
                (getFileInfo()[0].getFileFormat() != FileBase.XML) &&
                (getFileInfo()[0].getFileFormat() != FileBase.MINC) &&
                (getFileInfo()[0].getFileFormat() != FileBase.NIFTI) &&
                (getFileInfo()[0].getFileFormat() != FileBase.AFNI)) {
            return;
        }

        int nDims = getNDims();

        if (nDims > 3) {
            nDims = 3;
        }

        float[] coord = new float[3];
        float[] tCoord = new float[3];
        float[] origin = new float[3];
        float[] res = new float[3];

        // Get the voxel coordinate in from mouse events in image space
        coord[0] = x;
        coord[1] = y;
        coord[2] = z;

        // Get the DICOM reference point ( origin ) in the image orientation (axial, sag, coronal )
        origin[0] = getFileInfo()[0].getOrigin()[0]; // LPS = x,y,z orientation for DICOM origin
        origin[1] = getFileInfo()[0].getOrigin()[1];
        origin[2] = getFileInfo()[0].getOrigin()[2];
        // System.out.println("Origin     "  + origin[0] + ", " + origin[1] + ", " + origin[2] );

        // Get voxel resolutions
        res[0] = getFileInfo(0).getResolutions()[0];
        res[1] = getFileInfo(0).getResolutions()[1];
        res[2] = getFileInfo(0).getResolutions()[2];
        // System.out.println("res " + res[0] + ", " + res[1] + ", " + res[2]);

        // Change voxel coordinate into millimeter space
        coord[0] = coord[0] * res[0];
        coord[1] = coord[1] * res[1];
        coord[2] = coord[2] * res[2];

        // Get the DICOM transform that discribes the transformation from axial to this image orientation
        TransMatrix dicomMatrix = (TransMatrix) (getMatrix().clone());

        // System.out.println("dicomMatrix = " + dicomMatrix.toString());
        if ((getFileInfo()[0].getFileFormat() == FileBase.MINC) ||
                (getFileInfo()[0].getFileFormat() == FileBase.AFNI) ||
                (getFileInfo()[0].getFileFormat() == FileBase.NIFTI) ||
                (getFileInfo()[0].getFileFormat() == FileBase.XML)) {

            for (int r = 0; r < 4; r++) {

                for (int c = 0; c < 4; c++) {
                    dicomMatrix.setMatrix(0.0, r, c);
                }
            }

            dicomMatrix.setMatrix(1.0, 3, 3);

            for (int j = 0; j < 3; j++) {

                switch (getFileInfo(0).getAxisOrientation()[j]) {

                    case FileInfoBase.ORI_L2R_TYPE:
                    case FileInfoBase.ORI_R2L_TYPE:
                        if (origin[0] > 0.0) {
                            dicomMatrix.setMatrix(-1.0, 0, j);
                        } else {
                            dicomMatrix.setMatrix(1.0, 0, j);
                        }

                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                    case FileInfoBase.ORI_A2P_TYPE:
                        if (origin[1] > 0.0) {
                            dicomMatrix.setMatrix(-1.0, 1, j);
                        } else {
                            dicomMatrix.setMatrix(1.0, 1, j);
                        }

                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                    case FileInfoBase.ORI_I2S_TYPE:
                        if (origin[2] > 0.0) {
                            dicomMatrix.setMatrix(-1.0, 2, j);
                        } else {
                            dicomMatrix.setMatrix(1.0, 2, j);
                        }

                        break;
                }
            }
        } // if ( getFileInfo()[0].getFileFormat() == FileBase.MINC)

        // System.out.println("dicomMatrix = " + dicomMatrix.toString());

        // Finally convert the point to axial millimeter DICOM space.
        dicomMatrix.transform(coord, tCoord);

        // Add in the DICOM origin
        scannerCoord[0] = origin[0] + tCoord[0];
        scannerCoord[1] = origin[1] + tCoord[1];
        scannerCoord[2] = origin[2] + tCoord[2];
    }


    /**
     * Takes input x,y,z coordinate and returns that point transformed into the RAS coordinate system. R = Right - first
     * axis is positive to the right A = Anterior - second axis is positive to the anterior S = Superior - third axis is
     * positive to the superior
     *
     * @param  x             Absolute x value in slice.
     * @param  y             Absolute y value in slice.
     * @param  z             Absolute z value in slice.
     * @param  scannerCoord  the point transformed into the scanner's (DICOM) coordinate system.
     */

    public void getScannerCoordRAS(int x, int y, int z, float[] scannerCoord) {

        if (getFileInfo()[0].getTransformID() != FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL) {
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
     * @return  TalairachTransformInfo talairach info
     */
    public TalairachTransformInfo getTalairachTransformInfo() {
        return talairach;
    }

    /**
     * Accessor to get the Transform ID for the matrix.
     *
     * @return  int transform ID
     */
    public int getTransformID() {
        return fileInfo[0].getTransformID();
    }

    /**
     * Method that returns the tri image frame if it exists else returns null.
     *
     * @return  tri image frame
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
     * @return  the reference to the user interface.
     */
    public ViewUserInterface getUserInterface() {
        return UI;
    }

    /**
     * Accessor that returns.
     *
     * @return  VOI vector
     */
    public VOIVector getVOIs() {
        return voiVector;
    }

    /**
     * Forms a single VOI structure from all the VOIs presently loaded in the imageModel.
     */
    public void groupVOIs() {
        int i, j, k;
        int nVOIs;
        int nSlices;
        int nContours, nPoints;
        int index = 1;
        Vector[] contours;
        Point3Df[] points;
        Point3Df[] point1 = new Point3Df[1];
        VOI newVOI = null;
        VOI newPtVOI = null;
        VOI newPLineVOI = null;
        VOI newLineVOI = null;
        VOI newProtractorVOI = null;

        String nameExt = null;
        int slices;

        if (getNDims() > 2) {
            nameExt = new String("3D");
            slices = getExtents()[2];
        } else {
            nameExt = new String("2D");
            slices = 1;
        }

        VOIVector tempVOIs = (VOIVector) voiVector.clone();

        VOIBase tempBase = null;
        nVOIs = tempVOIs.size();

        for (i = nVOIs - 1; i >= 0; i--) {

            if ((tempVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) && tempVOIs.VOIAt(i).isActive()) {

                if (newVOI == null) {
                    newVOI = new VOI((short) 0, "joinedContour", slices, VOI.CONTOUR, -1.0f);
                    newVOI.setAllActive(true);
                }

                nSlices = tempVOIs.VOIAt(i).getCurves().length;
                contours = tempVOIs.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = nContours - 1; k >= 0; k--) {

                        if (((VOIBase) contours[j].elementAt(k)).isActive()) {
                            tempBase = (VOIContour) contours[j].elementAt(k);
                            contours[j].removeElementAt(k);
                            tempBase.setName(newVOI.getName());
                            newVOI.getCurves()[j].addElement(tempBase);
                        }
                    }
                }

                // re-label all of the elements in the old contour
                index = 1;

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        ((VOIBase) contours[j].elementAt(k)).setLabel(String.valueOf((index++)));
                    }
                }

                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }


            } else if ((tempVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) && tempVOIs.VOIAt(i).isActive()) {

                if (newPLineVOI == null) {
                    newPLineVOI = new VOI((short) 0, "polyLine" + nameExt, slices, VOI.POLYLINE, -1.0f);
                    newPLineVOI.setAllActive(true);
                }

                nSlices = tempVOIs.VOIAt(i).getCurves().length;
                contours = tempVOIs.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = nContours - 1; k >= 0; k--) {

                        if (((VOIBase) contours[j].elementAt(k)).isActive()) {
                            tempBase = (VOIContour) contours[j].elementAt(k);
                            contours[j].removeElementAt(k);
                            tempBase.setName(newPLineVOI.getName());
                            newPLineVOI.getCurves()[j].addElement(tempBase);
                        }
                    }
                }

                // re-label all elements
                index = 1;

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        ((VOIBase) contours[j].elementAt(k)).setLabel(String.valueOf((index++)));
                    }
                }

                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }

            } else if ((tempVOIs.VOIAt(i).getCurveType() == VOI.LINE) && tempVOIs.VOIAt(i).isActive()) {

                if (newLineVOI == null) {
                    newLineVOI = new VOI((short) 0, "line" + nameExt, slices, VOI.LINE, -1.0f);
                    newLineVOI.setAllActive(true);
                }

                nSlices = tempVOIs.VOIAt(i).getCurves().length;
                contours = tempVOIs.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = nContours - 1; k >= 0; k--) {

                        if (((VOIBase) contours[j].elementAt(k)).isActive()) {
                            tempBase = (VOILine) contours[j].elementAt(k);
                            contours[j].removeElementAt(k);
                            tempBase.setName(newLineVOI.getName());
                            newLineVOI.getCurves()[j].addElement(tempBase);
                        }
                    }
                }

                // re-label all elements
                index = 1;

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        ((VOIBase) contours[j].elementAt(k)).setLabel(String.valueOf((index++)));
                    }
                }

                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }


            } else if ((tempVOIs.VOIAt(i).getCurveType() == VOI.POINT) && tempVOIs.VOIAt(i).isActive()) {

                if (newPtVOI == null) {
                    newPtVOI = new VOI((short) 0, "point" + nameExt, slices, VOI.POINT, -1.0f);
                    newPtVOI.setAllActive(true);
                }

                nSlices = tempVOIs.VOIAt(i).getCurves().length;

                for (j = 0; j < nSlices; j++) {
                    points = tempVOIs.VOIAt(i).exportPoints(j);
                    nPoints = points.length;

                    for (k = nPoints - 1; k >= 0; k--) {

                        if (((VOIBase) tempVOIs.VOIAt(i).getCurves()[j].elementAt(k)).isActive()) {
                            point1[0] = points[k];
                            newPtVOI.importCurve(point1, j);
                            tempVOIs.VOIAt(i).getCurves()[j].removeElementAt(k);
                        }
                    }

                    // re-label all elements
                    index = 1;
                    contours = tempVOIs.VOIAt(i).getCurves();

                    for (j = 0; j < nSlices; j++) {
                        nContours = contours[j].size();

                        for (k = 0; k < nContours; k++) {
                            ((VOIBase) contours[j].elementAt(k)).setLabel(String.valueOf(index++));
                        }
                    }

                    // if the old contour is now empty, remove it
                    if (tempVOIs.VOIAt(i).isEmpty()) {
                        tempVOIs.removeElementAt(i);
                    }
                }
            } else if ((tempVOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) && tempVOIs.VOIAt(i).isActive()) {

                if (newProtractorVOI == null) {
                    newProtractorVOI = new VOI((short) 0, "protractor" + nameExt, slices, VOI.PROTRACTOR, -1.0f);
                    newProtractorVOI.setAllActive(true);
                }

                nSlices = tempVOIs.VOIAt(i).getCurves().length;
                contours = tempVOIs.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = nContours - 1; k >= 0; k--) {

                        if (((VOIBase) tempVOIs.VOIAt(i).getCurves()[j].elementAt(k)).isActive()) {
                            tempBase = (VOIProtractor) contours[j].elementAt(k);
                            contours[j].removeElementAt(k);
                            tempBase.setName(newProtractorVOI.getName());
                            newProtractorVOI.getCurves()[j].addElement(tempBase);
                        }
                    }
                }

                // re-label all elements
                index = 1;

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        ((VOIBase) contours[j].elementAt(k)).setLabel(String.valueOf(index++));
                    }
                }

                // if the old contour is now empty, remove it
                if (tempVOIs.VOIAt(i).isEmpty()) {
                    tempVOIs.removeElementAt(i);
                }


            }
        }

        int sliceNum, curveNum;

        index = 1;

        if (newVOI != null) {

            for (sliceNum = 0; sliceNum < newVOI.getCurves().length; sliceNum++) {

                for (curveNum = 0; curveNum < newVOI.getCurves()[sliceNum].size(); curveNum++) {
                    ((VOIBase) newVOI.getCurves()[sliceNum].elementAt(curveNum)).setLabel(String.valueOf(index++));
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
         * System.err.println( i + ": VOI name: " + tempVOI.getName()); tempCurves = tempVOI.getCurves(); for (j = 0; j
         * < tempCurves.length; j++) {     System.err.println("\tSize: " + tempCurves[j].size());
         * System.err.println("\t" + tempCurves[j].toString()); } }
         */
        for (i = 0; i < voiVector.size(); i++) {
            ((VOI) voiVector.elementAt(i)).setID((short) i);
        }

        notifyImageDisplayListeners();

    }

    /**
     * Forms a single VOI structure from all the VOIs presently loaded in the imageModel.
     *
     * @param  newVOIVector  DOCUMENT ME!
     * @param  where         DOCUMENT ME!
     * @param  name          DOCUMENT ME!
     */
    public void groupVOIs(ViewVOIVector newVOIVector, int[] where, String name) {
        System.err.println("calling group VOIs, passing in new vector");

        int i, j, k;
        int nVOIs;
        int nSlices;
        int nContours, nPoints;
        Vector[] contours;
        Point3Df[] points;
        Point3Df[] point1 = new Point3Df[1];

        VOI newVOI = null;
        VOI newPtVOI = null;
        VOI newPLineVOI = null;
        VOI newLineVOI = null;
        VOI newProtractorVOI = null;

        String nameExt = null;
        int slices;

        if (getNDims() > 2) {
            nameExt = new String("3D");
            slices = getExtents()[2];
        } else {
            nameExt = new String("2D");
            slices = 1;
        }

        nVOIs = newVOIVector.size();

        for (i = 0; i < nVOIs; i++) {

            if (newVOIVector.VOIAt(i).getCurveType() == VOI.CONTOUR) {

                if (newVOI == null) {
                    newVOI = new VOI((short) 0, "polygon", slices, VOI.CONTOUR, -1.0f);
                }

                nSlices = newVOIVector.VOIAt(i).getCurves().length;
                contours = newVOIVector.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        newVOI.getCurves()[j].addElement(contours[j].elementAt(k));
                    }
                }
            } else if (newVOIVector.VOIAt(i).getCurveType() == VOI.POLYLINE) {

                if (newPLineVOI == null) {
                    newPLineVOI = new VOI((short) 0, "polyline" + nameExt, slices, VOI.POLYLINE, -1.0f);
                }

                nSlices = voiVector.VOIAt(i).getCurves().length;
                contours = voiVector.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        newPLineVOI.getCurves()[j].addElement(contours[j].elementAt(k));
                    }
                }
            } else if (newVOIVector.VOIAt(i).getCurveType() == VOI.LINE) {

                if (newLineVOI == null) {
                    newLineVOI = new VOI((short) 0, "line" + nameExt, slices, VOI.LINE, -1.0f);
                }

                nSlices = voiVector.VOIAt(i).getCurves().length;
                contours = voiVector.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        newLineVOI.getCurves()[j].addElement(contours[j].elementAt(k));
                    }
                }
            } else if (newVOIVector.VOIAt(i).getCurveType() == VOI.POINT) {

                if (newPtVOI == null) {
                    newPtVOI = new VOI((short) 0, "point" + nameExt, slices, VOI.POINT, -1.0f);
                }

                nSlices = voiVector.VOIAt(i).getCurves().length;

                for (j = 0; j < nSlices; j++) {
                    points = voiVector.VOIAt(i).exportPoints(j);
                    nPoints = points.length;

                    for (k = 0; k < nPoints; k++) {
                        point1[0] = points[k];
                        newPtVOI.importCurve(point1, j);
                    }
                }
            } else if (voiVector.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {

                if (newProtractorVOI == null) {
                    newProtractorVOI = new VOI((short) 0, "protractor" + nameExt, slices, VOI.PROTRACTOR, -1.0f);
                }

                nSlices = voiVector.VOIAt(i).getCurves().length;
                contours = voiVector.VOIAt(i).getCurves();

                for (j = 0; j < nSlices; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        newProtractorVOI.getCurves()[j].addElement(contours[j].elementAt(k));
                    }
                }
            }
        }

        // Sort where so that the length of the vector won't get screwed up
        for (i = 1; i < where.length; i++) {
            int tmp = where[i];

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
            ((VOI) voiVector.elementAt(i)).setID((short) i);
        }

        int sliceNum, curveNum, voiNum;
        int index = 1;

        for (voiNum = 0; voiNum < voiVector.size(); voiNum++) {

            if (((VOI) voiVector.elementAt(voiNum)).getName().equals(name)) {

                for (sliceNum = 0; sliceNum < ((VOI) voiVector.elementAt(voiNum)).getCurves().length; sliceNum++) {

                    for (curveNum = 0; curveNum < ((VOI) voiVector.elementAt(voiNum)).getCurves()[sliceNum].size();
                             curveNum++) {
                        ((VOIBase) ((VOI) voiVector.elementAt(voiNum)).getCurves()[sliceNum].elementAt(curveNum))
                            .setLabel(String.valueOf(index++));
                    }
                }

            }
        }

        notifyImageDisplayListeners();
    }

    /**
     * Accessor that returns whether or not the image is a color image.
     *
     * @return  <code>true</code> if color, <code>false</code> if not color.
     */
    public boolean isColorImage() {

        if ((getType() == ARGB) || (getType() == ARGB_USHORT) || (getType() == ARGB_FLOAT)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Accessor that returns whether or not the image is a DICOM image.
     *
     * @return  <code>true</code> if DICOM, <code>false</code> if not DICOM.
     */
    public boolean isDicomImage() {

        if (fileInfo[0].getFileFormat() == FileBase.DICOM) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Accessor that returns whether or not the image is a MINC image.
     *
     * @return  <code>true</code> if MINC, <code>false</code> if not MINC.
     */
    public boolean isMincImage() {

        if (fileInfo[0].getFileFormat() == FileBase.MINC) {
            return true;
        } else {
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

            if ((frameList.elementAt(i) instanceof ViewJFrameBase)) {
                ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages();
            }
        }
    }

    /**
     * Used to notify all frames that display this image model need to be updated.
     *
     * @param  LUT        new LUT used to display image (can be null);
     * @param  forceShow  force the display method(s) to reload image data and display image slower but needed if image
     *                    model changes.
     */
    public void notifyImageDisplayListeners(ModelLUT LUT, boolean forceShow) {

        if (frameList != null) {

            for (int i = 0; i < frameList.size(); i++) {

                if ((frameList.elementAt(i) instanceof ViewJFrameBase)) {
                    ModelImage imgA = ((ViewJFrameBase) frameList.elementAt(i)).getImageA();
                    ModelImage imgB = ((ViewJFrameBase) frameList.elementAt(i)).getImageB();

                    if (this == imgA) {
                        ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgB) {
                        ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages(null, LUT, forceShow, -1);
                    }
                } /* LUT update of a non-ModelImage data strucuture: */
                else if ((frameList.elementAt(i) instanceof
                              gov.nih.mipav.view.renderer.surfaceview.brainflattenerview.JPanelBrainSurfaceFlattener)) {
                    ModelImage imgA = ((gov.nih.mipav.view.renderer.surfaceview.brainflattenerview.JPanelBrainSurfaceFlattener)
                                           frameList.elementAt(i)).getImageA();
                    ModelImage imgB = ((gov.nih.mipav.view.renderer.surfaceview.brainflattenerview.JPanelBrainSurfaceFlattener)
                                           frameList.elementAt(i)).getImageB();

                    if (this == imgA) {
                        ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages(LUT, null, forceShow, -1);
                    } else if (this == imgB) {
                        ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages(null, LUT, forceShow, -1);
                    }
                }
            }
        }

        if ((getHistoLUTFrame() != null) && (forceShow == true)) {

            if (getHistoLUTFrame().getImageA() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_A);
            } else if (getHistoLUTFrame().getImageB() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_B);
            }
        }

        if ((getHistoRGBFrame() != null) && (forceShow == true)) {

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
     * @param  forceShow   force the display method(s) to reload image data and display image slower but needed if image
     *                     model changes.
     * @param  alphaBlend  the amount to blend between two images displayed in the same frame.
     * @param  RGBT        ModelRGB
     */
    public void notifyImageDisplayListeners(boolean forceShow, int alphaBlend, ModelRGB RGBT) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ((frameList.elementAt(i) instanceof ViewJFrameBase)) {
                ModelImage imgA = ((ViewJFrameBase) frameList.elementAt(i)).getImageA();
                ModelImage imgB = ((ViewJFrameBase) frameList.elementAt(i)).getImageB();

                if (this == imgA) {
                    ((ViewJFrameBase) frameList.elementAt(i)).setRGBTA(RGBT);
                } else if (this == imgB) {
                    ((ViewJFrameBase) frameList.elementAt(i)).setRGBTB(RGBT);
                }

                ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages(null, null, forceShow, -1);
            }
        }

        if ((getHistoLUTFrame() != null) && (forceShow == true)) {

            if (getHistoLUTFrame().getImageA() == this) {
                getHistoLUTFrame().notifyOfUpdate(null, ViewJFrameBase.IMAGE_A);
            } else if (getHistoLUTFrame().getImageB() == this) {
                getHistoLUTFrame().notifyOfUpdate(null, ViewJFrameBase.IMAGE_B);
            }
        }

        if ((getHistoRGBFrame() != null) && (forceShow == true)) {

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
     * @param  LUT         new LUT used to display image (can be null);
     * @param  forceShow   force the display method(s) to reload image data and display image slower but needed if image
     *                     model changes.
     * @param  alphaBlend  indicates the amount of blending between two images (image 1's blending value) 1.0 - all of
     *                     image 1; 0.5 - half image 1 and half image 2
     * @param  interpMode  image interpolation method (Nearest or Smooth)
     */
    public void notifyImageDisplayListeners(ModelLUT LUT, boolean forceShow, int alphaBlend, int interpMode) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ((frameList.elementAt(i) instanceof ViewJFrameBase)) {
                ModelImage imgA = ((ViewJFrameBase) frameList.elementAt(i)).getImageA();
                ModelImage imgB = ((ViewJFrameBase) frameList.elementAt(i)).getImageB();

                ((ViewJFrameBase) frameList.elementAt(i)).setAlphaBlend(alphaBlend);

                if (this == imgA) {
                    ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages(LUT, null, forceShow, interpMode);
                } else if (this == imgB) {
                    ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImages(null, LUT, forceShow, interpMode);
                }
            }
        }

        if ((getHistoLUTFrame() != null) && (forceShow == true)) {

            if (getHistoLUTFrame().getImageA() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_A);
            } else if (getHistoLUTFrame().getImageB() == this) {
                getHistoLUTFrame().notifyOfUpdate(LUT, ViewJFrameBase.IMAGE_B);
            }
        }

        if ((getHistoRGBFrame() != null) && (forceShow == true)) {

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

            if ((frameList.elementAt(i) instanceof ViewJFrameBase)) {
                ((ViewImageUpdateInterface) frameList.elementAt(i)).updateImageExtents();
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
     * @param  composite  if true make a composite matrix of the by multipling this matrix with the one to be read from
     *                    the file. If false replace this object matrix with a new matrix read from the file.
     */
    public void readTransformMatrix(boolean composite) {

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

            int returnVal = chooser.showOpenDialog(UI.getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                UI.setDefaultDirectory(directory);
            } else {
                return;
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ModelImage.readTransformMatrix");

            return;
        }

        try {
            File file = new File(UI.getDefaultDirectory() + fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "r");

            matrix.readMatrix(raFile, composite);
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix read error");

            return;
        }
    }

    /**
     * Read matrix from a file.
     *
     * @param  fileName   name of the matrix file.
     * @param  composite  if true make a composite matrix of the by multipling this matrix with the one to be read from
     *                    the file. If false replace this object matrix with a new matrix read from the file.
     */
    public void readTransformMatrix(String fileName, boolean composite) {

        if (fileName == null) {
            return;
        }

        try {
            File file = new File(UI.getDefaultDirectory() + fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "r");

            matrix.readMatrix(raFile, composite);
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix read error");

            return;
        }
    }

    /**
     * Reallocates ModelImage with new type and all image data lost.
     *
     * @param  type  new type of image that is to be allocated
     */
    public void reallocate(int type) {
        int[] dimExtents = getExtents();

        if (dimExtents.length == 2) {
            fileInfo[0].setDataType(type);
        } else if (dimExtents.length == 3) {

            for (int i = 0; i < dimExtents[2]; i++) {
                fileInfo[i].setDataType(type);
            }
        } else if (dimExtents.length == 4) {

            for (int i = 0; i < (dimExtents[2] * dimExtents[3]); i++) {
                fileInfo[i].setDataType(type);
            }
        }

        try {
            super.reallocate(type);
        } catch (IOException ioError) {
            MipavUtil.displayError("" + ioError);
        }
    }

    /**
     * Method that register an VOI to this image.
     *
     * @param  voi  Region of interest (VOI) to be registered with the image model
     */
    public void registerVOI(VOI voi) {
        voiVector.addVOI(voi);
        // need to add voi to list object!!!
    }

    /**
     * Remove a listener from the class.
     *
     * @param  obj  "object' to be added to the list
     */
    public void removeImageDisplayListener(ViewImageUpdateInterface obj) {

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
     * @param   directory  location where the image is to stored.
     * @param   fileName   the name of the file (without the extension).
     * @param   fileType   DOCUMENT ME!
     * @param   isActive   DOCUMENT ME!
     *
     * @return  true if succeeded in saving.
     */
    public boolean saveImage(String directory, String fileName, int fileType, boolean isActive) {
        FileWriteOptions options = new FileWriteOptions(false);

        if (this.getNDims() == 3) {
            options.setBeginSlice(0);
            options.setEndSlice(this.getExtents()[2] - 1);
        } else if (this.getNDims() == 4) {
            options.setBeginSlice(0);
            options.setEndSlice(this.getExtents()[2] - 1);
            options.setBeginTime(0);
            options.setEndTime(this.getExtents()[3] - 1);
        }

        options.setActiveImage(isActive);
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
        FileIO fileIO = new FileIO();

        fileIO.writeImage(this, options);

        return true;
    }

    /// Note to Matt should the matrix R/W be moved to the Matrix class ??
    /**
     * Saves the transformation matrix to file.
     */
    public void saveTransformMatrix() {

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

            int returnVal = chooser.showSaveDialog(UI.getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                UI.setDefaultDirectory(directory);
            } else {
                return;
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ModelImage.saveTransformMatrix");

            return;
        }

        try {
            File file = new File(UI.getDefaultDirectory() + fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");

            matrix.saveMatrix(raFile);
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error");

            return;
        }
    }

    /**
     * Save the images transformation matrix in the working directory with the supplied fileName.
     *
     * @param  fileName  - fileName of transformation matrix
     */
    public void saveTransformMatrix(String fileName) {

        if (fileName == null) {
            return;
        }

        try {
            File file = new File(UI.getDefaultDirectory() + fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");

            matrix.saveMatrix(raFile);
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error");

            return;
        }
    }

    /**
     * Accessor to set the history vector.
     *
     * @param  ha  history vector
     */
    public void setHistory(JTextArea ha) {
        this.historyArea = ha;
    }

    /**
     * Accessor to set the history scroll pane.
     *
     * @param  hp  history pane
     */
    public void setHistoryPane(JScrollPane hp) {
        this.historySPane = hp;
    }

    /**
     * Sets the image directory.
     *
     * @param  dir  string representing the directory
     */
    public void setImageDirectory(String dir) {

        if (fileInfo != null) {

            for (int i = 0; i < getFileInfo().length; i++) {
                getFileInfo(i).setFileDirectory(dir);
            }
        }
    }

    /**
     * Sets the image type (MRI, CT, ...).
     *
     * @param  type  integer representing the type
     */
    public void setImageModality(int type) {

        if (fileInfo != null) {
            fileInfo[0].setModality(type);
        }
    }

    /**
     * Accessor that sets the name of the image.
     *
     * @param  name  the String representing the filename
     */
    public void setImageName(String name) {

        // update the fileInfo names
        FileInfoBase[] fInfos = this.getFileInfo();
        String tempName = fInfos[0].getFileName();
        String suffix = "";


        if ((tempName != null) && (tempName.lastIndexOf(".") != -1)) {
            suffix = tempName.substring(tempName.lastIndexOf("."), tempName.length());
        }

        if (suffix == null) {
            suffix = "";
        }

        // System.err.println("Full new name: " + name + suffix);
        for (int i = 0; i < fInfos.length; i++) {
            fInfos[i].setFileName(name + suffix);
        }

        // first check to see if the image name already equals name
        if ((name != null) && name.equals(this.getImageName())) {

            // make sure the image is registered
            if (UI != null) {

                if (!UI.isImageRegistered(name)) {
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

    /**
     * Accessor that sets the name of the image. NOT TO BE USED BY ANYONE EXCEPT ViewUserInterface.registerImage. Use
     * setImageName instead!!!
     *
     * @param  name  the String representing the filename
     */
    public void setImageNamePrivate(String name) {
        imageName = name;
    }

    /**
     * For multiple image viewers this indicates order of the image.
     *
     * @param  order  integer indicating image order
     */
    public void setImageOrder(int order) {
        imageOrder = order;
    }

    /**
     * Sets the image orientation (sagittal, axial, ...).
     *
     * @param  orient  integer representing the orientation
     */
    public void setImageOrientation(int orient) {

        if (fileInfo != null) {

            for (int i = 0; i < getFileInfo().length; i++) {
                getFileInfo(i).setImageOrientation(orient);
            }
        }
    }

    /**
     * Sets the mask which indicate which pixels/voxels to process.
     *
     * @param  _mask  mask in the form of a BitSet, 1 indicates pixel should be processed 0 indicates pixel should not
     *                be processed
     */
    public void setMask(BitSet _mask) {
        mask = _mask;
    }

    /**
     * Sets the mask which indicate which pixels/voxels to process.
     *
     * @param  mask  mask in the form of a BitSet, 1 indicates pixel should be processed 0 indicates pixel should not be
     *               processed
     */
    public void setMaskBU(BitSet mask) {
        maskBU = mask;
    }

    /**
     * Accessor that sets transformation matrix.
     *
     * @param  matrix  transformation matrix structure.
     */
    public void setMatrix(TransMatrix matrix) {
        this.matrix = matrix;
    }

    /**
     * Sets the slice in all frames displaying this image.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {
            ((ViewImageUpdateInterface) frameList.elementAt(i)).setSlice(slice);
        }
    }

    /**
     * Accessor that sets the talairach transform information.
     *
     * @param  tal  TalairachTransformInfo talairach info
     */
    public void setTalairachTransformInfo(TalairachTransformInfo tal) {
        this.talairach = tal;
    }

    /**
     * Sets the time slice in all frames displaying this image.
     *
     * @param  tSlice  DOCUMENT ME!
     */
    public void setTimeSlice(int tSlice) {

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {
            ((ViewImageUpdateInterface) frameList.elementAt(i)).setTimeSlice(tSlice);
        }
    }

    /**
     * Sets user interface.
     *
     * @param  _UI  reference to user interface
     */
    public void setUserInterface(ViewUserInterface _UI) {
        UI = _UI;
    }

    /**
     * Sets VOI vector for with new VOIs.
     *
     * @param  VOIs  VOIs to image VOIs
     */
    public void setVOIs(VOIVector VOIs) {
        voiVector = new VOIVector();

        int nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {
            voiVector.add(VOIs.VOIAt(i).clone());
        }

        System.gc();
    }

    /**
     * Gives a readable representation of the ModelImage, including file name and extents.
     *
     * @return  the string representation
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

        s += "\n Type: \t\t" + getBufferTypeStr(getType());

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
            s += "\n\t Units:      \t" +
                 FileInfoBase.getUnitsOfMeasureAbbrevStr(getFileInfo()[0].getUnitsOfMeasure()[0]); // possibly expand
                                                                                                   // to see all
                                                                                                   // measurements
        }

        s += "\n\t Orientation:\t" + FileInfoBase.getImageOrientationStr(getImageOrientation());

        for (int i = 0; i < getNDims(); i++) {
            s += "\n\t Axis: " + i + "      \t" + FileInfoBase.getAxisOrientationStr(fileInfo[0].getAxisOrientation(i));
        }

        // s += "\n\t Orientation:\t" + FileInfoBase.getImageOrientationStr(getImageOrientation());
        for (int i = 0; i < getNDims(); i++) {
            s += "\n\t Origin: " + i + " \t" + getFileInfo()[0].getOrigin(i); // possibly expand to see all measurements
        }

        s += "\n\t Pixel Pad:  \t" + getFileInfo()[0].getPixelPadValue();
        s += "\n\t Photometric:\t" + getFileInfo()[0].getPhotometric();
        s += "\n\t Ref:  \t" + super.toString();

        return s;
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
        int i, j, k;
        int nSlices;
        VOI[] newVOI = null;
        VOI[] newPtVOI = null;
        VOI[] newPLineVOI = null;
        VOI[] newLineVOI = null;
        VOI[] newProtractorVOI = null;

        Vector[] contours;
        Point3Df[] point1 = new Point3Df[1];
        int nPoints;
        int nContours;
        Point3Df[] points;
        short id = 0;

        // make a copy of the VOIs stored until the final step
        VOIVector tempVOIs = (VOIVector) voiVector.clone();

        nVOIs = tempVOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if (tempVOIs.VOIAt(i).isActive() == true) {

                if (tempVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;

                    for (j = 0; j < nSlices; j++) {
                        numContours += tempVOIs.VOIAt(i).getCurves()[j].size();
                    }
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;

                    for (j = 0; j < nSlices; j++) {
                        numPoints += tempVOIs.VOIAt(i).exportPoints(j).length;
                    }
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;

                    for (j = 0; j < nSlices; j++) {
                        numPLines += tempVOIs.VOIAt(i).getCurves()[j].size();
                    }
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;

                    for (j = 0; j < nSlices; j++) {
                        numLines += tempVOIs.VOIAt(i).getCurves()[j].size();
                    }
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;

                    for (j = 0; j < nSlices; j++) {
                        numProtractors += tempVOIs.VOIAt(i).getCurves()[j].size();
                    }
                }
            } // if (tempVOIs.VOIAt(i).isActive() == true)
        } // for (i = 0; i < nVOIs; i++)

        if ((numContours == 0) && (numPoints == 0) && (numPLines == 0) && (numLines == 0) && (numProtractors == 0)) {
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
        int slices;

        if (getNDims() > 2) {
            nameExt = new String("3D");
            slices = getExtents()[2];
        } else {
            nameExt = new String("2D");
            slices = 1;
        }

        for (i = 0; i < numContours; i++, id++) {
            newVOI[i] = new VOI(id, "contour" + nameExt + i, slices, VOI.CONTOUR, -1.0f);
        }

        for (i = 0; i < numPoints; i++, id++) {
            newPtVOI[i] = new VOI(id, "point" + nameExt + i, slices, VOI.POINT, -1.0f);
        }

        for (i = 0; i < numPLines; i++, id++) {
            newPLineVOI[i] = new VOI(id, "polyline" + nameExt + i, slices, VOI.POLYLINE, -1.0f);
        }

        for (i = 0; i < numLines; i++, id++) {
            newLineVOI[i] = new VOI(id, "line" + nameExt + i, slices, VOI.LINE, -1.0f);
        }

        for (i = 0; i < numProtractors; i++, id++) {
            newProtractorVOI[i] = new VOI(id, "protractor" + nameExt + i, slices, VOI.PROTRACTOR, -1.0f);
        }

        VOIBase tempBase = null;

        for (i = nVOIs - 1; i >= 0; i--) {

            if (tempVOIs.VOIAt(i).isActive() == true) {

                if (tempVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;
                    contours = tempVOIs.VOIAt(i).getCurves();

                    for (j = 0; j < nSlices; j++) {
                        nContours = contours[j].size();

                        for (k = 0; k < nContours; k++) {
                            tempBase = (VOIBase) contours[j].elementAt(k);
                            tempBase.setName(newVOI[n].getName());
                            newVOI[n].getCurves()[j].addElement(tempBase);
                            n++;
                        }
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;
                    contours = tempVOIs.VOIAt(i).getCurves();

                    for (j = 0; j < nSlices; j++) {
                        nContours = contours[j].size();

                        for (k = 0; k < nContours; k++) {
                            tempBase = (VOIBase) contours[j].elementAt(k);
                            tempBase.setName(newPLineVOI[nPLine].getName());
                            newPLineVOI[nPLine].getCurves()[j].addElement(tempBase);
                            nPLine++;
                        }
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;
                    contours = tempVOIs.VOIAt(i).getCurves();

                    for (j = 0; j < nSlices; j++) {
                        nContours = contours[j].size();

                        for (k = 0; k < nContours; k++) {
                            tempBase = (VOIBase) contours[j].elementAt(k);
                            tempBase.setName(newLineVOI[nLine].getName());
                            newLineVOI[nLine].getCurves()[j].addElement(tempBase);
                            nLine++;
                        }
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;

                    for (j = 0; j < nSlices; j++) {
                        points = tempVOIs.VOIAt(i).exportPoints(j);
                        nPoints = points.length;

                        for (k = 0; k < nPoints; k++) {
                            point1[0] = points[k];
                            newPtVOI[nPt].importCurve(point1, j);
                            nPt++;
                        }
                    }

                    tempVOIs.removeElementAt(i);
                } else if (tempVOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {
                    nSlices = tempVOIs.VOIAt(i).getCurves().length;
                    contours = tempVOIs.VOIAt(i).getCurves();

                    for (j = 0; j < nSlices; j++) {
                        nContours = contours[j].size();

                        for (k = 0; k < nContours; k++) {
                            tempBase = (VOIBase) contours[j].elementAt(k);
                            tempBase.setName(newProtractorVOI[nProtractor].getName());
                            newProtractorVOI[nProtractor].getCurves()[j].addElement(contours[j].elementAt(k));
                            nProtractor++;
                        }
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
            ((VOI) voiVector.elementAt(i)).setID((short) i);
        }

        /**
                System.err.println("\n\nUngrouping DEBUG INFO:");
                VOI tempVOI = null;
                Vector [] tempCurves = null;
                //temp stuff for debugging purposes
                for (i = 0; i < voiVector.size(); i++) {
                    tempVOI = voiVector.VOIAt(i);
                    System.err.println( i + ": VOI name: " + tempVOI.getName());
                    tempCurves = tempVOI.getCurves();
                    for (j = 0; j < tempCurves.length; j++) {
                        System.err.println("\tSize: " + tempCurves[j].size());
                        System.err.println("\t" + tempCurves[j].toString());
                    }
                }
        */


        notifyImageDisplayListeners();
    }

    /**
     * Unregisters all VOIs from this image model.
     */
    public void unregisterAllVOIs() {
        voiVector.removeAllElements();
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
     * @param  voi  Volume of interest (VOI) to be removed from the image model
     */
    public void unregisterVOI(VOI voi) {
        voiVector.removeElement(voi);

        for (int i = 0; i < voiVector.size(); i++) { // ((VOI)(voiVector.elementAt(i))).setID((short)i);
        }
    }

    /**
     * Give the image a new image name, updates frame (if not null), and file infos.
     *
     * @param  newImageName  DOCUMENT ME!
     */
    public void updateFileName(String newImageName) {
        String oldName = getImageName();
        setImageName(newImageName);

        if (UI.isScriptRecording()) {

            if (UI.getScriptDialog().getImgTableVar(oldName) == null) {

                if (UI.getScriptDialog().getActiveImgTableVar(oldName) == null) {
                    UI.getScriptDialog().putActiveVar(getImageName());
                } else {
                    UI.getScriptDialog().changeActiveVar(oldName, newImageName);
                }
            } else {
                UI.getScriptDialog().changeVar(oldName, newImageName);
            }

            String line = "ChangeName " + UI.getScriptDialog().getVar(getImageName()) + " " + newImageName + "\n";
            UI.getScriptDialog().append(line);
        }

        try {
            UI.getFrameContainingImage(this).setTitle();
        } catch (Exception e) { // was not in frame..
        }

        int index;
        String tmpString = "";

        if (getNDims() == 2) {

            if (fileInfo[0].getFileFormat() == FileBase.DICOM) {
                ((FileInfoDicom) (fileInfo[0])).setValue("0010,0010", newImageName.trim(),
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

            if (fileInfo[0].getFileFormat() == FileBase.DICOM) {

                for (int i = 0; i < getExtents()[2]; i++) {
                    ((FileInfoDicom) (fileInfo[i])).setValue("0010,0010", newImageName.trim(),
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

            if (fileInfo[0].getFileFormat() == FileBase.DICOM) {

                for (int i = 0; i < (getExtents()[2] * getExtents()[3]); i++) {
                    ((FileInfoDicom) (fileInfo[i])).setValue("0010,0010", newImageName.trim(),
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

            if (fileInfo[0].getFileFormat() == FileBase.DICOM) {

                for (int i = 0; i < (getExtents()[2] * getExtents()[3] * getExtents()[4]); i++) {
                    ((FileInfoDicom) (fileInfo[i])).setValue("0010,0010", newImageName.trim(),
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
     * @param  xfrm  the transformation maxtrix used to transform the origin
     */
    public void updateImageOrigin(TransMatrix xfrm) {

        FileInfoBase[] fileInfo = getFileInfo();
        float[] imgOrigin = fileInfo[0].getOrigin();

        if (getNDims() == 2) {
            float[] tempOrigin = new float[2];

            xfrm.transform(imgOrigin[0], imgOrigin[1], tempOrigin);
            imgOrigin[0] = tempOrigin[0];
            imgOrigin[1] = tempOrigin[1];
        } else {
            float[] tempOrigin = new float[3];

            xfrm.transform(imgOrigin[0], imgOrigin[1], imgOrigin[2], tempOrigin);
            imgOrigin[0] = tempOrigin[0];
            imgOrigin[1] = tempOrigin[1];
            imgOrigin[2] = tempOrigin[2];
        }

        int direction = 1;
        float startPos = imgOrigin[2];
        int[] axisOrient = new int[3];

        axisOrient = (int[]) fileInfo[0].getAxisOrientation();

        if (getNDims() >= 3) {

            if ((axisOrient[2] == FileInfoBase.ORI_L2R_TYPE) || (axisOrient[2] == FileInfoBase.ORI_A2P_TYPE) ||
                    (axisOrient[2] == FileInfoBase.ORI_S2I_TYPE)) {
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
                    fileInfo[(j * getExtents()[2]) + i].setOrigin(imgOrigin);
                }
            }
        }
    }

    /**
     * Calls disposeLocal of this class to ensure this class nulls the references to global class variables so that
     * memory will be recovered.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    /**
     * Helper method for making the result image's name. Strips the current extension from the original name, adds the
     * given extension, and returns the new name.
     *
     * @param   image_name  DOCUMENT ME!
     * @param   ext         Extension to add which gives information about what algorithm was performed on the image.
     *
     * @return  The new image name.
     */
    private static String makeImageName(String image_name, String ext) {
        String name;
        int index;

        if (image_name != null) {
            index = image_name.lastIndexOf(".");
        } else {
            return null;
        }

        if (index == -1) {
            name = image_name;
        } else {
            name = image_name.substring(0, index);
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
                VOI voi = (VOI) (((Vector) voiVector).remove(j));

                try {
                    voi.finalize();
                } catch (Throwable e) { }

                voi = null;
            } // for (j = i-1; j >= 0; j--)

            voiVector = null;
        } // if ( voiVector != null )

        matrix = null;
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
     * @param  image  source image
     */
    private void fixFileTypeInfo(ModelImage image) {
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
     * Makes the information panes attached to each image.
     */
    private void makeInfoPanes() {


        historyArea = new JTextArea();
        historyArea.setBackground(Color.lightGray);
        historyArea.setEditable(true);
        historyArea.setFont(MipavUtil.font12);
        historyArea.setMargin(new Insets(3, 3, 3, 3));

        historySPane = new JScrollPane(historyArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                       JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        historySPane.setPreferredSize(new Dimension(595, 178));

    }

    /**
     * Accessor that sets the name of the image without registering the image.
     *
     * @param  name  the String representing the filename
     */
    private void setClonedImageName(String name) {

        // The clone method will register the image after the name has been set.
        imageName = name;
    }


    /**
     * Deep copy of important file information to resultant image structure.
     *
     * @param  image        source image
     * @param  resultImage  resultant image
     */
    private void updateFileTypeInfo(ModelImage image, ModelImage resultImage) {
        FileInfoBase fileInfo;

        for (int i = 0; i < image.getFileInfo().length; i++) {
            fileInfo = (FileInfoBase) (image.getFileInfo(i).cloneItself());
            resultImage.setFileInfo(fileInfo, i);
        }

        return;
    }

}
