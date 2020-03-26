package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import java.io.*;

import javax.media.j3d.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * Implementation of Volume Sculpting for the RayCast and ShearWarp Volume Renderers. See Sculptor.java.
 *
 * @author  Alexandra Bokinsky, Ph.D. Under contract from Magic Software.
 * @see     ViewJFrameVolumeView
 * @see     RayCastVolumeRenderer
 * @see     ShearWarpVolumeRenderer
 */
public class VolumeSculptor extends Sculptor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected boolean m_bShear = false;

    /** The volume renderer that is currently rendering the volume data:. */
    protected VolumeRenderer m_kVolumeRenderer;

    /** References to the sculpt image. */
    private ModelImage kImageAref = null;

    /** DOCUMENT ME! */
    private ModelImage kImageBref = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor:
     *
     * @param  kVolumeRenderer  reference to current renderer
     * @param  iSculptWidth     Canvas Width
     * @param  iSculptHeight    Canvas Height
     */
    public VolumeSculptor(VolumeRenderer kVolumeRenderer, int iSculptWidth, int iSculptHeight) {
        m_kVolumeRenderer = kVolumeRenderer;
        m_bShear = kVolumeRenderer instanceof VolumeRendererShearWarp;
        m_kCanvas3D = kVolumeRenderer.getCanvas();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * applySculpt: called by ViewJFrameVolumeView when the user presses the "Apply Sculpt" button. This function has
     * several steps: 1. Getting the ModelImage volume data and correctly calculating the center of the volume and the
     * spacing of the volume. 2. Calculating the viewing Transformations. 3. Determining which voxels in the volume fall
     * within the sculpt region drawn on the screen, and setting those values to the minimum voxel value.
     *
     * @return  DOCUMENT ME!
     */
    public boolean applySculpt() {

        /* Disable drawing the sculpt region outline: */
        m_bSculptEnabled = false;

        boolean bVolumeChanged = false;

        if (m_kVolumeRenderer == null) {
            return false;
        }

        if (m_bSculptDrawn == false) {
            return false;
        }

        /* Get the current width of the Canvas, which may have changed. The
         * width of the canvas is used to set the field of view for the
         * displayed image. */
        int iCanvasWidth = m_kCanvas3D.getWidth();

        /*
         * 1. Access the volume data and calculate the bounds of the data, center, and spacing parameters:
         */

        /* The ModelImages that will be changed by the sculpturing: */
        kImageAref = null;
        kImageBref = null;

        kImageAref = m_kVolumeRenderer.getImageA();
        kImageBref = m_kVolumeRenderer.getImageB();

        /* The size of the ModelImage for indexing: */
        int iXBound = kImageAref.getExtents()[0];
        int iYBound = kImageAref.getExtents()[1];
        int iZBound = kImageAref.getExtents()[2];
        int iSliceSize = iXBound * iYBound;

        /* Half the sizes of the ModelImage volume, used to translate the
         * volume so that the center of the volume is at the origin: */
        float fXHalf = (float) ((iXBound - 1) / 2.0);
        float fYHalf = (float) ((iYBound - 1) / 2.0);
        float fZHalf = (float) ((iZBound - 1) / 2.0);

        /* The following is used to calculate the "stretch" or "compression"
         * of the volume in 3Space. The ModelImage volume slices may be spaced differently than is represented by
         * indexing into the ModelImage volume array -- an implied spacing of 1,1,1 in each of the x,y,z directions. The
         * actual spacing may be different in any of the directions, and that spacing is important in the raytracing
         * step for producing a correctly rendered volume. The values must be reproduced here, since the rotation of the
         * volume is reversed before projecting the volume onto the 2D Sculpt Image for
         * sculpturing. */
        float fXDelta = kImageAref.getFileInfo(0).getResolutions()[0];
        float fYDelta = kImageAref.getFileInfo(0).getResolutions()[1];
        float fZDelta = kImageAref.getFileInfo(0).getResolutions()[2];

        if ((fXDelta <= 0.0f) || (fYDelta <= 0.0f) || (fZDelta <= 0.0f)) {
            fXDelta = 1.0f;
            fYDelta = 1.0f;
            fZDelta = 1.0f;
        }

        float fMinRes = fXDelta;

        if (fYDelta < fMinRes) {
            fMinRes = fYDelta;
        }

        if (fZDelta < fMinRes) {
            fMinRes = fZDelta;
        }

        fXDelta = fXDelta / fMinRes;
        fYDelta = fYDelta / fMinRes;
        fZDelta = fZDelta / fMinRes;

        /* Final Delta values used to correctly project the volume onto the 2D
         * Sculpt Image: */
        float fInvXDelta = 1.0f / fXDelta;
        float fInvYDelta = 1.0f / fYDelta;
        float fInvZDelta = 1.0f / fZDelta;

        /* When the volume is sculpted, the removed voxels are set to the
         * lowest (zero-value) in the volume. This step calculates the minimum
         * value in the ModelImage volume data:*/
        calculateMinMaxValues(kImageAref, kImageBref);

        /*
         * 2. Get the viewing transformations.
         *
         * Rotation is applied to the volume as a whole, and is performed in the raytracing step. The rotation matrix is
         * retrieved from the ViewJComponentRayCastRenderImage componentImageXY in the VolumeRenderer.
         *
         * Once the volume is raytraced, the 2D image is texture-mapped onto a 2D polygon and displayed in 3D world
         * coordinates. Translation and Zoom are preformed by translating the polygon in the x,y, or z-directions (for
         * zoom). These values are retrieved from the TransformGroup objTransXY in the VolumeRenderer.
         */

        /* Get the componentImageXY from the VolumeRenderer. The
         * componentImageXY is used to access the raytracer which renders the
         * volume as well as the current rotation matrix for the volume:  */
        ViewJComponentRenderImage kRenderImageXY = null;

        kRenderImageXY = (ViewJComponentRenderImage) (m_kVolumeRenderer.getComponentImageXY());

        if (kRenderImageXY == null) {
            return false;
        }

        /* Setup the inverse of the rotation matrix, to unrotate the
         * volume. Rotation is preformed in the raytracer, before raytracing the volume. The endresult of the raytraced
         * volume is a 2D texture, which is texture-mapped onto a 2D polygon in world space. Translation and Zoom
         * (scale) are performed as operations on
         * the 2D polygon and are separate from the raytracing step: */
        Matrix3d kRotateMatrix = null;

        kRotateMatrix = new Matrix3d(kRenderImageXY.getRayTracerA().getAxis(0).x,
                                     kRenderImageXY.getRayTracerA().getAxis(0).y,
                                     kRenderImageXY.getRayTracerA().getAxis(0).z,




            kRenderImageXY.getRayTracerA().getAxis(1).x, kRenderImageXY.getRayTracerA().getAxis(1).y,
                                     kRenderImageXY.getRayTracerA().getAxis(1).z,
                                     kRenderImageXY.getRayTracerA().getAxis(2).x,
                                     kRenderImageXY.getRayTracerA().getAxis(2).y,
                                     kRenderImageXY.getRayTracerA().getAxis(2).z);
        kRotateMatrix.invert();

        /* Zooming and Translating is done with the TransformGroup
         * objTransXY. Both are done after the volume is raytraced, and the operations are performed on a texture-mapped
         * polygon in world-space. matrixZoomTranslate is the final value used in this
         * function:  */
        TransformGroup kObjTransXY = null;

        kObjTransXY = m_kVolumeRenderer.getObjTransXY();

        Transform3D kTransform = new Transform3D();

        kObjTransXY.getTransform(kTransform);

        /* Get the zoom-translate matrix from the transform: */
        Matrix4d kMatrixZoomTranslate = new Matrix4d();

        kTransform.get(kMatrixZoomTranslate);

        /*
         * Zooming is done as a translation in Z, the value is an ofset from the original Z-position, which is stored in
         * the TransformGroup setup in the simple universe. This step gets that transform, and copies the final value
         * into matrixNormalizedCoords.
         */
        TransformGroup kTransGroup = null;

        kTransGroup = m_kVolumeRenderer.getUniverse().getViewingPlatform().getViewPlatformTransform();

        kTransGroup.getTransform(kTransform);

        Matrix4d kZoomMatrix = new Matrix4d();

        kTransform.get(kZoomMatrix);

        /* float fZoom = kRenderImageXY.getRayTracerA().getZoom();
         */

        /* fImageScale represents the scaling that is done by the raytracer. */
        float fImageScale = 1.0f;

        fImageScale = kRenderImageXY.getRayTracerA().getExtreme() /
                          (float) (m_kVolumeRenderer.getImageComponent().getWidth() / 2.0);

        fImageScale *= (float) iCanvasWidth / (float) m_iOriginalWidth;

        /* fTexScale represents the scaling that occurs when the 2D image
         * produced by the raytracer is texture mapped onto the polygon. Since all textures are mapped onto a square of
         * size 2, larger textures
         * are "shrunk" to fit and smaller textures are "stretched" to fit. */
        float fTexScale = (float) (iXBound) / 256.0f;

        /* fZoomScale represents zooming, which is done as a relative
         * translation of the polygon in Z, relative to the original translation, which is stored in kZoomMatrix.m23.
         * The amount of scale is proportional to the change in the z-distance to
         * the polygon. */
        double fZoomScale = kZoomMatrix.m23 / (kZoomMatrix.m23 - kMatrixZoomTranslate.m23);

        ;

        /* Calculate the overall scale, so that the multiplies and divides are
         * done once per volume: */
        float fScale = (float) (fImageScale * fZoomScale / fTexScale);

        /* Translation amounts are in image space: */
        float fXTranslate = (float) (kMatrixZoomTranslate.m03 * fZoomScale * (float) iCanvasWidth / 2.0f);
        float fYTranslate = (float) (kMatrixZoomTranslate.m13 * fZoomScale * (float) iCanvasWidth / 2.0f);

        /* Perform the actual sculpting step: */

        /* Input and output points for the rotation step */
        Point3d kInput3 = new Point3d();
        Point3d kOutput3 = new Point3d();

        /* Step over every voxel in the volume, transforming the voxel into
         * image space: */
        int iMod = iYBound / 10;

        for (int iY = 0; iY < iYBound; iY++) {
            int iIndexY = iY * iXBound;

            /* Update progress bar: */
            if (((iY % iMod) == 0) || (iY == (iYBound - 1))) {

                if (iY == (iYBound - 1)) {
                    m_kProgress.setValue(100);
                } else {
                    m_kProgress.setValue(Math.round((float) (iY) / (iYBound - 1) * 100));
                    m_kProgress.update(m_kProgress.getGraphics());
                }
            }

            for (int iX = 0; iX < iXBound; iX++) {

                for (int iZ = 0; iZ < iZBound; iZ++) {

                    /* Translate the volume so that the origin is at the
                     * center of the volume: */
                    if (m_bShear) {
                        fInvXDelta = 1;
                        fInvYDelta = 1;
                        fInvZDelta = 1;
                    }

                    kInput3.x = (iX - fXHalf) / fInvXDelta;
                    kInput3.y = (iY - fYHalf) / fInvYDelta;
                    kInput3.z = (iZ - fZHalf) / fInvZDelta;

                    /* Rotate the volume, according to the trackballl
                     * rotation: */
                    kRotateMatrix.transform(kInput3, kOutput3);

                    /* Scale: */
                    kOutput3.x *= fScale;
                    kOutput3.y *= fScale;

                    /* Translate */
                    if (!m_bShear) {
                        kOutput3.x -= fXTranslate;
                    } else {
                        kOutput3.x += fXTranslate;
                    }

                    kOutput3.y -= fYTranslate;

                    int iIndexZ = iZ * iSliceSize;
                    int iIndex = iIndexZ + iIndexY + iX;

                    /* Calculate the index into the 2D Sculpt Image: */
                    int iXIndex = 0;

                    if (!m_bShear) {
                        iXIndex = (int) ((m_iSculptImageWidth / 2) - kOutput3.x);
                    } else {
                        iXIndex = (int) (kOutput3.x + ((m_iSculptImageWidth) / 2));
                    }

                    int iYIndex = (int) (kOutput3.y + ((m_iSculptImageHeight) / 2));

                    /* If the index is inside the sculpt image: */
                    if ((iXIndex >= 0) && (iXIndex < m_iSculptImageWidth) && (iYIndex >= 0) &&
                            (iYIndex < m_iSculptImageHeight)) {

                        /* If the voxel index falls inside the sculpt
                         * region: */
                        if (m_kSculptImageOpaque.getRGB(iXIndex, iYIndex) == m_iColorSculpt) {
                            sculptImage(kImageAref, kImageBref, iIndex);
                            bVolumeChanged = true;
                        }
                    }
                }
            }
        }

        kRotateMatrix = null;
        kMatrixZoomTranslate = null;
        kZoomMatrix = null;
        kInput3 = null;
        kOutput3 = null;

        return bVolumeChanged;
    }

    /**
     * clearSculpt: called by ViewJFrameVolumeView when the user presses the "Clear Ouline" button, clearing the sculpt
     * outline from the canvas image. The function disables sculpting and reactivates the mouse events for the
     * m_kVolumeRenderer.
     */
    public void clearSculpt() {
        super.clearSculpt();

        if (m_kVolumeRenderer != null) {
            m_kVolumeRenderer.setEnableMouseBehaviors(true);
            m_kVolumeRenderer.updateImage();
        }
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  DOCUMENT ME!
     */
    public void disposeLocal(boolean flag) {
        m_kSculptImage = null;
        m_kSculptImageOpaque = null;
        m_aiImageA_backup = null;
        m_aiImageB_backup = null;
        m_aiXPoints = null;
        m_aiYPoints = null;

        m_kVolumeRenderer = null;
        kImageAref = null;
        kImageBref = null;
    }

    /**
     * enableSculpt: called by the ViewJFrameVolumeView object when the Draw Sculpt button is pressed. This function
     * deactivates the m_kVolumeRenderer's mouse response, so the mouse can be used to draw the sculpt outline.
     *
     * @param  bEnabled  DOCUMENT ME!
     */
    public void enableSculpt(boolean bEnabled) {
        super.enableSculpt(bEnabled);

        if (m_kVolumeRenderer != null) {
            m_kVolumeRenderer.setEnableMouseBehaviors(!m_bSculptEnabled);
            backupImage(m_kVolumeRenderer.getImageA(), m_kVolumeRenderer.getImageB());
        }
    }


    /**
     * Calls disposeLocal.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        this.disposeLocal(false);
        super.finalize();
    }

    /**
     * Creates save dialog so that the image can be saved // This should be moved to imageModel.save();
     *
     * @param  options     File-write options.
     * @param  filterType  only used if >= 0
     */
    public void save(FileWriteOptions options, int filterType) {
        String fileName = null;
        String extension = null;
        String directory = null;
        String suffix = null;
        int fileType = FileUtility.UNDEFINED;
        ModelImage img = null;
        ViewImageFileFilter vFilter = null;

        int i;

        if (kImageAref != null) {
            img = kImageAref;
        } else if (kImageBref != null) {
            img = kImageBref;
        } else {
            return;
        }

        if (options.isSaveAs()) {

            // save into its own subdirectory when on SaveAs.
            // (preferrably used in multi-file formats., ie DICOM)
            options.setSaveInSubdirectory(true);

            if (options.isSet()) {
                fileName = options.getFileName();
                directory = options.getFileDirectory();
            } else {

                try {
                    ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, true);

                
                        JFileChooser chooser = fileChooser.getFileChooser();

                        // chooser.setName("Save image as");
                        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                        } else {
                            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                        }

                        if (filterType >= 0) {
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(filterType));
                        } else {
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                        }

                        int returnVal = chooser.showSaveDialog(m_kVolumeRenderer);

                        if (returnVal == JFileChooser.APPROVE_OPTION) {
                            fileName = chooser.getSelectedFile().getName();

                            if (filterType >= 0) {
                                i = fileName.lastIndexOf('.');

                                if ((i > 0) && (i < (fileName.length() - 1))) {
                                    extension = fileName.substring(i + 1).toLowerCase();
                                    vFilter = new ViewImageFileFilter(filterType);

                                    if (!vFilter.accept(extension)) {
                                        MipavUtil.displayError("Extension does not match filter type");

                                        return;
                                    }
                                } // if ( i > 0 && i < fileName.length() - 1 )
                                else if (i < 0) {

                                    switch (filterType) {

                                        case ViewImageFileFilter.AVI:
                                            fileName = fileName + ".avi";
                                            break;

                                        case ViewImageFileFilter.VOI:
                                            fileName = fileName + ".voi";
                                            break;

                                        case ViewImageFileFilter.FUNCT:
                                            fileName = fileName + ".fun";
                                            break;

                                        case ViewImageFileFilter.LUT:
                                            fileName = fileName + ".lut";
                                            break;

                                        case ViewImageFileFilter.PLOT:
                                            fileName = fileName + ".plt";
                                            break;

                                        case ViewImageFileFilter.CLASS:
                                            fileName = fileName + ".class";
                                            break;

                                        case ViewImageFileFilter.SCRIPT:
                                            fileName = fileName + ".sct";
                                            break;

                                        case ViewImageFileFilter.SURFACE:
                                            fileName = fileName + ".sur";
                                            break;

                                        case ViewImageFileFilter.FREESURFER:
                                            fileName = fileName + ".asc";
                                            break;
                                    }
                                } // else if (i < 0)
                            } // if (filterType >= 0)

                            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                            ViewUserInterface.getReference().setDefaultDirectory(directory);
                        } else {
                            return;
                        }
                    
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                    return;
                }
            }

        } else {
            fileName = img.getFileInfo(0).getFileName();
            directory = img.getFileInfo(0).getFileDirectory();
        }

        /*
         * I'm not sure why this wasn't done before.... if we do a save-as we should also update the name of the file
         */
        // if (options.isSaveAs()) {
        // img.setImageName(fileName.substring(0, fileName.length()-4));
        // }

        options.setFileName(fileName);
        options.setFileDirectory(directory);

        if (!options.isSaveAs()) {

            if (img.getNDims() == 3) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
            } else if (img.getNDims() == 4) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
                options.setBeginTime(0);
                options.setEndTime(img.getExtents()[3] - 1);
            }
        }

        if (fileName != null) {
            FileIO fileIO = new FileIO();

            fileIO.writeImage(img, options);
        }

        // if the SaveAllOnSave preference flag is set, then
        // save all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

            // Since the options may have changed the filename
            // and the directory --- get new fileName and directory
            // from options
            String fName = options.getFileName(); // if you use the name from img, then DICOM has funny names
            //String dirName = img.getFileInfo(0).getFileDirectory();
            String filebase;
            int ind = fName.lastIndexOf(".");

            if (ind > 0) {
                filebase = fName.substring(0, fName.lastIndexOf("."));
            } else {
                filebase = new String(fName);
            }

            if (options.getFileType() == FileUtility.DICOM) {
                int newIndex = filebase.length();

                for (i = filebase.length() - 1; i >= 0; i--) {
                    char myChar = filebase.charAt(i);

                    if (Character.isDigit(myChar)) {
                        newIndex = i;
                    } else {
                        break;
                    } // as soon as something is NOT a digit, leave loop
                }

                if (newIndex > 0) {
                    filebase = filebase.substring(0, newIndex);
                }
            }

        }

        // set the new fileName and directory in the fileInfo for the img -- so that it's
        // updated correctly in memory as well -- don't move this before the saveAllOnSave loop --
        // that needs to look at the former settings!
        FileInfoBase[] fileInfo = img.getFileInfo();

        if (suffix == null) {
            suffix = FileUtility.getExtension(fileName);
            fileType = FileUtility.getFileType(fileName, directory, false);
        }

        // now, get rid of any numbers at the end of the name (these
        // are part of the dicom file name, but we only want the 'base'
        // part of the name
        String baseName = new String(fileName);

        if (fileType == FileUtility.DICOM) {
            int index = fileName.lastIndexOf(".");

            if (index > 0) {
                baseName = fileName.substring(0, index);
            }

            int newIndex = baseName.length();

            for (i = baseName.length() - 1; i >= 0; i--) {
                char myChar = baseName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex > 0) {
                baseName = baseName.substring(0, newIndex);
            }

            fileName = new String(baseName + ".dcm");

            if (!directory.endsWith(baseName)) {
                directory = new String(directory + baseName + File.separator);
            }
        }

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setFileDirectory(directory);

            if (fileType == FileUtility.DICOM) {
                fileInfo[i].setFileName(baseName + (i + 1) + ".dcm");
            } else {
                fileInfo[i].setFileName(fileName);
            }

            fileInfo[i].setFileSuffix(suffix);
            // fileInfo[i].setFileFormat (fileType);
        }

    }

    /**
     * undoSculpt: called by the ViewJFrameVolumeView object when the user presses the "Undo Sculpt" button. It resets
     * the volume data back to the original values, using the data stored in the m_aiImage_backup data members.
     */
    public void undoSculpt() {

        /* The RendererImage component from the VolumeRenderer */
        //ViewJComponentRenderImage kRenderImageXY = null;

        /* The original ModelImages */
        ModelImage kImageAref = null;
        ModelImage kImageBref = null;

        if (m_kVolumeRenderer != null) {
            //kRenderImageXY = (ViewJComponentRenderImage) (m_kVolumeRenderer.getComponentImageXY());
            kImageAref = m_kVolumeRenderer.getImageA();
            kImageBref = m_kVolumeRenderer.getImageB();
        }

        undoSculpt(kImageAref, kImageBref);
    }

    /**
     * Update the underlying volume and rerender. This function is called when the volume has changed.
     */
    public void update() {
        m_bSculptEnabled = false;

        if (m_kVolumeRenderer == null) {
            return;
        }

        /* Get the componentImageXY from the VolumeRenderer. The
         * componentImageXY is used to access the raytracer which renders the
         * volume as well as the current rotation matrix for the volume:  */
        ViewJComponentRenderImage kRenderImageXY = null;

        kRenderImageXY = (ViewJComponentRenderImage) (m_kVolumeRenderer.getComponentImageXY());

        if (kRenderImageXY == null) {
            return;
        }

        updateRenderedVolume(kRenderImageXY);
    }

    /**
     * This sequence causes the m_kVolumeRenderer to rerender the volume data, from the current ModelImage, not the data
     * that is stored in the Renderer.
     *
     * @param  kRenderImageXY  DOCUMENT ME!
     */
    private void updateRenderedVolume(ViewJComponentRenderImage kRenderImageXY) {
        kRenderImageXY.getRayTracerA().reloadInputData(true);
        kRenderImageXY.show(0, null, null, true, true);
        m_kVolumeRenderer.getImageComponent().set(kRenderImageXY.getImage());
        kRenderImageXY.getRayTracerA().reloadInputData(false);

        /* Reactivate mouse behavior in the m_kVolumeRenderer */
        m_kVolumeRenderer.setEnableMouseBehaviors(true);
    }
}
