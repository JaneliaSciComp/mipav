package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import java.io.*;

import javax.media.j3d.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * Volume Sculpting for the VolumeTexture Rendering. See Sculptor.java
 *
 * @author  Alexandra Bokinsky, Ph.D. Under contract from Magic Software.
 * @see     ViewJFrameVolumeView
 * @see     RayCastVolumeRenderer
 * @see     ShearWarpVolumeRenderer
 */
public class TextureSculptor extends Sculptor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** References to the sculpt images. */
    private ModelImage kImageAref = null;

    /** DOCUMENT ME! */
    private ModelImage kImageBref = null;

    /** Reference to the SurfaceRender containing the TextureVolume:. */
    private SurfaceRender m_kSurface = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor:
     *
     * @param  kSurface  DOCUMENT ME!
     */
    public TextureSculptor(SurfaceRender kSurface) {
        m_kSurface = kSurface;
        m_kCanvas3D = kSurface.getCanvas();
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

        /* If the Surface is not defined, return: */
        if (m_kSurface == null) {
            return false;
        }

        if (m_bSculptDrawn == false) {
            return false;
        }

        /* If no change is made to the volume, then don't update the
         * SurfaceRender*/
        boolean bVolumeChanged = false;

        /* Get the current width of the Canvas, which may have changed. The
         * width of the canvas is used to set the field of view for the
         * displayed image. */
        int iCanvasWidth = m_kCanvas3D.getWidth();
        //int iCanvasHeight = m_kCanvas3D.getHeight();

        /*
         * 1. Access the volume data and calculate the bounds of the data,   center, and spacing parameters:
         */
        /* The ModelImages that will be changed by the sculpturing: */
        kImageAref = null;
        kImageBref = null;

        kImageAref = m_kSurface.getImageA();
        kImageBref = m_kSurface.getImageB();

        /* The size of the ModelImage for indexing: */
        int iXBound = kImageAref.getExtents()[0];
        int iYBound = kImageAref.getExtents()[1];
        int iZBound = kImageAref.getExtents()[2];
        int iSliceSize = iXBound * iYBound;
        //int iImageSize = iZBound * iYBound * iXBound;

        /* Get the minimum and maximum intensity and color ARGB values: */
        calculateMinMaxValues(kImageAref, kImageBref);

        /*
         * 2. Get the viewing transformations.
         *
         */
        /* Modeling transformations: */
        TransformGroup kSceneTransform = m_kSurface.getSceneRootTG();
        Transform3D kTransformRotate = new Transform3D();
        kSceneTransform.getTransform(kTransformRotate);

        /* The volume is displayed back to front: */
        Transform3D kPreTransformRotate = new Transform3D();
        kPreTransformRotate.setRotation(new AxisAngle4d(0, 1, 0, Math.PI));

        Matrix4d kMatrixZoomTranslate = new Matrix4d();
        kTransformRotate.get(kMatrixZoomTranslate);
        kTransformRotate.setTranslation(new Vector3f(0, 0, 0));

        /* ScreenScale transformations: */
        View kView = m_kSurface.getUniverse().getViewer().getView();
        double dFov = kView.getFieldOfView();
        double dScreenScale = kView.getScreenScale();
        int iProject = kView.getProjectionPolicy();

        /* fImageScale and fZoomScale represents zooming, which is done as a
         * relative scale in screen space -- in the View.screenscale variable. The scale is relative to the original
         * scale, which is stored in m_kSurface.getOriginalScreenScale. The amount of scale is
         * proportional to the change in the View.screenscale variable. */
        float fImageScale = 1.0f;
        float fZoomScale = 1.0f;

        if (iProject == View.PARALLEL_PROJECTION) {
            fZoomScale = (float) (dScreenScale / m_kSurface.getOriginalScreenScale());
            fZoomScale /= dFov;
        }
        /* Zooming in perspective project depends on the object transform, and
         * on the iCanvasWidth if the WindowResizePolicy is VIRTUAL_WORLD: */
        else {

            /* Get the zoom-translate matrix from the transform: */
            TransformGroup kTransGroup = null;
            kTransGroup = m_kSurface.getUniverse().getViewingPlatform().getViewPlatformTransform();

            Transform3D kTransform = new Transform3D();
            kTransGroup.getTransform(kTransform);

            Matrix4d kZoomMatrix = new Matrix4d();
            kTransform.get(kZoomMatrix);

            int iResize = kView.getWindowResizePolicy();

            if (iResize == View.VIRTUAL_WORLD) {
                fImageScale = (float) iCanvasWidth / (float) m_iOriginalWidth;
            }

            fZoomScale = (float) (kZoomMatrix.m23 / (kZoomMatrix.m23 - kMatrixZoomTranslate.m23));
            fZoomScale /= 2.0f;

            kTransform = null;
            kZoomMatrix = null;
        }


        /* Calculate the overall scale, so that the multiplies and divides are
         * done once per volume: */
        float fScale = (float) (fImageScale * fZoomScale);

        /* Translation amounts are in image space: */
        float fXTranslate = (float) (kMatrixZoomTranslate.m03 * fScale * (float) iCanvasWidth);
        float fYTranslate = (float) (kMatrixZoomTranslate.m13 * fScale * (float) iCanvasWidth);


        /* Texture mapping functions to map voxels onto the rendered
         * slices: */
        Vector4f kCoordMapX = m_kSurface.getVolumeTexture().getCoordMapX();
        Vector4f kCoordMapY = m_kSurface.getVolumeTexture().getCoordMapY();
        Vector4f kCoordMapZ = m_kSurface.getVolumeTexture().getCoordMapZ();

        /* Step through the voxels in the volume, transform the voxels from
         * voxel space to modle space, transform to screen space and test
         * against the sculpt image: */
        Point3d kPoint = new Point3d();

        for (int iZ = 0; iZ < iZBound; iZ++) {

            for (int iX = 0; iX < iXBound; iX++) {

                for (int iY = 0; iY < iYBound; iY++) {
                    int iIndex = (int) (iZ * iSliceSize) + (iY * iXBound) + iX;

                    // Voxel to model coordinates:
                    double x = ((((double) iX / (double) (iXBound - 1)) - kCoordMapX.w) / kCoordMapX.x);
                    double y = ((((double) iY / (double) (iYBound - 1)) - kCoordMapY.w) / (kCoordMapY.y));
                    double z = ((((double) iZ / (double) (iZBound - 1)) - kCoordMapZ.w) / (kCoordMapZ.z));
                    kPoint.x = x;
                    kPoint.y = y;
                    kPoint.z = z;

                    // Model transformation:
                    kPreTransformRotate.transform(kPoint);
                    kTransformRotate.transform(kPoint);

                    // Model coordinates to Screen Space (pixels)
                    /* Scale: */
                    kPoint.x *= fScale;
                    kPoint.y *= fScale;

                    // Screen space to buffer space: (should be same)
                    int iScreenX = (int) (m_iSculptImageWidth -
                                          ((kPoint.x * m_iSculptImageWidth) + (m_iSculptImageWidth / 2)));
                    int iScreenY = (int) (((kPoint.y * m_iSculptImageWidth) + (m_iSculptImageHeight / 2)));

                    // Translate:
                    iScreenX += fXTranslate;
                    iScreenY -= fYTranslate;

                    /* If the index is inside the sculpt image: */
                    if ((iScreenX >= 0) && (iScreenX < m_iSculptImageWidth) && (iScreenY >= 0) &&
                            (iScreenY < m_iSculptImageHeight)) {

                        /* If the voxel index falls inside the sculpt
                         * region: */
                        if (m_kSculptImageOpaque.getRGB(iScreenX, iScreenY) == m_iColorSculpt) {

                            /* Testing, sculpt 101x101 square from center of
                             * screen: */
                            /*
                             * if ( (iScreenX > ( m_iSculptImageWidth/2 - 50)) &&  (iScreenX < ( m_iSculptImageWidth/2 +
                             * 50)) &&  (iScreenY > ( m_iSculptImageHeight/2 - 50)) &&  (iScreenY < (
                             * m_iSculptImageHeight/2 + 50)) ){
                             */
                            bVolumeChanged = true;
                            sculptImage(kImageAref, kImageBref, iIndex);
                        }
                    }
                }
            }
        }

        kTransformRotate = null;
        kPreTransformRotate = null;
        kMatrixZoomTranslate = null;
        kPoint = null;
        m_kSurface.setMouseRotateEnable(true);

        return bVolumeChanged;
    }

    /**
     * clearSculpt: called by ViewJFrameVolumeView when the user presses the "Clear Ouline" button, clearing the sculpt
     * outline from the canvas image. The function disables sculpting and reactivates the mouse events for the
     * m_kSurfaceRenderer.
     */
    public void clearSculpt() {
        super.clearSculpt();

        if (m_kSurface != null) {
            m_kSurface.setMouseRotateEnable(true);
            m_kSurface.updateImages(true);
        }

    }

    /**
     * enableSculpt: called by the ViewJFrameVolumeView object when the Draw Sculpt button is pressed. This function
     * deactivates the m_kSurfaceRender's mouse response, so the mouse can be used to draw the sculpt outline.
     *
     * @param  bEnabled  DOCUMENT ME!
     */
    public void enableSculpt(boolean bEnabled) {
        super.enableSculpt(bEnabled);

        if (m_kSurface != null) {
            m_kSurface.setMouseRotateEnable(!m_bSculptEnabled);
            backupImage(m_kSurface.getImageA(), m_kSurface.getImageB());
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        m_kSculptImage = null;
        m_kSculptImageOpaque = null;
        m_aiImageA_backup = null;
        m_aiImageB_backup = null;
        m_aiXPoints = null;
        m_aiYPoints = null;
    }

    /**
     * Creates save dialog so that the image can be saved // This should be moved to imageModel.save();
     *
     * @param   options     File-write options.
     * @param   filterType  only used if >= 0
     *
     * @return  DOCUMENT ME!
     */
    public boolean save(FileWriteOptions options, int filterType) {
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
            return false;
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

                        int returnVal = chooser.showSaveDialog(m_kSurface);

                        if (returnVal == JFileChooser.APPROVE_OPTION) {
                            fileName = chooser.getSelectedFile().getName();

                            if (filterType >= 0) {
                                i = fileName.lastIndexOf('.');

                                if ((i > 0) && (i < (fileName.length() - 1))) {
                                    extension = fileName.substring(i + 1).toLowerCase();
                                    vFilter = new ViewImageFileFilter(filterType);

                                    if (!vFilter.accept(extension)) {
                                        MipavUtil.displayError("Extension does not match filter type");

                                        return false;
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
                            return false;
                        }
                   
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                    return false;
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

            // save any luts
            //String lutName = new String(filebase + ".lut");
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

        return true;
    }

    /**
     * undoSculpt: called by the ViewJFrameVolumeView object when the user presses the "Undo Sculpt" button. It resets
     * the volume data back to the original values, using the data stored in the m_aiImage_backup data members.
     */
    public void undoSculpt() {

        if (m_kSurface == null) {
            return;
        }

        super.undoSculpt(m_kSurface.getImageA(), m_kSurface.getImageB());
    }

    /**
     * Update the underlying volume and rerender. This function is called when the volume has changed.
     */
    public void update() {
        m_bSculptEnabled = false;
        m_bSculptDrawn = false;

        if (m_kSurface != null) {
            m_kSurface.updateImages(true);
            m_kSurface.setMouseRotateEnable(true);
        }
    }

}
