package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.*;

import java.io.*;
import java.util.Vector;

import WildMagic.LibFoundation.Meshes.VETMesh;
import WildMagic.LibGraphics.Detail.*;
import WildMagic.LibGraphics.SceneGraph.*;


/**
 * Extracts a surface using Marching Cube Extraction. Triangle decimation can be invoked to reduce triangle count. The
 * decimation algorithm produces a continuous level of detail (clod) structure that can be used to optimize the the
 * visualization of the surface. The input to this algorithm is typically a mask image where 0 = background and 100 =
 * object (i.e. interior to a VOI). The mask image is then blurred slightly and the level (50) is extracted. A greyscale
 * image may also be input and a surface is extracted given a level. The steps are:
 * 
 * <ol>
 * <li>Build mask image of VOI (i.e. all point interior to VOI are set to 100. All points exterior are = 0.</li>
 * <li>Blur mask image if not grey-scale</li>
 * <li>Extract level surface at 50 or user defined level</li>
 * <li>Save surface ( ".sur")</li>
 * <li>If decimate then decimate surface and save (".sur")</li>
 * </ol>
 * 
 * @version 0.1 June, 2001
 * @author Matthew J. McAuliffe, Ph.D.
 * @author David H. Eberly, Ph.D. wrote all the extraction and decimation code found in the SurfaceExtraction,
 *         SurfaceDecimation and associated classes, with a little input from Matt with speed and memory optimizations
 * @see ModelSurfaceExtractor
 * @see ModelSurfaceDecimator
 */
public class AlgorithmExtractSurfaceCubes extends AlgorithmBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Extract surface from VOI. */
    public static final int VOI_MODE = 0;

    /** Extract surface from mask image. */
    public static final int MASK_MODE = 1;

    /** Extract surface based on intensity level. */
    public static final int LEVEL_MODE = 2;

    /**
     * Do not perform triangle consistency checking - all counter clockwise or all clockwise.
     */
    public static final int NONE_MODE = 0;

    /** Use adjacency model to perform triangle consistency. */
    public static final int ADJ_MODE = 1;

    /** Use smoothing model to perform triangle consistency. Smooths normals. */
    public static final int SMOOTH_MODE = 2;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** If true then the input image is blurred slightly. */
    private final boolean blurFlag;

    /** The amount to blur to smooth surface. */
    private float blurSigma = 0.5f;

    /**
     * If true then the extracted surface is decimated into a continuous level of detail surface (clod).
     */
    private final boolean decimateFlag;

    /** Indicates level surface to be extracted. */
    private final int level;

    /** Mask image to extract surface from. */
    private ModelImage maskImage;

    /** Indicates mode - VOI, LEVELSET, or MASK. */
    private final int mode;

    /**
     * Path and name of extracted surface file. ".sur" will be appended if necessary.
     */
    private String surfaceFileName;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmExtractSurfaceCubes object.
     * 
     * @param image mask image or gray-scale where a level surface is to be extracted
     * @param level indicates level surface to be extracted
     * @param mode Indicates mode - VOI, LEVELSET, or MASK.
     * @param decFlag indicates whether or not the decimation into a CLOD should take place.
     * @param blurFlag if true then the input image is blurred slightly
     * @param sigma the amount to blur the image
     * @param fileName path and name of extracted surface file. ".sur" will be appended if necessary.
     * @param smoothFlag whether the generated mesh should have smoothing applied to it
     */
    public AlgorithmExtractSurfaceCubes(final ModelImage image, final int level, final int mode, final boolean decFlag,
            final boolean blurFlag, final float sigma, final String fileName) {
        super(null, image);
        decimateFlag = decFlag;
        this.blurFlag = blurFlag;
        this.level = level;
        this.mode = mode;
        blurSigma = sigma;
        surfaceFileName = fileName;
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        if (maskImage != null) {
            maskImage.disposeLocal();
        }

        maskImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	super.setStartTime();
        // Blur image if necessary (the extraction algorithm does not work on
        // flat surfaces.
        // Therefore blur flat surface to produce a small intensity gradient and
        // then extract
        // a surface.
        fireProgressStateChanged(srcImage.getImageName(), "Extracting surface ...");

        fireProgressStateChanged(0);

        if (threadStopped) {

            if (maskImage != null) {
                maskImage.disposeLocal();
            }

            maskImage = null;
        }

        if (maskImage == null) {
            return;
        }

        if (maskImage.getNDims() > 2) {
            extractSurface();
        }
		System.err.println( "AlgorithmExtractSurfaceCubes Time : " + super.getElapsedTime() );
    }

    /**
     * Extracts a surface, in the form of triangles, from an image.
     */
    private void extractSurface() {
        int length;
        // double[][] inverseDicomArray = null;

        AlgorithmGaussianBlur blurAlgo;
        final float[] sigmas = {blurSigma, blurSigma, blurSigma};

        if (blurFlag == true) {

            try {
                blurAlgo = new AlgorithmGaussianBlur(null, maskImage, sigmas, true, false);

                fireProgressStateChanged("Blurring images");

                // fireProgressStateChanged(15, separateThread);
                blurAlgo.run();
                fireProgressStateChanged(15);

                if (blurAlgo.isCompleted() == false) {

                    if (maskImage != null) {
                        maskImage.disposeLocal();
                    }

                    errorCleanUp("Extract surface: image access error", true);

                    return;
                }
            } catch (final OutOfMemoryError error) {

                if (maskImage != null) {
                    maskImage.disposeLocal();
                }

                errorCleanUp("Extract surface: out of memory", true);

                return;
            }
        } else {
            fireProgressStateChanged(15);
        }

        // Uncomment next line to display blurred maskImage for debugging
        // purposes.
        // new ViewJFrameImage(maskImage, null, new Dimension(100,100),
        // maskImage.getUserInterface() );
        blurAlgo = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        int iXDim, iYDim, iZDim;

        iXDim = maskImage.getExtents()[0];
        iYDim = maskImage.getExtents()[1];
        iZDim = maskImage.getExtents()[2];

        float fXRes, fYRes, fZRes;

        fXRes = maskImage.getFileInfo()[0].getResolutions()[0];
        fYRes = maskImage.getFileInfo()[0].getResolutions()[1];
        fZRes = maskImage.getFileInfo()[0].getResolutions()[2];

        final float[] box = new float[3];

        box[0] = (iXDim - 1) * fXRes;
        box[1] = (iYDim - 1) * fYRes;
        box[2] = (iZDim - 1) * fZRes;

        /* Read the direction vector from the MipavCoordinateSystems class: */
        final int[] direction = MipavCoordinateSystems.getModelDirections(srcImage);
        final float[] startLocation = srcImage.getFileInfo(0).getOrigin();

        int[] buffer = null;

        // Make storage string
        surfaceFileName = ViewUserInterface.getReference().getDefaultDirectory() + File.separator + surfaceFileName;

        try {
            length = iXDim * iYDim * iZDim;
            buffer = new int[length];

            maskImage.exportData(0, length, buffer); // locks and releases
            // lock

            // Build surface extractor class
            // Build surface extractor class
            TransMatrix dicomMatrix = null;
            TransMatrix inverseDicomMatrix = null;
            if (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {

                // Get the DICOM transform that describes the transformation
                // from
                // axial to this image orientation
                dicomMatrix = srcImage.getMatrix();
                inverseDicomMatrix = new TransMatrix(srcImage.getMatrix());
                inverseDicomMatrix.Inverse();
            }

            final SurfaceExtractorCubes kExtractor = new SurfaceExtractorCubes(iXDim, iYDim, iZDim, buffer, fXRes,
                    fYRes, fZRes, direction, startLocation, dicomMatrix);

            fireProgressStateChanged("Starting surface extraction");

            final TriMesh kMesh = kExtractor.getLevelSurface(level, true);
            buffer = null;
            System.gc();
            fireProgressStateChanged(50);

            if (decimateFlag == true) {
                fireProgressStateChanged("Initializing surface.");
                VETMesh kVETMesh = new VETMesh(2 * kMesh.VBuffer.GetVertexQuantity(), .9f,
                        2 * kMesh.IBuffer.GetIndexQuantity(), .9f, 2 * kMesh.GetTriangleQuantity(), .9f, kMesh.IBuffer
                                .GetData());
                final Vector<VETMesh> kComponents = new Vector<VETMesh>();
                kVETMesh.GetComponents(kComponents);
                final int iNumComponents = kComponents.size();
                final ClodMesh[] akClod = new ClodMesh[iNumComponents];

                final int iIndex = surfaceFileName.lastIndexOf('.');
                final String kName = surfaceFileName.substring(0, iIndex);

                fireProgressStateChanged("Surface decimation in progress");
                for (int i = 0; (i < iNumComponents) && !threadStopped; i++) {
                    final VertexBuffer kVBuffer = new VertexBuffer(kMesh.VBuffer);
                    final IndexBuffer kIBuffer = new IndexBuffer(kComponents.get(i).GetTriangles());
                    final CreateClodMesh kDecimator = new CreateClodMesh(kVBuffer, kIBuffer);
                    kDecimator.decimate();
                    akClod[i] = new ClodMesh(kVBuffer, kIBuffer, kDecimator.getRecords());

                    fireProgressStateChanged("Saving surface");

                    FileSurface_WM.save(kName + i + ".sur", akClod[i], 1, akClod[i].VBuffer, true, direction,
                            startLocation, box, inverseDicomMatrix);
                }
                if (threadStopped) {
                    finalize();

                    return;
                }

                System.gc();

            } else {
                fireProgressStateChanged(75);
                fireProgressStateChanged("Saving surface");
                FileSurface_WM.save(surfaceFileName, kMesh, 0, kMesh.VBuffer, true, direction, startLocation, box,
                        inverseDicomMatrix);

            }
        } catch (final IOException error) {
            maskImage.disposeLocal();
            errorCleanUp("Extract surface: image access error", true);

            return;
        } catch (final OutOfMemoryError e) {
            maskImage.disposeLocal();
            errorCleanUp("Extract surface: out of memory error", true);

            return;
        }

        maskImage.disposeLocal();
        maskImage = null;
        System.gc();
        setCompleted(true);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {

        final int[] destExtents = new int[3];

        if (srcImage.getNDims() > 2) {
            destExtents[0] = srcImage.getExtents()[0];
            destExtents[1] = srcImage.getExtents()[1];
            destExtents[2] = srcImage.getExtents()[2];
        }

        try {

            if (mode == AlgorithmExtractSurfaceCubes.VOI_MODE) {
                int i;
                final ViewVOIVector VOIs = srcImage.getVOIs();
                int nVOI;

                nVOI = VOIs.size();

                short oldID = 0;
                boolean foundVOI = false;

                for (i = 0; i < nVOI; i++) {

                    if ( (VOIs.VOIAt(i).isActive() == true) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {

                        // VOI IDs start at 0 therefore ensure VOI ID is > 0
                        oldID = VOIs.VOIAt(i).getID();
                        VOIs.VOIAt(i).setID((short) (oldID + 1));
                        foundVOI = true;

                        break;
                    }
                }

                if ( !foundVOI) {

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {

                            // VOI IDs start at 0 therefore ensure VOI ID is > 0
                            oldID = VOIs.VOIAt(i).getID();
                            VOIs.VOIAt(i).setID((short) (oldID + 1));

                            break;
                        }
                    }
                } // (!foundVOI)

                // progressBar.setValue(2, separateThread);
                short[] tempImage = new short[destExtents[0] * destExtents[1] * destExtents[2]];

                // progressBar.setValue(3, separateThread);
                tempImage = srcImage.generateVOIMask(tempImage, i);

                // progressBar.setValue(4, separateThread);
                if (tempImage == null) {
                    MipavUtil.displayError("Error when making mask image from VOI.");
                    VOIs.VOIAt(i).setID(oldID);

                    return;
                }

                VOIs.VOIAt(i).setID(oldID);

                // progressBar.setValue(5, separateThread);
                for (i = 0; i < tempImage.length; i++) {

                    if (tempImage[i] > 0) {
                        tempImage[i] = 100;
                    }
                }

                // progressBar.setValue(10, separateThread);
                maskImage = new ModelImage(ModelStorageBase.USHORT, destExtents, "Surface image");
                maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                maskImage.importData(0, tempImage, true);

                // progressBar.setValue(12, separateThread);
                tempImage = null;
                System.gc();
            } else if (mode == AlgorithmExtractSurfaceCubes.MASK_MODE) {
                maskImage = new ModelImage(ModelStorageBase.USHORT, destExtents, "Surface image");

                maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());

                final int length = destExtents[0] * destExtents[1] * destExtents[2];

                for (int i = 0; i < length; i++) {

                    if (srcImage.getInt(i) > 0) { // && mask.get(i)) {
                        maskImage.set(i, 100);
                    } else {
                        maskImage.set(i, 0);
                    }
                }
            } else {
                maskImage = (ModelImage) srcImage.clone();
                maskImage.setImageName("Surface image");
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Algorithm extract surface: Image(s) locked");

            if (maskImage != null) {
                maskImage.disposeLocal();
                maskImage = null;
            }
        } catch (final OutOfMemoryError x) {
            MipavUtil.displayError("Dialog extract surface: unable to allocate enough memory");

            if (maskImage != null) {
                maskImage.disposeLocal(); // Clean up image memory
                maskImage = null;
            }
        }
    }
}
