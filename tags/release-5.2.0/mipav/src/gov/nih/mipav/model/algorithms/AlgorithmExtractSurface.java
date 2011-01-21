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
 * Extracts a surface using Tetrahedron Extraction. Triangle decimation can be invoked to reduce triangle count. The
 * decimation algorithm produces a continious level of detail (clod) structure that can be used to optimize the the
 * visualization of the surface. The input to this algorithm is typically a mask image where 0 = background and 100 =
 * object (i.e. interior to a VOI). The mask image is then blurred slightly and the level (50) is extracted. A greyscale
 * image may also be input and a surface is extracted given a level. The steps are:
 * 
 * <ol>
 * <li>Build mask image of VOI (i.e. all point interior to VOI are set to 100. All points exterior are = 0.</li>
 * <li>Blur mask image if not greyscale</li>
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
public class AlgorithmExtractSurface extends AlgorithmBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Extract surface from VOI. */
    public static final int VOI_MODE = 0;

    /** Extract surface from mask image. */
    public static final int MASK_MODE = 1;

    /** Extract surface based on intensity level. */
    public static final int LEVEL_MODE = 2;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** If true then the input image is blurred slightly. */
    private final boolean blurFlag;

    /** The amount to blur to smooth surface. */
    private float blurSigma = 0.5f;

    /** If true then the extracted surface is decimated into a continuous level of detail surface (clod). */
    private final boolean decimateFlag;

    /** Indicates level surface to be extracted. */
    private final float level;

    /** Mask image to extract surface from. */
    private ModelImage maskImage;

    /** Indicates mode - VOI, LEVELSET, or MASK. */
    private final int mode;

    /** Path and name of extracted surface file. ".sur" will be appended if necessary. */
    private String surfaceFileName;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmExtractSurface object.
     * 
     * @param image mask image or grayscale where a level surface is to be extracted
     * @param level indicates level surface to be extraced
     * @param mode Indicates mode - VOI, LEVELSET, or MASK.
     * @param decFlag indicates whether or not the decimation into a CLOD should take place.
     * @param blurFlag if true then the input image is blurred slightly
     * @param sigma the amount to blur the image
     * @param fileName path and name of extracted surface file. ".sur" will be appended if necessary.
     */
    public AlgorithmExtractSurface(final ModelImage image, final float level, final int mode, final boolean decFlag,
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

        // Blur image if necessary (the extraction algorithm does not work on flat surfaces.
        // Therefore blur flat surface to produce a small intensity gradient and then extract
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
    }

    /**
     * Extracts a surface, in the form of triangles, from an image.
     */
    private void extractSurface() {
        int i;
        int length, bufferSize, sliceSize;
        TransMatrix dicomMatrix = null;
        TransMatrix inverseDicomMatrix = null;

        AlgorithmGaussianBlur blurAlgo;
        final float[] sigmas = {blurSigma, blurSigma, blurSigma};

        if (blurFlag == true) {

            try {
                blurAlgo = new AlgorithmGaussianBlur(null, maskImage, sigmas, true, false);

                fireProgressStateChanged("Blurring images");

                // fireProgressStateChanged(15);
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

        // Uncomment next line to display blurred maskImage for debugging purposes.
        // new ViewJFrameImage(maskImage, null, new Dimension(100,100), maskImage.getUserInterface() );
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
        final float[] origin = srcImage.getFileInfo(0).getOrigin();

        int[] buffer = null;
        int[] buffer2 = null;

        // Make storage string
        if (surfaceFileName.endsWith(".sur") == false) {
            surfaceFileName = ViewUserInterface.getReference().getDefaultDirectory() + File.separator + surfaceFileName
                    + ".sur";
        } else {
            surfaceFileName = ViewUserInterface.getReference().getDefaultDirectory() + File.separator + surfaceFileName;
        }

        try {
            length = iXDim * iYDim * iZDim;
            buffer = new int[length];
            bufferSize = iXDim * iYDim * (iZDim + 2);
            buffer2 = new int[bufferSize];
            sliceSize = iXDim * iYDim;

            maskImage.exportData(0, length, buffer); // locks and releases lock

            // put two blank slices at the front and back of surface image buffer, then pass this mask into the
            // extractor
            for (i = 0; i < sliceSize; i++) {
                buffer2[i] = 0;
            }

            for (i = sliceSize; i < (buffer.length + sliceSize); i++) {
                buffer2[i] = buffer[i - sliceSize];
            }

            for (i = buffer.length + sliceSize; i < buffer2.length; i++) {
                buffer2[i] = 0;
            }

            // Build surface extractor class
            if (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {

                // Get the DICOM transform that describes the transformation from
                // axial to this image orientation
                dicomMatrix = srcImage.getMatrix();
                inverseDicomMatrix = new TransMatrix(srcImage.getMatrix());
                inverseDicomMatrix.Inverse();
            }

            final SurfaceExtractor kExtractor = new SurfaceExtractor(iXDim, iYDim, iZDim + 2, buffer2, fXRes, fYRes,
                    fZRes, direction, origin, dicomMatrix);

            buffer = null;
            System.gc();

            fireProgressStateChanged("Starting surface extraction");

            // Next line extracts the surface at the supplied level.
            // kExtractor.flag = true;
            final TriMesh kMesh = kExtractor.get((int) level);
            // Get the adjacent triangles:
            VETMesh kVETMesh = new VETMesh(2 * kMesh.VBuffer.GetVertexQuantity(), .9f, 2 * kMesh.IBuffer
                    .GetIndexQuantity(), .9f, 2 * kMesh.GetTriangleQuantity(), .9f, kMesh.IBuffer.GetData());
            kMesh.IBuffer = new IndexBuffer(kVETMesh.GetTriangles());

            buffer2 = null;
            fireProgressStateChanged(50);

            if (decimateFlag == true) {
                fireProgressStateChanged("Initializing surface.");
                kVETMesh = new VETMesh(2 * kMesh.VBuffer.GetVertexQuantity(), .9f,
                        2 * kMesh.IBuffer.GetIndexQuantity(), .9f, 2 * kMesh.GetTriangleQuantity(), .9f, kMesh.IBuffer
                                .GetData());
                final Vector<VETMesh> kComponents = new Vector<VETMesh>();
                kVETMesh.GetComponents(kComponents);
                final int iNumComponents = kComponents.size();
                final ClodMesh[] akClod = new ClodMesh[iNumComponents];

                final int iIndex = surfaceFileName.lastIndexOf('.');
                final String kName = surfaceFileName.substring(0, iIndex);

                fireProgressStateChanged("Surface decimation in progress");
                for (i = 0; (i < iNumComponents) && !threadStopped; i++) {
                    final VertexBuffer kVBuffer = new VertexBuffer(kMesh.VBuffer);
                    final IndexBuffer kIBuffer = new IndexBuffer(kComponents.get(i).GetTriangles());
                    final CreateClodMesh kDecimator = new CreateClodMesh(kVBuffer, kIBuffer);
                    kDecimator.decimate();
                    akClod[i] = new ClodMesh(kVBuffer, kIBuffer, kDecimator.getRecords());

                    fireProgressStateChanged("Saving surface");

                    FileSurface_WM.save(kName + i + ".sur", akClod[i], 1, akClod[i].VBuffer, true, direction, origin,
                            box, inverseDicomMatrix);
                }
                if (threadStopped) {
                    finalize();

                    return;
                }

                System.gc();
            } else {
                fireProgressStateChanged(75);
                fireProgressStateChanged("Saving surface");
                FileSurface_WM.save(surfaceFileName, kMesh, 0, kMesh.VBuffer, true, direction, origin, box,
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

            if (mode == AlgorithmExtractSurface.VOI_MODE) {
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

                // progressBar.setValue(2);
                short[] tempImage = new short[destExtents[0] * destExtents[1] * destExtents[2]];

                // progressBar.setValue(3);
                tempImage = srcImage.generateVOIMask(tempImage, i);

                // progressBar.setValue(4);
                if (tempImage == null) {
                    MipavUtil.displayError("Error when making mask image from VOI.");
                    VOIs.VOIAt(i).setID(oldID);

                    return;
                }

                VOIs.VOIAt(i).setID(oldID);

                // progressBar.setValue(5);
                for (i = 0; i < tempImage.length; i++) {

                    if (tempImage[i] > 0) {
                        tempImage[i] = 100;
                    }
                }

                // progressBar.setValue(10);
                maskImage = new ModelImage(ModelStorageBase.USHORT, destExtents, "Surface image");
                maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                maskImage.importData(0, tempImage, true);

                // progressBar.setValue(12);
                tempImage = null;
                System.gc();
            } else if (mode == AlgorithmExtractSurface.MASK_MODE) {
                maskImage = new ModelImage(ModelStorageBase.USHORT, destExtents, "Surface image");

                maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());

                final int length = destExtents[0] * destExtents[1] * destExtents[2];

                for (int i = 0; i < length; i++) {

                    if (srcImage.getInt(i) > 0) {
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
