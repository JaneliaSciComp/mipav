package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Meshes.*;
import WildMagic.LibGraphics.Detail.*;
import WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.*;

import java.io.*;
import java.util.*;



/**
 * Extracts a surface using Tetrahedron Extraction. Triangle decimation can be invoked to reduce triangle count. The
 * decimation algorithm produces a continious level of detail (clod) structure that can be used to optimize the the
 * visualization of the surface. The input to this algorithm is typically a mask image where 0 = background and 100 =
 * object (i.e. interior to a VOI). The mask image is then blurred slightly and the level (50) is extracted. A greyscale
 * image may also be input and a surface is extracted given a level. The steps are:
 *
 * <ol>
 *   <li>Build mask image of VOI (i.e. all point interior to VOI are set to 100. All points exterior are = 0.</li>
 *   <li>Blur mask image if not greyscale</li>
 *   <li>Extract level surface at 50 or user defined level</li>
 *   <li>Save surface ( ".sur")</li>
 *   <li>If decimate then decimate surface and save (".sur")</li>
 * </ol>
 *
 * @version  0.1 June, 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   David H. Eberly, Ph.D. wrote all the extration and decimation code found in the SurfaceExtraction,
 *           SurfaceDecimation and associated classes, with a little input from Matt with speed and memory optimizations
 * @see      ModelSurfaceExtractor
 * @see      ModelSurfaceDecimator
 * @see      ModelTriangleMesh
 */
public class AlgorithmExtractSurface extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Extract surface from VOI. */
    public static final int VOI_MODE = 0;

    /** Extract surface from mask image. */
    public static final int MASK_MODE = 1;

    /** Extract surface based on intensity level. */
    public static final int LEVEL_MODE = 2;

    /** Do not perform triangle consistency checking - all counter clockwise or all clockwise. */
    public static final int NONE_MODE = 0;

    /** Use adjacency model to perform triangle consistency. */
    public static final int ADJ_MODE = 1;

    /** Use smoothing model to perform triangle consistency. Smooths normals. */
    public static final int SMOOTH_MODE = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true then the input image is blurred slightly. */
    private boolean blurFlag;

    /** The amount to blur to smooth surface. */
    private float blurSigma = 0.5f;

    /** If true then the extracted surface is decimated into a continous level of detail surface (clod). */
    private boolean decimateFlag;

    /** Indicates level surface to be extraced. */
    private float level;

    /** Mask image to extract surface from. */
    private ModelImage maskImage;

    /** Indicates mode - VOI, LEVELSET, or MASK. */
    private int mode;

    /** The number of smoothing iterations to perform. */
    private int smoothIterations = 2;

    /** Positive smoothing scale factor. */
    private float smoothLambda = 0.33f;

    /** If true then the mesh of the surface is smoothed before it is saved. */
    private boolean smoothMeshFlag;

    /** Negative smoothing scale factor. */
    private float smoothMu = -0.34f;

    /** Path and name of extracted surface file. ".sur" will be appended if necessary. */
    private String surfaceFileName;

    /** Indicates triangle consistency mode. */
    private int triangleConsistencyMode = NONE_MODE;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmExtractSurface object.
     *
     * @param  image     mask image or grayscale where a level surface is to be extracted
     * @param  level     indicates level surface to be extraced
     * @param  mode      DOCUMENT ME!
     * @param  triMode   DOCUMENT ME!
     * @param  decFlag   indicates whether or not the decimation into a CLOD should take place.
     * @param  blurFlag  if true then the input image is blurred slightly
     * @param  sigma     the amount to blur the image
     * @param  fileName  path and name of extracted surface file. ".sur" will be appended if necessary.
     */
    public AlgorithmExtractSurface(ModelImage image, float level, int mode, int triMode, boolean decFlag,
                                   boolean blurFlag, float sigma, String fileName) {

        // default to no mesh smoothing
        this(image, level, mode, triMode, decFlag, blurFlag, sigma, fileName, false);
    }

    /**
     * Creates a new AlgorithmExtractSurface object.
     *
     * @param  image       mask image or grayscale where a level surface is to be extracted
     * @param  level       indicates level surface to be extraced
     * @param  mode        DOCUMENT ME!
     * @param  triMode     DOCUMENT ME!
     * @param  decFlag     indicates whether or not the decimation into a CLOD should take place.
     * @param  blurFlag    if true then the input image is blurred slightly
     * @param  sigma       the amount to blur the image
     * @param  fileName    path and name of extracted surface file. ".sur" will be appended if necessary.
     * @param  smoothFlag  whether the generated mesh should have smoothing applied to it
     */
    public AlgorithmExtractSurface(ModelImage image, float level, int mode, int triMode, boolean decFlag,
                                   boolean blurFlag, float sigma, String fileName, boolean smoothFlag) {
        super(null, image);
        decimateFlag = decFlag;
        this.blurFlag = blurFlag;
        this.level = level;
        this.mode = mode;
        this.triangleConsistencyMode = triMode;
        blurSigma = sigma;
        surfaceFileName = fileName;
        smoothMeshFlag = smoothFlag;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
     * Changes the surface smoothing settings.
     *
     * @param  iters   the number of iterations
     * @param  mu      negative scale factor
     * @param  lambda  positive scale factor
     */
    public void setSmoothnessVars(int iters, float mu, float lambda) {
        smoothIterations = iters;
        smoothMu = mu;
        smoothLambda = lambda;
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
        float[] sigmas = { blurSigma, blurSigma, blurSigma };

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
            } catch (OutOfMemoryError error) {

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

        float[] box = new float[3];

        box[0] = (iXDim - 1) * fXRes;
        box[1] = (iYDim - 1) * fYRes;
        box[2] = (iZDim - 1) * fZRes;

        /* Read the direction vector from the MipavCoordinateSystems class: */
        int[] direction = MipavCoordinateSystems.getModelDirections(srcImage);
        float[] origin = srcImage.getFileInfo(0).getOrigin();

        int[] buffer = null;
        int[] buffer2 = null;

        // Make storage string
        if (surfaceFileName.endsWith(".sur") == false) {
            surfaceFileName = ViewUserInterface.getReference().getDefaultDirectory() + File.separator +
                              surfaceFileName + ".sur";
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

            SurfaceExtractor kExtractor = new SurfaceExtractor(iXDim, iYDim, iZDim + 2, buffer2, fXRes, fYRes,
                                                                         fZRes, direction, origin, dicomMatrix);

            buffer = null;
            System.gc();

            fireProgressStateChanged("Starting surface extraction");

            // Next line extracts the surface at the supplied level.
            // kExtractor.flag = true;
            TriMesh kMesh = kExtractor.get((int) level);
            // Get the adjacent triangles:
            VETMesh kVETMesh = new VETMesh( 2* kMesh.VBuffer.GetVertexQuantity(), .9f,
                    2 * kMesh.IBuffer.GetIndexQuantity(), .9f,
                    2 * kMesh.GetTriangleQuantity(), .9f,
                    kMesh.IBuffer.GetData() );
            kMesh.IBuffer = new IndexBuffer( kVETMesh.GetTriangles() );

            buffer2 = null;
            fireProgressStateChanged(50);

            if (triangleConsistencyMode == SMOOTH_MODE) {
                //kMesh.smoothTwo(2, 0.03f, true, 0.01f, false);
            }

            if (smoothMeshFlag) {
                //kMesh.smoothThree(smoothIterations, smoothLambda, smoothMu, false);
            }

            if (decimateFlag == true) {
/*
                fireProgressStateChanged("Initializing surface.");
                VETMesh kVETMesh = new VETMesh( 2* kMesh.VBuffer.GetVertexQuantity(), .9f,
                		2 * kMesh.IBuffer.GetIndexQuantity(), .9f,
                		2 * kMesh.GetTriangleQuantity(), .9f,
                		kMesh.IBuffer.GetData() );
                Vector<VETMesh> kComponents = new Vector<VETMesh>();
                kVETMesh.GetComponents(kComponents);
                int iNumComponents = kComponents.size();
                ClodMesh[] akClod = new ClodMesh[iNumComponents];
                fireProgressStateChanged("Surface decimation in progress");
                for (i = 0; (i < iNumComponents) && !threadStopped; i++) {
                	VertexBuffer kVBuffer = new VertexBuffer(kMesh.VBuffer);
                	IndexBuffer kIBuffer = new IndexBuffer(kComponents.get(i).GetTriangles());
                    CreateClodMesh kDecimator = new CreateClodMesh(kVBuffer, kIBuffer);
                    kDecimator.decimate();
                    akClod[i] = new ClodMesh(kVBuffer, kIBuffer, kDecimator.getRecords());
                }

                if (threadStopped) {
                    finalize();

                    return;
                }

                System.gc();
                fireProgressStateChanged("Saving surface");
               FileSurface_WM.saveClodMesh(surfaceFileName, akClod, true, direction, origin, box, inverseDicomMatrix);
               */
            } else {
                fireProgressStateChanged(75);
                fireProgressStateChanged("Saving surface");
                FileSurface_WM.save(surfaceFileName, kMesh, 0, kMesh.VBuffer, true, direction, origin, box, inverseDicomMatrix);

            }
        } catch (IOException error) {
            maskImage.disposeLocal();
            errorCleanUp("Extract surface: image access error", true);

            return;
        } catch (OutOfMemoryError e) {
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

        int[] destExtents = new int[3];

        if (srcImage.getNDims() > 2) {
            destExtents[0] = srcImage.getExtents()[0];
            destExtents[1] = srcImage.getExtents()[1];
            destExtents[2] = srcImage.getExtents()[2];
        }

        try {

            if (mode == VOI_MODE) {
                int i;
                ViewVOIVector VOIs = srcImage.getVOIs();
                int nVOI;

                nVOI = VOIs.size();

                short oldID = 0;
                boolean foundVOI = false;

                for (i = 0; i < nVOI; i++) {

                    if ((VOIs.VOIAt(i).isActive() == true) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {

                        // VOI IDs start at 0 therefore ensure VOI ID is > 0
                        oldID = VOIs.VOIAt(i).getID();
                        VOIs.VOIAt(i).setID((short) (oldID + 1));
                        foundVOI = true;

                        break;
                    }
                }

                if (!foundVOI) {

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
                maskImage = new ModelImage(ModelImage.USHORT, destExtents, "Surface image");
                maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                maskImage.importData(0, tempImage, true);

                // progressBar.setValue(12);
                tempImage = null;
                System.gc();
            } else if (mode == MASK_MODE) {
                maskImage = new ModelImage(ModelImage.USHORT, destExtents, "Surface image");

                maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());

                int length = destExtents[0] * destExtents[1] * destExtents[2];


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
        } catch (IOException error) {
            MipavUtil.displayError("Algorithm extract surface: Image(s) locked");

            if (maskImage != null) {
                maskImage.disposeLocal();
                maskImage = null;
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog extract surface: unable to allocate enough memory");

            if (maskImage != null) {
                maskImage.disposeLocal(); // Clean up image memory
                maskImage = null;
            }
        }
    }
}
