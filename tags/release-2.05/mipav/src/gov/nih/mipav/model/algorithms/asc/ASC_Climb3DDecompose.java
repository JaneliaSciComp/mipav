package gov.nih.mipav.model.algorithms.asc;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import java.util.*;

import javax.vecmath.*;


/**
 * This class takes an image that is not cubic with dimension a power of two plus one and processes subimage blocks that
 * are cubic and of the correct dimension.
 */

public class ASC_Climb3DDecompose extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Axis orientation unknown. */
    public static final int ORI_UNKNOWN_TYPE = 0;

    /** Axis orientation Right to Left. */
    public static final int ORI_R2L_TYPE = 1;

    /** Axis orientation Left to Right. */
    public static final int ORI_L2R_TYPE = 2;

    /** Axis orientation Posterior to Anterior. */
    public static final int ORI_P2A_TYPE = 3;

    /** Axis orientation Anterior to Posterior. */
    public static final int ORI_A2P_TYPE = 4;

    /** Axis orientation Inferior to Superior. */
    public static final int ORI_I2S_TYPE = 5;

    /** Axis orientation Superior to Inferior. */
    public static final int ORI_S2I_TYPE = 6;

    /** Do not perform triangle consistency checking - all counter clockwise or all clockwise. */
    public static final int NONE_MODE = 0;

    /** Use adjacency model to perform triangle consistency. */
    public static final int ADJ_MODE = 1;

    /** Use smoothing model to perform triangle consistency. Smooths normals. */
    public static final int SMOOTH_MODE = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public Vector m_kTArray;

    /** mesh representing level surface. */
    public Vector m_kVArray;

    /** DOCUMENT ME! */
    private int[] axisOrientation;

    /** If true then the input image is blurred slightly. */
    private boolean blurFlag;

    /** The amount to blur to smooth surface. */
    private float blurSigma = 0.5f;

    /** DOCUMENT ME! */
    private float[] box;

    /**
     * For the fastest extraction, set this flag to 'false'. The vertices and triangles are extracted for each
     * monoregion without worrying about the duplication of vertices that occurs on edges shared by voxels. To eliminate
     * duplicate vertices, set this flag to 'true'.
     */
    private boolean bUnique; //// not needed I believe !!!!!*

    /** If true then the extracted surface is decimated into a continous level of detail surface (clod). */
    private boolean decimateFlag;

    /** DOCUMENT ME! */
    private int[] direction;

    /**
     * The level value for the desired isosurface. This value should not be an image value. The restriction allows for a
     * large amount of algorithmic simplification. A good choice for fLevel is a non--integral value, perhaps a value of
     * the form v.5 where v is an integer.
     */
    private float fLevel;

    /** DOCUMENT ME! */
    private float fXRes, fYRes, fZRes;

    /**
     * This parameter controls the resolution of the extracted isosurface. If its value is -1, the coarsest resolution
     * surface is constructed. If its value is power(2,n) where the subimage size is m-by-m-by-m with m = power(2,n)+1,
     * the finest resolution surface is constructed. This surface corresponds to an extraction at the voxel level. Any
     * value of iDepth between 0 and power(2,n)-1 may be used. The smaller the value, the coarser the resolution the
     * extracted surface is.
     */


    private int iDepth;

    /**
     * For the fastest extraction, set this flag to zero. The triangles are extracted for each monoregion without
     * worrying about the orientation of the triangles. In this mode, the display of triangles should have back-face
     * culling disabled. If you need the triangles to be consistently ordered, you have a choice of their normals being
     * in the direction of the image gradients at their centroids (set flag to +1) or in the opposite direction of the
     * image gradients (set flag to -1).
     */
    private int iOrientTriangles;

    /** DOCUMENT ME! */
    private float[] m_afData;

    /** block data. */
    private int m_iN, m_iTwoN, m_iSize;

    /** DOCUMENT ME! */
    private int m_iXBlocks, m_iYBlocks, m_iZBlocks;

    /** image data. */
    private int m_iXBound, m_iYBound, m_iZBound;

    /** the adaptive skeleton climber. */
    private ASC_Climb3D m_kASC;

    /** DOCUMENT ME! */
    private ModelImage maskImage;

    /** Indicates mode - VOI, LEVELSET, or MASK. */
    private int mode;

    /** DOCUMENT ME! */
    private float[] origin;

    /** If true then the mesh of the surface is smoothed before it is saved. */
    private boolean smoothMeshFlag;

    /** Path and name of extracted surface file. ".sur" will be appended if necessary. */
    private String surfaceFileName;

    /** Indicates triangle consistency mode. */
    private int triangleConsistencyMode = NONE_MODE;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a multiresolution extractor for isosurfaces that is based on adaptive skeleton climbing. The image
     * dimensions are not restricted to powers of two plus one. Level surfaces are extracted from subimage blocks that
     * are cubic with dimension that is a power of two plus one.
     *
     * @param  image     DOCUMENT ME!
     * @param  mode      DOCUMENT ME!
     * @param  triMode   DOCUMENT ME!
     * @param  blurFlag  DOCUMENT ME!
     * @param  sigma     DOCUMENT ME!
     * @param  fileName  DOCUMENT ME!
     */
    public ASC_Climb3DDecompose(ModelImage image, int mode, int triMode, boolean blurFlag, float sigma,
                                String fileName) {

        // default to no mesh smoothing
        this(image, mode, triMode, blurFlag, sigma, fileName, false);

    }

    /**
     * Create a multiresolution extractor for isosurfaces that is based on adaptive skeleton climbing. The image
     * dimensions are not restricted to powers of two plus one. Level surfaces are extracted from subimage blocks that
     * are cubic with dimension that is a power of two plus one.
     *
     * @param  image       DOCUMENT ME!
     * @param  mode        DOCUMENT ME!
     * @param  triMode     DOCUMENT ME!
     * @param  blurFlag    DOCUMENT ME!
     * @param  sigma       DOCUMENT ME!
     * @param  fileName    DOCUMENT ME!
     * @param  smoothFlag  whether the generated mesh should have smoothing applied to it
     */
    public ASC_Climb3DDecompose(ModelImage image, int mode, int triMode, boolean blurFlag, float sigma, String fileName,
                                boolean smoothFlag) {
        super(null, image);
        surfaceFileName = fileName;
        this.blurFlag = blurFlag;
        blurSigma = sigma;
        this.mode = mode;
        this.triangleConsistencyMode = triMode;
        smoothMeshFlag = smoothFlag;

        m_iXBound = image.getExtents()[0];
        m_iYBound = image.getExtents()[1];
        m_iZBound = image.getExtents()[2];

        int maxDim = m_iXBound;

        if (m_iYBound > maxDim) {
            maxDim = m_iYBound;
        }

        if (m_iZBound > maxDim) {
            maxDim = m_iZBound;
        }

        fXRes = image.getFileInfo()[0].getResolutions()[0];
        fYRes = image.getFileInfo()[0].getResolutions()[1];
        fZRes = image.getFileInfo()[0].getResolutions()[2];

        box = new float[3];
        box[0] = (m_iXBound - 1) * fXRes;
        box[1] = (m_iYBound - 1) * fYRes;
        box[2] = (m_iZBound - 1) * fZRes;

        origin = image.getFileInfo(0).getOrigin();
        axisOrientation = image.getFileInfo(0).getAxisOrientation();
        direction = new int[] { 1, 1, 1 };

        for (int i = 0; i <= 2; i++) {

            if ((axisOrientation[i] == ORI_L2R_TYPE) || (axisOrientation[i] == ORI_P2A_TYPE) ||
                    (axisOrientation[i] == ORI_S2I_TYPE)) {
                direction[i] = -1;
            }
        }

        // The requested block size is (iNBlock+1)^3 with 0 <= iNBlock <= 7.
        // Generate an array of subimages, each subimage corresponding to a
        // block.
        if (maxDim >= 256) {
            m_iN = 8;
        }

        if ((maxDim > 128) && (maxDim < 256)) {
            m_iN = 8;
        }

        if ((maxDim > 64) && (maxDim <= 128)) {
            m_iN = 7;
        }

        if ((maxDim > 32) && (maxDim <= 64)) {
            m_iN = 6;
        }

        if ((maxDim > 16) && (maxDim <= 32)) {
            m_iN = 5;
        }

        m_iTwoN = (1 << m_iN);
        m_iSize = m_iTwoN + 1;

        // Determine how many subimages are needed for level surface
        // extraction.
        double dTwoN = (double) m_iTwoN;
        m_iXBlocks = (int) Math.ceil(((double) m_iXBound - 1.0) / dTwoN);
        m_iYBlocks = (int) Math.ceil(((double) m_iYBound - 1.0) / dTwoN);
        m_iZBlocks = (int) Math.ceil(((double) m_iZBound - 1.0) / dTwoN);
        System.out.println(" m === " + m_iSize);
        m_kASC = new ASC_Climb3D(m_iN, new float[m_iSize * m_iSize * m_iSize]);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The function that the application calls to extract an isosurface of the specified level value.
     */
    public void extractContour() {
        m_kVArray = new Vector(); // array of Vertex3
        m_kTArray = new Vector(); // array of Triangle3

        int i, j;
        int length;
        TransMatrix dicomMatrix;
        TransMatrix inverseDicomMatrix = null;
        double[][] inverseDicomArray = null;
        float[] coord;
        float[] tCoord;

        AlgorithmGaussianBlur blurAlgo;
        float[] sigmas = { blurSigma, blurSigma, blurSigma };

        if (blurFlag == true) {

            try {
                blurAlgo = new AlgorithmGaussianBlur(maskImage, sigmas, true, false);

                progressBar.setMessage("Blurring images");
                progressBar.updateValue(15, activeImage);
                blurAlgo.setProgressBarVisible(false);
                blurAlgo.run();

                if (blurAlgo.isCompleted() == false) {
                    errorCleanUp("ASC: Blur failed", true);

                    return;
                }
            } catch (OutOfMemoryError error) {

                if (maskImage != null) {
                    maskImage.disposeLocal();
                }

                errorCleanUp("ASC: Out of memory", true);

                return;
            }
        }

        progressBar.updateValue(25, activeImage);

        // Uncomment next line to display blurred maskImage for debugging purposes.
        // new ViewJFrameImage(maskImage, null, new Dimension(100,100), maskImage.getUserInterface() );
        blurAlgo = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        fXRes = maskImage.getFileInfo()[0].getResolutions()[0];
        fYRes = maskImage.getFileInfo()[0].getResolutions()[1];
        fZRes = maskImage.getFileInfo()[0].getResolutions()[2];

        // Make storage string
        if (surfaceFileName.endsWith(".sur") == false) {
            surfaceFileName = maskImage.getUserInterface().getDefaultDirectory() + surfaceFileName + ".sur";
        } else {
            surfaceFileName = maskImage.getUserInterface().getDefaultDirectory() + surfaceFileName;
        }

        try {
            length = m_iXBound * m_iYBound * m_iZBound;
            m_afData = new float[length];
            maskImage.exportData(0, length, m_afData); // locks and releases lock
        } catch (IOException error) {
            errorCleanUp("ASC: image access error", true);

            return;
        } catch (OutOfMemoryError e) {
            errorCleanUp("ASC: Out of memory", true);

            return;
        }

        int iZMin = 0;

        for (int iZB = 0; iZB < m_iZBlocks; iZB++) {

            // compute maximum z-index, clamp to input z-bound
            int iZMax = iZMin + m_iTwoN;

            if (iZMax >= m_iZBound) {
                iZMax = m_iZBound - 1;
            }

            int iYMin = 0;

            for (int iYB = 0; iYB < m_iYBlocks; iYB++) {

                // compute maximum y-index, clamp to input y-bound
                int iYMax = iYMin + m_iTwoN;

                if (iYMax >= m_iYBound) {
                    iYMax = m_iYBound - 1;
                }

                int iXMin = 0;

                for (int iXB = 0; iXB < m_iXBlocks; iXB++) {

                    // compute maximum x-index, clamp to input x-bound
                    int iXMax = iXMin + m_iTwoN;

                    if (iXMax >= m_iXBound) {
                        iXMax = m_iXBound - 1;
                    }

                    // extract subimage
                    float[] afSubData = m_kASC.getData();
                    Arrays.fill(afSubData, Float.NEGATIVE_INFINITY);

                    for (int iZ = iZMin; iZ <= iZMax; iZ++) {

                        for (int iY = iYMin; iY <= iYMax; iY++) {

                            for (int iX = iXMin; iX <= iXMax; iX++) {
                                int iIndex = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
                                int iSubIndex = (iX - iXMin) + (m_iSize * ((iY - iYMin) + (m_iSize * (iZ - iZMin))));
                                afSubData[iSubIndex] = m_afData[iIndex];
                            }
                        }
                    }

                    progressBar.updateValue(35, activeImage);
                    progressBar.setMessage("Extracting surface");

                    // extract contour
                    m_kASC.extractContour(fLevel, iDepth, iOrientTriangles);
                    progressBar.updateValue(55, activeImage);

                    if (threadStopped) {
                        finalize();

                        return;
                    }

                    if (m_kASC.m_kVArray.size() > 0) {
                        int iOffset = m_kVArray.size();

                        for (i = 0; i < m_kASC.m_kTArray.size(); i++) {
                            ASC_Triangle3 kT = (ASC_Triangle3) m_kASC.m_kTArray.get(i);
                            kT.m_i0 += iOffset;
                            kT.m_i1 += iOffset;
                            kT.m_i2 += iOffset;
                            m_kTArray.add(kT);
                        }

                        for (i = 0; i < m_kASC.m_kVArray.size(); i++) {
                            ASC_Vertex3 kV = (ASC_Vertex3) m_kASC.m_kVArray.get(i);
                            kV.m_fX += iXMin;
                            kV.m_fY += iYMin;
                            kV.m_fZ += iZMin;
                            m_kVArray.add(kV);
                        }
                    }

                    // extractContour generates a lot of small objects.  The
                    // freeing of memory here is necessary to handle very
                    // large images.
                    System.gc();
                    progressBar.updateValue(65, activeImage);
                    progressBar.setMessage("Cleaning up");

                    if (threadStopped) {
                        finalize();

                        return;
                    }

                    iXMin += m_iTwoN;
                }

                iYMin += m_iTwoN;
            }

            iZMin += m_iTwoN;
        }

        ASC_Climb3D.makeUnique(m_kVArray, m_kTArray);

        // add code here to check consistancy.
        // build kMesh
        Point3f[] akVertex = null;
        int[] aiConnect = null;
        akVertex = new Point3f[m_kVArray.size()];
        aiConnect = new int[m_kTArray.size() * 3];

        if (srcImage.getFileInfo()[0].getTransformID() == FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL) {

            // Get the DICOM transform that describes the transformation from
            // axial to this image orientation
            dicomMatrix = (TransMatrix) (srcImage.getMatrix().clone());
            inverseDicomMatrix = (TransMatrix) (srcImage.getMatrix().clone());
            inverseDicomMatrix.invert();
            inverseDicomArray = inverseDicomMatrix.getMatrix();
            inverseDicomMatrix = null;
            coord = new float[3];
            tCoord = new float[3];

            for (i = 0; i < m_kVArray.size(); i++) {
                ASC_Vertex3 kV = (ASC_Vertex3) m_kVArray.get(i);

                // Change the voxel coordinate into millimeter space
                coord[0] = kV.m_fX * fXRes;
                coord[1] = kV.m_fY * fYRes;
                coord[2] = kV.m_fZ * fZRes;

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                tCoord[0] = tCoord[0] + origin[0];
                tCoord[1] = tCoord[1] + origin[1];
                tCoord[2] = tCoord[2] + origin[2];
                akVertex[i] = new Point3f(tCoord[0], tCoord[1], tCoord[2]);
            }
        } else {

            for (i = 0; i < m_kVArray.size(); i++) {

                // ASC_Vertex3 vert = (ASC_Vertex3)(m_kVArray.elementAt(i));
                ASC_Vertex3 kV = (ASC_Vertex3) m_kVArray.get(i);
                akVertex[i] = new Point3f((kV.m_fX * fXRes * direction[0]) + origin[0],
                                          (kV.m_fY * fYRes * direction[1]) + origin[1],
                                          (kV.m_fZ * fZRes * direction[2]) + origin[2]);
                // akVertex[i] = new Point3f(vert.m_fX, vert.m_fY, vert.m_fZ);
            }
        }

        for (i = 0, j = 0; i < m_kTArray.size(); i++) {
            ASC_Triangle3 triangle = (ASC_Triangle3) (m_kTArray.elementAt(i));
            aiConnect[j++] = triangle.m_i0;
            aiConnect[j++] = triangle.m_i1;
            aiConnect[j++] = triangle.m_i2;
        }

        ModelTriangleMesh kMesh = new ModelTriangleMesh(akVertex, aiConnect);

        if (triangleConsistencyMode == ADJ_MODE) {
            kMesh.getConsistentComponents();
        } else if (triangleConsistencyMode == SMOOTH_MODE) {
            kMesh.smoothTwo(2, 0.03f, true, 0.01f, false);
        }

        if (smoothMeshFlag) {
            kMesh.smoothMesh(2, 0.03f, true, 0.01f, false);
        }

        progressBar.updateValue(100, activeImage);

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            kMesh.save(surfaceFileName, true, direction, origin, box, inverseDicomArray);
            // save(surfaceFileName, true);
        } catch (IOException e) {
            System.out.println("Algorithm ASC: unable to open or write file");
        }

        System.gc();
        setCompleted(true);
        disposeProgressBar();

        return;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        if (maskImage != null) {
            maskImage.disposeLocal();
        }

        maskImage = null;
        m_kVArray = null;
        m_kTArray = null;
        m_afData = null;
        m_kASC = null;
        super.finalize();
    }

    /**
     * Print the triangle mesh to an ASCII file.
     *
     * @param      kName  the name of the output file
     *
     * @exception  IOException  if there is an error writing to the file
     */
    public void print(String kName) throws IOException {
        ASC_Vertex3[] akVertex = new ASC_Vertex3[m_kVArray.size()];
        int i;

        for (i = 0; i < akVertex.length; i++) {
            ASC_Vertex3 kV = (ASC_Vertex3) m_kVArray.get(i);
            akVertex[i] = new ASC_Vertex3((kV.m_fX * fXRes * direction[0]) + origin[0],
                                          (kV.m_fY * fYRes * direction[1]) + origin[1],
                                          (kV.m_fZ * fZRes * direction[2]) + origin[2]);
        }

        // get the vertex normals
        // ASC_Triangle3[] akTriangle = new ASC_Triangle3[m_kTriangle.size()];
        // for (i = 0; i < akTriangle.length; i++)
        // akTriangle[i] = (ASC_Triangle3)m_kTriangle.get(i);

        ASC_Vertex3[] akNormal = ASC_Climb3D.computeNormals(akVertex, m_kTArray);

        // print the mesh in the same ASCII format as for MjTriangleMesh
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));
        kOut.println('0'); // object is MjTriangleMesh
        kOut.println('1'); // one component

        // write vertices
        kOut.println(akVertex.length);

        for (i = 0; i < akVertex.length; i++) {
            ASC_Vertex3 kV = akVertex[i];
            kOut.print(kV.m_fX);
            kOut.print(' ');
            kOut.print(kV.m_fY);
            kOut.print(' ');
            kOut.println(kV.m_fZ);
        }

        // write normals
        for (i = 0; i < akVertex.length; i++) {
            ASC_Vertex3 kN = akNormal[i];
            kOut.print(kN.m_fX);
            kOut.print(' ');
            kOut.print(kN.m_fY);
            kOut.print(' ');
            kOut.println(kN.m_fZ);
        }

        // write connectivity
        kOut.println(m_kTArray.size());

        for (i = 0; i < m_kTArray.size(); i++) {
            ASC_Triangle3 kT = (ASC_Triangle3) m_kTArray.get(i);
            kOut.print(kT.m_i0);
            kOut.print(' ');
            kOut.print(kT.m_i1);
            kOut.print(' ');
            kOut.println(kT.m_i2);
        }

        kOut.close();
        akVertex = null;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        // Blur image if necessary (the extraction algorithm does not work on flat surfaces.
        // Therefore blur flat surface to produce a small intensity gradient and then extract
        // a surface.
        buildProgressBar(srcImage.getImageName(), "Extracting surface ...", 0, 100);
        initProgressBar();
        progressBar.updateValue(0, activeImage);

        constructLog();

        if (srcImage.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = srcImage.getExtents()[0];
            destExtents[1] = srcImage.getExtents()[1];
            destExtents[2] = srcImage.getExtents()[2];

            try {

                if (mode == AlgorithmExtractSurface.VOI_MODE) {
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

                    short[] tempImage = new short[destExtents[0] * destExtents[1] * destExtents[2]];
                    tempImage = srcImage.generateVOIMask(tempImage, i);

                    if (tempImage == null) {
                        MipavUtil.displayError("Error when making mask image from VOI.");
                        VOIs.VOIAt(i).setID(oldID);

                        return;
                    }

                    VOIs.VOIAt(i).setID(oldID);

                    for (i = 0; i < tempImage.length; i++) {

                        if (tempImage[i] > 0) {
                            tempImage[i] = 100;
                        }
                    }

                    System.gc();

                    maskImage = new ModelImage(ModelImage.FLOAT, destExtents, "Surface image",
                                               srcImage.getUserInterface());
                    maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                    maskImage.importData(0, tempImage, true);

                    System.gc();
                } else if (mode == AlgorithmExtractSurface.MASK_MODE) {
                    maskImage = new ModelImage(ModelImage.USHORT, destExtents, "Surface image",
                                               srcImage.getUserInterface());

                    maskImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());

                    int length = destExtents[0] * destExtents[1] * destExtents[2];
                    // BitSet mask = image.getMask(); // painted regions

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
                MipavUtil.displayError("Adaptive climbing: Image(s) locked");

                if (maskImage != null) {
                    maskImage.disposeLocal();
                }

                maskImage = null;
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Adaptive climbing: unable to allocate enough memory");

                if (maskImage != null) {
                    maskImage.disposeLocal(); // Clean up image memory
                    maskImage = null;
                }
            }
        }

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
            extractContour();
        }
    }

    /**
     * Save the triangle mesh to a binary file. The format for the file is
     *
     * <pre>
       int type;    // 0 = ModelTriangleMesh, 1 = ModelClodMesh
       int aCount;  // 1, write the entire mesh as a single component
       int vCount;  // number of vertices
       Point3f vertices[vCount];
       Point3f normals[vCount];
       int iCount;  // number of indices in the connectivity array
       int indices[iCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kName  the name of the file to which the triangle mesh is saved
     * @param      flip   if the y axis should be flipped - true for extract, false for from another surface
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public void save(String kName, boolean flip) throws IOException {
        RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");
        kOut.writeInt(0); // object is ModelTriangleMesh
        kOut.writeInt(1); // one component
        save(kOut, flip);
        kOut.close();

        if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
            print(JDialogBase.makeImageName(kName, "_sur.txt"));
        }
    }

    /**
     * Internal support for 'void save (String)' and 'void save (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to write vertices, normals, and connectivity indices to the file. ModelClodMesh overrides this to
     * additionally write collapse records to the file
     *
     * @param      kOut  the file to which the triangle mesh is saved
     * @param      flip  if the y axis should be flipped - true for extract, false for from another surface
     *
     * @exception  IOException  if there is an error writing to the file
     */

    /*protected void save (RandomAccessFile kOut, boolean flip) throws IOException
     * { float xRes, yRes, zRes; float fResMax = fXRes; if (fYRes > fResMax) fResMax = fYRes; if (fZRes > fResMax)
     * fResMax = fZRes;
     *
     * //xRes = fXRes/fResMax; //yRes = fYRes/fResMax; //zRes = fZRes/fResMax;
     *
     * xRes = fResMax/fXRes; yRes = fResMax/fYRes; zRes = fResMax/fZRes;
     *
     * xRes = 1; yRes = 1; zRes = 1;
     *
     * ASC_Vertex3[] akVertex = new ASC_Vertex3[m_kVertex.size()]; int i; for (i = 0; i < akVertex.length; i++) {
     * ASC_Vertex3 kV = (ASC_Vertex3)m_kVertex.get(i); akVertex[i] = new ASC_Vertex3( kV.m_fX*fXRes*direction[0] +
     * startLocation[0], kV.m_fY*fYRes*direction[1] + startLocation[1], kV.m_fZ*fZRes*direction[2] + startLocation[2]);
     * }
     *
     *
     * // get the vertex normals ASC_Triangle3[] akTriangle = new ASC_Triangle3[m_kTriangle.size()]; for (i = 0; i <
     * akTriangle.length; i++) akTriangle[i] = (ASC_Triangle3)m_kTriangle.get(i);
     *
     * ASC_Vertex3[] akNormal = ASC_Climb3D.computeNormals(akVertex, akTriangle);
     *
     *
     * // write vertices kOut.writeInt(akVertex.length); byte[] bufferByte = new byte[akVertex.length*24]; int index,
     * tmpInt; for (i = 0, index=0; i < akVertex.length; i++) { ASC_Vertex3 kVertex = akVertex[i];
     *
     * tmpInt = Float.floatToIntBits(kVertex.m_fX); bufferByte[index++] = (byte)(tmpInt >>> 24); bufferByte[index++] =
     * (byte)(tmpInt >>> 16); bufferByte[index++] = (byte)(tmpInt >>> 8); bufferByte[index++] = (byte)(tmpInt   & 0xff);
     *
     * if (flip) tmpInt = Float.floatToIntBits(-kVertex.m_fY); else tmpInt = Float.floatToIntBits(kVertex.m_fY);
     * bufferByte[index++] = (byte)(tmpInt >>> 24); bufferByte[index++] = (byte)(tmpInt >>> 16); bufferByte[index++] =
     * (byte)(tmpInt >>> 8); bufferByte[index++] = (byte)(tmpInt   & 0xff);
     *
     * if (flip) tmpInt = Float.floatToIntBits(-kVertex.m_fZ); else tmpInt = Float.floatToIntBits(kVertex.m_fZ);
     * bufferByte[index++] = (byte)(tmpInt >>> 24); bufferByte[index++] = (byte)(tmpInt >>> 16); bufferByte[index++] =
     * (byte)(tmpInt >>> 8); bufferByte[index++] = (byte)(tmpInt   & 0xff);
     *
     * }
     *
     * //ASC_Vertex3[] akNormal = computeNormals(); // write normals for (i = 0; i <  akVertex.length; i++) {
     * //getNormal(i,kNormal); ASC_Vertex3 kN = akNormal[i];
     *
     * tmpInt = Float.floatToIntBits(kN.m_fX); bufferByte[index++] = (byte)(tmpInt >>> 24); bufferByte[index++] =
     * (byte)(tmpInt >>> 16); bufferByte[index++] = (byte)(tmpInt >>> 8); bufferByte[index++] = (byte)(tmpInt   & 0xff);
     *
     * tmpInt = Float.floatToIntBits(kN.m_fY); bufferByte[index++] = (byte)(tmpInt >>> 24); bufferByte[index++] =
     * (byte)(tmpInt >>> 16); bufferByte[index++] = (byte)(tmpInt >>> 8); bufferByte[index++] = (byte)(tmpInt   & 0xff);
     *
     * tmpInt = Float.floatToIntBits(kN.m_fZ); bufferByte[index++] = (byte)(tmpInt >>> 24); bufferByte[index++] =
     * (byte)(tmpInt >>> 16); bufferByte[index++] = (byte)(tmpInt >>> 8); bufferByte[index++] = (byte)(tmpInt   & 0xff);
     * } kOut.write(bufferByte);
     *
     * // write connectivity kOut.writeInt( akTriangle.length * 3 ); byte[] bufferInt = new byte[ akTriangle.length*3*4];
     * for (i = 0, index=0; i <  akTriangle.length; i++) { ASC_Triangle3 kT = akTriangle[i];
     *
     * bufferInt[index++] = (byte)(kT.m_i0 >>> 24); bufferInt[index++] = (byte)(kT.m_i0 >>> 16); bufferInt[index++] =
     * (byte)(kT.m_i0 >>> 8); bufferInt[index++] = (byte)(kT.m_i0   & 0xff);
     *
     * bufferInt[index++] = (byte)(kT.m_i1 >>> 24); bufferInt[index++] = (byte)(kT.m_i1 >>> 16); bufferInt[index++] =
     * (byte)(kT.m_i1 >>> 8); bufferInt[index++] = (byte)(kT.m_i1   & 0xff);
     *
     * bufferInt[index++] = (byte)(kT.m_i2 >>> 24); bufferInt[index++] = (byte)(kT.m_i2 >>> 16); bufferInt[index++] =
     * (byte)(kT.m_i2 >>> 8); bufferInt[index++] = (byte)(kT.m_i2   & 0xff); }
     *
     * kOut.write(bufferInt);
     *
     * }*/


    /**
     * Save the triangle mesh to a binary file.
     *
     * @param      kOut  DOCUMENT ME!
     * @param      flip  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing to the file
     */
    public void save(RandomAccessFile kOut, boolean flip) throws IOException {

        ASC_Vertex3[] akVertex = new ASC_Vertex3[m_kVArray.size()];
        int i;

        for (i = 0; i < akVertex.length; i++) {
            ASC_Vertex3 kV = (ASC_Vertex3) m_kVArray.get(i);
            akVertex[i] = new ASC_Vertex3((kV.m_fX * fXRes * direction[0]) + origin[0],
                                          (kV.m_fY * fYRes * direction[1]) + origin[1],
                                          (kV.m_fZ * fZRes * direction[2]) + origin[2]);
        }

        // get the vertex normals
        // ASC_Triangle3[] akTriangle = new ASC_Triangle3[m_kTriangle.size()];
        // for (i = 0; i < akTriangle.length; i++)
        // akTriangle[i] = (ASC_Triangle3)m_kTriangle.get(i);

        // ASC_Vertex3[] akNormal = ASC_Climb3D.computeNormals(akVertex, m_kTArray);

        // print the mesh in the same binary format as for MjTriangleMesh
        // RandomAccessFile kOut = new RandomAccessFile(new File(kName),"rw");
        // kOut.writeInt(0);  // object is ASC_TriangleMesh
        // kOut.writeInt(1);  // one component

        // write vertices
        kOut.writeInt(akVertex.length);

        for (i = 0; i < akVertex.length; i++) {
            ASC_Vertex3 kV = akVertex[i];
            kOut.writeFloat(kV.m_fX);

            if (flip) {
                kV.m_fY = -kV.m_fY;
            }

            kOut.writeFloat(kV.m_fY);

            if (flip) {
                kV.m_fZ = -kV.m_fZ;
            }

            kOut.writeFloat(kV.m_fZ);
        }

        // Be very careful modifying vertices - please recompute the normals !!!
        ASC_Vertex3[] akNormal = ASC_Climb3D.computeNormals(akVertex, m_kTArray);

        // write normals
        for (i = 0; i < akVertex.length; i++) {
            ASC_Vertex3 kN = akNormal[i];
            kOut.writeFloat(kN.m_fX);
            kOut.writeFloat(kN.m_fY);
            kOut.writeFloat(kN.m_fZ);
        }

        // write connectivity
        kOut.writeInt(3 * m_kTArray.size());

        for (i = 0; i < m_kTArray.size(); i++) {
            ASC_Triangle3 kT = (ASC_Triangle3) m_kTArray.get(i);
            kOut.writeInt(kT.m_i0);
            kOut.writeInt(kT.m_i1);
            kOut.writeInt(kT.m_i2);
        }

        kOut.close();
        akVertex = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fLevel  DOCUMENT ME!
     */
    public void setFLevel(float fLevel) {
        this.fLevel = fLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  iDepth  DOCUMENT ME!
     */
    public void setIDepth(int iDepth) {
        this.iDepth = iDepth;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  iOrientTriangles  DOCUMENT ME!
     */
    public void setIOrientTriangles(int iOrientTriangles) {
        this.iOrientTriangles = iOrientTriangles;
    }

    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("ASC (" + m_iN + ", " + String.valueOf(blurFlag) + ", " + String.valueOf(blurSigma) +
                                   ", " + surfaceFileName + ")\n");
    }

}
