package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;

import java.io.*;

import java.util.*;


/**
 * A class for segmenting objects. The algorithm is partially based on the paper:
 *
 * <pre>
     BET: Brain Extraction Tool<br>
     Stephen M. Smith<br>
     FMRIB Technical Report TR00SMS2<br>
     Oxford Centre for Functional Magnetic Resonance Imaging of the Brain<br>
 * </pre>
 *
 * <p>See the document BrainExtraction.pdf for a detailed description of the algorithm as implemented in this class. A
 * few modifications to the original algorithm were made.</p>
 */
public class AlgorithmObjectExtractor extends AlgorithmBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected float c1Factor = 0.5f;

    /** DOCUMENT ME! */
    protected float c3Factor = 0.5f;

    /** DOCUMENT ME! */
    protected float[] m_afCurvature;

    /** The 3D MRI image stored as a 1D array. The mapping from (x,y,z) to 1D is: index = x + xbound*(y + ybound*z). */
    protected float[] m_afImage;

    /** DOCUMENT ME! */
    protected float[] m_afLength;

    /** DOCUMENT ME! */
    protected int[] m_aiConnect;

    /** object mask creation. */
    protected byte[] m_aiMask;

    /** DOCUMENT ME! */
    protected UnorderedSetInt[] m_akAdjacent;

    /** DOCUMENT ME! */
    protected Vector3f[] m_akSNormal;

    /** DOCUMENT ME! */
    protected Vector3f[] m_akSTangent;

    /** DOCUMENT ME! */
    protected Vector3f[] m_akVertex;

    /** DOCUMENT ME! */
    protected Vector3f[] m_akVMean;

    /** DOCUMENT ME! */
    protected Vector3f[] m_akVNormal;

    /** DOCUMENT ME! */
    protected float m_fEParam, m_fFParam;

    /** update parameters. */
    protected float m_fMeanEdgeLength;

    /** DOCUMENT ME! */
    protected float m_fRayDelta;

    /** DOCUMENT ME! */
    protected float m_fReductionX = 0.9f;

    /** DOCUMENT ME! */
    protected float m_fReductionY = 0.9f;

    /** DOCUMENT ME! */
    protected float m_fReductionZ = 0.9f;

    /** DOCUMENT ME! */
    protected float m_fStiffness;

    /** The size of a voxel, in voxel units. */
    protected float m_fXDelta, m_fYDelta, m_fZDelta;

    /** DOCUMENT ME! */
    protected int m_iDMax; // dilation size

    /** DOCUMENT ME! */
    protected int m_iEQuantity;

    /** DOCUMENT ME! */
    protected int m_iMedianIntensity;

    /** DOCUMENT ME! */
    protected int m_iTQuantity;

    /** mesh data. */
    protected int m_iVQuantity;

    /** The MRI image bounds and quantity of voxels. */
    protected int m_iXBound, m_iYBound, m_iZBound, m_iQuantity;

    /** initial ellipsoid parameters. */
    protected Vector3f m_kCenter;

    /** DOCUMENT ME! */
    protected HashMap<Edge,Integer> m_kEMap;

    /** DOCUMENT ME! */
    protected Matrix3f m_kRotate;

    /** DOCUMENT ME! */
    protected TriMesh triMesh = null;

    /** DOCUMENT ME! */
    private float[] box;

    /** DOCUMENT ME! */
    private int[] direction;

    /** DOCUMENT ME! */
    private float[] gvfBuffer;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int iSubdivisions;

    /** DOCUMENT ME! */
    private float oldUVal;

    /** DOCUMENT ME! */
    private float oldVVal;

    /** DOCUMENT ME! */
    private float oldWVal;

    /** DOCUMENT ME! */
    private boolean onlyInit = false;

    /** DOCUMENT ME! */
    private boolean saveGVF = false;

    /** DOCUMENT ME! */
    private float[] startLocation;

    /** DOCUMENT ME! */
    private float[] uVal = null;

    /** DOCUMENT ME! */
    private VOI voi;

    /** DOCUMENT ME! */
    private float[] vVal = null;

    /** DOCUMENT ME! */
    private float[] wVal = null;
    
    //  Storage for result of AlgorithmConvolver
    private float[] outputBuffer = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an extractor for segmenting an object from an image.
     *
     * @param  srcImg    the source image.
     * @param  voi       initial voi used to estimate the inital ellipsoid
     * @param  justInit  if true just perform one iteration
     * @param  saveGVF   if true save uvf, vvf, and wvf files
     * @param  triMesh   if not null use mesh instead of voi
     * @param  uVal      x component of GVF field
     * @param  vVal      y component of GVF field
     * @param  wVal      z component of GVF field
     */
    public AlgorithmObjectExtractor(ModelImage srcImg, VOI voi, boolean justInit, boolean saveGVF,
                                    TriMesh triMesh, float[] uVal, float[] vVal, float[] wVal) {
        onlyInit = justInit;
        this.saveGVF = saveGVF;
        this.triMesh = triMesh;
        this.uVal = uVal;
        this.vVal = vVal;
        this.wVal = wVal;

        /* The number of levels to subdivide the initial
         *   ellipsoid into a mesh that approximates the object surface.  The  number of triangles in the mesh is
         * 8*pow(4,S) where S is the  subdivision parameter.  A reasonable choice is 5, leading to a mesh
         *   with 8192 triangle. */
        iSubdivisions = 5; // 6 =  32K triangles, 5 = 8192 triangles

        // image bounds and voxel sizes
        m_iXBound = srcImg.getExtents()[0];
        m_iYBound = srcImg.getExtents()[1];
        m_iZBound = srcImg.getExtents()[2];
        m_iQuantity = m_iXBound * m_iYBound * m_iZBound;
        m_fXDelta = srcImg.getFileInfo()[0].getResolutions()[0];
        m_fYDelta = srcImg.getFileInfo()[0].getResolutions()[1];
        m_fZDelta = srcImg.getFileInfo()[0].getResolutions()[2];
        box = new float[3];
        box[0] = (m_iXBound - 1) * m_fXDelta;
        box[1] = (m_iYBound - 1) * m_fYDelta;
        box[2] = (m_iZBound - 1) * m_fZDelta;

        /* Read the direction vector from the MipavCoordinateSystems class: */
        direction = MipavCoordinateSystems.getModelDirections(srcImg);
        startLocation = srcImg.getFileInfo()[0].getOrigin();
        image = srcImg;
        this.voi = voi;

        m_fRayDelta = 0.75f;
        m_fStiffness = 0.1f;
        m_iDMax = 0;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The segmentation function. Various parameters may be modified, if necessary, before the call.
     */
    public void extractObject() {
        int i;
        int iMaxUpdate;

        // The parameters were selected basic on empirical studies.
        c1Factor = 0.75f; // c1
        m_fStiffness = 0.5f; // --> 0  less stiff  c2
        c3Factor = 0.05f; // --> 0  more stiff  c3

        fireProgressStateChanged(image.getImageName(), "Extracting object ...");


        if (onlyInit == true) {
            iMaxUpdate = 1;
        } else {
            iMaxUpdate = 500;
        }

        for (i = 1; (i <= iMaxUpdate) && !threadStopped; i++) {

            if (((i % 100) == 0)) {
                fireProgressStateChanged(Math.round((float) (i) / (iMaxUpdate) * 100));
            }

            updateMesh();
        }

        if (threadStopped) {

            setCompleted(false);
            finalize();

            return;
        }

        // identify those voxels inside the mesh

        try {
            saveMesh(true);
        } catch (IOException e) {
            System.out.println(" Problem saving mesh.");
        }

        setCompleted(true);
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        m_akVMean = null;
        m_akVNormal = null;
        m_akSNormal = null;
        m_akSTangent = null;
        m_afCurvature = null;
        m_aiMask = null;
        image = null;
        uVal = null;
        vVal = null;
        wVal = null;
        gvfBuffer = null;

        super.finalize();
    }

    /**
     * Get the dilation size for dilating the voxelized object surface obtained by rasterizing the triangle mesh. The
     * rasterization is designed so that the voxel surface has no holes, thereby allowing it to be flood-filled. But
     * just in case numerical round-off errors cause a few holes, this parameter is exposed for public use. The default
     * value is 0 (no dilation). If the value is D > 0, the dilation mask is a cube of size (2*D+1)x(2*D+1)x(2*D+1).
     *
     * @return  the current dilation size
     */
    public final int getDilationSize() {
        return m_iDMax;
    }


    /**
     * Get the 3D image that represents the extracted object. The image is ternary and has the same dimensions as the
     * input MRI. A voxel value of 0 indicates background. A voxel value of 1 indicates object surface. A voxel value of
     * 2 indicates a voxel inside the object surface.
     *
     * @return  the image that represents the extracted object
     */
    public final byte[] getObjectMask() {
        return m_aiMask;
    }

    /**
     * Get the spacing along the vertex normal rays, as described in BrainExtraction.pdf, that is part of the image term
     * in the surface evolution. The default value is 1.0.
     *
     * @return  the current spacing
     */
    public final float getRayDelta() {
        return m_fRayDelta;
    }


    /**
     * Set the reduction factor for estimating the initial ellipsoid, as described in BrainExtraction.pdf. The default
     * value is 0.75.
     *
     * @return  the current reduction factor
     */
    public final float getReductionX() {
        return m_fReductionX;
    }

    /**
     * Set the reduction factor for estimating the initial ellipsoid, as described in BrainExtraction.pdf. The default
     * value is 0.75.
     *
     * @return  the current reduction factor
     */
    public final float getReductionY() {
        return m_fReductionY;
    }

    /**
     * Set the reduction factor for estimating the initial ellipsoid, as described in BrainExtraction.pdf. The default
     * value is 0.75.
     *
     * @return  the current reduction factor
     */
    public final float getReductionZ() {
        return m_fReductionZ;
    }

    /**
     * Set the stiffness of the mesh, as described in BrainExtraction.pdf, that is part of the surface normal term in
     * the surface evolution. The default value is 0.1.
     *
     * @return  the current stiffness
     */
    public final float getStiffness() {
        return m_fStiffness;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x       DOCUMENT ME!
     * @param   y       DOCUMENT ME!
     * @param   z       DOCUMENT ME!
     * @param   buffer  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final float getTriLinear(float x, float y, float z, float[] buffer) {

        int imageSize = m_iXBound * m_iYBound;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz, dx1, dy1;
        float b1, b2;
        int incX;
        int incY;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        dx1 = 1 - dx;
        dy1 = 1 - dy;

        position1 = (intZ * imageSize) + (intY * m_iXBound) + intX;

        if (intZ < (m_iZBound - 1)) {
            position2 = position1 + imageSize;
        } else {
            position2 = position1;
        }

        if (intX < (m_iXBound - 1)) {
            incX = 1;
        } else {
            incX = 0;
        }

        if (intY < (m_iYBound - 1)) {
            incY = m_iXBound;
        } else {
            incY = 0;
        }

        b1 = (dy1 * ((dx1 * buffer[position1]) + (dx * buffer[position1 + incX]))) +
             (dy * ((dx1 * buffer[position1 + incY]) + (dx * buffer[position1 + incY + incX])));

        b2 = (dy1 * ((dx1 * buffer[position2]) + (dx * buffer[position2 + incX]))) +
             (dy * ((dx1 * buffer[position2 + incY]) + (dx * buffer[position2 + incY + incX])));

        return (((1 - dz) * b1) + (dz * b2));
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        if (image == null) {
            displayError("Source Image is null");
            finalize();
            return;
        }

        if (image.getNDims() != 3) {
            displayError("Source Image must be 3D");
            finalize();
            return;
        }     

        if ((uVal == null) || (vVal == null) || (wVal == null)) {
            calcGVF3D();
        }

        if (triMesh == null) {
            estimateEllipsoid();
            generateEllipsoidMesh();
        } 
        else { // triMesh != null
            m_iVQuantity = triMesh.VBuffer.GetVertexQuantity();
            m_akVertex = new Vector3f[m_iVQuantity];
            for ( int i = 0; i < m_iVQuantity; i++ )
            {
            	m_akVertex[i] = triMesh.VBuffer.GetPosition3(i);
            }
            m_aiConnect = new int[triMesh.IBuffer.GetIndexQuantity()];
            for ( int i = 0; i < triMesh.IBuffer.GetIndexQuantity(); i++ )
            {
            	m_aiConnect[i] = triMesh.IBuffer.GetData()[i];
            }
            m_iTQuantity = triMesh.GetTriangleQuantity();
            m_akAdjacent = new UnorderedSetInt[m_iVQuantity];

            for (int i = 0; i < m_iVQuantity; i++) {
                m_akAdjacent[i] = new UnorderedSetInt(6, 1);
            }

            m_kEMap = new HashMap<Edge,Integer>();

            Integer kInvalid = new Integer(-1);

            for (int iT = 0; iT < m_aiConnect.length;) {

                // get the vertices of the triangle
                int iP0 = m_aiConnect[iT++];
                int iP1 = m_aiConnect[iT++];
                int iP2 = m_aiConnect[iT++];

                m_kEMap.put(new Edge(iP0, iP1), kInvalid);
                m_kEMap.put(new Edge(iP1, iP2), kInvalid);
                m_kEMap.put(new Edge(iP2, iP0), kInvalid);

                m_akAdjacent[iP0].insert(iP1);
                m_akAdjacent[iP0].insert(iP2);
                m_akAdjacent[iP1].insert(iP0);
                m_akAdjacent[iP1].insert(iP2);
                m_akAdjacent[iP2].insert(iP0);
                m_akAdjacent[iP2].insert(iP1);
            }


            for (int iV = 0; iV < m_iVQuantity; iV++) {

                // into image space
                m_akVertex[iV].X = (m_akVertex[iV].X - startLocation[0]) / (direction[0] * m_fXDelta);

                // flip y
                m_akVertex[iV].Y = (2 * startLocation[1]) + (box[1] * direction[1]) - m_akVertex[iV].Y;
                m_akVertex[iV].Y = (m_akVertex[iV].Y - startLocation[1]) / (direction[1] * m_fYDelta);

                // flip z
                m_akVertex[iV].Z = (2 * startLocation[2]) + (box[2] * direction[2]) - m_akVertex[iV].Z;
                m_akVertex[iV].Z = (m_akVertex[iV].Z - startLocation[2]) / (direction[2] * m_fZDelta);
            }
        } // else triMesh != null

        // Supporting quantities for update of mesh.  VMean[i] stores the
        // average of the immediate vertex neighbors of vertex V[i].
        // VNormal[i] is the vertex normal at V[i] computed as the average
        // of the non-unit normals for all triangles sharing V[i].  Define
        // S = VMean[i] - V[i].  SNormal[i] is the component of S in the
        // VNormal[i] direction and STangent[i] = S - SNormal[i].  The value
        // Curvature[i] is an estimate of the surface curvature at V[i].
        m_akVMean = new Vector3f[m_iVQuantity];
        m_akVNormal = new Vector3f[m_iVQuantity];
        m_akSNormal = new Vector3f[m_iVQuantity];
        m_akSTangent = new Vector3f[m_iVQuantity];
        m_afCurvature = new float[m_iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akVMean[i] = new Vector3f();
            m_akVNormal[i] = new Vector3f();
            m_akSNormal[i] = new Vector3f();
            m_akSTangent[i] = new Vector3f();
        }

        // the binary mask for voxels inside the object surface
        m_aiMask = new byte[m_iQuantity];

        if (threadStopped) {

            setCompleted(false);
            finalize();

            return;
        }

        extractObject();
    }


    /**
     * Set the dilation size for dilating the voxelized object surface obtained by rasterizing the triangle mesh. The
     * rasterization is designed so that the voxel surface has no holes, thereby allowing it to be flood-filled. But
     * just in case numerical round-off errors cause a few holes, this parameter is exposed for public use. The default
     * value is 0 (no dilation). If the value is D > 0, the dilation mask is a cube of size (2*D+1)x(2*D+1)x(2*D+1).
     *
     * @param  iDMax  the new dilation size
     */
    public final void setDilationSize(int iDMax) {
        m_iDMax = iDMax;
    }


    /**
     * Indicates if surface evolution should be skipped. This is helpful when determining if the initial ellipsoid is a
     * good estimate.
     *
     * @param  flag  whether the surface evolution should be skipped
     */
    public final void setJustIntial(boolean flag) {
        onlyInit = flag;
    }


    /**
     * Set the spacing along the vertex normal rays, as described in BrainExtraction.pdf, that is part of the image term
     * in the surface evolution. The default value is 1.0.
     *
     * @param  fRayDelta  the new spacing along the vertex normal rays
     */
    public final void setRayDelta(float fRayDelta) {
        m_fRayDelta = fRayDelta;
    }

    /**
     * Set the reduction factor for estimating the initial ellipsoid, as described in BrainExtraction.pdf. The default
     * value is 0.6.
     *
     * @param  fReduction  the amount to reduce the axis of the ellipsoid.
     */
    public final void setReductionX(float fReduction) {
        m_fReductionX = fReduction;
    }

    /**
     * Set the reduction factor for estimating the initial ellipsoid, as described in BrainExtraction.pdf. The default
     * value is 0.5.
     *
     * @param  fReduction  the amount to reduce the axis of the ellipsoid.
     */
    public final void setReductionY(float fReduction) {
        m_fReductionY = fReduction;
    }

    /**
     * Set the reduction factor for estimating the initial ellipsoid, as described in BrainExtraction.pdf. The default
     * value is 0.6.
     *
     * @param  fReduction  the amount to reduce the axis of the ellipsoid.
     */
    public final void setReductionZ(float fReduction) {
        m_fReductionZ = fReduction;
    }

    /**
     * Set the stiffness of the mesh, as described in BrainExtraction.pdf, that is part of the surface normal term in
     * the surface evolution. The default value is 0.1.
     *
     * @param  fStiffness  the new stiffness
     */
    public final void setStiffness(float fStiffness) {
        m_fStiffness = fStiffness;
    }

    /**
     * The heart of the segmentation. This function is responsible for the evolution of the triangle mesh that
     * approximates the object surface. The update has a tangential component, a surface normal component, and a vertex
     * normal component for each vertex in the mesh. The first two components control the geometry of the mesh. The last
     * component is based on the MRI data itself. See BrainExtraction.pdf for a detailed description of the update
     * terms.
     */
    public void updateMesh() {
        computeMeanEdgeLength();
        computeVertexNormals();
        computeVertexInformation();

        int xDim = image.getExtents()[0] - 1;
        int yDim = image.getExtents()[1] - 1;
        int zDim = image.getExtents()[2] - 1;

        // update the vertices
        for (int i = 0; i < m_iVQuantity; i++) {
        	Vector3f kVertex = m_akVertex[i];

            // tangential update
        	kVertex.scaleAdd( c1Factor, m_akSTangent[i], kVertex );

            // normal update
            float fUpdate2 = update2(i);
            update3(i);

        	kVertex.scaleAdd( fUpdate2, m_akSNormal[i], kVertex );

            // uVal, vVal, and wVal go from about -0.5 to 0.5.
            kVertex.X += oldUVal;
            kVertex.Y += oldVVal;
            kVertex.Z += oldWVal;

            if (kVertex.X < 0) {
                kVertex.X = 0;
            }

            if (kVertex.Y < 0) {
                kVertex.Y = 0;
            }

            if (kVertex.Z < 0) {
                kVertex.Z = 0;
            }

            if (kVertex.X > (xDim - 1)) {
                kVertex.X = xDim - 1;
            }

            if (kVertex.Y > (yDim - 1)) {
                kVertex.Y = yDim - 1;
            }

            if (kVertex.Z > (zDim - 1)) {
                kVertex.Z = zDim - 1;
            }
        }
    }

    /**
     * Compute the average length of all the edges in the triangle mesh.
     */
    protected void computeMeanEdgeLength() {
        m_fMeanEdgeLength = 0.0f;

        Iterator<Map.Entry<Edge, Integer>> kEIter = m_kEMap.entrySet().iterator();
        Map.Entry <Edge,Integer>kEntry = null;
        Vector3f kEdge = new Vector3f();

        while (kEIter.hasNext()) {
            kEntry = (Map.Entry<Edge, Integer>) kEIter.next();
        	
            Edge kE = (Edge) kEntry.getKey();
            Vector3f kP0 = m_akVertex[kE.m_i0];
            Vector3f kP1 = m_akVertex[kE.m_i1];
            kEdge.copy(kP1).sub(kP0);
            m_fMeanEdgeLength += kEdge.length();
        }

        m_fMeanEdgeLength /= m_kEMap.size();
    }

    /**
     * Let V[i] be a vertex in the triangle mesh. This function computes VMean[i], the average of the immediate
     * neighbors of V[i]. Define S[i] = VMean[i] - V[i]. The function also computes a surface normal SNormal[i], the
     * component of S[i] in the vertex normal direction. STangent[i] = S[i] - SNormal[i] is computed as an approximation
     * to a tangent to the surface. Finally, Curvature[i] is an approximation of the surface curvature at V[i].
     */
    protected void computeVertexInformation() {
        float fMinCurvature = Float.POSITIVE_INFINITY;
        float fMaxCurvature = Float.NEGATIVE_INFINITY;
        float fInvMeanLength = 1.0f / m_fMeanEdgeLength;

        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVMean[i].set(0.0f, 0.0f, 0.0f);
        }

        Vector3f kS = new Vector3f();

        for (i = 0; i < m_iVQuantity; i++) {

            // compute the mean of the vertex neighbors
            // Point3f kMean = m_akVMean[i];
            UnorderedSetInt kAdj = m_akAdjacent[i];

            for (int j = 0; j < kAdj.getQuantity(); j++) {
                m_akVMean[i].add(m_akVertex[kAdj.get(j)]);
            }

            m_akVMean[i].scale(1.0f / kAdj.getQuantity());

            // compute the normal and tangential components of mean-vertex
            kS.copy(m_akVMean[i]).sub(m_akVertex[i]);
            m_akSNormal[i].copy(m_akVNormal[i]).scale(kS.dot(m_akVNormal[i]));
            m_akSTangent[i].copy(kS).sub(m_akSNormal[i]);

            // compute the curvature
            float fLength = m_akSNormal[i].length();
            m_afCurvature[i] = ((2.0f * fLength) * fInvMeanLength) * fInvMeanLength;

            if (m_afCurvature[i] < fMinCurvature) {
                fMinCurvature = m_afCurvature[i];
            }

            if (m_afCurvature[i] > fMaxCurvature) {
                fMaxCurvature = m_afCurvature[i];
            }
        }

        // compute the fractional function parameters for update2()
        m_fEParam = 0.5f * (fMinCurvature + fMaxCurvature);
        m_fFParam = 6.0f / (fMaxCurvature - fMinCurvature);
    }

    /**
     * Compute the vertex normals of the triangle mesh. Each vertex normal is the unitized average of the non-unit
     * triangle normals for those triangles sharing the vertex.
     */
    protected void computeVertexNormals() {

        // maintain a running sum of triangle normals at each vertex
        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVNormal[i].set(0.0f, 0.0f, 0.0f);
        }

        for (int iT = 0; iT < m_iTQuantity; iT++) {

            // get the vertices of the triangle
            int iP0 = m_aiConnect[3 * iT];
            int iP1 = m_aiConnect[(3 * iT) + 1];
            int iP2 = m_aiConnect[(3 * iT) + 2];
            Vector3f kP0 = m_akVertex[iP0];
            Vector3f kP1 = m_akVertex[iP1];
            Vector3f kP2 = m_akVertex[iP2];

            // compute the triangle normal
            Vector3f kEdge1 = Vector3f.sub(kP1, kP0);
            Vector3f kEdge2 = Vector3f.sub(kP2, kP0);
            Vector3f kNormal = Vector3f.cross(kEdge1, kEdge2);

            // the triangle normal partially contributes to each vertex normal
            m_akVNormal[iP0].add(kNormal);
            m_akVNormal[iP1].add(kNormal);
            m_akVNormal[iP2].add(kNormal);
        }

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVNormal[i].normalize();
        }
    }


    /**
     * Approximate the brain surface by an ellipsoid. The approximation is based on locating all voxels of intensity
     * larger than a brightness threshold and that are part of the upper-half of the head. The idea is that the scalp
     * voxels in the upper-half form lie approximately on an ellipsoidal surface.<br>
     * <br>
     *
     * <p>NOTE. The assumption is that the traversal from bottom to top of head is in the y-direction of the 3D image.
     * It does not matter if the top of the head has y-values smaller/larger than those for the bottom of the head. If
     * this assumption is not met, the image should be permuted OR this code must be modified to attempt to recognize
     * the orientation of the head</p>
     */
    protected void estimateEllipsoid() {

        // center-orient-length format for ellipsoid
        m_kCenter = new Vector3f();
        m_kRotate = new Matrix3f();
        m_afLength = new float[3];

        // Make the estimation numerically robust by tracking voxel positions
        // that are uniformly scaled into [-1,1]^3.

        float fBMax = (m_iXBound - 1) * m_fXDelta;

        if (((m_iYBound - 1) * m_fYDelta) > fBMax) {
            fBMax = (m_iYBound - 1) * m_fYDelta;
        }

        if (((m_iZBound - 1) * m_fZDelta) > fBMax) {
            fBMax = (m_iZBound - 1) * m_fZDelta;
        }

        float fInvBMax = 1.0f / fBMax;


        float fXScale = m_fXDelta * fInvBMax;
        float fYScale = m_fYDelta * fInvBMax;
        float fZScale = m_fZDelta * fInvBMax;

        Vector<Vector3f> kPts = new Vector<Vector3f>();
        VOIContour contour;
        Vector3f tmp3Pt = new Vector3f();
        Vector<VOIBase> curves = voi.getCurves();

        for (int i = 0; i < curves.size(); i++) {
            contour = (VOIContour) (curves.elementAt(i));

            for (int p = 0; p < contour.size(); p++) {
                tmp3Pt = contour.elementAt(p);

                Vector3f kVoxel = new Vector3f(((2.0f * tmp3Pt.X) - (m_iXBound - 1)) * fXScale,
                        ((2.0f * tmp3Pt.Y) - (m_iYBound - 1)) * fYScale,
                        ((2.0f * tmp3Pt.Z) - (m_iZBound - 1)) * fZScale);
                kPts.add(kVoxel);
            }
        }

        // Fit points with an ellipsoid.  The algorithm uses a least-squares
        // estimation of the coefficients for a quadratic equation that
        // represents the ellipsoid.
        AlgorithmQuadraticFit kQFit;
        kQFit = new AlgorithmQuadraticFit(kPts);

        // get the orientation matrix
        m_kRotate.copy(kQFit.getOrient());

        // compute the semi-axis lengths
        m_afLength[0] = kQFit.getConstant() / kQFit.getDiagonal(0);
        m_afLength[1] = kQFit.getConstant() / kQFit.getDiagonal(1);
        m_afLength[2] = kQFit.getConstant() / kQFit.getDiagonal(2);

        // assert: m_afLength[0] > 0 && m_afLength[1] > 0 && m_afLength[2] > 0
        m_afLength[0] = (float) Math.sqrt(m_afLength[0]);
        m_afLength[1] = (float) Math.sqrt(m_afLength[1]);
        m_afLength[2] = (float) Math.sqrt(m_afLength[2]);

        // rescale from [-1,1]^3 to voxel coordinates
        m_kCenter = kQFit.getCenter();
        m_kCenter.X = ((m_kCenter.X / fXScale) + (m_iXBound - 1)) / 2;
        m_kCenter.Y = ((m_kCenter.Y / fYScale) + (m_iYBound - 1)) / 2;
        m_kCenter.Z = ((m_kCenter.Z / fZScale) + (m_iZBound - 1)) / 2;
        Preferences.debug("Object extractor: extimateEllipsoid: Center = " + m_kCenter + "\n", Preferences.DEBUG_ALGORITHM);

        m_afLength[0] *= 1 / fXScale;
        m_afLength[1] *= 1 / fYScale;
        m_afLength[2] *= 1 / fZScale;

        // Use a smaller version of the ellipsoid for the initial mesh.  The
        // default reduction is 0.75, 0.75, 0,75.
        m_afLength[0] *= m_fReductionX;
        m_afLength[1] *= m_fReductionY;
        m_afLength[2] *= m_fReductionZ;

        Preferences.debug("Object extractor: extimateEllipsoid: ellipse length 1 = " + m_afLength[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Object extractor: extimateEllipsoid: ellipse length 2 = " + m_afLength[1] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Object extractor: extimateEllipsoid: ellipse length 3 = " + m_afLength[2] + "\n", 
        		Preferences.DEBUG_ALGORITHM);

        // Even if some lengths have failed. Estimate unknown axis using others.
        if (!(m_afLength[0] > 0) && (m_afLength[1] > 0) && (m_afLength[2] > 0)) {
            m_afLength[0] = (m_afLength[1] + m_afLength[2]) / 2.0f;
        } else if ((m_afLength[0] > 0) && !(m_afLength[1] > 0) && (m_afLength[2] > 0)) {
            m_afLength[1] = (m_afLength[0] + m_afLength[2]) / 2.0f;
        } else if ((m_afLength[0] > 0) && (m_afLength[1] > 0) && !(m_afLength[2] > 0)) {
            m_afLength[2] = (m_afLength[0] + m_afLength[1]) / 2.0f;
        } else if (!(m_afLength[0] > 0) && !(m_afLength[1] > 0) && (m_afLength[2] > 0)) {
            m_afLength[0] = m_afLength[1] = m_afLength[2];
        } else if (!(m_afLength[0] > 0) && (m_afLength[1] > 0) && !(m_afLength[2] > 0)) {
            m_afLength[0] = m_afLength[2] = m_afLength[1];
        } else if ((m_afLength[0] > 0) && !(m_afLength[1] > 0) && !(m_afLength[2] > 0)) {
            m_afLength[1] = m_afLength[2] = m_afLength[0];
        } else if (!(m_afLength[0] > 0) && !(m_afLength[1] > 0) && !(m_afLength[2] > 0)) { }

        Preferences.debug("Object extractor: extimateEllipsoid: ellipse length 1 = " + m_afLength[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Object extractor: extimateEllipsoid: ellipse length 2 = " + m_afLength[1] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Object extractor: extimateEllipsoid: ellipse length 3 = " + m_afLength[2] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Identify voxels enclosed by the brain surface by using a flood fill. The flood fill is nonrecursive to avoid
     * overflowing the program stack.
     *
     * @param  iX  the x-value of the seed point for the fill
     * @param  iY  the y-value of the seed point for the fill
     * @param  iZ  the z-value of the seed point for the fill
     */
    protected void floodFill(int iX, int iY, int iZ) {

        // Allocate the maximum amount of space needed.   An empty stack has
        // iTop == -1.
        int[] aiXStack = new int[m_iQuantity];
        int[] aiYStack = new int[m_iQuantity];
        int[] aiZStack = new int[m_iQuantity];

        // An empty stack has iTop = -1.  Push seed point onto stack.  All
        // points pushed onto stack have background color zero.
        int iTop = 0;
        aiXStack[iTop] = iX;
        aiYStack[iTop] = iY;
        aiZStack[iTop] = iZ;

        while (iTop >= 0) // stack is not empty
        {

            // Read top of stack.  Do not pop since we need to return to this
            // top value later to restart the fill in a different direction.
            iX = aiXStack[iTop];
            iY = aiYStack[iTop];
            iZ = aiZStack[iTop];

            // fill the pixel
            m_aiMask[getIndex(iX, iY, iZ)] = 2;

            int iXp1 = iX + 1;

            if ((iXp1 < m_iXBound) && (m_aiMask[getIndex(iXp1, iY, iZ)] == 0)) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXp1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iXm1 = iX - 1;

            if ((0 <= iXm1) && (m_aiMask[getIndex(iXm1, iY, iZ)] == 0)) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXm1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYp1 = iY + 1;

            if ((iYp1 < m_iYBound) && (m_aiMask[getIndex(iX, iYp1, iZ)] == 0)) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYp1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYm1 = iY - 1;

            if ((0 <= iYm1) && (m_aiMask[getIndex(iX, iYm1, iZ)] == 0)) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYm1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iZp1 = iZ + 1;

            if ((iZp1 < m_iZBound) && (m_aiMask[getIndex(iX, iY, iZp1)] == 0)) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZp1;

                continue;
            }

            int iZm1 = iZ - 1;

            if ((0 <= iZm1) && (m_aiMask[getIndex(iX, iY, iZm1)] == 0)) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZm1;

                continue;
            }

            // Done in all directions, pop and return to search other
            // directions.
            iTop--;
        }

        aiXStack = null;
        aiYStack = null;
        aiZStack = null;
    }

    /**
     * Tessellate a unit sphere centered at the origin. Start with an octahedron and subdivide. The final mesh is then
     * affinely mapped to the initial ellipsoid produced by estimateEllipsoid(). The subdivision scheme is described in
     * BrainExtraction.pdf.
     */
    protected void generateEllipsoidMesh() {

        // Compute the number of vertices, edges, and triangles for an
        // octahedron subdivided to the specified level.  The recursions are
        // V1 = V0 + E0
        // E1 = 2*E0 + 3*T0
        // T1 = 4*T0
        m_iVQuantity = 6;
        m_iEQuantity = 12;
        m_iTQuantity = 8;

        int iStep;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {
            m_iVQuantity = m_iVQuantity + m_iEQuantity;
            m_iEQuantity = (2 * m_iEQuantity) + (3 * m_iTQuantity);
            m_iTQuantity = 4 * m_iTQuantity;
        }

        // See BrainExtraction.pdf for a description of the subdivision
        // algorithm.  The use of the HashMap m_kEMap is to store midpoint
        // information for edges so that triangles sharing an edge know what
        // the new vertices are for replacing themselves with subtriangles.
        m_akVertex = new Vector3f[m_iVQuantity];
        m_aiConnect = new int[3 * m_iTQuantity];
        m_akAdjacent = new UnorderedSetInt[m_iVQuantity];

        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i] = new Vector3f();
            m_akAdjacent[i] = new UnorderedSetInt(6, 1);
        }

        m_akVertex[0].set(+1.0f, 0.0f, 0.0f);
        m_akVertex[1].set(-1.0f, 0.0f, 0.0f);
        m_akVertex[2].set(0.0f, +1.0f, 0.0f);
        m_akVertex[3].set(0.0f, -1.0f, 0.0f);
        m_akVertex[4].set(0.0f, 0.0f, +1.0f);
        m_akVertex[5].set(0.0f, 0.0f, -1.0f);

        m_aiConnect[0] = 4;
        m_aiConnect[1] = 0;
        m_aiConnect[2] = 2;
        m_aiConnect[3] = 4;
        m_aiConnect[4] = 2;
        m_aiConnect[5] = 1;
        m_aiConnect[6] = 4;
        m_aiConnect[7] = 1;
        m_aiConnect[8] = 3;
        m_aiConnect[9] = 4;
        m_aiConnect[10] = 3;
        m_aiConnect[11] = 0;
        m_aiConnect[12] = 5;
        m_aiConnect[13] = 2;
        m_aiConnect[14] = 0;
        m_aiConnect[15] = 5;
        m_aiConnect[16] = 1;
        m_aiConnect[17] = 2;
        m_aiConnect[18] = 5;
        m_aiConnect[19] = 3;
        m_aiConnect[20] = 1;
        m_aiConnect[21] = 5;
        m_aiConnect[22] = 0;
        m_aiConnect[23] = 3;

        m_kEMap = new HashMap<Edge,Integer>();

        Integer kInvalid = new Integer(-1);
        m_kEMap.put(new Edge(0, 4), kInvalid);
        m_kEMap.put(new Edge(1, 4), kInvalid);
        m_kEMap.put(new Edge(2, 4), kInvalid);
        m_kEMap.put(new Edge(3, 4), kInvalid);
        m_kEMap.put(new Edge(0, 5), kInvalid);
        m_kEMap.put(new Edge(1, 5), kInvalid);
        m_kEMap.put(new Edge(2, 5), kInvalid);
        m_kEMap.put(new Edge(3, 5), kInvalid);
        m_kEMap.put(new Edge(0, 2), kInvalid);
        m_kEMap.put(new Edge(2, 1), kInvalid);
        m_kEMap.put(new Edge(1, 3), kInvalid);
        m_kEMap.put(new Edge(3, 0), kInvalid);

        int iPNext = 6, iTSubQuantity = 8, iCNext = 24;
        int i0, i1, i2, iP0, iP1, iP2, iT;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {

            // generate midpoints of edges
            Iterator<Map.Entry<Edge, Integer>> kEIter = m_kEMap.entrySet().iterator();
            Map.Entry<Edge,Integer> kEntry = null;

            while (kEIter.hasNext()) {
                kEntry = (Map.Entry<Edge, Integer>) kEIter.next();

                Edge kE = (Edge) kEntry.getKey();
                Vector3f kP0 = m_akVertex[kE.m_i0];
                Vector3f kP1 = m_akVertex[kE.m_i1];
                Vector3f kPMid = m_akVertex[iPNext];
                kPMid.copy(kP0).add(kP1);

                float fInvLen = 1.0f /
                                    (float) Math.sqrt((kPMid.X * kPMid.X) + (kPMid.Y * kPMid.Y) + (kPMid.Z * kPMid.Z));
                kPMid.scale(fInvLen);
                kEntry.setValue(new Integer(iPNext));
                iPNext++;
            }

            // replace triangle by four subtriangles
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];

                Edge kE01 = new Edge(iP0, iP1);
                Edge kE12 = new Edge(iP1, iP2);
                Edge kE20 = new Edge(iP2, iP0);
                int iM01 = ((Integer) m_kEMap.get(kE01)).intValue();
                int iM12 = ((Integer) m_kEMap.get(kE12)).intValue();
                int iM20 = ((Integer) m_kEMap.get(kE20)).intValue();

                // add new edges

                // replace current triangle by middle triangle
                m_aiConnect[i0] = iM01;
                m_aiConnect[i1] = iM12;
                m_aiConnect[i2] = iM20;

                // append remaining subtriangles
                m_aiConnect[iCNext++] = iP0;
                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iM20;

                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iP1;
                m_aiConnect[iCNext++] = iM12;

                m_aiConnect[iCNext++] = iM20;
                m_aiConnect[iCNext++] = iM12;
                m_aiConnect[iCNext++] = iP2;
            }

            iTSubQuantity *= 4;

            // remove old edges
            m_kEMap.clear();

            // add new edges
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];
                m_kEMap.put(new Edge(iP0, iP1), kInvalid);
                m_kEMap.put(new Edge(iP1, iP2), kInvalid);
                m_kEMap.put(new Edge(iP2, iP0), kInvalid);
            }
        }

        // generate vertex adjacency
        for (iT = 0; iT < m_iTQuantity; iT++) {
            iP0 = m_aiConnect[3 * iT];
            iP1 = m_aiConnect[(3 * iT) + 1];
            iP2 = m_aiConnect[(3 * iT) + 2];

            m_akAdjacent[iP0].insert(iP1);
            m_akAdjacent[iP0].insert(iP2);
            m_akAdjacent[iP1].insert(iP0);
            m_akAdjacent[iP1].insert(iP2);
            m_akAdjacent[iP2].insert(iP0);
            m_akAdjacent[iP2].insert(iP1);
        }

        // rotate, scale, and translate sphere to get ellipsoid
        float resXFactor = m_fXDelta /
                               (float)
                                   Math.sqrt((m_kRotate.M00 * m_kRotate.M00 * m_fXDelta * m_fXDelta) +
                                                 (m_kRotate.M01 * m_kRotate.M01 * m_fYDelta * m_fYDelta) +
                                                 (m_kRotate.M02 * m_kRotate.M02 * m_fZDelta * m_fZDelta));
        float resYFactor = m_fYDelta /
                               (float)
                                   Math.sqrt((m_kRotate.M10 * m_kRotate.M10 * m_fXDelta * m_fXDelta) +
                                                 (m_kRotate.M11 * m_kRotate.M11 * m_fYDelta * m_fYDelta) +
                                                 (m_kRotate.M12 * m_kRotate.M12 * m_fZDelta * m_fZDelta));
        float resZFactor = m_fZDelta /
                               (float)
                                   Math.sqrt((m_kRotate.M20 * m_kRotate.M20 * m_fXDelta * m_fXDelta) +
                                                 (m_kRotate.M21 * m_kRotate.M21 * m_fYDelta * m_fYDelta) +
                                                 (m_kRotate.M22 * m_kRotate.M22 * m_fZDelta * m_fZDelta));

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i].X *= m_afLength[0];
            m_akVertex[i].Y *= m_afLength[1];
            m_akVertex[i].Z *= m_afLength[2];

            // Transform for equal reslution units
            m_kRotate.mult(m_akVertex[i], m_akVertex[i]);

            // Correct for unequal resolution units
            m_akVertex[i].X *= resXFactor;
            m_akVertex[i].Y *= resYFactor;
            m_akVertex[i].Z *= resZFactor;
            m_akVertex[i].add(m_kCenter);
        }
    }

    /**
     * A convenience function for mapping the 3D voxel position (iX,iY,iZ) to a 1D array index. The images are stored as
     * 1D arrays, so this function is used frequently.
     *
     * @param   iX  the x-value of the voxel position
     * @param   iY  the y-value of the voxel position
     * @param   iZ  the z-value of the voxel position
     *
     * @return  the 1D array index corresponding to (iX,iY,iZ)
     */
    protected final int getIndex(int iX, int iY, int iZ) {
        return iX + (m_iXBound * (iY + (m_iYBound * iZ)));
    }

    /**
     * Identify all voxels that are inside or on the mesh that represents the brain surface. The surface voxels are
     * constructed by rasterizing the triangles of the mesh in 3D. The centroid of these voxels is used as a seed point
     * for a flood fill of the region enclosed by the surface.
     */
    protected void getInsideVoxels() {

        // Arrays.fill(m_aiMask, 0);
        for (int n = 0; n < m_aiMask.length; n++) {
            m_aiMask[n] = 0;
        }

        int i, iX, iY, iZ;

        for (int iT = 0; iT < m_iTQuantity; iT++) {

            // get the vertices of the triangle
        	Vector3f kV0 = m_akVertex[m_aiConnect[3 * iT]];
        	Vector3f kV1 = m_akVertex[m_aiConnect[(3 * iT) + 1]];
        	Vector3f kV2 = m_akVertex[m_aiConnect[(3 * iT) + 2]];

            // compute the axis-aligned bounding box of the triangle
            float fXMin = kV0.X, fXMax = fXMin;
            float fYMin = kV0.Y, fYMax = fYMin;
            float fZMin = kV0.Z, fZMax = fZMin;

            if (kV1.X < fXMin) {
                fXMin = kV1.X;
            } else if (kV1.X > fXMax) {
                fXMax = kV1.X;
            }

            if (kV1.Y < fYMin) {
                fYMin = kV1.Y;
            } else if (kV1.Y > fYMax) {
                fYMax = kV1.Y;
            }

            if (kV1.Z < fZMin) {
                fZMin = kV1.Z;
            } else if (kV1.Z > fZMax) {
                fZMax = kV1.Z;
            }

            if (kV2.X < fXMin) {
                fXMin = kV2.X;
            } else if (kV2.X > fXMax) {
                fXMax = kV2.X;
            }

            if (kV2.Y < fYMin) {
                fYMin = kV2.Y;
            } else if (kV2.Y > fYMax) {
                fYMax = kV2.Y;
            }

            if (kV2.Z < fZMin) {
                fZMin = kV2.Z;
            } else if (kV2.Z > fZMax) {
                fZMax = kV2.Z;
            }

            // Rasterize the triangle.  The rasterization is repeated in all
            // three coordinate directions to make sure that floating point
            // round-off errors do not cause any holes in the rasterized
            // surface.
            int iXMin = (int) fXMin, iXMax = (int) fXMax;
            int iYMin = (int) fYMin, iYMax = (int) fYMax;
            int iZMin = (int) fZMin, iZMax = (int) fZMax;
            int ptr;
            int end = m_aiMask.length;

            for (iY = iYMin; iY <= iYMax; iY++) {

                for (iZ = iZMin; iZ <= iZMax; iZ++) {
                    iX = getIntersectX(kV0, kV1, kV2, iY, iZ);

                    if (iX != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask[ptr] = 1;
                            // m_aiMask[getIndex(iX,iY,iZ)] = 1;
                        }
                    }
                }
            }

            for (iX = iXMin; iX <= iXMax; iX++) {

                for (iZ = iZMin; iZ <= iZMax; iZ++) {
                    iY = getIntersectY(kV0, kV1, kV2, iX, iZ);

                    if (iY != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask[ptr] = 1;
                            // m_aiMask[getIndex(iX,iY,iZ)] = 1;
                        }
                    }
                }
            }

            for (iX = iXMin; iX <= iXMax; iX++) {

                for (iY = iYMin; iY <= iYMax; iY++) {
                    iZ = getIntersectZ(kV0, kV1, kV2, iX, iY);

                    if (iZ != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask[ptr] = 1;
                            // m_aiMask[getIndex(iX,iY,iZ)] = 1;
                        }
                    }
                }
            }
        }

        if (m_iDMax > 0) {

            // dilate to fill in gaps
            for (iZ = 1; iZ < (m_iZBound - 1); iZ++) {

                for (iY = 1; iY < (m_iYBound - 1); iY++) {

                    for (iX = 1; iX < (m_iXBound - 1); iX++) {
                        i = getIndex(iX, iY, iZ);

                        if (m_aiMask[i] == 1) {

                            for (int iDz = -m_iDMax; iDz <= m_iDMax; iDz++) {

                                for (int iDy = -m_iDMax; iDy <= m_iDMax; iDy++) {

                                    for (int iDx = -m_iDMax; iDx <= m_iDMax; iDx++) {
                                        int iX0 = iX + iDx;

                                        if ((iX0 < 0) || (iX0 >= m_iXBound)) {
                                            continue;
                                        }

                                        int iY0 = iY + iDy;

                                        if ((iY0 < 0) || (iY0 >= m_iYBound)) {
                                            continue;
                                        }

                                        int iZ0 = iZ + iDz;

                                        if ((iZ0 < 0) || (iZ0 >= m_iZBound)) {
                                            continue;
                                        }

                                        i = getIndex(iX0, iY0, iZ0);

                                        if (m_aiMask[i] == 0) {
                                            m_aiMask[i] = 2;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // reset to a binary image
            for (i = 0; i < m_iQuantity; i++) {

                if (m_aiMask[i] == 2) {
                    m_aiMask[i] = 1;
                }
            }
        }

        // compute centroid of the surface voxels to act as flood fill seed
        float fXC = 0.0f, fYC = 0.0f, fZC = 0.0f;
        int iCount = 0;

        for (iZ = 1; iZ < (m_iZBound - 1); iZ++) {

            for (iY = 1; iY < (m_iYBound - 1); iY++) {

                for (iX = 1; iX < (m_iXBound - 1); iX++) {

                    if (m_aiMask[getIndex(iX, iY, iZ)] > 0) {
                        fXC += (float) iX;
                        fYC += (float) iY;
                        fZC += (float) iZ;
                        iCount++;
                    }
                }
            }
        }

        float fInvCount = 1.0f / iCount;
        fXC *= fInvCount;
        fYC *= fInvCount;
        fZC *= fInvCount;

        floodFill((int) fXC, (int) fYC, (int) fZC);

        float fMin = (float) image.getMin();

        for (int m = 0; m < m_aiMask.length; m++) {

            if (m_aiMask[m] == 0) {
                image.set(m, fMin);
            }
        }

        image.calcMinMax();

    }

    /**
     * Compute the point of intersection between a line (0,iY,iZ)+t(1,0,0) and the triangle defined by the three input
     * points. All calculations are in voxel coordinates and the x-value of the intersection point is truncated to an
     * integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iY   the y-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the x-value of the intersection
     */
    protected int getIntersectX(Vector3f kV0, Vector3f kV1, Vector3f kV2, int iY, int iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iY - kV0.Y, fPv = iZ - kV0.Z;
        float fE1u = kV1.Y - kV0.Y, fE1v = kV1.Z - kV0.Z;
        float fE2u = kV2.Y - kV0.Y, fE2v = kV2.Z - kV0.Z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        return (int) (((fC0 * kV0.X) + (fC1 * kV1.X) + (fC2 * kV2.X)) / fDet);
    }

    /**
     * Compute the point of intersection between a line (iX,0,iZ)+t(0,1,0) and the triangle defined by the three input
     * points. All calculations are in voxel coordinates and the y-value of the intersection point is truncated to an
     * integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the y-value of the intersection
     */
    protected int getIntersectY(Vector3f kV0, Vector3f kV1, Vector3f kV2, int iX, int iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.X, fPv = iZ - kV0.Z;
        float fE1u = kV1.X - kV0.X, fE1v = kV1.Z - kV0.Z;
        float fE2u = kV2.X - kV0.X, fE2v = kV2.Z - kV0.Z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        int iY = (int) (((fC0 * kV0.Y) + (fC1 * kV1.Y) + (fC2 * kV2.Y)) / fDet);

        return iY;
    }

    /**
     * Compute the point of intersection between a line (iX,iY,0)+t(0,0,1) and the triangle defined by the three input
     * points. All calculations are in voxel coordinates and the z-value of the intersection point is truncated to an
     * integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iY   the y-value of the origin of the line
     *
     * @return  the z-value of the intersection
     */
    protected int getIntersectZ(Vector3f kV0, Vector3f kV1, Vector3f kV2, int iX, int iY) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.X, fPv = iY - kV0.Y;
        float fE1u = kV1.X - kV0.X, fE1v = kV1.Y - kV0.Y;
        float fE2u = kV2.X - kV0.X, fE2v = kV2.Y - kV0.Y;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        int iZ = (int) (((fC0 * kV0.Z) + (fC1 * kV1.Z) + (fC2 * kV2.Z)) / fDet);

        return iZ;
    }


    /**
     * Internal support to write vertices, normals, and connectivity indices to the file. 
     *
     * @param      flip  if the y axis should be inverted - true for extract, false for from another surface
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void saveMesh(boolean flip) throws IOException {
        TransMatrix dicomMatrix;
        TransMatrix inverseDicomMatrix = null;
        float[] coord;
        float[] tCoord;
        int i;

        String kName = ViewUserInterface.getReference().getDefaultDirectory() + image.getImageName() + "_object.sur";

        if (image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {

            // Get the DICOM transform that describes the transformation from
            // axial to this image orientation
            dicomMatrix = srcImage.getMatrix();
            inverseDicomMatrix = new TransMatrix(srcImage.getMatrix());
            inverseDicomMatrix.Inverse();

            // tDir corrects for the image orientation by putting the reference
            // point into the axial orientation
            coord = new float[3];
            tCoord = new float[3];

            for (i = 0; i < m_iVQuantity; i++) {

                // Change the voxel coordinate into millimeter space
                coord[0] = m_akVertex[i].X * m_fXDelta;
                coord[1] = m_akVertex[i].Y * m_fYDelta;
                coord[2] = m_akVertex[i].Z * m_fZDelta;

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                m_akVertex[i].X = startLocation[0] + tCoord[0];
                m_akVertex[i].Y = startLocation[1] + tCoord[1];
                m_akVertex[i].Z = startLocation[2] + tCoord[2];
            }
        } // if (image.getFileInfo()[0].getTransformID() ==
        else {

            for (i = 0; i < m_iVQuantity; i++) {
                m_akVertex[i].X = (m_akVertex[i].X * m_fXDelta * direction[0]) + startLocation[0];
                m_akVertex[i].Y = (m_akVertex[i].Y * m_fYDelta * direction[1]) + startLocation[1];
                m_akVertex[i].Z = (m_akVertex[i].Z * m_fZDelta * direction[2]) + startLocation[2];
            }
        } // else
        TriMesh kMesh = new TriMesh(new VertexBuffer(m_akVertex), new IndexBuffer(m_aiConnect));
        FileSurface_WM.save(kName, kMesh, 0, kMesh.VBuffer, flip, direction, startLocation, box, inverseDicomMatrix);
    }

    /**
     * Compute the coefficient of the surface normal for the update of the mesh vertex V[i] in the SNormal[i] direction.
     * See BrainExtraction.pdf for a description of the update.
     *
     * @param   i  the index of the vertex to update
     *
     * @return  the coefficient of SNormal[i] for the update
     */
    protected float update2(int i) {
        float fArg = m_fFParam * (m_afCurvature[i] - m_fEParam);
        float fExpP = (float) Math.exp(fArg);
        float fExpN = (float) Math.exp(-fArg);
        float fTanh = (fExpP - fExpN) / (fExpP + fExpN);
        float fUpdate2 = 0.5f * m_fStiffness * (1.0f + fTanh);

        return fUpdate2;
    }


    /**
     * Compute the coefficient of the vertex normal for the update of the mesh vertex V[i] in the VNormal[i] direction.
     * See BrainExtraction.pdf for a description of the update.
     *
     * @param  i  the index of the vertex to update
     */
    protected void update3(int i) {
    	Vector3f kVertex = m_akVertex[i];

        float x = kVertex.X;
        float y = kVertex.Y;
        float z = kVertex.Z;


        if (x < 0) {
            x = 0;
        } else if (x >= (m_iXBound - 1)) {
            x = m_iXBound - 1;
        }

        if (y < 0) {
            y = 0;
        } else if (y >= (m_iYBound - 1)) {
            y = m_iYBound - 1;
        }

        if (z < 0) {
            z = 0;
        } else if (z >= (m_iZBound - 1)) {
            z = m_iZBound - 1;
            // nearest neighbor interpolation
            /*iX = (int)(kVertex.X + 0.5f);
             * iY = (int)(kVertex.Y + 0.5f); iZ = (int)(kVertex.Z + 0.5f); if      ( iX < 0 )          iX = 0; else if (
             * iX >= m_iXBound ) iX = m_iXBound - 1;
             *
             * if      ( iY < 0 )          iY = 0; else if ( iY >= m_iYBound ) iY = m_iYBound - 1;
             *
             * if      ( iZ < 0 )          iZ = 0; else if ( iZ >= m_iZBound ) iZ = m_iZBound - 1;float oldGVF =
             * gvfBuffer[iX + m_iXBound*(iY+m_iYBound*iZ)];*/
        }

        oldUVal = getTriLinear(x, y, z, uVal);
        oldVVal = getTriLinear(x, y, z, vVal);
        oldWVal = getTriLinear(x, y, z, wVal);

        // float oldGVF = getTriLinear(x,y,z,gvfBuffer);
        return;

        /*// TO DO.  The ray depth should be in millimeters, not voxel units.
         * // For now I'll just use the value as specified.  Later I need to // input the dx, dy, and dz terms for
         * millimeters per voxel. Vector3f kDiff = new Vector3f();
         *
         * // get point on ray emanating from vertex into bounded region kDiff.scaleAdd(-m_fRayDelta, kNormal, kVertex);
         *
         * // nearest neighbor interpolation iX = (int)(kDiff.X + 0.5f); iY = (int)(kDiff.Y + 0.5f); iZ = (int)(kDiff.Z +
         * 0.5f); if      ( iX < 0 )          iX = 0; else if ( iX >= m_iXBound ) iX = m_iXBound - 1;
         *
         * if      ( iY < 0 )          iY = 0; else if ( iY >= m_iYBound ) iY = m_iYBound - 1;
         *
         * if      ( iZ < 0 )          iZ = 0; else if ( iZ >= m_iZBound ) iZ = m_iZBound - 1;
         *
         * float newGVF = gvfBuffer[iX + m_iXBound*(iY+m_iYBound*iZ)]; x = kDiff.X; y = kDiff.Y; z = kDiff.Z; if      ( x
         * < 0 )          x = 0; else if ( x >= m_iXBound-1 ) x = m_iXBound - 1;
         *
         * if      ( y < 0 )          y = 0; else if ( y >= m_iYBound-1 ) y = m_iYBound - 1;
         *
         * if      ( z < 0 )          z = 0; else if ( z>= m_iZBound-1 ) z = m_iZBound - 1; float newGVF =
         * getTriLinear(x,y,z,gvfBuffer); float fUpdate3; if (newGVF >= oldGVF ) { fUpdate3 = 0; //fUpdate3 = 0.5f *
         * c3Factor * m_fMeanEdgeLength; } else{ fUpdate3 = c3Factor;// * m_fMeanEdgeLength; }
         *
         *return fUpdate3;*/
    }


    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void calcGradMag() {
        ModelImage energyImage = null;
        AlgorithmGradientMagnitude gradMagAlgo = null;

        try {
            energyImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), image.getImageName() + "_gm");

            float[] sigmas = new float[3];
            sigmas[0] = 2f;
            sigmas[1] = 2f;
            sigmas[2] = 0.5f;
            gradMagAlgo = new AlgorithmGradientMagnitude(energyImage, image, sigmas, true, false);
            gradMagAlgo.setRunningInSeparateThread(runningInSeparateThread);
            gradMagAlgo.run();

            if (gradMagAlgo.isCompleted() == false) {
                setCompleted(false);

                if (energyImage != null) {
                    energyImage.disposeLocal();
                }

                energyImage = null;
                gradMagAlgo = null;
                System.gc();

                return;
            }
        } catch (OutOfMemoryError error) {

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            System.gc();
            setCompleted(false);

            return;
        }

        try {
            energyImage.exportData(0, m_afImage.length, m_afImage);
        } catch (IOException error) {

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;

            return;
        }

    }

    /**
     * Calculate GVF from 3D image buffer.
     */
    private void calcGVF3D() {
        float ix, iy, iz;
        float gvfMin, gvfMax;
        int x, y, z;
        float del2;
        int iteration;
        int i, i1, i2;
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        int[] extents = new int[3];
        extents[0] = xDim;
        extents[1] = yDim;
        extents[2] = zDim;

        int sliceSize = xDim * yDim;
        int length = sliceSize * zDim;
        int expSliceSize = (xDim + 2) * (yDim + 2);
        float[] sigmas = new float[] { 1.0f, 1.0f, 1.0f };
        float[] expGvfBuffer;
        float[] fx;
        float[] fy;
        float[] fz;
        float[] gVal;
        float[] imgBuffer;
        AlgorithmConvolver convolver;

        /** In the paper "Generalized gradient vector flow external forces for active contours"
         *by Chenyang Xu and Jerry Prince values of 0.05, 0.15, and 0.2 were used for k*/
        float kValue = 0.15f;

        /** Maximum iterations to generate generalized gradient vector field*/
        int gvfIterations = 200;

        try {
            expGvfBuffer = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            fx = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            fy = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            fz = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            uVal = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            vVal = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            wVal = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            gVal = new float[(xDim + 2) * (yDim + 2) * (zDim + 2)];
            imgBuffer = new float[xDim * yDim * zDim];
            gvfBuffer = new float[xDim * yDim * zDim];
        } catch (OutOfMemoryError e) {
            finalize();

            displayError("AlgorithmObjectExtractor: Out of memory");
            setCompleted(false);

            return;
        }

        try {
            image.exportData(0, length, imgBuffer);
        } catch (IOException error) {
            displayError("AlgorithmObjectExtractor: Source image is locked");
            finalize();

            setCompleted(false);

            return;
        }

        fireProgressStateChanged(image.getImageName(), "Calculating 3D GVF ...");

        // Make 3D kernels
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        int[] kExtents = new int[3];

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(5 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        kExtents[2] = zkDim;

        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        float[] GxData = new float[xkDim * ykDim * zkDim];
        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 0;

        float[] GyData = new float[xkDim * ykDim * zkDim];
        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);
        Gy.calc(true);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 1;

        float[] GzData = new float[xkDim * ykDim * zkDim];
        GenerateGaussian Gz = new GenerateGaussian(GzData, kExtents, sigmas, derivOrder);
        Gz.calc(true);

        // Edge map = |grad(Gsigma(x,y,z)*I(x,y,z))|
        if ((sigmas[0] != 0.0) && (sigmas[1] != 0.0) && (sigmas[2] != 0.0)) {
            convolver = new AlgorithmConvolver(image, GxData, GyData, GzData, kExtents, true); 
            convolver.addListener(this);
            convolver.run();
            convolver.finalize();
            for (i = 0; i < length; i++) {
                gvfBuffer[i] = outputBuffer[i];
            }
        } // if ((sigmas[0] != 0.0) && (sigmas[1] != 0.0) && (sigmas[2] != 0.0))
        else {
            for (z = 0; (z < zDim) && (!threadStopped); z++) {
                i1 = z * sliceSize;
    
                for (y = 0; (y < yDim) && (!threadStopped); y++) {
                    i2 = i1 + (xDim * y);
    
                    for (x = 0; (x < xDim) && (!threadStopped); x++) {
                        i = i2 + x;
                        ;
    
                        if (sigmas[0] != 0.0f) {
                            ix = AlgorithmConvolver.convolveWhole3DPt(i, extents, imgBuffer, kExtents, GxData);
                        } // if (sigmas[0] != 0.0f)
                        else { // (sigmas[0] == 0.0f)
    
                            if (x == 0) {
                                ix = imgBuffer[1 + (y * xDim) + (z * sliceSize)] - imgBuffer[i];
                            } else if (x == (xDim - 1)) {
                                ix = imgBuffer[i] - imgBuffer[(xDim - 2) + (y * xDim) + (z * sliceSize)];
                            } else {
                                ix = (imgBuffer[(x + 1) + (y * xDim) + (z * sliceSize)] -
                                      imgBuffer[(x - 1) + (y * xDim) + (z * sliceSize)]) / 2.0f;
                            }
                        } // else (sigmas[0] == 0.0f)
    
                        if (sigmas[1] != 0.0f) {
                            iy = AlgorithmConvolver.convolveWhole3DPt(i, extents, imgBuffer, kExtents, GyData);
                        } // if (sigmas[1] != 0.0f)
                        else { // sigmas[1] == 0.0f
    
                            if (y == 0) {
                                iy = imgBuffer[x + xDim + (z * sliceSize)] - imgBuffer[i];
                            } else if (y == (yDim - 1)) {
                                iy = imgBuffer[i] - imgBuffer[x + ((yDim - 2) * xDim) + (z * sliceSize)];
                            } else {
                                iy = (imgBuffer[x + ((y + 1) * xDim) + (z * sliceSize)] -
                                      imgBuffer[x + ((y - 1) * xDim) + (z * sliceSize)]) / 2.0f;
                            }
                        } // else sigmas[1] == 0.0f
    
                        if (sigmas[2] != 0.0) {
                            iz = AlgorithmConvolver.convolveWhole3DPt(i, extents, imgBuffer, kExtents, GzData);
                        } // if (sigmas[2] != 0.0f)
                        else { // sigmas[2] == 0.0f
    
                            if (z == 0) {
                                iz = imgBuffer[x + (y * xDim) + sliceSize] - imgBuffer[x + (y * xDim)];
                            } else if (z == (zDim - 1)) {
                                iz = imgBuffer[x + (y * xDim) + ((zDim - 1) * sliceSize)] -
                                     imgBuffer[x + (y * xDim) + ((zDim - 2) * sliceSize)];
                            } else {
                                iz = (imgBuffer[x + (y * xDim) + ((z + 1) * sliceSize)] -
                                      imgBuffer[x + (y * xDim) + ((z - 1) * sliceSize)]) / 2.0f;
                            }
                        } // else sigmas[2] == 0.0f
    
                        gvfBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                    } // for (x = 0; x < xDim && !threadStopped; x++)
                } // for (y = 0; y < yDim && !threadStopped; y++)
            } // for (z = 0; z < zDim && !threadStopped; z++)
        } // else 


        if (threadStopped) {
            finalize();


            return;
        }

        // Compute the gradient vector flow of the edge map
        // Normalize to the range [0,1]
        gvfMin = gvfBuffer[0];
        gvfMax = gvfBuffer[0];

        for (i = 1; i < length; i++) {

            if (gvfBuffer[i] > gvfMax) {
                gvfMax = gvfBuffer[i];
            }

            if (gvfBuffer[i] < gvfMin) {
                gvfMin = gvfBuffer[i];
            }
        }

        for (i = 0; i < length; i++) {
            gvfBuffer[i] = (gvfBuffer[i] - gvfMin) / (gvfMax - gvfMin);
        }


        // Take care of the boundary condition
        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    expGvfBuffer[(x + 1) + ((y + 1) * (xDim + 2)) + ((z + 1) * expSliceSize)] = gvfBuffer[x +
                                                                                                          (y * xDim) +
                                                                                                          (z *
                                                                                                               sliceSize)];
                }
            }
        }

        // Create a mirror at the 8 corner points
        expGvfBuffer[0] = gvfBuffer[1 + xDim + sliceSize];
        expGvfBuffer[xDim + 1] = gvfBuffer[(xDim - 2) + xDim + sliceSize];
        expGvfBuffer[(yDim + 1) * (xDim + 2)] = gvfBuffer[((yDim - 2) * xDim) + 1 + sliceSize];
        expGvfBuffer[((xDim + 2) * (yDim + 2)) - 1] = gvfBuffer[((yDim - 2) * xDim) + xDim - 2 + sliceSize];
        expGvfBuffer[(xDim + 2) * (yDim + 2) * (zDim + 1)] = gvfBuffer[1 + xDim + (sliceSize * (zDim - 2))];
        expGvfBuffer[((xDim + 2) * (yDim + 2) * (zDim + 1)) + xDim + 1] = gvfBuffer[(xDim - 2) + xDim +
                                                                                    (sliceSize * (zDim - 2))];
        expGvfBuffer[((xDim + 2) * (yDim + 2) * (zDim + 1)) + ((yDim + 1) * (xDim + 2))] = gvfBuffer[((yDim - 2) *
                                                                                                          xDim) + 1 +
                                                                                                     (sliceSize *
                                                                                                          (zDim - 2))];
        expGvfBuffer[((xDim + 2) * (yDim + 2) * (zDim + 1)) + ((xDim + 2) * (yDim + 2)) - 1] = gvfBuffer[((yDim - 2) *
                                                                                                              xDim) +
                                                                                                         xDim - 2 +
                                                                                                         (sliceSize *
                                                                                                              (zDim -
                                                                                                                   2))];

        // Create a mirror at the corner line segments from x = 1 to x = xDim slices
        for (x = 0; x < xDim; x++) {
            expGvfBuffer[x + 1] = gvfBuffer[x + xDim + sliceSize];
            expGvfBuffer[x + 1 + ((yDim + 1) * (xDim + 2))] = gvfBuffer[x + ((yDim - 2) * xDim) + sliceSize];
            expGvfBuffer[x + 1 + ((zDim + 1) * expSliceSize)] = gvfBuffer[x + xDim + ((zDim - 2) * sliceSize)];
            expGvfBuffer[x + 1 + ((yDim + 1) * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = gvfBuffer[x +
                                                                                                      ((yDim - 2) *
                                                                                                           xDim) +
                                                                                                      ((zDim - 2) *
                                                                                                           sliceSize)];
        }

        // Create a mirror at the corner line segments from y = 1 to y = yDim slices
        for (y = 0; y < yDim; y++) {
            expGvfBuffer[(y + 1) * (xDim + 2)] = gvfBuffer[1 + (y * xDim) + sliceSize];
            expGvfBuffer[xDim + 1 + ((y + 1) * (xDim + 2))] = gvfBuffer[xDim - 2 + (y * xDim) + sliceSize];
            expGvfBuffer[((y + 1) * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = gvfBuffer[1 + (y * xDim) +
                                                                                           ((zDim - 2) * sliceSize)];
            expGvfBuffer[xDim + 1 + ((y + 1) * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = gvfBuffer[xDim - 2 +
                                                                                                      (y * xDim) +
                                                                                                      ((zDim - 2) *
                                                                                                           sliceSize)];
        }

        // Create a mirror at the corner line segments from z = 1 to z = zDim slices
        for (z = 0; z < zDim; z++) {
            expGvfBuffer[(z + 1) * expSliceSize] = gvfBuffer[1 + xDim + (z * sliceSize)];
            expGvfBuffer[xDim + 1 + ((z + 1) * expSliceSize)] = gvfBuffer[(xDim - 2) + xDim + (z * sliceSize)];
            expGvfBuffer[((yDim + 1) * (xDim + 2)) + ((z + 1) * expSliceSize)] = gvfBuffer[((yDim - 2) * xDim) + 1 +
                                                                                           (z * sliceSize)];
            expGvfBuffer[((xDim + 2) * (yDim + 2)) - 1 + ((z + 1) * expSliceSize)] = gvfBuffer[((yDim - 2) * xDim) +
                                                                                               xDim - 2 +
                                                                                               (z * sliceSize)];
        }

        // Mirror left and right x boundaries
        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {
                expGvfBuffer[((y + 1) * (xDim + 2)) + ((z + 1) * expSliceSize)] = gvfBuffer[(z * sliceSize) +
                                                                                            (y * xDim) + 1];
                expGvfBuffer[((y + 1) * (xDim + 2)) + xDim + 1 + ((z + 1) * expSliceSize)] = gvfBuffer[(z * sliceSize) +
                                                                                                       (y * xDim) +
                                                                                                       xDim - 2];
            }
        }

        // Mirror top and bottom y boundaries
        for (z = 0; z < zDim; z++) {

            for (x = 0; x < xDim; x++) {
                expGvfBuffer[x + 1 + ((z + 1) * expSliceSize)] = gvfBuffer[(z * sliceSize) + xDim + x];
                expGvfBuffer[((yDim + 1) * (xDim + 2)) + x + 1 + ((z + 1) * expSliceSize)] = gvfBuffer[(z * sliceSize) +
                                                                                                       ((yDim - 2) *
                                                                                                            xDim) + x];
            }
        }

        // Mirror front and back z boundaries
        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                expGvfBuffer[x + 1 + ((y + 1) * (xDim + 2))] = gvfBuffer[sliceSize + (y * xDim) + x];
                expGvfBuffer[x + 1 + ((y + 1) * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = gvfBuffer[((zDim - 2) *
                                                                                                            sliceSize) +
                                                                                                       (y * xDim) + x];
            }
        }

        // Calculate the x and y and z gradients
        for (z = 0; z < (zDim + 2); z++) {

            for (y = 0; y < (yDim + 2); y++) {

                for (x = 0; x < (xDim + 2); x++) {

                    if (x == 0) {
                        fx[(y * (xDim + 2)) + (z * expSliceSize)] = expGvfBuffer[1 + (y * (xDim + 2)) +
                                                                                 (z * expSliceSize)] -
                                                                    expGvfBuffer[(y * (xDim + 2)) + (z * expSliceSize)];
                    } else if (x == (xDim + 1)) {
                        fx[xDim + 1 + (y * (xDim + 2)) + (z * expSliceSize)] = expGvfBuffer[xDim + 1 +
                                                                                            (y * (xDim + 2)) +
                                                                                            (z * expSliceSize)] -
                                                                               expGvfBuffer[xDim + (y * (xDim + 2)) +
                                                                                            (z * expSliceSize)];
                    } else {
                        fx[x + (y * (xDim + 2)) + (z * expSliceSize)] = (expGvfBuffer[(x + 1) + (y * (xDim + 2)) +
                                                                                      (z * expSliceSize)] -
                                                                         expGvfBuffer[(x - 1) + (y * (xDim + 2)) +
                                                                                      (z * expSliceSize)]) / 2.0f;
                    }

                    if (y == 0) {
                        fy[x + (z * expSliceSize)] = expGvfBuffer[x + (xDim + 2) + (z * expSliceSize)] -
                                                     expGvfBuffer[x + (z * expSliceSize)];
                    } else if (y == (yDim + 1)) {
                        fy[x + ((yDim + 1) * (xDim + 2)) + (z * expSliceSize)] = expGvfBuffer[x +
                                                                                              ((yDim + 1) *
                                                                                                   (xDim + 2)) +
                                                                                              (z * expSliceSize)] -
                                                                                 expGvfBuffer[x + (yDim * (xDim + 2)) +
                                                                                              (z * expSliceSize)];
                    } else {
                        fy[x + (y * (xDim + 2)) + (z * expSliceSize)] = (expGvfBuffer[x + ((y + 1) * (xDim + 2)) +
                                                                                      (z * expSliceSize)] -
                                                                         expGvfBuffer[x + ((y - 1) * (xDim + 2)) +
                                                                                      (z * expSliceSize)]) / 2.0f;
                    }

                    if (z == 0) {
                        fz[x + (y * (xDim + 2))] = expGvfBuffer[x + (y * (xDim + 2)) + expSliceSize] -
                                                   expGvfBuffer[x + (y * (xDim + 2))];
                    } else if (z == (zDim + 1)) {
                        fz[x + (y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = expGvfBuffer[x + (y * (xDim + 2)) +
                                                                                              ((zDim + 1) *
                                                                                                   expSliceSize)] -
                                                                                 expGvfBuffer[x + (y * (xDim + 2)) +
                                                                                              (zDim * expSliceSize)];
                    } else {
                        fz[x + (y * (xDim + 2)) + (z * expSliceSize)] = (expGvfBuffer[x + (y * (xDim + 2)) +
                                                                                      ((z + 1) * expSliceSize)] -
                                                                         expGvfBuffer[x + (y * (xDim + 2)) +
                                                                                      ((z - 1) * expSliceSize)]) / 2.0f;
                    }
                } // for (x = 0; x < (xDim+2); x++)
            } // for (y = 0; y < (yDim+2); y++)
        } // for (z = 0; z < (zDim+2); z++)

        // Initialize GVF to the gradient
        for (i = 0; i < fx.length; i++) {
            uVal[i] = fx[i];
            vVal[i] = fy[i];
            wVal[i] = fz[i];
        }

        for (i = 0; i < fx.length; i++) {
            gVal[i] = (float) Math.exp(-((fx[i] * fx[i]) + (fy[i] * fy[i]) + (fz[i] * fz[i])) / (kValue * kValue));
        }

        // gvfIterations = 200;
        for (iteration = 0; (iteration < gvfIterations) && (!threadStopped); iteration++) {

            // Create a mirror at the u boundaries
            // Create a mirror at the 8 corner points of the cube
            fireProgressStateChanged((iteration * 100) / gvfIterations);
            uVal[0] = uVal[2 + (2 * (xDim + 2)) + (2 * expSliceSize)];
            uVal[xDim + 1] = uVal[(xDim - 1) + (2 * (xDim + 2)) + (2 * expSliceSize)];
            uVal[(yDim + 1) * (xDim + 2)] = uVal[((yDim - 1) * (xDim + 2)) + 2 + (2 * expSliceSize)];
            uVal[((xDim + 2) * (yDim + 2)) - 1] = uVal[((yDim - 1) * (xDim + 2)) + xDim - 1 + (2 * expSliceSize)];
            uVal[(zDim + 1) * expSliceSize] = uVal[2 + (2 * (xDim + 2)) + ((zDim - 1) * expSliceSize)];
            uVal[((zDim + 1) * expSliceSize) + xDim + 1] = uVal[(xDim - 1) + (2 * (xDim + 2)) +
                                                                ((zDim - 1) * expSliceSize)];
            uVal[((zDim + 1) * expSliceSize) + ((yDim + 1) * (xDim + 2))] = uVal[((yDim - 1) * (xDim + 2)) + 2 +
                                                                                 ((zDim - 1) * expSliceSize)];
            uVal[((zDim + 1) * expSliceSize) + ((xDim + 2) * (yDim + 2)) - 1] = uVal[((yDim - 1) * (xDim + 2)) + xDim -
                                                                                     1 + ((zDim - 1) * expSliceSize)];

            // Create a mirror at the corner line segments from x = 1 to x = xDim
            for (x = 1; x <= xDim; x++) {
                uVal[x] = uVal[x + (2 * (xDim + 2)) + (2 * expSliceSize)];
                uVal[x + ((yDim + 1) * (xDim + 2))] = uVal[x + ((yDim - 1) * (xDim + 2)) + (2 * expSliceSize)];
                uVal[x + ((zDim + 1) * expSliceSize)] = uVal[x + (2 * (xDim + 2)) + ((zDim - 1) * expSliceSize)];
                uVal[x + ((yDim + 1) * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = uVal[x + ((yDim - 1) * (xDim + 2)) +
                                                                                         ((zDim - 1) * expSliceSize)];
            }

            // Create a mirror at the corner line segments from y = 1 to y = yDim
            for (y = 1; y <= yDim; y++) {
                uVal[y * (xDim + 2)] = uVal[2 + (y * (xDim + 2)) + (2 * expSliceSize)];
                uVal[xDim + 1 + (y * (xDim + 2))] = uVal[xDim - 2 + (y * (xDim + 2)) + (2 * expSliceSize)];
                uVal[(y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = uVal[2 + (y * (xDim + 2)) +
                                                                            ((zDim - 1) * expSliceSize)];
                uVal[xDim + 1 + (y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = uVal[xDim - 1 + (y * (xDim + 2)) +
                                                                                       ((zDim - 1) * expSliceSize)];
            }

            // Create a mirror at the corner line segments from z = 1 to z = zDim
            for (z = 1; z <= zDim; z++) {
                uVal[z * expSliceSize] = uVal[2 + (2 * (xDim + 2)) + (z * expSliceSize)];
                uVal[(z * expSliceSize) + xDim + 1] = uVal[(xDim - 1) + (2 * (xDim + 2)) + (z * expSliceSize)];
                uVal[(z * expSliceSize) + ((yDim + 1) * (xDim + 2))] = uVal[((yDim - 1) * (xDim + 2)) + 2 +
                                                                            (z * expSliceSize)];
                uVal[(z * expSliceSize) + ((xDim + 2) * (yDim + 2)) - 1] = uVal[((yDim - 1) * (xDim + 2)) + xDim - 1 +
                                                                                (z * expSliceSize)];
            }

            // Mirror left and right x boundaries for z = 1 to z = zDim
            for (z = 1; z < (zDim + 1); z++) {

                for (y = 1; y < (yDim + 1); y++) {
                    uVal[(z * expSliceSize) + (y * (xDim + 2))] = uVal[(z * expSliceSize) + (y * (xDim + 2)) + 2];
                    uVal[(z * expSliceSize) + (y * (xDim + 2)) + xDim + 1] = uVal[(z * expSliceSize) +
                                                                                  (y * (xDim + 2)) + xDim - 1];
                }
            }

            // Mirror top and bottom y boundaries
            for (z = 1; z < (zDim + 1); z++) {

                for (x = 1; x < (xDim + 1); x++) {
                    uVal[(z * expSliceSize) + x] = uVal[(z * expSliceSize) + (2 * (xDim + 2)) + x];
                    uVal[(z * expSliceSize) + ((yDim + 1) * (xDim + 2)) + x] = uVal[(z * expSliceSize) +
                                                                                    ((yDim - 1) * (xDim + 2)) + x];
                }
            }

            // Mirror front and back z boundaries
            for (y = 1; y < (yDim + 1); y++) {

                for (x = 1; x < (xDim + 1); x++) {
                    uVal[x + (y * (xDim + 2))] = uVal[x + (y * (xDim + 2)) + (2 * expSliceSize)];
                    uVal[x + (y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = uVal[x + (y * (xDim + 2)) +
                                                                                    ((zDim - 1) * expSliceSize)];
                }
            }

            // Create a mirror at the v boundaries
            // Create a mirror at the 8 corners
            vVal[0] = vVal[2 + (2 * (xDim + 2)) + (2 * expSliceSize)];
            vVal[xDim + 1] = vVal[(xDim - 1) + (2 * (xDim + 2)) + (2 * expSliceSize)];
            vVal[(yDim + 1) * (xDim + 2)] = vVal[((yDim - 1) * (xDim + 2)) + 2 + (2 * expSliceSize)];
            vVal[((xDim + 2) * (yDim + 2)) - 1] = vVal[((yDim - 1) * (xDim + 2)) + xDim - 1 + (2 * expSliceSize)];
            vVal[(zDim + 1) * expSliceSize] = vVal[2 + (2 * (xDim + 2)) + ((zDim - 1) * expSliceSize)];
            vVal[((zDim + 1) * expSliceSize) + xDim + 1] = vVal[(xDim - 1) + (2 * (xDim + 2)) +
                                                                ((zDim - 1) * expSliceSize)];
            vVal[((zDim + 1) * expSliceSize) + ((yDim + 1) * (xDim + 2))] = vVal[((yDim - 1) * (xDim + 2)) + 2 +
                                                                                 ((zDim - 1) * expSliceSize)];
            vVal[((zDim + 1) * expSliceSize) + ((xDim + 2) * (yDim + 2)) - 1] = vVal[((yDim - 1) * (xDim + 2)) + xDim -
                                                                                     1 + ((zDim - 1) * expSliceSize)];

            // Create a mirror at the corner line segments from x = 1 to x = xDim
            for (x = 1; x <= xDim; x++) {
                vVal[x] = vVal[x + (2 * (xDim + 2)) + (2 * expSliceSize)];
                vVal[x + ((yDim + 1) * (xDim + 2))] = vVal[x + ((yDim - 1) * (xDim + 2)) + (2 * expSliceSize)];
                vVal[x + ((zDim + 1) * expSliceSize)] = vVal[x + (2 * (xDim + 2)) + ((zDim - 1) * expSliceSize)];
                vVal[x + ((yDim + 1) * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = vVal[x + ((yDim - 1) * (xDim + 2)) +
                                                                                         ((zDim - 1) * expSliceSize)];
            }

            // Create a mirror at the corner line segments from y = 1 to y = yDim
            for (y = 1; y <= yDim; y++) {
                vVal[y * (xDim + 2)] = vVal[2 + (y * (xDim + 2)) + (2 * expSliceSize)];
                vVal[xDim + 1 + (y * (xDim + 2))] = vVal[xDim - 2 + (y * (xDim + 2)) + (2 * expSliceSize)];
                vVal[(y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = vVal[2 + (y * (xDim + 2)) +
                                                                            ((zDim - 1) * expSliceSize)];
                vVal[xDim + 1 + (y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = vVal[xDim - 1 + (y * (xDim + 2)) +
                                                                                       ((zDim - 1) * expSliceSize)];
            }

            // Create a mirror at the corner line segments from z = 1 to z = zDim
            for (z = 1; z <= zDim; z++) {
                vVal[z * expSliceSize] = vVal[2 + (2 * (xDim + 2)) + (z * expSliceSize)];
                vVal[(z * expSliceSize) + xDim + 1] = vVal[(xDim - 1) + (2 * (xDim + 2)) + (z * expSliceSize)];
                vVal[(z * expSliceSize) + ((yDim + 1) * (xDim + 2))] = vVal[((yDim - 1) * (xDim + 2)) + 2 +
                                                                            (z * expSliceSize)];
                vVal[(z * expSliceSize) + ((xDim + 2) * (yDim + 2)) - 1] = vVal[((yDim - 1) * (xDim + 2)) + xDim - 1 +
                                                                                (z * expSliceSize)];
            }

            // Mirror left and right x boundaries
            for (z = 1; z < (zDim + 1); z++) {

                for (y = 1; y < (yDim + 1); y++) {
                    vVal[(z * expSliceSize) + (y * (xDim + 2))] = vVal[(z * expSliceSize) + (y * (xDim + 2)) + 2];
                    vVal[(z * expSliceSize) + (y * (xDim + 2)) + xDim + 1] = vVal[(z * expSliceSize) +
                                                                                  (y * (xDim + 2)) + xDim - 1];
                }
            }

            // Mirror top and bottom y boundaries
            for (z = 1; z < (zDim + 1); z++) {

                for (x = 1; x < (xDim + 1); x++) {
                    vVal[(z * expSliceSize) + x] = vVal[(z * expSliceSize) + (2 * (xDim + 2)) + x];
                    vVal[(z * expSliceSize) + ((yDim + 1) * (xDim + 2)) + x] = vVal[(z * expSliceSize) +
                                                                                    ((yDim - 1) * (xDim + 2)) + x];
                }
            }

            // Mirror front and back z boundaries
            for (y = 1; y < (yDim + 1); y++) {

                for (x = 1; x < (xDim + 1); x++) {
                    vVal[x + (y * (xDim + 2))] = vVal[x + (y * (xDim + 2)) + (2 * expSliceSize)];
                    vVal[x + (y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = vVal[x + (y * (xDim + 2)) +
                                                                                    ((zDim - 1) * expSliceSize)];
                }
            }

            // Create a mirror at the w boundaries
            // Create a mirror at the 8 corner points
            wVal[0] = wVal[2 + (2 * (xDim + 2)) + (2 * expSliceSize)];
            wVal[xDim + 1] = wVal[(xDim - 1) + (2 * (xDim + 2)) + (2 * expSliceSize)];
            wVal[(yDim + 1) * (xDim + 2)] = wVal[((yDim - 1) * (xDim + 2)) + 2 + (2 * expSliceSize)];
            wVal[((xDim + 2) * (yDim + 2)) - 1] = wVal[((yDim - 1) * (xDim + 2)) + xDim - 1 + (2 * expSliceSize)];
            wVal[(zDim + 1) * expSliceSize] = wVal[2 + (2 * (xDim + 2)) + ((zDim - 1) * expSliceSize)];
            wVal[((zDim + 1) * expSliceSize) + xDim + 1] = wVal[(xDim - 1) + (2 * (xDim + 2)) +
                                                                ((zDim - 1) * expSliceSize)];
            wVal[((zDim + 1) * expSliceSize) + ((yDim + 1) * (xDim + 2))] = wVal[((yDim - 1) * (xDim + 2)) + 2 +
                                                                                 ((zDim - 1) * expSliceSize)];
            wVal[((zDim + 1) * expSliceSize) + ((xDim + 2) * (yDim + 2)) - 1] = wVal[((yDim - 1) * (xDim + 2)) + xDim -
                                                                                     1 + ((zDim - 1) * expSliceSize)];

            // Create a mirror at the corner line segments from x = 1 to x = xDim
            for (x = 1; x <= xDim; x++) {
                wVal[x] = wVal[x + (2 * (xDim + 2)) + (2 * expSliceSize)];
                wVal[x + ((yDim + 1) * (xDim + 2))] = wVal[x + ((yDim - 1) * (xDim + 2)) + (2 * expSliceSize)];
                wVal[x + ((zDim + 1) * expSliceSize)] = wVal[x + (2 * (xDim + 2)) + ((zDim - 1) * expSliceSize)];
                wVal[x + ((yDim + 1) * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = wVal[x + ((yDim - 1) * (xDim + 2)) +
                                                                                         ((zDim - 1) * expSliceSize)];
            }

            // Create a mirror at the corner line segments from y = 1 to y = yDim
            for (y = 1; y <= yDim; y++) {
                wVal[y * (xDim + 2)] = wVal[2 + (y * (xDim + 2)) + (2 * expSliceSize)];
                wVal[xDim + 1 + (y * (xDim + 2))] = wVal[xDim - 2 + (y * (xDim + 2)) + (2 * expSliceSize)];
                wVal[(y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = wVal[2 + (y * (xDim + 2)) +
                                                                            ((zDim - 1) * expSliceSize)];
                wVal[xDim + 1 + (y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = wVal[xDim - 1 + (y * (xDim + 2)) +
                                                                                       ((zDim - 1) * expSliceSize)];
            }

            // Create a mirror at the corner line segments for z = 1 to z = zDim
            for (z = 1; z <= zDim; z++) {
                wVal[z * expSliceSize] = wVal[2 + (2 * (xDim + 2)) + (z * expSliceSize)];
                wVal[(z * expSliceSize) + xDim + 1] = wVal[(xDim - 1) + (2 * (xDim + 2)) + (z * expSliceSize)];
                wVal[(z * expSliceSize) + ((yDim + 1) * (xDim + 2))] = wVal[((yDim - 1) * (xDim + 2)) + 2 +
                                                                            (z * expSliceSize)];
                wVal[(z * expSliceSize) + ((xDim + 2) * (yDim + 2)) - 1] = wVal[((yDim - 1) * (xDim + 2)) + xDim - 1 +
                                                                                (z * expSliceSize)];
            }

            // Mirror left and right x boundaries
            for (z = 1; z < (zDim + 1); z++) {

                for (y = 1; y < (yDim + 1); y++) {
                    wVal[(z * expSliceSize) + (y * (xDim + 2))] = wVal[(z * expSliceSize) + (y * (xDim + 2)) + 2];
                    wVal[(z * expSliceSize) + (y * (xDim + 2)) + xDim + 1] = wVal[(z * expSliceSize) +
                                                                                  (y * (xDim + 2)) + xDim - 1];
                }
            }

            // Mirror top and bottom y boundaries
            for (z = 1; z < (zDim + 1); z++) {

                for (x = 1; x < (xDim + 1); x++) {
                    wVal[(z * expSliceSize) + x] = wVal[(z * expSliceSize) + (2 * (xDim + 2)) + x];
                    wVal[(z * expSliceSize) + ((yDim + 1) * (xDim + 2)) + x] = wVal[(z * expSliceSize) +
                                                                                    ((yDim - 1) * (xDim + 2)) + x];
                }
            }

            // Mirror front and back z boundaries
            for (y = 1; y < (yDim + 1); y++) {

                for (x = 1; x < (xDim + 1); x++) {
                    wVal[x + (y * (xDim + 2))] = wVal[x + (y * (xDim + 2)) + (2 * expSliceSize)];
                    wVal[x + (y * (xDim + 2)) + ((zDim + 1) * expSliceSize)] = wVal[x + (y * (xDim + 2)) +
                                                                                    ((zDim - 1) * expSliceSize)];
                }
            }

            for (z = 0; z < (zDim + 2); z++) {
                i1 = z * expSliceSize;

                for (y = 0; y < (yDim + 2); y++) {
                    i2 = i1 + (y * (xDim + 2));

                    for (x = 0; x < (xDim + 2); x++) {
                        del2 = 0.0f;
                        i = x + i2;

                        if (x == 0) {
                            del2 += 2 * uVal[(x + 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        } else if (x == (xDim + 1)) {
                            del2 += 2 * uVal[(x - 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        } else {
                            del2 += uVal[(x + 1) + (y * (xDim + 2)) + (z * expSliceSize)] +
                                    uVal[(x - 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        }

                        if (y == 0) {
                            del2 += 2 * uVal[x + ((y + 1) * (xDim + 2)) + (z * expSliceSize)];
                        } else if (y == (yDim + 1)) {
                            del2 += 2 * uVal[x + ((y - 1) * (xDim + 2)) + (z * expSliceSize)];
                        } else {
                            del2 += uVal[x + ((y + 1) * (xDim + 2)) + (z * expSliceSize)] +
                                    uVal[x + ((y - 1) * (xDim + 2)) + (z * expSliceSize)];

                        }

                        if (z == 0) {
                            del2 += 2 * uVal[x + (y * (xDim + 2)) + ((z + 1) * expSliceSize)];
                        } else if (z == (zDim + 1)) {
                            del2 += 2 * uVal[x + (y * (xDim + 2)) + ((z - 1) * expSliceSize)];
                        } else {
                            del2 += uVal[x + (y * (xDim + 2)) + ((z + 1) * expSliceSize)] + +uVal[x + (y * (xDim + 2)) +
                                                                                                  ((z - 1) *
                                                                                                       expSliceSize)];
                        }

                        del2 -= 6 * uVal[i];
                        uVal[i] += 0.1666f * ((gVal[i] * del2) - ((1 - gVal[i]) * (uVal[i] - fx[i])));

                        del2 = 0.0f;

                        if (x == 0) {
                            del2 += 2 * vVal[(x + 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        } else if (x == (xDim + 1)) {
                            del2 += 2 * vVal[(x - 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        } else {
                            del2 += vVal[(x + 1) + (y * (xDim + 2)) + (z * expSliceSize)] +
                                    vVal[(x - 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        }

                        if (y == 0) {
                            del2 += 2 * vVal[x + ((y + 1) * (xDim + 2)) + (z * expSliceSize)];
                        } else if (y == (yDim + 1)) {
                            del2 += 2 * vVal[x + ((y - 1) * (xDim + 2)) + (z * expSliceSize)];
                        } else {
                            del2 += vVal[x + ((y + 1) * (xDim + 2)) + (z * expSliceSize)] +
                                    vVal[x + ((y - 1) * (xDim + 2)) + (z * expSliceSize)];
                        }

                        if (z == 0) {
                            del2 += 2 * vVal[x + (y * (xDim + 2)) + ((z + 1) * expSliceSize)];
                        } else if (z == (zDim + 1)) {
                            del2 += 2 * vVal[x + (y * (xDim + 2)) + ((z - 1) * expSliceSize)];
                        } else {
                            del2 += vVal[x + (y * (xDim + 2)) + ((z + 1) * expSliceSize)] + +vVal[x + (y * (xDim + 2)) +
                                                                                                  ((z - 1) *
                                                                                                       expSliceSize)];
                        }

                        del2 -= 6 * vVal[i];
                        vVal[i] += 0.1666f * ((gVal[i] * del2) - ((1 - gVal[i]) * (vVal[i] - fy[i])));

                        del2 = 0.0f;

                        if (x == 0) {
                            del2 += 2 * wVal[(x + 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        } else if (x == (xDim + 1)) {
                            del2 += 2 * wVal[(x - 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        } else {
                            del2 += wVal[(x + 1) + (y * (xDim + 2)) + (z * expSliceSize)] +
                                    wVal[(x - 1) + (y * (xDim + 2)) + (z * expSliceSize)];
                        }

                        if (y == 0) {
                            del2 += 2 * wVal[x + ((y + 1) * (xDim + 2)) + (z * expSliceSize)];
                        } else if (y == (yDim + 1)) {
                            del2 += 2 * wVal[x + ((y - 1) * (xDim + 2)) + (z * expSliceSize)];
                        } else {
                            del2 += wVal[x + ((y + 1) * (xDim + 2)) + (z * expSliceSize)] +
                                    wVal[x + ((y - 1) * (xDim + 2)) + (z * expSliceSize)];
                        }

                        if (z == 0) {
                            del2 += 2 * wVal[x + (y * (xDim + 2)) + ((z + 1) * expSliceSize)];
                        } else if (z == (zDim + 1)) {
                            del2 += 2 * wVal[x + (y * (xDim + 2)) + ((z - 1) * expSliceSize)];
                        } else {
                            del2 += wVal[x + (y * (xDim + 2)) + ((z + 1) * expSliceSize)] + +wVal[x + (y * (xDim + 2)) +
                                                                                                  ((z - 1) *
                                                                                                       expSliceSize)];
                        }

                        del2 -= 6 * wVal[i];
                        wVal[i] += 0.1666f * ((gVal[i] * del2) - ((1 - gVal[i]) * (wVal[i] - fz[i])));
                    }
                }
            }
        } // for (iteration = 0; iteration < gvfIterations && (!threadStopped); iteration++)

        if (threadStopped) {

            setCompleted(false);
            finalize();

            return;
        }

        for (z = 1; z < (zDim + 1); z++) {

            for (y = 1; y < (yDim + 1); y++) {

                for (x = 1; x < (xDim + 1); x++) {
                    gvfBuffer[(x - 1) + ((y - 1) * xDim) + ((z - 1) * sliceSize)] = uVal[x + (y * (xDim + 2)) +
                                                                                         (z * expSliceSize)];
                }
            }
        }

        uVal = new float[length];

        for (i = 0; i < length; i++) {
            uVal[i] = gvfBuffer[i];
        }

        for (z = 1; z < (zDim + 1); z++) {

            for (y = 1; y < (yDim + 1); y++) {

                for (x = 1; x < (xDim + 1); x++) {
                    gvfBuffer[(x - 1) + ((y - 1) * xDim) + ((z - 1) * sliceSize)] = vVal[x + (y * (xDim + 2)) +
                                                                                         (z * expSliceSize)];
                }
            }
        }

        vVal = new float[length];

        for (i = 0; i < length; i++) {
            vVal[i] = gvfBuffer[i];
        }

        for (z = 1; z < (zDim + 1); z++) {

            for (y = 1; y < (yDim + 1); y++) {

                for (x = 1; x < (xDim + 1); x++) {
                    gvfBuffer[(x - 1) + ((y - 1) * xDim) + ((z - 1) * sliceSize)] = wVal[x + (y * (xDim + 2)) +
                                                                                         (z * expSliceSize)];
                }
            }
        }

        wVal = new float[length];

        for (i = 0; i < length; i++) {
            wVal[i] = gvfBuffer[i];
        }

        if (saveGVF) {
            ModelImage gvfImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), image.getImageName() + "_uvf");

            try {
                gvfImage.importData(0, uVal, true);
            } catch (IOException error) {

                if (gvfImage != null) {
                    gvfImage.disposeLocal();
                }

                gvfImage = null;
                MipavUtil.displayError("Error on gvfImage.importData");

                setCompleted(false);

                return;
            }

            try {
                gvfImage.saveImage(image.getFileInfo(0).getFileDirectory(), image.getImageName() + "_uvf",
                                   FileUtility.XML, true);
            } catch (OutOfMemoryError error) {

                if (gvfImage != null) {
                    gvfImage.disposeLocal();
                }

                gvfImage = null;
                MipavUtil.displayError("Error on gvfImage.saveImage");

                setCompleted(false);

                return;
            }

            try {
                gvfImage.importData(0, vVal, true);
            } catch (IOException error) {

                if (gvfImage != null) {
                    gvfImage.disposeLocal();
                }

                gvfImage = null;
                MipavUtil.displayError("Error on gvfImage.importData");

                setCompleted(false);

                return;
            }

            try {
                gvfImage.saveImage(image.getFileInfo(0).getFileDirectory(), image.getImageName() + "_vvf",
                                   FileUtility.XML, true);
            } catch (OutOfMemoryError error) {

                if (gvfImage != null) {
                    gvfImage.disposeLocal();
                }

                gvfImage = null;
                MipavUtil.displayError("Error on gvfImage.saveImage");

                setCompleted(false);

                return;
            }

            try {
                gvfImage.importData(0, wVal, true);
            } catch (IOException error) {

                if (gvfImage != null) {
                    gvfImage.disposeLocal();
                }

                gvfImage = null;
                MipavUtil.displayError("Error on gvfImage.importData");

                setCompleted(false);

                return;
            }

            try {
                gvfImage.saveImage(image.getFileInfo(0).getFileDirectory(), image.getImageName() + "_wvf",
                                   FileUtility.XML, true);
            } catch (OutOfMemoryError error) {

                if (gvfImage != null) {
                    gvfImage.disposeLocal();
                }

                gvfImage = null;
                MipavUtil.displayError("Error on gvfImage.saveImage");

                setCompleted(false);

                return;
            }

            gvfImage.disposeLocal();
            gvfImage = null;
        } // if (saveGVF)


        return;
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmConvolver) {
            AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            outputBuffer = convolver.getOutputBuffer();
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * A representation of an edge for the vertex-edge-triangle table. This class stores the pair of vertex indices for
     * the end points of the edge. The edges <V0,V1> and <V1,V0> are considered to be identical. To simplify
     * comparisons, the class stores the ordered indices. The class extends Object to obtain support for hashing into a
     * map of edges.
     */
    protected class Edge extends Object {

        /** DOCUMENT ME! */
        public int m_i0, m_i1;

        /**
         * Constructs an edge in the table.
         *
         * @param  i0  a vertex index for an end point
         * @param  i1  a vertex index for an end point
         */
        public Edge(int i0, int i1) {

            if (i0 < i1) {

                // i0 is minimum
                m_i0 = i0;
                m_i1 = i1;
            } else {

                // i1 is minimum
                m_i0 = i1;
                m_i1 = i0;
            }
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @param   kObject  an edge for comparison to the current one
         *
         * @return  true iff the edges are identical. Because the class stores ordered indices, it is not necessary to
         *          use the more expensive test (i0 == other.i0 && i1 == other.i1) || (i0 == other.i1 && i1 ==
         *          other.i0).
         */
        public boolean equals(Object kObject) {
            Edge kE = (Edge) kObject;

            return (m_i0 == kE.m_i0) && (m_i1 == kE.m_i1);
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @return  the hash key for the edge
         */
        public int hashCode() {
            return (m_i0 << 16) | m_i1;
        }
    }


    /**
     * An unordered set of 'int' stored in an array. The class is used to store adjacency information for the triangles
     * in the mesh representing the object surface. The reason for using an array is to minimize reallocations during
     * dynamic changes to a mesh. When an item is deleted from the set, the last element in the array is moved into that
     * location. The sets for which this class is used are typically small, so the costs for searching the unordered
     * items are not a factor.
     *
     * <p>The class has a static value DEFAULT_GROW that is used to increase the number of elements when a reallocation
     * must occur. The new storage size is the current maximum quantity plus the growth value.</p>
     */

    private class UnorderedSetInt {

        /** The array storage for the set. */
        protected int[] m_aiElement;

        /** On a reallocation, the old maximum quantity is incremented by this value. */
        protected int m_iGrow;

        /** The maximum number of elements in the array. It is always the case that m_iQuantity <= m_iMaxQuantity. */
        protected int m_iMaxQuantity;

        /** Support for remove and removeAt. */
        protected int m_iOldIndex, m_iNewIndex;

        /** The number of valid elements in the array. The valid indices are 0 <= i < m_iQuantity. */
        protected int m_iQuantity;

        /**
         * The default growth value for reallocations of the array representing the set. The application can change this
         * to whatever is appropriate for its purposes.
         */
        private int DEFAULT_GROW = 8;

        /**
         * Construct an empty unordered set. The initial maximum quantity and growth values are DEFAULT_GROW. When
         */
        @SuppressWarnings("unused")
        public UnorderedSetInt() {
            reset();
        }

        /**
         * Create an unordered set that is a deep copy of the input set.
         *
         * @param  kSet  The input set to copy.
         */
        @SuppressWarnings("unused")
        public UnorderedSetInt(UnorderedSetInt kSet) {
            copy(kSet);
        }

        /**
         * Construct an empty unordered set with the specified maximum quantity and growth values.
         *
         * @param  iMaxQuantity  The initial number of elements in the array. If the value is nonpositive, the initial
         *                       number is DEFAULT_GROW.
         * @param  iGrow         The growth amount for a reallocation. If a reallocation occurs, the new number of
         *                       elements is the current maximum quantity plus the growth value. If the input value is
         *                       nonpositive, the growth is set to DEFAULT_GROW.
         */
        public UnorderedSetInt(int iMaxQuantity, int iGrow) {
            reset(iMaxQuantity, iGrow);
        }

        /**
         * Append an element to the end of the storage array.
         *
         * @param   iElement  The element to append.
         *
         * @return  The array location that contains the newly appended element. A side effect of this call is
         *          reallocation of the storage array, if necessary.
         */
        @SuppressWarnings("unused")
        public int append(int iElement) {

            if (m_iQuantity == m_iMaxQuantity) {
                int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
                int[] aiNewElement = new int[iNewMaxQuantity];
                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
                m_iMaxQuantity = iNewMaxQuantity;
                m_aiElement = aiNewElement;
            }

            int iLocation = m_iQuantity++;
            m_aiElement[iLocation] = iElement;

            return iLocation;
        }

        /**
         * Use exactly the amount of array storage for the current elements in the set. After the call, getQuantity()
         * and getMaximumQuantity() return the same value. This call does cause a reallocation.
         */
        @SuppressWarnings("unused")
        public void compactify() {

            if (m_iQuantity > 0) {

                // Try Catch - Matt
                int[] aiNewElement = new int[m_iQuantity];
                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iQuantity);
                m_iMaxQuantity = m_iQuantity;
                m_aiElement = aiNewElement;
            } else {
                reset();
            }
        }

        /**
         * Make a deep copy of the input set.
         *
         * @param  kSet  The set to make a deep copy of.
         */
        public void copy(UnorderedSetInt kSet) {
            m_iQuantity = kSet.m_iQuantity;
            m_iMaxQuantity = kSet.m_iMaxQuantity;
            m_iGrow = kSet.m_iGrow;
            m_aiElement = new int[m_iMaxQuantity];
            System.arraycopy(kSet.m_aiElement, 0, m_aiElement, 0, m_iMaxQuantity);
        }

        /**
         * Search the set to see if the input element currently exists.
         *
         * @param   iElement  The element to search for.
         *
         * @return  The value is true if and only if the element is found in the set.
         */
        @SuppressWarnings("unused")
        public boolean exists(int iElement) {

            for (int i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    return true;
                }
            }

            return false;
        }

        /**
         * Retrieve the element in the array location i. It is necessary that 0 <= i < getQuantity() in order to read
         * valid elements.
         *
         * @param   i  The array location whose element is to be retrieved.
         *
         * @return  The element in array location i.
         */
        public final int get(int i) {
            return m_aiElement[i];
        }

        /**
         * The growth value for reallocations. If a reallocation must occur, the new maximum quantity is the current
         * maximum quantity plus the growth amount.
         *
         * @return  The growth value.
         */
        @SuppressWarnings("unused")
        public final int getGrow() {
            return m_iGrow;
        }

        /**
         * The maximum quantity of elements in the set. Not all elements are necessarily used. The used quantity is
         * provided by getQuantity().
         *
         * @return  The maximum quantity of elements in the set.
         */
        @SuppressWarnings("unused")
        public final int getMaxQuantity() {
            return m_iMaxQuantity;
        }

        /**
         * On a call to remove or removeAt, the last element in the array is potentially moved to the array location
         * vacated by the removed element. The new location of the last element is retrived by this function. However,
         * if the last element is the one that was removed, this function returns -1. If you need the value, you must
         * call this function before the next call to remove or removeAt.
         *
         * @return  The new location of the last element that was moved.
         */
        @SuppressWarnings("unused")
        public final int getNewIndex() {
            return m_iNewIndex;
        }

        /**
         * On a call to remove or removeAt, the last element in the array is moved to the array location vacated by the
         * removed element. The old location of the last element is retrived by this function. If you need the value,
         * you must call this function before the next call to remove or removeAt.
         *
         * @return  The old location of the last element that was moved.
         */
        @SuppressWarnings("unused")
        public final int getOldIndex() {
            return m_iOldIndex;
        }

        /**
         * The current number of valid elements in the array. This number is less than or equal to the maximum quantity.
         * The elements with indices 0 through getQuantity()-1 are the valid ones.
         *
         * @return  The current number of valid elements.
         */
        public final int getQuantity() {
            return m_iQuantity;
        }

        /**
         * Insert an element into the set.
         *
         * @param   iElement  The element to insert.
         *
         * @return  The value is true if and only if the element is inserted. The input element is not inserted if it
         *          already exists in the set. A side effect of this call is reallocation of the storage array, if
         *          necessary.
         */
        public boolean insert(int iElement) {
            int i;

            for (i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    return false;
                }
            }

            if (m_iQuantity == m_iMaxQuantity) {
                int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
                int[] aiNewElement = new int[iNewMaxQuantity];
                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
                m_iMaxQuantity = iNewMaxQuantity;
                m_aiElement = aiNewElement;
            }

            m_aiElement[m_iQuantity++] = iElement;

            return true;
        }

        /**
         * Remove the specified element from the set.
         *
         * @param   iElement  The element to remove.
         *
         * @return  The value is true if and only if the element existed and was removed. The last element is
         *          potentially moved into the slot vacated by the specified element. If needed, the old and new
         *          locations of the last element can be retrieved by calls to getOldIndex() and getNewIndex(). If the
         *          last element was the one removed, getNewIndex() returns -1.
         */
        @SuppressWarnings("unused")
        public boolean remove(int iElement) {

            for (int i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    m_iQuantity--;
                    m_iOldIndex = m_iQuantity;

                    if (i != m_iQuantity) {
                        m_aiElement[i] = m_aiElement[m_iQuantity];
                        m_iNewIndex = i;
                    } else {
                        m_iNewIndex = -1;
                    }

                    return true;
                }
            }

            return false;
        }

        /**
         * Remove the element from the set in the specified location.
         *
         * @param   i  The array location whose element is to be removed.
         *
         * @return  The value is true if and only if the input location is within the valid index range 0 <= i <
         *          getQuantity(). The last element is potentially moved into the slot vacated by the specified element.
         *          If needed, the old and new locations of the last element can be retrieved by calls to getOldIndex()
         *          and getNewIndex(). If the last element was the one removed, getNewIndex() returns -1.
         */
        @SuppressWarnings("unused")
        public boolean removeAt(int i) {

            if ((0 <= i) && (i < m_iQuantity)) {
                m_iQuantity--;
                m_iOldIndex = m_iQuantity;

                if (i != m_iQuantity) {
                    m_aiElement[i] = m_aiElement[m_iQuantity];
                    m_iNewIndex = i;
                } else {
                    m_iNewIndex = -1;
                }

                return true;
            }

            return false;
        }

        /**
         * Reset the unordered set to its initial state. The old array is deleted. The new array has a maximum quantity
         * of DEFAULT_GROW and the growth value is DEFAULT_GROW.
         */
        public void reset() {
            reset(0, 0);
        }

        /**
         * Reset the unordered set to the specified state. The old array is deleted. The new array has a maximum
         * quantity and growth value as specified by the inputs.
         *
         * @param  iMaxQuantity  The new maximum quantity for the array.
         * @param  iGrow         The new growth value.
         */
        public void reset(int iMaxQuantity, int iGrow) {

            if (iMaxQuantity <= 0) {
                iMaxQuantity = DEFAULT_GROW;
            }

            if (iGrow <= 0) {
                iGrow = DEFAULT_GROW;
            }

            m_iQuantity = 0;
            m_iMaxQuantity = iMaxQuantity;
            m_iGrow = iGrow;
            m_aiElement = new int[m_iMaxQuantity];
        }

        /**
         * Assign the specified element to array location i. It is necessary that 0 <= i < getMaxQuantity().
         *
         * @param  i         The array location to assign to.
         * @param  iElement  The element to assign to array location i.
         */
        @SuppressWarnings("unused")
        public final void set(int i, int iElement) {
            m_aiElement[i] = iElement;
        }
    }

}
