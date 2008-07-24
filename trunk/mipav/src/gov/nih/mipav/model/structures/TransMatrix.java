package gov.nih.mipav.model.structures;


import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;

import gov.nih.mipav.model.file.FileInfoBase;
import Jama.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 * Transformation matrix class is an affine homogenous class that can be used to rotate objects like images and VOIs. It
 * can constructed as a 2D(3x3) or 3D(4x4) homogenous matrix for transform (rotation, translation, skew and zoom)
 * images/VOIs. Skew is not commonly used.
 *
 * <p>The MIPAV 3D model for 4 X 4 transformations is:<br>
 * </p>
 *
 * <pre>
        [ m00 m01 m02 m03 ]   [ x ]   [ x' ]
        [ m10 m11 m12 m13 ] . [ y ] = [ y' ]
        [ m20 m21 m22 m23 ]   [ z ]   [ z' ]
        [ m30 m31 m32 m33 ]   [ w ]   [ w' ]

        x' = m00*x + m01*y + m02*z + m03*w
        y' = m10*x + m11*y + m12*z + m13*w
        z' = m20*x + m21*y + m22*z + m23*w
        w' = m30*x + m31*y + m32*z + m33*w
 *  </pre>
 *
 * <p>ORDER OF TRANSFORMATIONS = TRANSLATE, ROTATE, ZOOM or TRANSLATE, ROTATE, SKEW, ZOOM<br>
 * Row, Col format - right hand rule<br>
 * 2D Example<br>
 * </p>
 *
 * <pre>
         zoom_x    theta    tx
         theta     zoom_y   ty
         0         0         1
 *   </pre>
 *
 * <p>Note for 3D - ref. Foley, Van Dam p. 214</p>
 *
 * <pre>
         Axis of rotation            Direction of positive rotation is
             x                               y to z
             y                               z to x
             z                               x to y
 *   </pre>
 *
 * <pre>
         Order of rotation is important (i.e. not communitive)
         Rx Ry Ry != Ry Rx Rz


 *   </pre>
 *
 * @version  0.1 Nov 15, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class TransMatrix extends Matrix // implements TableModelListener
{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5604493833934574127L;

    /** Composite transform ID. */
    public static final int TRANSFORM_UNKNOWN = 0;

    /** Scanner Anatomical transform ID. */
    public static final int TRANSFORM_SCANNER_ANATOMICAL = 1;

    /** Another Dataset transform ID. */
    public static final int TRANSFORM_ANOTHER_DATASET = 2;

    /** Talairach Tournoux transform ID. */
    public static final int TRANSFORM_TALAIRACH_TOURNOUX = 3;

    /** MNI 152 transform ID. */
    public static final int TRANSFORM_MNI_152 = 4;
    
    /** Composite, dynamically generated matrix type */
    public static final int TRANSFORM_COMPOSITE = 5;
    
    /** Scanner anatomical matrix associated with NIFTI type */
    public static final int TRANSFORM_NIFTI_SCANNER_ANATOMICAL = 6;
    
    /** Array of transform ID strings. */
    private static final String[] transformIDStr = {
        "Unknown", "Scanner Anatomical", "Another Dataset", "Talairach Tournoux", "MNI 152", "Composite",
        "Scanner Anatomical"
    };
    
    /** DOCUMENT ME! */
    public static final int DEGREES = 0;

    /** DOCUMENT ME! */
    public static final int RADIANS = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[] tran;

    /** DOCUMENT ME! */
    private int U_PERSPW = 15;

    /** DOCUMENT ME! */
    private int U_PERSPX = 12;

    /** DOCUMENT ME! */
    private int U_PERSPY = 13;

    /** DOCUMENT ME! */
    private int U_PERSPZ = 14;

    /** DOCUMENT ME! */
    private int U_ROTATEX = 6;

    /** DOCUMENT ME! */
    private int U_ROTATEY = 7;

    /** DOCUMENT ME! */
    private int U_ROTATEZ = 8;

    /** DOCUMENT ME! */
    private int U_SCALEX = 0;

    /** DOCUMENT ME! */
    private int U_SCALEY = 1;

    /** DOCUMENT ME! */
    private int U_SCALEZ = 2;

    /** DOCUMENT ME! */
    private int U_SHEARXY = 3;

    /** DOCUMENT ME! */
    private int U_SHEARXZ = 4;

    /** DOCUMENT ME! */
    private int U_SHEARYZ = 5;

    /** DOCUMENT ME! */
    private int U_TRANSX = 9;

    /** DOCUMENT ME! */
    private int U_TRANSY = 10;

    /** DOCUMENT ME! */
    private int U_TRANSZ = 11;

    /** Transform ID associated with the matrix. */
    private int transformID = TRANSFORM_COMPOSITE;
    
    /** boolean indicating whether this matrix is associated with a NIFTI image (special case handling)*/
    private boolean isNIFTI = false;
    
    /** If true, nifti matrix codes for a qform matrix
     *  If false, nifti matrix codes for a sform matrix 
     *  Value has no effect if not a nifti matrix 
     */
    private boolean isQform = true;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct a transformation matrix.
     *
     * @param  dim  should be 3 or 4 (square matrix)
     */
    public TransMatrix(int dim) {
       this(dim, TRANSFORM_ANOTHER_DATASET);
    }

    public TransMatrix(int dim, int id) {
    	this(dim, id, false, false);
    }
    
    public TransMatrix(int dim, int id, boolean is_nifti, boolean isQform) {
    	super(dim, dim);
    	identity();
    	this.transformID = id;
    	this.isNIFTI = is_nifti;
        this.isQform = isQform;
    }
    
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Make a deep copy (i.e. clone of the matrix).
     *
     * @return  a TransMatrix object.
     */
    public Object clone() {
        TransMatrix tMat = new TransMatrix(this.m, transformID, isNIFTI, isQform);
        tMat.A = this.getArrayCopy();

        return tMat;
    }

    /**
     * Replaces casting Matrix to TransMatrix.
     *
     * @param  b  Matrix to be cast
     */
    public void convertFromMatrix(Matrix b) {
        A = b.getArray();
    }

    /**
     * Decodes the matrix from a string into the matrix array.
     *
     * @param  str  Matrix represented as a string.
     */
    public void decodeMatrixString(String str) {
        StringTokenizer tok = new StringTokenizer(str);

        for (int i = 0; i < A.length; i++) {

            for (int j = 0; j < A[0].length; j++) {
                A[i][j] = Double.valueOf((String) tok.nextElement()).doubleValue();
            }
        }
    }

    /**
     * Decomposing a matrix into simple transformations TransMatrix transformation sequence: Scale(Sx, Sy,
     * Sz)*ShearXY*ShearXZ*ShearYZ*RotateX*RotateY*RotateZ*Translate(tx, ty, tz)*Perspective(Px, Py, Pz, Pw).
     *
     * @param   mat  TransMatrix to decompose.
     *
     * @return  successfully decompose or not.
     */
    public boolean decomposeMatrix(Matrix mat) {
        tran = new double[16];

        int i, j;
        Matrix locmat = new Matrix(4, 4);

        Matrix pmat = Matrix.identity(4, 4);
        Matrix invpmat = Matrix.identity(4, 4);
        Matrix tinvpmat = Matrix.identity(4, 4);

        /* Vector4 type and functions need to be added to the common set. */
        Vector4f prhs, psol;
        prhs = new Vector4f();
        psol = new Vector4f();

        Vector3f[] row;
        Vector3f pdum3;
        row = new Vector3f[3];
        pdum3 = new Vector3f();

        for (i = 0; i < 3; i++) {
            row[i] = new Vector3f();
        }

        locmat = (Matrix) mat.clone();
        // locmat = mat;

        /* Normalize the matrix. */
        if (locmat.get(3, 3) == 0) {
            return false;
        }

        for (i = 0; i < 4; i++) {

            for (j = 0; j < 4; j++) {
                double val;
                val = locmat.get(i, j) / locmat.get(3, 3);
                locmat.set(i, j, val);
            }
        }

        // pmat is used to solve for perspective, but it also provides
        // an easy way to test for singularity of the upper 3x3 component.
        pmat = (Matrix) locmat.clone();

        // pmat = locmat;
        for (i = 0; i < 3; i++) {
            pmat.set(i, 3, 0);
        }

        pmat.set(3, 3, 1);

        if (pmat.getMatrix(0, 2, 0, 2).det() == 0.0) {
            return false;
        }

        /// removed perspective code -- we shouldn't need to do this since our transforms are affine
        /*
         * // First, isolate perspective.  This is the messiest. if ( locmat.get( 0, 3 ) != 0 || locmat.get( 1, 3 ) != 0
         * || locmat.get( 2, 3 ) != 0 ) {
         *
         * // prhs is the right hand side of the equation. prhs.x = locmat.get( 0, 3 ); prhs.Y = locmat.get( 1, 3 );
         * prhs.Z = locmat.get( 2, 3 ); prhs.w = locmat.get( 3, 3 );
         *
         * // Solve the equation by inverting pmat and multiplying // prhs by the inverse.  (This is the easiest way, not
         * // necessarily the best.) // inverse function (and det4x4, above) from the Matrix // Inversion gem in the
         * first volume.
         *
         * // inverse( &pmat, &invpmat ); invpmat = pmat.inverse(); // TransposeMatrix4( &invpmat, &tinvpmat ); tinvpmat =
         * invpmat.transpose(); V4MulPointByMatrix( prhs, tinvpmat, psol );
         *
         * // Stuff the answer away. tran[U_PERSPX] = psol.x; tran[U_PERSPY] = psol.Y; tran[U_PERSPZ] = psol.Z;
         * tran[U_PERSPW] = psol.w;
         *
         * tran[U_TRANSX] = psol.x; tran[U_TRANSY] = psol.Y; tran[U_TRANSZ] = psol.Z;
         *
         * // Clear the perspective partition. locmat.set( 0, 3, 0 ); locmat.set( 1, 3, 0 ); locmat.set( 2, 3, 0 );
         * locmat.set( 3, 3, 1 ); } else { // No perspective. tran[U_PERSPX] = tran[U_PERSPY] = tran[U_PERSPZ] =
         * tran[U_PERSPW] = 0;}*/

        // Next take care of translation (easy).
        for (i = 0; i < 3; i++) {

            //            tran[U_TRANSX + i] = locmat.get(3, i);
            //            locmat.set(3, i, 0);
            tran[U_TRANSX + i] = locmat.get(i, 3);
            locmat.set(i, 3, 0);
        }

        // Now get scale and shear.
        for (i = 0; i < 3; i++) {
            row[i].X = (float)locmat.get(i, 0);
            row[i].Y = (float)locmat.get(i, 1);
            row[i].Z = (float)locmat.get(i, 2);
        }

        // Compute X scale factor and normalize first row.
        tran[U_SCALEX] = row[0].Length();

        // row[0] = *V3Scale(&row[0], 1.0);
        row[0].Scale(1.0f);

        // Compute XY shear factor and make 2nd row orthogonal to 1st.
        tran[U_SHEARXY] = row[0].Dot(row[1]);
        V3Combine(row[1], row[0], row[1], 1.0, -tran[U_SHEARXY]);

        // Now, compute Y scale and normalize 2nd row.
        // tran[U_SCALEY] = V3Length(&row[1]);
        tran[U_SCALEY] = row[1].Length();
        row[1].Scale(1.0f);
        tran[U_SHEARXY] /= tran[U_SCALEY];

        // Compute XZ and YZ shears, orthogonalize 3rd row.
        tran[U_SHEARXZ] = row[0].Dot(row[2]);
        V3Combine(row[2], row[0], row[2], 1.0, -tran[U_SHEARXZ]);
        tran[U_SHEARYZ] = row[1].Dot(row[2]);
        V3Combine(row[2], row[1], row[2], 1.0, -tran[U_SHEARYZ]);

        // Next, get Z scale and normalize 3rd row.
        // tran[U_SCALEZ] = V3Length(&row[2]);
        tran[U_SCALEZ] = row[2].Length();
        row[2].Scale(1.0f);
        tran[U_SHEARXZ] /= tran[U_SCALEZ];
        tran[U_SHEARYZ] /= tran[U_SCALEZ];

        // At this point, the matrix (in rows[]) is orthonormal.
        // Check for a coordinate system flip.  If the determinant
        // is -1, then negate the matrix and the scaling factors.
        pdum3.Cross( row[1], row[2] );
        if (row[0].Dot(pdum3) < 0) {

            for (i = 0; i < 3; i++) {
                tran[U_SCALEX + i] *= -1;
                row[i].X *= -1;
                row[i].Y *= -1;
                row[i].Z *= -1;
            }
        }

        // Now, get the rotations out, similar to gem
        // but gem uses different rotation matrices then we do
        // In going between our 3 rotation matrices and the
        // 3 gem rotation matrices the sine and -sine reverse
        // positions.  We have:
        // rx = 1        0         0
        // 0        cos(A)    -sin(A)
        // 0        sin(A)    cos(A)
        // ry = cos(B)   0         sin(B)
        // 0        1         0
        // -sin(B)  0         cos(B)
        // rz = cos(G)   -sin(G)   0
        // sin(G)   cos(G)    0
        // 0        0         1
        // for a composite rotation matrix:
        // cos(B)cos(G)            -cos(B)sin(G)         sin(B)
        //
        // sin(A)sin(B)cos(G)      -sin(A)sin(B)sin(G)   -sin(A)cos(B)
        // + cos(A)sin(G)          + cos(A)cos(G)
        //
        // -cos(A)sin(B)cos(G)     cos(A)sin(B)sin(G)    cos(A)cos(B)
        // +sin(A)sin(G)           +sin(A)cos(G)
        tran[U_ROTATEY] = Math.asin(row[0].Z);

        if (Math.cos(tran[U_ROTATEY]) != 0) {
            tran[U_ROTATEX] = -Math.atan2(row[1].Z, row[2].Z);
            tran[U_ROTATEZ] = -Math.atan2(row[0].Y, row[0].X);
        } else {
            tran[U_ROTATEX] = Math.atan2(row[2].Y, row[1].Y);
            tran[U_ROTATEZ] = 0;
        }

        // System.out.println( "transform:" ); System.out.println( "rot x: " + getRotateX() + " rot y: " + getRotateY()
        // + " rot z: " + getRotateZ() ); System.out.println( "scl x: " + getScaleX() + " scl y: " + getScaleY() + " scl
        // z: " + getScaleZ() ); System.out.println( "trn x: " + getTranslateX() + " trn y: " + getTranslateY() + " trn
        // z: " + getTranslateZ() );

        return true;
    }

    /**
     * Decomposing a matrix into simple transformations TransMatrix transformation sequence: Scale(Sx,
     * Sy)*ShearXY*RotateZ*Translate(tx, ty)*Perspective(Px, Py, Pw).
     *
     * @param   mat  TransMatrix to decompose.
     *
     * @return  successfully decompose or not.
     */
    public boolean decomposeMatrix2D(Matrix mat) {
        tran = new double[16];

        int i, j;
        Matrix locmat = new Matrix(3, 3);

        Matrix pmat = Matrix.identity(3, 3);
        Matrix invpmat = Matrix.identity(3, 3);
        Matrix tinvpmat = Matrix.identity(3, 3);

        /* Vector3 type and functions need to be added to the common set. */
        Vector3f prhs, psol;
        prhs = new Vector3f();
        psol = new Vector3f();

        Vector2f[] row;
        Vector2f pdum2;
        row = new Vector2f[2];
        pdum2 = new Vector2f();

        for (i = 0; i < 2; i++) {
            row[i] = new Vector2f();
        }

        locmat = (Matrix) mat.clone();
        // locmat = mat;

        /* Normalize the matrix. */
        if (locmat.get(2, 2) == 0) {
            return false;
        }

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {
                double val;
                val = locmat.get(i, j) / locmat.get(2, 2);
                locmat.set(i, j, val);
            }
        }

        // pmat is used to solve for perspective, but it also provides
        // an easy way to test for singularity of the upper 3x3 component.
        pmat = (Matrix) locmat.clone();

        // pmat = locmat;
        for (i = 0; i < 2; i++) {
            pmat.set(i, 2, 0);
        }

        pmat.set(2, 2, 1);

        if (pmat.getMatrix(0, 1, 0, 1).det() == 0.0) {
            return false;
        }

        /// removed perspective code -- we shouldn't need to do this since our transforms are affine
        /*
         * // First, isolate perspective.  This is the messiest. if ( locmat.get( 0, 2 ) != 0 || locmat.get( 1, 2 ) !=
         * 0) {
         *
         * // prhs is the right hand side of the equation. prhs.x = locmat.get( 0, 2 ); prhs.Y = locmat.get( 1, 2 );
         * prhs.w = locmat.get( 2, 2 );
         *
         * // Solve the equation by inverting pmat and multiplying // prhs by the inverse.  (This is the easiest way, not
         * // necessarily the best.) // inverse function (and det3x3, above) from the Matrix // Inversion gem in the
         * first volume.
         *
         * // inverse( &pmat, &invpmat ); invpmat = pmat.inverse(); // TransposeMatrix3( &invpmat, &tinvpmat ); tinvpmat =
         * invpmat.transpose(); V3MulPointByMatrix( prhs, tinvpmat, psol );
         *
         * // Stuff the answer away. tran[U_PERSPX] = psol.x; tran[U_PERSPY] = psol.Y; tran[U_PERSPW] = psol.w;
         *
         * tran[U_TRANSX] = psol.x; tran[U_TRANSY] = psol.Y;
         *
         * // Clear the perspective partition. locmat.set( 0, 2, 0 ); locmat.set( 1, 2, 0 ); locmat.set( 2, 2, 1 ); } else
         * { // No perspective. tran[U_PERSPX] = tran[U_PERSPY] = tran[U_PERSPW] = 0;}*/

        // Next take care of translation (easy).
        for (i = 0; i < 2; i++) {

            //            tran[U_TRANSX + i] = locmat.get(2, i);
            //            locmat.set(2, i, 0);
            tran[U_TRANSX + i] = locmat.get(i, 2);
            locmat.set(i, 2, 0);
        }

        // Now get scale and shear.
        for (i = 0; i < 2; i++) {
            row[i].X = (float)locmat.get(i, 0);
            row[i].Y = (float)locmat.get(i, 1);
        }

        // Compute X scale factor and normalize first row.
        tran[U_SCALEX] = row[0].Length();

        // row[0] = *V2Scale(&row[0], 1.0);
        row[0].Normalize();

        // Compute XY shear factor and make 2nd row orthogonal to 1st.
        tran[U_SHEARXY] = V2Dot(row[0], row[1]);
        V2Combine(row[1], row[0], row[1], 1.0, -tran[U_SHEARXY]);

        // Now, compute Y scale and normalize 2nd row.
        // tran[U_SCALEY] = V3Length(&row[1]);
        tran[U_SCALEY] = row[1].Length();
        row[1].Normalize();
        tran[U_SHEARXY] /= tran[U_SCALEY];

        // At this point, the matrix (in rows[]) is orthonormal.
        // Check for a coordinate system flip.  If the determinant
        // is positive, then negate the matrix and the scaling factors.
        if (((row[0].X * row[1].Y) - row[1].X - row[0].Y) < 0) {

            for (i = 0; i < 2; i++) {
                tran[U_SCALEX + i] *= -1;
                row[i].X *= -1;
                row[i].Y *= -1;
            }
        }

        // Now, get the rotation out, similar to gem
        // but gem uses  a different rotation matrix then we do
        // In going between our rotation matrix and the
        // gem rotation matrix the sine and -sine reverse
        // positions.  We have:
        // rz = cos(G)   -sin(G)   0
        // sin(G)   cos(G)    0
        // 0        0         1

        tran[U_ROTATEZ] = Math.asin(row[1].X);

        // System.out.println( "transform:" );
        // System.out.println( "rot z: " + getRotateZ() );
        // System.out.println( "scl x: " + getScaleX() + " scl y: " + getScaleY());
        // System.out.println( "trn x: " + getTranslateX() + " trn y: " + getTranslateY());

        return true;
    }

    /**
     * Accessor that returns the transformation matrix.
     *
     * @return  transformation matrix
     */
    public double[][] getMatrix() {
        return A;
    }

    /**
     * Accessor that returns the number of cols.
     *
     * @return  number of cols
     */
    public int getNCols() {
        return n;
    }

    /**
     * Accessor that returns the number of rows.
     *
     * @return  number of rows
     */
    public int getNRows() {
        return m;
    }

    /**
     * Returns the rotation about the X axis from the decomposed matrix.
     *
     * @return  the rotation about the X axis in degrees
     *
     * @see     decomposeMatrix
     */
    public double getRotateX() {
        double tempRotX = tran[U_ROTATEX] * 180 / Math.PI;

        return tempRotX;
    }

    /**
     * Returns the rotation about the Y axis from the decomposed matrix.
     *
     * @return  the roation about the Y axis in degrees
     *
     * @see     decomposeMatrix
     */
    public double getRotateY() {
        double tempRotY = tran[U_ROTATEY] * 180 / Math.PI;

        return tempRotY;
    }

    /**
     * Returns the rotation about the Z axis from the decomposed matrix.
     *
     * @return  the roation about the Z axis in degrees
     *
     * @see     decomposeMatrix
     */
    public double getRotateZ() {
        double tempRotZ = tran[U_ROTATEZ] * 180 / Math.PI;

        return tempRotZ;
    }

    /**
     * Returns the scaling factor of the X axis calculated by decomposing the matrix.
     *
     * @return  the scaling factor for the X axis
     *
     * @see     decomposeMatrix
     */
    public double getScaleX() {
        return tran[U_SCALEX];
    }

    /**
     * Returns the scaling factor of the Y axis calculated by decomposing the matrix.
     *
     * @return  the scaling factor for the Y axis
     *
     * @see     decomposeMatrix
     */
    public double getScaleY() {
        return tran[U_SCALEY];
    }

    /**
     * Returns the scaling factor of the Z axis calculated by decomposing the matrix.
     *
     * @return  the scaling factor for the Z axis
     *
     * @see     decomposeMatrix
     */
    public double getScaleZ() {
        return tran[U_SCALEZ];
    }

    /**
     * Returns the translation for the X axis calculated by decomposing the matrix.
     *
     * @return  the translation in voxels for the X axis
     *
     * @see     decomposeMatrix
     */
    public double getTranslateX() {
        return tran[U_TRANSX];
    }

    /**
     * Returns the translation for the Y axis calculated by decomposing the matrix.
     *
     * @return  the translation in voxels for the Y axis
     *
     * @see     decomposeMatrix
     */
    public double getTranslateY() {
        return tran[U_TRANSY];
    }

    /**
     * Returns the translation for the Z axis calculated by decomposing the matrix.
     *
     * @return  the translation in voxels for the Z axis
     *
     * @see     decomposeMatrix
     */
    public double getTranslateZ() {
        return tran[U_TRANSZ];
    }

    /**
     * Makes simple identity matrix ( ones on the diagonal, zeros every place else.
     */
    public void identity() {
        int r, c;
        int rDim = getRowDimension();
        int cDim = getColumnDimension();

        for (r = 0; r < rDim; r++) {

            for (c = 0; c < cDim; c++) {

                if (r == c) {
                    A[r][c] = (float) 1.0;
                } else {
                    A[r][c] = (float) 0.0;
                }
            }
        }
    }

    /**
     * Reports whether matrix is Identity matrix.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isIdentity() {
        int r, c;
        int rDim = getRowDimension();
        int cDim = getColumnDimension();

        boolean isId = true;
        float epsilon = 0.0001f;

        for (r = 0; r < rDim; r++) {

            for (c = 0; c < cDim; c++) {

                if (r == c) {

                    if ((A[r][c] < (1.0f - epsilon)) || (A[r][c] > (1.0f + epsilon))) {
                        isId = false;
                    }
                } else {

                    if ((A[r][c] < (0.0f - epsilon)) || (A[r][c] > (0.0f + epsilon))) {
                        isId = false;
                    }
                }
            }
        }

        return isId;
    }

    /**
     * Tells whether this matrix is associated with a NIFTI image
     * this only matters when the matrix is being saved/when the composite matrix is
     * being generated in MatrixHolder
     * @return is this a nifti matrix
     */
    public boolean isNIFTI() {
    	return this.isNIFTI;
    }
    
    /**
     * Accessor that sets whether or not the matrix is a NIFTI matrix.
     * @param isNIFTI
     */
    public void setIsNIFTI(boolean isNIFTI) {
        this.isNIFTI = isNIFTI;
    }
    
    /**
     * Tells whether a NIFTI matrix is a qform matrix or a sform matrix
     * @return
     */
    public boolean isQform() {
        return this.isQform;
    }
    
    /**
     * Accessor that sets whether a nifti matrix is a qform matrix or a 
     * sform matrix.
     * @param isQform
     */
    public void setIsQform(boolean isQform) {
        this.isQform = isQform;
    }
    
    /**
     * Multiplies two matrices together. General in nature for two-dimensional matrices but specifically used here to
     * concatenate matrices.
     *
     * @param  oneMatrix     two-dimensional input matrix
     * @param  twoMatrix     two-dimensional input matrix
     * @param  resultMatrix  contains result of the multiplication of the two input matrices
     */
    public void multMatrix(double[][] oneMatrix, double[][] twoMatrix, double[][] resultMatrix) {
        int i, j, k;
        int cDim = getColumnDimension();
        int rDim = getRowDimension();

        for (i = 0; i < cDim; i++) {

            for (j = 0; j < rDim; j++) {

                for (k = 0; k < cDim; k++) {
                    resultMatrix[j][i] += oneMatrix[j][k] * twoMatrix[k][i];
                }
            }
        }
    }

    /**
     * Reads transformation matrix to a text file.
     *
     * <p>This method reads two formats MIPAV format 4 // number of rows in matrix 4 // number of cols in matrix 0.234
     * 0.33 0.22 5.0 // matrix info separated by a space 0.234 0.33 0.22 10.0 // matrix info separated by a space 0.234
     * 0.33 0.22 12.0 // matrix info separated by a space 0.0 0.0 0.0 1.0 // matrix info separated by a space</p>
     *
     * <p>Note the above is a homogenous transformation matrix</p>
     *
     * <p>FSL or alternate format supported 0.234 0.33 0.22 5.0 // matrix info separated by 2 spaces 0.234 0.33 0.22 5.0
     * // matrix info separated by 2 spaces 0.234 0.33 0.22 5.0 // matrix info separated by 2 spaces 0 0 0 1 // matrix
     * info separated by 2 spaces also note integer values</p>
     *
     * @param  raFile     random access file pointer
     * @param  composite  if true make a composite matrix of the by multipling this matrix with the one to be read from
     *                    the file. If false replace this object matrix with a new matrix read from the file.
     */
    public void readMatrix(RandomAccessFile raFile, boolean composite) {
        int i, r, c;
        r=4;
        c=4;
        String str;
        boolean isXFM = false;

        if (raFile != null) {

            try {
                str = raFile.readLine().trim();
                if(str.equalsIgnoreCase("MNI Transform File")) {
                	isXFM = true;
                	//make sure that this is a linear transform type file
                	boolean isLinearTransform = false;
                	str = raFile.readLine().trim();
                	while(str != null) {
        	    		if(str.equalsIgnoreCase("Transform_Type = Linear;")) {
        	    			isLinearTransform = true;
        	    			//read next line in which should be "Linear_Transform =" to set the file pointer to the next line
        	    			raFile.readLine();
        	    			break;
        	    		}
        	    		str = raFile.readLine().trim();
        	    	}
                	if(!isLinearTransform) {
                		MipavUtil.displayError("Matrix file must be a linear transform type");
                        return;
                	}
                	r = 4;
                	c = 4;
                }else {

	                if (str.length() > 1) { // assume FSL matrix file and 4 x 4
	                    r = 4;
	                    c = 4;
	                    raFile.seek(0);
	                } else {
	                    raFile.seek(0);
	                    r = Integer.valueOf(raFile.readLine().trim()).intValue();
	                    c = Integer.valueOf(raFile.readLine().trim()).intValue();
	                }
                }

                if (composite == false) {
                    reConstruct(r, c); // reallocate matrix to row, col sizes
                    if(isXFM) {
                    	for (i = 0; i < 3; i++) {
	                        decodeLine(raFile, i, A);
	                    }
                    	A[3][0] = 0;
                    	A[3][1] = 0;
                    	A[3][2] = 0;
                    	A[3][3] = 1.0;
                    }else {
	                    for (i = 0; i < r; i++) {
	                        decodeLine(raFile, i, A);
	                    }
                    }
                } else {
                    double[][] mat = new double[4][4];
                    if(isXFM) {
                    	for (i = 0; i < 3; i++) {
                            decodeLine(raFile, i, mat);
                        }
                    	mat[3][0] = 0;
                    	mat[3][1] = 0;
                    	mat[3][2] = 0;
                    	mat[3][3] = 1.0;
                    }else {
                    	for (i = 0; i < r; i++) {
                            decodeLine(raFile, i, mat);
                        }	
                    }
                    

                    // need to composite here.
                    Matrix tmpMatrix = Matrix.constructWithCopy(mat);
                    timesEquals(tmpMatrix);
                }
            } catch (IOException error) {
                MipavUtil.displayError("Matrix save error " + error);

                return;
            }
        }
        // this.print(4, 4);
    }

    /**
     * Saves transformation matrix to a text file MIPAV format 4 // number of rows in matrix 4 // number of cols in
     * matrix 0.234 0.33 0.22 5.0 // matrix info separated by a space 0.234 0.33 0.22 10.0 // matrix info separated by a
     * space 0.234 0.33 0.22 12.0 // matrix info separated by a space 0.0 0.0 0.0 1.0 // matrix info separated by a
     * space.
     *
     * @param  fileName  - file name, including the path
     */
    public void saveMatrix(String fileName) {

        try {
            File file = new File(fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");
            saveMatrix(raFile);
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error");

            return;
        }
    }

    /**
     * Saves transformation matrix to a text file MIPAV format 4 // number of rows in matrix 4 // number of cols in
     * matrix 0.234 0.33 0.22 5.0 // matrix info separated by a space 0.234 0.33 0.22 10.0 // matrix info separated by a
     * space 0.234 0.33 0.22 12.0 // matrix info separated by a space 0.0 0.0 0.0 1.0 // matrix info separated by a
     * space.
     *
     * @param  raFile  random access file pointer
     */
    public void saveMatrix(RandomAccessFile raFile) {
        int r, c;

        if (raFile != null) {
            try {
                raFile.writeBytes(Integer.toString(m) + "\n"); // write number of rows
                raFile.writeBytes(Integer.toString(n) + "\n"); // write number of columns

                for (r = 0; r < m; r++) {

                    for (c = 0; c < n; c++) {
                        raFile.writeBytes(Double.toString(A[r][c]) + " ");
                    }

                    raFile.writeBytes("\n");
                }

                raFile.writeBytes("\n");
            } catch (IOException error) {
                MipavUtil.displayError("Matrix save error " + error);

                return;
            }
        }
    }
    
    
    /**
     * Saves transformation matrix to McGill XFM format
     *
     * @param  raFile  random access file pointer
     */
    public void saveXFMMatrix(RandomAccessFile raFile) {
        if (raFile != null) {
            try {
                raFile.writeBytes("MNI Transform File" + "\n" + "\n");
                raFile.writeBytes("Transform_Type = Linear;" + "\n");
                raFile.writeBytes("Linear_Transform =" + "\n");
                

                for (int r = 0; r < 3; r++) {

                    for (int c = 0; c < 4; c++) {
                        raFile.writeBytes(Double.toString(A[r][c]));
                        if(r == 2 && c == 3) {
                        	raFile.writeBytes(";");
                        }else {
                        	raFile.writeBytes(" ");
                        }
                    }
                
                    raFile.writeBytes("\n");
                }

                raFile.writeBytes("\n");
            } catch (IOException error) {
                MipavUtil.displayError("Matrix save error " + error);

                return;
            }
        }
    }
    
    
    

    /**
     * Saves transformation matrix and a message to a text file MIPAV format 4 // number of rows in matrix 4 // number
     * of cols in matrix 0.234 0.33 0.22 5.0 // matrix info separated by a space 0.234 0.33 0.22 10.0 // matrix info
     * separated by a space 0.234 0.33 0.22 12.0 // matrix info separated by a space 0.0 0.0 0.0 1.0 // matrix info
     * separated by a space <message goes here>.
     *
     * @param  fileName  - file name, including the path
     * @param  message   String
     */
    public void saveMatrix(String fileName, String message) {

        try {
            File file = new File(fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");
            saveMatrix(raFile, message);
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error");

            return;
        }
    }

    /**
     * Saves transformation matrix and message to a text file MIPAV format 4 // number of rows in matrix 4 // number of
     * cols in matrix 0.234 0.33 0.22 5.0 // matrix info separated by a space 0.234 0.33 0.22 10.0 // matrix info
     * separated by a space 0.234 0.33 0.22 12.0 // matrix info separated by a space 0.0 0.0 0.0 1.0 // matrix info
     * separated by a space <message goes here>.
     *
     * @param  raFile   random access file pointer
     * @param  message  String
     */
    public void saveMatrix(RandomAccessFile raFile, String message) {
        int r, c;

        if (raFile != null) {

            try {
                raFile.writeBytes(Integer.toString(m) + "\n"); // write number of rows
                raFile.writeBytes(Integer.toString(n) + "\n"); // write number of columns

                for (r = 0; r < m; r++) {

                    for (c = 0; c < n; c++) {
                        raFile.writeBytes(Double.toString(A[r][c]) + " ");
                    }

                    raFile.writeBytes("\n");
                }

                raFile.writeBytes("\n");
                raFile.writeBytes(message);
            } catch (IOException error) {
                MipavUtil.displayError("Matrix save error " + error);

                return;
            }
        }
    }

    /**
     * Replaces transformation matrix.
     *
     * @param  newMatrix  replaces transformation matrix with a new matrix assumes users has correct size matrix Will
     *                    add bounds checking in the future
     */
    public void setMatrix(double[][] newMatrix) {
        A = newMatrix;
    }

    /**
     * Replaces a value in the matrix. Row and column start indexing with zero. Any Array IndexOutOfBoundsExceptions
     * caught, are ignored and matrix is left unchanged.
     *
     * @param  newval  DOCUMENT ME!
     * @param  row     DOCUMENT ME!
     * @param  col     DOCUMENT ME!
     */
    public void setMatrix(double newval, int row, int col) {

        try {
            A[row][col] = newval;
        } catch (ArrayIndexOutOfBoundsException aioobe) { /*doing nothing*/
        }
    }

    /**
     * Sets the rotation (2D) of transformation matrix.
     *
     * @param  theta  angle of rotation
     */
    public void setRotate(double theta) {
        double sinTheta;
        double cosTheta;
        double[][] rotateMatrix = new double[3][3];
        double[][] tmpMatrix = new double[3][3];

        if (getRowDimension() != 3) {
            return;
        }

        cosTheta = Math.cos((theta / 180.0) * Math.PI);
        sinTheta = Math.sin((theta / 180.0) * Math.PI);

        rotateMatrix[0][0] = cosTheta;
        rotateMatrix[1][1] = cosTheta;
        rotateMatrix[2][2] = 1;
        rotateMatrix[0][1] = -sinTheta;
        rotateMatrix[1][0] = sinTheta;

        multMatrix(A, rotateMatrix, tmpMatrix);

        for (int i = 0; i <= 2; i++) {

            for (int j = 0; j <= 2; j++) {
                A[i][j] = tmpMatrix[i][j];
            }
        }
    }

    // ***************************   Three dimensional  ******************************

    /**
     * Sets rotation of transformation matrix.
     *
     * @param  alpha  DOCUMENT ME!
     * @param  beta   DOCUMENT ME!
     * @param  gamma  DOCUMENT ME!
     */
    public void setRotate(Vector3f alpha, Vector3f beta, Vector3f gamma) {

        double[][] tmpMatrix = new double[4][4];
        double[][] rotateMatrix = new double[4][4];
        int rDim = getRowDimension();
        int cDim = getColumnDimension();

        rotateMatrix[0][0] = alpha.X;
        rotateMatrix[0][1] = alpha.Y;
        rotateMatrix[0][2] = alpha.Z;
        rotateMatrix[1][0] = beta.X;
        rotateMatrix[1][1] = beta.Y;
        rotateMatrix[1][2] = beta.Z;
        rotateMatrix[2][0] = gamma.X;
        rotateMatrix[2][1] = gamma.Y;
        rotateMatrix[2][2] = gamma.Z;
        rotateMatrix[3][3] = 1;

        int r, c;

        for (r = 0; r < rDim; r++) {

            for (c = 0; c < cDim; c++) {
                tmpMatrix[r][c] = A[r][c];
                A[r][c] = 0;
            }
        }

        multMatrix(tmpMatrix, rotateMatrix, A);
    }

    /**
     * Sets rotation (3D) of transformation matrix.
     *
     * @param  thetaX          angle (degrees or radians) of rotation about the X axis;
     * @param  thetaY          angle (degrees or radians) of rotation about the Y axis;
     * @param  thetaZ          angle (degrees or radians) of rotation about the Z axis;
     * @param  degreeORradian  DOCUMENT ME!
     */
    public void setRotate(double thetaX, double thetaY, double thetaZ, int degreeORradian) {
        double cosTheta, sinTheta;
        double[][] matrixX;
        double[][] matrixY;
        double[][] matrixZ;
        double[][] tmpMatrix = new double[4][4];
        double[][] rotateMatrix = new double[4][4];
        int rDim = getRowDimension();
        int cDim = getColumnDimension();

        matrixX = new double[4][4];

        if (degreeORradian == DEGREES) {
            cosTheta = Math.cos((thetaX / 180.0) * Math.PI);
            sinTheta = Math.sin((thetaX / 180.0) * Math.PI);
        } else {
            cosTheta = Math.cos(thetaX);
            sinTheta = Math.sin(thetaX);
        }

        matrixX[0][0] = 1;
        matrixX[1][1] = cosTheta;
        matrixX[2][2] = cosTheta;
        matrixX[3][3] = 1;
        matrixX[2][1] = sinTheta;
        matrixX[1][2] = -sinTheta;

        matrixY = new double[4][4];

        if (degreeORradian == DEGREES) {
            cosTheta = Math.cos((thetaY / 180.0) * Math.PI);
            sinTheta = Math.sin((thetaY / 180.0) * Math.PI);
        } else {
            cosTheta = Math.cos(thetaY);
            sinTheta = Math.sin(thetaY);
        }

        matrixY[0][0] = cosTheta;
        matrixY[1][1] = 1;
        matrixY[2][2] = cosTheta;
        matrixY[3][3] = 1;
        matrixY[0][2] = sinTheta;
        matrixY[2][0] = -sinTheta;

        matrixZ = new double[4][4];

        if (degreeORradian == DEGREES) {
            cosTheta = Math.cos((thetaZ / 180.0) * Math.PI);
            sinTheta = Math.sin((thetaZ / 180.0) * Math.PI);
        } else {
            cosTheta = Math.cos(thetaZ);
            sinTheta = Math.sin(thetaZ);
        }

        matrixZ[0][0] = cosTheta;
        matrixZ[1][1] = cosTheta;
        matrixZ[2][2] = 1;
        matrixZ[3][3] = 1;
        matrixZ[0][1] = -sinTheta;
        matrixZ[1][0] = sinTheta;

        multMatrix(matrixY, matrixX, tmpMatrix);
        multMatrix(matrixZ, tmpMatrix, rotateMatrix);

        int r, c;

        for (r = 0; r < rDim; r++) {

            for (c = 0; c < cDim; c++) {
                tmpMatrix[r][c] = A[r][c];
                A[r][c] = 0;
            }
        }

        multMatrix(tmpMatrix, rotateMatrix, A);
    }

    /**
     * Sets skew part of 2D matrix.
     *
     * @param  x  Skew x parameter.
     * @param  y  Skew y parameter.
     */
    public void setSkew(double x, double y) {
        double[][] tmpMtx = new double[3][3];

        for (int i = 0; i < A.length; i++) {
            tmpMtx[i] = A[i];
        }

        A[0][0] = tmpMtx[0][0] + (y * tmpMtx[0][1]);
        A[1][0] = tmpMtx[1][0] + (y * tmpMtx[1][1]);
        A[0][1] = (x * tmpMtx[0][0]) + tmpMtx[0][1];
        A[1][1] = (x * tmpMtx[1][0]) + tmpMtx[1][1];
    }

    /**
     * Sets the skew parts of 3D matrix (4D Homogenous).
     *
     * @param  x  x skew
     * @param  y  y skew
     * @param  z  z skew
     */
    public void setSkew(double x, double y, double z) {
        A[0][1] = (x * A[0][0]) + A[0][1];
        A[0][2] = (y * A[0][0]) + (z * A[0][1]) + A[0][2];
        A[1][1] = (x * A[1][0]) + A[1][1];
        A[1][2] = (y * A[1][0]) + (z * A[1][1]) + A[1][2];
        A[2][1] = (x * A[2][0]) + A[2][1];
        A[2][2] = (y * A[2][0]) + (z * A[2][1]) + A[2][2];
    }

    /**
     * Sets 2D transformation matrix [3x3].
     *
     * @param  tX  x translation
     * @param  tY  y translation
     * @param  r   rotation angle in degrees, about unseen z axis
     */
    public void setTransform(double tX, double tY, double r) {
        int j;
        double sinR, cosR;
        cosR = Math.cos((r / 180.0) * Math.PI);
        sinR = Math.sin((r / 180.0) * Math.PI);

        A[0][0] = cosR;
        A[0][1] = -sinR;
        A[0][2] = tX;
        A[1][0] = sinR;
        A[1][1] = cosR;
        A[1][2] = tY;

        for (j = 0; j < 2; j++) {
            A[2][j] = 0;
        }

        A[2][2] = 1;

        return;
    }

    /**
     * Sets the 3D transformation matrix [4x4].
     *
     * @param  tX  x translation
     * @param  tY  y translation
     * @param  tZ  z translation
     * @param  rX  x rotation angle in degrees
     * @param  rY  y rotation angle in degrees
     * @param  rZ  z rotation angle in degrees
     */
    public void setTransform(double tX, double tY, double tZ, double rX, double rY, double rZ) {
        int j;
        double sinrX, sinrY, sinrZ, cosrX, cosrY, cosrZ;
        cosrX = Math.cos((rX / 180.0) * Math.PI);
        sinrX = Math.sin((rX / 180.0) * Math.PI);
        cosrY = Math.cos((rY / 180.0) * Math.PI);
        sinrY = Math.sin((rY / 180.0) * Math.PI);
        cosrZ = Math.cos((rZ / 180.0) * Math.PI);
        sinrZ = Math.sin((rZ / 180.0) * Math.PI);

        A[0][0] = cosrZ * cosrY;
        A[0][1] = -sinrZ * cosrY;
        A[0][2] = sinrY;
        A[0][3] = tX;
        A[1][0] = (cosrZ * sinrY * sinrX) + (sinrZ * cosrX);
        A[1][1] = (-sinrZ * sinrY * sinrX) + (cosrZ * cosrX);
        A[1][2] = -cosrY * sinrX;
        A[1][3] = tY;
        A[2][0] = (-cosrZ * sinrY * cosrX) + (sinrZ * sinrX);
        A[2][1] = (sinrZ * sinrY * cosrX) + (cosrZ * sinrX);
        A[2][2] = cosrY * cosrX;
        A[2][3] = tZ;

        for (j = 0; j < 3; j++) {
            A[3][j] = 0;
        }

        A[3][3] = 1;

        return;
    }

    /**
     * Sets the 3D transformation matrix [4x4].
     *
     * @param  tX  x translation
     * @param  tY  y translation
     * @param  tZ  z translation
     * @param  rX  x rotation angle in degrees
     * @param  rY  y rotation angle in degrees
     * @param  rZ  z rotation angle in degrees
     * @param  sX  x scale
     * @param  sY  y scale
     * @param  sZ  z scale
     */
    public void setTransform(double tX, double tY, double tZ, double rX, double rY, double rZ, double sX, double sY,
                             double sZ) {
        int j;
        double sinrX, sinrY, sinrZ, cosrX, cosrY, cosrZ;
        cosrX = Math.cos((rX / 180.0) * Math.PI);
        sinrX = Math.sin((rX / 180.0) * Math.PI);
        cosrY = Math.cos((rY / 180.0) * Math.PI);
        sinrY = Math.sin((rY / 180.0) * Math.PI);
        cosrZ = Math.cos((rZ / 180.0) * Math.PI);
        sinrZ = Math.sin((rZ / 180.0) * Math.PI);

        A[0][0] = cosrZ * cosrY * sX;
        A[0][1] = -sinrZ * cosrY * sY;
        A[0][2] = sinrY * sZ;
        A[0][3] = tX;
        A[1][0] = ((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * sX;
        A[1][1] = ((-sinrZ * sinrY * sinrX) + (cosrZ * cosrX)) * sY;
        A[1][2] = -cosrY * sinrX * sZ;
        A[1][3] = tY;
        A[2][0] = ((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * sX;
        A[2][1] = ((sinrZ * sinrY * cosrX) + (cosrZ * sinrX)) * sY;
        A[2][2] = cosrY * cosrX * sZ;
        A[2][3] = tZ;

        for (j = 0; j < 3; j++) {
            A[3][j] = 0;
        }

        A[3][3] = 1;

        return;
    }

    /**
     * Sets the 3D transformation matrix [4x4].
     *
     * @param  tX   x translation
     * @param  tY   y translation
     * @param  tZ   z translation
     * @param  rX   x rotation angle in degrees
     * @param  rY   y rotation angle in degrees
     * @param  rZ   z rotation angle in degrees
     * @param  sX   x scale
     * @param  sY   y scale
     * @param  sZ   z scale
     * @param  skX  x skew
     * @param  skY  y skew
     * @param  skZ  z skew
     */
    public void setTransform(double tX, double tY, double tZ, double rX, double rY, double rZ, double sX, double sY,
                             double sZ, double skX, double skY, double skZ) {
        int j;
        double sinrX, sinrY, sinrZ, cosrX, cosrY, cosrZ;
        cosrX = Math.cos((rX / 180.0) * Math.PI);
        sinrX = Math.sin((rX / 180.0) * Math.PI);
        cosrY = Math.cos((rY / 180.0) * Math.PI);
        sinrY = Math.sin((rY / 180.0) * Math.PI);
        cosrZ = Math.cos((rZ / 180.0) * Math.PI);
        sinrZ = Math.sin((rZ / 180.0) * Math.PI);

        A[0][0] = cosrZ * cosrY * sX;
        A[0][1] = ((cosrZ * cosrY * skX) - (sinrZ * cosrY)) * sY;
        A[0][2] = ((cosrZ * cosrY * skY) - (sinrZ * cosrY * skZ) + sinrY) * sZ;
        A[0][3] = tX;
        A[1][0] = ((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * sX;
        A[1][1] = ((((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * skX) - (sinrZ * sinrY * sinrX) +
                        (cosrZ * cosrX)) * sY;
        A[1][2] = ((((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * skY) +
                        (((-sinrZ * sinrY * sinrX) + (cosrZ * cosrX)) * skZ) - (cosrY * sinrX)) * sZ;
        A[1][3] = tY;
        A[2][0] = ((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * sX;
        A[2][1] = ((((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * skX) + (sinrZ * sinrY * cosrX) +
                        (cosrZ * sinrX)) * sY;
        A[2][2] = ((((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * skY) +
                        (((sinrZ * sinrY * cosrX) + (cosrZ * sinrX)) * skZ) + (cosrY * cosrX)) * sZ;
        A[2][3] = tZ;

        for (j = 0; j < 3; j++) {
            A[3][j] = 0;
        }

        A[3][3] = 1;

        return;
    }

    /**
     * Sets translation parts of 2D matrix.
     *
     * @param  x  x translation
     * @param  y  y translation
     */
    public void setTranslate(double x, double y) {
        A[0][2] = (x * A[0][0]) + (y * A[0][1]) + A[0][2];
        A[1][2] = (x * A[1][0]) + (y * A[1][1]) + A[1][2];
    }

    /**
     * Sets the translation parts of 3D matrix (4D Homogenous).
     *
     * @param  x  x translation
     * @param  y  y translation
     * @param  z  z translation
     */
    public void setTranslate(double x, double y, double z) {
        A[0][3] = (x * A[0][0]) + (y * A[0][1]) + (z * A[0][2]) + A[0][3];
        A[1][3] = (x * A[1][0]) + (y * A[1][1]) + (z * A[1][2]) + A[1][3];
        A[2][3] = (x * A[2][0]) + (y * A[2][1]) + (z * A[2][2]) + A[2][3];
    }

    /**
     * Sets the zoom parts of 2D matrix.
     *
     * @param  sx  zoom in the x coordinate
     * @param  sy  zoom in the y coordinate
     */
    public void setZoom(double sx, double sy) {

        A[0][0] = sx * A[0][0];
        A[1][0] = sx * A[1][0];

        A[0][1] = sy * A[0][1];
        A[1][1] = sy * A[1][1];
    }

    /**
     * Sets the zoom parts of 3D matrix.
     *
     * @param  sx  zoom in the x coordinate
     * @param  sy  zoom in the y coordinate
     * @param  sz  zoom in the z coordinate
     */
    public void setZoom(double sx, double sy, double sz) {

        A[0][0] = sx * A[0][0];
        A[1][0] = sx * A[1][0];
        A[2][0] = sx * A[2][0];

        A[0][1] = sy * A[0][1];
        A[1][1] = sy * A[1][1];
        A[2][1] = sy * A[2][1];

        A[0][2] = sz * A[0][2];
        A[1][2] = sz * A[1][2];
        A[2][2] = sz * A[2][2];
    }

    /**
     * Produces a string of the matrix values, rows separated by tabs.
     *
     * @return  DOCUMENT ME!
     */
    public String toDialogString() {
        String s = new String();

        DecimalFormat format = new DecimalFormat();
        format.setMinimumIntegerDigits(1);
        format.setMaximumFractionDigits(4);
        format.setMinimumFractionDigits(4);
        format.setGroupingUsed(false);

        for (int i = 0; i < m; i++) {
            s += "  ";

            for (int j = 0; j < n; j++) {
                s += format.format(A[i][j]); // format the number
                s = s + "  ";
            }

            s = s + "\t";
        }

        return s;
    }

    /**
     * Tranposes a Polygon (2D).
     *
     * @param   gon  input polygon
     *
     * @return  returns the
     */
    public final Polygon transform(Polygon gon) {
        Polygon newGon = new Polygon();
        int n;
        int length = gon.npoints;
        int newX, newY;

        for (n = 0; n < length; n++) {
            newX = (int) Math.round((gon.xpoints[n] * A[0][0]) + (gon.ypoints[n] * A[0][1]) + A[0][2]);
            newY = (int) Math.round((gon.xpoints[n] * A[1][0]) + (gon.ypoints[n] * A[1][1]) + A[1][2]);
            newGon.addPoint(newX, newY);
        }

        return newGon;
    }


    /**
     * Takes a 2D vector (as a Point2Df) and premultiplies it by the 2d
     * transformation matrix.
     *
     * @param  vect   vector of floats to be transformed
     * @param  tVect  transformed vector
     */
//     public final void transform(Point2Df vect, Point2Df tVect) {

//         tVect.x = (float) (((double) vect.x * A[0][0]) +
//                            ((double) vect.Y * A[0][1]) + A[0][2]);

//         tVect.Y = (float) (((double) vect.x * A[1][0]) +
//                            ((double) vect.Y * A[1][1]) + A[1][2]);

//         return;
//     }

    /**
     * Takes a 2D vector (as a Vector3Df) and premultiplies it by the 2d
     * transformation matrix.
     *
     * @param  vect   vector of floats to be transformed
     * @param  tVect  transformed vector
     */
//     public final void transform(Vector3Df vect, Vector3Df tVect) {

//         tVect.x = (float) (((double) vect.x * A[0][0]) +
//                            ((double) vect.Y * A[0][1]) + A[0][2]);

//         tVect.Y = (float) (((double) vect.x * A[1][0]) +
//                            ((double) vect.Y * A[1][1]) + A[1][2]);
//         tVect.Z = 1;

//         return;
//     }

    /**
     * Takes an array of Point2Df 2D vectors and multiplies them with the 2d
     * transformation matrix.
     *
     * @param  vects   float vectors to be transformed
     * @param  tVects  transformed vectors
     */
    public final void transformAsPoint2Df(Vector2f[] vects, Vector2f[] tVects) {
        int n;
        int length = vects.length;

        for (n = 0; n < length; n++) {
            tVects[n].X = (float) (((double) vects[n].X * A[0][0]) +
                                   ((double) vects[n].Y * A[0][1]) +
                                   A[0][2]);
            
            tVects[n].Y = (float) (((double) vects[n].X * A[1][0]) +
                                   ((double) vects[n].Y * A[1][1]) +
                                   A[1][2]);

        }

        return;
    }

    /**
     * Takes an array of Vector3Df 2D vectors and multiplies them with the 2d
     * transformation matrix.
     *
     * @param  vects   float vectors to be transformed
     * @param  tVects  transformed vectors
     */
    public final void transformAsVector3Df(Vector3f[] vects, Vector3f[] tVects) {
        int n;
        int length = vects.length;

        for (n = 0; n < length; n++) {
            tVects[n].X = (float) (((double) vects[n].X * A[0][0]) +
                                   ((double) vects[n].Y * A[0][1]) +
                                   A[0][2]);

            tVects[n].Y = (float) (((double) vects[n].X * A[1][0]) +
                                   ((double) vects[n].Y * A[1][1]) +
                                   A[1][2]);

            tVects[n].Z = 1;
        }

        return;
    }

    /**
     * Takes a 3D or 2D point (as a double array) and premultiplies it by the
     * 3D transformation matrix.
     *
     * @param  pt   coordinate to be transformed
     * @param  tPt  transformed point
     */
    public final void transform(double[] pt, double[] tPt) {

        if (pt.length == 3) {
            tPt[0] = (pt[0] * A[0][0]) +
                (pt[1] * A[0][1]) +
                (pt[2] * A[0][2]) +
                A[0][3];

            tPt[1] = (pt[0] * A[1][0]) +
                (pt[1] * A[1][1]) +
                (pt[2] * A[1][2]) +
                A[1][3];

            tPt[2] = (pt[0] * A[2][0]) +
                (pt[1] * A[2][1]) +
                (pt[2] * A[2][2]) +
                A[2][3];

        } else if (pt.length == 2) {
            tPt[0] = (pt[0] * A[0][0]) + (pt[1] * A[0][1]) + A[0][2];
            tPt[1] = (pt[0] * A[1][0]) + (pt[1] * A[1][1]) + A[1][2];
        }

        return;
    }

    /**
     * Takes a 3D point, as a float array, and premultiplies it by the 3D
     * transformation matrix.
     *
     * @param  pt   coordinate to be transformed
     * @param  tPt  the transformed point
     */
    public final void transform(float[] pt, float[] tPt) {

        tPt[0] = (float) (((double) pt[0] * A[0][0]) +
                          ((double) pt[1] * A[0][1]) +
                          ((double) pt[2] * A[0][2]) +
                          A[0][3]);

        tPt[1] = (float) (((double) pt[0] * A[1][0]) +
                          ((double) pt[1] * A[1][1]) +
                          ((double) pt[2] * A[1][2]) +
                          A[1][3]);

        tPt[2] = (float) (((double) pt[0] * A[2][0]) +
                          ((double) pt[1] * A[2][1]) +
                          ((double) pt[2] * A[2][2]) +
                          A[2][3]);

        return;
    }

    /**
     * Takes a Point3Df 3D point and multiplies it by the 3D transformation
     * matrix.
     *
     * @param  pt   3D float point to be transformed
     * @param  tPt  transformed point
     */
    public final void transformAsPoint3Df(Vector3f pt, Vector3f tPt) {

        tPt.X = (float) (((double) pt.X * A[0][0]) +
                         ((double) pt.Y * A[0][1]) +
                         ((double) pt.Z * A[0][2]) +
                         A[0][3]);

        tPt.Y = (float) (((double) pt.X * A[1][0]) +
                         ((double) pt.Y * A[1][1]) +
                         ((double) pt.Z * A[1][2]) +
                         A[1][3]);

        tPt.Z = (float) (((double) pt.X * A[2][0]) +
                         ((double) pt.Y * A[2][1]) +
                         ((double) pt.Z * A[2][2]) +
                         A[2][3]);

        return;
    }

    /**
     * Takes an array of Point3Df 3D points and premultiplies it by the 3D
     * transformation matrix.
     *
     * @param  pt    3D float points to be transformed
     * @param  tPts  transformed points
     */
//     public final void transform(Point3Df[] pt, Point3Df[] tPts) {
//         int n;
//         int length = pt.length;

//         for (n = 0; n < length; n++) {
//             tPts[n].X = (float) (((double) pt[n].X * A[0][0]) +
//                                  ((double) pt[n].Y * A[0][1]) +
//                                  ((double) pt[n].Z * A[0][2]) +
//                                  A[0][3]);

//             tPts[n].Y = (float) (((double) pt[n].X * A[1][0]) +
//                                  ((double) pt[n].Y * A[1][1]) +
//                                  ((double) pt[n].Z * A[1][2]) +
//                                  A[1][3]);

//             tPts[n].Z = (float) (((double) pt[n].X * A[2][0]) +
//                                  ((double) pt[n].Y * A[2][1]) +
//                                  ((double) pt[n].Z * A[2][2]) +
//                                  A[2][3]);
//         }

//         return;
//     }

    /**
     * Takes a Vector4Df 3D vector and premultiplies it by the 3D
     * transformation matrix.
     *
     * @param  vect   4D float vector to be transformd
     * @param  tVect  transformed vector
     */
//     public final void transform(Vector4Df vect, Vector4Df tVect) {

//         tVect.X = (float) (((double) vect.X * A[0][0]) +
//                            ((double) vect.Y * A[0][1]) +
//                            ((double) vect.Z * A[0][2]) +
//                            A[0][3]);

//         tVect.Y = (float) (((double) vect.X * A[1][0]) +
//                            ((double) vect.Y * A[1][1]) +
//                            ((double) vect.Z * A[1][2]) +
//                            A[1][3]);

//         tVect.Z = (float) (((double) vect.X * A[2][0]) +
//                            ((double) vect.Y * A[2][1]) +
//                            ((double) vect.Z * A[2][2]) +
//                            A[2][3]);
//         tVect.w = 1;

//         return;
//     }

    /**
     * Takes an array of Vector4Df 3D vectors and multiplies them with a 3D
     * transformation matrix.
     *
     * @param  vects   4D float vectors to be transformed
     * @param  tVects  transformed vectors
     */
//     public final void transform(Vector4Df[] vects, Vector4Df[] tVects) {
//         int n;
//         int length = vects.length;

//         for (n = 0; n < length; n++) {
//             tVects[n].X = (float) (((double) vects[n].X * A[0][0]) +
//                                    ((double) vects[n].Y * A[0][1]) +
//                                    ((double) vects[n].Z * A[0][2]) +
//                                    A[0][3]);

//             tVects[n].Y = (float) (((double) vects[n].X * A[1][0]) +
//                                    ((double) vects[n].Y * A[1][1]) +
//                                    ((double) vects[n].Z * A[1][2]) +
//                                    A[1][3]);

//             tVects[n].Z = (float) (((double) vects[n].X * A[2][0]) +
//                                    ((double) vects[n].Y * A[2][1]) +
//                                    ((double) vects[n].Z * A[2][2]) +
//                                    A[2][3]);
//             tVects[n].w = 1;
//         }

//         return;
//     }


    /**
     * Takes double components of a 2D point and premultiplies it by the 2d
     * transformation matrix.
     *
     * @param  x    x coordinate to be transformd
     * @param  y    y coordinate to be transformd
     * @param  tPt  transformed point
     */
    public final void transform(double x, double y, double[] tPt) {

        tPt[0] = (x * A[0][0]) + (y * A[0][1]) + A[0][2];
        tPt[1] = (x * A[1][0]) + (y * A[1][1]) + A[1][2];

        return;
    }

    /**
     * Takes float components of a 2D point and premultiplies it by the 2d
     * transformation matrix.
     *
     * @param  x    x coordinate to be transformed
     * @param  y    y coordinate to be transformed
     * @param  tPt  transformed point
     */
    public final void transform(float x, float y, float[] tPt) {

        tPt[0] = (float) (((double) x * A[0][0]) +
                          ((double) y * A[0][1]) +
                          A[0][2]);
        tPt[1] = (float) (((double) x * A[1][0]) +
                          ((double) y * A[1][1]) +
                          A[1][2]);

        return;
    }

    /**
     * Takes the double components of a 3D point and premultiplies it by the
     * 3D transformation matrix.
     *
     * @param  x    x coordinate to be transformed
     * @param  y    y coordinate to be transformed
     * @param  z    z coordinate to be transformed
     * @param  tPt  transformed point
     */
    public final void transform(double x, double y, double z, double[] tPt) {

        tPt[0] = (x * A[0][0]) +
            (y * A[0][1]) +
            (z * A[0][2]) +
            A[0][3];

        tPt[1] = (x * A[1][0]) +
            (y * A[1][1]) +
            (z * A[1][2]) +
            A[1][3];

        tPt[2] = (x * A[2][0]) +
            (y * A[2][1]) +
            (z * A[2][2]) +
            A[2][3];

        return;
    }

    /**
     * Takes the float components of a 3D point and premultiplies it by the 3D
     * transformation matrix.
     *
     * @param  x    x coordinate to be transformed
     * @param  y    y coordinate to be transformed
     * @param  z    z coordinate to be transformed
     * @param  tPt  the transformed point
     */
    public final void transform(float x, float y, float z, float[] tPt) {

        tPt[0] = (float) (((double) x * A[0][0]) +
                          ((double) y * A[0][1]) +
                          ((double) z * A[0][2]) +
                          A[0][3]);

        tPt[1] = (float) (((double) x * A[1][0]) +
                          ((double) y * A[1][1]) +
                          ((double) z * A[1][2]) +
                          A[1][3]);

        tPt[2] = (float) (((double) x * A[2][0]) +
                          ((double) y * A[2][1]) +
                          ((double) z * A[2][2]) +
                          A[2][3]);

        return;
    }


    /**
     * Returns the transform ID associated with the matrix.
     *
     * @return  int transform ID
     */
    public final int getTransformID() {
        return transformID;
    }
    
    /**
     * Returns the transform ID associated with a string.
     *
     * @param   s  String to test
     *
     * @return  data type
     */
    public static int getTransformIDFromStr(String s) {

        // look through the array of strings to see if there's a match.
        try {

            for (int i = 0; i < transformIDStr.length; i++) {

                if (TransMatrix.getTransformIDStr(i).regionMatches(true, 0, s, 0,
                		TransMatrix.getTransformIDStr(i).length())) {
                    return i;
                }
            }
        } catch (ArrayIndexOutOfBoundsException aie) {
            return FileInfoBase.TRANSFORM_UNKNOWN;
        }

        return FileInfoBase.TRANSFORM_UNKNOWN;

    } // getTransformIDFromStr()

    /**
     * Return the list of transform ID strings (for edit attributes combo box.
     *
     * @return  string [] of transform ID
     */
    public static String[] getTransformIDStr() {
        return transformIDStr;
    }

    /**
     * Return the string associated with the matrix transform ID.
     *
     * @param   m  transform ID
     *
     * @return  the string associated with the transform ID
     */
    public static String getTransformIDStr(int m) {

        try {
            return TransMatrix.transformIDStr[m];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";
    }
    
    /**
     * Sets the transform ID for the matrix.
     *
     * @param  t_id  transform ID
     */
    public void setTransformID(int t_id) {
        transformID = t_id;
    }
    
    /**
     * ToString method that includes the matrix printout as well as the transform ID of the transmatrix
     */
    public String toString() {
    	String desc = new String("TransMatrix: ");
    	desc+="\n\ttransform id: " + TransMatrix.getTransformIDStr(this.transformID);
    	desc+=super.toString();
    	
    	return desc;
    }
    
    /**
     * Decodes the line in the matrix file.
     *
     * @param  raFile  file pointer
     * @param  row     row reference to store transformation matrix
     * @param  matrix  the matrix where the data is to be stored
     */
    private void decodeLine(RandomAccessFile raFile, int row, double[][] matrix) {
        int c;
        int index, nextIndex;
        String str, tmpStr;
        boolean twoSpace = false;

        try {
            str = raFile.readLine().trim();
            //xfm files have a ";" at end of matrix...so get rid of it
            if(str.indexOf(";") != -1) {
    			str = str.substring(0, str.indexOf(";"));
    		}
            index = 0;

            for (c = 0; c < n; c++) {

                if (str.indexOf("  ", index) > 0) {
                    nextIndex = str.indexOf("  ", index); // - handle FSL matrix files with two spaces
                    twoSpace = true;
                } else {
                    nextIndex = str.indexOf(" ", index);
                }

                if (nextIndex != -1) {
                    tmpStr = str.substring(index, nextIndex).trim();

                    if (twoSpace == true) {
                        index = nextIndex + 2;
                    } else {
                        index = nextIndex + 1;
                    }

                    if (tmpStr.indexOf(".") != -1) {
                    	matrix[row][c] = (Double.valueOf(tmpStr).doubleValue());
                    } else {
                    	matrix[row][c] = (Integer.valueOf(tmpStr).doubleValue());
                    }
                } else { // spaces trimmed from end
                    tmpStr = str.substring(index, str.length()).trim();
                    index = nextIndex;

                    if (tmpStr.indexOf(".") != -1) {
                    	matrix[row][c] = (Double.valueOf(tmpStr).doubleValue());
                    } else {
                    	matrix[row][c] = (Integer.valueOf(tmpStr).doubleValue());
                    }
                }
            }

        } catch (IOException error) {
            MipavUtil.displayError("Matrix read error " + error);

            return;
        }
    }

    /**
     * make a linear combination of two vectors and return the result. result = (a * ascl) + (b * bscl)
     *
     * @param   a       DOCUMENT ME!
     * @param   b       DOCUMENT ME!
     * @param   result  DOCUMENT ME!
     * @param   ascl    DOCUMENT ME!
     * @param   bscl    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector2f V2Combine(Vector2f a, Vector2f b, Vector2f result, double ascl, double bscl) {
        result.X = (float)((ascl * a.X) + (bscl * b.X));
        result.Y = (float)((ascl * a.Y) + (bscl * b.Y));

        return (result);
    }

    /**
     * return the dot product of vectors a and b.
     *
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double V2Dot(Vector2f a, Vector2f b) {
        return ((a.X * b.X) + (a.Y * b.Y));
    }

    /**
     * make a linear combination of two vectors and return the result. result = (a * ascl) + (b * bscl)
     *
     * @param   a       DOCUMENT ME!
     * @param   b       DOCUMENT ME!
     * @param   result  DOCUMENT ME!
     * @param   ascl    DOCUMENT ME!
     * @param   bscl    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f V3Combine(Vector3f a, Vector3f b, Vector3f result, double ascl, double bscl) {
        result.X = (float)((ascl * a.X) + (bscl * b.X));
        result.Y = (float)((ascl * a.Y) + (bscl * b.Y));
        result.Z = (float)((ascl * a.Z) + (bscl * b.Z));

        return (result);
    }


    /**
     * multiply a hom. point by a matrix and return the transformed point
     *
     * @param  pin   DOCUMENT ME!
     * @param  m     DOCUMENT ME!
     * @param  pout  DOCUMENT ME!
     */
    private void V4MulPointByMatrix(Vector4f pin, Matrix m, Vector4f pout) {

        pout.X = (float)((pin.X * m.get(0, 0)) + (pin.Y * m.get(1, 0)) + (pin.Z * m.get(2, 0)) + (pin.W * m.get(3, 0)));

        pout.Y = (float)((pin.X * m.get(0, 1)) + (pin.Y * m.get(1, 1)) + (pin.Z * m.get(2, 1)) + (pin.W * m.get(3, 1)));

        pout.Z = (float)((pin.X * m.get(0, 2)) + (pin.Y * m.get(1, 2)) + (pin.Z * m.get(2, 2)) + (pin.W * m.get(3, 2)));

        pout.W = (float)((pin.X * m.get(0, 3)) + (pin.Y * m.get(1, 3)) + (pin.Z * m.get(2, 3)) + (pin.W * m.get(3, 3)));
    }
    
    /**
     * Reallocates memory for matrix without constructing a new object a new object.
     *
     * @param  r  Number of rows.
     * @param  c  Number of colums.
     */
    protected void reConstruct(int r, int c) {
        this.m = r;
        this.n = c;
        A = new double[m][n];
    }

    /**
     * Matrix inversion that replaces this objects matrix with an inverted matrix. This method calls this classes
     * inverse() method.
     */
    public void invert() {
        A = inverse().getArray();
    }
    
    /**
     * Produces a string of the matrix values.
     *
     * @param   w  Column width.
     * @param   d  Number of digits after the decimal.
     *
     * @return  String containing the values from the matrix.
     */
    public String matrixToString(int w, int d) {
        String s = new String();
        int i, j;
        DecimalFormat format = new DecimalFormat();
        format.setMinimumIntegerDigits(1);
        format.setMaximumFractionDigits(d);
        format.setMinimumFractionDigits(d);
        format.setGroupingUsed(false);

        for (i = 0; i < m; i++) {
            s += "  ";

            for (j = 0; j < n; j++) {
                s += format.format(A[i][j]); // format the number
                s = s + "  ";
            }

            s = s + "\n";
        }

        return s;
    }



}
