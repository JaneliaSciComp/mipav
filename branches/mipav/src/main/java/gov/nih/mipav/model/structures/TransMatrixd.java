package gov.nih.mipav.model.structures;


import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Matrix4d;
import WildMagic.LibFoundation.Mathematics.Mathd;

import gov.nih.mipav.model.file.FileInfoBase;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;
import java.lang.IllegalArgumentException;


/**
 * Transformation matrix class is an affine homogeneous class that can be used
 * to rotate objects like images and VOIs. It can constructed as a 2D(3x3) or
 * 3D(4x4) homogeneous matrix for transform (rotation, translation, skew and
 * zoom) images/VOIs. Skew is not commonly used.
 * 
 * <p>
 * The MIPAV 3D model for 4 X 4 transformations is:<br>
 * </p>
 * 
 * <pre>
 *         [ m00 m01 m02 m03 ]   [ x ]   [ x' ]
 *         [ m10 m11 m12 m13 ] . [ y ] = [ y' ]
 *         [ m20 m21 m22 m23 ]   [ z ]   [ z' ]
 *         [ m30 m31 m32 m33 ]   [ w ]   [ w' ]
 * 
 *         x' = m00*x + m01*y + m02*z + m03*w
 *         y' = m10*x + m11*y + m12*z + m13*w
 *         z' = m20*x + m21*y + m22*z + m23*w
 *         w' = m30*x + m31*y + m32*z + m33*w
 * </pre>
 *
 * <p> However, because the transform type is limited, we can always set the
 * third row to [ 0 0 0 1] and write instead: </p>
 * <pre>
 *         [ m00 m01 m02 ]   [ x ]   [ t0 ]   [ x' ]
 *         [ m10 m11 m12 ] . [ y ] + [ t1 ] = [ y' ]
 *         [ m20 m21 m22 ]   [ z ]   [ t2 ]   [ z' ]
 * 
 *         x' = m00*x + m01*y + m02*z + t0
 *         y' = m10*x + m11*y + m12*z + t1
 *         z' = m20*x + m21*y + m22*z + t2
 * </pre>
 *
 * <p> We still represent 4x4 m with a WildMagic Matrix4d, because of the
 * existing implementations that assume a 4x4 matrix, and the use of the upper
 * 3x3 matrix as a 2D transform.</p>
 *
 * <p> We considered representing m with a WildMagic Matrix3d, and t with a
 * Vector3d, as encapsulated in the WildMagic Transformation class, but it
 * does not match typical Mipav usage.</p>
 * 
 * <p>
 * ORDER OF TRANSFORMATIONS = TRANSLATE, ROTATE, ZOOM or TRANSLATE, ROTATE,
 * SKEW, ZOOM<br>
 * Row, Col format - right hand rule<br>
 * 2D Example<br>
 * </p>
 * 
 * <pre>
 *          zoom_x    theta    tx
 *          theta     zoom_y   ty
 *          0         0         1
 * represented in 3D by leaving Z the identity transform:
 *          zoom_x    theta    0    tx
 *          theta     zoom_y   0    ty
 *          0         0        1    0
 *          0         0        0    1
 *
 * </pre>
 *
 * <p> Note that for 2D, the tx and ty components are stored in M02 and M12,
 * and not in M03 and M13, as might be guessed by the use of a Matrix4d to
 * store both 2D and 3D transforms. </p>
 *
 * <p>
 * Note for 3D - ref. Foley, Van Dam p. 214
 * </p>
 * 
 * <pre>
 *          Axis of rotation            Direction of positive rotation is
 *              x                               y to z
 *              y                               z to x
 *              z                               x to y
 * </pre>
 * 
 * <pre>
 *          Order of rotation is important (i.e. not commutative)
 *          Rx Ry Ry != Ry Rx Rz
 * 
 * 
 * </pre>
 * 
 * @version 2, July 24, 2008, original 0.1 Nov 15, 1997
 * @author Matthew J. McAuliffe, Ph.D., changes Aron Helser, Geometric Tools
 * 
 */
public class TransMatrixd extends Matrix4d
{

    //~ Static fields/initializers ---------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID = 5604493833934574127L;

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
    private static final String[] TRANSFORM_ID_STR = {
        "Unknown", "Scanner Anatomical", "Another Dataset", "Talairach Tournoux", "MNI 152", "Composite",
        "NIFTI Scanner Anatomical"
    };
    
    /** used for setting rotation */
    public static final int DEGREES = 0;

    /** used for setting rotation */
    public static final int RADIANS = 1;

    //~ Instance fields -----------------------------------------------------------

    /** Transform ID associated with the matrix. */
    private int m_TransformID = TRANSFORM_COMPOSITE;
    
    /** boolean indicating whether this matrix is associated with a NIFTI
     * image (special case handling) */
    private boolean m_IsNIFTI = false;
    
    /** If true, nifti matrix codes for a qform matrix
     *  If false, nifti matrix codes for a sform matrix 
     *  Value has no effect if not a nifti matrix 
     */
    private boolean m_IsQform = true;
    
    /** Transform was constructed to transform 2D vectors, instead of 3D */
    private boolean m_Is2D = false;

    //~ Constructors ------------------------------------------------------------

    /**
     * Construct a transformation matrix.
     *
     * @param  dim  should be 3 or 4 (square matrix)
     */
    public TransMatrixd(int dim) {
       this(dim, TRANSFORM_ANOTHER_DATASET);
    }

    public TransMatrixd(int dim, int id) {
    	this(dim, id, false, false);
    }
    
    public TransMatrixd(int dim, int id, boolean is_nifti, boolean isQform) {
        // start with Identity matrix, ones along diagonal.
    	super(false);  // calls MakeIdentity(); 

        if (dim == 3) {
            m_Is2D = true;
        } else if (dim != 4) {
            throw new IllegalArgumentException("Dimension must be 3 or 4");
        }
    	this.m_TransformID = id;
    	this.m_IsNIFTI = is_nifti;
        this.m_IsQform = isQform;
    }

    /** copy constructor.
     * @param rkTM matrix to copy
     */
    public TransMatrixd(TransMatrixd rkTM) {
        Copy(rkTM);
    }
    
    //~ Methods -----------------------------------------------------------------

    /**
     * Return dimension passed to constructor.
     * @return 3 for 2D transform, 4 for 3D transform.
     */
    public int getDim() {
        if (m_Is2D) return 3;
        return 4;
    }
    
    public int getID() {
    	return m_TransformID;
    }

    /** 
     * @note this doesn't conform to the Cloneable interface, because it returns
     * TransMatrixd instead of Object. 
     * @return deep copy of this. 
     */
    public TransMatrixd clone() {
        TransMatrixd kTM = new TransMatrixd(this);
        return kTM;
    }


    /** copy, overwrite this. 
     * @param rkTM matrix to copy
     */
    public void Copy(TransMatrixd rkTM) {
        m_Is2D = rkTM.m_Is2D;
        m_TransformID = rkTM.m_TransformID;
    	m_IsNIFTI = rkTM.m_IsNIFTI;
        m_IsQform = rkTM.m_IsQform;
        super.copy(rkTM);
    }


    /**
     * Decodes the matrix from a string into the matrix array.
     *
     * @param  str  Matrix represented as a string.
     */
    public void decodeMatrixString(String str) {
        StringTokenizer tok = new StringTokenizer(str);
        int dim = getDim();
        for (int i = 0; i < dim; i++) {

            for (int j = 0; j < dim; j++) {
            	set(i, j, Double.valueOf((String) tok.nextElement()).doubleValue());
            }
        }
    }

    /**
     * Decomposing a matrix into simple transformations TransMatrixd
     * transformation sequence: 
     * Scale(Sx, Sy, Sz)*ShearXY*ShearXZ*ShearYZ*
     * RotateX*RotateY*RotateZ*Translate(tx, ty, tz)
     * ( *Perspective(Px, Py, Pz, Pw), no longer supported)
     * @param rotate rotation about x, y, z axis, in radians
     * @param trans translation
     * @param scale scale, for each dimension
     * @param shear shear.X is XY, shear.Y is XZ, shear.Z is YZ
     *
     * @return true if decompose was successful.
     */
    
    public boolean decomposeMatrix(Vector3d rotate, Vector3d trans, Vector3d scale, 
                                   Vector3d shear) {
        int i, j;
        // Make a copy of our data, so we can change it.
        TransMatrixd locmat = new TransMatrixd(this);

        TransMatrixd pmat = new TransMatrixd(4);

        Vector3d[] row = new Vector3d[3];
        for (i = 0; i < 3; i++) {
            row[i] = new Vector3d();
        }


        // Normalize the matrix. 
        if (locmat.get(3, 3) == 0) {
            return false;
        }

        for (i = 0; i < 4; i++) {

            for (j = 0; j < 4; j++) {
                double val = locmat.get(i, j) / locmat.get(3, 3);
                locmat.set(i, j, val);
            }
        }

        // pmat is used to solve for perspective, but it also provides
        // an easy way to test for singularity of the upper 3x3 component.
        pmat.Copy(locmat);

        // zero out translation.
        for (i = 0; i < 3; i++) {
            pmat.set(i, 3, 0);
        }

        pmat.set(3, 3, 1);

        if (pmat.determinant() == 0.0) {
            return false;
        }
        // allocate args, if they haven't been:
        if (rotate == null) rotate = new Vector3d();
        if (trans == null) trans = new Vector3d();
        if (scale == null) scale = new Vector3d();
        if (shear == null) shear = new Vector3d();

        // Next take care of translation (easy).
        trans.set(locmat.get(0, 3), locmat.get(1, 3), locmat.get(2, 3));
        for (i = 0; i < 3; i++) {
            locmat.set(i, 3, 0);
        }

        // Now get scale and shear.
        for (i = 0; i < 3; i++) {
            row[i].X = locmat.get(i, 0);
            row[i].Y = locmat.get(i, 1);
            row[i].Z = locmat.get(i, 2);
        }

        // Compute X scale factor and normalize first row.
        scale.X = row[0].length();

        // row[0] = *V3Scale(&row[0], 1.0);
        row[0].scale(1.0);
        // XXX Should this be row[0].Normalize()??

        // Compute XY shear factor and make 2nd row orthogonal to 1st.
        // shear.X is XY, shear.Y is XZ, shear.Z is YZ
        shear.X = row[0].dot(row[1]);
        // row[1] += -shear.X * row[0]
        row[1].scaleAdd(-shear.X, row[0], row[1]);

        // Now, compute Y scale and normalize 2nd row.
        scale.Y = row[1].length();
        row[1].scale(1.0);
        // XXX Should this be row[1].Normalize()??
        shear.X /= scale.Y;

        // Compute XZ and YZ shears, orthogonalize 3rd row.
        shear.Y = row[0].dot(row[2]);
        // row[2] += -shear.Y * row[0]
        row[2].scaleAdd(-shear.Y, row[0], row[2]);

        shear.Z = row[1].dot(row[2]);
        // row[2] += -shear.Z * row[1]
        row[2].scaleAdd(-shear.Z, row[1], row[2]);

        // Next, get Z scale and normalize 3rd row.
        scale.Z = row[2].length();
        row[2].scale(1.0);
        // XXX Should this be row[2].Normalize()??
        shear.Y /= scale.Z;
        shear.Z /= scale.Z;

        // At this point, the matrix (in rows[]) is orthonormal.
        // Check for a coordinate system flip.  If the determinant
        // is -1, then negate the matrix and the scaling factors.
        Vector3d pdum3 = Vector3d.cross( row[1], row[2] );
        if (row[0].dot(pdum3) < 0) {

            scale.neg();
            for (i = 0; i < 3; i++) {
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
        rotate.Y = Math.asin(row[0].Z);

        if (Math.cos(rotate.Y) != 0) {
            rotate.X = -Math.atan2(row[1].Z, row[2].Z);
            rotate.Z = -Math.atan2(row[0].Y, row[0].X);
        } else {
            rotate.X = Math.atan2(row[2].Y, row[1].Y);
            rotate.Z = 0;
        }

        return true;
    }


    /**
     * Reports whether transform is Identity transform, to within a small epsilon.
     *
     * @return true iff identity
     */
    public boolean isIdentity() {
        double epsilon = 1.0e-8;

        if (Math.abs(M00 - 1.0) > epsilon) return false;
        if (Math.abs(M01) > epsilon) return false;
        if (Math.abs(M02) > epsilon) return false;
        if (Math.abs(M10) > epsilon) return false;
        if (Math.abs(M11 - 1.0) > epsilon) return false;
        if (Math.abs(M12) > epsilon) return false;
        if (Math.abs(M20) > epsilon) return false;
        if (Math.abs(M21) > epsilon) return false;
        if (Math.abs(M22 - 1.0) > epsilon) return false;
        if (!m_Is2D) {
            if (Math.abs(M03) > epsilon) return false;
            if (Math.abs(M13) > epsilon) return false;
            if (Math.abs(M23) > epsilon) return false;
            if (Math.abs(M30) > epsilon) return false;
            if (Math.abs(M31) > epsilon) return false;
            if (Math.abs(M32) > epsilon) return false;
            if (Math.abs(M33 - 1.0) > epsilon) return false;
        }
        return true;
    }

    /**
     * Tells whether this matrix is associated with a NIFTI image
     * this only matters when the matrix is being saved/when the composite matrix is
     * being generated in MatrixHolder
     * @return is this a nifti matrix
     */
    public boolean isNIFTI() {
    	return this.m_IsNIFTI;
    }
    
    /**
     * Accessor that sets whether or not the matrix is a NIFTI matrix.
     * @param is_NIFTI val to set
     */
    public void setIsNIFTI(boolean is_NIFTI) {
        this.m_IsNIFTI = is_NIFTI;
    }
    
    /**
     * Tells whether a NIFTI matrix is a qform matrix or a sform matrix
     * @return true if qform, false if sform.
     */
    public boolean isQform() {
        return this.m_IsQform;
    }
    
    /**
     * Accessor that sets whether a nifti matrix is a qform matrix or a 
     * sform matrix.
     * @param is_Qform val to set
     */
    public void setIsQform(boolean is_Qform) {
        this.m_IsQform = is_Qform;
    }
    

    /**
     * Reads transformation matrix to a text file.
     *
     * <p>This method reads two formats MIPAV format</p>
     * <pre>
     * 4                    // number of rows in matrix 
     * 4                    // number of cols in matrix 
     * 0.234 0.33 0.22 5.0  // matrix info separated by a space 
     * 0.234 0.33 0.22 10.0 // matrix info separated by a space 
     * 0.234 0.33 0.22 12.0 // matrix info separated by a space 
     * 0.0 0.0 0.0 1.0      // matrix info separated by a space.
     * <optional message goes here>
     * </pre>
     *
     * <p>Note the above is a homogenous transformation matrix</p>
     *
     * <p>FSL or alternate format supported </p>
     * <pre>
     * 0.234  0.33  0.22  5.0 // matrix info separated by 2 spaces 
     * 0.234  0.33  0.22  5.0 // matrix info separated by 2 spaces 
     * 0.234  0.33  0.22  5.0 // matrix info separated by 2 spaces
     * 0  0  0  1             // matrix info separated by 2 spaces
     * </pre>
     * <p>also note integer values</p>
     *
     * @param  raFile     random access file pointer
     * @param composite if true make a composite matrix of the by multipling
     * this matrix with the one to be read from the file. If false replace
     * this object matrix with a new matrix read from the file.
     */
    public void readMatrix(RandomAccessFile raFile, boolean composite) {
        int i, r = 4, c = 4;
        String str;
        // is MNI transformation matrix, leaves out last row.
        boolean isXFM = false;

        if (raFile == null) return;

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
                        //read next line in which should be 
                        // "Linear_Transform =" to set the file pointer to the next line
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
            } else {
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
            if ( r != c || ! (r == 3 || r == 4) ) {
                MipavUtil.displayError("Matrix file must be a linear transform type, dimensions incompatible, must be 3 or 4.");
                return;
            }
            
            Matrix4d mat = null;
            if (!composite) {
                mat = this;
            } else {
                mat = new Matrix4d();
            }
            if(isXFM) {
                for (i = 0; i < 3; i++) {
                    decodeLine(raFile, i, mat);
                }
                //Third row is already zero
                mat.M33 = 1.0;
            } else {
                for (i = 0; i < r; i++) {
                    decodeLine(raFile, i, mat);
                }
            }

            if (composite) {
                mult(mat);
            }
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error " + error);
            
            return;
        }
        // this.print(4, 4);
    }

    /**
     * Saves transformation matrix to a text file MIPAV format 
     * @see saveMatrix(RandomAccessFile raFile)
     * @param  fileName  - file name, including the path
     */
    public void saveMatrix(String fileName) {
        saveMatrix(fileName, null);
    }

    /**
     * Saves transformation matrix to a text file MIPAV format 
     * @see saveMatrix(RandomAccessFile raFile, String message)
     * @param  fileName  - file name, including the path
     * @param  message   String, may be null for no message.
     */
    public void saveMatrix(String fileName, String message) {

        try {
            File file = new File(fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");
            saveMatrix(raFile, message);
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error " + error);

            return;
        }
    }

    /**
     * Saves transformation matrix to a text file MIPAV format
     * <pre>
     * 4                    // number of rows in matrix 
     * 4                    // number of cols in matrix 
     * 0.234 0.33 0.22 5.0  // matrix info separated by a space 
     * 0.234 0.33 0.22 10.0 // matrix info separated by a space 
     * 0.234 0.33 0.22 12.0 // matrix info separated by a space 
     * 0.0 0.0 0.0 1.0      // matrix info separated by a space.
     * </pre>
     *
     * @see saveMatrix(RandomAccessFile raFile, String message)
     * @param  raFile  random access file pointer
     */
    public void saveMatrix(RandomAccessFile raFile) {
        saveMatrix(raFile, null);
    }

    /**
     * Saves transformation matrix to a text file MIPAV format
     * <pre>
     * 4                    // number of rows in matrix 
     * 4                    // number of cols in matrix 
     * 0.234 0.33 0.22 5.0  // matrix info separated by a space 
     * 0.234 0.33 0.22 10.0 // matrix info separated by a space 
     * 0.234 0.33 0.22 12.0 // matrix info separated by a space 
     * 0.0 0.0 0.0 1.0      // matrix info separated by a space.
     * <optional message goes here>
     * </pre>
     *
     *
     * @param  raFile  random access file pointer
     * @param  message  String, may be null for no message.
     */
    public void saveMatrix(RandomAccessFile raFile, String message) {
        int r, c;

        if (raFile == null) return;

        try {
            raFile.writeBytes(Integer.toString(getDim()) + "\n"); // write number of rows
            raFile.writeBytes(Integer.toString(getDim()) + "\n"); // write number of columns
            int dim = getDim();
            for (r = 0; r < dim; r++) {
                for (c = 0; c < dim; c++) {
                    raFile.writeBytes(Double.toString(get(r, c)) + " ");
                }
                    
                raFile.writeBytes("\n");
            }
            raFile.writeBytes("\n");
            if (message != null) {
                raFile.writeBytes(message);
            }
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error " + error);
            
            return;
        }
    }
    
    
    /**
     * Saves transformation matrix to McGill XFM format
     *
     * @param  raFile  random access file pointer
     */
    public void saveXFMMatrix(RandomAccessFile raFile) {
        if (raFile == null) return;

        if (m_Is2D) {
            MipavUtil.displayError("saveXFMMatrix of 2D transform, aborting.");
            return;
        }

        try {
            raFile.writeBytes("MNI Transform File" + "\n" + "\n");
            raFile.writeBytes("Transform_Type = Linear;" + "\n");
            raFile.writeBytes("Linear_Transform =" + "\n");
            
            for (int r = 0; r < 3; r++) {
                
                for (int c = 0; c < 4; c++) {
                    raFile.writeBytes(Double.toString(get(r, c)));
                    if (r == 2 && c == 3) {
                        raFile.writeBytes(";");
                    } else {
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
    
    
    /**
     * Copies provided transformation matrix.
     *
     * @param newMatrix 2D array to copy, 3x3 or 4x4
     */
     public void copyMatrix(double[][] newMatrix) {
         assert(newMatrix != null);
         int dim = getDim();
         for (int r = 0; r < dim; r++) {
             for (int c = 0; c < dim; c++) {
            	 set(r, c, newMatrix[r][c]);
             }
         }
     }

    /** Copy our data into the provided double array
     * @param r row to copy
     * @param column place to put column data, length 3 or 4
     */
     public void getColumn(int r, double[] column) {
         for (int c = 0; c < getDim(); c++) {
             column[c] = get(r, c);
         }
     }


    /** Set a submatrix. Borrowed from Jama, useful for copying a Jama
     * matrix M by passing in M.getArray() for param X
     *  @param i0   Initial row index
     *  @param i1   Final row index
     *  @param j0   Initial column index
     *  @param j1   Final column index
     *  @param X    A(i0:i1,j0:j1)
     *  @exception  ArrayIndexOutOfBoundsException Submatrix indices
     */
    
    public void setMatrix (int i0, int i1, int j0, int j1, double[][] X) {
        try {
            for (int r = i0; r <= i1; r++) {
                for (int c = j0; c <= j1; c++) {
                	set(r, c, X[r-i0][c-j0]);
                }
            }
        } catch(ArrayIndexOutOfBoundsException e) {
            throw new ArrayIndexOutOfBoundsException("Submatrix indices");
        }
    }
    
    
    /** Set a submatrix. Borrowed from Jama, useful for copying a Jama
     * matrix M by passing in M.getArray() for param X
     *  @param X    A(i0:i1,j0:j1)
     *  @exception  ArrayIndexOutOfBoundsException Submatrix indices
     */
    
    public void setMatrix (double[][] X) {
        try {
            for (int r = 0; r < X.length; r++) {
                for (int c = 0; c < X[0].length; c++) {
                	set(r, c, X[r][c]);
                }
            }
        } catch(ArrayIndexOutOfBoundsException e) {
            throw new ArrayIndexOutOfBoundsException("Submatrix indices");
        }
    }
    

    /**
     * Sets the rotation (2D) of transformation matrix.
     *
     * @param  theta  angle of rotation, in degrees
     */
    public void setRotate(double theta) {
        assert(m_Is2D);
        if (getDim() != 3) {
            return;
        }

        double cosTheta, sinTheta;
        TransMatrixd axis_rot = new TransMatrixd(3);

        cosTheta = Math.cos((theta / 180.0) * Math.PI);
        sinTheta = Math.sin((theta / 180.0) * Math.PI);

        axis_rot.set(0, 0, cosTheta);
        axis_rot.set(1, 1, cosTheta);
        axis_rot.set(2, 2, 1);
        axis_rot.set(0, 1, -sinTheta);
        axis_rot.set(1, 0, sinTheta);

        // compose with our current matrix.
        mult(axis_rot);
    }

    /**
     * Sets rotation (3D) of transformation matrix.
     *
     * @param  alpha  row0
     * @param  beta   row1
     * @param  gamma  row2
     */
    public void setRotate(Vector3d alpha, Vector3d beta, Vector3d gamma) {
        TransMatrixd axis_rot = new TransMatrixd(4);

        axis_rot.set(0, 0, alpha.X);
        axis_rot.set(0, 1, alpha.Y);
        axis_rot.set(0, 2, alpha.Z);
        axis_rot.set(1, 0, beta.X);
        axis_rot.set(1, 1, beta.Y);
        axis_rot.set(1, 2, beta.Z);
        axis_rot.set(2, 0, gamma.X);
        axis_rot.set(2, 1, gamma.Y);
        axis_rot.set(2, 2, gamma.Z);
        axis_rot.set(3, 3, 1);

        // compose with our current matrix.
        mult(axis_rot);
    }

    /**
     * Sets rotation (3D) of transformation matrix.
     *
     * @param  thetaX          angle (degrees or radians) of rotation about the X axis;
     * @param  thetaY          angle (degrees or radians) of rotation about the Y axis;
     * @param  thetaZ          angle (degrees or radians) of rotation about the Z axis;
     * @param  degreeORradian  DEGREES or RADIANS
     */
    public void setRotate(double thetaX, double thetaY, double thetaZ, int degreeORradian) {
        double cosTheta, sinTheta;
        TransMatrixd axis_rot = new TransMatrixd(4);

        TransMatrixd tmpMatrix = new TransMatrixd(4);

        if (degreeORradian == DEGREES) {
            cosTheta = Math.cos((thetaZ / 180.0) * Math.PI);
            sinTheta = Math.sin((thetaZ / 180.0) * Math.PI);
        } else {
            cosTheta = Math.cos(thetaZ);
            sinTheta = Math.sin(thetaZ);
        }

        axis_rot.set(0, 0, cosTheta);
        axis_rot.set(1, 1, cosTheta);
        axis_rot.set(2, 2, 1);
        axis_rot.set(3, 3, 1);
        axis_rot.set(0, 1, -sinTheta);
        axis_rot.set(1, 0, sinTheta);

        tmpMatrix.Copy(axis_rot);

        axis_rot.makeZero();

        if (degreeORradian == DEGREES) {
            cosTheta = Math.cos((thetaY / 180.0) * Math.PI);
            sinTheta = Math.sin((thetaY / 180.0) * Math.PI);
        } else {
            cosTheta = Math.cos(thetaY);
            sinTheta = Math.sin(thetaY);
        }

        axis_rot.set(0, 0, cosTheta);
        axis_rot.set(1, 1, 1);
        axis_rot.set(2, 2, cosTheta);
        axis_rot.set(3, 3, 1);
        axis_rot.set(0, 2, sinTheta);
        axis_rot.set(2, 0, -sinTheta);

        tmpMatrix.mult(axis_rot);
        axis_rot.makeZero();

        if (degreeORradian == DEGREES) {
            cosTheta = Math.cos((thetaX / 180.0) * Math.PI);
            sinTheta = Math.sin((thetaX / 180.0) * Math.PI);
        } else {
            cosTheta = Math.cos(thetaX);
            sinTheta = Math.sin(thetaX);
        }

        axis_rot.set(0, 0, 1);
        axis_rot.set(1, 1, cosTheta);
        axis_rot.set(2, 2, cosTheta);
        axis_rot.set(3, 3, 1);
        axis_rot.set(2, 1, sinTheta);
        axis_rot.set(1, 2, -sinTheta);

        tmpMatrix.mult(axis_rot);
        // compose with our current matrix.
        mult(tmpMatrix);
    }

    /**
     * Sets skew part of 2D matrix.
     *
     * @param  x  Skew x parameter.
     * @param  y  Skew y parameter.
     */
    public void setSkew(double x, double y) {
        assert(m_Is2D);
        double 
            tmpM00 = M00, tmpM01 = M01, 
            tmpM10 = M10, tmpM11 = M11;

        M00 = tmpM00 + (y * tmpM01);
        M10 = tmpM10 + (y * tmpM11);
        M01 = (x * tmpM00) + tmpM01;
        M11 = (x * tmpM10) + tmpM11;
    }

    /**
     * Sets the skew parts of 3D matrix (4D Homogenous).
     *
     * @param  x  x skew
     * @param  y  y skew
     * @param  z  z skew
     */
    public void setSkew(double x, double y, double z) {
        // XXX not all elements are set, some referenced after being set, 
        // NOT the same as 2D case. Is this really right???
        M01 = ((x * M00) + M01);
        M02 = ((y * M00) + (z * M01) + M02);
        M11 = ((x * M10) + M11);
        M12 = ((y * M10) + (z * M11) + M12);
        M21 = ((x * M20) + M21);
        M22 = ((y * M20) + (z * M21) + M22);
    }

    /**
     * Sets 2D transformation matrix.
     *
     * @param  tX  x translation
     * @param  tY  y translation
     * @param  r   rotation angle in degrees, about unseen z axis
     */
    public void setTransform(double tX, double tY, double r) {
        assert(m_Is2D);
        double sinR, cosR;
        cosR = Math.cos((r / 180.0) * Math.PI);
        sinR = Math.sin((r / 180.0) * Math.PI);

        M00 = cosR;
        M01 = -sinR;
        M02 = tX;
        M10 = sinR;
        M11 = cosR;
        M12 = tY;

        M20 = M21 = 0;
        M22 = 1;

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
        double sinrX, sinrY, sinrZ, cosrX, cosrY, cosrZ;
        cosrX = Math.cos((rX / 180.0) * Math.PI);
        sinrX = Math.sin((rX / 180.0) * Math.PI);
        cosrY = Math.cos((rY / 180.0) * Math.PI);
        sinrY = Math.sin((rY / 180.0) * Math.PI);
        cosrZ = Math.cos((rZ / 180.0) * Math.PI);
        sinrZ = Math.sin((rZ / 180.0) * Math.PI);

        M00 = cosrZ * cosrY;
        M01 = -sinrZ * cosrY;
        M02 = sinrY;
        M03 = tX;
        M10 = (cosrZ * sinrY * sinrX) + (sinrZ * cosrX);
        M11 = (-sinrZ * sinrY * sinrX) + (cosrZ * cosrX);
        M12 = -cosrY * sinrX;
        M13 = tY;
        M20 = (-cosrZ * sinrY * cosrX) + (sinrZ * sinrX);
        M21 = (sinrZ * sinrY * cosrX) + (cosrZ * sinrX);
        M22 = cosrY * cosrX;
        M23 = tZ;

        M30 = M31 = M32 = 0;
        M33 = 1;

        return;
    }

    // Unused:
//     /**
//      * Sets the 3D transformation matrix [4x4].
//      *
//      * @param  tX  x translation
//      * @param  tY  y translation
//      * @param  tZ  z translation
//      * @param  rX  x rotation angle in degrees
//      * @param  rY  y rotation angle in degrees
//      * @param  rZ  z rotation angle in degrees
//      * @param  sX  x scale
//      * @param  sY  y scale
//      * @param  sZ  z scale
//      */
//     public void setTransform(double tX, double tY, double tZ, double rX, double rY, double rZ, double sX, double sY,
//                              double sZ) {
//         double sinrX, sinrY, sinrZ, cosrX, cosrY, cosrZ;
//         cosrX = Math.cos((rX / 180.0) * Math.PI);
//         sinrX = Math.sin((rX / 180.0) * Math.PI);
//         cosrY = Math.cos((rY / 180.0) * Math.PI);
//         sinrY = Math.sin((rY / 180.0) * Math.PI);
//         cosrZ = Math.cos((rZ / 180.0) * Math.PI);
//         sinrZ = Math.sin((rZ / 180.0) * Math.PI);

//         M00 = cosrZ * cosrY * sX;
//         M01 = -sinrZ * cosrY * sY;
//         M02 = sinrY * sZ;
//         M03 = tX;
//         M10 = ((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * sX;
//         M11 = ((-sinrZ * sinrY * sinrX) + (cosrZ * cosrX)) * sY;
//         M12 = -cosrY * sinrX * sZ;
//         M13 = tY;
//         M20 = ((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * sX;
//         M21 = ((sinrZ * sinrY * cosrX) + (cosrZ * sinrX)) * sY;
//         M22 = cosrY * cosrX * sZ;
//         M23 = tZ;

//         M30 = M31 = M32 = 0;
//         M33 = 1;

//         return;
//     }

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
        //System.err.println( rX + " " + rY + " " + rZ + " " + tX + " " + tY + " " + tZ + " " +
        //        sX + " " + sY + " " + sZ + " " + skX + " " + skY + " " + skZ );
        
        double sinrX, sinrY, sinrZ, cosrX, cosrY, cosrZ;
        cosrX = Math.cos((rX / 180.0) * Math.PI);
        sinrX = Math.sin((rX / 180.0) * Math.PI);
        cosrY = Math.cos((rY / 180.0) * Math.PI);
        sinrY = Math.sin((rY / 180.0) * Math.PI);
        cosrZ = Math.cos((rZ / 180.0) * Math.PI);
        sinrZ = Math.sin((rZ / 180.0) * Math.PI);

        M00 = (cosrZ * cosrY * sX);
        M01 = (((cosrZ * cosrY * skX) - (sinrZ * cosrY)) * sY);
        M02 = (((cosrZ * cosrY * skY) - (sinrZ * cosrY * skZ) + sinrY) * sZ);
        M03 = tX;
        M10 = (((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * sX);
        M11 = (((((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * skX) - (sinrZ * sinrY * sinrX) +
                        (cosrZ * cosrX)) * sY);
        M12 = (((((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * skY) +
                        (((-sinrZ * sinrY * sinrX) + (cosrZ * cosrX)) * skZ) - (cosrY * sinrX)) * sZ);
        M13 = tY;
        M20 = (((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * sX);
        M21 = (((((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * skX) + (sinrZ * sinrY * cosrX) +
                        (cosrZ * sinrX)) * sY);
        M22 = (((((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * skY) +
                        (((sinrZ * sinrY * cosrX) + (cosrZ * sinrX)) * skZ) + (cosrY * cosrX)) * sZ);
        M23 = tZ;

        M30 = M31 = M32 = 0;
        M33 = 1;

        return;
    }


    /**
     * Sets translation parts of 2D matrix.
     *
     * @param  x  x translation
     * @param  y  y translation
     */
    public void setTranslate(double x, double y) {
        assert(m_Is2D);
        M02 = ((x * M00) + (y * M01) + M02);
        M12 = ((x * M10) + (y * M11) + M12);
    }

    /**
     * Sets the translation parts of 3D matrix (4D Homogenous).
     *
     * @param  x  x translation
     * @param  y  y translation
     * @param  z  z translation
     */
    public void setTranslate(double x, double y, double z) {
        M03 = ((x * M00) + (y * M01) + (z * M02) + M03);
        M13 = ((x * M10) + (y * M11) + (z * M12) + M13);
        M23 = ((x * M20) + (y * M21) + (z * M22) + M23);
    }

    /**
     * Sets the zoom parts of 2D matrix.
     *
     * @param  sx  zoom in the x coordinate
     * @param  sy  zoom in the y coordinate
     */
    public void setZoom(double sx, double sy) {
        assert(m_Is2D);

        M00 = sx * M00;
        M10 = sx * M10;

        M01 = sy * M01;
        M11 = sy * M11;
    }

    /**
     * Sets the zoom parts of 3D matrix.
     *
     * @param  sx  zoom in the x coordinate
     * @param  sy  zoom in the y coordinate
     * @param  sz  zoom in the z coordinate
     */
    public void setZoom(double sx, double sy, double sz) {

        M00 = sx * M00;
        M10 = sx * M10;
        M20 = sx * M20;

        M01 = sy * M01;
        M11 = sy * M11;
        M21 = sy * M21;

        M02 = sz * M02;
        M12 = sz * M12;
        M22 = sz * M22;
    }

    /**
     * Produces a string of the matrix values, rows separated by tabs.
     *
     * @return  formatted string
     */
    public String toDialogString() {
        String s = new String();

        DecimalFormat format = new DecimalFormat();
        format.setMinimumIntegerDigits(1);
        format.setMaximumFractionDigits(4);
        format.setMinimumFractionDigits(4);
        format.setGroupingUsed(false);

        for (int i = 0; i < getDim(); i++) {
            s += "  ";

            for (int j = 0; j < getDim(); j++) {
                s += format.format(get(i, j)); // format the number
                s = s + "  ";
            }

            s = s + "\t";
        }

        return s;
    }

    /**
     * Tranforms a Polygon (2D).
     *
     * @param   gon  input polygon
     *
     * @return  returns new transformed Polygon
     */
    public final Polygon transform(Polygon gon) {
        Polygon newGon = new Polygon();
        int length = gon.npoints;
        int newX, newY;

        for (int n = 0; n < length; n++) {
            newX = (int)Math.round((gon.xpoints[n] * M00) + (gon.ypoints[n] * M01) + M02);
            newY = (int)Math.round((gon.xpoints[n] * M10) + (gon.ypoints[n] * M11) + M12);
            newGon.addPoint(newX, newY);
        }

        return newGon;
    }


    /**
     * Takes an array of Point2Dd 2D vectors and multiplies them with the 2d
     * transformation matrix.
     *
     * @param  vects   double vectors to be transformed
     * @param  tVects  transformed vectors
     */
    public final void transformAsPoint2Dd(Vector2d[] vects, Vector2d[] tVects) {
        int n;
        int length = vects.length;

        for (n = 0; n < length; n++) {
            tVects[n].X = ((vects[n].X * M00) +
                                   (vects[n].Y * M01) +
                                   M02);
            
            tVects[n].Y = ((vects[n].X * M10) +
                                   (vects[n].Y * M11) +
                                   M12);

        }

        return;
    }

    /**
     * Takes an array of Vector3Dd 2D vectors and multiplies them with the 2d
     * transformation matrix.
     *
     * @param  vects   double vectors to be transformed
     * @param  tVects  transformed vectors
     */
    public final void transformAsVector3Dd(Vector3d[] vects, Vector3d[] tVects) {
        int n;
        int length = vects.length;

        for (n = 0; n < length; n++) {
            tVects[n].X = ((vects[n].X * M00) +
                                   (vects[n].Y * M01) +
                                   M02);

            tVects[n].Y = ((vects[n].X * M10) +
                                   (vects[n].Y * M11) +
                                   M12);

            tVects[n].Z = 1;
        }

        return;
    }
    
    /**
     * Takes an array of Vector3Dd 2D vectors and multiplies them with the 2d
     * transformation matrix.
     *
     * @param  vects   double vectors to be transformed
     * @param  tVects  transformed vectors
     */
    public final void transformAsVector3Dd(VOIBase vects, Vector3d[] tVects) {
        int n;
        int length = vects.size();

        for (n = 0; n < length; n++) {
            tVects[n].X = ((vects.elementAt(n).X * M00) +
                                   (vects.elementAt(n).Y * M01) +
                                   M02);

            tVects[n].Y = ((vects.elementAt(n).X * M10) +
                                   (vects.elementAt(n).Y * M11) +
                                   M12);

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
            tPt[0] = (pt[0] * M00) +
                (pt[1] * M01) +
                (pt[2] * M02) +
                M03;

            tPt[1] = (pt[0] * M10) +
                (pt[1] * M11) +
                (pt[2] * M12) +
                M13;

            tPt[2] = (pt[0] * M20) +
                (pt[1] * M21) +
                (pt[2] * M22) +
                M23;


        } else if (pt.length == 2) {
            tPt[0] = (pt[0] * M00) + (pt[1] * M01) + M02;
            tPt[1] = (pt[0] * M10) + (pt[1] * M11) + M12;
            
            
            
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

        tPt[0] = (float) (((double) pt[0] * M00) +
                          ((double) pt[1] * M01) +
                          ((double) pt[2] * M02) +
                          M03);

        tPt[1] = (float) (((double) pt[0] * M10) +
                          ((double) pt[1] * M11) +
                          ((double) pt[2] * M12) +
                          M13);

        tPt[2] = (float) (((double) pt[0] * M20) +
                          ((double) pt[1] * M21) +
                          ((double) pt[2] * M22) +
                          M23);

        return;
    }

    /**
     * Takes a Point3Dd 3D point and multiplies it by the 3D transformation
     * matrix.
     *
     * @param  pt   3D double point to be transformed
     * @param  tPt  transformed point
     */
    public final void transformAsPoint3Dd(Vector3d pt, Vector3d tPt) {

        tPt.X = ((pt.X * M00) +
                         (pt.Y * M01) +
                         (pt.Z * M02) +
                         M03);

        tPt.Y = ((pt.X * M10) +
                         (pt.Y * M11) +
                         (pt.Z * M12) +
                         M13);

        tPt.Z = ((pt.X * M20) +
                         (pt.Y * M21) +
                         (pt.Z * M22) +
                         M23);

        return;
    }


    /**
     * Takes double components of a 2D point and premultiplies it by the 2d
     * transformation matrix.
     *
     * @param  x    x coordinate to be transformd
     * @param  y    y coordinate to be transformd
     * @param  tPt  transformed point
     */
    public final void transform(double x, double y, double[] tPt) {

        tPt[0] = (x * M00) + (y * M01) + M02;
        tPt[1] = (x * M10) + (y * M11) + M12;

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

        tPt[0] = (float) (((double) x * M00) +
                          ((double) y * M01) +
                          M02);
        tPt[1] = (float) (((double) x * M10) +
                          ((double) y * M11) +
                          M12);

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

        tPt[0] = (x * M00) +
            (y * M01) +
            (z * M02) +
            M03;

        tPt[1] = (x * M10) +
            (y * M11) +
            (z * M12) +
            M13;

        tPt[2] = (x * M20) +
            (y * M21) +
            (z * M22) +
            M23;

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

        tPt[0] = (float) (( x * M00) +
                          ( y * M01) +
                          ( z * M02) +
                          M03);

        tPt[1] = (float)(( x * M10) +
                          ( y * M11) +
                          ( z * M12) +
                          M13);

        tPt[2] = (float)(( x * M20) +
                          ( y * M21) +
                          ( z * M22) +
                          M23);

        return;
    }


    /**
     * Returns the transform ID associated with the matrix.
     *
     * @return  int transform ID
     */
    public final int getTransformID() {
        return m_TransformID;
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

            for (int i = 0; i < TRANSFORM_ID_STR.length; i++) {

                if (TransMatrixd.getTransformIDStr(i).regionMatches(true, 0, s, 0,
                		TransMatrixd.getTransformIDStr(i).length())) {
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
        return TRANSFORM_ID_STR;
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
            return TransMatrixd.TRANSFORM_ID_STR[m];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";
    }
    
    /**
     * Sets the transform ID for the matrix.
     *
     * @param  t_id  transform ID
     */
    public void setTransformID(int t_id) {
        m_TransformID = t_id;
    }
    
    /**
     * ToString method that includes the matrix printout as well as the
     * transform ID of the transmatrixd
     * @return printout
     */
    public String toString() {
    	String s = new String("TransMatrixd: ");
    	s += "\n\ttransform id: " + TransMatrixd.getTransformIDStr(this.m_TransformID);
        s += "\n";
        s += matrixToString(10, 4);

    	return s;
    }
    
    /**
     * Decodes the line in the matrix file.
     *
     * @param  raFile  file pointer
     * @param  row     row reference to store transformation matrix
     * @param  matrix  the matrix where the data is to be stored
     */
    private void decodeLine(RandomAccessFile raFile, int row, Matrix4d matrix) {
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

            for (c = 0; c < getDim(); c++) {

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
                } else { // spaces trimmed from end
                    tmpStr = str.substring(index, str.length()).trim();
                    index = nextIndex;
                }
                if (tmpStr.indexOf(".") != -1) {
                    matrix.set(row, c, Double.valueOf(tmpStr).doubleValue());
                } else {
                    matrix.set(row, c, Integer.valueOf(tmpStr).doubleValue());
                }
            }

        } catch (IOException error) {
            MipavUtil.displayError("Matrix read error " + error);

            return;
        }
    }
    
    /**
     * Matrix inversion that replaces this objects matrix with an inverted
     * matrix. Special handling for 2D vs 3D cases.
     */
    public void Inverse ()
    {
        if (m_Is2D) {
            // borrow code from Matrix3d,
            // so that we don't construct and destroy a Matrix 3d.
        double inverse_M00 =
            M11*M22 - M12*M21;
        double inverse_M01 =
            M02*M21 - M01*M22;
        double inverse_M02 =
            M01*M12 - M02*M11;
        double inverse_M10 =
            M12*M20 - M10*M22;
        double inverse_M11 =
            M00*M22 - M02*M20;
        double inverse_M12 =
            M02*M10 - M00*M12;
        double inverse_M20 =
            M10*M21 - M11*M20;
        double inverse_M21 =
            M01*M20 - M00*M21;
        double inverse_M22 =
            M00*M11 - M01*M10;

        double dDet =
            M00*inverse_M00 +
            M01*inverse_M10 +
            M02*inverse_M20;

        if (Math.abs(dDet) <= Mathd.ZERO_TOLERANCE)
        {
        	copy(Matrix4d.ZERO);
        }

        double dInvDet = 1.0/dDet;
        inverse_M00 *= dInvDet;
        inverse_M01 *= dInvDet;
        inverse_M02 *= dInvDet;
        inverse_M10 *= dInvDet;
        inverse_M11 *= dInvDet;
        inverse_M12 *= dInvDet;
        inverse_M20 *= dInvDet;
        inverse_M21 *= dInvDet;
        inverse_M22 *= dInvDet;
        // Set 4x4, even though we're only using a 3x3
        set( inverse_M00, inverse_M01, inverse_M02, 0.0,
             inverse_M10, inverse_M11, inverse_M12, 0.0,
             inverse_M20, inverse_M21, inverse_M22, 0.0, 
             0.0, 0.0, 0.0, 1.0);
        } else {
            super.inverse();
        }
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

        for (i = 0; i < getDim(); i++) {
            s += "  ";

            for (j = 0; j < getDim(); j++) {
                s += format.format(get(i,j)); // format the number
                s += "  ";
            }

            s += "\n";
        }

        return s;
    }
    
    



}
