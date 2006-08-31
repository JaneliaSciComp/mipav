package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.Toolkit;
import java.io.*;
import java.util.zip.GZIPInputStream;


/**
 * The class reads MGH files.  The 4 by 4
 * matrix in MGH transforms x,y,z indexes to (right, anterior, superior) coordinates where +x = Right, +y = Anterior, +z = Superior. In
 * MIPAV the 4 by 4 matrix does not imply the axis orientations.</p>
 * 
 * The MGH definition is:
 * [right]       [xr yr zr] [       xsize * x]   [rorigin]
 * [anterior] =  [xa ya za] [       ysize * y] + [aorigin]
 * [superior]    [xs ys zs] [       zsize * z]   [sorigin]
 * Now in going to MIPAV a change must occur.
 * MGH has L->R and P->A while MIPAV uses R->L and A->P, so this would cause
 * xr, yr, zr, rorigin, xa, ya, za, and aorigin to be multiplied by -1.
 
 A 284 byte header always precedes the data buffer
 In version 1 if the goodRASFlag <= 0, then only the first 30 bytes are read.  
 If the goodRASFlag > 0, then the first 90 bytes are read
 
  Immediately after the data buffer other optional data structures may be found:
  They are:
  Recovery time float
  Flip angle in radians float
  Echo time float
  Inversion time float
  Field of view float
  Lastly, tags including the path to a talairach transform, and a list of commands used
  to create this data(provenance info) may be present.  The reading of the Talairach
  transformation information is not currently implemented.  I am missing the documentation
  on TAGreadStart, TAG_OLD_MGH_XFORM, TAG_MGH_XFORM, TAG_CMDLINE, fio_dirname, 
  input_transform_file, get_linear_transform_ptr, get_inverse_linear_transform_ptr, and
  TAGskip.
  
  What information I have about the Talairach transformation is as follows:
  "FreeSurfer uses some of the tools developed at the Montreal Neurological Institute (MNI)
  (http://www.bic.mni.mcgill.ca) to align anatomical volumes (using minctracc) and to compute
  the linear Talairach transform (using mritotal).  The mritotal documentation says that it
  generally works well for T1 dominant images.  The transform is contained in a 3x4 matrix
  file called talairach.xfm and is located in the <subject name>/mri/transforms directory.
  
  The content of one talairach.xfm file was:
  MNI Transform File
%Tue Feb 28 18:17:20 2006>>> /usr/local/freesurfer/stable3/mni/bin/minctracc -clobber -debug /tmp/
mritotal_935/src_8_dxyz.mnc /usr/local/freesurfer/stable3/mni/bin/../share/mni_autoreg/
average_305_8_dxyz.mnc transforms/talairach.auto.xfm -transformation /tmp/mritotal_935/
src_8tmp2c.xfm -lsq9 -xcorr -model_mask /usr/local/freesurfer/stable3/mni/bin/../share/mni_autoreg/
average_305_8_mask.mnc -center 6.976350 19.544212 -19.984585 -step 4 4 4 -tol 0.004 -simplex 2
%(Package MNI AutoReg, version 0.98r, compiled by nicks@minerva (x86_64-unknown-linux-gnu) on
 2005-11-15 at 20:38:24)

Transform_Type = Linear;
Linear_Transform =
 1.15497899055481 -0.023911377415061 0.0364909172058105 -5.71282863616943
 0.0152252428233624 0.942800223827362 0.13589183986187 -21.7210540771484
 -0.0388212203979492 -0.161248967051506 1.1230742931366 -0.750320434570312;
 
 mritotal is in the package mni_autoreg.  mritotal uses the MNI average_305
 template volume to create the linear talairach affine transform.  The template
 volume average_305.mnc resides in $FREESURFER_HOME/lib/mni/share/mni_autoreg
 directory in the MGH environment.

 </p>
 *
 * @see  FileIO
 * @see  FileInfoMGH
 * @see  FileRaw
 */

public class FileMGH extends FileBase {
    //~ Static fields/initializers -------------------------------------------
    
    private static final int MRI_UCHAR = 0;
    
    private static final int MRI_INT = 1;
    
    private static final int MRI_LONG = 2;
    
    private static final int MRI_FLOAT = 3;
    
    private static final int MRI_SHORT = 4;
    
    private static final int MRI_BITMAP = 5;
    
    private static final int MRI_TENSOR = 6;
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] axisOrientation;

    /** DOCUMENT ME! */
    private String fileDir;
    
    private File file;
    
    private boolean gunzip;
    
    private FileInputStream fis;

    /** DOCUMENT ME! */
    private FileInfoMGH fileInfo = null;

    /** DOCUMENT ME! */
    private String fileName;
    
    private long fileLength;
    
    /** Present version number is 1 */
    private int version;
    
    private int width;
    
    private int height;
    
    private int depth;
    
    private int nFrames;
    
    private int extents[];
    
    private int mghType;
    
    private int dataType = ModelStorageBase.UBYTE;
    
    private int bytesPerValue;
    
    private int dof;
    
    private short goodRASFlag;
    
    private float resolutions[] = new float[3];
    
    private float xr;
    
    private float xa;
    
    private float xs;
    
    private float yr;
    
    private float ya;
    
    private float ys;
    
    private float zr;
    
    private float za;
    
    private float zs;
    
    private float cr;
    
    private float ca;
    
    private float cs;
    
    private long optionalStructuresLocation;
    
    private float tr;
    
    private float flipAngle;
    
    private float te;
    
    private float ti;
    
    private float fov;
    
    private TransMatrix matrix = new TransMatrix(4);

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private float[] origin;
    
    
    /** DOCUMENT ME! */
    private boolean showProgress = true;

    

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  _UI    User interface.
     * @param  fName  File name.
     * @param  fDir   File directory.
     * @param  show   Flag for showing the progress bar.
     */
    public FileMGH(ViewUserInterface _UI, String fName, String fDir, boolean show) {
        UI = _UI;
        fileName = fName;
        fileDir = fDir;
        showProgress = show;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

   

    /**
     * Returns the FileInfoMGH read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoMGH getFileInfo() {
        return fileInfo;
    }

    /**
     * Reads the MGH header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoMGH
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        int j;
        int index;
        boolean endianess;

        index = fileName.lastIndexOf(".");

        if ((fileName.substring(index + 1).equalsIgnoreCase("mgz")) ||
            (fileName.substring(index + 1).equalsIgnoreCase("gz"))) {
            gunzip = true;
        } else {
            gunzip = false;
        }

        file = new File(fileDir + fileName);

        if (gunzip) {
            int totalBytesRead = 0;
            progressBar.setVisible(isProgressBarVisible());
            progressBar.setMessage("Uncompressing GZIP file ...");
            fis = new FileInputStream(file);

            GZIPInputStream gzin = new GZIPInputStream(new BufferedInputStream(fis));
            if (fileName.substring(index+1).equalsIgnoreCase("mgz")) {
                 fileName = fileName.substring(0,index).concat(".mgh");
            }
            else {
                fileName = fileName.substring(0,index);
            }
            String uncompressedName = fileDir + fileName;
            FileOutputStream out = new FileOutputStream(uncompressedName);
            byte[] buffer = new byte[256];

            while (true) {
                int bytesRead = gzin.read(buffer);

                if (bytesRead == -1) {
                    break;
                }

                totalBytesRead += bytesRead;
                out.write(buffer, 0, bytesRead);
            }

            out.close();
            file = new File(uncompressedName);
            fileInfo.setFileName(fileName);
        } // if (gunzip)

        raFile = new RandomAccessFile(file, "r");
        fileLength = raFile.length();

        // MGH file are always BIG_ENDIAN
        endianess = BIG_ENDIAN;
        fileInfo.setEndianess(BIG_ENDIAN);
       
        // Current value of version number is 1
        version = getInt(endianess); // 0
        fileInfo.setVersion(version);
        
        // First dimension of the image buffer
        width = getInt(endianess); // 4
        
        // Second dimension of the image buffer
        height = getInt(endianess); // 8
        
        // Third dimension of the image buffer
        depth = getInt(endianess); // 12
        
        // Foruth dimension of the image buffer
        nFrames = getInt(endianess); // 16
        
        if (nFrames > 1) {
            extents = new int[4];
            extents[0] = width;
            extents[1] = height;
            extents[2] = depth;
            extents[3] = nFrames;
        } // if (nFrames > 1)
        else if (depth > 1) {
            extents = new int[3];
            extents[0] = width;
            extents[1] = height;
            extents[2] = depth;
        }
        else {
            extents = new int[2];
            extents[0] = width;
            extents[1] = height;
        }
        fileInfo.setExtents(extents);
        
        mghType = getInt(endianess); // 20
        switch (mghType) {
            case MRI_UCHAR:
                dataType = ModelStorageBase.UBYTE;
                bytesPerValue = 1;
                break;
            case MRI_SHORT:
                dataType = ModelStorageBase.SHORT;
                bytesPerValue = 2;
                break;
            case MRI_INT:
                dataType = ModelStorageBase.INTEGER;
                bytesPerValue = 4;
                break;
            case MRI_LONG:
                dataType = ModelStorageBase.LONG;
                bytesPerValue = 8;
                break;
            case MRI_FLOAT:
                dataType = ModelStorageBase.FLOAT;
                bytesPerValue = 4;
                break;
            case MRI_TENSOR:
                dataType = ModelStorageBase.FLOAT;
                bytesPerValue = 4;
                if (nFrames != 9) {
                    nFrames = 9;
                    extents = new int[4];
                    extents[0] = width;
                    extents[1] = height;
                    extents[2] = depth;
                    extents[3] = nFrames;
                    fileInfo.setExtents(extents);
                } // if (nFrames != 9)
                break;
            case MRI_BITMAP:
                Preferences.debug("Cannot handle type = MRI_BITMAP");
                return false;
        } // switch (mghType)
         // raFile.close();
        fileInfo.setDataType(dataType);
        
        // Degrees of freedom
        dof = getInt(endianess); // 24
        fileInfo.setDOF(dof);
        
        goodRASFlag = (short)getSignedShort(endianess); // 28
        // The x, y, and z variables define the rotational part
        // of the affine transform.
        // The "c_ras" values define where the volume center 
        // sits in the RAS coordinate system.  That is, c_r,
        // c_a, c_s are the RAS coordinate values of a voxel
        // point (width/2, height/2, depth/2).  The convention
        // used is that the center of a voxel corresponds to 
        // the integer voxel coordinate position.
        if (goodRASFlag > 0) {
            resolutions[0] = getFloat(endianess); // 30
            resolutions[1] = getFloat(endianess); // 34
            resolutions[2] = getFloat(endianess); // 38
            xr = getFloat(endianess); // 42
            xa = getFloat(endianess); // 46
            xs = getFloat(endianess); // 50
            yr = getFloat(endianess); // 54
            ya = getFloat(endianess); // 58
            ys = getFloat(endianess); // 62
            zr = getFloat(endianess); // 66
            za = getFloat(endianess); // 70
            zs = getFloat(endianess); // 74
            cr = getFloat(endianess); // 78
            ca = getFloat(endianess); // 82
            cs = getFloat(endianess); // 86
        } // if (goodRASFlag > 0)
        else {
            // Default coronal orientation with z axis to the left,
            // y axis inferior, and z axis anterior
            resolutions[0] = 1.0f;
            resolutions[1] = 1.0f;
            resolutions[2] = 1.0f;
            xr = -1.0f;
            xa = 0.0f;
            xs = 0.0f;
            yr = 0.0f;
            ya = 0.0f;
            ys = -1.0f;
            zr = 0.0f;
            za = 1.0f;
            zs = 0.0f;
            cr = 0.0f;
            ca = 0.0f;
            cs = 0.0f;
        }
        matrix.setMatrix((double)-xr*resolutions[0], 0, 0);
        matrix.setMatrix((double)-yr*resolutions[1], 0, 1);
        matrix.setMatrix((double)-zr*resolutions[2], 0, 2);
        
        matrix.setMatrix((double)-xa*resolutions[0], 1, 0);
        matrix.setMatrix((double)-ya*resolutions[1], 1, 1);
        matrix.setMatrix((double)-za*resolutions[2], 1, 2);
        
        matrix.setMatrix((double)xs*resolutions[0], 2, 0);
        matrix.setMatrix((double)ys*resolutions[1], 2, 1);
        matrix.setMatrix((double)zs*resolutions[2], 2, 2);
        
        
        axisOrientation = getAxisOrientation(matrix);
        Preferences.debug("axisOrientation = " + axisOrientation[0] + "  " + axisOrientation[1] + "  " +
                          axisOrientation[2] + "\n");
        fileInfo.setAxisOrientation(axisOrientation);
        
        if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) ||
                (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
        } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                       (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
        } else {
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
        }
        
        origin = new float[3];
        for (j = 0; j < 3; j++) {

            if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE){
                origin[j] = -(width/2.0f)*resolutions[0]; 
            }
            else if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                origin[j] = (width/2.0f)*resolutions[0];    
            }
            else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE){
                origin[j] = -(height/2.0f)*resolutions[1];      
            }
            else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                origin[j] = (height/2.0f)*resolutions[1];
            }
            else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE){
                origin[j] = -(depth/2.0f)*resolutions[2];
            }
            else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                origin[j] = (depth/2.0f)*resolutions[2];
            }
        }
        
        fileInfo.setOrigin(origin);
        matrix.setMatrix((double) origin[0], 0, 3);
        matrix.setMatrix((double) origin[1], 1, 3);
        matrix.setMatrix((double) origin[2], 2, 3);
        Preferences.debug("matrix = \n" + matrix + "\n");
        fileInfo.setMatrix(matrix);
        
        fileInfo.setLeftCenter(-cr);
        fileInfo.setPosteriorCenter(-ca);
        fileInfo.setSuperiorCenter(cs);
        
        optionalStructuresLocation = 284 + bytesPerValue*width*height*depth*nFrames;
        if (fileLength >= optionalStructuresLocation + 4) {
            raFile.seek(optionalStructuresLocation);
            // Recovery time in milliseconds
            tr = getFloat(endianess);
            fileInfo.setTR(tr);
        }
        
        if (fileLength >= optionalStructuresLocation + 8) {
            // Flip angle in radians
            flipAngle = getFloat(endianess);
            fileInfo.setFlipAngle(flipAngle);
        }
        
        if (fileLength >= optionalStructuresLocation + 12) {
            // Echo time in milliseconds
            te = getFloat(endianess);
            fileInfo.setTE(te);
        }
        
        if (fileLength >= optionalStructuresLocation + 16) {
            // Inversion time in milliseconds
            ti = getFloat(endianess);
            fileInfo.setTI(ti);
        }
        
        if (fileLength >= optionalStructuresLocation + 20) {
            // Field of view in millimeters
            fov = getFloat(endianess);
            fileInfo.setFOV(fov);
        }
        
        raFile.close();
        return true; // If it got this far, it has successfully read in the header
    }
    
    

    /**
     * Reads a MGH image file by reading the header then making a FileRaw to read the image for all filenames in the
     * file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        int offset;
        
        progressBar = new ViewJProgressBar(ViewUserInterface.getReference().getProgressBarPrefix() + fileName,
                ViewUserInterface.getReference().getProgressBarPrefix() + "MGH image(s) ...",
                0, 100, false, null, null);
        progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);
        setProgressBarVisible(ViewUserInterface.getReference().isAppFrameVisible());
        
        fileInfo = new FileInfoMGH(fileName, fileDir, FileBase.MGH);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" MGH header file error"));
        }

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                       fileInfo.getFileName(), UI);
            } else {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName(), UI);
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        attachFileInfo(fileInfo, image);
        updateorigins(image.getFileInfo());
        image.setMatrix(matrix);

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, showProgress,
                                  FileBase.READ);

            offset = 284;

            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = offset + getOffset();
                }
            }

            rawFile.readImage(image, offset);
            
            if (one) {
                fileInfo.setExtents(extents);
            }
            
            if (progressBar != null) {
                progressBar.dispose();
            }
        } catch (IOException error) {
            throw new IOException("FileMGH: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return image;
    }

    /**
     * Reads a MGH image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first. Image is not 'flipped', and
     * neither units of measure nor orientation are set.
     *
     * @param      buffer  Image buffer to store image data into.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {
        int offset;

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoMGH(fileName, fileDir, FileBase.MGH);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
                throw (new IOException("Cannot read image because of MGH header file error"));
            }
        }

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, showProgress,
                                  FileBase.READ);

            offset = 284;

            rawFile.readImage(buffer, offset, dataType); 
            rawFile.raFile.close();
        } catch (IOException error) {
            throw new IOException("FileMGH: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }

    
    

    /**
     * Return the 3 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j,k)
     * axes.
     *
     * @param   mat  4x4 matrix that transforms (i,j,k) indexes to x,y,z coordinates where +x =Left, +y = Posterior, +z
     *               = Superior Only the upper-left 3x3 corner of the matrix is used This routine finds the permutation
     *               of (x,y,z) which has the smallest angle to the (i,j,k) axes directions, which are columns of the
     *               input matrix Errors: The codes returned will be zero.
     *
     * @return  DOCUMENT ME!
     */
    private int[] getAxisOrientation(TransMatrix mat) {
        int[] axisOrientation = new int[3];
        double[][] array;
        double xi, xj, xk, yi, yj, yk, zi, zj, zk, val;
        Matrix Q;
        double detQ;
        double vbest;
        int ibest, jbest, kbest, pbest, qbest, rbest;
        int i, j, k, p, q, r;
        Matrix P;
        double detP;
        Matrix M;

        array = mat.getMatrix(0, 2, 0, 2).getArray();
        xi = array[0][0];
        xj = array[0][1];
        xk = array[0][2];
        yi = array[1][0];
        yj = array[1][1];
        yk = array[1][2];
        zi = array[2][0];
        zj = array[2][1];
        zk = array[2][2];

        int izero = 0;
        int jzero = 0;
        int kzero = 0;
        int xzero = 0;
        int yzero = 0;
        int zzero = 0;

        if (xi == 0.0) {
            izero++;
            xzero++;
        }

        if (yi == 0.0) {
            izero++;
            yzero++;
        }

        if (zi == 0.0) {
            izero++;
            zzero++;
        }

        if (xj == 0.0) {
            jzero++;
            xzero++;
        }

        if (yj == 0.0) {
            jzero++;
            yzero++;
        }

        if (zj == 0.0) {
            jzero++;
            zzero++;
        }

        if (xk == 0.0) {
            kzero++;
            xzero++;
        }

        if (yk == 0.0) {
            kzero++;
            yzero++;
        }

        if (zk == 0.0) {
            kzero++;
            zzero++;
        }

        if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2)) {

            if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
            }

            return axisOrientation;
        } // if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2))

        // Normalize column vectors to get unit vectors along each ijk-axis

        // Normalize i axis
        val = Math.sqrt((xi * xi) + (yi * yi) + (zi * zi));

        if (val == 0.0) {
            MipavUtil.displayError("xi = yi = zi = 0 in getAxisOrientation");

            return null;
        }

        xi /= val;
        yi /= val;
        zi /= val;

        // Normalize j axis
        val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj));

        if (val == 0.0) {
            MipavUtil.displayError("xj = yj = zj = 0 in getAxisOrientation");

            return null;
        }

        xj /= val;
        yj /= val;
        zj /= val;

        // Orthogonalize j axis to i axis, if needed
        val = (xi * xj) + (yi * yj) + (zi * zj); // dot product between i and j

        if (Math.abs(val) > 1.0e-4) {
            xj -= val * xi;
            yj -= val * yi;
            zj -= val * zi;
            val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj)); // Must renormalize

            if (val == 0.0) {
                MipavUtil.displayError("j was parallel to i in getAxisOrientation");

                return null;
            }

            xj /= val;
            yj /= val;
            zj /= val;
        }

        // Normalize k axis; if it is zero, make it the cross product i x j
        val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

        if (val == 0.0) {
            xk = (yi * zj) - (zi * yj);
            yk = (zi * xj) - (zj * xi);
            zk = (xi * yj) - (yi * xj);
        } else {
            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to i
        val = (xi * xk) + (yi * yk) + (zi * zk); // dot product between i and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xi;
            yk -= val * yi;
            zk -= val * zi;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to i");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to j
        val = (xj * xk) + (yj * yk) + (zj * zk); // dot product between j and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xj;
            yk -= val * yj;
            zk -= val * zj;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to j");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        array[0][0] = xi;
        array[0][1] = xj;
        array[0][2] = xk;
        array[1][0] = yi;
        array[1][1] = yj;
        array[1][2] = yk;
        array[2][0] = zi;
        array[2][1] = zj;
        array[2][2] = zk;

        // At this point, Q is the rotation matrix from the (i,j,k) to the (x,y,z) axes
        Q = new Matrix(array);
        detQ = Q.det();

        if (detQ == 0.0) {
            MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");

            return null;
        }

        // Build and test all possible +1/-1 coordinate permutation matrices P;
        // then find the P such that the rotation matrix M=PQ is closest to the
        // identity, in the sense of M having the smallest total rotation angle

        // Despite the formidable looking 6 nested loops, there are
        // only 3*3*3*2*2*2 = 216 passes, which will run very quickly
        vbest = -Double.MAX_VALUE;
        pbest = 1;
        qbest = 1;
        rbest = 1;
        ibest = 1;
        jbest = 2;
        kbest = 3;

        for (i = 1; i <= 3; i++) { // i = column number to use for row #1

            for (j = 1; j <= 3; j++) { // j = column number to use for row #2

                if (i == j) {
                    continue;
                }

                for (k = 1; k <= 3; k++) { // k = column number to use for row #3

                    if ((i == k) || (j == k)) {
                        continue;
                    }

                    array[0][0] = 0.0;
                    array[0][1] = 0.0;
                    array[0][2] = 0.0;
                    array[1][0] = 0.0;
                    array[1][1] = 0.0;
                    array[1][2] = 0.0;
                    array[2][0] = 0.0;
                    array[2][1] = 0.0;
                    array[2][2] = 0.0;
                    P = new Matrix(array);

                    for (p = -1; p <= 1; p += 2) { // p,q,r are -1 or +1 and go into rows #1,2,3

                        for (q = -1; q <= 1; q += 2) {

                            for (r = -1; r <= 1; r += 2) {
                                P.set(0, i - 1, p);
                                P.set(1, j - 1, q);
                                P.set(2, k - 1, r);
                                detP = P.det();

                                // sign of permutation doesn't match sign of Q
                                if ((detP * detQ) <= 0.0) {
                                    continue;
                                }

                                M = P.times(Q);

                                // angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))
                                // we want largest trace(M) == smallest angle == M nearest to I
                                val = M.get(0, 0) + M.get(1, 1) + M.get(2, 2); // trace

                                if (val > vbest) {
                                    vbest = val;
                                    ibest = i;
                                    jbest = j;
                                    kbest = k;
                                    pbest = p;
                                    qbest = q;
                                    rbest = r;
                                }
                            }
                        }
                    }
                }
            }
        }

        // At this point ibest is 1 or 2 or 3; pbest is -1 or +1; etc.

        // The matrix P that corresponds is the best permutation approximation
        // to Q-inverse; that is, P (approximately) takes (x,y,z) coordinates
        // to the (i,j,k) axes

        // For example, the first row of P (which contains pbest in column ibest)
        // determines the way the i axis points relative to the anatomical
        // (x,y,z) axes.  If ibest is 2, then the i axis is along the yaxis,
        // which is direction P2A (if pbest < 0) or A2P (if pbest > 0).

        // So, using ibest and pbest, we can assign the output code for
        // the i axis.  The same also applies for the j and k axes.

        switch (ibest * pbest) {

            case -1:
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case -1:
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (kbest * rbest) {

            case -1:
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        return axisOrientation;
    }

    /**
     * Helper method to calculate the offset for getting only the middle NIFTI image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset() {
        int offset = width * height * depth / 2;

        switch (dataType) {

            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
                offset *= 2;
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.FLOAT:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
                offset *= 8;
                break;

            
        }

        return offset;
    }

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateorigins(FileInfoBase[] fileInfo) {
        int axisOrient;

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin);
                axisOrient = fileInfo[i].getAxisOrientation(2);

                if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_A2P_TYPE) ||
                        (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                    origin[2] += resolutions[2];
                } else { // ORI_L2R_TYPE, ORI_P2A_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
                }
            }
        } else if (image.getNDims() == 4) {
            float tmp = origin[2];

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin);
                    axisOrient = fileInfo[i].getAxisOrientation(2);

                    if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_A2P_TYPE) ||
                            (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                        origin[2] += resolutions[2];
                    } else { // ORI_L2R_TYPE, ORI_P2A_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                    }
                }

                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
        /*else if (image.getNDims() == 5) {
         * fileInfo = image.getFileInfo(); for (int i = 0;    i <    image.getExtents()[2] * image.getExtents()[3] *
         * image.getExtents()[4];    i++) { fileInfo[i].setorigins(startLocs); startLocs[4] += resolutions[4]; }  }*/
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fileInfo  -- a NIFTI file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    private void attachFileInfo(FileInfoMGH fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

        if (image.getNDims() == 2) {
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image
            for (int i = 0; i < extents[2]; i++) {
                FileInfoMGH newFileInfo = (FileInfoMGH) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoMGH newFileInfo = (FileInfoMGH) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    } // end updateUnitsOfMeasure()

    
}
