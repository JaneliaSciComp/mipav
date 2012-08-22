package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoSurfaceRefXML;
import gov.nih.mipav.model.file.FileSurfaceXML;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.J3D.surfaceview.JPanelSurface;
import gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceAttributes;
import gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.io.StreamTokenizer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Detail.ClodMesh;
import WildMagic.LibGraphics.Detail.CollapseRecord;
import WildMagic.LibGraphics.Detail.CollapseRecordArray;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * FileSurface. Reads and writes surface files for the JPanelSurface class. When surface files are loaded by the user in
 * the JPanelSurface.java class through the "Add" button, when surfaces are loaded as surfaces attached to .xml image
 * files, or surfaces loaded through the FlyThruRender class. Any time a surface file is read from disk for display in
 * the JPanelSurface (SurfaceRender) class the FileSurface.java class is used to provide the interface. Loaded surfaces
 * are returned in an array of SurfaceAttributes[] which are then used to add the surfaces to the SurfaceRender scene
 * graph.
 *
 * <p>This class also handles saving files from the JPanelSurface class. Surfaces are saved as surface files (.sur),
 * single-level (.wrl), multi-level (.wrl) or XML surfaces (.xml).</p>
 *
 * @see  JPanelSurface.java
 * @see  SurfaceRender.java
 * @see  SurfaceAttributes.java
 * @see  FileSurfaceXML.java
 */
public class FileSurface_WM {

	/** flag indicates to flip y, z coordinate or not. */
	private static boolean flip;
	
	/** flag indicates to read dicom matrix or not. */
	private static boolean dicom;
	
	/** inverse dicom matrix array. */
	private static double[][] inverseDicomArray;
	
	/** inverset dicom matrix. */
	private static TransMatrix inverseDicomMatrix = new TransMatrix(4);
	
	/** image x, y, z orientation. */
	private static int[] direction = new int[3];
	
	/** image start location. */
	private static float[] startLocation = new float[3];
	
	/** image volume bounding box. */
	private static float[] box = new float[3];
	
	/** coordinate used to transform data point from image coordinate to dicom coordinate. */
	private static float[] tCoord = new float[3];
	
	/** coordinate used to transform data point from image coordinate to dicom coordinate */
	private static float[] coord = new float[3];
	
	/** dicom matrix file. */
	private static File dicomMatrixFile = null;
	
	
	//~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Load the clod mesh from a binary file. The caller must have already opened the file and read the mesh type (0 =
     * TriMesh, 1 = ModelClodMesh) and the number of meshes in the file. The caller then calls this function
     * for each mesh. The format for a mesh is
     *
     * <pre>
     int vCount;  // number of vertices
     Point3f vertices[vCount];
     Point3f normals[vCount];
     int iCount;  // number of indices in the connectivity array
     int indices[iCount];
     int rCount;
     ModelCollapseRecord collapses[rCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kIn    the file from which the triangle mesh is loaded
     * @param      pBar   DOCUMENT ME!
     * @param      added  DOCUMENT ME!
     * @param      piece  DOCUMENT ME!
     *
     * @return     the loaded triangle mesh
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static ClodMesh loadCMesh(RandomAccessFile kIn, ViewJProgressBar pBar, int added, int piece)
    {
        MaterialState kMaterial = new MaterialState();
        float[] startLocation = new float[3];
        float[] direction = new float[3];
        float[] box = new float[3]; 
        TriMesh kMesh = loadTMesh(kIn, pBar, added, piece,
                true, kMaterial,
                startLocation, direction, box );
        
        try {
            int iRecordCount = kIn.readInt();
            CollapseRecord[] akRecord = new CollapseRecord[iRecordCount];
            int prog = iRecordCount * piece / 25;

            for (int i = 0; i < iRecordCount; i++) {
                akRecord[i] = load(kIn);

                if ((pBar != null) && (prog != 0) && ((i % prog) == 0)) {
                    pBar.updateValueImmed(Math.round(75 / piece) + added + (i / prog));
                    pBar.update(pBar.getGraphics());
                }
            }

            return new ClodMesh( kMesh.VBuffer, 
                                 kMesh.IBuffer, new CollapseRecordArray(iRecordCount,akRecord));
        } catch (IOException e) {
            return null;
        }
    }


    /**
     * Load the TriMesh from a binary file. The caller must have already opened the file and read the mesh type (0 =
     * TriMesh, 1 = ClodMesh) and the number of meshes in the file. The caller then calls this function
     * for each mesh. The format for a mesh is
     *
     * <pre>
     int vCount;  // number of vertices
     Point3f vertices[vCount];
     Point3f normals[vCount];
     int iCount;  // number of indices in the connectivity array
     int indices[iCount];
     * @param kIn file containing TriMesh data
     * @param progress progress bar
     * @param added number of meshes added so far
     * @param total total number of meshes to add
     * @param isVisible sets the visibility of the progress bar
     * @param kMaterial MaterialState for the new mesh
     * @param startLocation 3D location of the mesh
     * @param direction 3D orientation of the mesh
     * @param box mesh bounding box
     * @return new TriMesh
     */
    public static TriMesh loadTMesh(RandomAccessFile kIn, ViewJProgressBar progress,
                                    int added, int total, boolean isVisible, MaterialState kMaterial,
                                    float[] startLocation, float[] direction, float[] box )
{

        try {
            int i, index, tmpInt;
            int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
            int actions;
            boolean flip;
            boolean dicom;
            long c1 = 0, c2 = 0, c3 = 0, c4 = 0, c5 = 0, c6 = 0, c7 = 0, c8 = 0;
            long tmpLong;
            int j;
            double[][] inverseDicomArray;
            TransMatrix inverseDicomMatrix = new TransMatrix(4);
            float[] tCoord = new float[3];
            float[] coord = new float[3];

            actions = kIn.readInt();

            if ((actions == 1) || (actions == 3)) {
                flip = true;
            } else {
                flip = false;
            }
            flip = !flip;
            if ((actions == 2) || (actions == 3)) {
                dicom = true;
            } else {
                dicom = false;
            }

            direction[0] = kIn.readInt();
            direction[1] = kIn.readInt();
            direction[2] = kIn.readInt();

            byte[] buffer = new byte[24];

            kIn.read(buffer);
            index = 0;
            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[0] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[1] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[2] = Float.intBitsToFloat(tmpInt);
            
            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[0] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[1] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[2] = Float.intBitsToFloat(tmpInt);
            
            if (dicom) {
                buffer = new byte[128];
                kIn.read(buffer);
                index = 0;
                inverseDicomArray = new double[4][4];

                for (i = 0; i <= 3; i++) {

                    for (j = 0; j <= 3; j++) {
                        c1 = buffer[index++] & 0xffL;
                        c2 = buffer[index++] & 0xffL;
                        c3 = buffer[index++] & 0xffL;
                        c4 = buffer[index++] & 0xffL;
                        c5 = buffer[index++] & 0xffL;
                        c6 = buffer[index++] & 0xffL;
                        c7 = buffer[index++] & 0xffL;
                        c8 = buffer[index++] & 0xffL;
                        tmpLong = ((c1 << 56) | (c2 << 48) | (c3 << 40) | (c4 << 32) | (c5 << 24) | (c6 << 16) |
                                   (c7 << 8) | c8);
                        inverseDicomArray[i][j] = Double.longBitsToDouble(tmpLong);
                    }
                }

                inverseDicomMatrix.copyMatrix(inverseDicomArray);

            } // if (dicom)

            int iVertexCount = kIn.readInt();
            Vector3f[] akVertex = new Vector3f[iVertexCount];
            int bufferSize = 12 * iVertexCount;
            byte[] bufferVertex = new byte[bufferSize];
            byte[] bufferNormal = new byte[bufferSize];

            progress.setLocation(200, 200);
            progress.setVisible(isVisible);

            //          read vertices
            kIn.read(bufferVertex);
            kIn.read(bufferNormal);

            for (i = 0, index = 0; i < iVertexCount; i++) {
                akVertex[i] = new Vector3f();

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].X = Float.intBitsToFloat(tmpInt);

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].Y = Float.intBitsToFloat(tmpInt);

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].Z = Float.intBitsToFloat(tmpInt);

                if (dicom) {
                    tCoord[0] = akVertex[i].X - startLocation[0];
                    tCoord[1] = akVertex[i].Y - startLocation[1];
                    tCoord[2] = akVertex[i].Z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    akVertex[i].X = (coord[0] * direction[0]) + startLocation[0];
                    akVertex[i].Y = (coord[1] * direction[1]) + startLocation[1];
                    akVertex[i].Z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {

                    //                  Flip (kVertex.y - startLocation[1], but
                    //                  don't flip startLocation[1]
                    akVertex[i].Y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[i].Y );
                    akVertex[i].Z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[i].Z );
                }

            }

            progress.updateValueImmed(added + (33 / total));

            //          read normals
            Vector3f[] akNormal = new Vector3f[iVertexCount];

            for (i = 0, index = 0; i < iVertexCount; i++) {
                akNormal[i] = new Vector3f();

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].X = Float.intBitsToFloat(tmpInt);

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].Y = Float.intBitsToFloat(tmpInt);

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].Z = Float.intBitsToFloat(tmpInt);
            }

            progress.updateValueImmed(added + (66 / total));

            //          read connectivity
            int iIndexCount = kIn.readInt();

            //          System.out.println("connect count = " + iIndexCount);
            int[] aiConnect = new int[iIndexCount];
            byte[] bufferConnect = new byte[iIndexCount * 4];

            kIn.read(bufferConnect);

            for (i = 0, index = 0; i < iIndexCount; i++) {
                b1 = bufferConnect[index++] & 0x000000ff;
                b2 = bufferConnect[index++] & 0x000000ff;
                b3 = bufferConnect[index++] & 0x000000ff;
                b4 = bufferConnect[index++] & 0x000000ff;

                aiConnect[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                //              System.out.println("connect[" + i + "]" + aiConnect[i]);
            }

            //          read per vertex color array
            int R, G, B, A;
            int isPerVertexColor = kIn.readInt();
            ColorRGBA[] perVertexColor = null;
            if ( isPerVertexColor == 1 ) {
                perVertexColor = new ColorRGBA[iVertexCount];
                byte[] bufferPerVertexColor = new byte[iVertexCount * 4 * 4];
                kIn.read(bufferPerVertexColor);
                for (i = 0, index = 0; i < iVertexCount; i++) {
                    perVertexColor[i] = new ColorRGBA();

                    b1 = bufferPerVertexColor[index++] & 0xff;
                    b2 = bufferPerVertexColor[index++] & 0xff;
                    b3 = bufferPerVertexColor[index++] & 0xff;
                    b4 = bufferPerVertexColor[index++] & 0xff;

                    R = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                    perVertexColor[i].R = Float.intBitsToFloat(R);

                    b1 = bufferPerVertexColor[index++] & 0xff;
                    b2 = bufferPerVertexColor[index++] & 0xff;
                    b3 = bufferPerVertexColor[index++] & 0xff;
                    b4 = bufferPerVertexColor[index++] & 0xff;

                    G = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                    perVertexColor[i].G = Float.intBitsToFloat(G);

                    b1 = bufferPerVertexColor[index++] & 0xff;
                    b2 = bufferPerVertexColor[index++] & 0xff;
                    b3 = bufferPerVertexColor[index++] & 0xff;
                    b4 = bufferPerVertexColor[index++] & 0xff;

                    B = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                    perVertexColor[i].B = Float.intBitsToFloat(B);

                    b1 = bufferPerVertexColor[index++] & 0xff;
                    b2 = bufferPerVertexColor[index++] & 0xff;
                    b3 = bufferPerVertexColor[index++] & 0xff;
                    b4 = bufferPerVertexColor[index++] & 0xff;

                    A = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                    perVertexColor[i].A = Float.intBitsToFloat(A);

                }
            }

            progress.updateValueImmed(added + (100 / total));

            Attributes kAttr = new Attributes();
            kAttr.SetPChannels(3);
            kAttr.SetNChannels(3);
            kAttr.SetTChannels(0,3);
            kAttr.SetCChannels(0,4);
            /*
              if ( perVertexColor != null )
              {
              kAttr.SetCChannels(1,4);
              }
            */
            VertexBuffer pkVB = new VertexBuffer(kAttr,iVertexCount);
            for ( i = 0; i < iVertexCount; i++ )
            {
                pkVB.SetPosition3(i, akVertex[i]);
                pkVB.SetNormal3(i, akNormal[i]);

                if ( perVertexColor != null )
                {
                    pkVB.SetColor4(0, i, perVertexColor[i].R, perVertexColor[i].G, perVertexColor[i].B, perVertexColor[i].A);
                }
                else
                {
                    pkVB.SetColor4(0, i, 1f, 1f, 1f, 1f );
                }

            }
            IndexBuffer pkIB = new IndexBuffer(iIndexCount, aiConnect);
            TriMesh kMesh = new TriMesh(pkVB,pkIB);
            if ( kMaterial != null )
            {
                kMesh.AttachGlobalState( kMaterial );
            }
            return kMesh;
        } catch (IOException e) {
            return null;
        }
    }
    
    /**
     *Load the triangle mesh from a VRML file specifically written by MIPAV. The caller must have already opened the
     * file. 
     * @param kIn the file from which the triangle mesh is loaded
     * @param progress progress bar
     * @param added number of meshes added so far
     * @param total number of meshes to add
     * @param flag when true this is the first mesh, read header info.
     * @param startLocation 3D location of mesh
     * @param direction 3D orientation of mesh
     * @param box mesh bounding box
     * @return new TriMesh
     */
    public static TriMesh loadVRMLMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int total,
                                       boolean flag,
                                       float[] startLocation, float[] direction, float[] box
                                       ) {

        String str = null;
        String str2 = null;
        String str3 = null;
        StringTokenizer stoken = null;
        try {
            // read vertices
            if (flag) {

                if (kIn.getFilePointer() == 0) {
                    str = kIn.readLine().trim();
                    str2 = kIn.readLine().trim();
                    str3 = kIn.readLine().trim();

                    if (!str.equals("#VRML V2.0 utf8") || !str2.equals("#MIPAV") ||
                        (str3.indexOf("#Number of shapes =") == -1)) {
                        MipavUtil.displayWarning("File doesn't appear to be a VRML 2.0 file written by MIPAV");
                        Preferences.debug("TriMesh.loadVRMLMesh: File doesn't appear to be a VRML 2.0 file written by MIPAV.\n");

                        // or throw error.
                        return null;
                    }

                    // read flip line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#flip")) {
                        stoken.nextToken();
                    }

                    // read direction line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#direction")) {
                        stoken.nextToken();
                        direction[0] = Integer.valueOf(stoken.nextToken()).intValue();
                        direction[1] = Integer.valueOf(stoken.nextToken()).intValue();
                        direction[2] = Integer.valueOf(stoken.nextToken()).intValue();
                    }

                    // read start location line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#startLocation")) {
                        stoken.nextToken();
                        startLocation[0] = Float.valueOf(stoken.nextToken()).floatValue();
                        startLocation[1] = Float.valueOf(stoken.nextToken()).floatValue();
                        startLocation[2] = Float.valueOf(stoken.nextToken()).floatValue();
                    }

                    // read box line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#box")) {
                        stoken.nextToken();
                        box[0] = Float.valueOf(stoken.nextToken()).floatValue();
                        box[1] = Float.valueOf(stoken.nextToken()).floatValue();
                        box[2] = Float.valueOf(stoken.nextToken()).floatValue();
                    }
                }
            }

            str = kIn.readLine().trim();

            if (!str.equals("Shape")) {
                return null;
            }

            str = kIn.readLine();

            str = kIn.readLine().trim();
            stoken = new StringTokenizer(str);

            MaterialState kMaterial = null;
            float transparency = 0f;
            if (stoken.nextToken().equals("appearance")) {
                kMaterial = new MaterialState();
                
                str = kIn.readLine(); // material Material {
                str = kIn.readLine().trim(); // emissive Color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                float red = Float.valueOf( stoken.nextToken() ).floatValue();
                float green = Float.valueOf( stoken.nextToken() ).floatValue();
                float blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.Emissive.R = red;
                kMaterial.Emissive.G = green; 
                kMaterial.Emissive.B = blue;

                str = kIn.readLine().trim(); // diffuse color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                red = Float.valueOf( stoken.nextToken() ).floatValue();
                green = Float.valueOf( stoken.nextToken() ).floatValue();
                blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.Diffuse.R = red;
                kMaterial.Diffuse.G = green;
                kMaterial.Diffuse.B = blue;

                str = kIn.readLine().trim(); // specular Color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                red = Float.valueOf( stoken.nextToken() ).floatValue();
                green = Float.valueOf( stoken.nextToken() ).floatValue();
                blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.Specular.R = red;
                kMaterial.Specular.G = green;
                kMaterial.Specular.B = blue;

                str = kIn.readLine().trim(); // transparency
                stoken = new StringTokenizer(str);  stoken.nextToken();
                transparency = Float.valueOf( stoken.nextToken() ).floatValue();

                str = kIn.readLine(); // }
                str = kIn.readLine(); // }
            }

            str = kIn.readLine().trim();

            if (!str.equals("geometry IndexedFaceSet")) {
                return null;
            }

            str = kIn.readLine();

            str = kIn.readLine().trim();

            if (!str.equals("coord Coordinate")) {
                return null;
            }

            str = kIn.readLine();
            str = kIn.readLine().trim();

            if (!str.equals("point [")) {
                return null;
            }

            ArrayList<Vector3f> vertexPts = new ArrayList<Vector3f>(1000);
            ArrayList<Integer> connArray = new ArrayList<Integer>(1000);

            boolean readMore = true;
            Vector3f fPt;

            while (readMore) {

                str = kIn.readLine().trim();

                if (str.equals("]")) {
                    break;
                }

                stoken = new StringTokenizer(str);
                fPt = new Vector3f();
                fPt.Set( Float.valueOf(stoken.nextToken()).floatValue(),
                         Float.valueOf(stoken.nextToken()).floatValue(),
                         Float.valueOf(stoken.nextToken()).floatValue() );

                vertexPts.add(fPt);
            }

            progress.updateValueImmed(added + (25 / total));

            Attributes kAttr = new Attributes();
            kAttr.SetPChannels(3);
            kAttr.SetNChannels(3);
            kAttr.SetTChannels(0,3);
            kAttr.SetCChannels(0,4);
            VertexBuffer kVBuffer = new VertexBuffer( kAttr, vertexPts.size() );
            for (int i = 0; i < vertexPts.size(); i++) {
                Vector3f kVertex = vertexPts.get(i);
                
                kVBuffer.SetPosition3(i, kVertex);      
                kVBuffer.SetColor4( 0, i,
                                    kMaterial.Diffuse.R,
                                    kMaterial.Diffuse.G,
                                    kMaterial.Diffuse.B, 1.0f - transparency );
            }

            vertexPts = null;
            System.gc();
            progress.updateValueImmed(added + (50 / total));

            str = kIn.readLine().trim();
            str = kIn.readLine().trim();

            if (!str.equals("coordIndex [")) {
                return null;
            }

            while (readMore) {

                str = kIn.readLine().trim();

                if (str.equals("]")) {
                    break;
                }

                stoken = new StringTokenizer(str);

                Integer iConn;

                iConn = Integer.valueOf(stoken.nextToken());
                connArray.add(iConn);

                iConn = Integer.valueOf(stoken.nextToken());
                connArray.add(iConn);

                iConn = Integer.valueOf(stoken.nextToken());
                connArray.add(iConn);
            }

            progress.updateValueImmed(added + (75 / total));

            long position = kIn.getFilePointer();

            while ((str != null) && (str.indexOf("Shape") == -1)) {
                position = kIn.getFilePointer();
                str = kIn.readLine();
            }

            kIn.seek(position);

            int[] aiConnect = new int[connArray.size()];

            for (int i = 0; i < connArray.size(); i++) {
                aiConnect[i] = connArray.get(i).intValue();
            }

            progress.updateValueImmed(added + (100 / total));
            
            IndexBuffer kIBuffer = new IndexBuffer( aiConnect.length, aiConnect );
            TriMesh kMesh = new TriMesh(kVBuffer, kIBuffer);
            kMesh.AttachGlobalState( kMaterial );
            return kMesh;
        } catch (IOException e) {
            return null;
        }
    }


    /**
     * The action taken when the Add button is clicked in the JPanelSurface class. A file dialog is launched that allows
     * the user to select new surfaces to load from disk.
     * @param kImage ModelImage for location/orientation and scale information.
     * @return array of TriMesh objects.
     */
    public static TriMesh[] openSurfaces(ModelImage kImage) {
        File[] akFiles = openFiles(true);

        if (akFiles == null) {
            return null;
        }

        TriMesh[] kSurface = new TriMesh[akFiles.length];
        for (int i = 0; i < akFiles.length; i++) {
            String kName = akFiles[i].getName();
            if ((kName.indexOf(".sur") != -1) || (kName.indexOf(".wrl") != -1) || 
            	(kName.indexOf(".vtk") != -1) || (kName.indexOf(".vtp") != -1) || 
            	(kName.indexOf(".stl")!= -1) || 
            	(kName.indexOf(".ply") != -1) || (kName.indexOf(".txt") != -1) || 
            	(kName.indexOf(".gii") != -1)) {
                kSurface[i] = readSurface(kImage, akFiles[i], null);
            } else if (kName.indexOf(".xml") != -1) {
                FileSurfaceRefXML_WM kSurfaceXML = new FileSurfaceRefXML_WM(kName, akFiles[i].getParent());
                FileInfoSurfaceRefXML_WM kFileInfo = kSurfaceXML.readSurfaceXML(kName, akFiles[i].getParent());
                akFiles[i] = new File(akFiles[i].getParent()+ File.separatorChar + kFileInfo.getSurfaceFileName());
                kSurface[i] = readSurface(kImage, akFiles[i], kFileInfo.getMaterial());
            } 
        }

        return kSurface;
    }

    /**
     * Parses the VRML to see how many surfaces there are.
     * @param   kIn  the file to parse
     * @return  int value for the number of surfaces in the file.
     * @throws  IOException             
     * @throws  NoSuchElementException  
     */
    public static int parseVRMLMesh(RandomAccessFile kIn) throws IOException, NoSuchElementException {
        String str = kIn.readLine();

        str = kIn.readLine();
        str = kIn.readLine();

        StringTokenizer token = new StringTokenizer(str);

        token.nextToken();
        token.nextToken();
        token.nextToken();
        token.nextToken();

        return Integer.valueOf(token.nextToken()).intValue();
    }

    
    /**
     * Save a TriMesh to disk
     * @param kName file name
     * @param kMesh TriMesh
     * @param iType type of mesh TriMesh or ClodMesh
     * @param kVBuffer VertexBuffer
     * @param flip invert y,z values
     * @param direction 3D orientation of mesh
     * @param startLocation 3D location of mesh
     * @param box 3D bounding box
     * @param inverseDicomMatrix dicom matrix
     * @throws IOException file I/O exception
     */
    public static void save(String kName, TriMesh kMesh,
                            int iType, VertexBuffer kVBuffer,
                            boolean flip, int[] direction, float[] startLocation, float[] box,
                            TransMatrix inverseDicomMatrix)
    throws IOException
    {
        if (kName == null)
        {
            return;
        }
        
        int i = kName.lastIndexOf('.');
        String kExt = kName.substring(i);
        if ( kExt.equals(".sur" ) )
        { 
        	saveSur(kName,kMesh,iType,kVBuffer,flip,direction,startLocation,box,inverseDicomMatrix);
        }
        else if ( kExt.equals(".ply" ) )
        {
        	saveDicomMatrixInfo(flip,direction,startLocation,box,inverseDicomMatrix);
            saveSinglePlyMesh(kName, null, kMesh);
        }
        else if ( kExt.equals(".stl" ) )
        {
        	saveDicomMatrixInfo(flip,direction,startLocation,box,inverseDicomMatrix);
        	saveSingleSTLMesh(kName, null, kMesh);
        }
        else if ( kExt.equals(".txt" ) )
        {
            saveAsTextFile(kName, kMesh );
        }
        else if ( kExt.equals(".vtk" ) )
        {
            saveAsVTKLegacy(kName, kMesh);
        }
        else if ( kExt.equals(".vtp" ) )
        {
            saveAsVTKXML(kName, kMesh);
        } 
        else if ( kExt.equals(".gii" ) )
        {
        	saveAsGiftiXML(kName, kMesh);
        }
        else if ( kExt.equals(".xml" ) )
        {
            try {
                FileSurfaceRefXML_WM kSurfaceXML = new FileSurfaceRefXML_WM(null, null);
                MaterialState kMaterial = (MaterialState)kMesh.GetGlobalState(GlobalState.StateType.MATERIAL);
                if ( kMaterial == null )
                {
                    kMaterial = new MaterialState();
                }
                float fOpacity = 0f;
                if ( kMesh.VBuffer.GetAttributes().GetCChannels(0) == 4 )
                {
                    fOpacity = 1.0f - kMesh.VBuffer.GetColor4(0, 0).A;
                }
                int iLOD = 0;
                kSurfaceXML.writeXMLsurface_WM(kName, kMaterial, fOpacity, iLOD );     
                String surfaceName = kName.substring(0, i) + ".sur"; 
                saveSur(surfaceName,kMesh,iType,kVBuffer,flip,direction,startLocation,box,inverseDicomMatrix);
            } catch ( IOException kError ) { }
        }
        else if ( kExt.equals(".wrl" ) )
        {
            saveAsVRML( kName, kMesh, kVBuffer, flip, direction, startLocation,  box );
        }
    }
    
    
    /**
     * Save a TriMesh to disk
     * @param kName file name
     * @param kMesh TriMesh
     * @param iType type of mesh TriMesh or ClodMesh
     * @param kVBuffer VertexBuffer
     * @param flip invert y,z values
     * @param direction 3D orientation of mesh
     * @param startLocation 3D location of mesh
     * @param box 3D bounding box
     * @param inverseDicomMatrix dicom matrix
     * @throws IOException file I/O exception
     */
    public static void saveProstateSurface(String kName, TriMesh kMesh,
                            int iType, VertexBuffer kVBuffer,
                            boolean flip, int[] direction, float[] startLocation, float[] box,
                            TransMatrix inverseDicomMatrix, ModelImage keyImage)
    throws IOException
    {
        if (kName == null)
        {
            return;
        }
        
        int i = kName.lastIndexOf('.');
        String kExt = kName.substring(i);
        if ( kExt.equals(".ply" ) )
        {
        	saveDicomMatrixInfo(flip,direction,startLocation,box,inverseDicomMatrix);
            saveProstatePlyMesh(kName, keyImage, kMesh);
        }
        else if ( kExt.equals(".stl" ) )
        {
        	saveDicomMatrixInfo(flip,direction,startLocation,box,inverseDicomMatrix);
        	saveProstateSTLMesh(kName, keyImage, kMesh);
        }
      }
    
    
    
    /**
     * Save ClodMesh to disk
     * @param kName file name
     * @param akComponent array of ClodMesh to save
     * @param flip invert y,z values
     * @param direction 3D mesh orientation
     * @param startLocation 3D mesh location
     * @param box mesh bounding box
     * @param inverseDicomMatrix dicom matrix
     */
    public static void saveClodMesh(String kName, ClodMesh[] akComponent,
                                    boolean flip, int[] direction,
                                    float[] startLocation, float[] box,
                                    TransMatrix inverseDicomMatrix)
    {
        if (akComponent.length == 0) {
            return;
        }
        try {
            for (int i = 0; i < akComponent.length; i++) {
                saveSur(kName, akComponent[i], 1, akComponent[i].VBuffer, flip,
                     direction, startLocation, box, inverseDicomMatrix);
            }
        } catch ( IOException kIOError ) {}
    }

    /**
     * The action taken when the one of the save surface buttons is pressed in the JPanelSurface class. A file dialog is
     * launched that allows the user to select where to save the surfaces.
     *
     * @param  kImage      the ModelImage displayed in the SurfaceRender class
     * @param  akSurfaces  an array of surfaces described by their SurfaceAttributes, containing information that is
     *                     saved with the TriMesh
     * @param  kCommand    the type of save operation to perform
     */
    public static void saveSurfaces(ModelImage kImage, TriMesh[] akSurfaces, String kCommand) {

        if (akSurfaces.length == 0) {
            MipavUtil.displayError("Select a surface to save.");

            return;
        }

        if (kCommand.equals("LevelS") || kCommand.equals("LevelV")) {

            for (int i = 0; i < akSurfaces.length; i++) {
                saveSingleMesh(kImage, kCommand.equals("LevelS"), akSurfaces[i]);
            }
        }
        //         else if (kCommand.equals("LevelW")) {
        //             saveMultiMesh(kImage, akSurfaces);
        //         }
        else if (kCommand.equals("LevelXML")) {

            for (int i = 0; i < akSurfaces.length; i++) {
                writeTriangleMeshXML(null, kImage, akSurfaces[i]);
            }
        }
        else if (kCommand.equals("LevelSTL")) {

            for (int i = 0; i < akSurfaces.length; i++) {
            	saveSingleSTLMesh(null, kImage, akSurfaces[i]);
            }
        }
        else if (kCommand.equals("LevelPLY")) {

            for (int i = 0; i < akSurfaces.length; i++) {
            	saveSinglePlyMesh(null, kImage, akSurfaces[i] );
            }
        }                           
    }
    
    
    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format. File name should end with ".wrl"
     *
     * @param      kOut           the file to which the triangle mesh is saved
     * @param      flip           if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                            To have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      direction      1 or -1 for each axis
     * @param      startLocation  DOCUMENT ME!
     * @param      box            (dimension-1)*resolution
     * @param      color          DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected static void saveAsVRML(PrintWriter kOut, TriMesh kMesh, VertexBuffer kVBuffer, boolean flip, int[] direction, float[] startLocation, float[] box)
    {
        kOut.print("#flip { ");

        if (flip) {
            kOut.print(1);
        } else {
            kOut.print(0);
        }

        kOut.print(" }\n");

        kOut.print("#direction { ");
        kOut.print(direction[0]);
        kOut.print(' ');
        kOut.print(direction[1]);
        kOut.print(' ');
        kOut.print(direction[2]);
        kOut.print(" }\n");

        kOut.print("#startLocation { ");
        kOut.print(startLocation[0]);
        kOut.print(' ');
        kOut.print(startLocation[1]);
        kOut.print(' ');
        kOut.print(startLocation[2]);
        kOut.print(" }\n");

        kOut.print("#box { ");
        kOut.print(box[0]);
        kOut.print(' ');
        kOut.print(box[1]);
        kOut.print(' ');
        kOut.print(box[2]);
        kOut.print(" }\n");

        MaterialState kMaterial = (MaterialState)kMesh.GetGlobalState( GlobalState.StateType.MATERIAL );
        if ( kMaterial == null )
        {
            kMaterial = new MaterialState();
        }

        kOut.print("Shape\n{\n");
        kOut.print("\tappearance Appearance {\n");
        kOut.print("\t\tmaterial Material {\n");
        kOut.print("\t\t\temissiveColor\t");
        kOut.print( kMaterial.Emissive.R + " ");
        kOut.print( kMaterial.Emissive.G + " ");
        kOut.print( kMaterial.Emissive.B );
        kOut.print("\n\t\t\tdiffuseColor\t");
        kOut.print( kMaterial.Diffuse.R + " ");
        kOut.print( kMaterial.Diffuse.G + " ");
        kOut.print( kMaterial.Diffuse.B );
        kOut.print("\n\t\t\tspecularColor\t");
        kOut.print( kMaterial.Specular.R + " ");
        kOut.print( kMaterial.Specular.G + " ");
        kOut.print( kMaterial.Specular.B );

        float fTransparency = 0f;
        if ( kVBuffer.GetAttributes().GetCChannels(0) == 4)
        {
        	fTransparency = 1.0f - kVBuffer.GetColor4(0, 0).A;
        }
        kOut.print("\n\t\t\ttransparency " + fTransparency + "\n");
        kOut.print("\t\t}\n");
        kOut.print("\t}\n");

        kOut.print("\tgeometry IndexedFaceSet\n\t{\n");

        kOut.print("\t\tcoord Coordinate\n");
        kOut.print("\t\t{\n");
        kOut.print("\t\t\tpoint [\n\t\t\t\t");

        // write vertices
        Vector3f kPos = new Vector3f();
        int i;
        for (i = 0; i < kVBuffer.GetVertexQuantity(); i++) {
            kVBuffer.GetPosition3(i, kPos);
            kOut.print(kPos.X);
            kOut.print(' ');

            kOut.print(kPos.Y);
            kOut.print(' ');

            kOut.print(kPos.Z);

            if (i < (kVBuffer.GetVertexQuantity() - 1)) {
                kOut.print(" ,\n\t\t\t\t");
            } else {
                kOut.print("\n\t\t\t\t]\n");
            }
        }

        // write connectivity
        kOut.print("\t\t\t}\n\t\t\tcoordIndex [\n\t\t\t\t");
        int iTriangleCount = kMesh.IBuffer.GetIndexQuantity() / 3;
        int[] aiIndex = kMesh.IBuffer.GetData();
        for (i = 0; i < iTriangleCount; i++) {
            kOut.print(aiIndex[3 * i]);
            kOut.print(' ');
            kOut.print(aiIndex[(3 * i) + 1]);
            kOut.print(' ');
            kOut.print(aiIndex[(3 * i) + 2]);

            if (i < (iTriangleCount - 1)) {
                kOut.print(" -1\n\t\t\t\t");
            } else {
                kOut.print("\n\t\t\t\t]\n");
            }
        }

        kOut.print("\t\t\tconvex FALSE\n");
        kOut.print("\t\t\tcreaseAngle 1.5\n");
        kOut.print("\t}\n}\n");
    }
    
    
    /**
     * Calls a dialog to get a file name.
     * @param   bLoad  if <code>true</code>, make it a load dialog.
     * @return  File name.
     */
    private static String getFileName(boolean bLoad) {
        File[] files = openFiles(bLoad);

        if (files != null) {
            return new String(files[0].getPath());
        }

        return null;
    }
    
    
    /**
     * Support for loading the collapse records from a binary file. 
     * @param      kIn  the file from which the records are loaded
     * @return CollapseRecord 
     * @exception  IOException  if the there is an error reading from the file
     */
    private static CollapseRecord load(RandomAccessFile kIn)
    {
    	try {
            int iVKeep = kIn.readInt();
            int iVThrow = kIn.readInt();
            int iVQuantity = kIn.readInt();
            int iTQuantity = kIn.readInt();
            CollapseRecord kRecord = new CollapseRecord(iVKeep, iVThrow, iVQuantity, iTQuantity);
            kRecord.IQuantity = kIn.readInt();
            if ( kRecord.IQuantity > 0 ) {
                byte[] bufferByte = new byte[ kRecord.IQuantity * 4 ];
                int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
                kRecord.Index = new int[kRecord.IQuantity];
                kIn.read(bufferByte);

                for (int i = 0, index = 0; i < kRecord.IQuantity; i++) {
                    b1 = bufferByte[index++] & 0x000000ff;
                    b2 = bufferByte[index++] & 0x000000ff;
                    b3 = bufferByte[index++] & 0x000000ff;
                    b4 = bufferByte[index++] & 0x000000ff;
                    kRecord.Index[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                }
            }
            return kRecord;
    	} catch (IOException kIOError ) {}
    	return null;
    }


    /**
     * Read a TriMesh from a Ply ascii file.
     * @param file file storing the mesh
     * @return TriMesh
     */
    private static TriMesh loadPlyAsciiMesh(File file) {

        TriMesh mesh;
        int i;
        int iVertexCount = 0;
        int iTriangleCount = 0;
        boolean readHeader = true;
        float x = 0f, y = 0f, z = 0f;
        int idx1 = 0, idx2 = 0, idx3 = 0;
       
        Vector<Vector3f> vertexArray = new Vector<Vector3f>();
        Vector<Integer> connectivity = new Vector<Integer>();
        VertexBuffer kVBuffer;
        int[] aiConnect;
		
        try {
        	
        	readDicomMatrix();
        	
            DataInputStream in = new DataInputStream(new BufferedInputStream(new FileInputStream(file)));
            String s, token;
            while ( (s = readLine(in)).length() > 0) {
                StringTokenizer st = new StringTokenizer(s);
                while ( st.hasMoreTokens() && readHeader ) {
                    // System.err.print(st.nextToken() + " ");
                    token = st.nextToken();
                    if ( token.equals("vertex")) {
                        iVertexCount = Integer.valueOf(st.nextToken());
                    } else if ( token.equals("face") ) {
                        iTriangleCount = Integer.valueOf(st.nextToken());
                        readLine(in);
                        readLine(in);  // skip two lines follow the face count attribute in PLY file format.
                        readHeader = false;
                        break;
                    }
                }
                if ( readHeader == false) break;
            }
		    
            // read Vertex 
            for ( i = 0; i < iVertexCount; i++ ) {
                s = readLine(in);
                StringTokenizer st = new StringTokenizer(s);
                x = Float.valueOf(st.nextToken());
                y = Float.valueOf(st.nextToken());
                z = Float.valueOf(st.nextToken());
                
                if (dicom) {
                    tCoord[0] = x - startLocation[0];
                    tCoord[1] = y - startLocation[1];
                    tCoord[2] = z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    x = (coord[0] * direction[0]) + startLocation[0];
                    y = (coord[1] * direction[1]) + startLocation[1];
                    z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {

                    //                  Flip (kVertex.y - startLocation[1], but
                    //                  don't flip startLocation[1]
                    y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - y );
                    z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - z );
                }

                
                vertexArray.add(new Vector3f(x, y, z));
            }
		    
            // read connectivity
            for ( i = 0; i < iTriangleCount; i++ ) {
                s = readLine(in);
                StringTokenizer st = new StringTokenizer(s);
                st.nextToken();  // skip 3
                idx1 = Integer.valueOf(st.nextToken());
                connectivity.add(idx1);
                idx2 = Integer.valueOf(st.nextToken());
                connectivity.add(idx2);
                idx3 = Integer.valueOf(st.nextToken());
                connectivity.add(idx3);
            }
		    
            int vertexCount = vertexArray.size();
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels(3);
            kAttr.SetNChannels(3);
            kAttr.SetTChannels(0, 3);
            kAttr.SetCChannels(0, 4);
            kVBuffer = new VertexBuffer(kAttr, vertexCount);

            int index = 0;
            Vector3f pos;
            for (i = 0; i < vertexCount; i++) {
                pos = vertexArray.elementAt(i);
                kVBuffer.SetPosition3(index, pos);
                kVBuffer.SetColor4(0, index, 1.0f, 1.0f, 1.0f, 1.0f);
                index++;
            }

            int indexCount = connectivity.size();
            aiConnect = new int[indexCount];
            for (i = 0; i < indexCount; i++) {
                aiConnect[i] = connectivity.get(i);
            }
		    
            IndexBuffer kIBuffer = new IndexBuffer( aiConnect.length, aiConnect );
            mesh = new TriMesh(kVBuffer, kIBuffer);
            MaterialState kMaterial = new MaterialState();
            mesh.AttachGlobalState( kMaterial );
            return mesh;
        } catch (FileNotFoundException e) {
            System.err.println("ERROR: Can't find file " + file);
            return null;
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Load the STL ASCII file.
     * @param file STL surface file reference
     * @return TriMesh
     */
    private static TriMesh loadSTLAsciiMesh(File file) {

        TriMesh mesh; 
        try {
            BufferedReader reader;
            reader = new BufferedReader(new FileReader(file));
            StreamTokenizer tokenizer = new StreamTokenizer(reader);
            tokenizer.resetSyntax();
            tokenizer.whitespaceChars(0, 0x20);
            tokenizer.wordChars(0x21, 0xff);
            mesh = readSTLAscii(tokenizer);
            reader.close();
            return mesh;
        } catch (FileNotFoundException e) {
            System.err.println("ERROR: Can't find file " + file);
            return null;
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Load the STL Binary file.
     * @param file STL surface file reference
     * @return TriMesh
     */
    private static TriMesh loadSTLBinaryMesh(RandomAccessFile kIn)
    {
        int iTriangleCount;
        HashMap<String, Integer> vertexHashtable = new HashMap<String, Integer>();
        float x, y, z;
        byte[] value = new byte[4];
        byte[] header = new byte[80];
        byte[] attribute = new byte[2];
        VertexBuffer kVBuffer;
        int[] aiConnect;
        TriMesh kMesh = null;
        Vector<Vector3f> vertexArray = new Vector<Vector3f>();
        Integer searchIndex;
        Vector<Integer> connectivity = new Vector<Integer>();
        Vector3f vertex1, vertex2, vertex3;
        Vector3f temp = new Vector3f();
        Vector3f normal = new Vector3f();
        Vector3f side1 = new Vector3f();
        Vector3f side2 = new Vector3f();
        Vector3f surfaceNormal = new Vector3f();

        try {
        	
        	readDicomMatrix();
        	
            // nothing header
            kIn.read(header);

            // number of facets
            kIn.read(value);
            iTriangleCount = FileBase.bytesToInt(false, 0, value);

            int index = 0;

            for (int i = 0; i < iTriangleCount; i++) {
                // read normal.x, normal.y, normal.z
                kIn.read(value);
                x = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                y = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                z = FileBase.bytesToFloat(false, 0, value);
                normal = new Vector3f(x, y, z);

                // index 1
                kIn.read(value);
                x = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                y = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                z = FileBase.bytesToFloat(false, 0, value);

                if (dicom) {
                    tCoord[0] = x - startLocation[0];
                    tCoord[1] = y - startLocation[1];
                    tCoord[2] = z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    x = (coord[0] * direction[0]) + startLocation[0];
                    y = (coord[1] * direction[1]) + startLocation[1];
                    z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {
                    y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - y );
                    z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - z );
                }
                vertex1 = new Vector3f(x, y, z);

                // index 2
                kIn.read(value);
                x = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                y = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                z = FileBase.bytesToFloat(false, 0, value);
                if (dicom) {
                    tCoord[0] = x - startLocation[0];
                    tCoord[1] = y - startLocation[1];
                    tCoord[2] = z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    x = (coord[0] * direction[0]) + startLocation[0];
                    y = (coord[1] * direction[1]) + startLocation[1];
                    z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {
                    y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - y );
                    z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - z );
                }
                vertex2 = new Vector3f(x, y, z);

                // index 3
                kIn.read(value);
                x = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                y = FileBase.bytesToFloat(false, 0, value);
                kIn.read(value);
                z = FileBase.bytesToFloat(false, 0, value);
                if (dicom) {
                    tCoord[0] = x - startLocation[0];
                    tCoord[1] = y - startLocation[1];
                    tCoord[2] = z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    x = (coord[0] * direction[0]) + startLocation[0];
                    y = (coord[1] * direction[1]) + startLocation[1];
                    z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {
                    y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - y );
                    z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - z );
                }
                vertex3 = new Vector3f(x, y, z);

                // Check that the normal is in the correct direction
                side1.Sub(vertex2, vertex1);
                side2.Sub(vertex3, vertex2);
                surfaceNormal.Cross(side1, side2);
                //side1.Cross(side2, surfaceNormal);
                if (normal.Dot(surfaceNormal) < 0) {
                    // vertices were specified in the wrong order, so reverse
                    // two of them
                    temp.Copy(vertex2);
                    vertex2.Copy(vertex3);
                    vertex3.Copy(temp);
                }

                // index1
                x = vertex1.X;
                y = vertex1.Y;
                z = vertex1.Z;
                searchIndex = vertexHashtable.get((x + " " + y + " " + z));
                if (searchIndex == null) { // not found
                    vertexHashtable.put((x + " " + y + " " + z), new Integer(
                                                                             index));
                    connectivity.add(new Integer(index));
                    vertexArray.add(new Vector3f(x, y, z));
                    index++;
                } else {
                    connectivity.add(searchIndex);
                }

                // index 2
                x = vertex2.X;
                y = vertex2.Y;
                z = vertex2.Z;
                searchIndex = vertexHashtable.get((x + " " + y + " " + z));
                if (searchIndex == null) {
                    vertexHashtable.put((x + " " + y + " " + z), new Integer(
                                                                             index));
                    connectivity.add(new Integer(index));
                    vertexArray.add(new Vector3f(x, y, z));
                    index++;
                } else {
                    connectivity.add(searchIndex);
                }

                // index 3
                x = vertex3.X;
                y = vertex3.Y;
                z = vertex3.Z;
                searchIndex = vertexHashtable.get((x + " " + y + " " + z));
                if (searchIndex == null) {
                    vertexHashtable.put((x + " " + y + " " + z), new Integer(
                                                                             index));
                    vertexArray.add(new Vector3f(x, y, z));
                    connectivity.add(new Integer(index));
                    index++;
                } else {
                    connectivity.add(searchIndex);
                }

                // attribute
                kIn.read(attribute);
            }

            int vertexCount = vertexArray.size();
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels(3);
            kAttr.SetNChannels(3);
            kAttr.SetTChannels(0, 3);
            kAttr.SetCChannels(0, 4);
            kVBuffer = new VertexBuffer(kAttr, vertexCount);

            index = 0;
            Vector3f pos;
            for (int i = 0; i < vertexCount; i++) {
                pos = vertexArray.elementAt(i);
                kVBuffer.SetPosition3(index, pos);
                kVBuffer.SetColor4(0, index, 1.0f, 1.0f, 1.0f, 1.0f);
                index++;
            }

            int indexCount = connectivity.size();
            aiConnect = new int[indexCount];
            for (int i = 0; i < indexCount; i++) {
                aiConnect[i] = connectivity.get(i);
            }

        } catch (IOException e) {
            return null;
        }
        IndexBuffer kIBuffer = new IndexBuffer(aiConnect.length, aiConnect);
        kMesh = new TriMesh(kVBuffer, kIBuffer);
        MaterialState kMaterial = new MaterialState();
        kMesh.AttachGlobalState(kMaterial);
        return kMesh;

    }


    /**
     * Load legacy VTK mesh.
     * @param kIn file containing mesh
     * @param progress progress bar
     * @param added number of meshes read from file so far
     * @param total total number of meshes in file
     * @param flag invert y,z values
     * @return TriMesh
     * @throws IOException
     */
    private static TriMesh loadVTKLegacyMesh(RandomAccessFile kIn, ViewJProgressBar progress, 
            int added, int total, String fileName) {
        TriMesh kMesh;
    	
        System.out.println(fileName);
        
        StringBuffer buff = new StringBuffer();
        try {
            //progress.setLocation(200, 200);
            progress.setVisible(true);
            String str;
            // Read file as string
            while ((str = kIn.readLine()) != null) {
                buff.append(str+"\n");
            }
        } catch (Exception e) {
            System.err.println("Error occured while reading parameter file:\n"+e.getMessage());
            e.printStackTrace();
            return null;
        }
        Pattern header=Pattern.compile("POINTS\\s\\d+\\sfloat");
        Matcher m=header.matcher(buff);
        int vertexCount=0;
        int indexCount=0;

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0,3);
        kAttr.SetCChannels(0,4);
        //kAttr.SetCChannels(1,4);

        VertexBuffer kVBuffer = null;
        int[] indices;
        if(m.find()){
            String head=buff.substring(m.start(),m.end());
            String[] vals=head.split("\\D+");
            if(vals.length>0){
                try {
                    vertexCount=Integer.parseInt(vals[vals.length-1]);
                } catch(NumberFormatException e){
                    System.err.println("CANNOT DETERMINE VERTEX COUNT");
                    return null;
                }
            }

            kVBuffer = new VertexBuffer( kAttr, vertexCount );

            System.out.println("vertex count is " + vertexCount);
            progress.updateValueImmed(added + (25 / total));
            System.out.println(m.end());
            System.out.println(buff.length());
            String[] strs=buff.substring(m.end(),buff.length()).split("\\s+",vertexCount*3+2);
            System.out.println(strs[0]);
            System.out.println(strs[1]);
            for(int i=1;i<strs.length-1;i+=3){
                try {
                    kVBuffer.SetPosition3( (i-1)/3,
                                           Float.parseFloat(strs[i]),
                                           Float.parseFloat(strs[i+1]),
                                           Float.parseFloat(strs[i+2]));
                    kVBuffer.SetColor4(0, (i-1)/3, 1f, 1f, 1f, 1f );
                    //System.out.println(i/3+")"+p);
                } catch(NumberFormatException e){
                    System.err.println("CANNOT FORMAT VERTS");
                    return null;
                }
            }
        } else {
            return null;
        }
        
        progress.updateValueImmed(added + (50 / total));
        
        header=Pattern.compile("POLYGONS\\s+\\d+\\s+\\d+");
        m=header.matcher(buff);
        if(m.find()){
            String head=buff.substring(m.start(),m.end());
            String[] vals=head.split("\\D+");
            if(vals.length>1){
                try {
                    indexCount=Integer.parseInt(vals[1]);
                } catch(NumberFormatException e){
                    System.err.println("CANNOT DETERMINE INDEX COUNT");
                    return null;
                }
            }
            indices=new int[indexCount*3];
            System.out.println("INDICES "+indexCount);
            String[] strs=buff.substring(m.end(),buff.length()).split("\\s+",indexCount*4+2);	
            int count=0;
            System.out.println(strs[0]);
            System.out.println(strs[1]);
            for(int i=1;i<strs.length-1;i+=4){			
                try {
                    if(Integer.parseInt(strs[i]) != 3) {
                        System.err.println("CANNOT FORMAT INDICES");
                        return null;
                    }
                    indices[count++]=Integer.parseInt(strs[i+1]);
                    indices[count++]=Integer.parseInt(strs[i+2]);
                    indices[count++]=Integer.parseInt(strs[i+3]);
                } catch(NumberFormatException e){
                    System.err.println("CANNOT FORMAT INDICES");
                    return null;
                }
            }
        } else {
            return null;
        }
        
        header=Pattern.compile("POINT_DATA\\s+\\d+\\D+float\\s+\\d+\\nLOOKUP_TABLE\\s");
        m=header.matcher(buff);
        double[][] dat;
        int count=0;
        int dim=0;
        if(m.find()){
            String head=buff.substring(m.start(),m.end());
            String[] vals=head.split("\\D+");
            if(vals.length>0){
                try {
                    count=Integer.parseInt(vals[1]);
                    dim=Integer.parseInt(vals[2]);
                } catch(NumberFormatException e){
                    System.err.println("CANNOT DETERMINE DATA POINTS");
                    return null;
                }
            }
            dat=new double[count][dim];
            System.out.println("DATA POINTS "+count+" by "+dim);
            String[] strs=buff.substring(m.end(),buff.length()).split("\\s+",count*dim+2);
            int index=0;
            for(int i=1;i<strs.length&&index<count*dim;i++){
                try {		
                    dat[index/dim][index%dim]=Double.parseDouble(strs[i]);
                    index++;
                } catch(NumberFormatException e){
                    System.err.println("CANNOT FORMAT DATA ["+strs[i]+"]");
                    //return null;
                }
            }
            //System.out.println(index+" "+count);
            
            progress.updateValueImmed(added + (100 / total));
            
        } else { 
            
        }
        IndexBuffer kIBuffer = new IndexBuffer( indices.length, indices );
        kMesh = new TriMesh( kVBuffer, kIBuffer );
        return kMesh;
    }
    
    
    /**
     * Read VTK mesh from file.
     * @param absPath file path
     * @param fileName file name
     * @param dir file directory
     * @return TriMesh
     */
    private static TriMesh loadVTKXMLMesh(String absPath, String fileName, String dir)
    {
        TriMesh kMesh = null;
        
        FileSurfaceVTKXML_WM surfaceVTKXML = new FileSurfaceVTKXML_WM(fileName, dir);
        kMesh = surfaceVTKXML.readXMLSurface_WM(absPath);

        return kMesh;
    }
    
    /**
     * Read Gifti mesh from file.
     * @param absPath file path
     * @param fileName file name
     * @param dir file directory
     * @return TriMesh
     */
    private static TriMesh loadGiftiXMLMesh(String absPath, String fileName, String dir)
    {
        TriMesh kMesh = null;
        
        FileSurfaceGiftiXML_WM surfaceGiftiXML = new FileSurfaceGiftiXML_WM(fileName, dir);
        kMesh = surfaceGiftiXML.readSurfaceXML(fileName, dir);

        return kMesh;
    }
   
    /**
     * Returns an array of File objects, based on the user-selected files from the FileChooser dialog.
     * @param   bLoad  whether the files are opened for reading (bLoad = true) or writing (bLoad = false)
     * @return  File[] array of opened files.
     */
    public static File[] openFiles(boolean bLoad) {

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(bLoad);
        chooser.resetChoosableFileFilters();
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal;

        if (bLoad) {
            returnVal = chooser.showOpenDialog(null);
        } else {
            returnVal = chooser.showSaveDialog(null);
        }

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                                 File.separatorChar);

            if (bLoad) {
                File[] files = chooser.getSelectedFiles();

                return files;
            } 
            File[] files = new File[1];
            files[0] = chooser.getSelectedFile();
            return files;
        }

        return null;
    }

    
    /**
     * Returns a dicomMatrix file objects, based on the user-selected filed from the FileChooser dialog.
     * @param   bLoad  whether the files are opened for reading (bLoad = true) or writing (bLoad = false)
     * @return  File of opened file.
     */
    private static File openDicomMatrixFiles(boolean bLoad) {

        // file dialog to select the dicom matrix info file
        JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(bLoad);
        chooser.resetChoosableFileFilters();
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.DICOMMATRIX));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal;

        if (bLoad) {
            returnVal = chooser.showOpenDialog(null);
        } else {
            returnVal = chooser.showSaveDialog(null);
        }

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                                 File.separatorChar);

            if (bLoad) {
                File file = chooser.getSelectedFile();

                return file;
            } 
            File[] file = new File[1];
            file[0] = chooser.getSelectedFile();
            return file[0];
        }

        return null;
    }
    
    
    /**
     * Read dicom matrix info file
     */
    public static void readDicomMatrix() {
		try {
			// read the dicom info first, then read the .ply surface file.
			dicomMatrixFile = openDicomMatrixFiles(true);
			FileInputStream fis = new FileInputStream(dicomMatrixFile
					.getParent()
					+ File.separatorChar + dicomMatrixFile.getName());
			DataInputStream dis = new DataInputStream(fis);

			int actions = dis.readInt();

			if ((actions == 1) || (actions == 3)) {
				flip = true;
			} else {
				flip = false;
			}
			flip = !flip;
			if ((actions == 2) || (actions == 3)) {
				dicom = true;
			} else {
				dicom = false;
			}

			direction[0] = dis.readInt();
			direction[1] = dis.readInt();
			direction[2] = dis.readInt();

			startLocation[0] = dis.readFloat();
			startLocation[1] = dis.readFloat();
			startLocation[2] = dis.readFloat();

			box[0] = dis.readFloat();
			box[1] = dis.readFloat();
			box[2] = dis.readFloat();

			if (dicom) {
				inverseDicomArray = new double[4][4];

				for (int i = 0; i <= 3; i++) {
					for (int j = 0; j <= 3; j++) {
						inverseDicomArray[i][j] = dis.readDouble();
					}
				}
				inverseDicomMatrix.copyMatrix(inverseDicomArray);
			}
		} catch (FileNotFoundException e) {
			System.err.println("ERROR: Can't find file "
					+ dicomMatrixFile.getName());
		} catch (IOException e) {

		}
	}

    /**
     * Save the dicom matrix header info.  The .ply file format can't save the dicom info.   We decide to 
     * save the dicom info when save the VOI file.  So, the dicom info will be read when load the .ply surface
     * into the volume render.  This ensures the correct scale of surface.   The dicom matrix info is 
     * saved in the current image directory with the .dicomMatrix suffix. 
     */
    private static void saveDicomMatrixInfo(boolean flip, int[] direction, float[] startLocation, float[] box, TransMatrix inverseDicomMatrix) {
    
 		try {
			dicomMatrixFile = openDicomMatrixFiles(false);
			
			FileOutputStream  fOut = new FileOutputStream(dicomMatrixFile.getParent()
					+ File.separatorChar + dicomMatrixFile.getName());
			DataOutputStream  kOut = new DataOutputStream(fOut);
 
			
			 if (inverseDicomMatrix == null) {
		            
		            if (flip) {
		                kOut.writeInt(1);
		            } else {
		                kOut.writeInt(0);
		            }
		        } else {
		            
		            if (flip) {
		                kOut.writeInt(3);
		            } else {
		                kOut.writeInt(2);
		            }
		        }

		        kOut.writeInt(direction[0]);
		        kOut.writeInt(direction[1]);
		        kOut.writeInt(direction[2]);

		        kOut.writeFloat(startLocation[0]);
		        kOut.writeFloat(startLocation[1]);
		        kOut.writeFloat(startLocation[2]);
		        kOut.writeFloat(box[0]);
		        kOut.writeFloat(box[1]);
		        kOut.writeFloat(box[2]);
		        
		        if (inverseDicomMatrix != null) {
		            for (int i = 0; i <= 3; i++) {
		                for (int j = 0; j <= 3; j++) {
		                	kOut.writeDouble(inverseDicomMatrix.Get(i, j));
		                }
		            }
		        }
		        kOut.close();
		        
		} catch (Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveDicomMatrixInfo() of FileVOI");
			e.printStackTrace();
		}

	}
    
    /**
	 * Print the contents of the TriMesh in ascii format.
	 * 
	 * @param kOut
	 *            output
	 * @param kMesh
	 *            TriMesh
	 */
    private static void printAscii(PrintWriter kOut, TriMesh kMesh) {
    	Vector3f kVertex = new Vector3f();
        Vector3f kNormal = new Vector3f();

        // write vertices
        kOut.println(kMesh.VBuffer.GetVertexQuantity());
        kOut.println("Vertices");

        int i;

        for (i = 0; i < kMesh.VBuffer.GetVertexQuantity(); i++) {
            kMesh.VBuffer.GetPosition3(i, kVertex);
            kOut.print(kVertex.X);
            kOut.print(' ');
            kOut.print(kVertex.Y);
            kOut.print(' ');
            kOut.println(kVertex.Z);
        }

        kOut.println("Normals");

        // write normals
        for (i = 0; i < kMesh.VBuffer.GetVertexQuantity(); i++) {
        	kMesh.VBuffer.GetNormal3(i, kNormal);
            kOut.print(kNormal.X);
            kOut.print(' ');
            kOut.print(kNormal.Y);
            kOut.print(' ');
            kOut.println(kNormal.Z);
        }

        kOut.println("Connectivity");

        // write connectivity
        int iTriangleCount = kMesh.GetTriangleQuantity();
        int[] aiIndex = kMesh.IBuffer.GetData();
        kOut.println(iTriangleCount);

        for (i = 0; i < iTriangleCount; i++) {
            kOut.print(aiIndex[(3 * i)]);
            kOut.print(' ');
            kOut.print(aiIndex[((3 * i) + 1)]);
            kOut.print(' ');
            kOut.println(aiIndex[((3 * i) + 2)]);
        }
    }

    /**
     * Print the contents of the TriMesh in STL ascii format.
     * @param kOut output
     * @param kMesh TriMesh
     */
    private static void printSTLAscii(PrintWriter kOut, ModelImage kImage, TriMesh kMesh)
    {
        int i;
        int index1, index2, index3;
        VertexBuffer kVBuffer;
        int iTriangleCount = kMesh.IBuffer.GetIndexQuantity() / 3;
        int[] aiIndex = kMesh.IBuffer.GetData();
        Vector3f kVertex = new Vector3f();
        Vector3f kNormal = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector3f kNormal2 = new Vector3f();
        Vector3f kNormal3 = new Vector3f();

        if ( kImage != null ) {
          kVBuffer = convertVertex(kImage, kMesh);
        } else {
          kVBuffer = kMesh.VBuffer;
        }
        
        kOut.println("solid");
        
        for (i = 0; i < iTriangleCount; i++) {
            index1 = aiIndex[(3 * i)];    
            index2 = aiIndex[((3 * i) + 1)];
            index3 = aiIndex[((3 * i) + 2)];
            
            kVBuffer.GetPosition3(index1, kNormal1);
            kVBuffer.GetPosition3(index2, kNormal2);
            kVBuffer.GetPosition3(index3, kNormal3);
	            
            // Compute facet normal
            kNormal.Set(0f, 0f, 0f);
            kNormal.Add(kNormal1);
            kNormal.Add(kNormal2);
            kNormal.Add(kNormal3);
            kNormal.Scale(1f/3f);
            
            kOut.print(" facet normal  ");
            kOut.print(kNormal.X);
            kOut.print(' ');
            kOut.print(kNormal.Y);
            kOut.print(' ');
            kOut.println(kNormal.Z);
            
            kOut.println("   outer loop");
            // index 1
            kVBuffer.GetPosition3(index1, kVertex);
            kOut.print("     vertex ");
            kOut.print(kVertex.X);
            kOut.print(' ');
            kOut.print(kVertex.Y);
            kOut.print(' ');
            kOut.println(kVertex.Z);
            // index 2
            kVBuffer.GetPosition3(index2, kVertex);
            kOut.print("     vertex ");
            kOut.print(kVertex.X);
            kOut.print(' ');
            kOut.print(kVertex.Y);
            kOut.print(' ');
            kOut.println(kVertex.Z);
            // index 3
            kVBuffer.GetPosition3(index3, kVertex);
            kOut.print("     vertex ");
            kOut.print(kVertex.X);
            kOut.print(' ');
            kOut.print(kVertex.Y);
            kOut.print(' ');
            kOut.println(kVertex.Z);
            
            kOut.println("   endloop");
            kOut.println(" endfacet");
        }
        kOut.println("endsolid");
    }
    
    /**
     * Print the contents of the TriMesh in STL ascii format.
     * @param kOut output
     * @param kMesh TriMesh
     */
    private static void printProstateSTLAscii(PrintWriter kOut, ModelImage kImage, TriMesh kMesh)
    {
        int i;
        int index1, index2, index3;
        VertexBuffer kVBuffer;
        float res[] = kImage.getFileInfo(0).getResolutions();
        int iTriangleCount = kMesh.IBuffer.GetIndexQuantity() / 3;
        int[] aiIndex = kMesh.IBuffer.GetData();
        
        Vector3f kNormal = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector3f kNormal2 = new Vector3f();
        Vector3f kNormal3 = new Vector3f();
        
        Vector3f kVertex1 = new Vector3f();
        Vector3f kVertex2 = new Vector3f();
        Vector3f kVertex3 = new Vector3f();
        
        kVBuffer = kMesh.VBuffer;
       
        kOut.println("solid");
        
        for (i = 0; i < iTriangleCount; i++) {
            index1 = aiIndex[(3 * i)];    
            index2 = aiIndex[((3 * i) + 1)];
            index3 = aiIndex[((3 * i) + 2)];
        
            kVBuffer.GetPosition3(index1, kNormal1);
            kVBuffer.GetPosition3(index2, kNormal2);
            kVBuffer.GetPosition3(index3, kNormal3);
            
            MipavCoordinateSystems.scannerToFile(kNormal1, kVertex1, kImage);
            MipavCoordinateSystems.scannerToFile(kNormal2, kVertex2, kImage);
            MipavCoordinateSystems.scannerToFile(kNormal3, kVertex3, kImage);
            
            kVertex1.X *= res[0]; kVertex1.Y *= res[1]; kVertex1.Z *= res[2];
            kVertex2.X *= res[0]; kVertex2.Y *= res[1]; kVertex2.Z *= res[2];
            kVertex3.X *= res[0]; kVertex3.Y *= res[1]; kVertex3.Z *= res[2];
            
            // Compute facet normal
            kNormal.Set(0f, 0f, 0f);
            kNormal.Add(kVertex1);
            kNormal.Add(kVertex2);
            kNormal.Add(kVertex3);
            kNormal.Scale(1f/3f);
            
            kOut.print(" facet normal  ");
            kOut.print(kNormal.X);
            kOut.print(' ');
            kOut.print(kNormal.Y);
            kOut.print(' ');
            kOut.println(kNormal.Z);
            
            kOut.println("   outer loop");
    		
            // index 1
            kOut.print("     vertex ");
            kOut.print(kVertex1.X);
            kOut.print(' ');
            kOut.print(kVertex1.Y);
            kOut.print(' ');
            kOut.println(kVertex1.Z);
            // index 2
            kOut.print("     vertex ");
            kOut.print(kVertex2.X);
            kOut.print(' ');
            kOut.print(kVertex2.Y);
            kOut.print(' ');
            kOut.println(kVertex2.Z);
            // index 3
            kOut.print("     vertex ");
            kOut.print(kVertex3.X);
            kOut.print(' ');
            kOut.print(kVertex3.Y);
            kOut.print(' ');
            kOut.println(kVertex3.Z);
            
            kOut.println("   endloop");
            kOut.println(" endfacet");
        }
        kOut.println("endsolid");
    }
    
    /**
     * Print the contents of the TriMesh in STL binary format.
     * @param kOut output
     * @param kMesh TriMesh
     * @throws IOException I/O exception.
     */
    private static void printSTLBinary( RandomAccessFile kOut, ModelImage kImage, TriMesh kMesh ) throws IOException
    {
        int index1, index2, index3;
        byte[] attribute = new byte[2];
        VertexBuffer kVBuffer;
        int iTriangleCount = kMesh.IBuffer.GetIndexQuantity() / 3;
        int[] aiIndex = kMesh.IBuffer.GetData();
	               
        Vector3f kVertex = new Vector3f();
        Vector3f kNormal = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector3f kNormal2 = new Vector3f();
        Vector3f kNormal3 = new Vector3f();
	        
        if (kImage != null) {
			kVBuffer = convertVertex(kImage, kMesh);
		} else {
			kVBuffer = kMesh.VBuffer;
		}
          
        
        // nothing header
        byte[] header = new byte[80];
        kOut.write(header);
	       
        // number of facets
        byte[] buff = new byte[4];
        kOut.write(FileBase.intToBytes(iTriangleCount, false, buff));
        byte[] buff2 = new byte[4];  
        for (int i = 0; i < iTriangleCount; i++) {	            
            index1 = aiIndex[(3 * i)];    
            index2 = aiIndex[((3 * i) + 1)];
            index3 = aiIndex[((3 * i) + 2)];
	            
            kVBuffer.GetPosition3(index1, kNormal1);
            kVBuffer.GetPosition3(index2, kNormal2);
            kVBuffer.GetPosition3(index3, kNormal3);
	            
            // Compute facet normal
            kNormal.Set(0f, 0f, 0f);
            kNormal.Add(kNormal1);
            kNormal.Add(kNormal2);
            kNormal.Add(kNormal3);
            kNormal.Scale(1f/3f);
	        
            // facet normal
            kOut.write(FileBase.floatToBytes(kNormal.X, false,buff2));
            kOut.write(FileBase.floatToBytes(kNormal.Y, false,buff2));
            kOut.write(FileBase.floatToBytes(kNormal.Z, false,buff2));
	            
            // index 1
            kVBuffer.GetPosition3(index1, kVertex);;            
            kOut.write(FileBase.floatToBytes(kVertex.X, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.Y, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.Z, false,buff2));
	                       
            // index 2
            kVBuffer.GetPosition3(index2, kVertex);;
            kOut.write(FileBase.floatToBytes(kVertex.X, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.Y, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.Z, false,buff2));

            // index 3
            kVBuffer.GetPosition3(index3, kVertex);
            kOut.write(FileBase.floatToBytes(kVertex.X, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.Y, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.Z, false,buff2));
	            
            // 2 byte attribute == 0
            kOut.write(attribute);
	            
        }

    }

    /**
     * Read a TriMesh from an ascii file.
     * @param kFile file containing TriMesh
     * @return TriMesh
     * @throws IOException I/O exception
     */
    private static TriMesh readAscii(File kFile) throws IOException {
        DataInputStream in =
            new DataInputStream(new BufferedInputStream(new FileInputStream(kFile)));
        String s, token;
        s = readLine(in);
        s = readLine(in);
        s = readLine(in);
        StringTokenizer st = new StringTokenizer(s);
        int iNumVertices = 1;
        if ( st.hasMoreTokens() )
        {
            token = st.nextToken();
            iNumVertices = Integer.valueOf(token).intValue();
        }
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0, 3);
        kAttr.SetCChannels(0, 4);
        VertexBuffer kVBuffer = new VertexBuffer(kAttr, iNumVertices);
        float fX, fY, fZ;
        s = readLine(in);
        for ( int i = 0; i < iNumVertices; i++ )
        {
            s = readLine(in);
            st = new StringTokenizer(s);
            token = st.nextToken();
            fX = Float.valueOf(token).floatValue();
            token = st.nextToken();
            fY = Float.valueOf(token).floatValue();
            token = st.nextToken();
            fZ = Float.valueOf(token).floatValue();
            kVBuffer.SetPosition3( i, fX, fY, fZ );
            kVBuffer.SetColor4( 0, i, ColorRGBA.WHITE);
        }
        s = readLine(in);
        for ( int i = 0; i < iNumVertices; i++ )
        {
            s = readLine(in);
            st = new StringTokenizer(s);
            token = st.nextToken();
            fX = Float.valueOf(token).floatValue();
            token = st.nextToken();
            fY = Float.valueOf(token).floatValue();
            token = st.nextToken();
            fZ = Float.valueOf(token).floatValue();
            kVBuffer.SetNormal3( i, fX, fY, fZ );
        }
        
        int iTriangleCount = 0;
        s = readLine(in);
        s = readLine(in);
        st = new StringTokenizer(s);
        if ( st.hasMoreTokens() )
        {
            token = st.nextToken();
            iTriangleCount = Integer.valueOf(token).intValue();
        }

        int[] aiIndex = new int[ iTriangleCount * 3 ];

        for (int i = 0; i < iTriangleCount; i++) {
            s = readLine(in);
            st = new StringTokenizer(s);
            token = st.nextToken();
            aiIndex[(3 * i)] = Integer.valueOf(token).intValue();
            token = st.nextToken();
            aiIndex[((3 * i) + 1)] = Integer.valueOf(token).intValue();
            token = st.nextToken();
            aiIndex[((3 * i) + 2)] = Integer.valueOf(token).intValue();
        }
        return new TriMesh( kVBuffer, new IndexBuffer(aiIndex) );
    }
    
    /** Read a line of ASCII text from the input stream. 
     * @param in InputStream
     * @return line of the ascii file as a String
     * @throws IOException I/O exception
     */
    private static String readLine(InputStream in) throws IOException
    {
        StringBuffer buf = new StringBuffer();
        int c;
        while ((c = in.read()) > -1 && c != '\n')
        {
            buf.append((char) c);
        }
        return buf.toString();
    }

    /**
     * Find the given label in the tokenizer stream and then return the next
     * three numbers as a point. Return null if end of stream
     * @param tokenizer stream in
     * @param label string label (vertex, normal ) in the STL ASCII file.
     * @return Vector3f point coordinate.
     * @throws IOException
     */
    private static Vector3f readPoint(StreamTokenizer tokenizer, String label)
        throws IOException {
        while (true) {
            if (tokenizer.nextToken() == StreamTokenizer.TT_EOF)
                return null;
            if (tokenizer.sval.equals(label))
                break;
        }

        if (tokenizer.nextToken() == StreamTokenizer.TT_EOF)
            return null;
        float x = Float.valueOf(tokenizer.sval).floatValue();
        if (tokenizer.nextToken() == StreamTokenizer.TT_EOF)
            return null;
        float y = Float.valueOf(tokenizer.sval).floatValue();
        if (tokenizer.nextToken() == StreamTokenizer.TT_EOF)
            return null;
        float z = Float.valueOf(tokenizer.sval).floatValue();

        return new Vector3f(x, y, z);
    }
    
    /**
     * Read the STL ASCII file as a single stream tokenizer.
     * @param tokenizer  stream in
     * @return  Triangle mesh
     * @throws IOException
     */
    private static TriMesh readSTLAscii(StreamTokenizer tokenizer) throws IOException {
		
        Vector3f temp = new Vector3f();
        Vector3f normal = new Vector3f(); 
        Vector3f side1 = new Vector3f();
        Vector3f side2 = new Vector3f();
        Vector3f surfaceNormal = new Vector3f();
        Vector<Vector3f> vertexArray = new Vector<Vector3f>();
        VertexBuffer kVBuffer;
        int[] aiConnect;
        
        TriMesh kMesh = null;
        float x, y, z;
        Vector3f vertex1, vertex2, vertex3;
        int index = 0;
        Integer searchIndex;
        Vector<Integer> connectivity = new Vector<Integer>();
        HashMap<String, Integer> vertexHashtable = new HashMap<String, Integer>();
        try {
        	
        	readDicomMatrix();
        	
            while (true) {
                if ((temp = readPoint(tokenizer, "normal")) == null)
                    break;
                normal = new Vector3f(temp);

                vertex1 = readPoint(tokenizer, "vertex");
                vertex2 = readPoint(tokenizer, "vertex");
                vertex3 = readPoint(tokenizer, "vertex");

                // Check that the normal is in the correct direction
                side1.Sub(vertex2, vertex1);
                side2.Sub(vertex3, vertex2);
				
                surfaceNormal.Cross(side1, side2);
                //side1.Cross(side2, surfaceNormal);
                if (normal.Dot(surfaceNormal) < 0) {
                    // vertices were specified in the wrong order, so reverse
                    // two of them
                    temp.Copy(vertex2);
                    vertex2.Copy(vertex3);
                    vertex3.Copy(temp);
                }

                // index 1;
                x = vertex1.X;
                y = vertex1.Y;
                z = vertex1.Z;

                if (dicom) {
                    tCoord[0] = x - startLocation[0];
                    tCoord[1] = y - startLocation[1];
                    tCoord[2] = z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    x = (coord[0] * direction[0]) + startLocation[0];
                    y = (coord[1] * direction[1]) + startLocation[1];
                    z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {
                    y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - y );
                    z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - z );
                }
                
                searchIndex = vertexHashtable.get((x + " " + y + " " + z));
                if (searchIndex == null) { // not found
                    vertexHashtable.put((x + " " + y + " " + z), new Integer(
                                                                             index));
                    connectivity.add(new Integer(index));
                    vertexArray.add(new Vector3f(x, y, z));
                    index++;
                } else {
                    connectivity.add(searchIndex);
                }
				
                // index 2;
                x = vertex2.X;
                y = vertex2.Y;
                z = vertex2.Z;

                if (dicom) {
                    tCoord[0] = x - startLocation[0];
                    tCoord[1] = y - startLocation[1];
                    tCoord[2] = z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    x = (coord[0] * direction[0]) + startLocation[0];
                    y = (coord[1] * direction[1]) + startLocation[1];
                    z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {
                    y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - y );
                    z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - z );
                }
                
                searchIndex = vertexHashtable.get((x + " " + y + " " + z));
                if (searchIndex == null) { // not found
                    vertexHashtable.put((x + " " + y + " " + z), new Integer(
                                                                             index));
                    connectivity.add(new Integer(index));
                    vertexArray.add(new Vector3f(x, y, z));
                    index++;
                } else {
                    connectivity.add(searchIndex);
                }
				
                // index 3;
                x = vertex3.X;
                y = vertex3.Y;
                z = vertex3.Z;

                if (dicom) {
                    tCoord[0] = x - startLocation[0];
                    tCoord[1] = y - startLocation[1];
                    tCoord[2] = z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    x = (coord[0] * direction[0]) + startLocation[0];
                    y = (coord[1] * direction[1]) + startLocation[1];
                    z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {
                    y = ( (2 * startLocation[1]) + (box[1] * direction[1]) - y );
                    z = ( (2 * startLocation[2]) + (box[2] * direction[2]) - z );
                }
                
                searchIndex = vertexHashtable.get((x + " " + y + " " + z));
                if (searchIndex == null) { // not found
                    vertexHashtable.put((x + " " + y + " " + z), new Integer(
                                                                             index));
                    connectivity.add(new Integer(index));
                    vertexArray.add(new Vector3f(x, y, z));
                    index++;
                } else {
                    connectivity.add(searchIndex);
                }

            }

            int vertexCount = vertexArray.size();
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels(3);
            kAttr.SetNChannels(3);
            kAttr.SetTChannels(0, 3);
            kAttr.SetCChannels(0, 4);
            kVBuffer = new VertexBuffer(kAttr, vertexCount);

            index = 0;
            Vector3f pos;
            for (int i = 0; i < vertexCount; i++) {
                pos = vertexArray.elementAt(i);
                kVBuffer.SetPosition3(index, pos);
                kVBuffer.SetColor4(0, index, 1.0f, 1.0f, 1.0f, 1.0f);
                index++;
            }

            int indexCount = connectivity.size();
            aiConnect = new int[indexCount];
            for (int i = 0; i < indexCount; i++) {
                aiConnect[i] = connectivity.get(i);
            }

        } catch (IOException e) {
            throw e;
        }
        IndexBuffer kIBuffer = new IndexBuffer( aiConnect.length, aiConnect );
        kMesh = new TriMesh(kVBuffer, kIBuffer);
        MaterialState kMaterial = new MaterialState();
        kMesh.AttachGlobalState( kMaterial );
        return kMesh;
    }
    

    /**
     * Load a triangle mesh from the specified file and assign to it the MaterialState.
     * @param   kImage  ModelImage displayed in the SurfaceRender class
     * @param   file    The triangle mesh file to load.
     * @param   kMaterial The Material for the surface.
     * @return  TriMesh
     */
    public static TriMesh readSurface(ModelImage kImage, File file, MaterialState kMaterial )
    {
        int iType = 0, iQuantity = 0;
        boolean isSur = true;
        int[] extents = kImage.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];


        float[] resols = kImage.getFileInfo()[0].getResolutions();
        float xBox = (xDim - 1) * resols[0];
        float yBox = (yDim - 1) * resols[1];
        float zBox = (zDim - 1) * resols[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));

        FileSurfaceRefXML_WM kSurfaceXML;
        FileInfoSurfaceRefXML kFileInfo = null;
        RandomAccessFile in = null;
        
        if (file.getName().endsWith("sur")) {
            try {
                in = new RandomAccessFile(file, "r");
                iType = in.readInt();
                iQuantity = in.readInt();
                isSur = true;
            } catch (IOException e) {
                return null;
            }
        } 
        else if ( file.getName().endsWith("xml") ) {
            kSurfaceXML = new FileSurfaceRefXML_WM(file.getName(), file.getParent());
            kFileInfo = kSurfaceXML.readSurfaceXML(file.getName(), file.getParent());

            file = new File(file.getParent()+ File.separatorChar + kFileInfo.getSurfaceFileName());
            try {
                in = new RandomAccessFile(file, "r");
                iType = in.readInt();
                iQuantity = in.readInt();
                isSur = true;
            } catch (IOException e) {
                return null;
            }
        } else if ( file.getName().endsWith("wrl") ) {

            try {
                in = new RandomAccessFile(file, "r");
                iType = 0;
                iQuantity = parseVRMLMesh(in);
                in.seek(0);
                isSur = false;
            } catch (NoSuchElementException e) {
                MipavUtil.displayError("Only load VRML file specifically written by MIPAV! ");

                return null;
            } catch (IOException e) {
                return null;
            }
        } else if ( file.getName().endsWith("stl") ) {
            iType = 0;
            iQuantity = 1;
            isSur = false;
        } else if ( file.getName().endsWith("ply") ) {
            iType = 0;
            iQuantity = 1;
            isSur = false;
        } else if ( file.getName().endsWith("gii") ) {
            try {
                in = new RandomAccessFile(file, "r");
                iType = 0;
                iQuantity = 1;
                isSur = false;
            } catch (IOException e) {
                return null;
            }
        } else {
            //has to be vtk legacy or vtk xml
            try {
                in = new RandomAccessFile(file, "r");
                iType = 0;
                iQuantity = 1;
                in.seek(0);
                //not sure what this flag means
                isSur = false;
            }
            catch (IOException e) {
                return null;
            }
        }

        TriMesh[] akComponent = new TriMesh[iQuantity];
        ViewJProgressBar progress = new ViewJProgressBar("Loading surface", "Loading surface", 0, 100, false, null,
                                                         null);
        progress.setVisible(true);

        try {
            // meshes are type TriangleMesh
            for (int i = 0; i < iQuantity; i++) {
                
                float[] startLocation = kImage.getFileInfo(0).getOrigin();
                int[] aiModelDirection = MipavCoordinateSystems.getModelDirections(kImage);
                float[] direction = new float[] { aiModelDirection[0], aiModelDirection[1], aiModelDirection[2]}; 
                float[] box = new float[]{0f,0f,0f};
                if (iType == 0) {

                    if (isSur == true) {
                        akComponent[i] = loadTMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                   true, kMaterial,
                                                   startLocation, direction, box );
                        //xBox = box[0];
                        //yBox = box[1];
                        //zBox = box[2];
                        //maxBox = Math.max(xBox, Math.max(yBox, zBox));
                    }
                    else {
                    	if ( file.getName().endsWith("wrl") ) {
                            akComponent[i] = loadVRMLMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                          (i == 0),
                                                          startLocation, direction, box );
                    	}
                        else if (file.getName().endsWith("vtk")){
                            //vtk legacy
                            akComponent[i] = loadVTKLegacyMesh(in, progress, i * 100 / iQuantity, iQuantity, file.getName());
                        }
                    	else if(file.getName().endsWith("vtp")) {
                            //vtk xml
                            akComponent[i] = loadVTKXMLMesh( file.getAbsolutePath(), file.getName(), file.getParent());
                    	}
                    	else if(file.getName().endsWith("gii")) {
                            akComponent[i] = loadGiftiXMLMesh( file.getAbsolutePath(), file.getName(), file.getParent());
                    	}
                    	else if (file.getName().endsWith("stl")) {
                            akComponent[i] = loadSTLAsciiMesh( file );
                    	}
                    	
                    	else if (file.getName().endsWith("ply")) {
                            akComponent[i] = loadPlyAsciiMesh( file );
                    	}
                        else if (file.getName().endsWith("txt")) {
                            akComponent[i] = readAscii( file );
                        }
                    }
                }
                else
                {
                    ClodMesh kClod = loadCMesh(in, progress, i * 100 / iQuantity, iQuantity );
                    kClod.TargetRecord(0);
                    kClod.SelectLevelOfDetail();
                    akComponent[i] = kClod;
                }
                if (akComponent[i] != null) 
                {
                    for (int j = 0; j < akComponent[i].VBuffer.GetVertexQuantity(); j++) {

                    	//System.err.println( akComponent[i].VBuffer.GetPosition3( j ) ); 
                        // The mesh files save the verticies as
                        // pt.x*resX*direction[0] + startLocation
                        // The loaded vertices go from -1 to 1
                        // The loaded vertex is at (2.0f*pt.x*xRes - (xDim-1)*xRes)/((dim-1)*res)max
                        akComponent[i].VBuffer.SetPosition3( j, 
                                ((2.0f * (akComponent[i].VBuffer.GetPosition3fX(j) - startLocation[0]) / direction[0]) -
                                        xBox) / (2.0f*maxBox),
                                        ((2.0f * (akComponent[i].VBuffer.GetPosition3fY(j) - startLocation[1]) / direction[1]) -
                                                yBox) / (2.0f*maxBox),
                                                ((2.0f * (akComponent[i].VBuffer.GetPosition3fZ(j) - startLocation[2]) / direction[2]) -
                                                        zBox) / (2.0f*maxBox) );
                    }
                    akComponent[i].SetName( file.getName() );
                }
                if (akComponent[i] == null) {
                    MipavUtil.displayError("Error while reading in triangle mesh.");
                    return null;
                }
            }
        } catch (IOException e) {
            return null;
        }

        progress.dispose();
        akComponent[0].UpdateMS();
        return akComponent[0];
    }

    
    /**
     * Save the TriMesh as an ascii text file.
     * @param kName file name
     * @param kMesh TriMesh
     * @throws IOException I/O exception
     */
    private static void saveAsTextFile(String kName, TriMesh kMesh) throws IOException {


        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println('0');
        kOut.println(1);

        printAscii(kOut, kMesh);

        kOut.close();
    }

    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format (text format).
     *
     * @param kName the name of file to which the triangle mesh is saved
     * @param kMesh TriMesh to save.
     * @param kVBuffer TriMesh VertexBuffer
     * @param      flip           if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                            To have proper orientations in surface file if flip is true flip y and z on reading.
     *                            param direction 1 or -1 for each axis param startLocation param box (dim-1)*resolution

     * @param direction 3D mesh orientation
     * @param startLocation 3D mesh location
     * @param box 3D mesh bounding box
     * @throws IOException  if the specified file could not be opened for writing
     */
    private static void saveAsVRML(String kName, TriMesh kMesh, VertexBuffer kVBuffer, boolean flip, int[] direction,
                                  float[] startLocation, float[] box) 
        throws IOException
    {
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println("#VRML V2.0 utf8");
        kOut.println("#MIPAV");
        kOut.println("#Number of shapes = " + 1);
        saveAsVRML(kOut, kMesh, kVBuffer, flip, direction, startLocation, box);
        kOut.close();
    }
    
    /**
     * Save the TriMesh in VTK legacy format.
     * @param kName file name
     * @param kMesh TriMesh
     * @throws IOException I/O exception
     */
    private static void saveAsVTKLegacy(String kName, TriMesh kMesh) throws IOException {
    	PrintWriter kOut = new PrintWriter(new FileWriter(kName));
    	int pointCount = kMesh.VBuffer.GetVertexQuantity();
        int indexCount = kMesh.IBuffer.GetIndexQuantity();
        kOut.println("# vtk DataFile Version 2.0");
        kOut.println(kMesh.GetName());
        kOut.println("ASCII");
        kOut.println("DATASET POLYDATA");
        kOut.println("POINTS "+pointCount+" float");
        Vector3f p=new Vector3f();
        String tmp;
        for(int i=0;i<pointCount;i++){
            kMesh.VBuffer.GetPosition3(i,p);
            tmp=String.format("%.5f %.5f %.5f\n", p.X,p.Y,p.Z);
            kOut.print(tmp);
        }
        kOut.println("POLYGONS "+indexCount/3+" "+(4*indexCount/3));
        int[] aiIndex = kMesh.IBuffer.GetData();
        for(int i=0;i<indexCount;i+=3){
            kOut.println(3+" "+aiIndex[i]+" "+aiIndex[i+1]+" "+aiIndex[i+2]);
        }
        kOut.close();
    }
   
    /**
     * Save the TriMesh in VTK format.
     * @param fileName file name
     * @param kMesh TriMesh
     */
    private static void saveAsVTKXML(String fileName, TriMesh kMesh)
    {
    	try {
    		FileSurfaceVTKXML_WM surfaceVTKXML = new FileSurfaceVTKXML_WM(null, null);
        	surfaceVTKXML.writeXMLsurface(fileName, kMesh);
        } catch (IOException kError) { }
    }
    
    /**
     * Save the TriMesh in Gifti format.
     * @param fileName file name
     * @param kMesh TriMesh
     */
    private static void saveAsGiftiXML(String fileName, TriMesh kMesh)
    {
    	try {
    		FileSurfaceGiftiXML_WM surfaceGiftiXML = new FileSurfaceGiftiXML_WM(null, null);
        	surfaceGiftiXML.writeXMLsurface(fileName, kMesh);
        } catch (IOException kError) { }
    }
    

    /**
     * Support for saving the collapse records to a binary file. 
     * @param kOut  the file to which the records are saved
     * @param kRecord CollapseRecord to save.
     * @exception  IOException  if the there is an error writing to the file
     */
    private static void saveCollapseRecord(RandomAccessFile kOut, CollapseRecord kRecord) throws IOException {
        kOut.writeInt(kRecord.VKeep);
        kOut.writeInt(kRecord.VThrow);
        kOut.writeInt(kRecord.VQuantity);
        kOut.writeInt(kRecord.TQuantity);
        kOut.writeInt(kRecord.IQuantity);

        byte[] bufferInt = new byte[kRecord.IQuantity * 4];
        int tmpInt;
        for (int i = 0, index = 0; i < kRecord.IQuantity; i++) {
            tmpInt = kRecord.Index[i];
            bufferInt[index++] = (byte) (tmpInt >>> 24);
            bufferInt[index++] = (byte) (tmpInt >>> 16);
            bufferInt[index++] = (byte) (tmpInt >>> 8);
            bufferInt[index++] = (byte) (tmpInt & 0xff);
        }
        kOut.write(bufferInt);
    }
	
    /**
     * Saves a single level of detail to a mesh file. Opens a file dialog to get the output file name from the user.
     *
     * @param  kImage  ModelImage displayed in the SurfaceRender object
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param kMesh TruNesg
     */
    private static void saveSingleMesh(ModelImage kImage, boolean isSur, TriMesh kMesh)
    {
        String name = getFileName(false);

        if (name == null) {
            return;
        }

        int i = name.lastIndexOf('.');

        if ((i > 0) && (i < (name.length() - 1))) {
            String extension = name.substring(i + 1).toLowerCase();

            if (isSur && !extension.equals("sur")) {
                MipavUtil.displayError("Extension must be .sur");

                return;
            } else if (!isSur && !extension.equals("wrl")) {
                MipavUtil.displayError("Extension must be .wrl");

                return;
            }
        } else if (isSur) {
            name = name + ".sur";
        } else {
            name = name + ".wrl";
        }

        saveSingleMesh( name, kImage, isSur, kMesh );
    }
	
    /**
     * Saves a single level of detail to a mesh file. The file name is passed as a parameter.
     *
     * @param  name    the file name
     * @param  kImage  ModelImage displayed in the SurfaceRender object
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param kMesh TriMesh
     */
    private static void saveSingleMesh(String name, ModelImage kImage, boolean isSur, TriMesh kMesh )
    {
        if (name != null) {

            try {
                float[] startLocation = kImage.getFileInfo(0).getOrigin();
                int[] extents = kImage.getExtents();
                int xDim = extents[0];
                int yDim = extents[1];
                int zDim = extents[2];
                
                float[] resols = kImage.getFileInfo()[0].getResolutions();
                float xBox = (xDim - 1) * resols[0];
                float yBox = (yDim - 1) * resols[1];
                float zBox = (zDim - 1) * resols[2];
                float maxBox = Math.max(xBox, Math.max(yBox, zBox));

                int[] direction = MipavCoordinateSystems.getModelDirections(kImage);
                float[] box = new float[]{ xBox, yBox, zBox };

                //                 for (int i = 0; i < meshes.length; i++) {
                VertexBuffer kVBuffer = new VertexBuffer( kMesh.VBuffer );

                // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                // The mesh files must save the verticies as
                // pt.x*resX*direction[0] + startLocation
                for (int j = 0; j < kVBuffer.GetVertexQuantity(); j++) {
                    kVBuffer.SetPosition3( j,
                                           ((((kMesh.VBuffer.GetPosition3fX(j) * 2.0f * maxBox) + xBox) / 2.0f) * direction[0]) +
                                           startLocation[0],
                                           ((((kMesh.VBuffer.GetPosition3fY(j) * 2.0f * maxBox) + yBox) / 2.0f) * direction[1]) +
                                           startLocation[1],
                                           ((((kMesh.VBuffer.GetPosition3fZ(j) * 2.0f * maxBox) + zBox) / 2.0f) * direction[2]) +
                                           startLocation[2] );

                    // flip y and z
                    //                         kVBuffer.SetPosition3( j, kVBuffer.GetPosition3fX(j),
                    //                                                (2 * startLocation[1]) + (box[1] * direction[1]) - kVBuffer.GetPosition3fY(j),
                    //                                                (2 * startLocation[2]) + (box[2] * direction[2]) - kVBuffer.GetPosition3fZ(j) );

                    if (isSur &&
                        (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))) {

                        // Get the DICOM transform that describes the transformation from
                        // axial to this image orientation
                        TransMatrix dicomMatrix = kImage.getMatrix().clone();
                        float[] coord = new float[3];
                        float[] tCoord = new float[3];

                        // Change the voxel coordinate into millimeter space
                        coord[0] = (kVBuffer.GetPosition3fX(j) - startLocation[0]) / direction[0];
                        coord[1] = (kVBuffer.GetPosition3fY(j) - startLocation[1]) / direction[1];
                        coord[2] = (kVBuffer.GetPosition3fZ(j) - startLocation[2]) / direction[2];

                        // Convert the point to axial millimeter DICOM space
                        dicomMatrix.transform(coord, tCoord);

                        // Add in the DICOM origin
                        tCoord[0] = tCoord[0] + startLocation[0];
                        tCoord[1] = tCoord[1] + startLocation[1];
                        tCoord[2] = tCoord[2] + startLocation[2];
                        kVBuffer.SetPosition3(j, tCoord[0], tCoord[1], tCoord[2]);
                    }
                }
                //                 }

                if (isSur == true) {
                    //double[][] inverseDicomArray = null;
                    TransMatrix inverse_DicomMatrix = null;
                    if (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
                        inverse_DicomMatrix = kImage.getMatrix().clone();
                        inverse_DicomMatrix.Inverse();
                        //inverseDicomArray = inverseDicomMatrix.getMatrix();
                    }

                    saveSur(name, kMesh, 0, kVBuffer, true, direction, startLocation, box, inverse_DicomMatrix);
                } else {
                    saveAsVRML(name, kMesh, kVBuffer, true, direction, startLocation, box );
                }
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
    }

    /**
     * Saves a single level of detail to a STL mesh file.
     * @param  kName file name.
     * @param  kMesh   Triangle mesh
     */
    private static void saveSinglePlyMesh(String kName, ModelImage kImage, TriMesh kMesh)
    {
    	String name = kName;
    	VertexBuffer kVBuffer;

    	if ( kName == null )
    	{
    		name = getFileName(false);
    	}
        if (name == null) 
        {
            return;
        }         
        try {
            PrintWriter kOut = new PrintWriter(new FileWriter(name));
            int iTriangleCount = kMesh.IBuffer.GetIndexQuantity() / 3;
            int iVertexCount = kMesh.VBuffer.GetVertexQuantity();
            int[] aiIndex = kMesh.IBuffer.GetData();
	        
            // write header
            kOut.println("ply"); // object is TriMesh
            kOut.println("format ascii 1.0");
            kOut.println("element vertex " + iVertexCount);
            kOut.println("property float32 x");
            kOut.println("property float32 y");
            kOut.println("property float32 z");
            kOut.println("element face " + iTriangleCount);
            kOut.println("property list uint8 int32 vertex_indices");
            kOut.println("end_header");
	       
            if (kImage != null) {
    			kVBuffer = convertVertex(kImage, kMesh);
    		} else {
    			kVBuffer = kMesh.VBuffer;
    		}
              
            
            
            Vector3f kVertex = new Vector3f();
            for (int i = 0; i < iVertexCount; i++) {
                kVBuffer.GetPosition3(i, kVertex);;   
                kOut.print(kVertex.X);
                kOut.print(' ');
                kOut.print(kVertex.Y);
                kOut.print(' ');
                kOut.println(kVertex.Z);
            }
	        	        
            for (int i = 0; i < iTriangleCount; i++) {
                kOut.print('3');
                kOut.print(' ');
                kOut.print(aiIndex[3 * i]);
                kOut.print(' ');
                kOut.print(aiIndex[(3 * i) + 1]);
                kOut.print(' ');
                kOut.println(aiIndex[(3 * i) + 2]);
            }
	        
            kOut.close();
	   	    
        } catch (IOException error) {
            MipavUtil.displayError("Error while trying to save single mesh");
        }
	        
    }

    /**
     * Saves a single level of detail to a STL mesh file.
     * @param  kName file name.
     * @param  kMesh   Triangle mesh
     */
    private static void saveProstatePlyMesh(String kName, ModelImage kImage, TriMesh kMesh)
    {
    	String name = kName;
    	VertexBuffer kVBuffer;

    	if ( kName == null )
    	{
    		name = getFileName(false);
    	}
        if (name == null) 
        {
            return;
        }         
        try {
            PrintWriter kOut = new PrintWriter(new FileWriter(name));
            int iTriangleCount = kMesh.IBuffer.GetIndexQuantity() / 3;
            int iVertexCount = kMesh.VBuffer.GetVertexQuantity();
            int[] aiIndex = kMesh.IBuffer.GetData();
	        
            // write header
            kOut.println("ply"); // object is TriMesh
            kOut.println("format ascii 1.0");
            kOut.println("element vertex " + iVertexCount);
            kOut.println("property float32 x");
            kOut.println("property float32 y");
            kOut.println("property float32 z");
            kOut.println("element face " + iTriangleCount);
            kOut.println("property list uint8 int32 vertex_indices");
            kOut.println("end_header");
	       
    	    kVBuffer = kMesh.VBuffer;
    		
            Vector3f kVertex = new Vector3f();
            Vector3f output = new Vector3f();
            float res[] = kImage.getFileInfo(0).getResolutions();
            for (int i = 0; i < iVertexCount; i++) {
                kVBuffer.GetPosition3(i, kVertex);
                
                MipavCoordinateSystems.scannerToFile(kVertex, output, kImage);
                
                kOut.print(output.X * res[0]);
                kOut.print(' ');
                kOut.print(output.Y * res[1]);
                kOut.print(' ');
                kOut.println(output.Z * res[2]);
            }
	        	        
            for (int i = 0; i < iTriangleCount; i++) {
                kOut.print('3');
                kOut.print(' ');
                kOut.print(aiIndex[3 * i]);
                kOut.print(' ');
                kOut.print(aiIndex[(3 * i) + 1]);
                kOut.print(' ');
                kOut.println(aiIndex[(3 * i) + 2]);
            }
	        
            kOut.close();
	   	    
        } catch (IOException error) {
            MipavUtil.displayError("Error while trying to save single mesh");
        }
	        
    }

    
    /**
     * Saves a single level of detail to a STL mesh file.
     * @param  kName file name.
     * @param  kMesh   Triangle mesh
     */
    private static void saveSingleSTLMesh(String kName, ModelImage kImage, TriMesh kMesh ) {
    	String name = kName;
    	if ( kName == null )
    	{
            name = getFileName(false);
    	}
        if (name == null) 
        {
            return;
        } 
        int i = name.lastIndexOf('.');
      
        try {
                PrintWriter kOut = new PrintWriter(new FileWriter(name));
                printSTLAscii(kOut, kImage, kMesh);
                kOut.close();
      	   	    
        } catch (IOException error) {
            MipavUtil.displayError("Error while trying to save single mesh");
        }
	        
    }
    
    /**
     * Saves a single level of detail to a STL ascii mesh file.
     * @param  kName file name.
     * @param  kMesh   Triangle mesh
     */
    private static void saveProstateSTLMesh(String kName, ModelImage kImage, TriMesh kMesh ) {
    	String name = kName;
    	if ( kName == null )
    	{
            name = getFileName(false);
    	}
        if (name == null) 
        {
            return;
        } 
        int i = name.lastIndexOf('.');
      
         try {
                PrintWriter kOut = new PrintWriter(new FileWriter(name));
                printProstateSTLAscii(kOut, kImage, kMesh);
                kOut.close();
        
            
        } catch (IOException error) {
            MipavUtil.displayError("Error while trying to save single mesh");
        }
	        
    }


    /**
     * Write the TriMesh to disk in the .sur format.
     * @param kName file name.
     * @param kMesh TriMesh.
     * @param iType type == 0 for TriMesh, type == 1 for ClodMesh
     * @param kVBuffer TriMesh VertexBuffer
     * @param flip flag for inverting the y,z values
     * @param direction 3D mesh direction
     * @param startLocation 3D mesh location
     * @param box 3D mesh bounding box
     * @param inverseDicomMatrix dicom matrix information.
     * @throws IOException if there is an error writing to the file
     */
    private static void saveSur(String kName, TriMesh kMesh,
                               int iType, VertexBuffer kVBuffer,
                               boolean flip, int[] direction, float[] startLocation, float[] box,
                               TransMatrix inverseDicomMatrix)
    throws IOException
    {
        RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");
        kOut.writeInt(iType); 
        kOut.writeInt(1);
        
        if (inverseDicomMatrix == null) {
            
            if (flip) {
                kOut.writeInt(1);
            } else {
                kOut.writeInt(0);
            }
        } else {
            
            if (flip) {
                kOut.writeInt(3);
            } else {
                kOut.writeInt(2);
            }
        }

        kOut.writeInt(direction[0]);
        kOut.writeInt(direction[1]);
        kOut.writeInt(direction[2]);

        byte[] buffer = new byte[24];
        int index, tmpInt;
        long tmpLong;

        index = 0;
        tmpInt = Float.floatToIntBits(startLocation[0]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(startLocation[1]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(startLocation[2]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(box[0]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(box[1]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(box[2]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        kOut.write(buffer);

        if (inverseDicomMatrix != null) {
            buffer = new byte[128];
            index = 0;

            for (int i = 0; i <= 3; i++) {

                for (int j = 0; j <= 3; j++) {
                    tmpLong = Double.doubleToLongBits(inverseDicomMatrix.Get(i, j));
                    buffer[index++] = (byte) (tmpLong >>> 56);
                    buffer[index++] = (byte) (tmpLong >>> 48);
                    buffer[index++] = (byte) (tmpLong >>> 40);
                    buffer[index++] = (byte) (tmpLong >>> 32);
                    buffer[index++] = (byte) (tmpLong >>> 24);
                    buffer[index++] = (byte) (tmpLong >>> 16);
                    buffer[index++] = (byte) (tmpLong >>> 8);
                    buffer[index++] = (byte) (tmpLong & 0xff);
                }
            }

            kOut.write(buffer);
        }

        // write vertices
        kOut.writeInt(kVBuffer.GetVertexQuantity());
        byte[] bufferByte = new byte[kVBuffer.GetVertexQuantity() * 24];

        Vector3f kPos = new Vector3f();
        index = 0;
        for (int i = 0; i < kVBuffer.GetVertexQuantity(); i++) {
            kVBuffer.GetPosition3( i, kPos );

            tmpInt = Float.floatToIntBits(kPos.X);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kPos.Y);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kPos.Z);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

        }

        // write normals
        Vector3f kNormal = new Vector3f();
        for (int i = 0; i < kVBuffer.GetVertexQuantity(); i++) {
        	kVBuffer.GetNormal3( i, kNormal );

            tmpInt = Float.floatToIntBits(kNormal.X);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kNormal.Y);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kNormal.Z);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

        }

        kOut.write(bufferByte);
        // System.err.println("buffer byte size = " + index + "  actual size = " + (getVertexCount() * 24));
        // write connectivity
        kOut.writeInt(kMesh.IBuffer.GetIndexQuantity());

        byte[] bufferInt = new byte[kMesh.IBuffer.GetIndexQuantity() * 4];
        int[] aiIndex = kMesh.IBuffer.GetData();
        index = 0;
        for (int i = 0; i < kMesh.IBuffer.GetIndexQuantity(); i++) {
            tmpInt = aiIndex[i];
            bufferInt[index++] = (byte) (tmpInt >>> 24);
            bufferInt[index++] = (byte) (tmpInt >>> 16);
            bufferInt[index++] = (byte) (tmpInt >>> 8);
            bufferInt[index++] = (byte) (tmpInt & 0xff);

        }

        kOut.write(bufferInt);
        if ( kVBuffer.GetAttributes().GetCChannels(0) == 0 )
        {
            kOut.writeInt(0);
        }
        else if ( kVBuffer.GetAttributes().GetCChannels(0) != 0 )
        {
            kOut.writeInt(1);
            byte[] bufferColor = new byte[kVBuffer.GetVertexQuantity() * 4 * 4];
            ColorRGBA kColor = new ColorRGBA();
            int R, G, B ,A;
            index = 0;
            for (int i = 0; i < kVBuffer.GetVertexQuantity(); i++) {
            	kVBuffer.GetColor4( 0, i, kColor );

                R = Float.floatToIntBits(kColor.R);
                bufferColor[index++] = (byte) (R >>> 24);
                bufferColor[index++] = (byte) (R >>> 16);
                bufferColor[index++] = (byte) (R >>> 8);
                bufferColor[index++] = (byte) (R & 0xff);

                G = Float.floatToIntBits(kColor.G);
                bufferColor[index++] = (byte) (G >>> 24);
                bufferColor[index++] = (byte) (G >>> 16);
                bufferColor[index++] = (byte) (G >>> 8);
                bufferColor[index++] = (byte) (G & 0xff);

                B = Float.floatToIntBits(kColor.B);
                bufferColor[index++] = (byte) (B >>> 24);
                bufferColor[index++] = (byte) (B >>> 16);
                bufferColor[index++] = (byte) (B >>> 8);
                bufferColor[index++] = (byte) (B & 0xff);

                A = Float.floatToIntBits(kColor.A);
                bufferColor[index++] = (byte) (A >>> 24);
                bufferColor[index++] = (byte) (A >>> 16);
                bufferColor[index++] = (byte) (A >>> 8);
                bufferColor[index++] = (byte) (A & 0xff);

            }
            kOut.write(bufferColor);
        }
        if ( iType == 1 )
        {
            // write collapse records
            kOut.writeInt( ((ClodMesh)kMesh).GetRecordQuantity());
            
            for (int i = 0; i < ((ClodMesh)kMesh).GetRecordQuantity(); i++) {
                saveCollapseRecord(kOut,((ClodMesh)kMesh).GetRecord(i));
            }
        }
        kOut.close();
    }

    
    

    /**
     * Writes a TriMesh to disk in the xml format, based on surface.xsd.
     * @param  kName file name.
     * @param kImage ModelImage associated with the TriMesh.
     * @param  kMesh    TriMesh surface mesh.
     */
    private static void writeTriangleMeshXML(String kName, ModelImage kImage, TriMesh kMesh)
    {
    	String name = kName;
    	if ( kName == null )
    	{
            name = getFileName(false);
    	}
        if (name == null) 
        {
            return;
        } 
        String surfaceName = null;
        // Check the filename extension:
        int i = name.lastIndexOf('.');
        surfaceName = name.substring(0, i) + ".sur";
        if ((i > 0) && (i < (name.length() - 1))) {
            String extension = name.substring(i + 1).toLowerCase();

            if (!extension.equals("xml")) {
                MipavUtil.displayError("Extension must be .xml");

                return;
            }
            
        } else {
            surfaceName = name + ".sur";
            name = name + ".xml"; 
        }
        
        try {
            FileSurfaceRefXML_WM kSurfaceXML = new FileSurfaceRefXML_WM(null, null);
            MaterialState kMaterial = (MaterialState)kMesh.GetGlobalState(GlobalState.StateType.MATERIAL);
            float fOpacity = 1.0f - kMesh.VBuffer.GetColor4(0, 0).A;
            int iLOD = 0;
            kSurfaceXML.writeXMLsurface_WM(name, kMaterial, fOpacity, iLOD );
            saveSingleMesh( surfaceName, kImage, true, kMesh );
        } catch ( IOException kError ) { }
    }
    
    /**
     * Convert the vertex from display space to dicom space.
     * @param kImage  ModelImage
     * @param kMesh   Triangle mesh
     * @return kVBuffer   Converted vertex buffer
     */
    private static VertexBuffer convertVertex(ModelImage kImage, TriMesh kMesh ) {
    	float[] startLocation = kImage.getFileInfo(0).getOrigin();
        int[] extents = kImage.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];
        
        float[] resols = kImage.getFileInfo()[0].getResolutions();
        float xBox = (xDim - 1) * resols[0];
        float yBox = (yDim - 1) * resols[1];
        float zBox = (zDim - 1) * resols[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));

        int[] direction = MipavCoordinateSystems.getModelDirections(kImage);
        float[] box = new float[]{ xBox, yBox, zBox };

        //                 for (int i = 0; i < meshes.length; i++) {
        VertexBuffer kVBuffer = new VertexBuffer( kMesh.VBuffer );

        // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
        // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
        // The mesh files must save the verticies as
        // pt.x*resX*direction[0] + startLocation
        for (int j = 0; j < kVBuffer.GetVertexQuantity(); j++) {
            kVBuffer.SetPosition3( j,
                                   ((((kMesh.VBuffer.GetPosition3fX(j) * 2.0f * maxBox) + xBox) / 2.0f) * direction[0]) +
                                   startLocation[0],
                                   ((((kMesh.VBuffer.GetPosition3fY(j) * 2.0f * maxBox) + yBox) / 2.0f) * direction[1]) +
                                   startLocation[1],
                                   ((((kMesh.VBuffer.GetPosition3fZ(j) * 2.0f * maxBox) + zBox) / 2.0f) * direction[2]) +
                                   startLocation[2] );

            // flip y and z
            //                         kVBuffer.SetPosition3( j, kVBuffer.GetPosition3fX(j),
            //                                                (2 * startLocation[1]) + (box[1] * direction[1]) - kVBuffer.GetPosition3fY(j),
            //                                                (2 * startLocation[2]) + (box[2] * direction[2]) - kVBuffer.GetPosition3fZ(j) );

            if (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {

                // Get the DICOM transform that describes the transformation from
                // axial to this image orientation
                TransMatrix dicomMatrix = kImage.getMatrix().clone();
                float[] coord = new float[3];
                float[] tCoord = new float[3];

                // Change the voxel coordinate into millimeter space
                coord[0] = (kVBuffer.GetPosition3fX(j) - startLocation[0]) / direction[0];
                coord[1] = (kVBuffer.GetPosition3fY(j) - startLocation[1]) / direction[1];
                coord[2] = (kVBuffer.GetPosition3fZ(j) - startLocation[2]) / direction[2];

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                tCoord[0] = tCoord[0] + startLocation[0];
                tCoord[1] = tCoord[1] + startLocation[1];
                tCoord[2] = tCoord[2] + startLocation[2];
                kVBuffer.SetPosition3(j, tCoord[0], tCoord[1], tCoord[2]);
            }
        }
        return kVBuffer;
    }
    

}
