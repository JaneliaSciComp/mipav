package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.surfaceview.*;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import javax.swing.*;
import javax.vecmath.Color3f;
import javax.vecmath.Point3f;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

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
 * @see  FileInfoSurfaceXML.java
 * @see  ModelTriangleMesh.java
 */
public class FileSurface_WM {
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The action taken when the Add button is clicked in the JPanelSurface class. A file dialog is launched that allows
     * the user to select new surfaces to load from disk.
     *
     * @param   kImage     the ModelImage displayed in the SurfaceRender class
     * @param   iListSize  the current number of triangle-mesh surfaces displayed in the SurfaceRender class (for
     *                     calculating the surface color)
     *
     * @return  SurfaceAttributes[] an array of surfaces described by their SurfaceAttributes, used to add the surfaces
     *          to a scene graph.
     */
    //public static SurfaceAttributes_WM[] openSurfaces(ModelImage kImage, int iListSize) {
    public static TriMesh[] openSurfaces(ModelImage kImage, int iListSize) {
        File[] akFiles = openFiles(true);

        if (akFiles == null) {
            return null;
        }

        TriMesh[] kSurface = new TriMesh[akFiles.length];
        for (int i = 0; i < akFiles.length; i++) {
            String kName = akFiles[i].getName();
            
            ColorRGB kColor = JPanelSurface_WM.getNewSurfaceColor(iListSize + i);

            if ((kName.indexOf(".sur") != -1) || (kName.indexOf(".wrl") != -1) || (kName.indexOf(".vtk") != -1) || (kName.indexOf(".vtp") != -1)) {
                kSurface[i] = readSurface(kImage, akFiles[i], kColor, 1.0f, null, 0 );
            } else if (kName.indexOf(".xml") != -1) {
                FileSurfaceRefXML kSurfaceXML = new FileSurfaceRefXML(kName, akFiles[i].getParent());
                try {
                    FileInfoSurfaceRefXML kFileInfo = kSurfaceXML.readSurfaceXML(kName, akFiles[i].getParent());
                    akFiles[i] = new File(akFiles[i].getParent()+ File.separatorChar + kFileInfo.getSurfaceFileName());
                    kSurface[i] = readSurface(kImage, akFiles[i], kColor, kFileInfo.getOpacity(), kFileInfo.getMaterialWM(), kFileInfo.getLevelDetail());
                } catch (IOException e) {
                    return null;
                }
            }
        }

        return kSurface;
    }


    /**
     * Load a triangle mesh from the specified file and assign to it the specified color.
     *
     * @param   kImage  ModelImage displayed in the SurfaceRender class
     * @param   file    The triangle mesh file to load.
     * @param   color   The diffuse and specular color for the surface material.
     *
     * @return  DOCUMENT ME!
     */
    //public static SurfaceAttributes_WM readSurface(ModelImage kImage, File file, Color4f color) {
    public static TriMesh readSurface(ModelImage kImage, File file, ColorRGB kColor, float fOpacity, MaterialState kMaterial, int iLOD )
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
        int iV1, iV2, iV3;
        float d1, d2, d3;

        FileSurfaceRefXML kSurfaceXML;
        FileInfoSurfaceRefXML kFileInfo = null;
        RandomAccessFile in = null;
        boolean isXMLSurface = false;
        
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
            isXMLSurface = true;
            kSurfaceXML = new FileSurfaceRefXML(file.getName(), file.getParent());
            try {
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
            } catch (IOException e) {
                return null;
            }
        } else if ( file.getName().endsWith("wrl") ) {

            try {
                in = new RandomAccessFile(file, "r");
                iType = 0;
                iQuantity = ModelTriangleMesh.parseVRMLMesh(in);
                in.seek(0);
                isSur = false;
            } catch (NoSuchElementException e) {
                MipavUtil.displayError("Only load VRML file specifically written by MIPAV! ");

                return null;
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

        ModelClodMesh kClod = null;
        TriMesh[] akComponent = new TriMesh[iQuantity];
        ViewJProgressBar progress = new ViewJProgressBar("Loading surface", "Loading surface", 0, 100, false, null,
                                                         null);
        progress.setVisible(true);

        try {
            // meshes are type TriangleMesh
            for (int i = 0; i < iQuantity; i++) {
                
                float[] startLocation = kImage.getFileInfo(0).getOrigin();
                int[] aiModelDirection = MipavCoordinateSystems.getModelDirections(kImage);
                float[] direction = new float[] { (int)aiModelDirection[0], (int)aiModelDirection[1], (int)aiModelDirection[2]}; 
                float[] box = new float[]{0f,0f,0f};
                if (iType == 0) {

                    if (isSur == true) {
                        akComponent[i] = loadTMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                   true, kColor, fOpacity, kMaterial, iLOD,
                                                   startLocation, direction, box );
                        akComponent[i].SetName( file.getName() );
                        System.err.println( "box " + box[0] + " " +  box[1] + " " +  box[2] );
                        xBox = box[0];
                        yBox = box[1];
                        zBox = box[2];
                        maxBox = Math.max(xBox, Math.max(yBox, zBox));
                    }
                    else {
                    	if ( file.getName().endsWith("wrl") ) {
                            akComponent[i] = loadVRMLMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                          (i == 0),
                                                          startLocation, direction, box );
                            akComponent[i].SetName( file.getName() );
                            System.err.println( "box " + box[0] + " " +  box[1] + " " +  box[2] );
                    	}
                        else if (file.getName().endsWith("vtk")){
                            //vtk legacy
                            akComponent[i] = loadVTKLegacyMesh(in, progress, i * 100 / iQuantity, iQuantity, (i == 0), file.getName());
                            if ( akComponent[i] != null)
                            {
                                akComponent[i].SetName( file.getName() );
                            }
                        }
                    	else if(file.getName().endsWith("vtp")) {
                            //vtk xml
                            akComponent[i] = loadVTKXMLMesh( file.getAbsolutePath(), file.getName(), file.getParent());
                            if ( akComponent[i] != null)
                            {
                                akComponent[i].SetName( file.getName() );
                            }
                    	}

                    }
                    
                }
                //                 else {
                //                     kClod = ModelClodMesh.loadCMesh(in, progress, i * 100 / iQuantity, iQuantity);
                //                     akComponent[i] = kClod.getMesh();
                //                     kClod.setLOD(kClod.getLOD() + 1);
                //                     direction = ModelClodMesh.getDirection();
                //                     startLocation = ModelClodMesh.getStartLocation();
                //                     akVertex = kClod.getMesh().getVertexCopy();
                //                     aiConnect = kClod.getMesh().getIndexCopy();
                //                     akTriangle = new Point3f[aiConnect.length / 3][3];
                //                 }

                if (akComponent[i] == null) {
                    MipavUtil.displayError("Error while reading in triangle mesh.");

                    return null;
                }
                System.err.println( "StartLocation " + startLocation[0] + " " + startLocation[1] + " " + startLocation[2] );
                System.err.println( "Direction " + direction[0] + " " + direction[1] + " " + direction[2] );
                System.err.println( "xBox " + xBox + " " + yBox + " " + zBox );
                for (int j = 0; j < akComponent[i].VBuffer.GetVertexQuantity(); j++) {

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

                //                 if (iType != 0) {
                //                     kClod.setLOD(kClod.getMaximumLOD());
                //                     akComponent[i] = kClod.getMesh();
                //                 }

                //                 akComponent[i].setVerticies(akVertex);

                //                 if (iType != 0) {
                //                     kClod.setVerticies(akVertex);
                //                 }
            }
        } catch (IOException e) {
            return null;
        }

        progress.dispose();
        akComponent[0].UpdateMS();
        return akComponent[0];
    }

    /**
     * The action taken when the one of the save surface buttons is pressed in the JPanelSurface class. A file dialog is
     * launched that allows the user to select where to save the surfaces.
     *
     * @param  kImage      the ModelImage displayed in the SurfaceRender class
     * @param  akSurfaces  an array of surfaces described by their SurfaceAttributes, containing information that is
     *                     saved with the ModelTriangleMesh
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
                 writeTriangleMeshXML(kImage, akSurfaces[i]);
             }
        }
    }


    /**
     * Calls a dialog to get a file name.
     *
     * @param   bLoad  if <code>true</code>, make it a load dialog.
     *
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
     * Returns an array of File objects, based on the user-selected files from the FileChooser dialog.
     *
     * @param   bLoad  whether the files are opened for reading (bLoad = true) or writing (bLoad = false)
     *
     * @return  File[] array of opened files.
     */
    private static File[] openFiles(boolean bLoad) {

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(bLoad);
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
            } else {
                File[] files = new File[1];
                files[0] = chooser.getSelectedFile();

                return files;
            }
        }

        return null;
    }

    
    public static TriMesh loadTMesh(RandomAccessFile kIn, ViewJProgressBar progress,
                                    int added, int total, boolean isVisible,
                                    ColorRGB kColor, float fOpacity, MaterialState kMaterial, int iLOD,
                                    float[] startLocation, float[] direction, float[] box )
        throws IOException {

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
            System.err.println("flip = " + flip);
            if ((actions == 2) || (actions == 3)) {
                dicom = true;
            } else {
                dicom = false;
            }
            System.err.println("dicom = " + dicom);

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

            System.err.println("Surface: startLocation[0] = " + startLocation[0] + "  startLocation[1] = " +  startLocation[1] + "  startLocation[2] = " +  startLocation[2]);
            
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

            System.err.println("Surface: box[0] = " + box[0] + "  box[1] = " +  box[1] + "  box[2] = " +  box[2]);
            
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

                inverseDicomMatrix.setMatrix(inverseDicomArray);

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

                akVertex[i].X( Float.intBitsToFloat(tmpInt) );

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].Y( Float.intBitsToFloat(tmpInt) );

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].Z( Float.intBitsToFloat(tmpInt) );

                if (dicom) {
                    tCoord[0] = akVertex[i].X() - startLocation[0];
                    tCoord[1] = akVertex[i].Y() - startLocation[1];
                    tCoord[2] = akVertex[i].Z() - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    akVertex[i].X( (coord[0] * direction[0]) + startLocation[0] );
                    akVertex[i].Y( (coord[1] * direction[1]) + startLocation[1] );
                    akVertex[i].Z( (coord[2] * direction[2]) + startLocation[2] );
                } // if (dicom)

                if (flip) {

                    //                  Flip (kVertex.y - startLocation[1], but
                    //                  don't flip startLocation[1]
                    akVertex[i].Y( (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[i].Y() );
                    akVertex[i].Z( (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[i].Z() );
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

                akNormal[i].X( Float.intBitsToFloat(tmpInt) );

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].Y( Float.intBitsToFloat(tmpInt) );

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].Z( Float.intBitsToFloat(tmpInt) );
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

                    perVertexColor[i].R( Float.intBitsToFloat(R) );

                    b1 = bufferPerVertexColor[index++] & 0xff;
                    b2 = bufferPerVertexColor[index++] & 0xff;
                    b3 = bufferPerVertexColor[index++] & 0xff;
                    b4 = bufferPerVertexColor[index++] & 0xff;

                    G = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                    perVertexColor[i].G( Float.intBitsToFloat(G) );

                    b1 = bufferPerVertexColor[index++] & 0xff;
                    b2 = bufferPerVertexColor[index++] & 0xff;
                    b3 = bufferPerVertexColor[index++] & 0xff;
                    b4 = bufferPerVertexColor[index++] & 0xff;

                    B = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                    perVertexColor[i].B( Float.intBitsToFloat(B) );

                    b1 = bufferPerVertexColor[index++] & 0xff;
                    b2 = bufferPerVertexColor[index++] & 0xff;
                    b3 = bufferPerVertexColor[index++] & 0xff;
                    b4 = bufferPerVertexColor[index++] & 0xff;

                    A = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                    perVertexColor[i].A( Float.intBitsToFloat(A) );

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
                    pkVB.SetColor4(0, i, perVertexColor[i].R(), perVertexColor[i].G(), perVertexColor[i].B(), perVertexColor[i].A());
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
     * Load the triangle mesh from a VRML file specifically written by MIPAV!. The caller must have already opened the
     * file. Must be made more robust to better parse the file. It only reads VRML 2.0 that MIPAV has written.
     *
     * @param      kIn       the file from which the triangle mesh is loaded
     * @param      progress  DOCUMENT ME!
     * @param      added     DOCUMENT ME!
     * @param      total     DOCUMENT ME!
     * @param      flag      DOCUMENT ME!
     *
     * @return     the loaded triangle mesh
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static TriMesh loadVRMLMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int total,
                                       boolean flag,
                                       float[] startLocation, float[] direction, float[] box
                                       ) throws IOException {

        String str = null;
        String str2 = null;
        String str3 = null;
        boolean flip = true;
        StringTokenizer stoken = null;
        try {
            //progress.setLocation(200, 200);
            //progress.setVisible(true);

            // read vertices
            if (flag) {

                if (kIn.getFilePointer() == 0) {
                    str = kIn.readLine().trim();
                    str2 = kIn.readLine().trim();
                    str3 = kIn.readLine().trim();

                    if (!str.equals("#VRML V2.0 utf8") || !str2.equals("#MIPAV") ||
                        (str3.indexOf("#Number of shapes =") == -1)) {
                        MipavUtil.displayWarning("File doesn't appear to be a VRML 2.0 file written by MIPAV");
                        Preferences.debug("ModelTriangleMesh.loadVRMLMesh: File doesn't appear to be a VRML 2.0 file written by MIPAV.\n");

                        // or throw error.
                        return null;
                    }

                    // read flip line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#flip")) {
                        stoken.nextToken();

                        int flipInt = Integer.valueOf(stoken.nextToken()).intValue();

                        if (flipInt == 1) {
                            flip = true;
                        } else {
                            flip = false;
                        }
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
                kMaterial.Emissive.SetData( red, green, blue );

                str = kIn.readLine().trim(); // diffuse color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                red = Float.valueOf( stoken.nextToken() ).floatValue();
                green = Float.valueOf( stoken.nextToken() ).floatValue();
                blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.Diffuse.SetData( red, green, blue );

                str = kIn.readLine().trim(); // specular Color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                red = Float.valueOf( stoken.nextToken() ).floatValue();
                green = Float.valueOf( stoken.nextToken() ).floatValue();
                blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.Specular.SetData( red, green, blue );

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
            ArrayList<Integer> connArray = new ArrayList(1000);

            boolean readMore = true;
            Vector3f fPt;

            while (readMore) {

                str = kIn.readLine().trim();

                if (str.equals("]")) {
                    break;
                }

                stoken = new StringTokenizer(str);
                fPt = new Vector3f();
                fPt.SetData( Float.valueOf(stoken.nextToken()).floatValue(),
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
                Vector3f kVertex = (Vector3f) (vertexPts.get(i));
                
                kVBuffer.SetPosition3(i, kVertex);      
                kVBuffer.SetColor4( 0, i,
                                    kMaterial.Diffuse.R(),
                                    kMaterial.Diffuse.G(),
                                    kMaterial.Diffuse.B(), 1.0f - transparency );
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
                aiConnect[i] = ((Integer) (connArray.get(i))).intValue();
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
     * 
     * @param kIn
     * @param progress
     * @param added
     * @param total
     * @param flag
     * @return
     * @throws IOException
     */
    public static TriMesh loadVTKLegacyMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int total,
                                            boolean flag, String fileName) throws IOException {
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
			
//             kMesh=new ModelTriangleMesh(points,indices);
//             kMesh.setVertexData(dat);
//             kMesh.SetName(fileName);
            
        } else { 
//             kMesh=new ModelTriangleMesh(points,indices);
//             kMesh.setName(fileName);
            
        }
        IndexBuffer kIBuffer = new IndexBuffer( indices.length, indices );
        kMesh = new TriMesh( kVBuffer, kIBuffer );
    	return kMesh;
    }

    /**
     * Saves a single level of detail to a mesh file. Opens a file dialog to get the output file name from the user.
     *
     * @param  kImage  ModelImage displayed in the SurfaceRender object
     * @param  isSur   true if .sur file, otherwise .wrl file
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
     */
    private static void saveSingleMesh(String name, ModelImage kImage, boolean isSur, TriMesh kMesh )
    {
        if (name != null) {

            try {
                float[] startLocation = kImage.getFileInfo(0).getOrigin();
                float[] resolution = kImage.getFileInfo(0).getResolutions();
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
                            TransMatrix dicomMatrix = (TransMatrix) (kImage.getMatrix().clone());
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
                    double[][] inverseDicomArray = null;

                    if (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
                        TransMatrix inverseDicomMatrix = (TransMatrix) (kImage.getMatrix().clone());
                        inverseDicomMatrix.invert();
                        inverseDicomArray = inverseDicomMatrix.getMatrix();
                    }

                    save(name, kMesh, kVBuffer, true, direction, startLocation, box, inverseDicomArray);
                } else {
                    saveAsVRML(name, kMesh, kVBuffer, true, direction, startLocation, box );
                }
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
    }


    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format (text format).
     *
     * @param      kName          the name of file to which the triangle mesh is saved
     * @param      akComponent    DOCUMENT ME!
     * @param      flip           if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                            To have proper orientations in surface file if flip is true flip y and z on reading.
     *                            param direction 1 or -1 for each axis param startLocation param box (dim-1)*resolution
     * @param      direction      DOCUMENT ME!
     * @param      startLocation  DOCUMENT ME!
     * @param      box            DOCUMENT ME!
     * @param      color          DOCUMENT ME!
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public static void saveAsVRML(String kName, TriMesh kMesh, VertexBuffer kVBuffer, boolean flip, int[] direction,
                                  float[] startLocation, float[] box) 
        throws IOException
    {

//         if (akComponent.length == 0) {
//             return;
//         }

        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println("#VRML V2.0 utf8"); // object is ModelTriangleMesh
        kOut.println("#MIPAV");
        //kOut.println("#Number of shapes = " + akComponent.length);
        kOut.println("#Number of shapes = " + 1);
//         for (int i = 0; i < akComponent.length; i++) {
            saveAsVRML(kOut, kMesh, kVBuffer, flip, direction, startLocation, box);
//         }

        kOut.close();
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
        throws IOException
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
        kOut.print( kMaterial.Emissive.R() + " ");
        kOut.print( kMaterial.Emissive.G() + " ");
        kOut.print( kMaterial.Emissive.B() );
        kOut.print("\n\t\t\tdiffuseColor\t");
        kOut.print( kMaterial.Diffuse.R() + " ");
        kOut.print( kMaterial.Diffuse.G() + " ");
        kOut.print( kMaterial.Diffuse.B() );
        kOut.print("\n\t\t\tspecularColor\t");
        kOut.print( kMaterial.Specular.R() + " ");
        kOut.print( kMaterial.Specular.G() + " ");
        kOut.print( kMaterial.Specular.B() );

        float fTransparency = 1.0f - kVBuffer.GetColor4(0, 0).A();
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
            kOut.print(kPos.X());
            kOut.print(' ');

            kOut.print(kPos.Y());
            kOut.print(' ');

            kOut.print(kPos.Z());

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
     * Internal support for 'void save (String)' and 'void save (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to write vertices, normals, and connectivity indices to the file. ModelClodMesh overrides this to
     * additionally write collapse records to the file
     *
     * @param      kOut               the file to which the triangle mesh is saved
     * @param      flip               if the y and z axes should be flipped - true in extraction algorithms and in
     *                                JDialogSurface. To have proper orientations in surface file if flip is true flip y
     *                                and z on reading.
     * @param      direction          either 1 or -1 for each axis
     * @param      startLocation      DOCUMENT ME!
     * @param      box                (dim-1)*res
     * @param      inverseDicomArray  DOCUMENT ME!
     * @param 	   perVertexColorArray   color per vertex array.
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected static void save(String kName, TriMesh kMesh, VertexBuffer kVBuffer, boolean flip, int[] direction, float[] startLocation, float[] box,
                        double[][] inverseDicomArray)
        throws IOException
    {
        RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");
        kOut.writeInt(0); // objects are ModelTriangleMesh
        kOut.writeInt(1);
        
        if (inverseDicomArray == null) {
            
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
        int i, index, tmpInt;
        int j;
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

        if (inverseDicomArray != null) {
            buffer = new byte[128];
            index = 0;

            for (i = 0; i <= 3; i++) {

                for (j = 0; j <= 3; j++) {
                    tmpLong = Double.doubleToLongBits(inverseDicomArray[i][j]);
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
        for (i = 0, index = 0; i < kVBuffer.GetVertexQuantity(); i++) {
            kVBuffer.GetPosition3( i, kPos );

            tmpInt = Float.floatToIntBits(kPos.X());
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kPos.Y());
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kPos.Z());
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

        }

        // write normals
        Vector3f kNormal = new Vector3f();
        for (i = 0; i < kMesh.VBuffer.GetVertexQuantity(); i++) {
            kMesh.VBuffer.GetPosition3( i, kNormal );

            tmpInt = Float.floatToIntBits(kNormal.X());
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kNormal.Y());
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kNormal.Z());
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
        for (i = 0, index = 0; i < kMesh.IBuffer.GetIndexQuantity(); i++) {
            tmpInt = aiIndex[i];
            bufferInt[index++] = (byte) (tmpInt >>> 24);
            bufferInt[index++] = (byte) (tmpInt >>> 16);
            bufferInt[index++] = (byte) (tmpInt >>> 8);
            bufferInt[index++] = (byte) (tmpInt & 0xff);

        }

        kOut.write(bufferInt);

        kOut.writeInt(1);
        byte[] bufferColor = new byte[kMesh.VBuffer.GetVertexQuantity() * 4 * 4];
        ColorRGBA kColor = new ColorRGBA();
        int R, G, B ,A;
        for (i = 0, index = 0; i < kMesh.VBuffer.GetVertexQuantity(); i++) {
            kMesh.VBuffer.GetColor4( 0, i, kColor );
        	
            R = Float.floatToIntBits(kColor.R());
            bufferColor[index++] = (byte) (R >>> 24);
            bufferColor[index++] = (byte) (R >>> 16);
            bufferColor[index++] = (byte) (R >>> 8);
            bufferColor[index++] = (byte) (R & 0xff);
            
             G = Float.floatToIntBits(kColor.G());
             bufferColor[index++] = (byte) (G >>> 24);
             bufferColor[index++] = (byte) (G >>> 16);
             bufferColor[index++] = (byte) (G >>> 8);
             bufferColor[index++] = (byte) (G & 0xff);

             B = Float.floatToIntBits(kColor.B());
             bufferColor[index++] = (byte) (B >>> 24);
             bufferColor[index++] = (byte) (B >>> 16);
             bufferColor[index++] = (byte) (B >>> 8);
             bufferColor[index++] = (byte) (B & 0xff);
             
             A = Float.floatToIntBits(kColor.A());
             bufferColor[index++] = (byte) (A >>> 24);
             bufferColor[index++] = (byte) (A >>> 16);
             bufferColor[index++] = (byte) (A >>> 8);
             bufferColor[index++] = (byte) (A & 0xff);
        	
        }
        kOut.write(bufferColor);
    }

    public static TriMesh loadVTKXMLMesh(String absPath, String fileName, String dir)
        throws IOException
    {
    	TriMesh kMesh = null;
        
        FileSurfaceVTKXML surfaceVTKXML = new FileSurfaceVTKXML(fileName, dir);
    	kMesh = surfaceVTKXML.readXMLSurface_WM(absPath);

        return kMesh;
    }

    /**
     * Writes a ModelTriangleMesh and Material to disk in the xml format, based on surface.xsd.
     *
     * @param  kMesh    ModelTriangleMesh surface mesh
     * @param  surface  Material material reference.
     */
    private static void writeTriangleMeshXML(ModelImage kImage, TriMesh kMesh)
    {

        // Dialog: Prompt the user to select the filename:
        String name = getFileName(false);
        String surfaceName = null;

        if (name == null) {
            return;
        }

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
            FileSurfaceRefXML kSurfaceXML = new FileSurfaceRefXML(null, null);
            MaterialState kMaterial = (MaterialState)kMesh.GetGlobalState(GlobalState.StateType.MATERIAL);
            float fOpacity = 1.0f - kMesh.VBuffer.GetColor4(0, 0).A();
            int iLOD = 0;
            kSurfaceXML.writeXMLsurface_WM(name, kMaterial, fOpacity, iLOD );
            saveSingleMesh( surfaceName, kImage, true, kMesh );
        } catch ( IOException kError ) { }
    }


}
