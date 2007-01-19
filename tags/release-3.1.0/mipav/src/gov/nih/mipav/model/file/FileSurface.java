package gov.nih.mipav.model.file;


import gov.nih.mipav.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.surfaceview.*;

import java.io.*;

import java.util.*;
import javax.media.j3d.*;
import javax.vecmath.*;
import javax.swing.*;


/**
 * FileSurface. Reads and writes surface files for the JPanelSurface class.
 * When surface files are loaded by the user in the JPanelSurface.java class
 * through the "Add" button, when surfaces are loaded as surfaces attached to
 * .xml image files, or surfaces loaded through the FlyThruRender class. Any
 * time a surface file is read from disk for display in the JPanelSurface
 * (SurfaceRender) class the FileSurface.java class is used to provide the
 * interface. Loaded surfaces are returned in an array of SurfaceAttributes[]
 * which are then used to add the surfaces to the SurfaceRender scene graph.
 * <p>
 * This class also handles saving files from the JPanelSurface class. Surfaces
 * are saved as surface files (.sur), single-level (.wrl), multi-level (.wrl)
 * or XML surfaces (.xml).
 * 
 *
 * @see JPanelSurface.java
 * @see SurfaceRender.java
 * @see SurfaceAttributes.java
 * @see FileSurfaceXML.java
 * @see FileInfoSurfaceXML.java
 * @see ModelTriangleMesh.java
 */
public class FileSurface
{
    /** Default Constructor: */
    public FileSurface() {}

    /**
     * The action taken when the Add button is clicked in the JPanelSurface
     * class. A file dialog is launched that allows the user to select new
     * surfaces to load from disk.
     *
     * @param imageA, the ModelImage displayed in the SurfaceRender class
     * @param listSize, the current number of triangle-mesh surfaces displayed
     * in the SurfaceRender class (for calculating the surface color)
     * @return SurfaceAttributes[] an array of surfaces described by their
     * SurfaceAttributes, used to add the surfaces to a scene graph.
     */
    public SurfaceAttributes[] openSurfaces( ModelImage imageA, int listSize )
    {
        File[] files = null;
        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();

        chooser.setMultiSelectionEnabled(true);
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal = chooser.showOpenDialog(null);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            files = chooser.getSelectedFiles();
        } else {
            return null;
        }

        ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                             File.separatorChar);
        SurfaceAttributes[] kSurface = new SurfaceAttributes[ files.length ];
        for (int i = 0; i < files.length; i++) {
            String kName = files[i].getName();

            int index = listSize + i;
            Color4f color = JPanelSurface.getNewSurfaceColor( index );

            // add the surface to the scene graph
            System.err.println(kName);

            if ((kName.indexOf(".sur") != -1) || (kName.indexOf(".wrl") != -1)) {
                kSurface[i] = addSurface( imageA, files[i], color, kName, 0.5f, -1, true);
            }
            /* Read the xml file and add to the scene graph: */
            else if (kName.indexOf(".xml") != -1) {
                FileSurfaceXML kSurfaceXML = new FileSurfaceXML(ViewUserInterface.getReference(), kName, files[i].getParent());
                try {
                    FileInfoSurfaceXML kFileInfo =  kSurfaceXML.readSurfaceXML(kName, files[i].getParent());
                    kSurface[i] = new SurfaceAttributes( kFileInfo.getMesh(), files[i].getPath(), kName, 0, 0f, 0f, new Point3f( 0, 0, 0 ) );
                    kSurface[i].setMaterial( kFileInfo.getMaterial() );
                    kSurface[i].setOpacity( kFileInfo.getOpacity() );
                    kSurface[i].setLevelDetail( kFileInfo.getLevelDetail() );
                } catch ( IOException e ) {
                    kSurface[i] = null;
                }
            }
        }
        return kSurface;
    }

    /**
     * The action taken when the one of the save surface buttons is pressed in
     * the JPanelSurface class. A file dialog is launched that allows the user
     * to select where to save the surfaces.
     *
     * @param imageA, the ModelImage displayed in the SurfaceRender class
     * @param surfaces an array of surfaces described by their
     * SurfaceAttributes, containing information that is saved with the ModelTriangleMesh
     * @param command, the type of save operation to perform
     */
    public void saveSurfaces( ModelImage imageA, SurfaceAttributes[] surfaces, String command )
    {
        if ( surfaces.length == 0 )
        {
            MipavUtil.displayError("Select a surface to save.");
            return;
        }

        if (command.equals("LevelS") || command.equals("LevelV")) {
            for ( int i = 0; i < surfaces.length; i++ )
            {
                ModelTriangleMesh[] meshes = surfaces[i].getMesh();
                Color4f color = surfaces[i].getColor();
                saveSingleMesh( imageA, meshes, command.equals("LevelS"), color);
            }
        } 
        else if (command.equals("LevelW")) {
            String name = getFileName(false);
            if (name == null) {
                return;
            }

            int i = name.lastIndexOf('.');
            if ((i > 0) && (i < (name.length() - 1))) {
                String extension = name.substring(i + 1).toLowerCase();
                if (!extension.equals("wrl")) {
                    MipavUtil.displayError("Extension must be .wrl");
                    return;
                }
            } else {
                name = name + ".wrl";
            }

            try {
                PrintWriter kOut = new PrintWriter(new FileWriter(name));

                kOut.println("#VRML V2.0 utf8");
                kOut.println("#MIPAV");
                kOut.println("#Number of shapes = " + surfaces.length);

                kOut.print("#flip { ");

                if (true) {
                    kOut.print(1);
                } else {
                    kOut.print(0);
                }

                kOut.print(" }\n");

                float[] startLocation = imageA.getFileInfo(0).getOrigin();
                float[] resolution = imageA.getFileInfo(0).getResolutions();
                int[] extents = imageA.getExtents();
                float[] box = new float[3];
                box[0] = extents[0] * resolution[0];
                box[1] = extents[1] * resolution[1];
                box[2] = extents[2] * resolution[2];

                int[] direction = MipavCoordinateSystems.getModelDirections( imageA );

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

                for (i = 0; i < surfaces.length; i++) {
                    BranchGroup surfaceBG = surfaces[i].getBranch();
                    ModelTriangleMesh[] meshes = new ModelTriangleMesh[surfaceBG.numChildren()];
                    for ( int j = 0; j < surfaceBG.numChildren(); j++ )
                    {
                        Shape3D shape = (Shape3D) surfaceBG.getChild(j);
                        meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
                    }
                    Color3f color = new Color3f();

                    ((Shape3D) (surfaceBG.getChild(0))).getAppearance().getMaterial().getDiffuseColor(color);
                    savePortableMesh( imageA, meshes, kOut, color );
                }
                kOut.close();
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
        else if (command.equals("LevelXML")) {
            for ( int i = 0; i < surfaces.length; i++ )
            {
                ModelTriangleMesh[] kMesh = surfaces[i].getMesh();
                writeTriangleMeshXML(kMesh, surfaces[i]);
            }
        }
    }

    /**
     * Calls a dialog to get a file name.
     *
     * @param   load  if <code>true</code>, make it a load dialog.
     *
     * @return  File name.
     */
    private String getFileName(boolean load) {
        String name;

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();

        chooser.setMultiSelectionEnabled(false);
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal;

        if (load) {
            returnVal = chooser.showOpenDialog(null);
        } else {
            returnVal = chooser.showSaveDialog(null);
        }

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            name = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar +
                   chooser.getSelectedFile().getName();
        } else {
            return null;
        }

        ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                             File.separatorChar);

        return name;
    }


    /**
     * Load a triangle mesh from the specified file and assign to it the specified color. A Shape3D object is created
     * whose geometry is the triangle mesh and whose appearance contains a material. The material's diffuse and specular
     * colors are set to the specified color. A BranchGroup node is created as the parent of the Shape3D object. The
     * parent node is necessary to allow dynamic modification of a compiled scene graph.
     *
     * <p>Create outVoxels and outerVoxels byte arrays with sizes equal to the size of the volume and all the values
     * initialized to zero. The surface is designed to go thru the voxel center so that the target points will be
     * enclosed. Calculate the surface point in standard voxel space that goes from 0 to xDim - 1, 0 to yDim - 1, and 0
     * to zDim - 1. Find the floor and ceilings for these x,y, and z values - this gives a total of 8 voxels. Set these
     * 8 voxels to 1 in the outVoxels array. The verticies for a triangle in this standard voxel space may be too far
     * apart and crate gaps. To prevent gaps from occurring recursively divide a triangle into 4 subtriangles if any 2
     * of the verticies are a distance >= 1 apart. In creating 4 subtriangles the midpoints of the original triangle
     * segments are added to the outVoxels array. Set all edge voxels of outVoxels that do not equal 1 to 2. Recursively
     * set all zero valued outVoxels voxels that have a 6-connected neighbor with a value of 2 to 2. Then, in outVoxels
     * all voxels outside the surface will have a value of 2, all voxels serving as floors or ceilings to the surface
     * points will have a value of 1, and all voxels inside the surface will have a value of 0. For all the 1 values in
     * outVoxels that represent a rounding toward the inside of the surface, set the corresponding values in outerVoxels
     * to 1.</p>
     *
     * @param  imageA     ModelImage displayed in the SurfaceRender class
     * @param  file       The triangle mesh file to load.
     * @param  color      The diffuse and specular color for the surface material.
     * @param  name       file name
     * @param  opacity    opacity value
     * @param  idx        surface voxel index
     * @param  isVisible  The progress bar is visible or not.
     */
    public SurfaceAttributes addSurface( ModelImage imageA, File file,
                                         Color4f color, String name, float opacity,
                                         int idx, boolean isVisible)
    {

        // open the file containing one or more meshes
        RandomAccessFile in;
        int iType, iQuantity;
        int numTriangles = 0;
        float volume = 0;
        float area = 0;
        boolean isSur = true;
        int[] direction;
        float[] startLocation;
        Point3f[] akVertex;
        Point3f center = new Point3f();
        int[] aiConnect;
        Point3f[][] akTriangle;
        //ModelImage imageA = parentScene.getImageA();
        int[] extents = imageA.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        Point3f s1 = new Point3f();
        Point3f s2 = new Point3f();
        Point3f s3 = new Point3f();
        float sx1, sy1, sz1, sx2, sy2, sz2, sx3, sy3, sz3;

        float[] resols = imageA.getFileInfo()[0].getResolutions();
        float xBox = (xDim - 1) * resols[0];
        float yBox = (yDim - 1) * resols[1];
        float zBox = (zDim - 1) * resols[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));
        int i, j, k;
        int iV1, iV2, iV3;
        float d1, d2, d3;

        if (file.getName().endsWith("sur")) {

            try {
                in = new RandomAccessFile(file, "r");
                iType = in.readInt();
                iQuantity = in.readInt();

                isSur = true;
            } catch (IOException e) {
                return null;
            }
        } else {

            try {
                in = new RandomAccessFile(file, "r");
                iType = 0;
                iQuantity = ModelTriangleMesh.parseVRMLMesh(in);
                in.seek(0);
                isSur = false;
            } catch (NoSuchElementException e) {
                MipavUtil.displayError("Only load VRML file specifically written by MIPAV!");

                return null;
            } catch (IOException e) {
                return null;
            }
        }

        ModelTriangleMesh[] akComponent = new ModelTriangleMesh[iQuantity];
        ViewJProgressBar progress = new ViewJProgressBar("Loading surface", "Loading surface", 0, 100, false, null,
                                                         null);
        progress.setVisible(isVisible);

        try {

            if (iType == 0) {

                // meshes are type TriangleMesh
                for (i = 0; i < iQuantity; i++) {

                    if (isSur == true) {
                        akComponent[i] = ModelTriangleMesh.loadTMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                                     true);
                    } else {

                        if (i == 0) {
                            akComponent[i] = ModelTriangleMesh.loadVRMLMesh(in, progress, i * 100 / iQuantity,
                                                                            iQuantity, true);
                        } else if (i > 0) {
                            akComponent[i] = ModelTriangleMesh.loadVRMLMesh(in, progress, i * 100 / iQuantity,
                                                                            iQuantity, false);
                        }
                    }

                    if (akComponent[i] == null) {
                        MipavUtil.displayError("Error while reading in triangle mesh.");

                        return null;
                    }

                    direction = ModelTriangleMesh.getDirection();
                    int[] imageDirection = MipavCoordinateSystems.getModelDirections( imageA );
                    if ( (direction[0] != imageDirection[0]) ||
                         (direction[1] != imageDirection[1]) ||
                         (direction[2] != imageDirection[2])   )
                    {
                        MipavUtil.displayError( "ModelTriangleMesh surface orientation does not match ModelImage orientation" );
                    }
                    
                    startLocation = ModelTriangleMesh.getStartLocation();
                    akVertex = akComponent[i].getVertexCopy();
                    aiConnect = akComponent[i].getIndexCopy();
                    akTriangle = new Point3f[aiConnect.length / 3][3];
                    for (j = 0; j < (aiConnect.length / 3); j++) {

                        for (k = 0; k < 3; k++) {
                            akTriangle[j][k] = new Point3f();
                        }
                    }

                    for (j = 0; j < aiConnect.length;) {
                        iV1 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV1, s1);
                        sx1 = (s1.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy1 = (s1.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz1 = (s1.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV2 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV2, s2);
                        sx2 = (s2.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy2 = (s2.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz2 = (s2.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV3 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV3, s3);
                        sx3 = (s3.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy3 = (s3.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz3 = (s3.z - startLocation[2]) / (resols[2] * direction[2]);

                        d1 = (float) Math.sqrt(((sx1 - sx2) * (sx1 - sx2)) + ((sy1 - sy2) * (sy1 - sy2)) +
                                               ((sz1 - sz2) * (sz1 - sz2)));
                        d2 = (float) Math.sqrt(((sx1 - sx3) * (sx1 - sx3)) + ((sy1 - sy3) * (sy1 - sy3)) +
                                               ((sz1 - sz3) * (sz1 - sz3)));
                        d3 = (float) Math.sqrt(((sx2 - sx3) * (sx2 - sx3)) + ((sy2 - sy3) * (sy2 - sy3)) +
                                               ((sz2 - sz3) * (sz2 - sz3)));

                        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
                            subTriangles( imageA, sx1, sy1, sz1, sx2, sy2, sz2, sx3, sy3, sz3);
                        }
                    }

                    for (j = 0; j < akVertex.length; j++) {

                        // The mesh files save the verticies as
                        // pt.x*resX*direction[0] + startLocation
                        // The loaded vertices go from -1 to 1
                        // The loaded vertex is at (2.0f*pt.x*xRes - (xDim-1)*xRes)/((dim-1)*res)max
                        akVertex[j].x = ((2.0f * (akVertex[j].x - startLocation[0]) / direction[0]) -
                                         ((xDim - 1) * resols[0])) / maxBox;
                        akVertex[j].y = ((2.0f * (akVertex[j].y - startLocation[1]) / direction[1]) -
                                         ((yDim - 1) * resols[1])) / maxBox;
                        akVertex[j].z = ((2.0f * (akVertex[j].z - startLocation[2]) / direction[2]) -
                                         ((zDim - 1) * resols[2])) / maxBox;
                    }

                    float xSum = 0f, ySum = 0f, zSum = 0f;
                    for (j = 0; j < akVertex.length; j++) {
                        xSum += akVertex[j].x;
                        ySum += akVertex[j].y;
                        zSum += akVertex[j].z;
                    }

                    center = new Point3f(xSum / akVertex.length, ySum / akVertex.length, zSum / akVertex.length);

                    // Make sure the volume is calculated when in the original file units.
                    volume += akComponent[i].volume();
                    area += akComponent[i].area();
                    akComponent[i].setVerticies(akVertex);
                    numTriangles += akComponent[i].getIndexCount();
                }
            }
            else {
                
                // meshes are type ClodMesh
                for (i = 0; i < iQuantity; i++) {
                    ModelClodMesh kClod = ModelClodMesh.loadCMesh(in, progress, i * 100 / iQuantity, iQuantity);

                    direction = ModelClodMesh.getDirection();
                    startLocation = ModelClodMesh.getStartLocation();
                    akVertex = kClod.getMesh().getVertexCopy();
                    aiConnect = kClod.getMesh().getIndexCopy();
                    akTriangle = new Point3f[aiConnect.length / 3][3];

                    for (j = 0; j < (aiConnect.length / 3); j++) {

                        for (k = 0; k < 3; k++) {
                            akTriangle[j][k] = new Point3f();
                        }
                    }

                    akComponent[i] = kClod.getMesh();
                    kClod.setLOD(kClod.getLOD() + 1);

                    for (j = 0; j < aiConnect.length;) {
                        iV1 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV1, s1);
                        sx1 = (s1.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy1 = (s1.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz1 = (s1.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV2 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV2, s2);
                        sx2 = (s2.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy2 = (s2.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz2 = (s2.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV3 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV3, s3);
                        sx3 = (s3.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy3 = (s3.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz3 = (s3.z - startLocation[2]) / (resols[2] * direction[2]);

                        d1 = (float) Math.sqrt(((sx1 - sx2) * (sx1 - sx2)) + ((sy1 - sy2) * (sy1 - sy2)) +
                                               ((sz1 - sz2) * (sz1 - sz2)));
                        d2 = (float) Math.sqrt(((sx1 - sx3) * (sx1 - sx3)) + ((sy1 - sy3) * (sy1 - sy3)) +
                                               ((sz1 - sz3) * (sz1 - sz3)));
                        d3 = (float) Math.sqrt(((sx2 - sx3) * (sx2 - sx3)) + ((sy2 - sy3) * (sy2 - sy3)) +
                                               ((sz2 - sz3) * (sz2 - sz3)));

                        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
                            subTriangles( imageA, sx1, sy1, sz1, sx2, sy2, sz2, sx3, sy3, sz3 );
                        }
                    }

                    for (j = 0; j < akVertex.length; j++) {

                        // The mesh files save the verticies as
                        // pt.x*resX*direction[0] + startLocation
                        // The loaded vertices go from -1 to 1
                        // The loaded vertex is at (2.0f*pt.x*resX - (xDim-1)*resX)/((dim-1)*res)max
                        akVertex[j].x = ((2.0f * (akVertex[j].x - startLocation[0]) / direction[0]) -
                                         ((xDim - 1) * resols[0])) / maxBox;
                        akVertex[j].y = ((2.0f * (akVertex[j].y - startLocation[1]) / direction[1]) -
                                         ((yDim - 1) * resols[1])) / maxBox;
                        akVertex[j].z = ((2.0f * (akVertex[j].z - startLocation[2]) / direction[2]) -
                                         ((zDim - 1) * resols[2])) / maxBox;
                    }

                    float xSum = 0f, ySum = 0f, zSum = 0f;

                    for (j = 0; j < akVertex.length; j++) {
                        xSum += akVertex[j].x;
                        ySum += akVertex[j].y;
                        zSum += akVertex[j].z;
                    }

                    center = new Point3f(xSum / akVertex.length, ySum / akVertex.length, zSum / akVertex.length);

                    kClod.setLOD(kClod.getMaximumLOD());
                    akComponent[i] = kClod.getMesh();

                    // Make sure the volume is calculated when in the original file units.
                    volume += akComponent[i].volume();
                    area += akComponent[i].area();
                    akComponent[i].setVerticies(akVertex);
                    kClod.setVerticies(akVertex);
                    numTriangles += akComponent[i].getIndexCount();
                }
            }
        }
        catch (IOException e) {
            return null;
        }

        progress.dispose();
        numTriangles = numTriangles / 3;
        SurfaceAttributes surface = new SurfaceAttributes( akComponent, file.getPath(), name, numTriangles, volume, area, center );
        surface.setColor( color );

        return surface;
    }

    /**
     * Saves a single level of detail to a mesh file.
     *
     * @param   imageA  ModelImage displayed in the SurfaceRender object.
     * @param   meshes  ModelTriangleMesh[] The triangle meshes that make up that level of detail surface.
     * @param   kOut    PrintWriter File output reference
     * @param   color   Color3f surface color
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void savePortableMesh( ModelImage imageA,
                                  ModelTriangleMesh[] meshes, PrintWriter kOut, Color3f color) throws IOException {
        int i;
        ModelTriangleMesh[] meshesCopy = new ModelTriangleMesh[meshes.length];

        ModelTriangleMesh meshCopy;

        if (kOut != null) {
            float[] startLocation = imageA.getFileInfo(0).getOrigin();
            float[] resolution = imageA.getFileInfo(0).getResolutions();
            int[] extents = imageA.getExtents();
            float[] box = new float[3];

            box[0] = extents[0] * resolution[0];
            box[1] = extents[1] * resolution[1];
            box[2] = extents[2] * resolution[2];

            float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
            int[] direction = MipavCoordinateSystems.getModelDirections( imageA );
            int j;

            Point3f[] akVertex;

            for (i = 0; i < meshes.length; i++) {
                meshCopy = new ModelTriangleMesh(meshes[i].getVertexCopy(), meshes[i].getNormalCopy(),
                                                 meshes[i].getIndexCopy());
                akVertex = meshCopy.getVertexCopy();

                // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                // The mesh files must save the verticies as
                // pt.x*resX*direction[0] + startLocation
                for (j = 0; j < akVertex.length; j++) {
                    akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) + startLocation[0];
                    akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) + startLocation[1];
                    akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) + startLocation[2];

                    // flip y and z
                    akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                    akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;
                }

                meshCopy.setVerticies(akVertex);
                meshesCopy[i] = meshCopy;
            }

            ModelTriangleMesh.saveAsPortableVRML(kOut, meshesCopy, true, direction, startLocation, box, color);
        }
    }


    /**
     * Saves a single level of detail to a mesh file. Opens a file dialog to
     * get the output file name from the user.
     *
     * @param imageA, ModelImage displayed in the SurfaceRender object
     * @param  meshes  The triangle meshes that make up that level of detail surface.
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param  color   surface color
     */
    public void saveSingleMesh(ModelImage imageA, ModelTriangleMesh[] meshes, boolean isSur, Color4f color) {
        String extension;
        String name = getFileName(false);

        if (name == null) {
            return;
        }

        int i = name.lastIndexOf('.');
        if ((i > 0) && (i < (name.length() - 1))) {
            extension = name.substring(i + 1).toLowerCase();

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

        saveSingleMesh( name, imageA, meshes, isSur, color );
    }

    /**
     * Saves a single level of detail to a mesh file. The file name is passed
     * as a parameter.
     *
     * @param name the file name
     * @param imageA, ModelImage displayed in the SurfaceRender object
     * @param  meshes  The triangle meshes that make up that level of detail surface.
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param  color   surface color
     */
    public void saveSingleMesh( String name, ModelImage imageA, ModelTriangleMesh[] meshes, boolean isSur, Color4f color) {
        TransMatrix dicomMatrix;
        TransMatrix inverseDicomMatrix = null;
        double[][] inverseDicomArray = null;
        float[] coord;
        float[] tCoord;
        ModelTriangleMesh[] meshesCopy = new ModelTriangleMesh[meshes.length];
        ModelTriangleMesh meshCopy;

        if (name != null) {

            try {
                float[] startLocation = imageA.getFileInfo(0).getOrigin();
                float[] resolution = imageA.getFileInfo(0).getResolutions();
                int[] extents = imageA.getExtents();
                float[] box = new float[3];

                box[0] = extents[0] * resolution[0];
                box[1] = extents[1] * resolution[1];
                box[2] = extents[2] * resolution[2];

                float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
                int[] direction = MipavCoordinateSystems.getModelDirections( imageA );
                int j;
                Point3f[] akVertex;

                for ( int i = 0; i < meshes.length; i++) {
                    meshCopy = new ModelTriangleMesh(meshes[i].getVertexCopy(), meshes[i].getNormalCopy(),
                                                     meshes[i].getIndexCopy());
                    akVertex = meshCopy.getVertexCopy();

                    // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                    // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                    // The mesh files must save the verticies as
                    // pt.x*resX*direction[0] + startLocation
                    if (isSur &&
                            (imageA.getFileInfo()[0].getTransformID() ==
                                 FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL)) {

                        // Get the DICOM transform that describes the transformation from
                        // axial to this image orientation
                        dicomMatrix = (TransMatrix) (imageA.getMatrix().clone());
                        inverseDicomMatrix = (TransMatrix) (imageA.getMatrix().clone());
                        inverseDicomMatrix.invert();
                        inverseDicomArray = inverseDicomMatrix.getMatrix();
                        inverseDicomMatrix = null;
                        coord = new float[3];
                        tCoord = new float[3];

                        for (j = 0; j < akVertex.length; j++) {
                            akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) +
                                            startLocation[0];
                            akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) +
                                            startLocation[1];
                            akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) +
                                            startLocation[2];

                            // flip y and z
                            akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                            akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;

                            // Change the voxel coordinate into millimeter space
                            coord[0] = (akVertex[j].x - startLocation[0]) / direction[0];
                            coord[1] = (akVertex[j].y - startLocation[1]) / direction[1];
                            coord[2] = (akVertex[j].z - startLocation[2]) / direction[2];

                            // Convert the point to axial millimeter DICOM space
                            dicomMatrix.transform(coord, tCoord);

                            // Add in the DICOM origin
                            tCoord[0] = tCoord[0] + startLocation[0];
                            tCoord[1] = tCoord[1] + startLocation[1];
                            tCoord[2] = tCoord[2] + startLocation[2];
                            akVertex[j] = new Point3f(tCoord[0], tCoord[1], tCoord[2]);
                        }
                    } else {

                        for (j = 0; j < akVertex.length; j++) {
                            akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) +
                                            startLocation[0];
                            akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) +
                                            startLocation[1];
                            akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) +
                                            startLocation[2];

                            // flip y and z
                            akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                            akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;
                        }
                    }

                    meshCopy.setVerticies(akVertex);
                    meshesCopy[i] = meshCopy;
                }

                if (isSur == true) {
                    ModelTriangleMesh.save(name, meshesCopy, true, direction, startLocation, box, inverseDicomArray);
                } else {
                    ModelTriangleMesh.saveAsVRML(name, meshesCopy, true, direction, startLocation, box,
                                                 new Color3f( color.x, color.y, color.z ) );
                }
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
    }

    /**
     * Writes a ModelTriangleMesh and Material to disk in the xml format,
     * based on surface.xsd.
     *
     * @param  kMesh      ModelTriangleMesh surface mesh
     * @param  kMaterial  Material material reference.
     */
    private void writeTriangleMeshXML(ModelTriangleMesh[] kMesh, SurfaceAttributes surface) {

        // Dialog: Prompt the user to select the filename:
        String name = getFileName(false);

        if (name == null) {
            return;
        }

        // Check the filename extension:
        int i = name.lastIndexOf('.');

        if ((i > 0) && (i < (name.length() - 1))) {
            String extension = name.substring(i + 1).toLowerCase();

            if (!extension.equals("xml")) {
                MipavUtil.displayError("Extension must be .xml");

                return;
            }
        } else {
            name = name + ".xml";
        }

        i = name.lastIndexOf(File.separator);

        String dir = name.substring(0, i + 1);
        name = name.substring(i + 1);

        // Create the FileSurfaceXML to write the mesh:
        FileSurfaceXML kSurfaceXML = new FileSurfaceXML(ViewUserInterface.getReference(), null, null);

        try {
            kSurfaceXML.writeHeader(name, dir, kMesh,
                                    surface.getMaterial(), surface.getOpacity(), surface.getLevelDetail());
        } catch (IOException kError) { }
    }

    /**
     * Subdivide the triangle.
     *
     * @param imageA the ModelImage displayed in the SurfaceRender object
     * @param  sx1  float
     * @param  sy1  float
     * @param  sz1  float
     * @param  sx2  float
     * @param  sy2  float
     * @param  sz2  float
     * @param  sx3  float
     * @param  sy3  float
     * @param  sz3  float
     */
    private void subTriangles( ModelImage imageA,
                               float sx1, float sy1, float sz1,
                               float sx2, float sy2, float sz2,
                               float sx3, float sy3, float sz3)
    {
        int[] extents = imageA.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];
        float sx4, sy4, sz4;
        float sx5, sy5, sz5;
        float sx6, sy6, sz6;
        int sxf4, syf4, szf4;
        int sxf5, syf5, szf5;
        int sxf6, syf6, szf6;
        float d1, d2, d3;

        sx4 = (sx1 + sx3) / 2.0f;
        sy4 = (sy1 + sy3) / 2.0f;
        sz4 = (sz1 + sz3) / 2.0f;
        sx5 = (sx1 + sx2) / 2.0f;
        sy5 = (sy1 + sy2) / 2.0f;
        sz5 = (sz1 + sz2) / 2.0f;
        sx6 = (sx2 + sx3) / 2.0f;
        sy6 = (sy2 + sy3) / 2.0f;
        sz6 = (sz2 + sz3) / 2.0f;

        sxf4 = Math.max(0, (int) Math.floor(sx4));
        syf4 = Math.max(0, (int) Math.floor(sy4));
        szf4 = Math.max(0, (int) Math.floor(sz4));
        sxf4 = Math.min(xDim - 1, sxf4);
        syf4 = Math.min(yDim - 1, syf4);
        szf4 = Math.min(zDim - 1, szf4);

        sxf5 = Math.max(0, (int) Math.floor(sx5));
        syf5 = Math.max(0, (int) Math.floor(sy5));
        szf5 = Math.max(0, (int) Math.floor(sz5));
        sxf5 = Math.min(xDim - 1, sxf5);
        syf5 = Math.min(yDim - 1, syf5);
        szf5 = Math.min(zDim - 1, szf5);

        sxf6 = Math.max(0, (int) Math.floor(sx6));
        syf6 = Math.max(0, (int) Math.floor(sy6));
        szf6 = Math.max(0, (int) Math.floor(sz6));
        sxf6 = Math.min(xDim - 1, sxf6);
        syf6 = Math.min(yDim - 1, syf6);
        szf6 = Math.min(zDim - 1, szf6);

        d1 = (float) Math.sqrt(((sx1 - sx4) * (sx1 - sx4)) + ((sy1 - sy4) * (sy1 - sy4)) + ((sz1 - sz4) * (sz1 - sz4)));
        d2 = (float) Math.sqrt(((sx1 - sx5) * (sx1 - sx5)) + ((sy1 - sy5) * (sy1 - sy5)) + ((sz1 - sz5) * (sz1 - sz5)));
        d3 = (float) Math.sqrt(((sx4 - sx5) * (sx4 - sx5)) + ((sy4 - sy5) * (sy4 - sy5)) + ((sz4 - sz5) * (sz4 - sz5)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles( imageA, sx1, sy1, sz1, sx4, sy4, sz4, sx5, sy5, sz5);
        }

        d1 = (float) Math.sqrt(((sx4 - sx5) * (sx4 - sx5)) + ((sy4 - sy5) * (sy4 - sy5)) + ((sz4 - sz5) * (sz4 - sz5)));
        d2 = (float) Math.sqrt(((sx4 - sx6) * (sx4 - sx6)) + ((sy4 - sy6) * (sy4 - sy6)) + ((sz4 - sz6) * (sz4 - sz6)));
        d3 = (float) Math.sqrt(((sx5 - sx6) * (sx5 - sx6)) + ((sy5 - sy6) * (sy5 - sy6)) + ((sz5 - sz6) * (sz5 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles( imageA, sx4, sy4, sz4, sx5, sy5, sz5, sx6, sy6, sz6);
        }

        d1 = (float) Math.sqrt(((sx3 - sx4) * (sx3 - sx4)) + ((sy3 - sy4) * (sy3 - sy4)) + ((sz3 - sz4) * (sz3 - sz4)));
        d2 = (float) Math.sqrt(((sx3 - sx6) * (sx3 - sx6)) + ((sy3 - sy6) * (sy3 - sy6)) + ((sz3 - sz6) * (sz3 - sz6)));
        d3 = (float) Math.sqrt(((sx4 - sx6) * (sx4 - sx6)) + ((sy4 - sy6) * (sy4 - sy6)) + ((sz4 - sz6) * (sz4 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles( imageA, sx3, sy3, sz3, sx4, sy4, sz4, sx6, sy6, sz6);
        }

        d1 = (float) Math.sqrt(((sx2 - sx5) * (sx2 - sx5)) + ((sy2 - sy5) * (sy2 - sy5)) + ((sz2 - sz5) * (sz2 - sz5)));
        d2 = (float) Math.sqrt(((sx2 - sx6) * (sx2 - sx6)) + ((sy2 - sy6) * (sy2 - sy6)) + ((sz2 - sz6) * (sz2 - sz6)));
        d3 = (float) Math.sqrt(((sx5 - sx6) * (sx5 - sx6)) + ((sy5 - sy6) * (sy5 - sy6)) + ((sz5 - sz6) * (sz5 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles( imageA, sx2, sy2, sz2, sx5, sy5, sz5, sx6, sy6, sz6);
        }
    }


}
