package gov.nih.mipav.view.renderer.J3D.model.file;


import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import java.io.*;

import java.util.*;

import javax.swing.*;

import javax.vecmath.*;


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
public class FileSurface_J3D {

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
    public static SurfaceAttributes[] openSurfaces(ModelImage kImage, int iListSize) {
        File[] akFiles = openFiles(true);

        if (akFiles == null) {
            return null;
        }

        SurfaceAttributes[] kSurface = new SurfaceAttributes[akFiles.length];

        for (int i = 0; i < akFiles.length; i++) {
            String kName = akFiles[i].getName();
            
            Color4f kColor = JPanelSurface.getNewSurfaceColor(iListSize + i);

            if ((kName.indexOf(".sur") != -1) || (kName.indexOf(".wrl") != -1) || (kName.indexOf(".vtk") != -1) || (kName.indexOf(".vtp") != -1)) {
                kSurface[i] = readSurface(kImage, akFiles[i], kColor);
            } else if (kName.indexOf(".xml") != -1) {
            	/*
                FileSurfaceXML kSurfaceXML = new FileSurfaceXML(kName, akFiles[i].getParent());

                try {
                    FileInfoSurfaceXML kFileInfo = kSurfaceXML.readSurfaceXML(kName, akFiles[i].getParent());
                    kSurface[i] = new SurfaceAttributes(kFileInfo.getMesh(), akFiles[i].getPath(), kName);
                    kSurface[i].setOpacity(kFileInfo.getOpacity());
                    kSurface[i].setMaterial(kFileInfo.getMaterial());
                    kSurface[i].setLevelDetail(kFileInfo.getLevelDetail());
                   
                } catch (IOException e) {
                    kSurface[i] = null;
                }
         
                */
	             FileSurfaceRefXML_J3D kSurfaceXML = new FileSurfaceRefXML_J3D(kName, akFiles[i].getParent());
	           	 try {
	                    FileInfoSurfaceRefXML_J3D kFileInfo = kSurfaceXML.readSurfaceXML(kName, akFiles[i].getParent());
	                    // System.err.println("afa" + (akFiles[i].getParent()+ File.separatorChar + kFileInfo.getSurfaceFileName()));
	                    akFiles[i] = new File(akFiles[i].getParent()+ File.separatorChar + kFileInfo.getSurfaceFileName());
	                    kSurface[i] = readSurface(kImage, akFiles[i], kColor);
	                    kSurface[i].setOpacity(kFileInfo.getOpacity());
	                    kSurface[i].setMaterial(kFileInfo.getMaterial());
	                    kSurface[i].setLevelDetail(kFileInfo.getLevelDetail());
	                   
	                    if ( kSurface[i].getPerVertexColorArray() != null ) {
	                       kSurface[i].setPerVertexColorArray(kSurface[i].getPerVertexColorArray()[0], 0);
	                    }
	                    
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
    public static SurfaceAttributes readSurface(ModelImage kImage, File file, Color4f color) {

        int iType = 0, iQuantity = 0;
        boolean isSur = true;
        int[] direction;
        float[] startLocation;
        Point3f[] akVertex;
        int[] aiConnect;
        Point3f[][] akTriangle;
        int[] extents = kImage.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        float[] resols = kImage.getFileInfo()[0].getResolutions();
        float xBox = (xDim - 1) * resols[0];
        float yBox = (yDim - 1) * resols[1];
        float zBox = (zDim - 1) * resols[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));

        FileSurfaceRefXML_J3D kSurfaceXML;
        FileInfoSurfaceRefXML_J3D kFileInfo = null;
        RandomAccessFile in;
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
        } else if ( file.getName().endsWith("xml") ) {
        	/*
        	FileSurfaceXML kSurfaceXML = new FileSurfaceXML(file.getName(), file.getParent());
        	SurfaceAttributes surface = null;
            try {
                FileInfoSurfaceXML kFileInfo = kSurfaceXML.readSurfaceXML(file.getName(), file.getParent());
                surface = new SurfaceAttributes(kFileInfo.getMesh(), file.getPath(), file.getName());
                surface.setOpacity(kFileInfo.getOpacity());
                surface.setMaterial(kFileInfo.getMaterial());
                surface.setLevelDetail(kFileInfo.getLevelDetail());
               
            } catch (IOException e) {
            	surface = null;
            }
            return surface;
        	*/
        	 isXMLSurface = true;
        	 kSurfaceXML = new FileSurfaceRefXML_J3D(file.getName(), file.getParent());
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
        ModelTriangleMesh[] akComponent = new ModelTriangleMesh[iQuantity];
        ViewJProgressBar progress = new ViewJProgressBar("Loading surface", "Loading surface", 0, 100, false, null,
                                                         null);
        progress.setVisible(true);

        try {

            // meshes are type TriangleMesh
            for (int i = 0; i < iQuantity; i++) {

                if (iType == 0) {

                    if (isSur == true) {
                        akComponent[i] = ModelTriangleMesh.loadTMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                                     true);
                    } else {
                    	if ( file.getName().endsWith("wrl") ) {
                    		akComponent[i] = ModelTriangleMesh.loadVRMLMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                                        (i == 0));
                    	}
                    	else if (file.getName().endsWith("vtk")){
                    		//vtk legacy
                    		akComponent[i] = ModelTriangleMesh.loadVTKLegacyMesh(in, progress, i * 100 / iQuantity, iQuantity, (i == 0), file.getName());
                    		if(akComponent[i] == null) {
                    			progress.dispose();
                    			return null;
                    		}
                    	}
                    	else if(file.getName().endsWith("vtp")) {
                    		//vtk xml
                    		akComponent[i] = ModelTriangleMesh.loadVTKXMLMesh(file.getAbsolutePath(), progress, i * 100 / iQuantity, iQuantity, (i == 0), file.getName(), file.getParent());
                    		if(akComponent[i] == null) {
                    			progress.dispose();
                    			return null;
                    		}
                    	}
                    }

                    direction = ModelTriangleMesh.getDirection();
                    startLocation = ModelTriangleMesh.getStartLocation();
                    akVertex = akComponent[i].getVertexCopy();
                    aiConnect = akComponent[i].getIndexCopy();
                    akTriangle = new Point3f[aiConnect.length / 3][3];
                } else {
                    kClod = ModelClodMesh.loadCMesh(in, progress, i * 100 / iQuantity, iQuantity);
                    akComponent[i] = kClod.getMesh();
                    kClod.setLOD(kClod.getLOD() + 1);
                    direction = ModelClodMesh.getDirection();
                    startLocation = ModelClodMesh.getStartLocation();
                    akVertex = kClod.getMesh().getVertexCopy();
                    aiConnect = kClod.getMesh().getIndexCopy();
                    akTriangle = new Point3f[aiConnect.length / 3][3];
                }

                if (akComponent[i] == null) {
                    MipavUtil.displayError("Error while reading in triangle mesh.");

                    return null;
                }

                for (int j = 0; j < (aiConnect.length / 3); j++) {

                    for (int k = 0; k < 3; k++) {
                        akTriangle[j][k] = new Point3f();
                    }
                }

                for (int j = 0; j < akVertex.length; j++) {

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

                if (iType != 0) {
                    kClod.setLOD(kClod.getMaximumLOD());
                    akComponent[i] = kClod.getMesh();
                }

                akComponent[i].setVerticies(akVertex);

                if (iType != 0) {
                    kClod.setVerticies(akVertex);
                }
            }
        } catch (IOException e) {
            return null;
        }

        progress.dispose();

        SurfaceAttributes surface = new SurfaceAttributes(akComponent, file.getPath(), file.getName());
        // surface.setColor(color);
       
        if ( isXMLSurface ) {
	        surface.setOpacity(kFileInfo.getOpacity());
	        surface.setMaterial(kFileInfo.getMaterial());
	        surface.setLevelDetail(kFileInfo.getLevelDetail());
        } else {
        	surface.setColor(color);
        	
        	for ( int i = 0; i < akComponent.length; i++ ) {
        	
	        	if ( akComponent[i].getPerVertexColor() != null ) {
	        	   surface.setPerVertexColorArray(akComponent[i].getPerVertexColor(), i);
	        	}
        	}
        } 
        return surface;
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
    public static void saveSurfaces(ModelImage kImage, SurfaceAttributes[] akSurfaces, String kCommand) {

        if (akSurfaces.length == 0) {
            MipavUtil.displayError("Select a surface to save.");

            return;
        }

        if (kCommand.equals("LevelS") || kCommand.equals("LevelV")) {

            for (int i = 0; i < akSurfaces.length; i++) {
                ModelTriangleMesh[] akMeshes = akSurfaces[i].getMesh();
                Color4f kColor = akSurfaces[i].getColor4f();
                saveSingleMesh(kImage, akMeshes, kCommand.equals("LevelS"), kColor, akSurfaces[i]);
            }
        } else if (kCommand.equals("LevelW")) {
            saveMultiMesh(kImage, akSurfaces);
        } else if (kCommand.equals("LevelXML")) {

            for (int i = 0; i < akSurfaces.length; i++) {
                ModelTriangleMesh[] kMesh = akSurfaces[i].getMesh();
                Color4f kColor = akSurfaces[i].getColor4f();
                writeTriangleMeshXML(kImage, kMesh, akSurfaces[i], kColor);
                
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

    /**
     * Writes multiple surfaces to one file.
     *
     * @param  kImage    the ModelImage the surface is associated with.
     * @param  surfaces  the list of surfaces to save.
     */
    private static void saveMultiMesh(ModelImage kImage, SurfaceAttributes[] surfaces) {
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

            int[] direction = MipavCoordinateSystems.getModelDirections(kImage);
            kOut.println("#direction { " + direction[0] + " " + direction[1] + " " + direction[2] + " }");

            float[] startLocation = kImage.getFileInfo(0).getOrigin();
            kOut.println("#startLocation { " + startLocation[0] + " " + startLocation[1] + " " + startLocation[2] +
                         " }");

            float[] resolution = kImage.getFileInfo(0).getResolutions();
            int[] extents = kImage.getExtents();
            float[] box = new float[3];
            box[0] = extents[0] * resolution[0];
            box[1] = extents[1] * resolution[1];
            box[2] = extents[2] * resolution[2];
            kOut.println("#box { " + box[0] + " " + box[1] + " " + box[2] + " }");

            for (i = 0; i < surfaces.length; i++) {
                ModelTriangleMesh[] meshes = surfaces[i].getMesh();
                Color4f color = surfaces[i].getColor4f();
                savePortableMesh(kImage, meshes, kOut, new Color3f(color.x, color.y, color.z));
            }

            kOut.close();
        } catch (IOException error) {
            MipavUtil.displayError("Error while trying to save single mesh");
        }
    }

    /**
     * Saves a single level of detail to a mesh file.
     *
     * @param   kImage  ModelImage displayed in the SurfaceRender object.
     * @param   meshes  ModelTriangleMesh[] The triangle meshes that make up that level of detail surface.
     * @param   kOut    PrintWriter File output reference
     * @param   color   Color3f surface color
     *
     * @throws  IOException  IOException
     */
    private static void savePortableMesh(ModelImage kImage, ModelTriangleMesh[] meshes, PrintWriter kOut, Color3f color)
            throws IOException {

        ModelTriangleMesh[] meshesCopy = new ModelTriangleMesh[meshes.length];
        ModelTriangleMesh meshCopy;

        if (kOut != null) {
            float[] startLocation = kImage.getFileInfo(0).getOrigin();
            float[] resolution = kImage.getFileInfo(0).getResolutions();
            int[] extents = kImage.getExtents();
            float[] box = new float[3];
            box[0] = extents[0] * resolution[0];
            box[1] = extents[1] * resolution[1];
            box[2] = extents[2] * resolution[2];

            float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
            int[] direction = MipavCoordinateSystems.getModelDirections(kImage);
            Point3f[] akVertex;

            for (int i = 0; i < meshes.length; i++) {
                meshCopy = new ModelTriangleMesh(meshes[i]);
                akVertex = meshCopy.getVertexCopy();

                // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                // The mesh files must save the verticies as
                // pt.x*resX*direction[0] + startLocation
                for (int j = 0; j < akVertex.length; j++) {
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
     * Saves a single level of detail to a mesh file. Opens a file dialog to get the output file name from the user.
     *
     * @param  kImage  ModelImage displayed in the SurfaceRender object
     * @param  meshes  The triangle meshes that make up that level of detail surface.
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param  color   surface color
     */
    private static void saveSingleMesh(ModelImage kImage, ModelTriangleMesh[] meshes, boolean isSur, Color4f color, SurfaceAttributes surfacesAttribute) {
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

        saveSingleMesh(name, kImage, meshes, isSur, color, surfacesAttribute.getPerVertexColorArray());
    }

    /**
     * Saves a single level of detail to a mesh file. The file name is passed as a parameter.
     *
     * @param  name    the file name
     * @param  kImage  ModelImage displayed in the SurfaceRender object
     * @param  meshes  The triangle meshes that make up that level of detail surface.
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param  color   surface color
     * @param  perVertexColorArray    color per vertex array
     */
    private static void saveSingleMesh(String name, ModelImage kImage, ModelTriangleMesh[] meshes, boolean isSur,
                                       Color4f color, Color4f[][] perVertexColorArray) {
        ModelTriangleMesh[] meshesCopy = new ModelTriangleMesh[meshes.length];
        ModelTriangleMesh meshCopy;

        if (name != null) {

            try {
                float[] startLocation = kImage.getFileInfo(0).getOrigin();
                float[] resolution = kImage.getFileInfo(0).getResolutions();
                int[] extents = kImage.getExtents();
                float[] box = new float[3];
                box[0] = extents[0] * resolution[0];
                box[1] = extents[1] * resolution[1];
                box[2] = extents[2] * resolution[2];

                float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
                int[] direction = MipavCoordinateSystems.getModelDirections(kImage);
                Point3f[] akVertex;

                for (int i = 0; i < meshes.length; i++) {
                    meshCopy = new ModelTriangleMesh(meshes[i]);
                    akVertex = meshCopy.getVertexCopy();

                    // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                    // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                    // The mesh files must save the verticies as
                    // pt.x*resX*direction[0] + startLocation
                    for (int j = 0; j < akVertex.length; j++) {
                        akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) +
                                        startLocation[0];
                        akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) +
                                        startLocation[1];
                        akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) +
                                        startLocation[2];

                        // flip y and z
                        akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                        akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;

                        if (isSur &&
                                (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))) {

                            // Get the DICOM transform that describes the transformation from
                            // axial to this image orientation
                            TransMatrix dicomMatrix = (TransMatrix) (kImage.getMatrix().clone());
                            float[] coord = new float[3];
                            float[] tCoord = new float[3];

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
                    }

                    meshCopy.setVerticies(akVertex);
                    meshesCopy[i] = meshCopy;
                }

                if (isSur == true) {
                	TransMatrix inverseDicomMatrix = null;

                    if (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
                        inverseDicomMatrix = (TransMatrix) (kImage.getMatrix().clone());
                        inverseDicomMatrix.Inverse();
                        //inverseDicomArray = inverseDicomMatrix.getMatrix();
                    }

                    ModelTriangleMesh.save(name, meshesCopy, true, direction, startLocation, box, inverseDicomMatrix, perVertexColorArray);
                } else {
                    ModelTriangleMesh.saveAsVRML(name, meshesCopy, true, direction, startLocation, box,
                                                 new Color3f(color.x, color.y, color.z));
                }
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
    }

    /**
     * Subdivide the triangle.
     *
     * @param  kImage  the ModelImage displayed in the SurfaceRender object
     * @param  sx1     float
     * @param  sy1     float
     * @param  sz1     float
     * @param  sx2     float
     * @param  sy2     float
     * @param  sz2     float
     * @param  sx3     float
     * @param  sy3     float
     * @param  sz3     float
     */
    private static void subTriangles(ModelImage kImage, float sx1, float sy1, float sz1, float sx2, float sy2,
                                     float sz2, float sx3, float sy3, float sz3) {
        int[] extents = kImage.getExtents();
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
            subTriangles(kImage, sx1, sy1, sz1, sx4, sy4, sz4, sx5, sy5, sz5);
        }

        d1 = (float) Math.sqrt(((sx4 - sx5) * (sx4 - sx5)) + ((sy4 - sy5) * (sy4 - sy5)) + ((sz4 - sz5) * (sz4 - sz5)));
        d2 = (float) Math.sqrt(((sx4 - sx6) * (sx4 - sx6)) + ((sy4 - sy6) * (sy4 - sy6)) + ((sz4 - sz6) * (sz4 - sz6)));
        d3 = (float) Math.sqrt(((sx5 - sx6) * (sx5 - sx6)) + ((sy5 - sy6) * (sy5 - sy6)) + ((sz5 - sz6) * (sz5 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles(kImage, sx4, sy4, sz4, sx5, sy5, sz5, sx6, sy6, sz6);
        }

        d1 = (float) Math.sqrt(((sx3 - sx4) * (sx3 - sx4)) + ((sy3 - sy4) * (sy3 - sy4)) + ((sz3 - sz4) * (sz3 - sz4)));
        d2 = (float) Math.sqrt(((sx3 - sx6) * (sx3 - sx6)) + ((sy3 - sy6) * (sy3 - sy6)) + ((sz3 - sz6) * (sz3 - sz6)));
        d3 = (float) Math.sqrt(((sx4 - sx6) * (sx4 - sx6)) + ((sy4 - sy6) * (sy4 - sy6)) + ((sz4 - sz6) * (sz4 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles(kImage, sx3, sy3, sz3, sx4, sy4, sz4, sx6, sy6, sz6);
        }

        d1 = (float) Math.sqrt(((sx2 - sx5) * (sx2 - sx5)) + ((sy2 - sy5) * (sy2 - sy5)) + ((sz2 - sz5) * (sz2 - sz5)));
        d2 = (float) Math.sqrt(((sx2 - sx6) * (sx2 - sx6)) + ((sy2 - sy6) * (sy2 - sy6)) + ((sz2 - sz6) * (sz2 - sz6)));
        d3 = (float) Math.sqrt(((sx5 - sx6) * (sx5 - sx6)) + ((sy5 - sy6) * (sy5 - sy6)) + ((sz5 - sz6) * (sz5 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles(kImage, sx2, sy2, sz2, sx5, sy5, sz5, sx6, sy6, sz6);
        }
    }


    /**
     * Writes a ModelTriangleMesh and Material to disk in the xml format, based on surface.xsd.
     *
     * @param  kMesh    ModelTriangleMesh surface mesh
     * @param  surface  Material material reference.
     */
    private static void writeTriangleMeshXML(ModelImage kImage, ModelTriangleMesh[] kMesh, SurfaceAttributes surface, Color4f color) {

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
        
      /*
        i = name.lastIndexOf(File.separator);
   
        String dir = name.substring(0, i + 1);
        name = name.substring(i + 1);
   */
        /*
        // Create the FileSurfaceXML to write the mesh:
        FileSurfaceXML kSurfaceXML = new FileSurfaceXML(null, null);

        try {
            kSurfaceXML.writeHeader(name, dir, kMesh, surface.getMaterial(), surface.getOpacity(),
                                    surface.getLevelDetail());
        } catch (IOException kError) { }
        */
        try {
            FileSurfaceRefXML_J3D kSurfaceXML = new FileSurfaceRefXML_J3D(null, null);
            kSurfaceXML.writeXMLsurface(name, surface.getMaterial(), surface.getOpacity(),
                    surface.getLevelDetail());
            saveSingleMesh(surfaceName, kImage, kMesh, true, color, surface.getPerVertexColorArray());
        } catch ( IOException kError ) { }
    }
}
