package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.surfaceview.*;

import java.io.*;
import java.util.*;

import javax.swing.*;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
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
public class FilePolyline_WM {
    /** DOCUMENT ME! */
    private static int[] direction = new int[3];

    /** DOCUMENT ME! */
    private static float[] startLocation = new float[3];

    /** DOCUMENT ME! */
    private static float[] box = new float[3];
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
    public static Polyline[] openPolylines(ModelImage kImage, int iListSize) {
        File[] akFiles = openFiles(true);
        
        if (akFiles == null) {
            return null;
        }

        Polyline[] kPolyline = new Polyline[akFiles.length];
        for (int i = 0; i < akFiles.length; i++) {
            String kName = akFiles[i].getName();
            if (kName.indexOf(".xml") != -1) {	           	 
	           	Vector<Point3Df> coordVector = new Vector<Point3Df>();
            	
	           	try {
            	   FilePolylineVOIXML polylineXML = new FilePolylineVOIXML(kName, akFiles[i].getParent());
            	   coordVector = polylineXML.readVOI(false);
                   kPolyline[i] = createPolyline(kImage, coordVector);
                   kPolyline[i].SetName(kName);
               
            	 } catch (IOException e) {
            		 e.printStackTrace();
                     return null;
                 }
            } 
        }

        return kPolyline;
    }

    public static Polyline createPolyline(ModelImage kImage, Vector<Point3Df> coordVector) {

		int iType = 0, iQuantity = 0;
		ColorRGB kColor1;
		float fX, fY, fZ;
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
	
		startLocation = kImage.getFileInfo(0).getOrigin();
		direction = MipavCoordinateSystems.getModelDirections(kImage);
		
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetCChannels(0, 3);
		kAttr.SetCChannels(1, 3);
		VertexBuffer pkVBuffer = new VertexBuffer(kAttr, coordVector.size());
		kColor1 = new ColorRGB(0.5f, 0f, 0f);
		for (int i = 0; i < coordVector.size(); i++) {
			Point3Df point = (Point3Df) (coordVector.elementAt(i));

			fX = point.x;
			fY = point.y;
			fZ = point.z;

		    fX =  ((2.0f * (fX - startLocation[0]) / direction[0]) -
                     ((xDim - 1) * resols[0])) / (2.0f*maxBox);
            fY =  ((2.0f * (fY - startLocation[1]) / direction[1]) -
                  ((yDim - 1) * resols[1])) / (2.0f*maxBox);
            fZ =  ((2.0f * (fZ - startLocation[2]) / direction[2]) -
                     ((zDim - 1) * resols[2])) / (2.0f*maxBox) ;

			
			pkVBuffer.SetPosition3(i, (float) (fX - .5f), (float) (fY - .5f),
					(float) (fZ - .5f));
			pkVBuffer.SetColor3(0, i, new ColorRGB(fX, fY, fZ));
			pkVBuffer.SetColor3(1, i, kColor1);

		}
		boolean bClosed = false;
		boolean bContiguous = true;

		return new Polyline(pkVBuffer, bClosed, bContiguous);

	}
   
    /**
     * Returns an array of File objects, based on the user-selected files from the FileChooser dialog.
     *
     * @param   bLoad  whether the files are opened for reading (bLoad = true) or writing (bLoad = false)
     *
     * @return  File[] array of opened files.
     */
    private static File[] openFiles(boolean bLoad) {

        JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(bLoad);
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));

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
    
    

}
