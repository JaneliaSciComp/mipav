package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.file.FilePolylineVOIXML;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;

import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * FilePolyline. Reads and writes Polyline files for the JPanelSurface class. When polyline files are loaded by the user in
 * the JPanelSurface.java class through the "AddPolyline" button.
 *
 * @see  JPanelSurface_WM.java
 */
public class FilePolyline_WM {
    /** DOCUMENT ME! */
    private static int[] direction = new int[3];

    /** DOCUMENT ME! */
    private static float[] startLocation = new float[3];

    /** DOCUMENT ME! */
    private static float[] box = new float[3];
    
    private static boolean flip;
    
    private static double[][] inverseDicomArray = new double[4][4];
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Create the polyline from the given polyline coordinates vector.  
     * This method converts the image space to viewing space
     * @param kImage        Image A
     * @param coordVector   polyline coordinate vector
     * @return  polyline     created
     */
    public static Polyline createPolyline(ModelImage kImage, Vector<Vector3f> coordVector) {

		ColorRGB kColor1;
		float fX, fY, fZ;
		TransMatrix inverseDicomMatrix = new TransMatrix(4);
		
		int[] extents = kImage.getExtents();
		int xDim = extents[0];
		int yDim = extents[1];
		int zDim = extents[2];

		float[] resols = kImage.getFileInfo()[0].getResolutions();
		float xBox = (xDim - 1) * resols[0];
		float yBox = (yDim - 1) * resols[1];
		float zBox = (zDim - 1) * resols[2];
		float maxBox = Math.max(xBox, Math.max(yBox, zBox));
	
		
		boolean dicom = false;
		// boolean flip = true;
		
		float[] tCoord = new float[3];
        float[] coord = new float[3];
		
        int actions;
        
        if (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {

            dicom = true;
            
            inverseDicomMatrix.copyMatrix(inverseDicomArray);
        }
		
        if (dicom == false) {

            if (flip) {
                actions = 1;
            } else {
                actions = 0;
            }
        } else {

            if (flip) {
                actions = 3;
            } else {
                actions = 2;
            }
        }
        
        if ((actions == 1) || (actions == 3)) {
            flip = true;
        } else {
            flip = false;
        }
        flip = !flip;
        
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetCChannels(0, 3);
		kAttr.SetCChannels(1, 3);
		VertexBuffer pkVBuffer = new VertexBuffer(kAttr, coordVector.size());
		kColor1 = new ColorRGB(0.5f, 0f, 0f);
		for (int i = 0; i < coordVector.size(); i++) {
			Vector3f point = (coordVector.elementAt(i));

			fX = point.X;
			fY = point.Y;
			fZ = point.Z;

			
			 if (dicom) {
             	System.err.println("dicom");
             	
                 tCoord[0] = fX - startLocation[0];
                 tCoord[1] = fY - startLocation[1];
                 tCoord[2] = fZ - startLocation[2];
                 inverseDicomMatrix.transform(tCoord, coord);
                 fX = ( (coord[0] * direction[0]) + startLocation[0] );
                 fY = ( (coord[1] * direction[1]) + startLocation[1] );
                 fZ = ( (coord[2] * direction[2]) + startLocation[2] );
                 
             } // if (dicom)

			 
			 
             if (flip) {
             	System.err.println("flip");
//               Flip (kVertex.y - startLocation[1], but
//               don't flip startLocation[1]
             	 
                 fY = ( (2 * startLocation[1]) + (box[1] * direction[1]) - fY );
                 fZ = ( (2 * startLocation[2]) + (box[2] * direction[2]) - fZ );
             }
            
           
		    fX =  ((2.0f * (fX - startLocation[0]) / direction[0]) -
                     ((xDim - 1) * resols[0])) / (2.0f*maxBox);
            fY =  ((2.0f * (fY - startLocation[1]) / direction[1]) -
                  ((yDim - 1) * resols[1])) / (2.0f*maxBox);
            fZ =  ((2.0f * (fZ - startLocation[2]) / direction[2]) -
                     ((zDim - 1) * resols[2])) / (2.0f*maxBox) ;
              
             /*
             fX = fX * resols[0];
             fY = fY * resols[1];
             fZ = fZ * resols[2];
            
             fX =  ((2.0f * (fX) / direction[0]) -
                     ((xDim - 1) * resols[0])) / (2.0f*maxBox);
            fY =  ((2.0f * (fY) / direction[1]) -
                  ((yDim - 1) * resols[1])) / (2.0f*maxBox);
            fZ =  ((2.0f * (fZ) / direction[2]) -
                     ((zDim - 1) * resols[2])) / (2.0f*maxBox) ;
                     */
        
            
			pkVBuffer.SetPosition3(i, (fX), (fY), (fZ));
			// pkVBuffer.SetColor3(0, i, new ColorRGB(fX, fY, fZ));
			pkVBuffer.SetColor3(0, i, kColor1);
			pkVBuffer.SetColor3(1, i, kColor1);

		}
		boolean bClosed = false;
		boolean bContiguous = true;

		return new Polyline(pkVBuffer, bClosed, bContiguous);

	}

    /**
     * The action taken when the Add button is clicked in the JPanelSurface class. A file dialog is launched that allows
     * the user to select new surfaces to load from disk.
     *
     * @param   kImage     the ModelImage displayed in the SurfaceRender class
     */
    public static Polyline[] openPolylines(ModelImage kImage) {
        File[] akFiles = openFiles(true);
        
        if (akFiles == null) {
            return null;
        }

        Polyline[] kPolyline = new Polyline[akFiles.length];
        for (int i = 0; i < akFiles.length; i++) {
            String kName = akFiles[i].getName();
            if (kName.indexOf(".xml") != -1) {	           	 
	           	Vector<Vector3f> coordVector = new Vector<Vector3f>();
            	
	           	try {
            	   FilePolylineVOIXML polylineXML = new FilePolylineVOIXML(kName, akFiles[i].getParent());
            	   coordVector = polylineXML.readVOI(false);
                   flip = polylineXML.getFlip();
                   direction = polylineXML.getDirection();
                   startLocation = polylineXML.getOrigin();
                   box = polylineXML.getBox();
                   inverseDicomArray = polylineXML.getInverseDicomArray();
                   
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
            } 
            File[] files = new File[1];
            files[0] = chooser.getSelectedFile();

            return files;
        }

        return null;
    }
    
    

}
