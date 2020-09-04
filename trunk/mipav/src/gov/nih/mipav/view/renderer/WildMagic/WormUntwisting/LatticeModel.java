package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;


import java.awt.Color;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.BitSet;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Distance.DistanceSegment3Segment3;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Intersection.IntrSegment3Box3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;


/**
 * Supports the worm-straightening algorithms that use a 3D lattice as the basis of the straightening process.
 */
public class LatticeModel {

	protected static final int SampleLimit = 5;

	public static void checkParentDir( String parentDir )
	{
		File parentFileDir = new File(parentDir);
		if (parentFileDir.exists() && parentFileDir.isDirectory()) { // do nothing
		} else if (parentFileDir.exists() && !parentFileDir.isDirectory()) { // do nothing
		} else { // voiFileDir does not exist
			//			System.err.println( "LatticeModel:checkParentDir" + parentDir);
			parentFileDir.mkdir();
		}
	}


	static public boolean match( Color c1, Color c2 )
	{
		return ( (c1.getRed() == c2.getRed()) && (c1.getGreen() == c2.getGreen()) && (c1.getBlue() == c2.getBlue()) );
	}
	
	public static String checkName(String name, String original, int count, VOI annotationVOIs) {
		if ( annotationVOIs == null ) return name;
		if ( annotationVOIs.getCurves() == null ) return name;
		if ( annotationVOIs.getCurves().size() == 0 ) return name;
		
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ ) {
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( text.getText().contentEquals(name) ) {
				name = original + "_" + count++;
				return checkName(name, original, count, annotationVOIs);
			}
		}
		return name;
	}

	/**
	 * Read a list of annotations from a CSV file: name,x,y,z,radius (optional)
	 * @param fileName
	 * @return VOI containing list of annotations.
	 */
	public static VOI readAnnotationsCSV( String fileName )
	{
		File file = new File(fileName);
		if ( file.exists() )
		{		
			short sID = 0;
			//        	System.err.println( fileName );
			FileReader fr;
			try {
				fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String line = br.readLine();
				line = br.readLine();
				String renameString = "";
				VOI annotationVOIs = new VOI( sID, "annotationVOIs", VOI.ANNOTATION, 0 );
				int count = 1;
				while ( line != null && (line.length() > 1) )
				{
					String[] parsed = line.split( "," );
					if ( parsed.length != 0 )
					{
						VOIWormAnnotation text = new VOIWormAnnotation();
						text.setUseMarker(false);
						text.setNote(fileName);
						float x, y, z, r, g, b;
						if ( parsed.length > 6 )
						{
							// name, position and color
							int parsedIndex = 0;
							String name = String.valueOf( parsed[parsedIndex++] );
							String name1 = checkName(name, new String(name), 1, annotationVOIs);
							if ( !name.equals(name1) ) {
								renameString += name + "->" + name1 + "\n";
							}
							text.setText(name1);
							x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
							y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
							z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;
							r    = (parsed.length > parsedIndex+3) ? (parsed[parsedIndex+3].length() > 0) ? Float.valueOf( parsed[parsedIndex+3] ) : 1 : 1;
							g    = (parsed.length > parsedIndex+4) ? (parsed[parsedIndex+4].length() > 0) ? Float.valueOf( parsed[parsedIndex+4] ) : 1 : 1;
							b    = (parsed.length > parsedIndex+5) ? (parsed[parsedIndex+5].length() > 0) ? Float.valueOf( parsed[parsedIndex+5] ) : 1 : 1;
							//							System.err.println( name + " " + x + " " + y + " " + z + " " + r );
							if ( (parsedIndex + 6) < parsed.length ) {
								// read the lattice segment:
								int segment = (parsed.length > parsedIndex+6) ? (parsed[parsedIndex+6].length() > 0) ? Integer.valueOf( parsed[parsedIndex+6] ) : 1 : 1;
								//								System.err.println( name + "  " + segment );
								text.setLatticeSegment(segment);
							}
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x, y, z ) );
							text.setColor( new Color(r/255f,g/255f,b/255f) );
							annotationVOIs.getCurves().add(text);
						}
						else if ( parsed.length >= 4 )
						{
							// name, position and radius:
							int parsedIndex = 0;
							String name = String.valueOf( parsed[parsedIndex++] );
							String name1 = checkName(name, new String(name), 1, annotationVOIs);
							if ( !name.equals(name1) ) {
								renameString += name + "->" + name1 + "\n";
							}
							text.setText(name1);
							x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
							y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
							z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;
							r    = (parsed.length > parsedIndex+3) ? (parsed[parsedIndex+3].length() > 0) ? Float.valueOf( parsed[parsedIndex+3] ) : 1 : 1;
							//							System.err.println( name + " " + x + " " + y + " " + z + " " + r );
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x+r, y, z ) );
							annotationVOIs.getCurves().add(text);
						}
						else if ( parsed.length == 3 )
						{
							if ( count > 1 ) {
							// position only
							x    = (parsed.length > 0) ? (parsed[0].length() > 0) ? Float.valueOf( parsed[0] ) : 0 : 0; 
							y    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf( parsed[1] ) : 0 : 0; 
							z    = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf( parsed[2] ) : 0 : 0; 
							text.setText( "pt_" + count );
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x+1, y, z ) );
							annotationVOIs.getCurves().add(text);
							}
						}
						count++;
					}
					line = br.readLine();
				}
				if ( renameString.length() > 0 ) {
					MipavUtil.displayError( "Renamed duplicate annotations:\n" + renameString );
				}
				fr.close();
				if ( count > 1 )
				{
					return annotationVOIs;
				}
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		return null;
	}

	public static VOIVector readLatticeCSV(String fileName) {
		return readLatticeCSV(fileName, false);
	}

	public static VOIVector readLatticeCSV(String fileName, boolean saveSeamCells ) {
		File file = new File(fileName);
		if ( file.exists() )
		{		
			VOIVector lattice = new VOIVector();
			short sID = 0;
			FileReader fr;
			try {
				fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String line = br.readLine();
				//				System.err.println(line);
				line = br.readLine();
				//				System.err.println(line);

				VOI left = new VOI(sID, "left", VOI.ANNOTATION, (float) Math.random());
				left.setColor(new Color(0, 0, 255));
				VOI right = new VOI(sID++, "right", VOI.ANNOTATION, (float) Math.random());
				right.setColor(new Color(0, 0, 255));
				lattice.add(left);
				lattice.add(right);
				int count = 1;
				
				while ( line != null && (line.length() > 1) )
				{
					String[] parsed = line.split( "," );
					if ( parsed.length != 0 )
					{
						int parsedIndex = 0;
						String name = String.valueOf( parsed[parsedIndex++] );
						float x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
						float y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
						float z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;
						
						VOIWormAnnotation leftAnnotation = new VOIWormAnnotation( new Vector3f(x,y,z) );
						leftAnnotation.setText(name);
						if ( name.contains("H") || name.contains("V") || name.contains("Q") || name.contains("T") ) {
							leftAnnotation.setSeamCell(true);
						}
						left.getCurves().add( leftAnnotation );
						count++;
					}
					line = br.readLine();
					//					System.err.println(line);

					parsed = line.split( "," );
					if ( parsed.length != 0 )
					{
						int parsedIndex = 0;
						String name = String.valueOf( parsed[parsedIndex++] );
						float x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
						float y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
						float z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;
						
						VOIWormAnnotation rightAnnotation = new VOIWormAnnotation( new Vector3f(x,y,z) );
						rightAnnotation.setText(name);
						if ( name.contains("H") || name.contains("V") || name.contains("T") ) {
							rightAnnotation.setSeamCell(true);
						}
						right.getCurves().add( rightAnnotation );
						count++;
					}
					line = br.readLine();
					//					System.err.println(line);
				}
				fr.close();
				if ( count > 1 )
				{
					// create contour lines & pairs

					VOI leftLattice = new VOI(sID++, "leftContour", VOI.POLYLINE, (float) Math.random());					
					VOIContour leftC = new VOIContour(false);
					leftLattice.getCurves().add(leftC);
					lattice.add(leftLattice);
					
					VOI rightLattice = new VOI(sID++, "rightContour", VOI.POLYLINE, (float) Math.random());
					VOIContour rightC = new VOIContour(false);
					rightLattice.getCurves().add(rightC);
					lattice.add(rightLattice);

					for ( int i = 0; i < left.getCurves().size(); i++ ) {
						leftC.add(left.getCurves().elementAt(i).elementAt(0));
						rightC.add(left.getCurves().elementAt(i).elementAt(0));
						
						
						final VOI marker = new VOI(sID++, "pair_" + (i+1), VOI.POLYLINE, (float) Math.random());
						final VOIContour mainAxis = new VOIContour(false);
						mainAxis.add(left.getCurves().elementAt(i).elementAt(0));
						mainAxis.add(left.getCurves().elementAt(i).elementAt(0));
						marker.getCurves().add(mainAxis);
						marker.setColor(new Color(255, 255, 0));
						mainAxis.update(new ColorRGBA(1, 1, 0, 1));
						if (i == 0) {
							marker.setColor(new Color(0, 255, 0));
							mainAxis.update(new ColorRGBA(0, 1, 0, 1));
						}
						lattice.add(marker);
					}
				}
				return lattice;
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		return null;
	}




	/**
	 * Saves all VOIs to the specified file.
	 * 
	 * @param voiDir
	 * @param image
	 */
	public static void saveAllVOIsTo(final String voiDir, final ModelImage image) {
		try {
			final ViewVOIVector VOIs = image.getVOIs();

			final File voiFileDir = new File(voiDir);

			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
				final String[] list = voiFileDir.list();
				for (int i = 0; i < list.length; i++) {
					final File lrFile = new File(voiDir + list[i]);
					lrFile.delete();
				}
			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { 
				voiFileDir.delete();
			} else { // voiFileDir does not exist
				//				System.err.println( "saveAllVOIsTo " + voiDir);
				voiFileDir.mkdir();
			}

			final int nVOI = VOIs.size();
			for (int i = 0; i < nVOI; i++) {
				if ( VOIs.VOIAt(i).getCurves().size() > 0 ) {
					if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
						final FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, image);
						fileVOI.writeXML(VOIs.VOIAt(i), true, true);
					} else {
						final FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, image);
						fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), true);
					}
				}
			}

		} catch (final IOException error) {
			MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
		}

	} // end saveAllVOIsTo()
	
	

	public static void saveAnnotationsAsCSV(final String dir, final String fileName, VOI annotations )
	{
		LatticeModel.saveAnnotationsAsCSV(dir,fileName, annotations, false);
	}
	
	
	
	/**
	 * Saves the input annotations to the CSV file in the following format:
	 * name,x,y,z
	 * @param dir
	 * @param fileName
	 * @param annotations
	 */
	public static void saveAnnotationsAsCSV(final String dir, final String fileName, VOI annotations, boolean isCurve )
	{		
		Preferences.debug("Saving annotations list: " + "\n", Preferences.DEBUG_ALGORITHM );
		//		System.err.println("Saving annotations list: " + dir + "  " + fileName );
		int numSaved = 0;
		// check files, create new directories and delete any existing files:
		final File fileDir = new File(dir);

		if (fileDir.exists() && fileDir.isDirectory()) {} 
		else if (fileDir.exists() && !fileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			fileDir.mkdir();
		}
		
		File file = new File(fileDir + File.separator + fileName);
		if (file.exists()) {
			if ( annotations == null || annotations.getCurves().size() == 0 ) {
	            int result = JOptionPane.showConfirmDialog(null, "No annotations found, delete annotations File from: " + fileDir + "?", "Delete Annotations", JOptionPane.YES_NO_OPTION);
	            System.err.println( "Delete answer " + (result == JOptionPane.YES_OPTION) );
	            if (result != JOptionPane.YES_OPTION) {
	            	return;
	            }
			}
			file.delete();
			file = new File(fileDir + File.separator + fileName);
		}
		
		if ( annotations == null ) {
			System.err.println("annotations null");
			return;
		}
		if ( annotations.getCurves().size() == 0 ) {
			System.err.println("annotations size = 0");
			return;
		}
		
		try {
			boolean saveLatticeSegment = false;
			int curveAnnotationCount = 0;
			for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
				VOIWormAnnotation annotation = (VOIWormAnnotation)annotations.getCurves().elementAt(i);
				if ( !annotation.isCurveAnnotation() && annotation.getLatticeSegment() != -1 ) {
					saveLatticeSegment = true;
				}
				if ( annotation.isCurveAnnotation() ) {
					curveAnnotationCount++;
				}
			}
			// only write out annotations that aren't part of a curve segment:
			if ( !isCurve && (curveAnnotationCount == annotations.getCurves().size()) ) return;
			

			
//			System.err.println( "saveAnnotationsAsCSV " + (annotations.getCurves().size() - curveAnnotationCount) );
			
			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			if ( saveLatticeSegment ) {
				bw.write("name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "R" + "," + "G" + "," + "B" + "," + "lattice segment" + "\n");
			}
			else {
				bw.write("name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "R" + "," + "G" + "," + "B" + "\n");
			}
			
			for (int i = 0; i < annotations.getCurves().size(); i++) {

				VOIWormAnnotation annotation = (VOIWormAnnotation)annotations.getCurves().elementAt(i);
				if ( !isCurve && annotation.isCurveAnnotation() ) continue;
				if ( isCurve && !annotation.isCurveAnnotation() ) continue;
				if ( annotation.size() == 0 ) {
					System.err.println("error");
				}
				Vector3f position = annotation.elementAt(0);
				if ( saveLatticeSegment ) {
					int segment = annotation.getLatticeSegment();
					String text = annotation.getText();
					if ( text.length() == 0 ) {
						// generate the name:
					}
					if ( segment != -1 ) {
						bw.write(annotation.getText() + "," + position.X + "," + position.Y + ","	+ position.Z + "," + annotation.getColorString() + "," + segment + "\n");
					}
					else {
						bw.write(annotation.getText() + "," + position.X + "," + position.Y + ","	+ position.Z + "," + annotation.getColorString() + "\n");
					}
				}
				else {
					bw.write(annotation.getText() + "," + position.X + "," + position.Y + ","	+ position.Z + "," + annotation.getColorString() + "\n");
				}
				numSaved++;
				Preferences.debug(numSaved + "   " + annotation.getText() + "\n", Preferences.DEBUG_ALGORITHM );
				//				System.err.println( numSaved++ + "   " + annotation.getText() );
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveSeamCellsTo");
			e.printStackTrace();
		}
		Preferences.debug("Annotation written: " + numSaved + "\n", Preferences.DEBUG_ALGORITHM );
//		System.err.println( "saveAnnotationsAsCSV " + fileName + "  " + numSaved );
	}

	protected static Vector3f inverseDiagonal( ModelImage image, int slice, int extent, Vector3f[] verts, Vector3f target ) 
	{
		final int[] dimExtents = image.getExtents();
		final int iBound = extent;
		final int jBound = extent;

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		float minDistance = Float.MAX_VALUE;
		Vector3f closest = new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE);
		Vector3f pt = new Vector3f();
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) ) {

					// do nothing
				} else {
					pt.set(i, j, slice);
					float dist = pt.distance(target);
					if ( dist < minDistance )
					{
						minDistance = dist;
						closest.set(iIndex, jIndex, kIndex);
					}
				}
				//				}
				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
		//		System.err.println( "inverseDiagonal " + target + "     " + minDistance);
		//		System.err.println( "                " + closest );
		return closest;
	}
	/**
	 * @param image the ModelImage for registering loaded VOIs.
	 * @param voiDir the directory to load voi's from.
	 * @param quietMode if true indicates that warnings should not be displayed.
	 * @param resultVector the result VOI Vector containing the loaded VOIs.
	 * @param registerVOIs when true the VOIs are registered in the input image.
	 */
	private static void loadAllVOIsFrom(ModelImage image, final String voiDir, boolean quietMode, VOIVector resultVector, boolean registerVOIs) {

		int i, j;
		VOI[] VOIs;
		FileVOI fileVOI;

		try {

			// if voiDir does not exist, then return
			// if voiDir exists, then get list of voi's from directory (*.voi)
			final File voiFileDir = new File(voiDir);
			final Vector<String> filenames = new Vector<String>();
			final Vector<Boolean> isLabel = new Vector<Boolean>();

			if (voiFileDir.exists() && voiFileDir.isDirectory()) {

				// get list of files
				final File[] files = voiFileDir.listFiles();

				for (final File element : files) {

					if (element.getName().endsWith(".voi") || element.getName().endsWith(".xml")) {
						filenames.add(element.getName());
						isLabel.add(false);
					} else if (element.getName().endsWith(".lbl")) {
						filenames.add(element.getName());
						isLabel.add(true);
					}
				}
			} else { // voiFileDir either doesn't exist, or isn't a directory

				if ( !quietMode) {
					MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
				}

				return;
			}

			// open each voi array, then register voi array to this image
			for (i = 0; i < filenames.size(); i++) {

				fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, image);

				VOIs = fileVOI.readVOI(isLabel.get(i));

				for (j = 0; j < VOIs.length; j++) {

					if ( registerVOIs )
					{   	
						image.registerVOI(VOIs[j]);
					}
					if ( resultVector != null )
					{
						resultVector.add(VOIs[j]);
					}
				}
			}
			// when everything's done, notify the image listeners
			if ( registerVOIs )
			{   	
				image.notifyImageDisplayListeners();
			}

		} catch (final Exception error) {

			if ( !quietMode) {
				MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
			}
		}

	}

	protected ModelImage imageA;
	private Vector3f imageDims;
	
	protected ModelImage seamCellImage;
	// Set of two contours, each connected to the other pair-wise
	protected VOIVector lattice = null;
	// left side of the lattice:
	protected VOI left;
	protected VOI leftContour;
//	protected VOIContour leftLattice;
	// right side of the lattice:
	protected VOI right;
	protected VOI rightContour;
//	protected VOIContour rightLattice;
	// horizontal bars connecting left and right lattice pairs:
	private VOIVector latticeGrid;
	// center points half-way between left and right sides of the lattice:
	protected VOIContour center;

	// positions in time along the center-line spline for each lattice point
	protected int[] latticeSlices;

	// time positions for the center spline running through the center points:
	protected float[] afTimeC;
	// interpolated spline times
	protected float[] allTimes;
	// index into the interpolated curve points, each position corresponds to 
	// a point on the center curve:
	protected int[] splineRangeIndex;
	// spline through the center points, passes through each point
	protected NaturalSpline3 centerSpline;
	// contour for displaying the center spline - interpolated with 1-voxel stepsize
	protected VOIContour centerPositions;
	// contour for displaying the left spline - interpolated with 1-voxel stepsize
	protected VOIContour leftPositions;

	// contour for displaying the right spline - interpolated with 1-voxel stepsize
	protected VOIContour rightPositions;
	// VOI for displaying the left spline - interpolated with 1-voxel stepsize
	private VOI leftLine;

	// VOI for displaying the right spline - interpolated with 1-voxel stepsize
	private VOI rightLine;
	// VOI for displaying the center spline - interpolated with 1-voxel stepsize
	private VOI centerLine;
	// distance between left and right splines per voxel step
	protected Vector<Float> wormDiameters;

	// maximum diameter plus buffer:
	protected int extent = -1;
	// The following vector create an orthogonal frame at each position along
	// the center-line curve of the lattice - at each interpolated point along the curve:
	// right vector per voxel step
	protected Vector<Vector3f> rightVectors;

	// normal vector tangent to the spline per voxel step
	protected Vector<Vector3f> normalVectors;
	// up vector per voxel step
	protected Vector<Vector3f> upVectors;

	// old / used for testing:
	// bounding box containing the ellipses:
	protected Vector<Box3f> boxBounds;

	// ellipse bounds around the worm mode;
	protected Vector<Ellipsoid3f> ellipseBounds;

	// Set when the sampling planes, etc are initialized/updated:
	private boolean latticeInterpolationInit = false;

	// planes that sweep through the worm volume, following the splines, used to sample the volume and untwist:
	protected VOI samplingPlanes;

	private VOI displayContours;

	protected VOI displayInterpolatedContours;

	private Vector3f pickedPoint = null;

	private VOI showSelectedVOI = null;

	private VOIContour[] showSelected = null;

	protected final int DiameterBuffer = 30;

	protected final float minRange = .025f;

	private VOI leftMarker;
	private VOI rightMarker;

	protected VOI annotationVOIs;
	protected Vector3f wormOrigin = null;
	protected Vector3f transformedOrigin = new Vector3f();

	private Vector<VOI> neuriteData;
	protected String outputDirectory;

	protected Short voiID = 0;

	private int[][] seamCellIDs = null;
	private int[][] allSeamCellIDs = null;

	private boolean colorAnnotations = false;

	private Vector<AnnotationListener> annotationListeners;
	private Vector<CurveListener> curveListeners;
	private Vector<LatticeListener> latticeListeners;

	private String annotationPrefix = "A";

	private int paddingFactor = 0;


	private final int numEllipsePts = 32;

	private VOI latticeContours = null;

	private Vector<String> markerNames;

	private Vector<Vector3f> markerCenters;

	private Vector<Integer> markerLatticeSegments;
	private Vector<Integer> markerSlices;

	private VOI annotationsStraight = null;

	private VOIVector latticeStraight = null;

	private boolean previewMode = false;

	private Vector<boolean[]> clipMask = null;
	
	// spline data members:
	private class AnnotationSplineControlPts  {
		public String name;
		public String prefix;
		public Vector<VOIWormAnnotation> annotations;
		public VOIContour curve;
		public VOI curveVOI;
		public boolean selected = true;
		public AnnotationSplineControlPts() {}
	}
	protected Vector<AnnotationSplineControlPts> splineControlPtsList;

	/**
	 * Creates a new LatticeModel
	 * 
	 * @param imageA
	 */
	public LatticeModel(final ModelImage imageA) {
		this.imageA = imageA;
		if ( imageA != null )
		{
			String imageName = imageA.getImageName();
			if (imageName.contains("_clone")) {
				imageName = imageName.replaceAll("_clone", "");
			}
			outputDirectory = new String(imageA.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
			String parentDir = new String(imageA.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator);
			if ( !imageName.contains("straightened" ) ) {
				checkParentDir(parentDir);
			}
			
			imageDims = new Vector3f( imageA.getExtents()[0] - 1, imageA.getExtents()[1] - 1, imageA.getExtents()[2] - 1 );
		}
	}

	/**
	 * Creats a new LatticeModel with the given input lattice.
	 * 
	 * @param imageA
	 * @param lattice
	 */
	public LatticeModel(final ModelImage imageA, final VOIVector lattice) {
		this.imageA = imageA;
		imageDims = new Vector3f( imageA.getExtents()[0] - 1, imageA.getExtents()[1] - 1, imageA.getExtents()[2] - 1 );
		String imageName = imageA.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		outputDirectory = new String(imageA.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
		String parentDir = new String(imageA.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator);
		checkParentDir(parentDir);			
		this.lattice = lattice;

		// Assume image is isotropic (square voxels).
		if (lattice.size() < 2) {
			return;
		}
		left = (VOI) lattice.elementAt(0);
		right = (VOI) lattice.elementAt(1);
		if (left.getCurves().size() != right.getCurves().size()) {
			return;
		}

		this.imageA.registerVOI(left);
		this.imageA.registerVOI(right);
		showLatticeLabels(false);
		
		updateLattice(true);
	}

	/**
	 * Add an annotation to the annotation list.
	 * 
	 * @param textVOI
	 */
	public void addAnnotation(final VOI textVOI, boolean multiSelect ) {
		if (annotationVOIs == null) {
			final int colorID = 0;
			annotationVOIs = new VOI((short) colorID, "annotationVOIs", VOI.ANNOTATION, -1.0f);
			imageA.registerVOI(annotationVOIs);
		}
		VOIWormAnnotation newText = new VOIWormAnnotation( (VOIText)textVOI.getCurves().firstElement());
		addAnnotation(newText, multiSelect);
	}
	
	public void addAnnotation( VOIWormAnnotation newText, boolean multiSelect ) {
		newText.firstElement().X = Math.round( newText.firstElement().X );	newText.firstElement().Y = Math.round( newText.firstElement().Y );  newText.firstElement().Z = Math.round( newText.firstElement().Z );
		newText.lastElement().X  = Math.round( newText.lastElement().X );	newText.lastElement().Y  = Math.round( newText.lastElement().Y );   newText.lastElement().Z  = Math.round( newText.lastElement().Z );
		Color c = newText.getColor();
		newText.update(new ColorRGBA(c.getRed() / 255.0f, c.getGreen() / 255.0f, c.getBlue() / 255.0f, 1f));
		newText.setUseMarker(false);
		newText.retwist(previewMode);
		annotationVOIs.getCurves().add(newText);
		annotationVOIs.setColor(c);

		// checks if the annotation is the worm nose or the origin position:
		if (newText.getText().equalsIgnoreCase("nose") || newText.getText().equalsIgnoreCase("origin")) {
			if (wormOrigin == null) {
				wormOrigin = new Vector3f(newText.elementAt(0));
			} else {
				wormOrigin.copy(newText.elementAt(0));
			}
		}
		// set the annotation colors, if necessary
		colorAnnotations();
		// update which annotations are selected:
		newText.setSelected(true);
		newText.updateSelected(imageA);
		Vector3f pt = newText.elementAt(0);
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( text != newText ) {
				// set selection to false of not using multiSelect
				text.setSelected( text.isSelected() && multiSelect );
				text.updateSelected(imageA);
			}
			if ( text.isSelected() ) {
				text.setSelectionOffset( Vector3f.sub(pt, text.elementAt(0)) );
			}
		}
		imageA.notifyImageDisplayListeners();

		// update the annotation listeners to changes:
		updateAnnotationListeners();
	}

	/**
	 * Adds an annotation listener. The annotation listeners are updated when
	 * the annotations change in any way.
	 * @param listener
	 */
	public void addAnnotationListener( AnnotationListener listener )
	{
		if ( annotationListeners == null ) {
			annotationListeners = new Vector<AnnotationListener>();
		}
		if ( !annotationListeners.contains(listener ) ) {
			annotationListeners.add(listener);
		}
	}

	/**
	 * Add a curve to the list.
	 * 
	 * @param textVOI
	 */
	public void addSplineControlPts( Vector<VOIWormAnnotation> controlPts ) {
		if ( controlPts.size() < 2 ) return;
		
		if ( splineControlPtsList == null ) {
			splineControlPtsList = new Vector<AnnotationSplineControlPts>();
		}
		
		// make the curve a spline and display:
		AnnotationSplineControlPts annotationSpline = new AnnotationSplineControlPts();
		splineControlPtsList.add(annotationSpline);
		
		annotationSpline.annotations = controlPts;
		// generate curve:
		NaturalSpline3 spline = smoothCurve(annotationSpline.annotations);

		//display:
		annotationSpline.prefix = getPrefix(controlPts.elementAt(0).getText());
		annotationSpline.curve = new VOIContour(false);
		float length = spline.GetLength(0, 1);
		int maxLength = (int)Math.ceil(length);
		float step = 1;
		if ( maxLength != length )
		{
			step = (length / maxLength);
		}
		for (int i = 0; i <= maxLength; i++) {
			final float t = spline.GetTime(i*step);
			annotationSpline.curve.add(spline.GetPosition(t));
		}
		annotationSpline.name = new String( annotationSpline.prefix );
		annotationSpline.curveVOI = new VOI((short)splineControlPtsList.size(), annotationSpline.name );
		annotationSpline.curveVOI.getCurves().add(annotationSpline.curve);
		annotationSpline.curveVOI.setColor(Color.red);
		annotationSpline.curve.update(new ColorRGBA(1, 0, 0, 1));
		imageA.registerVOI(annotationSpline.curveVOI);
		
		imageA.notifyImageDisplayListeners();

		// update the annotation listeners to changes:
		updateCurveListeners();
	}

	public void addCurveListener( CurveListener listener )
	{
		if ( curveListeners == null ) {
			curveListeners = new Vector<CurveListener>();
		}
		if ( !curveListeners.contains(listener ) ) {
			curveListeners.add(listener);
		}
	}

	public void addLatticeListener( LatticeListener listener )
	{
		if ( latticeListeners == null ) {
			latticeListeners = new Vector<LatticeListener>();
		}
		if ( !latticeListeners.contains(listener ) ) {
			latticeListeners.add(listener);
		}
	}

	/**
	 * Adds a new left/right marker to the worm image.
	 * 
	 * @param pt
	 */
	public void addLeftRightMarker(final Vector3f pt, boolean isSeam ) {
		if (lattice == null) {
			lattice = new VOIVector();
		}
		if ( left == null ) {
			short id = (short) imageA.getVOIs().getUniqueID();
			left = new VOI(id, "left", VOI.ANNOTATION, (float) Math.random());
			right = new VOI(id++, "right", VOI.ANNOTATION, (float) Math.random());
			lattice.add(left);
			lattice.add(right);
		}
		int seamCount = 0;
		int otherCount = 0;
		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			if ( ((VOIWormAnnotation)left.getCurves().elementAt(i)).isSeamCell() ) {
				seamCount++;
			}
			else {
				otherCount++;
			}
		}
		if (left.getCurves().size() == right.getCurves().size()) {
			VOIWormAnnotation annotation = new VOIWormAnnotation(new Vector3f(pt));
			annotation.setSeamCell(isSeam);
			if ( isSeam ) {
				String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
				annotation.setText(name + "L");
			}
			else {
				String name = "a" + otherCount;
				annotation.setText(name + "L");
			}
			left.getCurves().add(annotation);
			pickedPoint = left.getCurves().lastElement().elementAt(0);
			// System.err.println( pt );

			if (leftMarker == null) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				leftMarker = new VOI(id, "leftMarker", VOI.POINT, (float) Math.random());
				this.imageA.registerVOI(leftMarker);
				leftMarker.importPoint(pt);
			} else {
				leftMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
				leftMarker.update();
			}
			return;
		} else {
			// adding to the right side - keep the last count values:
			seamCount--;
			otherCount--;
			VOIWormAnnotation annotation = new VOIWormAnnotation(new Vector3f(pt));
			annotation.setSeamCell(isSeam);
			if ( isSeam ) {
				String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
				annotation.setText(name + "R");
			}
			else {
				String name = "a" + otherCount;
				annotation.setText(name + "R");
			}
			right.getCurves().add(annotation);
			pickedPoint = right.getCurves().lastElement().elementAt(0);
			// System.err.println( pt );

			if (rightMarker == null) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				rightMarker = new VOI(id, "rightMarker", VOI.POINT, (float) Math.random());
				this.imageA.registerVOI(rightMarker);
				rightMarker.importPoint(pt);
			} else {
				rightMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
				rightMarker.update();
			}
		}
		// rename cells if seam count > 10 to include Q cells:
		updateSeamCount();
		updateLattice(true);
		showLatticeLabels(displayLatticeLabels);
	}

	/**
	 * Generates a natural spline curve to fit the input set of annotation points to model a neurite.
	 */
	public void addNeurite( VOI annotionVOI, String name ) {
		short sID;

		// 1. The center line of the worm is calculated from the midpoint between the left and right points of the
		// lattice.
		VOIContour neurite = new VOIContour(false);
		for (int i = 0; i < annotionVOI.getCurves().size(); i++) {
			VOIWormAnnotation text = (VOIWormAnnotation) annotionVOI.getCurves().elementAt(i);
			neurite.add( new Vector3f( text.elementAt(0) ) );
		}
		float[] time = new float[neurite.size()];
		NaturalSpline3 neuriteSpline = smoothCurve(neurite, time);

		VOIContour neuriterPositions = new VOIContour(false);

		float length = neuriteSpline.GetLength(0, 1);
		for (int i = 0; i <= length; i++) {
			final float t = neuriteSpline.GetTime(i);
			neuriterPositions.add(neuriteSpline.GetPosition(t));
		}

		sID = (short) (imageA.getVOIs().getUniqueID());
		VOI neuriteVOI = new VOI(sID, name, VOI.POLYLINE, (float) Math.random() );
		neuriteVOI.getCurves().add(neuriterPositions);
		neuriteVOI.setColor(Color.white);
		neuriterPositions.update(new ColorRGBA(1, 1, 1, 1));

		if ( neuriteData == null )
		{
			neuriteData = new Vector<VOI>();
		}
		for ( int i = 0; i < neuriteData.size(); i++ )
		{
			if ( neuriteData.elementAt(i).getName().equals(name) )
			{
				imageA.unregisterVOI( neuriteData.remove(i) );
				break;
			}
		}
		neuriteData.add(neuriteVOI);
		imageA.registerVOI(neuriteVOI);
	}


	/**
	 * Clears the selected VOI or Annotation point.
	 */
	public void clear3DSelection() {
		if ( (annotationVOIs != null) && (annotationVOIs.getCurves() != null) ) {
			for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ ) {
				final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
				text.setSelected(false);
				text.updateSelected(imageA);
			}
		}
		pickedPoint = null;
		if (showSelected != null) {
			imageA.unregisterVOI(showSelectedVOI);
		}
		if (showSelected != null) {
			for (int i = 0; i < showSelected.length; i++) {
				showSelected[i].dispose();
			}
			showSelected = null;
		}
		showSelectedVOI = null;
		imageA.notifyImageDisplayListeners();
	}

	/**
	 * Enables user to start editing the lattice.
	 */
	public void clearAddLeftRightMarkers() {
		imageA.unregisterVOI(leftMarker);
		imageA.unregisterVOI(rightMarker);
		if (leftMarker != null) {
			leftMarker.dispose();
			leftMarker = null;
		}
		if (rightMarker != null) {
			rightMarker.dispose();
			rightMarker = null;
		}
	}

	public void colorAnnotations( boolean setColor )
	{
		colorAnnotations = setColor;
		colorAnnotations();
	}

	public void deleteAnnotations() {
		clear3DSelection();
		if (annotationVOIs != null) {
			imageA.unregisterVOI(annotationVOIs);
		}
		annotationVOIs = null;
		updateAnnotationListeners();
	}

	/**
	 * Deletes the selected annotation or lattice point.
	 * 
	 * @param doAnnotation
	 */
	public void deleteSelectedPoint(final boolean doAnnotation) {
		if (doAnnotation)
		{
			for ( int i = annotationVOIs.getCurves().size()-1; i >= 0; i-- ) {
				final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
				if ( text.isSelected() ) {
					annotationVOIs.getCurves().remove(i);
					text.setSelected(false);
					text.updateSelected(imageA);

					if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
						wormOrigin = null;
					}
				}
			}
			colorAnnotations();
			updateAnnotationListeners();
		}
		else if ( !doAnnotation && !previewMode )
		{
			boolean deletedLeft = false;
			boolean deletedRight = false;
			if ( (rightMarker != null) && pickedPoint.equals(rightMarker.getCurves().elementAt(0).elementAt(0))) {
				imageA.unregisterVOI(rightMarker);
				rightMarker.dispose();
				rightMarker = null;

				deletedRight = true;
			}

			if ( (leftMarker != null) && pickedPoint.equals(leftMarker.getCurves().elementAt(0).elementAt(0))) {
				imageA.unregisterVOI(leftMarker);
				leftMarker.dispose();
				leftMarker = null;
				deletedLeft = true;

				if (rightMarker != null) {
					imageA.unregisterVOI(rightMarker);
					rightMarker.dispose();
					rightMarker = null;
					deletedRight = true;
				}
			}
			if (deletedLeft || deletedRight) {
				if (deletedLeft) {
					left.getCurves().remove(left.getCurves().lastElement());
				}
				if (deletedRight) {
					right.getCurves().remove(right.getCurves().lastElement());
				}
			} else {
				final int leftIndex = findPoint(left, pickedPoint);
				final int rightIndex = findPoint(right, pickedPoint);
				if (leftIndex != -1) {
					left.getCurves().remove(leftIndex);
					right.getCurves().remove(leftIndex);
					deletedLeft = true;
					deletedRight = true;
				} else if (rightIndex != -1) {
					left.getCurves().remove(rightIndex);
					right.getCurves().remove(rightIndex);
					deletedLeft = true;
					deletedRight = true;
				}
			}
			clear3DSelection();
			updateLattice(deletedLeft | deletedRight);
		}
		pickedPoint = null;
	}

	public void displayAnnotation( String name, boolean display ) {
		if ( annotationVOIs == null ) {
			return;
		}
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( text.getText().equals(name) ) {
				text.display(display);
			}
		}
		updateAnnotationListeners();
	}


	/**
	 * Deletes this LatticeModel
	 */
	public void dispose() {
		if (latticeGrid != null) {
			for (int i = latticeGrid.size() - 1; i >= 0; i--) {
				final VOI marker = latticeGrid.remove(i);
				imageA.unregisterVOI(marker);
			}
		}
		if ( imageA != null )
		{
			for ( int i = 0; i < lattice.size(); i++ ) {
				imageA.unregisterVOI(lattice.elementAt(i));
			}
			imageA.unregisterVOI(displayContours);
			imageA.unregisterVOI(leftLine);
			imageA.unregisterVOI(rightLine);
			imageA.unregisterVOI(centerLine);
			imageA.unregisterVOI(leftContour);
			imageA.unregisterVOI(rightContour);
			clear3DSelection();
		}

		imageA = null;
		latticeGrid = null;
		lattice = null;
		left = null;
		right = null;
		leftContour = null;
		rightContour = null;
		center = null;
		afTimeC = null;
		allTimes = null;
		centerSpline = null;
		centerPositions = null;
		leftPositions = null;
		rightPositions = null;
		leftLine = null;
		rightLine = null;
		centerLine = null;

		// if ( centerTangents != null )
		// centerTangents.clear();
		// centerTangents = null;

		if (wormDiameters != null) {
			wormDiameters.clear();
		}
		wormDiameters = null;

		if (rightVectors != null) {
			rightVectors.clear();
		}
		rightVectors = null;

		if (upVectors != null) {
			upVectors.clear();
		}
		upVectors = null;

		if (boxBounds != null) {
			boxBounds.clear();
		}
		boxBounds = null;

		if (ellipseBounds != null) {
			ellipseBounds.clear();
		}
		ellipseBounds = null;

		samplingPlanes = null;
		displayContours = null;
		pickedPoint = null;
		showSelectedVOI = null;
		showSelected = null;
	}

	public void flipLattice() {
		for ( int i = left.getCurves().size() - 1; i >= 0; i-- ) {
			VOIWormAnnotation textL = (VOIWormAnnotation) left.getCurves().remove(i);
			VOIWormAnnotation textR = (VOIWormAnnotation) right.getCurves().remove(i);
			
			String labelL2R = textL.getText();
			labelL2R = labelL2R.replace("L", "R");
			textL.setText(labelL2R);
			textL.updateText();
			textL.update();
			
			String labelR2L = textR.getText();
			labelR2L = labelR2L.replace("R", "L");			
			textR.setText(labelR2L);
			textR.updateText();
			textR.update();
			
			textL.retwist(previewMode);
			textR.retwist(previewMode);
			
			left.getCurves().add(i, textR );
			right.getCurves().add(i, textL );
		}
		
		updateLattice(true);
	}

//	public BitSet computeVolumeMask(BitSet surfaceMask, ModelImage volMask, int stepSize ) {
//		int dimX = imageA.getExtents().length > 0 ? imageA.getExtents()[0] : 1;
//		int dimY = imageA.getExtents().length > 1 ? imageA.getExtents()[1] : 1;		
//		int dimZ = imageA.getExtents().length > 2 ? imageA.getExtents()[2] : 1;	
//		System.err.println("computeVolumeMask " + dimX + "   " + dimY + "   " + dimZ );
//		
//		BitSet volMask = new BitSet(dimX*dimY*dimZ);
//
//		generateCurves(1, stepSize);
//		Vector<Vector3f> seeds = new Vector<Vector3f>();
//		for ( int i = 20; i < centerPositions.size() - 20; i++ ) {
//			seeds.add(new Vector3f(centerPositions.elementAt(i)));
//		}
//		System.err.println("Number of seeds " + seeds.size());
//
//		WormSegmentation.fill(surfaceMask, seeds, volMask, dimX, dimY, dimZ);
//		
//		return volMask;
//	}

	public TriMesh generateTriMesh( boolean returnMesh, int stepSize ) {

		generateCurves(1, stepSize);

		ModelImage overlapImage = null;
		//		if ( !returnMesh ) {
		//			FileIO fileIO = new FileIO();
		//			overlapImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_overlap.xml" );
		//			dimX = overlapImage.getExtents().length > 0 ? overlapImage.getExtents()[0] : 1;
		//			dimY = overlapImage.getExtents().length > 1 ? overlapImage.getExtents()[1] : 1;
		//			dimZ = overlapImage.getExtents().length > 2 ? overlapImage.getExtents()[2] : 1;
		//		}

		Vector<Vector3f> positions = new Vector<Vector3f>();
		int numContours = displayContours.getCurves().size();
		for ( int i = 0; i < numContours; i++ ) {
			VOIContour contour = (VOIContour) displayContours.getCurves().elementAt(i);
			Vector3f min = new Vector3f( contour.elementAt(0) );
			Vector3f max = new Vector3f( contour.elementAt(0) );
			Vector3f center = new Vector3f();
			int contourSize = contour.size();
			for ( int j = 0; j < contourSize; j++ )
			{
				min.min(contour.elementAt(j));
				max.max(contour.elementAt(j));
				if ( (i == 0) || (i == (numContours-1) ) ) {
					center.add(contour.elementAt(j));
				}
				positions.add(contour.elementAt(j));
			}
			if ( i == 0 ) {
				center.scale(1.0f/contourSize);
				// first positions is center of 'head'
				positions.add(0, center);
			}
			if ( i == (numContours-1) ) {
				center.scale(1.0f/contourSize);
				// last position is center of 'tail'
				positions.add(center);
			}
		}
		VertexBuffer vBuf = new VertexBuffer(positions);

		Box3f[][] boxes = new Box3f[numContours-1][numEllipsePts];
		System.err.println( "TriMesh " + numContours );
		Vector<Integer> indexList = new Vector<Integer>();
		for ( int i = 0; i < numContours; i++ ) {
			VOIContour contour1 = (VOIContour) displayContours.getCurves().elementAt(i);
			int contourSize1 = contour1.size();
			for ( int j = 0; j < contourSize1; j++ )
			{
				if ( i == 0 ) {
					int index = 0;
					int sliceIndex1 = positions.indexOf(contour1.elementAt(j) );
					int sliceIndex2 = positions.indexOf( contour1.elementAt((j+1)%contourSize1) );
					indexList.add(index);
					indexList.add(sliceIndex2);
					indexList.add(sliceIndex1);
				}
				if ( i == (numContours-1) ) {
					int index = positions.size()-1;
					int sliceIndex1 = positions.indexOf(contour1.elementAt(j) );
					int sliceIndex2 = positions.indexOf( contour1.elementAt((j+1)%contourSize1) );
					indexList.add(index);
					indexList.add(sliceIndex1);
					indexList.add(sliceIndex2);
				}
				if ( i < (numContours - 1) ) {
					VOIContour contour2 = (VOIContour) displayContours.getCurves().elementAt(i+1);
					int contourSize2 = contour2.size();
					//					System.err.println( i + "   " + (contourSize1 == contourSize2) );
					int sliceIndexJ = positions.indexOf(contour1.elementAt(j) );
					int nextSliceIndexJ = positions.indexOf( contour2.elementAt(j) );
					int sliceIndexJP1 = positions.indexOf(contour1.elementAt((j+1)%contourSize1) );
					int nextSliceIndexJP1 = positions.indexOf( contour2.elementAt((j+1)%contourSize2) );
					// tri 1:
					indexList.add(sliceIndexJ);
					indexList.add(nextSliceIndexJP1);
					indexList.add(nextSliceIndexJ);
					// tri 2:
					indexList.add(sliceIndexJ);
					indexList.add(sliceIndexJP1);
					indexList.add(nextSliceIndexJP1);

					Vector3f center = new Vector3f();
					center.add(contour1.elementAt(j));
					center.add(contour2.elementAt(j));
					center.add(contour2.elementAt((j+1)%contourSize2));
					center.add(contour1.elementAt((j+1)%contourSize1));
					center.scale(0.25f);


					Vector3f edge1 = Vector3f.sub(contour1.elementAt((j+1)%contourSize1), contour1.elementAt(j));
					float dist1 = edge1.normalize();

					Vector3f edge2 = Vector3f.sub(contour2.elementAt((j+1)%contourSize2), contour1.elementAt(j));
					float dist2 = edge2.normalize();

					Vector3f edge3 = Vector3f.sub(contour2.elementAt(j), contour1.elementAt(j));
					float dist3 = edge3.normalize();

					Vector3f edge4 = Vector3f.sub(contour2.elementAt((j+1)%contourSize2), contour1.elementAt((j+1)%contourSize1));
					float dist4 = edge4.normalize();

					Vector3f edge5 = Vector3f.sub(contour2.elementAt((j+1)%contourSize2), contour2.elementAt(j));
					float dist5 = edge5.normalize();

					Vector3f normal1 = edge1.cross(edge2); normal1.normalize();
					Vector3f normal2 = edge2.cross(edge3); normal2.normalize();
					Vector3f normal = Vector3f.add(normal1, normal2);
					normal.scale(0.5f);

					Vector3f axis1 = Vector3f.add(edge1, edge5);
					axis1.scale(0.5f);
					float extent1 = (dist1 + dist5)/4f;

					Vector3f axis2 = Vector3f.add(edge3, edge4);
					axis2.scale(0.5f);
					float extent2 = (dist3 + dist4)/4f;

					Box3f box = new Box3f(center, new Vector3f[] {axis1, axis2, normal }, new float[] {extent1, extent2, 1} );
					boxes[i][j] = box;
				}
			}
		}

		clipMask = new Vector<boolean[]>();
		int boxIndex = 0;
		boolean[] currentMask = null;
		boolean[] nextMask = null;
		for ( int i = 0; i < numContours; i++ ) {
			VOIContour contour1 = (VOIContour) displayContours.getCurves().elementAt(i);
			int contourSize1 = contour1.size();
			if ( i == 0 )
			{
				currentMask = new boolean[contourSize1];
				nextMask = new boolean[contourSize1];
			}
			else
			{
				currentMask = nextMask;
				nextMask = new boolean[contourSize1];
			}
			for ( int j = 0; j < contourSize1; j++ )
			{				
				if ( i < (numContours - 1) ) {
					VOIContour contour2 = (VOIContour) displayContours.getCurves().elementAt(i+1);
					int contourSize2 = contour2.size();

					Vector3f posJ = contour1.elementAt(j);
					Vector3f posJNext = contour2.elementAt(j);
					Vector3f posJP1 = contour1.elementAt((j+1)%contourSize1);
					Vector3f posJP1Next =contour2.elementAt((j+1)%contourSize2);

					int sliceIndexJ = positions.indexOf( posJ );
					int nextSliceIndexJ = positions.indexOf( posJNext );
					int sliceIndexJP1 = positions.indexOf( posJP1 );
					int nextSliceIndexJP1 = positions.indexOf( posJP1Next );

					boolean test = false;
					//					if ( overlapImage != null ) {
					//						if (    (overlapImage.getFloat( Math.min( dimX-1, Math.max(0, (int)posJ.X)),       Math.min( dimY-1, Math.max(0, (int)posJ.Y)),        Math.min( dimZ-1, Math.max(0, (int)posJ.Z)) ) != 0 ) || 
					//								(overlapImage.getFloat( Math.min( dimX-1, Math.max(0, (int)posJNext.X)),   Math.min( dimY-1, Math.max(0, (int)posJNext.Y)),    Math.min( dimZ-1, Math.max(0, (int)posJNext.Z)) ) != 0 ) || 
					//								(overlapImage.getFloat( Math.min( dimX-1, Math.max(0, (int)posJP1.X)),     Math.min( dimY-1, Math.max(0, (int)posJP1.Y)),      Math.min( dimZ-1, Math.max(0, (int)posJP1.Z)) ) != 0 ) || 
					//								(overlapImage.getFloat( Math.min( dimX-1, Math.max(0, (int)posJP1Next.X)), Math.min( dimY-1, Math.max(0, (int)posJP1Next.Y)),  Math.min( dimZ-1, Math.max(0, (int)posJP1Next.Z)) ) != 0 ) ) {
					//
					//							vBuf.SetColor3(0, sliceIndexJ, 1, 0, 0);
					//							vBuf.SetColor3(0, nextSliceIndexJ, 1, 0, 0);
					//							vBuf.SetColor3(0, sliceIndexJP1, 1, 0, 0);
					//							vBuf.SetColor3(0, nextSliceIndexJP1, 1, 0, 0);
					//							currentMask[j] = true;
					//							currentMask[(j+1)%contourSize1] = true;
					//							nextMask[j] = true;
					//							nextMask[(j+1)%contourSize1] = true;
					//							test = true;
					//						}
					//					}
					if ( !test ) {
						Vector3f center = new Vector3f();
						center.add(contour1.elementAt(j));
						center.add(contour2.elementAt(j));
						center.add(contour2.elementAt((j+1)%contourSize2));
						center.add(contour1.elementAt((j+1)%contourSize1));
						center.scale(0.25f);

						Box3f box = boxes[i][j];
						Vector3f normal = box.Axis[2];

						Segment3f segment = new Segment3f();
						segment.Center.scaleAdd(50, normal, center);
						segment.Direction = normal;
						segment.Extent = 50;
						for ( int k = 0; k < boxes.length; k++ ) {
							if ( (k < (i-1)) || (k > (i+1)) ) 
							{
								for ( int b = 0; b < boxes[k].length; b++ ) {
									Box3f testBox = boxes[k][b];
									if ( testBox != box ) {
										IntrSegment3Box3f testSB = new IntrSegment3Box3f(segment,testBox,true);
										if ( testSB.Test() ) {
											vBuf.SetColor3(0, sliceIndexJ, 1, 0, 0);
											vBuf.SetColor3(0, nextSliceIndexJ, 1, 0, 0);
											vBuf.SetColor3(0, sliceIndexJP1, 1, 0, 0);
											vBuf.SetColor3(0, nextSliceIndexJP1, 1, 0, 0);
											currentMask[j] = true;
											currentMask[(j+1)%contourSize1] = true;
											nextMask[j] = true;
											nextMask[(j+1)%contourSize1] = true;
											break;
										}
									}
								}
							}
						}
					}
				}
			}
			for ( int j = 0; j < contourSize1; j++ )
			{				
				if ( currentMask[((j-1)+contourSize1)%contourSize1] && currentMask[(j+1)%contourSize1] ) 
				{
					currentMask[j] = true;
					nextMask[j] = true;

					if ( i < (numContours - 1) ) {
						VOIContour contour2 = (VOIContour) displayContours.getCurves().elementAt(i+1);
						int contourSize2 = contour2.size();
						//					System.err.println( i + "   " + (contourSize1 == contourSize2) );
						int sliceIndexJ = positions.indexOf(contour1.elementAt(j) );
						int nextSliceIndexJ = positions.indexOf( contour2.elementAt(j) );
						int sliceIndexJP1 = positions.indexOf(contour1.elementAt((j+1)%contourSize1) );
						int nextSliceIndexJP1 = positions.indexOf( contour2.elementAt((j+1)%contourSize2) );

						vBuf.SetColor3(0, sliceIndexJ, 1, 0, 0);
						vBuf.SetColor3(0, nextSliceIndexJ, 1, 0, 0);
						vBuf.SetColor3(0, sliceIndexJP1, 1, 0, 0);
						vBuf.SetColor3(0, nextSliceIndexJP1, 1, 0, 0);
					}
				}
			}
			clipMask.add(currentMask);
		}

		if ( overlapImage != null ) {
			overlapImage.disposeLocal(false);
			overlapImage = null;
		}

		if ( returnMesh ) {
			int[] indexInput = new int[indexList.size()];
			for ( int i = 0; i < indexList.size(); i++ ) {
				indexInput[i] = indexList.elementAt(i);
			}
			IndexBuffer iBuf = new IndexBuffer(indexInput);
			System.err.println("TriMesh " + vBuf.GetVertexQuantity() + " " + iBuf.GetIndexQuantity() );
			return new TriMesh(vBuf, iBuf);		
		}
		return null;
	}


	public String getAnnotationPrefix()
	{
//		System.err.println("getAnnotationPrefix " + annotationPrefix);
		return annotationPrefix;
	}

	public VOI getAnnotations() {
		return annotationVOIs;
	}

	public VOI getAnnotationsStraight()
	{
		return annotationsStraight;
	}
	
	public Vector<String> getSplineCurves() {
		if ( splineControlPtsList == null ) return null;
		Vector<String> curveNames = new Vector<String>();
		for ( int i = 0; i < splineControlPtsList.size(); i++ )
		{
			curveNames.add(splineControlPtsList.elementAt(i).name);
		}
		return curveNames;
	}

	public void deleteSelectedCurve()
	{
		if ( splineControlPtsList == null ) return;
		for ( int i = splineControlPtsList.size() - 1; i >= 0; i-- )
		{
			if ( splineControlPtsList.elementAt(i).selected )
			{
				AnnotationSplineControlPts splinePts = splineControlPtsList.remove(i);
				imageA.unregisterVOI(splinePts.curveVOI);
				splinePts.curveVOI.dispose();
				splinePts.curveVOI = null;
			}
		}
		updateAnnotationListeners();
		updateCurveListeners();
	}
	
	public boolean isCurveSelected(String name)
	{
		if ( splineControlPtsList == null ) return false;
		for ( int i = 0; i < splineControlPtsList.size(); i++ )
		{
			if ( name.equals(splineControlPtsList.elementAt(i).name) )
			{
				return splineControlPtsList.elementAt(i).selected;
			}
		}
		return false;
	}
	
	public void setCurveName(String oldName, String newName)
	{
		if ( splineControlPtsList == null ) return;
		for ( int i = 0; i < splineControlPtsList.size(); i++ )
		{
			if ( oldName.equals(splineControlPtsList.elementAt(i).name) )
			{
				splineControlPtsList.elementAt(i).name = new String(newName);
				splineControlPtsList.elementAt(i).curveVOI.setName(newName);
				break;
			}
		}
	}
	
	public void setCurveSelected(String name, boolean selected)
	{
		if ( splineControlPtsList == null ) return;
		for ( int i = 0; i < splineControlPtsList.size(); i++ )
		{
			if ( name.equals(splineControlPtsList.elementAt(i).name) )
			{
				splineControlPtsList.elementAt(i).selected = selected;
				for ( int j = 0; j < splineControlPtsList.elementAt(i).annotations.size(); j++ )
				{
					VOIWormAnnotation text = splineControlPtsList.elementAt(i).annotations.elementAt(j);
					text.setSelected( selected );
					text.updateSelected( imageA );
				}
				updateAnnotationListeners();
				break;
			}
		}
	}
	
	public boolean isCurveVisible(String name)
	{
		if ( splineControlPtsList == null ) return false;
		for ( int i = 0; i < splineControlPtsList.size(); i++ )
		{
			if ( name.equals(splineControlPtsList.elementAt(i).name) )
			{
				VOIContour curve = splineControlPtsList.elementAt(i).curve;
				if ( curve.getVolumeVOI() != null ) {
					return curve.getVolumeVOI().GetDisplay();
				}
				break;
			}
		}
		return false;
	}
	
	public void setCurveVisible(String name, boolean visible)
	{
		if ( splineControlPtsList == null ) return;
		for ( int i = 0; i < splineControlPtsList.size(); i++ )
		{
			if ( name.equals(splineControlPtsList.elementAt(i).name) )
			{
				VOIContour curve = splineControlPtsList.elementAt(i).curve;
				if ( curve.getVolumeVOI() != null ) {
					curve.getVolumeVOI().SetDisplay(visible);
				}
				break;
			}
		}
	}

	public int getCurrentIndex()
	{

		if ( annotationVOIs == null )
		{
			return 0;
		}
		int highestIndex = 0;
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);

			if ( !(text.getText().contains("nose") || text.getText().contains("Nose")) && !text.getText().equalsIgnoreCase("origin"))
			{
				int value = 0;
				String name = new String(text.getText());
				String prefix = JPanelAnnotations.getPrefix(name);
				if ( prefix.equals(annotationPrefix) ) {
					for ( int j = 0; j < name.length(); j++ )
					{
						if ( Character.isDigit(name.charAt(j)) )
						{
							value *= 10;
							value += Integer.valueOf(name.substring(j,j+1));
							//						System.err.println( name + " " + value + " " + name.substring(j,j+1) + " " + Integer.valueOf(name.substring(j,j+1)));
						}
					}
					highestIndex = Math.max( highestIndex, value );
				}
			}
		}

		return (highestIndex + 1);
	}

	public int getExtent()
	{
		return extent;
	}

	public ModelImage getImage()
	{
		return imageA;
	}
	
	public VOIVector getLattice() {
		return lattice;
	}

	public VOIVector getLatticeStraight()
	{
		return latticeStraight;
	}
	
	public VOIContour getCenter()
	{
		return centerPositions;
	}
	
	public int getLatticeCurveLength()
	{
		if ( centerPositions == null ) return 0;
		System.err.println("getLatticeCurveLength " + centerPositions.size());
		return centerPositions.size();
	}

	// return position
	public Vector3f getCenter(int i) {
		if ( centerPositions == null ) return null;
		return new Vector3f(centerPositions.elementAt(i));
	}
	
	private VOIWormAnnotation latticepositionMarker = null;
	public void showMarker(int i) {
		Vector3f pos = new Vector3f(centerPositions.elementAt(i));
		if ( latticepositionMarker == null ) {

			latticepositionMarker = new VOIWormAnnotation();
			latticepositionMarker.setText("latticeCenterLine");
			latticepositionMarker.add( new Vector3f(pos) );
			annotationVOIs.getCurves().add(latticepositionMarker);
		}
		latticepositionMarker.firstElement().copy(pos);
		latticepositionMarker.update();
	}
	
	public int getDiameter(int i) {
		return (int) (wormDiameters.elementAt(i).intValue());
	}
	
	// return lookat, up, right
	public Vector3f[] getBasisVectors(int i) {
		if ( centerPositions == null ) return null;
		return new Vector3f[] { 
				new Vector3f(rightVectors.elementAt(i)),
				new Vector3f(upVectors.elementAt(i)),
				new Vector3f(normalVectors.elementAt(i))};
	}

	public VOIContour getLeftCurve()
	{
		return leftPositions;
	}

	public VOIContour getRightCurve()
	{
		return rightPositions;
	}

	public VOIContour getCenterControlPoints()
	{
		return center;
	}

	public Vector<Vector3f> getNormalVectors()
	{
		return normalVectors;
	}

	public Vector<Vector3f> getRightVectors()
	{
		return rightVectors;
	}

	public Vector<Vector3f> getUpVectors()
	{
		return upVectors;
	}

	/**
	 * Returns the currently selected point, either on the lattice or from the
	 * annotation list. 
	 * @return
	 */
	public Vector3f getPicked() {
		return pickedPoint;
	}


	/**
	 * Finds the closest point to the input point and sets it as the currently selected lattice or annotation point.
	 * 
	 * @param pt
	 * @param doAnnotation
	 * @return
	 */
	public Vector3f getPicked(final Vector3f pt, final boolean doAnnotation) {
		pickedPoint = null;

		if (left == null) {
			return pickedPoint;
		}
		int closestL = -1;
		float minDistL = Float.MAX_VALUE;
		for (int i = 0; i < left.getCurves().size(); i++) {
			final float distance = pt.distance(left.getCurves().elementAt(i).elementAt(0));
			if (distance < minDistL) {
				minDistL = distance;
				if (minDistL <= 12) {
					closestL = i;
				}
			}
		}
		int closestR = -1;
		float minDistR = Float.MAX_VALUE;
		if (right != null) {
			for (int i = 0; i < right.getCurves().size(); i++) {
				final float distance = pt.distance(right.getCurves().elementAt(i).elementAt(0));
				if (distance < minDistR) {
					minDistR = distance;
					if (minDistR <= 12) {
						closestR = i;
					}
				}
			}
		}

		// System.err.println( minDistL + " " + minDistR );
		if ( (closestL != -1) && (closestR != -1)) {
			if (minDistL < minDistR) {
				// System.err.println( "Picked Lattice Left " + closestL );
				pickedPoint = left.getCurves().elementAt(closestL).elementAt(0);
			} else {
				// System.err.println( "Picked Lattice Right " + closestR );
				pickedPoint = right.getCurves().elementAt(closestR).elementAt(0);
			}
		} else if (closestL != -1) {
			// System.err.println( "Picked Lattice Left " + closestL );
			pickedPoint = left.getCurves().elementAt(closestL).elementAt(0);
		} else if (closestR != -1) {
			// System.err.println( "Picked Lattice Right " + closestR );
			pickedPoint = right.getCurves().elementAt(closestR).elementAt(0);
		}

		if (pickedPoint != null) {
			updateLattice(false);
		}

		return pickedPoint;
	}

	public Vector<VOIWormAnnotation> getPickedAnnotation() {
		if (annotationVOIs == null) {
			return null;
		}
		if (annotationVOIs.getCurves() == null) {
			return null;
		}
		Vector<VOIWormAnnotation> selectedAnnotations = null;
		for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( text.isSelected() ) {
				if ( selectedAnnotations == null ) {
					selectedAnnotations = new Vector<VOIWormAnnotation>();
				}
				selectedAnnotations.add(text);
			}
		}
		return selectedAnnotations;
	}
	
	public VOI getSamplingPlanes( boolean scale )
	{
		final short sID = (short) (imageA.getVOIs().getUniqueID());
		VOI samplingPlanes = new VOI(sID, "samplingPlanes");
		int localExtent = scale ? extent + 10 : extent;
		for (int i = 0; i < centerPositions.size(); i++) {
			final Vector3f rkEye = centerPositions.elementAt(i);
			final Vector3f rkRVector = rightVectors.elementAt(i);
			final Vector3f rkUVector = upVectors.elementAt(i);

			final Vector3f[] output = new Vector3f[4];
			final Vector3f rightV = Vector3f.scale(localExtent, rkRVector);
			final Vector3f upV = Vector3f.scale(localExtent, rkUVector);
			output[0] = Vector3f.add(Vector3f.neg(rightV), Vector3f.neg(upV));
			output[1] = Vector3f.add(rightV, Vector3f.neg(upV));
			output[2] = Vector3f.add(rightV, upV);
			output[3] = Vector3f.add(Vector3f.neg(rightV), upV);
			for (int j = 0; j < 4; j++) {
				output[j].add(rkEye);
			}
			final VOIContour kBox = new VOIContour(true);
			for (int j = 0; j < 4; j++) {
				kBox.addElement(output[j].X, output[j].Y, output[j].Z);
			}
			kBox.update(new ColorRGBA(0, 0, 1, 1));
			{
				samplingPlanes.getCurves().add(kBox);
				//				samplingPlanes.importCurve(kBox);
			}
		}
		return samplingPlanes;
	}

	public boolean hasPicked() {
		return ( (pickedPoint != null) || (getPickedAnnotation() != null) ) ;
	}

	public void initializeInterpolation(boolean saveStats) {
		latticeInterpolationInit = true;

		// The algorithm interpolates between the lattice points, creating two smooth curves from head to tail along
		// the left and right-hand sides of the worm body. A third curve down the center-line of the worm body is
		// also generated. Eventually, the center-line curve will be used to determine the number of sample points
		// along the length of the straightened worm, and therefore the final length of the straightened worm volume.
		generateCurves(1, 1);
		generateEllipses();
		if ( saveStats ) {
			saveLatticeStatistics();
		}
	}

	/**
	 * Entry point in the lattice-based straightening algorithm. At this point a lattice must be defined, outlining how
	 * the worm curves in 3D. A lattice is defined ad a VOI with two curves of equal length marking the left-hand and
	 * right-hand sides or the worm.
	 * 
	 * @param displayResult, when true intermediate volumes and results are displayed as well as the final straightened
	 *            image.
	 */
	public void interpolateLattice(final boolean displayResult, final boolean useModel, final boolean untwistImage, final boolean untwistMarkers) {

		if ( !latticeInterpolationInit ) {
			initializeInterpolation(true);
		}

		final int[] resultExtents = new int[] {((2 * extent)), ((2 * extent)), samplingPlanes.getCurves().size()};
		if ( untwistImage )
		{
			untwist(imageA, resultExtents, true);
			if ( seamCellImage != null )
			{
				untwist(seamCellImage, resultExtents, false);
			}
			untwistLattice(imageA, resultExtents);
		}
		if ( untwistMarkers && (markerCenters != null) )
		{
			//			untwistMarkers(imageA);
			//			untwistMarkers(imageA, resultExtents, true);
			untwistMarkers(imageA, resultExtents);
		}
	}

	/**
	 * Enables the user to move an annotation point with the mouse.
	 * 
	 * @param startPt 3D start point of a ray intersecting the volume.
	 * @param endPt 3D end point of a ray intersecting the volume.
	 * @param pt point along the ray with the maximum intensity value.
	 */
	public boolean modifyAnnotation( final Vector3f pt ) {
		if ( annotationVOIs == null )
		{
			return false;
		}
		if ( annotationVOIs.getCurves() == null )
		{
			return false;
		}
		if ( annotationVOIs.getCurves().size() == 0 )
		{
			return false;
		}

		boolean modified = false;
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ ) {
			final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( text.isSelected() ) {
				Vector3f diff = text.getSelectionOffset();
				
				Vector3f newPt = Vector3f.sub(pt,  diff);
				newPt.max( Vector3f.ZERO );
				newPt.min( imageDims );

				text.elementAt(0).copy(newPt);
				text.elementAt(1).copy(newPt);
				
				
				text.update();
				text.updateSelected(imageA);
				// point was modified so set the retwist flag:
				text.retwist(previewMode);
				modified = true;
				
			}
		}
		
		if ( modified ) {
			updateAnnotationListeners();
		}

		return modified;
	}

	/**
	 * Enables the user to modify the lattice point with the mouse.
	 * 
	 * @param startPt 3D start point of a ray intersecting the volume.
	 * @param endPt 3D end point of a ray intersecting the volume.
	 * @param pt point along the ray with the maximum intensity value.
	 */
	public boolean modifyLattice(final Vector3f startPt, final Vector3f endPt, final Vector3f pt) {
		if ( lattice == null )
		{
			return false;
		}

		// if preview constrain lattice points to x,0,z coordinates:
		if (pickedPoint != null) {
			if ( previewMode ) {
				final int leftIndex = findPoint(left, pickedPoint);
				final int rightIndex = findPoint(right, pickedPoint);
				if ( leftIndex != -1 ) {
					((VOIWormAnnotation)left.getCurves().elementAt(leftIndex)).retwist(true);
				}
				if ( rightIndex != -1 ) {
					((VOIWormAnnotation)right.getCurves().elementAt(rightIndex)).retwist(true);
				}
				pickedPoint.X = pt.X;
				pickedPoint.Y = pt.Y;
				pickedPoint.Z = pt.Z;
				
			}
			else {
//				pickedPoint.X = pt.X;
				pickedPoint.copy(pt);
			}
			updateLattice(false);
			return true;
		}
		return false;
	}

	/**
	 * Sets the currently selected point (lattice or annotation).
	 * 
	 * @param pt
	 * @param doAnnotation
	 */
	public void modifyLeftRightMarker(final Vector3f pt) {
		if (pickedPoint == null) {
			return;
		}
		pt.X = Math.round( pt.X );		pt.Y = Math.round( pt.Y );		pt.Z = Math.round( pt.Z );

		if ( (leftMarker != null) && pickedPoint.equals(leftMarker.getCurves().elementAt(0).elementAt(0))) {
			leftMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
			leftMarker.update();
		}
		if ( (rightMarker != null) && pickedPoint.equals(rightMarker.getCurves().elementAt(0).elementAt(0))) {
			rightMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
			rightMarker.update();
		}
		pickedPoint.copy(pt);
		updateLattice(false);
	}

	/**
	 * Enables the user to move the selected point (lattice or annotation) with the arrow keys.
	 * 
	 * @param direction
	 * @param doAnnotation
	 */
	public void moveSelectedPoint(final Vector3f direction, final boolean doAnnotation) {
		if ( doAnnotation ) {
			boolean updateImage = false;
			for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ ) {
				final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
				if ( text.isSelected() ) {
					text.elementAt(0).add(direction);
					text.elementAt(1).add(direction);
					text.update();

					text.updateSelected(imageA);
					text.retwist(previewMode);
					updateImage = true;
				}				
			}
			if ( updateImage )
			{
				imageA.notifyImageDisplayListeners();
			}
			updateAnnotationListeners();
		}
		else if ( pickedPoint != null ) {

			if ( previewMode ) {
				final int leftIndex = findPoint(left, pickedPoint);
				final int rightIndex = findPoint(right, pickedPoint);
				if ( leftIndex != -1 ) {
					((VOIWormAnnotation)left.getCurves().elementAt(leftIndex)).retwist(true);
				}
				if ( rightIndex != -1 ) {
					((VOIWormAnnotation)right.getCurves().elementAt(rightIndex)).retwist(true);
				}
//				pickedPoint.X += direction.X;
				pickedPoint.add(direction);
			}
			else {
				pickedPoint.add(direction);
			}
			updateLattice(false);
		}
	}

	/**
	 * VOI operation redo
	 */
	public void redo() {
		updateLinks();
	}


	public VOI retwistAnnotations(VOIVector lattice) {	
		
		setLattice(lattice);
		if ( annotationVOIs == null ) return null;
		for ( int i = annotationVOIs.getCurves().size() - 1; i >=0; i-- ) {

			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( text.retwist() ) {
				if ( !latticeInterpolationInit ) {
					initializeInterpolation(false);
				}
				Vector3f retwistedPt = retwistAnnotation(text);
				text.firstElement().copy(retwistedPt);
				text.lastElement().copy(retwistedPt);
				text.modified(true);
				System.err.println( "RETWIST ANNOTATIONS " + text.getText() );
			}
		}	
		return annotationVOIs;
	}

	public VOIVector retwistLattice(VOIVector lattice) {
		
		VOIVector newLattice = null;
		VOI left3D = null;
		VOI right3D = null;

		short id = (short) imageA.getVOIs().getUniqueID();
		// retwist current lattice - using the input lattice:		
		for ( int i = 0; i < left.getCurves().size(); i++ ) {			
			boolean retwistLeft = ((VOIWormAnnotation)left.getCurves().elementAt(i)).retwist();
			boolean retwistRight = ((VOIWormAnnotation)right.getCurves().elementAt(i)).retwist();
			if ( retwistLeft || retwistRight) {
				if ( newLattice == null ) {
					// create a copy of the lattice so we can change it:
					newLattice = new VOIVector();
					left3D = new VOI(id, "left", VOI.ANNOTATION, (float) Math.random());
					right3D = new VOI(id++, "right", VOI.ANNOTATION, (float) Math.random());
					newLattice.add(left3D);
					newLattice.add(right3D);
				}
			}
		}
		if ( newLattice == null ) return null;
		
		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			left3D.getCurves().add(new VOIWormAnnotation((VOIWormAnnotation)left.getCurves().elementAt(i)));
			right3D.getCurves().add(new VOIWormAnnotation((VOIWormAnnotation)right.getCurves().elementAt(i)));
		}
		
		// set the lattice:
		setLattice(lattice);
		// retwist copy (modified lattice):	
		for ( int i = 0; i < left3D.getCurves().size(); i++ ) {

			VOIWormAnnotation text = (VOIWormAnnotation) left3D.getCurves().elementAt(i);
//			System.err.print( i + "  " + text.getText() + "  " );
			if ( text.retwist() ) {
				if ( !latticeInterpolationInit ) {
					initializeInterpolation(false);
				}
				Vector3f retwistedPt = retwistAnnotation(text);
				text.clear();
				text.add(retwistedPt);
				text.modified(true);
			}
			else
			{
				text.clear();
				text.add(left.getCurves().elementAt(i).elementAt(0));
			}

			text = (VOIWormAnnotation) right3D.getCurves().elementAt(i);
//			System.err.println( text.getText() );
			if ( text.retwist() ) {
				if ( !latticeInterpolationInit ) {
					initializeInterpolation(false);
				}
				Vector3f retwistedPt = retwistAnnotation(text);
				text.clear();
				text.add(retwistedPt);
				text.modified(true);
			}
			else
			{
				text.clear();
				text.add(right.getCurves().elementAt(i).elementAt(0));
			}
		}
		
		return newLattice;
	}

	public void saveAnnotationStraight(final ModelImage image, String fileDirName, String fileName )
	{
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}	

		// Load the straightened image:
		//		FileIO fileIO = new FileIO();
		//		System.err.println( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_straight_unmasked.xml" );
		//		ModelImage resultImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_straight_unmasked.xml" );


		String voiDir;
		// Save the markers as a VOI file:
		//		resultImage.unregisterAllVOIs();
		if ( annotationsStraight.getCurves().size() > 0 )
		{
			//			resultImage.registerVOI(annotationsStraight);
			voiDir = outputDirectory + File.separator + fileDirName + File.separator;
			//			saveAllVOIsTo(voiDir, resultImage);

			LatticeModel.saveAnnotationsAsCSV(voiDir, fileName, annotationsStraight);
			//			resultImage.unregisterAllVOIs();
		}

		//		resultImage.disposeLocal(false);
		//		resultImage = null;
	}


	/**
	 * Enables the user to save the lattice to a user-selected file.
	 */
	public void saveLattice() {
		final JFileChooser chooser = new JFileChooser();

		if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
			chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
		} else {
			chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
		}

		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		final int returnVal = chooser.showSaveDialog(null);

		String fileName = null, directory = null, voiDir;
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			fileName = chooser.getSelectedFile().getName();
			directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
			Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}

		if (fileName != null) {
			voiDir = new String(directory + fileName + File.separator);

			clear3DSelection();

			VOI latticePoints = new VOI( (short)0, "lattice", VOI.ANNOTATION, 0);
			for ( int j = 0; j < left.getCurves().size(); j++ )
			{
				latticePoints.getCurves().add(left.getCurves().elementAt(j));
				latticePoints.getCurves().add(right.getCurves().elementAt(j));
			}
			LatticeModel.saveAnnotationsAsCSV(voiDir + File.separator, "lattice.csv", latticePoints);

			imageA.unregisterAllVOIs();
			
			if (leftMarker != null) {
				imageA.registerVOI(leftMarker);
			}
			if (rightMarker != null) {
				imageA.registerVOI(rightMarker);
			}
			if (annotationVOIs != null) {
				imageA.registerVOI(annotationVOIs);
			}
			updateLattice(true);
		}

	}

	/**
	 * Saves the lattice to the specified file and directory.
	 * 
	 * @param directory
	 * @param fileName
	 */
	public void saveLattice(final String directory, final String fileName)
	{
		saveLattice(directory, fileName, lattice );
	}
	
	
	public void openNeuriteCurves()
	{
		String dir = WormData.getOutputDirectory(imageA);
		String fileName = WormData.neuriteOutput;
		
		final String voiDir = new String(dir + File.separator + fileName + File.separator);
		File voiFileDir = new File(voiDir);
		
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
			final String[] list = voiFileDir.list();
			for (int i = 0; i < list.length; i++) {
				final File lrFile = new File(voiDir + list[i]);
				System.err.println( lrFile.getName() );
				if ( lrFile.getName().contains("controlPts") ) {
					VOI annotations = LatticeModel.readAnnotationsCSV(voiDir + list[i]);
					if ( annotations != null ) {
						addAnnotations(annotations);
						Vector<VOIWormAnnotation> splineControlPts = new Vector<VOIWormAnnotation>();
						for ( int j = 0; j < annotations.getCurves().size(); j++ ) {
							VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(j);
							text.setCurveAnnotation(true);
							splineControlPts.add(text);
						}
						addSplineControlPts(splineControlPts);
					}
				}
			}
		} 
	}

	public void saveNeuriteCurves()
	{
		if ( splineControlPtsList == null ) return;
		if ( splineControlPtsList.size() <= 0 ) return;
		
	
		String dir = WormData.getOutputDirectory(imageA) + File.separator;
		String fileName = WormData.neuriteOutput;

		String imageName = JDialogBase.makeImageName(imageA.getImageFileName(), "");
		
		
		final String voiDir = new String(dir + fileName + File.separator);
		for ( int i = 0; i < splineControlPtsList.size(); i++ ) {
			AnnotationSplineControlPts annotationSpline = splineControlPtsList.elementAt(i);
			
			for ( int j = 0; j < annotationSpline.annotations.size(); j++ ) {
				annotationSpline.annotations.elementAt(j).setCurveAnnotation(true);
			}

			VOI curveVOI = new VOI((short)0, annotationSpline.name );
			for ( int j = 0; j < annotationSpline.annotations.size(); j++ ) curveVOI.getCurves().add(new VOIWormAnnotation(annotationSpline.annotations.elementAt(j)) );

			LatticeModel.saveAnnotationsAsCSV(voiDir + File.separator, imageName + "_" + annotationSpline.name + "_controlPts.csv", curveVOI, true );
			LatticeModel.saveContourAsCSV( imageA, fileName, "_" + annotationSpline.name + "_spline", annotationSpline.curve );
		}
		
	}
	

	
	public void untwistNeuriteCurves(boolean useLatticeModel)
	{		
		String dir = WormData.getOutputDirectory(imageA);
		String fileName = WormData.neuriteOutput;
		
		final String voiDir = new String(dir + File.separator + fileName + File.separator);
		File voiFileDir = new File(voiDir);
		
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
			final String[] list = voiFileDir.list();
			for (int i = 0; i < list.length; i++) {
				final File lrFile = new File(voiDir + list[i]);
				System.err.println( lrFile.getName() );
				VOI annotations = LatticeModel.readAnnotationsCSV(voiDir + list[i]);
				if ( annotations != null ) {
					setMarkers(annotations);
					untwistMarkers(true);	
					saveAnnotationStraight(imageA, "straightened_neurites", "straightened_" + list[i] );	
				}
			}
		} 
	}
	
	public static void openStraightNeuriteCurves(ModelImage image) {

		String dir = WormData.getOutputDirectory(image);
		
		final String voiDir = new String(dir + File.separator + "straightened_neurites" + File.separator );
		File voiFileDir = new File(voiDir);
		
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
			final String[] list = voiFileDir.list();
			for (int i = 0; i < list.length; i++) {
				final File lrFile = new File(voiDir + list[i]);
				System.err.println( lrFile.getName() );
				if ( lrFile.getName().contains("controlPts") ) {
					VOI annotations = LatticeModel.readAnnotationsCSV(voiDir + list[i]);
					image.registerVOI(annotations);					
				}
				else {

					VOI annotations = LatticeModel.readAnnotationsCSV(voiDir + list[i]);
					VOIContour curve = new VOIContour(false);
					Vector<VOIContour> curveList = null;
					System.err.println( annotations.getCurves().size() );
					for (int j = 0; j < annotations.getCurves().size(); j++ ) {
						curve.add( annotations.getCurves().elementAt(j).elementAt(0) );
						if ( curve.size() > 1 ) {
							float dist = curve.elementAt(curve.size() -1 ).distance(curve.elementAt( curve.size() -2 ) );
							if ( dist > 5 ) // should be one voxel spacing... 
							{
//								System.err.println(dist);
								if ( curveList == null ) {
									curveList = new Vector<VOIContour>();
								}
								curveList.add(curve);
								// remove the point with the big jump:
								Vector3f lastPt = curve.lastElement();
								curve.remove( curve.lastElement() );
								curve = new VOIContour(false);
								curve.add( lastPt );
							}
						}
					}
//					System.err.println("");
//					System.err.println("");
					if ( curveList != null ) {
						curveList.add(curve);
						curve = null;
						
						int maxSize = -1;
						int maxSizeIndex = -1;
						for ( int j = 0; j < curveList.size(); j++ ) {
							if ( curveList.elementAt(j).size() > maxSize ) {
								maxSize = curveList.elementAt(j).size();
								maxSizeIndex = j;
							}
						}
						if ( maxSizeIndex != -1 ) {
							VOIContour newCurve = new VOIContour(false);
							newCurve.addAll(curveList.elementAt(maxSizeIndex));
//							System.err.println( newCurve.size() );
							for ( int j = maxSizeIndex + 1; j < curveList.size(); j++ ) {
//								System.err.println( curveList.elementAt(j).size() );
								float skipDist = newCurve.lastElement().distance( curveList.elementAt(j).firstElement());
//								System.err.println(skipDist);
								if ( skipDist < 20 ) {
									newCurve.addAll( curveList.elementAt(j) );
								}
							}
							for ( int j = maxSizeIndex - 1; j >= 0; j-- ) {
//								System.err.println( curveList.elementAt(j).size() );
								float skipDist = curveList.elementAt(j).lastElement().distance( newCurve.firstElement());
//								System.err.println(skipDist);
								if ( skipDist < 20 ) {
									for ( int k = 0; k < curveList.elementAt(j).size(); k++ ) {
										newCurve.add(k, curveList.elementAt(j).elementAt(k) );
									}
								}
							}
//							System.err.println("");
//							System.err.println("");
//							System.err.println( newCurve.size() );
							curve = newCurve;
						}
					}
					if ( curve != null ) {
						VOI curveVOI = new VOI((short)0, list[i]);
						curveVOI.getCurves().add(curve);
						image.registerVOI(curveVOI);
					}
				}
			}
		} 
	}
	

	public ModelImage segmentLattice(final ModelImage image, boolean saveContourImage, int paddingFactor, boolean segmentLattice )
	{
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		String voiDir = outputDirectory + File.separator + "contours" + File.separator;
		final File voiFileDir = new File(voiDir);

		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
			final String[] list = voiFileDir.list();
			for (int i = 0; i < list.length; i++) {
				final File lrFile = new File(voiDir + list[i]);
				lrFile.delete();
			}
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "segmentLattice" + voiDir);
			voiFileDir.mkdir();
		}

		System.err.println( "Segment Lattice " + paddingFactor );
		long time = System.currentTimeMillis();

		FileIO fileIO = new FileIO();

		ModelImage resultImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_straight_unmasked.xml" );
		int dimZ = resultImage.getExtents().length > 2 ? resultImage.getExtents()[2] : 1;

		ModelImage contourImage = new ModelImage( ModelStorageBase.FLOAT, resultImage.getExtents(), imageName + "_straight_contour.xml" );
		contourImage.setResolutions( resultImage.getResolutions(0) );
		System.err.println( "   new images " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();

		int dimX = (resultImage.getExtents()[0]);
		int dimY = (resultImage.getExtents()[1]);
		Vector3f center = new Vector3f( dimX/2, dimY/2, 0 );
		float maxDist = Vector3f.normalize(center);

		//		System.err.println( dimX + " " + dimY + " " + dimZ );

		if ( !segmentLattice ) {
			generateTriMesh( false, 1);
		}
		latticeContours = new VOI( (short)1, "contours", VOI.POLYLINE, (float) Math.random());
		resultImage.registerVOI( latticeContours );
		for (int i = 0; i < dimZ; i++)
		{			
			VOIContour contour = new VOIContour(true);


			float radius = (float) (1.05 * rightPositions.elementAt(i).distance(leftPositions.elementAt(i))/(2f));
			radius += paddingFactor;
			//			System.err.println( i + "  " + radius );
			makeEllipse2DA(Vector3f.UNIT_X, Vector3f.UNIT_Y, center, radius, contour);			

			if ( !segmentLattice && (clipMask != null) && (clipMask.size() == dimZ) ) {
				//				System.err.println( "use clip mask " + (i/10) + "  " + clipMask.size() );
				boolean[] mask = clipMask.elementAt(i);
				for ( int j = 0; j < mask.length; j++ ) {
					if ( !mask[j] ) {
						// extend contour out to edge:
						Vector3f dir = Vector3f.sub( contour.elementAt(j), center );
						dir.normalize();
						dir.scale(maxDist);
						dir.add(center);
						contour.elementAt(j).copy(dir);
					}
				}
				for ( int y = 0; y < dimY; y++ ) {
					for ( int x = 0; x < dimX; x++ ) {
						if ( contour.contains(x,y) ) {
							contourImage.set(x,  y, i, 10 );
						}
					}
				}
			}
			else
			{
				float radiusSq = radius*radius;
				for ( int y = (int)Math.max(0, center.Y - radius); y < Math.min(dimY, center.Y + radius + 1); y++ )
				{
					for ( int x = (int)Math.max(0, center.X - radius); x < Math.min(dimX, center.X + radius + 1); x++ )
					{
						float dist = (x - center.X) * (x - center.X) + (y - center.Y) * (y - center.Y); 
						if ( dist <= radiusSq )
						{
							contourImage.set(x,  y, i, 10 );
						}
					}
				}
			}
			for ( int j = 0; j < contour.size(); j++ )
			{
				contour.elementAt(j).Z = i;
			}
			latticeContours.getCurves().add( contour );
		}

		System.err.println( "   contours " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();

		// Optional VOI interpolation & smoothing:
		ModelImage contourImageBlur = WormSegmentation.blur(contourImage, 3);
		System.err.println( "   blur " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
		contourImage.disposeLocal(false);
		contourImage = null;

		for (int z = 0; z < dimZ; z++)
		{			
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( contourImageBlur.getFloat(x,y,z) <= 1 )
					{
						if ( resultImage.isColorImage() )
						{
							resultImage.setC(x, y, z, 0, 0);	
							resultImage.setC(x, y, z, 1, 0);	
							resultImage.setC(x, y, z, 2, 0);	
							resultImage.setC(x, y, z, 3, 0);							
						}
						else
						{
							resultImage.set(x, y, z, 0);
						}
					}
				}
			}			
		}
		System.err.println( "   clip images " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();

		resultImage.setImageName( imageName + "_straight.xml" );
		saveImage(imageName, resultImage, true);

		// Save the contour vois to file.
		voiDir = outputDirectory + File.separator + "contours" + File.separator;
		saveAllVOIsTo(voiDir, resultImage);
		resultImage.unregisterAllVOIs();
		System.err.println( "   save vois " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();

		resultImage.disposeLocal(false);
		resultImage = null;

		if ( saveContourImage ) {
			contourImageBlur.setImageName( imageName + "_contours.xml" );
			saveImage(imageName, contourImageBlur, false);
			System.err.println( "   save image " + AlgorithmBase.computeElapsedTime(time) );
			time = System.currentTimeMillis();
			return contourImageBlur;
		}
		contourImageBlur.disposeLocal(false);
		contourImageBlur = null;
		return null;
	}

	public void segmentLattice(final ModelImage image, final ModelImage contourImageBlur) {

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		FileIO fileIO = new FileIO();
		ModelImage resultImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_straight_unmasked.xml" );
		int dimX = resultImage.getExtents().length > 0 ? resultImage.getExtents()[0] : 1;
		int dimY = resultImage.getExtents().length > 1 ? resultImage.getExtents()[1] : 1;
		int dimZ = resultImage.getExtents().length > 2 ? resultImage.getExtents()[2] : 1;

		for (int z = 0; z < dimZ; z++)
		{			
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( contourImageBlur.getFloat(x,y,z) <= 1 )
					{
						if ( resultImage.isColorImage() )
						{
							resultImage.setC(x, y, z, 0, 0);	
							resultImage.setC(x, y, z, 1, 0);	
							resultImage.setC(x, y, z, 2, 0);	
							resultImage.setC(x, y, z, 3, 0);							
						}
						else
						{
							resultImage.set(x, y, z, 0);
						}
					}
				}
			}			
		}
		resultImage.setImageName( imageName + "_straight.xml" );
		saveImage(imageName, resultImage, true);
		resultImage.disposeLocal(false);
		resultImage = null;		
	}

	/**
	 * As part of growing the contours to fit the worm model, this function checks that all annotations are included in
	 * the current version of the model.
	 * 
	 * @param model
	 * @return
	 */
	//	private boolean checkAnnotations(final ModelImage model) {
	//		return checkAnnotations(model, false);
	//	}

	//	private void shiftLattice()
	//	{	
	//
	//		// save the original lattice into a backup in case the lattice
	//		// is modified to better fit the fluorescent marker segmentation:
	//		leftBackup = new VOIContour(false);
	//		rightBackup = new VOIContour(false);
	//		for (int i = 0; i < left.size(); i++) {
	//			leftBackup.add(new Vector3f(left.elementAt(i)));
	//			rightBackup.add(new Vector3f(right.elementAt(i)));
	//		}
	//		
	//		if ( latticeShifted || (seamCellImage == null) )
	//		{
	//			return;
	//		}
	//
	//		latticeShifted = true;
	//		seamCellIDs = new int[left.size()][2];
	//		for ( int i = 0; i < left.size(); i++ )
	//		{
	//			Vector3f leftPt = left.elementAt(i);
	//			int leftID = seamCellImage.getInt( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
	//			seamCellIDs[i][0] = leftID;
	//			Vector3f rightPt = right.elementAt(i);
	//			int rightID = seamCellImage.getInt( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
	//			seamCellIDs[i][1] = rightID;
	//
	//			if ( (leftID != 0) || (rightID != 0) )
	//			{
	//				Vector3f centerPt = Vector3f.add(leftPt, rightPt);
	//				centerPt.scale(0.5f);
	//				Vector3f leftDir = Vector3f.sub(leftPt, centerPt);   leftDir.normalize();
	//				Vector3f rightDir = Vector3f.sub(rightPt, centerPt); rightDir.normalize();
	//
	//				int newLeftID = leftID;
	//				while ( (leftID != newLeftID) && (newLeftID != 0) )
	//				{
	//					leftPt.add(leftDir);
	//					newLeftID = seamCellImage.getInt( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
	//				}
	//				if ( leftID != 0 )
	//				{
	//					leftPt.sub(leftDir);
	//				}
	//				int newRightID = leftID;
	//				while ( (rightID != newRightID) && (newRightID != 0) )
	//				{
	//					rightPt.add(rightDir);
	//					newRightID = seamCellImage.getInt( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
	//				}
	//				if ( rightID != 0 )
	//				{
	//					rightPt.sub(rightDir);
	//				}
	//			}
	//			if ( leftID == 0 )
	//			{
	//				Vector3f centerPt = Vector3f.add(leftPt, rightPt);
	//				centerPt.scale(0.5f);
	//				Vector3f leftDir = Vector3f.sub(leftPt, centerPt);
	//				float length = leftDir.normalize();
	//				int newLeftID = leftID;
	//				for ( int j = 1; j < length; j++ )
	//				{
	//					centerPt.add(leftDir);
	//					newLeftID = seamCellImage.getInt( (int)centerPt.X, (int)centerPt.Y, (int)centerPt.Z );
	//					if ( newLeftID != 0 )
	//					{
	//						seamCellIDs[i][0] = newLeftID;
	//						break;
	//					}
	//				}						
	//			}
	//			if ( rightID == 0 )
	//			{
	//				Vector3f centerPt = Vector3f.add(leftPt, rightPt);
	//				centerPt.scale(0.5f);
	//				Vector3f rightDir = Vector3f.sub(rightPt, centerPt);
	//				float length = rightDir.normalize();
	//				int newRightID = rightID;
	//				for ( int j = 1; j < length; j++ )
	//				{
	//					centerPt.add(rightDir);
	//					newRightID = seamCellImage.getInt( (int)centerPt.X, (int)centerPt.Y, (int)centerPt.Z );
	//					if ( newRightID != 0 )
	//					{
	//						seamCellIDs[i][1] = newRightID;
	//						break;
	//					}
	//				}	
	//			}
	//			//					System.err.println( "Left " + i + " " + seamCellIDs[i][0] );
	//			//					System.err.println( "Right " + i + " " + seamCellIDs[i][1] );
	//		}
	//	}

	//	protected boolean checkAnnotations(final ModelImage model, final boolean print) {
	//		boolean outsideFound = false;
	//		for (int i = 0; i < left.size(); i++) {
	//			Vector3f position = left.elementAt(i);
	//			int x = Math.round(position.X);
	//			int y = Math.round(position.Y);
	//			int z = Math.round(position.Z);
	//			int value = model.getInt(x, y, z);
	//			// float value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
	//			if (value == 0) {
	//				outsideFound = true;
	//			}
	//			position = right.elementAt(i);
	//			x = Math.round(position.X);
	//			y = Math.round(position.Y);
	//			z = Math.round(position.Z);
	//			value = model.getInt(x, y, z);
	//			// value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
	//			if (value == 0) {
	//				outsideFound = true;
	//			}
	//		}
	//
	//		if (annotationVOIs == null) {
	//			return !outsideFound;
	//			// return true;
	//		}
	//		if (annotationVOIs.getCurves().size() == 0) {
	//			return !outsideFound;
	//			// return true;
	//		}
	//
	//		// outsideFound = false;
	//		for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
	//			final VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(i);
	//			final Vector3f position = text.elementAt(0);
	//			final int x = Math.round(position.X);
	//			final int y = Math.round(position.Y);
	//			final int z = Math.round(position.Z);
	//			final int value = model.getInt(x, y, z);
	//			if ( print )
	//			{
	//				System.err.println( text.getText() + " " + position + "  " + value );
	//			}
	//			if (value == 0) {
	//				outsideFound = true;
	//			}
	//		}
	//		return !outsideFound;
	//	}

	public boolean selectAnnotation(final Vector3f startPt, final Vector3f endPt, final Vector3f pt, boolean rightMouse, boolean multiSelect ) {
		if ( annotationVOIs == null )
		{
			return false;
		}
		if ( annotationVOIs.getCurves() == null )
		{
			return false;
		}
		if ( annotationVOIs.getCurves().size() == 0 )
		{
			return false;
		}
		VOIWormAnnotation nearest = findNearestAnnotation(startPt, endPt, pt);
//		System.err.println("selectAnnotation " + nearest );
		if ( nearest == null ) {
			return false;
		}
		boolean repeat = false;
		if ( !rightMouse ) {
			// toggle nearest selection:
			nearest.setSelected( !nearest.isSelected() );
			nearest.updateSelected(imageA);
			for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
			{
				VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
				if ( text != nearest ) {
					// set selection to false of not using multiSelect
					text.setSelected( text.isSelected() && multiSelect );
					text.updateSelected(imageA);
				}
				if ( text.isSelected() ) {
					text.setSelectionOffset( Vector3f.sub(pt, text.elementAt(0)) );
				}
			}
			imageA.notifyImageDisplayListeners();
			updateAnnotationListeners();
			return true;
		}
		if ( rightMouse ) {

			Vector<VOIWormAnnotation> selectedAnnotations = getPickedAnnotation();
			if ( selectedAnnotations == null ) {
				return true;
			}

			if ( nearest.isSelected() && (selectedAnnotations.size() == 1) )
			{
				// rename add notes, etc.:
				String oldName = new String(nearest.getText());
				new JDialogAnnotation(imageA, annotationVOIs, annotationVOIs.getCurves().indexOf(nearest), true, true);
				String newName = new String(nearest.getText());
				for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ ) {
					if ( annotationVOIs.getCurves().elementAt(i) != nearest ) {
						VOIWormAnnotation current = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
						if ( current.getText().contentEquals(newName) ) {
							MipavUtil.displayError( "Name already exists " + newName );
							nearest.setText(oldName);
							repeat = true;
						}
					}
				}
				nearest.updateText();
				colorAnnotations();
			}
			else if ( selectedAnnotations.size() > 1 )
			{
				// open dialog to group annotations
				String groupName = JOptionPane.showInputDialog("Create group: ");
				if ( groupName != null ) {
					for ( int i = 0; i < selectedAnnotations.size(); i++ ) {
						VOIWormAnnotation text = selectedAnnotations.elementAt(i);
						text.setText( groupName + JPanelAnnotations.getPostfix(text.getText() ) );
						text.updateText();
					}
				}
			}
			if ( repeat ) {
				return selectAnnotation(startPt, endPt, pt, rightMouse, multiSelect);
			}
			imageA.notifyImageDisplayListeners();
			updateAnnotationListeners();
			return true;
		}

		return false;
	}

	public boolean selectLattice(final Vector3f startPt, final Vector3f endPt, final Vector3f pt, boolean isSeam) {
		if ( (lattice == null) || (left == null) || (right == null) ) {
			return false;
		}
		pickedPoint = null;
		int closestL = -1;
		float minDistL = Float.MAX_VALUE;
		for (int i = 0; i < left.getCurves().size(); i++) {
			final float distance = pt.distance(left.getCurves().elementAt(i).elementAt(0));
			if (distance < minDistL) {
				minDistL = distance;
				if (minDistL <= 12) {
					closestL = i;
				}
			}
		}
		int closestR = -1;
		float minDistR = Float.MAX_VALUE;
		for (int i = 0; i < right.getCurves().size(); i++) {
			final float distance = pt.distance(right.getCurves().elementAt(i).elementAt(0));
			if (distance < minDistR) {
				minDistR = distance;
				if (minDistR <= 12) {
					closestR = i;
				}
			}
		}
		// System.err.println( minDistL + " " + minDistR );
		if ( (closestL != -1) && (closestR != -1)) {
			if (minDistL < minDistR) {
				// System.err.println( "Picked Lattice Left " + closestL );
				pickedPoint = left.getCurves().elementAt(closestL).elementAt(0);
			} else {
				// System.err.println( "Picked Lattice Right " + closestR );
				pickedPoint = right.getCurves().elementAt(closestR).elementAt(0);
			}
		} else if (closestL != -1) {
			// System.err.println( "Picked Lattice Left " + closestL );
			pickedPoint = left.getCurves().elementAt(closestL).elementAt(0);
		} else if (closestR != -1) {
			// System.err.println( "Picked Lattice Right " + closestR );
			pickedPoint = right.getCurves().elementAt(closestR).elementAt(0);
		}
		if (pickedPoint != null) {
			updateLattice(false);
			return true;
		}
		// look at the vector under the mouse and see which lattice point is closest...
		final Segment3f mouseVector = new Segment3f(startPt, endPt);
		float minDist = Float.MAX_VALUE;
		for (int i = 0; i < left.getCurves().size(); i++) {
			DistanceVector3Segment3 dist = new DistanceVector3Segment3(left.getCurves().elementAt(i).elementAt(0), mouseVector);
			float distance = dist.Get();
			if (distance < minDist) {
				minDist = distance;
				pickedPoint = left.getCurves().elementAt(i).elementAt(0);
			}
		}
		for ( int i = 0; i < right.getCurves().size(); i++ ) {
			DistanceVector3Segment3 dist = new DistanceVector3Segment3(right.getCurves().elementAt(i).elementAt(0), mouseVector);
			float distance = dist.Get();
			if (distance < minDist) {
				minDist = distance;
				pickedPoint = right.getCurves().elementAt(i).elementAt(0);
			}
		}
		if ( (pickedPoint != null) && (minDist <= 12)) {
			updateLattice(false);
			return true;
		}
		if ( !previewMode ) {
			return addInsertionPoint(startPt, endPt, pt, isSeam);
		}
		else {
			return false;
		}
	}

	public void setAnnotationPrefix(String s)
	{
		annotationPrefix = s;
	}

	/**
	 * Called when new annotations are loaded from file, replaces current annotations.
	 * 
	 * @param newAnnotations
	 */
	public void setAnnotations(final VOI newAnnotations) {
		if (annotationVOIs != null) {
			imageA.unregisterVOI(annotationVOIs);
			if ( (annotationsStraight != null) && imageA.isRegistered(annotationsStraight) != -1 ) {
				imageA.unregisterVOI(annotationsStraight);
			}
		}
		clear3DSelection();

		if (showSelected != null) {
			for (int i = 0; i < showSelected.length; i++) {
				showSelected[i].dispose();
			}
			showSelected = null;
		}
		showSelectedVOI = null;
		clearAddLeftRightMarkers();		

		annotationVOIs = newAnnotations;
		if ( annotationVOIs == null )
		{
			return;
		}
		
		annotationVOIs.setName("annotationVOIs");
		if ( imageA.isRegistered(annotationVOIs) == -1 ) {
			imageA.registerVOI(annotationVOIs);
		}
		for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
			final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			//			text.setColor(Color.blue);
			final Color c = text.getColor();
			//			System.err.println( text.getText() + "  " + text.getColor() );
			text.update(new ColorRGBA(c.getRed() / 255.0f, c.getGreen() / 255.0f, c.getBlue() / 255.0f, 1f));
			//			text.elementAt(1).add(6, 0, 0);
			text.setUseMarker(false);

			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				if (wormOrigin == null) {
					wormOrigin = new Vector3f(text.elementAt(0));
					// updateLattice(true);
				} else {
					wormOrigin.copy(text.elementAt(0));
					// updateLattice(false);
				}
			}
		}
		colorAnnotations();
		updateAnnotationListeners();
	}

	/**
	 * Called when new annotations are loaded from file, replaces current annotations.
	 * 
	 * @param newAnnotations
	 */
	public void addAnnotations(final VOI newAnnotations) {
		if ( newAnnotations.getCurves().size() == 0 ) return;
		if (annotationVOIs == null) {
			annotationVOIs = newAnnotations;
			annotationVOIs.setName("annotationVOIs");
			imageA.registerVOI(annotationVOIs);
		}
		else {
			annotationVOIs.getCurves().addAll( newAnnotations.getCurves() );
		}
		clear3DSelection();

		if (showSelected != null) {
			for (int i = 0; i < showSelected.length; i++) {
				showSelected[i].dispose();
			}
			showSelected = null;
		}
		showSelectedVOI = null;
		clearAddLeftRightMarkers();		

		if ( annotationVOIs == null )
		{
			return;
		}
		if ( imageA.isRegistered(annotationVOIs) == -1 ) {
			imageA.registerVOI(annotationVOIs);
		}
		for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
			final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			//			text.setColor(Color.blue);
			final Color c = text.getColor();
			//			System.err.println( text.getText() + "  " + text.getColor() );
			text.update(new ColorRGBA(c.getRed() / 255.0f, c.getGreen() / 255.0f, c.getBlue() / 255.0f, 1f));
			text.elementAt(1).copy(text.elementAt(0));
			//			text.elementAt(1).add(6, 0, 0);
			text.setUseMarker(false);

			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				if (wormOrigin == null) {
					wormOrigin = new Vector3f(text.elementAt(0));
					// updateLattice(true);
				} else {
					wormOrigin.copy(text.elementAt(0));
					// updateLattice(false);
				}
			}
		}
		colorAnnotations();
		updateAnnotationListeners();
	}

	
	
	/**
	 * Change the underlying image for the latticeModel. Update the output directories.
	 * @param image
	 */
	public void setImage(ModelImage image)
	{
		imageA = image;
		if ( imageA != null )
		{
			String imageName = imageA.getImageName();
			if (imageName.contains("_clone")) {
				imageName = imageName.replaceAll("_clone", "");
			}
			outputDirectory = new String(imageA.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
			String parentDir = new String(imageA.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator);
			checkParentDir(parentDir);			
			
			imageDims = new Vector3f( imageA.getExtents()[0] - 1, imageA.getExtents()[1] - 1, imageA.getExtents()[2] - 1 );
		}
	}

	/**
	 * Creates the worm model based on the segmented left-right marker images and the current lattice and natural
	 * splines fitting the lattice.
	 * 
	 * @param imageA
	 * @param samplingPlanes
	 * @param ellipseBounds
	 * @param diameters
	 * @param diameter
	 * @param straighten
	 * @param displayResult
	 */
	//	private void createWormModel(final ModelImage imageA, final VOI samplingPlanes, final Vector<Ellipsoid3f> ellipseBounds, final Vector<Float> diameters,
	//			final int diameter, final boolean displayResult) {
	//		final int[] resultExtents = new int[] {diameter, diameter, samplingPlanes.getCurves().size()};
	//
	//		String imageName = imageA.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//		ModelImage model = new ModelImage(ModelStorageBase.INTEGER, imageA.getExtents(), imageName + "_model.xml");
	//		JDialogBase.updateFileInfo(imageA, model);
	//
	//		ModelImage insideConflict = new ModelImage(ModelStorageBase.BOOLEAN, imageA.getExtents(), imageName + "_insideConflict.xml");
	//		JDialogBase.updateFileInfo(imageA, insideConflict);
	//
	//		// 7. The set of ellipses from the head of the worm to the tail defines an approximate outer boundary of the
	//		// worm in 3D.
	//		// The centers of each ellipse are spaced one voxel apart along the center line curve of the worm, and each
	//		// ellipse
	//		// corresponds to a single output slice in the final straightened image. This step generates a model of the worm
	//		// where each voxel that falls within one of the ellipses is labeled with the corresponding output slice value.
	//		// Voxels where multiple ellipses intersect are labeled as conflict voxels. Once all ellipses have been
	//		// evaluated,
	//		// the conflict voxels are removed from the model.
	//		final int dimX = imageA.getExtents().length > 0 ? imageA.getExtents()[0] : 1;
	//		final int dimY = imageA.getExtents().length > 1 ? imageA.getExtents()[1] : 1;
	//		final int dimZ = imageA.getExtents().length > 2 ? imageA.getExtents()[2] : 1;
	//		for (int z = 0; z < dimZ; z++) {
	//			for (int y = 0; y < dimY; y++) {
	//				for (int x = 0; x < dimX; x++) {
	//					model.set(x, y, z, 0);
	//					insideConflict.set(x, y, z, false);
	//				}
	//			}
	//		}
	//
	//		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
	//			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	//			final Vector3f[] corners = new Vector3f[4];
	//			for (int j = 0; j < 4; j++) {
	//				corners[j] = kBox.elementAt(j);
	//			}
	//
	//			float planeDist = -Float.MAX_VALUE;
	//			if (i < (samplingPlanes.getCurves().size() - 1)) {
	//				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//				for (int j = 0; j < 4; j++) {
	//					final float distance = corners[j].distance(kBox.elementAt(j));
	//					if (distance > planeDist) {
	//						planeDist = distance;
	//					}
	//				}
	//			}
	//
	//			if (i < (samplingPlanes.getCurves().size() - 1)) {
	//				planeDist *= 3;
	//				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//				final Vector3f[] steps = new Vector3f[4];
	//				final Vector3f[] cornersSub = new Vector3f[4];
	//				for (int j = 0; j < 4; j++) {
	//					steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
	//					steps[j].scale(1f / planeDist);
	//					cornersSub[j] = new Vector3f(corners[j]);
	//				}
	//				for (int j = 0; j < planeDist; j++) {
	//					initializeModelandConflicts(imageA, model, insideConflict, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i),
	//							1.5f * diameters.elementAt(i), boxBounds.elementAt(i), i + 1);
	//					for (int k = 0; k < 4; k++) {
	//						cornersSub[k].add(steps[k]);
	//					}
	//				}
	//			} else {
	//				planeDist = 15;
	//				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
	//				final Vector3f[] steps = new Vector3f[4];
	//				final Vector3f[] cornersSub = new Vector3f[4];
	//				for (int j = 0; j < 4; j++) {
	//					steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
	//					steps[j].scale(1f / planeDist);
	//					// cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
	//					cornersSub[j] = new Vector3f(corners[j]);
	//				}
	//				for (int j = 0; j < planeDist; j++) {
	//					initializeModelandConflicts(imageA, model, insideConflict, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i),
	//							1.5f * diameters.elementAt(i), boxBounds.elementAt(i), i + 1);
	//					for (int k = 0; k < 4; k++) {
	//						cornersSub[k].add(steps[k]);
	//					}
	//				}
	//			}
	//		}
	//
	//		for (int z = 0; z < dimZ; z++) {
	//			for (int y = 0; y < dimY; y++) {
	//				for (int x = 0; x < dimX; x++) {
	//					if (insideConflict.getBoolean(x, y, z)) {
	//						model.set(x, y, z, 0);
	//					}
	//				}
	//			}
	//		}
	//		insideConflict.disposeLocal(true);
	//		insideConflict = null;
	//
	//		// Save the marker segmentation image:
	//		//		saveImage(imageName, markerSegmentation, true);
	//		//		markerImageToColor(markerSegmentation, displayResult);
	//
	//		// 8. The marker segmentation image is used to resolve conflicts where multiple ellipses overlap.
	//		// Each slice in the output image should extend only to the edges of the left-right markers for the
	//		// corresponding region of the worm volume. This prevents a slice from extending beyond the worm boundary and
	//		// capturing the
	//		// adjacent fold of worm. Because the marker segmentation image only segments the left-right markers it is not
	//		// possible
	//		// to resolve all potential conflicts.
	//
	//		// Calculate which slice IDs correspond to which segmented markers:
	//		final float[] sliceIDs = new float[samplingPlanes.getCurves().size()];
	//		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
	//			sliceIDs[i] = 0;
	//			final VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	//			final Vector3f[] corners = new Vector3f[4];
	//			for (int j = 0; j < 4; j++) {
	//				corners[j] = kBox.elementAt(j);
	//			}
	//			mapSliceIDstoMarkerIDs(model, markerSegmentation, sliceIDs, markerIDs, completedIDs, currentID, 0, i, resultExtents, corners,
	//					ellipseBounds.elementAt(i));
	//		}
	//
	//		// Fill in the marker segmentation image with the corresponding slice IDs:
	//		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
	//			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	//			final Vector3f[] corners = new Vector3f[4];
	//			for (int j = 0; j < 4; j++) {
	//				corners[j] = kBox.elementAt(j);
	//			}
	//
	//			float planeDist = -Float.MAX_VALUE;
	//			if (i < (samplingPlanes.getCurves().size() - 1)) {
	//				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//				for (int j = 0; j < 4; j++) {
	//					final float distance = corners[j].distance(kBox.elementAt(j));
	//					if (distance > planeDist) {
	//						planeDist = distance;
	//					}
	//				}
	//			}
	//
	//			if (sliceIDs[i] != 0) {
	//				if (i < (samplingPlanes.getCurves().size() - 1)) {
	//					planeDist *= 3;
	//					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//					final Vector3f[] steps = new Vector3f[4];
	//					final Vector3f[] cornersSub = new Vector3f[4];
	//					for (int j = 0; j < 4; j++) {
	//						steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
	//						steps[j].scale(1f / planeDist);
	//						cornersSub[j] = new Vector3f(corners[j]);
	//					}
	//					for (int j = 0; j < planeDist; j++) {
	//						fillMarkerSegmentationImage( markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i));
	//						for (int k = 0; k < 4; k++) {
	//							cornersSub[k].add(steps[k]);
	//						}
	//					}
	//				} else {
	//					planeDist = 15;
	//					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
	//					final Vector3f[] steps = new Vector3f[4];
	//					final Vector3f[] cornersSub = new Vector3f[4];
	//					for (int j = 0; j < 4; j++) {
	//						steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
	//						steps[j].scale(1f / planeDist);
	//						cornersSub[j] = new Vector3f(corners[j]);
	//					}
	//					for (int j = 0; j < planeDist; j++) {
	//						fillMarkerSegmentationImage( markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i));
	//						for (int k = 0; k < 4; k++) {
	//							cornersSub[k].add(steps[k]);
	//						}
	//					}
	//				}
	//			}
	//		}
	//
	//		// if ( displayResult )
	//		// {
	//		// markerSegmentation.calcMinMax();
	//		// new ViewJFrameImage((ModelImage)markerSegmentation.clone());
	//		// }
	//
	//		// resolve conflicts in the model with the marker segmentation image:
	//		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
	//			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	//			final Vector3f[] corners = new Vector3f[4];
	//			for (int j = 0; j < 4; j++) {
	//				corners[j] = kBox.elementAt(j);
	//			}
	//
	//			float planeDist = -Float.MAX_VALUE;
	//			if (i < (samplingPlanes.getCurves().size() - 1)) {
	//				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//				for (int j = 0; j < 4; j++) {
	//					final float distance = corners[j].distance(kBox.elementAt(j));
	//					if (distance > planeDist) {
	//						planeDist = distance;
	//					}
	//				}
	//			}
	//
	//			if (sliceIDs[i] != 0) {
	//				if (i < (samplingPlanes.getCurves().size() - 1)) {
	//					planeDist *= 3;
	//					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//					final Vector3f[] steps = new Vector3f[4];
	//					final Vector3f[] cornersSub = new Vector3f[4];
	//					for (int j = 0; j < 4; j++) {
	//						steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
	//						steps[j].scale(1f / planeDist);
	//						cornersSub[j] = new Vector3f(corners[j]);
	//					}
	//					for (int j = 0; j < planeDist; j++) {
	//						resolveModelConflicts(model, markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, i + 1);
	//						for (int k = 0; k < 4; k++) {
	//							cornersSub[k].add(steps[k]);
	//						}
	//					}
	//				} else {
	//					planeDist = 15;
	//					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
	//					final Vector3f[] steps = new Vector3f[4];
	//					final Vector3f[] cornersSub = new Vector3f[4];
	//					for (int j = 0; j < 4; j++) {
	//						steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
	//						steps[j].scale(1f / planeDist);
	//						cornersSub[j] = new Vector3f(corners[j]);
	//					}
	//					for (int j = 0; j < planeDist; j++) {
	//						resolveModelConflicts(model, markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, i + 1);
	//						for (int k = 0; k < 4; k++) {
	//							cornersSub[k].add(steps[k]);
	//						}
	//					}
	//				}
	//			}
	//		}
	//
	//		// 9. The last step is an attempt to ensure that as much of the worm data is captured by the algorithm as
	//		// possible.
	//		// Using the marker segmentation image where possible as a guide to the worm boundary, each slice of worm model
	//		// is
	//		// grown outward. The points on the boundary are expanded in an iterative process until the point comes in
	//		// contact
	//		// with another edge of the worm. For areas of the worm where it folds back on itself this results in a
	//		// flattened
	//		// cross-section where the folds press against each other.
	//		// For areas of the worm where the cross-section does not contact other sections of the worm the 2D contour
	//		// extends
	//		// outward until it reaches the edge of the sample plane, capturing as much data as possible.
	//		int growStep = 0;
	//		int max = (int) (resultExtents[0] / 3);
	//		System.err.println( max );
	//		while ( (growStep < 25) && ( !checkAnnotations(model) || (growStep < 20)))
	//		{
	//			//			for (int z = 0; z < dimZ; z++) {
	//			//				for (int y = 0; y < dimY; y++) {
	//			//					for (int x = 0; x < dimX; x++) {
	//			//						if (model.getFloat(x, y, z) != 0) {
	//			//							inside.set(x, y, z, 1);
	//			//						}
	//			//						else
	//			//						{
	//			//							inside.set(x, y, z, 0);
	//			//						}
	//			//					}
	//			//				}
	//			//			}
	//			//			inside.setImageName( imageName + "_" + growStep + "_insideMask.xml" );
	//			//			saveImage(imageName, inside, false, "masks");
	//
	//
	//			growEdges(maskImage, model, markerSegmentation, sliceIDs, growStep++);
	//
	//
	//		}
	//		markerSegmentation.disposeLocal();
	//		markerSegmentation = null;
	//
	//		System.err.println("    generateMasks " + growStep );
	//		if ( !checkAnnotations(model)) {
	//			System.err.println("    generateMasks " + growStep + " " + false);
	//		}
	//
	//		final short sID = (short) (imageA.getVOIs().getUniqueID());
	//		if (displayInterpolatedContours != null) {
	//			imageA.unregisterVOI(displayInterpolatedContours);
	//			displayInterpolatedContours.dispose();
	//			displayInterpolatedContours = null;
	//		}
	//		displayInterpolatedContours = new VOI(sID, "interpolatedContours");
	//		displayInterpolatedContours.setColor(Color.blue);
	//
	//		for (int i = 0; i < growContours.getCurves().size(); i += 30) {
	//			final VOIContour contour = (VOIContour) growContours.getCurves().elementAt(i).clone();
	//			contour.trimPoints(0.5, true);
	//			displayInterpolatedContours.getCurves().add(contour);
	//			contour.update(new ColorRGBA(0, 1, 0, 1));
	//			contour.setVolumeDisplayRange(minRange);
	//		}
	//
	//
	//		//		ModelImage inside = new ModelImage(ModelStorageBase.INTEGER, imageA.getExtents(), imageName + "_insideMask.xml");
	//		//		JDialogBase.updateFileInfo(imageA, inside);
	//		//		// Call the straightening step:
	//		//		for (int z = 0; z < dimZ; z++) {
	//		//			for (int y = 0; y < dimY; y++) {
	//		//				for (int x = 0; x < dimX; x++) {
	//		//					if (model.getFloat(x, y, z) != 0) {
	//		//						inside.set(x, y, z, 1);
	//		//					}
	//		//				}
	//		//			}
	//		//		}
	//		//		inside.setImageName( imageName + "_" + growStep + "_insideMask.xml" );
	//		//		saveImage(imageName, inside, false);
	//		//		inside.disposeLocal();
	//		//		inside = null;
	//
	//		saveImage(imageName, model, false);
	//		straighten(imageA, resultExtents, imageName, model, true, displayResult, true);
	//		//		straighten(imageA, resultExtents, imageName, model, false, displayResult, true);
	//
	//		model.disposeLocal();
	//		model = null;
	//	}

	/**
	 * Fills in the fluorescent marker for the marker segmentation.
	 * 
	 * @param image
	 * @param gmImage
	 * @param model
	 * @param gmMin
	 * @param intensityMin
	 * @param centerPt
	 * @param seedList
	 * @param saveSeedList
	 * @param maxDiameter
	 * @param id
	 * @return
	 */
	//	private int fill(final ModelImage image, final ModelImage gmImage, final ModelImage model, final float gmMin, final float intensityMin,
	//			final Vector3f centerPt, final Vector<Vector3f> seedList, final Vector<Vector3f> saveSeedList,
	//
	//			final int maxDiameter, final int id) {
	//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
	//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
	//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
	//
	//		double averageValue = 0;
	//		int count = 0;
	//
	//		while (seedList.size() > 0) {
	//			final Vector3f seed = seedList.remove(0);
	//			if (centerPt.distance(seed) > maxDiameter) {
	//				saveSeedList.add(seed);
	//				continue;
	//			}
	//
	//			final int z = Math.round(seed.Z);
	//			final int y = Math.round(seed.Y);
	//			final int x = Math.round(seed.X);
	//			float value = model.getFloat(x, y, z);
	//			if (value != 0) {
	//				continue;
	//			}
	//			float valueGM;
	//			if (image.isColorImage()) {
	//				value = image.getFloatC(x, y, z, 2);
	//				valueGM = gmImage.getFloatC(x, y, z, 2);
	//			} else {
	//				value = image.getFloat(x, y, z);
	//				valueGM = gmImage.getFloat(x, y, z);
	//			}
	//			if ( (value >= intensityMin) && (valueGM >= gmMin)) {
	//				for (int z1 = Math.max(0, z - 1); z1 <= Math.min(dimZ - 1, z + 1); z1++) {
	//					for (int y1 = Math.max(0, y - 1); y1 <= Math.min(dimY - 1, y + 1); y1++) {
	//						for (int x1 = Math.max(0, x - 1); x1 <= Math.min(dimX - 1, x + 1); x1++) {
	//							if ( ! ( (x == x1) && (y == y1) && (z == z1))) {
	//								if (image.isColorImage()) {
	//									value = image.getFloatC(x1, y1, z1, 2);
	//									valueGM = gmImage.getFloatC(x1, y1, z1, 2);
	//								} else {
	//									value = image.getFloat(x1, y1, z1);
	//									valueGM = gmImage.getFloat(x1, y1, z1);
	//								}
	//								if (value >= intensityMin) {
	//									seedList.add(new Vector3f(x1, y1, z1));
	//								}
	//							}
	//						}
	//					}
	//				}
	//				count++;
	//				model.set(x, y, z, id);
	//				if (image.isColorImage()) {
	//					value = image.getFloatC(x, y, z, 2);
	//				} else {
	//					value = image.getFloat(x, y, z);
	//				}
	//				averageValue += value;
	//			}
	//		}
	//		// if ( count != 0 )
	//		// {
	//		// averageValue /= (float)count;
	//		// System.err.println( "fill markers " + count + " " + (float)averageValue + " " +
	//		// (float)(averageValue/image.getMax()) );
	//		// }
	//		return count;
	//	}

	/**
	 * Fills each model slice of the marker segmentation image with the current slice value.
	 * 
	 * @param markerSegmentation
	 * @param sliceIDs
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param ellipseBound
	 */
	//	private void fillMarkerSegmentationImage(final ModelImage markerSegmentation, final float[] sliceIDs, final int tSlice,
	//			final int slice, final int[] extents, final Vector3f[] verts, final Ellipsoid3f ellipseBound) {
	//		final int iBound = extents[0];
	//		final int jBound = extents[1];
	//
	//		final int[] dimExtents = markerSegmentation.getExtents();
	//
	//		/*
	//		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
	//		 * coordinate-systems: transformation:
	//		 */
	//		final int iFactor = 1;
	//		final int jFactor = dimExtents[0];
	//		final int kFactor = dimExtents[0] * dimExtents[1];
	//		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];
	//
	//		final int buffFactor = 1;
	//
	//		final Vector3f center = new Vector3f();
	//		for (int i = 0; i < verts.length; i++) {
	//			center.add(verts[i]);
	//		}
	//		center.scale(1f / verts.length);
	//
	//		/* Calculate the slopes for traversing the data in x,y,z: */
	//		float xSlopeX = verts[1].X - verts[0].X;
	//		float ySlopeX = verts[1].Y - verts[0].Y;
	//		float zSlopeX = verts[1].Z - verts[0].Z;
	//
	//		float xSlopeY = verts[3].X - verts[0].X;
	//		float ySlopeY = verts[3].Y - verts[0].Y;
	//		float zSlopeY = verts[3].Z - verts[0].Z;
	//
	//		float x0 = verts[0].X;
	//		float y0 = verts[0].Y;
	//		float z0 = verts[0].Z;
	//
	//		xSlopeX /= (iBound);
	//		ySlopeX /= (iBound);
	//		zSlopeX /= (iBound);
	//
	//		xSlopeY /= (jBound);
	//		ySlopeY /= (jBound);
	//		zSlopeY /= (jBound);
	//
	//		/* loop over the 2D image (values) we're writing into */
	//		float x = x0;
	//		float y = y0;
	//		float z = z0;
	//
	//		final Vector3f currentPoint = new Vector3f();
	//		for (int j = 0; j < jBound; j++) {
	//
	//			/* Initialize the first diagonal point(x,y,z): */
	//			x = x0;
	//			y = y0;
	//			z = z0;
	//
	//			for (int i = 0; i < iBound; i++) {
	//				final int iIndex = Math.round(x);
	//				final int jIndex = Math.round(y);
	//				final int kIndex = Math.round(z);
	//
	//				/* calculate the ModelImage space index: */
	//				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));
	//
	//				// Bounds checking:
	//				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
	//						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {
	//
	//					// do nothing
	//				} else {
	//					currentPoint.set(x, y, z);
	//					final boolean isInside = ellipseBound.Contains(currentPoint);
	//					if (isInside) {
	//						markerSegmentation.set(iIndex, jIndex, kIndex, sliceIDs[slice]);
	//					}
	//				}
	//
	//				/*
	//				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
	//				 * ySlopeX and zSlopeX values:
	//				 */
	//				x = x + xSlopeX;
	//				y = y + ySlopeX;
	//				z = z + zSlopeX;
	//			}
	//
	//			/*
	//			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
	//			 * ySlopeY and zSlopeY values:
	//			 */
	//			x0 = x0 + xSlopeY;
	//			y0 = y0 + ySlopeY;
	//			z0 = z0 + zSlopeY;
	//		}
	//	}

	/**
	 * Called when a new lattice is loaded from file, replaces the current lattice.
	 * 
	 * @param newLattice
	 */
	public void setLattice(final VOIVector newLattice) {
		
		if ( lattice != null ) {
			for ( int i = 0; i < lattice.size(); i++ ) {
				imageA.unregisterVOI(lattice.elementAt(i));
			}
			if ( leftContour != null )  imageA.unregisterVOI(leftContour);
			if ( rightContour != null ) imageA.unregisterVOI(rightContour);
			if ( lattice != newLattice ) {
				lattice.removeAllElements();
			}
		}		
		
		this.lattice = newLattice;
		left = null;
		right = null;
		leftContour = null;
		rightContour = null;
		clearCurves(true);
		if ( this.lattice == null )
		{
			updateLattice(true);
			return;
		}

		// Assume image is isotropic (square voxels).
		if (lattice.size() < 2) {
			updateLattice(true);
			return;
		}
		left = (VOI) lattice.elementAt(0);
		right = (VOI) lattice.elementAt(1);
		if (left.getCurves().size() != right.getCurves().size()) {
			updateLattice(true);
			return;
		}

		imageA.registerVOI(left);
		imageA.registerVOI(right);
		showLatticeLabels(false);

		clear3DSelection();
		clearAddLeftRightMarkers();
		updateLattice(true);
		showLatticeLabels(displayLatticeLabels);
	}

	public void setMarkers(VOI markerVOIs)
	{
		markerCenters = new Vector<Vector3f>();
		markerNames = new Vector<String>();
		markerLatticeSegments = new Vector<Integer>();
		markerSlices = new Vector<Integer>();
		for ( int i = 0; i < markerVOIs.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)markerVOIs.getCurves().elementAt(i);
			Vector3f center = new Vector3f(text.elementAt(0));
			markerCenters.add(center);
			markerNames.add(text.getText());

			int value = text.getLatticeSegment();
			markerLatticeSegments.add(value);
			
			int slice = text.getSlice();
			markerSlices.add(slice);
		}

		//		writeMarkers(imageA_LF);
	}

	/**
	 * Grows the outer edge of the worm model outward to capture more of the worm, using the segmented marker image as a
	 * guide.
	 * 
	 * @param model
	 * @param markers
	 * @param sliceIDs
	 * @param step
	 */
	//	private void growEdges(final ModelImage mask, final ModelImage model, final ModelImage markers, final float[] sliceIDs, final int step) {
	//		final int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
	//		final int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
	//		final int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;
	//
	//		if (step == 0) {
	//			if (growContours != null) {
	//				growContours.dispose();
	//				growContours = null;
	//			}
	//			final short sID = (short) (imageA.getVOIs().getUniqueID());
	//			growContours = new VOI(sID, "growContours");
	//
	//			for (int i = 0; i < centerPositions.size(); i++) {
	//				final int value = i + 1;
	//
	//				final Vector3f rkEye = centerPositions.elementAt(i);
	//				final Vector3f rkRVector = rightVectors.elementAt(i);
	//				final Vector3f rkUVector = upVectors.elementAt(i);
	//
	//				final VOIContour ellipse = new VOIContour(true);
	//				ellipse.setVolumeDisplayRange(minRange);
	//				makeEllipse2D(rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i),  ellipse, 32);
	//
	//				interpolateContour(ellipse);
	//				for (int j = 0; j < ellipse.size(); j++) {
	//					final Vector3f start = new Vector3f(ellipse.elementAt(j));
	//					final Vector3f pt = ellipse.elementAt(j);
	//					Vector3f diff = Vector3f.sub(pt, centerPositions.elementAt(i));
	//					float length = diff.normalize();
	//
	//					pt.copy(centerPositions.elementAt(i));
	//					int x = Math.round(pt.X);
	//					int y = Math.round(pt.Y);
	//					int z = Math.round(pt.Z);
	//					int currentValue = model.getInt(x, y, z);
	//					while ( ( (length != 0) && (currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit)) {
	//						pt.add(diff);
	//						x = Math.round(pt.X);
	//						y = Math.round(pt.Y);
	//						z = Math.round(pt.Z);
	//						if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
	//							break;
	//						}
	//						currentValue = model.getInt(x, y, z);
	//					}
	//					if ( !pt.isEqual(centerPositions.elementAt(i))) {
	//						pt.sub(diff);
	//					}
	//					final float distStart = start.distance(centerPositions.elementAt(i));
	//					float distPt = pt.distance(centerPositions.elementAt(i));
	//					if (distStart > distPt) {
	//						x = Math.round(start.X);
	//						y = Math.round(start.Y);
	//						z = Math.round(start.Z);
	//						if ( ! ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ))) {
	//							currentValue = model.getInt(x, y, z);
	//							if ( ( (currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit)) {
	//								diff = Vector3f.sub(start, pt);
	//								length = diff.normalize();
	//								while ( (length != 0) && !pt.isEqual(start) && (distPt < distStart)) {
	//									pt.add(diff);
	//									x = Math.round(pt.X);
	//									y = Math.round(pt.Y);
	//									z = Math.round(pt.Z);
	//									if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
	//										break;
	//									}
	//									model.set(x, y, z, currentValue);
	//									distPt = pt.distance(centerPositions.elementAt(i));
	//								}
	//							}
	//						}
	//					}
	//				}
	//				growContours.getCurves().add(ellipse);
	//			}
	//			// return;
	//		}
	//
	//		for (int i = 0; i < centerPositions.size(); i++) {
	//			final int value = i + 1;
	//			final VOIContour ellipse = (VOIContour) growContours.getCurves().elementAt(i);
	//			interpolateContour(ellipse);
	//
	//			for (int j = 0; j < ellipse.size(); j++) {
	//				final Vector3f pt = ellipse.elementAt(j);
	//				final Vector3f diff = Vector3f.sub(pt, centerPositions.elementAt(i));
	//				final float distance = diff.normalize();
	//				// diff.scale(0.5f);
	//
	//				final float x = pt.X + diff.X;
	//				final float y = pt.Y + diff.Y;
	//				final float z = pt.Z + diff.Z;
	//				boolean extend = true;
	//				for (int z1 = Math.max(0, (int) Math.floor(z)); (z1 <= Math.min(dimZ - 1, Math.ceil(z))) && extend; z1++) {
	//					for (int y1 = Math.max(0, (int) Math.floor(y)); (y1 <= Math.min(dimY - 1, Math.ceil(y))) && extend; y1++) {
	//						for (int x1 = Math.max(0, (int) Math.floor(x)); (x1 <= Math.min(dimX - 1, Math.ceil(x))) && extend; x1++) {
	//							final int currentValue = model.getInt(x1, y1, z1);
	//							if (currentValue != 0) {
	//								if (Math.abs(currentValue - value) > SampleLimit) {
	//									extend = false;
	//									break;
	//								}
	//							}
	//							if (markers != null) {
	//								final float markerValue = markers.getFloat(x1, y1, z1);
	//								if ( (markerValue != 0) && (markerValue != sliceIDs[i])) {
	//									extend = false;
	//									break;
	//								}
	//							}
	//							if ( maskImage != null )
	//							{
	//								final boolean maskValue = maskImage.getBoolean(x1, y1, z1);
	//								if ( maskValue )
	//								{
	//									extend = false;
	//								}
	//							}
	//						}
	//					}
	//				}
	//				if (extend) {
	//					for (int z1 = Math.max(0, (int) Math.floor(z)); (z1 <= Math.min(dimZ - 1, Math.ceil(z))) && extend; z1++) {
	//						for (int y1 = Math.max(0, (int) Math.floor(y)); (y1 <= Math.min(dimY - 1, Math.ceil(y))) && extend; y1++) {
	//							for (int x1 = Math.max(0, (int) Math.floor(x)); (x1 <= Math.min(dimX - 1, Math.ceil(x))) && extend; x1++) {
	//								final int currentValue = model.getInt(x1, y1, z1);
	//								if (currentValue == 0) {
	//									model.set(x1, y1, z1, value);
	//								}
	//							}
	//						}
	//					}
	//					pt.add(diff);
	//				}
	//			}
	//		}
	//	}

	public void setPaddingFactor( int padding ) {
		paddingFactor = padding;
		boolean display = (imageA.isRegistered(displayContours) != -1);

		if ( display )
		{
			generateCurves(5, 10);
			imageA.registerVOI(displayContours);
		}
	}

	public boolean isPreview()
	{
		return previewMode;
	}

	public void setPreviewMode( boolean preview, VOIVector lattice, VOI annotations )
	{
		this.previewMode = preview;
		setLattice(lattice);
		setAnnotations(annotations);
	}



	public void setSeamCellImage(ModelImage image)
	{
		seamCellImage = image;
	}
	/**
	 * Enables the user to visualize the final expanded contours.
	 */
	public void showExpandedModel() {
		if (displayInterpolatedContours != null) {
			if ( (imageA.isRegistered(displayInterpolatedContours) == -1)) {
				imageA.registerVOI(displayInterpolatedContours);
				imageA.notifyImageDisplayListeners();
			} else if ( (imageA.isRegistered(displayInterpolatedContours) != -1)) {
				imageA.unregisterVOI(displayInterpolatedContours);
				imageA.notifyImageDisplayListeners();
			}
		}
		//		if ( samplingPlanes != null )
		//		{
		//			imageA.unregisterAllVOIs();
		//			imageA.registerVOI(samplingPlanes);
		//		}
	}

	/**
	 * Enables the user to visualize the simple ellipse-based model of the worm during lattice construction.
	 */
	public void showModel() {
		if ( (imageA.isRegistered(displayContours) == -1)) {
			imageA.registerVOI(displayContours);
			imageA.notifyImageDisplayListeners();
		} else if ( (imageA.isRegistered(displayContours) != -1)) {
			imageA.unregisterVOI(displayContours);
			imageA.notifyImageDisplayListeners();
		}
	}

	/**
	 * Enables the user to visualize the simple ellipse-based model of the worm during lattice construction.
	 */
	public void showModel(boolean display) {
//		if ( display && (imageA.isRegistered(displayContours) == -1) ) {
//			imageA.registerVOI(displayContours);
//			imageA.notifyImageDisplayListeners();
//		}
//		if ( !display && (imageA.isRegistered(displayContours) != -1) ) {
//			imageA.unregisterVOI(displayContours);
//			imageA.notifyImageDisplayListeners();
//		}
		if ( samplingPlanes == null ) generateEllipses();
		if ( display && (imageA.isRegistered(samplingPlanes) == -1) ) {
			imageA.registerVOI(samplingPlanes);
			imageA.notifyImageDisplayListeners();
		}
		if ( !display && (imageA.isRegistered(samplingPlanes) != -1) ) {
			imageA.unregisterVOI(samplingPlanes);
			imageA.notifyImageDisplayListeners();
		}
	}
	
	public boolean isModelDisplayed()
	{
		return (imageA.isRegistered(samplingPlanes) != -1);
	}

	private boolean displayLatticeLabels = false;
	public void showLatticeLabels(boolean display) {
		if ( left == null ) return;
		displayLatticeLabels = display;
		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			VOIWormAnnotation text = (VOIWormAnnotation) left.getCurves().elementAt(i);
			text.display(displayLatticeLabels);
			
			text = (VOIWormAnnotation) right.getCurves().elementAt(i);
			text.display(displayLatticeLabels);
		}
	}
	
	
	/**
	 * Turns on/off lattice display:
	 */
	public void showLattice(boolean display) {		
		if ( display ) {
			updateLattice(true);
		}
		if ( !display ) {
			if ( leftContour != null ) {
				imageA.unregisterVOI(leftContour);
			}
			if ( rightContour != null ) {
				imageA.unregisterVOI(rightContour);
			}
			if (latticeGrid != null) {
				for (int i = latticeGrid.size() - 1; i >= 0; i--) {
					final VOI marker = latticeGrid.remove(i);
					imageA.unregisterVOI(marker);
				}
			}
			if (centerLine != null) {
				imageA.unregisterVOI(centerLine);
			}
			if (rightLine != null) {
				imageA.unregisterVOI(rightLine);
			}
			if (leftLine != null) {
				imageA.unregisterVOI(leftLine);
			}
			imageA.notifyImageDisplayListeners();
		}
//		VOIVector vois = imageA.getVOIs();
//		for (int i = 0; i < vois.size(); i++) {
//			System.err.println( vois.elementAt(i).getName() );
//		}
	}



	public void testLatticeConflicts( VOI leftIn, VOI rightIn, float[] intersectionCountResult )
	{		
		left = leftIn;
		right = rightIn;
		generateCurves(1, 1);
		generateEllipses();

		//		lengthResult[0] = length;
		//		curvatureResult[0] = totalCurvature;

		final int[] resultExtents = new int[] {2 * extent, 2 * extent, samplingPlanes.getCurves().size()};

		String imageName = imageA.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		ModelImage model = new ModelImage(ModelStorageBase.INTEGER, imageA.getExtents(), imageName + "_model.xml");
		JDialogBase.updateFileInfo(imageA, model);


		// 7. The set of ellipses from the head of the worm to the tail defines an approximate outer boundary of the
		// worm in 3D.
		// The centers of each ellipse are spaced one voxel apart along the center line curve of the worm, and each
		// ellipse
		// corresponds to a single output slice in the final straightened image. This step generates a model of the worm
		// where each voxel that falls within one of the ellipses is labeled with the corresponding output slice value.
		// Voxels where multiple ellipses intersect are labeled as conflict voxels. Once all ellipses have been
		// evaluated,
		// the conflict voxels are removed from the model.
		final int dimX = imageA.getExtents().length > 0 ? imageA.getExtents()[0] : 1;
		final int dimY = imageA.getExtents().length > 1 ? imageA.getExtents()[1] : 1;
		final int dimZ = imageA.getExtents().length > 2 ? imageA.getExtents()[2] : 1;


		BitSet conflicts = new BitSet(dimX*dimY*dimZ);
		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			final Vector3f[] corners = new Vector3f[4];
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			} 

			testConflicts( imageA, model, conflicts, i, resultExtents, corners, boxBounds.elementAt(i), ellipseBounds.elementAt(i), i + 1);
		}

		int conflictCount = 0;
		int totalCount = 0;
		for ( int i = 0; i < model.getDataSize(); i++ )
		{
			if ( model.getInt(i) > 0 )
			{
				totalCount++;
			}
			if ( conflicts.get(i) )
			{
				conflictCount++;
			}
		}

		model.disposeLocal(true);
		model = null;

		//		System.err.println( "Lattice conflicts " + conflictCount + "   " + totalCount );
		intersectionCountResult[0] = 100f * conflictCount/totalCount;
		conflicts = null;
	}

	public float[] testLatticeImage( VOI leftIn, VOI rightIn )
	{		
		left = leftIn;
		right = rightIn;

		Vector<Integer> seamCellIds = new Vector<Integer>();
		for ( int i = 0; i < left.getCurves().size(); i++ )
		{
			Vector3f leftPt = left.getCurves().elementAt(i).elementAt(0);
			Vector3f rightPt = right.getCurves().elementAt(i).elementAt(0);

			if ( seamCellImage != null )
			{
				int leftID = seamCellImage.getInt( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
				if ( leftID != 0 )
				{
					if ( !seamCellIds.contains(leftID) )
					{
						seamCellIds.add(leftID);
					}
				}
				int rightID = seamCellImage.getInt( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
				if ( rightID != 0 )
				{
					if ( !seamCellIds.contains(rightID) )
					{
						seamCellIds.add(rightID);
					}
				}
			}
		}



		if ( !generateCurves(5, 10) )
		{
			return new float[]{-1,1};
		}

		int step = 5;
		for ( int i = 0; i < upVectors.size()-step; i++ )
		{
			float angle = upVectors.elementAt(i).angle(upVectors.elementAt(i+step));
			//			System.err.println( angle );
			if ( angle > (Math.PI/2f) )
			{
				//				System.err.println( i + "   " + angle );
				return new float[]{-1,1};
			}
		}

		//		// Test how much the angle changes between seam cells:
		//		Vector<Integer> closestIndex = new Vector<Integer>();
		//		Vector<Float> closestDist = new Vector<Float>();
		//		Vector<Vector3f> centerPts = new Vector<Vector3f>();
		//		for ( int i = 0; i < left.size(); i++ )
		//		{
		//			Vector3f centerPt = Vector3f.add(left.elementAt(i), right.elementAt(i) );
		//			centerPt.scale(0.5f);
		//			centerPts.add( centerPt );
		//			closestIndex.add( -1 );
		//			closestDist.add( Float.MAX_VALUE );
		//		}
		//		for ( int i = 0; i < centerPositions.size(); i++ )
		//		{
		//			for ( int j = 0; j < centerPts.size(); j++ )
		//			{
		//				float dist = centerPositions.elementAt(i).distance(centerPts.elementAt(j));
		//				if ( dist < closestDist.elementAt(j) )
		//				{
		//					closestDist.set(j, dist);
		//					closestIndex.set(j, i );
		//				}
		//			}
		//		}
		//		for ( int i = 0; i < closestIndex.size()-1; i++ )
		//		{
		//			int index = closestIndex.elementAt(i);
		//			int indexP1 = closestIndex.elementAt(i+1);
		//			if ( index == -1 || indexP1 == -1 )
		//				continue;
		//			float totalAngle = 0;
		//			for ( int j = index; j < indexP1; j++ )
		//			{
		//				Vector3f up1 = upVectors.elementAt(j);
		//				Vector3f up2 = upVectors.elementAt(j+1);
		//
		//				float angle = up1.angle(up2);
		//				totalAngle += angle;
		//			}
		//			//			System.err.println( "   testLatticeImage angle:   " + totalAngle );
		//			if ( totalAngle  > (2*Math.PI/3f) )
		//			{
		//				return new float[]{-1,1};
		//			}
		//		}

		// Look for intersecting seam cells along the curve path:
		float[] avgValue = new float[]{0,0};
		float leftVals = 0;
		float rightVals = 0;
		//		float centerVals = 0;
		float min = (float)imageA.getMin();
		for ( int i = 0; i < leftPositions.size(); i++ )
		{
			Vector3f leftPt = leftPositions.elementAt(i);
			Vector3f rightPt = rightPositions.elementAt(i);
			Vector3f centerPt = centerPositions.elementAt(i);

			//			if ( seamCellImage != null )
			//			{
			//				leftVals = seamCellImage.getFloat( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
			//				rightVals = seamCellImage.getFloat( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
			//			}
			//			else
			//			{
			//				leftVals = 1;
			//				rightVals = 1;
			//			}
			//			leftVals = imageA.getFloat( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
			//			if ( leftVals > min )
			//			{
			//				avgValue[0]++;
			//			}
			//			rightVals = imageA.getFloat( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
			//			if ( rightVals > min )
			//			{
			//				avgValue[1]++;
			//			}
			//			centerVals = imageA.getFloat( (int)centerPt.X, (int)centerPt.Y, (int)centerPt.Z );
			avgValue[0] += imageA.getFloat( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
			avgValue[0] += imageA.getFloat( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
			avgValue[1] += imageA.getFloat( (int)centerPt.X, (int)centerPt.Y, (int)centerPt.Z );

			if ( (seamCellImage != null) && (allSeamCellIDs != null) )
			{
				int leftID = seamCellImage.getInt( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
				int rightID = seamCellImage.getInt( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
				int centerID = seamCellImage.getInt( (int)centerPt.X, (int)centerPt.Y, (int)centerPt.Z );

				if ( seamCellIds.contains(leftID) && (leftID != 0) && !((leftID == allSeamCellIDs[i][0]) || (leftID == allSeamCellIDs[i][1])) && (allSeamCellIDs[i][0] != 0) && (allSeamCellIDs[i][1] != 0))
				{
					if ( !((leftID == allSeamCellIDs[i][2]) || (leftID == allSeamCellIDs[i][3])) )
					{
						//					System.err.println( "  Left: " + leftID + "   " + allSeamCellIDs[i][0] + "  " + allSeamCellIDs[i][1] + "   " + !((leftID == allSeamCellIDs[i][0]) || (leftID == allSeamCellIDs[i][1])));
						return new float[]{-1,1};
					}
				}
				if ( seamCellIds.contains(rightID) && (rightID != 0) && !((rightID == allSeamCellIDs[i][2]) || (rightID == allSeamCellIDs[i][3])) && (allSeamCellIDs[i][2] != 0) && (allSeamCellIDs[i][3] != 0))
				{
					if ( !((rightID == allSeamCellIDs[i][0]) || (rightID == allSeamCellIDs[i][1])) )
					{
						//					System.err.println( "  Right: " + rightID + "   " + allSeamCellIDs[i][2] + "  " + allSeamCellIDs[i][3] + "   " + !((rightID == allSeamCellIDs[i][2]) || (rightID == allSeamCellIDs[i][3])));
						return new float[]{-1,1};
					}
				}
				if ( seamCellIds.contains(centerID) && !((centerID == leftID) || (centerID == rightID)) && (leftID != 0) && (rightID != 0) )
				{
					//					System.err.println( "  Center: " + centerID + "   " + leftID + "  " + rightID + "   " + !((centerID == leftID) || (centerID == rightID)));
					return new float[]{-1,1};
				}
			}
		}
		avgValue[0] /= (2* (float)leftPositions.size());
		avgValue[1] /= leftPositions.size();


		//		System.err.println( leftVals/(float)leftPositions.size() + "   " + rightVals/(float)leftPositions.size() + "      " + centerVals/(float)leftPositions.size() );

		return avgValue;
	}

	/**
	 * VOI operation undo.
	 */
	public void undo() {
		updateLinks();
	}

	/**
	 * Writes out slice IDs that match the segmented marker IDs, set the value for every slice in the straightened image
	 * to the corresponding marker ID for the marker that intersects that slice. Only markers that are inside the
	 * current worm model are included -- even when multiple markers intersect the same slice. the markers that are
	 * inside the worm model are included.
	 * 
	 * @param model
	 * @param markerSegmentation
	 * @param sliceIDs
	 * @param markerIDs
	 * @param completedIDs
	 * @param currentID
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param ellipseBound
	 */
	//	private void mapSliceIDstoMarkerIDs(final ModelImage model, final ModelImage markerSegmentation, final float[] sliceIDs, final int[] markerIDs,
	//			final boolean[] completedIDs, final int[] currentID, final int tSlice, final int slice, final int[] extents, final Vector3f[] verts,
	//			final Ellipsoid3f ellipseBound) {
	//		final int iBound = extents[0];
	//		final int jBound = extents[1];
	//
	//		final int[] dimExtents = markerSegmentation.getExtents();
	//
	//		/*
	//		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
	//		 * coordinate-systems: transformation:
	//		 */
	//		final int iFactor = 1;
	//		final int jFactor = dimExtents[0];
	//		final int kFactor = dimExtents[0] * dimExtents[1];
	//		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];
	//
	//		final int buffFactor = 1;
	//
	//		final Vector3f center = new Vector3f();
	//		for (int i = 0; i < verts.length; i++) {
	//			center.add(verts[i]);
	//		}
	//		center.scale(1f / verts.length);
	//
	//		/* Calculate the slopes for traversing the data in x,y,z: */
	//		float xSlopeX = verts[1].X - verts[0].X;
	//		float ySlopeX = verts[1].Y - verts[0].Y;
	//		float zSlopeX = verts[1].Z - verts[0].Z;
	//
	//		float xSlopeY = verts[3].X - verts[0].X;
	//		float ySlopeY = verts[3].Y - verts[0].Y;
	//		float zSlopeY = verts[3].Z - verts[0].Z;
	//
	//		float x0 = verts[0].X;
	//		float y0 = verts[0].Y;
	//		float z0 = verts[0].Z;
	//
	//		xSlopeX /= (iBound);
	//		ySlopeX /= (iBound);
	//		zSlopeX /= (iBound);
	//
	//		xSlopeY /= (jBound);
	//		ySlopeY /= (jBound);
	//		zSlopeY /= (jBound);
	//
	//		/* loop over the 2D image (values) we're writing into */
	//		float x = x0;
	//		float y = y0;
	//		float z = z0;
	//
	//		final Vector3f currentPoint = new Vector3f();
	//
	//		final float[] values = new float[iBound * jBound];
	//		for (int j = 0; j < jBound; j++) {
	//
	//			/* Initialize the first diagonal point(x,y,z): */
	//			x = x0;
	//			y = y0;
	//			z = z0;
	//
	//			for (int i = 0; i < iBound; i++) {
	//				values[j * iBound + i] = 0;
	//				final int iIndex = Math.round(x);
	//				final int jIndex = Math.round(y);
	//				final int kIndex = Math.round(z);
	//
	//				/* calculate the ModelImage space index: */
	//				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));
	//
	//				// Bounds checking:
	//				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
	//						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {
	//
	//					// do nothing
	//				} else {
	//					currentPoint.set(x, y, z);
	//					final boolean isInside = ellipseBound.Contains(currentPoint);
	//					final int currentValue = model.getInt(iIndex, jIndex, kIndex);
	//					// float currentValue = model.getFloatTriLinearBounds(x, y, z);
	//					if (isInside && (currentValue != 0)) {
	//						values[j * iBound + i] = markerSegmentation.getFloat(iIndex, jIndex, kIndex);
	//					}
	//				}
	//
	//				/*
	//				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
	//				 * ySlopeX and zSlopeX values:
	//				 */
	//				x = x + xSlopeX;
	//				y = y + ySlopeX;
	//				z = z + zSlopeX;
	//			}
	//
	//			/*
	//			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
	//			 * ySlopeY and zSlopeY values:
	//			 */
	//			x0 = x0 + xSlopeY;
	//			y0 = y0 + ySlopeY;
	//			z0 = z0 + zSlopeY;
	//		}
	//
	//		float markerPt = 0;
	//
	//		boolean inconsistent = false;
	//		for (int j = 0; j < values.length && !inconsistent; j++) {
	//			if (values[j] > 0) {
	//				for (int k = j + 1; k < values.length && !inconsistent; k++) {
	//					if ( (values[k] != 0) && (values[j] != values[k])) {
	//						inconsistent = true;
	//						break;
	//					}
	//				}
	//				markerPt = values[j];
	//			}
	//		}
	//		if (inconsistent || (markerPt == 0)) {
	//			return;
	//		}
	//
	//		if ( (markerIDs[currentID[0]] < markerPt) && completedIDs[currentID[0]]) {
	//			for (int i = currentID[0] + 1; i < markerIDs.length; i++) {
	//				if (markerIDs[i] == 0) {
	//					continue;
	//				} else {
	//					currentID[0] = i;
	//					break;
	//				}
	//			}
	//		}
	//
	//		if (markerIDs[currentID[0]] != markerPt) {
	//			return;
	//		}
	//		completedIDs[currentID[0]] = true;
	//
	//		sliceIDs[slice] = markerPt;
	//		// System.err.println( slice + " " + markerPt + " " + markerIDs[ currentID[0] ] );
	//	}

	/**
	 * Converts the marker segmentation image into a color image where each marker is colored based on the corresponding
	 * lattice ID
	 * 
	 * @param image
	 * @param displayResult
	 */
	//	private void markerImageToColor(final ModelImage image, final boolean displayResult) {
	//
	//		String imageName = imageA.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//		ModelImage colorImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), imageName + "_color_markers.xml");
	//		JDialogBase.updateFileInfo(imageA, colorImage);
	//		final float numColors = (float) image.getMax();
	//		// for ( int i = 0; i < numColors; i++ )
	//		// {
	//		// Color color = new Color( Color.HSBtoRGB( i / numColors, 1, 1) );
	//		// System.err.println( i + " " + (i / numColors) + " " + color );
	//		// }
	//
	//		final int dimX = colorImage.getExtents().length > 0 ? colorImage.getExtents()[0] : 1;
	//		final int dimY = colorImage.getExtents().length > 1 ? colorImage.getExtents()[1] : 1;
	//		final int dimZ = colorImage.getExtents().length > 2 ? colorImage.getExtents()[2] : 1;
	//		for (int z = 0; z < dimZ; z++) {
	//			for (int y = 0; y < dimY; y++) {
	//				for (int x = 0; x < dimX; x++) {
	//					final float value = image.getFloat(x, y, z);
	//					if (value > 0) {
	//						final Color color = new Color(Color.HSBtoRGB(value / numColors, 1, 1));
	//						colorImage.setC(x, y, z, 0, 1);
	//						colorImage.setC(x, y, z, 1, color.getRed() / 255f);
	//						colorImage.setC(x, y, z, 2, color.getGreen() / 255f);
	//						colorImage.setC(x, y, z, 3, color.getBlue() / 255f);
	//					}
	//				}
	//			}
	//		}
	//
	//		saveImage(imageName, colorImage, false);
	//		if (displayResult) {
	//			image.calcMinMax();
	//			new ViewJFrameImage((ModelImage) image.clone());
	//			colorImage.calcMinMax();
	//			new ViewJFrameImage((ModelImage) colorImage.clone());
	//		}
	//		colorImage.disposeLocal();
	//		colorImage = null;
	//	}

	/**
	 * Moves the lattice point to better match the marker segementation results.
	 * 
	 * @param markerImage
	 * @param pt
	 * @param dir
	 * @param id
	 */
	//	private void moveMarker(final ModelImage markerImage, final Vector3f pt, final Vector3f dir, final int id) {
	//		final int dimX = markerImage.getExtents().length > 0 ? markerImage.getExtents()[0] : 1;
	//		final int dimY = markerImage.getExtents().length > 1 ? markerImage.getExtents()[1] : 1;
	//		final int dimZ = markerImage.getExtents().length > 2 ? markerImage.getExtents()[2] : 1;
	//
	//		float length = dir.normalize();
	//		if ( length == 0 )
	//		{
	//			return;
	//		}
	//		final Vector3f temp = new Vector3f(pt);
	//		temp.add(dir);
	//		int x = Math.round(temp.X);
	//		int y = Math.round(temp.Y);
	//		int z = Math.round(temp.Z);
	//		if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
	//			return;
	//		}
	//		float value = markerImage.getFloat(x, y, z);
	//		while (value == id) {
	//			pt.copy(temp);
	//			temp.add(dir);
	//			x = Math.round(temp.X);
	//			y = Math.round(temp.Y);
	//			z = Math.round(temp.Z);
	//			if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
	//				return;
	//			}
	//			value = markerImage.getFloat(x, y, z);
	//		}
	//	}

	/**
	 * Entry point in the lattice-based straightening algorithm. At this point a lattice must be defined, outlining how
	 * the worm curves in 3D. A lattice is defined ad a VOI with two curves of equal length marking the left-hand and
	 * right-hand sides or the worm.
	 * 
	 * @param displayResult, when true intermediate volumes and results are displayed as well as the final straightened
	 *            image.
	 */
	public void untwistImage( final boolean mainImage ) {

		if ( !latticeInterpolationInit ) {
			initializeInterpolation(mainImage);
		}

		final int[] resultExtents = new int[] {((2 * extent)), ((2 * extent)), samplingPlanes.getCurves().size()};
		long time = System.currentTimeMillis();
		untwist(imageA, resultExtents, true);
		System.err.println( "untwist elapsed time =  " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
		if ( seamCellImage != null )
		{
			untwist(seamCellImage, resultExtents, false);
			System.err.println( "untwist seamCellImage elapsed time =  " + AlgorithmBase.computeElapsedTime(time) );
		}
		if ( mainImage ) {
			time = System.currentTimeMillis();
			untwistLattice(imageA, resultExtents);
			System.err.println( "untwistLattice elapsed time =  " + AlgorithmBase.computeElapsedTime(time) );
		}
	}

	/**
	 * Resolves conflict voxels (where possible) in the worm model using the updated marker segmentation image.
	 * 
	 * @param model
	 * @param markerSegmentation
	 * @param sliceIDs
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param value
	 */
	//	private void resolveModelConflicts(final ModelImage model, final ModelImage markerSegmentation, final float[] sliceIDs, final int tSlice, final int slice,
	//			final int[] extents, final Vector3f[] verts, final int value) {
	//		final int iBound = extents[0];
	//		final int jBound = extents[1];
	//
	//		final int[] dimExtents = markerSegmentation.getExtents();
	//
	//		/*
	//		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
	//		 * coordinate-systems: transformation:
	//		 */
	//		final int iFactor = 1;
	//		final int jFactor = dimExtents[0];
	//		final int kFactor = dimExtents[0] * dimExtents[1];
	//		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];
	//
	//		final int buffFactor = 1;
	//
	//		final Vector3f center = new Vector3f();
	//		for (int i = 0; i < verts.length; i++) {
	//			center.add(verts[i]);
	//		}
	//		center.scale(1f / verts.length);
	//
	//		/* Calculate the slopes for traversing the data in x,y,z: */
	//		float xSlopeX = verts[1].X - verts[0].X;
	//		float ySlopeX = verts[1].Y - verts[0].Y;
	//		float zSlopeX = verts[1].Z - verts[0].Z;
	//
	//		float xSlopeY = verts[3].X - verts[0].X;
	//		float ySlopeY = verts[3].Y - verts[0].Y;
	//		float zSlopeY = verts[3].Z - verts[0].Z;
	//
	//		float x0 = verts[0].X;
	//		float y0 = verts[0].Y;
	//		float z0 = verts[0].Z;
	//
	//		xSlopeX /= (iBound);
	//		ySlopeX /= (iBound);
	//		zSlopeX /= (iBound);
	//
	//		xSlopeY /= (jBound);
	//		ySlopeY /= (jBound);
	//		zSlopeY /= (jBound);
	//
	//		/* loop over the 2D image (values) we're writing into */
	//		float x = x0;
	//		float y = y0;
	//		float z = z0;
	//
	//		final Vector3f currentPoint = new Vector3f();
	//		for (int j = 0; j < jBound; j++) {
	//
	//			/* Initialize the first diagonal point(x,y,z): */
	//			x = x0;
	//			y = y0;
	//			z = z0;
	//
	//			for (int i = 0; i < iBound; i++) {
	//				final int iIndex = Math.round(x);
	//				final int jIndex = Math.round(y);
	//				final int kIndex = Math.round(z);
	//
	//				/* calculate the ModelImage space index: */
	//				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));
	//
	//				// Bounds checking:
	//				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
	//						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {
	//
	//					// do nothing
	//				} else {
	//					currentPoint.set(x, y, z);
	//					final int currentValue = model.getInt(iIndex, jIndex, kIndex);
	//					final float markerValue = markerSegmentation.getFloat(iIndex, jIndex, kIndex);
	//					if ( (currentValue == 0) && (markerValue == sliceIDs[slice])) {
	//						model.set(iIndex, jIndex, kIndex, value);
	//					}
	//				}
	//
	//				/*
	//				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
	//				 * ySlopeX and zSlopeX values:
	//				 */
	//				x = x + xSlopeX;
	//				y = y + ySlopeX;
	//				z = z + zSlopeX;
	//			}
	//
	//			/*
	//			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
	//			 * ySlopeY and zSlopeY values:
	//			 */
	//			x0 = x0 + xSlopeY;
	//			y0 = y0 + ySlopeY;
	//			z0 = z0 + zSlopeY;
	//		}
	//	}

	/**
	 * Entry point in the lattice-based straightening algorithm. At this point a lattice must be defined, outlining how
	 * the worm curves in 3D. A lattice is defined ad a VOI with two curves of equal length marking the left-hand and
	 * right-hand sides or the worm.
	 * 
	 * @param displayResult, when true intermediate volumes and results are displayed as well as the final straightened
	 *            image.
	 */
	public void untwistMarkers(boolean untwistAll) {
		if ( markerCenters == null ) {
			return;
		}

		if ( !latticeInterpolationInit ) {
			initializeInterpolation(false);
		}

		final int[] resultExtents = new int[] {((2 * extent)), ((2 * extent)), samplingPlanes.getCurves().size()};
		//			untwistMarkers(imageA);
		//		untwistMarkers(imageA, resultExtents, !useLatticeModel);
		untwistMarkers(imageA, resultExtents);
	}

	public void untwistMarkers() {		
		setMarkers(annotationVOIs);
		untwistMarkers(true);	
		saveAnnotationStraight(imageA, "straightened_annotations", "straightened_annotations.csv" );	
	}
	
	
	/**
	 * Untwists the worm image quickly for the preview mode - without saving any images or statistics
	 * @return untwisted image.
	 */
	public ModelImage untwistTest()
	{
		initializeInterpolation(false);
		final int[] resultExtents = new int[] {((2 * extent)), ((2 * extent)), samplingPlanes.getCurves().size()};
		return untwistTest(imageA, true, resultExtents);

	}

	public void updateAnnotation( VOIWormAnnotation annotation )
	{
		int pickedAnnotation = -1;
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
		{
			if ( annotationVOIs.getCurves().elementAt(i) == annotation )
			{
				pickedAnnotation = i;
				break;
			}
		}
		if (pickedAnnotation != -1)
		{
			final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(pickedAnnotation);
			text.update();
			text.retwist(previewMode);
			text.setSelected(true);
			text.updateSelected(imageA);
			pickedPoint = text.elementAt(0);
			//			updateSelected();

			if ( text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin") )
			{
				if ( wormOrigin == null )
				{
					wormOrigin = new Vector3f(pickedPoint);
				}
				wormOrigin.copy(pickedPoint);
				// updateLattice(false);
			}
		}
		else
		{
			System.err.println("No matching VOI");
		}		
		updateAnnotationListeners();
	}

	/**
	 * Saves the lattice positions to a file.
	 * 
	 * @param image
	 * @param originToStraight
	 * @param left
	 * @param right
	 * @param volumes
	 * @param postFix
	 */
	//	private void saveLatticePositions(final ModelImage image, final ModelImage originToStraight, final VOIContour left,
	//			final VOIContour right, final int[][] volumes, final String postFix) {
	//		String imageName = image.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//		String voiDir = outputDirectory + File.separator;
	//		File voiFileDir = new File(voiDir);
	//		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
	//		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
	//		} else { // voiFileDir does not exist
	//			voiFileDir.mkdir();
	//		}
	//		voiDir = outputDirectory + File.separator + "statistics" + File.separator;
	//		voiFileDir = new File(voiDir);
	//		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
	//			// does
	//			// not
	//			// exist
	//			voiFileDir.mkdir();
	//		}
	//
	//		File file = new File(voiDir + "LatticePositions" + postFix + ".csv");
	//		if (file.exists()) {
	//			file.delete();
	//			file = new File(voiDir + "LatticePositions" + postFix + ".csv");
	//		}
	//
	//		try {
	//			final float cubicVolume = VOILatticeManagerInterface.VoxelSize * VOILatticeManagerInterface.VoxelSize * VOILatticeManagerInterface.VoxelSize;
	//
	//
	//			final FileWriter fw = new FileWriter(file);
	//			final BufferedWriter bw = new BufferedWriter(fw);
	//			bw.write("name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "," + "volume_voxel" + ","
	//					+ "volume_um" + "\n");
	//			for (int i = 0; i < left.size(); i++) {
	//				Vector3f position = left.elementAt(i);
	//				// if ( (model != null) && (originToStraight != null) )
	//				// {
	//				// position = originToStraight( model, originToStraight, position, "left"+i);
	//				// }
	//				bw.write("L" + i + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
	//						+ (position.Z - transformedOrigin.Z) + "," +
	//
	//                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
	//                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + ","
	//                        + volumes[i][0] + "," + cubicVolume * volumes[i][0] + "\n");
	//
	//				position = right.elementAt(i);
	//				// if ( originToStraight != null )
	//				// {
	//				// position = originToStraight( model, originToStraight, position, "right"+i);
	//				// }
	//				bw.write("R" + i + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
	//						+ (position.Z - transformedOrigin.Z) + "," +
	//
	//                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
	//                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + ","
	//                        + volumes[i][1] + "," + cubicVolume * volumes[i][1] + "\n");
	//			}
	//			bw.newLine();
	//			bw.close();
	//		} catch (final Exception e) {
	//			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
	//			e.printStackTrace();
	//		}
	//	}

	public void updateSelectedPoint( Color color )
	{
		Vector<VOIWormAnnotation> selectedAnnotations = getPickedAnnotation();
		if ( selectedAnnotations != null ) {
			for ( int i = 0; i < selectedAnnotations.size(); i++ ) {
				final VOIWormAnnotation text = selectedAnnotations.elementAt(i);

				if ( !match(text.getColor(), color) )
				{
					text.setColor(color);
				}
				else
				{
					text.setColor(Color.blue);
				}
				text.updateText();
				colorAnnotations();
			}
			updateAnnotationListeners();
		}
	}


	/**
	 * Generates the set of natural spline curves to fit the current lattice.
	 */
	protected boolean generateCurves( float stepSize, float contourStepSize ) {
		clearCurves(false);

		if ( seamCellImage != null )
		{
			seamCellIDs = new int[left.getCurves().size()][2];
		}

		//		System.err.println( "generateCurves test angles" );
		//		for ( int i = 0; i < left.size()-1; i++ )
		//		{
		//			Vector3f edgeL = Vector3f.sub( left.elementAt(i+1), left.elementAt(i) );    edgeL.normalize();
		//			Vector3f edgeR = Vector3f.sub( right.elementAt(i+1), right.elementAt(i) );  edgeR.normalize();
		//			float angle = edgeL.angle(edgeR);
		//			System.err.println( i + "  " + ((angle) * (180f / Math.PI)) );
		//			if ( angle > (Math.PI/3f) )
		//			{
		//				System.err.println( "   error!" );
		//			}
		//		}



		//		System.err.println("generateCurves");
		// 1. The center line of the worm is calculated from the midpoint between the left and right points of the
		// lattice.
		center = new VOIContour(false);
		for (int i = 0; i < left.getCurves().size(); i++)
		{
			final Vector3f centerPt = Vector3f.add(left.getCurves().elementAt(i).elementAt(0), right.getCurves().elementAt(i).elementAt(0));
			centerPt.scale(0.5f);
			center.add(centerPt);

			if ( seamCellImage != null )
			{
				Vector3f leftPt = left.getCurves().elementAt(i).elementAt(0);
				int leftID = seamCellImage.getInt( (int)leftPt.X, (int)leftPt.Y, (int)leftPt.Z );
				seamCellIDs[i][0] = leftID;
				Vector3f rightPt = right.getCurves().elementAt(i).elementAt(0);
				int rightID = seamCellImage.getInt( (int)rightPt.X, (int)rightPt.Y, (int)rightPt.Z );
				seamCellIDs[i][1] = rightID;

//				System.err.println( "left " + leftID + " right " + rightID );
			}
		}

		// 2. The center curve is generated from the center points using natural splines
		// to fit the points. Natural splines generate curves that pass through the control points, have continuous
		// first and second derivatives and minimize the bending between points.
		afTimeC = new float[center.size()];
		centerSpline = smoothCurve(center, afTimeC);

		centerPositions = new VOIContour(false);
		leftPositions = new VOIContour(false);
		rightPositions = new VOIContour(false);

		wormDiameters = new Vector<Float>();
		rightVectors = new Vector<Vector3f>();
		upVectors = new Vector<Vector3f>();
		normalVectors = new Vector<Vector3f>();

		// 3. The center curve is uniformly sampled along the length of the curve.
		// The step size is set to be one voxel. This determines the length of the final straightened
		// image and ensures that each slice in the straightened image is equally spaced. The points
		// along the curve are the center-points of the output slices.
		// 4. Each spline can be parametrized with a parameter t, where the start of the curve has t = 0
		// and the end of the curve has t = 1. This parameter t is calculated on the center curve, and
		// used to determine the corresponding locations on the left and right hand curves, which may be
		// longer or shorter than the center curve, depending on how the worm bends. Using the parametrization
		// ensures that the left and right hand curves are sampled the same number of times as the center curve
		// and that the points from start to end on all curves are included.
		// 5. Given the current point on the center curve and the corresponding positions on the left and right hand
		// curves, the 2D sampling plane can be defined. The center point of the plane is the current point on the
		// center curve.
		// The plane normal is the first derivative of the center line spline. The plane horizontal axis is the vector
		// from the position on the left hand curve to the position on the right hand curve.
		// The plane vertical axis is the cross-product of the plane normal with the plane horizontal axis.
		// This method fully defines the sample plane location and orientation as it sweeps through the 3D volume of the
		// worm.

		float length = centerSpline.GetLength(0, 1) / stepSize;
		//		System.err.println( "Generate Curves " + length );
		int maxLength = (int)Math.ceil(length);
		float step = stepSize;
		if ( maxLength != length )
		{
			step = stepSize * (length / maxLength);
		}
		allTimes = new float[maxLength + 1];
		for (int i = 0; i <= maxLength; i++) {
			final float t = centerSpline.GetTime(i*step);
			centerPositions.add(centerSpline.GetPosition(t));
			allTimes[i] = t;
		}



		extent = -1;
		float minDiameter = Float.MAX_VALUE;


		splineRangeIndex = new int[center.size()];
		int maxIndex = -1;
		for ( int i = 0; i < center.size(); i++ )
		{
			splineRangeIndex[i] = -1;
			int minIndex = -1;
			float minDist = Float.MAX_VALUE;
			for ( int j = 0; j <= maxLength; j++ )
			{	
				//				final float t = centerSpline.GetTime(j*step);
				//				Vector3f centerPt = centerSpline.GetPosition(t);


				Vector3f centerPt = centerPositions.elementAt(j);
				if ( centerPt.isEqual(center.elementAt(i)) )
				{
					splineRangeIndex[i] = j;
					break;
				}
				float distance = centerPt.distance(center.elementAt(i) );
				if ( distance < minDist )
				{
					minDist = distance;
					minIndex = j;
				}
			}
			if ( (splineRangeIndex[i] == -1) && (minIndex != -1 ))
			{
				splineRangeIndex[i] = minIndex;
			}
			//			System.err.println( i + " " + indexes[i] );

			if ( i == 0 )
			{
				maxIndex = splineRangeIndex[i];
			}
			if ( splineRangeIndex[i] < maxIndex )
			{
				return false;
			}
			if ( splineRangeIndex[i] > maxIndex )
			{
				maxIndex = splineRangeIndex[i];
			}
		}

		if ( seamCellIDs != null )
		{
			allSeamCellIDs = new int[maxLength+1][4];
		}
		float[] diameters = new float[maxLength+1];
		Vector3f[] rightVectorsInterp = new Vector3f[maxLength+1];
		for ( int i = 0; i < center.size()-1; i++ )
		{
			int startIndex = splineRangeIndex[i];
			int endIndex = splineRangeIndex[i+1];
			//			System.err.println( startIndex + "   " + endIndex );
			float startRadius = (left.getCurves().elementAt(i).elementAt(0).distance(right.getCurves().elementAt(i).elementAt(0)))/2f;
			float endRadius = (left.getCurves().elementAt(i+1).elementAt(0).distance(right.getCurves().elementAt(i+1).elementAt(0)))/2f;
			for ( int j = startIndex; j <= endIndex; j++ )
			{
				float interp = (j - startIndex) / (float)(endIndex - startIndex);
				diameters[j] = (1 - interp) * startRadius + interp * endRadius;

				//				System.err.println( j + "     " + diameters[j] );
			}

			Vector3f startRight = Vector3f.sub( right.getCurves().elementAt(i).elementAt(0), left.getCurves().elementAt(i).elementAt(0) );
			startRight.normalize();
			Vector3f endRight = Vector3f.sub( right.getCurves().elementAt(i+1).elementAt(0), left.getCurves().elementAt(i+1).elementAt(0) );
			endRight.normalize();

			//			startRight.copy(Vector3f.UNIT_X);
			//			endRight.copy(Vector3f.UNIT_Y);
			Vector3f rotationAxis = Vector3f.cross(startRight, endRight);
			rotationAxis.normalize();
			float angle = startRight.angle(endRight);
			int steps = endIndex - startIndex;
			float fAngle = angle / steps;
			Matrix3f mat = new Matrix3f(true);
			mat.fromAxisAngle(rotationAxis, angle);
			//			System.err.println( rotationAxis + "   " + angle );


			for ( int j = startIndex; j <= endIndex; j++ )
			{
				float interp = (j - startIndex) / (float)(endIndex - startIndex);
				diameters[j] = (1 - interp) * startRadius + interp * endRadius;

				if ( allSeamCellIDs != null )
				{
					// left ID:
					allSeamCellIDs[j][0] = seamCellIDs[i][0];
					allSeamCellIDs[j][1] = seamCellIDs[i+1][0];
					// right ID:
					allSeamCellIDs[j][2] = seamCellIDs[i][1];
					allSeamCellIDs[j][3] = seamCellIDs[i+1][1];
				}


				mat.fromAxisAngle(rotationAxis, (j - startIndex)*fAngle);
				rightVectorsInterp[j] = mat.multRight(startRight);
				rightVectorsInterp[j].normalize();

				//				System.err.println( j + "    " + startRadius + " " + diameters[j] + " " + endRadius );
				//				System.err.println( j + "    " + startRight + "    " + rightVectorsInterp[j] + "    " + endRight );
			}
			float diffAngle = rightVectorsInterp[endIndex].angle(endRight);
			//			System.err.println(i + " " + rightVectorsInterp[endIndex]);
			//			System.err.println(i + " " + endRight + "    " + diffAngle);
			if ( (diffAngle/Math.PI)*180 > 2 )
			{
				angle = (float) ((2*Math.PI) - angle); 				
				fAngle = angle / steps;
				//				System.err.println( "TRYING AGAIN" );
				//				System.err.println( rotationAxis + "   " + angle );

				for ( int j = startIndex; j <= endIndex; j++ )
				{					
					mat.fromAxisAngle(rotationAxis, (j - startIndex)*fAngle);
					rightVectorsInterp[j] = mat.multRight(startRight);
					rightVectorsInterp[j].normalize();
					//					System.err.println( j + "    " + startRadius + " " + diameters[j] + " " + endRadius );
					//					System.err.println( j + "    " + startRight + "    " + rightVectorsInterp[j] + "    " + endRight );
				}
			}
			diffAngle = rightVectorsInterp[endIndex].angle(endRight);
			if ( (diffAngle/Math.PI)*180 > 2 )
			{
				//				System.err.println("ERROR");
			}
		}

		for ( int i = 0; i < rightVectorsInterp.length; i++ )
		{
			if ( rightVectorsInterp[i] == null )
			{
				System.err.println( step + " " + maxLength + " " + stepSize + " " + length );
			}
		}

		//		System.err.println( maxLength );
		//		totalCurvature = 0;
		for (int i = 0; i <= maxLength; i++) {
			final float t = allTimes[i];
			Vector3f normal = centerSpline.GetTangent(t);
			final Vector3f rightDir = rightVectorsInterp[i];
			float diameter = diameters[i];
			if (diameter > extent) {
				extent = (int) Math.ceil(diameter);
			}
			if ( (diameter > 0) && (diameter < minDiameter) )
			{
				minDiameter = diameter;
			}
			wormDiameters.add(diameter);
			rightVectors.add(rightDir);
			if (rightDir == null || normal == null )
			{
				System.err.println("error");
			}

			final Vector3f upDir = Vector3f.cross(normal, rightDir);
			upDir.normalize();
			upVectors.add(upDir);

			Vector3f normalTest = Vector3f.cross(rightDir, upDir);
			normalVectors.add(normalTest);

			Vector3f rightPt = new Vector3f(rightDir);
			rightPt.scale(diameters[i]);
			rightPt.add(centerPositions.elementAt(i));
			rightPositions.add(rightPt);
			//			
			Vector3f leftPt = new Vector3f(rightDir);
			leftPt.scale(-diameters[i]);
			leftPt.add(centerPositions.elementAt(i));
			leftPositions.add(leftPt);


			//			System.err.println( i + "     " + diameters[i] + "     " + leftPt.distance(rightPt));
		}		

		extent += DiameterBuffer;
		for ( int i = 0; i < wormDiameters.size(); i++ )
		{
			if ( wormDiameters.elementAt(i) < minDiameter )
			{
				wormDiameters.set(i, minDiameter);
			}
		}


		latticeSlices = new int[left.getCurves().size()];
		for ( int i = 0; i < afTimeC.length; i++ )
		{
			float min = Float.MAX_VALUE;
			int minIndex = -1;
			for ( int j = 0; j < allTimes.length; j++ )
			{
				float diff = Math.abs(allTimes[j] - afTimeC[i]);
				if ( diff < min )
				{
					min = diff;
					minIndex = j;
				}
			}
			latticeSlices[i] = minIndex;
		}

		// 6. Once the sample planes are defined, the worm cross-section within each plane needs to be determined.
		// Without a model of the worm cross-section the sample planes will overlap in areas where the worm folds
		// back on top of itself. The first step in modeling the worm cross-section is to define an ellipse
		// within each sample plane, centered in the plane. The long axis of the ellipse is parallel to the
		// horizontal axis of the sample plane. The length is the distance between the left and right hand points.
		// The ellipse short axis is in the direction of the plane vertical axis; the length is set to 1/2 the length
		// of the ellipse long axis. This ellipse-based model approximates the overall shape of the worm, however
		// it cannot model how the worm shape changes where sections of the worm press against each other.
		// The next step of the algorithm attempts to solve this problem.
		short sID = (short) 1;
		displayContours = new VOI(sID, "wormContours");
//		System.err.println("generateCurves " + paddingFactor );
		for (int i = 0; i < centerPositions.size(); i += contourStepSize) {
			Vector3f rkEye = centerPositions.elementAt(i);
			Vector3f rkRVector = rightVectors.elementAt(i);
			Vector3f rkUVector = upVectors.elementAt(i);

			VOIContour ellipse = new VOIContour(true);
			ellipse.setVolumeDisplayRange(minRange);


			float radius = (float) (1.05 * rightPositions.elementAt(i).distance(leftPositions.elementAt(i))/(2f));
			radius += paddingFactor;

			makeEllipse2DA(rkRVector, rkUVector, rkEye, radius, ellipse);	

			//			makeEllipse2D(rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), ellipse, 32);
			displayContours.getCurves().add(ellipse);

			if ( (contourStepSize != 1) && (i != (centerPositions.size() - 1)) && (i + 10) >= centerPositions.size() ) {
				// add last contour:
				i = centerPositions.size() - 1;
				rkEye = centerPositions.elementAt(i);
				rkRVector = rightVectors.elementAt(i);
				rkUVector = upVectors.elementAt(i);

				ellipse = new VOIContour(true);
				ellipse.setVolumeDisplayRange(minRange);


				radius = (float) (1.05 * rightPositions.elementAt(i).distance(leftPositions.elementAt(i))/(2f));
				radius += paddingFactor;

				makeEllipse2DA(rkRVector, rkUVector, rkEye, radius, ellipse);	
				displayContours.getCurves().add(ellipse);
			}
		}
		sID++;
		centerLine = new VOI(sID, "center line");
		centerLine.getCurves().add(centerPositions);
		centerLine.setColor(Color.red);
		centerPositions.update(new ColorRGBA(1, 0, 0, 1));

		sID++;
		leftLine = new VOI(sID, "left line");
		leftLine.getCurves().add(leftPositions);
		leftLine.setColor(Color.magenta);
		leftPositions.update(new ColorRGBA(1, 0, 1, 1));

		sID++;
		rightLine = new VOI(sID, "right line");
		rightLine.getCurves().add(rightPositions);
		rightLine.setColor(Color.green);
		rightPositions.update(new ColorRGBA(0, 1, 0, 1));

		imageA.registerVOI(leftLine);
		imageA.registerVOI(rightLine);
		imageA.registerVOI(centerLine);

		return true;
	}

	protected double GetSquared ( Vector3f point, Ellipsoid3f ellipsoid )
	{
		// compute coordinates of point in ellipsoid coordinate system
		Vector3d kDiff = new Vector3d( point.X - ellipsoid.Center.X, point.Y - ellipsoid.Center.Y, point.Z - ellipsoid.Center.Z);
		Vector3d kEPoint = new Vector3d( 
				(kDiff.X * ellipsoid.Axis[0].X + kDiff.Y * ellipsoid.Axis[0].Y + kDiff.Z * ellipsoid.Axis[0].Z),
				(kDiff.X * ellipsoid.Axis[1].X + kDiff.Y * ellipsoid.Axis[1].Y + kDiff.Z * ellipsoid.Axis[1].Z),
				(kDiff.X * ellipsoid.Axis[2].X + kDiff.Y * ellipsoid.Axis[2].Y + kDiff.Z * ellipsoid.Axis[2].Z) );

		final float[] afExtent = ellipsoid.Extent;
		double fA2 = afExtent[0]*afExtent[0];
		double fB2 = afExtent[1]*afExtent[1];
		double fC2 = afExtent[2]*afExtent[2];
		double fU2 = kEPoint.X*kEPoint.X;
		double fV2 = kEPoint.Y*kEPoint.Y;
		double fW2 = kEPoint.Z*kEPoint.Z;
		double fA2U2 = fA2*fU2, fB2V2 = fB2*fV2, fC2W2 = fC2*fW2;

		// initial guess
		double fURatio = kEPoint.X/afExtent[0];
		double fVRatio = kEPoint.Y/afExtent[1];
		double fWRatio = kEPoint.Z/afExtent[2];
		double fT;
		if (fURatio*fURatio+fVRatio*fVRatio+fWRatio*fWRatio < 1.0f)
		{
			fT = 0.0f;
		}
		else
		{
			double fMax = afExtent[0];
			if (afExtent[1] > fMax)
			{
				fMax = afExtent[1];
			}
			if (afExtent[2] > fMax)
			{
				fMax = afExtent[2];
			}

			fT = fMax*kEPoint.length();
		}

		// Newton's method
		final int iMaxIteration = 64;
		double fP = 1.0f, fQ = 1.0f, fR = 1.0f;
		for (int i = 0; i < iMaxIteration; i++)
		{
			fP = fT+fA2;
			fQ = fT+fB2;
			fR = fT+fC2;
			double fP2 = fP*fP;
			double fQ2 = fQ*fQ;
			double fR2 = fR*fR;
			double fS = fP2*fQ2*fR2-fA2U2*fQ2*fR2-fB2V2*fP2*fR2-fC2W2*fP2*fQ2;
			if (Math.abs(fS) < Mathf.ZERO_TOLERANCE)
			{
				break;
			}

			double fPQ = fP*fQ, fPR = fP*fR, fQR = fQ*fR, fPQR = fP*fQ*fR;
			double fDS = (2.0f)*(fPQR*(fQR+fPR+fPQ)-fA2U2*fQR*(fQ+fR)-
					fB2V2*fPR*(fP+fR)-fC2W2*fPQ*(fP+fQ));
			fT -= fS/fDS;
		}

		Vector3d kClosest = new Vector3d(fA2*kEPoint.X/fP,
				fB2*kEPoint.Y/fQ,
				fC2*kEPoint.Z/fR);
		kDiff = Vector3d.sub( kClosest, kEPoint );
		double fSqrDistance = kDiff.squaredLength();

		return fSqrDistance;
	}

	/**
	 * Produces an image segmentation of the left-right fluorescent markers using the lattice points as a guide. The
	 * fluorescent markers in the worm volume are automatically segmented, using the positions of the lattice as a
	 * guide. During segmentation the voxels that fall within the segmented regions are labeled with a corresponding
	 * marker ID. In the event that not all 10 markers are segmented, which may occur if the input lattice points are
	 * placed just outside the fluorescent marker boundaries, the lattice points will be modified to be inside the
	 * segmented boundaries.
	 * 
	 * @param image
	 * @param left
	 * @param right
	 * @param markerIDs
	 * @param markerVolumes
	 * @param segAll
	 * @return
	 */
	//	private ModelImage segmentMarkers(final ModelImage image, final VOIContour left, final VOIContour right, final int[] markerIDs,
	//			final int[][] markerVolumes, final boolean segAll) {
	//		String imageName = image.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//		final ModelImage markerSegmentation = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName + "_markers.xml");
	//		JDialogBase.updateFileInfo(image, markerSegmentation);
	//
	//		// Generate the gradient magnitude image:
	//		ModelImage gmImage = VolumeImage.getGradientMagnitude(image, 0);
	//
	//		// determine the maxiumum image value of the fluorescent markers at each lattice point:
	//		float maxValue = -Float.MAX_VALUE;
	//		for (int i = 0; i < left.size(); i++) {
	//			Vector3f temp = right.elementAt(i);
	//			int x = Math.round(temp.X);
	//			int y = Math.round(temp.Y);
	//			int z = Math.round(temp.Z);
	//			float value;
	//			if (image.isColorImage()) {
	//				value = image.getFloatC(x, y, z, 2); // Green Channel contains markers
	//			} else {
	//				value = image.getFloat(x, y, z);
	//			}
	//			if (value > maxValue) {
	//				maxValue = value;
	//			}
	//			temp = left.elementAt(i);
	//			x = Math.round(temp.X);
	//			y = Math.round(temp.Y);
	//			z = Math.round(temp.Z);
	//			if (image.isColorImage()) {
	//				value = image.getFloatC(x, y, z, 2); // Green Channel contains markers
	//			} else {
	//				value = image.getFloat(x, y, z);
	//			}
	//			if (value > maxValue) {
	//				maxValue = value;
	//			}
	//		}
	//
	//		// Find the minimum distance between all lattice points (left, right):
	//		float minOverall = Float.MAX_VALUE;
	//		for (int i = 0; i < left.size(); i++) {
	//			final Vector3f tempL = left.elementAt(i);
	//			for (int j = i + 1; j < left.size(); j++) {
	//				final float dist = tempL.distance(left.elementAt(j));
	//				if (dist < minOverall) {
	//					minOverall = dist;
	//				}
	//			}
	//			for (int j = 1; j < right.size(); j++) {
	//				final float dist = tempL.distance(right.elementAt(j));
	//				if (dist < minOverall) {
	//					minOverall = dist;
	//				}
	//			}
	//		}
	//		for (int i = 0; i < right.size(); i++) {
	//			final Vector3f tempR = right.elementAt(i);
	//			for (int j = i + 1; j < right.size(); j++) {
	//				final float dist = tempR.distance(right.elementAt(j));
	//				if (dist < minOverall) {
	//					minOverall = dist;
	//				}
	//			}
	//			for (int j = 1; j < left.size(); j++) {
	//				final float dist = tempR.distance(left.elementAt(j));
	//				if (dist < minOverall) {
	//					minOverall = dist;
	//				}
	//			}
	//		}
	//
	//		minOverall *= 0.75;
	//		final int step = (int) Math.max(1, (minOverall / 3f));
	//
	//		final Vector<Vector3f> seedList = new Vector<Vector3f>();
	//		final VOIContour[][] savedSeedList = new VOIContour[left.size()][2];
	//		final int[][] counts = new int[left.size()][2];
	//		for (int diameter = step; diameter <= minOverall; diameter += step) {
	//			for (int i = 0; i < left.size(); i++) {
	//				// int diameter = (int) (minDistance[i][0]/2f);
	//				if (savedSeedList[i][0] == null) {
	//					savedSeedList[i][0] = new VOIContour(false);
	//					seedList.clear();
	//					seedList.add(new Vector3f(left.elementAt(i)));
	//					counts[i][0] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(left.elementAt(i)), seedList,
	//							savedSeedList[i][0], diameter, i + 1);
	//					// System.err.println( "left " + i + " " + counts[i][0] );
	//				} else {
	//					seedList.clear();
	//					counts[i][0] += fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(left.elementAt(i)), savedSeedList[i][0],
	//							seedList, diameter, i + 1);
	//					savedSeedList[i][0].clear();
	//					savedSeedList[i][0].addAll(seedList);
	//					// System.err.println( "left " + i + " " + counts[i][0] );
	//				}
	//			}
	//			for (int i = 0; i < right.size(); i++) {
	//				// int diameter = (int) (minDistance[i][1]/2f);
	//				if (savedSeedList[i][1] == null) {
	//					savedSeedList[i][1] = new VOIContour(false);
	//					seedList.clear();
	//					seedList.add(new Vector3f(right.elementAt(i)));
	//					counts[i][1] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(right.elementAt(i)), seedList,
	//							savedSeedList[i][0], diameter, i + 1);
	//					// System.err.println( "right " + i + " " + counts[i][1] );
	//				} else {
	//					seedList.clear();
	//					counts[i][1] += fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(right.elementAt(i)), savedSeedList[i][0],
	//							seedList, diameter, i + 1);
	//					savedSeedList[i][1].clear();
	//					savedSeedList[i][1].addAll(seedList);
	//					// System.err.println( "right " + i + " " + counts[i][1] );
	//				}
	//			}
	//		}
	//		for (int i = 0; i < left.size(); i++) {
	//			markerVolumes[i][0] = counts[i][0];
	//			markerVolumes[i][1] = counts[i][1];
	//		}
	//
	//		for (int i = 0; i < left.size(); i++) {
	//			if (counts[i][0] == 0) {
	//				// markerIDs[i] = 0;
	//				final Vector3f dir = Vector3f.sub(right.elementAt(i), left.elementAt(i));
	//				dir.normalize();
	//				dir.scale(step);
	//				seedList.clear();
	//				final Vector3f newPt = Vector3f.add(left.elementAt(i), dir);
	//				seedList.add(newPt);
	//				savedSeedList[i][0].clear();
	//				counts[i][0] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, newPt, seedList, savedSeedList[i][0], (int) minOverall, i + 1);
	//				if (counts[i][0] == 0) {
	//					seedList.clear();
	//					seedList.add(new Vector3f(left.elementAt(i)));
	//					savedSeedList[i][0].clear();
	//					counts[i][0] = fill(image, gmImage, markerSegmentation, 0, 0, new Vector3f(left.elementAt(i)), seedList, savedSeedList[i][0], step, i + 1);
	//				}
	//				markerIDs[i] = i + 1;
	//				moveMarker(markerSegmentation, left.elementAt(i), Vector3f.sub(left.elementAt(i), right.elementAt(i)), i + 1);
	//			} else {
	//				markerIDs[i] = i + 1;
	//				moveMarker(markerSegmentation, left.elementAt(i), Vector3f.sub(left.elementAt(i), right.elementAt(i)), i + 1);
	//			}
	//		}
	//		for (int i = 0; i < right.size(); i++) {
	//			if (counts[i][1] == 0) {
	//				// markerIDs[i] = 0;
	//				final Vector3f dir = Vector3f.sub(left.elementAt(i), right.elementAt(i));
	//				dir.normalize();
	//				dir.scale(step);
	//				seedList.clear();
	//				final Vector3f newPt = Vector3f.add(right.elementAt(i), dir);
	//				seedList.add(newPt);
	//				savedSeedList[i][1].clear();
	//				counts[i][1] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, newPt, seedList, savedSeedList[i][0], (int) minOverall, i + 1);
	//				if (counts[i][1] == 0) {
	//					seedList.clear();
	//					seedList.add(new Vector3f(right.elementAt(i)));
	//					savedSeedList[i][1].clear();
	//					counts[i][1] = fill(image, gmImage, markerSegmentation, 0, 0, new Vector3f(right.elementAt(i)), seedList, savedSeedList[i][1], step, i + 1);
	//					markerIDs[i] = i + 1;
	//					moveMarker(markerSegmentation, right.elementAt(i), Vector3f.sub(right.elementAt(i), left.elementAt(i)), i + 1);
	//				}
	//			} else {
	//				markerIDs[i] = i + 1;
	//				moveMarker(markerSegmentation, right.elementAt(i), Vector3f.sub(right.elementAt(i), left.elementAt(i)), i + 1);
	//			}
	//		}
	//		markerSegmentation.calcMinMax();
	//		// new ViewJFrameImage((ModelImage)markerSegmentation.clone());
	//
	//		if (segAll) {
	//			final ModelImage markerSegmentation2 = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName + "_markers2.xml");
	//			JDialogBase.updateFileInfo(image, markerSegmentation2);
	//
	//			seedList.clear();
	//			for (int i = 0; i < left.size(); i++) {
	//				seedList.add(new Vector3f(left.elementAt(i)));
	//				seedList.add(new Vector3f(right.elementAt(i)));
	//
	//				if (markerVolumes[i][0] == 0) {
	//					final Vector3f dir = Vector3f.sub(right.elementAt(i), left.elementAt(i));
	//					dir.normalize();
	//					dir.scale(step);
	//					final Vector3f newPt = Vector3f.add(left.elementAt(i), dir);
	//					seedList.add(newPt);
	//				}
	//				if (markerVolumes[i][1] == 0) {
	//					final Vector3f dir = Vector3f.sub(left.elementAt(i), right.elementAt(i));
	//					dir.normalize();
	//					dir.scale(step);
	//					final Vector3f newPt = Vector3f.add(right.elementAt(i), dir);
	//					seedList.add(newPt);
	//				}
	//			}
	//
	//			savedSeedList[0][0].clear();
	//			final int count = fill(image, gmImage, markerSegmentation2, 10, 0.1f * maxValue, Vector3f.ZERO, seedList, savedSeedList[0][0], Integer.MAX_VALUE,
	//					(int) (.75 * (image.getMax() - image.getMin())));
	//
	//			System.err.println("segment all " + count);
	//			markerSegmentation2.calcMinMax();
	//			new ViewJFrameImage((ModelImage) markerSegmentation2.clone());
	//		}
	//
	//		if ( gmImage != null )
	//		{
	//			gmImage.disposeLocal();
	//			gmImage = null;
	//		}
	//
	//		return markerSegmentation;
	//	}


	//	private ModelImage segmentMarkersSimple(final ModelImage image, final VOIContour left, final VOIContour right, final int[] markerIDs,
	//			final int[][] markerVolumes)
	//	{
	//		String imageName = image.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//		final ModelImage markerSegmentation = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName + "_markers.xml");
	//		JDialogBase.updateFileInfo(image, markerSegmentation);
	//
	//		for (int i = 0; i < left.size(); i++)
	//		{
	//			markerIDs[i] = i + 1;
	//
	//			Vector3f temp = right.elementAt(i);
	//			int x = Math.round(temp.X);
	//			int y = Math.round(temp.Y);
	//			int z = Math.round(temp.Z);
	//			markerSegmentation.set( x, y, z, markerIDs[i] );
	//
	//			temp = left.elementAt(i);
	//			x = Math.round(temp.X);
	//			y = Math.round(temp.Y);
	//			z = Math.round(temp.Z);
	//			markerSegmentation.set( x, y, z, markerIDs[i] );
	//
	//			markerVolumes[i][0] = 1;
	//			markerVolumes[i][1] = 1;
	//		}
	//
	//		return markerSegmentation;
	//	}




	/**
	 * Interpolates the input contour so that the spacing between contour points is <= 1 voxel.
	 * 
	 * @param contour
	 */
	protected void interpolateContour(final VOIContour contour) {
		int index = 0;
		while (index < contour.size()) {
			final Vector3f p1 = contour.elementAt(index);
			final Vector3f p2 = contour.elementAt( (index + 1) % contour.size());
			// System.err.println( index + " " + (index+1)%contour.size() );
			final float distance = p1.distance(p2);
			if (distance > 1) {
				final Vector3f dir = Vector3f.sub(p2, p1);
				dir.normalize();
				final int count = (int) distance;
				final float stepSize = distance / (count + 1);
				float currentStep = stepSize;
				index++;
				for (int i = 0; i < count; i++) {
					final Vector3f newPt = new Vector3f();
					newPt.scaleAdd(currentStep, dir, p1);
					contour.add(index++, newPt);
					// System.err.println( "    adding pt at " + (index-1) + " " + newPt.distance(p1) + " " +
					// newPt.distance(p2) );
					currentStep += stepSize;
				}
			} else {
				index++;
			}
		}
		// System.err.println(contour.size());
		// for ( int i = 0; i < contour.size(); i++ )
		// {
		// System.err.println( contour.elementAt(i) + " " + contour.elementAt(i).distance(
		// contour.elementAt((i+1)%contour.size() ) ) );
		// }
	}


	/**
	 * Once the 3D model of the worm is finalized, a 2D slice plane is swept through the model. At each sample-point
	 * along the 3D center spline of the worm, the 2D plane is intersected with the original 3D volume. Voxels that fall
	 * outside the updated 2D worm contour are set to the image minimum value (typically = 0). Voxels that fall inside
	 * the 2D worm contour are copied into the output slice. The set of 2D slices from the worm head to tail are
	 * concatenated to form the final 3D straightened volume.
	 * 
	 * During the straightening step as well as during the model-building process or anytime the 2D sample plane is
	 * intersected with the 3D volume steps are taken by the algorithm to minimize sampling artifacts. Due to the
	 * twisted configuration of the worm, sampling the volume along the outer-edge of a curve will result under-sampling
	 * the data while the inside edge of the curve will be over-sampled.
	 * 
	 * To reduce sampling artifacts the sample planes are interpolated between sample points, using the maximum distance
	 * between points along consecutive contours to determine the amount of super-sampling. The multiple sample planes
	 * are averaged to produce the final slice in the straightened image. In addition, each contour is modeled as having
	 * the thickness of one voxel and sample points that fall between voxels in the volume are trilinearly interpolated.
	 * 
	 * @param image
	 * @param resultExtents
	 * @param baseName
	 * @param model
	 * @param saveStats
	 * @param displayResult
	 * @param saveAsTif
	 */
	//	private void straighten(final ModelImage image, final int[] resultExtents, final String baseName, final ModelImage model, final boolean saveStats,
	//			final boolean displayResult, final boolean saveAsTif) {
	//		String imageName = image.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//
	//		final int colorFactor = image.isColorImage() ? 4 : 1;
	//		final float[] values = new float[resultExtents[0] * resultExtents[1] * colorFactor];
	//
	//		//		float[] dataOrigin = null;
	//		//		float[] sampleDistance = null;
	//		//		float[] sampleDistanceP = null;
	//
	//		ModelImage resultImage = new ModelImage(image.getType(), resultExtents, imageName + "_straight.xml");
	//		JDialogBase.updateFileInfo(image, resultImage);
	//		resultImage.setResolutions(new float[] {1, 1, 1});
	//
	//		//		ModelImage straightToOrigin = null;
	//		ModelImage originToStraight = null;
	//		//		ModelImage overlap2 = null;
	//
	//		if (saveStats) {
	//			//			dataOrigin = new float[resultExtents[0] * resultExtents[1] * 4];
	//			//			straightToOrigin = new ModelImage(ModelStorageBase.ARGB_FLOAT, resultExtents, imageName + "_toOriginal.xml");
	//			//			JDialogBase.updateFileInfo(image, straightToOrigin);
	//			//			straightToOrigin.setResolutions(new float[] {1, 1, 1});
	//			//			for (int i = 0; i < straightToOrigin.getDataSize(); i++) {
	//			//				straightToOrigin.set(i, 0);
	//			//			}
	//
	//			originToStraight = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), imageName + "_toStraight.xml");
	//			JDialogBase.updateFileInfo(image, originToStraight);
	//			for (int i = 0; i < originToStraight.getDataSize(); i++) {
	//				originToStraight.set(i, 0);
	//			}
	//
	//			//			overlap2 = new ModelImage(ModelStorageBase.FLOAT, resultExtents, imageName + "_sampleDensity.xml");
	//			//			JDialogBase.updateFileInfo(image, overlap2);
	//			//			overlap2.setResolutions(new float[] {1, 1, 1});
	//			//
	//			//			sampleDistance = new float[resultExtents[0] * resultExtents[1]];
	//			//			sampleDistanceP = new float[resultExtents[0] * resultExtents[1] * 4];
	//			//			final int length = resultExtents[0] * resultExtents[1];
	//			//			for (int j = 0; j < length; j++) {
	//			//				sampleDistance[j] = 0;
	//			//				for (int c = 0; c < 4; c++) {
	//			//					// sampleDistance[j * 4 + c] = 0;
	//			//					sampleDistanceP[j * 4 + c] = 0;
	//			//				}
	//			//			}
	//		}
	//
	//		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
	//			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	//			final Vector3f[] corners = new Vector3f[4];
	//			for (int j = 0; j < 4; j++) {
	//				corners[j] = kBox.elementAt(j);
	//			}
	//			float planeDist = -Float.MAX_VALUE;
	//			if (i < (samplingPlanes.getCurves().size() - 1)) {
	//				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//				for (int j = 0; j < 4; j++) {
	//					final float distance = corners[j].distance(kBox.elementAt(j));
	//					if (distance > planeDist) {
	//						planeDist = distance;
	//					}
	//					// System.err.println( distance + "  " + centerPositions.elementAt(i).distance(
	//					// centerPositions.elementAt(i+1) ) );
	//				}
	//				// System.err.println("");
	//			}
	//			try {
	//				for (int j = 0; j < values.length / colorFactor; j++) {
	//					if (colorFactor == 4) {
	//						values[ (j * 4) + 0] = (float) image.getMinA();
	//						values[ (j * 4) + 1] = (float) image.getMinR();
	//						values[ (j * 4) + 2] = (float) image.getMinG();
	//						values[ (j * 4) + 3] = (float) image.getMinB();
	//					}
	//					/* not color: */
	//					else {
	//						values[j] = (float) image.getMin();
	//					}
	//					//					if (dataOrigin != null) {
	//					//						for (int c = 0; c < 4; c++) {
	//					//							dataOrigin[j * 4 + c] = 0;
	//					//						}
	//					//					}
	//				}
	//
	//				int planeCount = 0;
	//				if (i < (samplingPlanes.getCurves().size() - 1)) {
	//					planeDist *= 3;
	//					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
	//					final Vector3f[] steps = new Vector3f[4];
	//					final Vector3f[] cornersSub = new Vector3f[4];
	//					for (int j = 0; j < 4; j++) {
	//						steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
	//						steps[j].scale(1f / planeDist);
	//						cornersSub[j] = new Vector3f(corners[j]);
	//						//						System.err.print( kBox.elementAt(j) + "      " );
	//					}
	//					//					System.err.println("");
	//					for (int j = 0; j < planeDist; j++) {
	//						writeDiagonal(image, model, originToStraight, 0, i, resultExtents, cornersSub, values, null, null, null);
	//						//						writeDiagonal(image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin, sampleDistance, sampleDistanceP);
	//						planeCount++;
	//						for (int k = 0; k < 4; k++) {
	//							cornersSub[k].add(steps[k]);
	//						}
	//					}
	//				} else {
	//					planeDist = 15;
	//					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
	//					final Vector3f[] steps = new Vector3f[4];
	//					final Vector3f[] cornersSub = new Vector3f[4];
	//					for (int j = 0; j < 4; j++) {
	//						steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
	//						steps[j].scale(1f / planeDist);
	//						// cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
	//						cornersSub[j] = new Vector3f(corners[j]);
	//					}
	//					for (int j = 0; j < planeDist; j++) {
	//						writeDiagonal(image, model, originToStraight, 0, i, resultExtents, cornersSub, values, null, null, null);
	//						//						writeDiagonal(image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin, sampleDistance, sampleDistanceP);
	//						planeCount++;
	//						for (int k = 0; k < 4; k++) {
	//							cornersSub[k].add(steps[k]);
	//						}
	//					}
	//					// writeDiagonal( image, model, originToStraight, 0, i, resultExtents, corners, values, dataOrigin);
	//				}
	//				for (int j = 0; j < values.length / colorFactor; j++) {
	//					if (colorFactor == 4) {
	//						values[ (j * 4) + 1] /= planeCount;
	//						values[ (j * 4) + 2] /= planeCount;
	//						values[ (j * 4) + 3] /= planeCount;
	//					}
	//					/* not color: */
	//					else {
	//						values[j] /= planeCount;
	//					}
	//					//					if (dataOrigin != null) {
	//					//						for (int c = 1; c < 4; c++) {
	//					//							dataOrigin[j * 4 + c] /= planeCount;
	//					//						}
	//					//					}
	//				}
	//
	//				resultImage.importData(i * values.length, values, false);
	//				//				if (straightToOrigin != null) {
	//				//					straightToOrigin.importData(i * dataOrigin.length, dataOrigin, false);
	//				//				}
	//				//				if (overlap2 != null) {
	//				//					overlap2.importData(i * sampleDistance.length, sampleDistance, false);
	//				//				}
	//
	//			} catch (final IOException e) {
	//				e.printStackTrace();
	//			}
	//		}
	//
	//		VOI transformedAnnotations = null;
	//		if (saveStats) {
	//			//			testOriginToStraight(model, originToStraight);
	//			//			saveImage(baseName, overlap2, true);
	//			//
	//			//			saveImage(baseName, straightToOrigin, false);
	//			saveImage(baseName, originToStraight, false);
	//
	//			// calculate the transformed origin point:
	//			if ( (model != null) && (originToStraight != null) && (wormOrigin != null)) {
	//				transformedOrigin = originToStraight(model, originToStraight, wormOrigin, "wormOrigin");
	//			}
	//			else if ( transformedOrigin == null )
	//			{
	//				transformedOrigin = new Vector3f();
	//			}
	//
	//			transformedAnnotations = saveAnnotationStatistics(outputDirectory + File.separator, model, originToStraight, resultExtents, "_after");
	//			//			straightToOrigin.disposeLocal();
	//			//			straightToOrigin = null;
	//			//
	//			//			overlap2.disposeLocal();
	//			//			overlap2 = null;
	//
	//			short id = (short) image.getVOIs().getUniqueID();
	//			final VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float) Math.random());
	//			final VOIContour leftSide = new VOIContour(false);
	//			final VOIContour rightSide = new VOIContour(false);
	//			lattice.getCurves().add(leftSide);
	//			lattice.getCurves().add(rightSide);
	//			for (int i = 0; i < left.size(); i++) {
	//
	//				// USE backup markers
	//				final Vector3f leftPt = originToStraight(model, originToStraight, leftBackup.elementAt(i), "left" + i);
	//
	//				// USE backup markers
	//				final Vector3f rightPt = originToStraight(model, originToStraight, rightBackup.elementAt(i), "right" + i);
	//
	//				if (leftPt.isEqual(Vector3f.ZERO) || rightPt.isEqual(Vector3f.ZERO)) {
	//					System.err.println("    " + imageA.getImageName() + " " + i + " " + leftPt + "     " + rightPt);
	//				} else if ( (leftSide.size() > 0) && ( (leftPt.Z <= leftSide.lastElement().Z) || (rightPt.Z <= rightSide.lastElement().Z))) {
	//					System.err.println("    " + imageA.getImageName() + " " + i + " " + leftPt + "     " + rightPt);
	//				} else {
	//					leftSide.add(leftPt);
	//					rightSide.add(rightPt);
	//				}
	//			}
	//			final float[] leftDistances = new float[leftSide.size()];
	//			final float[] rightDistances = new float[leftSide.size()];
	//			for (int i = 0; i < leftSide.size(); i++) {
	//				leftDistances[i] = 0;
	//				rightDistances[i] = 0;
	//				if (i > 1) {
	//					leftDistances[i] = leftSide.elementAt(i).distance(leftSide.elementAt(i - 1));
	//					rightDistances[i] = rightSide.elementAt(i).distance(rightSide.elementAt(i - 1));
	//				}
	//			}
	//
	//			resultImage.registerVOI(lattice);
	//			lattice.setColor(new Color(0, 0, 255));
	//			lattice.getCurves().elementAt(0).update(new ColorRGBA(0, 0, 1, 1));
	//			lattice.getCurves().elementAt(1).update(new ColorRGBA(0, 0, 1, 1));
	//			lattice.getCurves().elementAt(0).setClosed(false);
	//			lattice.getCurves().elementAt(1).setClosed(false);
	//
	//			id = (short) image.getVOIs().getUniqueID();
	//			for (int j = 0; j < leftSide.size(); j++) {
	//				id = (short) image.getVOIs().getUniqueID();
	//				final VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float) Math.random());
	//				final VOIContour mainAxis = new VOIContour(false);
	//				mainAxis.add(leftSide.elementAt(j));
	//				mainAxis.add(rightSide.elementAt(j));
	//				marker.getCurves().add(mainAxis);
	//				marker.setColor(new Color(255, 255, 0));
	//				mainAxis.update(new ColorRGBA(1, 1, 0, 1));
	//				if (j == 0) {
	//					marker.setColor(new Color(0, 255, 0));
	//					mainAxis.update(new ColorRGBA(0, 1, 0, 1));
	//				}
	//				resultImage.registerVOI(marker);
	//			}
	//
	//			String voiDir = outputDirectory + File.separator + "straightened_lattice"
	//					+ File.separator;
	//			saveAllVOIsTo(voiDir, resultImage);
	//
	//			final VOIVector temp = resultImage.getVOIsCopy();
	//			resultImage.resetVOIs();
	//			if (transformedAnnotations != null) {
	//
	//				if ( transformedAnnotations.getCurves().size() > 0 )
	//				{
	//					resultImage.registerVOI(transformedAnnotations);
	//					voiDir = outputDirectory + File.separator + "straightened_annotations"
	//							+ File.separator;
	//					saveAllVOIsTo(voiDir, resultImage);
	//				}
	//			}
	//			saveLatticeStatistics(outputDirectory + File.separator, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");
	//
	//			resultImage.restoreVOIs(temp);
	//			if (transformedAnnotations != null) {
	//				resultImage.registerVOI(transformedAnnotations);
	//			}
	//
	//			saveNeuriteData( imageA, resultImage, model, originToStraight, baseName );
	//
	//
	//			//			final int[] markerIDs = new int[leftSide.size()];
	//			//			final int[][] markerVolumes = new int[leftSide.size()][2];
	//			//			ModelImage straightMarkers;
	//			//			if ( maskImage == null )
	//			//			{
	//			//				straightMarkers = segmentMarkers(resultImage, leftSide, rightSide, markerIDs, markerVolumes, false);
	//			//			}
	//			//			else
	//			//			{
	//			//				straightMarkers = segmentMarkersSimple(resultImage, leftSide, rightSide, markerIDs, markerVolumes);
	//			//			}
	//			saveLatticePositions(imageA, originToStraight, leftSide, rightSide, markerVolumes, "_after");
	//
	//			//			saveImage(baseName, straightMarkers, true);
	//			//			if (displayResult) {
	//			//				straightMarkers.calcMinMax();
	//			//				new ViewJFrameImage(straightMarkers);
	//			//			} else {
	//			//				straightMarkers.disposeLocal();
	//			//				straightMarkers = null;
	//			//			}
	//		}
	//
	//		saveImage(baseName, resultImage, saveAsTif);
	//		if (displayResult) {
	//			resultImage.calcMinMax();
	//			new ViewJFrameImage(resultImage);
	//		} else {
	//			resultImage.disposeLocal();
	//			resultImage = null;
	//		}
	//
	//		if (originToStraight != null) {
	//			originToStraight.disposeLocal();
	//			originToStraight = null;
	//		}
	//	}


	protected Vector<Float> loadDiameters( String imageDir ) {

		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			return null;
		}
		voiDir = imageDir + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			return null;
		}

		File file = new File(voiDir + "Diameters.csv");
		if (file.exists()) {
			return null;
		}

		try {
			Vector<Float> diameters = new Vector<Float>();
			final FileReader fr = new FileReader(file);
			final BufferedReader br = new BufferedReader(fr);

			String line = br.readLine(); // first line is header
			line = br.readLine();

			while ( line != null )
			{
				StringTokenizer st = new StringTokenizer(line, ",");
				if (st.hasMoreTokens()) {
					diameters.add(Float.valueOf(st.nextToken()));
				}
				line = br.readLine();
			}

			br.close();

			return diameters;
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
		return null;
	}
	protected VOIContour loadPositions( String imageDir, String name ) {

		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			return null;
		}
		voiDir = imageDir + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			return null;
		}

		File file = new File(voiDir + name + "Positions.csv");
		if ( !file.exists()) {
			return null;
		}

		try {
			VOIContour contour = new VOIContour(false);
			final FileReader fr = new FileReader(file);
			final BufferedReader br = new BufferedReader(fr);

			String line = br.readLine(); // first line is header
			line = br.readLine();

			while ( line != null )
			{
				Vector3f pos = new Vector3f();
				StringTokenizer st = new StringTokenizer(line, ",");
				if (st.hasMoreTokens()) {
					pos.X = Float.valueOf(st.nextToken());
				}
				if (st.hasMoreTokens()) {
					pos.Y = Float.valueOf(st.nextToken());
				}
				if (st.hasMoreTokens()) {
					pos.Z = Float.valueOf(st.nextToken());
				}

				contour.add(pos);
				line = br.readLine();
			}

			br.close();

			return contour;
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
		return null;
	}

	protected VOI loadSamplePlanes( String imageDir ) {


		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			return null;
		}
		voiDir = imageDir + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			return null;
		}

		File file = new File(voiDir + "SamplePlanes.csv");
		if (file.exists()) {
			return null;
		}

		try {
			final FileReader fr = new FileReader(file);
			final BufferedReader br = new BufferedReader(fr);

			String line = br.readLine(); // first line is header
			line = br.readLine();

			final short sID = voiID++;
			VOI planes = new VOI(sID, "samplingPlanes");
			while ( line != null )
			{
				VOIContour contour = new VOIContour(true);
				StringTokenizer st = new StringTokenizer(line, ",");
				for ( int i = 0; i < 4; i++ )
				{
					Vector3f pos = new Vector3f();
					if (st.hasMoreTokens()) {
						pos.X = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						pos.Y = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						pos.Z = Float.valueOf(st.nextToken());
					}
					contour.add(pos);
				}

				planes.getCurves().add(contour);
				line = br.readLine();
			}

			br.close();

			return planes;
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Generates the 3D 1-voxel thick ellipsoids used in the intial worm model.
	 * 
	 * @param right
	 * @param up
	 * @param center
	 * @param diameterA
	 * @param diameterB
	 * @param ellipse
	 * @return
	 */
	protected Ellipsoid3f makeEllipse(final Vector3f right, final Vector3f up, final Vector3f center, final float diameterA, final float diameterB,
			final VOIContour ellipse) {
		final double[] adCos = new double[32];
		final double[] adSin = new double[32];
		for (int i = 0; i < numEllipsePts; i++) {
			adCos[i] = Math.cos(Math.PI * 2.0 * i / numEllipsePts);
			adSin[i] = Math.sin(Math.PI * 2.0 * i / numEllipsePts);
		}
		for (int i = 0; i < numEllipsePts; i++) {
			final Vector3f pos1 = Vector3f.scale((float) (diameterA * adCos[i]), right);
			final Vector3f pos2 = Vector3f.scale((float) (diameterB * adSin[i]), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
		final float[] extents = new float[] {diameterA, diameterB, 1};
		final Vector3f[] axes = new Vector3f[] {right, up, Vector3f.cross(right, up)};
		return new Ellipsoid3f(center, axes, extents);
	}


	/**
	 * Generates the simple 2D VOI contour ellipse for the worm model.
	 * 
	 * @param right
	 * @param up
	 * @param center
	 * @param diameterA
	 * @param scale
	 * @param ellipse
	 */
	protected void makeEllipse2D(final Vector3f right, final Vector3f up, final Vector3f center, final float diameterA,
			final VOIContour ellipse) {
		final float diameterB = diameterA / 2f;// + (1-scale) * diameterA/4f;
		for (int i = 0; i < numEllipsePts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numEllipsePts);
			final double s = Math.sin(Math.PI * 2.0 * i / numEllipsePts);
			final Vector3f pos1 = Vector3f.scale((float) (diameterA * c), right);
			final Vector3f pos2 = Vector3f.scale((float) (diameterB * s), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
	}


	protected void makeEllipse2DA(final Vector3f right, final Vector3f up, final Vector3f center, final float radius,
			final VOIContour ellipse) {
		for (int i = 0; i < numEllipsePts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numEllipsePts);
			final double s = Math.sin(Math.PI * 2.0 * i / numEllipsePts);
			final Vector3f pos1 = Vector3f.scale((float) (radius * c), right);
			final Vector3f pos2 = Vector3f.scale((float) (radius * s), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
			//			System.err.println(pos);
		}
	}

	protected void makeEllipse2DSkin(final Vector3f right, final Vector3f up, final Vector3f center, final float horizontalRadius, final float verticalRadius,
			final VOIContour ellipse, int numPts) {
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (horizontalRadius * c), right);
			final Vector3f pos2 = Vector3f.scale((float) (verticalRadius * s), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
			//			System.err.println(pos);
		}
	}





	/**
	 * Given a point in the twisted volume, calculates and returns the corresponding point in the straightened image.
	 * 
	 * @param model
	 * @param originToStraight
	 * @param pt
	 * @param text
	 * @return
	 */
	protected Vector3f originToStraight(final ModelImage model, final ModelImage originToStraight, final Vector3f pt, final String text) {
		final int x = Math.round(pt.X);
		final int y = Math.round(pt.Y);
		final int z = Math.round(pt.Z);

		final float outputA = originToStraight.getFloatC(x, y, z, 0);
		final float outputX = originToStraight.getFloatC(x, y, z, 1);
		final float outputY = originToStraight.getFloatC(x, y, z, 2);
		final float outputZ = originToStraight.getFloatC(x, y, z, 3);

		if (outputA == 0) {
			final float m = model.getFloat(x, y, z);
			if (m != 0) {
				final int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
				final int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
				final int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;

				int count = 0;
				final Vector3f pts = new Vector3f();
				for (int z1 = Math.max(0, z - 2); z1 < Math.min(dimZ, z + 2); z1++) {
					for (int y1 = Math.max(0, y - 2); y1 < Math.min(dimY, y + 2); y1++) {
						for (int x1 = Math.max(0, x - 2); x1 < Math.min(dimX, x + 2); x1++) {
							final float a1 = originToStraight.getFloatC(x1, y1, z1, 0);
							final int m1 = model.getInt(x1, y1, z1);
							if ( (a1 != 0) && (m1 == m)) {
								final float x2 = originToStraight.getFloatC(x1, y1, z1, 1);
								final float y2 = originToStraight.getFloatC(x1, y1, z1, 2);
								final float z2 = originToStraight.getFloatC(x1, y1, z1, 3);
								pts.add(x2, y2, z2);
								count++;
							}
						}
					}
				}
				if (count != 0) {
					// System.err.println( imageA.getImageName() + " originToStraight " + text + " " + pt + " OK ");
					pts.scale(1f / count);
					return pts;
				}
			} else {
				//				System.err.println(imageA.getImageName() + " originToStraight " + text + " " + pt);
			}
		}

		return new Vector3f(outputX, outputY, outputZ);
	}


	protected ModelImage readImage(String name, int sliceID)
	{
		String voiDir = outputDirectory + File.separator + JDialogBase.makeImageName(name, "") + File.separator;
		String imageName = name + "_" + sliceID;
		FileIO fileIO = new FileIO();
		ModelImage image = fileIO.readImage(imageName + ".tif", voiDir, false, null);
		fileIO.dispose();
		fileIO = null;
		return image;
	}




	/**
	 * Saves the annotation statistics to a file.
	 * 
	 * @param image
	 * @param model
	 * @param originToStraight
	 * @param outputDim
	 * @param postFix
	 * @return
	 */
	protected VOI saveAnnotationStatistics(final String imageDir, final ModelImage model, final ModelImage originToStraight, final int[] outputDim,
			final String postFix) {
		if (annotationVOIs == null) {
			return null;
		}
		if (annotationVOIs.getCurves().size() == 0) {
			return null;
		}

		//		String imageName = image.getImageName();
		//		if (imageName.contains("_clone")) {
		//			imageName = imageName.replaceAll("_clone", "");
		//		}
		//		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator;
		//		File voiFileDir = new File(voiDir);
		//		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		//		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		//		} else { // voiFileDir does not exist
		//			voiFileDir.mkdir();
		//		}
		//		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "statistics" + File.separator;


		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveAnnotationStatistics " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = imageDir + "statistics" + File.separator;


		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
			// does
			// not
			// exist
			//			System.err.println( "saveAnnotationStatistics " + voiDir);
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + "AnnotationInfo" + postFix + ".csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + "AnnotationInfo" + postFix + ".csv");
		}

		VOI transformedAnnotations = null;
		try {
			if (originToStraight != null) {
				transformedAnnotations = new VOI(annotationVOIs);
			}

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "\n");
			for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
				final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
				Vector3f position = text.elementAt(0);
				if ( (model != null) && (originToStraight != null)) {
					position = originToStraight(model, originToStraight, position, text.getText());

					transformedAnnotations.getCurves().elementAt(i).elementAt(0).copy(position);
					transformedAnnotations.getCurves().elementAt(i).elementAt(1).set(position.X + 5, position.Y, position.Z);
				}
				bw.write(text.getText() + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
						+ (position.Z - transformedOrigin.Z) + "," +

                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
			e.printStackTrace();
		}

		return transformedAnnotations;
	}


	protected void saveDiameters( Vector<Float> diameters, String imageDir ) {

		if ( diameters == null )
		{
			return;
		}
		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveDiameters " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = imageDir + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			//			System.err.println( "saveDiameters " + voiDir);
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + "Diameters.csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + "Diameters.csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);

			bw.write("diameter" + "\n");
			for ( int i = 0; i < diameters.size(); i++ )
			{
				bw.write(diameters.elementAt(i) + "\n");				
			}

			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
	}


	/**
	 * Saves the image to the output_images directory.
	 * 
	 * @param imageName
	 * @param image
	 * @param saveAsTif
	 */
	protected void saveImage(final String imageName, final ModelImage image, final boolean saveAsTif) {
		saveImage(imageName, image, saveAsTif, null);
	}


	protected void saveImage(String outputDirectory, final ModelImage image, int sliceID)
	{
		String imageName = image.getImageName();
		String voiDir = outputDirectory + File.separator + JDialogBase.makeImageName(imageName, "") + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveImage " + voiDir);
			voiFileDir.mkdir();
		}

		imageName = imageName + "_" + sliceID;
		final File file = new File(voiDir + imageName);
		if (file.exists()) {
			file.delete();
		}
		ModelImage.saveImage(image, imageName + ".tif", voiDir, false);
		//		System.err.println( "saveImage " + voiDir + " " + imageName + ".tif" );
	}

	/**
	 * Saves the lattice statistics to a file.
	 * 
	 * @param image
	 * @param length
	 * @param left
	 * @param right
	 * @param leftPairs
	 * @param rightPairs
	 * @param postFix
	 */
	protected void saveLatticeStatistics( String imageDir, final float length, final VOI left, final VOI right, final float[] leftPairs,
			final float[] rightPairs, final String postFix) {

		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = imageDir + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + "LatticeInfo" + postFix + ".csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + "LatticeInfo" + postFix + ".csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("Total Length:," + VOILatticeManagerInterface.VoxelSize * length + "\n");
			bw.newLine();
			bw.write("pair" + "," + "diameter" + "," + "left distance" + "," + "right distance" + "\n");
			for (int i = 0; i < left.getCurves().size(); i++) {
				bw.write(i + "," + VOILatticeManagerInterface.VoxelSize * left.getCurves().elementAt(i).elementAt(0).distance(right.getCurves().elementAt(i).elementAt(0)) + ","
						+ VOILatticeManagerInterface.VoxelSize * leftPairs[i] + "," + VOILatticeManagerInterface.VoxelSize * rightPairs[i] + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
			e.printStackTrace();
		}
	}


	protected void savePositions( VOIContour contour, String imageDir, String name ) {

		if ( contour == null )
		{
			return;
		}
		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "savePositions " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = imageDir + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			//			System.err.println( "savePositions " + voiDir);
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + name + "Positions.csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + name + "Positions.csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);

			bw.write("X" + "," + "Y" + "," + "Z" + "\n");
			for ( int i = 0; i < contour.size(); i++ )
			{
				Vector3f pos = contour.elementAt(i);
				bw.write(pos.X + "," + pos.Y + "," + pos.Z + "\n");				
			}

			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
	}


	protected void saveSamplePlanes( VOI planes, String imageDir ) {

		if ( planes == null )
		{
			return;
		}
		String voiDir = imageDir;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveSamplePlanes " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = imageDir + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			//			System.err.println( "saveSamplePlanes " + voiDir);
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + "SamplePlanes.csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + "SamplePlanes.csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);

			bw.write("X1" + "," + "Y1" + "," + "Z1" + "X2" + "," + "Y2" + "," + "Z2" + "X3" + "," + "Y3" + "," + "Z3" + "X4" + "," + "Y4" + "," + "Z4" + "\n");
			for ( int i = 0; i < planes.getCurves().size(); i++ )
			{
				VOIContour kBox = (VOIContour) planes.getCurves().elementAt(i);
				for (int j = 0; j < 4; j++) {
					Vector3f pos = kBox.elementAt(j);
					if ( j < (4-1) )
					{
						bw.write(pos.X + "," + pos.Y + "," + pos.Z + ",");
					}
					else
					{
						bw.write(pos.X + "," + pos.Y + "," + pos.Z + "\n");
					}
				}
			}

			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
	}




	/**
	 * Generates the Natural Spline for the lattice center-line curve. Sets the time values for each point on the curve.
	 * 
	 * @param curve
	 * @param time
	 * @return
	 */
	protected NaturalSpline3 smoothCurve(final VOIContour curve, final float[] time) {
		float totalDistance = 0;
		for (int i = 0; i < curve.size() - 1; i++) {
			totalDistance += curve.elementAt(i).distance(curve.elementAt(i + 1));
		}

		final Vector3f[] akPoints = new Vector3f[curve.size()];
		float distance = 0;
		for (int i = 0; i < curve.size(); i++) {
			if (i > 0) {
				distance += curve.elementAt(i).distance(curve.elementAt(i - 1));
				time[i] = distance / totalDistance;
				akPoints[i] = new Vector3f(curve.elementAt(i));
			} else {
				time[i] = 0;
				akPoints[i] = new Vector3f(curve.elementAt(i));
			}
		}

		return new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, curve.size() - 1, time, akPoints);
	}

	public static NaturalSpline3 smoothCurve(final Vector<VOIWormAnnotation> controlPts) {
		float totalDistance = 0;
		for (int i = 0; i < controlPts.size() - 1; i++) {
			totalDistance += controlPts.elementAt(i).elementAt(0).distance(controlPts.elementAt(i + 1).elementAt(0));
		}

		final Vector3f[] akPoints = new Vector3f[controlPts.size()];
		float[] time = new float[controlPts.size()];
		float distance = 0;
		for (int i = 0; i < controlPts.size(); i++) {
			if (i > 0) {
				distance += controlPts.elementAt(i).elementAt(0).distance(controlPts.elementAt(i - 1).elementAt(0));
				time[i] = distance / totalDistance;
				akPoints[i] = new Vector3f(controlPts.elementAt(i).elementAt(0));
			} else {
				time[i] = 0;
				akPoints[i] = new Vector3f(controlPts.elementAt(i).elementAt(0));
			}
		}

		return new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, controlPts.size() - 1, time, akPoints);
	}


	protected Vector3f[] straightenFrame(final ModelImage image, int slice,
			final int[] extents, final Vector3f[] verts, Vector3f centerPos, Vector3f leftPos, Vector3f rightPos ) 
	{
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		float minDistanceCenterPos = Float.MAX_VALUE;
		float minDistanceLeftPos = Float.MAX_VALUE;
		float minDistanceRightPos = Float.MAX_VALUE;
		Vector3f[] closest = new Vector3f[]{new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE),
				new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE),
				new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE)	};

		Vector3f pt = new Vector3f();
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) ) {

					// do nothing
				} else {
					pt.set(x, y, z);
					float dist = pt.distance(centerPos);
					if ( dist < minDistanceCenterPos )
					{
						minDistanceCenterPos = dist;
						closest[0].set(i, j, slice);
					}
					dist = pt.distance(leftPos);
					if ( dist < minDistanceLeftPos )
					{
						minDistanceLeftPos = dist;
						closest[1].set(i, j, slice);
					}
					dist = pt.distance(rightPos);
					if ( dist < minDistanceRightPos )
					{
						minDistanceRightPos = dist;
						closest[2].set(i, j, slice);
					}
				}
				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
		return closest;
	}


	protected Vector3f writeDiagonal(final ModelImage image, int slice,
			final int[] extents, final Vector3f[] verts, Vector3f target, float[] minDistance ) 
	{
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		minDistance[0] = Float.MAX_VALUE;
		Vector3f closest = new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE);
		Vector3f pt = new Vector3f();
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) ) {

					// do nothing
				} else {
					pt.set(x, y, z);
					float dist = pt.distance(target);
					if ( dist < minDistance[0] )
					{
						minDistance[0] = dist;
						closest.set(i, j, slice);
					}
				}
				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
		//		System.err.println( minDistance );
		return closest;
	}

	protected void writeDiagonal(final ModelImage image, final ModelImage result, final int tSlice, final int slice,
			final int[] extents, final Vector3f[] verts) {
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				// Initialize to 0:
				if (buffFactor == 4) {						
					result.setC(i, j, slice, 0, 0 );
					result.setC(i, j, slice, 1, 0 );
					result.setC(i, j, slice, 2, 0 );
					result.setC(i, j, slice, 3, 0 );
				}
				else {
					result.set(i, j, slice, 0 );
				}		

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {
					/* if color: */
					if (buffFactor == 4) {						
						result.setC(i, j, slice, 0, image.getFloatC(iIndex, jIndex, kIndex, 0) );
						result.setC(i, j, slice, 1, image.getFloatC(iIndex, jIndex, kIndex, 1) );
						result.setC(i, j, slice, 2, image.getFloatC(iIndex, jIndex, kIndex, 2) );
						result.setC(i, j, slice, 3, image.getFloatC(iIndex, jIndex, kIndex, 3) );
					}
					/* not color: */
					else {
						result.set(i, j, slice, image.getFloat(iIndex, jIndex, kIndex));
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}

		//		if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1)) {
		//			System.err.println("writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX);
		//			System.err.println("writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY);
		//		}
	}

	/**
	 * Called from the straightening function. Exports the finalized worm model slice-by-slice into the straightened
	 * image.
	 * 
	 * @param image
	 * @param model
	 * @param originToStraight
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param values
	 * @param dataOrigin
	 * @param sampleDistance
	 * @param sampleDistanceP
	 */
	protected void writeDiagonal(final ModelImage image, final ModelImage model, final ModelImage originToStraight, final int tSlice, final int slice,
			final int[] extents, final Vector3f[] verts, final float[] values, final float[] dataOrigin, final float[] sampleDistance,
			final float[] sampleDistanceP) {
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				if (sampleDistance != null) {
					sampleDistance[ (j * iBound) + i] = 0;
				}

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {
					int currentValue = (slice + 1);
					if (model != null) {
						// currentValue = model.getFloat((int)x, (int)y, (int)z);
						// currentValue = model.getFloatTriLinearBounds(x, y, z);
						currentValue = model.getInt(iIndex, jIndex, kIndex);
					}
					if (currentValue == 0) {
						if (buffFactor == 4) {
							values[ ( ( (j * iBound) + i) * 4) + 0] = Math.max((float) image.getMinA(), values[ ( ( (j * iBound) + i) * 4) + 0]);
							values[ ( ( (j * iBound) + i) * 4) + 1] = Math.max((float) image.getMinR(), values[ ( ( (j * iBound) + i) * 4) + 1]);
							values[ ( ( (j * iBound) + i) * 4) + 2] = Math.max((float) image.getMinG(), values[ ( ( (j * iBound) + i) * 4) + 2]);
							values[ ( ( (j * iBound) + i) * 4) + 3] = Math.max((float) image.getMinB(), values[ ( ( (j * iBound) + i) * 4) + 3]);
						}
						/* not color: */
						else {
							values[ (j * iBound) + i] = Math.max((float) image.getMin(), values[ (j * iBound) + i]);
						}
					} else if (Math.abs(currentValue - (slice + 1)) < SampleLimit) {
						/* if color: */
						if (buffFactor == 4) {
							final float tempV = Math.max(image.getFloatC(iIndex, jIndex, kIndex, 1), image.getFloatC(iIndex, jIndex, kIndex, 2));
							if ( (tempV > values[ ( ( (j * iBound) + i) * 4) + 1]) || (tempV > values[ ( ( (j * iBound) + i) * 4) + 2])) {
								values[ ( ( (j * iBound) + i) * 4) + 0] = image.getFloatC(iIndex, jIndex, kIndex, 0);
								values[ ( ( (j * iBound) + i) * 4) + 1] = image.getFloatC(iIndex, jIndex, kIndex, 1);
								values[ ( ( (j * iBound) + i) * 4) + 2] = image.getFloatC(iIndex, jIndex, kIndex, 2);
								values[ ( ( (j * iBound) + i) * 4) + 3] = image.getFloatC(iIndex, jIndex, kIndex, 3);
								if (dataOrigin != null) {
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] = x;
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] = y;
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] = z;
								}
							}
						}
						/* not color: */
						else {
							final float tempV = image.getFloat(iIndex, jIndex, kIndex);
							// if ( tempV > values[ (j * iBound) + i] )
							// {
							// values[ (j * iBound) + i] = tempV;
							// if ( dataOrigin != null )
							// {
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] = x;
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] = y;
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] = z;
							// }
							// }
							values[ (j * iBound) + i] += tempV;
							if (dataOrigin != null) {
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] += x;
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] += y;
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] += z;
							}
						}

						if ( (sampleDistanceP != null) && (sampleDistance != null)) {
							if ( (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1] != 0) && (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2] != 0)
									&& (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3] != 0)) {
								float distance = (x - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1])
										* (x - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1]) + (y - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2])
										* (y - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2]) + (z - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3])
										* (z - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3]);
								distance = (float) Math.sqrt(distance);
								sampleDistance[ (j * iBound) + i] = distance;
							}
						}

						if (originToStraight != null) {
							originToStraight.setC(iIndex, jIndex, kIndex, 0, 1);
							originToStraight.setC(iIndex, jIndex, kIndex, 1, i);
							originToStraight.setC(iIndex, jIndex, kIndex, 2, j);
							originToStraight.setC(iIndex, jIndex, kIndex, 3, slice);
						}
					}
				}
				if (sampleDistanceP != null) {
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 0] = 1;
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1] = x;
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2] = y;
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3] = z;
				}
				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}

		if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1)) {
			System.err.println("writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX);
			System.err.println("writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY);
		}
	}	protected void writeDiagonal(final ModelImage image, ModelImage distanceStart, ModelImage distanceEnd, final int slice,
			final int[] extents, final Vector3f[] verts, ModelImage contourImage) {
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				// twisted space:
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {		
					if ( (contourImage == null) || ((contourImage != null) && contourImage.getFloat(i,j,slice) > 1) )
					{
						if ( distanceStart != null )
						{
							int isSet = (int)distanceStart.getFloatC(iIndex, jIndex, kIndex, 0);
							// distanceStart is the first sampled point inside the straightened contour.
							if ( isSet == 0 )
							{
								distanceStart.setC(iIndex, jIndex, kIndex, 0, 1);
								distanceStart.setC(iIndex, jIndex, kIndex, 1, i);
								distanceStart.setC(iIndex, jIndex, kIndex, 2, j);
								distanceStart.setC(iIndex, jIndex, kIndex, 3, slice);
							}
							// distanceEnd is the last sampled point inside the straightened contour.
							distanceEnd.setC(iIndex, jIndex, kIndex, 0, 1);
							distanceEnd.setC(iIndex, jIndex, kIndex, 1, i);
							distanceEnd.setC(iIndex, jIndex, kIndex, 2, j);
							distanceEnd.setC(iIndex, jIndex, kIndex, 3, slice);
						}
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}

		//		if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1)) {
		//			System.err.println("writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX);
		//			System.err.println("writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY);
		//		}
	}

	protected void writeDiagonal(final ModelImage image, ModelImage distanceStart, ModelImage distanceEnd, final int slice,
			final int[] extents, final Vector3f[] verts, VOIContour contour) {
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				// twisted space:
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {		
					if ( contour.contains( i, j ) )
					{
						if ( distanceStart != null )
						{
							int isSet = (int)distanceStart.getFloatC(iIndex, jIndex, kIndex, 0);
							// distanceStart is the first sampled point inside the straightened contour.
							if ( isSet == 0 )
							{
								distanceStart.setC(iIndex, jIndex, kIndex, 0, 1);
								distanceStart.setC(iIndex, jIndex, kIndex, 1, i);
								distanceStart.setC(iIndex, jIndex, kIndex, 2, j);
								distanceStart.setC(iIndex, jIndex, kIndex, 3, slice);
							}
							// distanceEnd is the last sampled point inside the straightened contour.
							distanceEnd.setC(iIndex, jIndex, kIndex, 0, 1);
							distanceEnd.setC(iIndex, jIndex, kIndex, 1, i);
							distanceEnd.setC(iIndex, jIndex, kIndex, 2, j);
							distanceEnd.setC(iIndex, jIndex, kIndex, 3, slice);
						}
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}

		//		if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1)) {
		//			System.err.println("writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX);
		//			System.err.println("writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY);
		//		}
	}


	protected void writeDiagonalTest(final ModelImage image, final ModelImage result, final int tSlice, final int slice,
			final int[] extents, final Vector3f[] verts, float radiusSq ) {
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		float centerI = iBound / 2f;
		float centerJ = jBound / 2f;

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		float fMin = Float.MAX_VALUE;
		float fMax = -Float.MAX_VALUE;
		float fMinG = Float.MAX_VALUE;
		float fMaxG = -Float.MAX_VALUE;
		float fMinB = Float.MAX_VALUE;
		float fMaxB = -Float.MAX_VALUE;
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				// Initialize to 0:
				if (buffFactor == 4) {						
					result.setC(i, j, slice, 0, 0 );
					result.setC(i, j, slice, 1, 0 );
					result.setC(i, j, slice, 2, 0 );
					result.setC(i, j, slice, 3, 0 );
				}
				else {
					result.set(i, j, slice, 0 );
				}		

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} 
				else {
					float dist = (i - centerI) * (i - centerI) + (j - centerJ) * (j - centerJ); 
					if ( dist <= radiusSq )
					{
						/* if color: */
						if (buffFactor == 4) {						
							float value = image.getFloatC(iIndex, jIndex, kIndex, 1);
							float valueG = image.getFloatC(iIndex, jIndex, kIndex, 2);
							float valueB = image.getFloatC(iIndex, jIndex, kIndex, 3);
							result.setC(i, j, slice, 0, image.getFloatC(iIndex, jIndex, kIndex, 0) );
							result.setC(i, j, slice, 1, value );
							result.setC(i, j, slice, 2, valueG );
							result.setC(i, j, slice, 3, valueB );
							if ( value < fMin ) fMin = value;
							if ( value > fMax ) fMax = value;
							if ( valueG < fMinG ) fMinG = valueG;
							if ( valueG > fMaxG ) fMaxG = valueG;
							if ( valueB < fMinB ) fMinB = valueB;
							if ( valueB > fMaxB ) fMaxB = valueB;
						}
						/* not color: */
						else {
							float value = image.getFloat(iIndex, jIndex, kIndex);
							result.set(i, j, slice, value);
							if ( value < fMin ) fMin = value;
							if ( value > fMax ) fMax = value;
						}					
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}

		//		if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1)) {
		//			System.err.println("writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX);
		//			System.err.println("writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY);
		//		}

		result.setMax( Math.max(fMax, result.getMax()) );
		result.setMin( Math.min(fMin, result.getMin()) );
		if (buffFactor == 4) {	
			result.setMaxR(Math.max(fMax, result.getMaxR()));
			result.setMinR(Math.min(fMin, result.getMinR()));
			result.setMaxG(Math.max(fMaxG, result.getMaxG()));
			result.setMinG(Math.min(fMinG, result.getMinG()));
			result.setMaxB(Math.max(fMaxB, result.getMaxB()));
			result.setMinB(Math.min(fMinB, result.getMinB()));
		}
	}




	/**
	 * Adds a point to the lattice.
	 * 
	 * @param startPt
	 * @param endPt
	 * @param maxPt
	 */
	private boolean addInsertionPoint(final Vector3f startPt, final Vector3f endPt, final Vector3f maxPt, boolean isSeam) {
		final Segment3f mouseVector = new Segment3f(startPt, endPt);
		float minDistL = Float.MAX_VALUE;
		int minIndexL = -1;
		int minSeamL = -1;
		Vector3f newLeft = null;
		int seamCount = 0;
		int otherCount = 0;
		for (int i = 0; i < left.getCurves().size() - 1; i++) {
			if ( ((VOIWormAnnotation)left.getCurves().elementAt(i)).isSeamCell() ) {
				seamCount++;
			}
			else {
				otherCount++;
			}
			
			VOIWormAnnotation leftPt = (VOIWormAnnotation) left.getCurves().elementAt(i);
			VOIWormAnnotation leftPtP1 = (VOIWormAnnotation) left.getCurves().elementAt(i+1);
			final Segment3f leftS = new Segment3f( leftPt.elementAt(0), leftPtP1.elementAt(0));
			final DistanceSegment3Segment3 dist = new DistanceSegment3Segment3(mouseVector, leftS);
			final float distance = dist.Get();
			if (distance < minDistL) {
				minDistL = distance;
				if (minDistL <= 12) {
					// System.err.println( dist.GetSegment0Parameter() + " " + dist.GetSegment1Parameter() );
					minIndexL = i;
					minSeamL = seamCount;
					//					newLeft = Vector3f.add(leftS.Center, Vector3f.scale(dist.GetSegment1Parameter(), leftS.Direction));
					newLeft = new Vector3f(maxPt);
				}
			}
		}
		float minDistR = Float.MAX_VALUE;
		int minIndexR = -1;
		int minSeamR = -1;
		Vector3f newRight = null;
		for (int i = 0; i < right.getCurves().size() - 1; i++) {
			VOIWormAnnotation rightPt = (VOIWormAnnotation) right.getCurves().elementAt(i);
			VOIWormAnnotation rightPtP1 = (VOIWormAnnotation) right.getCurves().elementAt(i+1);
			final Segment3f rightS = new Segment3f(rightPt.elementAt(0), rightPtP1.elementAt(0));
			final DistanceSegment3Segment3 dist = new DistanceSegment3Segment3(mouseVector, rightS);
			final float distance = dist.Get();
			if (distance < minDistR) {
				minDistR = distance;
				if (minDistR <= 12) {
					// System.err.println( dist.GetSegment0Parameter() + " " + dist.GetSegment1Parameter() );
					minIndexR = i;
					minSeamR = seamCount;
					//					newRight = Vector3f.add(rightS.Center, Vector3f.scale(dist.GetSegment1Parameter(), rightS.Direction));
					newRight = new Vector3f(maxPt);
				}
			}
		}
		if ( (minIndexL != -1) && (minIndexR != -1)) {
			if (minDistL < minDistR) {
				seamCount = minSeamL + 1;
				// System.err.println( "Add to left " + (minIndexL+1) );
				VOIWormAnnotation newLeftAnnotation = new VOIWormAnnotation(newLeft);
				newLeftAnnotation.setSeamCell(isSeam);
				if ( isSeam ) {
					String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
					newLeftAnnotation.setText(name + "L");
				}
				else {
					String name = "a" + otherCount;
					newLeftAnnotation.setText(name + "L");
				}
				left.getCurves().add(minIndexL + 1, newLeftAnnotation);
				pickedPoint = left.getCurves().elementAt(minIndexL + 1).elementAt(0);
				
				newRight = Vector3f.add(right.getCurves().elementAt(minIndexL).elementAt(0), right.getCurves().elementAt(minIndexL + 1).elementAt(0));
				newRight.scale(0.5f);
				VOIWormAnnotation newRightAnnotation = new VOIWormAnnotation(newRight);
				newRightAnnotation.setSeamCell(isSeam);
				if ( isSeam ) {
					String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
					newRightAnnotation.setText(name + "RL");
				}
				else {
					String name = "a" + otherCount;
					newRightAnnotation.setText(name + "R");
				}
				right.getCurves().add(minIndexL + 1, newRightAnnotation);
				updateSeamCount();
				updateLattice(true);
				showLatticeLabels(displayLatticeLabels);
			} else {
				seamCount = minSeamR + 1;
				// System.err.println( "Add to right " + (minIndexR+1) );
				VOIWormAnnotation newRightAnnotation = new VOIWormAnnotation(newRight);
				newRightAnnotation.setSeamCell(isSeam);
				if ( isSeam ) {
					String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
					newRightAnnotation.setText(name + "RL");
				}
				else {
					String name = "a" + otherCount;
					newRightAnnotation.setText(name + "R");
				}
				right.getCurves().add(minIndexR + 1, newRightAnnotation);
				pickedPoint = right.getCurves().elementAt(minIndexR + 1).elementAt(0);
				
				
				newLeft = Vector3f.add(left.getCurves().elementAt(minIndexR).elementAt(0), left.getCurves().elementAt(minIndexR + 1).elementAt(0));
				newLeft.scale(0.5f);
				VOIWormAnnotation newLeftAnnotation = new VOIWormAnnotation(newLeft);
				newLeftAnnotation.setSeamCell(isSeam);
				if ( isSeam ) {
					String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
					newLeftAnnotation.setText(name + "L");
				}
				else {
					String name = "a" + otherCount;
					newLeftAnnotation.setText(name + "L");
				}
				left.getCurves().add(minIndexR + 1, newLeftAnnotation);
				updateSeamCount();
				updateLattice(true);
				showLatticeLabels(displayLatticeLabels);
			}
		} else if ((minIndexL != -1) && ((minIndexL + 1) < right.getCurves().size())) {
			seamCount = minSeamL + 1;
			// System.err.println( "Add to left " + (minIndexL+1) );
			VOIWormAnnotation newLeftAnnotation = new VOIWormAnnotation(newLeft);
			newLeftAnnotation.setSeamCell(isSeam);
			if ( isSeam ) {
				String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
				newLeftAnnotation.setText(name + "L");
			}
			else {
				String name = "a" + otherCount;
				newLeftAnnotation.setText(name + "L");
			}
			left.getCurves().add(minIndexL + 1, newLeftAnnotation);
			pickedPoint = left.getCurves().elementAt(minIndexL + 1).elementAt(0);
			newRight = Vector3f.add(right.getCurves().elementAt(minIndexL).elementAt(0), right.getCurves().elementAt(minIndexL + 1).elementAt(0));
			newRight.scale(0.5f);
			VOIWormAnnotation newRightAnnotation = new VOIWormAnnotation(newRight);
			newRightAnnotation.setSeamCell(isSeam);
			if ( isSeam ) {
				String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
				newRightAnnotation.setText(name + "RL");
			}
			else {
				String name = "a" + otherCount;
				newRightAnnotation.setText(name + "R");
			}
			right.getCurves().add(minIndexL + 1, newRightAnnotation);
			updateSeamCount();
			updateLattice(true);
			showLatticeLabels(displayLatticeLabels);
		} else if (minIndexR != -1 && ((minIndexR + 1) < left.getCurves().size())) {
			seamCount = minSeamR + 1;
			// System.err.println( "Add to right " + (minIndexR+1) );
			VOIWormAnnotation newRightAnnotation = new VOIWormAnnotation(newRight);
			newRightAnnotation.setSeamCell(isSeam);
			if ( isSeam ) {
				String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
				newRightAnnotation.setText(name + "RL");
			}
			else {
				String name = "a" + otherCount;
				newRightAnnotation.setText(name + "R");
			}
			right.getCurves().add(minIndexR + 1, newRightAnnotation);
			pickedPoint = right.getCurves().elementAt(minIndexR + 1).elementAt(0);
			
			
			newLeft = Vector3f.add(left.getCurves().elementAt(minIndexR).elementAt(0), left.getCurves().elementAt(minIndexR + 1).elementAt(0));
			newLeft.scale(0.5f);
			VOIWormAnnotation newLeftAnnotation = new VOIWormAnnotation(newLeft);
			newLeftAnnotation.setSeamCell(isSeam);
			if ( isSeam ) {
				String name = seamCount < 3 ? ("H" + seamCount) : (seamCount < 9) ? ("V" + (seamCount - 2)) : "T";
				newLeftAnnotation.setText(name + "L");
			}
			else {
				String name = "a" + otherCount;
				newLeftAnnotation.setText(name + "L");
			}
			left.getCurves().add(minIndexR + 1, newLeftAnnotation);

			// rename cells if seam count > 10 to include Q cells:
			updateSeamCount();
			updateLattice(true);
			showLatticeLabels(displayLatticeLabels);
		}
		else
		{
			pickedPoint = null;
			return false;
		}
		return true;
	}
	/**
	 * Resets the natural spline curves when the lattice changes.
	 */
	private void clearCurves( boolean clearGrid )
	{
		if (center != null) {
			center.dispose();
			center = null;
		}
		afTimeC = null;
		centerSpline = null;

		centerPositions = null;
		leftPositions = null;
		rightPositions = null;

		if (wormDiameters != null) {
			wormDiameters.removeAllElements();
			wormDiameters = null;
		}
		if (rightVectors != null) {
			rightVectors.removeAllElements();
			rightVectors = null;
		}
		if (upVectors != null) {
			upVectors.removeAllElements();
			upVectors = null;
		}

		allTimes = null;

		if (centerLine != null) {
			imageA.unregisterVOI(centerLine);
			centerLine.dispose();
			centerLine = null;
		}
		if (rightLine != null) {
			imageA.unregisterVOI(rightLine);
			rightLine.dispose();
			rightLine = null;
		}
		if (leftLine != null) {
			imageA.unregisterVOI(leftLine);
			leftLine.dispose();
			leftLine = null;
		}

		if (displayContours != null) {
			imageA.unregisterVOI(displayContours);
			displayContours.dispose();
			displayContours = null;
		}
		if ( clearGrid )
		{
			if (latticeGrid != null) {
				for (int i = latticeGrid.size() - 1; i >= 0; i--) {
					final VOI marker = latticeGrid.remove(i);
					imageA.unregisterVOI(marker);
				}
			} 
		}
	}
	/**
	 * Counts the annotations and colors them
	 * based on the number of annotations. This is only used
	 * when the user is labeling seam-cells as an aid in determining if 20
	 * pairs of seam cells have been found.
	 */
	private void colorAnnotations()
	{
		// count markers (not nose or origin)
		// even = all blue
		// odd = yellow
		// some colors are now used to designate the first and last pairs of seam cells - first = green; last = red
		if ( annotationVOIs == null )
		{
			return;
		}

		int count = 0;
		for (int i = 0; i < annotationVOIs.getCurves().size(); i++)
		{
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( !(text.getText().contains("nose") || text.getText().contains("Nose")) && !text.getText().equalsIgnoreCase("origin"))
			{
				count++;
			}
		}
		Color c = Color.yellow;
		if ( (count % 2) == 0 )
		{
			c = Color.blue;
		}
		if ( !colorAnnotations )
		{
			c = Color.white;
		}


		for (int i = 0; i < annotationVOIs.getCurves().size(); i++)
		{
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			if ( !(text.getText().contains("nose") || text.getText().contains("Nose")) && !text.getText().equalsIgnoreCase("origin"))
			{
				//				System.err.println( text.getText() + "   " + text.getColor() );
				if ( !match(text.getColor(), Color.red) && !match(text.getColor(), Color.green)  )
				{
					text.setColor(c);
					text.updateText();
				}
			}
		}
		updateAnnotationListeners();
	}


	private VOIWormAnnotation findNearestAnnotation( final Vector3f startPt, final Vector3f endPt, final Vector3f pt ) {
		int pickedAnnotation = -1;
		float minDist = Float.MAX_VALUE;
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
		{
			final Vector3f annotationPt = annotationVOIs.getCurves().elementAt(i).elementAt(0);
			final float distance = pt.distance(annotationPt);
			if ( distance < minDist )
			{
				minDist = distance;
				if ( minDist <= 12 )
				{
					pickedAnnotation = i;
				}
			}
		}
//		System.err.println("findNearestAnnotation " + minDist + "  " + pickedAnnotation );
		if ( (pickedAnnotation == -1) && (startPt != null) && (endPt != null) )
		{
			minDist = Float.MAX_VALUE;
			// look at the vector under the mouse and see which lattice point is closest...
			final Segment3f mouseVector = new Segment3f(startPt, endPt);
			for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
			{
				DistanceVector3Segment3 dist = new DistanceVector3Segment3(annotationVOIs.getCurves().elementAt(i).elementAt(0), mouseVector);
				float distance = dist.Get();
				//					System.err.println( i + " " + distance );
				if ( distance < minDist )
				{
					minDist = distance;
					if ( minDist <= 12 )
					{
						pickedAnnotation = i;
					}
				}
			}
		}
		if ( pickedAnnotation != -1 ) {
			return (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(pickedAnnotation);
		}
		return null;
	}

	private void generateEllipses()
	{
		boxBounds = new Vector<Box3f>();
		ellipseBounds = new Vector<Ellipsoid3f>();
		final short sID = (short)10;
		samplingPlanes = new VOI(sID, "samplingPlanes");
		for (int i = 0; i < centerPositions.size(); i++) {
			final Vector3f rkEye = centerPositions.elementAt(i);
			final Vector3f rkRVector = rightVectors.elementAt(i);
			final Vector3f rkUVector = upVectors.elementAt(i);

			final Vector3f[] output = new Vector3f[4];
			final Vector3f rightV = Vector3f.scale(extent, rkRVector);
			final Vector3f upV = Vector3f.scale(extent, rkUVector);
			output[0] = Vector3f.add(Vector3f.neg(rightV), Vector3f.neg(upV));
			output[1] = Vector3f.add(rightV, Vector3f.neg(upV));
			output[2] = Vector3f.add(rightV, upV);
			output[3] = Vector3f.add(Vector3f.neg(rightV), upV);
			for (int j = 0; j < 4; j++) {
				output[j].add(rkEye);
			}
			final VOIContour kBox = new VOIContour(true);
			for (int j = 0; j < 4; j++) {
				kBox.addElement(output[j].X, output[j].Y, output[j].Z);
			}
			kBox.update(new ColorRGBA(0, 0, 1, 1));
			{
				samplingPlanes.getCurves().add(kBox);
				//				samplingPlanes.importCurve(kBox);
			}

			//			final float curve = centerSpline.GetCurvature(allTimes[i]);
			//			final float scale = curve;
			final VOIContour ellipse = new VOIContour(true);
			final Ellipsoid3f ellipsoid = makeEllipse(rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), wormDiameters.elementAt(i)/2f, ellipse);
			ellipseBounds.add(ellipsoid);

			final Box3f box = new Box3f(ellipsoid.Center, ellipsoid.Axis, new float[] {extent, extent, 1});
			boxBounds.add(box);
		}

		saveSamplePlanes( samplingPlanes, outputDirectory + File.separator );
		saveDiameters( wormDiameters, outputDirectory + File.separator );
	}


	/**
	 * Generates the VOI that highlights which point (lattice or annotation) is currently selected by the user.
	 * 
	 * @param right
	 * @param up
	 * @param center
	 * @param diameter
	 * @param ellipse
	 */
	private void makeSelectionFrame(final Vector3f right, final Vector3f up, final Vector3f center, final float diameter, final VOIContour ellipse) {
		final int numPts = 12;
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (diameter * c), right);
			final Vector3f pos2 = Vector3f.scale((float) (diameter * s), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
	}


	private Vector3f retwistAnnotation(VOIWormAnnotation annotation)
	{
		// this is the untwisting code:
		final Vector3f[] corners = new Vector3f[4];
		int slice = (int) annotation.firstElement().Z;
		VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(slice);
		for (int j = 0; j < 4; j++) {
			corners[j] = kBox.elementAt(j);
		}

		return inverseDiagonal(imageA, slice, 2*extent, corners, annotation.firstElement() );
	}


	private void saveImage(final String imageName, final ModelImage image, final boolean saveAsTif, String dir) {
		String voiDir = outputDirectory + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveImage " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + File.separator + "output_images" + File.separator;
		if ( dir != null )
		{
			voiDir = voiDir + dir + File.separator;
		}
		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
			// does
			// not
			// exist
			//			System.err.println( "saveImage " + voiDir);
			voiFileDir.mkdir();
		}

		final File file = new File(voiDir + imageName);
		if (file.exists()) {
			file.delete();
		}
		// System.err.println( voiDir );
		// System.err.println( image.getImageName() + ".xml" );
		//		long time = System.currentTimeMillis();
		ModelImage.saveImage(image, image.getImageName() + ".xml", voiDir, false);
		//		System.err.println( "                saveImage XML " + AlgorithmBase.computeElapsedTime(time) );
		if (saveAsTif) {
			//			time = System.currentTimeMillis();
			ModelImage.saveImage(image, image.getImageName() + ".tif", voiDir, false);
			//			System.err.println( "               saveImage TIF " + AlgorithmBase.computeElapsedTime(time) );
		}
	}
	



	public static void saveImage(final ModelImage originalImage, final ModelImage image, String subDir, String postScript ) {
		
		String outputDirectory = new String(originalImage.getImageDirectory() + JDialogBase.makeImageName(originalImage.getImageFileName(), "") + File.separator + JDialogBase.makeImageName(originalImage.getImageFileName(), "_results") );
		String parentDir = new String(originalImage.getImageDirectory() + JDialogBase.makeImageName(originalImage.getImageFileName(), "") + File.separator);
		checkParentDir(parentDir);	
		
		String voiDir = outputDirectory + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveImage " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + File.separator + subDir + File.separator;
		
		String imageName = JDialogBase.makeImageName(image.getImageFileName(), "");
		imageName = imageName + postScript;
		
//		int maxVal = -1;
//		voiFileDir = new File(voiDir);
//		if (voiFileDir.exists() && voiFileDir.isDirectory()) { 
//			final String[] list = voiFileDir.list();
//			for (int i = 0; i < list.length; i++) {
//				int fileVal = -1;
////				System.err.println(list[i] + "   " + imageName + "   " + list[i].contains(imageName) );
//				if ( list[i].contains(imageName) ) {
//					String sub = list[i].substring( list[i].indexOf(imageName) + imageName.length(), list[i].length() );
//					for ( int j = 0; j < sub.length(); j++ ) {
//						String test = new String( "" + sub.charAt(j) );
//						int val;
//						try {
//							val = Integer.valueOf(test);
//							if ( fileVal == -1 ) {
//								fileVal = val;
//							}
//							else
//							{
//								fileVal = fileVal * 10 + val;
//							}
//						} catch(NumberFormatException e) {
//						}
//					}
////					System.err.println( "    " + fileVal );
//				}
//				if ( fileVal > maxVal )
//				{
//					maxVal = fileVal;
//				}
//			}
//		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//		} else { // voiFileDir does not exist
//			//			System.err.println( "saveImage " + voiDir);
//			voiFileDir.mkdir();
//		}
//
//		maxVal++;
//		imageName = imageName + "_" + maxVal;
		

		imageName = imageName + ".tif";
		File imageFile = new File(voiDir + imageName);
		if (imageFile.exists() && !imageFile.isDirectory()) { 
			imageFile.delete();
		}

    	System.err.println(voiDir + imageName );
		ModelImage.saveImage(image, imageName, voiDir, false);

	}

	private void saveLatticeStatistics()
	{
		// Determine the distances between points on the lattice
		// distances are along the curve, not straight-line distances:			
		final float[] leftDistances = new float[left.getCurves().size()];
		final float[] rightDistances = new float[right.getCurves().size()];
		for ( int i = 0; i < left.getCurves().size(); i++ )
		{
			leftDistances[i] = 0;
			rightDistances[i] = 0;
		}
		for ( int i = 0; i < left.getCurves().size() - 1; i++ )
		{
			leftDistances[i] = 0;
			rightDistances[i] = 0;
			for ( int j = latticeSlices[i]; j < latticeSlices[i+1]; j++ )
			{
				leftDistances[i] += leftPositions.elementAt(j).distance(leftPositions.elementAt(j+1));
				rightDistances[i] += rightPositions.elementAt(j).distance(rightPositions.elementAt(j+1));
			}
		}

		saveLatticeStatistics(outputDirectory + File.separator, centerSpline.GetLength(0, 1), left, right, leftDistances, rightDistances, "_before");
	}

	public static void saveContourAsCSV( ModelImage image, String subDir, String postScript, VOIContour contour )
	{
		String outputDirectory = new String(image.getImageDirectory() + JDialogBase.makeImageName(image.getImageFileName(), "") + File.separator + JDialogBase.makeImageName(image.getImageFileName(), "_results") );
		String parentDir = new String(image.getImageDirectory() + JDialogBase.makeImageName(image.getImageFileName(), "") + File.separator);
		checkParentDir(parentDir);	
		
		String voiDir = outputDirectory + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveImage " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + File.separator + subDir + File.separator;		
		
		String imageName = JDialogBase.makeImageName(image.getImageFileName(), "");
		imageName = imageName + postScript + ".csv";
		
		// check files, create new directories and delete any existing files:
		final File fileDir = new File(voiDir);

		if (fileDir.exists() && fileDir.isDirectory()) {} 
		else if (fileDir.exists() && !fileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			fileDir.mkdir();
		}
		File file = new File(fileDir + File.separator + imageName);
		if (file.exists()) {
			file.delete();
			file = new File(fileDir + File.separator + imageName);
		}


		if ( contour == null )
			return;
		if ( contour.size() == 0 )
			return;

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("Contour" + "\n");
			bw.write("x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "\n");
			for (int i = 0; i < contour.size(); i++) {
				Vector3f position = contour.elementAt(i);
				bw.write(position.X + "," + position.Y + "," + position.Z + "\n");

			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveSeamCellsTo");
			e.printStackTrace();
		}
	}


	public static void saveBasisVectorsAsCSV( ModelImage image, String subDir, String postScript, VOIContour positions,
			Vector<Vector3f> normals, Vector<Vector3f> rightVectors, Vector<Vector3f> upVectors )
	{
		String outputDirectory = new String(image.getImageDirectory() + JDialogBase.makeImageName(image.getImageFileName(), "") + File.separator + JDialogBase.makeImageName(image.getImageFileName(), "_results") );
		String parentDir = new String(image.getImageDirectory() + JDialogBase.makeImageName(image.getImageFileName(), "") + File.separator);
		checkParentDir(parentDir);	
		
		String voiDir = outputDirectory + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveImage " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + File.separator + subDir + File.separator;		
		
		String imageName = JDialogBase.makeImageName(image.getImageFileName(), "");
		imageName = imageName + postScript + ".csv";
		
		// check files, create new directories and delete any existing files:
		final File fileDir = new File(voiDir);

		if (fileDir.exists() && fileDir.isDirectory()) {} 
		else if (fileDir.exists() && !fileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			fileDir.mkdir();
		}
		File file = new File(fileDir + File.separator + imageName);
		if (file.exists()) {
			file.delete();
			file = new File(fileDir + File.separator + imageName);
		}


		if ( positions == null || normals == null || rightVectors == null || upVectors == null )
			return;
		if ( positions.size() == 0 || normals.size() == 0 || rightVectors.size() == 0 || upVectors.size() == 0 )
			return;

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("x" + "," + "y" + "," + "z" + "," + "xN" + "," + "yN" + "," + "zN" + "," + "xR" + "," + "yR" + "," + "zR" + "," + "xU" + "," + "yU" + "," + "zU" + "\n");
			for (int i = 0; i < positions.size(); i++) {
				Vector3f position = positions.elementAt(i);
				Vector3f normal = normals.elementAt(i);      normal.normalize();
				Vector3f right  = rightVectors.elementAt(i); right.normalize();
				Vector3f up     = upVectors.elementAt(i);    up.normalize();
				bw.write(position.X + "," + position.Y + "," + position.Z + "," + normal.X + "," + normal.Y + "," + normal.Z + "," + right.X + "," + right.Y + "," + right.Z + "," + up.X + "," + up.Y + "," + up.Z + "\n");

			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveSeamCellsTo");
			e.printStackTrace();
		}
	}


    public static void saveTriMesh( ModelImage image, String subDir, String postScript, TriMesh mesh ) {
		
		String outputDirectory = new String(image.getImageDirectory() + JDialogBase.makeImageName(image.getImageFileName(), "") + File.separator + JDialogBase.makeImageName(image.getImageFileName(), "_results") );
		String parentDir = new String(image.getImageDirectory() + JDialogBase.makeImageName(image.getImageFileName(), "") + File.separator);
		checkParentDir(parentDir);	
		
		String voiDir = outputDirectory + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveImage " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + File.separator + subDir + File.separator;		
		
		String imageName = JDialogBase.makeImageName(image.getImageFileName(), "");
		imageName = imageName + postScript;
		
		

//		int maxVal = -1;
//		voiFileDir = new File(voiDir);
//		if (voiFileDir.exists() && voiFileDir.isDirectory()) { 
//			final String[] list = voiFileDir.list();
//			for (int i = 0; i < list.length; i++) {
//				int fileVal = -1;
////				System.err.println(list[i] + "   " + imageName + "   " + list[i].contains(imageName) );
//				if ( list[i].contains(imageName) ) {
//					String sub = list[i].substring( list[i].indexOf(imageName) + imageName.length(), list[i].length() );
//					for ( int j = 0; j < sub.length(); j++ ) {
//						String test = new String( "" + sub.charAt(j) );
//						int val;
//						try {
//							val = Integer.valueOf(test);
//							if ( fileVal == -1 ) {
//								fileVal = val;
//							}
//							else
//							{
//								fileVal = fileVal * 10 + val;
//							}
//						} catch(NumberFormatException e) {
//						}
//					}
////					System.err.println( "    " + fileVal );
//				}
//				if ( fileVal > maxVal )
//				{
//					maxVal = fileVal;
//				}
//			}
//		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//		} else { // voiFileDir does not exist
//			//			System.err.println( "saveImage " + voiDir);
//			voiFileDir.mkdir();
//		}
//
//		maxVal++;
//		imageName = imageName + "_" + maxVal + ".ply";
		
		imageName = imageName + ".ply";
		File meshFile = new File(voiDir + imageName);
		if (meshFile.exists() && !meshFile.isDirectory()) { 
			meshFile.delete();
		}
        try {
        	System.err.println(voiDir + imageName );
            FileSurface_WM.save( voiDir + imageName, mesh, 0, mesh.VBuffer, false, null, null, null, null);
        } catch (IOException error) {
        	MipavUtil.displayError("Error while trying to save single mesh");
        }
    }



	private void saveSpline(final ModelImage image, VOI data, Vector3f transformedOrigin, final String postFix) {

		VOIContour spline = (VOIContour) data.getCurves().elementAt(0);

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		String voiDir = outputDirectory + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "saveSpline " + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + File.separator + "statistics" + File.separator;
		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
			// does
			// not
			// exist
			//			System.err.println( "saveSpline " + voiDir);
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + data.getName() + postFix + ".csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + data.getName() + postFix + ".csv");
		}

		try {			
			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("index" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "\n");
			for (int i = 0; i < spline.size(); i++) {
				Vector3f position = spline.elementAt(i);
				bw.write(i + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
						+ (position.Z - transformedOrigin.Z) + "," +

                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
			e.printStackTrace();
		}
	}

	/**
	 * First pass generating the worm model from the simple ellipse-based model. Any voxels that fall inside overlapping
	 * ellipses are set as conflict voxels.
	 * 
	 * @param image
	 * @param model
	 * @param insideConflict
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param ellipseBound
	 * @param diameter
	 * @param boxBound
	 * @param value
	 */
	//	private void initializeModelandConflicts(final ModelImage image, final ModelImage model, final ModelImage insideConflict, final int tSlice,
	//			final int slice, final int[] extents, final Vector3f[] verts, final Ellipsoid3f ellipseBound, final float diameter, final Box3f boxBound,
	//			final int value) {
	//		final int iBound = extents[0];
	//		final int jBound = extents[1];
	//
	//		final int[] dimExtents = image.getExtents();
	//
	//		/*
	//		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
	//		 * coordinate-systems: transformation:
	//		 */
	//		final int iFactor = 1;
	//		final int jFactor = dimExtents[0];
	//		final int kFactor = dimExtents[0] * dimExtents[1];
	//		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];
	//
	//		int buffFactor = 1;
	//
	//		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
	//				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
	//			buffFactor = 4;
	//		}
	//
	//		final Vector3f center = new Vector3f();
	//		for (int i = 0; i < verts.length; i++) {
	//			center.add(verts[i]);
	//		}
	//		center.scale(1f / verts.length);
	//
	//		/* Calculate the slopes for traversing the data in x,y,z: */
	//		float xSlopeX = verts[1].X - verts[0].X;
	//		float ySlopeX = verts[1].Y - verts[0].Y;
	//		float zSlopeX = verts[1].Z - verts[0].Z;
	//
	//		float xSlopeY = verts[3].X - verts[0].X;
	//		float ySlopeY = verts[3].Y - verts[0].Y;
	//		float zSlopeY = verts[3].Z - verts[0].Z;
	//
	//		float x0 = verts[0].X;
	//		float y0 = verts[0].Y;
	//		float z0 = verts[0].Z;
	//
	//		xSlopeX /= (iBound);
	//		ySlopeX /= (iBound);
	//		zSlopeX /= (iBound);
	//
	//		xSlopeY /= (jBound);
	//		ySlopeY /= (jBound);
	//		zSlopeY /= (jBound);
	//
	//		/* loop over the 2D image (values) we're writing into */
	//		float x = x0;
	//		float y = y0;
	//		float z = z0;
	//
	//		final Vector3f currentPoint = new Vector3f();
	//
	//		final boolean[][] values = new boolean[iBound][jBound];
	//		for (int j = 0; j < jBound; j++) {
	//
	//			/* Initialize the first diagonal point(x,y,z): */
	//			x = x0;
	//			y = y0;
	//			z = z0;
	//
	//			for (int i = 0; i < iBound; i++) {
	//				values[i][j] = false;
	//				final int iIndex = Math.round(x);
	//				final int jIndex = Math.round(y);
	//				final int kIndex = Math.round(z);
	//
	//				/* calculate the ModelImage space index: */
	//				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));
	//
	//				// Bounds checking:
	//				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
	//						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {
	//
	//					// do nothing
	//				} else {
	//					currentPoint.set(x, y, z);
	//					final boolean isInside = ellipseBound.Contains(currentPoint);
	//					if ( !isInside) {
	//						// do nothing
	//					} else {
	//						values[i][j] = true;
	//						for (int z1 = Math.max(0, (int) Math.floor(z)); z1 <= Math.min(dimExtents[2] - 1, Math.ceil(z)); z1++) {
	//							for (int y1 = Math.max(0, (int) Math.floor(y)); y1 <= Math.min(dimExtents[1] - 1, Math.ceil(y)); y1++) {
	//								for (int x1 = Math.max(0, (int) Math.floor(x)); x1 <= Math.min(dimExtents[0] - 1, Math.ceil(x)); x1++) {
	//									final int currentValue = model.getInt(x1, y1, z1);
	//									if (currentValue != 0) {
	//										if (Math.abs(currentValue - value) < SampleLimit) {
	//											// model.set(x1, y1, z1, (currentValue + value)/2f);
	//										} else {
	//											insideConflict.set(x1, y1, z1, true);
	//										}
	//									} else {
	//										model.set(x1, y1, z1, value);
	//									}
	//								}
	//							}
	//						}
	//					}
	//				}
	//
	//				/*
	//				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
	//				 * ySlopeX and zSlopeX values:
	//				 */
	//				x = x + xSlopeX;
	//				y = y + ySlopeX;
	//				z = z + zSlopeX;
	//			}
	//
	//			/*
	//			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
	//			 * ySlopeY and zSlopeY values:
	//			 */
	//			x0 = x0 + xSlopeY;
	//			y0 = y0 + ySlopeY;
	//			z0 = z0 + zSlopeY;
	//		}
	//	}

	private void testConflicts( ModelImage image, ModelImage model, BitSet conflicts,
			final int slice, final int[] extents, final Vector3f[] verts, final Box3f boxBound, final Ellipsoid3f ellipseBound, final int value) {
		final int iBound = extents[0];
		final int jBound = extents[1];

		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		final Vector3f currentPoint = new Vector3f();

		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {
					currentPoint.set(x, y, z);
					final boolean inBox = ContBox3f.InBox( currentPoint, boxBound );
					if ( inBox )
					{
						final boolean isInside = ellipseBound.Contains(currentPoint);
						if ( isInside) {
							final int currentValue = model.getInt(iIndex, jIndex, kIndex);
							if (currentValue != 0) {
								if (Math.abs(currentValue - value) < SampleLimit) {
								} else {
									conflicts.set(index);
								}
							} else {
								model.set(iIndex, jIndex, kIndex, value);
							}
						}
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
	}


	/**
	 * Straightens the worm image based on the input lattice positions. The image is straightened without first building a worm model.
	 * The image is segmented after clipping based on surface markers or the lattice shape.
	 * @param image
	 * @param resultExtents
	 */
	private void untwist(final ModelImage image, final int[] resultExtents, boolean saveTif)
	{
		long time = System.currentTimeMillis();
		int size = samplingPlanes.getCurves().size();

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		ModelImage resultImage;
		if ( image.isColorImage() )
		{
			resultImage = new ModelImage( ModelStorageBase.ARGB, resultExtents, imageName + "_straight_unmasked.xml");
		}
		else
		{
			resultImage = new ModelImage( ModelStorageBase.FLOAT, resultExtents, imageName + "_straight_unmasked.xml");
		}	
		resultImage.setResolutions(new float[] {1, 1, 1});


		System.err.println( "starting untwist..." );

		int dimX = (resultImage.getExtents()[0]);
		int dimY = (resultImage.getExtents()[1]);
		int dimZ = size;
		Vector3f center = new Vector3f( dimX/2, dimY/2, 0 );

		// this is the untwisting code:
		final Vector3f[] corners = new Vector3f[4];
		for (int i = 0; i < size; i++)
		{			
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			writeDiagonal(image, resultImage, 0, i, resultExtents, corners);
		}

		System.err.println( "saving image " + imageName + " " + resultImage.getImageName() );
		saveImage(imageName, resultImage, saveTif);
		resultImage.disposeLocal(false);
		resultImage = null;


		System.err.println( "writeDiagonal " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
	}

	/**
	 * Untwists the lattice and lattice interpolation curves, writing the
	 * straightened data to spreadsheet format .csv files and the straightened lattice
	 * to a VOI file.  The target slices for the lattice and curve data is generated
	 * from the distance along the curve. No segmentation or interpolation is necessary:
	 * @param image
	 * @param resultExtents
	 */
	private void untwistLattice(final ModelImage image, final int[] resultExtents)
	{
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		// Straightens the interpolation curves:
		Vector3f[][] averageCenters = new Vector3f[centerPositions.size()][];
		final Vector3f[] corners = new Vector3f[4];		
		for (int i = 0; i < centerPositions.size(); i++)
		{			
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}
			averageCenters[i] = straightenFrame(image, i, resultExtents, corners, centerPositions.elementAt(i), leftPositions.elementAt(i), rightPositions.elementAt(i) );
		}

		// saves the data to the output directory:
		String voiDir = outputDirectory;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			//			System.err.println( "untwistLattice" + voiDir);
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + File.separator + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			//			System.err.println( "untwistLattice" + voiDir);
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + imageName + "_Frame_Straight.csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + imageName + "_Frame_Straight.csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);

			bw.write("Center" + "," + "X" + "," + "Y" + "," + "Z" + "," + "Left" + "," + "X" + "," + "Y" + "," + "Z" + "," + "Right" + "," + "X" + "," + "Y" + "," + "Z" + "\n");

			for ( int i = 0; i < centerPositions.size(); i++ )
			{
				if ( averageCenters[i] != null )
				{
					Vector3f pt = averageCenters[i][0];
					// calculate the output in worm coordinates:
					pt.X -= resultExtents[0] / 2f;
					pt.Y -= resultExtents[1] / 2f;
					//					pt.scale( resX, resY, 1 );
					bw.write("C" + i + "," + pt.X + "," + pt.Y + "," + pt.Z + ",");

					pt = averageCenters[i][1];
					// calculate the output in worm coordinates:
					pt.X -= resultExtents[0] / 2f;
					pt.Y -= resultExtents[1] / 2f;
					//					pt.scale( resX, resY, 1 );
					bw.write("L" + i + "," + pt.X + "," + pt.Y + "," + pt.Z + ",");

					pt = averageCenters[i][2];
					// calculate the output in worm coordinates:
					pt.X -= resultExtents[0] / 2f;
					pt.Y -= resultExtents[1] / 2f;
					//					pt.scale( resX, resY, 1 );
					bw.write("R" + i + "," + pt.X + "," + pt.Y + "," + pt.Z + "\n");
				}
			}

			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}



		// Straightens and saves the lattice:
		FileIO fileIO = new FileIO();

		ModelImage resultImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_straight_unmasked.xml" );
		int dimX = (resultImage.getExtents()[0]);
		int dimY = (resultImage.getExtents()[1]);
		Vector3f center = new Vector3f( dimX/2, dimY/2, 0 );

		short id = (short) image.getVOIs().getUniqueID();
		final VOIVector lattice = new VOIVector();
		VOI leftSide = new VOI(id, "left", VOI.ANNOTATION, (float) Math.random());
		VOI rightSide = new VOI(id++, "right", VOI.ANNOTATION, (float) Math.random());
		lattice.add(leftSide);
		lattice.add(rightSide);
		float[] minDistance = new float[1];
		for (int i = 0; i < left.getCurves().size(); i++) {

			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(latticeSlices[i]);
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			final Vector3f leftPt = writeDiagonal(image, latticeSlices[i], resultExtents, corners, left.getCurves().elementAt(i).elementAt(0), minDistance );
			final Vector3f rightPt = writeDiagonal(image, latticeSlices[i], resultExtents, corners, right.getCurves().elementAt(i).elementAt(0), minDistance );

			//			System.err.println( i + " " + latticeSlices[i] + "    " + leftPt + "    " + rightPt );

			VOIWormAnnotation leftAnnotation = new VOIWormAnnotation((VOIWormAnnotation)left.getCurves().elementAt(i));
			leftAnnotation.clear();
			leftAnnotation.add(leftPt); 
			leftSide.getCurves().add(leftAnnotation);

			VOIWormAnnotation rightAnnotation = new VOIWormAnnotation((VOIWormAnnotation)right.getCurves().elementAt(i));
			rightAnnotation.clear();
			rightAnnotation.add(rightPt);
			rightSide.getCurves().add(rightAnnotation);
		}
		final float[] leftDistances = new float[leftSide.getCurves().size()];
		final float[] rightDistances = new float[leftSide.getCurves().size()];
		for (int i = 0; i < leftSide.getCurves().size(); i++) {
			leftDistances[i] = 0;
			rightDistances[i] = 0;
		}
		for (int i = 0; i < leftSide.getCurves().size() -1; i++) {
			leftDistances[i] = leftSide.getCurves().elementAt(i).elementAt(0).distance(leftSide.getCurves().elementAt(i + 1).elementAt(0));
			rightDistances[i] = rightSide.getCurves().elementAt(i).elementAt(0).distance(rightSide.getCurves().elementAt(i + 1).elementAt(0));
		}
//
//		resultImage.registerVOI(lattice);
//		lattice.setColor(new Color(0, 0, 255));
//		lattice.getCurves().elementAt(0).update(new ColorRGBA(0, 0, 1, 1));
//		lattice.getCurves().elementAt(1).update(new ColorRGBA(0, 0, 1, 1));
//		lattice.getCurves().elementAt(0).setClosed(false);
//		lattice.getCurves().elementAt(1).setClosed(false);
//
//		id = (short) image.getVOIs().getUniqueID();
//		for (int j = 0; j < leftSide.size(); j++) {
//			id = (short) image.getVOIs().getUniqueID();
//			final VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float) Math.random());
//			final VOIContour mainAxis = new VOIContour(false);
//			mainAxis.add(leftSide.elementAt(j));
//			mainAxis.add(rightSide.elementAt(j));
//			marker.getCurves().add(mainAxis);
//			marker.setColor(new Color(255, 255, 0));
//			mainAxis.update(new ColorRGBA(1, 1, 0, 1));
//			if (j == 0) {
//				marker.setColor(new Color(0, 255, 0));
//				mainAxis.update(new ColorRGBA(0, 1, 0, 1));
//			}
//			resultImage.registerVOI(marker);
//		}

		voiDir = outputDirectory + File.separator + "straightened_lattice" + File.separator;
		//		saveAllVOIsTo(voiDir, resultImage);


		VOI latticePoints = new VOI( (short)0, "lattice", VOI.ANNOTATION, 0);
		for ( int j = 0; j < leftSide.getCurves().size(); j++ )
		{
			latticePoints.getCurves().add(leftSide.getCurves().elementAt(j));
			latticePoints.getCurves().add(rightSide.getCurves().elementAt(j));
		}
		LatticeModel.saveAnnotationsAsCSV(voiDir, "straightened_lattice.csv", latticePoints);
		saveLatticeStatistics(outputDirectory + File.separator, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");


		String voiSeamDir = outputDirectory + File.separator + "straightened_seamcells" + File.separator;
		latticePoints.getCurves().clear();
		for ( int j = 0; j < leftSide.getCurves().size(); j++ )
		{
			VOIWormAnnotation leftAnnotation = (VOIWormAnnotation) leftSide.getCurves().elementAt(j);
			VOIWormAnnotation rightAnnotation = (VOIWormAnnotation) rightSide.getCurves().elementAt(j);
			if ( leftAnnotation.isSeamCell() || rightAnnotation.isSeamCell() ) {
				latticePoints.getCurves().add(leftAnnotation);
				latticePoints.getCurves().add(rightAnnotation);
			}
		}
		LatticeModel.saveAnnotationsAsCSV(voiSeamDir, "straightened_seamcells.csv", latticePoints);
		
		transformedOrigin = new Vector3f( center );

		resultImage.disposeLocal(false);
		resultImage = null;
	}

	private void untwistMarkers(final ModelImage image, final int[] resultExtents )
	{
		System.err.println( "untwistMarkers NEW " + paddingFactor);

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		long time = System.currentTimeMillis();
		if ( samplingPlanes == null )
		{
			samplingPlanes = loadSamplePlanes( outputDirectory + File.separator );
		}
		if ( wormDiameters == null )
		{
			wormDiameters = loadDiameters( outputDirectory + File.separator );
		}
		int size = samplingPlanes.getCurves().size();

        ModelImage contourImage = null;
		VOIVector contourVector = new VOIVector();
		if ( latticeContours != null ) {
			System.err.println("using lattice contours");
			contourVector.add(latticeContours);
		}
		else {
			// Load skin marker contours:
			System.err.println(outputDirectory + File.separator + "output_images" + File.separator + imageName + "_contours.xml" );
			FileIO fileIO = new FileIO();
			contourImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_contours.xml" );
		}

		VOIContour[] contours = null;
		if ( contourVector.size() > 0 ) {
			for ( int i = 0; i < contourVector.elementAt(0).getCurves().size(); i++ )
			{	
				if ( contours == null ) {
					contours = new VOIContour[size];
				}
				VOIContour contour = (VOIContour)contourVector.elementAt(0).getCurves().elementAt(i);
				int index = (int) contour.elementAt(0).Z;
				contours[index] =  contour;
			}
		}


		final Vector3f[] corners = new Vector3f[4];		
		float[] minDistance = new float[1];
		annotationsStraight = new VOI( (short)0, "straightened annotations", VOI.ANNOTATION, 0 );
		for ( int i = 0; i < markerCenters.size(); i++ )
		{
			int latticeSegment = markerLatticeSegments.elementAt(i);
			int startIndex = 0;
			int endIndex = size;
			if ( (latticeSegment > 0) && (latticeSegment < splineRangeIndex.length) ) {
				startIndex = splineRangeIndex[latticeSegment-1];
				endIndex = splineRangeIndex[latticeSegment];
			}

			float minUntwist = Float.MAX_VALUE;
			int minSlice = -1;
			Vector3f minPt = new Vector3f();
			int tryCount = 0;
			int slice = markerSlices.elementAt(i);
			if (slice != -1 ) {
				startIndex = slice;
				endIndex = slice+1;
			}
			while ( minSlice == -1 && tryCount < 3 ) {
				for ( int j = startIndex; j < endIndex; j++ )
				{			
					// Calculate the straightened marker location:
					VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(j);
					for (int k = 0; k < 4; k++) {
						corners[k] = kBox.elementAt(k);
					}
					Vector3f markerPt = writeDiagonal(image, j, resultExtents, corners, markerCenters.elementAt(i), minDistance );

					// If it is inside the skin marker contour:
					if ( contours == null && contourImage != null )
					{
						if ( ( minDistance[0] < (tryCount * 5) ) && 
								contourImage.getFloat( (int)markerPt.X, (int)markerPt.Y, j) != 0 )
						{
							if ( minDistance[0] < minUntwist ) {
								minUntwist = minDistance[0];
								minSlice = j;
								minPt.copy(markerPt);
							}
						}						

					}
					else
					{
						if ( ( minDistance[0] < (tryCount * 5) ) && ((contours[j] == null) || contours[j].contains( markerPt.X, markerPt.Y )) )
						{
							if ( minDistance[0] < minUntwist ) {
								minUntwist = minDistance[0];
								minSlice = j;
								minPt.copy(markerPt);
							}
						}						
					}

				}
				if ( minSlice != -1 ) {
					VOIWormAnnotation text = new VOIWormAnnotation();
					text.setText(markerNames.elementAt(i));
					text.add( new Vector3f(minPt) );
					text.add( new Vector3f(minPt) );
					annotationsStraight.getCurves().add(text);
				}
				tryCount++;
			}
//			System.err.println( markerNames.elementAt(i) + "   " + minSlice + "   " + minUntwist );

		}

		if ( contourImage != null ) {
			contourImage.disposeLocal(false);
			contourImage = null;
		}
		
		
		System.err.println( "untwist markers (skin segmentation) " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();

	}

	public ModelImage untwistAnnotations(ModelImage contourImage)
	{

		String imageName = imageA.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		
		String voiDir = outputDirectory + File.separator + "straightened_annotations" + File.separator + "straightened_annotations.csv";
		File file = new File(voiDir);
		boolean untwistAll = !file.exists();
		
		// if the lattice has changed untwist the image and generate the contours and contour image again:
		File lattice = new File(outputDirectory + File.separator + WormData.editLatticeOutput + File.separator + "lattice.csv");
		long latticeChanged = lattice.lastModified();
		File contourFile = new File( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_contours.xml" );
		long contourChanged = contourFile.lastModified();
		
		ModelImage currentContourImage = contourImage;
		if ( latticeChanged < contourChanged ) {
			// lattice was changed more recently than the contour image - restraighten all.
			untwistAll = true;
		}		
		if ( latticeChanged() || untwistAll ) 
		{
			System.err.println( "lattice changed" );
			initializeInterpolation(true);
			untwistImage( true );
			segmentLattice(imageA, true, paddingFactor, false);
			System.err.println( outputDirectory + File.separator + WormData.editLatticeOutput );
			saveLattice( outputDirectory + File.separator, WormData.editLatticeOutput );
			untwistAll = true;
			
			FileIO fileIO = new FileIO();
			currentContourImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_contours.xml" );
		}		
		else if ( !latticeInterpolationInit ) {
			initializeInterpolation(false);
		}
		if ( currentContourImage == null ) {
			FileIO fileIO = new FileIO();
			currentContourImage = fileIO.readImage( outputDirectory + File.separator + "output_images" + File.separator + imageName + "_contours.xml" );	
		}


		// load the existing annotation file - use it to determine which annotations need untwisting.
		VOI originalAnnotation = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + WormData.integratedAnnotationOutput + File.separator + "annotations.csv");

		// look for changes in annotations - build list to untwist:
		VOI changed = annotationChanged( annotationVOIs, originalAnnotation );

		// if the lattice has changed untwist the image and generate the contours and contour image again:
		File annotationTwisted = new File(outputDirectory + File.separator + WormData.integratedAnnotationOutput + File.separator + "annotations.csv");
		long twistedChanged = annotationTwisted.lastModified();
		File annotationStraight = new File( outputDirectory + File.separator + "straightened_annotations" + File.separator + "straightened_annotations.csv" );
		long straightChanged = annotationStraight.lastModified();

		VOI originalStraight = null;
		// if the twisted file has changed since straightened - then straighten all:
		if ( twistedChanged < straightChanged ) {
			untwistAll = true;
		}		
		// load the straightened annotations - if they exist.
		if ( file.exists() && !untwistAll ) {	
			if ( changed.getCurves().size() == 0 ) {
				// no changes just return:
				System.err.println("untwisting none...");

				return currentContourImage;
			}
			originalStraight = LatticeModel.readAnnotationsCSV(voiDir);
		}

		
		
		final int[] resultExtents = currentContourImage.getExtents();
		int size = resultExtents[2];

		final Vector3f[] corners = new Vector3f[4];		
		float[] minDistance = new float[1];
		annotationsStraight = new VOI( (short)0, "straightened annotations", VOI.ANNOTATION, 0 );
		
		if ( untwistAll ) {
			System.err.println("untwisting all...");
			for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
			{
				VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);

				int latticeSegment = -1; //markerLatticeSegments.elementAt(i);
				int startIndex = 0;
				int endIndex = size;
				if ( (latticeSegment > 0) && (latticeSegment < splineRangeIndex.length) ) {
					startIndex = splineRangeIndex[latticeSegment-1];
					endIndex = splineRangeIndex[latticeSegment];
				}

				float minUntwist = Float.MAX_VALUE;
				int minSlice = -1;
				Vector3f minPt = new Vector3f();
				int tryCount = 0;
				int slice = -1; //markerSlices.elementAt(i);
				if (slice != -1 ) {
					startIndex = slice;
					endIndex = slice+1;
				}
				while ( minSlice == -1 && tryCount < 3 ) {
					for ( int j = startIndex; j < endIndex; j++ )
					{			
						// Calculate the straightened marker location:
						VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(j);
						for (int k = 0; k < 4; k++) {
							corners[k] = kBox.elementAt(k);
						}
						Vector3f markerPt = writeDiagonal(imageA, j, resultExtents, corners, text.elementAt(0), minDistance );

						// If it is inside the skin marker contour:
						if ( contourImage != null )
						{
							if ( ( minDistance[0] < (tryCount * 5) ) && 
									contourImage.getFloat( (int)markerPt.X, (int)markerPt.Y, j) != 0 )
							{
								if ( minDistance[0] < minUntwist ) {
									minUntwist = minDistance[0];
									minSlice = j;
									minPt.copy(markerPt);
								}
							}						

						}
					}
					if ( minSlice != -1 ) {
						VOIWormAnnotation textStraight = new VOIWormAnnotation();
						textStraight.setSeamCell(text.isSeamCell());
						textStraight.setText(text.getText());
						textStraight.add( new Vector3f(minPt) );
						textStraight.add( new Vector3f(minPt) );
						annotationsStraight.getCurves().add(textStraight);
					}
					tryCount++;
				}
				//			System.err.println( markerNames.elementAt(i) + "   " + minSlice + "   " + minUntwist );

			}
		}
		else {
			System.err.println("untwisting some...");

			// remove change from original straight:
			for ( int i = 0; i < changed.getCurves().size(); i++ ) {      
				VOIWormAnnotation newA = (VOIWormAnnotation) changed.getCurves().elementAt(i);

	    		for ( int j = 0; j < originalStraight.getCurves().size(); j++ ) {
	            	VOIWormAnnotation orig = (VOIWormAnnotation) originalStraight.getCurves().elementAt(j);
	            	if ( newA.getText().equals(orig.getText()) ) {
	            		originalStraight.getCurves().remove(j);
	            		break;
	            	}
	    		}				
			}
			// untwist change -> original straight:

			for ( int i = 0; i < changed.getCurves().size(); i++ )
			{
				VOIWormAnnotation text = (VOIWormAnnotation) changed.getCurves().elementAt(i);

				int latticeSegment = -1; //markerLatticeSegments.elementAt(i);
				int startIndex = 0;
				int endIndex = size;
				if ( (latticeSegment > 0) && (latticeSegment < splineRangeIndex.length) ) {
					startIndex = splineRangeIndex[latticeSegment-1];
					endIndex = splineRangeIndex[latticeSegment];
				}

				float minUntwist = Float.MAX_VALUE;
				int minSlice = -1;
				Vector3f minPt = new Vector3f();
				int tryCount = 0;
				int slice = -1; //markerSlices.elementAt(i);
				if (slice != -1 ) {
					startIndex = slice;
					endIndex = slice+1;
				}
				while ( minSlice == -1 && tryCount < 3 ) {
					for ( int j = startIndex; j < endIndex; j++ )
					{			
						// Calculate the straightened marker location:
						VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(j);
						for (int k = 0; k < 4; k++) {
							corners[k] = kBox.elementAt(k);
						}
						Vector3f markerPt = writeDiagonal(imageA, j, resultExtents, corners, text.elementAt(0), minDistance );

						// If it is inside the skin marker contour:
						if ( contourImage != null )
						{
							if ( ( minDistance[0] < (tryCount * 5) ) && 
									contourImage.getFloat( (int)markerPt.X, (int)markerPt.Y, j) != 0 )
							{
								if ( minDistance[0] < minUntwist ) {
									minUntwist = minDistance[0];
									minSlice = j;
									minPt.copy(markerPt);
								}
							}						

						}
					}
					if ( minSlice != -1 ) {
						VOIWormAnnotation textStraight = new VOIWormAnnotation();
						textStraight.setSeamCell(text.isSeamCell());
						textStraight.setText(text.getText());
						textStraight.add( new Vector3f(minPt) );
						textStraight.add( new Vector3f(minPt) );
						originalStraight.getCurves().add(textStraight);
					}
					tryCount++;
				}
			}
			
			// double check all current annotations are in original straight:
			if ( annotationVOIs.getCurves().size() < originalStraight.getCurves().size() ) {
				for ( int i = originalStraight.getCurves().size() - 1; i >= 0; i-- ) {
	            	VOIWormAnnotation orig = (VOIWormAnnotation) originalStraight.getCurves().elementAt(i);
	            	boolean found = false;
		    		for ( int j = 0; j < annotationVOIs.getCurves().size(); j++ ) {
		            	VOIWormAnnotation annotation = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(j);
		            	if ( annotation.getText().equals(orig.getText()) ) {
		            		found = true;
		            		break;
		            	}
		    		}
		    		if ( !found ) {
		    			originalStraight.getCurves().remove(i);
		    		}
				}
			}
			if ( annotationVOIs.getCurves().size() != originalStraight.getCurves().size() ) {
				MipavUtil.displayError("untwist annotations error" );
			}

			
			// copy into annotationsStraight:
			annotationsStraight.getCurves().clear();
			annotationsStraight.getCurves().addAll(originalStraight.getCurves());
		}
		
		// save annotations:
		saveAnnotationsAsCSV(outputDirectory + File.separator + WormData.integratedAnnotationOutput + File.separator, "annotations.csv", annotationVOIs);
		// save straight annotations:
		saveAnnotationStraight(imageA, "straightened_annotations", "straightened_annotations.csv" );	
		
		return currentContourImage;
	}


	/**
	 * Untwists the worm image quickly for the preview mode - without saving any images or statistics
	 * Straightens the worm image based on the input lattice positions. The image is straightened without first building a worm model.
	 * The image is segmented after clipping based on surface markers or the lattice shape.
	 * @param image
	 * @param resultExtents
	 */
	private ModelImage untwistTest(final ModelImage image, boolean straightenImage, final int[] resultExtents)
	{
		System.err.println("untwistTest "  + paddingFactor );
		long time = System.currentTimeMillis();
		int size = samplingPlanes.getCurves().size();

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		ModelImage resultImage = null;
		if ( straightenImage ) {
			if ( image.isColorImage() )
			{
				resultImage = new ModelImage( ModelStorageBase.ARGB, resultExtents, imageName + "_straight_unmasked.xml");
			}
			else
			{
				resultImage = new ModelImage( ModelStorageBase.FLOAT, resultExtents, imageName + "_straight_unmasked.xml");
			}	
			resultImage.setResolutions(new float[] {1, 1, 1});
		}


		int dimX = resultExtents[0];
		int dimY = resultExtents[1];
		int dimZ = size;
		Vector3f center = new Vector3f( dimX/2f, dimY/2f, 0f );

		if ( latticeContours != null ) {
			latticeContours.dispose();
			latticeContours = null;
		}
		latticeContours = new VOI( (short)1, "contours", VOI.POLYLINE, (float) Math.random());

		float maxDist = Vector3f.normalize(center);
		// this is the untwisting code:
		final Vector3f[] corners = new Vector3f[4];
		for (int i = 0; i < size; i++)
		{			
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}	

			float radius = (float) (1.05 * rightPositions.elementAt(i).distance(leftPositions.elementAt(i))/(2f));
			radius += paddingFactor;

			VOIContour contour = new VOIContour(true);
			makeEllipse2DA(Vector3f.UNIT_X, Vector3f.UNIT_Y, center, radius, contour);			
			if ( (clipMask != null) && (clipMask.size() == dimZ) ) {
				//					System.err.println( "use clip mask " + i + "  " + clipMask.size() );
				boolean[] mask = clipMask.elementAt(i);
				for ( int j = 0; j < mask.length; j++ ) {
					if ( !mask[j] ) {
						// extend contour out to edge:
						Vector3f dir = Vector3f.sub( contour.elementAt(j), center );
						dir.normalize();
						dir.scale(maxDist);
						dir.add(center);
						contour.elementAt(j).copy(dir);
					}
				}
			}
			for ( int j = 0; j < contour.size(); j++ )
			{
				contour.elementAt(j).Z = i;
			}
			latticeContours.getCurves().add( contour );

			if ( straightenImage ) {
				float radiusSq = radius*radius;
				writeDiagonalTest(image, resultImage, 0, i, resultExtents, corners, radiusSq);
			}
		}
		if ( straightenImage ) {
			if ( resultImage.getMin() > 0 ) {
				resultImage.setMin(0);
			}
		}
		// straighten lattice:
		short id = (short) image.getVOIs().getUniqueID();
		latticeStraight = new VOIVector();
		VOI leftSide = new VOI(id, "left", VOI.ANNOTATION, (float) Math.random());
		VOI rightSide = new VOI(id++, "right", VOI.ANNOTATION, (float) Math.random());
		latticeStraight.add(leftSide);
		latticeStraight.add(rightSide);
		float[] minDistance = new float[1];
		for (int i = 0; i < left.getCurves().size(); i++) {

			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(latticeSlices[i]);
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			final Vector3f leftPt = writeDiagonal(image, latticeSlices[i], resultExtents, corners, left.getCurves().elementAt(i).elementAt(0), minDistance );
			VOIWormAnnotation leftTemp = (VOIWormAnnotation)left.getCurves().elementAt(i);
			VOIWormAnnotation leftAnnotation = new VOIWormAnnotation(leftTemp);
			leftAnnotation.clear();
			leftAnnotation.add(leftPt);			
			leftSide.getCurves().add(leftAnnotation);
			
			final Vector3f rightPt = writeDiagonal(image, latticeSlices[i], resultExtents, corners, right.getCurves().elementAt(i).elementAt(0), minDistance );
			VOIWormAnnotation rightTemp = (VOIWormAnnotation)right.getCurves().elementAt(i);
			VOIWormAnnotation rightAnnotation = new VOIWormAnnotation(rightTemp);
			rightAnnotation.clear();
			rightAnnotation.add(rightPt);
			rightSide.getCurves().add(rightAnnotation);
			
//			System.err.println( leftTemp.getText() + "  " + rightTemp.getText() + "         " + leftAnnotation.getText() + "  " + rightAnnotation.getText() );
		}

		// straighten markers:
		if ( annotationVOIs != null ) {

			VOIContour[] contours = new VOIContour[size];
			for ( int i = 0; i < latticeContours.getCurves().size(); i++ )
			{	
				VOIContour contour = (VOIContour)latticeContours.getCurves().elementAt(i);
				int index = (int) contour.elementAt(0).Z;
				contours[index] =  contour;
			}

			annotationsStraight = new VOI( (short)0, "annotationVOIs", VOI.ANNOTATION, 0 );
			String failList = new String("The following annotations are outside the worm bounds: \n" );


			boolean allFound = false;
			boolean[] found = new boolean[annotationVOIs.getCurves().size()];
			for ( int i = 0; i < found.length; i++ ) {
				found[i] = false;
			}
			int tryCount = 0;
			while ( !allFound && (tryCount < 3) )
			{
				for ( int i = 0; i < size; i++ )
				{
					for ( int j = 0; j < annotationVOIs.getCurves().size(); j++ )
					{
						VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(j);
						// if first loop or still no matching point:
						if ( (tryCount == 0) || (text.getUntwistTest() == null) )
						{
							int startIndex = 0;
							int endIndex = size;
							int latticeSegment = text.getLatticeSegment();
							if ( (latticeSegment > 0) && (latticeSegment < splineRangeIndex.length) ) {
								startIndex = splineRangeIndex[latticeSegment-1];
								endIndex = splineRangeIndex[latticeSegment];
							}
							int slice = text.getSlice();
							if ( slice != -1 ) {
								startIndex = slice;
								endIndex = slice;
							}
							if ( i >= startIndex && i <= endIndex ) 
							{
								// untwist this annotation:
								// Calculate the straightened marker location:
								VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
								for (int k = 0; k < 4; k++) {
									corners[k] = kBox.elementAt(k);
								}
								Vector3f untwistPt = writeDiagonal(image, i, resultExtents, corners, text.firstElement(), minDistance );

								// If it is inside the skin marker contour:
								if ( ( minDistance[0] < (tryCount * 5) ) && ((contours[i] == null) || contours[i].contains( untwistPt.X, untwistPt.Y )) )
								{
									text.untwistTest( untwistPt, minDistance[0] );
									found[j] = true;
								}
								text.untwistTestNoBounds( untwistPt, minDistance[0] );
							}
						}
					}
				}
				allFound = true;
				for ( int j = 0; j < found.length; j++ )
				{
					allFound &= found[j];
				}
				tryCount++;
			}

			int failCount = 0;
			for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
			{
				VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
				// No matching point:
				if ( text.getUntwistTest() == null )
				{
					failList += text.getText() + "\n";
					failCount++;
					System.err.println( text.getText() + " FAIL" );
					VOIWormAnnotation newText = new VOIWormAnnotation(text);
					newText.firstElement().copy(text.getUntwistTestNoBounds());
					newText.lastElement().copy(text.getUntwistTestNoBounds());
					newText.setColor(Color.red);
					annotationsStraight.getCurves().add(newText);
				}
				else
				{
					VOIWormAnnotation newText = new VOIWormAnnotation(text);
					newText.firstElement().copy(text.getUntwistTest());
					newText.lastElement().copy(text.getUntwistTest());
					annotationsStraight.getCurves().add(newText);
				}
			}
			if ( failCount > 0 ) {
				MipavUtil.displayInfo(failList);
			}
		}
		System.err.println( "Untwisting TEST elapsed time =  " + AlgorithmBase.computeElapsedTime(time) );

		return resultImage;
	}

	public static String getPrefix(String name) {
		String prefix = new String();
		for ( int j = 0; j < name.length(); j++ ) {
			if ( Character.isLetter(name.charAt(j) ) ) {
				prefix += name.charAt(j);
			}
			else {
				break;
			}
		}
		return prefix;
	}
	
	private boolean containsAnnotation( VOI annotationsList, VOIWormAnnotation text ) {
		for ( int i = 0; i < annotationsList.getCurves().size(); i++ ) {
			if ( annotationsList.getCurves().elementAt(i) == text ) {
				return true;
			}
		}
		return false;
	}
	
	private void updateSplineControlPoints() {
		if ( splineControlPtsList == null ) return;
		if ( splineControlPtsList.size() == 0 ) return;
		
		// check for added / removed points:
		// update spline:
		for ( int i = splineControlPtsList.size() - 1; i >= 0; i-- )
		{
			AnnotationSplineControlPts annotationSpline = splineControlPtsList.elementAt(i);
			// remove deleted points:
			for ( int j = annotationSpline.annotations.size() - 1; j >= 0; j-- ) {
				if ( !containsAnnotation(annotationVOIs, annotationSpline.annotations.elementAt(j) ) )
				{
					annotationSpline.annotations.remove(j);
				}
				else if ( !annotationSpline.prefix.equals( getPrefix(annotationSpline.annotations.elementAt(j).getText()) ) ) 
				{
					annotationSpline.annotations.remove(j);
				}
			}
			// add new points w/same prefix:
			for ( int j = 0; j < annotationVOIs.getCurves().size(); j++ ) {
				VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(j);
				if ( getPrefix(text.getText()).equals( annotationSpline.prefix ) ) {
					if ( !annotationSpline.annotations.contains(text) ) {
						// find two closest pts and add it...
						int nearest1 = -1;
						float minDist1 = Float.MAX_VALUE;
						for ( int k = 0; k < annotationSpline.annotations.size(); k++ ) {
							float dist = text.elementAt(0).distance( annotationSpline.annotations.elementAt(k).elementAt(0) );
							if ( dist < minDist1 ) {
								minDist1 = dist;
								nearest1 = k;
							}
						}
						System.err.println(text.getText() + "  " + nearest1 + "  " + annotationSpline.annotations.size() );
						if ( nearest1 == 0 ) {
							// place either before of after first element:
							float distBetween = annotationSpline.annotations.elementAt(nearest1).elementAt(0).distance( annotationSpline.annotations.elementAt(nearest1+1).elementAt(0) );
							float distNext = text.elementAt(0).distance( annotationSpline.annotations.elementAt(1).elementAt(0) );
							if ( distNext > distBetween ) {
								annotationSpline.annotations.add(0, text);
							}
							else {
								annotationSpline.annotations.add(1, text);
							}							
						}
						else if ( nearest1 == annotationSpline.annotations.size() -1 ) {
							// place either before of after last element:
							float distBetween = annotationSpline.annotations.elementAt(nearest1).elementAt(0).distance( annotationSpline.annotations.elementAt(nearest1-1).elementAt(0) );
							float distNext = text.elementAt(0).distance( annotationSpline.annotations.elementAt(nearest1-1).elementAt(0) );
							if ( distNext > distBetween ) {
								annotationSpline.annotations.add(text);
							}
							else {
								annotationSpline.annotations.add(nearest1-1, text);
							}							
						}
						else if ( nearest1 != -1 ) {
							float distPrev = text.elementAt(0).distance( annotationSpline.annotations.elementAt(nearest1-1).elementAt(0) );						
							float distNext = text.elementAt(0).distance( annotationSpline.annotations.elementAt(nearest1+1).elementAt(0) );
							if ( distPrev <= distNext ) {
								annotationSpline.annotations.add(nearest1, text);
							}
							else {
								annotationSpline.annotations.add(nearest1+1, text);
							}
						}
					}
				}
			}
			if ( annotationSpline.annotations.size() < 2 ) {
				imageA.unregisterVOI(annotationSpline.curveVOI);
				splineControlPtsList.remove(i);
				annotationSpline.curveVOI.dispose();
				annotationSpline.curveVOI = null;

				updateCurveListeners();
			}
			else {
				NaturalSpline3 spline = smoothCurve(annotationSpline.annotations);

				//display:
				imageA.unregisterVOI(annotationSpline.curveVOI);

				annotationSpline.curveVOI.getCurves().clear();
				annotationSpline.curve.dispose();
				annotationSpline.curve = null;
				annotationSpline.curve = new VOIContour(false);

				annotationSpline.curveVOI.dispose();
				annotationSpline.curveVOI = null;
				annotationSpline.curveVOI = new VOI((short)splineControlPtsList.size(), annotationSpline.name );
				annotationSpline.curveVOI.getCurves().add(annotationSpline.curve);


				float length = spline.GetLength(0, 1);
				int maxLength = (int)Math.ceil(length);
				float step = 1;
				if ( maxLength != length )
				{
					step = (length / maxLength);
				}
				for (int j = 0; j <= maxLength; j++) {
					final float t = spline.GetTime(j*step);
					annotationSpline.curve.add(spline.GetPosition(t));
				}
				annotationSpline.curveVOI.setColor(Color.red);
				annotationSpline.curve.update(new ColorRGBA(1, 0, 0, 1));

				imageA.registerVOI(annotationSpline.curveVOI);
			}
			imageA.notifyImageDisplayListeners();
		}
		
		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ ) {
			VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			text.setCurveAnnotation(false);
		}
		for ( int i = 0; i < splineControlPtsList.size(); i++ )
		{
			AnnotationSplineControlPts annotationSpline = splineControlPtsList.elementAt(i);
			// update annotations with isCurveAnnotation flag:
			for ( int j = 0; j < annotationSpline.annotations.size(); j++ ) {
				annotationSpline.annotations.elementAt(j).setCurveAnnotation(true);
			}
		}
	}
	
	/**
	 * Updates the list of listeners that the annotations have
	 * changed. This is how the latticeModel communicates changes
	 * to the different plugins that display lists of annotations, etc.
	 */
	private void updateAnnotationListeners() {
		updateSplineControlPoints();
		if ( annotationListeners != null ) {
			for ( int i = 0; i < annotationListeners.size(); i++ ) {
				annotationListeners.elementAt(i).annotationChanged();
			}
		}
	}
	
	private void updateCurveListeners() {
		if ( curveListeners != null ) {
			for ( int i = 0; i < curveListeners.size(); i++ ) {
				curveListeners.elementAt(i).curveChanged();
			}
		}
	}

	private void updateLatticeListeners() {
		if ( latticeListeners != null ) {
			for ( int i = 0; i < latticeListeners.size(); i++ ) {
				latticeListeners.elementAt(i).latticeChanged();
			}
		}
	}
		
	private void makeLatticeContours() {
		if ( leftContour != null ) {
			imageA.unregisterVOI(leftContour);
			if ( leftContour.getCurves() != null ) {
				leftContour.getCurves().clear();
			}
		}
		else {
			final short id = (short) imageA.getVOIs().getUniqueID();
			leftContour = new VOI(id, "leftContour", VOI.POLYLINE, (float) Math.random());
		}
		if ( rightContour != null ) {
			imageA.unregisterVOI(rightContour);
			if ( leftContour.getCurves() != null ) {
				rightContour.getCurves().clear();
			}
		}
		else {
			final short id = (short) imageA.getVOIs().getUniqueID();
			rightContour = new VOI(id, "rightContour", VOI.POLYLINE, (float) Math.random());
		}

		VOIContour leftC = new VOIContour(false);
		leftContour.getCurves().add(leftC);
		
		VOIContour rightC = new VOIContour(false);
		rightContour.getCurves().add(rightC);

		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			leftC.add(left.getCurves().elementAt(i).elementAt(0));
		}
		for ( int i = 0; i < right.getCurves().size(); i++ ) {
			rightC.add(right.getCurves().elementAt(i).elementAt(0));
		}

		leftContour.setColor(new Color(0, 0, 255));
		leftC.update(new ColorRGBA(0, 0, 1, 1));
		
		rightContour.setColor(new Color(0, 0, 255));
		rightC.update(new ColorRGBA(0, 0, 1, 1));

		imageA.registerVOI(leftContour);
		imageA.registerVOI(rightContour);
			
		imageA.notifyImageDisplayListeners();
	}
	
	/**
	 * Updates the lattice data structures for rendering whenever the user changes the lattice.
	 * 
	 * @param rebuild
	 */
	private void updateLattice(final boolean rebuild) {
		if (left == null || right == null) {
			updateLatticeListeners();
			return;
		}
		if (right.getCurves().size() == 0) {
			updateLatticeListeners();
			return;
		}
		
		makeLatticeContours();
		
		if (rebuild) {
			// System.err.println( "new pt added" );
			if (latticeGrid != null) {
				for (int i = latticeGrid.size() - 1; i >= 0; i--) {
					final VOI marker = latticeGrid.remove(i);
					imageA.unregisterVOI(marker);
				}
			} else {
				latticeGrid = new VOIVector();
			}
			for (int j = 0; j < Math.min(left.getCurves().size(), right.getCurves().size()); j++) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				final VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float) Math.random());
				final VOIContour mainAxis = new VOIContour(false);
				mainAxis.add(left.getCurves().elementAt(j).elementAt(0));
				mainAxis.add(right.getCurves().elementAt(j).elementAt(0));
				marker.getCurves().add(mainAxis);
				marker.setColor(new Color(255, 255, 0));
				mainAxis.update(new ColorRGBA(1, 1, 0, 1));
				if (j == 0) {
					marker.setColor(new Color(0, 255, 0));
					mainAxis.update(new ColorRGBA(0, 1, 0, 1));
				}
				imageA.registerVOI(marker);
				latticeGrid.add(marker);
			}
		} else {
			for (int i = 0; i < latticeGrid.size(); i++) {
				final VOI marker = latticeGrid.elementAt(i);
				marker.getCurves().elementAt(0).elementAt(0).copy(left.getCurves().elementAt(i).elementAt(0));
				marker.getCurves().elementAt(0).elementAt(1).copy(right.getCurves().elementAt(i).elementAt(0));
				marker.update();
			}
		}
		left.update();
		right.update();

		if (centerLine != null) {
			imageA.unregisterVOI(centerLine);
		}
		if (rightLine != null) {
			imageA.unregisterVOI(rightLine);
		}
		if (leftLine != null) {
			imageA.unregisterVOI(leftLine);
		}
		boolean showContours = false;
		if (displayContours != null) {
			showContours = (imageA.isRegistered(displayContours) != -1);
			if (showContours) {
				imageA.unregisterVOI(displayContours);
			}
		}

		if ( (left.getCurves().size() == right.getCurves().size()) && (left.getCurves().size() >= 2)) {
			generateCurves(5, 10);
			if (showContours) {
				imageA.registerVOI(displayContours);
			}
		}

		updateSelected();

		latticeInterpolationInit = false;

		updateLatticeListeners();

		// when everything's done, notify the image listeners
		imageA.notifyImageDisplayListeners();
	}

	/**
	 * Updates the lattice data structures on undo/redo.
	 */
	private void updateLinks() {
		if (latticeGrid != null) {
			latticeGrid.clear();
		} else {
			latticeGrid = new VOIVector();
		}

		annotationVOIs = null;
		leftMarker = null;
		rightMarker = null;
		VOIVector vois = imageA.getVOIs();
//		System.err.println( "updateLinks " + vois.size() );
		for (int i = 0; i < vois.size(); i++) {
			final VOI voi = vois.elementAt(i);
			final String name = voi.getName();
//			System.err.println( vois.elementAt(i).getName() );
			if (name.equals("left")) {
				left = voi;
			} else if (name.equals("right")) {
				right = voi;
			} else if (name.equals("leftContour")) {
				leftContour = voi;
			} else if (name.equals("rightContour")) {
				rightContour = voi;
			} else if (name.equals("left line")) {
				leftLine = voi;
			} else if (name.equals("right line")) {
				rightLine = voi;
			} else if (name.equals("center line")) {
				centerLine = voi;
			} else if (name.contains("pair_")) {
				latticeGrid.add(voi);
			} else if (name.contains("wormContours")) {
				displayContours = voi;
			} else if (name.contains("interpolatedContours")) {
				displayInterpolatedContours = voi;
			} else if (name.contains("showSelected")) {
				showSelectedVOI = voi;
				imageA.unregisterVOI(showSelectedVOI);
				showSelectedVOI = null;
//				System.err.println("updateLinks showSelected ");
			} else if (name.equals("leftMarker")) {
				leftMarker = voi;
			} else if (name.equals("rightMarker")) {
				rightMarker = voi;
			} else if (name.contains("annotationVOIs")) {
//				System.err.println("updateLinks update annotations: " + name + "   " + annotationVOIs );
				annotationVOIs = voi;
			}
		}
		imageA.unregisterVOI(leftContour);
		imageA.unregisterVOI(rightContour);
		
		lattice.clear();
		lattice.add(left);
		lattice.add(right);
		
		clear3DSelection();
		colorAnnotations();
//		System.err.println("updateLinks " + (annotationVOIs != null) );
		if ( annotationVOIs != null ) {
			//			for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ ) {
			//				final VOIWormAnnotation text = (VOIWormAnnotation) annotationVOIs.getCurves().elementAt(i);
			//				if ( text.isSelected() ) {
			//					text.setSelected(false);
			//					text.updateSelected(imageA);
			//				}
			//			}
		}
		updateLattice(true);
		showLatticeLabels(displayLatticeLabels);

		vois = imageA.getVOIs();
		for (int i = 0; i < vois.size(); i++) {
			final VOI voi = vois.elementAt(i);
			final String name = voi.getName(); 
			if (name.contains("showSelected")) {
				imageA.unregisterVOI(voi);
			}
		}
	}

	/**
	 * Updates the VOI displaying which point (lattice or annotation) is currently selected when the selection changes.
	 */
	private void updateSelected() {
		if (pickedPoint != null) {
			if (showSelectedVOI == null) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				showSelectedVOI = new VOI(id, "showSelected", VOI.POLYLINE, (float) Math.random());
				imageA.registerVOI(showSelectedVOI);
				showSelectedVOI.setColor(new Color(0, 255, 255));
			}
			if ((showSelected == null) || (showSelectedVOI.getCurves().size() == 0)) {
				showSelected = new VOIContour[3];
				showSelected[0] = new VOIContour(true);
				makeSelectionFrame(Vector3f.UNIT_X, Vector3f.UNIT_Y, pickedPoint, 4, showSelected[0]);
				showSelectedVOI.getCurves().add(showSelected[0]);
				showSelected[0].update(new ColorRGBA(0, 1, 1, 1));

				showSelected[1] = new VOIContour(true);
				makeSelectionFrame(Vector3f.UNIT_Z, Vector3f.UNIT_Y, pickedPoint, 4, showSelected[1]);
				showSelectedVOI.getCurves().add(showSelected[1]);
				showSelected[1].update(new ColorRGBA(0, 1, 1, 1));

				showSelected[2] = new VOIContour(true);
				makeSelectionFrame(Vector3f.UNIT_Z, Vector3f.UNIT_X, pickedPoint, 4, showSelected[2]);
				showSelectedVOI.getCurves().add(showSelected[2]);
				showSelected[2].update(new ColorRGBA(0, 1, 1, 1));

				showSelectedVOI.setColor(new Color(0, 255, 255));
			} else {
				for (int i = 0; i < showSelected.length; i++) {
					final Vector3f center = new Vector3f();
					for (int j = 0; j < showSelected[i].size(); j++) {
						center.add(showSelected[i].elementAt(j));
					}
					center.scale(1f / showSelected[i].size());
					final Vector3f diff = Vector3f.sub(pickedPoint, center);
					for (int j = 0; j < showSelected[i].size(); j++) {
						showSelected[i].elementAt(j).add(diff);
					}
				}
				showSelectedVOI.update();
			}
			if (imageA.isRegistered(showSelectedVOI) == -1) {
				imageA.registerVOI(showSelectedVOI);
			}

			if ( (left != null) && (pickedPoint == left.getCurves().lastElement().elementAt(0)) ) {
				if ( leftMarker != null )
				{
					leftMarker.getCurves().elementAt(0).elementAt(0).copy(pickedPoint);
					leftMarker.update();
				}
			}
			else if ( (right != null) && (pickedPoint == right.getCurves().lastElement().elementAt(0)) ) {
				if ( rightMarker != null )
				{
					rightMarker.getCurves().elementAt(0).elementAt(0).copy(pickedPoint);
					rightMarker.update();
				}
			}
		}
	}
	
	private int findPoint( VOI left, Vector3f pickedPoint) 
	{
		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			if ( pickedPoint == left.getCurves().elementAt(i).elementAt(0) )
				return i;
		}
		return -1;
	}
	
	private void updateSeamCount() {
		int totalSeamCount = 0;
		int seamCount = 0;
		int nonSeamCount = 0;
		if (left.getCurves().size() == right.getCurves().size()) {
			totalSeamCount = 0;
			for ( int i = 0; i < left.getCurves().size(); i++ ) {
				VOIWormAnnotation leftAnnotation = (VOIWormAnnotation) left.getCurves().elementAt(i);
				if ( leftAnnotation.isSeamCell() ) {
					totalSeamCount++;
				}
			}
			for ( int i = 0; i < left.getCurves().size(); i++ ) {
				VOIWormAnnotation leftAnnotation = ((VOIWormAnnotation)left.getCurves().elementAt(i));
				VOIWormAnnotation rightAnnotation = ((VOIWormAnnotation)right.getCurves().elementAt(i));
				if ( leftAnnotation.isSeamCell() ) {
					seamCount++;
				}
				else {
					nonSeamCount++;
				}
				String name = "";
				if ( totalSeamCount <= 10 ) {
					// H0-H1-H2-V1-V2-V3-V4-V5-V6-T 
					if ( leftAnnotation.isSeamCell() ) {
						name = seamCount < 4 ? ("H" + (seamCount-1)) : (seamCount <= 9) ? ("V" + (seamCount - 3)) : "T";
					}
					else {
						name = "a" + (nonSeamCount-1);
					}
				}
				else if ( totalSeamCount > 10 ) {
					// H0-H1-H2-V1-V2-V3-V4-Q-V5-V6-T 
					if ( leftAnnotation.isSeamCell() ) {
						name = seamCount < 4 ? ("H" + (seamCount-1)) : (seamCount < 8) ? ("V" + (seamCount - 3)) : (seamCount == 8) ? "Q" : (seamCount < 11) ? ("V" + (seamCount - 4)) : "T";
					}
					else {
						name = "a" + (nonSeamCount-1);
					}
				}
				leftAnnotation.setText( name + "L" );
				leftAnnotation.updateText();
				leftAnnotation.update();
				
				rightAnnotation.setText( name + "R" );
				rightAnnotation.updateText();
				rightAnnotation.update();
			}
		}
	}
	
	public static boolean renameLatticeOnLoad( ModelImage image, VOIVector latticeVector)
	{
		if ( latticeVector == null ) return false;
		if ( latticeVector.size() < 2 ) return false;
		
		VOI left = latticeVector.elementAt(0);
		VOI right = latticeVector.elementAt(1);
		if ( left == null || right == null ) return false;
		
		// check if lattice is already named, if so return as is:
		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			VOIWormAnnotation text = (VOIWormAnnotation) left.getCurves().elementAt(i);
			if ( text.getText().contains("H") || text.getText().contains("V") || text.getText().contains("T") ) {
				return false;
			}
			text = (VOIWormAnnotation) right.getCurves().elementAt(i);
			if ( text.getText().contains("H") || text.getText().contains("V") || text.getText().contains("T") ) {
				return false;
			}
		}

		boolean rename = false;
		float[] pairSort = new float[left.getCurves().size()];
		// decide which 10 points are seam cells by taking max pair values
		for ( int i = 0; i < left.getCurves().size() -1; i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation) left.getCurves().elementAt(i);
			int x = (int) text.elementAt(0).X;
			int y = (int) text.elementAt(0).Y;
			int z = (int) text.elementAt(0).Z;
			float value;
			if ( image.isColorImage() ) 
			{
				value = image.getFloatC(x,y,z,2);
			}
			else
			{
				value = image.getFloat(x,y,z);
			}

			text = (VOIWormAnnotation) right.getCurves().elementAt(i);
			x = (int) text.elementAt(0).X;
			y = (int) text.elementAt(0).Y;
			z = (int) text.elementAt(0).Z;
			if ( image.isColorImage() ) 
			{
				value += image.getFloatC(x,y,z,2);
			}
			else
			{
				value += image.getFloat(x,y,z);
			}
			pairSort[i] = value;
		}
		Arrays.sort(pairSort);

		int pairCount = 0;
		int extraCount = 0;
		for ( int i = 0; i < left.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation) left.getCurves().elementAt(i);
			int x = (int) text.elementAt(0).X;
			int y = (int) text.elementAt(0).Y;
			int z = (int) text.elementAt(0).Z;
			float value;
			if ( image.isColorImage() ) 
			{
				value = image.getFloatC(x,y,z,2);
			}
			else
			{
				value = image.getFloat(x,y,z);
			}
			
			text = (VOIWormAnnotation) right.getCurves().elementAt(i);
			x = (int) text.elementAt(0).X;
			y = (int) text.elementAt(0).Y;
			z = (int) text.elementAt(0).Z;
			if ( image.isColorImage() ) 
			{
				value += image.getFloatC(x,y,z,2);
			}
			else
			{
				value += image.getFloat(x,y,z);
			}

			boolean isSeamPair = left.getCurves().size() <= 10;
			if ( !isSeamPair )
			{
				// check if this pair has a high enough value to be added to the list of seam cells:
				for ( int j = 0; j < Math.min(10, pairSort.length); j++ )
				{
					if ( value >= pairSort[(pairSort.length -1) - j] )
					{
//						System.err.println( value + "  " + pairSort[(pairSort.length -1) - j]);
						isSeamPair = true;
						break;
					}
				}
			}
			if ( isSeamPair )
			{
				rename = true;
				String name = pairCount < 3 ? ("H" + pairCount) : (pairCount < 9) ? ("V" + (pairCount - 2)) : "T";
				pairCount++;

				// left seam cell:
				text = (VOIWormAnnotation) left.getCurves().elementAt(i);
				text.setText( name + "L" );
				text.setSeamCell(true);

				// right seam cell:
				text = (VOIWormAnnotation) right.getCurves().elementAt(i);
				text.setText( name + "R" );
				text.setSeamCell(true);
			}
			else
			{
				String name = "a" + extraCount++;

				// left seam cell:
				text = (VOIWormAnnotation) left.getCurves().elementAt(i);
				text.setText( name + "L" );

				// right seam cell:
				text = (VOIWormAnnotation) right.getCurves().elementAt(i);
				text.setText( name + "R" );
			}
		}
		return rename;
	}

	public static void saveLattice(final String directory, final String fileName, VOIVector latticeVector )
	{
		if ( (latticeVector == null) || (latticeVector.size() < 2) ) return;
		
		VOI left = latticeVector.elementAt(0);
		VOI right = latticeVector.elementAt(1);
		
		if ( left == null || right == null )
		{
			return;
		}
		if ( left.getCurves().size() != right.getCurves().size() )
		{
			return;
		}
		if (fileName != null)
		{
			final String voiDir = new String(directory + fileName + File.separator);

			VOI latticePoints = new VOI( (short)0, "lattice", VOI.ANNOTATION, 0);
			for ( int j = 0; j < left.getCurves().size(); j++ )
			{
				latticePoints.getCurves().add(new VOIWormAnnotation((VOIWormAnnotation)left.getCurves().elementAt(j)));
				latticePoints.getCurves().add(new VOIWormAnnotation((VOIWormAnnotation)right.getCurves().elementAt(j)));
			}
			if ( latticePoints.getCurves().size() == 0 ) {
//				System.err.println( "saveLattice " + latticePoints.getCurves().size() );
				return;
			}
			LatticeModel.saveAnnotationsAsCSV(voiDir + File.separator, "lattice.csv", latticePoints);
			
			// save seam-cells derived from lattice:
			String voiSeamDir = directory + "seam_cell_final" + File.separator;
			latticePoints.getCurves().clear();
			for ( int j = 0; j < left.getCurves().size(); j++ )
			{
				VOIWormAnnotation leftAnnotation = (VOIWormAnnotation) left.getCurves().elementAt(j);
				VOIWormAnnotation rightAnnotation = (VOIWormAnnotation) right.getCurves().elementAt(j);
				if ( leftAnnotation.isSeamCell() || rightAnnotation.isSeamCell() ) {
					latticePoints.getCurves().add(leftAnnotation);
					latticePoints.getCurves().add(rightAnnotation);
				}
			}
			LatticeModel.saveAnnotationsAsCSV(voiSeamDir, "seam_cells.csv", latticePoints);
		}
	}
	

	private VOI annotationChanged( VOI annotationsNew, VOI annotationOld ) {
		VOI annotationsChangeList = new VOI( (short)0, "changed annotations", VOI.ANNOTATION, 0 );
		
		for ( int i = 0; i < annotationsNew.getCurves().size(); i++ ) {
        	VOIWormAnnotation annotation = (VOIWormAnnotation) annotationsNew.getCurves().elementAt(i);
        	// find match by name:
        	boolean found = false;
    		for ( int j = 0; j < annotationOld.getCurves().size(); j++ ) {
            	VOIWormAnnotation orig = (VOIWormAnnotation) annotationOld.getCurves().elementAt(j);
            	if ( annotation.getText().equals(orig.getText()) ) {

                	Vector3f ptO = new Vector3f(annotation.elementAt(0));
                	ptO.X = Math.round(ptO.X);
                	ptO.Y = Math.round(ptO.Y);
                	ptO.Z = Math.round(ptO.Z);
                	
                	Vector3f pt = new Vector3f(orig.elementAt(0));
                	pt.X = Math.round(pt.X);
                	pt.Y = Math.round(pt.Y);
                	pt.Z = Math.round(pt.Z);
                	
                	if ( !pt.equals(ptO) ) {
                		// point changed position - add to change list: 
                		annotationsChangeList.getCurves().add(annotation);
                		System.err.println("moved annotation " + annotation.getText() );
                	}
            		found = true;
            		break;
            	}
    		}
    		if ( !found ) {
        		// new point - add to change list: 
        		annotationsChangeList.getCurves().add(annotation);
        		System.err.println("new annotation " + annotation.getText() );
    		}
		}
		
		return annotationsChangeList;
	}
	
	private boolean latticeChanged() {
		
		VOIVector finalLattice = readLatticeCSV(outputDirectory + File.separator + WormData.editLatticeOutput + File.separator + "lattice.csv");
        if ( finalLattice.size() < 2 ) 
        	return true;

		VOI leftOrig = (VOI) finalLattice.elementAt(0);
		VOI rightOrig = (VOI) finalLattice.elementAt(1);
		
		if ( leftOrig.getCurves().size() != left.getCurves().size() || rightOrig.getCurves().size() != right.getCurves().size() ) 
			return true;
		
		for ( int i = 0; i < leftOrig.getCurves().size(); i++ ) {
        	VOIWormAnnotation leftPtOrig = (VOIWormAnnotation) leftOrig.getCurves().elementAt(i);
        	VOIWormAnnotation leftPt = (VOIWormAnnotation) left.getCurves().elementAt(i);

        	Vector3f ptO = new Vector3f(leftPtOrig.elementAt(0));
        	ptO.X = Math.round(ptO.X);
        	ptO.Y = Math.round(ptO.Y);
        	ptO.Z = Math.round(ptO.Z);
        	
        	Vector3f pt = new Vector3f(leftPt.elementAt(0));
        	pt.X = Math.round(pt.X);
        	pt.Y = Math.round(pt.Y);
        	pt.Z = Math.round(pt.Z);
        	
        	if ( !pt.equals(ptO) ) 
        		return true;

        	VOIWormAnnotation rightPtOrig = (VOIWormAnnotation) rightOrig.getCurves().elementAt(i);
        	VOIWormAnnotation rightPt = (VOIWormAnnotation) right.getCurves().elementAt(i);
        	ptO = new Vector3f(rightPtOrig.elementAt(0));
        	ptO.X = Math.round(ptO.X);
        	ptO.Y = Math.round(ptO.Y);
        	ptO.Z = Math.round(ptO.Z);
        	
        	pt = new Vector3f(rightPt.elementAt(0));
        	pt.X = Math.round(pt.X);
        	pt.Y = Math.round(pt.Y);
        	pt.Z = Math.round(pt.Z);
        	
        	if ( !pt.equals(ptO) ) 
        		return true;
        }
		return false;
	}
}
